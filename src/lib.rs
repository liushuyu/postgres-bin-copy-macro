use std::convert::TryInto;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{
    bracketed, parse_macro_input, Expr, ExprLit, ExprPath, Ident, Lit, LitByteStr, LitStr, Path,
    PathSegment, Token,
};

const PGCOPY_MAGIC: &[u8] = b"PGCOPY\n\xff\r\n\x00";
const PGCOPY_OPTIONS: &[u8] = &[0, 0, 0, 0, 0, 0, 0, 0];

enum GeneratorState {
    Head,
    Body,
    Trailer,
}

struct PgSerialize {
    writer_name: Ident,
    ty: Punctuated<ExprLit, Comma>,
    fields: Punctuated<Expr, Comma>,
    mode: GeneratorState,
}

fn create_new_serializer(writer_name: Ident, mode: GeneratorState) -> PgSerialize {
    PgSerialize {
        writer_name,
        ty: Punctuated::new(),
        fields: Punctuated::new(),
        mode
    }
}

impl Parse for PgSerialize {
    fn parse(input: ParseStream) -> Result<Self> {
        let writer_name = input.parse()?;
        input.parse::<Token![,]>()?;
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            let name = input.parse::<LitStr>()?;
            match name.value().as_str() {
                "start" => return Ok(create_new_serializer(writer_name, GeneratorState::Head)),
                "end" => return Ok(create_new_serializer(writer_name, GeneratorState::Trailer)),
                _ => {
                    return Err(syn::Error::new_spanned(
                        &name,
                        format!("unexpected specifier: {}", name.value()),
                    ))
                }
            }
        }
        let type_names;
        bracketed!(type_names in input);
        let ty = type_names.parse_terminated(ExprLit::parse)?;
        input.parse::<Token![:]>()?;

        Ok(PgSerialize {
            writer_name,
            ty,
            fields: input.parse_terminated(Expr::parse)?,
            mode: GeneratorState::Body,
        })
    }
}

fn encode_header(w: &Ident) -> proc_macro2::TokenStream {
    // calculate the header value
    let mut header = PGCOPY_MAGIC.to_vec();
    header.extend(PGCOPY_OPTIONS);
    let header_value = LitByteStr::new(&header, Span::call_site());
    quote! {
        #w.write_all(#header_value)?;
    }
}

fn encode_string(w: &Ident, s: &Expr) -> proc_macro2::TokenStream {
    quote! {
        let v = #s;
        let size: u32 = v.len().try_into()?;
        #w.write_all(&size.to_be_bytes())?;
        #w.write_all(v.as_bytes())?;
    }
}

fn encode_integer(w: &Ident, s: &Expr) -> proc_macro2::TokenStream {
    quote! {
        let v = #s;
        let size: u32 = std::mem::size_of_val(&v).try_into()?;
        #w.write_all(&size.to_be_bytes())?;
        #w.write_all(&v.to_be_bytes())?;
    }
}

fn encode_null(w: &Ident) -> proc_macro2::TokenStream {
    quote! {
        #w.write_all(0xFFFFu32.to_be_bytes().as_ref())?;
    }
}

fn create_fake_expr(name: &str, donor: &Expr) -> ExprPath {
    let unwrap_value = Ident::new(name, donor.span());
    let mut path = Punctuated::new();
    path.push(PathSegment::from(unwrap_value));
    ExprPath {
        attrs: vec![],
        qself: None,
        path: Path {
            leading_colon: None,
            segments: path,
        },
    }
}

/// Syntax: pg_serialize!(writer, [type_names], args...)
///
/// Example:
/// `pg_serialize!(writer, ["i","i","s","n"]: 3, 3, "foo", ())`
#[proc_macro]
pub fn pg_serialize(input: TokenStream) -> TokenStream {
    let PgSerialize {
        writer_name,
        ty,
        fields,
        mode,
    } = parse_macro_input!(input as PgSerialize);
    match mode {
        GeneratorState::Head => return TokenStream::from(encode_header(&writer_name)),
        GeneratorState::Body => (),
        GeneratorState::Trailer => return TokenStream::from(encode_null(&writer_name)),
    }
    if ty.len() != fields.len() {
        return syn::Error::new_spanned(
            &fields,
            format!(
                "Expected {} arguments according to the type specification, found {}",
                ty.len(),
                fields.len()
            ),
        )
        .into_compile_error()
        .into();
    }
    if fields.len() > u16::MAX.into() {
        return syn::Error::new_spanned(&fields, format!("Too many arguments! PostgreSQL binary protocol only supports up to {} tuples per row!", u16::MAX))
            .into_compile_error()
            .into();
    }
    let fields = fields.iter().collect::<Vec<_>>();
    let fields_len: u16 = fields.len().try_into().unwrap();
    let mut assert_type_stmts = proc_macro2::TokenStream::new();
    let mut writer_stmts = proc_macro2::TokenStream::new();

    for (i, t) in ty.iter().enumerate() {
        let ty = match &t.lit {
            Lit::Str(s) => s.value(),
            Lit::Char(s) => s.value().to_string(),
            _ => {
                return syn::Error::new_spanned(&t.lit, format!("Unsupported literal type"))
                    .into_compile_error()
                    .into();
            }
        };
        let assert_type;
        let value = fields[i];
        match ty.as_str() {
            "s" => {
                assert_type = quote_spanned! {value.span()=>
                    #value.to_string();
                };
                writer_stmts.extend(encode_string(&writer_name, &value));
            }
            "s?" => {
                // here we test if the type is an Option type
                assert_type = quote_spanned! {value.span()=>
                    let _: Option<_> = #value;
                };
                let unwrap_value = create_fake_expr("v", &value);
                let str_stmt = encode_string(&writer_name, &Expr::Path(unwrap_value));
                let null_stmt = encode_null(&writer_name);
                writer_stmts.extend(quote! {
                    if let Some(v) = #value {
                        #str_stmt
                    } else {
                        #null_stmt
                    }
                });
            }
            "i" => {
                // here we test if the type could be converted to a SQL (big-)integer
                assert_type = quote_spanned! {value.span() =>
                    let _: i64 = #value.try_into().unwrap();
                };
                writer_stmts.extend(encode_integer(&writer_name, &value));
            }
            "i?" => {
                assert_type = quote_spanned! {value.span()=>
                    let _: Option<_> = #value;
                };
                let unwrap_value = create_fake_expr("v", &value);
                let int_stmt = encode_integer(&writer_name, &Expr::Path(unwrap_value));
                let null_stmt = encode_null(&writer_name);
                writer_stmts.extend(quote! {
                    if let Some(v) = #value {
                        #int_stmt
                    } else {
                        #null_stmt
                    }
                });
            }
            "n" => {
                assert_type = quote_spanned! {value.span() =>
                    let _: () = #value;
                };
                writer_stmts.extend(encode_null(&writer_name));
            }
            _ => {
                return syn::Error::new_spanned(&t, format!("Unknown type specifier: `{}`", ty))
                    .into_compile_error()
                    .into();
            }
        }
        assert_type_stmts.extend(assert_type);
    }

    let expanded = quote! {{
        use std::convert::TryInto;
        #writer_name.write_all(&#fields_len.to_be_bytes())?;
        #assert_type_stmts
        #writer_stmts
    }};

    TokenStream::from(expanded)
}
