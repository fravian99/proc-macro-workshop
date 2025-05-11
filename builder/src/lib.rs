use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    //print!("{:?}", input);
    let input = parse_macro_input!(input as DeriveInput);

    // ident is the name of the type
    let name = &input.ident;
    println!(">Struct name: {}", name);

    let builder_name = format_ident!("{}Builder", name);

    let data = &input.data;
    if let Data::Struct(data) = data {
        let fields = match &data.fields {
            Fields::Named(fields) => fields.named.iter(),
            _ => unimplemented!(),
        };

        let fields: Vec<&Field> = fields.collect();

        let token_ident: Vec<proc_macro2::TokenStream> = fields
            .iter()
            .map(|Field { ident, .. }| quote! {#ident})
            .collect();

        let setters = fields.iter().map(|a| setter(a));
        let parameters_dec = fields.iter().map(|a| parameter_declaration(a));
        let parameters_init = fields.iter().map(|a| parameter_init(a));
        let parameters_check = fields.iter().map(|a| parameter_check(a));

        let token_stream = proc_macro::TokenStream::from(quote! {
            #[derive(Clone)]
            pub struct #builder_name {
                #(#parameters_dec,)*
            }

            impl #builder_name {
                #(#setters)*

                fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                    let cloned = self.clone();
                    #(#parameters_check)*
                    Ok(#name { #(#token_ident,)* })
                }
            }

            impl #name {
                fn builder() -> #builder_name {
                    #builder_name {
                        #(#parameters_init, )*
                    }
                }
            }
        });

        println!("> TokenStream: {}", token_stream);
        return token_stream;
    }
    TokenStream::new()
}

fn setter(field: &Field) -> proc_macro2::TokenStream {
    let Field { ident, ty, .. } = field;
    let function = move |field: &Field, value: proc_macro2::Ident| {
        let Field { ident, ty, .. } = &field;
        let field_type = unwrap_vec(ty).unwrap_or(ty);
        quote! {
            fn #value(&mut self, #value: #field_type) -> &mut Self {
                self.#ident.push(#value);
                self
            }
        }
    };
    let token_stream = builder_attrs(field, function);
    if let Some(token_stream) = token_stream {
        return token_stream;
    }
    let field_type = unwrap_optional(ty).unwrap_or(ty);
    quote! {
        fn #ident(&mut self, #ident: #field_type) -> &mut Self {
            self.#ident = Some(#ident);
            self
        }
    }
}

fn parameter_init(Field { attrs, ident, .. }: &Field) -> proc_macro2::TokenStream {
    if attrs_each(attrs) {
        quote! {
            #ident: Vec::new()
        }
    } else {
        quote! {
            #ident : None
        }
    }
}

fn parameter_declaration(
    Field {
        attrs, ident, ty, ..
    }: &Field,
) -> proc_macro2::TokenStream {
    if attrs_each(attrs) {
        let field_type = unwrap_vec(ty).unwrap_or(ty);
        quote! {
            #ident : std::vec::Vec<#field_type>
        }
    } else {
        let field_type = unwrap_optional(ty).unwrap_or(ty);
        quote! {
            #ident : std::option::Option<#field_type>
        }
    }
}

fn parameter_check(
    Field {
        attrs, ident, ty, ..
    }: &Field,
) -> proc_macro2::TokenStream {
    if unwrap_optional(ty).is_some() || attrs_each(attrs) {
        quote! {
            let #ident = cloned.#ident;
        }
    } else {
        let string_ident = ident.as_ref().map(|ident| ident.to_string());
        quote! {
            let #ident = match cloned.#ident {
                Some(#ident) => Ok(#ident),
                None => Err(#string_ident)
            }?;
        }
    }
}

fn builder_attrs<'a, 'b>(
    field: &'a Field,
    function: impl Fn(&'a Field, proc_macro2::Ident) -> proc_macro2::TokenStream,
) -> Option<proc_macro2::TokenStream> {
    let values: Vec<_> = field
        .attrs
        .iter()
        .filter_map(|a| match parse_attrs_each(a) {
            Some(Ok(ident)) => Some(function(field, ident)),
            Some(Err(err)) => Some(err.to_compile_error()),
            _ => None,
        })
        .collect();
    if !values.is_empty() {
        Some(quote! {
            #(#values)*
        })
    } else {
        None
    }
}

fn attrs_each(attrs_builder: &[syn::Attribute]) -> bool {
    attrs_builder
        .iter()
        .filter_map(|a| match parse_attrs_each(a) {
            Some(Ok(ident)) => Some(ident),
            _ => None,
        })
        .next()
        .is_some()
}
/// Return None if not a builder attribute
/// Return Some(Err) if it is an builder attribute and not a valid each attribute
/// Return Some(Ok(ident)) if it is an builder attribute and a valid each attribute
fn parse_attrs_each(attr: &syn::Attribute) -> Option<Result<syn::Ident, syn::Error>> {
    if !attr.path().is_ident("builder") {
        return None;
    }
    let syn::Attribute { meta, .. } = attr;
    let args = if let syn::Meta::List(list) = meta {
        list.parse_args::<syn::MetaNameValue>().ok()
    } else {
        None
    };

    if let Some(args) = args {
        if args.path.is_ident("each") {
            if let syn::Expr::Lit(value) = args.value {
                if let syn::Lit::Str(value) = value.lit {
                    let value = value.value();
                    let ident = syn::Ident::new(&value, proc_macro2::Span::call_site());
                    return Some(Ok(ident));
                }
            }
        }
    }
    Some(Err(syn::Error::new_spanned(
        meta,
        "expected `builder(each = \"...\")`",
    )))
}

fn unwrap_optional(field_type: &syn::Type) -> Option<&syn::Type> {
    unwrap_type(field_type, "Option")
}

fn unwrap_vec(field_type: &syn::Type) -> Option<&syn::Type> {
    unwrap_type(field_type, "Vec")
}

fn unwrap_type<'a>(field_type: &'a syn::Type, outer_type: &'a str) -> Option<&'a syn::Type> {
    let segments = match field_type {
        syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) if segments.len() == 1 => segments,
        _ => return None,
    };
    let args = match &segments[0] {
        syn::PathSegment {
            ident,
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
        } if ident == outer_type && args.len() == 1 => args,
        _ => return None,
    };

    let ty = match &args[0] {
        syn::GenericArgument::Type(t) => t,
        _ => return None,
    };

    Some(ty)
}
