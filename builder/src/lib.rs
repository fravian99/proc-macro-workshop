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

        let setters = fields.iter().map(|Field { ident, ty, .. }| {
            let field_type = unwrap_optional(ty).or(Some(ty));
            quote! {
                fn #ident(&mut self, #ident: #field_type) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        });

        let parameters_dec = fields.iter().map(|Field { ident, ty, .. }| {
            let field_type = unwrap_optional(ty).or(Some(ty));
            quote! {
                #ident : Option<#field_type>
            }
        });

        let parameters_check = fields.iter().map(|Field { ident, ty, .. }| {
            if unwrap_optional(ty).is_some() {
                quote! {
                    let #ident = cloned.#ident;
                }
            } else {
                let string_ident = ident.clone().map(|ident| ident.to_string());
                quote! {
                    let #ident = match cloned.#ident {
                        Some(#ident) => Ok(#ident),
                        None => Err(#string_ident)
                    }?;
                }
            }
        });

        let token_stream = proc_macro::TokenStream::from(quote! {
            #[derive(Clone)]
            pub struct #builder_name {
                #(#parameters_dec,)*
            }

            impl #builder_name {
                #(#setters)*

                fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                    let cloned = self.clone();
                    #(#parameters_check)*
                    Ok(#name { #(#token_ident,)* })
                }
            }

            impl #name {
                fn builder() -> #builder_name {
                    #builder_name {
                        #(#token_ident: None,)*
                    }
                }
            }
        });

        println!("> TokenStream: {}", token_stream);
        return token_stream;
    }
    TokenStream::new()
}

fn unwrap_optional(field_type: &syn::Type) -> Option<&syn::Type> {
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
        } if ident == "Option" && args.len() == 1 => args,
        _ => return None,
    };

    let ty = match &args[0] {
        syn::GenericArgument::Type(t) => t,
        _ => return None,
    };

    Some(ty)
}
