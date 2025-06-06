use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, Data, DeriveInput, Field, Fields};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;
    let data = input.data;
    let generics = input.generics;

    let ident_string = ident.to_string();

    let token_stream = if let Data::Struct(data) = data {
        let fields = match &data.fields {
            Fields::Named(fields) => fields.named.iter(),
            _ => unimplemented!(),
        };

        let fields: Vec<&Field> = fields.collect();
        let debug_struct_params = fields.iter().map(|a| debug_struct_param(a));

        let generics_with_debug = add_trait_bounds(generics.clone(), fields.iter().cloned());
        let (impl_generics, ty_generics, where_clause) = generics_with_debug.split_for_impl();

        TokenStream::from(quote! {
            impl #impl_generics std::fmt::Debug for #ident #ty_generics #where_clause  {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.debug_struct(#ident_string)
                        #(#debug_struct_params)*
                        .finish()
            }}
        })
    } else {
        TokenStream::new()
    };

    //println!("{}", token_stream);
    token_stream
}

fn debug_struct_param(field: &Field) -> proc_macro2::TokenStream {
    let Field { ident, .. } = field;
    if let Some(ident) = ident {
        let ident_string = ident.to_string();

        let function = move |field: &Field, value: String| {
            let Field { ident, .. } = &field;
            let ident_string = ident.as_ref().unwrap().to_string();
            quote! {
                .field(#ident_string, &format_args!(#value, &self.#ident))
            }
        };
        let token_stream = debug_attrs(field, function);
        if let Some(token_stream) = token_stream {
            return token_stream;
        }
        quote! {
            .field(#ident_string, &self.#ident)
        }
    } else {
        proc_macro2::TokenStream::new()
    }
}

fn debug_attrs<'a, 'b>(
    field: &'a Field,
    function: impl Fn(&'a Field, String) -> proc_macro2::TokenStream,
) -> Option<proc_macro2::TokenStream> {
    let values: Vec<_> = field
        .attrs
        .iter()
        .filter_map(|a| parse_debug_attrs(a).map(|ident| function(field, ident)))
        .collect();
    if !values.is_empty() {
        Some(quote! {
            #(#values)*
        })
    } else {
        None
    }
}

fn parse_debug_attrs(attr: &syn::Attribute) -> Option<String> {
    if !attr.path().is_ident("debug") {
        return None;
    }
    if let syn::Meta::NameValue(meta) = &attr.meta {
        if let syn::Expr::Lit(value) = &meta.value {
            if let syn::Lit::Str(value) = &value.lit {
                let value = value.value();
                return Some(value);
            }
        }
    }
    None
}

fn add_trait_bounds<'a>(
    mut generics: syn::Generics,
    mut fields: impl Iterator<Item = &'a Field>,
) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            let ident = &type_param.ident;
            let is_phantom = parse_quote!(PhantomData<#ident>);
            if !fields.any(|field| field.ty == is_phantom) {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics
}
