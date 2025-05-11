use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let data = &input.data;

    let ident_string = ident.to_string();

    let token_stream = if let Data::Struct(data) = data {
        let fields = match &data.fields {
            Fields::Named(fields) => fields.named.iter(),
            _ => unimplemented!(),
        };

        let fields: Vec<&Field> = fields.collect();
        let debug_struct_params = fields.iter().map(|a| debug_struct_param(a));

        TokenStream::from(quote! {
            impl std::fmt::Debug for #ident {
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

fn debug_struct_param(Field { ident, .. }: &Field) -> proc_macro2::TokenStream {
    if let Some(ident) = ident {
        let ident_string = ident.to_string();
        quote! {
            .field(#ident_string, &self.#ident)
        }
    } else {
        proc_macro2::TokenStream::new()
    }
}
