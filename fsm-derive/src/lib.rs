#![recursion_limit = "128"]
#![allow(dead_code)]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput, Fields};

#[proc_macro_derive(State)]
pub fn state(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = match input.data {
        Data::Enum(DataEnum { variants, .. }) => variants
            .into_iter()
            .map(|d| {
                match d.fields {
                    Fields::Unit => {}
                    _ => panic!("can only derive on unit enums"),
                };
                d.ident
            })
            .collect::<Vec<_>>(),
        _ => panic!("can only derive on enums"),
    };

    let from = variants.clone().into_iter().enumerate().map(|(i, c)| {
        let i = i as u8;
        quote! { #i => #c }
    });

    let into = variants.clone().into_iter().enumerate().map(|(i, c)| {
        let i = i as u8;
        quote! { #c => #i }
    });

    let ident = input.ident;
    let len = variants.len() as u8;

    let flip = if len == 2 {
        quote! {
            impl fsm::StateFlip for #ident { }
        }
    } else {
        quote! {}
    };

    let expanded = quote! {
        impl fsm::State for #ident {
            const MAX: u8 = #len;
        }

        impl std::convert::TryFrom<u8> for #ident {
            type Error = ();
            fn try_from(d: u8) -> std::result::Result<Self, Self::Error> {
                use #ident::*;
                Ok(match d {
                    #(#from,)*
                    _ => return Err(()),
                })
            }
        }

        impl From<#ident> for u8 {
            fn from(state: #ident) -> Self {
                use #ident::*;
                match state {
                    #(#into),*
                }
            }
        }

        #flip
    };

    TokenStream::from(expanded)
}

// #[derive(Copy, Clone, PartialEq)]
// enum State {
//     Integral = 0,
//     Radix,
//     Exponent,
// }
// impl From<u8> for State {
//     fn from(d: u8) -> Self {
//         match d {
//             0 => State::Integral,
//             1 => State::Radix,
//             2 => State::Exponent,
//             _ => unreachable!(),
//         }
//     }
// }
// // impl From<State> for u8 {
// //     fn from(state: State) -> Self {
// //         match state {
// //             State::Integral => 0,
// //             State::Radix => 1,
// //             State::Exponent => 2,
// //         }
// //     }
// // }
// impl StateAdvance for State {
//     const MAX: u8 = 3;
// }
