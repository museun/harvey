#![recursion_limit = "128"]
#![allow(dead_code)]
extern crate proc_macro;

use heck::ShoutySnekCase;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{self, Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput, Ident, LitStr, Token};

#[derive(Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
struct Entry(Ident, String);

impl Parse for Entry {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let key: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let value: LitStr = input.parse()?;
        Ok(Entry(key, value.value()))
    }
}

struct Map(Ident, Vec<Entry>);

impl Parse for Map {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![=>]>()?;

        let parsed = Punctuated::<Entry, Token![,]>::parse_terminated(input)?;

        let map = parsed.into_iter().collect::<Vec<_>>();
        let set = map.iter().collect::<std::collections::HashSet<_>>();
        if set.len() != map.len() {
            panic!("duplicates found");
            // TODO say which duplicates
        }

        Ok(Self(ident, map))
    }
}

#[proc_macro]
pub fn phf_mapping(input: TokenStream) -> TokenStream {
    let Map(ident, map) = parse_macro_input!(input as Map);
    let phf = make_phf(map.iter().map(|Entry(_, k)| k).enumerate(), ident.clone());

    let enum_name = ident.clone();
    let variants = map
        .iter()
        .map(|Entry(var, _)| var.clone())
        .collect::<Vec<_>>();
    let variants2 = variants.clone();

    let tokens = map
        .iter()
        .map(|Entry(_, name)| name.clone())
        .collect::<Vec<_>>();

    let debug_vars = map
        .iter()
        .map(|Entry(k, v)| quote!(#k => #v))
        .collect::<Vec<_>>();

    let lookup = make_lookup(ident, enum_name.clone(), &variants2, &tokens);

    let expanded = quote! {
        #[allow(dead_code)]
        #[derive(Debug, Copy, Clone, PartialEq, Hash)]
        pub enum #enum_name {
            #(#variants,)*
        }

        impl std::fmt::Display for #enum_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use #enum_name::*;
                let s = match self {
                    #(#debug_vars,)*
                };
                write!(f, "{}", s)
            }
        }

        #lookup
        #phf
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(PerfectHash)]
pub fn perfect_hash(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let variants = match input.data {
        Data::Enum(DataEnum { variants, .. }) => {
            variants.into_iter().map(|d| d.ident).collect::<Vec<_>>()
        }
        _ => panic!("can only derive on enums"),
    };

    let ident = input.ident;
    let enum_name = ident.clone();

    let tokens = variants
        .clone()
        .iter()
        .map(|k| k.to_string().to_lowercase())
        .collect::<Vec<_>>();

    let phf = make_phf(tokens.iter().enumerate(), ident.clone());
    let lookup = make_lookup(ident, enum_name, &variants, &tokens);

    let expanded = quote! {
        #lookup
        #phf
    };

    TokenStream::from(expanded)
}

fn make_lookup(
    ident: Ident,
    enum_name: Ident,
    variants: &[Ident],
    tokens: &[String],
) -> proc_macro2::TokenStream {
    let tokens_name = Ident::new(
        &format!("{}_tokens", ident).TO_SHOUTY_SNEK_CASE(),
        ident.span(),
    );
    let tokens_len = variants.len();
    let tokens_hash = Ident::new(
        &format!("{}_derive_phf", ident).TO_SHOUTY_SNEK_CASE(),
        ident.span(),
    );

    let index = variants
        .iter()
        .enumerate()
        .map(|(i, _)| i)
        .collect::<Vec<_>>();

    quote! {
        impl #ident {
            #[inline]
            pub fn lookup(s: &str) -> Option<Self> {
                static #tokens_name: [&str; #tokens_len] = [#(#tokens),*];
                use #enum_name::*;
                let d = #tokens_hash.hash(s);
                if d >= #tokens_len || #tokens_name[d] != s {
                    return None
                }
                Some(match d {
                    #(#index => #variants,)*
                    _ => return None,
                })
            }
        }

        impl std::fmt::Display for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use #ident::*;
                match self {
                    #(#variants => write!(f, "{}", #tokens),)*
                }
            }
        }
    }
}

fn make_phf<I, K>(s: I, ident: Ident) -> proc_macro2::TokenStream
where
    K: AsRef<[u8]>,
    I: ExactSizeIterator<Item = (usize, K)> + Clone,
{
    use phf::PerfectHash;
    let phf = phf::generate_hash(s);

    let name_phf = Ident::new(&format!("{}_derive_phf", ident), ident.span());
    let phf_static = Ident::new(
        &format!("{}_derive_phf", ident).TO_SHOUTY_SNEK_CASE(),
        ident.span(),
    );

    let (left, left_len) = (phf.left(), phf.left().len());
    let (right, right_len) = (phf.right(), phf.right().len());
    let (graph, graph_len) = (phf.graph(), phf.graph().len());

    quote! {
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        struct #name_phf {
            left: [phf::Scalar; #left_len],
            right: [phf::Scalar; #right_len],
            graph: [phf::Scalar; #graph_len],
        }

        use phf::PerfectHash as _;
        impl phf::PerfectHash for #name_phf {
            fn left(&self) -> &[phf::Scalar] { &self.left }
            fn right(&self) -> &[phf::Scalar] { &self.right }
            fn graph(&self) -> &[phf::Scalar] { &self.graph }
        }

        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        static #phf_static: #name_phf = #name_phf {
            left: [#(#left,)*],
            right: [#(#right,)*],
            graph: [#(#graph,)*],
        };
    }
}
