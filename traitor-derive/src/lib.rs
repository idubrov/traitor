#![recursion_limit = "128"]
extern crate proc_macro;

use quote::quote;
use syn::spanned::Spanned;
use syn::visit_mut::visit_type_mut;
use syn::{
    parse_macro_input, parse_quote, FnArg, Ident, ItemTrait, Lifetime, ReturnType, TraitItem,
    TraitItemMethod, Type,
};

type Error = syn::parse::Error;

#[proc_macro_attribute]
pub fn shadow(
    _args: proc_macro::TokenStream,
    item_tokens: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item_tokens_clone = item_tokens.clone();
    let item_tokens = proc_macro2::TokenStream::from(item_tokens);
    let shadow = parse_macro_input!(item_tokens_clone as ItemTrait);

    let shadow_tokens = match generate_shadow(shadow) {
        Err(err) => err.to_compile_error().into(),
        Ok(tokens) => tokens,
    };

    let output = quote! {
      #item_tokens

      #shadow_tokens
    };
    output.into()
}

fn generate_shadow(mut shadow_trait: ItemTrait) -> Result<proc_macro2::TokenStream, Error> {
    let target_ident = shadow_trait.ident.clone();
    let shadow_ident = Ident::new(&format!("{}Shadow", target_ident), target_ident.span());
    let details_ident = Ident::new(&format!("{}ShadowInfo", target_ident), target_ident.span());
    let bind_box_ident = Ident::new(&format!("bind_{}_box", target_ident), target_ident.span());
    let visibility = shadow_trait.vis.clone();

    shadow_trait.ident = shadow_ident.clone();

    // Mark as unsafe! our "shadows" are sketchy and only work for limited cases.
    shadow_trait.unsafety = Some(syn::token::Unsafe {
        span: shadow_trait.ident.span(),
    });

    if !shadow_trait.supertraits.is_empty() {
        return Err(Error::new(
            shadow_trait.supertraits.span(),
            "supertraits are not supported yet!",
        ));
    }

    let mut funcs = Vec::new();
    for item in &mut shadow_trait.items {
        if let TraitItem::Method(ref mut method) = item {
            let (lifetime, mutability) = self_lifetime(method)?.clone();
            // Replace the first argument
            let first_mut = method.sig.decl.inputs.iter_mut().next().unwrap();

            *first_mut = parse_quote! { data: &#lifetime #mutability Self::Data };

            // Collect function arguments we care about
            let mut arg_types = Vec::new();
            for arg in &method.sig.decl.inputs {
                if let FnArg::Captured(arg) = arg {
                    let mut ty = arg.ty.clone();
                    visit_type_mut(&mut EraseLifetimes {}, &mut ty);
                    arg_types.push(ty);
                } else {
                    return Err(Error::new(arg.span(), "argument not supported"));
                }
            }

            // Generate layout info for the function
            let mut ret_type: Type = match method.sig.decl.output {
                ReturnType::Default => parse_quote! { () },
                ReturnType::Type(_, ref ty) => ty.as_ref().clone(),
            };
            visit_type_mut(&mut EraseLifetimes {}, &mut ret_type);

            let func_ident = &method.sig.ident;
            funcs.push(quote! {
              traitor::details::ShadowLayout {
                ret_slots: ret_slots::<#ret_type>(),
                arg_slots: #(slots::<#arg_types>())+*,
                func_ptr: <S as #shadow_ident>::#func_ident as *const u8,
              }
            });

            // Append &Self as a last argument -- this is our extended metadata. Use the same lifetime as
            // the original `&self` parameter, so we can borrow from the metadata (we guarantee that
            // metadata lives longer than any borrow of the data itself).
            method
                .sig
                .decl
                .inputs
                .push(parse_quote! { meta: &#lifetime Self });
        }
    }

    // Append `Data` associated type
    // FIXME: bounds for supertraits: type Data: Sized + std::fmt::Debug + ...;
    shadow_trait.items.push(parse_quote! {
        type Data: Sized;
    });

    let details_struct = quote! {
        #visibility struct #details_ident<'meta, S>
            where
                S: #shadow_ident + 'meta,
        {
            inner: S,
            _phantom: std::marker::PhantomData<&'meta S>,
        }
    };

    let details_impl = quote! {
        impl<'meta, S> #details_ident<'meta, S>
        where
            S: #shadow_ident + 'meta,
        {
            pub fn new(meta: S) -> Self {
                Self {
                    inner: meta,
                    _phantom: Default::default(),
                }
            }
        }
    };

    // FIXME: we should handle floats / doubles!
    let details_binding = quote! {
        impl<'meta, S> traitor::details::InternalBindingInfo for #details_ident<'meta, S>
          where
            S: #shadow_ident + 'meta,
        {
            type Data = <S as #shadow_ident>::Data;
            type Shadow = S;
            type Target = #target_ident;

            fn layout() -> traitor::details::LayoutInfo {
                const US: usize = std::mem::size_of::<usize>();
                fn slots<T>() -> usize {
                    match (std::mem::size_of::<T>() + US - 1) / US {
                        v if v <= 2 => v,
                        // uses pointer instead which is one slot
                        _ => 1,
                    }
                }
                fn ret_slots<T>() -> usize {
                    match (std::mem::size_of::<T>() + US - 1) / US {
                        // uses pointer to return value, pointer is passed by caller
                        v if v > 2 => 1,
                        // uses registers, no slots are taken
                        _ => 0,
                    }
                }
                traitor::details::LayoutInfo {
                    shadow: vec![
                        #(#funcs),*
                    ],
                  other: vec![
                //            <traitor::details::VTableInfo as traitor::details::VTableInfoTrait<
                //              Self::Data,
                //              std::fmt::Debug,
                //            >>::VTABLE_FUNCS,
                  ],
                }
            }

            fn into_shadow(self) -> S {
                self.inner
            }
        }

    };

    let bind_box = quote! {
        #visibility fn #bind_box_ident<'b, D>(
            data: Box<D>,
            binder: &'b traitor::Binder<D, #target_ident>
        ) -> Box<#target_ident + 'b> {
            unsafe {
                let raw = Box::into_raw(data);
                let bound = binder.bind_mut(std::mem::transmute::<*mut D, &mut D>(raw));
                Box::from_raw(bound)
            }
        }
    };

    let result = quote! {
        #shadow_trait

        #details_struct
        #details_impl
        #details_binding
        #bind_box
    };
    Ok(result)
}

fn self_lifetime(method: &TraitItemMethod) -> Result<(Lifetime, Option<syn::token::Mut>), Error> {
    match method.sig.decl.inputs.first().as_ref().map(|v| v.value()) {
        Some(FnArg::SelfRef(self_ref)) => {
            let lifetime = self_ref.lifetime.as_ref().ok_or_else(|| {
                Error::new(
                    self_ref.self_token.span,
                    "&self argument must have an explicit lifetime!",
                )
            })?;

            Ok((lifetime.clone(), self_ref.mutability.clone()))
        }
        _ => {
            return Err(Error::new(
                method.sig.ident.span(),
                "function must be object-safe and have `&self` as a first argument!",
            ));
        }
    }
}

struct EraseLifetimes {}

impl syn::visit_mut::VisitMut for EraseLifetimes {
    fn visit_type_reference_mut(&mut self, tr: &mut syn::TypeReference) {
        tr.lifetime = None;
        syn::visit_mut::visit_type_reference_mut(self, tr)
    }

    fn visit_angle_bracketed_generic_arguments_mut(
        &mut self,
        list: &mut syn::AngleBracketedGenericArguments,
    ) {
        let old = std::mem::replace(&mut list.args, Default::default());
        for item in old {
            if let syn::GenericArgument::Lifetime(_) = item {
                // drop all lifetimes
            } else {
                list.args.push(item);
            }
        }
        syn::visit_mut::visit_angle_bracketed_generic_arguments_mut(self, list)
    }
}
