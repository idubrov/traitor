//! Traitor is a library that allows to generate trait objects by binding any pre-existing data type
//! to any kind of meta-information.
//!
//! It uses some assumptions about implementation details, so is not 100% bulletproof.
//!
//! Let's say we want to generate [`Colored`] trait object for an instance of [`String`] by
//! attaching some "color" information to it.
//!
//! ```rust
//! #[derive(Clone, Debug, PartialEq)]
//! enum Color {
//!     Red,
//!     Green,
//!     Blue,
//!     /// It's possible to attach any kind of data!
//!     Other(String),
//! }
//!
//! /// Marking with a `traitor::shadow` generates necessary "glue" code.
//! #[traitor::shadow]
//! trait Colored {
//!     /// Any object-safe function is okay. However, currently it is required that `&self` is
//!     /// marked with an explicit lifetime.
//!     fn color<'data>(&'data self) -> Color;
//! }
//! 
//! /// `ColoredShadow` is the shadow trait generated by proc macro. It's marked by `unsafe` because
//! /// the whole mechanism is sketchy and only works in limited cases.
//! unsafe impl ColoredShadow for Color {
//!     /// Associated type indicates which data type this "shadow" can attach to.
//!     type Data = String;
//! 
//!     /// In the shadow trait, each function is the same as in the original trait with the two
//!     /// differences:
//!     /// 1. First argument becomes reference to the data instead of `&self`. The value passed
//!     /// here is `&self` reference on which [`Colored::color`] trait object function is invoked.
//!     /// 2. An additional argument is added at the end. This argument is the reference to the
//!     /// metadata we pre-allocated. It's of the type `Self`, therefore this trait is supposed to
//!     /// be implemented on the metadata type ([`Color`] in our case).
//!     fn color<'data>(_data: &'data String, meta: &'data Self) -> Color {
//!         // Juts return a clone of ourselves!
//!         meta.clone()
//!     }
//! }
//!
//! fn main() {
//!     let traitor = traitor::Traitor::new();
//! 
//!     // `declare` function takes metadata wrapped into the `[*]ShadowInfo` struct (which is
//!     // another item generated by `shadow` proc macro) and returns a "binder". Each metadata is
//!     // moved to an internal arena managed by the [`Traitor`] instance. "binder" provides a
//!     // `bind` function which takes a reference to the data and returns a trait object.
//!     // "Declaring" a binding allocates memory for internal data structures and for the
//!     // attached "metadata".
//!     let binder = traitor.declare(ColoredShadowInfo::new(Color::Other("turquoise".into())));
//!     let data = "hello".to_string();
//!
//!     // "Binding" does not allocate anything
//!     let bound: &Colored = binder.bind(&data);
//!     assert_eq!(Color::Other("turquoise".into()), bound.color());
//! }
//! ```
use crate::details::ShadowLayout;
use dynasmrt::DynasmApi;
use std::marker::PhantomData;

pub use traitor_derive::shadow;

/// This module is public so it can be used by code generated by proc macro.
#[doc(hidden)]
pub mod details {
    /// Used for discovering contents of vtables
    #[doc(hidden)]
    pub struct VTableInfo;

    #[doc(hidden)]
    pub unsafe trait VTableInfoTrait<S, T: ?Sized> {
        /// Pointers to vtable functions, in order they appear in vtable.
        /// WARNING: ordering of functions in trait object is an implementation details!
        const VTABLE_FUNCS: &'static [*const u8];
    }

    /// Layout information of the target trait
    #[doc(hidden)]
    pub struct LayoutInfo {
        /// vtable layout for the primary trait we are targeting.
        pub shadow: Vec<ShadowLayout>,
        /// vtable functions for other traits used as bound.
        pub other: Vec<&'static [*const u8]>,
    }

    #[doc(hidden)]
    pub struct ShadowLayout {
        /// How many "slots" (registers) are used for passing arguments.
        pub ret_slots: usize,
        /// How many "slots" (registers) are used for passing return value.
        pub arg_slots: usize,
        /// Pointer to the implementation function.
        pub func_ptr: *const u8,
    }

    /// Internal trait that defines vtable functions layouts
    #[doc(hidden)]
    pub trait InternalBindingInfo {
        type Data: Sized;
        type Shadow: Sized;
        /// Target trait
        type Target: ?Sized;

        fn layout() -> LayoutInfo;
        fn into_shadow(self) -> Self::Shadow;
    }
}

/// Layout info for commonly used traits (we only support these traits as super-traits).
#[doc(hidden)]
mod common {
    use std::fmt::Debug;

    unsafe impl<T: Debug> crate::details::VTableInfoTrait<T, Debug> for crate::details::VTableInfo {
        // FIXME: grab funcs from vtable instead, we only need to know the count!...
        const VTABLE_FUNCS: &'static [*const u8] = &[<T as Debug>::fmt as *const u8];
    }
}

struct VTable {
    vtable: Vec<*const u8>,
    #[allow(unused)]
    buffer: dynasmrt::ExecutableBuffer,
}

/// Every virtual table starts with these entries.
/// We use arbitrary empty trait which is implemented by every type in our system
/// to capture these entries.
#[repr(C)]
#[derive(Clone)]
struct VirtualTableHeader {
    destructor: fn(*mut ()),
    size: usize,
    align: usize,
}

/// Trait object runtime representation
#[repr(C)]
struct TraitObject {
    pub data: *mut (),
    pub vtable: *const (),
}

/// Trait used to capture destructor, align and size of our object
trait Whatever {}
impl<T> Whatever for T {}

pub struct Traitor {
    vtables: typed_arena::Arena<VTable>,
    shadows: typed_arena::Arena<Box<Whatever>>,
}

impl Traitor {
    pub fn new() -> Self {
        Traitor {
            vtables: typed_arena::Arena::new(),
            shadows: typed_arena::Arena::new(),
        }
    }
}

/// Binder allows converting provided reference to the data into the trait object by attaching
/// vtable generated via [`ShadowCaster::declare`] call.
#[repr(transparent)]
pub struct Binder<Data, Trait: ?Sized>(VTable, PhantomData<(Data, Trait)>);

impl<'sc, D, R: ?Sized> PartialEq for Binder<D, R> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self as *const Self, other)
    }
}

impl<'sc, Data, Trait: ?Sized> Eq for Binder<Data, Trait> {}

impl<'sc, Data, Trait: ?Sized> Binder<Data, Trait> {
    /// Bind given reference to the data and return a trait object
    pub fn bind<'output, 'input>(&'sc self, data: &'input Data) -> &'output Trait
    where
        'sc: 'output,
        'input: 'output,
    {
        let obj = TraitObject {
            data: data as *const Data as *mut (),
            vtable: self.0.vtable.as_ptr() as *const (),
        };
        unsafe { *(&obj as *const TraitObject as *const &Trait) }
    }

    /// Bind given mutable reference to the data and return a trait object
    pub fn bind_mut<'output, 'input>(&'sc self, data: &'input mut Data) -> &'output mut Trait
    where
        'sc: 'output,
        'input: 'output,
    {
        let mut obj = TraitObject {
            data: data as *mut Data as *mut (),
            vtable: self.0.vtable.as_ptr() as *const (),
        };
        unsafe { *(&mut obj as *mut TraitObject as *mut &mut Trait) }
    }
}

impl Traitor {
    pub fn declare<'sc, B>(&'sc self, binder: B) -> &'sc Binder<B::Data, B::Target>
    where
        B: crate::details::InternalBindingInfo,
        B::Shadow: 'sc,
    {
        unsafe {
            // Move shadow information into heap
            let shadow: Box<Whatever> = Box::new(binder.into_shadow());
            // Move box ignoring lifetimes; we require that "shadow" lives at least as `&self`, so
            // this should be fine?
            let shadow: &mut Box<Whatever> = self
                .shadows
                .alloc(std::mem::transmute::<Box<Whatever + 'sc>, Box<Whatever>>(
                    shadow,
                ));
            let shadow_ptr = shadow.as_ref() as *const Whatever as *const B::Shadow;

            // Build vtable
            let layout = B::layout();

            let other_funcs_cnt: usize = layout.other.iter().map(|o| o.len()).sum();
            // 3 is for destructor pointer, size and align, which are at the top of the vtable
            let vtable_len = 3 + layout.shadow.len() + other_funcs_cnt;
            let mut vtable: Vec<*const u8> = Vec::with_capacity(vtable_len);

            // Capture destructor, size and align for our data type from other vtable
            let data: *const B::Data = std::ptr::null();
            let whatever = data as *const dyn Whatever;
            let whatever_obj = std::mem::transmute::<*const dyn Whatever, TraitObject>(whatever);
            vtable.extend_from_slice(std::slice::from_raw_parts(
                whatever_obj.vtable as *const *const u8,
                3,
            ));

            // Compile trampoline functions and put them to the table
            let (buffer, funcs) = Self::compile_funcs(&layout.shadow, shadow_ptr as *const ());
            for offset in funcs {
                let pointer = buffer.ptr(offset);
                vtable.push(pointer);
            }

            for other in layout.other {
                vtable.extend_from_slice(other);
            }
            assert_eq!(vtable.len(), vtable_len);

            // Keep vtable and buffer with compiled code in our arena!
            let vtable = VTable { vtable, buffer };
            let vtable: &VTable = self.vtables.alloc(vtable);

            std::mem::transmute::<&VTable, &Binder<B::Data, B::Target>>(vtable)
        }
    }

    fn compile_funcs(
        layout: &[ShadowLayout],
        shadow_ptr: *const (),
    ) -> (dynasmrt::ExecutableBuffer, Vec<dynasmrt::AssemblyOffset>) {
        // Compile trampoline functions
        let mut ops = dynasmrt::x64::Assembler::new().unwrap();

        let mut func_offsets = Vec::new();
        for info in layout {
            func_offsets.push(ops.offset());

            // System V calling convention uses: rdi, rsi, rdx, rcx, r8, r9
            let slots = info.arg_slots + info.ret_slots;
            match slots {
                1 => {
                    // mov rsi, QWORD shadow_ptr as _
                    ops.push_u16(0xbe48);
                    ops.push_u64(shadow_ptr as u64);
                }
                2 => {
                    // mov rdx, QWORD shadow_ptr as _
                    ops.push_u16(0xba48);
                    ops.push_u64(shadow_ptr as u64);
                }
                3 => {
                    // mov rcx, QWORD shadow_ptr as _
                    ops.push_u16(0xb948);
                    ops.push_u64(shadow_ptr as u64);
                }
                4 => {
                    // mov r8, QWORD shadow_ptr as _
                    ops.push_u16(0xb849);
                    ops.push_u64(shadow_ptr as u64);
                }
                _ => unimplemented!("argument count not supported"),
            }

            // mov rax, QWORD info.func_ptr as _
            ops.push(0x48);
            ops.push(0xB8);
            ops.push_u64(info.func_ptr as u64);
            // align stack
            // sub rsp, BYTE 0x8
            ops.push_u32(0x08ec8348);
            // call rax
            ops.push_u16(0xd0ff);
            // add rsp, BYTE 0x8
            ops.push_u32(0x08c48348);
            // ret
            ops.push(0xc3);
        }

        (ops.finalize().unwrap(), func_offsets)
    }
}
