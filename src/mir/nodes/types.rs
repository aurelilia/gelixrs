/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 2/3/20 3:04 AM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::{
    fmt::{Debug, Display, Error, Formatter},
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::{
    ast,
    mir::{
        generator::builder::Context,
        get_iface_impls,
        nodes::{ADTType, CastType, Function, Variable, ADT},
        MutRc,
    },
};

/// All types in Gelix.
/// For all types that can have generic parameters, these parameters
/// are not part of the type. They are erased when the type
/// is first created from the prototype (see prototypes.rs).
#[derive(Debug, Clone, EnumAsGetters, EnumIsA, EnumIntoGetters)]
pub enum Type {
    /// The Any type is considered equal to all other types.
    /// This is used as the type of 'return' or 'break' expressions,
    /// since they change control flow, preventing their value
    /// from ever being used.
    /// Simply an empty struct in IR.
    Any,
    /// Same behavior as Any, but is equal to no type.
    /// Used for expressions that cannot return any reasonable value,
    /// like an empty block or if without else.
    None,
    /// Simply a boolean type (i1; bit). Required mainly by
    /// if and for expressions.
    Bool,

    /// Signed integer types from 8 to 64 bit width.
    I8,
    I16,
    I32,
    I64,

    /// Unsigned integer types from 8 to 64 bit width.
    U8,
    U16,
    U32,
    U64,

    /// Floating-point numbers with 32 and 64 bit width.
    F32,
    F64,

    /// A function. Note that this is not the signature of a function,
    /// but simply the function directly. Because of this, calling this
    /// a type is kinda questionable, as it is also a value...
    Function(MutRc<Function>),

    /// A closure. Essentially a function pointer together with
    /// another pointer to a struct containing captured data.
    /// Functions can be cast to closures using Expr::ConstructClosure.
    Closure(Rc<ClosureType>),

    /// A simple struct holding all the variables captured by a closure.
    /// Only used as the receiver on closure functions, this anywhere
    /// else is undefined behavior.
    ClosureCaptured(Rc<Vec<Rc<Variable>>>),

    /// An ADT. Used for all data types that are stored inside an
    /// underlying struct in IR that are not directly callable
    /// (looking at you, closures.)
    Adt(MutRc<ADT>),

    /// A weak reference to an ADT.
    Weak(MutRc<ADT>),

    /// A direct value of an ADT.
    Value(MutRc<ADT>),

    /// A pointer to a value that is usually a value (primitives).
    /// This is mainly for C interop.
    Pointer(Box<Type>),

    /// A type.
    /// This is used mainly for accessing static members of types,
    /// and constructors.
    Type(Box<Type>),
}

impl Type {
    /// A list of all primitive types that are not defined in any gelix code,
    /// but are instead indirectly globally defined.
    pub fn primitives() -> [Type; 12] {
        [
            Type::None,
            Type::Bool,
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::F32,
            Type::F64,
        ]
    }

    pub fn is_primitive(&self) -> bool {
        self.is_none() || self.is_number()
    }

    /// Returns the context of the type, if any.
    pub fn context(&self) -> Option<Context> {
        Some(match self {
            Type::Function(func) => func.borrow().context.clone(),
            Type::Adt(adt) => adt.borrow().context.clone(),
            _ => return None,
        })
    }

    /// Is this type a number?
    pub fn is_number(&self) -> bool {
        self.is_int() || self.is_float()
    }

    /// Is this type an integer?
    pub fn is_int(&self) -> bool {
        self.is_signed_int() || self.is_unsigned_int() || self.is_bool()
    }

    /// Is this type a signed integer?
    pub fn is_signed_int(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => true,
            _ => false,
        }
    }

    /// Is this type an unsigned integer?
    pub fn is_unsigned_int(&self) -> bool {
        match self {
            Type::U8 | Type::U16 | Type::U32 | Type::U64 => true,
            _ => false,
        }
    }

    /// Is this type a floating-point number?
    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 => true,
            _ => false,
        }
    }

    /// Is this type a pointer at machine level?
    pub fn is_ptr(&self) -> bool {
        self.is_adt() || self.is_pointer() || self.is_closure() || self.is_weak()
    }

    /// Can this type be assigned to variables?
    /// True for everything but types, as they are static.
    pub fn is_assignable(&self) -> bool {
        !self.is_type()
    }

    /// Can this type 'escape' the function it is in?
    /// True for everything except weak references.
    pub fn can_escape(&self) -> bool {
        !self.is_weak()
    }

    pub fn to_adt(&self) -> &MutRc<ADT> {
        match self {
            Type::Adt(adt) | Type::Weak(adt) | Type::Value(adt) => adt,
            _ => panic!(),
        }
    }

    pub fn try_adt(&self) -> Option<&MutRc<ADT>> {
        match self {
            Type::Adt(adt) | Type::Weak(adt) | Type::Value(adt) => Some(adt),
            _ => None,
        }
    }

    pub fn to_strong(&self) -> Type {
        match self {
            Type::Adt(adt) | Type::Weak(adt) | Type::Value(adt) => Type::Adt(Rc::clone(&adt)),
            _ => self.clone()
        }
    }

    /// Returns a list of available constructors, should self be a
    /// static type access.
    /// TODO: Copying the list of constructors is not great for performance
    pub fn get_constructors(&self) -> Option<Vec<Rc<Variable>>> {
        // Thanks, no box pattern matching!
        if let Type::Type(ty) = self {
            if let Type::Adt(ty) = &**ty {
                Some(ty.borrow().constructors.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn has_marker(&self, marker: &ast::Type) -> bool {
        if !marker.is_ident() {
            return false;
        }
        let lexeme = &marker.get_token().lexeme;
        match &lexeme[..] {
            "Primitive" => self.is_primitive(),
            "Number" => self.is_number(),
            "Integer" => self.is_int(),
            "SignedInt" => self.is_signed_int(),
            "UnsignedInt" => self.is_unsigned_int(),
            "Float" => self.is_float(),

            "IsPointer" => self.is_ptr(),
            "IsValue" => !self.is_ptr(),

            _ if self.is_adt() => {
                let adt = self.as_adt().borrow();
                match (&lexeme[..], &adt.ty) {
                    ("Class", ADTType::Class { .. })
                    | ("Interface", ADTType::Interface { .. })
                    | ("Enum", ADTType::Enum { .. })
                    | ("EnumCase", ADTType::EnumCase { .. }) => true,

                    ("ExtClass", ADTType::Class { external, .. }) => *external,

                    _ => false,
                }
            }

            // Marker does not exist
            _ => false,
        }
    }

    pub fn maybe_simplify(self) -> Self {
        match self {
            Type::Pointer(inner) if inner.is_value() => Type::Adt(inner.into_value()),
            _ => self,
        }
    }

    /// first_call indicates if this is a recursive call or not to prevent SO
    pub fn can_cast_to(
        &self,
        other: &Type,
        first_call: bool,
    ) -> Option<(Option<CastType>, Option<(CastType, Type)>)> {
        match (self, other) {
            _ if self == other => return Some((None, None)),

            // SR to WR cast
            (Type::Adt(adt), Type::Weak(weak)) if Rc::ptr_eq(adt, weak) => {
                return Some((Some(CastType::SRtoWR), None))
            }

            // SR/WR to DV cast
            (Type::Adt(adt), Type::Value(value)) | (Type::Weak(adt), Type::Value(value))
                if Rc::ptr_eq(adt, value) =>
            {
                return Some((Some(CastType::ToDV), None))
            }

            // DV to WR cast
            (Type::Value(adt), Type::Weak(weak)) if Rc::ptr_eq(adt, weak) => {
                return Some((Some(CastType::DVtoWR), None))
            }

            (Type::Adt(adt), Type::Adt(other)) => {
                match &adt.borrow().ty {
                    // Enum case to enum cast
                    ADTType::EnumCase { parent, .. } if Rc::ptr_eq(parent, other) => {
                        return Some((Some(CastType::Bitcast), None))
                    }

                    _ => (),
                }
            }

            // Number cast
            _ if self.is_number() && other.is_int() => return Some((Some(CastType::ToInt), None)),
            _ if self.is_number() && other.is_float() => {
                return Some((Some(CastType::ToFloat), None))
            }

            _ => (),
        }

        // Interface cast
        if let (Some(adt), Some(impls)) = (other.try_adt(), get_iface_impls(&self)) {
            if impls.borrow().interfaces.get(&Type::Adt(Rc::clone(adt))).is_some() {
                return Some((Some(CastType::ToIface), None));
            }
        }

        // TODO: Rather ugly...
        match self {
            _ if !first_call => None,

            Type::Adt(adt) => Type::Weak(Rc::clone(adt))
                .can_cast_to(&other, false)
                .map(|c| (c.0, Some((CastType::SRtoWR, Type::Weak(Rc::clone(adt))))))
                .or_else(|| {
                    Type::Value(Rc::clone(adt))
                        .can_cast_to(&other, false)
                        .map(|c| (c.0, Some((CastType::ToDV, Type::Value(Rc::clone(adt))))))
                }),

            Type::Weak(adt) => Type::Value(Rc::clone(adt))
                .can_cast_to(&other, false)
                .map(|c| (c.0, Some((CastType::ToDV, Type::Value(Rc::clone(adt)))))),

            Type::Value(adt) => Type::Weak(Rc::clone(adt))
                .can_cast_to(&other, false)
                .map(|c| (c.0, Some((CastType::DVtoWR, Type::Weak(Rc::clone(adt)))))),

            _ => None,
        }
    }

    pub fn display_full(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Type::Function(func) => write!(f, "{}", func.borrow()),
            Type::Adt(adt) => write!(f, "{}", adt.borrow()),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::None
    }
}

impl PartialEq for Type {
    fn eq(&self, o: &Self) -> bool {
        if let Type::Any = o {
            return true;
        }

        match self {
            Type::Function(f) => {
                if let Type::Function(o) = o {
                    Rc::ptr_eq(f, o)
                } else {
                    false
                }
            }

            Type::Closure(c) => {
                if let Type::Closure(o) = o {
                    c.parameters == o.parameters && c.ret_type == o.ret_type
                } else {
                    false
                }
            }

            Type::Adt(c) => {
                if let Type::Adt(o) = o {
                    Rc::ptr_eq(c, o)
                } else {
                    false
                }
            }

            Type::Value(t) => {
                if let Type::Value(o) = o {
                    Rc::ptr_eq(t, o)
                } else {
                    false
                }
            }

            Type::Weak(t) => {
                if let Type::Weak(o) = o {
                    Rc::ptr_eq(t, o)
                } else {
                    false
                }
            }

            Type::Pointer(t) => {
                if let Type::Pointer(o) = o {
                    t == o
                } else {
                    false
                }
            }

            Type::Type(t) => {
                if let Type::Type(o) = o {
                    t == o
                } else {
                    false
                }
            }

            Type::Any => true,

            _ => std::mem::discriminant(self) == std::mem::discriminant(o),
        }
    }
}

impl Eq for Type {}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Function(v) => v.borrow().name.hash(state),
            Type::Adt(v) | Type::Value(v) => v.borrow().name.hash(state),
            Type::Pointer(v) | Type::Type(v) => v.hash(state),
            _ => std::mem::discriminant(self).hash(state),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Type::Function(func) => write!(f, "{}", func.borrow().to_closure_type()),
            Type::Closure(closure) => write!(f, "{}", closure),
            Type::Adt(adt) => write!(f, "{}", adt.borrow().name),
            Type::Value(inner) => write!(f, "^{}", inner.borrow().name),
            Type::Weak(inner) => write!(f, "&{}", inner.borrow().name),
            Type::Pointer(inner) => write!(f, "*{}", inner),
            Type::Type(ty) => match **ty {
                Type::Function(_) => write!(f, "Function"),
                Type::Closure(_) => write!(f, "Closure"),
                Type::Adt(_) => write!(f, "ADT"),
                _ => write!(f, "{:?}", self),
            },
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug)]
pub struct ClosureType {
    pub parameters: Vec<Type>,
    pub ret_type: Type,
}

impl Display for ClosureType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "(")?;
        if !self.parameters.is_empty() {
            let mut p_iter = self.parameters.iter();
            write!(f, "{}", p_iter.next().unwrap())?;
            for ty in p_iter {
                write!(f, ", {}", ty)?;
            }
        }
        write!(f, "): {}", self.ret_type)
    }
}
