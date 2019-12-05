/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/4/19 9:55 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use indexmap::IndexMap;

use crate::ast::expression::Expression as ASTExpr;
use crate::mir::MutRc;
use crate::mir::nodes::{Expression, InterfacePrototype, Type};

/// A full class including all members and methods.
/// Members are ordered, as the class is represented as a struct in IR;
/// structs in IR only have indices for members, not names.
#[derive(Debug, Default)]
pub struct Class {
    /// The name of the class.
    pub name: Rc<String>,
    /// All class members.
    pub members: IndexMap<Rc<String>, Rc<ClassMember>>,
    /// All class methods. Inserted as "doThing", not "Class-doThing".
    pub methods: HashMap<Rc<String>, Rc<Variable>>,
    /// An internal function that creates an instance of the class
    /// and populates all fields with a user-given default value.
    /// When the user wants to create an instance by calling a constructor,
    /// this function is called first, followed by one of the constructor methods.
    pub instantiator: Rc<Variable>,
    /// All constructors of the class. They are simply methods
    /// with special constraints to enforce safety.
    /// Only call on instances produced by the instantiator function.
    pub constructors: Vec<Rc<Variable>>,
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "class {} {{", self.name)?;
        for (name, member) in self.members.iter() {
            writeln!(
                f,
                "    {} {}: {}",
                if member.mutable { "var" } else { "val" },
                name,
                member.type_
            )?;
        }
        writeln!(f, "}}")
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

/// A member of a class.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassMember {
    pub mutable: bool,
    pub type_: Type,
    /// This is the index used for StructGet and StructSet
    /// expressions. This is needed due to the IR only using indices for members
    pub index: u32,
}

/// An interface consisting of methods a type can implement.
#[derive(Debug, Default)]
pub struct Interface {
    /// The name of the interface. Generally the name given by the user;
    /// should the interface have been created from a prototype with generic parameters,
    /// its name is $Name<$Param1,$Param2>
    pub name: Rc<String>,
    /// A map of all methods.
    /// Indexed due to the vtable in IR being a struct.
    pub methods: IndexMap<Rc<String>, IFaceMethod>,
    /// The prototype this interface was built from, if any.
    /// Only used for some intrinsics.
    pub proto: Option<MutRc<InterfacePrototype>>,
}

impl Hash for Interface {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl PartialEq for Interface {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// An implementation of an interface.
#[derive(Debug)]
pub struct IFaceImpl {
    pub implementor: Type,
    pub iface: MutRc<Interface>,
    pub methods: IndexMap<Rc<String>, Rc<Variable>>,
}

impl PartialEq for IFaceImpl {
    fn eq(&self, other: &Self) -> bool {
        self.implementor == other.implementor && Rc::ptr_eq(&self.iface, &other.iface)
    }
}

impl Eq for IFaceImpl {}

impl Hash for IFaceImpl {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.iface.borrow().hash(state)
    }
}

/// A struct representing all interfaces implemented by a type.
/// A simple map of interfaces is not enough, as it does not
/// prevent naming collisions.
#[derive(Debug)]
pub struct IFaceImpls {
    pub implementor: Type,
    pub interfaces: HashSet<IFaceImpl>,
    pub methods: HashMap<Rc<String>, Rc<Variable>>,
}

impl PartialEq for IFaceImpls {
    fn eq(&self, other: &Self) -> bool {
        self.implementor == other.implementor
    }
}

impl Hash for IFaceImpls {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.implementor.hash(state)
    }
}

/// A method inside an interface.
/// The default implementation is left in AST state so that it can be compiled
/// individually on concrete implementations if needed.
#[derive(Debug)]
pub struct IFaceMethod {
    pub name: Rc<String>,
    pub parameters: Vec<Type>,
    pub ret_type: Type,
    pub default_impl: Option<ASTExpr>,
}

/// A function.
#[derive(Debug, Default)]
pub struct Function {
    /// The name of the function, with its module before it ($mod:$func)
    /// The only functions with no name change are external functions
    pub name: String,
    /// All parameters needed to call this function.
    pub parameters: Vec<Rc<Variable>>,
    /// All blocks of this function, which contain the expressions making up the func.
    pub blocks: HashMap<Rc<String>, Block>,
    /// All variables declared inside that need alloca in IR.
    pub variables: HashMap<Rc<String>, Rc<Variable>>,
    /// The return type of the function; Type::None if omitted.
    pub ret_type: Type,
}

impl Function {
    /// Either appends a new block or returns the one already present with the given name.
    /// When force_new is true, a new block is always created.
    /// Because of this, the name can be different than the given one when it was already in use.
    pub fn append_block(&mut self, name: &str, force_new: bool) -> Rc<String> {
        let mut name = name.to_string();
        if self.blocks.contains_key(&name) {
            if force_new {
                name = format!("{}-{}", name, self.blocks.len());
            } else {
                return Rc::new(name);
            }
        }
        let rc = Rc::new(name);
        self.blocks.insert(Rc::clone(&rc), Vec::with_capacity(5));
        rc
    }

    /// Inserts a variable into the functions allocation table.
    /// Returns the name of it (should be used since a change can be needed due to colliding names).
    pub fn insert_var(&mut self, mut name: Rc<String>, var: Rc<Variable>) -> Rc<String> {
        if self.variables.contains_key(&name) {
            name = Rc::new(format!("{}-{}", name, self.variables.len()));
        }
        self.variables.insert(Rc::clone(&name), var);
        name
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "func {}(", self.name)?;

        let mut params = self.parameters.iter();
        params
            .next()
            .map(|param| write!(f, "{}: {}", param.name, param.type_));
        for param in params {
            write!(f, ", {}: {}", param.name, param.type_)?;
        }

        writeln!(f, ") {{")?;
        for (name, block) in self.blocks.iter() {
            writeln!(f, "{}:", name)?;
            for inst in block.iter() {
                writeln!(f, "    {}", inst)?;
            }
        }
        writeln!(f, "}}")
    }
}

/// A block inside a function.
pub type Block = Vec<Expression>;

/// A variable. Used for function variables as well as for referencing functions.
#[derive(Debug, Default, Clone)]
pub struct Variable {
    pub mutable: bool,
    pub type_: Type,
    pub name: Rc<String>,
}

impl Hash for Variable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}
