/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 12/15/19 5:16 PM.
 * This file is under the Apache 2.0 license. See LICENSE in the root of this repository for details.
 */

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::rc::Rc;

use either::Either;
use indexmap::IndexMap;

use crate::{Error, ModulePath};
use crate::ast::declaration::{Class as ASTClass, Constructor, Function as ASTFunc};
use crate::ast::expression::Expression as ASTExpr;
use crate::ast::literal::Literal;
use crate::ast::module::Module;
use crate::ast::Type as ASTType;
use crate::error::Res;
use crate::lexer::token::{Token, TType};
use crate::mir::{IFACE_IMPLS, MModule, MutRc};
use crate::mir::generator::builder::MIRBuilder;
use crate::mir::generator::intrinsics::INTRINSICS;
use crate::mir::nodes::{
    ArrayLiteral, Class, ClassMember, Expr, Flow, Function, Type, Variable,
};
use crate::mir::result::ToMIRResult;
use crate::option::Flatten;

pub mod builder;
mod intrinsics;
pub mod module;
pub mod passes;

/// The MIRGenerator turns a list of declarations produced by the parser
/// into their MIR representation.
///
/// MIR is an intermediate format between the AST and LLVM IR.
///
/// The generator not only generates MIR, but also checks the code
/// for correctness (type-checking, scoping, etc.).
pub struct MIRGenerator {
    /// The module that the generator is linked to.
    pub module: MutRc<MModule>,

    /// The builder used to build the MIR.
    pub builder: MIRBuilder,

    /// The current insertion position.
    position: Option<Pointer>,

    /// An environment is a scope that variables live in.
    /// This field is used like a stack.
    /// See the begin_scope and end_scope functions for more info.
    environments: Vec<HashMap<Rc<String>, Rc<Variable>>>,

    /// The current loop, if in one.
    current_loop: Option<ForLoop>,

    /// All class members that are not initialized yet.
    /// This is only used when generating constructors to check
    /// that all constructors don't access uninitialized fields,
    /// and initialize all fields when finished.
    ///
    /// Because of this, calling is_empty() on this set
    /// can be used to determine if 'this' is fully
    /// initialized yet and if methods can be used.
    ///
    /// TODO: The code checking for illegal uninitialized access
    /// does not validate that the object the access occurs on is 'this'.
    /// Because of this, accesses of members on other objects of the same type
    /// (that ARE initialized) will be considered illegal.
    uninitialized_this_members: HashSet<Rc<ClassMember>>,
}

impl MIRGenerator {
    /// Fill a function's body.
    /// The AST given must be from a function inside the module.
    pub fn generate_function(&mut self, func: &ASTFunc) -> Res<()> {
        // Don't have to generate anything for external functions
        // which do not have a body
        let body = match func.body.as_ref() {
            None => return Ok(()),
            Some(body) => body,
        };

        let function = self.module.borrow().find_global(&func.sig.name.lexeme).unwrap();
        self.prepare_function(function.type_.as_function(), func.sig.name.line)?;
        let body = self.generate_expression(body)?;

        let ret_type = function.type_.as_function().borrow().ret_type.clone();
        match () {
            _ if ret_type == Type::None => self.insert_at_ptr(body),
            _ if ret_type == body.get_type() => self.insert_at_ptr(Expr::ret(body)),
            _ => {
                return Err(self.err(
                    &func.sig.name,
                    format!(
                        "Function return type ({}) does not match body type ({}).",
                        ret_type,
                        body.get_type()
                    ),
                ));
            }
        };

        self.end_scope();
        Ok(())
    }

    pub fn generate_constructors(&mut self, class: &ASTClass) -> Res<()> {
        let class_rc = self.module.borrow().find_type(&class.name.lexeme).unwrap().as_class().clone();

        for (constructor, mir_fn) in class.constructors.iter().zip(
            class_rc
                .borrow()
                .constructors
                .iter()
                .map(|v| v.type_.as_function()),
        ) {
            self.prepare_function(
                mir_fn,
                constructor.parameters.get(0).map(|l| l.0.line).unwrap_or(0),
            )?;
            self.set_uninitialized_members(constructor, &class_rc.borrow().members);
            let body = self.generate_expression(&constructor.body)?;
            self.insert_at_ptr(body);
            self.end_scope();
            self.check_no_uninitialized(&class.name)?;
        }

        self.uninitialized_this_members.clear();
        Ok(())
    }

    fn set_uninitialized_members(
        &mut self,
        constructor: &Constructor,
        class_mems: &IndexMap<Rc<String>, Rc<ClassMember>>,
    ) {
        self.uninitialized_this_members.clear();
        for (name, mem) in class_mems.iter() {
            let initialized = constructor
                .parameters
                .iter()
                .filter(|p| p.1.is_none())
                .any(|p| &p.0.lexeme == name);
            if !initialized && !mem.has_default_value {
                self.uninitialized_this_members.insert(Rc::clone(&mem));
            }
        }
    }

    fn check_no_uninitialized(&mut self, err_tok: &Token) -> Res<()> {
        if self.uninitialized_this_members.is_empty() {
            Ok(())
        } else {
            Err(self.err(
                err_tok,
                "Cannot have uninitialized fields after constructor.".to_string(),
            ))
        }
    }

    fn prepare_function(&mut self, function: &MutRc<Function>, err_line: usize) -> Res<()> {
        let mut func = function.borrow_mut();
        let entry_block = func.append_block("entry", false);

        self.set_pointer(Rc::clone(function), Rc::clone(&entry_block));
        self.begin_scope();
        for param in func.parameters.iter() {
            self.insert_variable(Rc::clone(param), false, err_line)?;
        }

        Ok(())
    }

    fn generate_expression(&mut self, expression: &ASTExpr) -> Res<Expr> {
        unimplemented!()
        /*
        Ok(match expression {
            ASTExpr::Assignment { name, value } => {
                let var = self.find_var(&name)?;
                if var.mutable {
                    let value = self.generate_expression(&**value)?;
                    if value.get_type() == var.type_ {
                        self.builder.build_store(var, value)
                    } else {
                        return Err(self.error(
                            &name,
                            &name,
                            &format!("Variable {} is a different type", name.lexeme),
                        ));
                    }
                } else {
                    return Err(self.error(
                        &name,
                        &name,
                        &format!("Variable {} is not assignable (val)", name.lexeme),
                    ));
                }
            }

            ASTExpr::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.generate_expression(&**left)?;
                let right = self.generate_expression(&**right)?;
                let left_ty = left.get_type();
                let right_ty = right.get_type();

                if (left_ty == right_ty) && (left_ty.is_int() || left_ty.is_float()) {
                    self.builder.build_binary(left, operator.t_type, right)
                } else {
                    let method_var = self
                        .get_operator_overloading_method(operator.t_type, &left_ty, &right_ty)
                        .or_err(
                            self,
                            operator,
                            "No implementation of operator found for types.",
                        )?;
                    let method_rc = method_var.type_.as_function();
                    let method = method_rc.borrow();
                    if method.parameters[1].type_ != right_ty {
                        return Err(self.error(
                            operator,
                            operator,
                            "Right-hand side expression is wrong type.",
                        ));
                    }
                    drop(method);

                    let mut expr = self
                        .builder
                        .build_call(Expr::VarGet(method_var), vec![left, right]);
                    if operator.t_type == TType::BangEqual {
                        expr = self.builder.build_unary(expr, TType::Bang);
                    }
                    expr
                }
            }

            ASTExpr::Block(expressions) => {
                if expressions.is_empty() {
                    return Ok(Self::none_const());
                }

                self.begin_scope();

                for expression in expressions.iter().take(expressions.len() - 1) {
                    let expression = self.generate_expression(&expression)?;
                    self.builder.insert_at_ptr(expression);
                }
                let last = self.generate_expression(expressions.last().unwrap())?;

                self.end_scope();
                last
            }

            ASTExpr::Break(expr) => {
                if self.current_loop.is_none() {
                    return Err(self.anon_err(
                        expr.as_ref().map(|e| e.get_token()).flatten_(),
                        "Break is only allowed in loops.",
                    ));
                }

                if let Some(expression) = expr {
                    let expression = self.generate_expression(&**expression)?;
                    self.get_or_create_loop_var(&expression.get_type())?;
                    let cur_block = self.builder.cur_block_name();
                    self.cur_loop().phi_nodes.push((expression, cur_block));
                }

                let cont_block = Rc::clone(&self.cur_loop().cont_block);
                self.builder.build_jump(&cont_block);
                Self::any_const()
            }

            ASTExpr::Call { callee, arguments } => {
                match &**callee {
                    // Method call
                    ASTExpr::Get { object, name } => {
                        if !self.uninitialized_this_members.is_empty() {
                            return Err(self.error(name, name, "Cannot call methods in constructors until all class members are initialized."));
                        }

                        let (object, field) = self.get_field(object, name)?;
                        let func =
                            field
                                .right()
                                .or_err(self, name, "Class members cannot be called.")?;

                        let args = self.generate_func_args(
                            Rc::clone(func.type_.as_function()),
                            arguments,
                            Some(object),
                        )?;
                        return Ok(self.builder.build_call(Expr::VarGet(func), args));
                    }

                    // Might be class constructor
                    ASTExpr::Variable(name) => {
                        if let Some(class) = self.builder.find_class(&name.lexeme) {
                            return self.generate_class_instantiation(class, arguments, name);
                        }
                    }

                    ASTExpr::VarWithGenerics { name, generics } => {
                        if let Some(proto) = self
                            .builder
                            .find_class_or_proto(&name.lexeme)
                            .map(|o| o.right())
                            .flatten_()
                        {
                            let types = generics
                                .iter()
                                .map(|ty| self.find_type(ty))
                                .collect::<Result<Vec<Type>, MIRError>>()?;

                            let class = proto.borrow().build(self, types, &name)?;
                            return self.generate_class_instantiation(class, arguments, name);
                        }
                    }

                    _ => (),
                }

                // match above fell through, its either a function call or invalid
                let callee_mir = self.generate_expression(&**callee)?;
                if let Type::Function(func) = callee_mir.get_type() {
                    let args = self.generate_func_args(func, arguments, None)?;
                    self.builder.build_call(callee_mir, args)
                } else {
                    return Err(self.anon_err(
                        callee.get_token(),
                        "Only functions are allowed to be called",
                    ));
                }
            }

            ASTExpr::For {
                condition,
                body,
                else_b,
            } => {
                let loop_block = self.builder.append_block("for-loop");
                let mut else_block = self.builder.append_block("for-else");
                let cont_block = self.builder.append_block("for-cont");

                let prev_loop =
                    std::mem::replace(&mut self.current_loop, Some(ForLoop::new(&cont_block)));

                let cond = self.generate_expression(&**condition)?;
                if cond.get_type() != Type::Bool {
                    return Err(
                        self.anon_err(condition.get_token(), "For condition must be a boolean.")
                    );
                }

                self.builder
                    .build_branch(cond.clone(), &loop_block, &else_block);

                self.builder.set_block(&loop_block);
                let body = self.generate_expression(&**body)?;
                let body_type = body.get_type();

                let loop_end_block = self.builder.cur_block_name();
                let body_alloca = self.get_or_create_loop_var(&body_type)?;

                let store = self.builder.build_store(Rc::clone(&body_alloca), body);
                self.builder.insert_at_ptr(store);
                self.builder.build_branch(cond, &loop_block, &cont_block);

                let mut ret = Self::none_const();
                if let Some(else_b) = else_b {
                    self.builder.set_block(&else_block);
                    let else_val = self.generate_expression(&**else_b)?;
                    else_block = self.builder.cur_block_name();

                    if else_val.get_type() == body_type {
                        self.builder.set_block(&cont_block);

                        let load = self.builder.build_load(body_alloca);
                        self.cur_loop()
                            .phi_nodes
                            .push((load, Rc::clone(&loop_end_block)));
                        self.cur_loop()
                            .phi_nodes
                            .push((else_val, Rc::clone(&else_block)));

                        ret = self
                            .builder
                            .build_phi(self.current_loop.take().unwrap().phi_nodes)
                    }
                }

                self.builder.set_block(&else_block);
                self.builder.build_jump(&cont_block);
                self.builder.set_block(&cont_block);
                self.current_loop = prev_loop;

                ret
            }

            ASTExpr::Get { object, name } => {
                let (object, field) = self.get_field(&**object, name)?;
                let field =
                    field
                        .left()
                        .or_err(self, name, "Cannot get class method (must be called)")?;

                if self.uninitialized_this_members.contains(&field) {
                    return Err(self.error(name, name, "Cannot get uninitialized class member."));
                }
                self.builder.build_struct_get(object, field)
            }

            ASTExpr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.generate_expression(&**condition)?;
                if cond.get_type() != Type::Bool {
                    return Err(self.anon_err(
                        condition.get_token().or_else(|| then_branch.get_token()),
                        "If condition must be a boolean",
                    ));
                }

                let mut then_block = self.builder.append_block("then");
                let mut else_block = self.builder.append_block("else");
                let cont_block = self.builder.append_block("cont");

                self.builder.build_branch(cond, &then_block, &else_block);

                self.builder.set_block(&then_block);
                let then_val = self.generate_expression(&**then_branch)?;
                then_block = self.builder.cur_block_name();

                self.builder.set_block(&else_block);
                if let Some(else_branch) = else_branch {
                    let else_val = self.generate_expression(&**else_branch)?;
                    else_block = self.builder.cur_block_name();

                    if then_val.get_type() == else_val.get_type() {
                        self.builder.build_jump(&cont_block);
                        self.builder.set_block(&then_block);
                        self.builder.build_jump(&cont_block);

                        self.builder.set_block(&cont_block);
                        return Ok(self
                            .builder
                            .build_phi(vec![(then_val, then_block), (else_val, else_block)]));
                    } else {
                        self.builder.insert_at_ptr(else_val);
                        self.builder.build_jump(&cont_block);
                    }
                } else {
                    self.builder.set_block(&else_block);
                    self.builder.build_jump(&cont_block);
                }

                self.builder.set_block(&then_block);
                self.builder.insert_at_ptr(then_val);
                self.builder.build_jump(&cont_block);

                self.builder.set_block(&cont_block);
                Self::none_const()
            }

            ASTExpr::Literal(literal) => {
                if let Literal::Array(arr) = literal {
                    let ast_values = arr.as_ref().left().unwrap();
                    let mut values_mir = Vec::new();
                    let mut ast_values = ast_values.iter();
                    let first = self.generate_expression(ast_values.next().unwrap())?;
                    let arr_type = first.get_type();

                    values_mir.push(first);
                    for value in ast_values {
                        let mir_val = self.generate_expression(value)?;

                        if mir_val.get_type() != arr_type {
                            return Err(self.anon_err(
                                value.get_token(),
                                &format!(
                                    "Type of array value ({}) does not rest of array ({}).",
                                    mir_val.get_type(),
                                    arr_type
                                ),
                            ));
                        }

                        values_mir.push(mir_val);
                    }

                    Expr::Literal(Literal::Array(Either::Right(ArrayLiteral {
                        values: values_mir,
                        type_: arr_type,
                    })))
                } else {
                    Expr::Literal(literal.clone())
                }
            }

            ASTExpr::Return(val) => {
                let value = val
                    .as_ref()
                    .map(|v| self.generate_expression(&*v))
                    .transpose()?
                    .unwrap_or_else(Self::none_const);

                if value.get_type() != self.builder.cur_fn().borrow().ret_type.clone() {
                    return Err(self.anon_err(
                        val.as_ref().map(|v| v.get_token()).flatten_(),
                        "Return expression in function has wrong type",
                    ));
                }

                self.builder.set_return(Flow::Return(value));
                Self::any_const()
            }

            ASTExpr::Set {
                object,
                name,
                value,
            } => {
                let (object, field) = self.get_field(&**object, name)?;
                let field = field.left().or_err(self, name, "Cannot set class method")?;
                let value = self.generate_expression(&**value)?;

                if value.get_type() != field.type_ {
                    return Err(self.error(name, name, "Class member is a different type"));
                }
                if !field.mutable && !self.uninitialized_this_members.contains(&field) {
                    return Err(self.error(name, name, "Cannot set immutable class member"));
                }

                self.uninitialized_this_members.remove(&field);
                self.builder.build_struct_set(object, field, value)
            }

            ASTExpr::Unary { operator, right } => {
                let right = self.generate_expression(&**right)?;

                match operator.t_type {
                    TType::Bang if right.get_type() != Type::Bool => Err(self.error(
                        operator,
                        operator,
                        "'!' can only be used on boolean values",
                    )),

                    _ => Ok(()),
                }?;

                self.builder.build_unary(right, operator.t_type)
            }

            ASTExpr::Variable(var) => {
                let var = self.find_var(&var)?;
                self.builder.build_load(var)
            }

            ASTExpr::VarWithGenerics { name, generics } => {
                // All valid cases of this are function prototypes.
                // Class prototypes can only be called and not assigned;
                // which would be handled in the ASTExpr::Call branch.
                let prototype = self
                    .builder
                    .find_func_or_proto(&name.lexeme)
                    .or_err(self, &name, "Unknown variable.")?
                    .right()
                    .or_err(self, &name, "Function does not take generic parameters.")?;
                let types = generics
                    .iter()
                    .map(|ty| self.find_type(ty))
                    .collect::<Result<Vec<Type>, MIRError>>()?;

                let function = prototype
                    .borrow_mut()
                    .build(self, types, name)?;
                self.builder.build_load(function)
            }

            ASTExpr::When {
                value,
                branches,
                else_branch,
            } => {
                let start_b = self.builder.cur_block_name();

                let value = self.generate_expression(value)?;
                let val_type = value.get_type();

                let else_b = self.builder.append_block("when-else");
                let cont_b = self.builder.append_block("when-cont");

                self.builder.set_block(&else_b);
                let else_val = self.generate_expression(else_branch)?;
                let branch_type = else_val.get_type();
                self.builder.build_jump(&cont_b);

                let mut cases = Vec::with_capacity(branches.len());
                let mut phi_nodes = Vec::with_capacity(branches.len());
                for (b_val, branch) in branches.iter() {
                    self.builder.set_block(&start_b);
                    let val = self.generate_expression(b_val)?;
                    if val.get_type() != val_type {
                        return Err(self.anon_err(
                            b_val.get_token(),
                            "Branches of when must be of same type as the value compared.",
                        ));
                    }
                    let val = self
                        .builder
                        .build_binary(val, TType::EqualEqual, value.clone());

                    let branch_b = self.builder.append_block("when-br");
                    self.builder.set_block(&branch_b);
                    let branch_val = self.generate_expression(branch)?;
                    if branch_val.get_type() != branch_type {
                        return Err(self
                            .anon_err(branch.get_token(), "Branch results must be of same type."));
                    }
                    self.builder.build_jump(&cont_b);

                    let branch_b = self.builder.cur_block_name();
                    cases.push((val, Rc::clone(&branch_b)));
                    phi_nodes.push((branch_val, branch_b))
                }

                phi_nodes.push((else_val, Rc::clone(&else_b)));

                self.builder.set_block(&start_b);
                self.builder.set_return(Flow::Switch {
                    cases,
                    default: else_b,
                });

                self.builder.set_block(&cont_b);
                self.builder.build_phi(phi_nodes)
            }

            ASTExpr::VarDef(var) => {
                let init = self.generate_expression(&var.initializer)?;
                let _type = init.get_type();
                let var = self.define_variable(&var.name, var.mutable, _type);
                self.builder.build_store(var, init)
            }
        })
        */
    }

    /// Defines a new variable. It is put into the variable list in the current function
    /// and placed in the topmost scope.
    fn define_variable(&mut self, token: &Token, mutable: bool, ty: Type) -> Rc<Variable> {
        let def = Rc::new(Variable {
            mutable,
            type_: ty,
            name: Rc::clone(&token.lexeme),
        });

        self.add_function_variable(Rc::clone(&def));
        self.insert_variable(Rc::clone(&def), true, token.line).unwrap_or(());
        def
    }

    /// Inserts a variable into the topmost scope.
    /// Note that the variable does NOT get added to the function!
    fn insert_variable(&mut self, var: Rc<Variable>, allow_redefine: bool, line: usize) -> Res<()> {
        let cur_env = self.environments.last_mut().unwrap();
        let was_defined = cur_env
            .insert(Rc::clone(&var.name), Rc::clone(&var))
            .is_some();
        if was_defined && !allow_redefine {
            let mut tok = Token::generic_identifier((*var.name).clone());
            tok.line = line;
            return Err(self.err(
                &tok,
                format!(
                    "Cannot redefine variable '{}' in the same scope.",
                    &var.name
                ),
            ));
        }

        Ok(())
    }

    /// Will create the variable in the current function.
    pub fn add_function_variable(&mut self, variable: Rc<Variable>) {
        self.cur_fn()
            .borrow_mut()
            .insert_var(Rc::clone(&variable.name), Rc::clone(&variable));
    }

    /// Searches all scopes for a variable, starting at the top.
    fn find_var(&mut self, token: &Token) -> Res<Rc<Variable>> {
        for env in self.environments.iter().rev() {
            if let Some(var) = env.get(&token.lexeme) {
                return Ok(Rc::clone(var));
            }
        }

        self.module.borrow().find_global(&token.lexeme).or_err(
            &self.builder.path,
            token,
            &format!("Variable '{}' is not defined", token.lexeme),
        )
    }

    /// Returns the variable of the current loop or creates it if it does not exist yet
    fn get_or_create_loop_var(&mut self, type_: &Type) -> Res<Rc<Variable>> {
        let var = self.cur_loop().result_var.clone().unwrap_or_else(|| {
            self.define_variable(
                &Token::generic_identifier("for-body".to_string()),
                true,
                type_.clone(),
            )
        });
        self.cur_loop().result_var = Some(Rc::clone(&var));

        if &var.type_ != type_ {
            Err(Error::new(
                &Token::generic_token(TType::Break),
                "MIR",
                "Break expressions and for body must have same type".to_string(),
                &self.builder.path,
            ))
        } else {
            Ok(var)
        }
    }

    fn get_field(
        &mut self,
        object: &ASTExpr,
        name: &Token,
    ) -> Res<(Expr, Either<Rc<ClassMember>, Rc<Variable>>)> {
        let object = self.generate_expression(object)?;
        let ty = object.get_type();

        if let Type::Class(class) = &ty {
            let class = class.borrow();
            let field = class.members.get(&name.lexeme);
            if let Some(field) = field {
                return Ok((object, Either::Left(Rc::clone(field))));
            }
        }

        self.builder
            .find_associated_method(ty, name)
            .map(|m| (object, Either::Right(m)))
            .or_err(&self.builder.path, name, "Unknown field or method.")
    }

    fn generate_class_instantiation(
        &mut self,
        class: MutRc<Class>,
        args: &Vec<ASTExpr>,
        err_tok: &Token,
    ) -> Res<Expr> {
        let mut args = args
            .iter()
            .map(|arg| self.generate_expression(arg))
            .collect::<Res<Vec<Expr>>>()?;
        let inst = self.build_class_inst(Rc::clone(&class));
        args.insert(0, inst.clone());

        let class = class.borrow();
        let constructor = class
            .constructors
            .iter()
            .find(|constructor| {
                let constructor = constructor.type_.as_function().borrow();
                if constructor.parameters.len() != args.len() {
                    return false;
                }
                for (param, arg) in constructor.parameters.iter().zip(args.iter()) {
                    if param.type_ != arg.get_type() {
                        return false;
                    }
                }
                true
            })
            .or_err(
                &self.builder.path,
                err_tok,
                "No matching constructor found for arguments.",
            )?;

        let call = Expr::call(Expr::load(constructor), args);
        self.insert_at_ptr(call);
        Ok(inst)
    }

    /// Builds a class instance and returns an expression that loads the instance.
    /// The expression returned can be safely cloned to reuse the instance.
    fn build_class_inst(&mut self, class_ref: MutRc<Class>) -> Expr {
        let call = {
            let class = class_ref.borrow();
            Expr::call(Expr::load(&class.instantiator), vec![])
        };

        let var = Rc::new(Variable {
            mutable: true,
            type_: Type::Class(class_ref),
            name: Rc::new("tmp-constructor-var".to_string()),
        });
        self.add_function_variable(Rc::clone(&var));
        self.insert_at_ptr(Expr::store(&var, call));

        Expr::load(&var)
    }

    fn generate_func_args(
        &mut self,
        func_ref: MutRc<Function>,
        arguments: &[ASTExpr],
        first_arg: Option<Expr>,
        err_tok: &Token
    ) -> Res<Vec<Expr>> {
        let func = func_ref.borrow();

        let args_len = arguments.len() + (first_arg.is_some() as usize);
        if func.parameters.len() != args_len {
            return Err(self.err(
                err_tok,
                format!(
                    "Incorrect amount of function arguments. (Expected {}; got {})",
                    func.parameters.len(),
                    arguments.len()
                ),
            ));
        }

        let mut result = Vec::with_capacity(args_len);
        let first_arg_is_some = first_arg.is_some();
        if let Some(arg) = first_arg {
            let ty = &func.parameters[0].type_;
            let arg = self
                .check_call_arg_type(arg, ty)
                .expect("internal error: method call");
            result.push(arg)
        }
        for (argument, parameter) in arguments
            .iter()
            .zip(func.parameters.iter().skip(first_arg_is_some as usize))
        {
            let arg = self.generate_expression(argument)?;
            let arg_type = arg.get_type();
            let arg = self
                .check_call_arg_type(arg, &parameter.type_)
                .or_err(
                    &self.builder.path,
                    argument.get_token(),
                    &format!(
                        "Call argument is the wrong type (was {}, expected {})",
                        arg_type, parameter.type_
                    ),
                )?;
            result.push(arg)
        }
        Ok(result)
    }

    /// Returns the method that corresponds to the operator given (operator overloading).
    /// Returns None if the given class does not implement overloading.
    fn get_operator_overloading_method(
        &mut self,
        op: TType,
        left_ty: &Type,
        right_ty: &Type,
    ) -> Option<Rc<Variable>> {
        let proto = INTRINSICS.with(|i| i.borrow().get_op_iface(op));
        let iface_impls = IFACE_IMPLS.with(|impls| impls.borrow().get(left_ty).cloned())?;
        let iface_impls = iface_impls.borrow();
        let op_impls = iface_impls
            .interfaces
            .iter()
            .filter(|im| unimplemented!());

        for im in op_impls {
            let method = im.methods.get_index(0).unwrap().1;
            if &method.type_.as_function().borrow().parameters[1].type_ == right_ty {
                return Some(method).cloned();
            }
        }
        None
    }

    /// Checks if the arg parameter is of the given type ty.
    /// Will do casts if needed to make the types match;
    /// returns the new expression that should be used in case a cast happened.
    fn check_call_arg_type(&self, arg: Expr, ty: &Type) -> Option<Expr> {
        let arg_type = arg.get_type();
        if &arg_type == ty {
            Some(arg)
        } else {
            None
        }
    }

    /// Creates a new scope. A new scope is created for every function and block,
    /// in addition to the bottom global scope.
    ///
    /// # Example
    /// (global scope #1)
    /// func main() {       <- new scope (#2) for the class main
    ///     var a = 5       <- a now in scope #2
    ///     {               <- new scope (#3)
    ///         var b = 1   <- b now in scope #3
    ///     }               <- scope #3 gets removed, along with b
    /// }                   <- scope #2 gets removed, along with a
    fn begin_scope(&mut self) {
        self.environments.push(HashMap::new());
    }

    /// Removes the topmost scope.
    fn end_scope(&mut self) {
        self.environments.pop();
    }

    /// Will append a block to the given function, always creating a new one.
    pub fn append_block(&mut self, name: &str) -> Rc<String> {
        self.cur_fn().borrow_mut().append_block(name, true)
    }

    pub fn insert_at_ptr(&mut self, expr: Expr) {
        let func = self.cur_fn();
        let mut func = func.borrow_mut();
        func.blocks
            .get_mut(&self.position.as_ref().unwrap().block)
            .unwrap()
            .push(expr)
    }

    pub fn set_pointer(&mut self, function: MutRc<Function>, block: Rc<String>) {
        self.position = Some(Pointer { function, block })
    }

    pub fn set_block(&mut self, block: &Rc<String>) {
        if let Some(pos) = self.position.as_mut() {
            pos.block = Rc::clone(block)
        }
    }

    pub fn cur_fn(&self) -> MutRc<Function> {
        self.position.as_ref().unwrap().function.clone()
    }

    pub fn cur_block_name(&self) -> Rc<String> {
        Rc::clone(&self.position.as_ref().unwrap().block)
    }

    fn cur_loop(&mut self) -> &mut ForLoop {
        self.current_loop.as_mut().unwrap()
    }

    fn err(&self, tok: &Token, msg: String) -> Error {
        Error::new(tok, "MIR", msg, &self.builder.path)
    }

    pub fn new(module: &MutRc<MModule>) -> Self {
        MIRGenerator {
            module: Rc::clone(module),
            builder: MIRBuilder::new(module),
            position: None,
            environments: Vec::with_capacity(5),
            current_loop: None,
            uninitialized_this_members: HashSet::with_capacity(10)
        }
    }
}

/// A pointer is the location the generator is inserting into.
pub struct Pointer {
    /// The function inserting into
    pub function: MutRc<Function>,
    /// The name of the block appending to
    block: Rc<String>,
}

/// All data of a loop.
struct ForLoop {
    /// The alloca of the for loop result. Can be None for loops that return None type.
    result_var: Option<Rc<Variable>>,
    /// The block to jump to when the current loop finishes.
    cont_block: Rc<String>,
    /// The phi nodes of the loop (loops are expressions).
    phi_nodes: Vec<(Expr, Rc<String>)>,
}

impl ForLoop {
    fn new(cont_block: &Rc<String>) -> ForLoop {
        ForLoop {
            result_var: None,
            cont_block: Rc::clone(cont_block),
            phi_nodes: vec![],
        }
    }
}
