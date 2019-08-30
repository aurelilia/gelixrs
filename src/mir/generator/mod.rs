/*
 * Developed by Ellie Ang. (git@angm.xyz).
 * Last modified on 8/30/19 2:52 PM.
 * This file is under the GPL3 license. See LICENSE in the root directory of this repository for details.
 */

mod builder;
mod passes;

use crate::ast::declaration::{DeclarationList, FuncSignature, Function};
use crate::ast::expression::{display_slice, Expression};
use crate::ast::literal::Literal;
use crate::lexer::token::{Token, Type};
use crate::mir::generator::passes::declare::DeclarePass;
use crate::mir::generator::passes::fill_struct::FillStructPass;
use crate::mir::generator::passes::PreMIRPass;
use crate::mir::nodes::{MIRExpression, MIRFlow, MIRFunction, MIRStructMem, MIRType, MIRVariable};
use crate::mir::{MutRc, MIR};
use builder::MIRBuilder;
use std::collections::HashMap;
use std::rc::Rc;

type Res<T> = Result<T, Error>;

pub struct MIRGenerator {
    builder: MIRBuilder,
    environments: Vec<HashMap<Rc<String>, Rc<MIRVariable>>>,

    is_in_loop: bool,
    current_loop_ret_type: Option<MIRType>,
    current_loop_cont_block: Option<Rc<String>>
}

impl MIRGenerator {
    pub fn generate(self, list: DeclarationList) -> Option<MIR> {
        self.run(list)
            .or_else(|err| {
                eprintln!(
                    "[MIRGenerator] {}:
{} |
{} | {}
{} |",
                    err.message,
                    err.line
                        .map(|l| (l - 1).to_string())
                        .unwrap_or_else(|| "?".to_string()),
                    err.line.map(|l| l.to_string()).unwrap_or_else(|| "?".to_string()),
                    err.code,
                    err.line
                        .map(|l| (l + 1).to_string())
                        .unwrap_or_else(|| "?".to_string()),
                );
                Err(())
            })
            .ok()
    }

    fn run(mut self, mut list: DeclarationList) -> Res<MIR> {
        // Run all pre-MIR passes
        DeclarePass::new(&mut self).run(&mut list)?;
        FillStructPass::new(&mut self).run(&mut list)?;

        // Generate the MIR
        self.generate_mir(list)?;

        // Return the finished MIR
        Ok(MIR {
            types: self.builder.get_types(),
            functions: self.environments.remove(0),
        })
    }

    fn generate_mir(&mut self, list: DeclarationList) -> Result<(), Error> {
        for func in list.functions.into_iter().chain(
            list.classes
                .into_iter()
                .map(|class| class.methods)
                .flatten(),
        ) {
            self.generate_function(func)?;
        }

        Ok(())
    }

    fn generate_function(&mut self, func: Function) -> Res<()> {
        let function_rc = self.builder.find_function(&func.sig.name.lexeme).unwrap();
        let mut function = function_rc.borrow_mut();
        let func_type = function.ret_type.clone();
        function.append_block("entry".to_string());
        drop(function);
        self.builder
            .set_pointer(Rc::clone(&function_rc), Rc::new("entry".to_string()));

        self.begin_scope();
        for param in function_rc.borrow().parameters.iter() {
            self.insert_variable(Rc::clone(param), false, func.sig.name.line)?;
        }

        let body = self.generate_expression(func.body)?;
        if func_type != MIRType::None {
            if func_type == body.get_type() {
                self.builder.set_return(MIRFlow::Return(body));
            } else {
                return Err(Error::new_fn(
                    "Function return type does not match body type",
                    &func.sig,
                ));
            }
        } else {
            self.builder.insert_at_ptr(body)
        }

        self.end_scope();
        Ok(())
    }

    fn generate_expression(&mut self, expression: Expression) -> Res<MIRExpression> {
        Ok(match expression {
            Expression::Assignment { name, value } => {
                let var = self.find_var(&name)?;
                if var.mutable {
                    let value = self.generate_expression(*value)?;
                    if value.get_type() == var._type {
                        self.builder.build_store(var, value)
                    } else {
                        return Err(Error::new(
                            Some(name.line),
                            &format!("Variable {} is a different type", name.lexeme),
                            name.lexeme.to_string(),
                        ))
                    }
                } else {
                    return Err(Error::new(
                        Some(name.line),
                        &format!("Variable {} is not assignable (val)", name.lexeme),
                        name.lexeme.to_string(),
                    ))
                }
            }

            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.generate_expression(*left)?;
                let right = self.generate_expression(*right)?;

                if (left.get_type() == MIRType::Int) && (right.get_type() == MIRType::Int) {
                    self.builder.build_binary(left, operator, right)
                } else {
                    return Err(Error::new(
                        Some(operator.line),
                        "Binary operations are only allowed on i64.",
                        format!("{:?}", operator.t_type),
                    ))
                }
            }

            Expression::Block(mut expressions) => {
                self.begin_scope();

                let last = expressions.pop();
                for expression in expressions {
                    let expression = self.generate_expression(expression)?;
                    self.builder.insert_at_ptr(expression);
                }

                let last = last
                    .map(|e| self.generate_expression(e))
                    .unwrap_or_else(|| Ok(MIRGenerator::none_const()))?;

                self.end_scope();
                last
            }

            Expression::Break(expr) => {
                if let Some(expression) = expr {
                    let expression = self.generate_expression(*expression)?;
                    let body_alloca = self.find_or_create_var(
                        expression.get_type(),
                        Token::generic_identifier("for-body".to_string())
                    )?;
                    self.builder.build_store(body_alloca, expression);
                }

                self.builder.set_return(MIRFlow::Jump(Rc::clone(self.current_loop_cont_block.as_ref().unwrap())));
                Self::none_const()
            },

            Expression::Call { callee, arguments } => {
                match &*callee {
                    // Method call
                    Expression::Get { object: _, name: _ } => unimplemented!(),

                    // Might be class constructor
                    Expression::Variable(name) => {
                        if let Some(struc) = self.builder.find_struct(&name.lexeme) {
                            return Ok(self.builder.build_constructor(struc));
                        }
                    }

                    _ => (),
                }

                // match above fell through, its either a function call or invalid
                let callee = self.generate_expression(*callee)?;
                if let MIRType::Function(func) = callee.get_type() {
                    let args = self.generate_func_args(func, arguments)?;
                    self.builder.build_call(callee, args)
                } else {
                    return Err(Error::useless("Only functions or classes are allowed to be called"));
                }
            }

            Expression::For { condition, body, } => {
                let cur_fn_rc = self.builder.cur_fn();
                let mut cur_fn = cur_fn_rc.borrow_mut();
                let cond_block = cur_fn.append_block("forcond".to_string());
                let loop_block = cur_fn.append_block("forloop".to_string());
                let cont_block = cur_fn.append_block("forcont".to_string());
                let prev_cont_block = std::mem::replace(&mut self.current_loop_cont_block, Some(Rc::clone(&cond_block)));
                drop(cur_fn);

                self.builder.set_return(MIRFlow::Jump(Rc::clone(&cond_block)));
                self.builder.set_block(&cond_block);
                let condition = self.generate_expression(*condition)?;
                if condition.get_type() != MIRType::Bool {
                    return Err(Error::useless("For condition must be a boolean."))
                }

                self.builder.set_return(MIRFlow::Branch {
                    condition,
                    then_b: Rc::clone(&loop_block),
                    else_b: Rc::clone(&cont_block)
                });

                self.builder.set_block(&loop_block);
                let body = self.generate_expression(*body)?;
                let body_alloca = self.find_or_create_var(
                    body.get_type(),
                    Token::generic_identifier("for-body".to_string())
                )?;

                let store = self.builder.build_store(Rc::clone(&body_alloca), body);
                self.builder.insert_at_ptr(store);
                self.builder.set_return(MIRFlow::Jump(Rc::clone(&cond_block)));

                self.builder.set_block(&cont_block);
                self.current_loop_cont_block = prev_cont_block;
                self.builder.build_load(body_alloca)
            },

            Expression::Get { object, name } => {
                let (object, field) = self.get_class_field(*object, name)?;
                self.builder.build_struct_get(object, field)
            }

            Expression::Grouping(expr) => self.generate_expression(*expr)?,

            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.generate_expression(*condition)?;
                if let MIRType::Bool = condition.get_type() {
                } else {
                    return Err(Error::new(
                        then_branch.get_line(),
                        "If condition must be a boolean",
                        "if (...)".to_string(),
                    ));
                };

                let func = self.builder.cur_fn();
                let mut func = func.borrow_mut();
                let mut then_b = func.append_block("then".to_string());
                let mut else_b = func.append_block("else".to_string());
                let cont_b = func.append_block("cont".to_string());
                // Setting return will mutably borrow the func; drop this one to prevent panic
                drop(func);

                self.builder.set_return(MIRFlow::Branch {
                    condition,
                    then_b: Rc::clone(&then_b),
                    else_b: Rc::clone(&else_b),
                });

                self.builder.set_block(&then_b);
                let then_val = self.generate_expression(*then_branch)?;
                then_b = self.builder.cur_block_name();
                self.builder.set_return(MIRFlow::Jump(Rc::clone(&cont_b)));

                self.builder.set_block(&else_b);
                if let Some(else_branch) = else_branch {
                    let else_val = self.generate_expression(*else_branch)?;
                    else_b = self.builder.cur_block_name();
                    self.builder.set_return(MIRFlow::Jump(Rc::clone(&cont_b)));
                    self.builder.set_block(&cont_b);

                    if then_val.get_type() == else_val.get_type() {
                        return Ok(self.builder.build_phi(
                            (then_val, Rc::clone(&then_b)),
                            (else_val, Rc::clone(&else_b)),
                        ));
                    } else {
                        self.builder.set_block(&else_b);
                        self.builder.insert_at_ptr(else_val);
                    }
                }

                self.builder.set_block(&then_b);
                self.builder.insert_at_ptr(then_val);

                self.builder.set_block(&else_b);
                self.builder.set_return(MIRFlow::Jump(Rc::clone(&cont_b)));

                self.builder.set_block(&cont_b);
                MIRGenerator::none_const()
            }

            Expression::Literal(literal) => self.builder.build_literal(literal),

            Expression::Return(value) => {
                let value = value.map(|v| self.generate_expression(*v)).transpose()?;
                let _type = value
                    .as_ref()
                    .map(|v| v.get_type())
                    .unwrap_or(MIRType::None);

                if _type != self.builder.cur_fn().borrow().ret_type {
                    // TODO: useless error
                    return Err(Error::new(
                        None,
                        "Return expression has wrong type",
                        "".to_string(),
                    ));
                }

                self.builder.set_return(MIRFlow::Return(
                    value.unwrap_or_else(Self::none_const),
                ));
                Self::none_const()
            }

            Expression::Set {
                object,
                name,
                value,
            } => {
                let (object, field) = self.get_class_field(*object, name)?;
                let value = self.generate_expression(*value)?;

                if value.get_type() != field._type {
                    return Err(Error::useless("Cannot set class member to different type."));
                }

                self.builder.build_struct_set(object, field, value)
            }

            Expression::Unary { operator, right, } => {
                let right = self.generate_expression(*right)?;

                match operator.t_type {
                    Type::Minus => self.builder.build_unary(right, operator),
                    Type::Bang => {
                        if right.get_type() == MIRType::Bool {
                            self.builder.build_unary(right, operator)
                        } else {
                            return Err(Error::useless("'!' can only be used on boolean values"))
                        }
                    }
                    _ => panic!("Invalid unary expression"),
                }
            },

            Expression::Variable(var) => {
                let var = self.find_var(&var)?;
                self.builder.build_load(var)
            }

            Expression::When {
                value: _,
                branches: _,
                else_branch: _,
            } => return Err(Error::useless("Unimplemented: when")),

            Expression::VarDef(var) => {
                let init = self.generate_expression(var.initializer)?;
                let _type = init.get_type();
                let var = self.define_variable(&var.name, !var.is_val, _type);
                self.builder.build_store(var, init)
            }
        })
    }

    fn define_variable(
        &mut self,
        token: &Token,
        mutable: bool,
        _type: MIRType,
    ) -> Rc<MIRVariable> {
        let def = Rc::new(MIRVariable::new(Rc::clone(&token.lexeme), _type, mutable));
        self.builder.add_function_variable(Rc::clone(&def));
        self.insert_variable(Rc::clone(&def), true, token.line);
        def
    }

    fn insert_variable(
        &mut self,
        var: Rc<MIRVariable>,
        allow_redefine: bool,
        line: usize,
    ) -> Res<()> {
        let cur_env = self.environments.last_mut().unwrap();
        let was_defined = cur_env
            .insert(Rc::clone(&var.name), Rc::clone(&var))
            .is_some();
        if was_defined && !allow_redefine {
            return Err(Error {
                line: Some(line),
                message: format!(
                    "Cannot redefine variable '{}' in the same scope.",
                    &var.name
                ),
                code: (*var.name).clone(),
            });
        }

        Ok(())
    }

    fn find_var(&mut self, token: &Token) -> Res<Rc<MIRVariable>> {
        for env in self.environments.iter().rev() {
            if let Some(var) = env.get(&token.lexeme) {
                return Ok(Rc::clone(var));
            }
        }

        Err(Error {
            line: Some(token.line),
            message: format!("Variable '{}' is not defined", token.lexeme),
            code: (*token.lexeme).clone(),
        })
    }

    fn find_or_create_var(&mut self, type_: MIRType, name: Token) -> Res<Rc<MIRVariable>> {
        let var = self
            .find_var(&name)
            .unwrap_or_else(|_| self.define_variable(&name, true, type_.clone()));

        if var._type != type_ {
            Err(Error::useless("Break expressions + for body must have same type"))
        } else {
            Ok(var)
        }
    }

    fn get_class_field(
        &mut self,
        object: Expression,
        name: Token,
    ) -> Res<(MIRExpression, Rc<MIRStructMem>)> {
        let object = self.generate_expression(object)?;

        if let MIRType::Struct(struc) = object.get_type() {
            Ok((
                object,
                Rc::clone(
                    struc
                        .borrow()
                        .members
                        .get(&name.lexeme)
                        .ok_or_else(|| Error::useless(
                            "Unknown class member",
                        ))?,
                ),
            ))
        } else {
            Err(Error::useless(
                "Get syntax is only supported on class instances",
            ))
        }
    }

    fn generate_func_args(
        &mut self,
        func_ref: MutRc<MIRFunction>,
        arguments: Vec<Expression>,
    ) -> Res<Vec<MIRExpression>> {
        let func = func_ref.borrow();

        if func.parameters.len() != arguments.len() {
            return Err(Error {
                line: arguments.first().map(|e| e.get_line()).flatten(),
                message: format!(
                    "Incorrect amount of function arguments. (Expected {}; got {})",
                    func.parameters.len(),
                    arguments.len()
                ),
                code: display_slice(&arguments),
            });
        }

        let mut result = Vec::with_capacity(arguments.len());
        for (argument, parameter) in arguments.into_iter().zip(func.parameters.iter()) {
            let argument = self.generate_expression(argument)?;
            if argument.get_type() != parameter._type {
                return Err(Error::new(
                    None,
                    "Call argument is the wrong type",
                    "".to_string(),
                ));
            }
            result.push(argument)
        }

        Ok(result)
    }

    fn begin_scope(&mut self) {
        self.environments.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.environments.pop();
    }

    fn none_const() -> MIRExpression {
        MIRExpression::Literal(Literal::None)
    }

    pub fn new() -> Self {
        let mut generator = MIRGenerator {
            builder: MIRBuilder::new(),
            environments: Vec::with_capacity(5),

            is_in_loop: false,
            current_loop_ret_type: None,
            current_loop_cont_block: None,
        };

        // Global scope
        generator.begin_scope();

        generator
    }
}

pub struct Error {
    pub line: Option<usize>,
    pub message: String,
    pub code: String,
}

impl Error {
    pub fn new_expr(message: String, expr: &Expression) -> Error {
        Error {
            line: expr.get_line(),
            message,
            code: expr.to_string(),
        }
    }

    pub fn new_fn(message: &str, func_sig: &FuncSignature) -> Error {
        Error {
            line: Some(func_sig.name.line),
            message: message.to_string(),
            code: format!("func {}(...)", func_sig.name.lexeme),
        }
    }

    // TODO: Should redo error handling entirely, to be honest...
    pub fn useless(message: &str) -> Error {
        Error {
            line: None,
            message: message.to_string(),
            code: "".to_string(),
        }
    }

    pub fn new(line: Option<usize>, message: &str, code: String) -> Error {
        Error {
            line,
            message: message.to_owned(),
            code,
        }
    }
}
