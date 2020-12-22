//! This file is autogenerated by ast-generator. Do not modify!

use crate::CSTNode;
use parser::SyntaxToken;
use smol_str::SmolStr;
use syntax::{kind::SyntaxKind, language::GelixLang};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct GenericIdent {
    pub cst: CSTNode,
}
impl GenericIdent {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Ident = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn name(&self) -> SmolStr {
        self.cst
            .children_with_tokens()
            .find(|c| {
                c.as_token().map(SyntaxToken::<GelixLang>::kind) == Some(SyntaxKind::Identifier)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .text()
            .clone()
    }
    pub fn type_args(&self) -> impl Iterator<Item = Type> + '_ {
        self.cst.children().filter_map(Type::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Type {
    pub cst: CSTNode,
}
impl Type {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Type = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Literal {
    pub cst: CSTNode,
}
impl Literal {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Literal = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Adt {
    pub cst: CSTNode,
}
impl Adt {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::AdtDecl | SyntaxKind::EnumCase = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.cst
            .children_with_tokens()
            .find(|c| {
                c.as_token()
                    .map(SyntaxToken::<GelixLang>::kind)
                    .as_ref()
                    .map(SyntaxKind::is_token)
                    == Some(true)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .kind()
    }
    pub fn ident(&self) -> GenericIdent {
        self.cst.children().find_map(GenericIdent::cast).unwrap()
    }
    pub fn members(&self) -> impl Iterator<Item = Variable> + '_ {
        self.cst.children().filter_map(Variable::cast)
    }
    pub fn constructors(&self) -> impl Iterator<Item = Constructor> + '_ {
        self.cst.children().filter_map(Constructor::cast)
    }
    pub fn methods(&self) -> impl Iterator<Item = Function> + '_ {
        self.cst.children().filter_map(Function::cast)
    }
    pub fn cases(&self) -> impl Iterator<Item = Adt> + '_ {
        self.cst.children().filter_map(Adt::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Constructor {
    pub cst: CSTNode,
}
impl Constructor {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Constructor = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn sig(&self) -> FunctionSignature {
        self.cst
            .children()
            .find_map(FunctionSignature::cast)
            .unwrap()
    }
    pub fn body(&self) -> Option<Expression> {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::FunctionBody)
            .map(|i| i.children().find_map(Expression::cast))
            .flatten()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Function {
    pub cst: CSTNode,
}
impl Function {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::FunctionDecl | SyntaxKind::Method = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn sig(&self) -> FunctionSignature {
        self.cst
            .children()
            .find_map(FunctionSignature::cast)
            .unwrap()
    }
    pub fn modifiers(&self) -> impl Iterator<Item = SyntaxKind> + '_ {
        self.cst
            .children()
            .filter(|i| i.kind() == SyntaxKind::Modifier)
            .map(|c| {
                c.children_with_tokens().find(|c| {
                    c.as_token()
                        .map(SyntaxToken::kind)
                        .as_ref()
                        .map(SyntaxKind::is_token)
                        == Some(true)
                })
            })
            .flatten()
            .map(|c| c.as_token().unwrap().kind())
    }
    pub fn body(&self) -> Option<Expression> {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::FunctionBody)
            .map(|i| i.children().find_map(Expression::cast))
            .flatten()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct FunctionSignature {
    pub cst: CSTNode,
}
impl FunctionSignature {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::FunctionSignature = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn ident(&self) -> GenericIdent {
        self.cst.children().find_map(GenericIdent::cast).unwrap()
    }
    pub fn ret_type(&self) -> Type {
        self.cst.children().find_map(Type::cast).unwrap()
    }
    pub fn parameters(&self) -> impl Iterator<Item = Parameter> + '_ {
        self.cst.children().filter_map(Parameter::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Parameter {
    pub cst: CSTNode,
}
impl Parameter {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Parameter = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn name(&self) -> SmolStr {
        self.cst
            .children_with_tokens()
            .find(|c| {
                c.as_token().map(SyntaxToken::<GelixLang>::kind) == Some(SyntaxKind::Identifier)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .text()
            .clone()
    }
    pub fn _type(&self) -> Type {
        self.cst.children().find_map(Type::cast).unwrap()
    }
    pub fn maybe_type(&self) -> Option<Type> {
        self.cst.children().find_map(Type::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Import {
    pub cst: CSTNode,
}
impl Import {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Import = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn parts(&self) -> impl Iterator<Item = SmolStr> + '_ {
        self.cst
            .children_with_tokens()
            .filter(|c| {
                c.as_token().map(SyntaxToken::<GelixLang>::kind) == Some(SyntaxKind::Identifier)
            })
            .map(|c| c.as_token().unwrap().text().clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IfaceImpl {
    pub cst: CSTNode,
}
impl IfaceImpl {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::ImplDecl = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn implementor(&self) -> Type {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Implementor)
            .unwrap()
            .children()
            .find_map(Type::cast)
            .unwrap()
    }
    pub fn iface(&self) -> Type {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Implementing)
            .unwrap()
            .children()
            .find_map(Type::cast)
            .unwrap()
    }
    pub fn methods(&self) -> impl Iterator<Item = Function> + '_ {
        self.cst.children().filter_map(Function::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Binary(Binary),
    Prefix(Prefix),
    Call(Call),
    Get(Get),
    GetStatic(GetStatic),
    Block(Block),
    If(IfExpr),
    For(ForExpr),
    Return(Return),
    Break(Break),
    When(When),
    Variable(GenericIdent),
    VarDef(Variable),
    Grouping(Grouping),
    Literal(Literal),
}

impl Expression {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if node.kind() == SyntaxKind::BinaryExpr {
            return Some(Self::Binary(Binary::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::PrefixExpr {
            return Some(Self::Prefix(Prefix::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::CallExpr {
            return Some(Self::Call(Call::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::GetExpr {
            return Some(Self::Get(Get::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::GetStaticExpr {
            return Some(Self::GetStatic(GetStatic::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::Block {
            return Some(Self::Block(Block::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::IfExpr {
            return Some(Self::If(IfExpr::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::ForExpr {
            return Some(Self::For(ForExpr::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::Return {
            return Some(Self::Return(Return::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::Break {
            return Some(Self::Break(Break::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::When {
            return Some(Self::When(When::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::Ident {
            return Some(Self::Variable(GenericIdent::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::Variable {
            return Some(Self::VarDef(Variable::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::Grouping {
            return Some(Self::Grouping(Grouping::cast(node).unwrap()));
        }
        if node.kind() == SyntaxKind::Literal {
            return Some(Self::Literal(Literal::cast(node).unwrap()));
        }
        None
    }

    pub fn cst(&self) -> &CSTNode {
        match self {
            Self::Binary(inner) => &inner.cst,
            Self::Prefix(inner) => &inner.cst,
            Self::Call(inner) => &inner.cst,
            Self::Get(inner) => &inner.cst,
            Self::GetStatic(inner) => &inner.cst,
            Self::Block(inner) => &inner.cst,
            Self::If(inner) => &inner.cst,
            Self::For(inner) => &inner.cst,
            Self::Return(inner) => &inner.cst,
            Self::Break(inner) => &inner.cst,
            Self::When(inner) => &inner.cst,
            Self::Variable(inner) => &inner.cst,
            Self::VarDef(inner) => &inner.cst,
            Self::Grouping(inner) => &inner.cst,
            Self::Literal(inner) => &inner.cst,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Variable {
    pub cst: CSTNode,
}
impl Variable {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Variable = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.cst
            .children_with_tokens()
            .find(|c| {
                c.as_token()
                    .map(SyntaxToken::<GelixLang>::kind)
                    .as_ref()
                    .map(SyntaxKind::is_token)
                    == Some(true)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .kind()
    }
    pub fn name(&self) -> SmolStr {
        self.cst
            .children_with_tokens()
            .find(|c| {
                c.as_token().map(SyntaxToken::<GelixLang>::kind) == Some(SyntaxKind::Identifier)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .text()
            .clone()
    }
    pub fn _type(&self) -> Option<Type> {
        self.cst.children().find_map(Type::cast)
    }
    pub fn initializer(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Initializer)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
    pub fn maybe_initializer(&self) -> Option<Expression> {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Initializer)
            .map(|i| i.children().find_map(Expression::cast))
            .flatten()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Grouping {
    pub cst: CSTNode,
}
impl Grouping {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Grouping = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn inner(&self) -> Expression {
        self.cst.children().find_map(Expression::cast).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Binary {
    pub cst: CSTNode,
}
impl Binary {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::BinaryExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn operator(&self) -> SyntaxKind {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Operator)
            .unwrap()
            .children_with_tokens()
            .find(|c| {
                c.as_token()
                    .map(SyntaxToken::<GelixLang>::kind)
                    .as_ref()
                    .map(SyntaxKind::is_token)
                    == Some(true)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .kind()
    }
    pub fn left(&self) -> Expression {
        self.cst
            .children()
            .next()
            .map(Expression::cast)
            .unwrap()
            .unwrap()
    }
    pub fn right(&self) -> Expression {
        self.cst
            .children()
            .skip(2)
            .next()
            .map(Expression::cast)
            .unwrap()
            .unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Prefix {
    pub cst: CSTNode,
}
impl Prefix {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::PrefixExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn operator(&self) -> SyntaxKind {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Operator)
            .unwrap()
            .children_with_tokens()
            .find(|c| {
                c.as_token()
                    .map(SyntaxToken::<GelixLang>::kind)
                    .as_ref()
                    .map(SyntaxKind::is_token)
                    == Some(true)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .kind()
    }
    pub fn right(&self) -> Expression {
        self.cst
            .children()
            .next()
            .map(Expression::cast)
            .unwrap()
            .unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Call {
    pub cst: CSTNode,
}
impl Call {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::CallExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn callee(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Callee)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
    pub fn args(&self) -> impl Iterator<Item = Expression> + '_ {
        self.cst
            .children()
            .filter(|i| i.kind() == SyntaxKind::CallArgument)
            .map(|i| i.children().find_map(Expression::cast).unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Get {
    pub cst: CSTNode,
}
impl Get {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::GetExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn callee(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Callee)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
    pub fn property(&self) -> GenericIdent {
        self.cst.children().find_map(GenericIdent::cast).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct GetStatic {
    pub cst: CSTNode,
}
impl GetStatic {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::GetStaticExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn callee(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::Callee)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
    pub fn property(&self) -> SmolStr {
        self.cst
            .children_with_tokens()
            .find(|c| {
                c.as_token().map(SyntaxToken::<GelixLang>::kind) == Some(SyntaxKind::Identifier)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .text()
            .clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Block {
    pub cst: CSTNode,
}
impl Block {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::Block = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn expressions(&self) -> impl Iterator<Item = Expression> + '_ {
        self.cst.children().filter_map(Expression::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IfExpr {
    pub cst: CSTNode,
}
impl IfExpr {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::IfExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn condition(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprCondition)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
    pub fn then_branch(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprBody)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
    pub fn else_branch(&self) -> Option<Expression> {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprElse)
            .map(|i| i.children().find_map(Expression::cast))
            .flatten()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ForExpr {
    pub cst: CSTNode,
}
impl ForExpr {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::ForExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn condition(&self) -> Option<Expression> {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprCondition)
            .map(|i| i.children().find_map(Expression::cast))
            .flatten()
    }
    pub fn iter_cond(&self) -> Option<ForIterCond> {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ForIterCond)
            .map(|i| i.children().find_map(ForIterCond::cast))
            .flatten()
    }
    pub fn body(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprBody)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
    pub fn else_branch(&self) -> Option<Expression> {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprElse)
            .map(|i| i.children().find_map(Expression::cast))
            .flatten()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ForIterCond {
    pub cst: CSTNode,
}
impl ForIterCond {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::ForIterCond = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn name(&self) -> SmolStr {
        self.cst
            .children_with_tokens()
            .find(|c| {
                c.as_token().map(SyntaxToken::<GelixLang>::kind) == Some(SyntaxKind::Identifier)
            })
            .unwrap()
            .as_token()
            .unwrap()
            .text()
            .clone()
    }
    pub fn iterator(&self) -> Expression {
        self.cst.children().find_map(Expression::cast).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Return {
    pub cst: CSTNode,
}
impl Return {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::ReturnExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn value(&self) -> Option<Expression> {
        self.cst.children().find_map(Expression::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Break {
    pub cst: CSTNode,
}
impl Break {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::BreakExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn value(&self) -> Option<Expression> {
        self.cst.children().find_map(Expression::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct When {
    pub cst: CSTNode,
}
impl When {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::WhenExpr = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn condition(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprCondition)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
    pub fn branches(&self) -> impl Iterator<Item = WhenBranch> + '_ {
        self.cst.children().filter_map(WhenBranch::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct WhenBranch {
    pub cst: CSTNode,
}
impl WhenBranch {
    #[allow(unused)]
    pub fn cast(node: CSTNode) -> Option<Self> {
        if let SyntaxKind::WhenBranch = node.kind() {
            Some(Self { cst: node })
        } else {
            None
        }
    }

    pub fn condition(&self) -> Option<Expression> {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprCondition)
            .map(|i| i.children().find_map(Expression::cast))
            .flatten()
    }
    pub fn branch(&self) -> Expression {
        self.cst
            .children()
            .find(|i| i.kind() == SyntaxKind::ExprBody)
            .unwrap()
            .children()
            .find_map(Expression::cast)
            .unwrap()
    }
}
