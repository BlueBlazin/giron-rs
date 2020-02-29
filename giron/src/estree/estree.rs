use crate::errors::errors::{Error, ErrorType};
use crate::errors::result::Result;
use crate::span::span::{Marker, Span};
use crate::token::value::Value;
use serde::Serialize;

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct CatchClause {
    pub param: Option<Binding>,
    pub body: Box<Node>, // BlockStatement
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum ImportDeclarationSpecifier {
    ImportDefaultSpecifier {
        local: Identifier,
    },
    ImportNamespaceSpecifier {
        local: Identifier,
    },
    ImportSpecifier {
        local: Identifier,
        imported: Identifier,
    },
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct ImportDeclaration {
    pub specifiers: Vec<ImportDeclarationSpecifier>,
    pub source: Literal,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct ExportSpecifier {
    pub exported: Identifier,
    pub local: Identifier,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct Property {
    pub key: PropertyKey,
    pub computed: bool,
    pub value: Option<PropertyValue>,
    pub kind: &'static str,
    pub method: bool,
    pub shorthand: bool,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct ClassBody {
    pub body: Vec<MethodDefinition>,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct Regex {
    pub pattern: String,
    pub flags: Vec<char>,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct SpreadElement {
    pub argument: Node,
    #[serde(skip)]
    pub span: Span,
}

// export type ArrayExpressionElement = Expression | SpreadElement | null;
#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum ArrayExpressionElement {
    Expression(Node),
    SpreadElement(SpreadElement),
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct ArrayPattern {
    pub elements: Vec<Option<ArrayPatternElement>>,
    #[serde(skip)]
    pub span: Span,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct ObjectPattern {
    pub properties: Vec<ObjectPatternProperty>,
    #[serde(skip)]
    pub span: Span,
}

#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum Binding {
    BindingIdentifier(BindingIdentifier),
    ArrayPattern(ArrayPattern),
    ObjectPattern(ObjectPattern),
    Expression(Box<Node>),
}

impl Binding {
    pub fn to_node(self) -> Result<Node> {
        match self {
            Binding::BindingIdentifier(BindingIdentifier { name, span }) => {
                Ok(Node::BindingIdentifier { name, span })
            }
            Binding::ArrayPattern(ArrayPattern { elements, span }) => Ok(Node::ArrayPattern {
                elements,
                assign_target_type: "simple",
                span,
            }),
            Binding::ObjectPattern(ObjectPattern { properties, span }) => Ok(Node::ObjectPattern {
                properties,
                assign_target_type: "simple",
                span,
            }),
            _ => panic!(),
        }
    }

    pub fn to_id(self) -> Result<Identifier> {
        match self {
            Binding::BindingIdentifier(BindingIdentifier { name, span }) => {
                Ok(Identifier { name, span })
            }
            _ => panic!(),
        }
    }
}

// export type ArrayPatternElement = AssignmentPattern | BindingIdentifier | BindingPattern | RestElement | null;
#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum ArrayPatternElement {
    AssignmentPattern {
        left: Binding,
        right: Node,
    },
    BindingIdentifier {
        name: String,
    },
    ArrayPattern {
        elements: Vec<Option<ArrayPatternElement>>,
    },
    ObjectPattern {
        properties: Vec<ObjectPatternProperty>,
    },
    RestElement {
        argument: Binding,
    },
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct RestElement {
    pub argument: Binding,
}

#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum ArrayAssignmentPatternElement {
    Expression(Node),
    RestElement(RestElement),
}

#[derive(Serialize, Debug, Clone)]
#[serde(tag = "type")]
pub struct Identifier {
    pub name: String,
    #[serde(skip)]
    pub span: Span,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct Literal {
    pub value: Value,
    pub raw: String,
    #[serde(skip)]
    pub span: Span,
}

// export type PropertyKey = Identifier | Literal | Expression;
#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum PropertyKey {
    Identifier(Identifier),
    Literal(Literal),
    Expression(Node),
}

impl PropertyKey {
    pub fn as_node(self) -> Node {
        match self {
            PropertyKey::Identifier(Identifier { name, span }) => Node::Identifier {
                name,
                assign_target_type: "invalid",
                span,
            },
            PropertyKey::Literal(Literal { value, raw, span }) => Node::Literal {
                value,
                raw,
                assign_target_type: "invalid",
                span,
            },
            PropertyKey::Expression(node) => node,
        }
    }
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct AssignmentPattern {
    pub left: Binding,
    pub right: Node,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct AsyncFunctionExpression {
    pub id: Option<Box<Node>>, // Identifier | null
    pub params: Vec<FunctionParameter>,
    pub body: Box<Node>, // BlockStatement | Expression
    pub generator: bool,
    pub expression: bool,
    pub r#async: bool,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
#[serde(rename = "Identifier")]
pub struct BindingIdentifier {
    pub name: String,
    #[serde(skip)]
    pub span: Span,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct FunctionExpression {
    pub id: Option<Box<Node>>, // Identifier | null
    pub params: Vec<FunctionParameter>,
    pub body: Box<Node>, // BlockStatement
    pub generator: bool,
    pub expression: bool,
    pub r#async: bool,
}

// export type PropertyValue = AssignmentPattern | AsyncFunctionExpression | BindingIdentifier | BindingPattern | FunctionExpression;
#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum PropertyValue {
    AssignmentPattern(AssignmentPattern),
    AsyncFunctionExpression(AsyncFunctionExpression),
    BindingIdentifier(BindingIdentifier),
    BindingPattern(BindingPattern),
    FunctionExpression(FunctionExpression),
    Expression(Node),
}

// export type ObjectPatternProperty = Property | RestElement;
#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum ObjectPatternProperty {
    Property {
        key: PropertyKey,
        computed: bool,
        value: Option<PropertyValue>,
        kind: &'static str,
        method: bool,
        shorthand: bool,
    },
    RestElement {
        argument: Binding,
    },
}

// export type BindingPattern = ArrayPattern | ObjectPattern;
#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum BindingPattern {
    ArrayPattern {
        elements: Vec<Option<ArrayPatternElement>>,
    },
    ObjectPattern {
        properties: Vec<ObjectPatternProperty>,
    },
}

// export type FunctionParameter = AssignmentPattern | BindingIdentifier | BindingPattern;
#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum FunctionParameter {
    AssignmentPattern {
        left: Binding,
        right: Node,
    },
    BindingIdentifier {
        name: String,
    },
    ArrayPattern {
        elements: Vec<Option<ArrayPatternElement>>,
    },
    ObjectPattern {
        properties: Vec<ObjectPatternProperty>,
    },
    RestElement {
        argument: Binding,
    },
}

// export type ArgumentListElement = Expression | SpreadElement;
#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum ArgumentListElement {
    Expression(Node),
    SpreadElement(SpreadElement),
}

impl ArgumentListElement {
    pub fn as_node(self) -> Result<Node> {
        match self {
            ArgumentListElement::Expression(node) => Ok(node),
            ArgumentListElement::SpreadElement(SpreadElement { argument, span }) => {
                Ok(Node::SpreadElement {
                    argument: Box::from(argument),
                    span,
                })
            }
        }
    }
}

// export type ObjectExpressionProperty = Property | SpreadElement;
#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum ObjectExpressionProperty {
    Property {
        key: PropertyKey,
        computed: bool,
        value: Option<PropertyValue>,
        kind: &'static str,
        method: bool,
        shorthand: bool,
        span: Span,
    },
    SpreadElement {
        argument: Node,
        span: Span,
    },
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct MethodDefinition {
    pub key: Node, // Expression
    pub computed: bool,
    pub value: Option<FunctionExpression>,
    pub kind: &'static str,
    pub r#static: bool,
}

#[derive(Serialize, Debug)]
#[serde(untagged)]
pub enum MethodDefinitionValue {
    AsyncFunctionExpression(AsyncFunctionExpression),
    FunctionExpression(FunctionExpression),
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct TemplateLiteral {
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Node>,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct TemplateElement {
    pub value: TemplateElementValue,
    pub tail: bool,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct TemplateElementValue {
    pub cooked: String,
    pub raw: String,
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct SwitchCase {
    pub test: Option<Node>,    // Expression | null
    pub consequent: Vec<Node>, // Statement[]
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct VariableDeclarator {
    pub id: Binding,
    pub init: Option<Node>, // Expression | null
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub struct BlockStatement {
    pub body: Vec<Node>, // Statement
}

#[derive(Serialize, Debug)]
#[serde(tag = "type")]
pub enum Node {
    ArrayExpression {
        elements: Vec<Option<ArrayExpressionElement>>,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    ArrayPattern {
        elements: Vec<Option<ArrayPatternElement>>,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    #[serde(rename = "ArrayPattern")]
    ArrayAssignmentPattern {
        elements: Vec<Option<ArrayAssignmentPatternElement>>,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    ArrowFunctionExpression {
        id: Option<Box<Node>>, // Identifier | null
        params: Vec<FunctionParameter>,
        body: Box<Node>, // BlockStatement | Expression
        generator: bool,
        expression: bool,
        r#async: bool,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    AssignmentExpression {
        operator: &'static str,
        left: Box<Node>,  // Expression
        right: Box<Node>, // Expression
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    AssignmentPattern {
        left: Binding,    // Expression
        right: Box<Node>, // Expression
        #[serde(skip)]
        span: Span,
    },
    AssignmentRestElement {
        argument: Box<Node>,
        #[serde(skip)]
        span: Span,
    },
    AsyncArrowFunctionExpression {
        id: Option<Box<Node>>, // Identifier | null
        params: Vec<FunctionParameter>,
        body: Box<Node>, // BlockStatement | Expression
        generator: bool,
        expression: bool,
        r#async: bool,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    AsyncFunctionExpression {
        id: Option<Box<Node>>, // Identifier | null
        params: Vec<FunctionParameter>,
        body: Box<Node>, // BlockStatement | Expression
        generator: bool,
        expression: bool,
        r#async: bool,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    AwaitExpression {
        argument: Box<Node>, // Expression
        #[serde(skip)]
        span: Span,
    },
    BinaryExpression {
        operator: String,
        left: Box<Node>,  // Expression
        right: Box<Node>, // Expression
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    #[serde(rename = "Identifier")]
    BindingIdentifier {
        name: String,
        #[serde(skip)]
        span: Span,
    },
    LogicalExpression {
        operator: &'static str,
        left: Box<Node>,  // Expression
        right: Box<Node>, // Expression
        #[serde(skip)]
        span: Span,
    },
    CallExpression {
        callee: Box<Node>, // Expression | Import
        arguments: Vec<ArgumentListElement>,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    ClassExpression {
        id: Option<Box<Node>>,          // Identifier | null
        super_class: Option<Box<Node>>, // Identifier | null
        body: ClassBody,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    ComputedMemberExpression {
        computed: bool,
        object: Box<Node>,   // Expression
        property: Box<Node>, // Expression
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    ConditionalExpression {
        test: Box<Node>,
        consequent: Box<Node>,
        alternate: Box<Node>,
        #[serde(skip)]
        span: Span,
    },
    Identifier {
        name: String,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    FunctionExpression {
        id: Option<Box<Node>>, // Identifier | null
        params: Vec<FunctionParameter>,
        body: Box<Node>, // BlockStatement
        generator: bool,
        expression: bool,
        r#async: bool,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    Literal {
        value: Value,
        raw: String,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    NewExpression {
        callee: Box<Node>, // Expression
        arguments: Vec<ArgumentListElement>,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    ObjectExpression {
        properties: Vec<ObjectExpressionProperty>,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    ObjectPattern {
        properties: Vec<ObjectPatternProperty>,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    RegexLiteral {
        value: String,
        raw: String,
        regex: Regex,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    SequenceExpression {
        expressions: Vec<Node>, // Expression[]
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    StaticMemberExpression {
        computed: bool,
        object: Box<Node>,   // Expression
        property: Box<Node>, // Expression
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    TaggedTemplateExpression {
        tag: Box<Node>, // Expression
        quasi: TemplateLiteral,
        #[serde(skip)]
        span: Span,
    },
    ThisExpression {
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    UnaryExpression {
        operator: String,
        argument: Box<Node>, // Expression
        prefix: bool,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    UpdateExpression {
        operator: String,
        argument: Box<Node>, // Expression
        prefix: bool,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    YieldExpression {
        argument: Option<Box<Node>>, // Expression | null
        delegate: bool,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    ArrowParameterPlaceHolder {
        params: Vec<FunctionParameter>,
        r#async: bool,
        #[serde(skip)]
        span: Span,
    },
    RestElement {
        argument: Binding,
        #[serde(skip)]
        span: Span,
    },
    SpreadElement {
        argument: Box<Node>,
        #[serde(skip)]
        span: Span,
    },
    Super {
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    TemplateLiteral {
        quasis: Vec<TemplateElement>,
        expressions: Vec<Node>,
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    MetaProperty {
        meta: Box<Node>,     // Identifier
        property: Box<Node>, // Identifier
        #[serde(skip)]
        assign_target_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
    // ****************************************************
    // Statements & Declarations
    // ****************************************************
    AsyncFunctionDeclaration {
        id: Option<Box<Node>>, // Identifier | null
        params: Vec<FunctionParameter>,
        body: Box<Node>, // BlockStatement
        generator: bool,
        expression: bool,
        r#async: bool,
        #[serde(skip)]
        span: Span,
    },
    BreakStatement {
        label: Option<Box<Node>>, // Identifier | null
        #[serde(skip)]
        span: Span,
    },
    ContinueStatement {
        label: Option<Box<Node>>, // Identifier | null
        #[serde(skip)]
        span: Span,
    },
    DebuggerStatement {
        #[serde(skip)]
        span: Span,
    },
    DoWhileStatement {
        body: Box<Node>, // Statement
        test: Box<Node>, // Expression
        #[serde(skip)]
        span: Span,
    },
    EmptyStatement {
        #[serde(skip)]
        span: Span,
    },
    ExpressionStatement {
        expression: Box<Node>, // Expression
        #[serde(skip)]
        span: Span,
    },
    Directive {
        expression: Box<Node>, // Expression
        directive: String,
        #[serde(skip)]
        span: Span,
    },
    ForStatement {
        init: Option<Box<Node>>,   // Expression | null
        test: Option<Box<Node>>,   // Expression | null
        update: Option<Box<Node>>, // Expression | null
        body: Box<Node>,           // Statement
        #[serde(skip)]
        span: Span,
    },
    ForInStatement {
        left: Box<Node>,  // Expression
        right: Box<Node>, // Expression
        body: Box<Node>,  // Statement
        each: bool,
        #[serde(skip)]
        span: Span,
    },
    ForOfStatement {
        left: Box<Node>,  // Expression
        right: Box<Node>, // Expression
        body: Box<Node>,  // Statement
        #[serde(skip)]
        span: Span,
    },
    FunctionDeclaration {
        id: Option<Box<Node>>, // Identifier | null
        params: Vec<FunctionParameter>,
        body: Box<Node>, // BlockStatement
        generator: bool,
        expression: bool,
        r#async: bool,
        #[serde(skip)]
        span: Span,
    },
    IfStatement {
        test: Box<Node>,              // Expression
        consequent: Box<Node>,        // Statement
        alternate: Option<Box<Node>>, // Statement | null
        #[serde(skip)]
        span: Span,
    },
    ReturnStatement {
        argument: Option<Box<Node>>, // Expression | null
        #[serde(skip)]
        span: Span,
    },
    SwitchStatement {
        discriminant: Box<Node>, // Expression
        cases: Vec<SwitchCase>,
        #[serde(skip)]
        span: Span,
    },
    ThrowStatement {
        argument: Box<Node>, // Expression
        #[serde(skip)]
        span: Span,
    },
    TryStatement {
        block: Box<Node>,             // BlockStatement
        handler: Option<CatchClause>, // CatchClause | null
        finalizer: Option<Box<Node>>, // BlockStatement | null
        #[serde(skip)]
        span: Span,
    },
    VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        kind: &'static str,
        #[serde(skip)]
        span: Span,
    },
    WhileStatement {
        test: Box<Node>, // Expression
        body: Box<Node>, // Statement
        #[serde(skip)]
        span: Span,
    },
    WithStatement {
        object: Box<Node>, // Expression
        body: Box<Node>,   // Statement
        #[serde(skip)]
        span: Span,
    },
    BlockStatement {
        body: Vec<Node>, // Statement
        #[serde(skip)]
        span: Span,
    },
    ClassDeclaration {
        id: Option<Box<Node>>, // Identifier | null
        #[serde(rename = "superClass")]
        super_class: Option<Box<Node>>, // Expression | null
        body: ClassBody,
        #[serde(skip)]
        span: Span,
    },
    ExportAllDeclaration {
        source: Literal,
        #[serde(skip)]
        span: Span,
    },
    ExportDefaultDeclaration {
        // AnonymousDefaultExportedFunctionDeclaration | FunctionDeclaration | AnonymousDefaultExportedClassDeclaration | ClassDeclaration | Expression
        declaration: Box<Node>,
        #[serde(skip)]
        span: Span,
    },
    ExportNamedDeclaration {
        declaration: Option<Box<Node>>, // Declaration | null
        specifiers: Vec<ExportSpecifier>,
        source: Option<Literal>,
        #[serde(skip)]
        span: Span,
    },
    ImportDeclaration {
        specifiers: Vec<ImportDeclarationSpecifier>,
        source: Literal,
        #[serde(skip)]
        span: Span,
    },
    ImportDefaultSpecifier {
        local: Identifier,
        #[serde(skip)]
        span: Span,
    },
    ImportNamespaceSpecifier {
        local: Identifier,
        #[serde(skip)]
        span: Span,
    },
    ImportSpecifier {
        local: Identifier,
        imported: Identifier,
        #[serde(skip)]
        span: Span,
    },
    LabeledStatement {
        label: Identifier,
        body: Box<Node>, // Statement
        #[serde(skip)]
        span: Span,
    },
    Module {
        body: Vec<Node>, // StatementListItem[]
        #[serde(rename = "sourceType")]
        source_type: String,
        #[serde(skip)]
        span: Span,
    },
    // ****************************************************
    // Scripts & Modules
    // ****************************************************
    Program {
        body: Vec<Node>, // StatementListItem[]
        #[serde(rename = "sourceType")]
        source_type: &'static str,
        #[serde(skip)]
        span: Span,
    },
}

impl Node {
    pub fn expr_span(self) -> Span {
        match self {
            Node::ArrayExpression { span, .. } => span,
            Node::ArrayPattern { span, .. } => span,
            Node::ArrayAssignmentPattern { span, .. } => span,
            Node::ArrowFunctionExpression { span, .. } => span,
            Node::AssignmentExpression { span, .. } => span,
            Node::AssignmentPattern { span, .. } => span,
            Node::AssignmentRestElement { span, .. } => span,
            Node::AsyncArrowFunctionExpression { span, .. } => span,
            Node::AsyncFunctionExpression { span, .. } => span,
            Node::AwaitExpression { span, .. } => span,
            Node::BinaryExpression { span, .. } => span,
            Node::BindingIdentifier { span, .. } => span,
            Node::LogicalExpression { span, .. } => span,
            Node::CallExpression { span, .. } => span,
            Node::ClassExpression { span, .. } => span,
            Node::ComputedMemberExpression { span, .. } => span,
            Node::ConditionalExpression { span, .. } => span,
            Node::Identifier { span, .. } => span,
            Node::FunctionExpression { span, .. } => span,
            Node::Literal { span, .. } => span,
            Node::NewExpression { span, .. } => span,
            Node::ObjectExpression { span, .. } => span,
            Node::ObjectPattern { span, .. } => span,
            Node::RegexLiteral { span, .. } => span,
            Node::SequenceExpression { span, .. } => span,
            Node::StaticMemberExpression { span, .. } => span,
            Node::TaggedTemplateExpression { span, .. } => span,
            Node::ThisExpression { span, .. } => span,
            Node::UnaryExpression { span, .. } => span,
            Node::UpdateExpression { span, .. } => span,
            Node::YieldExpression { span, .. } => span,
            Node::ArrowParameterPlaceHolder { span, .. } => span,
            Node::RestElement { span, .. } => span,
            Node::SpreadElement { span, .. } => span,
            Node::Super { span, .. } => span,
            Node::TemplateLiteral { span, .. } => span,
            Node::MetaProperty { span, .. } => span,
            _ => panic!(),
        }
    }

    pub fn assign_target_type(&self) -> &'static str {
        match self {
            Node::ArrayExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::ArrayPattern {
                assign_target_type, ..
            } => assign_target_type,
            Node::ArrowFunctionExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::AssignmentExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::AsyncArrowFunctionExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::AsyncFunctionExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::BinaryExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::CallExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::ClassExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::ComputedMemberExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::ArrayAssignmentPattern { .. } => "simple",
            Node::Identifier {
                assign_target_type, ..
            } => assign_target_type,
            Node::FunctionExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::Literal {
                assign_target_type, ..
            } => assign_target_type,
            Node::NewExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::ObjectExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::ObjectPattern {
                assign_target_type, ..
            } => assign_target_type,
            Node::RegexLiteral {
                assign_target_type, ..
            } => assign_target_type,
            Node::SequenceExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::StaticMemberExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::ThisExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::UnaryExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::UpdateExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::YieldExpression {
                assign_target_type, ..
            } => assign_target_type,
            Node::Super {
                assign_target_type, ..
            } => assign_target_type,
            Node::TemplateLiteral {
                assign_target_type, ..
            } => assign_target_type,
            Node::MetaProperty {
                assign_target_type, ..
            } => assign_target_type,
            _ => "invalid",
        }
    }

    fn to_binding(self) -> Result<Binding> {
        let node = self.to_binding_pattern()?;
        match node {
            Node::BindingIdentifier { name, span } => {
                Ok(Binding::BindingIdentifier(BindingIdentifier { name, span }))
            }
            Node::ArrayPattern { elements, span, .. } => {
                Ok(Binding::ArrayPattern(ArrayPattern { elements, span }))
            }
            Node::ObjectPattern {
                properties, span, ..
            } => Ok(Binding::ObjectPattern(ObjectPattern { properties, span })),
            _ => Ok(Binding::Expression(Box::from(node))),
        }
    }

    fn to_strict_binding(self, marker: Marker) -> Result<Binding> {
        let node = self.to_binding_pattern()?;
        match node {
            Node::BindingIdentifier { name, span } => {
                Ok(Binding::BindingIdentifier(BindingIdentifier { name, span }))
            }
            Node::ArrayPattern { elements, span, .. } => {
                Ok(Binding::ArrayPattern(ArrayPattern { elements, span }))
            }
            Node::ObjectPattern {
                properties, span, ..
            } => Ok(Binding::ObjectPattern(ObjectPattern { properties, span })),
            _ => Node::error(ErrorType::InvalidOrUnexpectedToken, marker),
        }
    }

    pub fn to_assignment_pattern(self) -> Result<Node> {
        match self {
            Node::ArrayExpression { elements, span, .. } => {
                Node::array_to_assign_pattern(elements, span)
            }
            Node::ObjectExpression {
                properties, span, ..
            } => Node::object_to_assign_pattern(properties, span),
            _ => Ok(self),
        }
    }

    fn array_to_assign_pattern(
        elements: Vec<Option<ArrayExpressionElement>>,
        span: Span,
    ) -> Result<Node> {
        let mut pat_elems = vec![];
        let last_idx = elements.len();
        for (i, element) in elements.into_iter().enumerate() {
            match element {
                Some(ArrayExpressionElement::Expression(expr)) => {
                    let element = expr.as_assignment_element()?;
                    pat_elems.push(Some(ArrayAssignmentPatternElement::Expression(element)));
                }
                Some(ArrayExpressionElement::SpreadElement(SpreadElement { argument, span })) => {
                    if i != last_idx {
                        return Node::error(ErrorType::ParamAfterRest, span.start);
                    }
                    let argument = match argument {
                        Node::ArrayExpression { span, .. }
                        | Node::ObjectExpression { span, .. } => {
                            return Node::error(ErrorType::InvalidRestBindingPattern, span.start)
                        }
                        _ => argument,
                    };
                    if argument.assign_target_type() != "simple" {
                        return Node::error(ErrorType::InvalidLhsInAssignment, span.start);
                    }
                    pat_elems.push(Some(ArrayAssignmentPatternElement::RestElement(
                        RestElement {
                            argument: argument.to_binding()?,
                        },
                    )))
                }
                None => pat_elems.push(None),
            }
        }
        Ok(Node::ArrayAssignmentPattern {
            elements: pat_elems,
            assign_target_type: "simple",
            span,
        })
    }

    fn as_assignment_element(self) -> Result<Node> {
        let element = self.to_assignment_pattern()?;
        match element {
            Node::AssignmentExpression {
                operator: "=",
                left,
                right,
                span,
                ..
            } => {
                if left.assign_target_type() != "simple" {
                    Node::error(ErrorType::InvalidLhsInAssignment, span.start)
                } else {
                    Ok(Node::AssignmentPattern {
                        left: Binding::Expression(left),
                        right,
                        span,
                    })
                }
            }
            _ => {
                if element.assign_target_type() != "simple" {
                    Node::error(ErrorType::InvalidLhsInAssignment, element.expr_span().start)
                } else {
                    Ok(element)
                }
            }
        }
    }

    fn object_to_assign_pattern(
        properties: Vec<ObjectExpressionProperty>,
        span: Span,
    ) -> Result<Node> {
        // AssignmentProperty[Yield, Await]:
        //     IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await](opt)
        //     PropertyName[?Yield, ?Await] ":" AssignmentElement[?Yield, ?Await]
        // AssignmentElement[Yield, Await]:
        //     DestructuringAssignmentTarget[?Yield, ?Await] Initializer[+In, ?Yield, ?Await](opt)
        // AssignmentRestElement[Yield, Await]:
        //     "..." DestructuringAssignmentTarget[?Yield, ?Await]
        // DestructuringAssignmentTarget[Yield, Await]:
        //     LeftHandSideExpression[?Yield, ?Await]
        let mut pat_props = vec![];
        if properties.len() == 0 {
            return Ok(Node::ObjectPattern {
                properties: pat_props,
                assign_target_type: "simple",
                span,
            });
        }
        let last_idx = properties.len() - 1;
        for (i, property) in properties.into_iter().enumerate() {
            match property {
                ObjectExpressionProperty::Property {
                    key,
                    computed,
                    value,
                    kind,
                    method,
                    shorthand,
                    span,
                } => {
                    let value = match value {
                        Some(PropertyValue::Expression(expr)) => Ok(Some(
                            PropertyValue::Expression(expr.as_assignment_element()?),
                        )),
                        None => Ok(None),
                        _ => Node::error(ErrorType::InvalidOrUnexpectedToken, span.start),
                    }?;
                    pat_props.push(ObjectPatternProperty::Property {
                        key,
                        computed,
                        value: value,
                        kind,
                        method,
                        shorthand,
                    });
                }
                ObjectExpressionProperty::SpreadElement { argument, span } => {
                    if i != last_idx {
                        return Node::error(ErrorType::ParamAfterRest, span.start);
                    }
                    let argument = match argument {
                        Node::ArrayExpression { span, .. }
                        | Node::ObjectExpression { span, .. } => {
                            return Node::error(ErrorType::InvalidLhsInAssignment, span.start);
                        }
                        _ => argument,
                    };
                    if argument.assign_target_type() != "simple" {
                        return Node::error(ErrorType::InvalidLhsInAssignment, span.start);
                    }
                    pat_props.push(ObjectPatternProperty::RestElement {
                        argument: argument.to_binding()?,
                    });
                }
            }
        }
        Ok(Node::ObjectPattern {
            properties: pat_props,
            assign_target_type: "simple",
            span,
        })
    }

    pub fn to_binding_pattern(self) -> Result<Node> {
        match self {
            Node::Identifier { name, span, .. } => Ok(Node::BindingIdentifier { name, span }),
            Node::ArrayExpression { elements, span, .. } => Node::array_to_pattern(elements, span),
            Node::ObjectExpression {
                properties, span, ..
            } => Node::object_to_pattern(properties, span),
            Node::AssignmentExpression {
                operator: "=",
                left,
                right,
                span,
                ..
            } => Ok(Node::AssignmentPattern {
                left: left.to_strict_binding(span.clone().start)?,
                right,
                span,
            }),
            Node::SpreadElement { argument, span } => Ok(Node::RestElement {
                argument: (*argument).to_strict_binding(span.clone().start)?,
                span,
            }),
            _ => Ok(self),
        }
    }

    fn array_to_pattern(elements: Vec<Option<ArrayExpressionElement>>, span: Span) -> Result<Node> {
        let mut pat_elems = vec![];
        let last_idx = elements.len() - 1;
        for (i, element) in elements.into_iter().enumerate() {
            match element {
                Some(ArrayExpressionElement::Expression(expr)) => {
                    pat_elems.push(Some(expr.to_array_pattern_element(i == last_idx)?))
                }
                Some(ArrayExpressionElement::SpreadElement(SpreadElement { argument, span })) => {
                    if i != last_idx {
                        return Node::error(ErrorType::ParamAfterRest, span.start);
                    }
                    pat_elems.push(Some(ArrayPatternElement::RestElement {
                        argument: argument.to_strict_binding(span.start)?,
                    }));
                }
                None => pat_elems.push(None),
            }
        }
        Ok(Node::ArrayPattern {
            elements: pat_elems,
            assign_target_type: "simple",
            span,
        })
    }

    fn to_array_pattern_element(self, is_last: bool) -> Result<ArrayPatternElement> {
        let element = self.to_binding_pattern()?;
        match element {
            Node::AssignmentPattern { left, right, .. } => {
                Ok(ArrayPatternElement::AssignmentPattern {
                    left,
                    right: *right,
                })
            }
            Node::BindingIdentifier { name, .. } => {
                Ok(ArrayPatternElement::BindingIdentifier { name })
            }
            Node::ObjectPattern { properties, .. } => {
                Ok(ArrayPatternElement::ObjectPattern { properties })
            }
            Node::ArrayPattern { elements, .. } => {
                Ok(ArrayPatternElement::ArrayPattern { elements })
            }
            Node::RestElement { argument, span } => {
                if is_last {
                    Ok(ArrayPatternElement::RestElement { argument })
                } else {
                    Node::error(ErrorType::ParamAfterRest, span.start)
                }
            }
            _ => {
                return Node::error(
                    ErrorType::InvalidOrUnexpectedToken,
                    element.expr_span().start,
                )
            }
        }
    }

    fn object_to_pattern(properties: Vec<ObjectExpressionProperty>, span: Span) -> Result<Node> {
        let mut pat_props = vec![];
        if properties.len() == 0 {
            return Ok(Node::ObjectPattern {
                properties: pat_props,
                assign_target_type: "simple",
                span,
            });
        }
        let last_idx = properties.len() - 1;
        for (i, property) in properties.into_iter().enumerate() {
            match property {
                ObjectExpressionProperty::Property {
                    key,
                    computed,
                    value,
                    kind,
                    method,
                    shorthand,
                    span,
                } => {
                    let value = match value {
                        Some(PropertyValue::Expression(expr)) => match expr.to_binding_pattern()? {
                            Node::AssignmentPattern { left, right, .. } => {
                                Ok(Some(PropertyValue::AssignmentPattern(AssignmentPattern {
                                    left,
                                    right: *right,
                                })))
                            }
                            Node::BindingIdentifier { name, span } => {
                                Ok(Some(PropertyValue::BindingIdentifier(BindingIdentifier {
                                    name,
                                    span,
                                })))
                            }
                            Node::ObjectPattern { properties, .. } => {
                                Ok(Some(PropertyValue::BindingPattern(
                                    BindingPattern::ObjectPattern { properties },
                                )))
                            }
                            Node::ArrayPattern { elements, .. } => {
                                Ok(Some(PropertyValue::BindingPattern(
                                    BindingPattern::ArrayPattern { elements },
                                )))
                            }
                            _ => Node::error(ErrorType::InvalidOrUnexpectedToken, span.start),
                        },
                        None => Ok(None),
                        _ => Node::error(ErrorType::InvalidOrUnexpectedToken, span.start),
                    }?;
                    pat_props.push(ObjectPatternProperty::Property {
                        key,
                        computed,
                        value,
                        kind,
                        method,
                        shorthand,
                    });
                }
                ObjectExpressionProperty::SpreadElement { argument, span } => {
                    if i == last_idx {
                        pat_props.push(ObjectPatternProperty::RestElement {
                            argument: argument.to_strict_binding(span.start)?,
                        });
                    } else {
                        return Node::error(ErrorType::InvalidOrUnexpectedToken, span.start);
                    }
                }
            }
        }
        Ok(Node::ObjectPattern {
            properties: pat_props,
            assign_target_type: "simple",
            span,
        })
    }

    pub fn error<T>(errortype: ErrorType, marker: Marker) -> Result<T> {
        Err(Error {
            line: marker.line,
            col: marker.col,
            errortype,
        })
    }
}
