use std::fmt;

// T(AmbiguousExport, "The requested module '%' contains conflicting star exports for name '%'")
// T(BadSetterArity, "Setter must have exactly one formal parameter.")
// T(ConstructorIsAccessor, "Class constructor may not be an accessor")
// T(ConstructorIsGenerator, "Class constructor may not be a generator")
// T(ConstructorIsAsync, "Class constructor may not be an async method")
// T(ConstructorIsPrivate, "Class constructor may not be a private method")
// T(DerivedConstructorReturnedNonObject, "Derived constructors may only return object or undefined")
// T(DuplicateConstructor, "A class may only have one constructor")
// T(DuplicateProto, "Duplicate __proto__ fields are not allowed in object literals")
// T(ForInOfLoopInitializer, "% loop variable declaration may not have an initializer.")
// T(ForOfLet, "The left-hand side of a for-of loop may not start with 'let'.") \
// T(ForInOfLoopMultiBindings, "Invalid left-hand side in % loop: Must have a single binding.")
// T(AsyncFunctionInSingleStatementContext, "Async functions can only be declared at the top level or inside a block.")
// T(IllegalContinue, "Illegal continue statement: '%' does not denote an iteration statement")
// T(IllegalLanguageModeDirective, "Illegal '%' directive in function with non-simple parameter list")
// T(IntrinsicWithSpread, "Intrinsic calls do not support spread arguments")
// T(InvalidPropertyBindingPattern, "Illegal property in declaration context")
// T(InvalidRestAssignmentPattern, "`...` must be followed by an assignable reference in assignment contexts")
// T(InvalidEscapedReservedWord, "Keyword must not contain escaped characters")
// T(InvalidEscapedMetaProperty, "'%' must not contain escaped characters")
// T(InvalidCoverInitializedName, "Invalid shorthand property initializer")
// T(InvalidRegExpFlags, "Invalid flags supplied to RegExp constructor '%'")
// T(InvalidPrivateFieldResolution, "Private field '%' must be declared in an enclosing class")
// T(InvalidPrivateMemberRead, "Cannot read private member % from an object whose class did not declare it")
// T(InvalidPrivateMemberWrite, "Cannot write private member % to an object whose class did not declare it")
// T(InvalidPrivateMethodWrite, "Private method '%' is not writable")
// T(InvalidPrivateGetterAccess, "'%' was defined without a getter")
// T(InvalidPrivateSetterAccess, "'%' was defined without a setter")
// T(JsonParseUnexpectedToken, "Unexpected token % in JSON at position %")
// T(JsonParseUnexpectedTokenNumber, "Unexpected number in JSON at position %")
// T(JsonParseUnexpectedTokenString, "Unexpected string in JSON at position %")
// T(LabelRedeclaration, "Label '%' has already been declared")
// T(LabelledFunctionDeclaration, "Labelled function declaration not allowed as the body of a control flow structure")
// T(MalformedArrowFunParamList, "Malformed arrow function parameter list")
// T(MalformedRegExp, "Invalid regular expression: /%/: %")
// T(MalformedRegExpFlags, "Invalid regular expression flags")
// T(ModuleExportUndefined, "Export '%' is not defined in module")
// T(HtmlCommentInModule, "HTML comments are not allowed in modules")
// T(NoCatchOrFinally, "Missing catch or finally after try")
// T(FlattenPastSafeLength, "Flattening % elements on an array-like of length % is disallowed, as the total surpasses 2**53-1")
// T(PushPastSafeLength, "Pushing % elements on an array-like of length % is disallowed, as the total surpasses 2**53-1")
// T(ElementAfterRest, "Rest element must be last element")
// T(BadSetterRestParameter, "Setter function argument must not be a rest parameter")
// T(ParamDupe, "Duplicate parameter name not allowed in this context")
// T(ParenthesisInArgString, "Function arg string contains parenthesis")
// T(ArgStringTerminatesParametersEarly, "Arg string terminates parameters early")
// T(UnexpectedEndOfArgString, "Unexpected end of arg string")
// T(RestDefaultInitializer, "Rest parameter may not have a default initializer")
// T(RuntimeWrongNumArgs, "Runtime function given wrong number of arguments")
// T(SuperNotCalled, "Must call super constructor in derived class before accessing 'this' or returning from derived constructor")
// T(SingleFunctionLiteral, "Single function literal required")
// T(SloppyFunction, "In non-strict mode code, functions can only be declared at top level, inside a block, or as the body of an if statement.")
// T(SpeciesNotConstructor, "object.constructor[Symbol.species] is not a constructor")
// T(StrictDelete, "Delete of an unqualified identifier in strict mode.")
// T(StrictEvalArguments, "Unexpected eval or arguments in strict mode")
// T(StrictOctalLiteral, "Octal literals are not allowed in strict mode.")
// T(StrictDecimalWithLeadingZero, "Decimals with leading zeros are not allowed in strict mode.")
// T(StrictOctalEscape, "Octal escape sequences are not allowed in strict mode.")
// T(TemplateOctalLiteral, "Octal escape sequences are not allowed in template strings.")
// T(ThisFormalParameter, "'this' is not a valid formal parameter name")
// T(AwaitBindingIdentifier, "'await' is not a valid identifier name in an async function")
// T(TooManyArguments, "Too many arguments in function call (only 65535 allowed)")
// T(TooManyParameters, "Too many parameters in function definition (only 65534 allowed)")
// T(TooManyProperties, "Too many properties to enumerate")
// T(TooManySpreads, "Literal containing too many nested spreads (up to 65534 allowed)")
// T(TooManyVariables, "Too many variables declared (only 4194303 allowed)")
// T(TooManyElementsInPromiseAll, "Too many elements passed to Promise.all")
// T(TypedArrayTooShort, "Derived TypedArray constructor created an array which was too small")
// T(UnexpectedPrivateField, "Unexpected private field")
// T(UnexpectedStrictReserved, "Unexpected strict mode reserved word")
// T(UnexpectedSuper, "'super' keyword unexpected here")
// T(UnexpectedNewTarget, "new.target expression is not allowed here")
// T(UnexpectedTokenIdentifier, "Unexpected identifier")
// T(UnexpectedTokenNumber, "Unexpected number")
// T(UnexpectedTokenString, "Unexpected string")
// T(UnexpectedTokenRegExp, "Unexpected regular expression")
// T(UnexpectedLexicalDeclaration, "Lexical declaration cannot appear in a single-statement context")
// T(UnknownLabel, "Undefined label '%'")
// T(UnresolvableExport, "The requested module '%' does not provide an export named '%'")
// T(UnterminatedArgList, "missing ) after argument list")
// T(UnterminatedRegExp, "Invalid regular expression: missing /")
// T(UnterminatedTemplate, "Unterminated template literal")
// T(UnterminatedTemplateExpr, "Missing } in template expression")
// T(FoundNonCallableHasInstance, "Found non-callable @@hasInstance")
// T(InvalidHexEscapeSequence, "Invalid hexadecimal escape sequence")
// T(UndefinedUnicodeCodePoint, "Undefined Unicode code-point")

#[derive(Debug, PartialEq)]
pub struct Error {
    pub line: usize,
    pub col: usize,
    pub errortype: ErrorType,
}

#[derive(Debug, PartialEq)]
pub enum ErrorType {
    UnexpectedToken(String),
    UnexpectedReserved,
    IllegalReturn,
    MissingInitializer,
    InvalidLhsInFor,
    MultipleDefaultsInSwitch,
    NoIterationStatement,
    IllegalBreak,
    StrictWith,
    StrictFunction,
    NewlineAfterThrow,
    GeneratorInSingleStatementContext,
    MissingFunctionName,
    InvalidLhsInAssignment,
    UnexpectedTokenUnaryExponentiation,
    InvalidLhsInPostfixOp,
    InvalidLhsInPrefixOp,
    ParamAfterRest,
    InvalidDestructuringTarget,
    UnexpectedTemplateString,
    YieldInParameter,
    AwaitExpressionFormalParameter,
    UnexpectedEOS,
    InvalidOrUnexpectedToken,
    InvalidUnicodeEscapeSequence,
    InvalidRestBindingPattern,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.errortype {
            ErrorType::UnexpectedToken(s) => write!(f, "Unexpected token {}", s.clone()),
            ErrorType::UnexpectedReserved => write!(f, "Unexpected reserved word"),
            ErrorType::IllegalReturn => write!(f, "Illegal return statement"),
            ErrorType::MissingInitializer => {
                write!(f, "Missing initializer in destructuring declaration")
            }
            ErrorType::InvalidLhsInFor => write!(f, "Invalid left-hand side in for-loop"),
            ErrorType::MultipleDefaultsInSwitch => {
                write!(f, "More than one default clause in switch statement")
            }
            ErrorType::NoIterationStatement => write!(
                f,
                "Illegal continue statement: no surrounding iteration statement"
            ),
            ErrorType::IllegalBreak => write!(f, "Illegal break statement"),
            ErrorType::StrictWith => write!(f, "Strict mode code may not include a with statement"),
            ErrorType::StrictFunction => write!(f, "In strict mode code, functions can only be declared at top level or inside a block."),
            ErrorType::NewlineAfterThrow => write!(f, "Illegal newline after throw"),
            ErrorType::GeneratorInSingleStatementContext => write!(f, "Generators can only be declared at the top level or inside a block."),
            ErrorType::MissingFunctionName => write!(f, "Function statements require a function name"),
            ErrorType::InvalidLhsInAssignment => write!(f, "Invalid left-hand side in assignment"),
            ErrorType::UnexpectedTokenUnaryExponentiation => write!(f, "Unary operator used immediately before exponentiation expression. Parenthesis must be used to disambiguate operator precedence"),
            ErrorType::InvalidLhsInPostfixOp => write!(f, "Invalid left-hand side expression in postfix operation"),
            ErrorType::InvalidLhsInPrefixOp => write!(f, "Invalid left-hand side expression in prefix operation"),
            ErrorType::ParamAfterRest => write!(f, "Rest parameter must be last formal parameter"),
            ErrorType::InvalidDestructuringTarget => write!(f, "Invalid destructuring assignment target"),
            ErrorType::UnexpectedTemplateString => write!(f, "Unexpected template string"),
            ErrorType::YieldInParameter => write!(f, "Yield expression not allowed in formal parameter"),
            ErrorType::AwaitExpressionFormalParameter => write!(f, "Illegal await-expression in formal parameters of async function"),
            ErrorType::UnexpectedEOS => write!(f, "Unexpected end of input"),
            ErrorType::InvalidOrUnexpectedToken => write!(f, "Invalid or unexpected token"),
            ErrorType::InvalidUnicodeEscapeSequence => write!(f, "Invalid Unicode escape sequence"),
            ErrorType::InvalidRestBindingPattern => write!(f, "`...` must be followed by an identifier in declaration contexts"),
        }
    }
}
