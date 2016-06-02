{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Language.Dart.Syntax
  ( AnnotatedNode(..)
  , Annotation(..)
  , ArgumentList(..)
  , AstNode(..)
  , AsyncModifier(..)
  , Block(..)
  , CatchClause(..)
  , ClassMember(..)
  , Combinator(..)
  , Comment(..)
  , CommentReference(..)
  , CommentType(..)
  , CompilationUnit(..)
  , CompilationUnitMember(..)
  , Configuration(..)
  , ConstructorInitializer(..)
  , ConstructorName(..)
  , Declaration(..)
  , DeclaredIdentifier(..)
  , Directive(..)
  , DottedName(..)
  , EnumConstantDeclaration(..)
  , Expression(..)
  , ExtendsClause(..)
  , FinalConstVarOrType(..)
  , FinalVarOrType(..)
  , FormalParameter(..)
  , FormalParameterList(..)
  , FunctionBody(..)
  , FunctionDeclaration(..)
  , FunctionExpression(..)
  , Identifier(..)
  , ImplementsClause(..)
  , InterpolationElement(..)
  , InvocationExpression(..)
  , Label(..)
  , LibraryIdentifier(..)
  , Literal(..)
  , MapLiteralEntry(..)
  , MethodModifier(..)
  , NamedCompilationUnitMember(..)
  , NamespaceDirective(..)
  , NewOrConst(..)
  , NormalFormalParameter(..)
  , ParameterKind(..)
  , PropertyKeyword(..)
  , ScriptTag(..)
  , SimpleIdentifier(..)
  , SingleStringLiteral(..)
  , Statement(..)
  , StringLiteral(..)
  , SwitchMember(..)
  , Token
  , TypeAlias(..)
  , TypeArgumentList(..)
  , TypedLiteral(..)
  , TypeName(..)
  , TypeParameter(..)
  , TypeParameterList(..)
  , UriBasedDirective(..)
  , VariableDeclaration(..)
  , VariableDeclarationList(..)
  , WithClause(..)
  ) where

import Data.Data
import GHC.Generics (Generic)

type Token = String

-- | A literal that has a type associated with it.
--
--      typedLiteral ::=
--          [ListLiteral]
--        | [MapLiteral]
data TypedLiteral
  -- | A literal map.
  --
  --      mapLiteral ::=
  --          'const'? ('<' [TypeName] (',' [TypeName])* '>')?
  --          '{' ([MapLiteralEntry] (',' [MapLiteralEntry])* ','?)? '}'
  = MapLiteral Bool -- isConst
               (Maybe TypeArgumentList) -- typeArguments
               [MapLiteralEntry] -- entries
  -- | A list literal.
  --
  --      listLiteral ::=
  --          'const'? ('<' [TypeName] '>')? '[' ([Expression] ','?)? ']'
  | ListLiteral Bool -- isConst
                (Maybe TypeArgumentList) -- typeArguments
                [Expression] -- elements
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node within a [StringInterpolation].
--
--      interpolationElement ::=
--          [InterpolationExpression]
--        | [InterpolationString]
data InterpolationElement
  -- | An expression embedded in a string interpolation.
  --
  --      interpolationExpression ::=
  --          '$' [SimpleIdentifier]
  --        | '$' '{' [Expression] '}'
  = InterpolationExpression Expression -- expression
  -- | A non-empty substring of an interpolated string.
  --
  --    interpolationString ::=
  --        characters
  | InterpolationString String -- value
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A single string literal expression.
--
--      singleStringLiteral ::=
--          [SimpleStringLiteral]
--        | [StringInterpolation]
data SingleStringLiteral
  -- | A string literal expression that does not contain any interpolations.
  --
  --      simpleStringLiteral ::=
  --          rawStringLiteral
  --        | basicStringLiteral
  --
  --      rawStringLiteral ::=
  --          'r' basicStringLiteral
  --
  --      simpleStringLiteral ::=
  --          multiLineStringLiteral
  --        | singleLineStringLiteral
  --
  --      multiLineStringLiteral ::=
  --          "'''" characters "'''"
  --        | '"""' characters '"""'
  --
  --      singleLineStringLiteral ::=
  --          "'" characters "'"
  --        | '"' characters '"'
  = SimpleStringLiteral String -- value
  -- | A string interpolation literal.
  --
  --      stringInterpolation ::=
  --          ''' [InterpolationElement]* '''
  --        | '"' [InterpolationElement]* '"'
  | StringInterpolation [InterpolationElement] -- elements
  deriving (Eq, Show, Typeable, Generic, Data)

--   A string literal expression.
--
--      stringLiteral ::=
--          [SimpleStringLiteral]
--        | [AdjacentStrings]
--        | [StringInterpolation]
data StringLiteral
  = SingleStringLiteral' SingleStringLiteral
  -- | Two or more string literals that are implicitly concatenated because of being
  --   adjacent (separated only by whitespace).
  --
  --   While the grammar only allows adjacent strings when all of the strings are of
  --   the same kind (single line or multi-line), this class doesn't enforce that
  --   restriction.
  --
  --      adjacentStrings ::=
  --          [StringLiteral] [StringLiteral]+
  | AdjacentStrings [StringLiteral] -- strings
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that represents a literal expression.
--
--      literal ::=
--          [BooleanLiteral]
--        | [DoubleLiteral]
--        | [IntegerLiteral]
--        | [ListLiteral]
--        | [MapLiteral]
--        | [NullLiteral]
--        | [StringLiteral]
data Literal
  --   A boolean literal expression.
  --
  --      booleanLiteral ::=
  --          'false' | 'true'
  = BooleanLiteral Bool -- bool
  --   A floating point literal expression.
  --
  --      doubleLiteral ::=
  --          decimalDigit+ ('.' decimalDigit*)? exponent?
  --        | '.' decimalDigit+ exponent?
  --
  --      exponent ::=
  --          ('e' | 'E') ('+' | '-')? decimalDigit+
  | DoubleLiteral Double -- value
  | TypedLiteral TypedLiteral
  -- | An integer literal expression.
  --
  --      integerLiteral ::=
  --          decimalIntegerLiteral
  --        | hexadecimalIntegerLiteral
  --
  --      decimalIntegerLiteral ::=
  --          decimalDigit+
  --
  --      hexadecimalIntegerLiteral ::=
  --          '0x' hexadecimalDigit+
  --        | '0X' hexadecimalDigit+
  | IntegerLiteral Integer -- value
  -- | A null literal expression.
  --
  --      nullLiteral ::=
  --          'null'
  | NullLiteral
  | StringLiteral' StringLiteral
  -- | A symbol literal expression.
  --
  --      symbolLiteral ::=
  --          '#' (operator | (identifier ('.' identifier)*))
  | SymbolLiteral [Token] -- components
  deriving (Eq, Show, Typeable, Generic, Data)

data FinalConstVarOrType = FCVTFinal TypeName
                         | FCVTConst TypeName
                         | FCVTType  TypeName
                         | FCVTVar
  deriving (Eq, Show, Typeable, Generic, Data)

data FinalVarOrType = FVTFinal TypeName
                    | FVTType TypeName
                    | FVTVar
  deriving (Eq, Show, Typeable, Generic, Data)

data NewOrConst = NCNew | NCConst
  deriving (Eq, Show, Typeable, Generic, Data)

-- | An identifier that has an initial value associated with it. Instances of this
--   class are always children of the class [VariableDeclarationList].
--
--      variableDeclaration ::=
--          [SimpleIdentifier] ('=' [Expression])?
data VariableDeclaration = VariableDeclaration SimpleIdentifier -- name
                           (Maybe Expression) -- initializer
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The declaration of one or more variables of the same type.
--
--      variableDeclarationList ::=
--          finalConstVarOrType [VariableDeclaration] (',' [VariableDeclaration])*
--
--      finalConstVarOrType ::=
--        | 'final' [TypeName]?
--        | 'const' [TypeName]?
--        | 'var'
--        | [TypeName]
data VariableDeclarationList = VariableDeclarationList (Maybe Comment) -- comment
                                                       [Annotation] -- metadata
                                                       FinalConstVarOrType -- kind
                                                       [VariableDeclaration] -- variables
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The declaration of a single identifier.
--
--      declaredIdentifier ::=
--          [Annotation] finalConstVarOrType [SimpleIdentifier]
data DeclaredIdentifier = DeclaredIdentifier (Maybe Comment) -- comment
                                             [Annotation] -- metadata
                                             FinalConstVarOrType -- kind
                                             SimpleIdentifier -- identifier
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The declaration of an enum constant.
data EnumConstantDeclaration = EnumConstantDeclaration (Maybe Comment)
                                                       [Annotation]
                                                       SimpleIdentifier
  deriving (Eq, Show, Typeable, Generic, Data)

data PropertyKeyword = Get | Set | Empty
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A top-level declaration.
--
--      functionDeclaration ::=
--          'external' functionSignature
--        | functionSignature [FunctionBody]
--
--      functionSignature ::=
--          [Type]? ('get' | 'set')? [SimpleIdentifier] [FormalParameterList]
data FunctionDeclaration = FunctionDeclaration (Maybe Comment) -- comment
                                               [Annotation] -- metadata
                                               Bool -- isExternal
                                               (Maybe TypeName) -- returnType
                                               PropertyKeyword -- propertyKeyword
                                               SimpleIdentifier -- name
                                               FunctionExpression -- functionExpression
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The with clause in a class declaration.
--
--      withClause ::=
--          'with' [TypeName] (',' [TypeName])*
newtype WithClause = WithClause [TypeName] -- mixinTypes
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The "implements" clause in an class declaration.
--
--      implementsClause ::=
--          'implements' [TypeName] (',' [TypeName])*
newtype ImplementsClause = ImplementsClause [TypeName] -- interfaces
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The declaration of a type alias.
--
--      typeAlias ::=
--          'typedef' typeAliasBody
--
--      typeAliasBody ::=
--          classTypeAlias
--        | functionTypeAlias
data TypeAlias
  -- | A class type alias.
  --
  --      classTypeAlias ::=
  --          [SimpleIdentifier] [TypeParameterList]? '=' 'abstract'? mixinApplication
  --
  --      mixinApplication ::=
  --          [TypeName] [WithClause] [ImplementsClause]? ';'
  = ClassTypeAlias    (Maybe Comment) -- comment
                      [Annotation] -- metadata
                      SimpleIdentifier -- name
                      (Maybe TypeParameterList) -- typeParameters
                      Bool -- isAbstract
                      TypeName -- superclass
                      WithClause -- withClause
                      (Maybe ImplementsClause) -- implementsClause
  -- | A function type alias.
  --
  --      functionTypeAlias ::=
  --          functionPrefix [TypeParameterList]? [FormalParameterList] ';'
  --
  --      functionPrefix ::=
  --          [TypeName]? [SimpleIdentifier]
  | FunctionTypeAlias (Maybe Comment) -- comment
                      [Annotation] -- metadata
                      (Maybe TypeName) -- returnType
                      SimpleIdentifier -- name
                      (Maybe TypeParameterList) -- typeParameters
                      FormalParameterList -- parameters
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that declares a single name within the scope of a compilation unit.
data NamedCompilationUnitMember
  = FunctionDeclaration' FunctionDeclaration
  | TypeAlias TypeAlias
  -- | The declaration of a class.
  --
  --      classDeclaration ::=
  --          'abstract'? 'class' [SimpleIdentifier] [TypeParameterList]?
  --          ([ExtendsClause] [WithClause]?)?
  --          [ImplementsClause]?
  --          '{' [ClassMember]* '}'
  | ClassDeclaration (Maybe Comment) -- comment
                     [Annotation] -- metadata
                     Bool -- isAbstract
                     SimpleIdentifier -- name
                     (Maybe TypeParameterList) -- typeParameters
                     (Maybe ExtendsClause) -- extendsClause
                     (Maybe WithClause) -- withClause
                     (Maybe ImplementsClause) -- implementsClause
                     [ClassMember] -- members
  -- | The declaration of an enumeration.
  --
  --      enumType ::=
  --          metadata 'enum' [SimpleIdentifier] '{' [SimpleIdentifier] (',' [SimpleIdentifier])* (',')? '}'
  | EnumDeclaration (Maybe Comment) -- comment
                    [Annotation] -- metadata
                    SimpleIdentifier -- name
                    [EnumConstantDeclaration] -- constants
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that declares one or more names within the scope of a compilation
--   unit.
--
--      compilationUnitMember ::=
--          [ClassDeclaration]
--        | [TypeAlias]
--        | [FunctionDeclaration]
--        | [MethodDeclaration]
--        | [VariableDeclaration]
--        | [VariableDeclaration]
data CompilationUnitMember
  -- | The declaration of one or more top-level variables of the same type.
  --
  --      topLevelVariableDeclaration ::=
  --          ('final' | 'const') type? staticFinalDeclarationList ';'
  --        | variableDeclaration ';'
  = TopLevelVariableDeclaration (Maybe Comment) -- comment
                                [Annotation] -- metadata
                                VariableDeclarationList -- variableList
  | NamedCompilationUnitMember NamedCompilationUnitMember
  deriving (Eq, Show, Typeable, Generic, Data)

data MethodModifier = Abstract | Static
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that declares a name within the scope of a class.
data ClassMember
  --   A constructor declaration.
  --
  --      constructorDeclaration ::=
  --          constructorSignature [FunctionBody]?
  --        | constructorName formalParameterList ':' 'this' ('.' [SimpleIdentifier])? arguments
  --
  --      constructorSignature ::=
  --          'external'? constructorName formalParameterList initializerList?
  --        | 'external'? 'factory' factoryName formalParameterList initializerList?
  --        | 'external'? 'const'  constructorName formalParameterList initializerList?
  --
  --      constructorName ::=
  --          [SimpleIdentifier] ('.' [SimpleIdentifier])?
  --
  --      factoryName ::=
  --          [Identifier] ('.' [SimpleIdentifier])?
  --
  --      initializerList ::=
  --          ':' [ConstructorInitializer] (',' [ConstructorInitializer])*
  = ConstructorDeclaration (Maybe Comment) -- comment
                           [Annotation] -- metadata
                           Bool -- isExternal
                           Bool -- isConst
                           Bool -- isFactory
                           Identifier -- returnType
                           (Maybe SimpleIdentifier) -- name
                           FormalParameterList -- parameters
                           [ConstructorInitializer] -- initializers
                           (Maybe ConstructorName) -- redirectedConstructor
                           (Maybe FunctionBody) -- body
  -- | A method declaration.
  --
  --      methodDeclaration ::=
  --          methodSignature [FunctionBody]
  --
  --      methodSignature ::=
  --          'external'? ('abstract' | 'static')? [Type]? ('get' | 'set')?
  --          methodName [TypeParameterList] [FormalParameterList]
  --
  --      methodName ::=
  --          [SimpleIdentifier]
  --        | 'operator' [SimpleIdentifier]
  | MethodDeclaration (Maybe Comment) -- comment
                      [Annotation] -- metadata
                      Bool -- isExternal
                      (Maybe MethodModifier) -- methodModifier
                      (Maybe TypeName) -- returnType
                      PropertyKeyword
                      Bool -- isOperator
                      SimpleIdentifier -- name
                      (Maybe TypeParameterList) -- typeParameters
                      (Maybe FormalParameterList) -- parameters
                      FunctionBody -- body
  -- | The declaration of one or more fields of the same type.
  --
  --      fieldDeclaration ::=
  --          'static'? [VariableDeclarationList] ';'
  | FieldDeclaration  (Maybe Comment) -- comment
                      [Annotation] -- metadata
                      Bool -- isStatic
                      VariableDeclarationList -- fieldList
  deriving (Eq, Show, Typeable, Generic, Data)

-- |  A node that represents the declaration of one or more names. Each declared
--    name is visible within a name scope.
data Declaration
  = VariableDeclaration' VariableDeclaration
  | TypeParameter' TypeParameter
  | ClassMember ClassMember
  | DeclaredIdentifier' DeclaredIdentifier
  | EnumConstantDeclaration' EnumConstantDeclaration
  | CompilationUnitMember' CompilationUnitMember
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that represents a directive that impacts the namespace of a library.
--
--      directive ::=
--          [ExportDirective]
--        | [ImportDirective]
data NamespaceDirective
  -- | An export directive.
  --
  --      exportDirective ::=
  --          [Annotation] 'export' [StringLiteral] [Combinator]* ';'
  = ExportDirective (Maybe Comment) -- comment
                    [Annotation] -- metadata
                    StringLiteral -- libraryUri
                    [Configuration] -- configurations
                    [Combinator] -- combinators
  -- | An import directive.
  --
  --      importDirective ::=
  --          [Annotation] 'import' [StringLiteral] ('as' identifier)? [Combinator]* ';'
  --        | [Annotation] 'import' [StringLiteral] 'deferred' 'as' identifier [Combinator]* ';'
  | ImportDirective (Maybe Comment) -- comment
                    [Annotation] -- metadata
                    StringLiteral -- libraryUri
                    [Configuration] -- configurations
                    Bool -- isDeferred
                    (Maybe SimpleIdentifier) -- prefix
                    [Combinator] -- combinators
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A directive that references a URI.
--
--      uriBasedDirective ::=
--          [ExportDirective]
--        | [ImportDirective]
--        | [PartDirective]
data UriBasedDirective
  = NamespaceDirective NamespaceDirective
  -- | A part directive.
  --
  --      partDirective ::=
  --          [Annotation] 'part' [StringLiteral] ';'
  | PartDirective (Maybe Comment) -- comment
                  [Annotation] -- metadata
                  StringLiteral -- partUri
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that represents a directive.
--
--      directive ::=
--          [ExportDirective]
--        | [ImportDirective]
--        | [LibraryDirective]
--        | [PartDirective]
--        | [PartOfDirective]
data Directive
  = UriBasedDirective UriBasedDirective
  -- | A part-of directive.
  --
  --      partOfDirective ::=
  --          [Annotation] 'part' 'of' [Identifier] ';'
  | PartOfDirective (Maybe Comment) -- comment
                    [Annotation] -- metadata
                    LibraryIdentifier -- libraryName
  -- | A library directive.
  --
  --      libraryDirective ::=
  --          [Annotation] 'library' [Identifier] ';'
  | LibraryDirective (Maybe Comment) -- comment
                     [Annotation] -- metadata
                     LibraryIdentifier -- name
  deriving (Eq, Show, Typeable, Generic, Data)

-- | An AST node that can be annotated with both a documentation comment and a
--   list of annotations.
data AnnotatedNode
  = VariableDeclarationList' VariableDeclarationList
  | Declaration' Declaration
  | Directive Directive
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that can occur in the initializer list of a constructor declaration.
--
--      constructorInitializer ::=
--          [SuperConstructorInvocation]
--        | [ConstructorFieldInitializer]
--        | [RedirectingConstructorInvocation]
data ConstructorInitializer
  -- | The invocation of a constructor in the same class from within a constructor's
  --   initialization list.
  --
  --      redirectingConstructorInvocation ::=
  --          'this' ('.' identifier)? arguments
  = RedirectingConstructorInvocation (Maybe SimpleIdentifier) -- constructorName
                                     ArgumentList -- argumentList
  -- | The initialization of a field within a constructor's initialization list.
  --
  --      fieldInitializer ::=
  --          ('this' '.')? [SimpleIdentifier] '=' [Expression]
  | ConstructorFieldInitializer Bool -- explicitThis
                                SimpleIdentifier -- fieldName
                                Expression -- expression
  -- | The invocation of a superclass' constructor from within a constructor's
  --   initialization list.
  --
  --      superInvocation ::=
  --          'super' ('.' [SimpleIdentifier])? [ArgumentList]
  | SuperConstructorInvocation (Maybe SimpleIdentifier) -- constructorName
                               ArgumentList -- argumentList
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A combinator associated with an import or export directive.
--
--      combinator ::=
--          [HideCombinator]
--        | [ShowCombinator]
data Combinator
  -- | A combinator that restricts the names being imported to those in a given list.
  --
  --      showCombinator ::=
  --          'show' [SimpleIdentifier] (',' [SimpleIdentifier])*
  = ShowCombinator [SimpleIdentifier] -- shownNames
  -- | A combinator that restricts the names being imported to those that are not in
  --   a given list.
  --
  --      hideCombinator ::=
  --          'hide' [SimpleIdentifier] (',' [SimpleIdentifier])*
  | HideCombinator [SimpleIdentifier] -- hiddenNames
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A dotted name, used in a configuration within an import or export directive.
--
--      dottedName ::=
--          [SimpleIdentifier] ('.' [SimpleIdentifier])*
newtype DottedName = DottedName [SimpleIdentifier] -- components
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A configuration in either an import or export directive.
--
--      configuration ::=
--          'if' '(' test ')' uri
--
--      test ::=
--          dottedName ('==' stringLiteral)?
--
--      dottedName ::=
--          identifier ('.' identifier)*
data Configuration = Configuration DottedName -- name
                                   StringLiteral -- value
                                   StringLiteral -- libraryUri
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The "extends" clause in a class declaration.
--
--      extendsClause ::=
--          'extends' [TypeName]
newtype ExtendsClause = ExtendsClause TypeName -- superclass
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A single key/value pair in a map literal.
--
--      mapLiteralEntry ::=
--          [Expression] ':' [Expression]
data MapLiteralEntry = MapLiteralEntry Expression -- key
                                       Expression -- value
  deriving (Eq, Show, Typeable, Generic, Data)

newtype ScriptTag = ScriptTag Token -- scriptTag
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node in the AST structure for a Dart program.
data AstNode
  = SwitchMember SwitchMember
  | Statement Statement
  | CatchClause' CatchClause
  | TypeNameNode TypeName
  | TypeArgumentList' TypeArgumentList
  | Label' Label
  | Annotation' Annotation
  | Comment' Comment
  | CommentReference' CommentReference
  | ArgumentList' ArgumentList
  | TypeParameterList' TypeParameterList
  | FormalParameterList' FormalParameterList
  | AnnotatedNode' AnnotatedNode
  | WithClause' WithClause
  | ExtendsClause' ExtendsClause
  | ConstructorName' ConstructorName
  | InterpolationElement' InterpolationElement
  | ConstructorInitializer ConstructorInitializer
  | Combinator Combinator
  | Configuration' Configuration
  | DottedName' DottedName
  --   A script tag that can optionally occur at the beginning of a compilation unit.
  --
  --      scriptTag ::=
  --          '#!' (~NEWLINE)* NEWLINE
  | ScriptTag' ScriptTag
  -- | The "native" clause in an class declaration.
  --
  --      nativeClause ::=
  --          'native' [StringLiteral]
  | NativeClause StringLiteral -- name
  | MapLiteralEntry' MapLiteralEntry
  deriving (Eq, Show, Typeable, Generic, Data)

data ParameterKind = Required | Positional | Named
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A formal parameter that is required (is not optional).
--
--      normalFormalParameter ::=
--          [FunctionTypedFormalParameter]
--        | [FieldFormalParameter]
--        | [SimpleFormalParameter]
data NormalFormalParameter
  -- | A function-typed formal parameter.
  --
  --      functionSignature ::=
  --          [TypeName]? [SimpleIdentifier] [TypeParameterList]? [FormalParameterList]
  = FunctionTypedFormalParameter (Maybe Comment) -- comment
                                 [Annotation] -- metadata
                                 (Maybe TypeName) -- returnType
                                 SimpleIdentifier -- identifier
                                 (Maybe TypeParameterList) -- typeParameters
                                 FormalParameterList -- parameters
  -- | A field formal parameter.
  --
  --      fieldFormalParameter ::=
  --          ('final' [TypeName] | 'const' [TypeName] | 'var' | [TypeName])?
  --          'this' '.' [SimpleIdentifier]
  | FieldFormalParameter         (Maybe Comment) -- comment
                                 [Annotation] -- metadata
                                 (Maybe FinalConstVarOrType)
                                 Bool -- explicitThis
                                 SimpleIdentifier -- identifier
  -- | A simple formal parameter.
  --
  --      simpleFormalParameter ::=
  --          ('final' [TypeName] | 'var' | [TypeName])? [SimpleIdentifier]
  | SimpleFormalParameter        (Maybe Comment) -- comment
                                 [Annotation] -- metadata
                                 (Maybe FinalVarOrType)
                                 SimpleIdentifier -- identifier
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node representing a parameter to a function.
--
--      formalParameter ::=
--          [NormalFormalParameter]
--        | [DefaultFormalParameter]
data FormalParameter
  = NormalFormalParameter' NormalFormalParameter
  --   A formal parameter with a default value. There are two kinds of parameters
  --   that are both represented by this class: named formal parameters and
  --   positional formal parameters.
  --
  --      defaultFormalParameter ::=
  --          [NormalFormalParameter] ('=' [Expression])?
  --
  --      defaultNamedParameter ::=
  --          [NormalFormalParameter] (':' [Expression])?
  | DefaultFormalParameter NormalFormalParameter -- parameter
                           ParameterKind -- kind
                           (Maybe Expression) -- defaultValue
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The formal parameter list of a method declaration, function declaration, or
--   function type alias.
--
--   While the grammar requires all optional formal parameters to follow all of
--   the normal formal parameters and at most one grouping of optional formal
--   parameters, this class does not enforce those constraints. All parameters are
--   flattened into a single list, which can have any or all kinds of parameters
--   (normal, named, and positional) in any order.
--
--      formalParameterList ::=
--          '(' ')'
--        | '(' normalFormalParameters (',' optionalFormalParameters)? ')'
--        | '(' optionalFormalParameters ')'
--
--      normalFormalParameters ::=
--          [NormalFormalParameter] (',' [NormalFormalParameter])*
--
--      optionalFormalParameters ::=
--          optionalPositionalFormalParameters
--        | namedFormalParameters
--
--      optionalPositionalFormalParameters ::=
--          '[' [DefaultFormalParameter] (',' [DefaultFormalParameter])* ']'
--
--      namedFormalParameters ::=
--          '{' [DefaultFormalParameter] (',' [DefaultFormalParameter])* '}'
newtype FormalParameterList = FormalParameterList [FormalParameter] -- parameters
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A type parameter.
--
--      typeParameter ::=
--          [SimpleIdentifier] ('extends' [TypeName])?
data TypeParameter = TypeParameter (Maybe Comment) -- comment
                                   [Annotation] -- metadata
                                   SimpleIdentifier -- name
                                   (Maybe TypeName) -- bound
  deriving (Eq, Show, Typeable, Generic, Data)

-- | Type parameters within a declaration.
--
--      typeParameterList ::=
--          '<' [TypeParameter] (',' [TypeParameter])* '>'
newtype TypeParameterList = TypeParameterList [TypeParameter] -- typeParameters
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A list of arguments in the invocation of an executable element (that is, a
--   function, method, or constructor).
--
--      argumentList ::=
--          '(' arguments? ')'
--
--      arguments ::=
--          [NamedExpression] (',' [NamedExpression])*
--        | [Expression] (',' [Expression])* (',' [NamedExpression])*
newtype ArgumentList = ArgumentList [Expression] -- arguments
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A reference to a Dart element that is found within a documentation comment.
--
--      commentReference ::=
--          '[' 'new'? [Identifier] ']'
data CommentReference = CommentReference Bool -- isNew
                                         Identifier -- identifier
  deriving (Eq, Show, Typeable, Generic, Data)

data CommentType = BlockComment | DocumentationComment | EndOfLineComment
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A comment within the source code.
--
--      comment ::=
--          endOfLineComment
--        | blockComment
--        | documentationComment
--
--      endOfLineComment ::=
--          '//' (CHARACTER - EOL)* EOL
--
--      blockComment ::=
--          '/--  ' CHARACTER* '&#42;/'
--
--      documentationComment ::=
--          '/--  *' (CHARACTER | [CommentReference])* '&#42;/'
--        | ('///' (CHARACTER - EOL)* EOL)+
data Comment = Comment [Token] -- tokens
                       CommentType -- type
                       [CommentReference] -- references
  deriving (Eq, Show, Typeable, Generic, Data)

-- | An annotation that can be associated with an AST node.
--
--      metadata ::=
--          annotation*
--
--      annotation ::=
--          '@' [Identifier] ('.' [SimpleIdentifier])? [ArgumentList]?
data Annotation = Annotation Identifier -- name
                             (Maybe SimpleIdentifier) -- constructorName
                             (Maybe ArgumentList) -- arguments
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A label on either a [LabeledStatement] or a [NamedExpression].
--
--      label ::=
--          [SimpleIdentifier] ':'
newtype Label = Label SimpleIdentifier -- label
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The name of a type, which can optionally include type arguments.
--
--      typeName ::=
--          [Identifier] typeArguments?
data TypeName = TypeName Identifier -- name
                         (Maybe TypeArgumentList) --typeArguments
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A list of type arguments.
--
--      typeArguments ::=
--          '<' typeName (',' typeName)* '>'
data TypeArgumentList = TypeArgumentList [TypeName] -- arguments
  deriving (Eq, Show, Typeable, Generic, Data)

-- | An element within a switch statement.
--
--      switchMember ::=
--          switchCase
--        | switchDefault
data SwitchMember
  -- | The default case in a switch statement.
  --
  --      switchDefault ::=
  --          [SimpleIdentifier]* 'default' ':' [Statement]*
  = SwitchDefault [Label] -- labels
                  [Statement] -- statements
  -- | A case in a switch statement.
  --
  --      switchCase ::=
  --          [SimpleIdentifier]* 'case' [Expression] ':' [Statement]*
  | SwitchCase [Label] -- labels
               Expression -- expression
               [Statement] -- statements
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A catch clause within a try statement.
--
--      onPart ::=
--          catchPart [Block]
--        | 'on' type catchPart? [Block]
--
--      catchPart ::=
--          'catch' '(' [SimpleIdentifier] (',' [SimpleIdentifier])? ')'
data CatchClause = CatchClause (Maybe TypeName) -- exceptionType
                               SimpleIdentifier -- exceptionParameter
                               (Maybe SimpleIdentifier) -- stackTraceParameter
                               Block -- body
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A sequence of statements.
--
--      block ::=
--          '{' statement* '}'
data Block = Block [Statement] -- statements
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A simple identifier.
--
--      simpleIdentifier ::=
--          initialCharacter internalCharacter*
--
--      initialCharacter ::= '_' | '$' | letter
--
--      internalCharacter ::= '_' | '$' | letter | digit
newtype SimpleIdentifier = SimpleIdentifier Token -- token
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The identifier for a library.
--
--      libraryIdentifier ::=
--          [SimpleIdentifier] ('.' [SimpleIdentifier])*
newtype LibraryIdentifier = LibraryIdentifier [SimpleIdentifier] -- components
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that represents an identifier.
--
--      identifier ::=
--          [SimpleIdentifier]
--        | [PrefixedIdentifier]
data Identifier
  = SimpleIdentifier' SimpleIdentifier
  -- | An identifier that is prefixed or an access to an object property where the
  --   target of the property access is a simple identifier.
  --
  --      prefixedIdentifier ::=
  --          [SimpleIdentifier] '.' [SimpleIdentifier]
  | PrefixedIdentifier SimpleIdentifier -- prefix
                       SimpleIdentifier -- identifier
  | LibraryIdentifier' LibraryIdentifier
  deriving (Eq, Show, Typeable, Generic, Data)

data AsyncModifier = Async | Sync | AsyncStar | SyncStar
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node representing the body of a function or method.
--
--      functionBody ::=
--          [BlockFunctionBody]
--        | [EmptyFunctionBody]
--        | [ExpressionFunctionBody]
data FunctionBody
  -- | A function body that consists of a block of statements.
  --
  --      blockFunctionBody ::=
  --          ('async' | 'async' '*' | 'sync' '*')? [Block]
  = BlockFunctionBody AsyncModifier -- asyncModifier
                      Block -- block
  -- | An empty function body, which can only appear in constructors or abstract
  --   methods.
  --
  --      emptyFunctionBody ::=
  --          ';'
  | EmptyFunctionBody
  -- | A function body consisting of a single expression.
  --
  --      expressionFunctionBody ::=
  --          'async'? '=>' [Expression] ';'
  | ExpressionFunctionBody Bool -- isAsync
                           Expression -- expression
  -- | A function body that consists of a native keyword followed by a string
  --   literal.
  --
  --      nativeFunctionBody ::=
  --          'native' [SimpleStringLiteral] ';'
  | NativeFunctionBody StringLiteral -- stringLiteral
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A function expression.
--
--      functionExpression ::=
--          [TypeParameterList]? [FormalParameterList] [FunctionBody]
data FunctionExpression = FunctionExpression (Maybe TypeParameterList) -- typeParameters
                                             FormalParameterList -- parameters
                                             FunctionBody -- body
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The name of a constructor.
--
--      constructorName ::=
--          type ('.' identifier)?
data ConstructorName = ConstructorName TypeName -- type
                                       (Maybe SimpleIdentifier) -- name
  deriving (Eq, Show, Typeable, Generic, Data)

-- | The invocation of a function or method; either a
--   [FunctionExpressionInvocation] or a [MethodInvocation].
data InvocationExpression
  -- | The invocation of a function resulting from evaluating an expression.
  --   Invocations of methods and other forms of functions are represented by
  --   [MethodInvocation] nodes. Invocations of getters and setters are represented
  --   by either [PrefixedIdentifier] or [PropertyAccess] nodes.
  --
  --      functionExpressionInvocation ::=
  --          [Expression] [TypeArgumentList]? [ArgumentList]
  = FunctionExpressionInvocation Expression -- function
                                 (Maybe TypeArgumentList) -- typeArguments
                                 ArgumentList -- argumentList
  -- | The invocation of either a function or a method. Invocations of functions
  --   resulting from evaluating an expression are represented by
  --   [FunctionExpressionInvocation] nodes. Invocations of getters and setters are
  --   represented by either [PrefixedIdentifier] or [PropertyAccess] nodes.
  --
  --      methodInvocation ::=
  --          ([Expression] '.')? [SimpleIdentifier] [TypeArgumentList]? [ArgumentList]
  | MethodInvocation (Maybe Expression) -- target
                     SimpleIdentifier -- methodName
                     (Maybe TypeArgumentList) -- typeArguments
                     ArgumentList -- argumentList
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that represents an expression.
--
--      expression ::=
--          [AssignmentExpression]
--        | [ConditionalExpression] cascadeSection*
--        | [ThrowExpression]
data Expression
  = Literal' Literal
  | Identifier' Identifier
  -- | A prefix unary expression.
  --
  --      prefixExpression ::=
  --          [Token] [Expression]
  | PrefixExpression Token -- operator
                     Expression -- operand
  -- | A postfix unary expression.
  --
  --      postfixExpression ::=
  --          [Expression] [Token]
  | PostfixExpression Expression -- operand
                      Token -- operator
  -- | A binary (infix) expression.
  --
  --      binaryExpression ::=
  --          [Expression] [Token] [Expression]
  | BinaryExpression Expression -- leftOperand
                     Token -- operator
                     Expression -- rightOperand
  -- | An assignment expression.
  --
  --      assignmentExpression ::=
  --          [Expression] operator [Expression]
  | AssignmentExpression Expression -- leftHandSide
                         Token -- operator
                         Expression -- rightHandSide
  | FunctionExpression' FunctionExpression
  -- | An instance creation expression.
  --
  --      newExpression ::=
  --          ('new' | 'const') [TypeName] ('.' [SimpleIdentifier])? [ArgumentList]
  | InstanceCreationExpression NewOrConst
                               ConstructorName -- constructorName
                               ArgumentList -- argumentList
  -- | An as expression.
  --
  --      asExpression ::=
  --          [Expression] 'as' [TypeName]
  | AsExpression Expression -- expression
                 TypeName -- type
  -- | An is expression.
  --
  --      isExpression ::=
  --          [Expression] 'is' '!'? [TypeName]
  | IsExpression Expression -- expression
                 Bool -- isNot
                 TypeName -- type
  -- | A throw expression.
  --
  --      throwExpression ::=
  --          'throw' [Expression]
  | ThrowExpression Expression -- expression
  -- | A rethrow expression.
  --
  --      rethrowExpression ::=
  --          'rethrow'
  | RethrowExpression
  -- | A this expression.
  --
  --      thisExpression ::=
  --          'this'
  | ThisExpression
  -- | A super expression.
  --
  --      superExpression ::=
  --          'super'
  | SuperExpression
  -- | A parenthesized expression.
  --
  --      parenthesizedExpression ::=
  --          '(' [Expression] ')'
  | ParenthesizedExpression Expression -- expression
  -- | The access of a property of an object.
  --
  --   Note, however, that accesses to properties of objects can also be represented
  --   as [PrefixedIdentifier] nodes in cases where the target is also a simple
  --   identifier.
  --
  --      propertyAccess ::=
  --          [Expression] '.' [SimpleIdentifier]
  | PropertyAccess Expression -- target
                   SimpleIdentifier -- propertyName
  -- | An expression that has a name associated with it. They are used in method
  --   invocations when there are named parameters.
  --
  --      namedExpression ::=
  --          [Label] [Expression]
  | NamedExpression Label -- name
                    Expression -- expression
  | InvocationExpression InvocationExpression
  -- | A conditional expression.
  --
  --      conditionalExpression ::=
  --          [Expression] '?' [Expression] ':' [Expression]
  | ConditionalExpression Expression -- condition
                          Expression -- thenExpression
                          Expression -- elseExpression
  -- | A sequence of cascaded expressions: expressions that share a common target.
  --   There are three kinds of expressions that can be used in a cascade
  --   expression: [IndexExpression], [MethodInvocation] and [PropertyAccess].
  --
  --      cascadeExpression ::=
  --          [Expression] cascadeSection*
  --
  --      cascadeSection ::=
  --          '..'  (cascadeSelector arguments*) (assignableSelector arguments*)*
  --          (assignmentOperator expressionWithoutCascade)?
  --
  --      cascadeSelector ::=
  --          '[ ' expression '] '
  --        | identifier
  | CascadeExpression Expression -- target
                      [Expression] -- cascadeSections
  -- | An index expression.
  --
  --      indexExpression ::=
  --          [Expression] '[' [Expression] ']'
  | IndexExpressionForCasecade Expression -- index
  | IndexExpressionForTarget   Expression -- target
                               Expression -- index
  -- | An await expression.
  --
  --      awaitExpression ::=
  --          'await' [Expression]
  | AwaitExpression Expression -- expression
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A node that represents a statement.
--
--      statement ::=
--          [Block]
--        | [VariableDeclarationStatement]
--        | [ForStatement]
--        | [ForEachStatement]
--        | [WhileStatement]
--        | [DoStatement]
--        | [SwitchStatement]
--        | [IfStatement]
--        | [TryStatement]
--        | [BreakStatement]
--        | [ContinueStatement]
--        | [ReturnStatement]
--        | [ExpressionStatement]
--        | [FunctionDeclarationStatement]
data Statement
  = Block' Block
  -- | A list of variables that are being declared in a context where a statement is
  --   required.
  --
  --      variableDeclarationStatement ::=
  --          [VariableDeclarationList] ';'
  | VariableDeclarationStatement VariableDeclarationList -- variableList
  -- | A for statement.
  --
  --      forStatement ::=
  --          'for' '(' forLoopParts ')' [Statement]
  --
  --      forLoopParts ::=
  --          forInitializerStatement ';' [Expression]? ';' [Expression]?
  --
  --      forInitializerStatement ::=
  --          [DefaultFormalParameter]
  --        | [Expression]?
  | ForStatement (Maybe VariableDeclarationList) -- variableList
                 (Maybe Expression) -- initialization
                 (Maybe Expression) -- condition
                 [Expression] -- updaters
                 Statement -- body
  -- | A for-each statement.
  --
  --      forEachStatement ::=
  --          'await'? 'for' '(' [DeclaredIdentifier] 'in' [Expression] ')' [Block]
  --        | 'await'? 'for' '(' [SimpleIdentifier] 'in' [Expression] ')' [Block]
  | ForEachStatementWithDeclaration Bool -- isAwait
                                    DeclaredIdentifier -- loopVariable
                                    Expression -- iterator
                                    Statement -- body
  | ForEachStatementWithReference   Bool -- isAwait
                                    SimpleIdentifier -- identifier
                                    Expression -- iterator
                                    Statement -- body
  -- | A while statement.
  --
  --      whileStatement ::=
  --          'while' '(' [Expression] ')' [Statement]
  | WhileStatement Expression -- condition
                   Statement -- body
  -- | A do statement.
  --
  --      doStatement ::=
  --          'do' [Statement] 'while' '(' [Expression] ')' ';'
  | DoStatement Statement -- body
                Expression -- condition
  -- | A switch statement.
  --
  --      switchStatement ::=
  --          'switch' '(' [Expression] ')' '{' [SwitchCase]* [SwitchDefault]? '}'
  | SwitchStatement Expression -- expression
                    [SwitchMember] -- members
  -- | An if statement.
  --
  --      ifStatement ::=
  --          'if' '(' [Expression] ')' [Statement] ('else' [Statement])?
  | IfStatement Expression -- condition
                Statement -- thenStatement
                (Maybe Statement) -- elseStatement
  -- | A try statement.
  --
  --      tryStatement ::=
  --          'try' [Block] ([CatchClause]+ finallyClause? | finallyClause)
  --
  --      finallyClause ::=
  --          'finally' [Block]
  | TryStatement Block -- body
                 [CatchClause] -- catchClauses
                 (Maybe Block) -- finallyBlock
  --   A break statement.
  --
  --      breakStatement ::=
  --          'break' [SimpleIdentifier]? ';'
  | BreakStatement (Maybe SimpleIdentifier) -- label
  -- | A continue statement.
  --
  --      continueStatement ::=
  --          'continue' [SimpleIdentifier]? ';'
  | ContinueStatement (Maybe SimpleIdentifier) -- label
  -- | A return statement.
  --
  --      returnStatement ::=
  --          'return' [Expression]? ';'
  | ReturnStatement (Maybe Expression) -- expression
  -- | An expression used as a statement.
  --
  --      expressionStatement ::=
  --          [Expression]? ';'
  | ExpressionStatement Expression -- expression
  -- | A [FunctionDeclaration] used as a statement.
  | FunctionDeclarationStatement FunctionDeclaration
  -- | An assert statement.
  --
  --      assertStatement ::=
  --          'assert' '(' [Expression] ')' ';'
  | AssertStatement Expression -- condition
                    (Maybe Expression) -- message
  -- | A yield statement.
  --
  --    yieldStatement ::=
  --        'yield' '*'? [Expression] ‘;’
  | YieldStatement Bool -- isStar
                   Expression -- expression
  -- | An empty statement.
  --
  --      emptyStatement ::=
  --          ';'
  | EmptyStatement
  -- | A statement that has a label associated with them.
  --
  --      labeledStatement ::=
  --         [Label]+ [Statement]
  | LabeledStatement [Label] -- labels
                     Statement -- statement
  deriving (Eq, Show, Typeable, Generic, Data)

-- | A compilation unit.
--
--   While the grammar restricts the order of the directives and declarations
--   within a compilation unit, this class does not enforce those restrictions.
--   In particular, the children of a compilation unit will be visited in lexical
--   order even if lexical order does not conform to the restrictions of the
--   grammar.
--
--      compilationUnit ::=
--          directives declarations
--
--      directives ::=
--          [ScriptTag]? [LibraryDirective]? namespaceDirective* [PartDirective]*
--        | [PartOfDirective]
--
--      namespaceDirective ::=
--          [ImportDirective]
--        | [ExportDirective]
--
--      declarations ::=
--          [CompilationUnitMember]*
data CompilationUnit = CompilationUnit (Maybe ScriptTag) -- scriptTag
                                       [Directive] -- directives
                                       [CompilationUnitMember] -- declarations
  deriving (Eq, Show, Typeable, Generic, Data)

