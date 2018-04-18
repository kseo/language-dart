module Language.Dart.Pretty
  ( Pretty
  , prettyPrint
  ) where

import Data.Char (toLower)
import Data.List (partition)
import Text.PrettyPrint
import Text.Printf (printf)

import Language.Dart.Syntax

-- FIXME: Don't omit comments

prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
  | inheritedPrec > currentPrec = parens t
  | otherwise                   = t

class Pretty a where
  pretty :: a -> Doc
  pretty = prettyPrec 0

  prettyPrec :: Int -> a -> Doc
  prettyPrec _ = pretty

instance Pretty MapLiteralEntry where
  prettyPrec p (MapLiteralEntry key value) =
    hsep [ prettyPrec p key
         , colon
         , prettyPrec p value
         ]

instance Pretty TypedLiteral where
  prettyPrec p (MapLiteral isConst mTypeArguments entries)  =
    hsep [ optKeyword isConst "const"
         , maybePP p mTypeArguments
         ] <> braces (hsep (ppIntersperse p comma entries))

  prettyPrec p (ListLiteral isConst mTypeArguments elements) =
    hsep [ optKeyword isConst "const"
         , maybePP p mTypeArguments
         ] <> brackets (hsep (ppIntersperse p comma elements))

instance Pretty Literal where
  prettyPrec p (NullLiteral)      = text "null"
  prettyPrec p (BooleanLiteral b) = text . map toLower $ show b
  prettyPrec p (DoubleLiteral d)  = text (show d)
  prettyPrec p (IntegerLiteral i) = text (show i)
  prettyPrec p (TypedLiteral t)   = prettyPrec p t
  prettyPrec p (StringLiteral' s) = prettyPrec p s
  prettyPrec p (SymbolLiteral ts) = pound <> hsep (punctuate period (map text ts))

instance Pretty InterpolationElement where
  prettyPrec p (InterpolationString s) = text s

  prettyPrec p (InterpolationExpression expression) =
    dollar <> braces (prettyPrec p expression)

instance Pretty SingleStringLiteral where
  prettyPrec p (SimpleStringLiteral value) =
    doubleQuotes (text (concatMap escapeString value))

  prettyPrec p (StringInterpolation elements) =
    doubleQuotes (hcat (map (prettyPrec p) elements))

instance Pretty StringLiteral where
  prettyPrec p (SingleStringLiteral' l) = prettyPrec p l

  prettyPrec p (AdjacentStrings strings) =
    hsep (map (prettyPrec p) strings)

instance Pretty Combinator where
  prettyPrec p (ShowCombinator shownNames)  = text "show" <+>
    hsep (ppIntersperse p comma shownNames)

  prettyPrec p (HideCombinator hiddenNames) = text "hide" <+>
    hsep (ppIntersperse p comma hiddenNames)

-- FIXME: Handle configurations
instance Pretty NamespaceDirective where
  prettyPrec p (ExportDirective _ metadata libraryUri _ combinators) =
    ppMetadata p metadata $$
      hsep [ text "export"
           , prettyPrec p libraryUri
           , hsep (ppIntersperse p comma combinators)
           ] <> semi

  prettyPrec p (ImportDirective _ metadata libraryUri _ isDeferred mPrefix combinators) =
    ppMetadata p metadata $$
      hsep [ text "import"
           , prettyPrec  p libraryUri
           , optKeyword isDeferred "deferred"
           , maybe empty ((text "as" <+>) . prettyPrec p) mPrefix
           , hsep (ppIntersperse p comma combinators)
           ] <> semi

instance Pretty UriBasedDirective where
  prettyPrec p (NamespaceDirective nsDir) = prettyPrec p nsDir

  prettyPrec p (PartDirective _ metadata partUri) =
    ppMetadata p metadata $$ prettyPrec p partUri <> semi

instance Pretty Directive where
  prettyPrec p (UriBasedDirective dir) = prettyPrec p dir

  prettyPrec p (PartOfDirective _ metadata libraryName) =
    ppMetadata p metadata $$
      text "part of" <> prettyPrec p libraryName <> semi

  prettyPrec p (LibraryDirective _ metadata libraryName) =
    ppMetadata p metadata $$
      text "library" <> prettyPrec p libraryName <> semi

instance Pretty AsyncModifier where
  prettyPrec p AsyncStar  = text "async*"
  prettyPrec p Async      = text "async"
  prettyPrec p SyncStar   = text "sync*"
  prettyPrec p Sync       = empty

instance Pretty NormalFormalParameter where
  prettyPrec p (FunctionTypedFormalParameter _ metadata mReturnType identifier mTypeParameters parameters) =
    ppMetadata p metadata $$
      hsep [ maybePP p mReturnType
           , hcat [ prettyPrec p identifier
                  , maybePP p mTypeParameters
                  , prettyPrec p parameters
                  ]
           ]

  prettyPrec p (FieldFormalParameter _ metadata kind explicitThis identifier) =
    ppMetadata p metadata $$
      hsep [ maybePP p kind
           , opt explicitThis (text "this.") <> prettyPrec p identifier
           ]

  prettyPrec p (SimpleFormalParameter _ metadata kind identifier) =
    ppMetadata p metadata $$
      hsep [ maybePP p kind
           , prettyPrec p identifier
           ]

instance Pretty FormalParameter where
  prettyPrec p (NormalFormalParameter' parameter) = prettyPrec p parameter

  prettyPrec p (DefaultFormalParameter parameter kind mDefaultValue) =
    hsep [ prettyPrec p parameter
         , maybe empty ((separator kind <+>) . prettyPrec p) mDefaultValue
         ]
    where separator Positional = equals
          separator Named = colon

instance Pretty FormalParameterList where
  prettyPrec p (FormalParameterList parameters) =
    let (normals, defaults) = partition isNormal parameters
     in parens $ hcat [ hsep (ppIntersperse p comma normals)
                      , opt (not $ null defaults) (comma <+> (matchingBracket (head defaults)) (hsep (ppIntersperse p comma defaults)))
                      ]
    where isNormal (NormalFormalParameter' _) = True
          isNormal _ = False
          matchingBracket (DefaultFormalParameter _ Positional _) = brackets
          matchingBracket _ = braces

instance Pretty ConstructorName where
  prettyPrec p (ConstructorName type' mName) =
    hsep [ prettyPrec p type'
         , maybe empty ((period <>) . prettyPrec p) mName
         ]

instance Pretty SimpleIdentifier where
  prettyPrec _ (SimpleIdentifier token) = text token

instance Pretty LibraryIdentifier where
  prettyPrec p (LibraryIdentifier components) =
    hcat (ppIntersperse p period components)

instance Pretty Identifier where
  prettyPrec p (SimpleIdentifier' simpleId) = prettyPrec p simpleId

  prettyPrec p (PrefixedIdentifier prefix identifier) =
    hcat [ prettyPrec p prefix
         , period
         , prettyPrec p identifier
         ]

  prettyPrec p (LibraryIdentifier' libraryName) = prettyPrec p libraryName

instance Pretty TypeArgumentList where
  prettyPrec p (TypeArgumentList args) =
    angleBrackets (hsep (ppIntersperse p comma args))

instance Pretty TypeName where
  prettyPrec p (TypeName name typeArgList) = prettyPrec p name <> maybePP p typeArgList

instance Pretty PropertyKeyword where
  prettyPrec p Get   = text "get"
  prettyPrec p Set   = text "set"
  prettyPrec p Empty = empty

instance Pretty ArgumentList where
  prettyPrec p (ArgumentList args) =
    parens (hsep (ppIntersperse p comma args))

instance Pretty Annotation where
  prettyPrec p (Annotation name mConstructorName mArguments) =
    hcat [ at
         , prettyPrec p name
         , maybe empty ((period <>) . prettyPrec p) mConstructorName
         , maybePP p mArguments
         ]

instance Pretty ExtendsClause where
  prettyPrec p (ExtendsClause superclass) = text "extends" <+>
    prettyPrec p superclass

instance Pretty WithClause where
  prettyPrec p (WithClause mixinTypes) = text "with" <+>
    hsep (ppIntersperse p comma mixinTypes)

instance Pretty ImplementsClause where
  prettyPrec p (ImplementsClause interfaces) = text "implements" <+>
    hsep (ppIntersperse p comma interfaces)

instance Pretty TypeAlias where
  prettyPrec p (ClassTypeAlias _ metadata name mTypeParameters isAbstract superclass withClause mImplementsClause) =
    ppMetadata p metadata $$
      hsep [ optKeyword isAbstract "abstract"
           , text "class"
           , hcat [ prettyPrec p name
                  , maybePP p mTypeParameters
                  ]
           , equals
           , prettyPrec p superclass
           , prettyPrec p withClause
           , maybePP p mImplementsClause
           ] <> semi

  prettyPrec p (FunctionTypeAlias _ metadata mReturnType name mTypeParameters parameters) =
    ppMetadata p metadata $$
      hsep [ text "typedef"
           , maybePP p mReturnType
           , hcat [ prettyPrec p name
                  , maybePP p mTypeParameters
                  , prettyPrec p parameters
                  ]
           ] <> semi

instance Pretty EnumConstantDeclaration where
  prettyPrec p (EnumConstantDeclaration _ metadata name) =
    ppMetadata p metadata $$ prettyPrec p name

instance Pretty TypeParameter where
  prettyPrec p (TypeParameter _ metadata name mBound) =
    ppMetadata p metadata $$
      hsep [ prettyPrec p name
           , maybe empty ((text "extends" <+>) . prettyPrec p) mBound
           ]

instance Pretty TypeParameterList where
  prettyPrec p (TypeParameterList typeParameters) =
    angleBrackets (hsep (ppIntersperse p comma typeParameters))

instance Pretty Label where
  prettyPrec p (Label label) = prettyPrec p label <> char ':'

instance Pretty FinalConstVarOrType where
  prettyPrec p (FCVTFinal type')  = text "final" <+> maybe empty (prettyPrec p) type'
  prettyPrec p (FCVTConst type')  = text "const" <+> maybe empty (prettyPrec p) type'
  prettyPrec p (FCVTType type')   = prettyPrec p type'
  prettyPrec p FCVTVar            = text "var"

instance Pretty FinalVarOrType where
  prettyPrec p (FVTFinal type') = text "final" <+> prettyPrec p type'
  prettyPrec p (FVTType type')  = prettyPrec p type'
  prettyPrec p FVTVar           = text "var"

instance Pretty VariableDeclaration where
  prettyPrec p (VariableDeclaration name mInitializer) =
    hsep [ prettyPrec p name
         , maybe empty ((equals <+>) . prettyPrec p) mInitializer
         ]

instance Pretty VariableDeclarationList where
  prettyPrec p (VariableDeclarationList _ metadata kind variables) =
    ppMetadata p metadata $$
      prettyPrec p kind <+> hsep (ppIntersperse p comma variables)

instance Pretty CatchClause where
  prettyPrec p (CatchClause exceptionParameter mStackTraceParameter body) =
      text "catch" <+> parens (ppExceptionParameter p exceptionParameter mStackTraceParameter) $$ prettyPrec p body
    where ppExceptionParameter p ep Nothing = prettyPrec p ep
          ppExceptionParameter p ep (Just stp) = hsep (ppIntersperse p comma [ep, stp])
  prettyPrec p (OnClause exceptionType Nothing body) =
     text "on" <+> prettyPrec p exceptionType $$ prettyPrec p body
  prettyPrec p (OnClause exceptionType (Just (exceptionParameter, mStackTraceParameter)) body) =
     text "on" <+> prettyPrec p exceptionType
     <+> prettyPrec p (CatchClause exceptionParameter mStackTraceParameter body)

instance Pretty SwitchMember where
  prettyPrec p (SwitchCase labels expression statements) =
    vcat (map (prettyPrec p) labels) $$
      vcat (ppSwitchCase p expression : map (nest 2 . prettyPrec p) statements)
    where ppSwitchCase p expression = text "case" <+> prettyPrec p expression <> colon

  prettyPrec p (SwitchDefault labels statements) =
    vcat (map (prettyPrec p) labels) $$
      vcat (ppSwitchDefault : map (nest 2 . prettyPrec p) statements)
    where ppSwitchDefault = text "default" <> colon

instance Pretty DeclaredIdentifier where
  prettyPrec p (DeclaredIdentifier _ metadata kind identifier) =
    ppMetadata p metadata $$
      hsep [ prettyPrec p kind
           , prettyPrec p identifier
           ]

instance Pretty Statement where
  prettyPrec p (Block' block) = prettyPrec p block

  prettyPrec p (VariableDeclarationStatement variableList) =
    prettyPrec p variableList <> semi

  prettyPrec p (ForStatement mVariableList mInitialization mCondition updaters body) =
    text "for" <+> (parens $ hsep [ maybe empty ((<> semi) . prettyPrec p) mVariableList
                                  , maybe empty ((<> semi) . prettyPrec p) mInitialization
                                  , maybePP p mCondition <> semi
                                  , hsep (ppIntersperse p comma updaters)
                                  ]) $+$ prettyNestedStmt p body

  prettyPrec p (ForEachStatementWithDeclaration isAwait loopVariable iterator body) = undefined
    hsep [ optKeyword isAwait "await"
         , text "for"
         , parens $ hsep [ prettyPrec p loopVariable
                         , text "in"
                         , prettyPrec p iterator
                         ]
         , prettyPrec p body
         ]

  prettyPrec p (ForEachStatementWithReference isAwait identifier iterator body) =
    hsep [ optKeyword isAwait "await"
         , text "for"
         , parens $ hsep [ prettyPrec p identifier
                         , text "in"
                         , prettyPrec p iterator
                         ]
         , prettyPrec p body
         ]

  prettyPrec p (WhileStatement condition body) =
    text "while" <+> parens (prettyPrec p condition) $+$ prettyNestedStmt 0 body

  prettyPrec p (DoStatement body condition) =
    text "do" $+$ prettyPrec p body <+> text "while" <+> parens (prettyPrec p condition) <> semi

  prettyPrec p (SwitchStatement expression members) =
    text "switch" <+> parens (prettyPrec p expression)
      $$ braceBlock (map (prettyPrec p) members)

  prettyPrec p (IfStatement condition thenStatement mElseStatement) =
    text "if" <+> parens (prettyPrec p condition) $+$
      prettyNestedStmt 0 thenStatement $+$ ppElseStatement mElseStatement
    where ppElseStatement Nothing = empty
          ppElseStatement (Just elseStatement) = text "else" $+$ prettyNestedStmt 0 elseStatement

  prettyPrec p (TryStatement body catchCaluses mFinallyBlock) =
    text "try" $$ prettyPrec p body $$
      vcat (map (prettyPrec p) catchCaluses ++ [ppFinally mFinallyBlock])
    where ppFinally Nothing = empty
          ppFinally (Just finally) = text "finally" $$ prettyPrec p finally

  prettyPrec p (BreakStatement mLabel) =
    text "break" <+> maybePP p mLabel <> semi

  prettyPrec p (ContinueStatement mLabel) =
    text "continue" <+> maybePP p mLabel <> semi

  prettyPrec p (ReturnStatement mExpression) =
    text "return" <+> maybePP p mExpression <> semi

  prettyPrec p (ExpressionStatement expression) =
    prettyPrec p expression <> semi

  prettyPrec p (FunctionDeclarationStatement functionDeclaration) =
    prettyPrec p functionDeclaration <> semi

  -- FIXME: Show message
  prettyPrec p (AssertStatement condition _) =
    hcat [ text "assert"
         , parens (prettyPrec p condition)
         , semi
         ]

  prettyPrec p (YieldStatement isStar expression) =
    hsep [ text "yield"
         , opt isStar star
         , prettyPrec p expression
         ] <> semi

  prettyPrec p (EmptyStatement) = semi

  prettyPrec p (LabeledStatement labels statement) =
    vcat (map (prettyPrec p) labels) $$
      prettyPrec p statement

instance Pretty Block where
  prettyPrec p (Block statements) = braceBlock (map (prettyPrec p) statements)

instance Pretty NewOrConst where
  prettyPrec p NCNew    = text "new"
  prettyPrec p NCConst  = text "const"

instance Pretty InvocationExpression where
  prettyPrec p (FunctionExpressionInvocation function mTypeArguments argumentList) =
    hcat [ prettyPrec p function
         , maybePP p mTypeArguments
         , prettyPrec p argumentList
         ]

  prettyPrec p (MethodInvocation mTarget methodName mTypeArguments argumentList) =
    hcat [ maybe empty ((<> period) . prettyPrec p) mTarget
         , prettyPrec p methodName
         , maybePP p mTypeArguments
         , prettyPrec p argumentList
         ]

instance Pretty Expression where
  prettyPrec p (Literal' literal) = prettyPrec p literal

  prettyPrec p (Identifier' identifier) = prettyPrec p identifier

  prettyPrec p (PrefixExpression  operator operand) =
    parenPrec p 15 $ text operator <> prettyPrec 15 operand

  prettyPrec p (PostfixExpression operand operator) =
    parenPrec p 16 $ prettyPrec 15 operand <> text operator

  prettyPrec p (BinaryExpression leftOperand operator rightOperand) =
    let prec = opPrec operator
     in parenPrec p prec $ hsep [ prettyPrec prec leftOperand
                                , text operator
                                , prettyPrec prec rightOperand
                                ]

  prettyPrec p (AssignmentExpression leftHandSide operator rightHandSide) =
    hsep [ prettyPrec p leftHandSide
         , text operator
         , prettyPrec p rightHandSide
         ]

  prettyPrec p (FunctionExpression' functionExpression) = prettyPrec p functionExpression

  prettyPrec p (InstanceCreationExpression newOrConst constructorName argumentList) =
    hsep [ prettyPrec p newOrConst
         , hcat [ prettyPrec p constructorName
                , prettyPrec p argumentList
                ]
         ]

  prettyPrec p (AsExpression expression type') =
    parenPrec p 8 $ hsep [ prettyPrec 8 expression
                         , text "as"
                         , prettyPrec 8 type'
                         ]

  prettyPrec p (IsExpression expression isNot type') =
    parenPrec p 8 $ hsep [ prettyPrec 8 expression
                         , text "is"
                         , opt isNot exclamation
                         , prettyPrec 8 type'
                         ]

  prettyPrec p (ThrowExpression expression) = text "throw" <+> prettyPrec p expression
  prettyPrec p (RethrowExpression)          = text "rethrow"

  prettyPrec p (ThisExpression)  = text "this"
  prettyPrec p (SuperExpression) = text "super"

  prettyPrec p (ParenthesizedExpression expression) = parens (prettyPrec p expression)

  prettyPrec p (PropertyAccess target propertyName) =
    hcat [ prettyPrec p target
         , period
         , prettyPrec p propertyName
         ]

  prettyPrec p (NamedExpression name expression) =
    hsep [ prettyPrec p name <> colon
         , prettyPrec p expression
         ]

  prettyPrec p (InvocationExpression invocationExpression) =
    prettyPrec p invocationExpression

  prettyPrec p (ConditionalExpression condition thenExpression elseExpression) =
    parenPrec p 3 $ hsep [ prettyPrec 3 condition
                         , questionMark
                         , prettyPrec p thenExpression
                         , colon
                         , prettyPrec 3 elseExpression
                         ]

  prettyPrec p (CascadeExpression target cascadeSections) =
    let sections = target:cascadeSections
     in parenPrec p 2 $ hsep (ppIntersperse 2 (text "..") sections)

  prettyPrec p (IndexExpressionForCasecade index) = brackets (prettyPrec p index)

  prettyPrec p (IndexExpressionForTarget target index) =
    prettyPrec p target <> brackets (prettyPrec p index)

  prettyPrec p (AwaitExpression expression) =
    hsep [ text "await"
         , prettyPrec p expression
         ]

instance Pretty FunctionBody where
  prettyPrec p (BlockFunctionBody asyncModifier block) =
    prettyPrec p asyncModifier <+> prettyPrec p block

  prettyPrec p (EmptyFunctionBody) = semi

  prettyPrec p (ExpressionFunctionBody isAsync expression) =
    hsep [ optKeyword isAsync "async"
         , text "=>"
         , prettyPrec p expression
         ] <> semi

  prettyPrec p (NativeFunctionBody stringLiteral) =
    text "native" <+> prettyPrec p stringLiteral <> semi

instance Pretty FunctionExpression where
  prettyPrec p (FunctionExpression mTypeParameters parameters body) =
    case body of
      BlockFunctionBody _ _      -> ppParams $$  prettyPrec p body
      ExpressionFunctionBody _ _ -> ppParams <+> prettyPrec p body
      _                          -> ppParams <> semi
    where ppParams = maybePP p mTypeParameters <> prettyPrec p parameters

instance Pretty FunctionDeclaration where
  prettyPrec p (FunctionDeclaration _ metadata isExternal mReturnType propertyKeyword name (FunctionExpression mTypeParameters parameters body)) =
    ppMetadata p metadata $$
      hsep [ optKeyword isExternal "external"
           , maybePP p mReturnType
           , prettyPrec p propertyKeyword
           , hcat [ prettyPrec p name
                  , maybePP p mTypeParameters
                  , prettyPrec p parameters
                  ]
           ] $$$ prettyPrec p body
   where ($$$) = case body of
                   (ExpressionFunctionBody _ _) -> (<+>)
                   (BlockFunctionBody _ _) -> ($$)
                   _ -> (<>)

instance Pretty ConstructorInitializer where
  prettyPrec p (RedirectingConstructorInvocation mConstructorName argumentList) =
    hcat [ text "this"
         , maybe empty ((period <>) . prettyPrec p) mConstructorName
         , prettyPrec p argumentList
         ]

  prettyPrec p (ConstructorFieldInitializer explicitThis fieldName expression) =
    hsep [ hcat [ opt explicitThis (text "this.")
                , prettyPrec p fieldName
                ]
         , equals
         , prettyPrec p expression
         ]

  prettyPrec p (SuperConstructorInvocation mConstructorName argumentList) =
    hcat [ text "super"
         , maybe empty ((period <>) . prettyPrec p) mConstructorName
         , prettyPrec p argumentList
         ]

instance Pretty MethodModifier where
  prettyPrec _ Abstract = text "abstract"
  prettyPrec _ Static   = text "static"

instance Pretty NamedCompilationUnitMember where
  prettyPrec p (FunctionDeclaration' funDecl) = prettyPrec p funDecl

  prettyPrec p (TypeAlias typeAlias) = prettyPrec p typeAlias

  prettyPrec p (EnumDeclaration _ metadata name constants) =
    ppMetadata p metadata $$
      hsep [ text "enum"
           , prettyPrec p name
           ] $$ braceBlock (ppIntersperse p comma constants)

  prettyPrec p (ClassDeclaration _ metadata isAbstract name mTypeParameters mExtendsClause mWithClause mImplementsClause members) =
    ppMetadata p metadata $$
      hsep [ optKeyword isAbstract "abstract"
           , text "class"
           , prettyPrec p name <> maybePP p mTypeParameters
           , maybePP p mExtendsClause
           , maybePP p mWithClause
           , maybePP p mImplementsClause
           ] $$ braceBlock (map (prettyPrec p) members)

instance Pretty CompilationUnitMember where
  prettyPrec p (TopLevelVariableDeclaration _ metadata variableList) =
    ppMetadata p metadata $$ prettyPrec p variableList

  prettyPrec p (NamedCompilationUnitMember member) = prettyPrec p member

instance Pretty ScriptTag where
  prettyPrec _ (ScriptTag token) = text token

instance Pretty CompilationUnit where
  prettyPrec p (CompilationUnit mScriptTag dirs members) =
    vcat $ [maybePP p mScriptTag] ++ map (prettyPrec p) dirs ++ map (prettyPrec p) members

ppMetadata :: Int -> [Annotation] -> Doc
ppMetadata p metadata = vcat (map (prettyPrec p) metadata)

instance Pretty ClassMember where
  prettyPrec p (ConstructorDeclaration _ metadata isExternal isConst isFactory className mName parameters initializers mRedirectedConstructor mBody) =
    ppMetadata p metadata $$
      case mRedirectedConstructor of
        Nothing -> hsep [ optKeyword isExternal "external"
                        , optKeyword isConst "const"
                        , optKeyword isFactory "factory"
                        , hcat [ prettyPrec p className
                               , maybe empty ((period <>) . prettyPrec p) mName
                               , prettyPrec p parameters
                               ]
                        , opt (not $ null initializers) (colon <+> vcat (ppIntersperse p comma initializers))
                        ] <> maybe semi (const empty) mBody $$ maybePP p mBody
        Just redirectedConstructor -> hsep [ optKeyword isConst "const"
                                           , text "factory"
                                           , hcat [ prettyPrec p className
                                                  , maybe empty ((period <>) . prettyPrec p) mName
                                                  , prettyPrec p parameters
                                                  ]
                                           , equals
                                           , prettyPrec p redirectedConstructor <> semi
                                           ]

  prettyPrec p (MethodDeclaration _ metadata isExternal methodModifier mReturnType propertyKeyword isOperator name mTypeParameters mParameters body) =
    ppMetadata p metadata $$
      hsep [ optKeyword isExternal "external"
           , maybePP p methodModifier
           , maybePP p mReturnType
           , prettyPrec p propertyKeyword
           , optKeyword isOperator "operator"
           , hcat [ prettyPrec p name, maybePP p mTypeParameters, maybePP p mParameters]
           ] $$ prettyPrec p body
  prettyPrec p (FieldDeclaration _ metadata isStatic fieldList) =
    ppMetadata p metadata $$
      optKeyword isStatic "static" <+> prettyPrec p fieldList <> semi

ppIntersperse :: (Pretty a) => Int -> Doc -> [a] -> [Doc]
ppIntersperse p s as = punctuate s (map (prettyPrec p) as)

-----------------------------------------------------------------------
---- Help functionality
prettyNestedStmt :: Int -> Statement -> Doc
prettyNestedStmt prio b@(Block' _) = prettyPrec prio b
prettyNestedStmt prio s = nest 2 (prettyPrec prio s)

maybePP :: Pretty a => Int -> Maybe a -> Doc
maybePP p = maybe empty (prettyPrec p)

pound :: Doc
pound = char '#'

period :: Doc
period = char '.'

star :: Doc
star = char '*'

at :: Doc
at = char '@'

dollar :: Doc
dollar = char '$'

questionMark :: Doc
questionMark = char '?'

exclamation :: Doc
exclamation = char '!'

angleBrackets :: Doc -> Doc -- ^ Wrap document in @<...>@
angleBrackets p = char '<' <> p <> char '>'

opt :: Bool -> Doc -> Doc
opt x a = if x then a else empty

optKeyword :: Bool -> String -> Doc
optKeyword x s = opt x (text s)

block :: Char -> Char -> [Doc] -> Doc
block open close xs = char open
    $+$ nest 2 (vcat xs)
    $+$ char close

braceBlock :: [Doc] -> Doc
braceBlock = block '{' '}'

bracketBlock :: [Doc] -> Doc
bracketBlock = block '[' ']'

escapeGeneral :: Char -> String
escapeGeneral '\b' = "\\b"
escapeGeneral '\t' = "\\t"
escapeGeneral '\n' = "\\n"
escapeGeneral '\f' = "\\f"
escapeGeneral '\r' = "\\r"
escapeGeneral '\\' = "\\\\"
escapeGeneral c | c >= ' ' && c < '\DEL' = [c]
                | c <= '\xFFFF' = printf "\\u%04x" (fromEnum c)
                | otherwise = error $ "Language.Dart.Pretty.escapeGeneral: Char " ++ show c ++ " too large for Dart char"

escapeChar :: Char -> String
escapeChar '\'' = "\\'"
escapeChar c = escapeGeneral c

escapeString :: Char -> String
escapeString '"' = "\\\""
escapeString c | c <= '\xFFFF' = escapeGeneral c
               | otherwise = escapeGeneral lead ++ escapeGeneral trail
                   where c' = fromEnum c - 0x010000
                         lead = toEnum $ 0xD800 + c' `div` 0x0400
                         trail = toEnum $ 0xDC00 + c' `mod` 0x0400

-- Operator precedence of binary operators
-- 20.2 of Dart Programming Language Specification
-- http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-408.pdf
opPrec :: String -> Int
opPrec "*"  = 14
opPrec "/"  = 14
opPrec "~/" = 14
opPrec "%"  = 14
opPrec "+"  = 13
opPrec "-"  = 13
opPrec "<<" = 12
opPrec ">>" = 12
opPrec "&"  = 11
opPrec "^"  = 10
opPrec "|"  = 9
opPrec "<"  = 8
opPrec ">"  = 8
opPrec "<=" = 8
opPrec ">=" = 8
opPrec "==" = 7
opPrec "!=" = 7
opPrec "&&" = 6
opPrec "||" = 5
opPrec "??" = 4

