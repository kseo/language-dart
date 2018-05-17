{-# LANGUAGE RankNTypes, RecordWildCards #-}

module Language.Dart.Grammar (module Language.Dart.Grammar,
                              module Language.Dart.GrammarDeclaration) where

import Control.Applicative
import Data.Char (chr, isDigit, isHexDigit)
import Data.List (intersperse)
import Data.Monoid (Dual(..), Endo(..), (<>))
import Numeric (readHex)
import Text.Grampa hiding (Grammar)
import Text.Grampa.Combinators (concatSome, count, delimiter, flag, moptional, upto)
import Text.Grampa.ContextFree.LeftRecursive (Parser)
import qualified Text.Grampa as Lexical (identifier)
import qualified Text.Grampa as Grampa
import Text.Parser.Char (hexDigit)
import Text.Parser.Combinators (count, sepBy, sepBy1, sepEndBy, try)
import Text.Parser.Token (braces, brackets, parens)
import Language.Dart.GrammarDeclaration (Grammar(..), blockComment, endOfLineComment)
import Language.Dart.Syntax

import Debug.Trace
import Prelude hiding (exponent)

grammar :: GrammarBuilder Grammar Grammar Parser String
grammar Grammar{..} = Grammar{
   typedLiteral=
           listLiteral
       <|> mapLiteral,
     -- | A literal map.
   mapLiteral=
        MapLiteral
        <$> flag (keyword "const")
        <*> optional typeArguments
        <*> braces (sepEndBy mapLiteralEntry (delimiter ",")),
   -- | A list literal.
   listLiteral=
        ListLiteral
        <$> flag (keyword "const")
        <*> optional (delimiter "<" *> typeArguments <* delimiter ">")
        <*> brackets (sepEndBy expression (delimiter ",")),
   -- | An expression embedded in a string interpolation.
   interpolationExpression=
           delimiter "$" *> (InterpolationExpression . Identifier' . SimpleIdentifier' <$> simpleIdentifier)
       <|> delimiter "$" *> braces (InterpolationExpression <$> expression),
   -- | A non-empty substring of an interpolated string.
   interpolationStringNoSingleQuote=
         InterpolationString <$> (takeCharsWhile1 (`notElem` "\\\'$\r\n")
                                  <|> (:[]) <$> characterEscape),
   interpolationStringNoDoubleQuote=
         InterpolationString <$> (takeCharsWhile1 (`notElem` "\\\"$\r\n")
                                  <|> (:[]) <$> characterEscape),
   characterEscape= string "\\" *> ('\b' <$ string "b"
                                    <|> '\f' <$ string "b"
                                    <|> '\r' <$ string "r"
                                    <|> '\n' <$ string "n"
                                    <|> '\t' <$ string "t"
                                    <|> '\v' <$ string "v"
                                    <|> chr . fst . head . readHex
                                        <$> (string "x" *> count 2 hexDigit
                                             <|> string "u" *> (count 4 hexDigit
                                                                <|> string "{"
                                                                 *> ((:) <$> hexDigit <*> upto 4 hexDigit)
                                                                <*  string "}"))
                                    <|> satisfyChar (`notElem` "\r\n")),
   singleStringLiteral=
        SingleStringLiteral'
        <$> (    SimpleStringLiteral <$> simpleStringLiteral
             <|> StringInterpolation <$> stringInterpolation),
   -- | A string literal expression that does not contain any interpolations.
   simpleStringLiteral=
           rawStringLiteral
       <|> basicStringLiteral,
   rawStringLiteral=
        lexicalToken $
        string "r" *> (string "'''"
                           *> concatMany (takeCharsWhile1 (/= '\'')
                                          <|> string "'" <* notFollowedBy (string "''"))
                         <* string "'''"
                       <|> string "\"\"\""
                           *> concatMany (takeCharsWhile1 (/= '\"')
                                          <|> string "\"" <* notFollowedBy (string "\"\""))
                       <|> string "'" *> takeCharsWhile (`notElem` "\'\n") <* string "'"
                       <|> string "\"" *> takeCharsWhile (`notElem` "\"\n") <* string "\""),
   basicStringLiteral=
      lexicalToken (multiLineStringLiteral <|> singleLineStringLiteral),
   multiLineStringLiteral=
           string "'''" *> concatMany (takeCharsWhile1 (`notElem` "\\\'$")
                                       <|> string "'" <* notFollowedBy (string "''")) <* string "'''"
       <|> string "\"\"\"" *> concatMany (takeCharsWhile1 (`notElem` "\\\"$")
                                          <|> string "\"" <* notFollowedBy (string "\"\"")) <* string "\"\"\"",
   singleLineStringLiteral=
           string "'" *> takeCharsWhile (`notElem` "\\\'$\r\n") <* string "'"
       <|> string "\"" *> takeCharsWhile (`notElem` "\\\"$\r\n") <* string "\"",
   -- | A string interpolation literal.
   stringInterpolation=
      lexicalToken $
            (:) <$  string "'"
                <*> (interpolationExpression <|> interpolationStringNoSingleQuote <* notSatisfyChar (== '\''))
                <*> many (interpolationExpression <|> interpolationStringNoSingleQuote)
                <*  string "'"
        <|> (:) <$  string "\""
                <*> (interpolationExpression <|> interpolationStringNoDoubleQuote <* notSatisfyChar (== '"'))
                <*> many (interpolationExpression <|> interpolationStringNoDoubleQuote)
                <*  string "\"",
   stringLiteral=
           singleStringLiteral
       <|> AdjacentStrings <$> adjacentStrings,
   -- | Two or more string literals that are implicitly concatenated because of being
   --  adjacent (separated only by whitespace).
   --  While the grammar only allows adjacent strings when all of the strings are of
   --  the same kind (single line or multi-line), this class doesn't enforce that
   --  restriction.
   adjacentStrings= (:) <$> singleStringLiteral <*> some singleStringLiteral,
   literal=
           BooleanLiteral <$> booleanLiteral
       <|> DoubleLiteral <$> doubleLiteral
       <|> IntegerLiteral <$> integerLiteral
       <|> TypedLiteral <$> typedLiteral
       <|> nullLiteral
       <|> StringLiteral' <$> stringLiteral
       <|> SymbolLiteral <$> symbolLiteral,
    -- A boolean literal expression.
   booleanLiteral=
        False <$ keyword "false" <|> True <$ keyword "true",
    -- A floating point literal expression.
   doubleLiteral=
        lexicalToken $
        read <$> ((takeCharsWhile1 isDigit <|> pure "0") <> string "." <> takeCharsWhile isDigit <> moptional exponent
                   <|> takeCharsWhile1 isDigit <> exponent),
   exponent=
        lexicalToken $
        (string "e" <|> string "E") <> moptional (string "+" <|> string "-") <> takeCharsWhile1 isDigit,
   -- | An integer literal expression.
   integerLiteral=
        lexicalToken $
        decimalIntegerLiteral
        <|> hexadecimalIntegerLiteral,
   decimalIntegerLiteral=
        read <$> takeCharsWhile1 isDigit <* notSatisfyChar (`elem` ".eE"),
   hexadecimalIntegerLiteral=
        fst . head . readHex <$>
        (string "0x" *> takeCharsWhile1 isHexDigit
         <|> string "0X" *> takeCharsWhile1 isHexDigit),
   -- | A null literal expression.
   nullLiteral=
        NullLiteral <$ keyword "null",
   -- | A symbol literal expression.
   symbolLiteral=
        string "#" *> ((:[]) <$> operator 
                       <|> sepBy Lexical.identifier (delimiter ".")),
    -- class are always children of the class variableDeclarationList.
   variableDeclaration=
        VariableDeclaration <$> simpleIdentifier <*> optional (delimiter "=" *> expression),
   variableDeclarationList=
        VariableDeclarationList Nothing [] <$> finalConstVarOrType <*> sepBy1 variableDeclaration (delimiter ","),
   finalConstVarOrType=
        FCVTFinal <$ keyword "final" <*> optional typeName
        <|> FCVTConst <$ keyword "const" <*> optional typeName
        <|> FCVTVar <$ keyword "var"
        <|> FCVTType <$> typeName,
   declaredIdentifier=
        DeclaredIdentifier Nothing <$> metadata <*> finalConstVarOrType <*> simpleIdentifier,
   functionDeclaration=
            (FunctionDeclaration Nothing <$> metadata <*> pure True <* keyword "external")
            <**> functionSignature <*> pure EmptyFunctionBody
        <|> (FunctionDeclaration Nothing <$> metadata <*> pure False) <**> functionSignature <*> functionBody,
   functionSignature= let applySignature returnType propKwd name typeParams params f body = 
                             f returnType propKwd name (FunctionExpression typeParams params body)
                      in applySignature 
                             <$> optional typeName
                             <*> (Get <$ keyword "get")
                             <*> simpleIdentifier
                             <*> pure Nothing
                             <*> pure (FormalParameterList [])
                         <|>
                         applySignature
                             <$> optional typeName
                             <*> (Set <$ keyword "set" <|> pure Empty)
                             <*> simpleIdentifier
                             <*> optional typeParameterList
                             <*> formalParameterList,
   withClause=
        WithClause <$ keyword "with" <*> sepBy typeName (delimiter ","),
   implementsClause=
        ImplementsClause <$ keyword "implements" <*> sepBy typeName (delimiter ","),
   typeAlias=
        TypeAlias <$ keyword "typedef" <*> typeAliasBody,
   typeAliasBody=
           classTypeAlias
       <|> functionTypeAlias,
   -- | A class type alias.
   classTypeAlias=
        ClassTypeAlias Nothing []
        <$> simpleIdentifier
        <*> optional typeParameterList 
        <*  delimiter "="
        <*> flag (keyword "abstract")
        <*> typeName
        <*> withClause
        <*> optional implementsClause
         <* delimiter ";",
   -- | A function type alias.
   functionTypeAlias=
        FunctionTypeAlias Nothing []
        <$> optional typeName
        <*> simpleIdentifier
        <*> optional typeParameterList
        <*> formalParameterList
         <* delimiter ";",
   -- | The declaration of a class.
   classDeclaration=
        ClassDeclaration Nothing []
        <$> flag (keyword "abstract")
        <*  keyword "class"
        <*> simpleIdentifier
        <*> optional typeParameterList
        <**> pure uncurry
        <*> ((,) . Just <$> extendsClause <*> optional withClause
             <|> pure (Nothing, Nothing))
        <*> optional implementsClause
        <*> braces (many classMember),
   classMember= 
            constructorDeclaration 
        <|> fieldDeclaration
        <|> methodDeclaration,
   -- | The declaration of an enumeration.
   enumType=
        EnumDeclaration Nothing
        <$> metadata
        <*  keyword "enum"
        <*> simpleIdentifier
        <*> braces (sepEndBy (EnumConstantDeclaration Nothing [] <$> simpleIdentifier) (delimiter ",")),
   -- | unit.
   compilationUnitMember=
           NamedCompilationUnitMember 
           <$> (classDeclaration
                <|> typeAlias
                <|> FunctionDeclaration' <$> functionDeclaration
                -- <|> methodDeclaration
                <|> enumType)
       <|> topLevelVariableDeclaration,
   -- | The declaration of one or more top-level variables of the same type.
   topLevelVariableDeclaration=
       TopLevelVariableDeclaration Nothing [] 
       <$> (variableDeclarationList
            <|> VariableDeclarationList Nothing [] FCVTVar <$> ((:[]) <$> variableDeclaration))
       <*  delimiter ";",
    -- A constructor declaration.
   constructorDeclaration=
        -- Can't tell an ordinary constructor apart from a method, so parse it as a method
       constructorSignature <*> ({- Just <$> functionBody <|> -} Nothing <$ delimiter ";")
       <|> uncurry (ConstructorDeclaration Nothing [] False False False) 
           <$> constructorName 
           <*> formalParameterList 
           <*  delimiter ":" 
           <*> ((:[]) <$> (RedirectingConstructorInvocation <$ keyword "this"
                           <*> optional (delimiter "." *> simpleIdentifier)
                           <*> argumentList))
           <*> pure Nothing
           <*> pure Nothing,
   constructorSignature=
       (uncurry <$> (ConstructorDeclaration Nothing []
                         <$> flag (keyword "external") <*> flag (keyword "const") <*> (True <$ keyword "factory"))
                    <*> factoryName
        <|> uncurry <$> (ConstructorDeclaration Nothing []
                         <$> flag (keyword "external") <*> flag (keyword "const") <*> pure False)
                    <*> constructorName)
       <*> formalParameterList
       <*> moptional initializerList
       <*> optional (delimiter "=" *> constructorDesignation),
    constructorDesignation=
       do t <- typeName
          ConstructorName t
             <$> optional (delimiter "." *> simpleIdentifier
                           -- exclude the ambiguous case of preceding simple identifier
                           <* case t
                              of TypeName SimpleIdentifier'{} Nothing -> empty
                                 _ -> pure ()),
   constructorName=
       (,) <$> (SimpleIdentifier' <$> simpleIdentifier) <*> optional (delimiter "." *> simpleIdentifier),
   factoryName=
       (,) <$> identifier <*> optional (delimiter "." *> simpleIdentifier),
   initializerList=
       delimiter ":" *> sepBy1 constructorInitializer (delimiter ","),
   -- | A method declaration.
   methodDeclaration=
        methodSignature <*> functionBody,
   methodSignature=
        uncurry <$> (MethodDeclaration Nothing []
                     <$> flag (keyword "external")
                     <*> optional (Abstract <$ keyword "abstract" <|> Static <$ keyword "static")
                     <*> optional typeName
                     <*> (Get <$ keyword "get" <|> Set <$ keyword "set" <|> pure Empty))
                <*> methodName
                <*> optional typeParameterList
                <*> optional formalParameterList,
   methodName=
        (,) False <$> simpleIdentifier
        <|> (,) True <$ keyword "operator" <*> simpleIdentifier,
   -- | The declaration of one or more fields of the same type.
   fieldDeclaration=
        FieldDeclaration Nothing []
        <$> flag (keyword "static")
        <*> variableDeclarationList
        <*  delimiter ";",
   -- | name is visible within a name scope.
   namespaceDirective=
        exportDirective
        <|> importDirective,
   -- | An export directive.
   exportDirective=
        ExportDirective Nothing
        <$> metadata
        <*  keyword "export"
        <*> stringLiteral
        <*> pure []  -- configurations
        <*> many combinator
        <*  delimiter ";",
   -- | An import directive.
   importDirective=
        (ImportDirective Nothing
         <$> metadata
         <*  keyword "import"
         <*> stringLiteral
         <*> pure []  -- configurations
         <*> pure False
         <*> optional (keyword "as" *> simpleIdentifier)
         <|>
         ImportDirective Nothing
          <$> metadata
          <*  keyword "import"
          <*> stringLiteral
          <*> pure []  -- configurations
          <*> (True <$ keyword "deferred")
          <*> (Just <$ keyword "as" <*> simpleIdentifier))
        <*> many combinator
        <*  delimiter ";",
   uriBasedDirective=
        NamespaceDirective <$> namespaceDirective
        <|> partDirective,
   -- | A part directive.
   partDirective=
        PartDirective Nothing
        <$> metadata
        <*  keyword "part"
        <*> stringLiteral
        <*  delimiter ";",
   directive=
        UriBasedDirective <$> uriBasedDirective
        <|> libraryDirective
        <|> partOfDirective,
   -- | A part-of directive.
   partOfDirective=
        PartOfDirective Nothing
        <$> metadata
        <*  keyword "part"
        <*  keyword "of"
        <*> libraryIdentifier
        <*  delimiter ";",
   -- | A library directive.
   libraryDirective=
        LibraryDirective Nothing
        <$> metadata
        <*  keyword "library"
        <*> libraryIdentifier
        <*  delimiter ";",
   -- list of annotations.
   constructorInitializer=
        superInvocation
        <|> fieldInitializer
        <|> redirectingConstructorInvocation,
   -- | The invocation of a constructor in the same class from within a constructor's
   --   initialization list.
   redirectingConstructorInvocation=
        RedirectingConstructorInvocation
        <$ keyword "this"
        <*> optional (delimiter "." *> simpleIdentifier)
        <*> argumentList,
   -- | The initialization of a field within a constructor's initialization list.
   fieldInitializer=
        ConstructorFieldInitializer
        <$> flag (keyword "this" <* delimiter ".")
        <*> simpleIdentifier
        <*  delimiter "="
        <*> expression,
   -- | The invocation of a superclass' constructor from within a constructor's
   --   initialization list.
   superInvocation=
        SuperConstructorInvocation
        <$  keyword "super"
        <*> optional (delimiter "." *> simpleIdentifier)
        <*> argumentList,
   combinator=
           hideCombinator
       <|> showCombinator,
   -- | A combinator that restricts the names being imported to those in a given list.
   showCombinator=
        ShowCombinator
        <$ keyword "show"
        <*> sepBy1 simpleIdentifier (delimiter ","),
   -- | A combinator that restricts the names being imported to those that are not in
   -- a given list.
   hideCombinator=
        HideCombinator
        <$ keyword "hide"
        <*> sepBy1 simpleIdentifier (delimiter ","),
--   configuration=
--        uncurry Configuration
--        <$  keyword "if"
--        <*  delimiter "("
--        <*> test
--        <*  delimiter ")"
--        <*> stringLiteral,
--   test= (,) <$> dottedName <*> optional (delimiter "==" *> stringLiteral),
--   dottedName=
--        DottedName <$> sepBy1 simpleIdentifier (delimiter "."),
   extendsClause=
        ExtendsClause
        <$  keyword "extends"
        <*> typeName,
   mapLiteralEntry=
        MapLiteralEntry
        <$> expression
        <*  delimiter ":"
        <*> expression,
   -- A script tag that can optionally occur at the beginning of a compilation unit.
   scriptTag=
        ScriptTag
        <$  string "#!"
        <*> takeCharsWhile (/= '\n')
        <*  char '\n',
   -- | The "native" clause in an class declaration.
   nativeClause=
        NativeClause
        <$ keyword "native"
        <*> stringLiteral,
   normalFormalParameter=
        functionTypedFormalParameter
        <|> fieldFormalParameter
        <|> simpleFormalParameter,
   -- | A function-typed formal parameter.
   functionTypedFormalParameter=
        FunctionTypedFormalParameter Nothing []
        <$> optional typeName
        <*> simpleIdentifier
        <*> optional typeParameterList
        <*> formalParameterList,
   -- | A field formal parameter.
   fieldFormalParameter=
        FieldFormalParameter Nothing []
        <$> optional (FCVTFinal <$ keyword "final" <*> optional typeName
                      <|> FCVTConst <$ keyword "const" <*> optional typeName
                      <|> FCVTVar <$ keyword "var"
                      <|> FCVTType <$> typeName)
        <*> (True <$ keyword "this")
        <*  delimiter "."
        <*> simpleIdentifier,
   -- | A simple formal parameter.
   simpleFormalParameter=
        SimpleFormalParameter Nothing []
        <$> optional(FVTFinal <$ keyword "final" <*> typeName
                     <|> FVTVar <$ keyword "var"
                     <|> FVTType <$> typeName)
        <*> simpleIdentifier,
   formalParameter=
        NormalFormalParameter' <$> normalFormalParameter
        <|> defaultFormalParameter,
    -- A formal parameter with a default value. There are two kinds of parameters
    -- that are both represented by this class: named formal parameters and
    -- positional formal parameters.
   defaultFormalParameter=
        uncurry . DefaultFormalParameter
        <$> normalFormalParameter
        <*> (delimiter "=" *> ((,) Positional . Just <$> expression)
             <|> delimiter ":" *> ((,) Named . Just <$> expression)
             <|> pure (Required, Nothing)),
    -- function type alias.
    -- While the grammar requires all formal parameters to follow all of
    -- the normal formal parameters and at most one grouping of formal
    -- parameters, this class does not enforce those constraints. All parameters are
    -- flattened into a single list, which can have any or all kinds of parameters
    -- (normal, named, and positional) in any order.
   formalParameterList=
        FormalParameterList
        <$> parens (pure []
                    <|> normalFormalParameters <> moptional (delimiter "," *> optionalFormalParameters)
                    <|> optionalFormalParameters),
   normalFormalParameters=
        sepBy1 (NormalFormalParameter' <$> normalFormalParameter) (delimiter ","),
   optionalFormalParameters=
        optionalPositionalFormalParameters
        <|> namedFormalParameters,
   optionalPositionalFormalParameters=
        brackets (sepBy1 defaultFormalParameter (delimiter ",")),
   namedFormalParameters=
        braces (sepBy1 defaultFormalParameter (delimiter ",")),
   typeParameter=
        TypeParameter Nothing []
        <$> simpleIdentifier
        <*> optional (keyword "extends" *> typeName),
   typeParameterList=
        TypeParameterList
        <$  delimiter "<"
        <*> sepBy1 typeParameter (delimiter ",")
        <*  delimiter ">",
    -- function, method, or constructor.
   argumentList= parens (arguments <|> pure (ArgumentList [])),
   arguments=
        ArgumentList
        <$> (sepBy1 namedExpression (delimiter ",")
             <|> sepBy1 expression (delimiter ",") <> many (delimiter "," *> namedExpression)),
   commentReference=
        CommentReference
        <$ delimiter "["
        <*> flag (keyword "new")
        <*> identifier
        <*  delimiter "]",
   comment=
        endOfLineComment
        <|> blockComment
        <|> documentationComment,
   documentationComment=
        string "/--  *"
        *> concatMany (notFollowedBy (string "*/") *> anyToken <> takeCharsWhile (\c-> c /= '*' && c /= '[')
                       <|> show <$> commentReference)
        <* string "*/"
        <|> concatSome (string "///" *> takeCharsWhile (/= '\n') <* char '\n'),
   metadata=
        many annotation,
   annotation=
        Annotation
        <$  delimiter "@"
        <*> identifier
        <*> optional (delimiter "." *> simpleIdentifier)
        <*> optional argumentList,
   label=
        Label <$> simpleIdentifier <* delimiter ":",
   typeName=
        TypeName <$> (identifier <|> SimpleIdentifier' . SimpleIdentifier <$> lexicalToken (string "void"))
                 <*> optional typeArguments,
   typeArguments=
        TypeArgumentList
        <$  delimiter "<"
        <*> sepBy1 typeName (delimiter ",")
        <*  delimiter ">",
   switchMember=
        switchCase
        <|> switchDefault,
   -- | The default case in a switch statement.
   switchDefault=
        SwitchDefault
        <$> many (Label <$> simpleIdentifier)
        <*  keyword "default"
        <*  delimiter ":"
        <*> many statement,
   -- | A case in a switch statement.
   switchCase=
        SwitchCase
        <$> many label
        <*  keyword "case"
        <*> expression
        <*  delimiter ":"
        <*> many statement,
   onPart=
        uncurry CatchClause <$> catchPart <*> block
        <|> OnClause <$ keyword "on"
            <*> typeName
            <*> optional catchPart
            <*> block,
   catchPart=
        (,) <$ keyword "catch"
        <*  delimiter "("
        <*> simpleIdentifier
        <*> optional (delimiter "," *> simpleIdentifier)
        <*  delimiter ")",
   block= Block <$> braces (many statement),
   simpleIdentifier=
        SimpleIdentifier <$> Lexical.identifier,
   libraryIdentifier=
        LibraryIdentifier <$> sepBy1 simpleIdentifier (delimiter "."),
   identifier=
        SimpleIdentifier' <$> simpleIdentifier
        <|> prefixedIdentifier,
   -- | An identifier that is prefixed or an access to an object property where the
   -- target of the property access is a simple identifier.
   prefixedIdentifier=
        PrefixedIdentifier
        <$> simpleIdentifier
        <*  delimiter "."
        <*> simpleIdentifier,
   functionBody=
        blockFunctionBody
        <|> expressionFunctionBody,
   -- | A function body that consists of a block of statements.
   blockFunctionBody=
        BlockFunctionBody
        <$> (Async <$ keyword "async"
             <|> AsyncStar <$ keyword "async" <* delimiter "*"
             <|> SyncStar <$ keyword "sync" <* delimiter "*"
             <|> pure Sync)
        <*> block,
   -- | A function body consisting of a single expression.
   expressionFunctionBody=
        ExpressionFunctionBody
        <$> flag (keyword "async")
        <*  delimiter "=>"
        <*> expression
        <*  delimiter ";",
   -- | A function body that consists of a native keyword followed by a string
   -- literal.
   nativeFunctionBody=
        NativeFunctionBody
        <$ keyword "native"
        <*> (SingleStringLiteral' . SimpleStringLiteral <$> simpleStringLiteral)
        <* delimiter ";",
   functionExpression=
        FunctionExpression
        <$> optional typeParameterList
        <*> formalParameterList
        <*> functionExpressionBody,
   -- functionExpressionInvocation or a methodInvocation.,
   -- | The invocation of a function resulting from evaluating an expression.
   -- Invocations of methods and other forms of functions are represented by
   -- methodInvocation nodes. Invocations of getters and setters are represented
   -- by either prefixedIdentifier or propertyAccess nodes.
   functionExpressionInvocation=
        FunctionExpressionInvocation
        <$> expression
        <*> optional typeArguments
        <*> argumentList,
   -- | The invocation of either a function or a method. Invocations of functions
   -- resulting from evaluating an expression are represented by
   --  functionExpressionInvocation nodes. Invocations of getters and setters are
   --  represented by either prefixedIdentifier or propertyAccess nodes.
   methodInvocation=
        MethodInvocation
        <$> optional (expression <* delimiter ".")
        <*> simpleIdentifier
        <*> optional typeArguments
        <*> argumentList,

   expression=
           FunctionExpression' <$> functionExpression
       <|> throwExpression
       <|> AssignmentExpression <$> assignableExpression <*> assignmentOperator <*> expression
       <|> CascadeExpression <$> conditionalExpression <*> many cascadeSection
       <?> "an expression",
   expressionWithoutCascade=
           FunctionExpression' <$> functionExpressionWithoutCascade
       <|> throwExpressionWithoutCascade
       <|> AssignmentExpression <$> assignableExpression <*> assignmentOperator <*> expressionWithoutCascade
       <|> conditionalExpression,
   primary=
           thisExpression
       <|> SuperExpression <$ keyword "super" <**> (appEndo . getDual <$> unconditionalAssignableSelector)
       <|> constObjectExpression
       <|> newExpression
       <|> FunctionExpression' <$> functionPrimary
       <|> ParenthesizedExpression <$> parens expression
       <|> Literal' <$> literal
--       avoid ambiguity between a qualified identifier and field selector
--       <|> Identifier' <$> identifier,
       <|> Identifier' . SimpleIdentifier' <$> simpleIdentifier,

   throwExpression= ThrowExpression <$ keyword "throw" <*> expression,
   throwExpressionWithoutCascade= ThrowExpression <$ keyword "throw" <*> expressionWithoutCascade,

   functionExpressionBody=
        ExpressionFunctionBody
        <$> flag (keyword "async")
        <*  delimiter "=>"
        <*> expression,
   functionExpressionWithoutCascade=
        FunctionExpression
        <$> optional typeParameterList
        <*> formalParameterList
        <*> functionExpressionWithoutCascadeBody,
   functionExpressionWithoutCascadeBody=
        ExpressionFunctionBody
        <$> flag (keyword "async")
        <*  delimiter "=>"
        <*> expressionWithoutCascade,
   functionPrimary= 
        FunctionExpression
        <$> optional typeParameterList
        <*> formalParameterList
        <*> blockFunctionBody,

   --functionPrimaryBody= undefined, -- :    { startNonAsyncFunction(); } block { endFunction(); }
   --    |    (ASYNC | ASYNC '*' | SYNC '*')
   --         { startAsyncFunction(); } block { endFunction(); }
   --functionPrimaryBodyPrefix= undefined, -- : (ASYNC | ASYNC '*' | SYNC '*')? LBRACE

   thisExpression= ThisExpression <$ keyword "this",

   newExpression= 
      InstanceCreationExpression NCNew 
      <$  keyword "new"
      <*> constructorDesignation
      <*> argumentList,
   constObjectExpression=
      InstanceCreationExpression NCConst 
      <$  keyword "const" 
      <*> constructorDesignation
      <*> argumentList,

   --arguments= parens (argumentList <* optional (keyword ",")),

   --argumentList= moptional expressionList <> many (keyword "," *> namedArgument),

   --namedArgument= undefined, -- :    label expression

   cascadeSection=
       delimiter ".." *> cascadeSelector <**> (appEndo . getDual <$> concatMany argumentPart)
            <**> (appEndo . getDual <$> concatMany (assignableSelector <> concatMany argumentPart))
            <**> (flip AssignmentExpression <$> assignmentOperator <*> expressionWithoutCascade <|> pure id),
   cascadeSelector=
           IndexExpressionForCascade <$> brackets expression
       <|> PropertyAccessForCascade <$> simpleIdentifier,

   assignmentOperator=
       delimiter "=" <|> compoundAssignmentOperator,
   compoundAssignmentOperator=
           delimiter "*="
       <|> delimiter "/="
       <|> delimiter "~/="
       <|> delimiter "%="
       <|> delimiter "+="
       <|> delimiter "-="
       <|> delimiter "<<="
       <|> delimiter ">" <> delimiter ">" <> delimiter "="
       <|> delimiter "&="
       <|> delimiter "^="
       <|> delimiter "|="
       <|> delimiter "??=",

   conditionalExpression=
      ConditionalExpression 
          <$> ifNullExpression
          <*  delimiter "?" 
          <*> expressionWithoutCascade 
          <*  delimiter ":" 
          <*> expressionWithoutCascade
      <|> ifNullExpression,
   ifNullExpression=
      BinaryExpression <$> ifNullExpression <*> delimiter "??" <*> logicalOrExpression
      <|> logicalOrExpression,
   logicalOrExpression=
      BinaryExpression <$> logicalAndExpression <*> delimiter "||" <*> logicalOrExpression
      <|> logicalAndExpression,
   logicalAndExpression=
      BinaryExpression <$> equalityExpression <*> delimiter "&&" <*> logicalAndExpression
      <|> equalityExpression,
   equalityExpression=
      BinaryExpression 
          <$> (relationalExpression <|> SuperExpression <$ keyword "super")
          <*> equalityOperator 
          <*> relationalExpression
      <|> relationalExpression,
   equalityOperator=
          delimiter "=="
      <|> delimiter "!=",
   relationalExpression=
      BinaryExpression 
           <$> (bitwiseOrExpression <|> SuperExpression <$ keyword "super")
           <*> relationalOperator 
           <*> bitwiseOrExpression
      <|> IsExpression <$> bitwiseOrExpression
                       <*  keyword "is" <*> flag (delimiter "!") <*> typeName
      <|> AsExpression <$> bitwiseOrExpression
                       <*  keyword "as" <*> typeName
      <|> bitwiseOrExpression,
   relationalOperator=
           delimiter ">" <> delimiter "="
       <|> delimiter ">"
       <|> delimiter "<="
       <|> delimiter "<",

   bitwiseOrExpression=
      BinaryExpression
          <$> (bitwiseOrExpression <|> SuperExpression <$ keyword "super")
          <*> delimiter "|"
          <*> bitwiseXorExpression
       <|> bitwiseXorExpression,
   bitwiseXorExpression=
      BinaryExpression
          <$> (bitwiseXorExpression <|> SuperExpression <$ keyword "super")
          <*> delimiter "^"
          <*> bitwiseAndExpression
       <|> bitwiseAndExpression,
   bitwiseAndExpression=
      BinaryExpression
          <$> (bitwiseAndExpression <|> SuperExpression <$ keyword "super")
          <*> delimiter "&"
          <*> shiftExpression
       <|> shiftExpression,
   shiftExpression=
      BinaryExpression
          <$> (shiftExpression <|> SuperExpression <$ keyword "super")
          <*> shiftOperator 
          <*> additiveExpression
       <|> additiveExpression,
   shiftOperator=
           delimiter "<<"
       <|> delimiter ">" <> delimiter ">",
   additiveExpression=
      BinaryExpression
          <$> (additiveExpression <|> SuperExpression <$ keyword "super")
          <*> additiveOperator
          <*> multiplicativeExpression
      <|> multiplicativeExpression,
   additiveOperator=
           delimiter "+"
       <|> delimiter "-",
   multiplicativeExpression=
      BinaryExpression
          <$> (multiplicativeExpression <|> SuperExpression <$ keyword "super")
          <*> multiplicativeOperator
          <*> unaryExpression
      <|> unaryExpression,
   multiplicativeOperator=
           delimiter "*"
       <|> delimiter "/"
       <|> delimiter "%"
       <|> delimiter "~/",
   unaryExpression=
           PrefixExpression <$> prefixOperator <*> (notFollowedBy (keyword "super") *> unaryExpression)
       <|> awaitExpression
       <|> postfixExpression
       <|> PrefixExpression <$> (minusOperator <|> tildeOperator) <*> (SuperExpression <$ keyword "super")
       <|> PrefixExpression <$> incrementOperator <*> assignableExpression,
   prefixOperator=
           minusOperator
       <|> negationOperator
       <|> tildeOperator,
   minusOperator= delimiter "-",
   negationOperator= delimiter "!",
   tildeOperator= delimiter "~",

   awaitExpression= AwaitExpression <$ keyword "await" <*> unaryExpression,

   -- The `(selector)` predicate ensures that the parser commits to the longest
   -- possible chain of selectors, e.g., `a<b,c>(d)` as a call rather than as a
   -- sequence of two relational expressions.

   postfixExpression=
           PostfixExpression <$> assignableExpression <*> postfixOperator
       <|> primary <**> (appEndo . getDual <$> concatMany selector),
   postfixOperator= incrementOperator,
   selector=
           assignableSelector
       <|> argumentPart,
   argumentPart=
       Dual . Endo . (InvocationExpression .) 
       <$> (flip <$> (flip FunctionExpressionInvocation <$> optional typeArguments) <*> argumentList),
   incrementOperator=
           delimiter "++"
       <|> delimiter "--",

   operator= tildeOperator <|> binaryOperator <|> delimiter "[" <> delimiter "]" <> moptional (delimiter "="),
   binaryOperator= 
       multiplicativeOperator
       <|>  additiveOperator
       <|>  shiftOperator
       <|>  relationalOperator
       <|>  delimiter "=="
       <|>  bitwiseOperator,
   bitwiseOperator=
      delimiter "|"
      <|> delimiter "^"
      <|> delimiter "&",
   
   -- The `(assignableSelectorPart)` predicate ensures that the parser
   -- commits to the longest possible chain, e.g., `a<b,c>(d).e` as one rather
   -- than two expressions. The first `identifier` alternative handles all
   -- the simple cases; the final `identifier` alternative at the end catches
   -- the case where we have `identifier '<'` and the '<' is used as a
   -- relationalOperator, not the beginning of typeArguments.

   assignableExpression=
           SuperExpression <$ keyword "super" <**> (appEndo . getDual <$> unconditionalAssignableSelector)
   --    <|> (typeName typeArguments '.' identifier '(') =>
   --         constructorInvocation
   --         ((assignableSelectorPart) => assignableSelectorPart)+
       <|> primary <**> (appEndo . getDual <$> concatSome assignableSelectorPart)
--       avoid ambiguity between a qualified identifier and field selector
--       <|> Identifier' <$> identifier,
       <|> Identifier' . SimpleIdentifier' <$> simpleIdentifier,
   assignableSelectorPart=
       concatMany argumentPart <> assignableSelector,
   unconditionalAssignableSelector=
           Dual . Endo . flip IndexExpressionForTarget <$> brackets expression
       <|> Dual . Endo . flip PropertyAccess <$> (delimiter "." *> simpleIdentifier),
   assignableSelector=
           unconditionalAssignableSelector
       <|> Dual . Endo . flip ConditionalPropertyAccess <$> (delimiter "?." *> simpleIdentifier),

   namedExpression=
      NamedExpression <$> label <*> expression,
   
   {-
   expression=
           assignmentExpression
       <|> conditionalExpression (many cascadeSection)
       <|> throwExpression,
   -- | A prefix unary expression.
   prefixExpression=
        PrefixExpression <$> prefixOperator <*> expression,
   -- | A postfix unary expression.
   postfixExpression= PostfixExpression <$> assignableExpression <*> postfixOperator,
   -- | A binary (infix) expression.
   binaryExpression=
        expression token expression,
   -- | An assignment expression.
   assignmentExpression=
        AssignmentExpression <$> asignableExpression <*> assignmentOperator <*> expression,
   -- | An instance creation expression.
   newExpression=
        InstanceCreationExpression
        <$> (NCNew <$ keyword "new" <|> NCConst <$ keyword "const")
        <*> (ConstructorName <$> typeName <*> optional (delimiter "." *> simpleIdentifier))
        <*> argumentList,
   -- | An as expression.
   asExpression=
        AsExpression <$> bitwiseOrExpression <* keyword "as" <*> typeName,
   -- | An is expression.
   isExpression=
        IsExpression
        <$> bitwiseOrExpression
        <*  keyword "is"
        <*> flag (delimiter "!")
        <*> typeName,
   -- | A throw expression.
   throwExpression=
        ThrowExpression <$ keyword "throw" <*> expression,
   -- | A rethrow expression.
   rethrowExpression=
        RethrowExpression <$ keyword "rethrow",
   -- | A this expression.
   thisExpression=
        ThisExpression <$ keyword "this",
   -- | A super expression.
   superExpression=
        SuperExpression <$ keyword "super",
   -- | A parenthesized expression.
   parenthesizedExpression=
        delimiter "(" *> expression <* delimiter ")",
   -- | The access of a property of an object.
   -- Note, however, that accesses to properties of objects can also be represented
   -- as prefixedIdentifier nodes in cases where the target is also a simple
   -- identifier.
   propertyAccess=
        expression delimiter "." *> simpleIdentifier,
   -- | An expression that has a name associated with it. They are used in method
   -- invocations when there are named parameters.
   -- | A conditional expression.
       conditionalExpression=
           expression delimiter "?" *> expression delimiter ":" *> expression,
   -- | A sequence of cascaded expressions: expressions that share a common target.
   -- There are three kinds of expressions that can be used in a cascade
   -- expression: indexExpression, methodInvocation and propertyAccess.
       cascadeExpression=
           expression (many cascadeSection),
       cascadeSection=
           '..'  (cascadeSelector (many arguments)) (assignableSelector (many arguments))*
           (assignmentOperator expressionWithoutCascade)?,
       cascadeSelector=
           '[ ' expression '] '
       <|> identifier,
   -- | An index expression.
       indexExpression=
           expression delimiter "[" *> expression <* delimiter "]",
-}

   statement=
        Block' <$> block
        <|> variableDeclarationStatement
        <|> forStatement
        <|> forEachStatement
        <|> whileStatement
        <|> doStatement
        <|> switchStatement
        <|> ifStatement
        <|> tryStatement
        <|> breakStatement
        <|> continueStatement
        <|> returnStatement
        <|> expressionStatement
        <|> FunctionDeclarationStatement <$> functionDeclaration,
   -- | A list of variables that are being declared in a context where a statement is
   -- required.
   variableDeclarationStatement=
        VariableDeclarationStatement <$> variableDeclarationList <* delimiter ";",
   -- | A for statement.
   forStatement= keyword "for"
        *>  parens forLoopParts
        <*> statement,
   forLoopParts=
        uncurry ForStatement
        <$> forInitializerStatement
        <*  delimiter ";"
        <*> optional expression
        <*  delimiter ";"
        <*> sepBy expression (delimiter ","),
   forInitializerStatement=
        ($ Nothing) . (,) . Just <$> variableDeclarationList
        <|> (,) Nothing <$> optional expression,
--        <|> optional (VariableDeclarationList Nothing [] FCVTVar . (:[]) . VariableDeclaration undefined . Just <$> expression),
   -- | A for-each statement.
   forEachStatement=
        (ForEachStatementWithDeclaration
         <$> flag (keyword "await")
         <*  keyword "for"
         <*  delimiter "("
         <*> declaredIdentifier
         <|>
         ForEachStatementWithReference
         <$> flag (keyword "await")
         <*  keyword "for"
         <*  delimiter "("
         <*> simpleIdentifier)
        <*  keyword "in"
        <*> expression
        <*  delimiter ")"
        <*> statement,
   -- | A while statement.
   whileStatement=
        WhileStatement
        <$  keyword "while"
        <*> parens expression
        <*> statement,
   -- | A do statement.
   doStatement=
        DoStatement
        <$  keyword "do"
        <*> statement
        <*  keyword "while"
        <*> parens expression
        <*  delimiter ";",
   -- | A switch statement.
   switchStatement=
        SwitchStatement
        <$  keyword "switch"
        <*> parens expression
        <*> braces (many switchCase <> upto 1 switchDefault),
   -- | An if statement.
   ifStatement=
        IfStatement
        <$  keyword "if"
        <*> parens expression
        <*> statement
        <*> optional (keyword "else" *> statement),
   -- | A try statement.
   tryStatement=
        uncurry . TryStatement
        <$  keyword "try"
        <*> block
        <*> ((,) <$> some onPart <*> optional finallyClause
             <|> (,) [] . Just <$> finallyClause),
   finallyClause=
        keyword "finally" *> block,
   -- | A break statement.
   breakStatement=
        BreakStatement
        <$  keyword "break"
        <*> optional simpleIdentifier
        <*  delimiter ";",
   -- | A continue statement.
   continueStatement=
        ContinueStatement
        <$  keyword "continue"
        <*> optional simpleIdentifier
        <*  delimiter ";",
   -- | A return statement.
   returnStatement=
        ReturnStatement
        <$  keyword "return"
        <*> optional expression
        <*  delimiter ";",
   -- | An expression used as a statement.
   expressionStatement=
        ExpressionStatement <$> expression <*  delimiter ";"
        <|> emptyStatement,
   -- | A functionDeclaration used as a statement.,
   -- | An assert statement.
   assertStatement=
        AssertStatement
        <$  keyword "assert"
        <*   delimiter "("
        <*> expression
        <*> optional (delimiter "," *> expression)
        <*  delimiter ")"
        <*  delimiter ";",
   -- | A yield statement.
   yieldStatement=
        YieldStatement
        <$  keyword "yield"
        <*> flag (delimiter "*")
        <*> expression
        <*  delimiter ";",
   -- | An empty statement.
   emptyStatement=
        EmptyStatement <$ delimiter ";",
   -- | A statement that has a label associated with them.
   labeledStatement=
        LabeledStatement <$> some label <*> statement,
   -- While the grammar restricts the order of the directives and declarations
   -- within a compilation unit, this class does not enforce those restrictions.
   -- In particular, the children of a compilation unit will be visited in lexical
   -- order even if lexical order does not conform to the restrictions of the
   -- grammar.
   compilationUnit=
        Grampa.lexicalWhiteSpace *> directives <*> declarations,
   directives=
        CompilationUnit
        <$> optional scriptTag
        <*> (((:[]) <$> libraryDirective <|> pure [])
             <> (map UriBasedDirective <$> (many (NamespaceDirective <$> namespaceDirective)
                                            <> many partDirective))
             <|> (:[]) <$> partOfDirective),
   declarations=
        many compilationUnitMember}
