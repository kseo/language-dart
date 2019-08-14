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
import Text.Grampa.ContextFree.LeftRecursive (Parser, longest, peg)
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
   mapLiteral=
        MapLiteral
        <$> flag (keyword "const")
        <*> optional typeArguments
        <*> braces (sepEndBy mapLiteralEntry (delimiter ",")),
   listLiteral=
        ListLiteral
        <$> flag (keyword "const")
        <*> optional (delimiter "<" *> typeArguments <* delimiter ">")
        <*> brackets (sepEndBy expression (delimiter ",")),
   interpolationExpression=
           delimiter "$" *> (InterpolationExpression . Identifier' . SimpleIdentifier' <$> simpleIdentifier)
       <|> delimiter "$" *> braces (InterpolationExpression <$> expression),
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
   nullLiteral=
        NullLiteral <$ keyword "null",
   symbolLiteral=
        string "#" *> ((:[]) <$> operator 
                       <|> peg (longest $ sepBy Lexical.identifier (delimiter "."))),
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
        TypeAlias <$> typeAliasBody,
   typeAliasBody=
           classTypeAlias
       <|> functionTypeAlias,
   classTypeAlias=
        (\abstract name typeArgs-> ClassTypeAlias Nothing [] name typeArgs abstract)
        <$> flag (keyword "abstract")
        <*  keyword "class"
        <*> simpleIdentifier
        <*> optional typeParameterList 
        <*  delimiter "="
        <*> typeName
        <*> withClause
        <*> optional implementsClause
         <* delimiter ";",
   functionTypeAlias=
        FunctionTypeAlias Nothing []
        <$  keyword "typedef"
        <*> optional typeName
        <*> simpleIdentifier
        <*> optional typeParameterList
        <*> formalParameterList
         <* delimiter ";",
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
   enumType=
        EnumDeclaration Nothing
        <$> metadata
        <*  keyword "enum"
        <*> simpleIdentifier
        <*> braces (sepEndBy (EnumConstantDeclaration Nothing [] <$> simpleIdentifier) (delimiter ",")),
   compilationUnitMember=
           NamedCompilationUnitMember 
           <$> (classDeclaration
                <|> typeAlias
                <|> FunctionDeclaration' <$> functionDeclaration
                -- <|> methodDeclaration
                <|> enumType)
       <|> topLevelVariableDeclaration,
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
   methodDeclaration=
            methodSignature <*> functionBody
        <|> getterSetterSignature <*> (EmptyFunctionBody <$ delimiter ";"),
   methodSignature=
        uncurry <$> (MethodDeclaration Nothing []
                     <$> flag (keyword "external")
                     <*> optional (Abstract <$ keyword "abstract" <|> Static <$ keyword "static")
                     <*> optional typeName
                     <*> (Get <$ keyword "get" <|> Set <$ keyword "set" <|> pure Empty))
                <*> methodName
                <*> optional typeParameterList
                <*> optional formalParameterList,
   getterSetterSignature=
        uncurry <$> (MethodDeclaration Nothing []
                     <$> flag (keyword "external")
                     <*> optional (Abstract <$ keyword "abstract" <|> Static <$ keyword "static")
                     <*> optional typeName
                     <*> (Get <$ keyword "get" <|> Set <$ keyword "set"))
                <*> methodName
                <*> optional typeParameterList
                <*> optional formalParameterList,
   methodName=
        (,) False <$> simpleIdentifier
        <|> (,) True <$ keyword "operator" <*> simpleIdentifier,
   fieldDeclaration=
        FieldDeclaration Nothing []
        <$> flag (keyword "static")
        <*> variableDeclarationList
        <*  delimiter ";",
   namespaceDirective=
        exportDirective
        <|> importDirective,
   exportDirective=
        ExportDirective Nothing
        <$> metadata
        <*  keyword "export"
        <*> stringLiteral
        <*> pure []  -- configurations
        <*> many combinator
        <*  delimiter ";",
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
   partOfDirective=
        PartOfDirective Nothing
        <$> metadata
        <*  keyword "part"
        <*  keyword "of"
        <*> libraryIdentifier
        <*  delimiter ";",
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
   redirectingConstructorInvocation=
        RedirectingConstructorInvocation
        <$ keyword "this"
        <*> optional (delimiter "." *> simpleIdentifier)
        <*> argumentList,
   fieldInitializer=
        ConstructorFieldInitializer
        <$> flag (keyword "this" <* delimiter ".")
        <*> simpleIdentifier
        <*  delimiter "="
        <*> expression,
   superInvocation=
        SuperConstructorInvocation
        <$  keyword "super"
        <*> optional (delimiter "." *> simpleIdentifier)
        <*> argumentList,
   combinator=
           hideCombinator
       <|> showCombinator,
   showCombinator=
        ShowCombinator
        <$ keyword "show"
        <*> sepBy1 simpleIdentifier (delimiter ","),
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
   nativeClause=
        NativeClause
        <$ keyword "native"
        <*> stringLiteral,
   normalFormalParameter=
        functionTypedFormalParameter
        <|> fieldFormalParameter
        <|> simpleFormalParameter,
   functionTypedFormalParameter=
        FunctionTypedFormalParameter Nothing []
        <$> optional typeName
        <*> simpleIdentifier
        <*> optional typeParameterList
        <*> formalParameterList,
   fieldFormalParameter=
        FieldFormalParameter Nothing []
        <$> optional (FCVTFinal <$ keyword "final" <*> optional typeName
                      <|> FCVTConst <$ keyword "const" <*> optional typeName
                      <|> FCVTVar <$ keyword "var"
                      <|> FCVTType <$> typeName)
        <*> (True <$ keyword "this")
        <*  delimiter "."
        <*> simpleIdentifier,
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
   switchDefault=
        SwitchDefault
        <$> many (Label <$> simpleIdentifier)
        <*  keyword "default"
        <*  delimiter ":"
        <*> many statement,
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
   prefixedIdentifier=
        PrefixedIdentifier
        <$> simpleIdentifier
        <*  delimiter "."
        <*> simpleIdentifier,
   functionBody=
        blockFunctionBody
        <|> expressionFunctionBody,
   blockFunctionBody=
        BlockFunctionBody
        <$> (Async <$ keyword "async"
             <|> AsyncStar <$ keyword "async" <* delimiter "*"
             <|> SyncStar <$ keyword "sync" <* delimiter "*"
             <|> pure Sync)
        <*> block,
   expressionFunctionBody=
        ExpressionFunctionBody
        <$> flag (keyword "async")
        <*  delimiter "=>"
        <*> expression
        <*  delimiter ";",
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
   functionExpressionInvocation=
        FunctionExpressionInvocation
        <$> expression
        <*> optional typeArguments
        <*> argumentList,
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
   prefixExpression=
        PrefixExpression <$> prefixOperator <*> expression,
   postfixExpression= PostfixExpression <$> assignableExpression <*> postfixOperator,
   binaryExpression=
        expression token expression,
   assignmentExpression=
        AssignmentExpression <$> asignableExpression <*> assignmentOperator <*> expression,
   newExpression=
        InstanceCreationExpression
        <$> (NCNew <$ keyword "new" <|> NCConst <$ keyword "const")
        <*> (ConstructorName <$> typeName <*> optional (delimiter "." *> simpleIdentifier))
        <*> argumentList,
   asExpression=
        AsExpression <$> bitwiseOrExpression <* keyword "as" <*> typeName,
   isExpression=
        IsExpression
        <$> bitwiseOrExpression
        <*  keyword "is"
        <*> flag (delimiter "!")
        <*> typeName,
   throwExpression=
        ThrowExpression <$ keyword "throw" <*> expression,
   rethrowExpression=
        RethrowExpression <$ keyword "rethrow",
   thisExpression=
        ThisExpression <$ keyword "this",
   superExpression=
        SuperExpression <$ keyword "super",
   parenthesizedExpression=
        delimiter "(" *> expression <* delimiter ")",
   propertyAccess=
        expression delimiter "." *> simpleIdentifier,
       conditionalExpression=
           expression delimiter "?" *> expression delimiter ":" *> expression,
       cascadeExpression=
           expression (many cascadeSection),
       cascadeSection=
           '..'  (cascadeSelector (many arguments)) (assignableSelector (many arguments))*
           (assignmentOperator expressionWithoutCascade)?,
       cascadeSelector=
           '[ ' expression '] '
       <|> identifier,
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
   variableDeclarationStatement=
        VariableDeclarationStatement <$> variableDeclarationList <* delimiter ";",
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
   whileStatement=
        WhileStatement
        <$  keyword "while"
        <*> parens expression
        <*> statement,
   doStatement=
        DoStatement
        <$  keyword "do"
        <*> statement
        <*  keyword "while"
        <*> parens expression
        <*  delimiter ";",
   switchStatement=
        SwitchStatement
        <$  keyword "switch"
        <*> parens expression
        <*> braces (many switchCase <> upto 1 switchDefault),
   ifStatement=
        IfStatement
        <$  keyword "if"
        <*> parens expression
        <*> statement
        <*> optional (keyword "else" *> statement),
   tryStatement=
        uncurry . TryStatement
        <$  keyword "try"
        <*> block
        <*> ((,) <$> some onPart <*> optional finallyClause
             <|> (,) [] . Just <$> finallyClause),
   finallyClause=
        keyword "finally" *> block,
   breakStatement=
        BreakStatement
        <$  keyword "break"
        <*> optional simpleIdentifier
        <*  delimiter ";",
   continueStatement=
        ContinueStatement
        <$  keyword "continue"
        <*> optional simpleIdentifier
        <*  delimiter ";",
   returnStatement=
        ReturnStatement
        <$  keyword "return"
        <*> optional expression
        <*  delimiter ";",
   expressionStatement=
        ExpressionStatement <$> expression <*  delimiter ";"
        <|> emptyStatement,
   assertStatement=
        AssertStatement
        <$  keyword "assert"
        <*   delimiter "("
        <*> expression
        <*> optional (delimiter "," *> expression)
        <*  delimiter ")"
        <*  delimiter ";",
   yieldStatement=
        YieldStatement
        <$  keyword "yield"
        <*> flag (delimiter "*")
        <*> expression
        <*  delimiter ";",
   emptyStatement=
        EmptyStatement <$ delimiter ";",
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
