{-# LANGUAGE RankNTypes, RecordWildCards, TemplateHaskell, TypeFamilies #-}

module Language.Dart.Grammar where

import Control.Applicative
import Control.Monad (guard)
import Data.Char (chr, isAlphaNum, isDigit, isHexDigit, isLetter, isSpace)
import Data.List (intersperse)
import Data.Monoid (Endo(..), (<>))
import Numeric (readHex)
import Text.Grampa hiding (Grammar)
import Text.Grampa.ContextFree.LeftRecursive (Parser)
import qualified Text.Grampa as Lexical (identifier)
import qualified Text.Grampa as Grampa
import Text.Parser.Combinators (count, sepBy, sepBy1, sepEndBy, try)
import Text.Parser.Token (braces, brackets, parens)
import Language.Dart.Syntax

import qualified Rank2
import qualified Rank2.TH

import Prelude hiding (exponent)

data Grammar p = Grammar {
   typedLiteral :: p TypedLiteral,
   mapLiteral :: p TypedLiteral,
   listLiteral :: p TypedLiteral,
   interpolationExpression :: p InterpolationElement,
   interpolationStringNoSingleQuote :: p InterpolationElement,
   interpolationStringNoDoubleQuote :: p InterpolationElement,
   characterEscape :: p Char,
   singleStringLiteral :: p SingleStringLiteral,
   simpleStringLiteral :: p String,
   rawStringLiteral :: p String,
   basicStringLiteral :: p String,
   multiLineStringLiteral :: p String,
   singleLineStringLiteral :: p String,
   stringInterpolation :: p [InterpolationElement],
   stringLiteral :: p StringLiteral,
   adjacentStrings :: p [StringLiteral],
   literal :: p Literal,
   booleanLiteral :: p Bool,
   doubleLiteral :: p Double,
   exponent :: p String,
   integerLiteral :: p Integer,
   decimalIntegerLiteral :: p Integer,
   hexadecimalIntegerLiteral :: p Integer,
   nullLiteral :: p Literal,
   symbolLiteral :: p [Token],
   variableDeclaration :: p VariableDeclaration,
   variableDeclarationList :: p VariableDeclarationList,
   finalConstVarOrType :: p FinalConstVarOrType,
   declaredIdentifier :: p DeclaredIdentifier,
   functionDeclaration :: p FunctionDeclaration,
   functionSignature :: p ((Maybe TypeName -> PropertyKeyword -> SimpleIdentifier -> FunctionExpression 
                            -> FunctionDeclaration)
                           -> FunctionBody -> FunctionDeclaration),
   withClause :: p WithClause,
   implementsClause :: p ImplementsClause,
   typeAlias :: p NamedCompilationUnitMember,
   typeAliasBody :: p TypeAlias,
   classTypeAlias :: p TypeAlias,
--   mixinApplication :: p MixinApplication,
   functionTypeAlias :: p TypeAlias,
--   functionPrefix :: p FunctionPrefix,
   classDeclaration :: p NamedCompilationUnitMember,
   classMember :: p ClassMember,
   enumType :: p NamedCompilationUnitMember,
   compilationUnitMember :: p CompilationUnitMember,
   topLevelVariableDeclaration :: p CompilationUnitMember,
   constructorDeclaration :: p ClassMember,
   constructorSignature :: p (Maybe FunctionBody -> ClassMember),
   constructorName :: p (Identifier, Maybe SimpleIdentifier),
   constructorDesignation :: p ConstructorName,
   factoryName :: p (Identifier, Maybe SimpleIdentifier),
   initializerList :: p [ConstructorInitializer],
   methodDeclaration :: p ClassMember,
   methodSignature :: p (FunctionBody -> ClassMember),
   methodName :: p (Bool, SimpleIdentifier),
   fieldDeclaration :: p ClassMember,
   exportDirective :: p NamespaceDirective,
   importDirective :: p NamespaceDirective,
   uriBasedDirective :: p UriBasedDirective,
   partDirective :: p UriBasedDirective,
   directive :: p Directive,
   partOfDirective :: p Directive,
   libraryDirective :: p Directive,
   constructorInitializer :: p ConstructorInitializer,
   redirectingConstructorInvocation :: p ConstructorInitializer,
   fieldInitializer :: p ConstructorInitializer,
   superInvocation :: p ConstructorInitializer,
   combinator :: p Combinator,
   showCombinator :: p Combinator,
   hideCombinator :: p Combinator,
--   dottedName :: p DottedName,
--   configuration :: p Configuration,
--   test :: p (DottedName, StringLiteral),
   extendsClause :: p ExtendsClause,
   mapLiteralEntry :: p MapLiteralEntry,
   scriptTag :: p ScriptTag,
   nativeClause :: p AstNode,
   normalFormalParameter :: p NormalFormalParameter,
   functionTypedFormalParameter :: p NormalFormalParameter,
   fieldFormalParameter :: p NormalFormalParameter,
   simpleFormalParameter :: p NormalFormalParameter,
   formalParameter :: p FormalParameter,
   defaultFormalParameter :: p FormalParameter,
--   defaultNamedParameter :: p FormalParameter,
   formalParameterList :: p FormalParameterList,
   normalFormalParameters :: p [FormalParameter],
   optionalFormalParameters :: p [FormalParameter],
   optionalPositionalFormalParameters :: p [FormalParameter],
   namedFormalParameters :: p [FormalParameter],
   typeParameter :: p TypeParameter,
   typeParameterList :: p TypeParameterList,
   argumentList :: p ArgumentList,
   arguments :: p ArgumentList,
   commentReference :: p CommentReference,
   comment :: p String,
   endOfLineComment :: p String,
   blockComment :: p String,
   documentationComment :: p String,
   metadata :: p [Annotation],
   annotation :: p Annotation,
   label :: p Label,
   typeName :: p TypeName,
   typeArguments :: p TypeArgumentList,
   switchMember :: p SwitchMember,
   switchDefault :: p SwitchMember,
   switchCase :: p SwitchMember,
   onPart :: p CatchClause,
   catchPart :: p (SimpleIdentifier, Maybe SimpleIdentifier),
   block :: p Block,
   simpleIdentifier :: p SimpleIdentifier,
   libraryIdentifier :: p LibraryIdentifier,
   identifier :: p Identifier,
   prefixedIdentifier :: p Identifier,
   functionBody :: p FunctionBody,
   blockFunctionBody :: p FunctionBody,
   emptyFunctionBody :: p FunctionBody,
   expressionFunctionBody :: p FunctionBody,
   nativeFunctionBody :: p FunctionBody,
   functionExpression :: p FunctionExpression,
   functionExpressionWithoutCascade :: p FunctionExpression,
   functionExpressionWithoutCascadeBody :: p FunctionBody,
   functionPrimary :: p FunctionExpression,
--   constructorName :: p ConstructorName,
   functionExpressionInvocation :: p InvocationExpression,
   methodInvocation :: p InvocationExpression,
   expression :: p Expression,
   expressionWithoutCascade :: p Expression,
--   prefixExpression :: p Expression,
   postfixExpression :: p Expression,
--   binaryExpression :: p Expression,
   unaryExpression:: p Expression,
--   assignmentExpression :: p Expression,
   newExpression :: p Expression,
--   asExpression :: p Expression,
--   isExpression :: p Expression,
   throwExpression :: p Expression,
   throwExpressionWithoutCascade :: p Expression,
--   rethrowExpression :: p Expression,
   thisExpression :: p Expression,
--   superExpression :: p Expression,
--   parenthesizedExpression :: p Expression,
--   propertyAccess :: p Expression,
   namedExpression :: p Expression,
   constObjectExpression :: p Expression,
   conditionalExpression :: p Expression,
--   cascadeExpression :: p Expression,
   cascadeSection :: p Expression,
   cascadeSelector :: p Expression,
   selector :: p (Endo Expression),
   assignableExpression :: p Expression,
   assignableSelector :: p (Endo Expression),
   assignableSelectorPart :: p (Endo Expression),
   unconditionalAssignableSelector :: p (Endo Expression),
   argumentPart :: p (Endo Expression),
   ifNullExpression :: p Expression,
   logicalOrExpression :: p Expression,
   logicalAndExpression :: p Expression,
   equalityExpression :: p Expression,
   relationalExpression :: p Expression,
   additiveExpression :: p Expression,
   multiplicativeExpression :: p Expression,
   shiftExpression :: p Expression,
   bitwiseOrExpression :: p Expression,
   bitwiseXorExpression :: p Expression,
   bitwiseAndExpression :: p Expression,
   primary :: p Expression,
--   indexExpression :: p Expression,
   awaitExpression :: p Expression,
   statement :: p Statement,
   variableDeclarationStatement :: p Statement,
   forStatement :: p Statement,
   forLoopParts :: p (Statement -> Statement),
   forInitializerStatement :: p (Maybe VariableDeclarationList, Maybe Expression),
   forEachStatement :: p Statement,
   whileStatement :: p Statement,
   doStatement :: p Statement,
   switchStatement :: p Statement,
   ifStatement :: p Statement,
   tryStatement :: p Statement,
   finallyClause :: p Block,
   breakStatement :: p Statement,
   continueStatement :: p Statement,
   returnStatement :: p Statement,
   expressionStatement :: p Statement,
   assertStatement :: p Statement,
   yieldStatement :: p Statement,
   emptyStatement :: p Statement,
   labeledStatement :: p Statement,
   compilationUnit :: p CompilationUnit,
   directives :: p ([CompilationUnitMember] -> CompilationUnit),
   namespaceDirective :: p NamespaceDirective,
   declarations :: p [CompilationUnitMember],
   additiveOperator :: p String,
   assignmentOperator :: p String,
   compoundAssignmentOperator :: p String,
   equalityOperator :: p String,
   incrementOperator :: p String,
   minusOperator :: p String,
   multiplicativeOperator :: p String,
   negationOperator :: p String,
   postfixOperator :: p String,
   prefixOperator :: p String,
   relationalOperator :: p String,
   shiftOperator :: p String,
   tildeOperator :: p String,
   binaryOperator :: p String,
   bitwiseOperator :: p String,
   operator :: p String}

$(Rank2.TH.deriveAll ''Grammar)

instance Lexical Grammar where
   type LexicalConstraint p Grammar s = (s ~ String, p ~ Parser)
   lexicalComment = try (string "(*"
                         *> skipMany (lexicalComment
                                      <|> notFollowedBy (string "*)") <* anyToken <* takeCharsWhile isCommentChar)
                         <* string "*)")
      where isCommentChar c = c /= '*' && c /= '('
   lexicalWhiteSpace = takeCharsWhile isSpace *> skipMany (lexicalComment *> takeCharsWhile isSpace)
   isIdentifierStartChar c = isLetter c || c == '_' || c == '$'
   isIdentifierFollowChar c = isAlphaNum c || c == '_' || c == '$'
   identifierToken word = lexicalToken (do w <- word
                                           guard (w `notElem` reservedWords)
                                           return w)
                          
reservedWords :: [String]
reservedWords = undefined

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
        <*  delimiter "{"
        <*> sepEndBy mapLiteralEntry (delimiter ",")
        <*  delimiter "}",
   -- | A list literal.
   listLiteral=
        ListLiteral
        <$> flag (keyword "const")
        <*> optional (delimiter "<" *> typeArguments <* delimiter ">")
        <*  delimiter "["
        <*> sepEndBy expression (delimiter ",")
        <*  delimiter "]",
   -- | An expression embedded in a string interpolation.
   interpolationExpression=
           delimiter "$" *> (InterpolationExpression . Identifier' . SimpleIdentifier' <$> simpleIdentifier)
       <|> delimiter "$" *> delimiter "{" *> (InterpolationExpression <$> expression) <* delimiter "}",
   -- | A non-empty substring of an interpolated string.
   interpolationStringNoSingleQuote=
         InterpolationString <$> (takeCharsWhile (`notElem` "\\\'$\r\n")
                                  <|> (:[]) <$> characterEscape),
   interpolationStringNoDoubleQuote=
         InterpolationString <$> (takeCharsWhile (`notElem` "\\\"$\r\n")
                                  <|> (:[]) <$> characterEscape),
   characterEscape= string "\\" *> ('\b' <$ string "b"
                                    <|> '\f' <$ string "b"
                                    <|> '\r' <$ string "r"
                                    <|> '\n' <$ string "n"
                                    <|> '\t' <$ string "t"
                                    <|> '\v' <$ string "v"
                                    <|> chr . fst . head . readHex
                                        <$> (string "x" *> count 2 hexChar
                                             <|> string "u" *> (count 4 hexChar
                                                                <|> string "{"
                                                                 *> ((:) <$> hexChar <*> upto 4 hexChar)
                                                                <*  string "}"))
                                    <|> satisfyChar (`notElem` "\r\n")),
   singleStringLiteral=
           SimpleStringLiteral <$> simpleStringLiteral
       <|> StringInterpolation <$> stringInterpolation,
   -- | A string literal expression that does not contain any interpolations.
   simpleStringLiteral=
           rawStringLiteral
       <|> basicStringLiteral,
   rawStringLiteral=
           string "r" *> (string "'''" 
                              *> concatMany (takeCharsWhile (/= '\'')
                                             <|> string "'" <* notFollowedBy (string "''"))
                            <* string "'''"
                          <|> string "\"\"\""
                              *> concatMany (takeCharsWhile (/= '\"')
                                             <|> string "\"" <* notFollowedBy (string "\"\""))
                          <|> string "'" *> takeCharsWhile (`notElem` "\'\n") <* string "'"
                          <|> string "\"" *> takeCharsWhile (`notElem` "\"\n") <* string "\""),
   basicStringLiteral=
           multiLineStringLiteral
       <|> singleLineStringLiteral,
   multiLineStringLiteral=
           string "'''" *> concatMany (takeCharsWhile (`notElem` "\\\'$")
                                       <|> string "'" <* notFollowedBy (string "''")) <* string "'''"
       <|> string "\"\"\"" *> concatMany (takeCharsWhile (`notElem` "\\\"$")
                                          <|> string "\"" <* notFollowedBy (string "\"\"")) <* string "\"\"\"",
   singleLineStringLiteral=
           string "'" *> takeCharsWhile (`notElem` "\\\'$\r\n") <* string "'"
       <|> string "\"" *> takeCharsWhile (`notElem` "\\\"$\r\n") <* string "\"",
   -- | A string interpolation literal.
   stringInterpolation=
           string "'" *> many (interpolationExpression <|> interpolationStringNoSingleQuote) <* string "'"
       <|> string "\"" *> many (interpolationExpression <|> interpolationStringNoDoubleQuote) <* string "\"",
   stringLiteral=
           SingleStringLiteral' <$> singleStringLiteral
       <|> AdjacentStrings <$> adjacentStrings,
   -- | Two or more string literals that are implicitly concatenated because of being
   --  adjacent (separated only by whitespace).
   --  While the grammar only allows adjacent strings when all of the strings are of
   --  the same kind (single line or multi-line), this class doesn't enforce that
   --  restriction.
   adjacentStrings=
           (:) <$> stringLiteral <*> some stringLiteral,
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
        read <$> (takeCharsWhile1 isDigit <> moptional (string "." <> takeCharsWhile isDigit) <> moptional exponent
                  <|> string "." <> takeCharsWhile1 isDigit <> moptional exponent),
   exponent=
        (string "e" <|> string "E") <> moptional (string "+" <|> string "-") <> takeCharsWhile1 isDigit,
   -- | An integer literal expression.
   integerLiteral=
        decimalIntegerLiteral
        <|> hexadecimalIntegerLiteral,
   decimalIntegerLiteral=
        read <$> takeCharsWhile1 isDigit,
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
        VariableDeclarationList Nothing [] <$> finalConstVarOrType <*> sepBy variableDeclaration (delimiter ","),
   finalConstVarOrType=
        FCVTFinal <$ keyword "final" <*> optional typeName
        <|> FCVTConst <$ keyword "const" <*> optional typeName
        <|> FCVTVar <$ keyword "var"
        <|> FCVTType <$> typeName,
   declaredIdentifier=
        DeclaredIdentifier Nothing <$> ((:[]) <$> annotation) <*> finalConstVarOrType <*> simpleIdentifier,
   functionDeclaration=
        (FunctionDeclaration Nothing [] True <$ keyword "external") <**> functionSignature <*> pure EmptyFunctionBody
        <|> pure (FunctionDeclaration Nothing [] False) <**> functionSignature <*> functionBody,
   functionSignature= let applySignature returnType propKwd name typeParams params f body = 
                             f returnType propKwd name (FunctionExpression typeParams params body)
                      in applySignature 
                         <$> optional typeName
                         <*> (Get <$ keyword "get" <|> Set <$ keyword "set" <|> pure Empty)
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
        <*  delimiter "{"
        <*> many classMember
        <*  delimiter "}",
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
        <*  delimiter "{"
        <*> sepBy (EnumConstantDeclaration Nothing [] <$> simpleIdentifier) (delimiter ",")
        <*  moptional (delimiter ",")
        <*  delimiter "}",
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
       constructorSignature <*> optional functionBody
       <|> uncurry (ConstructorDeclaration Nothing [] False False False) 
           <$> constructorName 
           <*> formalParameterList 
           <*  delimiter ":" 
           <*> ((:[]) <$> (RedirectingConstructorInvocation <$ keyword "this"
                           <*> optional (delimiter "." *> simpleIdentifier)
                           <*> arguments))
           <*> pure Nothing
           <*> pure Nothing,
   constructorSignature=
       (uncurry <$> (ConstructorDeclaration Nothing []
                    <$> flag (keyword "external") <*> pure False <*> pure False)
                <*> constructorName
        <|> uncurry <$> (ConstructorDeclaration Nothing []
                         <$> flag (keyword "external") <*> pure False <*> (True <$ keyword "factory"))
                    <*> factoryName
        <|> uncurry <$> (ConstructorDeclaration Nothing []
                         <$> flag (keyword "external") <*> flag (keyword "const") <*> pure False)
                    <*> constructorName)
       <*> formalParameterList
       <*> moptional initializerList
       <*> optional (delimiter "=" *> constructorDesignation),
   constructorDesignation=
      ConstructorName
      <$> typeName 
      <*> optional (delimiter "." *> simpleIdentifier),
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
        <$> ((:[]) <$> annotation)
        <*  keyword "export"
        <*> stringLiteral
        <*> pure []  -- configurations
        <*> many combinator
        <*  delimiter ";",
   -- | An import directive.
   importDirective=
        (ImportDirective Nothing
         <$> ((:[]) <$> annotation)
         <*  keyword "import"
         <*> stringLiteral
         <*> pure []  -- configurations
         <*> pure False
         <*> optional (keyword "as" *> simpleIdentifier)
         <|>
         ImportDirective Nothing
          <$> ((:[]) <$> annotation)
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
        <$> ((:[]) <$> annotation)
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
        <$> ((:[]) <$> annotation)
        <*  keyword "part"
        <*  keyword "of"
        <*> libraryIdentifier
        <*  delimiter ";",
   -- | A library directive.
   libraryDirective=
        LibraryDirective Nothing
        <$> ((:[]) <$> annotation)
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
        <*> arguments,
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
        <$> (delimiter "(" *> pure [] <* delimiter ")"
             <|> delimiter "(" *> normalFormalParameters <> moptional (delimiter "," *> optionalFormalParameters)
                 <* delimiter ")"
             <|> delimiter "(" *> optionalFormalParameters <* delimiter ")"),
   normalFormalParameters=
        sepBy1 (NormalFormalParameter' <$> normalFormalParameter) (delimiter ","),
   optionalFormalParameters=
        optionalPositionalFormalParameters
        <|> namedFormalParameters,
   optionalPositionalFormalParameters=
        delimiter "[" *> sepBy1 defaultFormalParameter (delimiter ",") <* delimiter "]",
   namedFormalParameters=
        delimiter "{" *> sepBy1 defaultFormalParameter (delimiter ",") <* delimiter "}",
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
   argumentList=
            delimiter "("
         *> (arguments <|> pure (ArgumentList []))
        <*  delimiter ")",
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
   endOfLineComment=
        string "//" *> takeCharsWhile (/= '\n') <* char '\n',
   blockComment=
        string "/--  "
        *> concatMany (notFollowedBy (string "*/") *> anyToken <> takeCharsWhile (/= '*'))
        <* string "*/",
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
        TypeName <$> identifier <*> optional typeArguments,
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
        <$> many (Label <$> simpleIdentifier)
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
   block=
        Block
        <$  delimiter "{"
        <*> many statement
        <*  delimiter "}",
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
        <|> emptyFunctionBody
        <|> expressionFunctionBody,
   -- | A function body that consists of a block of statements.
   blockFunctionBody=
        BlockFunctionBody
        <$> (Async <$ keyword "async"
             <|> AsyncStar <$ keyword "async" <* delimiter "*"
             <|> SyncStar <$ keyword "sync" <* delimiter "*"
             <|> pure Sync)
        <*> block,
   -- | An empty function body, which can only appear in constructors or abstract
   -- methods.
   emptyFunctionBody=
        EmptyFunctionBody <$ string ";",
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
        <*> functionBody,
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
       <|> CascadeExpression <$> conditionalExpression <*> many cascadeSection,
   expressionWithoutCascade=
           FunctionExpression' <$> functionExpressionWithoutCascade
       <|> throwExpressionWithoutCascade
       <|> AssignmentExpression <$> assignableExpression <*> assignmentOperator <*> expressionWithoutCascade
       <|> conditionalExpression,
   primary=
           thisExpression
       <|> SuperExpression <$ keyword "super" <**> (appEndo <$> unconditionalAssignableSelector)
       <|> constObjectExpression
       <|> newExpression
       <|> FunctionExpression' <$> functionPrimary
       <|> ParenthesizedExpression <$> (keyword "(" *> expression <* keyword ")")
       <|> Literal' <$> literal
       <|> Identifier' <$> identifier,

   throwExpression= ThrowExpression <$ keyword "throw" <*> expression,
   throwExpressionWithoutCascade= ThrowExpression <$ keyword "throw" <*> expressionWithoutCascade,

--   functionExpression= undefined, -- :    formalParameterPart functionExpressionBody
   --functionExpressionBody= undefined, -- :    '=>' { startNonAsyncFunction(); } expression { endFunction(); }
   --    |    ASYNC '=>' { startAsyncFunction(); } expression { endFunction(); }
   --functionExpressionBodyPrefix= undefined, -- :    ASYNC? '=>'
   functionExpressionWithoutCascade= -- formalParameterPart functionExpressionWithoutCascadeBody,
        FunctionExpression
        <$> optional typeParameterList
        <*> formalParameterList
        <*> functionExpressionWithoutCascadeBody,
   functionExpressionWithoutCascadeBody=
        ExpressionFunctionBody
        <$> flag (keyword "async")
        <*  delimiter "=>"
        <*> expressionWithoutCascade
        <*  delimiter ";",
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
      <*> arguments,

   constObjectExpression=
      InstanceCreationExpression NCConst 
      <$  keyword "const" 
      <*> constructorDesignation
      <*> arguments,

   --arguments= parens (argumentList <* optional (keyword ",")),

   --argumentList= moptional expressionList <> many (keyword "," *> namedArgument),

   --namedArgument= undefined, -- :    label expression

   cascadeSection=
       delimiter ".." *> cascadeSelector <**> (appEndo <$> concatMany argumentPart)
            <**> (appEndo <$> concatMany (assignableSelector <> concatMany argumentPart))
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
      <|> IsExpression <$> bitwiseOrExpression <*> flag (delimiter "!") <*> typeName 
      <|> AsExpression <$> bitwiseOrExpression <*> typeName 
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
       <|> primary <**> (appEndo <$> concatMany selector),
   postfixOperator= incrementOperator,
   selector=
           assignableSelector
       <|> argumentPart,
   argumentPart=
       Endo . (InvocationExpression .) 
       <$> (flip <$> (flip FunctionExpressionInvocation <$> optional typeArguments) <*> arguments),
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
           SuperExpression <$ keyword "super" <**> (appEndo <$> unconditionalAssignableSelector)
   --    <|> (typeName typeArguments '.' identifier '(') =>
   --         constructorInvocation
   --         ((assignableSelectorPart) => assignableSelectorPart)+
       <|> primary <**> (appEndo <$> concatSome assignableSelectorPart)
       <|> Identifier' <$> identifier,
   assignableSelectorPart=
       concatMany argumentPart <> assignableSelector,
   unconditionalAssignableSelector=
           Endo . flip IndexExpressionForTarget <$> (delimiter "[" *> expression <* delimiter "]")
       <|> Endo . flip PropertyAccess <$> (delimiter "." *> simpleIdentifier),
   assignableSelector=
           unconditionalAssignableSelector
       <|> Endo . flip ConditionalPropertyAccess <$> (delimiter "?." *> simpleIdentifier),

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
   -- | An await expression.
   awaitExpression=
        AwaitExpression <$ keyword "await" <*> expression,
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
        *>  delimiter "("
        *>  forLoopParts
        <*  delimiter ")"
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
        <*  delimiter "("
        <*> expression
        <*  delimiter ")"
        <*> statement,
   -- | A do statement.
   doStatement=
        DoStatement
        <$  keyword "do"
        <*> statement
        <*  keyword "while"
        <*  delimiter "("
        <*> expression
        <*  delimiter ")"
        <*  delimiter ";",
   -- | A switch statement.
   switchStatement=
        SwitchStatement
        <$  keyword "switch"
        <*   delimiter "("
        <*> expression
        <*  delimiter ")"
        <*  delimiter "{"
        <*> (many switchCase <> upto 1 switchDefault)
        <*  delimiter "}",
   -- | An if statement.
   ifStatement=
        IfStatement
        <$  keyword "if"
        <*  delimiter "("
        <*> expression
        <*  delimiter ")"
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
        directives <*> declarations,
   directives=
        CompilationUnit
        <$> optional scriptTag
        <*> (((:[]) <$> libraryDirective <|> pure [])
             <> (map UriBasedDirective <$> (many (NamespaceDirective <$> namespaceDirective)
                                            <> many partDirective))
             <|> (:[]) <$> partOfDirective),
   declarations=
        many compilationUnitMember}

flag p = True <$ p <|> pure False

upto n p
   | n > 0 = (:) <$> p <*> upto (pred n) p 
             <|> pure []
   | otherwise = pure []
hexChar = satisfyChar isHexDigit

moptional p = p <|> pure mempty

concatSome p = (<>) <$> p <*> concatMany p

delimiter :: String -> Parser Grammar String String
delimiter s = lexicalToken (string s) <?> ("delimiter " <> show s)