module TestHelper where

import Language.Dart.Syntax

intExp :: Integer -> Expression
intExp = Literal' . IntegerLiteral

ident :: String -> Identifier
ident = SimpleIdentifier' . SimpleIdentifier

identExp :: String -> Expression
identExp = Identifier' . ident

tyName :: String -> TypeName
tyName name = TypeName (ident name) Nothing

strType = tyName "String"
numType = tyName "num"

formal :: TypeName -> String -> FormalParameter
formal ty p = NormalFormalParameter' $
  SimpleFormalParameter Nothing [] (Just (FVTType ty)) (SimpleIdentifier p)

posFormal :: TypeName -> String -> FormalParameter
posFormal ty p = DefaultFormalParameter parameter Positional Nothing
  where parameter = (SimpleFormalParameter Nothing [] (Just (FVTType ty)) (SimpleIdentifier p))

var :: String -> FormalParameter
var p = NormalFormalParameter' $
  SimpleFormalParameter Nothing [] (Just FVTVar) (SimpleIdentifier p)

strLit :: String -> StringLiteral
strLit = SingleStringLiteral' .  SimpleStringLiteral

strLitExp :: String -> Expression
strLitExp = Literal' . StringLiteral' . strLit

nullExp :: Expression
nullExp = Literal' NullLiteral

funStmt :: String -> Statement
funStmt name = ExpressionStatement (
                 InvocationExpression (
                   MethodInvocation Nothing
                                    (SimpleIdentifier name)
                                    Nothing
                                    (ArgumentList [])))

funStmt1 :: String -> Expression -> Statement
funStmt1 name arg1 = ExpressionStatement (
                       InvocationExpression (
                         MethodInvocation Nothing
                                          (SimpleIdentifier name)
                                          Nothing
                                          (ArgumentList [arg1])))

