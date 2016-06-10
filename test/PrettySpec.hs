{-# LANGUAGE QuasiQuotes #-}

module PrettySpec where

import Test.Hspec

import Language.Dart.Pretty
import Language.Dart.Syntax
import Text.RawString.QQ

import TestHelper

spec :: Spec
spec =
  describe "prettyPrint" $ do
    it "preserves operator precedence" $ do
      let l = BinaryExpression (intExp 1) "+" (intExp 2)
          r = BinaryExpression (intExp 3) "-" (intExp 2)
          e = BinaryExpression l "*" r
      prettyPrint e `shouldBe` "(1 + 2) * (3 - 2)"

    it "prints an import directive" $ do
      let libraryUri = strLit "package:analyzer/src/dart/ast/ast.dart"
          as = SimpleIdentifier "a"
          imp = ImportDirective Nothing
                                []
                                libraryUri
                                []
                                False
                                (Just as)
                                [ ShowCombinator [SimpleIdentifier "TypeName"]
                                , ShowCombinator [SimpleIdentifier "ArgumentList"]
                                ]
      prettyPrint imp `shouldBe` [r|import "package:analyzer/src/dart/ast/ast.dart" as a show TypeName, show ArgumentList;|]

    it "prints a function declaration" $ do
      let formals = FormalParameterList [formal numType "aNumber"]
          arg = Literal' . StringLiteral' . SingleStringLiteral' $
                  StringInterpolation [ InterpolationString "The number is "
                                      , InterpolationExpression (identExp "aNumber")
                                      , InterpolationString "."]
          body = BlockFunctionBody Sync
                                   (Block [ ExpressionStatement (
                                             InvocationExpression (
                                               MethodInvocation Nothing
                                                                (SimpleIdentifier "print")
                                                                Nothing
                                                                (ArgumentList [arg])))])
          funExp = FunctionExpression Nothing formals body
          funDecl = FunctionDeclaration Nothing [] False Nothing Empty (SimpleIdentifier "printNumber") funExp
      prettyPrint funDecl `shouldBe` [r|printNumber(num aNumber)
{
  print("The number is ${aNumber}.");
}|]

    it "prints an optional position parameter" $ do
      let formals = FormalParameterList [formal strType "from", formal strType "msg", posFormal strType "device"]
          str1 = Literal' . StringLiteral' . SingleStringLiteral' $
                  StringInterpolation [ InterpolationExpression (identExp "from")
                                      , InterpolationString " says "
                                      , InterpolationExpression (identExp "msg")
                                      ]
          str2 = Literal' . StringLiteral' . SingleStringLiteral' $
                  StringInterpolation [ InterpolationExpression (identExp "result")
                                      , InterpolationString " with a "
                                      , InterpolationExpression (identExp "device")
                                      ]
          body = BlockFunctionBody Sync
                                   (Block [ VariableDeclarationStatement (
                                              VariableDeclarationList Nothing
                                                                      []
                                                                      FCVTVar
                                                                      [VariableDeclaration (SimpleIdentifier "result") (Just str1)])
                                          , IfStatement (BinaryExpression (identExp "device") "!=" nullExp)
                                                        (Block' (Block [ ExpressionStatement (
                                                                          AssignmentExpression (identExp "result") "=" str2)]))
                                                        Nothing
                                          , ReturnStatement (Just (identExp "result"))
                                          ])
          funExp = FunctionExpression Nothing formals body
          funDecl = FunctionDeclaration Nothing [] False (Just strType) Empty (SimpleIdentifier "say") funExp
      prettyPrint funDecl `shouldBe` [r|String say(String from, String msg, [String device])
{
  var result = "${from} says ${msg}";
  if (device != null)
  {
    result = "${result} with a ${device}";
  }
  return result;
}|]

    it "prints an expression function" $ do
      let body = ExpressionFunctionBody False (
                  ConditionalExpression (BinaryExpression (identExp "msg") "==" nullExp)
                                        (InvocationExpression
                                          (MethodInvocation (Just SuperExpression)
                                                            (SimpleIdentifier "toString")
                                                            Nothing
                                                            (ArgumentList [])))
                                        (identExp "msg"))

          funExp = FunctionExpression Nothing (FormalParameterList []) body
          funDecl = FunctionDeclaration Nothing [] False (Just strType) Empty (SimpleIdentifier "toString") funExp
      prettyPrint funDecl `shouldBe` "String toString() => msg == null ? super.toString() : msg;"

    it "prints an enum" $ do
      let enumConstDecls = [ EnumConstantDeclaration Nothing [] (SimpleIdentifier "red")
                           , EnumConstantDeclaration Nothing [] (SimpleIdentifier "green")
                           , EnumConstantDeclaration Nothing [] (SimpleIdentifier "blue")
                           ]
          enumDecl = EnumDeclaration Nothing [] (SimpleIdentifier "Color") enumConstDecls
      prettyPrint enumDecl `shouldBe` [r|enum Color
{
  red,
  green,
  blue
}|]

    it "prints a for loop" $ do
      let variableList = VariableDeclarationList Nothing
                                                 []
                                                 FCVTVar
                                                 [VariableDeclaration (SimpleIdentifier "i") (Just (intExp 0))]
          condition = BinaryExpression (identExp "i") "<" (intExp 5)
          updater = PostfixExpression (identExp "i") "++"
          body = (Block' (Block [ ExpressionStatement (
                                    InvocationExpression (
                                      MethodInvocation (Just (identExp "message"))
                                                       (SimpleIdentifier "write")
                                                       Nothing
                                                       (ArgumentList [strLitExp "!"])))]))
          forLoop = ForStatement (Just variableList)
                                 Nothing
                                 (Just condition)
                                 [updater]
                                 body
      prettyPrint forLoop `shouldBe` [r|for (var i = 0; i < 5; i++)
{
  message.write("!");
}|]

    it "prints a list literal" $ do
      let litExp = Literal' . TypedLiteral $ ListLiteral False Nothing [intExp 0, intExp 1, intExp 2]
      prettyPrint litExp `shouldBe` "[0, 1, 2]"

    it "prints parameterized types" $ do
      let consName = ConstructorName (TypeName (SimpleIdentifier' (SimpleIdentifier "Map"))
                                               (Just (TypeArgumentList [tyName "int", tyName "View"])))
                                     Nothing
      let exp = InstanceCreationExpression NCNew consName (ArgumentList [])
      prettyPrint exp `shouldBe` "new Map<int, View>()"

    it "prints a switch statement" $ do
      let switchMembers = [ SwitchCase [] (strLitExp "CLOSED") [funStmt "executeClosed", (BreakStatement Nothing)]
                          , SwitchCase [] (strLitExp "PENDING") [funStmt "executePending", (BreakStatement Nothing)]
                          , SwitchCase [] (strLitExp "APPROVED") [funStmt "executeApproved", (BreakStatement Nothing)]
                          , SwitchCase [] (strLitExp "DENIED") [funStmt "executeDenied", (BreakStatement Nothing)]
                          , SwitchCase [] (strLitExp "OPEN") [funStmt "executeOpen", (BreakStatement Nothing)]
                          , SwitchDefault [] [funStmt "executeUnknown"]
                          ]
          switch = SwitchStatement (identExp "command") switchMembers
      prettyPrint switch `shouldBe` [r|switch (command)
{
  case "CLOSED":
    executeClosed();
    break;
  case "PENDING":
    executePending();
    break;
  case "APPROVED":
    executeApproved();
    break;
  case "DENIED":
    executeDenied();
    break;
  case "OPEN":
    executeOpen();
    break;
  default:
    executeUnknown();
}|]

    it "prints a try statement" $ do
      let arg = Literal' . StringLiteral' . SingleStringLiteral' $
                  StringInterpolation [ InterpolationString "Error: "
                                      , InterpolationExpression (identExp "e")
                                      ]
          try = TryStatement (Block [funStmt "breedMoreLlamas"])
                             [CatchClause Nothing (SimpleIdentifier "e") Nothing (Block [funStmt1 "print" arg])]
                             (Just (Block [funStmt "cleanLlamaStalls"]))
      prettyPrint try `shouldBe` [r|try
{
  breedMoreLlamas();
}
catch (e)
{
  print("Error: ${e}");
}
finally
{
  cleanLlamaStalls();
}|]

    it "prints a constructor" $ do
      let varDeclList ty name = VariableDeclarationList Nothing
                                                        []
                                                        (FCVTType (tyName ty))
                                                        [VariableDeclaration (SimpleIdentifier name) Nothing]

          returnType = SimpleIdentifier' (SimpleIdentifier "Point")
          consFormals1 = FormalParameterList [ NormalFormalParameter' (FieldFormalParameter Nothing [] Nothing True (SimpleIdentifier "x"))
                                             , NormalFormalParameter' (FieldFormalParameter Nothing [] Nothing True (SimpleIdentifier "y"))
                                             ]
          consFormals2 = FormalParameterList [ NormalFormalParameter' (
                                                 FieldFormalParameter Nothing
                                                                      []
                                                                      (Just (FCVTType (tyName "Map")))
                                                                      False
                                                                      (SimpleIdentifier "json"))
                                             ]
          methodFormals = FormalParameterList [ NormalFormalParameter' (
                                                  FieldFormalParameter Nothing
                                                                       []
                                                                       (Just (FCVTType (tyName "Point")))
                                                                       False
                                                                       (SimpleIdentifier "other"))
                                              ]
          consBody = BlockFunctionBody Sync
                                   (Block [ ExpressionStatement (
                                              AssignmentExpression (identExp "x")
                                                                   "="
                                                                   (IndexExpressionForTarget (identExp "json") (strLitExp "x")))
                                          , ExpressionStatement (
                                              AssignmentExpression (identExp "y")
                                                                   "="
                                                                   (IndexExpressionForTarget (identExp "json") (strLitExp "y")))
                                          ])
          exp1 = BinaryExpression (identExp "x") "-" (PropertyAccess (identExp "other") (SimpleIdentifier "x"))
          exp2 = BinaryExpression (identExp "y") "-" (PropertyAccess (identExp "other") (SimpleIdentifier "y"))
          methodBody = BlockFunctionBody Sync
                                   (Block [ VariableDeclarationStatement (
                                              VariableDeclarationList Nothing
                                                                      []
                                                                      FCVTVar
                                                                      [VariableDeclaration (SimpleIdentifier "dx") (Just exp1)])
                                          , VariableDeclarationStatement (
                                              VariableDeclarationList Nothing
                                                                      []
                                                                      FCVTVar
                                                                      [VariableDeclaration (SimpleIdentifier "dy") (Just exp2)])
                                          , ReturnStatement (Just (
                                              InvocationExpression (
                                                MethodInvocation Nothing
                                                                 (SimpleIdentifier "sqrt")
                                                                 Nothing
                                                                 (ArgumentList [BinaryExpression (BinaryExpression (identExp "dx") "*" (identExp "dx"))
                                                                                                 "+"
                                                                                                 (BinaryExpression (identExp "dy") "*" (identExp "dy"))]))))
                                          ])
          classMembers = [ FieldDeclaration Nothing [] False (varDeclList "num" "x")
                         , FieldDeclaration Nothing [] False (varDeclList "num" "y")
                         , ConstructorDeclaration Nothing [] False False False returnType Nothing consFormals1 [] Nothing Nothing
                         , ConstructorDeclaration Nothing [] False False False returnType (Just (SimpleIdentifier "fromJson")) consFormals2 [] Nothing (Just consBody)
                         , MethodDeclaration Nothing [] False Nothing (Just (tyName "num")) Empty False (SimpleIdentifier "distanceTo") Nothing (Just methodFormals) methodBody
                         ]
          clazz = ClassDeclaration Nothing
                                   []
                                   False
                                   (SimpleIdentifier "Point")
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   classMembers
      prettyPrint clazz `shouldBe` [r|class Point
{
  num x;
  num y;
  Point(this.x, this.y);
  Point.fromJson(Map json)
  {
    x = json["x"];
    y = json["y"];
  }
  num distanceTo(Point other)
  {
    var dx = x - other.x;
    var dy = y - other.y;
    return sqrt(dx * dx + dy * dy);
  }
}|]

    it "prints the initializer lists of a constructor" $ do
      let varDeclList ty name = VariableDeclarationList Nothing
                                                        []
                                                        (FCVTType (tyName ty))
                                                        [VariableDeclaration (SimpleIdentifier name) Nothing]
          returnType = SimpleIdentifier' (SimpleIdentifier "Point")
          formals = FormalParameterList [ NormalFormalParameter' (FieldFormalParameter Nothing [] Nothing True (SimpleIdentifier "x"))
                                        , NormalFormalParameter' (FieldFormalParameter Nothing [] Nothing True (SimpleIdentifier "y"))
                                        ]
          exp = InvocationExpression (
                 MethodInvocation Nothing
                                  (SimpleIdentifier "sqrt")
                                  Nothing
                                  (ArgumentList [BinaryExpression (BinaryExpression (identExp "x") "*" (identExp "x"))
                                                                  "+"
                                                                  (BinaryExpression (identExp "y") "*" (identExp "y"))]))
          classMembers = [ FieldDeclaration Nothing [] False (varDeclList "num" "x")
                         , FieldDeclaration Nothing [] False (varDeclList "num" "y")
                         , FieldDeclaration Nothing [] False (varDeclList "num" "distanceFromOrigin")
                         , ConstructorDeclaration Nothing
                                                  []
                                                  False
                                                  False
                                                  False
                                                  returnType
                                                  Nothing
                                                  formals
                                                  [ ConstructorFieldInitializer False (SimpleIdentifier "x") (identExp "x")
                                                  , ConstructorFieldInitializer False (SimpleIdentifier "y") (identExp "y")
                                                  , ConstructorFieldInitializer False (SimpleIdentifier "distanceFromOrigin") exp
                                                  ]
                                                  Nothing
                                                  Nothing
                         ]
          clazz = ClassDeclaration Nothing
                                   []
                                   False
                                   (SimpleIdentifier "Point")
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   classMembers

      prettyPrint clazz `shouldBe` [r|class Point
{
  num x;
  num y;
  num distanceFromOrigin;
  Point(this.x, this.y) : x = x,
                          y = y,
                          distanceFromOrigin = sqrt(x * x + y * y);
}|]

    it "prints the implement clause" $ do
      let implements = ImplementsClause [tyName "Comparable", tyName "Location"]
          clazz = ClassDeclaration Nothing
                                   []
                                   False
                                   (SimpleIdentifier "Point")
                                   Nothing
                                   Nothing
                                   Nothing
                                   (Just implements)
                                   []
      prettyPrint clazz `shouldBe` [r|class Point implements Comparable, Location
{
}|]

    it "prints the metadata" $ do
      let annotation = Annotation (SimpleIdentifier' (SimpleIdentifier "override")) Nothing Nothing
          methodFormals = FormalParameterList [ NormalFormalParameter' (
                                                  FieldFormalParameter Nothing
                                                                       []
                                                                       (Just (FCVTType (tyName "Invocation")))
                                                                       False
                                                                       (SimpleIdentifier "mirror"))
                                              ]
          methodDecl = MethodDeclaration Nothing
                                         [annotation]
                                         False
                                         Nothing
                                         (Just (tyName "void"))
                                         Empty
                                         False
                                         (SimpleIdentifier "noSuchMethod")
                                         Nothing
                                         (Just methodFormals)
                                         (BlockFunctionBody Sync (Block []))
      prettyPrint methodDecl `shouldBe` [r|@override
void noSuchMethod(Invocation mirror)
{
}|]

main :: IO ()
main = hspec spec
