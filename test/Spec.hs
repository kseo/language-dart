import Test.Hspec

import Language.Dart.Pretty
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
  SimpleFormalParameter Nothing [] (FVTType ty) (SimpleIdentifier p)

posFormal :: TypeName -> String -> FormalParameter
posFormal ty p = DefaultFormalParameter parameter Positional Nothing
  where parameter = (SimpleFormalParameter Nothing [] (FVTType ty) (SimpleIdentifier p))

var :: String -> FormalParameter
var p = NormalFormalParameter' $
  SimpleFormalParameter Nothing [] FVTVar (SimpleIdentifier p)

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

prettyPrintSpec :: Spec
prettyPrintSpec =
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
      prettyPrint imp `shouldBe` "import \"package:analyzer/src/dart/ast/ast.dart\" as a show TypeName, show ArgumentList;"

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
      prettyPrint funDecl `shouldBe` "printNumber(num aNumber)\n\
                                     \{\n\
                                     \  print(\"The number is ${aNumber}.\");\n\
                                     \}"

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
      prettyPrint funDecl `shouldBe` "String say(String from, String msg, [String device])\n\
                                     \{\n\
                                     \  var result = \"${from} says ${msg}\";\n\
                                     \  if (device != null)\n\
                                     \  {\n\
                                     \    result = \"${result} with a ${device}\";\n\
                                     \  }\n\
                                     \  return result;\n\
                                     \}"

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
      prettyPrint enumDecl `shouldBe` "enum Color\n\
                                      \{\n\
                                      \  red,\n\
                                      \  green,\n\
                                      \  blue\n\
                                      \}"

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
      prettyPrint forLoop `shouldBe` "for (var i = 0; i < 5; i++)\n\
                                     \{\n\
                                     \  message.write(\"!\");\n\
                                     \}"

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
      prettyPrint switch `shouldBe` "switch (command)\n\
                                    \{\n\
                                    \  case \"CLOSED\":\n\
                                    \    executeClosed();\n\
                                    \    break;\n\
                                    \  case \"PENDING\":\n\
                                    \    executePending();\n\
                                    \    break;\n\
                                    \  case \"APPROVED\":\n\
                                    \    executeApproved();\n\
                                    \    break;\n\
                                    \  case \"DENIED\":\n\
                                    \    executeDenied();\n\
                                    \    break;\n\
                                    \  case \"OPEN\":\n\
                                    \    executeOpen();\n\
                                    \    break;\n\
                                    \  default:\n\
                                    \    executeUnknown();\n\
                                    \}"

    it "prints a try statement" $ do
      let arg = Literal' . StringLiteral' . SingleStringLiteral' $
                  StringInterpolation [ InterpolationString "Error: "
                                      , InterpolationExpression (identExp "e")
                                      ]
          try = TryStatement (Block [funStmt "breedMoreLlamas"])
                             [CatchClause Nothing (SimpleIdentifier "e") Nothing (Block [funStmt1 "print" arg])]
                             (Just (Block [funStmt "cleanLlamaStalls"]))
      prettyPrint try `shouldBe` "try\n\
                                 \{\n\
                                 \  breedMoreLlamas();\n\
                                 \}\n\
                                 \catch (e)\n\
                                 \{\n\
                                 \  print(\"Error: ${e}\");\n\
                                 \}\n\
                                 \finally\n\
                                 \{\n\
                                 \  cleanLlamaStalls();\n\
                                 \}"

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
      prettyPrint clazz `shouldBe` "class Point\n\
                                   \{\n\
                                   \  num x;\n\
                                   \  num y;\n\
                                   \  Point(this.x, this.y);\n\
                                   \  Point.fromJson(Map json)\n\
                                   \  {\n\
                                   \    x = json[\"x\"];\n\
                                   \    y = json[\"y\"];\n\
                                   \  }\n\
                                   \  num distanceTo(Point other)\n\
                                   \  {\n\
                                   \    var dx = x - other.x;\n\
                                   \    var dy = y - other.y;\n\
                                   \    return sqrt(dx * dx + dy * dy);\n\
                                   \  }\n\
                                   \}"

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

      prettyPrint clazz `shouldBe` "class Point\n\
                                   \{\n\
                                   \  num x;\n\
                                   \  num y;\n\
                                   \  num distanceFromOrigin;\n\
                                   \  Point(this.x, this.y) : x = x,\n\
                                   \                          y = y,\n\
                                   \                          distanceFromOrigin = sqrt(x * x + y * y);\n\
                                   \}"

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
      prettyPrint clazz `shouldBe` "class Point implements Comparable, Location\n\
                                  \{\n\
                                  \}"

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
      prettyPrint methodDecl `shouldBe` "@override\n\
                                        \void noSuchMethod(Invocation mirror)\n\
                                        \{\n\
                                        \}"

main :: IO ()
main = hspec $ do
  prettyPrintSpec
