{-|
Module      : MZPrinter
Description : MiniZinc pretty-printer
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : BSD3
Maintainer  : Klara Marntirosian <klara.mar@cs.kuleuven.be>
Stability   : experimental

This module provides a pretty-printer of MiniZinc models represented through the 
"Interfaces.MZASTBase" module. This pretty-printer uses the "Text.PrettyPrint" module.
-}

module Interfaces.MZPrinter(
  Interfaces.MZASTBase.MZModel,
  printModel,
  printItem,
  printNakedExpr,
  printAnnExpr,
  layout
) where

import Text.PrettyPrint
import Data.List
import Interfaces.MZASTBase
import Interfaces.MZBuiltIns (opPrec)

layout :: MZModel -> String
layout = render . printModel
  
-- | Prints the represented MiniZinc model. Essentially, this function applies 'printItem' on
-- each element of the specified model.
printModel :: MZModel -> Doc
printModel = foldr1 ($+$) . map printItem

-- | Prints an item of the represented model. Example:
-- 
-- >>> printItem $ Pred "even" [(Dec, Int, "x")] (Just (Bi Eq (Bi Mod (Var "x") (IConst 2)) (IConst 0)))
-- predicate even(var int: x) =
--   x mod 2 = 0;
printItem :: Item -> Doc
printItem (Empty)           = space
printItem (Comment str)     = text "%" <+> text str
printItem (Include file)    = text "include" <+> doubleQuotes (text file) <> semi
printItem (Declare p)       = printDeclaration p <> semi
printItem (Constraint c)    = hang (text "constraint") 2 (printAnnExpr c <> semi)
printItem (Assign var expr) = text var <+> printBody (Just expr) <> semi
printItem (Output e)        = text "output" <+> printNakedExpr e <> semi
printItem (Solve s)         = text "solve" 
                              <+> printSolve s 
                              <> semi

printDeclaration :: Declaration -> Doc
printDeclaration (Declaration nd ans me) =
  hang (printDeclarationSig nd) 2 (sep [printAnnotations ans, printBody me])

printBody :: Maybe AnnExpr -> Doc
printBody = maybe empty (\e -> equals <+> (printAnnExpr e))
  

printDeclarationSig :: DeclarationSignature -> Doc
printDeclarationSig (Variable p)          = printParam p
printDeclarationSig (Predicate name ps)   = text "predicate"
                                            <+> text name
                                            <> parens (printParams ps)
printDeclarationSig (Test name ps)        = text "test"
                                            <+> text name
                                            <> parens (printParams ps)
printDeclarationSig (Function p ps)       = text "function"
                                            <+> printParam p
                                            <> parens (printParams ps)
printDeclarationSig (Annotation' name ps) = text "annotation"
                                            <+> text name
                                            <> parens (printParams ps)

-- Taking precedence into account
printAnnExpr :: AnnExpr -> Doc
printAnnExpr (AnnExpr e [])             = printNakedExpr e
printAnnExpr (AnnExpr e@(Bi _ _ _) ans) = parens (printNakedExpr e) 
                                          <+> printAnnotations ans
printAnnExpr (AnnExpr e@(U _ _) ans)    = parens (printNakedExpr e) 
                                          <+> printAnnotations ans
printAnnExpr (AnnExpr e ans)            = printNakedExpr e 
                                          <+> printAnnotations ans

-- | Prints the represented MiniZinc expressions of a model. Examples:
-- 
-- >>> printNakedExpr $ SetComp (Bi Times (IConst 2) (Var "i")) ([(["i"], Range (IConst 1) (IConst 5))], Nothing)
-- {2 * i | i in 1..5}
-- 
-- >>> printNakedExpr $ Let [Declare Dec Int "x" (Just (IConst 3)), Declare Dec Int "y" (Just (IConst 4))] (Bi BPlus (Var "x") (Var "y"))
-- let {var int: x = 3;
--      var int: y = 4;}
-- in x + y
printNakedExpr :: Expr -> Doc
printNakedExpr AnonVar             = text "_"
printNakedExpr (Var v)             = text v
printNakedExpr (BConst b)
  | b                              = text "true"
  | otherwise                      = text "false"
printNakedExpr (IConst n)          = int n
printNakedExpr (FConst x)          = float x
printNakedExpr (SConst str)        = doubleQuotes $ text (escape str)
printNakedExpr (SetLit es)         = braces $ commaSepExprs es
printNakedExpr (SetComp e ct)      = braces ( printNakedExpr e 
                                              <+> text "|" 
                                              <+> printCompTail ct )
printNakedExpr (ArrayLit es)       = brackets $ commaSepExprs es
printNakedExpr (ArrayLit2D ess)    = 
  brackets (foldl1 ($+$) (map (\x -> text "|" <+> commaSepExprs x) ess) <> text "|")
printNakedExpr (ArrayComp e ct)    = brackets (hang (printNakedExpr e <+> text "|") 0 (printCompTail ct))
printNakedExpr (ArrayElem v es)    = text v <> brackets (commaSepExprs es)
printNakedExpr (U op e)            = printOp op 
                                     <+> (
                                       if isAtomic e 
                                       then printNakedExpr e 
                                       else parens (printNakedExpr e)
                                     )
printNakedExpr (Bi op e1 e2)       = sep [printParensNakedExpr (opPrec op) e1 
                                         , printOp op 
                                         , printParensNakedExpr (opPrec op) e2]
printNakedExpr (Call name args)    = text name 
                                     <> printArgs printAnnExpr args
printNakedExpr (ITE (pe:pes) e)    = sep (listIT pe ++ listEIT pes ++ listEI e)
printNakedExpr (Let is e)          = text "let" 
                                     <+> braces (nest 4 (vcat (map printItem is))) 
                                     $+$ text "in" <+> printNakedExpr e
printNakedExpr (GenCall name ct e) = 
  hang (text name <> parens (printCompTail ct)) 2 (parens (printNakedExpr e))

listIT :: (Expr, Expr) -> [Doc]
listIT (e1, e2) = [ text "if" <+> printNakedExpr e1
                  , text "then" <+> printNakedExpr e2]

listEIT :: [(Expr, Expr)] -> [Doc]
listEIT []            = []
listEIT ((e1, e2):es) = [ text "elseif" <+> printNakedExpr e1
                        , text "then" <+> printNakedExpr e2]
                        ++ listEIT es 

listEI :: Expr -> [Doc]
listEI e = [ text "else" <+> printNakedExpr e
           , text "endif"]

-- This function is used for placing parentheses in expressions
printParensNakedExpr :: Int -> Expr -> Doc
-- A smaller integer represents higher precedence (tighter binding)
printParensNakedExpr n e@(Bi op _ _)
  | opPrec op <= n         = printNakedExpr e
  | otherwise             = parens $ printNakedExpr e
printParensNakedExpr _ e  = printNakedExpr e

printType :: Type -> Doc
printType Bool           = text "bool"
printType Float          = text "float"
printType Int            = text "int"
printType String         = text "string"
printType (Set t)        = text "set of" <+> printType t
printType (Array ts ti)  = text "array" <> brackets (commaSep printType ts) 
                           <+> text "of" <+> printTypeInst ti
printType (List ti)      = text "list of" <+> printTypeInst ti
printType (Opt t)        = text "opt" <+> printType t
printType (Ann)          = text "ann"
printType (Range e1 e2)  = printParensNakedExpr (opPrec (Op "..")) e1 
                           <+> text ".." 
                           <+> printParensNakedExpr (opPrec (Op "..")) e2
printType (Elems es)     = braces $ commaSepExprs es
printType (ACT name)     = text name
printType (VarType name) = text "$" <> text name

printCompTail :: CompTail -> Doc
printCompTail (gs, Nothing) = commaSep printGenerator gs
printCompTail (gs, Just wh) = commaSep printGenerator gs 
                              <+> text "where" 
                              <+> printNakedExpr wh

printGenerator :: Generator -> Doc
printGenerator (es, r) = text (intercalate ", " es) 
                         <+> text "in" 
                         <+> printNakedExpr r

printInst :: Inst -> Doc
printInst Dec = text "var"
printInst Par = text "par"

printAnnotations :: [Annotation] -> Doc
printAnnotations ans = sep $ map (\a -> colon <> colon <+> printAnnotation a) ans

printAnnotation :: Annotation -> Doc
printAnnotation (Annotation name args) 
  = text name
  <> case args of
       [] -> empty
       xs -> printArgs printGArg args

printArgs :: (a -> Doc) -> [a] -> Doc
printArgs f args = cat $ putParens (punctuateBefore comma (map f args))

printGArg :: GArguments -> Doc
printGArg (A a) = printAnnotation a
printGArg (E e) = printNakedExpr e

printOp :: Op -> Doc
printOp (Op op)  = text op

printSolve :: Solve -> Doc
printSolve (Satisfy  ans  ) = printAnnotations ans <+> text "satisfy"
printSolve (Minimize ans e) = printAnnotations ans 
                              <+> text "minimize"
                              <+> printNakedExpr e
printSolve (Maximize ans e) = printAnnotations ans 
                              <+> text "maximize"
                              <+> printNakedExpr e

printParams :: [Param] -> Doc
printParams ps = commaSep printParam ps

-- Prints the parameters of call expressions (predicates, tests and functions) or annotations
printParam :: Param -> Doc
printParam (i, t, n) = printTypeInst (i, t) <> colon <+> text n

-- Prints the instantiation (var or par) and the type in a variable declaration. If the
-- type is Array or String, it does not print the inst, since these types are of fixed
-- inst. Same with @Ann@ type, but for other reasons.
printTypeInst :: (Inst, Type) -> Doc
printTypeInst (_, t@(Array _ _)) = printType t
printTypeInst (_, String)        = printType String
printTypeInst (_, Ann)           = printType Ann
printTypeInst (i, t)             = printInst i <+> printType t

-- Horizontally concatinates Docs while also putting a comma-space (", ") in between
commaSepDoc :: [Doc] -> Doc
commaSepDoc = hsep . punctuate comma

-- Vertically prints expressions, one per line
-- lineSepExpr :: [Expr] -> Doc
-- lineSepExpr = vcat . map printNakedExpr

-- First, map a function to a list and produce a list of Docs and then apply commaSepDoc
commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f ls = commaSepDoc $ map f ls

-- Special case of commaSep, where f = printNakedExpr
commaSepExprs :: [Expr] -> Doc
commaSepExprs = commaSep printNakedExpr

commaSepArgs :: [Either Annotation Expr] -> Doc
commaSepArgs = commaSep printExprOrAnnot
  where printExprOrAnnot (Right e) = printNakedExpr e
        printExprOrAnnot (Left a)  = printAnnotation a

isAtomic :: Expr -> Bool
isAtomic AnonVar         = True
isAtomic (Var _)         = True
isAtomic (BConst _)      = True
isAtomic (IConst _)      = True
isAtomic (FConst _)      = True
isAtomic (SConst _)      = True
isAtomic (SetLit _)      = True
isAtomic (ArrayLit _)    = True
isAtomic (ArrayLit2D _)  = True
isAtomic (ArrayElem _ _) = True
isAtomic (SetComp _ _)   = True
isAtomic (ArrayComp _ _) = True
isAtomic (Call _ _)      = True
isAtomic _               = False

escape:: String -> String
escape str = concatMap escapeChar str

escapeChar :: Char -> String
escapeChar '\n' = "\\n"
escapeChar '\t' = "\\t"
escapeChar '\r' = "\\r"
escapeChar '\\' = "\\\\"
escapeChar '\f' = "\\f"
escapeChar '\a' = "\\a"
escapeChar c = [c]

putParens :: [Doc] -> [Doc]
putParens []  = []
putParens [x] = [parens x]
putParens xs  = let f = head xs
                    l = last xs
                in text "(" <> f : (init (tail xs)) ++ [l <> text ")"]

punctuateBefore :: Doc -> [Doc] -> [Doc]
punctuateBefore _ []     = []
punctuateBefore p (d:ds) = d : map (p <+>) ds