{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
-- {-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fdefer-typed-holes #-} -- TEMP

-- | Generate GLSL code from a circuit graph

module ConCat.Graphics.GLSL (Anim,CAnim,genHtml,runHtml) where

import Data.List (sort)
import qualified Data.Map as M
import Text.Printf (printf)
import System.Directory (createDirectoryIfMissing)
import qualified System.Info as SI
-- import qualified Debug.Trace as T

import Text.ParserCombinators.Parsec (runParser,ParseError)
import Text.PrettyPrint.HughesPJClass -- (Pretty,prettyShow)
import Language.GLSL.Syntax
import Language.GLSL.Pretty ()
import Language.GLSL.Parser hiding (parse)

-- import NeatInterpolation
-- import Data.Text (Text)

import ConCat.Misc ((:*),R)
import qualified ConCat.AltCat as A
import ConCat.Circuit (Comp(..),Bus(..),busTy,(:>),mkGraph,pattern CompS,systemSuccess)
import qualified ConCat.Circuit as C

type Image = R :* R -> Bool     -- TODO: color etc

type  Anim = R -> Image
type CAnim = R :> Image

-- genGlsl :: String -> CAnim -> IO ()
-- genGlsl name anim =
--   do createDirectoryIfMissing False outDir
--      writeFile (outDir++"/"++name++".frag") (glsl anim)
--  where
--    outDir = "out/shaders"

animHtml :: CAnim -> String
animHtml anim = unlines $
  [ "<!DOCTYPE html>" , "<html>" , "<head>"
  , "<meta charset='utf-8'/>"
  , "<script type='text/javascript' src='script.js'></script>"
  , "</head>"
  , "<body style='margin:0px' onload='go()'>"
  , "<canvas id='effect_canvas' style='background-color:green'></canvas>"
  , "</body>" , "</html>"
  , "<script>"
  , "var effect_source = `"
  , "", glsl anim, ""
  , "`;"
  , "</script>" ]

genHtml :: String -> CAnim -> IO ()
genHtml name anim =
  do createDirectoryIfMissing False outDir
     let o = outFile name
     writeFile o (animHtml anim)
     putStrLn ("Wrote " ++ o)

runHtml :: String -> CAnim -> IO ()
runHtml name anim =
  do genHtml name anim
     systemSuccess $ printf "%s %s" open (outFile name)

outDir :: String
outDir = "out/shaders"

outFile :: String -> String
outFile name = outDir++"/"++name++".html"

open :: String
open = case SI.os of
         "darwin" -> "open"
         "linux"  -> "display" -- was "xdg-open"
         _        -> error "unknown open for OS"

-- TODO: open is also defined in Circuit. Get it from there, or move elsewhere.
-- Move the createDirectoryIfMissing logic there as well.
-- Also the writeFile and putStrLn.

glsl :: CAnim -> String
glsl = prettyShow . fromComps . sort . mkGraph {- . T.traceShowId -} . A.uncurry {- . T.traceShowId -}

constExpr :: C.Ty -> String -> Expr
constExpr C.Bool   = BoolConstant        . read
constExpr C.Int    = IntConstant Decimal . read
constExpr C.Float  = FloatConstant       . read
constExpr C.Double = FloatConstant       . read
constExpr ty = error ("ConCat.GLSL.constExpr: unexpected literal type: " ++ show ty)

fromComps :: [Comp] -> ExternalDeclaration
-- fromComps comps | trace ("fromComps " ++ show comps) False = undefined
fromComps comps
  | (CompS _ "In" [] inputs,mid, CompS _ "Out" [res] _) <- splitComps comps
  , let (bindings, assignments) = accumComps (uses mid) mid
  = funDef Bool "effect" (paramDecl <$> inputs)
           (map (uncurry initBus) assignments
            ++ [Return (Just (bindings M.! res))])
fromComps comps = error ("ConCat.GLSL.fromComps: unexpected subgraph comp " ++ show comps)

-- Count uses of each output
uses :: [Comp] -> M.Map Bus Int
uses = M.unionsWith (+) . map uses1

-- Uses map for a single component
uses1 :: Comp -> M.Map Bus Int
uses1 (CompS _ _ ins _) = M.unionsWith (+) (flip M.singleton 1 <$> ins)
uses1 comp = error ("ConCat.GLSL.uses1: unexpected subgraph comp " ++ show comp)

nestExpressions :: Bool
nestExpressions = True -- False

-- Given usage counts, generate delayed bindings and assignments
accumComps :: M.Map Bus Int -> [Comp] -> (M.Map Bus Expr, [(Bus,Expr)])
-- accumComps counts | trace ("accumComps: counts = " ++ show counts) False = undefined
accumComps counts = go M.empty
 where
   -- Generate bindings for outputs used more than once,
   -- and accumulate a map of the others.
   go :: M.Map Bus Expr -> [Comp] -> (M.Map Bus Expr, [(Bus,Expr)])
   -- go saved comps | trace ("accumComps/go " ++ show saved ++ " " ++ show comps) False = undefined
   go saved [] = (saved, [])
   go saved (c@(CompS _ _ _ [o]) : comps) 
     | Just n <- M.lookup o counts, (n > 1 || not nestExpressions) =
         let (saved',bindings') = go saved comps in
           (saved', (o,e) : bindings')
     | otherwise = go (M.insert o e saved) comps
    where
      e = compExpr saved c
   go _ c = error ("ConCat.GLSL.accumComps: oops: " ++ show c)

compExpr :: M.Map Bus Expr -> Comp -> Expr
compExpr _ (CompS _ str [] [Bus _ _ ty]) = constExpr ty str
compExpr saved (CompS _ prim ins [Bus _ _ ty]) = app ty prim (inExpr <$> ins)
 where
   inExpr :: Bus -> Expr
   inExpr b | Just e <- M.lookup b saved = e
            | otherwise = bToE b
compExpr _ comp = error ("ConCat.GLSL.compExpr: unexpected subgraph comp " ++ show comp)

busType :: Bus -> TypeSpecifierNonArray
busType = glslTy . busTy

initBus :: Bus -> Expr -> Statement
initBus b e = initDecl (busType b) (varName b) e

glslTy :: C.Ty -> TypeSpecifierNonArray
glslTy C.Int    = Int
glslTy C.Bool   = Bool
glslTy C.Float  = Float
glslTy C.Double = Float
glslTy ty = error ("ConCat.GLSL.glslTy: unsupported type: " ++ show ty)

varName :: Bus -> String
varName (Bus 0 n _) = "in" ++ show n
varName (Bus c 0 _) = "v" ++ show c
varName b = error ("ConCat.GLSL.varName unexpected " ++ show b)

-- All actual primitives have exactly one output. The fake In primitive can have
-- any number, and the fake Out primitive has none. I think I'd like to
-- eliminate those fake prims, but I'm not ready to rule out multi-output
-- primitives.

app :: C.Ty -> String -> [Expr] -> Expr
app ty nm es =
  case nm of
    "not"    -> app1 UnaryNot
    "&&"     -> app2 And
    "||"     -> app2 Or 
    "<"      -> app2 Lt 
    ">"      -> app2 Gt 
    "<="     -> app2 Lte
    ">="     -> app2 Gte
    "=="     -> app2 Equ
    "/="     -> app2 Neq
    "negate" -> app1 UnaryNegate
    "+"      -> app2 Add
    "-"      -> app2 Sub
    "−"      -> app2 Sub
    "*"      -> app2 Mul
    "/"      -> app2 Div
    "mod"    -> app2 Mod
    "xor"    -> app2 Neq
    "fromIntegral" -> funcall (castFun (glslTy ty)) es
    _ | Just fun <- M.lookup nm knownFuncs -> funcall fun es
      | otherwise -> error ("ConCat.GLSL.app: not supported: " ++ show (nm,es))
 where
   err str = error ("app " ++ nm ++ ": expecting " ++ str
                    ++ " but got " ++ show es)
   app1 op | [e] <- es = op e
           | otherwise = err "one argument"
   app2 op | [e1,e2] <- es = op e1 e2
           | otherwise = err "two arguments"
   castFun Float = "float"
   castFun t = error ("ConCat.GLSL.app: fromIntegral on type " ++ show t)

knownFuncs :: M.Map String String
knownFuncs = M.fromList $
  [("ceiling","ceil")]
  ++ ((\ s -> (s,s)) <$> ["exp","cos","sin","floor"])

bToE :: Bus -> Expr
bToE = Variable . varName

-- Extract input, middle, output components. 
splitComps :: [Comp] -> (Comp,[Comp],Comp)
splitComps (i@(CompS _ "In" [] _)
            : (unsnoc -> (mid,o@(CompS _ "Out" _ [])))) = (i,mid,o)
splitComps comps = error ("ConCat.GLSL.splitComps: Oops: " ++ show comps)

unsnoc :: [a] -> ([a],a)
unsnoc as = (mid,o) where (mid,[o]) = splitAt (length as - 1) as

{--------------------------------------------------------------------
    GLSL syntax utilities
--------------------------------------------------------------------}

-- For experiments. Makes it easy to see syntax representations.
_parse :: P a -> String -> Either ParseError a
_parse p = runParser p S "GLSL"

initDecl :: TypeSpecifierNonArray -> String -> Expr -> Statement
initDecl ty var e =
 DeclarationStatement (
  InitDeclaration (
      TypeDeclarator (
          FullType Nothing (TypeSpec Nothing (TypeSpecNoPrecision ty Nothing))))
  [InitDecl var Nothing (Just e)])

paramDecl :: Bus -> ParameterDeclaration
paramDecl b =
  ParameterDeclaration Nothing Nothing 
    (TypeSpec Nothing (TypeSpecNoPrecision (busType b) Nothing))
    (Just (varName b,Nothing))

funDef :: TypeSpecifierNonArray -> String -> [ParameterDeclaration]
       -> [Statement] -> ExternalDeclaration
funDef resultTy name params statements =
  FunctionDefinition (
    FuncProt (FullType Nothing
              (TypeSpec Nothing (TypeSpecNoPrecision resultTy Nothing)))
             name params)
    (Compound statements)

funcall :: String -> [Expr] -> Expr
funcall fun args = FunctionCall (FuncId fun) (Params args)

-- funcall1 :: String -> Expr -> Expr
-- funcall1 fun = funcall fun . (:[])

#if 0
selectField :: String -> String -> Expr
selectField var field = FieldSelection (Variable var) field

assign :: String -> Expr -> Statement
assign v e = ExpressionStatement (Just (Equal (Variable v) e))
#endif
