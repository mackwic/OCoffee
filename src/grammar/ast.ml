open Tokens

module rec Value : Map.OrderedType = struct

  type objectKey = string
  and value_t
  and array_t
  and object_t
  and _ primitiveValue =
  | Undefined : unit primitiveValue
  | Bool : bool -> bool primitiveValue
  | Int : int -> int primitiveValue
  | Float : float -> float primitiveValue
  | String : string -> string primitiveValue
  and anyPrimitiveValue =
  | AUndefined : unit primitiveValue -> anyPrimitiveValue
  | ABool : bool primitiveValue -> anyPrimitiveValue
  | AInt : int primitiveValue -> anyPrimitiveValue
  | AFloat : float primitiveValue -> anyPrimitiveValue
  | AString : string primitiveValue -> anyPrimitiveValue
  and arrayValue =  anyPrimitiveValue list
  and _ value =
  | Value : 'a primitiveValue -> 'a value
  | Array : arrayValue -> array_t value
  | Object : anyValue Object.t -> object_t value
  and anyValue =
  | AValue : value_t value -> anyValue
  | AArray : array_t value -> anyValue
  | AObject : object_t value -> anyValue

  type t = anyValue

  let comparePrimitives : type a. a primitiveValue -> a primitiveValue -> _ =
  fun v1 -> fun v2 -> match v1, v2 with
  | Undefined,Undefined -> 0
  | Bool(b1), Bool(b2) -> Pervasives.compare b1 b2
  | Int(i1), Int(i2) -> Pervasives.compare i1 i2
  | Float(f1), Float(f2) -> Pervasives.compare f1 f2
  | String(s1), String(s2) -> Pervasives.compare s1 s2

  let compare : anyValue -> anyValue -> _ =
  fun k1 -> fun k2 -> match k1, k2 with
  | AValue(Value(v1)), AValue(Value(v2)) -> comparePrimitives v1 v2
  | _ -> 1
end

and Object : Map.S = Map.Make(Value)

module Decl = struct
  type varName = string
  type argList = varName list
end

module Ops = struct
  type unary
  and binary
end

module Assign = struct
  type simple
  and assignOp
end

(*
 *TODO:
   - classes
   - excepyions
   - instantiacions
   - array slices
   - for loops
   - regexes
   - comments
 *)
type untypedAst =
  | UABlock of untypedAst list
  | UAPrimitiveExpression of Value.t
  | UADeclVar of Decl.varName * untypedAst
  | UADeclFunc of Decl.varName * Decl.argList * untypedAst
  | UAUnaryOperation of Ops.unary * untypedAst
  | UABinaryOperation of Ops.binary * untypedAst * untypedAst
  | UAAssignment of Assign.simple * Decl.varName * untypedAst
  | UAAssingOp of Assign.assignOp * Decl.varName * untypedAst
  (* condition * thenBody * elseBody *)
  | UAConditional of untypedAst * untypedAst * untypedAst
  (* condition * body *)
  | UALoop of untypedAst * untypedAst
  | UASwitch of Decl.varName * (Value.t * untypedAst) list
  (* function name * arguments *)
  | UAFunctionCall of Decl.varName * Decl.argList
  (* constructor name * arguments *)
  | UAInstanciationExpr of Decl.varName * Decl.argList
  (* any expression. Throw is not an expression *)
  | UAThrowExpr of untypedAst
  (* try block / catch block
   * FIXME: support catch list *)
  (*| UATryCatchBlock of ast * ast option *)
