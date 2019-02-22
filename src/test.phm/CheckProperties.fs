module CheckProperties

open System
open Persistent.Dnc30

let notIdentical<'T when 'T : not struct> (f : 'T) (s : 'T) =
  obj.ReferenceEquals (f, s) |> not

let notIdenticalArray (f : _ []) (s : _ []) =
  (f.Length = 0 && s.Length = 0) || notIdentical f s

let makeRandom (seed : int) =
  let mutable state = int64 seed
  let m = 0x7FFFFFFFL // 2^31 - 1
  let d = 1. / float m
  let a = 48271L      // MINSTD
  let c = 0L
  fun (b : int) (e : int) ->
    state <- (a*state + c) % m
    let r = float state * d
    let v = float (e - b)*r + float b |> int
    v

let shuffle random vs =
  let a = Array.copy vs
  for i in 0..(vs.Length - 2) do
    let s =  random i vs.Length
    let t =  a.[s]
    a.[s] <- a.[i]
    a.[i] <- t
  a

[<AllowNullLiteral>]
type Empty () =
  inherit obj ()

type ComplexType =
  | IntKey    of  int
  | StringKey of  int
  | TupleKey  of  int*string

type HalfHash(v : int) =
  member x.Value = v

  interface IComparable<HalfHash> with
    member x.CompareTo(o : HalfHash)  = v.CompareTo o.Value

  interface IEquatable<HalfHash> with
    member x.Equals(o : HalfHash)  = v = o.Value

  override x.Equals(o : obj)  =
    match o with
    | :? HalfHash as k -> v = k.Value
    | _                -> false
  override x.GetHashCode()    = (v.GetHashCode ()) >>> 16 // In order to get a fair bunch of duplicated hashes
  override x.ToString()       = sprintf "%d" v

type Action =
  | Add     of int*string
  | Remove  of int

let uniqueKey vs =
  vs
  |> FsLinq.groupBy fst
  |> FsLinq.map (fun g -> g.Key, (g |> FsLinq.map snd |> FsLinq.last))
  |> FsLinq.sortBy fst
  |> FsLinq.toArray

let fromArray kvs =
  Array.fold
    (fun s (k, v) -> PersistentHashMap.set k v s)
    PersistentHashMap.empty
    kvs

let toArray m phm =
  phm
  |> m
  |> FsLinq.map (fun (KeyValue (k,v)) -> k, v)
  |> FsLinq.toArray

let toSortedKeyArray m phm =
  let vs = phm |> (toArray m)
  vs |> Array.sortInPlaceBy fst
  vs

let checkInvariant (phm : PersistentHashMap<_, _>) = phm.CheckInvariant ()

open FsCheck

module FsCheckConfig =
#if DEBUG
  let testCount = 100
#else
  let testCount = 1000
#endif
  let config = { Config.Quick with MaxTest = testCount; MaxFail = testCount }

type Properties () =

(*
  TODO: Test these properties by exposing
  static member ``PopCount returns number of set bits`` (i : uint32) =
    let expected  = popCount i
    let actual    = PersistentHashMap.PopCount i

    expected      = actual

  static member ``CopyArray copies the array`` (vs : int []) =
    let expected  = vs
    let actual    = PersistentHashMap.CopyArray vs

    notIdentical expected actual
    && expected = actual

  static member ``CopyArrayMakeHoleLast copies the array and leaves a hole in last pos`` (vs : Empty []) (hole : Empty)=
    let expected  = Array.append vs [| hole |]
    let actual    = PersistentHashMap.CopyArrayMakeHoleLast (vs, hole)

    notIdentical expected actual
    && expected = actual

  static member ``CopyArrayMakeHole copies the array and leaves a hole at pos`` (at : int) (vs : Empty []) (hole : Empty)=
    let at        = abs at % (vs.Length + 1)
    let expected  = copyArrayMakeHole at vs hole
    let actual    = PersistentHashMap.CopyArrayMakeHole (at, vs, hole)

    notIdentical expected actual
    && expected = actual
*)

  static member ``PHM to* must contain all added values`` (vs : (int*string) []) =
    let expected    = uniqueKey vs
    let phm         = vs |> fromArray
    let actualSeq   = phm |> toSortedKeyArray (PersistentHashMap.toSeq)
    let actualArray = phm |> toSortedKeyArray (PersistentHashMap.toArray)

    notIdenticalArray    expected  actualSeq
    && notIdenticalArray expected  actualArray
    && notIdenticalArray actualSeq actualArray
    && checkInvariant phm
    && expected = actualSeq
    && expected = actualArray

  static member ``PHM TryFind must return all added values`` (vs : (ComplexType*ComplexType) []) =
    let unique    = uniqueKey vs
    let phm       = unique |> fromArray

    let rec loop i =
      if i < unique.Length then
        let k, v = unique.[i]
        match PersistentHashMap.tryFind k phm with
        | Some fv when fv = v -> loop (i + 1)
        | _                   -> false
      else
        true

    checkInvariant phm
    && loop 0

  static member ``PHM Unset on all added values must yield empty map`` (vs : (HalfHash*int) []) =
    let unique    = uniqueKey vs
    let phm       = unique |> fromArray

    let rec loop (phm : PersistentHashMap<_, _>) i =
      if checkInvariant phm |> not then
        None
      elif i < unique.Length then
        if phm |> PersistentHashMap.isEmpty then
          None
        else
          let k, v = unique.[i]
          loop (PersistentHashMap.unset k phm) (i + 1)
      else
        Some phm

    match loop phm 0 with
    | Some phm  -> PersistentHashMap.isEmpty phm
    | None      -> false

  static member ``PHM should behave as Map`` (vs : Action []) =
    let compare map (phm : PersistentHashMap<_, _>) =
      let empty =
        match map |> Map.isEmpty, phm |> PersistentHashMap.isEmpty with
        | true  , true
        | false , false -> true
        | _     , _     -> false

      let visitor k v =
        match map |> Map.tryFind k with
        | Some fv -> v = fv
        | _       -> false

      checkInvariant phm
      && (PersistentHashMap.length phm = map.Count)
      && empty
      && PersistentHashMap.visit visitor phm

    let ra = ResizeArray<int> ()

    let rec loop map (phm : PersistentHashMap<_, _>) i =
      if i < vs.Length then
        match vs.[i] with
        | Add (k, v)  ->
          ra.Add k
          let map = map |> Map.add k v
          let phm = PersistentHashMap.set k v phm
          compare map phm && loop map phm (i + 1)
        | Remove r    ->
          if ra.Count > 0 then
            let r   = abs r % ra.Count
            let k   = ra.[r]
            ra.RemoveAt r
            let map = map |> Map.remove k
            let phm = PersistentHashMap.unset k phm
            compare map phm && loop map phm (i + 1)
          else
            loop map phm (i + 1)
      else
        true

    loop Map.empty PersistentHashMap.empty 0

  static member ``PHM mapValues must contain all added and mapped values`` (vs : (int*int) []) =
    let expected    = uniqueKey vs |> Array.map (fun (k, v) -> k, int64 k + int64 v + 1L)
    let phm         = vs |> fromArray |> PersistentHashMap.mapValues (fun k v -> int64 k + int64 v + 1L)
    let actualArray = phm |> toSortedKeyArray (PersistentHashMap.toArray)

    notIdenticalArray expected actualArray
    && checkInvariant phm
    && expected = actualArray


let testLongInsert () =
#if DEBUG
  let count       = 1000
#else
  let count       = 1000000
#endif
  let multiplier  = 8
  printfn "testLongInsert: count:%d, multiplier:%d" count multiplier
  let random      = makeRandom 19740531
  let inserts     = [| for x in 1..count -> random 0 (count * multiplier) |]
  let lookups     = shuffle random inserts
  let removals    = shuffle random inserts

  let mutable phm = PersistentHashMap.empty

  for i in inserts do
    phm <- phm |> PersistentHashMap.set i i
    match phm |> PersistentHashMap.tryFind i with
    | Some v when v = i -> ()
    | _                 -> failwith "testLongInsert/insert/tryFind failed"

#if DEBUG
    if phm |> checkInvariant |> not then
      failwith "testLongInsert/insert/checkInvariant failed"
#endif

  if phm.IsEmpty then
    failwith "testLongInsert/postInsert/checkEmpty failed"

  if phm |> checkInvariant |> not then
    failwith "testLongInsert/postInsert/checkInvariant failed"

  for l in lookups do
    match phm |> PersistentHashMap.tryFind l with
    | Some v when v = l -> ()
    | _                 -> failwith "testLongInsert/lookup/tryFind failed"

  if phm |> checkInvariant |> not then
    failwith "testLongInsert/postLookup/checkInvariant failed"

  for r in removals do
    phm <- phm |> PersistentHashMap.unset r
    match phm |> PersistentHashMap.tryFind r with
    | None  -> ()
    | _     -> failwith "testLongInsert/remove/tryFind failed"

#if DEBUG
    if phm |> checkInvariant |> not then
      failwith "testLongInsert/remove/checkInvariant failed"
#endif

  if phm |> checkInvariant |> not then
    failwith "testLongInsert/postRemove/checkInvariant failed"

  if phm.IsEmpty |> not then
    failwith "testLongInsert/postRemove/checkEmpty failed"

  printfn "  Done"

let run () =
  Check.All<Properties> FsCheckConfig.config
  testLongInsert ()
