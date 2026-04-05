open! Lunar
open Types
open Ortools.Sat

let make (_data : Planning.t) =
  let model = make ~name:"Toubenev" () in
  model
