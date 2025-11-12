open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open Entity
open World

let rec egg (egg_instance : entity) : unit =

  perform End_of_turn;
  egg egg_instance