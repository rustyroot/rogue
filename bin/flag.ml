open Sys

let is_curse_darkness_on = Array.mem "--is-curse-of-darkness-on" Sys.argv || Array.mem "--darkness-on" Sys.argv
let level_activated = Array.mem "--level-activated" Sys.argv