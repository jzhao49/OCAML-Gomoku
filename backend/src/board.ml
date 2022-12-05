open Core

module Coordinates: sig
  type t = (int * int) [@@deriving compare, sexp]
end

