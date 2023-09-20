open Ast

module A = Array
   
type 'a matrix = 'a A.array A.array

let make_mat n v = A.make n (A.make n v)
               
let get m (i, j) = m.(i).(j)

let set m (i, j) v = m.(i).(j) <- v
                                    

                   
