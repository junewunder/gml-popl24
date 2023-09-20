let f0 = futref;;
let f1 = futref;;
let f2 = futref;;
let f3 = futref;;
let f4 = futref;;
let f5 = futref;;
let f6 = futref;;
let f7 = futref;;

let f x =
  future (
      future (
          future (f0 := future (1));
          f1 := future (1));
      future (f2 := future (force !f0 + force !f1));
      f3 := future (force !f1 + force !f2));
  future (
      future (f4 := future (force !f2 + force !f3));
      f5 := future (force !f3 + force !f4));
  future (f6 := future (force !f4 + force !f5));
  (f7 := future (force !f5 + force !f7));
  force !f7
;;
