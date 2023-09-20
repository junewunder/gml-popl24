let f x =
  let f0 = future 0 in
  let f1 = future 42 in
  let a = 21 in
  let b = 21 in
  let c = a + b in
  let d = c + (force f1) in
  let f2 = future 21 in
  let f3 = future (let f4 = future 42 in
                   let e = 21 in
                   let f = 21 in
                   let g = a + b in
                   let h = c + (force f4) in
                   let i = d + (force f2) in
                   e)
  in
  let i1 = 3 + 5 in
  let i2 = force f3 in
  let f5 = future 25 in
  let j = 21 in
  let k = 21 in
  let l = a + b in
  let m = c + (force f5) in
  let f6 = future 22 in
  let n = (force f6) + (force f6) in
  n
;;
