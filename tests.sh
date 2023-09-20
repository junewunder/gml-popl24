dune build --release
cp _build/default/src/main.exe ./gml

for file in testcases/*; do
  clear
  echo running $file
  echo
  dune exec -- gml $file
  read;
done