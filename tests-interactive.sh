dune build --release

for file in paper-examples/*.ml; do
  clear
  echo running $file
  echo
  cat $file
  _build/default/src/main.exe $file
  echo press enter to continue
  read;
done

for file in testcases/*.ml; do
  clear
  echo running $file
  echo
  cat $file
  _build/default/src/main.exe $file
  echo press enter to continue
  read;
done