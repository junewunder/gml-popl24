dune build

echo This script runs all files in paper-examples and testcases with output sent to /dev/null

for file in paper-examples/*.ml; do
  printf "running $file..."
  _build/default/src/main.exe $file &> $file.out
  if [ $? -eq 0 ]; then
    echo " success"
  else
    echo " failure"
  fi
done

for file in testcases/*.ml; do
  printf "running $file..."
  _build/default/src/main.exe $file &> $file.out
  if [ $? -eq 0 ]; then
    echo " success"
  else
    echo " failure"
  fi
done
