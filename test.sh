# Minic compiling testing

./minic -batch tests/syntax/bad/*.c -parse-only|\
  egrep "Exit (0|2)"
./minic -batch -parse-only tests/syntax/good/*.c|\
  egrep "Exit (1|2)"
./minic -batch -type-only tests/typing/bad/*.c|\
  egrep "Exit (0|2)"
./minic -batch -type-only tests/typing/good/*.c|\
  egrep "Exit (1|2)"
./minic -batch tests/exec*/*.c |\
  egrep "Exit (1|2)"

exit 0
