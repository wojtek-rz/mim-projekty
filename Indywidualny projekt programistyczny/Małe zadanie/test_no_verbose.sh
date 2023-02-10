#!/bin/bash
if (($# < 2)); then
  echo "Podaj dwa argumenty: nazwa_programu katalog_z_testami"
  exit 1
fi
test_folder=$2
program=$1
GREEN="\e[32m"
RED="\e[31m"
NO_COLOR="\e[0m"

goods=0
wrongs=0

for f in "$test_folder"/*.in; do
  ./"$program" <"$f" 1>tmp.out 2>tmp.err
  diff "${f%in}out" "tmp.out" >/dev/null 2>&1
  c1=$?
  diff "${f%in}err" "tmp.err" >/dev/null 2>&1
  c2=$?
  if ((c1 == 0 && c2 == 0)); then
#    echo -e "${GREEN} Test '${f}' passed.${NO_COLOR}"
    goods=$((goods + 1))
  else
#    echo -e "${RED} Test '${f}' failed.${NO_COLOR}"
    wrongs=$((wrongs + 1))
  fi
  rm tmp.out tmp.err
  #  valgrind --error-exitcode=123 --leak-check=full --show-leak-kinds=all --errors-for-leak-kinds=all "./$program" < "$f" > /dev/null 2>&1
  #  if [[ $? == 123 ]]; then echo -e "  ${RED} Test '${f}' valgrind error. ${NO_COLOR}"; fi
done
echo -e " ${goods}/$((goods + wrongs)) tests passed."
echo -e " ${wrongs}/$((goods + wrongs)) tests failed."
if (($wrongs == 0)); then
	echo -e "${GREEN} FOLDER PASSED ${NO_COLOR}"
else
	echo -e "${RED} FOLDER FAILED ${NO_COLOR}"
fi
