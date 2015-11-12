#!/usr/bin/zsh
echo "Testing quality"
rm out
echo -n "Solved: "
for i in $(cat problems-lisp)
do
./run-expr.lisp ./solver-compiled tpman_uf problemas/$(echo $i | cut -d . -f 1) rand | grep ok | cut -d k -f 2 >> out
done
cat out | wc -l
echo -n "of: "
cat problems-lisp | wc -l

echo "Testing speed:"

echo "Total time: "
paste -s -d+ out | bc -l
echo "Average: "
echo \($(paste -s -d+ out)\)/$(cat out | wc -l) | bc -l
