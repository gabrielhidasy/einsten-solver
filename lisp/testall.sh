#!/usr/bin/zsh
for i in $(cat problems-lisp)
do
echo $i; ./run-expr.lisp ./solver tpman2 problemas/$(echo $i | cut -d . -f 1) rand | head -1
done

