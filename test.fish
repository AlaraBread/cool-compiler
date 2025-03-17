#!/usr/bin/env fish

set dir "examples/pa3c3-subset"

for file in $dir/*.cl
    set base (basename --suffix '.cl' $file)

    echo $base

    ../cool --tac "$dir/$base.cl"
    ../cool "$dir/$base.cl-tac" > out-reference.txt

    rm "$dir/$base.cl-tac"

    ../cool --type "$dir/$base.cl"
    src/main "$dir/$base.cl-type"
    ../cool "$dir/$base.cl-tac" > out-ours.txt

    rm $dir/$base.cl-*

    diff out-reference.txt out-ours.txt
    rm out-reference.txt out-ours.txt
end

