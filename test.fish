#!/usr/bin/env fish

set dir examples/pa3c3-subset

mkdir -p test-out

set n 100

for file in $dir/*.cl
    set base (basename --suffix '.cl' $file)

    echo $base

    echo $n | cool "$dir/$base.cl" >test-out/"$base"-reference.txt

    cool --type "$dir/$base.cl"
    src/main "$dir/$base.cl-type"
    gcc -no-pie -static "$dir/$base.s" 2&>/dev/null
    echo $n | ./a.out >test-out/"$base"-ours.txt

    rm "$dir/$base.cl-type"
    rm "$dir/$base.s"
    rm ./a.out

    diff test-out/"$base"-reference.txt test-out/"$base"-ours.txt
    echo ""
end
