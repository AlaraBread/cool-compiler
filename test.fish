#!/usr/bin/env fish

cd src
ghc Main.hs -prof -fprof-auto -outputdir ../build
cd ..

set dir examples/basic

mkdir -p test-out

for file in $dir/**/*.cl
    set base (basename --suffix '.cl' $file)

    echo $base

    cool --type "$dir/$base.cl"
    src/Main "$dir/$base.cl-type"
    gcc -no-pie -static "$dir/$base.s" 2&>/dev/null

    set input_files {$dir}/{$base}*.input
    if count $input_files >/dev/null
        for input_file in $input_files
            echo $input_file
            set input_base (basename --suffix '.input' $input_file)
            cat $input_file | cool "$dir/$base.cl" >test-out/"$base"-"$input_base"-reference.txt
            cat $input_file | ./a.out >test-out/"$base"-"$input_base"-ours.txt
            diff test-out/"$base"-"$input_base"-reference.txt test-out/"$base"-"$input_base"-ours.txt
        end
    else
        echo no input
        cool "$dir/$base.cl" >test-out/"$base"-reference.txt
        ./a.out >test-out/"$base"-ours.txt
        diff test-out/"$base"-reference.txt test-out/"$base"-ours.txt
    end

    rm "$dir/$base.cl-type"
    rm "$dir/$base.s"
    rm ./a.out

    echo ""
end
