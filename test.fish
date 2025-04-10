#!/usr/bin/env fish

cd src
ghc Main.hs -prof -fprof-auto -outputdir ../build
cd ..

set dir examples/

mkdir -p test-out

for file in $dir/**.cl
    set test_file (path change-extension '' $file)
    set base (basename $test_file)

    echo $test_file

    cool --type "$test_file.cl"
    src/Main "$test_file.cl-type"
    gcc -no-pie -static "$test_file.s" 2&>/dev/null

    set input_files {$test_file}*.input
    if count $input_files >/dev/null
        for input_file in $input_files
            echo $input_file
            set input_base (basename --suffix '.input' $input_file)
            cat $input_file | cool "$test_file.cl" >test-out/"$base"-"$input_base"-reference.txt
            cat $input_file | ./a.out >test-out/"$base"-"$input_base"-ours.txt
            diff test-out/"$base"-"$input_base"-reference.txt test-out/"$base"-"$input_base"-ours.txt
        end
    else
        echo no input
        cool "$test_file.cl" >test-out/"$base"-reference.txt
        ./a.out >test-out/"$base"-ours.txt
        diff test-out/"$base"-reference.txt test-out/"$base"-ours.txt
    end

    rm "$test_file.cl-type"
    rm "$test_file.s"
    rm ./a.out

    echo ""
end
