#!/usr/bin/env fish

cd src
ghc Main.hs -prof -fprof-auto -outputdir ../build -o ../build/Main
cd ..

set dir examples/

mkdir -p test-out

echo "" >failed_tests.txt

set total_count 0
set failed_count 0

for file in $dir/**.cl
    set test_file (echo $file | sed 's/\.cl$//')
    set base (basename $test_file)

    echo $test_file

    cool --type "$test_file.cl"
    build/Main "$test_file.cl-type"
    gcc -no-pie -static "$test_file.s" 2&>/dev/null

    set input_files {$test_file}.*.input
    if count $input_files >/dev/null
        for input_file in $input_files
            echo $input_file
            set input_base (basename --suffix '.input' $input_file)
            cat "$input_file" | cool "$test_file.cl" >test-out/"$base"-"$input_base"-reference.txt
            cat "$input_file" | ./a.out >test-out/"$base"-"$input_base"-ours.txt
            if not diff test-out/"$base"-"$input_base"-reference.txt test-out/"$base"-"$input_base"-ours.txt
                set failed_count (math $failed_count + 1)
                echo "$test_file" "$input_base" >>failed_tests.txt
            end
            set total_count (math $total_count + 1)
        end
    else
        echo no input
        cool "$test_file.cl" >test-out/"$base"-reference.txt
        ./a.out >test-out/"$base"-ours.txt
        if not diff test-out/"$base"-reference.txt test-out/"$base"-ours.txt
            set failed_count (math $failed_count + 1)
            echo "$test_file" >>failed_tests.txt
        end
        set total_count (math $total_count + 1)
    end

    rm "$test_file.cl-type"
    rm "$test_file.s"
    rm ./a.out

    echo ""
end

echo ran $total_count tests
echo failed $failed_count tests
cat failed_tests.txt | sed 's/^/    /'
