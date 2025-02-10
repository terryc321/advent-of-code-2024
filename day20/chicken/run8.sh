#!/bin/bash

echo "rebuilding multi program in case source file changes"
rm -f multi

csc -o multi multi.scm


echo "launching 8 tasks to solve puzzle "
pids=()
./multi -g tasker1.dat -f ../input.txt > tasker1.out &
pids[1]=$!
./multi -g tasker2.dat -f ../input.txt > tasker2.out &
pids[2]=$!
./multi -g tasker3.dat -f ../input.txt > tasker3.out &
pids[3]=$!
./multi -g tasker4.dat -f ../input.txt > tasker4.out &
pids[4]=$!
./multi -g tasker5.dat -f ../input.txt > tasker5.out &
pids[5]=$!
./multi -g tasker6.dat -f ../input.txt > tasker6.out &
pids[6]=$!
./multi -g tasker7.dat -f ../input.txt > tasker7.out &
pids[7]=$!
./multi -g tasker8.dat -f ../input.txt > tasker8.out &
pids[8]=$!



# ./multi -g tasker9.dat -f ../input.txt > tasker9.out &
# ./multi -g tasker10.dat -f ../input.txt > tasker10.out &
# ./multi -g tasker11.dat -f ../input.txt > tasker11.out &
# ./multi -g tasker12.dat -f ../input.txt > tasker12.out &
# ./multi -g tasker13.dat -f ../input.txt > tasker13.out & 
# ./multi -g tasker14.dat -f ../input.txt > tasker14.out & 
# ./multi -g tasker15.dat -f ../input.txt > tasker15.out &
# ./multi -g tasker16.dat -f ../input.txt > tasker16.out & 

# wait for all pids
for pid in ${pids[*]}; do
    wait $pid    
done



