for i in $(seq -f "%02g" 0 10)
do
    runghc Main.hs < tests/good$i.in > my_outs/$i.out
done

for i in $(seq -f "%02g" 0 10)
do
    echo $i
    diff tests/good$i.ps my_outs/$i.out
done