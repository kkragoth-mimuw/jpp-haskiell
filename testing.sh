good=0
bad=0
all=0

# runghc TestTransform.hs

for i in $(seq -f "%02g" 0 10)
do
    ((++all))
    
    runghc Main.hs < tests/good$i.in > my_outs/$i.out &>/dev/null;
    
    if diff -q tests/good$i.ps my_outs/$i.out &>/dev/null; then
        echo -e "\e[0mTest\e[0m \e[1m$i\e[0m:  \e[1m\e[92m OK"
        ((++good))
    else
        echo -e "\e[0mTest\e[0m \e[1m$i\e[0m:  \e[1m\e[31mWRONG"
        ((++bad))
    fi
done

echo -e "\e[0mPasses: $good/$all"
echo -e ""