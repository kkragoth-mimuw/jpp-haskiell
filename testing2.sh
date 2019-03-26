good=0
bad=0
all=0

# runghc TestTransform.hs

for i in $(seq  0 69)
do
    ((++all))
    
    runghc Main.hs < jpp1-testy/radekw_tests/good$i.in > my_outs/$i.out &>/dev/null;
    
    if diff -q jpp1-testy/radekw_tests/good$i.ps my_outs/$i.out &>/dev/null; then
        echo -e "\e[0mTest\e[0m \e[1m$i\e[0m:  \e[1m\e[92m OK"
        ((++good))
    else
        echo -e "\e[0mTest\e[0m \e[1m$i\e[0m:  \e[1m\e[31mWRONG"
        ((++bad))
    fi
done

echo -e "\e[0mPasses: $good/$all"
echo -e ""
