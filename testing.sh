for i in $(seq -f "%02g" 0 10)
do
    runghc Main.hs < tests/good$i.in > my_outs/$i.out &>/dev/null;
done

good=0
bad=0
all=0
for i in $(seq -f "%02g" 0 10)
do
    ((++all))
    if diff -q tests/good$i.ps my_outs/$i.out &>/dev/null; then
        echo -e "\e[0mTest\e[0m \e[1m$i\e[0m:  \e[1m\e[92mOK"
        ((++good))
    else
        echo -e "\e[0mTest\e[0m \e[1m$i\e[0m:  \e[1m\e[31mWRONG"
        ((++bad))
    fi
done

echo -e "\e[0m Passes: $good/$all"