#!/usr/bin/env bash

test_return_type()
{
    ./glados $1 &> test.txt
    if [[ $? == $2 ]]
    then
        echo -e "\E[0;34m \t\tTest return \E[0m ${3} \E[0;32m\e[1m\tPASSED\e\e[0m"
    else
        echo -e "\E[1;34m \t\tTest return \E[0;30m ${3} \E[0;31m\e[1m\tNOT PASSED\E[0m"
    fi
    rm -f test.txt
}

test_result()
{
    ./glados $1 &> test.txt
    if cmp -s "test.txt" "$2";
    then
        echo -e "\E[0;34m \t\tTest result \E[0m ${4} \E[0;32m\e[1m\tPASSED\e\e[0m"
    else
        echo -e "\E[1;34m \t\tTest result \E[0;30m ${4} \E[0;31m\e[1m\tNOT PASSED\E[0m"
    fi
    rm -f test.txt
}

echo -e "\E[0;35m\e[1m \t\t\tRETURN BASIC TESTS\e\e[0m"

echo -e "\E[0;36m \t\t\t  ~ Invocation and error hadling ~\e\e[0m"
test_return_type "test/yuyu_exemple/foo.yuyu" 0 "\t-Foo define-\t\t\t\t\t\t\t"
test_return_type "test/yuyu_exemple/error.yuyu" 0 "\t-Foo not bound define-\t\t\t\t\t\t"

echo -e "\E[0;36m \t\t\t  ~ User defined functions and lambda ~\e\e[0m"
test_return_type "test/yuyu_exemple/call.yuyu" 0 "\t-Simple division-\t\t\t\t\t\t"
test_return_type "test/yuyu_exemple/lambda1.yuyu" 0 "\t-Creation of lambda-\t\t\t\t\t\t"
test_return_type "test/yuyu_exemple/lambda2.yuyu" 0 "\t-Creation of lambda and variable assignation in one step-\t"
test_return_type "test/yuyu_exemple/function1.yuyu" 0 "\t-Addition function-\t\t\t\t\t\t"

echo -e "\E[0;36m \t\t\t  ~ Conditional expressions ~\e\e[0m"
test_return_type "test/yuyu_exemple/if1.yuyu" 0 "\t-If with true condition-\t\t\t\t\t"
test_return_type "test/yuyu_exemple/if2.yuyu" 0 "\t-If with false condition-\t\t\t\t\t"
test_return_type "test/yuyu_exemple/if3.yuyu" 0 "\t-Foo function with a condition-\t\t\t\t\t"

echo -e "\E[0;36m \t\t\t  ~ Builtin functions ~\e\e[0m"
test_return_type "test/yuyu_exemple/builtins1.yuyu" 0 "\t-Builtins calcul-\t\t\t\t\t\t"
test_return_type "test/yuyu_exemple/builtins2.yuyu" 0 "\t-Builtins boolean true-\t\t\t\t\t\t"
test_return_type "test/yuyu_exemple/builtins3.yuyu" 84 "\t-Builtins boolean false-\t\t\t\t\t"

echo -e "\E[0;36m \t\t\t  ~ Other exemples ~\e\e[0m"
test_return_type "test/yuyu_exemple/superior.yuyu" 0 "\t-Superior function comparison-\t\t\t\t\t"
test_return_type "test/yuyu_exemple/factorial.yuyu" 0 "\t-Factiorial function recursive-\t\t\t\t\t"

echo -e "\n\E[0;35m\e[1m \t\t\tRETURN TESTS\e\e[0m"
echo -e "\E[0;36m \t\t\t  ~ Basic function ~\e\e[0m"
test_return_type "test/yuyu_exemple/defense1.yuyu" 0 "\t-Define lambda incrementation-\t\t\t\t\t"
test_return_type "test/yuyu_exemple/defense2.yuyu" 0 "\t-Define lambda incrementation with variable-\t\t\t"
test_return_type "test/yuyu_exemple/defense3.yuyu" 84 "\t-Define lambda incrementation with var-\t\t\t\t"
test_return_type "test/yuyu_exemple/defense4.yuyu" 0 "\t-Define lambda incrementation with cond-\t\t\t"
test_return_type "test/yuyu_exemple/defense5.yuyu" 0 "\t-Define lambda incrementation with variable and condition-\t"


echo -e "\n\E[0;35m\e[1m \t\t\tFUNCTIONAL TESTS\e\e[0m"
echo -e "\E[0;36m \t\t\t  ~ Basic call ~\e\e[0m"
test_result "test/yuyu_exemple/func1.yuyu" "test/yuyu_solution/func1.txt" 0 "\t-Call of an addition with two arguments-\t\t\t"
test_result "test/yuyu_exemple/func2.yuyu" "test/yuyu_solution/func2.txt" 0 "\t-Addition without creation of function-\t\t\t\t"
test_result "test/yuyu_exemple/func3.yuyu" "test/yuyu_solution/func3.txt" 0 "\t-Call of an addition without arguments-\t\t\t\t"
test_result "test/yuyu_exemple/func4.yuyu" "test/yuyu_solution/func4.txt" 0 "\t-Call of an addition without arguments but float and int-\t"
test_result "test/yuyu_exemple/func5.yuyu" "test/yuyu_solution/func5.txt" 0 "\t-Call switch-\t\t"
test_result "test/yuyu_exemple/func6.yuyu" "test/yuyu_solution/func6.txt" 0 "\t-Call of an addition with one argument-\t"