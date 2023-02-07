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
    if [[ $(< test.txt) == $2 ]] && [[ $? == $3 ]]
    then
        echo -e "\E[0;34m \t\tTest result \E[0m ${4} \E[0;32m\e[1m\tPASSED\e\e[0m"
    else
        echo -e "\E[1;34m \t\tTest result \E[0;30m ${4} \E[0;31m\e[1m\tNOT PASSED\E[0m"
    fi
    rm -f test.txt
}

echo -e "\E[0;35m\e[1m \t\t\tBASIC SUBJECT TEST \e\e[0m"

echo -e "\E[0;36m \t\t\t  ~ Invocation and error hadling ~\e\e[0m"
test_result "test/scm_exemple/foo.scm" "42" 0 "\t-Foo define-\t\t\t\t\t\t\t"
test_return_type "test/scm_exemple/error.scm" 84 "\t-Foo not bound define-\t\t\t\t\t\t"

echo -e "\E[0;36m \t\t\t  ~ User defined functions and lambda ~\e\e[0m"
test_result "test/scm_exemple/call.scm" "5" 0 "\t-Simple call of hardcode function-\t\t\t\t"
test_return_type "test/scm_exemple/lambda1.scm" 0 "\t-Creation of lambda-\t\t\t\t\t\t"
test_result "test/scm_exemple/lambda2.scm" "3" 0 "\t-Creation of lambda and variable assignation in one step-\t"
test_result "test/scm_exemple/lambda3.scm" "7" 0 "\t-Creation of lambda and variable assignation in two steps-\t"
test_result "test/scm_exemple/function1.scm" "7" 0 "\t-Addition function-\t\t\t\t\t\t"

echo -e "\E[0;36m \t\t\t  ~ Conditional expressions ~\e\e[0m"
test_result "test/scm_exemple/if1.scm" "1" 0 "\t-If with true condition-\t\t\t\t\t"
test_result "test/scm_exemple/if2.scm" "2" 0 "\t-If with false condition-\t\t\t\t\t"
test_result "test/scm_exemple/if3.scm" "21" 0 "\t-Foo function with a condition-\t\t\t\t\t"

echo -e "\E[0;36m \t\t\t  ~ Builtin functions ~\e\e[0m"
test_result "test/scm_exemple/builtins1.scm" "11" 0 "\t-Builtins calcul-\t\t\t\t\t\t"
test_result "test/scm_exemple/builtins2.scm" "#t" 0 "\t-Builtins boolean true-\t\t\t\t\t\t"
test_result "test/scm_exemple/builtins3.scm" "#f" 0 "\t-Builtins boolean false-\t\t\t\t\t"

echo -e "\E[0;36m \t\t\t  ~ Other exemples ~\e\e[0m"
test_result "test/scm_exemple/superior.scm" "#t" 0 "\t-Superior function comparison-\t\t\t\t\t"
test_result "test/scm_exemple/factorial.scm" "3628800" 0 "\t-Factiorial function recursive-\t\t\t\t\t"