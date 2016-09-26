# run this script from the src folder
# your pwd should say something like: `wicer-entice3-grader/src`
# to run this script type:
# bash stack.sh

# the output will be in the `output/all_ck_bash.csv file`

echo '"patient_id","X30Anx_CK","X30Comp_CK","X30Dep_CK","Anx_CK","BevRec_CK","BevWk_CK","BMI_CK","Clover_CK","DepA_CK","DepB_CK","Fruit_CK","ModPA_CK","OvHea_CK","Panel_CK","RecPA_CK","RiskBMI_CK","RiskBP_CK1","RiskBP_CK2","RunD_CK","Stress_CK","Veg_CK","VigPA_CK","Waist_CK"'  > ../output/all_ck_bash.tmp

touch all_ck_bash_temp.tmp

for FILE in ../output/*.csv
do
    echo $FILE
    tail -n 1 $FILE >> ../output/all_ck_bash_temp.tmp
done

cat ../output/all_ck_bash.tmp > ../output/all_ck_bash.csv
cat ../output/all_ck_bash_temp.tmp >> ../output/all_ck_bash.csv

rm ../output/*.tmp
