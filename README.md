# abcd_adhd

This project uses the following ABCD instruments [version 3.0]:

1. pdem02
2. srpf01
3. abcd_sscep01
4. pmq01
5. abcd_ssmty01
6. abcd_ksad01
7. abcd_ksad501
8. medsy01
9. abcd_cbcls01


How to run the code:

1. update the config.R to reflect the location of the instruments above 
2. In the scripts folder, run the following scripts in any order:
    1) [suicide_clean.R](/scripts/suicide_clean.R)
    2) [ksad_externalizing_symptoms.R](/scripts/ksad_externalizing_symptoms.R)
    3) [demographics_clean.R](/scripts/demographics_clean.R)
    4) [medication_clean.R](/scripts/medication_clean.R)
    5) [exposome_clean.R](/scripts/exposome_clean.R)
    6) [psychopathology_clean.R](/scripts/psychopathology_clean.R)
    
    These scripts go over the abcd instruments and create new variables and datasets that are placed in the “outputs” folder.
    
3. Run the [merge_adhd.R](/scripts/merge_adhd.R) script. This script merges all the datasets that were created in 4 and adds some more adhd variables.  
