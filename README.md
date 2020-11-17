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

(@) update the config.R to reflect the location of the instruments above 
(@) In the scripts folder, run the following scripts in any order:

    a. suicide_clean.R
    b. ksad_externalizing_symptoms.R
    c. demographics_clean.R
    d. medication_clean.R
    e. exposome_clean
    f. psychopathology_clean.R
    
    These scripts go over the abcd instruments and create new variables and datasets that are placed in the “outputs” folder.

(@) Run merge_adhd.R script. This script merges all the datasets that were created in 4 and adds some more adhd variables.  
