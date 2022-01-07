clear all

global bd "C:\Users\Computer\Downloads\Data Task\"
global results "C:\Users\Computer\Downloads\Data Task\results"


******************************************************
******************Section 1 - DATA CLEANING **********
******************************************************

*1.Import the data


import excel "$bd\Data for Analysis Test.xlsx", sheet("Sheet1") firstrow cellrange(A1:J6971) clear
save "$results\Data for Analysis Test.dta",replace
clear all

import excel "$bd\Town Names for Analysis Test.xlsx", sheet("Sheet1") firstrow
save "$results\Town Names for Analysis Test.dta",replace
rename TownID town_id
save "$results\Town Names for Analysis Test.dta",replace
clear all

*2. Merge the data****
use "$results\Data for Analysis Test.dta",clear
merge m:1 town_id using "$results\Town Names for Analysis Test.dta"

*drop the towns that not merge with the main database
drop if _merge==2

*3. Creating a district variable such that it is numerical
encode district, gen(district_num)

*4. Creating a unique ID for each observation
bys town_id: gen count=_n
egen id_obs=concat(town_id count)
destring id_obs,replace

*5. Checking variables with missing data
misstable summarize /* Apparently there are not missing data*/
*I also checked values that contain "-999" or "-998" that indicated missing data
br if turnout_total==-999| turnout_total==-998
br if turnout_male==-999| turnout_male==-999
br if turnout_female==-999 | turnout_female==-998
* I detected 22 missing values in the next variables: registered_total,registered_male,registered_female
br if registered_total==-999|registered_total==-998
br if registered_male==-999|registered_male==-998
br if registered_female==-999 |registered_female==-998
*Due to the fact that the variables are numerical and the reduced time to carry out the adequate treatment of the missing values, it is decided to carry out an imputation by the mean for the missing values. It should be noted that there are other more sophisticated techniques for data imputation such as MCAR, MAR, among others.
replace registered_total=. if registered_total==-999|registered_total==-998
replace registered_male=. if registered_male==-999|registered_male==-998
replace registered_female=. if registered_female==-999|registered_female==-998

mean(registered_total) /* mean= 980.0558*/

mean(registered_male) /* mean= 536.7776*/

mean(registered_female) /*  mean= 443.2782*/

replace registered_total=980.0558 if registered_total==.
replace registered_male=536.7776 if registered_male==.
replace registered_female=443.2782 if registered_female==.


*6. Creating a dummy variable for each value of Town ID.
levelsof town_id, local(levels)
foreach l of local levels {
    gen dummy_town_`l'=0
    replace dummy_town_`l'=1 if town_id==`l'
}

*8. Labeling values for the treatment variable appropriately.
label define treatment_ 1 "Treatment" 0 "Control"
label value treatment treatment_



***************************************************************
******************Section 2 - DESCRIPTIVE STATISTICS **********
***************************************************************
/*
9 What is the average total turnout rate? Also note down the highest and lowest
turnout rates recorded. How many polling booths recorded the highest turnout
rate?
*/
*Creating the variable total turnout rate
gen total_turnout_rate=(turnout_total/registered_total)*100

*the average total turnout rate is 57.3%. The highest turnover is 100% and the lowest is 0%.  
summarize total_turnout_rate
*Just 20 polling booth recorded the highest turnout
tab total_turnout_rate if total_turnout_rate==100

/*
10. By treatment, tabulate the number of booths in phases 1 and 2 of the study
*/
*Phase 1
bys treatment:tab town_id if treatment_phase==1
*Phase 2
bys treatment:tab town_id if treatment_phase==2


/*
11. Tabulate the average turnout rate for females for each district which has a total
turnout rate of 75% or above.
*/
gen total_turnout_female_rate=(turnout_female/registered_female)*100

bys district_num: summarize total_turnout_female_rate if total_turnout_rate>=75


/*
12. Is the average turnout rate for females notably higher in treatment polling booths
than control? Can you say the difference is significant? How would you test for it?
*/

*average turnout rate for females in the treatment group
summ total_turnout_female_rate if treatment==1

*average turnout rate for females in the no treatment group
summ total_turnout_female_rate if treatment==0

*To find out if there is a significant difference in the means of both groups, the t-test of means is performed.
 ttest total_turnout_female_rate, by(treatment)
*The results for the test of means show that the means of both groups are different with a significance level of 95%.

/*
13. Create one simple, clearly-labeled bar graph that shows the difference in turnout
between treatment and control polling booths by gender as well as total turnout.
Please output your results in the clearest form possible
*/

graph bar (mean) turnout_total turnout_female turnout_male , over(treatment, label(labsize(small)))  blabel(bar, size(vsmall))  ylabel(0(100)500, angle(horizontal) labsize(small)) legend(position(6) label(1 "Average total turnout") label(2 "Average female turnout") label(3 "Average male turnout")) title("Average total turnout by gender and groups of treatment", size(small) color(black))
graph export "$results\average_turnout.png",replace


save "$results\final_dataset.dta",replace

***************************************************************
******************Section 3 - REGRESSION **********************
***************************************************************
use "$results\final_dataset.dta"
/*
14. Please output your results in Excel/Word in the clearest form possible. It is not
necessary to show the coefficients on the control variables. However, do show the
coefficient on registered voters.
*/

reg turnout_total treatment i.dummy_town_* registered_total, robust
outreg2 using "$results\rct_regression.doc"

/*
15. What is the mean turnout for the control group?
*/
mean turnout_total if treatment==0

/*
16. Note down the dependent variable.
*/
*The dependent variable is total turnout (turnout_total)

/*
17. What is the change in the dependent variable after the intervention?
*/

*After the intervention, the total turnout increases by 8.1 with respect to the control group.

/*18. Is the difference in turnout between the treatment and control booths statistically
significant? Explain in no more than 50 words how you would assess that.
*/

* To find out if there is a statistically difference between participation between the treatment and control groups, I would perform a mean test of the dependent variable in both groups.


***************************************************************************
******************Section 4 - INSTRUMENTAL VARIABLES **********************
****************************************************************************

/*
19. Is there a variable in this dataset that is plausibly an instrumental variable for the
presence of the voter turnout campaign?
*/

*Yes, there is a variable in the dataset. We can approximate the presence of the electoral participation campaign through the registered total of voters .

/*
20. Please state the relevance condition of instrumental variables and discuss/show
why it would hold or doesn’t hold in this case.
*/

/*Because some of the total number of registered people can decide to vote or not, there is a self-selection problem that generates biases in the estimation of treatment, so the variable of treatment should be considered as an endogenous variable that depends on other variables.
To do this, an instrumental variable must be found that is correlated with the treatment variable and that is not correlated with the error.*/


/*
21. Please state the exogeneity condition for instrumental variables and provide
evidence on whether it holds. Hint: the best variable in the data set to use for testing
the exogeneity condition is registered_total, so you can just use that one.
*/

ivregress 2sls turnout_total  (treatment=registered_total) , robust first
estat endogenous

*The tests show that the candidate variable comply the exogeneity condition and the endogeneity of the treatment variable is statistically significant

/*
22. Please run the instrumental variables regression showing the effect of take_up on
turnout using an instrumental variables approach and discuss the magnitude of this
effect relative to the effect you found previously in question 18.
*/

ivregress 2sls turnout_total  (treatment=registered_total ) i.dummy_town_* take_up, robust first
*The effect of the intervention increases considerably and changes the sign, in addition the result becomes non-significant. Compared to the magnitude estimated in the regression without considering the endogeneity of the treatment, this last result is much higher.

