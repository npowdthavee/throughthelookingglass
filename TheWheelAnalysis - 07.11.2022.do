cap cd "/Users/julianewiese/Dropbox/The wheel experiment/Data"
cap cd "/Users/nattavudhpowdthavee/Dropbox/The Wheel Experiment/Data"

use "completed_participants_clean.dta", clear

egen pid = group(prolific_id)
sort pid wheel_round
by pid: egen count_hotseat = count(hotseat) if hotseat==1
by pid: egen late_hotseat = mean(hotseat) if wheel_round>=4 & wheel_round<=6
by pid: egen time = max(sequence_no)
by pid: gen hot_correct = correct if hotseat==1
by pid: gen nhot_correct = correct if hotseat==0
by pid: gen hot_pcorrect = partner_correct if partner_hotseat==1
by pid: gen nhot_pcorrect = partner_correct if partner_hotseat==0
*Collapse by group and pid

forvalues x = 1/8 {
	rename wheel1playerdeserving`x' deserving`x'
	}

collapse (mean) treat wheel_winner group partner_correct /*
*/ hotseat partner_hotseat correct count_hotseat hot_* nhot* late time pg* wheel1player* deserving* altru* age gender income education employment married state race cheat* completed_game total_hotseat_correct partner_total_correct_hotseat total_nonhotseat_correct, by(pid)

//Keep only completed participants
keep if completed==1

//Drop 7 pairs of participants where winner/loser identification went wrong
duplicates tag group_id wheel_winner, gen(dup)

drop if dup==1
drop dup

//white dummy labels
	gen white = 0
	replace white = 1 if race==8

//factor altruism

		label var altru1 "I would give directions to someone I did not know."
		label var altru2 "I would make changes for someone I did not know."
		label var altru3 "I would give money to a charity."
		label var altru4 "I would donate clothes or goods to a charity."
		label var altru5 "I would help carry belongings of someone I did not know."
		label var altru6 "I would delay an elevator and hold the door for someone I did not know."
		label var altru7 "I would allow someone I did not know to go in front of me in line."
		label var altru8 "I would point out a clerk's error in undercharging me for an item."
		label var altru9 "I would let a neighbor I did not know well borrow an item of value to me."
		label var altru10 "I would help a classmate who I did not know well with a homework assignment when my knowledge was greater than his or hers."
		label var altru11 "I would voluntarily look after a neighbor's pet or children without being paid."
		label var altru12 "I would offer to help a handicapped or elderly person across the street."
		label var altru13 "I would offer my seat on a train or bus to someone who was standing."
		label var altru14 "I would help an acquaintance move houses."
		
			forvalues x = 1/14 {
			label val altru`x' oftlab
			}

//altruism scale is meant to be summed together -- a higher score=higher level of altruism
egen altruism_total = rowtotal(altru*), missing


**gen difference between real distribution and expected 

sort group_id
by group: gen loser_redist = wheel1playerredistribution if wheel_winner==0
by group: egen mloser_redist = max(loser_redist)

by group: gen winner_redist = wheel1playerredistribution if wheel_winner==1
by group: egen mwinner_redist = max(winner_redist)

//JW addition: absolute value of the difference between what the winner and loser of each round put in
gen diff_redistribution_jw = mwinner_redist-mloser_redist
replace diff_redistribution_jw = diff_redistribution_jw*-1 if diff_redistribution_jw <0

gen age_sq = age^2

lab val gender genlab
lab val income inclab
lab val education educlab
lab val employment employlab
lab val married married
lab val race race
lab val state state
lab val cheat_you cheatlab
lab val cheat_other cheatlab

forvalues x = 1/8 {
	label val deserving`x' deservinglab
	}

//partner is luckier/more successful than you
gen partner_luckier = 1 if partner_correct >= correct
replace partner_luckier = 0 if partner_correct<correct


replace cheat_you = 0 if cheat_you==. & completed_game==1
replace cheat_other = 0 if cheat_other==. & completed_game==1

replace cheat_you = 1 if cheat_you>1 & cheat_you<99
replace cheat_other = 1 if cheat_other>1 & cheat_other<99

*Simplify control variables
gen edu_college = 0 if education~=. & completed_game==1
replace edu_college = 1 if education>=5 & education<=7

gen high_income = 0 if income~=.
replace high_income = 1 if income>=6 & income~=.


**Deservingness

label var deserving1 "I am completely deserving of winning [losing]"
label var deserving2 "If the other player had won [lost], he/she would be completely deserving of winning [losing]"
label var deserving3 "My opponent was skilled in the quiz [prediction] task"
label var deserving4 "I was skilled in the quiz [prediction] task"
label var deserving5 "My opponent put in his/her maixmum effort in the skills [prediction] task"
label var deserving6 "I put in my maximum effort in the skills [prediction] task"
label var deserving7 "My winning [losing] was due entirely to chance."
label var deserving8 "My winning [losing] was due partially to chance"

//NP addition: Factor analysis on deservingness
factor deserving*

rotate

predict p1 p2 p3 p4

lab var p1 "My opponents was skilled and put maximum efforts"
lab var p2 "My winning was due to chance"
lab var p3 "I or the other player completely deserves the win"
lab var p4 "I put in maximum efforts/very skilled"

**Standardise factorial variables
egen std_p1 = std(p1)
egen std_p2 = std(p2)
egen std_p3 = std(p3)
egen std_p4 = std(p4)

drop p1 p2 p3 p4
ren std_p1 p1
ren std_p2 p2
ren std_p3 p3
ren std_p4 p4

**Generate egalitarian, i.e., split 50-50
gen egalitarian = 0 
replace egalitarian = 1 if wheel1playerredistribution==50

**Selfish, i.e., redistributed zero
gen zero_redist = 0
replace zero_redist = 1 if wheel1playerredistribution==0

*Recode treatment
gen treat2 = 1 if treat==2
replace treat2 = 2 if treat==1
replace treat2 = 3 if treat==4
replace treat2 = 4 if treat==3
replace treat2 = 5 if treat==5

lab define treatment 1 "Luck, not transparent" 2 "Luck, transparent" 3 "Effort, not transparent" 4 "Effort, transparent" 5 "Effort, transparent, redist war", modify
lab value treat2 treatment

drop treat
ren treat2 treat

lab var wheel1playerredistribution "Winner's redistribution after task 1"
lab var treat "Treatments"
lab var correct "Percentage correct"

label define treatlabnew 1 "Luck, not transp." 2 "Luck, transp." 3 "Ability, not transp." 4 "Ability, transp." 5 "Ability, transp., redistribution"
label val treat treatlabnew
x
**Analysis

**Figure 1
*Histogram of redistribution
hist wheel1playerredistribution if wheel_winner==1, bin(20) by(treat, graphregion(color(white)) note("N=993"))
graph export "Graphs/Fig1_histogram.png", as(png) name("Graph") replace

	ksmirnov wheel1playerredistribution if treat==1 | treat==3, by(treat)
	ksmirnov wheel1playerredistribution if treat==2 | treat==4, by(treat)
	
hist correct if treat>2, bin(20) by(treat, graphregion(color(white)) note("N=1,986"))
graph export "Graphs/Fig1b_histogram.png", as(png) name("Graph") replace

	ksmirnov correct if treat==3 | treat==4, by(treat)
	ksmirnov correct if treat==3 | treat==5, by(treat)
	ksmirnov correct if treat==4 | treat==5, by(treat)


**Figure 2
*Average redistribution by treatments
tab treat if wheel_winner==1, su(wheel1playerredistribution)
gen redistribution = wheel1playerredistribution
cibar redistribution if wheel_winner==1, over(treat)  barcol (gs0 gs3 gs6 gs8 gs11)  ciopts(lcolor(red))  barlabel(on)  graphopts(graphregion(color(white)))
graph export "Graphs/Fig2_cibar.png", as(png) name("Graph") replace
 
 	ranksum wheel1playerredistribution if treat==1 | treat==3, by(treat)
	ranksum wheel1playerredistribution if treat==2 | treat==4, by(treat)

**Figure 3 
**Lowess plots 
sort correct 
by correct: egen mredist_correct = mean(wheel1playerredistribution)
sort partner_correct 
by partner_correct: egen mredist_pcorrect = mean(wheel1playerredistribution)

lab var mredist_correct "Local averages"
lab var correct "Proportion of correct answers: self"
lab var partner_correct "Proportion of correct answers: partner"


twoway scatter mredist_correct  correct || lowess redistribution  correct || lfit redistribution  correct if wheel_winner==1, by(treat, graphregion(color(white)) note("N=993")) ytitle(Winner's redistribution after task 1) xtitle((mean) correct)
graph export "Graphs/Fig3_lowess.png", as(png) name("Graph") replace
 
twoway scatter mredist_pcorrect  partner_correct || lowess redistribution  partner_correct || lfit redistribution  partner_correct if wheel_winner==1, by(treat, graphregion(color(white)) note("N=993")) ytitle(Winner's redistribution after task 1) xtitle((mean) partner_correct)   
graph export "Graphs/Fig4_lowess.png", as(png) name("Graph") replace
 
**Table 1: OLS regressions

*No interaction
reg wheel1playerredistribution i.treat c.correct c.partner_correct  if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table1.xls",  replace stat(coef se) label dec(3)

**Interaction effects
reg wheel1playerredistribution i.treat##c.correct  i.treat##c.partner_correct  if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table1.xls",  append stat(coef se) label dec(3)

**Interaction effects + control variables
reg wheel1playerredistribution i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)
outreg2 using "Results/Table1.xls",  append stat(coef se) label dec(3)



xtile pq = partner_correct, nq(4)
sort treat 
by treat: reg wheel1playerredistribution i.treat c.correct i.pq hotseat  time age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

by treat: tab pq 

xtile pqtreat1 = partner_correct if treat==1, nq(4)
xtile pqtreat2 = partner_correct if treat==2, nq(4)

xtile pqtreat3 = partner_correct if treat==3, nq(4)
xtile pqtreat4 = partner_correct if treat==4, nq(4)
xtile pqtreat5 = partner_correct if treat==5, nq(4)

//Add these 3 points
//1) Include these in the appendix -- robustness check, showing evidence that relationship is not linear. Rate of redistribution rises with partner's correctness. (responding to concern that we are assuming a linear relationship between other's performance and redistribution. Relationship might not be linear. People who perform really badly, people might feel bad.)
reg wheel1playerredistribution c.correct i.pqtreat1 hotseat  time age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)
outreg2 using "Results/Table3A_nonlinear.xls",  replace stat(coef se) label dec(3) ctitle("LNT")


reg wheel1playerredistribution c.correct i.pqtreat2 hotseat  time age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)
outreg2 using "Results/Table3A_nonlinear.xls",  append stat(coef se) label dec(3) ctitle("LT")


reg wheel1playerredistribution c.correct i.pqtreat3 hotseat  time age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)
outreg2 using "Results/Table3A_nonlinear.xls",  append stat(coef se) label dec(3) ctitle("ANT")

reg wheel1playerredistribution c.correct i.pqtreat4 hotseat  time age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)
outreg2 using "Results/Table3A_nonlinear.xls",  append stat(coef se) label dec(3) ctitle("AT")

reg wheel1playerredistribution c.correct i.pqtreat5 hotseat  time age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)
outreg2 using "Results/Table3A_nonlinear.xls",  append stat(coef se) label dec(3) ctitle("ATR")


//2) Robustness check -- relative how I perfrom compared to other. Difference between my performance and partner's performance (see Nick's email). Interaction model replacing own and partner with just the gap. Whether results were driven by relativeness or magnitude
gen gap = correct - partner_correct
gen gap_performance = 0 if gap<0
replace gap_performance = 1 if gap==0
replace gap_performance = 2 if gap>0&gap<=1

label define gaplab 0 "Neg gap" 1 "Zero gap" 2 "Pos gap"
label val gap_performance gaplab
reg wheel1playerredistribution i.treat##ib2.gap_performance hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)
outreg2 using "Results/Table4A_relative.xls",  replace stat(coef se) label dec(3)



//3) Appendix and refer to this in paper (maybe descriptive statistics))(Nick's email with tables)Winners only give to those who perform well/deserve. Why treatment effects weren't there? Not all winners in transparent treatment give more than winners in other non-transparent treatment if they feel the partners didn't deserve it. Difference between non-transaprent winners and transparent winners are the same -- winners in transaprent don't give differently to winners in other treatments to those who they feel don't deserve. No treatment effect because there is heterogeneity going on here when partners perform well. When partners perform well, winners feel they can share money. But not everyone did that: it depends on how their partner performed. Policy implication: transparency alone isn't enough. 
*that's why we don't see an average non-interacted treatment effect
sort treat
tab gap_performance if wheel_winner==1
by treat: tab gap_performance if wheel_winner==1


**Plotting the marginal effects
reg wheel1playerredistribution i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

margins, dydx(correct) over(treat) pwcompare(effects)
margins treat  , at(correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))  
marginsplot, by(treat)

graph export "Graphs/Fig5_Marginsplot_correct.png", as(png) name("Graph") replace
 
margins, dydx(partner_correct) over(treat) pwcompare(effects)

margins treat  , at(partner_correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))
marginsplot, by(treat)

graph export "Graphs/Fig6_Marginsplot_pcorrect.png", as(png) name("Graph") replace
  


 
**Loser's attitudes towards redistribution had they won
reg wheel1playerredistribution i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner==0  , vce(hc3)

outreg2 using "Results/Table2.xls",  replace stat(coef se) label dec(3)

 
**Deservingness regressions -- winners only
reg p1 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table3.xls",  replace stat(coef se) label dec(3)
 /*
margins, dydx(correct) over(treat) pwcompare(effects)
margins treat  , at(correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))  
marginsplot, by(treat) plotregion(fcolor(white))

graph export "Graphs/Fig1A_Marginsplot_correct.png", as(png) name("Graph") replace

margins, dydx(partner_correct) over(treat) pwcompare(effects)

margins treat  , at(partner_correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))
marginsplot, by(treat)

graph export "Graphs/Fig1A_Marginsplot_pcorrect.png", as(png) name("Graph") replace
  */
 
reg p2 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table3.xls",  append stat(coef se) label dec(3)
/*
margins, dydx(correct) over(treat) pwcompare(effects)
margins treat  , at(correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))  
marginsplot, by(treat)  

graph export "Graphs/Fig2A_Marginsplot_correct.png", as(png) name("Graph") replace

margins, dydx(partner_correct) over(treat) pwcompare(effects)

margins treat  , at(partner_correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))
marginsplot, by(treat)

graph export "Graphs/Fig2A_Marginsplot_pcorrect.png", as(png) name("Graph") replace
 */
 
reg p3 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table3.xls",  append stat(coef se) label dec(3)
/*
margins, dydx(correct) over(treat) pwcompare(effects)
margins treat  , at(correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))  
marginsplot, by(treat)  

graph export "Graphs/Fig3A_Marginsplot_correct.png", as(png) name("Graph") replace

margins, dydx(partner_correct) over(treat) pwcompare(effects)

margins treat  , at(partner_correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))
marginsplot, by(treat)

graph export "Graphs/Fig3A_Marginsplot_pcorrect.png", as(png) name("Graph") replace
  */ 
reg p4 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table3.xls",  append stat(coef se) label dec(3)
/*
margins, dydx(correct) over(treat) pwcompare(effects)
margins treat  , at(correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))  
marginsplot, by(treat)  

graph export "Graphs/Fig4A_Marginsplot_correct.png", as(png) name("Graph") replace

margins, dydx(partner_correct) over(treat) pwcompare(effects)

margins treat  , at(partner_correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))
marginsplot, by(treat)

graph export "Graphs/Fig4A_Marginsplot_pcorrect.png", as(png) name("Graph") replace
   */ 

**Explaining away the partner correct results	
 
reg  wheel1playerredistribution p1 p2 p3 p4 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table4.xls",  replace stat(coef se) label dec(3)


**Spillover effects on PGG game

*******Robustness checks****** 

**Public good game - Do losers give more if they received more in the Wheel game?

reg pg_contribution mwinner mloser hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white altruism_total if wheel_winner ==0  , vce(hc3) 

outreg2 using "Results/Table5.xls",  replace stat(coef se) label dec(3)


/*
reg pg_contribution mwinner mloser hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married altruism_total if wheel_winner ==1  , vce(hc3) 

outreg2 using "Results/Table5.xls",  append stat(coef se) label dec(3)
*/

 
 
**Robustness checks 

*Tobit model
tobit wheel1playerredistribution ib2.treat##c.correct ib2.treat##c.partner_correct hotseat  time ///
age age_sq gender high_income edu_college ib5.married altruism_total if wheel_winner ==1  , ll(0) ul(100)  


***Appendix

**Table X: Egalitarian and Selfishness regressions

reg egal i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table_egalitarian.xls",  replace stat(coef se) label dec(3)

*Selfish redistribution
reg zero i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

outreg2 using "Results/Table_egalitarian.xls",  append stat(coef se) label dec(3)

x

**Deservingness regressions -- losers only
reg p1 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==0  , vce(hc3)

outreg2 using "Results/loser_deservingness.xls",  replace stat(coef se) label dec(3)

reg p2 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==0  , vce(hc3)

outreg2 using "Results/loser_deservingness.xls",  append stat(coef se) label dec(3)

reg p3 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==0  , vce(hc3)

outreg2 using "Results/loser_deservingness.xls",  append stat(coef se) label dec(3)

reg p4 i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married i.white ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==0  , vce(hc3)

outreg2 using "Results/loser_deservingness.xls",  append stat(coef se) label dec(3)


**Plotting the marginal effects
reg egal i.treat##c.correct i.treat##c.partner_correct hotseat  time ///
age age_sq i.gender high_income edu_college ib5.married ib0.cheat_you ib0.cheat_other altruism_total if wheel_winner ==1  , vce(hc3)

margins, dydx(partner_correct) over(treat) pwcompare(effects)
margins treat  , at(partner_correct=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1))  
marginsplot, by(treat)  

**Balance checks
	//female dummy
	tabulate gender, gen(gen)
	rename gen2 female
	
	//single dummy
	tabulate married, gen(relation)
	rename relation5 single
	
	//age dummies
	tabulate age, gen(agegroup)
	rename agegroup1 age_under20
	rename agegroup2 age_20_29
	rename agegroup3 age_30_39
	rename agegroup4 age_40_49
	rename agegroup5 age_50_59
	rename agegroup6 age_60plus
	
	
orth_out age_* female high_income edu_college white single altruism_total using "Results/balance.xls", by(treat) pcompare count replace

