# exploration-learning-coupling

Repository for data and analytical R code for the manuscript "Faster explorers are better at reversing learned association between prey colour and quality without trade-off with learning speed in a generalist predator".

List of files:

1. boldness.csv: raw data from exploration trials

**id:** unique ID of the individual<br/>
**sex:** sex of the individual<br/>
**exp:** exploration trial number (1 = the 1st trial, 2 = the 2nd trial)<br/>
**1stMove:** moving latency in seconds<br/>
**cross:** time until first enterning the exploration area in seconds<br/>
**distanced_moved:** raw distance moved in the duration of a trial in unit of pixel<br/>
**time_novel:** total time in seconds during which the lizard was in the exploration area<br/>
**total_time:** total observation time in seconds<br/>
**pc1:** score of the first principal component<br/>
**pc2:** score of the second principal component<br/>

2. reverse learning.csv: tabulated raw data from learning and reverse learning trials

**id:** unique ID of the individual<br/>
**sex:** sex of the individual<br/>
**group:** the colour associated with bitterness in the trial (R = red, Y = yellow)<br/>
**exp:** number of the daily trial. Trials 1-5 belong to the learning phase and 6-10 to the reverse learning phase<br/>
**exp_date:** date of the trial, in format of MM-DD-YYYY<br/>
**latency:** time until the lizard attacked the first cricket in seconds<br/>
**prey1:** type of first prey attacked (P = palatable/normal, U = unpalatable/bitter)<br/>
**unpalatable_50:** number of bitter crickets attacked among the first 50% of crickets attacked<br/>
**total_50:** number of first 50% of the crickets attacked<br/>
**palatable_50**: umber of normal crickets attacked among the first 50% of crickets attacked<br/>
**unpalatable_consumed:** number of bitter crickets consumed<br/>
**total_consumed:** total number of crickets consumed<br/>
**palatable_consumed:** number of normal crickets consumed<br/>
**prey1_2:** different coding of prey1 (1 = palatable/normal, 0 = unpalatable/bitter) for logistic GLM<br/>
**percent_unpalatable_50:** unpalatable_50/total_50<br/>
**percent_unpalatable_consumed:** unpalatable_consumed/total_consumed<br/>
**group2:** RY = individuals that associated first red (learning phase) and then yellow (reverse learning phase) with bitterness; YR = individuals that experienced the opposite colour treatment<br/>

3. stats.R: R code for statistical analyses and data visualization in this study
