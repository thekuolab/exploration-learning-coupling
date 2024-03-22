# exploration-learning-coupling

Repository for data and analytical R code for the manuscript "Faster explorers are better at reversing learned association between prey colour and quality without trade-off with learning speed", currently in review.

List of files:

1. boldness.csv: raw data from exploration trials
**id:** unique ID of the individual
**sex:** sex of the individual
**exp:** exploration trial number (1 = the 1st trial, 2 = the 2nd trial)
**1stMove:** moving latency in seconds
**cross:** time until first enterning the exploration area in seconds
**distanced_moved:** raw distance moved in the duration of a trial in unit of pixel
**time_novel:** total time in seconds during which the lizard was in the exploration area
**total_time:** total observation time in seconds
**pc1:** score of the first principal component
**pc2:** score of the second principal component

2. reverse learning.csv: tabulated raw data from learning and reverse learning trials
**id:** unique ID of the individual
**sex:** sex of the individual
**group:** the colour associated with bitterness in the trial (R = red, Y = yellow)
**exp:** number of the daily trial. Trials 1-5 belong to the learning phase and 6-10 to the reverse learning phase
**exp_date:** date of the trial, in format of MM-DD-YYYY
**latency:** time until the lizard attacked the first cricket in seconds
**prey1:** type of first prey attacked (P = palatable/normal, U = unpalatable/bitter)
**unpalatable_50:** number of bitter crickets attacked among the first 50% of crickets attacked
**total_50:** number of first 50% of the crickets attacked
**palatable_50**: umber of normal crickets attacked among the first 50% of crickets attacked
**unpalatable_consumed:** number of bitter crickets consumed
**total_consumed:** total number of crickets consumed
**palatable_consumed:** number of normal crickets consumed
**prey1_2:** different coding of prey1 (1 = palatable/normal, 0 = unpalatable/bitter)
**percent_unpalatable_50:** unpalatable_50/total_50
**percent_unpalatable_consumed:** unpalatable_consumed/total_consumed
**group2:** RY = individuals that associated first red (learning phase) and then yellow (reverse learning phase) with bitterness; YR = individuals that experienced the opposite colour treatment

3. stats.R: R code for statistical analyses and data visualization in this study
