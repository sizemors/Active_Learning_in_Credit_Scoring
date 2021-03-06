# Active_Learning_in_Credit_Scoring

This experiment uses two data sets: LendingClub (default rate of ~7%) and a DGP data set genereated from a synthetic data generating process (default rate of 40%). The LC cleaning code and DGP generating functions are not included here. Use prepared data sets (Step 2 in LC and Step 6 in DGP) below. The DGP experiment borrows some parameter settings from LC. LC should be run first. The experiment includes calculation of some financial variables from scripts which are not included here. Define: Total Interest of *good* risk / TN = 1,128, Expected Loss of *bad* risk / FN = 2,704, median funded loan amount = 10,000.

To perform the experiment on LC data set, perform these steps in order:

1. Open LOOP_HYPERPARAMETERS.R
2. Load lc_data_for_loop_clean.RDS
3. Run LOOP_HYPERPARAMETERS.R
4. Run LC_RI_FUNCTIONS.R
5. Run AL_FUNCTIONS.R
6. Run OPAL.R
7. Run LC_CLUSTERING.R
8. Run LC_LOOP.R
9. Run EVALUATE.R

To perform the experiment on the DGP data set, perform these steps in order:

0. Clear global environment
1. Run DGP_RI_FUNCTIONS.R
2. Run AL_FUNCTIONS.R
3. Run OPAL.R
4. Open DGP_CLUSTERING.R
5. Load dgp.RDS
6. Run DGP_CLUSTERING.R
7. Run DGP_LOOP.R
8. Run EVALUATE.R

This experiment uses following sources, cited in the corresponding scripts:

Anderson, B., Haller, S., & Siddiqi, N. (2009). Reject inference techniques implemented in credit scoring for SAS enterprise miner. 
SAS Global Forum, SAS Institute, Inc. 

Kottke, D., & Krempl, G. (2015b). OPALgain Python Script. Retrieved 20-04-2021 from https://kmd.cs.ovgu.de/res/opal/OPALgain.py  (from related paper:
Krempl, G., Kottke, D., & Lemaire, V. (2015b). Optimised probabilistic active learning (OPAL):  For fast, non-myopic, cost-sensitive active classification, Machine Learning, 100(2–3), 449–476.)

Kozodoi, N. (2020). Appendix of Unpublished Manuscript – Submitted to Management Science.

Ramey, J. (2017). Active learning in R Package. Deprecated from CRAN. Retrieved 04-01-2021 from: https://github.com/ramhiser/activelearning

Siddiqi, N. (2012). Credit risk scorecards: developing and implementing intelligent credit scoring. John Wiley & Sons, Inc. 

