# disentangling_gender_attitudes
Code for the project "Disentangling and Reassembling: Unveiling Hidden Patterns in Gender Attitudinal Surveys"
# Code Overview

This repository contains five scripts that form one analysis pipeline:

1. **`1_data_preprocessing.R` (R)**  
   - Reads CGSS 2017/2018/2021 data and extracts the five gender-attitude items.  
   - Harmonizes coding across waves and constructs the IRTree mapping.  
   - Outputs the IRTree input file `1721tree.txt` for Mplus.

2. **`2_1721irtree.inp` (Mplus)**  
   - Takes `1721tree.txt` as input.  
   - Fits the IRTree model and saves factor scores (four latent dimensions: D, NS, M, E) to an `.fsc` file (e.g., `tree1721-smde-logit1.fsc`).

3. **`3_latent_profile.py` (Python)**  
   - Reads the Mplus factor-score file.  
   - Standardizes the four latent dimensions and fits Gaussian mixture models for several values of *k*.  
   - Saves model-fit statistics to `infoCriterion.csv` and the chosen k = 5 solution (scores + profile membership) to `lpa_k5.csv`.

4. **`4_plot_lpa_results.R` (R)**  
   - Uses `infoCriterion.csv` to plot AIC/BIC/log-likelihood by number of profiles.  
   - Uses `lpa_k5.csv` to visualize the k = 5 profiles (boxplots of D, NS, M, E).  
   - Exports the LPA figures as PNG files.

5. **`5_multinomial_results.R` (R)**  
   - Merges respondent characteristics (gender, age, education, hukou, marital status, year) with `lpa_k5.csv`.  
   - Fits a multinomial logit model of profile membership and computes average marginal effects (AMEs).  
   - Plots AMEs for key covariates (e.g., gender, education, hukou).

## Minimal workflow

1. Run `1_data_preprocessing.R` → produces `1721tree.txt`.  
2. Run `2_1721irtree.inp` in Mplus → produces factor-score file `.fsc`.  
3. Run `3_latent_profile.py` → produces `infoCriterion.csv` and `lpa_k5.csv`.  
4. Run `4_plot_lpa_results.R` → produces LPA figures.  
5. Run `5_multinomial_results.R` → produces multinomial/AME figures.

(Details on variable coding and model specification are documented inside each script.)
