This repository contains code and data for: Zou, H.-X., and V.H.W. Rudolf. Consequences of phenological shifts are determined by the number of generations per season. Accepted at _Ecology_.

# Code

`1_Overview.R` contains code that descriptive results of final population of the two flour beetle species, and summary statistics between treatments.
`2_ModelFitting.R` contains code that fit Bayesian models to obtain competition coefficients and intrinsic growth rates at different number of generations and arrival times.
`3_Competition.R` contains code for visualization and analysis of intrinsic growth rates, competition coefficients, and predicted long-term competitive outcomes from these parameters. 

# Data

`AdditionalAssays.xlsx` contains data from two additional assays apart from the main experiments (see Appendix I: Additional Experiments for more information): 
- Adult fecundity (sheet `Fecundity`): number of eggs for each species counted on five consecutive days (columns `Day_1` to `Day_5`).
- Egg predation rate (sheet `Egg Predation New`): number of eggs survived one day after being added to 30 larvae of different sizes of the other species, recorded in column `Day_1`.

`Counts.csv` contains original data of final survey, with the following columns:
- `Vial_name`: Name of the vial, in the format of A-BX-C-D-Z, where A represents number of generations (1-3), B represents the species that arrives first (T, for _Tribolium castaneum_, or F, for _Tribolium confusm_), X represents the days by which species B arrives first (6, 12, 18; 0 represents simultaneous arrival), C represents the starting density of _T. castaneum_, D represents the starting density of _T. confusum_, and Z represents the replication number (roman numeral I to III).
- `Num_Gen`: Number of generations.
- `Arriv_Time`: Days by which the early species arrives first.
- `Start_T`: Starting density of _T. castaneum_.
- `Start_F`: Starting density of _T. confusum_.
- `Replication`: Replication number.
- `First_Addition`: Date when eggs of the first species were added.
- `Second_Addition`: Date when eggs of the second species were added; `NA` if the vial is single-species, or if the two species arrive at the same time.
- `Check_Date`: Date of the final survey of the vial, when all individuals in the vial emerged as adults.
- `Adults_T`: Final count of _T. castaneum_ adults.
- `Adults_F`: Final count of _T. confusum_ adults.
- `Relative_Arriv_Time`: The arrival time of _T. confusum_ minus that of _T. castaneum_, such that positive numbers indicate _T. confusum_ arrives first.

# Models

This directory contains Bayesian models used to fit intrinsic growth rates and competition coefficients.

`lv_Bayesian_single.stan` contains the stan code for fitting intrinsic growth rates from single-species vials, which are subsequently provided as priors for fitting competition coefficients.
`lv_Bayesian.stan` contains the stan code for fitting a discrete Lotka-Volterra model between the two species.

# Coefficients

This directory contains fitted coefficients from the script `2_ModelFitting.R`. Because fitting the Bayesian models can be time-consuming, we provided fitted coefficients for easy reproduction of main figures, which can be done by running the script `3_Competition.R`.

In both files, `Q10`, `Q90`, and `Median` contain the 10% quantile, 90% quantile, and median of the posterior distribution of parameters.

`CompCoefficients.csv` contains fitted ranges of competition coefficients under different numbers of generations and relative arrival times.
`Lambdas.csv` contains intrinsic growth rates fitted from single-species vials under different numbers of generations.
