Analyze Variable Plan 
==========================
*Analyze Variable Plan* allows the user to analyze a single stage variable sampling plan. This type of sampling involves measuring a continuous variable using a sample.

## Input
-------
## Options
- **Lot size (N)**: total number of items in the lot.
- **Sample size (n)**: number of items in the sample.
- **k**: the value used to compare the sample mean with the specification limits.
- **Standard Deviation (Historical) known**: whether or not the historical standard deviation is known.
- **Assess variable plan**: check if the plan satisfies the (user-specified) constraints associated with AQL and RQL.
    
    <u>Quality constraints</u>: user-specified constraints associated with AQL and RQL. All values have to lie between 0 and 1, both exclusive.
 - *Acceptable Quality Level (AQL)*: the smallest proportion of non-conforming items that makes the lot definitely acceptable.
 - *Rejectable Quality Level (RQL / LTPD)*: the proportion of non-conforming items in the lot that is unacceptable to the user.
 - *Producer's Risk (α)*: Risk associated with the rejection of a lot that has acceptable quality. Equals the probability of rejecting a lot at AQL.
 - *Consumer's Risk (β)*: Risk associated with the acceptance of an RQL quality lot.
- **Proportion non-conforming items**: the range of quality levels at which the plan will be analyzed.
 - *From*: lower bound of the quality level range.
 - *To*: upper bound of the quality level range.
 - *Step size*: difference between consecutive quality levels.
- **Output options**: the tables/plots to be generated for the variable plan.
 - *Summary table*: show the acceptance probabilities at each of the points in the quality level range specified through PD.
 - *OC curve*: display the operating characteristics (OC) curve for the sampling plan at the quality levels specified through PD.
 - *AOQ curve (plan with rectification)*: display the long term outgoing quality level of the sampling plan (with rectification) when rejected lots are 100% inspected, and all non-conforming items are replaced with conforming items.
 - *ATI curve (plan with rectification)*: display the long term total number of items inspected when rejected lots are 100% inspected, and all non-conforming items are replaced with conforming items.

## Output 
-------
- **Basic sampling table**: shows the table for the variable plan.
- **Summary table**: shows the acceptance probabilities at every point in the quality level range.
- **OC curve**: plots the operating characteristics of the plan.
- **AOQ curve**: plots the outgoing quality levels against the incoming quality levels, for a plan with rectification.
- **ATI curve**: plots the average number of items inspected against the incoming quality levels, for a plan with rectification.

## References 
-------
- Kiermeier, A. (2008). Visualizing and Assessing Acceptance Sampling Plans: The R Package AcceptanceSampling. *Journal of Statistical Software*, 26(6), 1–20. https://doi.org/10.18637/jss.v026.i06.
- Lawson, J. (2021). An Introduction to Acceptance Sampling and SPC with R (1st ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781003100270.

## R Packages
-------
- jaspGraphs
- ggplot2
- AcceptanceSampling