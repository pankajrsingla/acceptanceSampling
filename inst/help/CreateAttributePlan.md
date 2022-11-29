Create Attribute Plan 
==========================
*Create Attribute Plan* allows the user to generate a 2-class single stage attribute sampling plan that satisfies the user-specified constraints. This type of sampling involves classifying each inspected item as either acceptable or not acceptable.

## Input
-------
## Options
- **Quality constraints**: user-specified constraints associated with AQL and RQL.
 - *Acceptable Quality Level (AQL)*: the smallest proportion of non-conforming items that makes the lot definitely acceptable.
 - *Rejectable Quality Level (RQL / LTPD)*: the proportion of non-conforming items in the lot that is unacceptable to the user.
 - *Producer's Risk (α)*: Risk associated with the rejection of a lot that has acceptable quality. Equals the probability of rejecting a lot at AQL.
 - *Consumer's Risk (β)*: Risk associated with the acceptance of an RQL quality lot.
- **PD (Proportion non-conforming items)**: the range of quality levels at which the plan will be analyzed.
 - *From*: lower bound of the quality level range.
 - *To*: upper bound of the quality level range.
 - *Step size*: difference between consecutive quality levels.
- **Distribution**: the distribution assumed to be followed by the number of non-conforming items in the inspected sample.
 - *Binomial*: for the binomial distribution. Should be used when the sample size (n) is very small compared to the size of the lot (N), and the probability of drawing non-conforming items from the lot can be assumed to be constant.
 - *Hypergeometric*: for the hypergeometric distribution. Should be used when the lot size (N) is not significantly larger than the sample size (n).
   - *Lot size (N)*: total number of items in the lot. Proportion of non-conforming items * N gives the number of non-conforming items, which have to be non-negative integers.
 - *Poisson*: for the poisson distribution. Should be used when the sample size (n) is large, and the proportion of non-conforming items (PD) is small.
- **Output options**: the tables/plots to be generated for the generated attribute plan.
 - *Summary table*: show the acceptance probabilities at each of the points in the quality level range specified through PD.
 - *OC curve*: display the operating characteristics (OC) curve for the sampling plan at the quality levels specified through PD.

## Output 
-------
- **Basic sampling tables**: shows the tables of the generated attribute plan, and of the acceptance and rejection probabilities at AQL and RQL.
- **Summary table**: shows the acceptance probabilities at every point in the quality level range.
- **OC curve**: plots the operating characteristics of the plan.

## References 
-------
- Kiermeier, A. (2008). Visualizing and Assessing Acceptance Sampling Plans: The R Package AcceptanceSampling. *Journal of Statistical Software*, 26(6), 1–20. https://doi.org/10.18637/jss.v026.i06.

## R Packages
-------
- jaspGraphs
- ggplot2
- AcceptanceSampling