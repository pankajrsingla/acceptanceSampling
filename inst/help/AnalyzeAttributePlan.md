Analyze Attribute Plan 
==========================
*Analyze Attribute Plan* allows the user to analyze both single as well as multiple stage 2-class attribute sampling plans.

## Input
-------
## Options
- **Lot size (N)**: total number of items in the lot. Note that if hypergeometric distribution is specified, the proportion of non-conforming items * N gives the number of non-conforming items, which have to be non-negative integers.
- **Number of stages**: (only for multiple stage plans) number of stages in the multiple stage sampling plan.
- **Sample size (n)**: number of items chosen for inspection from the lot. For multiple stage plans, sample sizes for each stage of sampling are specified in a tabular input.
- **Acceptance number (c)**: the maximum number of non-conforming items in the sample that is acceptable. If there are c or fewer non-conforming items in the sample, the lot is accepted. For a multiple stage plan, the acceptance numbers (c1, c2, ...) are specified in a tabular input, and considered cumulatively.
- **Rejection number (r)**: the minimum number of non-conforming items in the sample that is unacceptable. If there are r or more non-conforming items in the sample, the lot is rejected. For a multiple stage plan, the rejection numbers (r1, r2, ...) are specified in a tabular input, and considered cumulatively.
- **Distribution**: the distribution assumed to be followed by the number of non-conforming items in the inspected sample(s).
 - *Binomial*: for the binomial distribution. Should be used when the sample size (n) is very small compared to the size of the lot (N), and the probability of drawing non-conforming items from the lot can be assumed to be constant.
 - *Hypergeometric*: for the hypergeometric distribution. Should be used when the lot size (N) is not significantly larger than the sample size (n).
   - *Lot size (N)*: total size of the lot. Proportion of non-conforming items * N gives the number of non-conforming items, which have to be non-negative integers.
 - *Poisson*: for the poisson distribution. Should be used when the sample size (n) is large, and the proportion of non-conforming items (PD) is small.
- **Assess sampling plan**: check if the plan satisfies the user-specified constraints associated with AQL and RQL.
 - *Acceptable Quality Level (AQL)*: the smallest proportion of non-conforming items that makes the lot definitely acceptable.
 - *Rejectable Quality Level (RQL / LTPD)*: the proportion of non-conforming items in the lot that is unacceptable to the user.
 - *Producer's Risk (α)*: Risk associated with the rejection of a lot that has acceptable quality. Equals the probability of rejecting a lot at AQL.
 - *Consumer's Risk (β)*: Risk associated with the acceptance of an RQL quality lot.
- **PD (Proportion non-conforming items)**: the range of quality levels at which the plan will be analyzed.
 - *From*: lower bound of the quality level range.
 - *To*: upper bound of the quality level range.
 - *Step size*: difference between consecutive quality levels.
- **Output options**: the tables/plots to be generated for the given plan.
 - *Summary table*: show the acceptance probabilities at each of the points in the quality level range specified through PD.
 - *OC curve*: display the operating characteristics (OC) curve for the sampling plan at the quality levels specified through PD.
 - *AOQ curve (plan with rectification)*: display the long term outgoing quality level of the sampling plan (with rectification) when rejected lots are 100% inspected, and all non-conforming items are replaced with conforming items.
 - *ATI curve (plan with rectification)*: display the long term total number of items inspected when rejected lots are 100% inspected, and all non-conforming items are replaced with conforming items.
 - *ASN curve*: (only for multiple stage plans) display the average number of items inspected to decide whether to accept or reject the lot. Indicates the sampling efficiency of the plan.
 
## Output 
-------
- **Summary table**: shows the acceptance probabilities at every point in the quality level range.
- **OC curve**: plots the operating characteristics of the plan.
- **AOQ curve**: plots the outgoing quality levels against the incoming quality levels, for a plan with rectification.
- **ATI curve**: plots the average number of items inspected against the incoming quality levels, for a plan with rectification.
- **ASN curve**: (only for multiple stage plans) plots the average number of items inspected to arrive at a decision on the lot, against the incoming quality levels.

## References 
-------
- Kiermeier, A. (2008). Visualizing and Assessing Acceptance Sampling Plans: The R Package AcceptanceSampling. *Journal of Statistical Software*, 26(6), 1–20. https://doi.org/10.18637/jss.v026.i06.

## R Packages
-------
- jaspGraphs
- ggplot2
- AcceptanceSampling