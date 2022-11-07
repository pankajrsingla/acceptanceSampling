#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

CreateAttributePlan <- function(jaspResults, dataset = NULL, options, ...) {
  optionNames <- c("lotSize", "pd_lower", "pd_upper", "pd_step", "pd_prp", "pa_prp", "pd_crp", "pa_crp", "distribution")
  .findPlan(jaspResults, options, optionNames)
}

# Find the sampling plan that satisfies the specified AQL and RQL constraints.
.findPlan <- function(jaspResults, options, names) {
  if (options$pd_prp && options$pa_prp && options$pd_crp && options$pa_crp) {
    pd_lower <- options$pd_lower
    pd_upper <- options$pd_upper
    pd_step <- options$pd_step
    pd <- seq(pd_lower, pd_upper, pd_step)
    dist = options$distribution
    plan_vars <- NULL
    plan <- NULL
      
    # # Create sampling plan with the specified values
    if (dist == "hypergeom") {
      # Need to provide the lot size (N) for hypergeometric distribution.
      plan_vars <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, 1-options$pa_prp), CRP = c(options$pd_crp, options$pa_crp), type = dist, N = options$lotSize)
      plan <- AcceptanceSampling::OC2c(N = options$lotSize, n = plan_vars$n, c = plan_vars$c, r = plan_vars$r, type = dist, pd = pd)
    } else if (dist == "normal") {
      # For now, the selection of normal distribution is disabled.
      # Need to specify standard deviation (whether known or unknown) for normal distribution.
      ## A value of known results in a sampling plan based on the population standard deviation, 
      ## while a value of unknown results in the use of the sample standard deviation.
      # plan <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, options$pa_prp), CRP = c(options$pd_crp, options$pa_crp), type = dist, s.type = options$stdev)
    } else {
      # Binomial and Poisson distributions don't require lot size (N) or standard deviation.
      plan_vars <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, 1-options$pa_prp), CRP = c(options$pd_crp, options$pa_crp), type = dist)
      plan <- AcceptanceSampling::OC2c(n = plan_vars$n, c = plan_vars$c, r = plan_vars$r, type = dist, pd = pd)
    }
    
    .attributePlanTable(jaspResults, names, plan_vars, positionInContainer=1)
    
    if (options$showSummary || options$showOCCurve) {
      df_plan <- data.frame(PD = pd, PA = plan@paccept)
      
      # OC Curve
      if (options$showOCCurve) {
        getOCCurve(jaspResults, df_plan, "", c(names, "showOCCurve"), positionInContainer=3)
      }

      # Summary
      if (options$showSummary) {
        getSummary(jaspResults, df_plan, "", c(names, "showSummary"), positionInContainer=2)
      }
    }
  }
}

# Create and fill the output table(s)
.attributePlanTable <- function(jaspResults, depend_variables, plan_vars, positionInContainer) {
  # Create and fill the output table(s)
  if (!is.null(jaspResults[["findTable"]])) {
    return ()
  }
  table <- createJaspTable(title = "Generated Sampling Plan")
  table$dependOn(depend_variables)
  # table$transpose <- TRUE
  table$addColumnInfo(name = "col_1", title = "", type = "string")
  table$addColumnInfo(name = "col_2", title = "Value", type = "integer")
  table$addRows(list("col_1" = "Sample size", "col_2" = plan_vars$n))
  table$addRows(list("col_1" = "Acceptance Number", "col_2" = plan_vars$c))
  table$showSpecifiedColumnsOnly <- TRUE
  table$position <- positionInContainer
  jaspResults[["findTable"]] <- table
}