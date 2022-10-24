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
  optionNames <- c("lotSize", "pd_lower", "pd_upper", "pd_step", "pd_prp", "pa_prp", "pd_crp", "pa_crp")
  .findSampleCheckErrors(dataset, options)
  .findPlan(jaspResults, options, optionNames)
}


.findSampleCheckErrors <- function(dataset = NULL, options) {
  # perform a check on the hypothesis

  sanityCheck <- function() {
    # if (options$pd_prp > options$pd_crp) {
    #   return(gettext("CRP should have worse quality (higher proportion defective) than PRP."))
    # }
  }
  
  # Error Check 1: Number of levels of the variables and the hypothesis
  .hasErrors(
    dataset              = dataset,
    custom               = sanityCheck,
    exitAnalysisIfErrors = TRUE
  )
}

# Results functions ----

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
    
    # Create and fill the output table(s)
    table <- createJaspTable(title = "Sampling plan meeting the specified risk points")
    # table$dependOn(c(names, "stdev", "distribution"))
    names <- c(names, "distribution")
    table$dependOn(names)
    table$addColumnInfo(name = "col_1", title = "", type = "string")
    table$addColumnInfo(name = "col_2", title = "Value", type = "integer")
    table$addRows(list("col_1" = "Sample size (n)", "col_2" = plan_vars$n))
    table$addRows(list("col_1" = "Acc. Number (c)", "col_2" = plan_vars$c))
    table$addRows(list("col_1" = "Rej. Number (r)", "col_2" = plan_vars$r))
    table$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["findTable"]] <- table

    if (options$showSummary || options$showOCCurve) {
      df_plan <- data.frame(PD = pd, PA = plan@paccept)
      
      # OC Curve
      if (options$showOCCurve) {
        getOCCurve(jaspResults, df_plan, "", c(names, "showOCCurve"))
      }

      # Summary
      if (options$showSummary) {
        getSummary(jaspResults, df_plan, "", c(names, "showSummary"))
      }
    }
  }
}