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

# txt = "Find the sampling plan that satisfies the specified AQL and RQL constraints."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------------------------
##  Find the sampling plan that satisfies the specified AQL and RQL constraints.  --
##----------------------------------------------------------------------------------
#' @param jaspResults <>.
#' @param options User specified options.
#' @param names <>.
#' @returns <>.
#' @seealso
#'   [()] for <>.
#' @examples
#' .findPlan(jaspResults, options, names)
.findPlan <- function(jaspResults, options, names) {
  if (options$pd_prp && options$pa_prp && options$pd_crp && options$pa_crp) {
    pd_lower <- options$pd_lower
    pd_upper <- options$pd_upper
    pd_step <- options$pd_step
    pd <- seq(pd_lower, pd_upper, pd_step)
    pd <- c(pd, options$pd_prp, options$pd_crp)
    pd <- pd[!duplicated(pd)]
    dist = options$distribution
    plan_vars <- NULL
    plan <- NULL
      
    # # Create sampling plan with the specified values
    if (dist == "hypergeom") {
      # Need to provide the lot size (N) for hypergeometric distribution.
      plan_vars <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, 1-options$pa_prp), 
                                                CRP = c(options$pd_crp, options$pa_crp), type = dist, N = options$lotSize)
      plan <- AcceptanceSampling::OC2c(N = options$lotSize, n = plan_vars$n, c = plan_vars$c, r = plan_vars$r, type = dist, pd = pd)
    } else if (dist == "normal") {
      # For now, the selection of normal distribution is disabled.
      # Need to specify standard deviation (whether known or unknown) for normal distribution.
      ## A value of known results in a sampling plan based on the population standard deviation, 
      ## while a value of unknown results in the use of the sample standard deviation.
      # plan <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, options$pa_prp), 
                                              # CRP = c(options$pd_crp, options$pa_crp), type = dist, s.type = options$stdev)
    } else {
      # Binomial and Poisson distributions don't require lot size (N) or standard deviation.
      plan_vars <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, 1-options$pa_prp), 
                                                CRP = c(options$pd_crp, options$pa_crp), type = dist)
      plan <- AcceptanceSampling::OC2c(n = plan_vars$n, c = plan_vars$c, r = plan_vars$r, type = dist, pd = pd)
    }
    
    df_plan <- data.frame(PD = pd, PA = plan@paccept)
    .attributePlanTable(jaspResults, names, plan_vars, options$pd_prp, df_plan$PA[df_plan$PD == options$pd_prp], 
                        options$pd_crp, df_plan$PA[df_plan$PD == options$pd_crp], positionInContainer=1)

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

# txt = "Create and fill the output table(s)."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##             Create and fill the output table(s).             --
##----------------------------------------------------------------
#' @param jaspResults <>.
#' @param depend_variables <>.
#' @param plan_vars <>.
#' @param pd_prp <>.
#' @param pa_prp <>.
#' @param pd_crp <>.
#' @param pa_crp <>.
#' @param positionInContainer <>.
#' @returns <>.
#' @seealso
#'   [()] for <>.
#' @examples
#' .attributePlanTable(jaspResults, depend_variables, plan_vars, df_plan, positionInContainer)
.attributePlanTable <- function(jaspResults, depend_variables, plan_vars, pd_prp, pa_prp, pd_crp, pa_crp, positionInContainer) {
  # Simple table with sample size and acc. number
  if (is.null(jaspResults[["findPlanTable"]])) {
    plan_table <- createJaspTable(title = "Generated Sampling Plan")
    plan_table$dependOn(depend_variables)
    plan_table$addColumnInfo(name = "col_1", title = "", type = "string")
    plan_table$addColumnInfo(name = "col_2", title = "Value", type = "integer")
    plan_table$addRows(list("col_1" = "Sample size", "col_2" = plan_vars$n))
    plan_table$addRows(list("col_1" = "Acceptance Number", "col_2" = plan_vars$c))
    plan_table$showSpecifiedColumnsOnly <- TRUE
    plan_table$position <- positionInContainer
    jaspResults[["findPlanTable"]] <- plan_table
  }

  # Table with acceptance and rejection probabilities for AQL, RQL
  if (is.null(jaspResults[["findProbTable"]])) {
    prob_table <- createJaspTable(title = "")
    prob_table$dependOn(depend_variables)
    prob_table$addColumnInfo(name = "col_1", title = "Proportion Non-conforming", type = "number")
    prob_table$addColumnInfo(name = "col_2", title = "Acceptance Probability", type = "number")
    prob_table$addColumnInfo(name = "col_3", title = "Rejection Probability", type = "number")
    prob_table$addRows(list("col_1" = pd_prp, "col_2" = pa_prp, "col_3" = 1 - pa_prp))
    prob_table$addRows(list("col_1" = pd_crp, "col_2" = pa_crp, "col_3" = 1 - pa_crp))
    prob_table$showSpecifiedColumnsOnly <- TRUE
    prob_table$position <- positionInContainer + 1
    jaspResults[["findProbTable"]] <- prob_table
  }

  # Description of the sampling plan:
  if (is.null(jaspResults[["description"]])) {
    description <- createJaspHtml(text = sprintf("If the number of defective items out of %d sampled is <= %d, accept the lot. Reject otherwise.", 
                                                 plan_vars$n, plan_vars$c),
                                  dependencies = depend_variables, position = positionInContainer + 2)
    description$position <- 2                                              
    jaspResults[["description"]] <- description
  }
}