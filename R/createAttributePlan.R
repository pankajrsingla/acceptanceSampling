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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

# txt = "Create attribute plan"
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##                    Create attribute plan                    --
##---------------------------------------------------------------
#' @param jaspResults <>
#' @param dataset <>
#' @param options <>
#' @returns <>
#' @seealso
#'   [()] for <>
#' @examples
#' CreateAttributePlan(jaspResults, dataset, options)
##---------------------------------------------------------------
CreateAttributePlan <- function(jaspResults, dataset = NULL, options, ...) {
  risk_vars <- c("aql", "prod_risk", "rql", "cons_risk")
  pd_vars <- c("pd_lower", "pd_upper", "pd_step")
  depend_vars <- c(pd_vars, risk_vars, "lotSize", "distribution")

  # Check if the container already exists
  if (is.null(jaspResults[["createContainer"]]) || jaspResults[["createContainer"]]$getError()) {
    createContainer <- createJaspContainer(title = "")
    createContainer$dependOn(depend_vars)
    jaspResults[["createContainer"]] <- createContainer
  } else {
    createContainer <- jaspResults[["createContainer"]]
  }

  # Plan table outline
  plan_table <- createJaspTable(title = "Generated Sampling Plan")
  plan_table$addColumnInfo(name = "col_1", title = "", type = "string")
  plan_table$addColumnInfo(name = "col_2", title = "Value", type = "integer")
  plan_table[["col_1"]] <- c("Sample size", "Acceptance number")
  plan_table$position <- 1
  createContainer[["findPlanTable"]] <- plan_table

  # Probability table outline
  prob_table <- createJaspTable(title = "Acceptance probabilities at AQL and RQL")
  prob_table$addColumnInfo(name = "col_1", title = "", type = "string")
  prob_table$addColumnInfo(name = "col_2", title = "Proportion Non-conforming", type = "number")
  prob_table$addColumnInfo(name = "col_3", title = "Acceptance Probability", type = "number")
  prob_table$addColumnInfo(name = "col_4", title = "Rejection Probability", type = "number")
  prob_table[["col_1"]] <- c("AQL", "RQL")
  prob_table$position <- 2
  createContainer[["findProbTable"]] <- prob_table
  
  # Error handling for hypergeometric distribution
  aql <- round(options$aql, 3)
  rql <- round(options$rql, 3)
  checkHypergeom(createContainer, pd_vars, options, type="", aql, rql)
  if (createContainer$getError()) {
    return ()
  }
  
  # Error handling for AQL/RQL
  if (aql >= rql) {
    createContainer$setError(sprintf("Error: AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }

  # Error handling for Producer's and Consumer's Risk
  pa_prod <- round((1 - options$prod_risk), 3)
  pa_cons <- round(options$cons_risk, 3)
  if (pa_prod <= pa_cons) {
    createContainer$setError(sprintf("Error: 1 - α (Producer's risk) has to be greater than β (consumer's risk)."))
    return ()
  }
  .findPlan(createContainer, options, depend_vars, aql, rql, pa_prod, pa_cons)
}

# txt = "Find the sampling plan that satisfies the specified AQL and RQL constraints."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------------------------
##  Find the sampling plan that satisfies the specified AQL and RQL constraints.  --
##----------------------------------------------------------------------------------
#' @param jaspContainer <>
#' @param options <>
#' @param depend_vars <>
#' @param aql <>
#' @param pa_prod <>
#' @param rql <>
#' @param pa_cons <>
#' @returns <>
#' @seealso
#'   [()] for <>
#' @examples
#' .findPlan(jaspContainer, options, depend_vars, aql, rql, pa_prod, pa_cons)
##----------------------------------------------------------------------------------
.findPlan <- function(jaspContainer, options, depend_vars, aql, rql, pa_prod, pa_cons) {
  pd_lower <- options$pd_lower
  pd_upper <- options$pd_upper
  pd_step <- options$pd_step
  pd <- seq(pd_lower, pd_upper, pd_step)
  pd <- c(pd, aql, rql)
  pd <- round(pd, 3)
  pd <- pd[!duplicated(pd)]
  pd <- sort(pd)
  dist <- options$distribution
  plan_values <- NULL
  plan <- NULL
  
  # # Create sampling plan with the specified values
  if (dist == "hypergeom") {
    # Need to provide the lot size (N) for hypergeometric distribution.
    plan_values <- AcceptanceSampling::find.plan(PRP = c(aql, pa_prod), CRP = c(rql, pa_cons), type = dist, N = options$lotSize)
    plan <- AcceptanceSampling::OC2c(N = options$lotSize, n = plan_values$n, c = plan_values$c, r = plan_values$r, type = dist, pd = pd)
  } else {
    # Binomial and Poisson distributions don't require lot size (N) or standard deviation.
    plan_values <- AcceptanceSampling::find.plan(PRP = c(aql, pa_prod), CRP = c(rql, pa_cons), type = dist)
    plan <- AcceptanceSampling::OC2c(n = plan_values$n, c = plan_values$c, r = plan_values$r, type = dist, pd = pd)
  }
  n <- plan_values$n
  c <- plan_values$c
  r <- plan_values$r
  
  df_plan <- data.frame(PD = pd, PA = plan@paccept)
  df_plan <- na.omit(df_plan)
  if (nrow(df_plan) == 0) {
    jaspContainer$setError(sprintf("Error: No valid values found in the plan. Check the inputs."))
    return ()
  }
  .attributePlanTable(jaspContainer, depend_vars, aql, df_plan$PA[df_plan$PD == aql], rql, df_plan$PA[df_plan$PD == rql], n, c, r)

  # Summary
  if (options$showSummary) {
    getSummary(jaspContainer, pos=4, c(depend_vars, "showSummary"), df_plan)
  }

  # OC Curve
  if (options$showOCCurve) {
    getOCCurve(jaspContainer, pos=5, c(depend_vars, "showOCCurve"), df_plan)
  }

  # AOQ Curve (for plans with rectification)
  if (options$showAOQCurve) {
    getAOQCurve(jaspContainer, pos=6, "showAOQCurve", df_plan, options, "", n)
    if (jaspContainer$getError()) {
      return ()
    }
  }

  # ATI Curve (for plans with rectification)
  if (options$showATICurve) {
    getATICurve(jaspContainer, pos=7, "showATICurve", df_plan, options, "", n)
    if (jaspContainer$getError()) {
      return ()
    }
  }
}

# txt = "Create and fill the output table(s)."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##             Create and fill the output table(s).             --
##----------------------------------------------------------------
#' @param jaspContainer <>
#' @param depend_vars <>
#' @param aql <>
#' @param pa_prod <>
#' @param rql <>
#' @param pa_cons <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @returns <>
#' @seealso
#'   [()] for <>
#' @examples
#' .attributePlanTable(jaspContainer, depend_vars, plan_values, df_plan)
##----------------------------------------------------------------
.attributePlanTable <- function(jaspContainer, depend_vars, aql, pa_prod, rql, pa_cons, n, c, r) {
  # Simple table with sample size and acc. number
  plan_table <- jaspContainer[["findPlanTable"]]
  plan_table[["col_2"]] <- c(n, c)

  # Table with acceptance and rejection probabilities for AQL, RQL
  prob_table <- jaspContainer[["findProbTable"]]
  prob_table[["col_2"]] <- c(aql, rql)
  prob_table[["col_3"]] <- c(pa_prod, pa_cons)
  prob_table[["col_4"]] <- c(1-pa_prod, 1-pa_cons)

  # Description of the sampling plan:
  if (is.null(jaspContainer[["description"]])) {
    description <- createJaspHtml(text = sprintf("If the number of defective items out of %d sampled is <= %d, accept the lot. Reject otherwise.", n, c), position = 3)
    jaspContainer[["description"]] <- description                        
  }
}