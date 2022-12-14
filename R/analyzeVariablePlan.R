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

# txt = "Analyze variable plan."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##                    Analyze variable plan.                    --
##----------------------------------------------------------------
#' @param jaspResults <>
#' @param dataset <>
#' @param options <>
#' @returns <>
#' @seealso
#'   [()] for <>
#' @examples
#' CreateVariablePlan(jaspResults, dataset, options)
AnalyzeVariablePlan <- function(jaspResults, dataset = NULL, options, ...) {
  # Dependency variables
  plan_vars <- c("sampleSize", "kValue")
  
  # Check if the container already exists. Create it if it doesn't.
  if (is.null(jaspResults[["analyzeVarContainer"]]) || jaspResults[["analyzeVarContainer"]]$getError()) {
    analyzeVarContainer <- createJaspContainer(title = "Analyze Variable Plan")
    analyzeVarContainer$dependOn(plan_vars) # Common dependencies
    jaspResults[["analyzeVarContainer"]] <- analyzeVarContainer
  } else {
    analyzeVarContainer <- jaspResults[["analyzeVarContainer"]]
  }
  # Plan variables
  N <- options$lotSize
  n <- options$sampleSize
  k <- options$kValue
  sd <- "unknown"
  if (options$sd) {
    sd <- "known"
  }
  
  # Initialize the plan table
  plan_table <- createJaspTable(title = gettextf("Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>)", sd))
  plan_table$transpose <- TRUE
  plan_table$transposeWithOvertitle <- FALSE
  plan_table$dependOn(c(plan_vars, "sd"))
  plan_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  plan_table$addColumnInfo(name = "col_1", title = "Sample size", type = "integer")
  plan_table$addColumnInfo(name = "col_2", title = "Critical Distance (k)", type = "number")
  plan_table$addRows(list("col_1" = n, "col_2" = k))
  plan_table$showSpecifiedColumnsOnly <- TRUE
  plan_table$position <- 1
  analyzeVarContainer[["plan_table"]] <- plan_table

  # Error check for n (sample size)
  if (!options$sd && (n <= 1)) {
    analyzeVarContainer$setError(sprintf("Error: Invalid input. If historical standard deviation is unknown, sample size has to be > 1."))
    return ()
  }

  # Error check for N (lot size)
  if (N < n) {
    analyzeVarContainer$setError(sprintf("Error: Invalid input. Lot size (N = %.0f) cannot be smaller than the sample size (n = %.0f) of the generated variable plan.", N, n))
    return ()
  }

  plan <- getPlan(analyzeVarContainer, options, "", n, k=k, sd=sd)
  if (analyzeVarContainer$getError()) {
    return ()
  }

  # Plan dataframe
  df_plan <- plan$df_plan
  oc_plan <- plan$oc_plan
    
  risk_vars <- c("aql", "prod_risk", "rql", "cons_risk")
  pd_vars <- c("pd_lower", "pd_upper", "pd_step")

  # 1. Assess plan
  if (options$assessPlan) {    
    assessPlan(analyzeVarContainer, pos=2, c(risk_vars, pd_vars, "assessPlan"), oc_plan, options, "")
    if (analyzeVarContainer$getError()) {
      return ()
    }
  }
  
  # 2. Plan summary
  if (options$showSummary) {
    getSummary(analyzeVarContainer, pos=4, c(pd_vars, "showSummary"), df_plan)
  }
  # 3. OC Curve
  if (options$showOCCurve) {
    getOCCurve(analyzeVarContainer, pos=5, c(pd_vars, "showOCCurve"), df_plan)
  }
  # 4. AOQ Curve
  if (options$showAOQCurve) {
    getAOQCurve(analyzeVarContainer, pos=6, c(pd_vars, "showAOQCurve", "lotSize"), df_plan, options, "", n) 
    if (analyzeVarContainer$getError()) {
      return ()
    }
  }
  # 5. ATI Curve
  if (options$showATICurve) {
    getATICurve(analyzeVarContainer, pos=7, c(pd_vars, "showATICurve", "lotSize"), df_plan, options, "", n)
  }
}