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

# txt = "Create variable plan."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##                    Create variable plan.                    --
##---------------------------------------------------------------
#' @param jaspResults <>
#' @param dataset <>
#' @param options <>
#' @returns <>
#' @seealso
#'   [()] for <>
#' @examples
#' CreateVariablePlan(jaspResults, dataset, options)
CreateVariablePlan <- function(jaspResults, dataset = NULL, options, ...) {
  # Dependency variables
  risk_vars <- c("pd_prp", "pa_prp", "pd_crp", "pa_crp")
  pd_vars <- c("pd_lower", "pd_upper", "pd_step")
  
  varContainer <- createJaspContainer(title = "Variable Sampling Plan")
  varContainer$dependOn(risk_vars) # Common dependencies
  jaspResults[["varContainer"]] <- varContainer
  
  # Information
  plan_op <- createJaspHtml(text = sprintf("%s\n\n%s", "Z.LSL = (mean - LSL) / historical standard deviation", "Accept lot if Z.LSL >= k, otherwise reject."))                              
  plan_op$position <- 1                              
  varContainer[["decision_info"]] <- plan_op

  sd <- "unknown"
  # sd_value <- 0
  if (options$sd) {
    sd <- "known"
    # sd_value <- options$stdev
  }

  plan_table <- createJaspTable(title = gettextf("Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>)", sd))
  plan_table$transpose <- TRUE
  plan_table$transposeWithOvertitle <- FALSE
  plan_table$dependOn(c(risk_vars, "sd"))
  plan_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  plan_table$addColumnInfo(name = "col_1", title = "Sample size", type = "integer")
  plan_table$addColumnInfo(name = "col_2", title = "Critical Distance (k)", type = "number")
  plan_table$showSpecifiedColumnsOnly <- TRUE
  plan_table$position <- 2
  varContainer[["plan_table"]] <- plan_table
  
  # Error handling for AQL/RQL
  if (options$pd_prp >= options$pd_crp) {
    varContainer$setError(sprintf("Error: AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
    return ()
  }
  # Error handling for Producer's and Consumer's Risk
  if ((1 - options$pa_prp) <= options$pa_crp) {
    varContainer$setError(sprintf("Error: 1 - α (Producer's risk) has to be greater than β (consumer's risk)."))
    return ()
  }

  N <- options$lotSizeSingle
  pd_lower <- options$pd_lower
  pd_upper <- options$pd_upper
  pd_step <- options$pd_step
  checkPdErrors(varContainer, pd_lower, pd_upper, pd_step)
  if (varContainer$getError()) {
    return ()
  }
  pd <- seq(pd_lower, pd_upper, pd_step)
  pd <- c(pd, options$pd_prp, options$pd_crp)
  pd <- sort(pd)
  pd <- round(pd, 3)
  pd <- pd[!duplicated(pd)]
  var_plan <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, 1-options$pa_prp), CRP = c(options$pd_crp, options$pa_crp), type = "normal", s.type = sd)
  n <- var_plan$n
  k <- var_plan$k
  oc_var <- AcceptanceSampling::OCvar(n = n, k = k, type = "normal", s.type = sd, pd = pd)

  # Error checking
  if (N < n) {
    varContainer$setError(sprintf("Error: Invalid input. Lot size (N = %d) cannot be smaller than the sample size (n = %d) of the generated variable plan.", N, n))
    return ()
  }
  
  # Plan Dataframe
  df_plan <- data.frame(PD = pd, PA = round(oc_var@paccept, 3))
  
  # Output
  output_vars <- c("showSummary", "showOCCurve", "showAOQCurve", "showATICurve")

  # 0. Variable plan table
  plan_table$addRows(list("col_1" = n, "col_2" = k))
  # .variablePlanTable(varContainer, sd, risk_vars, n, k, pos=2)

  # 1. Plan summary
  if (options$showSummary) {
    getSummary(varContainer, pos=3, c(pd_vars, output_vars[1]), df_plan)
  }
  # 2. OC Curve
  if (options$showOCCurve) {
    getOCCurve(varContainer, pos=4, c(pd_vars, output_vars[2]), df_plan)
  }
  # 3. AOQ Curve
  if (options$showAOQCurve) {
    getAOQCurve(varContainer, pos=5, c(pd_vars, output_vars[3], "lotSizeSingle"), df_plan, options, "Single", n)    
  }
  # 4. ATI Curve
  if (options$showATICurve) {
    getATICurve(varContainer, pos=6, c(pd_vars, output_vars[4], "lotSizeSingle"), df_plan, options, "Single", n)
  }
}

# txt = "Create table for the variable plan."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##             Create table for the variable plan.             --
##---------------------------------------------------------------
#' @param jaspContainer <>
#' @param sd <>
#' @param depend_vars <>
#' @param n <>
#' @param k <>
#' @param pos <>
#' @returns <>
#' @seealso
#'   [()] for <>
#' @examples
#' .variablePlanTable(jaspContainer, sd, depend_vars, n, k, pos)
# .variablePlanTable <- function(jaspContainer, sd, depend_vars, n, k, pos) {
  # if (!is.null(jaspContainer[["plan_table"]])) {
  #   return ()
  # }
  # plan_table <- createJaspTable(title = gettextf("Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>)", sd))
  # plan_table$transpose <- TRUE
  # plan_table$transposeWithOvertitle <- FALSE
  # plan_table$dependOn(c(depend_vars, "sd"))
  # plan_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  # plan_table$addColumnInfo(name = "col_1", title = "Sample size", type = "integer")
  # plan_table$addColumnInfo(name = "col_2", title = "Critical Distance (k)", type = "number")
  # plan_table$addRows(list("col_1" = n, "col_2" = k))
  # plan_table$showSpecifiedColumnsOnly <- TRUE
  # plan_table$position <- pos
  # jaspContainer[["plan_table"]] <- plan_table
# }