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

# txt = "Analyze single stage and multiple stage attribute plans"
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##   Analyze single stage and multiple stage attribute plans   --
##---------------------------------------------------------------
#' @param jaspResults <>.
#' @param dataset <>.
#' @param options "Single" or "Mult".
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' AnalyzeAttributePlan(jaspResults, dataset, optionss)
##---------------------------------------------------------------
AnalyzeAttributePlan <- function(jaspResults, dataset = NULL, options, ...) {
  plan_variables <- c("lotSize", "distribution")
  pd_variables <- c("pd_lower", "pd_upper", "pd_step")
  
  # Single sampling plan
  if ((options$sampleSizeSingle > 0) && (options$acceptNumberSingle >= 0)) {
    plan_vars_single <- c(plan_variables, "sampleSize", "acceptNumber", "rejectNumber")
    plan_vars_single <- paste0(plan_vars_single, "Single")
    pd_vars_single <- paste0(pd_variables, "Single")
    singleContainer <- createJaspContainer(title = "Single Sampling Plan")
    # Common dependencies for all single plans
    singleContainer$dependOn(c(plan_vars_single, pd_vars_single))
    jaspResults[["singleContainer"]] <- singleContainer
    .handleAttributePlan(singleContainer, options, plan_vars_single, pd_vars_single, "Single", position=0)
  }

  # Multiple sampling plan
  if (length(options$stages) > 1) {
    plan_vars_mult <- paste0(plan_variables, "Mult")
    plan_vars_mult <- c(plan_vars_mult, "stages")
    pd_vars_mult <- paste0(pd_variables, "Mult")
    multipleContainer <- createJaspContainer(title = "Multiple Sampling Plan")
    # Common dependencies for all multiple plans
    multipleContainer$dependOn(c(plan_vars_mult, pd_vars_mult))
    jaspResults[["multipleContainer"]] <- multipleContainer
    .handleAttributePlan(multipleContainer, options, plan_vars_mult, pd_vars_mult, "Mult", position=100)
  }
}

# txt = "Analyze attribute plans"
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##                   Analyze attribute plans                   --
##---------------------------------------------------------------
#' @param jaspContainer <>.
#' @param options User specified options.
#' @param plan_variables <>
#' @param pd_variables <>
#' @param planType "Single" or "Mult".
#' @param position <>
#' @returns <>.
#' @seealso
#'   [()] for <>.
#' @examples
#' .handleAttributePlan(jaspContainer, options, plan_variables, pd_variables, planType, position)
##---------------------------------------------------------------
.handleAttributePlan <- function(jaspContainer, options, plan_variables, pd_variables, planType, position) {
  if (options[[paste0("distribution", planType)]] == "hypergeom") {
    # Error handling for hypergeometric distribution
    pd_lower <- options[[pd_variables[1]]]
    pd_upper <- options[[pd_variables[2]]]
    pd_step <- options[[pd_variables[3]]]
    pd <- seq(pd_lower, pd_upper, pd_step)
    N <- options[[paste0("lotSize", planType)]]
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
      abs(x - round(x)) < tol
    }
    D <- N * pd
    if (!all(is.wholenumber(N), is.wholenumber(D))) {
      if (is.null(jaspContainer[["hypergeom_error"]])) {
        hypergeom_error <- createJaspTable(title = "", dependencies = paste0(c(pd_variables, "lotSize", "distribution"), planType), position = position)
        hypergeom_error$setError(sprintf("%s\n%s", "Error: Invalid input. Can not analyze plan. N*pd should be integer values.", "Check the values of N and pd."))
        jaspContainer[["hypergeom_error"]] <- hypergeom_error
        return ()
      }
    }
  }
  risk_variables <- paste0(c("pd_prp", "pa_prp", "pd_crp", "pa_crp"), planType)
  output_variables <- paste0(c("showOCCurve", "showSummary", "assessPlan", "showAOQCurve", "showATICurve", "showASNCurve"), planType)
  
  # Plan Dataframe
  df_plan <- getPlanDf(options, planType, FALSE)

  # Summary table
  if (options[[output_variables[2]]]) {
    getSummary(jaspContainer, df_plan, planType, output_variables[2], positionInContainer=position+3)
  }

  # OC Curve
  if (options[[output_variables[1]]]) {
    getOCCurve(jaspContainer, df_plan, planType, output_variables[1], positionInContainer=position+4)
  }

  # Assess plan
  if (options[[output_variables[3]]]) {
    assessPlan(jaspContainer, options, planType, c(risk_variables, output_variables[3]), positionInContainer=position+1)
  }

  # AOQ Curve
  if (options[[output_variables[4]]]) {
    getAOQCurve(jaspContainer, df_plan, options, planType, output_variables[4], positionInContainer=position+5)
  }

  # ATI Curve
  if (options[[output_variables[5]]]) {
    getATICurve(jaspContainer, df_plan, options, planType, output_variables[5], positionInContainer=position+6)
  }
  
  # ASN Curve (only for multiple sampling plan)
  if (options[[output_variables[6]]]) {
    getASNCurve(jaspContainer, options, output_variables[6], positionInContainer=position+7)
  }
}