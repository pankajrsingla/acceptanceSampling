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
#' @param jaspResults <>
#' @param dataset <>
#' @param options <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' AnalyzeAttributePlan(jaspResults, dataset, options)
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
    .handleAttributePlan(singleContainer, position=0, plan_vars_single, pd_vars_single, options, "Single")
  }

  # Multiple sampling plan
  if (length(options$stages) > 1) {
    plan_vars_mult <- paste0(plan_variables, "Mult")
    plan_vars_mult <- c(plan_vars_mult, "stages")
    pd_vars_mult <- paste0(pd_variables, "Mult")
    multContainer <- createJaspContainer(title = "Multiple Sampling Plan")
    # Common dependencies for all multiple plans
    multContainer$dependOn(c(plan_vars_mult, pd_vars_mult))
    jaspResults[["multContainer"]] <- multContainer
    .handleAttributePlan(multContainer, position=100, plan_vars_mult, pd_vars_mult, options, "Mult")
  }
}

# txt = "Analyze attribute plans"
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##                   Analyze attribute plans                   --
##---------------------------------------------------------------
#' @param jaspContainer <>
#' @param position <>
#' @param plan_variables <>
#' @param pd_variables <>
#' @param options <>
#' @param type <>
#' @seealso
#'   [()] for <>
#' @examples
#' .handleAttributePlan(jaspContainer, position, plan_variables, pd_variables, options, type)
##---------------------------------------------------------------
.handleAttributePlan <- function(jaspContainer, position, plan_variables, pd_variables, options, type) {
  # Error handling for hypergeometric distribution
  # Todo: make this work.
  if (!checkHypergeom(jaspContainer, position, pd_variables, options, type)) {
    return ()
  }
  risk_vars <- paste0(c("pd_prp", "pa_prp", "pd_crp", "pa_crp"), type)
  output_vars <- paste0(c("assessPlan", "showSummary", "showOCCurve", "showAOQCurve", "showATICurve", "showASNCurve"), type)
  
  # Plan variables
  plan_vars <- getPlanVars(options, type)
  n <- unlist(plan_vars[1])
  c <- unlist(plan_vars[2])
  r <- unlist(plan_vars[3])

  # Plan data
  plan <- getPlan(options, type)
  df_plan <- unlist(plan[1])
  oc_plan <- unlist(plan[2])

  # Assess plan
  if (options[[output_variables[3]]]) {
    assessPlan(jaspContainer, pos=position+1, c(output_vars[1], risk_vars), oc_plan, options, type, n, c, r)
  }

  # Summary table
  if (options[[output_variables[2]]]) {
    getSummary(jaspContainer, pos=position+3, output_vars[2], df_plan)
  }

  # OC Curve
  if (options[[output_variables[1]]]) {
    getOCCurve(jaspContainer, pos=position+4, output_vars[3], df_plan)
  }

  # AOQ Curve
  if (options[[output_variables[4]]]) {
    getAOQCurve(jaspContainer, pos=position+5, output_vars[4], df_plan, options, type, n, c, r)
  }

  # ATI Curve
  if (options[[output_variables[5]]]) {
    getATICurve(jaspContainer, pos=position+6, output_vars[5], df_plan, options, type, n, c, r)
  }

  # ASN Curve (only for multiple sampling plan)
  if (options[[output_variables[6]]]) {
    getASNCurve(jaspContainer, pos=position+7, output_vars[6], options, n, c, r)
  }
}