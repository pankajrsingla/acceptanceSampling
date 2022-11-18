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

AnalyzeAttributePlan <- function(jaspResults, dataset = NULL, options, ...) {
  # Single sampling plan
  if ((options$sampleSizeSingle > 0) && (options$acceptNumberSingle > 0)) {
    .handleAttributePlan(jaspResults, options, "Single")
  }

  # Multiple sampling plan
  if (length(options$stages) > 1) {
  # if (length(options$sampleSizeMult) > 0 && length(options$acceptNumberMult) > 0 && length(options$rejectNumberMult > 0)) {
    .handleAttributePlan(jaspResults, options, "Mult")
  }
}

# txt = "Analyze attribute plans"
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##                   Analyze attribute plans                   --
##---------------------------------------------------------------
#' @param jaspResults <>.
#' @param options User specified options.
#' @param planType "Single" or "Mult".
#' @returns <>.
#' @seealso
#'   [()] for <>.
#' @examples
#' .handleAttributePlan(jaspResults, dataset, options)
.handleAttributePlan <- function(jaspResults, options, planType) {
  plan_variables <- paste0(c("lotSize", "distribution"), planType)
  if (planType != "Mult") {
    plan_variables <- c(plan_variables, paste0(c("sampleSize", "acceptNumber", "rejectNumber"), planType))
  } else {
    plan_variables <- c(plan_variables, "stages")
  }
  pd_variables <- paste0(c("pd_lower", "pd_upper", "pd_step"), planType)
  risk_variables <- paste0(c("pd_prp", "pa_prp", "pd_crp", "pa_crp"), planType)
  output_variables <- paste0(c("showOCCurve", "showSummary", "assessPlan", "showAOQCurve", "showATICurve", "showASNCurve"), planType)
  
  # Plan Dataframe
  df_plan <- getPlanDf(options, planType, FALSE)

  # OC Curve
  if (options[[paste0("showOCCurve", planType)]]) {
    getOCCurve(jaspResults, df_plan, planType, c(plan_variables, pd_variables, output_variables[1]), positionInContainer=4)
  }
  # Plan summary
  if (options[[paste0("showSummary", planType)]]) {
    getSummary(jaspResults, df_plan, planType, c(plan_variables, pd_variables, output_variables[2]), positionInContainer=3)
  }
  # Assess plan
  if (options[[paste0("assessPlan", planType)]]) {
    assessPlan(jaspResults, options, planType, c(plan_variables, risk_variables, output_variables[3]), positionInContainer=1)
  }
  # AOQ Curve
  if (options[[paste0("showAOQCurve", planType)]]) {
    getAOQCurve(jaspResults, df_plan, options, planType, c(plan_variables, pd_variables, output_variables[4]), positionInContainer=5)
  }
  # ATI Curve
  if (options[[paste0("showATICurve", planType)]]) {
    getATICurve(jaspResults, df_plan, options, planType, c(plan_variables, pd_variables, output_variables[5]), positionInContainer=6)
  }
  # ASN Curve (only for multiple sampling plan)
  if (options[["showASNCurveMult"]]) {
    getASNCurve(jaspResults, options, c(plan_variables, pd_variables, output_variables[6]), positionInContainer=7)
  }
}