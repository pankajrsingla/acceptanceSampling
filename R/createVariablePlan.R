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

createVariablePlan <- function(jaspResults, dataset = NULL, options, ...) {
  sd <- "unknown"
  sd_value <- 0
  if (options$sd) {
    sd <- "known"
    sd_value <- options$stdev
  }
  pd_lower <- options$pd_lower
  pd_upper <- options$pd_upper
  pd_step <- options$pd_step
  pd <- seq(pd_lower, pd_upper, pd_step)
  var_plan <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, 1-options$pa_prp), CRP = c(options$pd_crp, options$pa_crp), type = "normal", s.type = sd)
  oc_var <- AcceptanceSampling::OCvar(n = var_plan$n, k = var_plan$k, type = "normal", s.type = sd, pd = pd)
  df_plan <- data.frame(PD = pd, PA = oc_var@paccept)
}

.handleVariablePlan <- function(jaspResults, options, planType) {
  # plan_variables <- paste0(c("lotSize", "sampleSize", "acceptNumber", "rejectNumber", "distribution"), planType)
  # pd_variables <- paste0(c("pd_lower", "pd_upper", "pd_step"), planType)
  # risk_variables <- paste0(c("pd_prp", "pa_prp", "pd_crp", "pa_crp"), planType)
  # output_variables <- paste0(c("showOCCurve", "showSummary", "assessPlan", "showAOQCurve", "showATICurve", "showASNCurve"), planType)
  
  # # Plan Dataframe
  # df_plan <- getPlanDf(options, planType, c(plan_variables, pd_variables))

  # # OC Curve
  # if (options[[paste0("showOCCurve", planType)]]) {
  #   getOCCurve(jaspResults, df_plan, planType, c(plan_variables, pd_variables, output_variables[1]))
  # }
  # # Plan summary
  # if (options[[paste0("showSummary", planType)]]) {
  #   getSummary(jaspResults, df_plan, planType, c(plan_variables, pd_variables, output_variables[2]))
  # }
  # # Assess plan
  # if (options[[paste0("assessPlan", planType)]]) {
  #   assessPlan(jaspResults, options, planType, c(plan_variables, risk_variables, output_variables[3]))
  # }
  # # AOQ Curve
  # if (options[[paste0("showAOQCurve", planType)]]) {
  #   getAOQCurve(jaspResults, df_plan, options, planType, c(plan_variables, pd_variables, output_variables[4]))
  # }
  # # ATI Curve
  # if (options[[paste0("showATICurve", planType)]]) {
  #   getATICurve(jaspResults, df_plan, options, planType, c(plan_variables, pd_variables, output_variables[5]))
  # }
  # # ASN Curve (only for multiple sampling plan)
  # if (options[["showASNCurveMult"]]) {
  #   getASNCurve(jaspResults, options, c(plan_variables, pd_variables, output_variables[6]))
  # }
}