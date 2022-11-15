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

CreateVariablePlan <- function(jaspResults, dataset = NULL, options, ...) {
  N <- options$lotSize
  sd <- "unknown"
  # sd_value <- 0
  if (options$sd) {
    sd <- "known"
    # sd_value <- options$stdev
  }
  pd_lower <- options$pd_lower
  pd_upper <- options$pd_upper
  pd_step <- options$pd_step
  pd <- seq(pd_lower, pd_upper, pd_step)
  var_plan <- AcceptanceSampling::find.plan(PRP = c(options$pd_prp, 1-options$pa_prp), CRP = c(options$pd_crp, options$pa_crp), type = "normal", s.type = sd)
  n <- var_plan$n
  k <- var_plan$k
  oc_var <- AcceptanceSampling::OCvar(n = n, k = k, type = "normal", s.type = sd, pd = pd)

  # Dependency variables
  risk_variables <- c("pd_prp", "pa_prp", "pd_crp", "pa_crp")
  pd_variables <- c("pd_lower", "pd_upper", "pd_step")
  output_variables <- c("showOCCurve", "showSummary", "showAOQCurve", "showATICurve")
  
  # Plan Dataframe
  df_plan <- data.frame(PD = pd, PA = oc_var@paccept)
  
  # Output
  # 0. Variable plan table
  .variablePlanTable(jaspResults, sd, risk_variables, n, k, positionInContainer=1)

  if (is.null(jaspResults[["decision_info"]])) {    
    plan_op <- createJaspHtml(text = gettextf("%s\n\n%s", "Z.LSL = (mean - LSL) / historical standard deviation", "Accept lot if Z.LSL >= k, otherwise reject."), 
                              dependencies = c(risk_variables, "sd"), position = 2)
    jaspResults[["decision_info"]] <- plan_op
  }
  
  # 1. OC Curve
  if (options$showOCCurve) {
    getOCCurve(jaspResults, df_plan, "", c(risk_variables, pd_variables, output_variables[1]), positionInContainer=4)
  }
  # 2. Plan summary
  if (options$showSummary) {
    getSummary(jaspResults, df_plan, "", c(risk_variables, pd_variables, output_variables[2]), positionInContainer=3)
  }
  # 3. AOQ Curve
  if (options$showAOQCurve) {
    if (is.null(jaspResults[["aoqCurveVariable"]])) {
      aoqCurve <- createJaspPlot(title = paste0("AOQ (Average Outgoing Quality) curve"),  width = 480, height = 320)
      aoqCurve$dependOn(c(risk_variables, pd_variables, output_variables[3], "lotSize"))
      jaspResults[["aoqCurveVariable"]] <- aoqCurve
      df_plan$AOQ <- df_plan$PA * pd * (N-n) / N
      aoq_max <- round(max(df_plan$AOQ),2)
      pd_aoq_max <- df_plan$PD[df_plan$AOQ == max(df_plan$AOQ)]
      plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = AOQ)) + 
                            ggplot2::geom_point(colour = "black", shape = 19) + ggplot2::labs(x = "Proportion non-confirming", y = "AOQ") +
                            ggplot2::geom_line(colour = "black", linetype = "dashed") +
                            ggplot2::geom_hline(yintercept = max(df_plan$AOQ), linetype = "dashed") +
                            ggplot2::annotate("text", label = gettextf("AOQL: %.2f", aoq_max), x = pd_aoq_max*0.9, y = aoq_max*1.1, color = "black", size = 6)
                            ggplot2::ylim(0,aoq_max+0.01)
      plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
      plt$position <- 5
      aoqCurve$plotObject <- plt
    }
  }
  # 4. ATI Curve
  if (options$showATICurve) {
    if (is.null(jaspResults[["atiCurveVariable"]])) {
      atiCurve <- createJaspPlot(title = paste0("ATI (Average Total Inspection) curve"),  width = 480, height = 320)
      atiCurve$dependOn(c(risk_variables, pd_variables, output_variables[4], "lotSize"))
      jaspResults[["atiCurveVariable"]] <- atiCurve
      df_plan$ATI <- df_plan$PA * n + (1 - df_plan$PA) * N
      plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = ATI)) + 
                      ggplot2::geom_point(colour = "black", shape = 19) + ggplot2::labs(x = "Proportion non-confirming", y = "ATI") +
                      ggplot2::geom_line(colour = "black", linetype = "dashed")
      plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
      plt$position <- 6
      atiCurve$plotObject <- plt
    }
  }
}

# Create table for the variable plan
.variablePlanTable <- function(jaspResults, sd, depend_variables, n, k, positionInContainer) {
  if (!is.null(jaspResults[["plan_table"]])) {
    return ()
  }
  plan_table <- createJaspTable(title = paste0("Variable Sampling Plan (Standard deviation assumed to be ", sd, ")"))
  plan_table$transpose <- TRUE
  plan_table$transposeWithOvertitle <- FALSE
  plan_table$dependOn(c(depend_variables, "sd"))
  plan_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  plan_table$addColumnInfo(name = "col_1", title = "Sample size", type = "integer")
  plan_table$addColumnInfo(name = "col_2", title = "Critical Distance (k)", type = "number")
  plan_table$addRows(list("col_1" = n, "col_2" = k))
  plan_table$showSpecifiedColumnsOnly <- TRUE
  plan_table$position <- positionInContainer
  jaspResults[["plan_table"]] <- plan_table
}