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
  # Error handling for AQL/RQL
  if (options$pd_prp >= options$pd_crp) {
    if (is.null(jaspResults[["pd_error"]])) {
      pd_error <- createJaspTable(title = "", dependencies = c("pd_prp", "pd_crp"), position = 1)
      pd_error$setError(sprintf("Error: AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
      jaspResults[["pd_error"]] <- pd_error
      return ()
    }    
  }
  # Error handling for Producer's and Consumer's Risk
  if ((1 - options$pa_prp) <= options$pa_crp) {
    if (is.null(jaspResults[["pa_error"]])) {
      pa_error <- createJaspTable(title = "", dependencies = c("pa_prp", "pa_crp"), position = 2)
      pa_error$setError(sprintf("Error: 1 - α (Producer's risk) has to be greater than β (consumer's risk)."))
      jaspResults[["pa_error"]] <- pa_error
      return ()
    }    
  }

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
  risk_vars <- c("pd_prp", "pa_prp", "pd_crp", "pa_crp")
  pd_vars <- c("pd_lower", "pd_upper", "pd_step")
  output_vars <- c("showSummary", "showOCCurve", "showAOQCurve", "showATICurve")
  
  # Plan Dataframe
  df_plan <- data.frame(PD = pd, PA = oc_var@paccept)
  
  # Output
  # 0. Variable plan table
  .variablePlanTable(jaspResults, sd, risk_vars, n, k, pos=1)

  if (is.null(jaspResults[["decision_info"]])) {    
    plan_op <- createJaspHtml(text = sprintf("%s\n\n%s", "Z.LSL = (mean - LSL) / historical standard deviation", "Accept lot if Z.LSL >= k, otherwise reject."), 
                              dependencies = c(risk_vars, "sd"), position = 2)
    jaspResults[["decision_info"]] <- plan_op
  }

  # 1. Plan summary
  if (options$showSummary) {
    getSummary(jaspResults, pos=3, c(risk_vars, pd_vars, output_vars[1]), df_plan)
  }
  # 2. OC Curve
  if (options$showOCCurve) {
    getOCCurve(jaspResults, pos=4, c(risk_vars, pd_vars, output_vars[2]), df_plan)
  }
  # 3. AOQ Curve
  if (options$showAOQCurve) {
    if (is.null(jaspResults[["aoqCurveVariable"]])) {
      aoqCurve <- createJaspPlot(title = paste0("AOQ (Average Outgoing Quality) curve"),  width = 480, height = 320)
      aoqCurve$dependOn(c(risk_vars, pd_vars, output_vars[3], "lotSize"))
      jaspResults[["aoqCurveVariable"]] <- aoqCurve
      df_plan$AOQ <- df_plan$PA * pd * (N-n) / N
      aoq_max <- max(df_plan$AOQ)
      pd_aoq_max <- df_plan$PD[df_plan$AOQ == max(df_plan$AOQ)]
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1.1*aoq_max))
      aoq_plot <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = AOQ)) + 
                         ggplot2::geom_point(colour = "black", shape = 19) + ggplot2::labs(x = "Proportion non-conforming AOQVAR", y = "Average Outgoing Quality") +
                         ggplot2::geom_line(colour = "black", linetype = "dashed") +
                         ggplot2::geom_hline(yintercept = aoq_max, linetype = "dotted") +
                         ggplot2::annotate("text", label = sprintf("AOQL: %.2f", aoq_max), 
                                            x = max(0.09, pd_aoq_max*0.9), y = aoq_max*1.1, color = "black", size = 6) +
                         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
                         #  ggplot2::ylim(0.0,round(aoq_max*1.2, 2))
      # aoq_plot <- aoq_plot + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
      aoq_plot$position <- 5
      aoqCurve$plotObject <- aoq_plot
    }
  }
  # 4. ATI Curve
  if (options$showATICurve) {
    if (is.null(jaspResults[["atiCurveVariable"]])) {
      atiCurve <- createJaspPlot(title = paste0("ATI (Average Total Inspection) curve"),  width = 480, height = 320)
      atiCurve$dependOn(c(risk_vars, pd_vars, output_vars[4], "lotSize"))
      jaspResults[["atiCurveVariable"]] <- atiCurve
      df_plan$ATI <- df_plan$PA * n + (1 - df_plan$PA) * N
      ati_plot <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = ATI)) + 
                      ggplot2::geom_point(colour = "black", shape = 19) +
                      ggplot2::geom_line(colour = "black", linetype = "dashed") +
                      ggplot2::labs(x = "Proportion non-conforming ATIVAR", y = "Average Total Inspection")
      # ati_plot <- ati_plot + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
      ati_plot$position <- 6
      atiCurve$plotObject <- ati_plot
    }
  }
}

# txt = "Create table for the variable plan."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##             Create table for the variable plan.             --
##---------------------------------------------------------------
#' @param jaspResults <>
#' @param sd <>
#' @param depend_vars <>
#' @param n <>
#' @param k <>
#' @param pos <>
#' @returns <>
#' @seealso
#'   [()] for <>
#' @examples
#' .variablePlanTable(jaspResults, sd, depend_vars, n, k, pos)
.variablePlanTable <- function(jaspResults, sd, depend_vars, n, k, pos) {
  if (!is.null(jaspResults[["plan_table"]])) {
    return ()
  }
  plan_table <- createJaspTable(title = paste0("Variable Sampling Plan (Standard deviation assumed to be ", sd, ")"))
  plan_table$transpose <- TRUE
  plan_table$transposeWithOvertitle <- FALSE
  plan_table$dependOn(c(depend_vars, "sd"))
  plan_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row
  plan_table$addColumnInfo(name = "col_1", title = "Sample size", type = "integer")
  plan_table$addColumnInfo(name = "col_2", title = "Critical Distance (k)", type = "number")
  plan_table$addRows(list("col_1" = n, "col_2" = k))
  plan_table$showSpecifiedColumnsOnly <- TRUE
  plan_table$position <- pos
  jaspResults[["plan_table"]] <- plan_table
}