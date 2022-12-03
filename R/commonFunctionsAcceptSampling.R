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

# txt = "Check if values specified for PD are valid."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##         Check if values specified for PD are valid.         --
##---------------------------------------------------------------
#' @param jaspContainer <>
#' @param pd_lower <>
#' @param pd_upper <>
#' @param pd_step <>
#' @returns <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' checkPdErrors(jaspContainer, pos, pd_vars, options, type)
##---------------------------------------------------------------
checkPdErrors <- function(jaspContainer, pd_lower, pd_upper, pd_step) {
  if (pd_lower > pd_upper) {
    jaspContainer$setError(sprintf("Error: Invalid input. Lower limit for PD needs to be smaller than the upper limit."))
    return ()
  }
  if (pd_step > (pd_upper - pd_lower)) {
    jaspContainer$setError(sprintf("Error: Invalid input. Step size for PD needs to be smaller than the difference between the upper and limits."))
  }
}

# txt = "Check if D*pd values for the hypergeomtric distribution are whole numbers."
# banner(txt, centre = TRUE, bandChar = "-")
##--------------------------------------------------------------------------------
##  Check if D*pd values for the hypergeomtric distribution are whole numbers.  --
##--------------------------------------------------------------------------------
#' @param jaspContainer <>
#' @param pd_vars <>
#' @param options <>
#' @param type <>
#' @returns <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' checkHypergeom(jaspContainer, pos, pd_vars, options, type)
##--------------------------------------------------------------------------------
checkHypergeom <- function(jaspContainer, pd_vars, options, type) {
  if (options[[paste0("distribution", type)]] == "hypergeom") {
    pd_lower <- options[[pd_vars[1]]]
    pd_upper <- options[[pd_vars[2]]]
    pd_step <- options[[pd_vars[3]]]
    checkPdErrors(jaspContainer, pd_lower, pd_upper, pd_step)
    if (jaspContainer$getError()) {
      return ()
    }
    pd <- seq(pd_lower, pd_upper, pd_step)

    # Function to check for whole numbers
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
      abs(x - round(x)) < tol
    }

    N <- options[[paste0("lotSize", type)]]
    D <- N * pd
    if (!all(is.wholenumber(N), is.wholenumber(D))) {
      jaspContainer$setError(sprintf("%s\n%s", "Error: Invalid input. For hypergeometric distribution, N*pd should be integer values.", "Check the values of N and pd."))
    }
  }
}

# txt = "Check for errors in single stage attribute sampling plan."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##  Check for errors in single stage attribute sampling plan.  --
##---------------------------------------------------------------
#' @param jaspContainer <>
#' @param N <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @returns <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' checkErrorsSinglePlan(jaspContainer, N, n, c, r)
##---------------------------------------------------------------
checkErrorsSinglePlan <- function(jaspContainer, N, n, c, r) {
  if (n > N) {
    jaspContainer$setError(sprintf("Error: Invalid input. Sample size (n) cannot be larger than the lot size (N)."))
    return ()
  }
  if (c > n) {
    jaspContainer$setError(sprintf("Error: Invalid input. Acceptance number (c) cannot be larger than the sample size (n)."))
    return ()
  }
  if (r > n) {
    jaspContainer$setError(sprintf("Error: Invalid input. Rejection number (r) cannot be larger than the sample size (n)."))
    return ()
  }
  if (r <= c) {
    jaspContainer$setError(sprintf("Error: Invalid input. Rejection number (r) has to be larger than the acceptance number (c)."))
  }
}

# txt = "Check for errors in multiple stage attribute sampling plan."
# banner(txt, centre = TRUE, bandChar = "-")
##-----------------------------------------------------------------
##  Check for errors in multiple stage attribute sampling plan.  --
##-----------------------------------------------------------------
#' @param jaspContainer <>
#' @param N <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @returns <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' checkErrorsMultiplePlan(jaspContainer, N, n, c, r)
##-----------------------------------------------------------------
checkErrorsMultiplePlan <- function(jaspContainer, N, n, c, r) {
  cum_n <- sum(n)
  cumsum_n <- cumsum(n)
  num_stages <- length(n)
  if (cum_n > N) {
    jaspContainer$setError(sprintf("Error: Invalid input. Cumulative sample size (n1+n2+...) cannot be larger than the lot size (N)."))
    return ()
  }
  if (r[num_stages] != c[num_stages] + 1) {
    jaspContainer$setError(sprintf("Error: Invalid input. Final rejection number (r) needs to be 1 more than the final acceptance number (c)."))
    return ()
  }
  if (any(c > cumsum_n)) {
    jaspContainer$setError(sprintf("Error: Invalid input. Acceptance number (c) cannot be larger than the sample size (n)."))
    return ()
  }
  if (any(r > cumsum_n)) {
    jaspContainer$setError(sprintf("Error: Invalid input. Rejection number (r) cannot be larger than the sample size (n)."))
    return ()
  }
  if (any(r <= c)) {
    jaspContainer$setError(sprintf("Error: Invalid input. Rejection number (r) for every stage has to be larger than the corresponding acceptance number (c)."))
    return ()
  }
  if (any(c[1:(num_stages-1)] > r[1:(num_stages-1)] - 2)) {
    # Check for r[i] > c[i] + 1 for i in 1:stages-1
    jaspContainer$setError(sprintf("%s\n%s", "Error: Invalid input. For all stages except the last stage, rejection numbers (r) have to be at at least 2 greater than the acceptance numbers (c).",
                                    "Else, subsequent stages become redundant."))
    return ()
  }
  if (any(c != sort(c))) {
    # Check for non-decreasing seqeuence of c
    jaspContainer$setError(sprintf("Error: Invalid input. Acceptance numbers (c) are cumulative, so they need to be in a non-decreasing sequence."))
    return ()
  }
  if (any(r != sort(r))) {
    # Check for non-decreasing seqeuence of r
    jaspContainer$setError(sprintf("Error: Invalid input. Rejection numbers (r) are cumulative, so they need to be in a non-decreasing sequence."))    
  }
}

# txt = "Return the plan variables - n, c, and r."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##           Return the plan variables - n, c, and r.           --
##----------------------------------------------------------------
#' @param jaspContainer <>
#' @param options <>
#' @param type <>
#' @returns <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' getPlanValues(options, type)
##----------------------------------------------------------------
getPlanValues <- function(jaspContainer, options, type) {
  n <- c <- r <- NULL
  N <- options[[paste0("lotSize", type)]]
  if (type == "Single") {
    # Single sampling plan
    n <- options[["sampleSizeSingle"]]
    c <- options[["acceptNumberSingle"]]
    r <- options[["rejectNumberSingle"]]
    # Error checking for single stage plan
    checkErrorsSinglePlan(jaspContainer, N, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  } else {
    # Multiple sampling plan
    stages <- options[["stages"]]
    for (i in 1:length(stages)) {
      n[i] <- stages[[i]]$sampleSizeMult
      c[i] <- stages[[i]]$acceptNumberMult
      r[i] <- stages[[i]]$rejectNumberMult
    }
    # Error checking for multiple stage plan
    checkErrorsMultiplePlan(jaspContainer, N, n, c, r)
    if (jaspContainer$getError()) {
      return ()
    }
  }
  return (list(n=n,c=c,r=r))
}

# txt = "Create and return a plan and its data"
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##            Create and return a plan and its data            --
##---------------------------------------------------------------
#' @param jaspContainer <>
#' @param options <>
#' @param type <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @returns <>
#' @seealso
#'   [()] for <>,
#'   [()] for <>
#' @examples
#' getPlan(jaspContainer, options, "Single", n, c, r)
#' getPlan(jaspContainer, options, "Mult", n, c, r)
##---------------------------------------------------------------
getPlan <- function(jaspContainer, options, type, n, c, r) {
  pd_lower <- options[[paste0("pd_lower", type)]]
  pd_upper <- options[[paste0("pd_upper", type)]]
  pd_step <- options[[paste0("pd_step", type)]]
  checkPdErrors(jaspContainer, pd_lower, pd_upper, pd_step)
  if (jaspContainer$getError()) {
    return ()
  }
  pd <- seq(pd_lower, pd_upper, pd_step)
  if (options[[paste0("assessPlan", type)]]) {
    # If assess plan option is specified, add the AQL and RQL values to pd.
    pd <- c(pd, options[[paste0("pd_prp", type)]], options[[paste0("pd_crp", type)]])
    pd <- sort(pd)
    pd <- round(pd, 3)
    pd <- pd[!duplicated(pd)]
  }
  dist <- options[[paste0("distribution", type)]]

  oc_plan <- NULL
  if (dist == "hypergeom") {
    N <- options[[paste0("lotSize", type)]]
    oc_plan <- AcceptanceSampling::OC2c(N = N, n = n, c = c, r = r, type = dist, pd = pd)
  } else {
    oc_plan <- AcceptanceSampling::OC2c(n = n, c = c, r = r, type = dist, pd = pd)
  }
  df_plan <- data.frame(PD = oc_plan@pd, PA = oc_plan@paccept)
  df_plan$PA <- round(df_plan$PA, 3)
  return (list(oc_plan=oc_plan, df_plan=df_plan))
}

# txt = "Check if the plan can satisfy the AQL and RQL constraints. Create tabular output."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------------------------------
##  Check if the plan can satisfy the AQL and RQL constraints. Create tabular output.  --
##---------------------------------------------------------------------------------------
#' @param jaspContainer <>
#' @param pos <>
#' @param depend_vars <>
#' @param oc_plan <>
#' @param options <>
#' @param type <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' assessPlan(jaspContainer, pos, depend_vars, oc_plan, options, type, n, c, r)
##---------------------------------------------------------------------------------------
assessPlan <- function(jaspContainer, pos, depend_vars, oc_plan, options, type, n, c, r) {
  pd_prp <- options[[paste0("pd_prp", type)]]
  pa_prp <- 1 - options[[paste0("pa_prp", type)]]
  pd_crp <- options[[paste0("pd_crp", type)]]
  pa_crp <- options[[paste0("pa_crp", type)]]
  
  if (pd_prp > 0 && pa_prp > 0 && pd_crp > 0 && pa_crp > 0) {
    # Assessment of the sampling plan
    assess <- AcceptanceSampling::assess(oc_plan, PRP = c(pd_prp, pa_prp), CRP = c(pd_crp, pa_crp))
    pa_prp_actual <- round(assess$PRP[3], 3)
    pa_crp_actual <- round(assess$CRP[3], 3)
    # Table with the specified and actual acceptance probabilities
    if (!is.null(jaspContainer[["riskTable"]])) {
      return ()
    }
    table <- createJaspTable(title = gettextf("Current plan <b>CAN %s</b> meet the specified risk point(s).", ifelse(assess$OK, "", "NOT")))
    table$dependOn(depend_vars)
    table$addColumnInfo(name = "col_1", title = "", type = "string")
    table$addColumnInfo(name = "col_2", title = "Proportion Non-conforming", type = "number")
    table$addColumnInfo(name = "col_3", title = "Required P(accept)", type = "number")
    table$addColumnInfo(name = "col_4", title = "Actual P(accept)", type = "number")
    table$addRows(list("col_1" = "AQL", "col_2" = pd_prp, "col_3" = pa_prp, "col_4" = pa_prp_actual))
    table$addRows(list("col_1" = "RQL", "col_2" = pd_crp, "col_3" = pa_crp, "col_4" = pa_crp_actual))
    table$showSpecifiedColumnsOnly <- TRUE
    table$position <- pos
    jaspContainer[["riskTable"]] <- table

    if (!assess$OK) {
      if (pa_crp_actual < pa_crp) {
        text = gettextf("Probability of acceptance (%.3f) at AQL (%.3f) is <b>lower</b> than the required probability of acceptance (%.3f) at AQL.", pa_prp_actual, pd_prp, pa_prp)
      } else if (pa_crp_actual > pa_crp) {
        text = gettextf("Probability of acceptance (%.3f) at RQL (%.3f) is <b>higher</b> than the required probability of acceptance (%.3f) at RQL.", pa_crp_actual, pd_crp, pa_crp)
      }
      explanation <- createJaspHtml(text = text, position=pos+1)
      if (is.null(jaspContainer[["explanation"]])) {
        jaspContainer[["explanation"]] <- explanation
      }
    }
  }
}

# txt = "Generate a summary table for the plan"
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##            Generate a summary table for the plan            --
##---------------------------------------------------------------
#' @param jaspContainer <>
#' @param pos <>
#' @param depend_vars <>
#' @param df_plan <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' getSummary(jaspContainer, pos, depend_vars, df_plan)
##---------------------------------------------------------------
getSummary <- function(jaspContainer, pos, depend_vars, df_plan) {
  if (!is.null(jaspContainer[["summaryTable"]])) {
    return()
  }
  summaryTable <- createJaspTable(title = "Acceptance Probabilities")
  summaryTable$dependOn(depend_vars)
  summaryTable$addColumnInfo(name = "col_1", title = "Prop. non-conforming", type = "number")
  summaryTable$addColumnInfo(name = "col_2", title = " P(accept)", type = "number")
  summaryTable$setData(list(col_1 = df_plan$PD, col_2 = round(df_plan$PA,3)))
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  summaryTable$position <- pos
  jaspContainer[["summaryTable"]] <- summaryTable
}

# txt = "Generate the operating characteristics curve for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##  Generate the operating characteristics curve for the plan.  --
##----------------------------------------------------------------
#' @param jaspContainer <>
#' @param pos <>
#' @param depend_vars <>
#' @param df_plan <>
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getOCCurve(jaspContainer, pos, depend_vars, df_plan)
##----------------------------------------------------------------
getOCCurve <- function(jaspContainer, pos, depend_vars, df_plan) {
  if (!is.null(jaspContainer[["ocCurve"]])) {
    return()
  }
  ocCurve <- createJaspPlot(title = paste0("OC (Operating Characteristics) Curve"),  width = 480, height = 320)
  ocCurve$dependOn(depend_vars)
  df_plan <- na.omit(df_plan)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD), max(df_plan$PD)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PA), max(df_plan$PA)))
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = PA)) + 
                  ggplot2::geom_point(colour = "black", shape = 19) + 
                  ggplot2::geom_line(colour = "black", linetype = "dashed") +
                  ggplot2::labs(x = "Proportion non-conforming", y = "Probability of Acceptance") +
                  ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
                  ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  # plt$position <- pos
  ocCurve$plotObject <- plt
  ocCurve$position <- pos
  jaspContainer[["ocCurve"]] <- ocCurve
}

# txt = "Generate the average outgoing quality curve for plan with rectification."
# banner(txt, centre = TRUE, bandChar = "-")
##------------------------------------------------------------------------------
##  Generate the average outgoing quality curve for plan with rectification.  --
##------------------------------------------------------------------------------
#' @param jaspContainer <>
#' @param pos <>
#' @param depend_vars <>
#' @param df_plan <>
#' @param options <>
#' @param type <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getAOQCurve(jaspContainer, pos, depend_vars, df_plan, options, type, n, c, r)
##------------------------------------------------------------------------------
getAOQCurve <- function(jaspContainer, pos, depend_vars, df_plan, options, type, n, c=NULL, r=NULL) {
  if (!is.null(jaspContainer[["aoqCurve"]])) {
    return ()
  }
  aoqCurve <- createJaspPlot(title = paste0("AOQ (Average Outgoing Quality) Curve"), width = 480, height = 320)
  aoqCurve$dependOn(depend_vars)
  jaspContainer[["aoqCurve"]] <- aoqCurve

  N <-  options[[paste0("lotSize", type)]]
  pd <- df_plan$PD
  AOQ <- numeric(length(pd))
  if (type == "Single") {
    AOQ <- df_plan$PA * pd * (N-n) / N
  } else {
    dist <- options[[paste0("distribution", type)]]
    stages <- length(n)
    probs_prod <- 1
    cum_n <- cumsum(n)
    stage_probs <- getStageProbability(pd, n, c, r, dist, N)
    if (is.null(stage_probs)) {
      aoqCurve$setError(sprintf("Error: Invalid input. Can not calculate AOQ. Check the plan parameters."))
      return ()
    }
    stage_probs <- stage_probs[[1]] # We only need acceptance probability.
    for (i in 1:stages) {
      pAcc_i <- stage_probs[i,] # Acceptance probability for ith stage
      AOQ <- AOQ + (pAcc_i * (N - cum_n[i]))
    }
    AOQ <- AOQ * pd / N
  }
  AOQ <- round(AOQ, 3)
  df_plan$AOQ <- AOQ
  df_plan <- na.omit(df_plan)
  aoq_max <- max(df_plan$AOQ)
  pd_aoq_max <- df_plan$PD[df_plan$AOQ == max(df_plan$AOQ)]
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD), max(df_plan$PD)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$AOQ), 1.2*aoq_max))
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = AOQ)) + 
         ggplot2::geom_point(colour = "black", shape = 19) + ggplot2::labs(x = "Proportion non-conforming", y = "Average Outgoing Quality") +
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::geom_hline(yintercept = aoq_max, linetype = "dotted") +
         ggplot2::annotate("text", label = sprintf("AOQL: %.3f", aoq_max), 
                           x = max(min(df_plan$PD)+0.09, pd_aoq_max*0.9), y = aoq_max*1.1, color = "black", size = 6) +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
        # ggplot2::ylim(0.0,round(aoq_max*1.2, 2))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  aoqCurve$plotObject <- plt
  # jaspContainer[["aoq_debug"]] <- createJaspHtml(text = sprintf("AOQ computed at %s\n", format(Sys.time(), "%X")), position = 0)
}

# txt = "Generate the average total inspection curve for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##  Generate the average total inspection curve for the plan.  --
##---------------------------------------------------------------
#' @param jaspContainer <>
#' @param pos <>
#' @param depend_vars <>
#' @param df_plan <>
#' @param options <>
#' @param type <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getATICurve(jaspContainer, pos, depend_vars, df_plan, options, type, n, c, r)
##---------------------------------------------------------------
getATICurve <- function(jaspContainer, pos, depend_vars, df_plan, options, type, n, c=NULL, r=NULL) {
  if (!is.null(jaspContainer[["atiCurve"]])) {
    return ()
  }
  atiCurve <- createJaspPlot(title = paste0("ATI (Average Total Inspection) Curve"), width = 480, height = 320)
  atiCurve$dependOn(depend_vars)
  jaspContainer[["atiCurve"]] <- atiCurve

  N <-  options[[paste0("lotSize", type)]]
  pd <- df_plan$PD
  ATI <- numeric(length(pd))

  if (type != "Mult") {
    # Single plan
    ATI <- df_plan$PA * n + (1 - df_plan$PA) * N
  } else {
    # Multiple plan
    dist <- options[[paste0("distribution", type)]]
    stages <- length(n)
    cum_n <- cumsum(n)
    stage_probs <- getStageProbability(pd, n, c, r, dist, N)
    if (is.null(stage_probs)) {
      atiCurve$setError(sprintf("Error: Invalid input. Can not calculate ATI. Check the plan parameters."))
      return ()
    }
    acc_probs <- stage_probs[[1]] # Acceptance probabilities
    rej_probs <- stage_probs[[2]] # Rejection probabilities
    for (i in 1:stages) {
      pAcc_i <- acc_probs[i,]
      ATI <- ATI + (pAcc_i * cum_n[i])
    }
    ATI <- ATI + N * colSums(rej_probs) # For any stage, if lot gets rejected, all N items are inspected under rectification plan.
  }
  ATI <- round(ATI, 3)
  df_plan$ATI <- ATI
  df_plan <- na.omit(df_plan)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df_plan$PD)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df_plan$ATI)
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = ATI)) + 
         ggplot2::geom_point(colour = "black", shape = 19) + 
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::labs(x = "Proportion non-conforming", y = "Average Total Inspection") +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
        #  ggplot2::scale_y_continuous(breaks = pretty(df_plan$ATI))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  atiCurve$plotObject <- plt
}

# txt = "Generate the average sample number curve for the plan. Only applicable for multiple sampling plans."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------------------------------------------------
##  Generate the average sample number curve for the plan. Only applicable for multiple sampling plans.  --
##---------------------------------------------------------------------------------------------------------
#' @param jaspContainer <>
#' @param pos <>
#' @param depend_vars <>
#' @param options <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getASNCurve(jaspContainer, pos, depend_vars, options, n, c, r) 
##---------------------------------------------------------------------------------------------------------
getASNCurve <- function(jaspContainer, pos, depend_vars, options, n, c, r) {
  if (!is.null(jaspContainer[["asnPlot"]])) {
    return ()
  }
  asnPlot <- createJaspPlot(title = "ASN (Average Sample Number) Curve",  width = 480, height = 320)
  asnPlot$dependOn(depend_vars)
  jaspContainer[["asnPlot"]] <- asnPlot

  # Parse option values
  dist <- options$distributionMult
  N <- options$lotSizeMult
  pd_lower <- options$pd_lowerMult
  pd_upper <- options$pd_upperMult
  pd_step <- options$pd_stepMult
  checkPdErrors(jaspContainer, pd_lower, pd_upper, pd_step)
  if (jaspContainer$getError()) {
    return ()
  }
  pd = seq(pd_lower, pd_upper, pd_step)
  stages <- length(n)
  num_values <- length(pd)
  ASN <- numeric(num_values)
  cum_n <- cumsum(n)
  stage_probs <- getStageProbability(pd, n, c, r, dist, N)
  if (is.null(stage_probs)) {
    asnPlot$setError(sprintf("Error: Invalid input. Can not calculate ASN. Check the plan parameters."))
    return ()
  }
  stage_probs <- stage_probs[[1]] + stage_probs[[2]] # Decision prob = p_acc + p_rej
  for (i in 1:(stages-1)) {
    pDecide_i <- stage_probs[i,]
    ASN <- ASN + pDecide_i * cum_n[i]
  }
  ASN <- round(ASN, 3)
  df_plan <- data.frame(PD = pd, ASN = ASN)
  df_plan <- na.omit(df_plan)

  # Draw ASN plot
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD), max(df_plan$PD)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$ASN), max(df_plan$ASN)))
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = ASN)) + 
         ggplot2::geom_point(colour = "black", shape = 19) + 
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::labs(x = "Proportion non-conforming", y = "Average Sample Number") +
         ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- pos
  asnPlot$plotObject <- plt
}

# txt = "Helper function to get stagewise acceptance and rejection probabilities for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##-------------------------------------------------------------------------------------------
##  Helper function to get stagewise acceptance and rejection probabilities for the plan.  --
##-------------------------------------------------------------------------------------------
#' @param pd <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @param dist <>
#' @param N <>
#' @seealso
#'   [getStageProbability()] to get stagewise acceptance and rejection probabilities of the plan.
#' @examples
#' getStageProbabilityHelper(pd, n, c, r, dist, N)
##-------------------------------------------------------------------------------------------
getStageProbabilityHelper <- function(pd, n, c, r, dist, N=1000) {
  D <- pd*N
  num_stages <- length(n)
  acc_probs <- matrix(nrow = num_stages, ncol = length(pd))
  rej_probs <- matrix(nrow = num_stages, ncol = length(pd))
  k.s <- num_stages ## number of stages in this sampling

  prob.acc <- function(x, n, p, dist, N=1000) {
    k <- length(x)
    k1 <- k-2
    if (dist == "binom") {
      # k = number of this stage + 2. x[1:k1] will give all values until the last stage before this one. n[1:k1] does the same for n.
      prob <- prod(dbinom(x[1:k1], n[1:k1], p)) * pbinom(x[k-1], n[k-1], p)
    } else if (dist == "poisson") {
      prob <- prod(dpois(x[1:k1], n[1:k1]*p)) * ppois(x[k-1], n[k-1]*p)
    } else if (dist == "hypergeom") {
      x.cum <- cumsum(x[1:(k-1)])
      n.cum <- cumsum(n)
      N.cum <- N - c(0, n.cum[1:(k-1)])
      D.cum <- D - c(0, x.cum[1:(k-1)])

      prob <- prod(dhyper(x=x[1:k1], m=pmax(D.cum[1:k1], 0), n = N.cum[1:k1] - pmax(D.cum[1:k1], 0), k=n[1:k1])) *
                   phyper(q=x[k-1], m=pmax(D.cum[k-1], 0), n = N.cum[k-1] - pmax(D.cum[k-1], 0), k=n[k-1])
    }
    return (prob)
  }

  prob.rej <- function(x, n, p, dist, N=1000) {
    k <- length(x)
    k1 <- k-2
    if (dist == "binom") {
      prob <- prod(dbinom(x[1:k1], n[1:k1], p)) * pbinom(x[k], n[k-1], p, lower.tail = FALSE)
    } else if (dist == "poisson") {
      prob <- prod(dpois(x[1:k1], n[1:k1]*p)) * ppois(x[k], n[k-1]*p, lower.tail = FALSE)
    } else if (dist == "hypergeom") {
      x.cum <- cumsum(x[c(1:(k-2), k)])
      n.cum <- cumsum(n)
      N.cum <- N - c(0,n.cum[1:(k-2)])
      D.cum <- D - c(0,x.cum[1:(k-2)])
      prob <- prod(dhyper(x=x[1:k1], m=pmax(D.cum[1:k1], 0), n = N.cum[1:k1] - pmax(D.cum[1:k1], 0), k=n[1:k1])) *
                   phyper(q=x[k], m=pmax(D.cum[k-1], 0), n = N.cum[k-1] - pmax(D.cum[k-1], 0), k=n[k-1], lower.tail = FALSE)
    }
    return (prob)
  }
  
  for (k in 1:k.s) {
    ## For each stage, find out all the possibilities which could
    ## lead to still not having made a decision and then calculate
    ## the appropriate probabilities.

    if(k == 1) {
      ## Only a single sampling stage
      p.acc <- sapply(pd, FUN = function(el) {
        if (dist == "binom") {
          return (pbinom(q=c[1], size=n[1], prob=el))
        } else if (dist == "poisson") {
          return (ppois(q=c[1], lambda=n[1]*el))
        } else if (dist == "hypergeom") {
          el = el * N
          return (phyper(q=c[1], m=el, n=N-el, k=n[1]))
        }
      })
      acc_probs[k,] = p.acc
      p.rej <- sapply(pd, FUN = function(el) {
        if (dist == "binom") {
          return (pbinom(q=r[1]-1, size=n[1], prob=el, lower.tail = FALSE))
        } else if (dist == "poisson") {
          return (ppois(q=r[1]-1, lambda = n[1]*el, lower.tail = FALSE))
        } else if (dist == "hypergeom") {
          el = el * N
          return (phyper(q=r[1]-1, m=el, n=N-el, k=n[1], lower.tail = FALSE))
        }
      })
      rej_probs[k,] = p.rej
      ## p.acc and p.rej now exist and can be used in the following stages.
    }
    else if (k == 2) {
      ## Two sampling stages. Needs to be handled separately from
      ## more stages due to matrix dimensions
      c.s <- c+1 ## Use to calculate limits
      r.s <- r-1 ## Use to calculate limits
      ## The possibilities which lead to a decision to be made at
      ## the second stage
      x <- data.frame(X1 = seq(c.s[1], r.s[1], by=1),
                      X.acc = c[2]-seq(c.s[1], r.s[1], by=1),
                      X.rej = r[2]-1-seq(c.s[1], r.s[1], by=1))
      p.acc_2 <- sum(apply(x, 1, FUN=prob.acc, n=n, p=pd, dist = dist, N=N))
      p.acc <- p.acc + p.acc_2
      acc_probs[k,] = p.acc_2
      p.rej_2 <- sum(apply(x, 1, FUN=prob.rej, n=n, p=pd, dist = dist, N=N))
      p.rej <- p.rej + p.rej_2
      rej_probs[k,] = p.rej_2
    }
    else {
      ## More than two sampling stages.
      ## Things are more tricky.
      c.s <- c+1 ## Use to calculate limits
      r.s <- r-1 ## Use to calculate limits
      expand.call <- "expand.grid(c.s[k-1]:r.s[k-1]"
      for(i in 2:(k-1)) {
        expand.call <- paste(expand.call, paste("c.s[k-",i,"]:r.s[k-",i,"]", sep=""), sep=",")
      }
      expand.call <- paste(expand.call,")", sep="")
      x <- eval(parse(text=expand.call)[[1]])
      x <- x[,(k-1):1] # Reverses the order of columns in dataframe x
      names(x) <- paste("X", 1:(k-1), sep="")
      
      for(i in ncol(x):2) {
        x[,i] <- x[,i] - x[,i-1]
      }
      x <- cbind(x, X.acc = c[k] - rowSums(x[,1:(k-1)]))
      x <- cbind(x, X.rej = r[k]-1 - rowSums(x[,1:(k-1)]))
      p.acc_k <- sum(apply(x, 1, FUN=prob.acc, n=n, p=pd, dist = dist, N=N))
      p.acc <- p.acc + p.acc_k
      acc_probs[k,] = p.acc_k
      p.rej_k <- sum(apply(x, 1, FUN=prob.rej, n=n, p=pd, dist = dist, N=N))
      p.rej <- p.rej + p.rej_k
      rej_probs[k,] = p.rej_k
    }
  }
  return(list(acc_probs,rej_probs))
}

# txt = "Get stagewise acceptance and rejection probabilities for the plan."
# banner(txt, centre = TRUE, bandChar = "#")
##------------------------------------------------------------------------
##  Get stagewise acceptance and rejection probabilities for the plan.  --
##------------------------------------------------------------------------
#' @param pd <>
#' @param n <>
#' @param c <>
#' @param r <>
#' @param dist <>
#' @param N <>
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' getStageProbability(pd, n, c, r, dist, N)
##------------------------------------------------------------------------
getStageProbability <- function(pd, n, c, r, dist, N=1000) {
  stage_probs <- sapply(pd, FUN=getStageProbabilityHelper, n=n, c=c, r=r, dist=dist, N=N)
  acc <- matrix(unlist(stage_probs[1,]), byrow=FALSE, nrow=length(n))
  rej <- matrix(unlist(stage_probs[2,]), byrow=FALSE, nrow=length(n))
  return (list(acc,rej))
}