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

##----------------------------------------------------------------
##       Create and return a data frame for sampling plan       --
##----------------------------------------------------------------
#' @param options User specified options.
#' @param planType "Single" or "Mult".
#' @param returnPlan Return the OC2c plan object. Defaults to '"FALSE'.
#' @returns A data frame, and optionally, an OC2c object.
#' @seealso
#'   [()] for <>,
#'   [()] for <>.
#' @examples
#' getPlanDf(options, "Single", TRUE)
#' getPlanDf(options, "Mult")
##----------------------------------------------------------------
getPlanDf <- function(options, planType, returnPlan=FALSE) {
  n <- c <- r <- NULL
  if (planType != "Mult") {
    # Single sampling plan
    n <- options[[paste0("sampleSize", planType)]]
    c <- options[[paste0("acceptNumber", planType)]]
    r <- options[[paste0("rejectNumber", planType)]]
  } else {
    # Multiple sampling plan
     stages <- options[["stages"]]
     for (i in 1:length(stages)) {
       n[i] <- stages[[i]]$sampleSizeMult
       c[i] <- stages[[i]]$acceptNumberMult
       r[i] <- stages[[i]]$rejectNumberMult
     }
  }
  
  pd_lower <- options[[paste0("pd_lower", planType)]]
  pd_upper <- options[[paste0("pd_upper", planType)]]
  pd_step <- options[[paste0("pd_step", planType)]]
  pd <- seq(pd_lower, pd_upper, pd_step)
  dist <- options[[paste0("distribution", planType)]]

  plan <- NULL
  if (dist == "hypergeom") {
    N <-  options[[paste0("lotSize", planType)]]
    plan <- AcceptanceSampling::OC2c(N = N, n = n, c = c, r = r, type = dist, pd = pd)
  } else {
    plan <- AcceptanceSampling::OC2c(n = n, c = c, r = r, type = dist, pd = pd)
  }
  df_plan <- data.frame(PD = plan@pd, PA = plan@paccept)

  if (returnPlan) {
    return (list(plan, df_plan))
  } else {
    return (df_plan)
  }
}

# txt = "Generate a summary table for the plan"
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##            Generate a summary table for the plan            --
##---------------------------------------------------------------
#' @param jaspContainer <>.
#' @param df_plan <>.
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' getSummary(jaspContainer, df_plan, planType, depend_variables, positionInContainer)
##---------------------------------------------------------------
getSummary <- function(jaspContainer, df_plan, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspContainer[[paste0("summaryTable", planType)]])) {
    return()
  }
  summaryTable <- createJaspTable(title = "Acceptance Probabilities")
  summaryTable$dependOn(depend_variables)
  summaryTable$addColumnInfo(name = "col_1", title = "Prop. non-conforming", type = "number")
  summaryTable$addColumnInfo(name = "col_2", title = " P(accept)", type = "number")
  summaryTable$setData(list(col_1 = df_plan$PD, col_2 = round(df_plan$PA,3)))
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  summaryTable$position <- positionInContainer
  jaspContainer[[paste0("summaryTable", planType)]] <- summaryTable
}

# txt = "Check if the plan can satisfy the AQL and RQL constraints. Create tabular output."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------------------------------
##  Check if the plan can satisfy the AQL and RQL constraints. Create tabular output.  --
##---------------------------------------------------------------------------------------
#' @param jaspContainer <>.
#' @param options <>.
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' assessPlan(jaspContainer, options, planType, depend_variables, positionInContainer)
##---------------------------------------------------------------------------------------
assessPlan <- function(jaspContainer, options, planType, depend_variables, positionInContainer) {
  pd_prp <- options[[paste0("pd_prp", planType)]]
  pa_prp <- 1 - options[[paste0("pa_prp", planType)]]
  pd_crp <- options[[paste0("pd_crp", planType)]]
  pa_crp <- options[[paste0("pa_crp", planType)]]
  
  if (pd_prp > 0 && pa_prp > 0 && pd_crp > 0 && pa_crp > 0) {
    output <- getPlanDf(options, planType, TRUE)
    plan <- output[[1]]
    df_plan <- output[[2]]
    # Assessment of the sampling plan
    assess <- capture.output(AcceptanceSampling::assess(plan, PRP = c(pd_prp, pa_prp), CRP = c(pd_crp, pa_crp)))

    # Create and fill the output tables
    # 1. Sampling plan table
    if (is.null(jaspContainer[[paste0("plan_table", planType)]])) {
      plan_table <- createJaspTable(title = "Acceptance Sampling Plan")
      n <- c <- r <- NULL
      if (planType != "Mult") {
        # Single sampling plan        
        plan_table$dependOn(paste0(c("sampleSize", "acceptNumber", "rejectNumber", "assessPlan"), planType))
        n <- options[[paste0("sampleSize", planType)]]
        c <- options[[paste0("acceptNumber", planType)]]
        r <- options[[paste0("rejectNumber", planType)]]
      } else {
        # Multiple sampling plan
        plan_table$dependOn(c("stages", "assessPlanMult"))
        stages <- options[["stages"]]
        for (i in 1:length(stages)) {
          n[i] <- stages[[i]]$sampleSizeMult
          c[i] <- stages[[i]]$acceptNumberMult
          r[i] <- stages[[i]]$rejectNumberMult
        }
      }
      
      if (planType != "Mult") {
        plan_table$addColumnInfo(name = "table_1_col_1", title = "", type = "string")
        plan_table$addColumnInfo(name = "table_1_col_2", title = "Value", type = "integer")
        plan_table$addRows(list("table_1_col_1" = "Sample size", "table_1_col_2" = n))
        plan_table$addRows(list("table_1_col_1" = "Acceptance Number", "table_1_col_2" = c))        
      } else {
        plan_table$addColumnInfo(name = "table_1_col_1", title = "Sample", type = "integer")
        plan_table$addColumnInfo(name = "table_1_col_2", title = "Sample Size", type = "integer")
        plan_table$addColumnInfo(name = "table_1_col_3", title = "Cum. Sample Size", type = "integer")
        plan_table$addColumnInfo(name = "table_1_col_4", title = "Acc. Number", type = "integer")
        plan_table$addColumnInfo(name = "table_1_col_5", title = "Rej. Number", type = "integer")
        plan_table$setData(list(table_1_col_1 = 1:length(stages), table_1_col_2 = n, table_1_col_3 = cumsum(n), 
                                table_1_col_4 = c, table_1_col_5 = r))
      }
      plan_table$showSpecifiedColumnsOnly <- TRUE
      plan_table$position <- positionInContainer
      jaspContainer[[paste0("plan_table", planType)]] <- plan_table
    }

    # 2. Table with the specified and actual acceptance probabilities
    getRiskPointTable(jaspContainer, assess, planType, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp, positionInContainer+1)
  }
}

# txt = "Create the table showing the specified risk quality levels and their acceptance probabilities."
# banner(txt, centre = TRUE, bandChar = "-")
##-------------------------------------------------------------------------------------------------------------------
##  Create the table showing the specified risk quality levels and their acceptance probabilities.  --
##-------------------------------------------------------------------------------------------------------------------
#' @param jaspContainer <>.
#' @param assess <>.
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param pd_prp <>.
#' @param pa_prp <>.
#' @param pd_crp <>.
#' @param pa_crp <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' getRiskPointTable(jaspContainer, assess, planType, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp, positionInContainer)
##-------------------------------------------------------------------------------------------------------------------
getRiskPointTable <- function(jaspContainer, assess, planType, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp, positionInContainer) {
  if (!is.null(jaspContainer[[paste0("riskTable", planType)]])) {
    return ()
  }
  table <- createJaspTable(title = as.character(assess[8]))
  table$dependOn(depend_variables)
  table$addColumnInfo(name = "col_1", title = "", type = "string")
  table$addColumnInfo(name = "col_2", title = "Quality", type = "number")
  table$addColumnInfo(name = "col_3", title = "RP P(accept)", type = "number")
  table$addColumnInfo(name = "col_4", title = "Plan P(accept)", type = "number")
  table$addRows(list("col_1" = "PRP", "col_2" = pd_prp, "col_3" = pa_prp, "col_4" = as.numeric(unlist(strsplit(assess[11], " +"))[4])))
  table$addRows(list("col_1" = "CRP", "col_2" = pd_crp, "col_3" = pd_crp, "col_4" = as.numeric(unlist(strsplit(assess[12], " +"))[4])))
  table$showSpecifiedColumnsOnly <- TRUE
  table$position <- positionInContainer
  jaspContainer[[paste0("riskTable", planType)]] <- table
}

# txt = "Generate the operating characteristics curve for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##  Generate the operating characteristics curve for the plan.  --
##----------------------------------------------------------------
#' @param jaspContainer <>.
#' @param df_plan <>.
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getOCCurve(jaspContainer, df_plan, planType, depend_variables, positionInContainer)
##----------------------------------------------------------------
getOCCurve <- function(jaspContainer, df_plan, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspContainer[[paste0("ocCurve", planType)]])) {
    return()
  }
  ocCurve <- createJaspPlot(title = paste0("OC (Operating Characteristics) Curve"),  width = 480, height = 320)
  ocCurve$dependOn(depend_variables)
  jaspContainer[[paste0("ocCurve", planType)]] <- ocCurve
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = PA)) + 
                  ggplot2::geom_point(colour = "black", shape = 19) + 
                  ggplot2::geom_line(colour = "black", linetype = "dashed") +
                  ggplot2::labs(x = "Proportion non-conforming", y = "P(accept)")
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  ocCurve$plotObject <- plt
}

# txt = "Generate the average outgoing quality curve for plan with rectification."
# banner(txt, centre = TRUE, bandChar = "-")
##------------------------------------------------------------------------------
##  Generate the average outgoing quality curve for plan with rectification.  --
##------------------------------------------------------------------------------
#' @param jaspContainer <>.
#' @param df_plan <>.
#' @param options <>
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getAOQCurve(jaspContainer, df_plan, options, planType, depend_variables, positionInContainer)
##------------------------------------------------------------------------------
getAOQCurve <- function(jaspContainer, df_plan, options, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspContainer[[paste0("aoqCurve", planType)]])) {
    return ()
  }
  aoqCurve <- createJaspPlot(title = paste0("AOQ (Average Outgoing Quality) Curve"),  width = 480, height = 320)
  aoqCurve$dependOn(depend_variables)
  jaspContainer[[paste0("aoqCurve", planType)]] <- aoqCurve

  N <-  options[[paste0("lotSize", planType)]]
  n <- c <- r <- NULL
  if (planType != "Mult") {
    # Single sampling plan
    n <- options[[paste0("sampleSize", planType)]]
    c <- options[[paste0("acceptNumber", planType)]]
    r <- options[[paste0("rejectNumber", planType)]]
  } else {
    # Multiple sampling plan
    stages <- options[["stages"]]
    for (i in 1:length(stages)) {
      n[i] <- stages[[i]]$sampleSizeMult
      c[i] <- stages[[i]]$acceptNumberMult
      r[i] <- stages[[i]]$rejectNumberMult
    }
  }
  dist <- options[[paste0("distribution", planType)]]
  pd <- df_plan$PD
  AOQ <- numeric(length(pd))
  if (planType == "Single") {
    AOQ <- df_plan$PA * pd * (N-n) / N
  } else {
    stages <- length(n)
    probs_prod <- 1
    cum_n <- cumsum(n)
    stage_probs <- getStageProbability(pd, n, c, r, dist, N, jaspContainer, depend_variables)
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
  df_plan$AOQ <- AOQ
  aoq_max <- max(df_plan$AOQ)
  pd_aoq_max <- df_plan$PD[df_plan$AOQ == max(df_plan$AOQ)]
  # xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(df_plan$PD), max(df_plan$PD)))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1.1*aoq_max))
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = AOQ)) + 
                         ggplot2::geom_point(colour = "black", shape = 19) + ggplot2::labs(x = "Proportion non-conforming", y = "AOQ") +
                         ggplot2::geom_line(colour = "black", linetype = "dashed") +
                         ggplot2::geom_hline(yintercept = aoq_max, linetype = "dotted") +
                         ggplot2::annotate("text", label = sprintf("AOQL: %.2f", aoq_max), 
                                            x = max(0.09, pd_aoq_max*0.9), y = aoq_max*1.1, color = "black", size = 6) +
                         ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))
                         #  ggplot2::ylim(0.0,round(aoq_max*1.2, 2))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  aoqCurve$plotObject <- plt
}

# txt = "Generate the average total inspection curve for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##  Generate the average total inspection curve for the plan.  --
##---------------------------------------------------------------
#' @param jaspContainer <>.
#' @param df_plan <>.
#' @param options <>
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getATICurve(jaspContainer, df_plan, options, planType, depend_variables, positionInContainer)
##---------------------------------------------------------------
getATICurve <- function(jaspContainer, df_plan, options, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspContainer[[paste0("atiCurve", planType)]])) {
    return ()
  }
  atiCurve <- createJaspPlot(title = paste0("ATI (Average Total Inspection) Curve"),  width = 480, height = 320)
  atiCurve$dependOn(depend_variables)
  jaspContainer[[paste0("atiCurve", planType)]] <- atiCurve

  N <-  options[[paste0("lotSize", planType)]]
  n <- c <- r <- NULL
  if (planType != "Mult") {
    # Single sampling plan
    n <- options[[paste0("sampleSize", planType)]]
    c <- options[[paste0("acceptNumber", planType)]]
    r <- options[[paste0("rejectNumber", planType)]]
  } else {
    # Multiple sampling plan
    stages <- options[["stages"]]
    for (i in 1:length(stages)) {
      n[i] <- stages[[i]]$sampleSizeMult
      c[i] <- stages[[i]]$acceptNumberMult
      r[i] <- stages[[i]]$rejectNumberMult
    }
  }
  dist <- options[[paste0("distribution", planType)]]
  pd <- df_plan$PD
  ATI <- numeric(length(pd))
  if (planType != "Mult") {
    # Single plan
    ATI <- df_plan$PA * n + (1 - df_plan$PA) * N
  } else {
    # Multiple plan
    stages <- length(n)
    cum_n <- cumsum(n)
    stage_probs <- getStageProbability(pd, n, c, r, dist, N, jaspContainer, depend_variables)
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
  df_plan$ATI <- ATI
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = ATI)) + 
                         ggplot2::geom_point(colour = "black", shape = 19) + 
                         ggplot2::geom_line(colour = "black", linetype = "dashed") +
                         ggplot2::labs(x = "Proportion non-conforming", y = "ATI")
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  atiCurve$plotObject <- plt
}

# txt = "Generate the average sample number curve for the plan. Only applicable for multiple sampling plans."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------------------------------------------------
##  Generate the average sample number curve for the plan. Only applicable for multiple sampling plans.  --
##---------------------------------------------------------------------------------------------------------
#' @param jaspContainer <>.
#' @param options <>
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getASNCurve(jaspContainer, options, depend_variables, positionInContainer) 
##---------------------------------------------------------------------------------------------------------
getASNCurve <- function(jaspContainer, options, depend_variables, positionInContainer) {
  if (!is.null(jaspContainer[["asnPlot"]])) {
    return ()
  }
  asnPlot <- createJaspPlot(title = "ASN Curve",  width = 480, height = 320)
  asnPlot$dependOn(depend_variables)
  jaspContainer[["asnPlot"]] <- asnPlot

  # Parse option values
  n <- c <- r <- NULL
  stages <- options[["stages"]]
  for (i in 1:length(stages)) {
    n[i] <- stages[[i]]$sampleSizeMult
    c[i] <- stages[[i]]$acceptNumberMult
    r[i] <- stages[[i]]$rejectNumberMult
  }
  dist <- options$distributionMult
  N <- options$lotSizeMult
  pd_lower <- options$pd_lowerMult
  pd_upper <- options$pd_upperMult
  pd_step <- options$pd_stepMult
  pd = seq(pd_lower, pd_upper, pd_step)
  stages <- length(n)
  num_values <- length(pd)
  ASN <- numeric(num_values)
  cum_n <- cumsum(n)
  stage_probs <- getStageProbability(pd, n, c, r, dist, N, jaspContainer, depend_variables)
  if (is.null(stage_probs)) {
    asnPlot$setError(sprintf("Error: Invalid input. Can not calculate ASN. Check the plan parameters."))
    return ()
  }
  stage_probs <- stage_probs[[1]] + stage_probs[[2]] # Decision prob = p_acc + p_rej
  for (i in 1:(stages-1)) {
    pDecide_i <- stage_probs[i,]
    ASN <- ASN + pDecide_i * cum_n[i]
  }
  df_asn <- data.frame(PD = pd, ASN = ASN)

  # Draw ASN plot
  plt <- ggplot2::ggplot(data = df_asn, ggplot2::aes(x = PD, y = ASN)) + 
         ggplot2::geom_point(colour = "black", shape = 19) + 
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::labs(x = "Proportion non-conforming", y = "Average Sample Number")
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  asnPlot$plotObject <- plt
}

# txt = "Helper function to get stagewise acceptance and rejection probabilities for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##-------------------------------------------------------------------------------------------
##  Helper function to get stagewise acceptance and rejection probabilities for the plan.  --
##-------------------------------------------------------------------------------------------
#' @param pd <>.
#' @param n <>.
#' @param c <>.
#' @param r <>.
#' @param dist <>.
#' @param N <>.
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

      prob <- prod(dhyper(x=x[1:k1], m=pmax(D.cum[1:k1], 0), n=N.cum[1:k1] - pmax(D.cum[1:k1], 0), k=n[1:k1])) *
              phyper(q=x[k-1], m=pmax(D.cum[k-1], 0), n=N.cum[k-1] - pmax(D.cum[k-1], 0), k=n[k-1])
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
      prob <- prod(dhyper(x=x[1:k1], m=pmax(D.cum[1:k1], 0), n=N.cum[1:k1] - pmax(D.cum[1:k1], 0), k=n[1:k1])) *
              phyper(q=x[k], m=pmax(D.cum[k-1], 0), n=N.cum[k-1] - pmax(D.cum[k-1], 0), k=n[k-1], lower.tail = FALSE)
    }
    return (prob)
  }
  
  for (k in 1:k.s) {
    ## For each stage, find out all the possibilities which could
    ## lead to still not having made a decision and then calculate
    ## the appropriate probabilities.

    if(k == 1) {
      ## Only a single sampling stage to do - this is simple
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
          return (pbinom(q=r[1]-1,size=n[1], prob=el, lower.tail = FALSE))
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
#' @param pd <>.
#' @param n <>.
#' @param c <>.
#' @param r <>.
#' @param dist <>.
#' @param N <>.
#' @param jaspContainer <>.
#' @param depend_variables <>.
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' getStageProbability(pd, n, c, r, dist, N, jaspContainer, depend_variables)
##------------------------------------------------------------------------
getStageProbability <- function(pd, n, c, r, dist, N=1000, jaspContainer, depend_variables) {
  stage_probs <- sapply(pd, FUN=getStageProbabilityHelper, n=n, c=c, r=r, dist=dist, N=N)
  acc <- matrix(unlist(stage_probs[1,]), byrow=FALSE, nrow=length(n))
  rej <- matrix(unlist(stage_probs[2,]), byrow=FALSE, nrow=length(n))
  return (list(acc,rej))
}