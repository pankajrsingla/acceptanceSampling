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

# Create and return a data frame with the quality levels and the corresponding acceptance probabilities for the plan.
getPlanDf <- function(options, planType, depend_variables, returnPlan=FALSE) {
  n <- options[[paste0("sampleSize", planType)]]
  c <- options[[paste0("acceptNumber", planType)]]
  r <- options[[paste0("rejectNumber", planType)]]
  pd_lower <- options[[paste0("pd_lower", planType)]]
  pd_upper <- options[[paste0("pd_upper", planType)]]
  pd_step <- options[[paste0("pd_step", planType)]]
  pd <- seq(pd_lower, pd_upper, pd_step)
  dist = options[[paste0("distribution", planType)]]

  plan <- NULL
  if (dist == "hypergeom") {
    N <-  options[[paste0("lotSize", planType)]]
    plan <- AcceptanceSampling::OC2c(N = N, n = n, c = c, r = r, type = dist, pd = pd)
  } else {
    plan <- AcceptanceSampling::OC2c(n = n, c = c, r = r, type = dist, pd = pd)
  }
  df_plan <- data.frame(PD = plan@pd, PA = plan@paccept)
  
  # To make sure the df gets updated when the plan variables change
  # dummy <- createJaspHtml()
  # dummy$dependOn(depend_variables)
  # dummy[["text"]] <- ""

  if (returnPlan) {
    return (list(plan, df_plan))
  } else {
    return (df_plan)
  }
}

# Generate a table with the quality levels and the corresponding acceptance probabilities for the plan.
getSummary <- function(jaspResults, df_plan, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[[paste0("summaryTable", planType)]])) {
    return()
  }
  summaryTable <- createJaspTable(title = "Detailed acceptance probabilities")
  summaryTable$dependOn(depend_variables)
  summaryTable$addColumnInfo(name = "col_1", title = "Prop. non-confirming", type = "number")
  summaryTable$addColumnInfo(name = "col_2", title = " P(accept)", type = "number")
  summaryTable$setData(list(col_1 = round(df_plan$PD,2), col_2 = round(df_plan$PA,2)))
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  summaryTable$position <- positionInContainer
  jaspResults[[paste0("summaryTable", planType)]] <- summaryTable
}

# Check if the plan can satisfy the AQL and RQL constraints. Create tabular output.
assessPlan <- function(jaspResults, options, planType, depend_variables, positionInContainer) {
  pd_prp <- options[[paste0("pd_prp", planType)]]
  pa_prp <- 1 - options[[paste0("pa_prp", planType)]]
  pd_crp <- options[[paste0("pd_crp", planType)]]
  pa_crp <- options[[paste0("pa_crp", planType)]]
  
  if (pd_prp > 0 && pa_prp > 0 && pd_crp > 0 && pa_crp > 0) {
    output <- getPlanDf(options, planType, depend_variables, TRUE)
    plan <- output[[1]]
    df_plan <- output[[2]]
    # Assessment of the sampling plan
    assess <- capture.output(AcceptanceSampling::assess(plan, PRP = c(pd_prp, pa_prp), CRP = c(pd_crp, pa_crp)))

    # Create and fill the output tables
    # 1. Sampling plan table
    if (is.null(jaspResults[[paste0("plan_table", planType)]])) {
      plan_table <- createJaspTable(title = "Acceptance Sampling Plan")
      plan_table$dependOn(paste0(c("sampleSize", "acceptNumber", "rejectNumber"), planType))
      n <- options[[paste0("sampleSize", planType)]]
      c <- options[[paste0("acceptNumber", planType)]]
      r <- options[[paste0("rejectNumber", planType)]]
      
      if (planType == "Single") {
        plan_table$addColumnInfo(name = "table_1_col_1", title = "", type = "string")
        plan_table$addColumnInfo(name = "table_1_col_2", title = "Value", type = "integer")
        plan_table$addRows(list("table_1_col_1" = "Sample size(s)", "table_1_col_2" = n))
        plan_table$addRows(list("table_1_col_1" = "Acc. Number(s)", "table_1_col_2" = c))
        # plan_table$addRows(list("table_1_col_1" = "Rej. Number(s)", "table_1_col_2" = r))
      } else {
        plan_table$addColumnInfo(name = "table_1_col_1", title = "Sample", type = "integer")
        plan_table$addColumnInfo(name = "table_1_col_2", title = "Sample Size", type = "integer")
        plan_table$addColumnInfo(name = "table_1_col_3", title = "Cum. Sample Size", type = "integer")
        plan_table$addColumnInfo(name = "table_1_col_4", title = "Acc. Number", type = "integer")
        plan_table$addColumnInfo(name = "table_1_col_5", title = "Rej. Number", type = "integer")
        plan_table$setData(list(table_1_col_1 = 1:length(options$sampleSizeMult), table_1_col_2 = n, table_1_col_3 = cumsum(n), table_1_col_4 = c, table_1_col_5 = r))
      }
      plan_table$showSpecifiedColumnsOnly <- TRUE
      plan_table$position <- positionInContainer
      jaspResults[[paste0("plan_table", planType)]] <- plan_table
    }

    # 2. Table with the specified and actual acceptance probabilities
    getRiskPointTable(jaspResults, assess, planType, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp, positionInContainer+1)
  }
}

# Create the table showing the specified risk quality levels and the acceptance probabilities for those levels.
getRiskPointTable <- function(jaspResults, assess, planType, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp, positionInContainer) {
  if (!is.null(jaspResults[[paste0("riskTable", planType)]])) {
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
  jaspResults[[paste0("riskTable", planType)]] <- table
}

# Generate the operating characteristics curve for the plan.
getOCCurve <- function(jaspResults, df_plan, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[[paste0("ocCurve", planType)]])) {
    return()
  }
  ocCurve <- createJaspPlot(title = paste0("OC (Operating Characteristics) curve"),  width = 480, height = 320)
  ocCurve$dependOn(depend_variables)
  jaspResults[[paste0("ocCurve", planType)]] <- ocCurve
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = PA)) + 
                  ggplot2::geom_point(colour = "black", shape = 19) + 
                  ggplot2::geom_line(colour = "black", linetype = "dashed") +
                  ggplot2::labs(x = "Proportion non-confirming", y = "P(accept)")
                  # ggplot2::theme_classic() +
                  # ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16, color = "black"), axis.text.y = ggplot2::element_text(size = 16, color = "black"),
                  #                axis.title.x = ggplot2::element_text(size = 20, color = "black"), axis.title.y = ggplot2::element_text(size = 20, color = "black"))
  # plt <- jaspGraphs::themeJasp(plt)
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  ocCurve$plotObject <- plt
}

# Generate the average outgoing quality curve for the plan.
getAOQCurve <- function(jaspResults, df_plan, options, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[[paste0("aoqCurve", planType)]])) {
    return ()
  }
  aoqCurve <- createJaspPlot(title = paste0("AOQ (Average Outgoing Quality) curve"),  width = 480, height = 320)
  aoqCurve$dependOn(depend_variables)
  jaspResults[[paste0("aoqCurve", planType)]] <- aoqCurve

  N <-  options[[paste0("lotSize", planType)]]
  n <- options[[paste0("sampleSize", planType)]]
  c <- options[[paste0("acceptNumber", planType)]]
  r <- options[[paste0("rejectNumber", planType)]]
  dist <- options[[paste0("distribution", planType)]]
  pd <- df_plan$PD
  AOQ <- numeric(length(pd))
  if (planType == "Single") {
    AOQ <- df_plan$PA * pd * (N-n) / N
  } else {
    stages <- length(n)
    probs_prod <- 1
    cum_n <- 0
    n_def <- pd * N
    for (i in 1:stages) {
      cum_n <- cum_n + n[i]
      p_acc_rej_i <- getProbability(N, n[i], c[i], r[i], dist, pd, n_def)
      # p_acc_rej_i <- getDecisionProbability(N, n, c, r, dist, pd, n_def, i)
      pAcc_i <- unlist(p_acc_rej_i[1])
      pDecide_i <- pAcc_i + unlist(p_acc_rej_i[2])
      AOQ <- AOQ + (N - cum_n) * pAcc_i * probs_prod
      probs_prod <- probs_prod * (1 - pDecide_i)
    }
    AOQ <- AOQ * pd / N
  }
  df_plan$AOQ <- AOQ
  aoq_max <- round(max(df_plan$AOQ),2)
  pd_aoq_max <- df_plan$PD[df_plan$AOQ == max(df_plan$AOQ)]
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = AOQ)) + 
                         ggplot2::geom_point(colour = "black", shape = 19) + ggplot2::labs(x = "Proportion non-confirming", y = "AOQ") +
                         ggplot2::geom_line(colour = "black", linetype = "dashed") +
                         ggplot2::geom_hline(yintercept = max(df_plan$AOQ), linetype = "dashed") +
                         ggplot2::annotate("text", label = gettextf("AOQL: %.2f", aoq_max), x = pd_aoq_max*0.9, y = aoq_max*1.1, color = "black", size = 6) +
                         ggplot2::ylim(0.0,round(aoq_max*1.1, 2))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  aoqCurve$plotObject <- plt
}

# Generate the average total inspection curve for the plan.
getATICurve <- function(jaspResults, df_plan, options, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[[paste0("atiCurve", planType)]])) {
    return ()
  }
  atiCurve <- createJaspPlot(title = paste0("ATI (Average Total Inspection) curve"),  width = 480, height = 320)
  atiCurve$dependOn(depend_variables)
  jaspResults[[paste0("atiCurve", planType)]] <- atiCurve

  N <-  options[[paste0("lotSize", planType)]]
  n <- options[[paste0("sampleSize", planType)]]
  c <- options[[paste0("acceptNumber", planType)]]
  r <- options[[paste0("rejectNumber", planType)]]
  dist <- options[[paste0("distribution", planType)]]
  pd <- df_plan$PD
  ATI <- numeric(length(pd))
  if (planType == "Single") {
    ATI <- df_plan$PA * n + (1 - df_plan$PA) * N
  } else {
    stages <- length(n)
    probs_prod <- 1
    cum_n <- 0
    n_def <- pd * N
    for (i in 1:stages) {
      cum_n <- cum_n + n[i]
      p_acc_rej_i <- getProbability(N, n[i], c[i], r[i], dist, pd, n_def)
      # p_acc_rej_i <- getDecisionProbability(N, n, c, r, dist, pd, n_def, i)
      pAcc_i <- unlist(p_acc_rej_i[1])
      pRej_i <- unlist(p_acc_rej_i[2])
      pDecide_i <- pAcc_i + pRej_i
      ATI <- ATI + (pAcc_i * cum_n + pRej_i * N) * probs_prod
      probs_prod <- probs_prod * (1 - pDecide_i)
    }
  }
  df_plan$ATI <- ATI
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = ATI)) + 
                         ggplot2::geom_point(colour = "black", shape = 19) + 
                         ggplot2::geom_line(colour = "black", linetype = "dashed") +
                         ggplot2::labs(x = "Proportion non-confirming", y = "ATI")
                        #  ggplot2::theme_classic() +
                        #  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16, color = "black"), axis.text.y = ggplot2::element_text(size = 16, color = "black"),
                        #                 axis.title.x = ggplot2::element_text(size = 20, color = "black"), axis.title.y = ggplot2::element_text(size = 20, color = "black"))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  atiCurve$plotObject <- plt
}

# Generate the average sample number curve for the plan. Only applicable for multiple sampling plans.
getASNCurve <- function(jaspResults, options, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[["asnPlot"]])) {
    return ()
  }
  # Parse option values
  n <- options$sampleSizeMult
  c <- options$acceptNumberMult
  r <- options$rejectNumberMult
  dist <- options$distributionMult
  N <- options$lotSizeMult
  pd_lower <- options$pd_lowerMult
  pd_upper <- options$pd_upperMult
  pd_step <- options$pd_stepMult
  pd = seq(pd_lower, pd_upper, pd_step)
  n_def <- pd * N
  stages <- length(n)
  num_values <- length(pd)
  ASN <- numeric(num_values)
  probs_prod <- 1
  cum_n <- 0
  for (i in 1:(stages-1)) {
    cum_n <- cum_n + n[i]
    prob_acc_rej_i <- getProbability(N, n[i], c[i], r[i], dist, pd, n_def)
    # prob_acc_rej_i <- getDecisionProbability(N, n, c, r, dist, pd, n_def, i)
    pDecide_i <- as.numeric(unlist(prob_acc_rej_i[1]) + unlist(prob_acc_rej_i[2]))
    ASN <- ASN + cum_n * pDecide_i * probs_prod
    probs_prod <- probs_prod * (1 - pDecide_i)
  }
  ASN <- ASN + (cum_n + n[stages]) * probs_prod
  df_asn <- data.frame(PD = pd, ASN = ASN)

  # Draw ASN plot
  asnPlot <- createJaspPlot(title = "ASN Curve for multiple sampling plan",  width = 480, height = 320)
  asnPlot$dependOn(depend_variables)
  jaspResults[["asnPlot"]] <- asnPlot
  plt <- ggplot2::ggplot(data = df_asn, ggplot2::aes(x = PD, y = ASN)) + 
         ggplot2::geom_point(colour = "black", shape = 19) + 
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::labs(x = "Proportion non-confirming", y = "Average Sample Number")
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  asnPlot$plotObject <- plt
}

# Currently in use. Need to modify.
# TODO [############# IN PROGRESS ##############]
getProbability <- function(N, n, c, r, dist, pd, n_def) {
  pAcc <- NULL
  pRej <- NULL
  if (dist == "binom") {
    pAcc <- pbinom(c(c), size = n, prob = pd, lower.tail = TRUE)
    pRej <- pbinom(c(r - 1), size = n, prob = pd, lower.tail = FALSE)
  } else if (dist == "hypergeom") {
    pAcc <- phyper(c(c), m = n_def, n = N - n_def, k = n, lower.tail = TRUE)
    pRej <- phyper(c(r - 1), m = n_def, n = N - n_def, k = n, lower.tail = FALSE)         
  } else if (dist == "poisson") {
    pAcc <- ppois(c(c), lambda = pd*n, lower.tail = TRUE)
    pRej <- ppois(c(r - 1), lambda = pd*n, lower.tail = FALSE)
  }
  return (list(pAcc,pRej))
}

# TODO [############# IN PROGRESS ##############]
# getProbBetween <- function(N, n, low, high, dist, pd, n_def) {
#   prob <- NULL
#   if (dist == "binom") {
#     prob <- pbinom(c(high), size = n, prob = pd, lower.tail = TRUE) - pbinom(c(low), size = n, prob = pd, lower.tail = TRUE)
#   } else if (dist == "hypergeom") {
#     prob <- phyper(c(high), m = n_def, n = N - n_def, k = n, lower.tail = TRUE) - phyper(c(low), m = n_def, n = N - n_def, k = n, lower.tail = TRUE)  
#   } else if (dist == "poisson") {
#     prob <- ppois(c(high), lambda = pd*n, lower.tail = TRUE) - ppois(c(low), lambda = pd*n, lower.tail = TRUE)
#   }
#   return (prob)
# }

# TODO [############# IN PROGRESS ##############]
getCumulativeProb <- function(N, n, c, dist, pd, n_def, lower_tail) {
  prob <- NULL
  if (dist == "binom") {
    prob <- pbinom(q = c, size = n, prob = pd, lower.tail = lower_tail)
  } else if (dist == "hypergeom") {
    prob <- phyper(q = c, m = n_def, n = N - n_def, k = n, lower.tail = lower_tail)
  } else if (dist == "poisson") {
    prob <- ppois(q = c, lambda = pd*n, lower.tail = lower_tail)
  }
  return (prob)
}

# TODO [############# IN PROGRESS ##############]
getPointProb <- function(N, n, c, dist, pd, n_def) {
  prob <- NULL
  if (dist == "binom") {
    prob <- dbinom(x = c, size = n, prob = pd)
  } else if (dist == "hypergeom") {
    prob <- dhyper(x = c, m = n_def, n = N - n_def, k = n)
  } else if (dist == "poisson") {
    prob <- dpois(x = c, lambda = pd*n)
  }
  return (prob)
}

# TODO [############# IN PROGRESS ##############]
# Right now, it's taking the previous prob of only the previous stages,
# but it needs to take the product of the probability of all the previous stages, with d1,d2,.. and so on.
getDecisionProbability <- function(N, n, c, r, dist, pd, n_def, i) {
  acc <- NULL
  rej <- NULL
  if (i == 1) {
    acc <- getCumulativeProb(N, n[i], c[i], dist, pd, n_def, TRUE)
    rej <- getCumulativeProb(N, n[i], r[i] - 1, dist, pd, n_def, FALSE)
  } else {
    for (stage in seq(1:i))
    d_vals <- seq(c[i-1]+1, r[i-1]-1, 1)
    for (d in d_vals) {
      prob_prev_d <- getPointProb(N, n[i-1], d, dist, pd, n_def)
      prob_acc_cur_d <- getCumulativeProb(N, n[i], c[i] - d, dist, pd, n_def, TRUE)
      acc <- acc + prob_prev_d*prob_acc_cur_d
      prob_rej_cur_d <- getCumulativeProb(N, n[i], r[i] - d - 1, dist, pd, n_def, FALSE)
      rej <- rej + prob_prev_d*prob_rej_cur_d
    }
  }
  return (list(acc,rej))
}