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
  dist = options[[paste0("distribution", planType)]]

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
#' @param jaspResults <>.
#' @param df_plan <>.
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' getSummary(jaspResults, df_plan, planType, depend_variables, positionInContainer)
getSummary <- function(jaspResults, df_plan, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[[paste0("summaryTable", planType)]])) {
    return()
  }
  summaryTable <- createJaspTable(title = "Detailed acceptance probabilities")
  summaryTable$dependOn(depend_variables)
  summaryTable$addColumnInfo(name = "col_1", title = "Prop. non-conforming", type = "number")
  summaryTable$addColumnInfo(name = "col_2", title = " P(accept)", type = "number")
  summaryTable$setData(list(col_1 = round(df_plan$PD,2), col_2 = round(df_plan$PA,2)))
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  summaryTable$position <- positionInContainer
  jaspResults[[paste0("summaryTable", planType)]] <- summaryTable
}

# txt = "Check if the plan can satisfy the AQL and RQL constraints. Create tabular output."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------------------------------
##  Check if the plan can satisfy the AQL and RQL constraints. Create tabular output.  --
##---------------------------------------------------------------------------------------
#' @param jaspResults <>.
#' @param options <>.
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getOCCurve()] for operating characteristics of the plan.
#' @examples
#' assessPlan(jaspResults, options, planType, depend_variables, positionInContainer)
assessPlan <- function(jaspResults, options, planType, depend_variables, positionInContainer) {
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
    if (is.null(jaspResults[[paste0("plan_table", planType)]])) {
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
        plan_table$addRows(list("table_1_col_1" = "Sample size(s)", "table_1_col_2" = n))
        plan_table$addRows(list("table_1_col_1" = "Acc. Number(s)", "table_1_col_2" = c))
        # plan_table$addRows(list("table_1_col_1" = "Rej. Number(s)", "table_1_col_2" = r))
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
      jaspResults[[paste0("plan_table", planType)]] <- plan_table
    }

    # 2. Table with the specified and actual acceptance probabilities
    getRiskPointTable(jaspResults, assess, planType, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp, positionInContainer+1)
  }
}

# txt = "Create the table showing the specified risk quality levels and their acceptance probabilities."
# banner(txt, centre = TRUE, bandChar = "-")
##-------------------------------------------------------------------------------------------------------------------
##  Create the table showing the specified risk quality levels and their acceptance probabilities.  --
##-------------------------------------------------------------------------------------------------------------------
#' @param jaspResults <>.
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
#' getRiskPointTable(jaspResults, assess, planType, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp, positionInContainer)
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

# txt = "Generate the operating characteristics curve for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##  Generate the operating characteristics curve for the plan.  --
##----------------------------------------------------------------
#' @param jaspResults <>.
#' @param df_plan <>.
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getOCCurve(jaspResults, df_plan, planType, depend_variables, positionInContainer)
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
                  ggplot2::labs(x = "Proportion non-conforming", y = "P(accept)")
                  # ggplot2::theme_classic() +
                  # ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16, color = "black"), 
                                  # axis.text.y = ggplot2::element_text(size = 16, color = "black"),
                                  # axis.title.x = ggplot2::element_text(size = 20, color = "black"), 
                                  # axis.title.y = ggplot2::element_text(size = 20, color = "black"))
  # plt <- jaspGraphs::themeJasp(plt)
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  ocCurve$plotObject <- plt
}

# txt = "Generate the average outgoing quality curve for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##  Generate the average outgoing quality curve for the plan.  --
##---------------------------------------------------------------
#' @param jaspResults <>.
#' @param df_plan <>.
#' @param options <>
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getAOQCurve(jaspResults, df_plan, options, planType, depend_variables, positionInContainer)
getAOQCurve <- function(jaspResults, df_plan, options, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[[paste0("aoqCurve", planType)]])) {
    return ()
  }
  aoqCurve <- createJaspPlot(title = paste0("AOQ (Average Outgoing Quality) curve"),  width = 480, height = 320)
  aoqCurve$dependOn(depend_variables)
  jaspResults[[paste0("aoqCurve", planType)]] <- aoqCurve

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
    cum_n <- 0
    n_def <- pd * N
    # probs <- getStageWiseProbability()
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
                         ggplot2::geom_point(colour = "black", shape = 19) + ggplot2::labs(x = "Proportion non-conforming", y = "AOQ") +
                         ggplot2::geom_line(colour = "black", linetype = "dashed") +
                         ggplot2::geom_hline(yintercept = max(df_plan$AOQ), linetype = "dashed") +
                         ggplot2::annotate("text", label = gettextf("AOQL: %.2f", aoq_max), 
                                            x = pd_aoq_max*0.9, y = aoq_max*1.1, color = "black", size = 6) +
                         ggplot2::ylim(0.0,round(aoq_max*1.2, 2))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  aoqCurve$plotObject <- plt
}

# txt = "Generate the average total inspection curve for the plan."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------
##  Generate the average total inspection curve for the plan.  --
##---------------------------------------------------------------
#' @param jaspResults <>.
#' @param df_plan <>.
#' @param options <>
#' @param planType "Single" or "Mult".
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getATICurve(jaspResults, df_plan, options, planType, depend_variables, positionInContainer)
getATICurve <- function(jaspResults, df_plan, options, planType, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[[paste0("atiCurve", planType)]])) {
    return ()
  }
  atiCurve <- createJaspPlot(title = paste0("ATI (Average Total Inspection) curve"),  width = 480, height = 320)
  atiCurve$dependOn(depend_variables)
  jaspResults[[paste0("atiCurve", planType)]] <- atiCurve

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
                         ggplot2::labs(x = "Proportion non-conforming", y = "ATI")
                        #  ggplot2::theme_classic() +
                        #  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16, color = "black"), 
                                          # axis.text.y = ggplot2::element_text(size = 16, color = "black"),
                                          # axis.title.x = ggplot2::element_text(size = 20, color = "black"), 
                                          # axis.title.y = ggplot2::element_text(size = 20, color = "black"))
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  atiCurve$plotObject <- plt
}

# txt = "Generate the average sample number curve for the plan. Only applicable for multiple sampling plans."
# banner(txt, centre = TRUE, bandChar = "-")
##---------------------------------------------------------------------------------------------------------
##  Generate the average sample number curve for the plan. Only applicable for multiple sampling plans.  --
##---------------------------------------------------------------------------------------------------------
#' @param jaspResults <>.
#' @param options <>
#' @param depend_variables <>.
#' @param positionInContainer <>.
#' @seealso
#'   [getSummaryTable()] for the summary table of the plan.
#' @examples
#' getASNCurve(jaspResults, options, depend_variables, positionInContainer) 
getASNCurve <- function(jaspResults, options, depend_variables, positionInContainer) {
  if (!is.null(jaspResults[["asnPlot"]])) {
    return ()
  }
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
  n_def <- pd * N
  stages <- length(n)
  num_values <- length(pd)
  ASN <- numeric(num_values)
  probs_prod <- 1
  cum_n <- 0
  stage_probs <- getStageProbability(n,c,r,pd,"binom")
  stage_probs <- stage_probs[[1]] + stage_probs[[2]] # Decision prob = p_acc + p_rej
  for (i in 1:(stages-1)) {
    cum_n <- cum_n + n[i]
    # prob_acc_rej_i <- getProbability(N, n[i], c[i], r[i], dist, pd, n_def)
    # prob_acc_rej_i <- getDecisionProbability(N, n, c, r, dist, pd, n_def, i)
    # pDecide_i <- as.numeric(unlist(prob_acc_rej_i[1]) + unlist(prob_acc_rej_i[2]))
    # ASN <- ASN + cum_n * pDecide_i * probs_prod
    # probs_prod <- probs_prod * (1 - pDecide_i)
    pDecide_i <- stage_probs[i,]
    ASN <- ASN + pDecide_i * cum_n
  }
  # ASN <- ASN + (cum_n + n[stages]) * probs_prod
  df_asn <- data.frame(PD = pd, ASN = ASN)

  # Draw ASN plot
  asnPlot <- createJaspPlot(title = "ASN Curve for multiple sampling plan",  width = 480, height = 320)
  asnPlot$dependOn(depend_variables)
  jaspResults[["asnPlot"]] <- asnPlot
  plt <- ggplot2::ggplot(data = df_asn, ggplot2::aes(x = PD, y = ASN)) + 
         ggplot2::geom_point(colour = "black", shape = 19) + 
         ggplot2::geom_line(colour = "black", linetype = "dashed") +
         ggplot2::labs(x = "Proportion non-conforming", y = "Average Sample Number")
  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw()
  plt$position <- positionInContainer
  asnPlot$plotObject <- plt
}

getStageProbabilityBinom <- function(pd,n,c,r) {
  num_stages <- length(n)
  acc_probs <- matrix(nrow = num_stages, ncol = length(pd))
  rej_probs <- matrix(nrow = num_stages, ncol = length(pd))
  k.s <- num_stages ## number of stages in this sampling

  prob.acc <- function(x, n, p){
    k <- length(x)
    k1 <- k-2
    prod(dbinom(x[1:k1], n[1:k1], p))*pbinom(x[k-1], n[k-1], p)
  }

  prob.rej <- function(x, n, p){
    k <- length(x)
    k1 <- k-2
    prod(dbinom(x[1:k1], n[1:k1], p))*pbinom(x[k], n[k-1], p, lower.tail = FALSE)
  }
  
  for (k in 1:k.s) {
    ## For each stage, find out all the possibilities which could
    ## lead to still not having made a decision and then calculate
    ## the appropriate probabilities.

    if(k==1) {
      ## Only a single sampling stage to do - this is simple
      p.acc <- sapply(pd, FUN=function(el) {
        pbinom(q=c[1],size=n[1],prob=el)})
      acc_probs[k,] = p.acc
      p.rej <- sapply(pd, FUN=function(el){
        pbinom(q=r[1]-1,size=n[1],prob=el, lower.tail = FALSE)})
      rej_probs[k,] = p.rej
      ## p.acc and p.rej now exist and can be used in the following stages.
    }
    else if (k==2) {
      ## Two sampling stages. Needs to be handled separately from
      ## more stages due to matrix dimensions
      c.s <- c+1 ## Use to calculate limits
      r.s <- r-1 ## Use to calculate limits
      ## The possibilities which lead to a decision to be made at
      ## the second stage
      x <- data.frame(X1=seq(c.s[1], r.s[1], by=1),
                      X.acc=c[2]-seq(c.s[1], r.s[1], by=1),
                      X.rej=r[2]-1-seq(c.s[1], r.s[1], by=1))
      p.acc_2 <- sum(apply(x, 1, FUN=prob.acc, n=n, p=pd))
      p.acc <- p.acc + p.acc_2
      acc_probs[k,] = p.acc_2
      p.rej_2 <- sum(apply(x, 1, FUN=prob.rej, n=n, p=pd))
      p.rej <- p.rej + p.rej_2
      rej_probs[k,] = p.rej_2
    }
    else {
      ## More than two sampling stages.
      ## Things are more tricky.
      c.s <- c+1 ## Use to calculate limits
      r.s <- r-1 ## Use to calculate limits
      expand.call <- "expand.grid(c.s[k-1]:r.s[k-1]"
      for(i in 2:(k-1)){
        expand.call <- paste(expand.call,paste("c.s[k-",i,"]:r.s[k-",i,"]",sep=""),sep=",")        
      }
      expand.call <- paste(expand.call,")",sep="")
      x <- eval(parse(text=expand.call)[[1]])
      x <- x[,(k-1):1] # Reverses the order of columns in dataframe x
      names(x) <- paste("X",1:(k-1),sep="")
      
      for(i in ncol(x):2){
        x[,i] <- x[,i]-x[,i-1]
      }
      x <- cbind(x, X.acc=c[k] - rowSums(x[,1:(k-1)]))
      x <- cbind(x, X.rej=r[k]-1 - rowSums(x[,1:(k-1)]))
      p.acc_k <- sum(apply(x, 1, FUN=prob.acc, n=n, p=pd))
      p.acc <- p.acc + p.acc_k
      acc_probs[k,] = p.acc_k
      p.rej_k <- sum(apply(x, 1, FUN=prob.rej, n=n, p=pd))
      p.rej <- p.rej + p.rej_k
      rej_probs[k,] = p.rej_k
    }
  }
  return(list(acc_probs,rej_probs))
}

getStageProbabilityHyper <- function(pd,n,c,r) {
  return ()
}

getStageProbabilityPois <- function(pd,n,c,r) {
  return ()
}

getStageProbability <- function(n,c,r,pd, dist="binom") {
  if (dist == "binom") {
    stage_probs <- sapply(pd, FUN=getStageProbabilityBinom, n=n, c=c, r=r)
  }
  # else if (dist == "hypergeom") {
  #   return ()
  #   # stage_probs <- sapply(pd, FUN=getStageProbabilityHyper, n=n, c=c, r=r)
  # } else if (dist == "poisson") {
  #   return ()
  #   # stage_probs <- sapply(pd, FUN=getStageProbabilityPois, n=n, c=c, r=r)
  # }
  acc <- matrix(unlist(stage_probs[1,]), byrow=FALSE, nrow=length(n))
  rej <- matrix(unlist(stage_probs[2,]), byrow=FALSE, nrow=length(n))
  # print(colSums(acc))
  # print(colSums(rej))
  return (list(acc,rej))
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