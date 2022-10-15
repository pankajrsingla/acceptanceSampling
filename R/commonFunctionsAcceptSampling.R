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

getPlanDf <- function(options, planType, returnPlan=FALSE) {
    N <-  options[[paste0("lotSize", planType)]]
    n <- options[[paste0("sampleSize", planType)]]
    c <- options[[paste0("acceptNumber", planType)]]
    r <- options[[paste0("rejectNumber", planType)]]
    pd_lower <- options[[paste0("pd_lower", planType)]]
    pd_upper <- options[[paste0("pd_upper", planType)]]
    pd_step <- options[[paste0("pd_step", planType)]]
    dist = options[[paste0("distribution", planType)]]

    plan <- NULL
    if (dist == "hypergeom") {
      plan <- AcceptanceSampling::OC2c(N = N, n = n, c = c, r = r, type = dist, pd = seq(pd_lower, pd_upper, pd_step))
    } else {
      plan <- AcceptanceSampling::OC2c(n = n, c = c, r = r, type = dist, pd = seq(pd_lower, pd_upper, pd_step))
    }
    df_plan <- data.frame(PD = plan@pd, PA = plan@paccept)
    if (returnPlan) {
      return (list(plan, df_plan))
    } else {
      return (df_plan)
    }
}

getOCCurve <- function(jaspResults, df_plan, depend_variables) {
  ocPlot <- createJaspPlot(title = paste0("OC (Operating Characteristics) curve"),  width = 320, height = 320)
  ocPlot$dependOn(depend_variables)
  jaspResults[["ocPlot"]] <- ocPlot
  plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = PA)) + 
                      ggplot2::geom_point(colour = "black", shape = 24) + ggplot2::labs(x = "Proportion non-confirming", y = "P(accept)")
  plt <- jaspGraphs::themeJasp(plt)
  ocPlot$plotObject <- plt
}

getSummary <- function(jaspResults, df_plan, depend_variables) {
  summaryTable <- createJaspTable(title = "Detailed acceptance probabilities")
  summaryTable$dependOn(depend_variables)
  summaryTable$addColumnInfo(name = "col_1", title = "Prop. non-confirming", type = "number")
  summaryTable$addColumnInfo(name = "col_2", title = " P(accept)", type = "number")
  summaryTable$setData(list(col_1 = df_plan$PD, col_2 = df_plan$PA))
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["summaryTable"]] <- summaryTable
  return (summaryTable)
}

assessPlan <- function(jaspResults, options, planType, depend_variables) {
  pd_prp <- options[[paste0("pd_prp", planType)]]
  pa_prp <- options[[paste0("pa_prp", planType)]]
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
      plan_table$addRows(list("table_1_col_1" = "Rej. Number(s)", "table_1_col_2" = r))
    } else {
      plan_table$addColumnInfo(name = "table_1_col_1", title = "Sample", type = "integer")
      plan_table$addColumnInfo(name = "table_1_col_2", title = "Sample Size", type = "integer")
      plan_table$addColumnInfo(name = "table_1_col_3", title = "Cum. Sample Size", type = "integer")
      plan_table$addColumnInfo(name = "table_1_col_4", title = "Acc. Number", type = "integer")
      plan_table$addColumnInfo(name = "table_1_col_5", title = "Rej. Number", type = "integer")
      # rows <- length(options$sampleSizeMult)
      # table_df_mult <- data.frame(sample = 1:rows, sample_size = n, cum_sample_size = cumsum(n),
      #                             acc_num = c, rej_num = r)
      plan_table$setData(list(table_1_col_1 = 1:length(options$sampleSizeMult), table_1_col_2 = n, table_1_col_3 = cumsum(n), table_1_col_4 = c, table_1_col_5 = r))
    }
    plan_table$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["plan_table"]] <- plan_table

    # 2. Table with the specified and actual acceptance probabilities
    risk_table <- getRiskPointTable(jaspResults, assess, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp)
  }
}

getRiskPointTable <- function(jaspResults, assess, depend_variables, pd_prp, pa_prp, pd_crp, pa_crp) {
  table <- createJaspTable(title = as.character(assess[8]))
  table$dependOn(depend_variables)
  table$addColumnInfo(name = "col_1", title = "", type = "string")
  table$addColumnInfo(name = "col_2", title = "Quality", type = "number")
  table$addColumnInfo(name = "col_3", title = "RP P(accept)", type = "number")
  table$addColumnInfo(name = "col_4", title = "Plan P(accept)", type = "number")
  table$addRows(list("col_1" = "PRP", "col_2" = pd_prp, "col_3" = pa_prp, "col_4" = as.numeric(unlist(strsplit(assess[11], " +"))[4])))
  table$addRows(list("col_1" = "CRP", "col_2" = pd_crp, "col_3" = pd_crp, "col_4" = as.numeric(unlist(strsplit(assess[12], " +"))[4])))
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["riskTable"]] <- table
  return (table)
}

getAOQCurve <- function(jaspResults, options, depend_variables) {
  # add code
}

getATICurve <- function(jaspResults, options, depend_variables) {
  # add code
}

getASNCurve <- function(jaspResults, options, depend_variables) {
    # Parse option values
    n <- options$sampleSizeMult
    c <- options$acceptNumberMult
    r <- options$rejectNumberMult
    dist <- options$distributionMult
    N <- options$lotSizeMult

    pd <- seq(0,1,0.01) # To-do: Add option to specify pd
    n_def <- pd * N
    stages <- length(n)
    num_values <- length(pd)
    ASN <- numeric(num_values)
    pacc_i <- numeric(num_values)
    prej_i <- numeric(num_values)    
    probs_prod <- 1
    cum_n <- 0
    for (i in 1:(stages-1)) {
        cum_n <- cum_n + n[i]
        if (dist == "binom") {
            pacc_i <- pbinom(c(c[i]), size = n[i], prob = pd, lower.tail = TRUE)
            prej_i <- pbinom(c(r[i] - 1), size = n[i], prob = pd, lower.tail = FALSE)
        } else if (dist == "hypergeom") {
            pacc_i <- phyper(c(c[i]), m = n_def, n = N - n_def, k = n[i], lower.tail = TRUE)
            prej_i <- phyper(c(r[i] - 1), m = n_def, n = N - n_def, k = n[i], lower.tail = FALSE)         
        } else if (dist == "poisson") {
            pacc_i <- ppois(c(c[i]), lambda = pd*n[i], lower.tail = TRUE)
            prej_i <- ppois(c(r[i] - 1), lambda = pd*n[i], lower.tail = FALSE)
        }
        prob_i <- pacc_i + prej_i
        ASN <- ASN + cum_n * prob_i * probs_prod
        probs_prod <- probs_prod * (1 - prob_i)
    }
    ASN <- ASN + (cum_n + n[stages]) * probs_prod
    df_asn <- data.frame(PD = pd, ASN = ASN)

    # Draw ASN plot
    asnPlot <- createJaspPlot(title = "ASN Curve for multiple sampling plan",  width = 320, height = 320)
    asnPlot$dependOn(depend_variables)
    jaspResults[["asnPlot"]] <- asnPlot
    plt <- ggplot2::ggplot(data = df_asn, ggplot2::aes(x = PD, y = ASN)) + 
                        ggplot2::geom_point(colour = "black") + ggplot2::labs(x = "Proportion non-confirming", y = "Average Sample Number")
    plt <- jaspGraphs::themeJasp(plt)
    asnPlot$plotObject <- plt
    return (asnPlot)
}