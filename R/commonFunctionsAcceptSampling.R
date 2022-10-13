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

getPlanDf <- function(sampleSize, acceptNumber, rejectNumber, distribution, lotSize=NULL, returnPlan=FALSE) {
    plan <- NULL
    if (distribution == "hypergeom") {
      plan <- AcceptanceSampling::OC2c(N = lotSize, n = sampleSize, c = acceptNumber, r = rejectNumber, type = distribution)
    } else {
      plan <- AcceptanceSampling::OC2c(n = sampleSize, c = acceptNumber, r = rejectNumber, type = distribution)
    }
    df_plan <- data.frame(PD = plan@pd, PA = plan@paccept)
    if (returnPlan) {
      return (list(plan, df_plan))
    } else {
      return (df_plan)
    }
}

getPlot <- function(jaspResults, df_plan, variables, identity, index=NULL) {
    ocPlot <- createJaspPlot(title = paste0("OC curve for ", identity, " sampling plan(s)"),  width = 320, height = 320)
    ocPlot$dependOn(variables)
    jaspResults[[paste0("ocPlot", index)]] <- ocPlot
    plt <- ggplot2::ggplot(data = df_plan, ggplot2::aes(x = PD, y = PA)) + 
                        ggplot2::geom_point(colour = "black", shape = 24) + ggplot2::labs(x = "Proportion defective", y = "P(accept)")
    plt <- jaspGraphs::themeJasp(plt)
    ocPlot$plotObject <- plt
    return (ocPlot)
}

printSummary <- function(jaspResults, df_plan, variables) {
  table <- createJaspTable(title = "Detailed acceptance probabilities:")
  table$dependOn(variables)
  table$addColumnInfo(name = "col_1", title = "Prop. defective", type = "number")
  table$addColumnInfo(name = "col_2", title = " P(accept)", type = "number")
  table$setData(list(col_1 = df_plan$PD, col_2 = df_plan$PA))
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["table"]] <- table
  return (table)
}

getRiskPointTable <- function(jaspResults, variables, assess, pd_prp, pa_prp, pd_crp, pa_crp) {
  table <- createJaspTable(title = as.character(assess[8]))
  table$dependOn(variables)
  table$addColumnInfo(name = "col_1", title = "", type = "string")
  table$addColumnInfo(name = "col_2", title = "Quality", type = "number")
  table$addColumnInfo(name = "col_3", title = "RP P(accept)", type = "number")
  table$addColumnInfo(name = "col_4", title = "Plan P(accept)", type = "number")
  table$addRows(list("col_1" = "PRP", "col_2" = as.numeric(pd_prp), "col_3" = as.numeric(pa_prp), "col_4" = as.numeric(unlist(strsplit(assess[11], " +"))[4])))
  table$addRows(list("col_1" = "CRP", "col_2" = as.numeric(pd_crp), "col_3" = as.numeric(pd_crp), "col_4" = as.numeric(unlist(strsplit(assess[12], " +"))[4])))
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["table"]] <- table
  return (table)
}

getASNCurve <- function(jaspResults, options, variables) {
    # Parse option values
    n <- options$sampleSizeMult
    c <- options$acceptNumberMult
    r <- options$rejectNumberMult
    dist <- options$distributionMult
    N <- options$lotSizeMult

    pd <- seq(0,1,0.01) # To-do: Add option to specify pd
    stages <- length(n)
    num_values <- length(pd)
    ASN <- numeric(num_values)
    probs_prod <- 1
    cum_n <- 0
    for (i in 1:(stages-1)) {
        cum_n <- cum_n + n[i]
        pacc_i <- numeric(num_values)
        prej_i <- numeric(num_values)
        if (dist == "binom") {
            pacc_i <- pbinom(c(c[i]), size = n[i], prob = pd, lower.tail = TRUE)
            prej_i <- pbinom(c(r[i] - 1), size = n[i], prob = pd, lower.tail = FALSE)
        } else if (dist == "hypergeom") {
            n_def <- pd * N
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
    asnPlot$dependOn(variables)
    jaspResults[["asnPlot"]] <- asnPlot
    plt <- ggplot2::ggplot(data = df_asn, ggplot2::aes(x = PD, y = ASN)) + 
                        ggplot2::geom_point(colour = "black") + ggplot2::labs(x = "Proportion defective", y = "Average Sample Number")
    plt <- jaspGraphs::themeJasp(plt)
    asnPlot$plotObject <- plt
    return (asnPlot)
}