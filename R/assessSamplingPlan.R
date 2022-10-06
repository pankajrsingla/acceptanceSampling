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

# This is a temporary fix
# TODO: remove it when R solves this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

AssessSamplingPlan <- function(jaspResults, dataset = NULL, options, ...) {
  optionNames <- c("lotSize", "sampleSize", "acceptNumber", "rejectNumber", "pd_prp", "pa_prp", "pd_crp", "pa_crp")
  optionNames_mult <- c("lotSize_mult", "sampleSize_mult", "acceptNumber_mult", "rejectNumber_mult", "pd_prp_mult", "pa_prp_mult", "pd_crp_mult", "pa_crp_mult")
  options <- .parseAndStoreFormulaOptions(jaspResults, options, names = optionNames)

  .assessSampleCheckErrors(dataset, options)
  # Single
  if (options$showOCCurveSingle) {
    .createOCCurveAssessSingle(jaspResults, options)
  }
  .assessPlanSingle(jaspResults, options, optionNames)
  # Multiple
  if (options$showOCCurveMult) {
    .createOCCurveAssessMult(jaspResults, options)
  }
  .assessPlanMult(jaspResults, options, optionNames_mult)
}


.assessSampleCheckErrors <- function(dataset = NULL, options) {
  # perform a check on the hypothesis

  sanityCheckAssess <- function(lotSize, sampleSize, acceptNumber, rejectNumber) {
    if (sampleSize > lotSize)
      return(gettext("Sample size cannot be greater than the lot size."))
    else if (acceptNumber > sampleSize)
      return(gettext("Acceptance number cannot be greater than the sample size."))
    else if (rejectNumber > sampleSize)
      return(gettext("Rejection number cannot be greater than the sample size."))
    else if (rejectNumber < acceptNumber)
      return(gettext("Rejection number cannot be smaller than the acceptance number."))
  }
  
  # Error Check 1: Number of levels of the variables and the hypothesis
  .hasErrors(
    dataset              = dataset,
    custom               = sanityCheckAssess,
    type                 = "factorLevels",
    factorLevels.target  = options$sampleSize,
    factorLevels.amount  = "< 1",
    exitAnalysisIfErrors = TRUE
  )
}

# Results functions ----

# Generate OC Curve:
.createOCCurveAssessSingle <- function(jaspResults, options) {
  if ((options$sampleSize > 0) && (options$acceptNumber > 0) && (options$rejectNumber > 0)) {
    # Create sampling plan
    x <- NULL
    if (options$distribution == "hypergeom") {
      # Need to provide the lot size (N) for hypergeometric distribution.
      x <- AcceptanceSampling::OC2c(N = options$lotSize, n = options$sampleSize, c = options$acceptNumber, r = options$rejectNumber, type = options$distribution)
    } else {
      x <- AcceptanceSampling::OC2c(n = options$sampleSize, c = options$acceptNumber, r = options$rejectNumber, type = options$distribution)
    }
    # res1 <- summary(x1, full = FALSE)
    df_x <- data.frame(PD = x@pd, PA = x@paccept)
    # Generate plot
    ocPlot_single <- createJaspPlot(title = "OC curve for single sampling plan",  width = 320, height = 320)
    ocPlot_single$dependOn(c("showOCCurveSingle", "lotSize", "sampleSize", "acceptNumber", "rejectNumber", "distribution"))
    jaspResults[["ocPlot_single"]] <- ocPlot_single
    plot_single <- ggplot2::ggplot(data = df_x, ggplot2::aes(x = PD, y = PA)) + 
                        ggplot2::geom_point(colour = "black", shape = 24) + ggplot2::labs(x = "Proportion defective", y = "P(accept)")
    ocPlot_single$plotObject <- plot_single
  }
}

.createOCCurveAssessMult <- function(jaspResults, options) {
  if (length(options$sampleSize_mult) > 0 && length(options$acceptNumber_mult) > 0 && length(options$rejectNumber_mult) > 0) {
    # Create sampling plan
    x_mult <- NULL
    if (options$distribution_mult == "hypergeom") {
      # Need to provide the lot size (N) for hypergeometric distribution.
      x_mult <- AcceptanceSampling::OC2c(N = options$lotSize_mult, n = options$sampleSize_mult, c = options$acceptNumber_mult, r = options$rejectNumber_mult, type = options$distribution_mult)
    } else {
      x_mult <- AcceptanceSampling::OC2c(n = options$sampleSize_mult, c = options$acceptNumber_mult, r = options$rejectNumber_mult, type = options$distribution_mult)
    }
    # res1 <- summary(x1, full = FALSE)
    df_x_mult <- data.frame(PD = x_mult@pd, PA = x_mult@paccept)
    # Generate plot
    ocPlot_mult <- createJaspPlot(title = "OC curve for multiple sampling plan",  width = 320, height = 320)
    ocPlot_mult$dependOn(c("showOCCurve_mult", "lotSize_mult", "sampleSize_mult", "acceptNumber_mult", "rejectNumber_mult", "distribution_mult"))
    jaspResults[["ocPlot_mult"]] <- ocPlot_mult
    plot_mult <- ggplot2::ggplot(data = df_x_mult, ggplot2::aes(x = PD, y = PA)) + 
                        ggplot2::geom_point(colour = "black", shape = 24) + ggplot2::labs(x = "Proportion defective", y = "P(accept)")
    ocPlot_mult$plotObject <- plot_mult
  }
}

.assessPlanSingle <- function(jaspResults, options, names) {
  if (options$pd_prp && options$pa_prp && options$pd_crp && options$pa_crp) {
    x <- NULL
    dist = options$distribution

    # Create sampling plan with the specified values
    if (dist == "hypergeom") {
      # Need to provide the lot size (N) only for hypergeometric distribution.
      x <- AcceptanceSampling::OC2c(N = options$lotSize, n = options$sampleSize, c = options$acceptNumber, r = options$rejectNumber, type = dist)
    } else {
      # Binomial and Poisson distributions don't require lot size (N).
      x <- AcceptanceSampling::OC2c(n = options$sampleSize, c = options$acceptNumber, r = options$rejectNumber, type = dist)
    }
    df_x = data.frame(PD = x@pd, PA = x@paccept)
    
    # Assessment of the sampling plan
    assess <- capture.output(AcceptanceSampling::assess(x, PRP = c(options$pd_prp, options$pa_prp), CRP = c(options$pd_crp, options$pa_crp)))

    # Create and fill the output table(s)
    # 1. Sampling plan table
    table1 <- createJaspTable(title = paste0("Acceptance Sampling Plan (", as.character(dist), ")"))
    table1$dependOn(c(names))
    table1$addColumnInfo(name = "table_1_col_1", title = "", type = "string")
    table1$addColumnInfo(name = "table_1_col_2", title = "Value", type = "integer")
    table1$addRows(list("table_1_col_1" = "Sample size(s)", "table_1_col_2" = as.numeric(options$sampleSize)))
    table1$addRows(list("table_1_col_1" = "Acc. Number(s)", "table_1_col_2" = as.numeric(options$acceptNumber)))
    table1$addRows(list("table_1_col_1" = "Rej. Number(s)", "table_1_col_2" = as.numeric(options$rejectNumber)))
    table1$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table1"]] <- table1

    # 2. Table with the specified and actual acceptance probabilities
    table2 <- createJaspTable(title = as.character(assess[8]))
    table2$dependOn(c(names))
    table2$addColumnInfo(name = "table_2_col_1", title = "", type = "string")
    table2$addColumnInfo(name = "table_2_col_2", title = "Quality", type = "number")
    table2$addColumnInfo(name = "table_2_col_3", title = "RP P(accept)", type = "number")
    table2$addColumnInfo(name = "table_2_col_4", title = "Plan P(accept)", type = "number")
    table2$addRows(list("table_2_col_1" = "PRP", "table_2_col_2" = as.numeric(options$pd_prp), "table_2_col_3" = as.numeric(options$pa_prp), "table_2_col_4" = as.numeric(unlist(strsplit(assess[11], " +"))[4])))
    table2$addRows(list("table_2_col_1" = "CRP", "table_2_col_2" = as.numeric(options$pd_crp), "table_2_col_3" = as.numeric(options$pa_crp), "table_2_col_4" = as.numeric(unlist(strsplit(assess[12], " +"))[4])))
    table2$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table2"]] <- table2

    if (options$showSummarySingle) {
      # df_x = data.frame(PD = x@pd, PA = x@paccept)
      .printSummaryAssess(jaspResults, names, df_x, FALSE)
    }
  }
}

.assessPlanMult <- function(jaspResults, options, names) {
  if (options$pd_prp_mult && options$pa_prp_mult && options$pd_crp_mult && options$pa_crp_mult) {
    x_mult <- NULL
    dist_mult = options$distribution_mult

    # Create sampling plan with the specified values
    if (dist_mult == "hypergeom") {
      # Need to provide the lot size (N) only for hypergeometric distribution.
      x_mult <- AcceptanceSampling::OC2c(N = options$lotSize_mult, n = options$sampleSize_mult, c = options$acceptNumber_mult, r = options$rejectNumber_mult, type = dist_mult)
    } else {
      # Binomial and Poisson distributions don't require lot size (N).
      x_mult <- AcceptanceSampling::OC2c(n = options$sampleSize_mult, c = options$acceptNumber_mult, r = options$rejectNumber_mult, type = dist_mult)
    }
    df_x_mult = data.frame(PD = x_mult@pd, PA = x_mult@paccept)
    
    # Assessment of the sampling plan
    assess_mult <- capture.output(AcceptanceSampling::assess(x_mult, PRP = c(options$pd_prp_mult, options$pa_prp_mult), CRP = c(options$pd_crp_mult, options$pa_crp_mult)))

    rows <- length(options$sampleSize_mult)
    table_df_mult <- data.frame(sample = 1:rows, sample_size = options$sampleSize_mult, cum_sample_size = cumsum(options$sampleSize_mult),
                                acc_num = options$acceptNumber_mult, rej_num = options$rejectNumber_mult)
    # Create and fill the output table(s)
    # 1. Sampling plan table
    table1 <- createJaspTable(title = paste0("Acceptance Sampling Plan (", as.character(dist_mult), ")"))
    table1$dependOn(c(names))
    table1$addColumnInfo(name = "table_1_col_1", title = "Sample", type = "integer")
    table1$addColumnInfo(name = "table_1_col_2", title = "Sample Size", type = "integer")
    table1$addColumnInfo(name = "table_1_col_3", title = "Cum. Sample Size", type = "integer")
    table1$addColumnInfo(name = "table_1_col_4", title = "Acc. Number", type = "integer")
    table1$addColumnInfo(name = "table_1_col_5", title = "Rej. Number", type = "integer")
    table1$setData(list(table_1_col_1 = table_df_mult$sample, table_1_col_2 = table_df_mult$sample_size, table_1_col_3 = table_df_mult$cum_sample_size,
                   table_1_col_4 = table_df_mult$acc_num, table_1_col_5 = table_df_mult$rej_num))
    # table1$addRows(list("table_1_col_1" = "Sample size(s)", "table_1_col_2" = as.numeric(options$sampleSize_mult)))
    # table1$addRows(list("table_1_col_1" = "Acc. Number(s)", "table_1_col_2" = as.numeric(options$acceptNumber_mult)))
    # table1$addRows(list("table_1_col_1" = "Rej. Number(s)", "table_1_col_2" = as.numeric(options$rejectNumber_mult)))
    table1$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table1"]] <- table1

    # 2. Table with the specified and actual acceptance probabilities
    table2 <- createJaspTable(title = as.character(assess_mult[8]))
    table2$dependOn(c(names))
    table2$addColumnInfo(name = "table_2_col_1", title = "", type = "string")
    table2$addColumnInfo(name = "table_2_col_2", title = "Quality", type = "number")
    table2$addColumnInfo(name = "table_2_col_3", title = "RP P(accept)", type = "number")
    table2$addColumnInfo(name = "table_2_col_4", title = "Plan P(accept)", type = "number")
    table2$addRows(list("table_2_col_1" = "PRP", "table_2_col_2" = as.numeric(options$pd_prp_mult), "table_2_col_3" = as.numeric(options$pa_prp_mult), 
                        "table_2_col_4" = as.numeric(unlist(strsplit(assess_mult[11], " +"))[4])))
    table2$addRows(list("table_2_col_1" = "CRP", "table_2_col_2" = as.numeric(options$pd_crp_mult), "table_2_col_3" = as.numeric(options$pa_crp_mult), 
                        "table_2_col_4" = as.numeric(unlist(strsplit(assess_mult[12], " +"))[4])))
    table2$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table2"]] <- table2

    if (options$showSummaryMult) {
      # df_x = data.frame(PD = x@pd, PA = x@paccept)
      .printSummaryAssess(jaspResults, names, df_x_mult, TRUE)
    }
  }
}

# Sampling plan summary table
.printSummaryAssess <- function(jaspResults, names, df_x, is_mult) {
    table <- createJaspTable(title = "Detailed acceptance probabilities:")
    if (is_mult) {
      names <- c(names, "showSummaryMult")
    } else {
      names <- c(names, "showSummarySingle")
    }
    table$dependOn(c(names))
    table$addColumnInfo(name = "col_1", title = "Prop. defective", type = "number")
    table$addColumnInfo(name = "col_2", title = " P(accept)", type = "number")
    jaspResults["debug"] <- createJaspHtml(text=as.character(df_x))
    # table$setData(list(col_1 = df_x$PD, col_2 = df_x$PA))
    table$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table"]] <- table
}