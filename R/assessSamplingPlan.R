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

AssessSamplingPlan <- function(jaspResults, options, ...) {
  .assessSampleCheckErrors(dataset, options)
  # Single
  if (options$showOCCurveSingle) {
    .createOCCurveAssessSingle(jaspResults, options)
  }
  .assessPlanSingle(jaspResults, options)
  # Multiple
  if (options$showOCCurveMult) {
    .createOCCurveAssessMult(jaspResults, options)
  }
  .assessPlanMult(jaspResults, options)
}


.assessSampleCheckErrors <- function(dataset = NULL, options) {
  # perform a check on the hypothesis

  # Check option values
  checkPlan <- function(options) {
    variables <- c("lotSize", "sampleSize", "acceptNumber", "rejectNumber")
    for (index in list("Single","Mult")) {
      variables_indexed <- paste0(variables, index)
      if (options[[variables_indexed[2]]] > options[[variables_indexed[1]]]) {
        return(gettextf("Sample size cannot be greater than the lot size."))
      } else if (options[[variables_indexed[3]]] > options[[variables_indexed[2]]]) {
        return(gettextf("Acceptance number cannot be greater than the sample size."))
      } else if (options[[variables_indexed[4]]] > options[[variables_indexed[2]]]) {
        return(gettextf("Rejection number cannot be greater than the sample size."))
      } else if (options[[variables_indexed[4]]] < options[[variables_indexed[3]]]) {
        return(gettextf("Rejection number cannot be smaller than the acceptance number."))
      }
    }
  }
  
  # Error Check 1: Number of levels of the variables and the hypothesis
  .hasErrors(
    dataset              = NULL,
    custom               = checkPlan,
    exitAnalysisIfErrors = TRUE
  )
}

# Results functions ----

# Generate OC Curve:
.createOCCurveAssessSingle <- function(jaspResults, options) {
  if ((options$sampleSizeSingle > 0) && (options$acceptNumberSingle > 0) && (options$rejectNumberSingle > 0)) {
    # Create sampling plan
    df_x <- getPlanDf(options$sampleSizeSingle, options$acceptNumberSingle, options$rejectNumberSingle, options$distributionSingle, options$lotSizeSingle)
    # Generate plot
    variable_names <- c("lotSizeSingle", "sampleSizeSingle", "acceptNumberSingle", "rejectNumberSingle", "distributionSingle", "showOCCurveSingle"))
    ocPlot_single <- getPlot(jaspResults, df_x, variable_names, "single")
  }
}

.createOCCurveAssessMult <- function(jaspResults, options) {
  if (length(options$sampleSizeMult) > 0 && length(options$acceptNumberMult) > 0 && length(options$rejectNumberMult) > 0) {
    # Create sampling plan
    df_x <- getPlanDf(options$sampleSizeMult, options$acceptNumberMult, options$rejectNumberMult, options$distributionMult, options$lotSizeMult)
    # Generate plot
    variable_names <- c("lotSizeMult", "sampleSizeMult", "acceptNumberMult", "rejectNumberMult", "distributionMult", "showOCCurveMult"))
    ocPlot_mult <- getPlot(jaspResults, df_x, variable_names, "multiple")
  }
}

.assessPlanSingle <- function(jaspResults, options) {
  if (options$pd_prpSingle && options$pa_prpSingle && options$pd_crpSingle && options$pa_crpSingle) {
    x <- NULL
    dist <- options$distributionSingle
    output <- getPlanDf(options$sampleSizeSingle, options$acceptNumberSingle, options$rejectNumberSingle, dist, options$lotSizeSingle, TRUE)
    x <- output[1]
    df_x <- output[2]
    
    # Assessment of the sampling plan
    assess <- capture.output(AcceptanceSampling::assess(x, PRP = c(options$pd_prpSingle, options$pa_prpSingle), CRP = c(options$pd_crpSingle, options$pa_crpSingle)))

    # Create and fill the output table(s)
    # 1. Sampling plan table
    table1 <- createJaspTable(title = paste0("Acceptance Sampling Plan (", as.character(dist), ")"))
    table1$dependOn(c("sampleSizeSingle", "acceptNumberSingle", "rejectNumberSingle"))
    table1$addColumnInfo(name = "table_1_col_1", title = "", type = "string")
    table1$addColumnInfo(name = "table_1_col_2", title = "Value", type = "integer")
    table1$addRows(list("table_1_col_1" = "Sample size(s)", "table_1_col_2" = as.numeric(options$sampleSizeSingle)))
    table1$addRows(list("table_1_col_1" = "Acc. Number(s)", "table_1_col_2" = as.numeric(options$acceptNumberSingle)))
    table1$addRows(list("table_1_col_1" = "Rej. Number(s)", "table_1_col_2" = as.numeric(options$rejectNumberSingle)))
    table1$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table1"]] <- table1

    # 2. Table with the specified and actual acceptance probabilities
    variable_names <- c("sampleSizeSingle", "acceptNumberSingle", "rejectNumberSingle", "distributionSingle", "pd_prpSingle", "pa_prpSingle", "pd_crpSingle", "pa_crpSingle")
    table2 <- getRiskPointTable(jaspResults, variable_names, assess, options$pd_prpSingle, options$pa_prpSingle, options$pd_crpSingle, options$pa_crpSingle)

    if (options$showSummarySingle) {
      printSummary(jaspResults, df_x, c(variable_names, "showSummarySingle"))
    }
  }
}

.assessPlanMult <- function(jaspResults, options) {
  if (options$pd_prpMult && options$pa_prpMult && options$pd_crpMult && options$pa_crpMult) {
    x_mult <- NULL
    dist_mult = options$distributionMult

    # Get plan
    output <- getPlanDf(options$sampleSizeMult, options$acceptNumberMult, options$rejectNumberMult, dist_mult, options$lotSizeMult)
    x_mult <- output[1]
    df_x_mult <- output[2]
    
    # Assessment of the sampling plan
    assess_mult <- capture.output(AcceptanceSampling::assess(x_mult, PRP = c(options$pd_prpMult, options$pa_prpMult), CRP = c(options$pd_crpMult, options$pa_crpMult)))

    rows <- length(options$sampleSizeMult)
    table_df_mult <- data.frame(sample = 1:rows, sample_size = options$sampleSizeMult, cum_sample_size = cumsum(options$sampleSizeMult),
                                acc_num = options$acceptNumberMult, rej_num = options$rejectNumberMult)
    # Create and fill the output table(s)
    # 1. Sampling plan table
    table1 <- createJaspTable(title = paste0("Acceptance Sampling Plan (", as.character(dist_mult), ")"))
    table1$dependOn(c("sampleSizeMult", "acceptNumberMult", "rejectNumberMult"))
    table1$addColumnInfo(name = "table_1_col_1", title = "Sample", type = "integer")
    table1$addColumnInfo(name = "table_1_col_2", title = "Sample Size", type = "integer")
    table1$addColumnInfo(name = "table_1_col_3", title = "Cum. Sample Size", type = "integer")
    table1$addColumnInfo(name = "table_1_col_4", title = "Acc. Number", type = "integer")
    table1$addColumnInfo(name = "table_1_col_5", title = "Rej. Number", type = "integer")
    table1$setData(list(table_1_col_1 = table_df_mult$sample, table_1_col_2 = table_df_mult$sample_size, table_1_col_3 = table_df_mult$cum_sample_size,
                   table_1_col_4 = table_df_mult$acc_num, table_1_col_5 = table_df_mult$rej_num))
    table1$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table1"]] <- table1

    # 2. Table with the specified and actual acceptance probabilities
    variable_names <- c("sampleSizeMult", "acceptNumberMult", "rejectNumberMult", "distributionMult", "pd_prpMult", "pa_prpMult", "pd_crpMult", "pa_crpMult")
    table2 <- getRiskPointTable(jaspResults, variable_names, assess_mult, options$pd_prpMult, options$pa_prpMult, options$pd_crpMult, options$pa_crpMult)

    if (options$showSummaryMult) {
      printSummary(jaspResults, df_x_mult, c(variable_names, "showSummaryMult"))
    }
  }
}