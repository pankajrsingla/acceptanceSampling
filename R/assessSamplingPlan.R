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
  options <- .parseAndStoreFormulaOptions(jaspResults, options, names = optionNames)

  .assessSampleCheckErrors(dataset, options)
  if (options$showOCCurve) {
    .createOCCurveAssess(jaspResults, options)
  }
  .assessPlan(jaspResults, options, optionNames)
}


.assessSampleCheckErrors <- function(dataset = NULL, options) {
  # perform a check on the hypothesis

  sanityCheckAssess <- function() {
    if (options$sampleSize > options$lotSize)
      return(gettext("Sample size cannot be greater than the lot size."))
    else if (options$acceptNumber > options$sampleSize)
      return(gettext("Acceptance number cannot be greater than the sample size."))
    else if (options$rejectNumber > options$sampleSize)
      return(gettext("Rejection number cannot be greater than the sample size."))
    else if (options$rejectNumber < options$acceptNumber)
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
.createOCCurveAssess <- function(jaspResults, options) {
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
    ocPlot <- createJaspPlot(title = "OC curve for the sampling plan",  width = 320, height = 320)
    ocPlot$dependOn(c("showOCCurve", "lotSize", "sampleSize", "acceptNumber", "rejectNumber", "distribution"))
    jaspResults[["ocPlot"]] <- ocPlot
    ggplot <- ggplot2::ggplot(data = df_x, ggplot2::aes(x = PD, y = PA)) + 
                        ggplot2::geom_point(colour = "black") + ggplot2::labs(x = "Proportion defective", y = "P(accept)")
    ocPlot$plotObject <- ggplot
  }
}

.assessPlan <- function(jaspResults, options, names) {
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
    
    # Assessment of the sampling plan
    assess <- capture.output(AcceptanceSampling::assess(x, PRP = c(options$pd_prp, options$pa_prp), CRP = c(options$pd_crp, options$pa_crp)))

    # Create and fill the output table(s)
    # 1. Sampling plan table
    table1 <- createJaspTable(title = paste0("Acceptance Sampling Plan (", as.character(dist), ")"))
    table1$dependOn(c(names))
    table1$addColumnInfo(name = "table_1_col_1", title = "", type = "string")
    table1$addColumnInfo(name = "table_1_col_2", title = "Value", type = "number")
    table1$addRows(list("table_1_col_1" = "Sample size(s)", "table_1_col_2" = as.numeric(options$sampleSize)))
    table1$addRows(list("table_1_col_1" = "Acc. Number(s)", "table_1_col_2" = as.numeric(options$acceptNumber)))
    table1$addRows(list("table_1_col_1" = "Rej. Number(s)", "table_1_col_2" = as.numeric(options$rejectNumber)))
    table1$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table1"]] <- table1

    # 2. Table with the specified and actual acceptance probabilities
    table3 <- createJaspTable(title = as.character(assess[8]))
    table3$dependOn(c(names))
    table3$addColumnInfo(name = "table_3_col_1", title = "", type = "string")
    table3$addColumnInfo(name = "table_3_col_2", title = "Quality", type = "number")
    table3$addColumnInfo(name = "table_3_col_3", title = "RP P(accept)", type = "number")
    table3$addColumnInfo(name = "table_3_col_4", title = "Plan P(accept)", type = "number")
    table3$addRows(list("table_3_col_1" = "PRP", "table_3_col_2" = as.numeric(options$pd_prp), "table_3_col_3" = as.numeric(options$pa_prp), "table_3_col_4" = as.numeric(unlist(strsplit(assess[11], " +"))[4])))
    table3$addRows(list("table_3_col_1" = "CRP", "table_3_col_2" = as.numeric(options$pd_crp), "table_3_col_3" = as.numeric(options$pa_crp), "table_3_col_4" = as.numeric(unlist(strsplit(assess[12], " +"))[4])))
    table3$showSpecifiedColumnsOnly <- TRUE
    jaspResults[["table3"]] <- table3
  }
}