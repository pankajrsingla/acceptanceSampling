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

OCCurves <- function(jaspResults, options, ...) {
  .ocCheckErrors(options)
  variable_names <- c("lotSize", "sampleSize", "acceptNumber", "rejectNumber", "distribution")
  
  # First plan
  plot1 <- NULL
  if ((options$sampleSize1 > 0) && (options$acceptNumber1 > 0) && (options$rejectNumber1 > 0)) {
    df_plan1 <- getPlanDf(options$sampleSize1, options$acceptNumber1, options$rejectNumber1, options$distribution1, options$lotSize1)
    plot1 <- getPlot(jaspResults, df_plan1, paste0(variable_names, 1), "first", 1)
    if (options$showSummary1) {
      printSummary(jaspResults, df_plan1, c(paste0(variable_names, 1), "showSummary1"))
    }
  }

  # Second plan
  plot2 <- NULL
  if ((options$sampleSize2 > 0) && (options$acceptNumber2 > 0) && (options$rejectNumber2 > 0)) {
    df_plan2 <- getPlanDf(options$sampleSize2, options$acceptNumber2, options$rejectNumber2, options$distribution2, options$lotSize2)
    plot2 <- getPlot(jaspResults, df_plan2, paste0(variable_names, 2), "second", 2)
    if (options$showSummary2) {
      printSummary(jaspResults, df_plan2, c(paste0(variable_names, 2), "showSummary2"))
    }
    p2 <- plot1$plotObject + plot2$plotObject
    plot2$plotObject <- p2
  }

  # Third plan
  plot3 <- NULL
  if ((options$sampleSize3 > 0) && (options$acceptNumber3 > 0) && (options$rejectNumber3 > 0)) {
    df_plan3 <- getPlanDf(options$sampleSize3, options$acceptNumber3, options$rejectNumber3, options$distribution3, options$lotSize3)
    plot3 <- getPlot(jaspResults, df_plan3, paste0(variable_names, 3), "third", 3)
    if (options$showSummary3) {
      printSummary(jaspResults, df_plan3, c(paste0(variable_names, 3), "showSummary3"))
    }
    p3 <- plot2$plotObject + plot3$plotObject
    plot3$plotObject <- p3
  }

  # Multiple sampling plan
  if (length(options$sampleSizeMult) > 0 && length(options$acceptNumberMult) > 0 && length(options$rejectNumberMult > 0)) {
    df_planMult <- getPlanDf(options$sampleSizeMult, options$acceptNumberMult, options$rejectNumberMult, options$distributionMult, options$lotSizeMult)
    plotMult <- getPlot(jaspResults, df_planMult, paste0(variable_names, "Mult"), "multiple")
    if (options$showSummaryMult) {
      printSummary(jaspResults, df_planMult, c(paste0(variable_names, "Mult"), "showSummaryMult"))
    }
  }
}

.ocCheckErrors <- function(options) {
  # Error Check

  checkPlan <- function(options) {
    variables <- c("lotSize", "sampleSize", "acceptNumber", "rejectNumber")
    for (index in list(1,2,3,"Mult")) {
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

  .hasErrors(
    dataset              = NULL,
    custom               = checkPlan,
    exitAnalysisIfErrors = FALSE
  )
}