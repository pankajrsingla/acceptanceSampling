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

OCCurves <- function(jaspResults, dataset = NULL, options, ...) {
  # ready <- length(options$variables) > 0

  # lotSize, sampleSize, ... are formulaFields: parse them and save the result in the state
  optionVals1 <- c("lotSize1", "sampleSize1", "acceptNumber1", "rejectNumber1", "showSummary1")
  optionVals2 <- c("lotSize2", "sampleSize2", "acceptNumber2", "rejectNumber2", "showSummary2")
  optionVals3 <- c("lotSize3", "sampleSize3", "acceptNumber3", "rejectNumber3", "showSummary3")
  # optionValsM <- c("lotSize_mult", "sampleSize_mult", "acceptNumber_mult", "rejectNumber_mult", "showSummary_mult")
  options <- .parseAndStoreFormulaOptions(jaspResults, options, names = c(optionVals1, optionVals2, optionVals3))

  .ocCheckErrors(dataset, options)
  .generateOCCurvesSingle(jaspResults, options)
  .generateOCCurvesMultiple(jaspResults, options)
  # .binomTableMain(       jaspResults, dataset, options, 1)
  # .binomPlotsDescriptive(jaspResults, dataset, options, 0)
}


.ocCheckErrors <- function(dataset = NULL, options) {
  # perform a check on the hypothesis

  # Need to extend to all three curves, currently check only for the first.
  ocSanityCheck <- function(lotSize, sampleSize, acceptNumber, rejectNumber) {
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
    custom               = ocSanityCheck,
    type                 = "factorLevels",
    factorLevels.target  = options$sampleSize1,
    factorLevels.amount  = "< 1",
    exitAnalysisIfErrors = TRUE
  )
}

# Results functions ----

.generateOCCurvesSingle <- function(jaspResults, options) {
  # OC Plot 1:
  ######################
  plot1 <- NULL
  if ((options$sampleSize1 > 0) && (options$acceptNumber1 > 0) && (options$rejectNumber1 > 0)) {
    # Create sampling plan
    x1 <- NULL
    if (options$distribution1 == "hypergeom") {
      x1 <- AcceptanceSampling::OC2c(N = options$lotSize1, n = options$sampleSize1, c = options$acceptNumber1, r = options$rejectNumber1, type = options$distribution1)
    } else {
      x1 <- AcceptanceSampling::OC2c(n = options$sampleSize1, c = options$acceptNumber1, r = options$rejectNumber1, type = options$distribution1)
    }
    df_x1 <- data.frame(PD = x1@pd, PA = x1@paccept)
    # Generate plot
    ocPlot1 <- createJaspPlot(title = "OC curve for first sampling plan",  width = 320, height = 320)
    ocPlot1$dependOn(c("lotSize1", "sampleSize1", "acceptNumber1", "rejectNumber1", "distribution1"))
    jaspResults[["ocPlot1"]] <- ocPlot1
    plot1 <- ggplot2::ggplot(data = df_x1, ggplot2::aes(x = PD, y = PA)) + 
                        ggplot2::geom_point(colour = "black", shape = 24) + ggplot2::labs(x = "Proportion defective", y = "P(accept)")
    ocPlot1$plotObject <- plot1
    
    # Sampling plan summary, if requested
    if (options$showSummary1) {
      .printSummary(jaspResults, df_x1, index = 1)
    }
  }

  # OC Plot 2:
  ######################
  plot2 <- NULL
  if ((options$sampleSize2 > 0) && (options$acceptNumber2 > 0) && (options$rejectNumber2 > 0)) {
    # Create sampling plan  
    x2 <- NULL
    if (options$distribution2 == "hypergeom") {
      x2 <- AcceptanceSampling::OC2c(N = options$lotSize2, n = options$sampleSize2, c = options$acceptNumber2, r = options$rejectNumber2, type = options$distribution2)
    } else {
      x2 <- AcceptanceSampling::OC2c(n = options$sampleSize2, c = options$acceptNumber2, r = options$rejectNumber2, type = options$distribution2)
    }
    df_x2 <- data.frame(PD = x2@pd, PA = x2@paccept)
    # Generate plot
    ocPlot2 <- createJaspPlot(title = "OC curve for first and second sampling plans",  width = 320, height = 320)
    ocPlot2$dependOn(c("lotSize2", "sampleSize2", "acceptNumber2", "rejectNumber2", "distribution2"))
    jaspResults[["ocPlot2"]] <- ocPlot2
    plot2 <- plot1 + ggplot2::geom_point(data = df_x2, ggplot2::aes(x = PD, y = PA), colour = "green", shape = 25)
    ocPlot2$plotObject <- plot2    
    
    # Sampling plan summary, if requested
    if (options$showSummary2) {
      .printSummary(jaspResults, df_x2, index = 2)
    }           
  }

  # OC Plot 3:
  ######################
  plot3 <- NULL
  if ((options$sampleSize3 > 0) && (options$acceptNumber3 > 0) && (options$rejectNumber3 > 0)) {
    # Create sampling plan  
    x3 <- NULL
    if (options$distribution3 == "hypergeom") {
      x3 <- AcceptanceSampling::OC2c(N = options$lotSize3, n = options$sampleSize3, c = options$acceptNumber3, r = options$rejectNumber3, type = options$distribution3)
    } else {
      x3 <- AcceptanceSampling::OC2c(n = options$sampleSize3, c = options$acceptNumber3, r = options$rejectNumber3, type = options$distribution3)
    }
    df_x3 <- data.frame(PD = x3@pd, PA = x3@paccept)
    # Generate plot
    ocPlot3 <- createJaspPlot(title = "OC curve for all three sampling plans",  width = 320, height = 320)
    ocPlot3$dependOn(c("lotSize3", "sampleSize3", "acceptNumber3", "rejectNumber3", "distribution3"))
    jaspResults[["ocPlot3"]] <- ocPlot3
    plot3 <- plot2 + ggplot2::geom_point(data = df_x3, ggplot2::aes(x = PD, y = PA), colour = "blue", shape = 0)
    ocPlot3$plotObject <- plot3
    
    # Sampling plan summary, if requested
    if (options$showSummary3) {
      .printSummary(jaspResults, df_x3, index = 3)
    }                        
  }
}

.generateOCCurvesMultiple <- function(jaspResults, options) {
  if (length(options$sampleSize_mult) > 0 && length(options$acceptNumber_mult) > 0 && length(options$rejectNumber_mult > 0)) {
    # Create sampling plan
    x_mult <- NULL
    if (options$distribution_mult == "hypergeom") {
      x_mult <- AcceptanceSampling::OC2c(N = options$lotSize_mult, n = options$sampleSize_mult, c = options$acceptNumber_mult, r = options$rejectNumber_mult, type = options$distribution_mult)
    } else {
      x_mult <- AcceptanceSampling::OC2c(n = options$sampleSize_mult, c = options$acceptNumber_mult, r = options$rejectNumber_mult, type = options$distribution_mult)
    }
    df_x_mult <- data.frame(PD = x_mult@pd, PA = x_mult@paccept)
    # Generate plot
    ocPlot_mult <- createJaspPlot(title = "OC curve for multiple sampling plan",  width = 320, height = 320)
    ocPlot_mult$dependOn(c("lotSize_mult", "sampleSize_mult", "acceptNumber_mult", "rejectNumber_mult", "distribution_mult"))
    jaspResults[["ocPlot_mult"]] <- ocPlot_mult
    plot_mult <- ggplot2::ggplot(data = df_x_mult, ggplot2::aes(x = PD, y = PA)) + 
                        ggplot2::geom_point(colour = "black", shape = 24) + ggplot2::labs(x = "Proportion defective", y = "P(accept)")
    ocPlot_mult$plotObject <- plot_mult
    
    # Sampling plan summary, if requested
    if (options$showSummary_mult) {
      .printSummary(jaspResults, df_x_mult, index = "mult")
    }
  }
}

# Sampling plan summary table
.printSummary <- function(jaspResults, df_x, index) {
    table <- createJaspTable(title = "Detailed acceptance probabilities:")
    names <- c("lotSize", "sampleSize", "acceptNumber", "rejectNumber", "distribution", "showSummary_mult")
    names <- paste0(names, index)
    table$dependOn(c(names))
    table$addColumnInfo(name = "col_1", title = "Prop. defective", type = "number")
    table$addColumnInfo(name = "col_2", title = " P(accept)", type = "number")
    table$setData(list(col_1 = df_x$PD, col_2 = df_x$PA))
    table$showSpecifiedColumnsOnly <- TRUE
    jaspResults[[paste0("table", index)]] <- table
}