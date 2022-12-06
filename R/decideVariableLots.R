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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

# txt = "Decision table for variable plan lots."
# banner(txt, centre = TRUE, bandChar = "-")
##----------------------------------------------------------------
##            Decision table for variable plan lots.            --
##----------------------------------------------------------------
#' @param jaspResults <>
#' @param dataset <>
#' @param options <>
#' @seealso
#'   [()] for <>
#' @examples
#' DecideVariableLots(jaspResults, dataset, options)
DecideVariableLots <- function(jaspResults, dataset = NULL, options, ...) {
  n <- NULL
  mean_sample <- NULL
  sd_sample <- NULL
  k <- options$kValue
  var_name <- NULL

  # all_vars <- unlist(options$allVariablesList)
  vars <- unlist(options$variables)
  if (options$sampleStats) {
    # Take sample size, sample mean and sample SD directly from sample statistics option values.
    n <- options$sampleSize
    mean_sample <- options$sampleMean
    sd_sample <- options$sampleSD
  } else {
    if (length(vars) == 1) {
      # Dataset is available. Read it.
      if (is.null(dataset)) {
        dataset <- .readDataSetToEnd(columns.as.numeric=vars)
        # Proceed with calculations for the data sample.
        data <- na.omit(dataset[[.v(vars)]])
        .hasErrors(dataset=dataset, type = c("infinity", "missingValues"), target = vars, exitAnalysisIfErrors = TRUE)
        n <- length(data)
        mean_sample <- mean(data)
        sd_sample <- sd(data)
        var_name <- vars[1]
      }
    }
  }
  # Todo: Need to check this - are negative values of sample mean to be allowed?
  mean_sample <- abs(mean_sample)

  depend_vars <- c("vars", "sampleStats", "sampleSize", "sampleMean", "sampleSD", "kValue", "lsl", "lower_spec", "usl", "upper_spec", "sd", "stdev")
  risk_vars <- c("pd_prp", "pd_crp")
  if (!is.null(jaspResults[["decision_table"]])) {
    return ()
  }
  decision_table <- createJaspTable(title = sprintf("Accept or Reject Lot %s", ifelse(!is.null(var_name), paste0("(", var_name, ")"), "")))
  decision_table$transpose <- TRUE
  decision_table$transposeWithOvertitle <- FALSE
  decision_table$dependOn(c(depend_vars, risk_vars))
  jaspResults[["decision_table"]] <- decision_table
  if (sd_sample <= 0) {
    decision_table$setError(sprintf("Error: Sample standard deviation has to be greater than 0."))
    return ()
  }
  
  if (n > 0 & !is.null(mean_sample) & sd_sample > 0) {
    # Initialize the decision table.
    sd_compare <- sd_sample
    
    sd <- "unknown"
    sd_historical <- 0
    if (options$sd) {
      sd <- "known"
      sd_historical <- options$stdev
      if (sd_sample <= 0) {
        decision_table$setError(sprintf("Error: Sample standard deviation has to be greater than 0."))
        return ()
      }
      sd_compare <- sd_historical
    }

    z.lsl <- NULL
    z.usl <- NULL
    decision <- NULL

    if (!options$lsl & !options$usl) {
      # decision_table$setError(sprintf("Error: Either LSL or USL needs to be specified for the lot to be accepted/rejected."))
      decision <- NULL
    }

    if (options$lsl) {
      z.lsl <- ((mean_sample - options$lower_spec) / sd_compare)
      if (!options$usl) {
        # Only LSL specified. Accept/reject lot based on Z.LSL.
        decision <- z.lsl >= k
      }
    }

    if (options$usl) {
      z.usl <- ((options$upper_spec - mean_sample) / sd_compare)
      if (!options$lsl) {
        # Only USL specified. Accept/reject lot based on Z.USL.
        decision <- z.usl >= k
      }
    }

    if (options$lsl & options$usl) {
      if (options$upper_spec < options$lower_spec) {
        decision_table$setError(sprintf("Error: USL can not be lower than LSL."))
        return ()
      }
      # Both LSL and USL specified. Decide based on SD.
      # Historical sd known
      if (options$sd) {
        # Error handling for AQL/RQL
        if (options$pd_prp >= options$pd_crp) {
          decision_table$setError(sprintf("Error: AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value."))
          return ()
        }
        z.p <- (options$lower_spec - options$upper_spec) / (2 * sd_historical)
        p <- pnorm(z.p)
        if (2*p >= options$pd_crp) {
          decision <- FALSE
        } else if (2*p <= options$pd_prp) {
          decision <- (z.lsl >= k) & (z.usl >= k)
        } else {
          if (n <= 1) {
            decision_table$setError(sprintf("Error: can not accept or reject lot: sample size has to be greater than 1."))
            return ()
          } else {
            q.l <- z.lsl * sqrt(n/(n-1))
            p.l <- pnorm(q.l, lower.tail = FALSE)
            q.u <- z.usl * sqrt(n/(n-1))
            p.u <- pnorm(q.u, lower.tail = FALSE)
            p.combined <- p.l + p.u
            z.m <- k * sqrt(n/(n-1))
            m <- pnorm(z.m, lower.tail = FALSE)
            decision <- p.combined <= m
          }
        }
      } else {
        # Historical sd unknown
        if (n <= 1) {
          decision_table$setError(sprintf("Error: Sample size has to be > 1 if both LSL and USL are provided, and historical standard deviation is unknown."))
          return ()
        } else {
          a <- (n - 2) / 2
          b <- (n - 2) / 2
          x.l <- max(0, 0.5 - (0.5 * z.lsl * sqrt(n)/(n-1)))
          p.l <- pbeta(x.l, a, b)
          x.u <- max(0, 0.5 - (0.5 * z.usl * sqrt(n)/(n-1)))
          p.u <- pbeta(x.u, a, b)
          b.m <- 0.5 * (1 - k * sqrt(n)/(n-1))
          m <- pbeta(b.m, a, b)
          p.combined <- p.l + p.u
          decision <- p.combined <= m
        }
      }
    }

    decision_table$addColumnInfo(name = "col_0", title = "", type = "string") # Dummy row for title. Add title if needed.
    decision_table$addColumnInfo(name = "col_1", title = "Sample Size", type = "integer")
    decision_table$addColumnInfo(name = "col_2", title = "Sample Mean", type = "number")
    decision_table$addColumnInfo(name = "col_3", title = "Sample Standard Deviation", type = "number")
    decision_table$addColumnInfo(name = "col_4", title = ifelse(options$sd, "Historical Standard Deviation", "Sample Standard Deviation"), type = "number")
    if (options$lsl) {
      decision_table$addColumnInfo(name = "col_5", title = "Lower Specification Limit (LSL)", type = "number")
    }
    if (options$usl) {
      decision_table$addColumnInfo(name = "col_6", title = "Upper Specification Limit (USL)", type = "number")
    }
    if (options$lsl) {
      decision_table$addColumnInfo(name = "col_7", title = "Z.LSL", type = "number")
    }
    if (options$usl) {
      decision_table$addColumnInfo(name = "col_8", title = "Z.USL", type = "number")
    }
    decision_table$addColumnInfo(name = "col_9", title = "Critical Distance (k)", type = "number")

    row = list("col_1" = n, "col_2" = round(mean_sample,2), "col_3" = round(sd_sample,2), "col_4" = round(sd_compare,2))
    if (options$lsl) {
      row = append(row, list("col_5" = round(options$lower_spec,2)))
    }
    if (options$usl) {
      row = append(row, list("col_6" = round(options$upper_spec,2)))
    }
    if (options$lsl) {
      row = append(row, list("col_7" = round(z.lsl,2)))
    }
    if (options$usl) {
      row = append(row, list("col_8" = round(z.usl,2)))
    }
    row = append(row, list("col_9" = round(k,2)))

    decision_table$addRows(row)
    decision_table$showSpecifiedColumnsOnly <- TRUE
    decision_table$position <- 1

    if (!is.null(decision)) {
      if (is.null(jaspResults[["decision_output"]])) {
        decision_output <- createJaspHtml(text = sprintf("Decision: %s lot.", ifelse(decision == TRUE, "Accept", "Reject")), 
                                          dependencies = c(depend_vars, risk_vars), position = 2)
        decision_output$position <- 2
        jaspResults[["decision_output"]] <- decision_output
      }
    }
  }
}