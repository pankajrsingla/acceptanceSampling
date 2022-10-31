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

DecideVariableLots <- function(jaspResults, dataset = NULL, options, ...) {
  n <- NULL
  mean_sample <- NULL
  sd_sample <- NULL
  sd_compare <- NULL
  k <- options$kValue
  variable_name <- NULL

  all_variables <- unlist(options$allVariablesList)
  variables <- unlist(options$variables)
  if (options$sampleStats) {
    # Take sample size, sample mean and sample SD directly from sample statistics option values.
    n <- options$sampleSize
    mean_sample <- options$sampleMean
    sd_sample <- options$sampleSD
  } else {
    if (length(variables) == 1) {
      # Dataset is available. Read it.
      if (is.null(dataset)) {
        dataset <- .readDataSetToEnd(columns.as.numeric=variables)
        # Proceed with calculations for the data sample.
        data <- na.omit(dataset[[.v(variables)]])
        n <- length(data)
        mean_sample <- mean(data)
        sd_sample <- sd(data)
        variable_name <- variables[1]
      }
    }
  }

  if (n > 0 & !is.null(mean_sample) & sd_sample > 0) {
    # Initialize the decision table.
    depend_variables <- c("variables", "sampleStats", "sampleSize", "sampleMean", "sampleSD", "kValue", "lsl", "lower_spec", "usl", "upper_spec", "sd", "stdev")
    risk_variables <- c("pd_prp", "pd_crp")
    decision_table <- createJaspTable(title = gettextf("Accept or Reject Lot %s", ifelse(!is.null(variable_name), paste0("(", variable_name, ")"), "")))
    decision_table$dependOn(c(depend_variables, risk_variables))
    jaspResults[["decision_table"]] <- decision_table
    
    sd_compare <- sd_sample
    
    sd <- "unknown"
    sd_historical <- 0
    if (options$sd) {
      sd <- "known"
      sd_historical <- options$stdev
      sd_compare <- sd_historical
    }

    z.lsl <- NULL
    z.usl <- NULL
    decision <- NULL

    if (!options$lsl & !options$usl) {
      # decision_table$setError(gettextf("Error - either LSL or USL needs to be specified for the lot to be accepted/rejected."))
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
      # Both LSL and USL specified. Decide based on SD.
      if (options$sd) {
        # Historical sd known
        z.p <- (options$lower_spec - options$upper_spec) / (2 * sd_historical)
        p <- pnorm(z.p)
        if (2*p >= options$pd_crp) {
          decision <- FALSE
        } else if (2*p <= options$pd_prp) {
          decision <- (z.lsl >= k) & (z.usl >= k)
        } else {
          if (n <= 1) {
            decision_table$setError(gettextf("Error: can not accept or reject lot: sample size has to be greater than 1."))
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
          decision_table$setError(gettextf("Error: Sample size has to be > 1 if both LSL and USL are provided, and historical standard deviation is unknown."))
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

    decision_table$addColumnInfo(name = "col_1", title = "", type = "string")
    decision_table$addColumnInfo(name = "col_2", title = "", type = "number")
    decision_table$addRows(list("col_1" = "Sample Size", "col_2" = as.integer(n)))
    decision_table$addRows(list("col_1" = "Sample Mean", "col_2" = round(mean_sample,3)))
    decision_table$addRows(list("col_1" = ifelse(options$sd, "Historical Standard Deviation", "Sample Standard Deviation"), "col_2" = round(sd_compare,3)))
    if (options$lsl) {
      decision_table$addRows(list("col_1" = "Lower Specification Limit (LSL)", "col_2" = round(options$lower_spec,3)))
    }
    if (options$usl) {
      decision_table$addRows(list("col_1" = "Upper Specification Limit (USL)", "col_2" = round(options$upper_spec,3)))
    }
    if (options$lsl) {
      decision_table$addRows(list("col_1" = "Z.LSL", "col_2" = round(z.lsl,3)))
    }
    if (options$usl) {
      decision_table$addRows(list("col_1" = "Z.USL", "col_2" = round(z.usl,3)))
    }
    decision_table$addRows(list("col_1" = "Critical Distance (k)", "col_2" = round(k,3)))
    decision_table$showSpecifiedColumnsOnly <- TRUE
    decision_table$position <- 1
    
    if (!is.null(decision)) {
      decision_output <- createJaspHtml(text = gettextf("Decision: %s lot.", ifelse(decision == TRUE, "Accept", "Reject")), 
                                        dependencies = c(depend_variables, risk_variables), position = 2)
      jaspResults[["decision_output"]] <- decision_output
    }
  }
}