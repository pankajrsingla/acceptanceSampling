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
  variables <- unlist(options$variables)
  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric=variables)
  }
  
  # Lot acceptance and rejection is valid for only a single variable.
  if (length(variables) != 1) {
    # Error
  } else {
    data <- na.omit(dataset[[.v(variables)]])
    n <- length(data)
    mean_sample <- mean(data)
    sd_sample <- sd(data)
    sd_compare <- sd_sample

    depend_variables <- c("variables", "sampleSize", "kValue", "lsl", "lower_spec", "usl", "upper_spec", "sd", "stdev")
    risk_variables <- c("pd_prp", "pa_prp", "pd_crp", "pa_crp")
  
    # n <- options$sampleSize
    k <- options$kValue
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
      # Error
      decision <- NULL
    }
    if (options$lsl) {
      z.lsl <- ((mean_sample - options$lower_spec) / sd_compare)
      if (!options$usl) {
        decision <- z.lsl >= k
      }
    }
    if (options$usl) {
      z.usl <- ((options$upper_spec - mean_sample) / sd_compare)
      if (!options$lsl) {
        decision <- z.usl >= k
      }
    }
    if (options$lsl & options$usl) {
      if (options$sd) {
        # Historical sd known
        z_p <- options$lower_spec - options$upper_spec
        p <- pnorm(z_p)
        if (2*p >= options$pd_crp) {
          decision <- FALSE
        } else if (2*p <= options$pd_prp) {
          decision <- (z.lsl > k) & (z.usl > k)
        } else {
          decision <- NULL
          # decision <- getDecisionMMethod()
        }
      } else {
        decision <- NULL
        # decision <- getDecisionMMethod()
      }
    }

    decision_table <- createJaspTable(title = paste0("Accept or Reject Lot Using ", variables[1]))
    decision_table$dependOn(c(depend_variables, risk_variables))
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
    jaspResults[["decision_table"]] <- decision_table

    if (!is.null(decision)) {
      decision_output <- createJaspHtml()
      decision_output$dependOn(c(depend_variables, risk_variables))
      decision_output$position <- 2
      decision_output[["text"]] <- gettextf("Decision: %s lot.", ifelse(decision == TRUE, "Accept", "Reject"))
    } else {
      null_output <- createJaspHtml()
      null_output$dependOn(c(depend_variables, risk_variables))
      null_output$position <- 1
      null_output[["text"]] <- gettextf("Decision: Undecided")
    }
  }
}
