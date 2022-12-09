# Tests for Decide variable lots

# 1. No dataset, LSL
test_that("Decide lots - no dataset, lsl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 24
  options$sampleMean <- 1.5
  options$sampleSD <- 1
  options$kValue <- 1.309
  options$lsl <- TRUE
  options$usl <- FALSE
  
  # 1.1 Historical SD unknown
  options$sd <- FALSE
  options$lower_spec <- 0.1
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(24, 1.5, 1, 0.1, 1.4, 1.309))
  lotDecisionUnknownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 1.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 2.4
  options$lower_spec <- 0.5
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableKnown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(24, 1.5, 1, 2.4, 0.5, 0.417, 1.309))
  lotDecisionKnownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
})

# 2. No dataset, USL
test_that("Decide lots - no dataset, usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 125
  options$sampleMean <- 172.3
  options$sampleSD <- 23.7
  options$kValue <- 2.5
  options$lsl <- FALSE
  options$usl <- TRUE
  
  # 2.1 Historical SD unknown
  options$sd <- FALSE
  # 2.1.1 Reject
  options$upper_spec <- 230
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 230, 2.435, 2.5))
  lotDecisionUnknownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 2.1.2 Accept
  options$upper_spec <- 232
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 232, 2.519, 2.5))
  lotDecisionUnknownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 2.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 19.5
  # 2.2.1 Reject
  options$upper_spec <- 221
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableKnown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(125, 172.3, 23.7, 19.5, 221, 2.497, 2.5))
  lotDecisionKnownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))

  # 2.2.2 Accept
  options$upper_spec <- 222
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 19.5, 222, 2.549, 2.5))
  lotDecisionKnownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

# 3. No dataset, both LSL and USL
test_that("Decide lots - no dataset, lsl & usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 785
  options$sampleMean <- 35.2
  options$sampleSD <- 8.92
  options$kValue <- 2
  options$lsl <- TRUE
  options$usl <- TRUE
  options$upper_spec <- 60
  
  # 3.1 Historical SD unknown
  options$sd <- FALSE
  
  # 3.1.1 Reject
  options$lower_spec <- 18
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 18, 60, 1.928, 2.78, 2))
  lotDecisionUnknownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 3.1.2 Accept
  options$lower_spec <- 17
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 17, 60, 2.04, 2.78, 2))
  lotDecisionUnknownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
  

  # 3.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 10.12
  options$pd_prp <- 0.05
  options$pd_crp <- 0.15
  options$lower_spec <- 10
  
  # 3.2.1 Reject
  options$upper_spec <- 55
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableKnown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(785, 35.2, 8.92, 10.12, 10, 55, 2.49, 1.957, 2))
  lotDecisionKnownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 3.2.2 Accept
  options$upper_spec <- 56
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 10.12, 10, 56, 2.49, 2.055, 2))
  lotDecisionKnownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

# Todo: Add tests for decide lots for dataset.
###########################################################################################################
if (0) {
# 1. No dataset, LSL
test_that("Decide lots - no dataset, lsl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 24
  options$sampleMean <- 1.5
  options$sampleSD <- 1
  options$kValue <- 1.309
  options$lsl <- TRUE
  options$usl <- FALSE
  
  # 1.1 Historical SD unknown
  options$sd <- FALSE
  options$lower_spec <- 0.1
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(24, 1.5, 1, 0.1, 1.4, 1.309))
  lotDecisionUnknownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 1.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 2.4
  options$lower_spec <- 0.5
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableKnown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(24, 1.5, 1, 2.4, 0.5, 0.417, 1.309))
  lotDecisionKnownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
})

# 2. No dataset, USL
test_that("Decide lots - no dataset, usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 125
  options$sampleMean <- 172.3
  options$sampleSD <- 23.7
  options$kValue <- 2.5
  options$lsl <- FALSE
  options$usl <- TRUE
  
  # 2.1 Historical SD unknown
  options$sd <- FALSE
  # 2.1.1 Reject
  options$upper_spec <- 230
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 230, 2.435, 2.5))
  lotDecisionUnknownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 2.1.2 Accept
  options$upper_spec <- 232
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 232, 2.519, 2.5))
  lotDecisionUnknownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))

  # 2.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 19.5
  # 2.2.1 Reject
  options$upper_spec <- 221
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableKnown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(125, 172.3, 23.7, 19.5, 221, 2.497, 2.5))
  lotDecisionKnownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))

  # 2.2.2 Accept
  options$upper_spec <- 222
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(125, 172.3, 23.7, 19.5, 222, 2.549, 2.5))
  lotDecisionKnownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})

# 3. No dataset, both LSL and USL
test_that("Decide lots - no dataset, lsl & usl", {
  options <- jaspTools::analysisOptions("DecideVariableLots")
  options$sampleStats <- TRUE
  options$sampleSize <- 785
  options$sampleMean <- 35.2
  options$sampleSD <- 8.92
  options$kValue <- 2
  options$lsl <- TRUE
  options$usl <- TRUE
  options$upper_spec <- 60
  
  # 3.1 Historical SD unknown
  options$sd <- FALSE
  
  # 3.1.1 Reject
  options$lower_spec <- 18
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 18, 60, 1.928, 2.78, 2))
  lotDecisionUnknownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 3.1.2 Accept
  options$lower_spec <- 17
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 17, 60, 2.04, 2.78, 2))
  lotDecisionUnknownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionUnknownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
  

  # 3.2 Historical SD known
  options$sd <- TRUE
  options$stdev <- 10.12
  options$pd_prp <- 0.05
  options$pd_crp <- 0.15
  options$lower_spec <- 10
  
  # 3.2.1 Reject
  options$upper_spec <- 55
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableKnown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableKnown, list(785, 35.2, 8.92, 10.12, 10, 55, 2.49, 1.957, 2))
  lotDecisionKnownReject <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownReject, equals("<u>Decision:</u> <b>Reject</b> lot."))
  
  # 3.2.2 Accept
  options$upper_spec <- 56
  results <- jaspTools::runAnalysis("DecideVariableLots", "test.csv", options, makeTests = TRUE)
  lotTableUnknown <- results[["results"]][["decision_table"]][["data"]]
  jaspTools::expect_equal_tables(lotTableUnknown, list(785, 35.2, 8.92, 10.12, 10, 56, 2.49, 2.055, 2))
  lotDecisionKnownAccept <- results[["results"]][["decision_output"]][["rawtext"]]
  expect_that(lotDecisionKnownAccept, equals("<u>Decision:</u> <b>Accept</b> lot."))
})
}
