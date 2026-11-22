test_that("sommer provides its complete UI without loading the ASReml module", {
  standalone <- new.env(parent = asNamespace("shiny"))
  standalone$radioTooltip <- radioTooltip
  sys.source(testthat::test_path("..", "..", "R", "mod_mtaSommerApp.R"), standalone)
  expect_false(exists("mod_mtaASREMLApp_ui", standalone, inherits = TRUE))
  expect_false(exists("mod_mtaASREMLApp_server", standalone, inherits = TRUE))
  sommer <- as.character(standalone$mod_mtaSommerApp_ui("sommer-test"))
  expect_match(sommer, "sommer-test-runMtaSommer", fixed = TRUE)
  expect_match(sommer, "sommer-test-mtaWizardStart", fixed = TRUE)
  expect_match(sommer, "sommer-test-tppIdMenuSommer", fixed = TRUE)
  expect_match(sommer, "Engine status", fixed = TRUE)
  expect_match(sommer, "Factor Analityc", fixed = TRUE)
  expect_match(sommer, "no.*license|does not require")
  expect_false(grepl("ASReml license</", sommer, fixed = TRUE))
})

test_that("sommer initializes without an ASReml license and uses native family labels", {
  standalone <- new.env(parent = asNamespace("shiny"))
  sys.source(testthat::test_path("..", "..", "R", "mod_mtaSommerApp.R"), standalone)
  shiny::testServer(standalone$mod_mtaSommerApp_server, args = list(data = shiny::reactiveVal(NULL)), {
    session$setInputs(trait2MtaSommer = "yield")
    expect_identical(moduleCode, "moaSommer")
    expect_identical(rownames(dtDistTrait()), c("gaussian", "binomial", "Gamma", "poisson"))
    expect_match(output$statusSommer$html, "sommer")
  })
})
