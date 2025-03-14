# penguins_examples()
test_that("penguins_examples with NULL lists files", {
  expect_true("analysis/penguins.qmd" %in% penguins_examples())
  expect_true("penguins_graph.R" %in% penguins_examples())
})

test_that("penguins_examples with file returns correct path", {
  expect_true(file.exists(penguins_examples("penguins_graph.R")))
  expect_true(file.exists(penguins_examples("analysis/penguins.qmd")))
  expect_error(file.exists(penguins_examples("not_a_file.R")))
})
