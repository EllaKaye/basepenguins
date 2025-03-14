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

# filter_by_extensions
test_that("filter_by_extensions returns correct patterns", {
  expect_equal(filter_by_extensions(NULL), "*")
  expect_equal(filter_by_extensions(character(0)), "*")
  expect_equal(filter_by_extensions(""), "*")
  expect_equal(filter_by_extensions("R"), "\\.(R)$")
  expect_equal(filter_by_extensions(c("R", "Rmd")), "\\.(R|Rmd)$")
})
