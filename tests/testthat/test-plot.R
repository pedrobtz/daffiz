test_that("the difference map builds and renders", {
  skip_if_not_installed("ggplot2")
  m <- mixed_comparison()
  plot <- plot_diff(m$cmp)

  expect_s3_class(plot, "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot))
  expect_match(plot$labels$title, "Difference map")
  expect_match(plot$labels$subtitle, "All 4 aligned or side-only rows")
})

test_that("the map is filled green for matches and red for mismatches", {
  skip_if_not_installed("ggplot2")
  m <- mixed_comparison()
  built <- ggplot2::ggplot_build(plot_diff(m$cmp))
  tiles <- built$data[[1L]]
  data <- plot_data(m$cmp)

  fills <- unique(tiles$fill[data$matched])
  mismatch_fills <- unique(tiles$fill[!data$matched])
  expect_equal(toupper(substr(fills, 1L, 7L)), "#009E73")
  expect_equal(toupper(substr(mismatch_fills, 1L, 7L)), "#D55E00")
})

test_that("small maps carry the redundant mark layer", {
  skip_if_not_installed("ggplot2")
  m <- mixed_comparison()
  built <- ggplot2::ggplot_build(plot_diff(m$cmp))

  # A text layer in addition to the tile layer means the four cell classes do
  # not depend on fill alone.
  expect_length(built$data, 2L)
  expect_setequal(built$data[[2L]]$label, c("", "!", "X", "Y"))
})

test_that("rendering to the default pdf device is encoding-clean", {
  # A delta glyph here raised "conversion failure in mbcsToSbcs" on pdf(),
  # which is the device R CMD check renders examples to.
  skip_if_not_installed("ggplot2")
  m <- mixed_comparison()

  device <- tempfile(fileext = ".pdf")
  grDevices::pdf(device)
  on.exit({
    grDevices::dev.off()
    unlink(device)
  }, add = TRUE)

  expect_no_warning(print(plot_diff(m$cmp)))
})

test_that("binned maps disclose binning and drop the per-tile symbols", {
  skip_if_not_installed("ggplot2")
  x <- data.frame(id = 1:40, v = as.double(1:40))
  y <- x
  y$v[1:10] <- 0
  plot <- plot_diff(compare_dt(x, y), max_rows = 10L, row_bins = 4L)
  built <- ggplot2::ggplot_build(plot)

  expect_match(plot$labels$subtitle, "All 40 rows, aggregated into 4")
  expect_length(built$data, 1L)
  expect_no_error(built)
})

test_that("plot() draws the map and returns it invisibly", {
  skip_if_not_installed("ggplot2")
  m <- mixed_comparison()

  device <- tempfile(fileext = ".pdf")
  grDevices::pdf(device)
  on.exit({
    grDevices::dev.off()
    unlink(device)
  }, add = TRUE)

  expect_invisible(plot(m$cmp))
  expect_s3_class(plot(m$cmp), "ggplot")
})

test_that("plot() rejects a positional second argument", {
  # `y` belongs to the plot() generic and is unused. Left unchecked it silently
  # swallowed `plot(cmp, "amount")`, the natural mirror of plot_diff(), and
  # drew every measure instead.
  skip_if_not_installed("ggplot2")
  m <- mixed_comparison()
  expect_error(plot(m$cmp, "a"), class = "daffiz_error_plot_argument")
  expect_error(plot(m$cmp, "a"), "Name the argument instead")
})

test_that("an empty comparison still produces a renderable plot", {
  skip_if_not_installed("ggplot2")
  x <- data.frame(id = integer(), a = numeric())
  plot <- plot_diff(compare_dt(x, x))

  expect_no_error(ggplot2::ggplot_build(plot))
  expect_match(plot$labels$subtitle, "No eligible rows")
})

test_that("plot_diff explains a missing ggplot2 rather than failing obscurely", {
  expect_error(check_ggplot2(available = FALSE), class = "daffiz_error_ggplot2")
  expect_error(check_ggplot2(available = FALSE), "requires the suggested package")
})
