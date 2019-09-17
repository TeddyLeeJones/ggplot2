context("position_nudgestack")

test_that("position_nudgestack draws correctly", {
  ESM <- data_frame(
    DAX = EuStockMarkets[colnames(EuStockMarkets) == "DAX"],
    SMI = EuStockMarkets[colnames(EuStockMarkets) == "SMI"],
    CAC = EuStockMarkets[colnames(EuStockMarkets) == "CAC"],
    FTSE = EuStockMarkets[colnames(EuStockMarkets) == "FTSE"],
    date = as.Date(paste(1, zoo::as.yearmon(time(EuStockMarkets))),
      format = "%d %b %Y"
    )
  )

  ESM_prep <- ESM %>%
    tidyr::gather(key = key, value = value, -date) %>%
    group_by(date, key) %>%
    summarize(value = mean(value)) %>%
    filter(date >= "1995-01-01" & date < "1998-01-01")

  stock_marked <- ggplot(data = ESM_prep, mapping = aes(x = date, y = value, fill = key)) +
    geom_col(position = position_nudgestack(x = 15))

  expect_doppelganger(
    "nudgestack EuStockMarkets data",
    stock_marked
  )
})
