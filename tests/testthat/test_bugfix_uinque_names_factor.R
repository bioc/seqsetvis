# providng names as a factor is causing mismatch between names and output
testthat::context("bugfix factor names")
library(testthat)
library(seqsetvis)
library(GenomicRanges)
# library(rtracklayer)
test_in = system.file("extdata", package = "seqsetvis", mustWork = TRUE)
bw_files = dir(test_in, pattern = "FE.+.bw$", full.names = TRUE)
test_df = data.frame(file = bw_files, sample = sub("_.+", "", basename(bw_files)))

data("CTCF_in_10a_overlaps_gr")
test_qgr = CTCF_in_10a_overlaps_gr

test_df.char = test_df
prof_dt.char = ssvFetchBigwig(test_df.char, test_qgr, return_data.table = TRUE)
prof_dt.char = prof_dt.char[, .(y = mean(y)), .(x, sample)]

test_df.fact = test_df
test_df.fact$sample = factor(test_df.fact$sample, levels = rev(test_df.fact$sample))

prof_dt.fact = ssvFetchBigwig(test_df.fact, test_qgr, return_data.table = TRUE)

prof_dt.fact = prof_dt.fact[, .(y = mean(y)), .(x, sample)]

prof_dt.char = prof_dt.char[order(x)][order(as.character(sample))]
prof_dt.fact = prof_dt.fact[order(x)][order(as.character(sample))]

ggplot() +
    geom_path(data = prof_dt.char, aes(x = x, y = y, group = sample), color = "blue") +
    geom_path(data = prof_dt.fact, aes(x = x, y = y+2, group = sample), color = "orange") +
    facet_wrap(~sample)

test_that("sample should be a factor in both profiles", {
    #for character sample query, order of levels in output should match input order
    expect_is(test_df.char$sample, "character")
    expect_is(prof_dt.char$sample, "factor")
    expect_equal(test_df.char$sample , levels(prof_dt.char$sample))

    #
    expect_is(test_df.fact$sample, "factor")
    expect_is(prof_dt.fact$sample, "factor")
    expect_equal(levels(test_df.fact$sample) , levels(prof_dt.fact$sample))

})

test_that("fragLen_calcStranded parameters matter", {
    expect_equal(prof_dt.char$y , prof_dt.fact$y)
})


