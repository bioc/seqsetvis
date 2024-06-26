testthat::context("Anchor")
# flipping viewGranges
library(seqsetvis)
library(GenomicRanges)
library(testthat)
library(data.table)
data(CTCF_in_10a_overlaps_gr)
qgr = CTCF_in_10a_overlaps_gr[1:5]
qgr = resize(qgr, 120)
strand(qgr) = c("+", "-", "-", "+", "-")
# qgr = centerFixedSizeGRanges(qgr, 500)
#bed used to intersect bam
# rtracklayer::export.bed(qgr, con = "ctcf_5.bed")
bam_file = system.file("extdata/test.bam", package = "seqsetvis")

fetchBam = seqsetvis:::fetchBam

bam_gr = fetchBam(bam_file, qgr)
# bams
bams = c("A" = bam_file, "B" = bam_file)

#sampling
test_that("viewGRangesWinSample_dt center", {
    exp_x = seq(-2, 1)*(30)+15
    dt = viewGRangesWinSample_dt(bam_gr, qgr, window_size = 30, anchor = "center")
    expect_setequal(dt$x, exp_x)
    # dt$group = "stranded"
    dt_us = viewGRangesWinSample_dt(bam_gr, qgr, window_size = 30, anchor = "center_unstranded")
    expect_setequal(dt_us$x, exp_x)
    # dt_us$group = "unstranded"
    # ssvSignalLineplot(rbind(dt, dt_us), sample_ = "id", color_ = "strand", group_ = "group")
    expect_true(all(dt[strand == "+"][order(x)]$y == dt_us[strand == "+"][order(x)]$y))
    expect_false(all(dt[strand == "-"][order(x)]$y == dt_us[strand == "-"][order(x)]$y))
    expect_false(all(dt$x > 0))
    expect_false(all(dt$x < 0))
    expect_false(all(dt_us$x > 0))
    expect_false(all(dt_us$x < 0))
})

test_that("viewGRangesWinSample_dt center odd window", {
    exp_x = seq(-2, 1)*30+15
    dt = viewGRangesWinSample_dt(bam_gr, qgr, window_size = 30, anchor = "center")
    expect_setequal(dt$x, exp_x)
    # dt$group = "stranded"
    dt_us = viewGRangesWinSample_dt(bam_gr, qgr, window_size = 30, anchor = "center_unstranded")
    expect_setequal(dt_us$x, exp_x)
})

test_that("viewGRangesWinSample_dt left", {
    exp_x = seq(-2, 1)*(30)+75
    dt = viewGRangesWinSample_dt(bam_gr, qgr, window_size = 30, anchor = "left")
    expect_setequal(dt$x, exp_x)
    # dt$group = "stranded"
    dt_us = viewGRangesWinSample_dt(bam_gr, qgr, window_size = 30, anchor = "left_unstranded")
    expect_setequal(dt_us$x, exp_x)
    # dt_us$group = "unstranded"
    # ssvSignalLineplot(rbind(dt, dt_us), sample_ = "id", color_ = "strand", group_ = "group")
    expect_equal(dt[strand == "+"][order(x)]$y, dt_us[strand == "+"][order(x)]$y)
    expect_failure(expect_equal(dt[strand == "-"][order(x)]$y, dt_us[strand == "-"][order(x)]$y))
    expect_true(all(dt$x > 0))
    expect_true(all(dt_us$x > 0))
})

test_that("viewGRangesWinSample_dt left odd window", {
    exp_x = (seq(-2, 0)+2)*40+20
    dt = viewGRangesWinSample_dt(bam_gr, qgr, window_size = 40, anchor = "left")
    expect_setequal(dt$x, exp_x)
    # dt$group = "stranded"
    dt_us = viewGRangesWinSample_dt(bam_gr, qgr, window_size = 40, anchor = "left_unstranded")
    expect_setequal(dt_us$x, exp_x)
})

test_that("ssvFetchBam anchors", {
    exp_30 = seq(-2, 1)*(30)+75
    dt_30_left = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 30, anchor = "left", fragLens = NA)
    dt_30_leftUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 30, anchor = "left_unstranded", fragLens = NA)
    dt_30_center = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 30, anchor = "center", fragLens = NA)
    dt_30_centerUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 30, anchor = "center_unstranded", fragLens = NA)

    expect_setequal(dt_30_left$x, exp_30)
    expect_setequal(dt_30_leftUn$x, exp_30)
    expect_setequal(dt_30_center$x, exp_30-60)
    expect_setequal(dt_30_centerUn$x, exp_30-60)

    exp_40 = (seq(-2, 0)+2)*40+20
    dt_40_left = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 40, anchor = "left", fragLens = NA)
    dt_40_leftUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 40, anchor = "left_unstranded", fragLens = NA)
    dt_40_center = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 40, anchor = "center", fragLens = NA)
    dt_40_centerUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 40, anchor = "center_unstranded", fragLens = NA)

    expect_setequal(dt_40_left$x, exp_40)
    expect_setequal(dt_40_leftUn$x, exp_40)
    expect_setequal(dt_40_center$x, exp_40-60)
    expect_setequal(dt_40_centerUn$x, exp_40-60)

    exp_5 = floor(seq(1, width(qgr[1])/5)*5-2.5)
    dt_5_left = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 5, anchor = "left", fragLens = NA)
    dt_5_leftUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 5, anchor = "left_unstranded", fragLens = NA)
    dt_5_center = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 5, anchor = "center", fragLens = NA)
    dt_5_centerUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 5, anchor = "center_unstranded", fragLens = NA)

    expect_setequal(dt_5_left$x, exp_5)
    expect_setequal(dt_5_leftUn$x, exp_5)
    expect_setequal(dt_5_center$x, exp_5-60)
    expect_setequal(dt_5_centerUn$x, exp_5-60)
})

test_that("ssvFetchBam anchors target_strand = 'both'", {
    exp_30 = seq(-2, 1)*(30)+75
    dt_30_left = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 30, anchor = "left", fragLens = NA, target_strand = "both")
    dt_30_leftUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 30, anchor = "left_unstranded", fragLens = NA, target_strand = "both")
    dt_30_center = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 30, anchor = "center", fragLens = NA, target_strand = "both")
    dt_30_centerUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 30, anchor = "center_unstranded", fragLens = NA, target_strand = "both")

    expect_setequal(dt_30_left$x, exp_30)
    expect_setequal(dt_30_leftUn$x, exp_30)
    expect_setequal(dt_30_center$x, exp_30-60)
    expect_setequal(dt_30_centerUn$x, exp_30-60)

    exp_40 = (seq(-2, 0)+2)*40+20
    dt_40_left = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 40, anchor = "left", fragLens = NA, target_strand = "both")
    dt_40_leftUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 40, anchor = "left_unstranded", fragLens = NA, target_strand = "both")
    dt_40_center = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 40, anchor = "center", fragLens = NA, target_strand = "both")
    dt_40_centerUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 40, anchor = "center_unstranded", fragLens = NA, target_strand = "both")

    expect_setequal(dt_40_left$x, exp_40)
    expect_setequal(dt_40_leftUn$x, exp_40)
    expect_setequal(dt_40_center$x, exp_40-60)
    expect_setequal(dt_40_centerUn$x, exp_40-60)

    exp_5 = floor(seq(1, width(qgr[1])/5)*5-2.5)
    dt_5_left = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 5, anchor = "left", fragLens = NA, target_strand = "both")
    dt_5_leftUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 5, anchor = "left_unstranded", fragLens = NA, target_strand = "both")
    dt_5_center = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 5, anchor = "center", fragLens = NA, target_strand = "both")
    dt_5_centerUn = ssvFetchBam(bam_file, qgr, win_method = "sample", win_size = 5, anchor = "center_unstranded", fragLens = NA, target_strand = "both")

    expect_setequal(dt_5_left$x, exp_5)
    expect_setequal(dt_5_leftUn$x, exp_5)
    expect_setequal(dt_5_center$x, exp_5-60)
    expect_setequal(dt_5_centerUn$x, exp_5-60)
})

#summary
test_that("viewGRangesWinSummary_dt center", {
    dt = viewGRangesWinSummary_dt(bam_gr, qgr, n_tiles = 10, anchor = "center")
    # dt$group = "stranded"
    dt_us = viewGRangesWinSummary_dt(bam_gr, qgr, n_tiles = 10, anchor = "center_unstranded")
    # dt_us$group = "unstranded"
    # ssvSignalLineplot(rbind(dt, dt_us), sample_ = "id", color_ = "strand", group_ = "group")
    expect_equal(dt[strand == "+"][order(x)]$y, dt_us[strand == "+"][order(x)]$y)
    expect_failure(expect_equal(dt[strand == "-"][order(x)]$y, dt_us[strand == "-"][order(x)]$y))
    expect_false(all(dt$x > 0))
    expect_false(all(dt$x < 0))
    expect_false(all(dt_us$x > 0))
    expect_false(all(dt_us$x < 0))
})

test_that("viewGRangesWinSummary_dt left", {
    dt = viewGRangesWinSummary_dt(bam_gr, qgr, n_tiles = 10, anchor = "left")
    # dt$group = "stranded"
    dt_us = viewGRangesWinSummary_dt(bam_gr, qgr, n_tiles = 10, anchor = "left_unstranded")
    # dt_us$group = "unstranded"
    # ssvSignalLineplot(rbind(dt, dt_us), sample_ = "id", color_ = "strand", group_ = "group")
    expect_equal(dt[strand == "+"][order(x)]$y, dt_us[strand == "+"][order(x)]$y)
    expect_failure(expect_equal(dt[strand == "-"][order(x)]$y, dt_us[strand == "-"][order(x)]$y))
    expect_true(all(dt$x > 0))
    expect_true(all(dt_us$x > 0))
})



test_that("ssvFetchBam anchor GRanges", {
    gr = ssvFetchBam(bams, qgr, win_size = 30, anchor = "center")
    gr_uns = ssvFetchBam(bams, qgr, win_size = 30, anchor = "center_unstranded")
    is_pos = as.character(strand(gr)) == "+"
    # plot(gr_uns$x[is_pos], gr$x[is_pos])
    # plot(gr_uns$x[!is_pos], gr$x[!is_pos])
    expect_equal(start(gr_uns)[is_pos], start(gr)[is_pos])
    expect_failure(expect_equal(start(gr_uns)[!is_pos], start(gr)[!is_pos]))
    expect_lt(min(gr$x), 0)
    expect_lt(min(gr_uns$x), 0)
})

test_that("ssvFetchBam anchor data.table", {
    dt = ssvFetchBam(bams, qgr, win_size = 30, anchor = "left",
                     return_data.table = TRUE)
    dt_uns = ssvFetchBam(bams, qgr, win_size = 30, anchor = "left_unstranded",
                         return_data.table = TRUE)

    is_pos = dt$strand == "+"
    # plot(dt_uns$x[is_pos], dt$x[is_pos])
    # plot(dt_uns$x[!is_pos], dt$x[!is_pos])
    expect_equal(dt_uns$start[is_pos], dt$start[is_pos])
    expect_failure(expect_equal(dt_uns$start[!is_pos], dt$start[!is_pos]))
    expect_gte(min(dt$x), 0)
    expect_gte(min(dt_uns$x), 0)

})

#bigwigs
bigwigs = dir(system.file("extdata", package = "seqsetvis"), pattern = "random100.bw$", full.names = TRUE)
test_that("ssvFetchBigwig anchor", {
    skip_on_os("windows")
    gr = ssvFetchBigwig(bigwigs, qgr, win_size = 30, anchor = "center")
    gr_uns = ssvFetchBigwig(bigwigs, qgr, win_size = 30, anchor = "center_unstranded")
    is_pos = as.character(strand(gr)) == "+"
    # plot(gr_uns$x[is_pos], gr$x[is_pos])
    # plot(gr_uns$x[!is_pos], gr$x[!is_pos])
    expect_equal(gr_uns$x[is_pos], gr$x[is_pos])
    expect_failure(expect_equal(gr_uns$x[!is_pos], gr$x[!is_pos]))
    expect_lt(min(gr$x), 0)
    expect_lt(min(gr_uns$x), 0)

    dt = ssvFetchBigwig(bigwigs, qgr, win_size = 30, anchor = "left", return_data.table = TRUE)
    dt_uns = ssvFetchBigwig(bigwigs, qgr, win_size = 30, anchor = "left_unstranded", return_data.table = TRUE)

    is_pos = dt$strand == "+"
    # plot(dt_uns$x[is_pos], dt$x[is_pos])
    # plot(dt_uns$x[!is_pos], dt$x[!is_pos])
    expect_equal(dt_uns$x[is_pos], dt$x[is_pos])
    expect_failure(expect_equal(dt_uns$x[!is_pos], dt$x[!is_pos]))
    expect_gte(min(dt$x), 0)
    expect_gte(min(dt_uns$x), 0)
})


# bams
bams = c("A" = bam_file, "B" = bam_file)

test_that("ssvFetchBam anchor - summary GRanges", {
    gr = ssvFetchBam(bams, qgr, win_size = 30, anchor = "center", win_method = "summary")
    gr_uns = ssvFetchBam(bams, qgr, win_size = 30, anchor = "center_unstranded", win_method = "summary")
    is_pos = as.character(strand(gr)) == "+"
    # plot(gr_uns$x[is_pos], gr$x[is_pos])
    # plot(gr_uns$x[!is_pos], gr$x[!is_pos])
    expect_equal(start(gr_uns)[is_pos], start(gr)[is_pos])
    expect_failure(expect_equal(start(gr_uns)[!is_pos], start(gr)[!is_pos]))
    expect_lt(min(gr$x), 0)
    expect_lt(min(gr_uns$x), 0)
})
test_that("ssvFetchBam anchor - summary data.table", {
    dt = ssvFetchBam(bams, qgr, win_size = 30, anchor = "left", return_data.table = TRUE, win_method = "summary")
    dt_uns = ssvFetchBam(bams, qgr, win_size = 30, anchor = "left_unstranded", return_data.table = TRUE, win_method = "summary")

    is_pos = dt$strand == "+"
    # plot(dt_uns$x[is_pos], dt$x[is_pos])
    # plot(dt_uns$x[!is_pos], dt$x[!is_pos])
    expect_equal(dt_uns$start[is_pos], dt$start[is_pos])
    expect_failure(expect_equal(dt_uns$start[!is_pos], dt$start[!is_pos]))
    expect_gte(min(dt$x), 0)
    expect_gte(min(dt_uns$x), 0)

})

#bigwigs
bigwigs = dir(system.file("extdata", package = "seqsetvis"), pattern = "random100.bw$", full.names = TRUE)
test_that("ssvFetchBigwig anchor - summary", {
    skip_on_os("windows")
    gr = ssvFetchBigwig(bigwigs, qgr, win_size = 30, anchor = "center", win_method = "summary")
    gr_uns = ssvFetchBigwig(bigwigs, qgr, win_size = 30, anchor = "center_unstranded", win_method = "summary")
    is_pos = as.character(strand(gr)) == "+"
    # plot(gr_uns$x[is_pos], gr$x[is_pos])
    # plot(gr_uns$x[!is_pos], gr$x[!is_pos])
    expect_equal(gr_uns$x[is_pos], gr$x[is_pos])
    expect_failure(expect_equal(gr_uns$x[!is_pos], gr$x[!is_pos]))
    expect_lt(min(gr$x), 0)
    expect_lt(min(gr_uns$x), 0)

    dt = ssvFetchBigwig(bigwigs, qgr, win_size = 30, anchor = "left", return_data.table = TRUE, win_method = "summary")
    dt_uns = ssvFetchBigwig(bigwigs, qgr, win_size = 30, anchor = "left_unstranded", return_data.table = TRUE, win_method = "summary")

    is_pos = dt$strand == "+"
    # plot(dt_uns$x[is_pos], dt$x[is_pos])
    # plot(dt_uns$x[!is_pos], dt$x[!is_pos])
    expect_equal(dt_uns$x[is_pos], dt$x[is_pos])
    expect_failure(expect_equal(dt_uns$x[!is_pos], dt$x[!is_pos]))
    expect_gte(min(dt$x), 0)
    expect_gte(min(dt_uns$x), 0)
})

qgr = CTCF_in_10a_overlaps_gr[1:5]
qgr = resize(qgr, 120)
strand(qgr) = c("+", "-", "-", "+", "-")

#bug target ssvFetchBam with stranded qgr and odd win_size causes inconsistent x values
test_that("bugfix - stranded qgr and odd win_size causes inconsistent x values - center(default)", {
    view_size = 30
    even_size = 10
    odd_size = 5
    qgr = resize(qgr, view_size, fix = "center")
    qdf = data.frame(file = bam_file, mark = "a")
    res_even = ssvFetchBam(qdf, win_size = even_size, qgr = qgr, fragLens = NA,
                           target_strand = "*", return_data.table = TRUE)
    # strand(qgr) = "+"
    res_odd = ssvFetchBam(qdf, win_size = odd_size, qgr = qgr, fragLens = NA,
                          target_strand = "*", return_data.table = TRUE)

    expect_equal(view_size / even_size, length(unique(res_even$x)))
    expect_equal(view_size / odd_size, length(unique(res_odd$x)))
})

test_that("bugfix - stranded qgr and odd win_size causes inconsistent x values - center(default) - summary", {
    view_size = 30
    even_size = 10
    odd_size = 5
    qgr = resize(qgr, view_size, fix = "center")
    qdf = data.frame(file = bam_file, mark = "a")
    res_even = ssvFetchBam(qdf, win_size = even_size, qgr = qgr, fragLens = NA,
                           win_method = "summary",
                           target_strand = "both", return_data.table = TRUE)
    # strand(qgr) = "+"
    res_odd = ssvFetchBam(qdf, win_size = odd_size, qgr = qgr, fragLens = NA,
                          win_method = "summary",
                          target_strand = "both", return_data.table = TRUE)

    expect_equal(even_size, length(unique(res_even$x)))
    expect_equal(odd_size, length(unique(res_odd$x)))
})


test_that("bugfix - stranded qgr and odd win_size causes inconsistent x values - left", {
    view_size = 30
    even_size = 10
    odd_size = 5
    qgr = resize(qgr, view_size, fix = "center")
    qdf = data.frame(file = bam_file, mark = "a")
    res_even = ssvFetchBam(qdf, win_size = even_size, qgr = qgr,
                           anchor = "left",
                           fragLens = NA,
                           target_strand = "both", return_data.table = TRUE)
    # strand(qgr) = "+"
    res_odd = ssvFetchBam(qdf, win_size = odd_size, qgr = qgr,
                          anchor = "left",
                          fragLens = NA,
                          target_strand = "both", return_data.table = TRUE)

    expect_equal(view_size / even_size, length(unique(res_even$x)))
    expect_equal(view_size / odd_size, length(unique(res_odd$x)))
})

test_that("bugfix - stranded qgr and odd win_size causes inconsistent x values - left - summary", {
    view_size = 30
    even_size = 10
    odd_size = 5
    qgr = resize(qgr, view_size, fix = "center")
    qdf = data.frame(file = bam_file, mark = "a")
    res_even = ssvFetchBam(qdf, win_size = even_size, qgr = qgr,
                           win_method = "summary",
                           anchor = "left",
                           fragLens = NA,
                           target_strand = "both", return_data.table = TRUE)
    # strand(qgr) = "+"
    res_odd = ssvFetchBam(qdf, win_size = odd_size, qgr = qgr,
                          win_method = "summary",
                          anchor = "left",
                          fragLens = NA,
                          target_strand = "both", return_data.table = TRUE)

    expect_equal(even_size, length(unique(res_even$x)))
    expect_equal(odd_size, length(unique(res_odd$x)))
})

#
test_that("ssvFetchBam bugfix - query GRanges size of 1 - loses strand sensitivity and x bounces around from 0 to 1", {
    view_size.sm = 1
    view_size.lg = 5
    win_size = 1
    qdf = data.frame(file = bam_file, mark = "a")
    qgr.sm = resize(qgr, view_size.sm, fix = "center")
    res_small = ssvFetchBam(qdf, win_size = win_size,
                            qgr = qgr.sm,
                            fragLens = NA,
                            target_strand = "+", return_data.table = TRUE)
    # strand(qgr) = "+"
    qgr.lg = resize(qgr, view_size.lg, fix = "center")
    res_large = ssvFetchBam(qdf, win_size = win_size,
                            qgr = qgr.lg,
                            fragLens = NA,
                            target_strand = "+", return_data.table = TRUE)


    setequal(res_small[x == 0]$y,
             res_large[x == 0]$y)

    expect_setequal(res_small[x == 0]$y, res_large[x == 0]$y)

    all(res_small[x == 0][order(strand)][order(x)]$y ==
            res_large[x == 0][order(strand)][order(x)]$y)

    expect_equal(res_small[x == 0][order(strand)][order(x)]$y,
                 res_large[x == 0][order(strand)][order(x)]$y)
})
