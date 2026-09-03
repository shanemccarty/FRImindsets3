# Shared setup for the FRI Peer Mentor Mindsets Guide - SEMESTER 3 EDITION.
# Scores come from data/survey_pre_sem3.csv: 25 peer mentors across three streams
# (Drug Discovery 11, Environmental Visualization 8, Public Health 6), de-identified
# (PASSWORD + STREAM + item responses), built by scoring-key.R from the Qualtrics export.
#
# Independent-study respondents (ROLE == 2) are EXCLUDED from the dataset entirely,
# so they appear in no distribution, percentile, or lookup table.
#
# Install once:  install.packages(c("dplyr","tibble","purrr","ggplot2","kableExtra"))
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(kableExtra)
})

# Captions and labels use typographic characters (en dash, x, minus, ellipsis). In a
# non-UTF-8 locale kableExtra mangles them into stray <u> tags in the rendered table
# captions - silently, and only visible in the HTML. Fail loudly instead.
if (!isTRUE(l10n_info()$`UTF-8`)) {
  stop("R is running in a non-UTF-8 locale (", Sys.getlocale("LC_CTYPE"), "). ",
       "Table captions would render corrupted. Set a UTF-8 locale before rendering, ",
       "e.g. LANG=en_US.UTF-8 quarto render.", call. = FALSE)
}

source("colors.R")
green <- brand_green; accent <- brand_accent
band_fill <- c(Low = "#EAF3EE", Mid = "#B9D8CA", High = "#005A43")

dat <- read.csv("data/survey_pre_sem3.csv", check.names = FALSE)

# Stream is a lookup aid, never a scoring boundary: every percentile and every
# distribution in this guide is computed across all 25 mentors pooled. Per-stream
# norming would put one mentor at the 0th and another at the 100th percentile on
# every measure (n = 6-11 per stream), which is why it is not done here.
stream_levels <- c("Drug Discovery", "Environmental Visualization", "Public Health")
dat$STREAM <- factor(dat$STREAM, levels = stream_levels)

# ---- Lookup key: last 2 letters + 4 digits ----------------------------------
# Passwords were created as 5 letters + 4 digits (e.g. BILLS8722). A full password
# can be identifying - people pick initials, names and dates - and every lookup
# table is published, so the guide shows only the LAST TWO LETTERS and the four
# digits (BILLS8722 -> LS8722).
#
# Done here, once, so every chapter inherits it: chapters read dat$PASSWORD.
# PASSWORD_FULL is kept in memory for matching the post-semester wave and for
# resolving a mentor who cannot find their row; it is never printed.
dat$PASSWORD_FULL <- dat$PASSWORD
dat$PASSWORD <- sub("^[A-Za-z]{3}", "", dat$PASSWORD)

if (anyDuplicated(dat$PASSWORD)) {
  stop("Truncating passwords to 2 letters + 4 digits created a collision: ",
       paste(unique(dat$PASSWORD[duplicated(dat$PASSWORD)]), collapse = ", "),
       ". Two mentors would share a lookup row - lengthen the key before rendering.",
       call. = FALSE)
}

# Percentile + low/mid/high label, e.g. "72% (high)"
add_percentile <- function(x) {
  pct <- round(dplyr::percent_rank(x) * 100)
  lab <- dplyr::case_when(pct < 33.33 ~ "low", pct < 66.67 ~ "mid", TRUE ~ "high")
  paste0(pct, "% (", lab, ")")
}

# Password lookup table for a named list of composite vectors.
# `extra` holds non-numeric columns (e.g. a typology label) appended after the composites.
# `stream` defaults to the stream column of `dat`, so chapters that compute composites
# in dat order get the right stream per row without passing anything. Rows are grouped
# by stream and alphabetised by password within it, so a mentor scans ~10 rows, not 25.
#
# `pct` names which composites get a percentile column; the default is all of them.
# Restrict it for CONSTANT-SUM measures. Where two subscales share a fixed budget
# (Mentoring Style = 40, Leadership Role = 50) each is the other's complement, so
# their percentiles are mirror images (r = -0.98 in this cohort). Printing both
# invites the reading that someone is simultaneously low on one and high on the
# other, when the pair carries only one number's worth of information.
score_table <- function(pw, named, caption, extra = NULL, stream = dat$STREAM,
                        pct = names(named)) {
  df <- tibble::tibble(Password = pw, Stream = stream)
  for (nm in names(named)) {
    df[[nm]] <- round(named[[nm]], 2)
    if (nm %in% pct) df[[paste0(nm, " (%)")]] <- add_percentile(named[[nm]])
  }
  for (nm in names(extra)) df[[nm]] <- extra[[nm]]
  df <- dplyr::arrange(df, Stream, Password)
  n_by_stream <- table(df$Stream)
  df$Stream <- NULL                       # shown as group headers instead of a column
  kbl(df, escape = FALSE, caption = caption) |>
    kable_styling(bootstrap_options = c("striped","hover","condensed"), full_width = FALSE) |>
    pack_rows(index = n_by_stream, label_row_css = "background-color:#EAF3EE;color:#005A43;")
}

# Class distribution histograms with Low/Mid/High tertile bands + 33rd/67th cutoffs.
# `scale` is the measure's full response range, e.g. c(1, 10). When given, every panel spans
# that range with a tick per response option, so panels are comparable within and across
# chapters. `by` widens the tick step for scales too dense to label one-by-one (e.g. 0-100);
# `labels` supplies verbal anchors instead of numbers.
# `bands = FALSE` drops the Low/Mid/High shading for measures whose distribution is too
# compressed for tertiles to mean anything (e.g. the EFFECTIVE item in Chapter 3, where
# almost everyone answered 4 or 5 and a tertile split would be an artefact).
norm_facets <- function(named, xlab = "Composite score", bins = 6,
                        scale = NULL, by = 1, labels = NULL, bands = TRUE) {
  long  <- purrr::imap_dfr(named, ~ tibble::tibble(subscale = .y, score = .x))
  rects <- purrr::imap_dfr(named, function(x, nm) {
    q <- quantile(x, c(1/3, 2/3), na.rm = TRUE)
    tibble::tibble(subscale = nm,
                   xmin = c(-Inf, q[1], q[2]), xmax = c(q[1], q[2], Inf),
                   band = factor(c("Low","Mid","High"), levels = c("Low","Mid","High")))
  })
  cuts <- purrr::imap_dfr(named, function(x, nm) {
    q <- quantile(x, c(1/3, 2/3), na.rm = TRUE); tibble::tibble(subscale = nm, xint = as.numeric(q))
  })
  lv <- names(named)
  long$subscale  <- factor(long$subscale,  levels = lv)
  rects$subscale <- factor(rects$subscale, levels = lv)
  cuts$subscale  <- factor(cuts$subscale,  levels = lv)
  if (!bands) { rects <- rects[0, ]; cuts <- cuts[0, ] }
  p <- ggplot() +
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf, fill = band), alpha = .55) +
    geom_histogram(data = long, aes(score), bins = bins, fill = green, color = "white") +
    geom_vline(data = cuts, aes(xintercept = xint), linetype = "dashed", color = "#333333", linewidth = .5) +
    scale_fill_manual(values = band_fill, name = "Band") +
    labs(x = xlab, y = "Peer mentors") +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(color = green, face = "bold"),
          plot.title = element_text(color = green, face = "bold"),
          legend.position = if (bands) "top" else "none")
  if (is.null(scale)) {
    p + facet_wrap(~ subscale, scales = "free_x")
  } else {
    brk <- seq(scale[1], scale[2], by = by)
    # coord_cartesian (not scale limits) so bins straddling the scale ends are clipped
    # visually rather than dropped, which would emit a missing-values warning.
    p + facet_wrap(~ subscale) +
      scale_x_continuous(breaks = brk, labels = if (is.null(labels)) brk else labels) +
      coord_cartesian(xlim = scale) +
      theme(panel.grid.minor.x = element_blank(),
            panel.spacing.x = grid::unit(1.1, "lines"),
            axis.text.x = element_text(size = rel(0.75)),
            # room for a wide outermost tick label (e.g. "Frequently") to sit uncut
            plot.margin = margin(5.5, 16, 5.5, 5.5))
  }
}

# ---- Mentoring-style typology (Support x Structure) -------------------------
# Quadrant tints read off the construct palette: the structure-heavy quadrant leans
# red-orange, the support-heavy quadrant leans teal, high-both blends the two, and
# low-both stays neutral grey.
quad_fill <- c(Authoritative = "#CFE4DC", Authoritarian = construct_light[["Structure"]],
               Permissive    = construct_light[["Support"]], Uninvolved = "#F0F0F0")

# Median split of the class on each axis -> one of four style labels.
# Median (not the 1-10 scale midpoint) because importance ratings skew high:
# a midpoint split would put nearly everyone in the same quadrant.
style_type <- function(support, structure) {
  ms <- median(support,   na.rm = TRUE)
  mt <- median(structure, na.rm = TRUE)
  factor(dplyr::case_when(
    support >= ms & structure >= mt ~ "Authoritative",
    support <  ms & structure >= mt ~ "Authoritarian",
    support >= ms & structure <  mt ~ "Permissive",
    TRUE                            ~ "Uninvolved"),
    levels = names(quad_fill))
}

# Scatterplot of the class with shaded, labelled quadrants and median split lines.
typology_plot <- function(support, structure,
                          xlab = "Support (importance 1-10)",
                          ylab = "Structure (importance 1-10)") {
  ms <- median(support, na.rm = TRUE); mt <- median(structure, na.rm = TRUE)
  pts <- tibble::tibble(support = support, structure = structure)
  xr  <- range(support,   na.rm = TRUE) + c(-0.5, 0.5)
  yr  <- range(structure, na.rm = TRUE) + c(-0.5, 0.5)
  quad <- tibble::tibble(
    label = factor(names(quad_fill), levels = names(quad_fill)),
    xmin  = c(ms, xr[1], ms, xr[1]), xmax = c(xr[2], ms, xr[2], ms),
    ymin  = c(mt, mt, yr[1], yr[1]), ymax = c(yr[2], yr[2], mt, mt),
    lx    = c(xr[2], xr[1], xr[2], xr[1]), ly = c(yr[2], yr[2], yr[1], yr[1]),
    hj    = c(1, 0, 1, 0), vj = c(1, 1, 0, 0))
  ggplot() +
    geom_rect(data = quad, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                               fill = label), alpha = 0.75) +
    geom_vline(xintercept = ms, linetype = "dashed", color = "#333333", linewidth = .5) +
    geom_hline(yintercept = mt, linetype = "dashed", color = "#333333", linewidth = .5) +
    geom_point(data = pts, aes(support, structure), color = green,
               size = 2.6, alpha = 0.8,
               position = position_jitter(width = 0.07, height = 0.07, seed = 1)) +
    geom_text(data = quad, aes(lx, ly, label = label, hjust = hj, vjust = vj),
              color = "#33553F", fontface = "bold", size = 3.5,
              nudge_x = c(-.12, .12, -.12, .12), nudge_y = c(-.12, -.12, .12, .12)) +
    scale_fill_manual(values = quad_fill, guide = "none") +
    coord_cartesian(xlim = xr, ylim = yr, expand = FALSE) +
    labs(x = xlab, y = ylab) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          # each axis title carries its own construct's color
          axis.title.x = element_text(color = construct_color("Support"), face = "bold"),
          axis.title.y = element_text(color = construct_color("Structure"), face = "bold"))
}

# ---- Regulatory focus by goal (Week 2) --------------------------------------
# Two-dimensional view of one focus: how strongly you hold it (x, on the response
# scale) against which goal it points at (y, bipolar academic vs social).
# The y axis is a difference score, so 0 is a real anchor (equal weight), unlike
# the x axis where high/low is relative to the class median.
goal_band <- c(Academic = "#ECEFEE", Social = "#F8F6F7")

focus_plot <- function(academic, social, focus, point_col, scale = c(1, 9)) {
  strength <- (academic + social) / 2      # mean of the two cells = overall focus
  lean     <- academic - social            # + = academic-leaning, - = social-leaning
  pts <- tibble::tibble(strength = strength, lean = lean)
  m   <- median(strength, na.rm = TRUE)
  yl  <- max(1, ceiling(max(abs(lean), na.rm = TRUE))) + 0.4
  halves <- tibble::tibble(
    goal = factor(c("Academic", "Social"), levels = names(goal_band)),
    ymin = c(0, -yl), ymax = c(yl, 0))
  ggplot() +
    geom_rect(data = halves, aes(xmin = scale[1], xmax = scale[2],
                                 ymin = ymin, ymax = ymax, fill = goal)) +
    geom_hline(yintercept = 0, color = "#333333", linewidth = .6) +
    geom_vline(xintercept = m, linetype = "dashed", color = "#333333", linewidth = .5) +
    geom_point(data = pts, aes(strength, lean), color = point_col, size = 2.6, alpha = .85,
               position = position_jitter(width = 0.07, height = 0.07, seed = 1)) +
    annotate("text", x = scale[1] + .15, y = yl - .15, label = "Academic goals",
             hjust = 0, vjust = 1, fontface = "bold", size = 3.4, color = "#4A5A52") +
    annotate("text", x = scale[1] + .15, y = -yl + .15, label = "Social goals",
             hjust = 0, vjust = 0, fontface = "bold", size = 3.4, color = "#4A5A52") +
    scale_fill_manual(values = goal_band, guide = "none") +
    scale_x_continuous(breaks = seq(scale[1], scale[2], 1)) +
    scale_y_continuous(breaks = seq(-floor(yl), floor(yl), 1)) +
    coord_cartesian(xlim = scale, ylim = c(-yl, yl), expand = FALSE) +
    labs(x = paste0(focus, " strength (", scale[1], "-", scale[2], ")"),
         y = "Goal lean (academic minus social)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          axis.title.x = element_text(color = point_col, face = "bold"),
          axis.title.y = element_text(color = "#333333"))
}

# Academic-leaning / balanced / social-leaning label for the lookup table.
goal_lean <- function(academic, social, tol = 0.25) {
  d <- academic - social
  factor(dplyr::case_when(d >  tol ~ "Academic-leaning",
                          d < -tol ~ "Social-leaning",
                          TRUE     ~ "Balanced"),
         levels = c("Academic-leaning", "Balanced", "Social-leaning"))
}
