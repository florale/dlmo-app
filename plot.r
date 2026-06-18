library(data.table)
library(ggplot2)
library(patchwork)
library(bayesplot)

bp_cols <- unname(unlist(bayesplot::color_scheme_get("red")))

profile_col <- bp_cols[4]
line_col <- bp_cols[2]
point_col <- bp_cols[4]
clicked_col <- bp_cols[5]

fmt_time <- function(x) {
  h <- floor(x) %% 24
  m <- round((x - floor(x)) * 60)
  h <- ifelse(m == 60, (h + 1) %% 24, h)
  m <- ifelse(m == 60, 0, m)
  sprintf("%02d:%02d", h, m)
}

dlmo_check <- readRDS("data/dlmo_check.rds")

plot_dlmo_app <- function(d, time_gap = 2, y_gap = 1, label_gap = 1,
                          clicked_dlmo = NA_real_) {
  m <- as.numeric(unlist(d$melatonin[[1]]))
  time_h <- as.numeric(unlist(d$time[[1]]))

  dat <- data.table(time_h = time_h, melatonin = m)

  hockey_y <- if (length(d[["dlmo_hs"]]) == 1 && !is.na(d[["dlmo_hs"]])) {
    approx(time_h, m, xout = d[["dlmo_hs"]], rule = 2)$y
  } else {
    NA_real_
  }

  methods <- rbindlist(list(
    data.table(method = "4 pg/ml", y = 4, dlmo = d[["dlmo_fixed_4"]]),
    data.table(method = "3 pg/ml", y = 3, dlmo = d[["dlmo_fixed_3"]]),
    data.table(method = "Hockey Stick", y = hockey_y, dlmo = d[["dlmo_hs"]])
  ))

  methods[, show_label := !is.na(dlmo) | method %in% c("Hockey Stick", "2SD Threshold", "Custom")]

  methods[, label := fifelse(
    is.na(dlmo),
    paste0(method, " – no DLMO"),
    paste0(method, " – DLMO: ", fmt_time(dlmo))
  )]

  methods[, y_lab := y]

  methods[
    is.na(dlmo) & show_label == TRUE,
    y_lab := min(m, na.rm = TRUE) - seq_len(.N) * 0.5
  ]

  methods[show_label == TRUE & !is.na(dlmo), time_grp := {
    o <- order(dlmo)
    g <- integer(.N)
    g[o] <- cumsum(c(
      TRUE,
      diff(dlmo[o]) > time_gap | abs(diff(y_lab[o])) > y_gap
    ))
    g
  }]

  methods[
    show_label == TRUE & !is.na(dlmo),
    y_lab := {
      o <- order(dlmo)
      z <- y_lab

      if (.N > 1) {
        z[o] <- mean(y_lab[o], na.rm = TRUE) +
          (seq_len(.N) - (.N + 1) / 2) * label_gap
      }

      z
    },
    by = time_grp
  ]

  methods[, time_grp := NULL]

  x_start <- min(time_h, na.rm = TRUE) - 1
  x_end <- max(time_h, na.rm = TRUE) + 1
  y_top <- max(m, na.rm = TRUE)

  p <- ggplot(dat, aes(time_h, melatonin)) +
    geom_line(linewidth = 0.75, colour = line_col) +
    geom_point(size = 2, colour = point_col) +
    # add melatonin level labels
    # geom_text(
    #   aes(x = time_h - 0.25, y = melatonin - 0.75, label = round(melatonin, 2)),
    #   # hjust = -0.5,
    #   # vjust = +0.5,
    #   # position = position_nudge(x = 0.25, y = -0.75),
    #   size = 3,
    #   colour = profile_col,
    #   inherit.aes = FALSE
    # ) +
    # geom_hline(yintercept = 3, linetype = "dashed", colour = "grey50") +
    # geom_hline(yintercept = 4, linetype = "dashed", colour = "grey50") +
    # annotate(
    #   "text",
    #   x = x_end + 0.15,
    #   y = 3,
    #   label = "3 pg/ml",
    #   hjust = 0,
    #   vjust = -0.4,
    #   size = 3.3,
    #   colour = "grey40",
    #   fontface = "bold"
    # ) +
    # annotate(
    #   "text",
    #   x = x_end + 0.15,
    #   y = 4,
    #   label = "4 pg/ml",
    #   hjust = 0,
    #   vjust = -0.4,
    #   size = 3.3,
    #   colour = "grey40",
    #   fontface = "bold"
    # ) +
    # geom_segment(
    #   data = methods[!is.na(y) & !is.na(dlmo)],
    #   aes(y = y, yend = y, colour = method),
    #   x = x_start,
    #   xend = x_end,
    #   linewidth = 0.75,
    #   linetype = "dashed",
    #   inherit.aes = FALSE
    # ) +
    # geom_point(
    #   data = methods[!is.na(y) & !is.na(dlmo)],
    #   aes(x = dlmo, y = y, colour = method),
    #   shape = 18,
    #   size = 4.5,
    #   inherit.aes = FALSE
    # ) +
    # geom_text(
    #   data = methods[show_label == TRUE],
    #   aes(x = x_end + 0.15, y = y_lab, label = label, colour = method),
    #   hjust = 0,
    #   size = 3.5,
    #   fontface = "bold",
    #   inherit.aes = FALSE
    # ) +
    # scale_colour_manual(values = c(
    #   "4 pg/ml" = "#7474A4",
    #   "3 pg/ml" = "#B3B3D3",
    #   "Hockey Stick" = "#7C93B2",
    #   "2SD Threshold" = "#3E5B82",
    #   "Custom" = "#b3cde0",
    #   "Expert" = "#F5C98E"
    # )) +
    scale_x_continuous(
      breaks = seq(floor(min(time_h)), ceiling(max(time_h)), by = 1),
      labels = function(x) sprintf("%02d", x %% 24),
      limits = c(x_start, max(time_h, na.rm = TRUE) + 4)
    ) +
    labs(
      # title = paste(d$id_tp, d$reason_category_revised, sep = " | "),
      title = paste(d$id_tp, sep = " | "),
      x = "Clock Time (h)",
      y = "Melatonin (pg/ml)"
    )

  if (!is.na(clicked_dlmo)) {
    p <- p +
      geom_vline(
        xintercept = clicked_dlmo,
        linewidth = 0.9,
        linetype = "solid",
        colour = clicked_col
      ) +
      annotate(
        "point",
        x = clicked_dlmo,
        y = y_top,
        shape = 18,
        size = 5,
        colour = clicked_col
      ) +
      annotate(
        "text",
        x = clicked_dlmo,
        y = y_top,
        label = paste0("Clicked: ", fmt_time(clicked_dlmo)),
        # vjust = -1,
        hjust = 1.5,
        size = 4,
        colour = clicked_col,
        fontface = "bold"
      )
  }

  p +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14)
    )
}
