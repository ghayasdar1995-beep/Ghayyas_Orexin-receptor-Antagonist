# Fig6A_7AB_combined.R
# Run: Rscript Fig6A_7AB_combined.R [output_directory]
# Base R only. Writes one combined figure as a vector PDF and a 300 dpi PNG.
# The three panels show individual contrasts. No weights or pooled diamonds.
# Effect signs, confidence limits and sample sizes are copied from the inputs.
# Some locomotor signs require source checking before directional interpretation.

WIDTH <- 21             # inches
FONT_PT <- 17           # base text size in points
DPI <- 300
SQUARE_SIZE <- 1.6
ZERO_LINE_WIDTH <- 3

d <- read.delim(
  text = "panel|id|substance|study|experiment|antagonist|receptor|dose|route|n_T|n_C|g|lo|hi
6A|C197|Alcohol|Anderson et al. (081)|Drinking in the dark|SB-334867|OX1R|3–30 mg/kg|i.p.|30|10|-1.1010146896|-1.8562793973|-0.345749982
6A|C198|Alcohol|Anderson et al. (081)|Drinking in the dark|LSN2424100|OX2R|15–60 mg/kg|i.p.|29|10|-0.5374382619|-1.2660360248|0.1911595011
6A|C199|Alcohol|Anderson et al. (081)|Drinking in the dark|Almorexant|DORA|25–100 mg/kg|p.o.|30|10|-0.8947070641|-1.6367672897|-0.1526468385
6A|C202|Alcohol|Carvajal et al. (082)|Drinking in the dark|SB-334867|OX1R|1–3 µg|i.c.v.|25|13|-0.7447949034|-1.4355997243|-0.0539900825
7A|C200|Alcohol|Carvajal et al. (082)|Locomotor activity|SB-334867|OX1R|10 µg|i.c.v.|8|8|0.0387591707|-0.9413328392|1.0188511806
7A|C201|Alcohol|Olney et al. (019)|Open-field activity|SB-334867|OX1R|10 mg/kg|i.p.|10|10|0.698084178|-0.2047570919|1.600925448
7A|C203|Alcohol|Brown et al. (027)|Locomotor activity|TCS-OX2-29|OX2R|100–300 µg|i.c.v.|12|5|0.0691712015|-0.9743775243|1.1127199272
7A|C204|Alcohol|Brown et al. (027)|Locomotor activity|TCS-OX2-29|OX2R|100 µg/side|intra-NAc core|5|4|-0.6920306379|-2.0451489878|0.661087712
7A|C205|Alcohol|Brown et al. (027)|Locomotor activity|TCS-OX2-29|OX2R|100 µg/side|intra-NAc shell|7|9|-0.6159063989|-1.6264430358|0.394630238
7A|C206|Methamphetamine|Zamanirad et al. (089)|Locomotor activity|TCS-OX2-29|OX2R|1–30 nmol|intra-VTA|35|9|-0.0196292946|-0.752173672|0.7129150829
7A|C207|Morphine|Alizamini et al. (094)|Locomotor activity|SB-334867|OX1R|1–30 nM/0.5 µL|intra-NAc|23|6|-0.0440664967|-0.9426327834|0.8544997899
7B|C196|Cocaine|Hutcheson et al. (16)|Conditioned reinforcement (E2)|SB-334867|OX1R|10–30 mg/kg|i.p.|12|5|-0.4248138727|-1.4778304735|0.6282027281
7B|C208|Cocaine|Zhou et al. (014)|Cue reinstatement (Exp 2)|SB-334867|OX1R|20 mg/kg|i.p.|21|22|-0.2989164072|-0.900200703|0.3023678886
7B|C209|Cocaine|Smith & Aston-Jones (097)|Seeking after 2 weeks (Exp 2)|SB-334867|OX1R|10–30 mg/kg|i.p.|37|12|-0.6054444642|-1.2675091362|0.0566202079
7B|C210|Cocaine|Smith & Aston-Jones (097)|Context reinstatement (Exp 3)|SB-334867|OX1R|10–30 mg/kg|i.p.|55|18|-0.5183554562|-1.0571869856|0.0204760733
7B|C211|Alcohol|Jupp et al. (064)|Immediate and delayed cue tests|SB-334867|OX1R|15 mg/kg|i.p.|19|20|-0.6233480648|-1.2663151637|0.019619034
7B|C212|Alcohol|Flores-Ramirez et al. (079)|Stress reinstatement|TCS1102|DORA|15 µg/0.5 µL/side|intra-IL|24|24|-0.0883601166|-0.6544394078|0.4777191745",
  sep = "|", quote = "", stringsAsFactors = FALSE, check.names = FALSE
)

panel_titles <- c(
  "6A" = "6A. Binge-like ethanol intake (g/kg)",
  "7A" = "7A. Locomotor activity controls",
  "7B" = "7B. Inactive responding controls"
)

limits <- c(-2.5, 2)
ticks <- seq(-2.5, 2, by = 0.5)

stopifnot(
  !anyDuplicated(d$id),
  all(d$panel %in% names(panel_titles)),
  identical(as.integer(table(factor(d$panel, levels = names(panel_titles)))),
            c(4L, 7L, 6L)),
  all(is.finite(d$g) & is.finite(d$lo) & is.finite(d$hi)),
  all(d$lo <= d$g & d$g <= d$hi),
  all(d$lo >= limits[1] & d$hi <= limits[2]),
  all(d$n_T > 0 & d$n_C > 0)
)

spacing <- FONT_PT / 17
rows <- list()
y <- 1.45 * spacing

add_row <- function(kind, label = "", index = NA_integer_) {
  rows[[length(rows) + 1L]] <<- list(
    kind = kind, label = label, index = index, y = y
  )
  y <<- y + spacing * switch(kind, panel = 0.44,
                             substance = 0.35, study = 0.37)
}

for (panel in names(panel_titles)) {
  add_row("panel", panel_titles[[panel]])
  ix <- which(d$panel == panel)
  for (substance in unique(d$substance[ix])) {
    add_row("substance", substance)
    for (i in ix[d$substance[ix] == substance]) {
      add_row("study", index = i)
    }
  }
  y <- y + 0.12 * spacing
}

axis_y <- y + 0.05 * spacing
HEIGHT <- axis_y + 1.85 * spacing

cols <- WIDTH * c(0.32, 3.44, 7.02, 9.50, 11.47, 13.03, 18.25) / 21
forest_left <- WIDTH * 14.05 / 21
forest_right <- cols[7] - 0.30

stopifnot(forest_right > forest_left, WIDTH > cols[7])

map_x <- function(g) {
  forest_left + (g - limits[1]) / diff(limits) *
    (forest_right - forest_left)
}

fmt <- function(x, digits = 2L) {
  sub("-", "\u2212", sprintf(paste0("%.", digits, "f"), x), fixed = TRUE)
}

draw <- function() {
  op <- par(mar = rep(0, 4), xaxs = "i", yaxs = "i",
            family = "sans", col = "black", bg = "white")
  on.exit(par(op), add = TRUE)
  plot.new()
  plot.window(xlim = c(0, WIDTH), ylim = c(HEIGHT, 0))
  
  # Fit long labels to their own columns without shrinking the whole figure.
  txt <- function(x, y, label, scale = 1, bold = FALSE,
                  align = 0, room = Inf) {
    cex <- FONT_PT / par("ps") * scale
    face <- if (bold) 2 else 1
    if (is.finite(room)) {
      w <- strwidth(label, units = "user", cex = cex, font = face)
      if (is.finite(w) && w > room) cex <- cex * 0.98 * room / w
    }
    text(x, y, label, adj = c(align, 0.5),
         cex = cex, font = face, col = "black")
  }
  
  headings <- c("Study", "Experiment", "Antagonist (receptor)",
                "Dose", "Route/site", "n (T/C)", "Hedges' g [95% CI]")
  
  room <- c(diff(cols[1:6]) - 0.15,
            forest_left - cols[6] - 0.14,
            WIDTH - cols[7] - 0.24)
  stopifnot(all(room > 0))
  
  # Draw bands first, then the thick, continuous black line at zero.
  for (r in rows) {
    if (r$kind == "study") next
    half_h <- spacing * (if (r$kind == "panel") 0.17 else 0.13)
    rect(0.22, r$y - half_h, WIDTH - 0.22, r$y + half_h,
         col = if (r$kind == "panel") "#DCE7F0" else "#EFF3F7",
         border = NA)
  }
  
  segments(map_x(0), 1.22 * spacing, map_x(0), axis_y,
           lty = 1, lwd = ZERO_LINE_WIDTH, col = "black")
  
  txt(cols[1], 0.39 * spacing,
      "Binge-like ethanol intake and behavioural specificity controls",
      scale = 1.45, bold = TRUE, room = WIDTH - 0.65)
  
  for (j in seq_along(headings)) {
    txt(cols[j], 0.99 * spacing, headings[j], scale = 0.96,
        bold = TRUE, room = room[j])
  }
  
  for (r in rows) {
    if (r$kind != "study") {
      txt(if (r$kind == "panel") cols[1] else cols[1] + 0.14,
          r$y, r$label,
          scale = if (r$kind == "panel") 1.10 else 1.00,
          bold = TRUE, room = WIDTH - 0.80)
      next
    }
    
    z <- d[r$index, ]
    study <- paste0(z$study, if (z$id == "C202") " \u2020" else "")
    vals <- c(
      study, z$experiment, paste0(z$antagonist, " (", z$receptor, ")"),
      z$dose, z$route, paste(z$n_T, z$n_C, sep = "/"),
      paste0(fmt(z$g), " [", fmt(z$lo), ", ", fmt(z$hi), "]")
    )
    for (j in seq_along(vals)) {
      txt(cols[j], r$y, vals[j], room = room[j])
    }
    
    segments(map_x(z$lo), r$y, map_x(z$hi), r$y,
             lwd = 1.8, col = "black")
    points(map_x(z$g), r$y, pch = 15, cex = SQUARE_SIZE, col = "#25843D")
  }
  
  # Separate vertical space for tick labels, axis title and notes.
  segments(forest_left, axis_y, forest_right, axis_y, lwd = 1.8)
  tick_room <- (forest_right - forest_left) / (length(ticks) - 1L) - 0.06
  for (tick in ticks) {
    x <- map_x(tick)
    segments(x, axis_y, x, axis_y + 0.08 * spacing, lwd = 1.3)
    txt(x, axis_y + 0.24 * spacing, fmt(tick, 1L),
        scale = 0.90, align = 0.5, room = tick_room)
  }
  
  txt(mean(c(forest_left, forest_right)), axis_y + 0.56 * spacing,
      "Standardized mean difference (Hedges' g)",
      scale = 0.98, bold = TRUE, align = 0.5,
      room = forest_right - forest_left + 0.65)
  
  notes <- c(
    "Squares show individual contrasts; lines show 95% confidence intervals. T/C = treatment/control. No estimates were pooled.",
    "\u2020 Carvajal binge intake: sample sizes were inferred in the source extraction.",
    "Locomotor signs are reproduced as supplied; confirm their orientation against the original reports before interpreting direction.",
    "Control results with confidence intervals crossing zero do not establish behavioural specificity."
  )
  
  for (i in seq_along(notes)) {
    txt(cols[1], axis_y + (0.86 + 0.25 * (i - 1L)) * spacing,
        notes[i], scale = 0.84, room = WIDTH - 0.65)
  }
  
  invisible(NULL)
}

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1L] else "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stem <- file.path(out, "Fig6A_7AB_combined")

if (capabilities("cairo")) {
  cairo_pdf(paste0(stem, ".pdf"), width = WIDTH, height = HEIGHT)
} else {
  pdf(paste0(stem, ".pdf"), width = WIDTH, height = HEIGHT,
      useDingbats = FALSE)
}
tryCatch(draw(), finally = dev.off())

png_args <- list(
  filename = paste0(stem, ".png"),
  width = WIDTH, height = HEIGHT, units = "in", res = DPI, bg = "white"
)
if (capabilities("cairo")) png_args$type <- "cairo"
do.call(png, png_args)
tryCatch(draw(), finally = dev.off())