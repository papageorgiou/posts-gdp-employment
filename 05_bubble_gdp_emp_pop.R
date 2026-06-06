# 05_bubble_gdp_emp_pop.R — Dark editorial infographic
# Palette: Midnight + silver | BG #13131F | Accent #C9B1FF | Sec #7B7F9E

suppressPackageStartupMessages({
  library(magick)
  library(ggplot2)
  library(tidyverse)
  library(scales)
})

BG     <- "#13131F"
ACCENT <- "#C9B1FF"
SEC    <- "#7B7F9E"
WHITE  <- "#FFFFFF"
MUTED  <- "#99AABB"
PBG    <- "#1E1E30"
SYMBOL <- "#5C5279"   # ACCENT blended into BG at 40%
W <- 1200L; H <- 2400L

draw_rect <- function(img, x1, y1, x2, y2, color) {
  w <- max(1L, as.integer(x2 - x1))
  h <- max(1L, as.integer(y2 - y1))
  image_composite(img, image_blank(w, h, color),
                  offset = paste0("+", as.integer(x1), "+", as.integer(y1)))
}

# ── Data ────────────────────────────────────────────────────────────────────
df <- tribble(
  ~country,   ~gdp_per_capita, ~emp_rate, ~population_m, ~flag,
  "USA",      71200,           73.7,      335,           "\U0001F1FA\U0001F1F8",
  "Germany",  61200,           77.2,       84,           "\U0001F1E9\U0001F1EA",
  "Japan",    49200,           75.8,      123,           "\U0001F1EF\U0001F1F5",
  "Brazil",   11700,           66.5,      215,           "\U0001F1E7\U0001F1F7",
  "Nigeria",  3550,            60.4,      230,           "\U0001F1F3\U0001F1EC"
)

world_avg_emp <- 68
world_avg_gdp <- 13500

pal <- c(USA="#C9B1FF", Germany="#A08ECC", Japan="#8877B3", Brazil="#6B5F9E", Nigeria="#5A508A")

# ── Chart ────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(gdp_per_capita, emp_rate)) +
  annotate("rect", xmin = 0, xmax = world_avg_gdp, ymin = 50, ymax = world_avg_emp,
           fill = "#1E1E35", alpha = 0.8) +
  annotate("text", x = 1200, y = 51.5, hjust = 0,
           label = "Low GDP  |  Low employment",
           size = 3.2, color = SEC, fontface = "italic") +
  geom_vline(xintercept = world_avg_gdp, linetype = "dashed", color = MUTED, linewidth = 0.5) +
  geom_hline(yintercept = world_avg_emp,  linetype = "dashed", color = MUTED, linewidth = 0.5) +
  geom_point(aes(size = population_m, color = country), alpha = 0.35) +
  geom_text(aes(label = flag), size = 9, vjust = 0.5) +
  geom_text(aes(label = paste0(country, "\n", round(population_m), "M"), color = country),
            size = 3.5, fontface = "bold", vjust = -2.8, lineheight = 0.85) +
  scale_color_manual(values = pal) +
  scale_size_continuous(range = c(8, 38)) +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "k"),
                     expand = expansion(mult = c(0.02, 0.06))) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(50, 82)) +
  labs(x = "GDP per capita (USD, 2024)", y = "Employment rate (%)",
       subtitle = "Bubble size = population (millions)  |  2024 snapshot") +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = BG,        color = NA),
    panel.background = element_rect(fill = "#0E0E1A", color = NA),
    panel.grid.major = element_line(color = "#1C1C2E", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks       = element_blank(),
    axis.text        = element_text(color = MUTED, size = 11),
    axis.title       = element_text(color = MUTED, size = 12),
    plot.subtitle    = element_text(color = MUTED, size = 11, margin = margin(b = 10)),
    legend.position  = "none",
    plot.margin      = margin(20, 30, 20, 30)
  )

tmp <- tempfile(fileext = ".png")
ggsave(tmp, p, width = 12, height = 8, units = "in", dpi = 100, bg = BG)
cimg <- image_read(tmp) |> image_scale("1200x")
ch   <- as.integer(image_info(cimg)$height)

# ── Canvas ───────────────────────────────────────────────────────────────────
canvas <- image_blank(W, H, color = BG)

canvas <- draw_rect(canvas, 0, 0, W, 10, ACCENT)

canvas <- image_annotate(canvas, "GDP, EMPLOYMENT & POPULATION  |  2024",
  location = "+40+40", size = 20, color = MUTED, font = "Helvetica")
canvas <- image_annotate(canvas, "Nigeria's 230M People",
  location = "+40+80", size = 76, color = WHITE, font = "Helvetica-Bold")
canvas <- image_annotate(canvas, "Make Its Employment Gap",
  location = "+40+177", size = 76, color = WHITE, font = "Helvetica-Bold")
canvas <- image_annotate(canvas, "a Global Labour Challenge",
  location = "+40+274", size = 76, color = ACCENT, font = "Helvetica-Bold")
canvas <- image_annotate(canvas,
  "Bubble size encodes population: low employment in a 230M nation",
  location = "+40+392", size = 24, color = MUTED, font = "Helvetica")
canvas <- image_annotate(canvas,
  "means millions of people locked out of productive participation.",
  location = "+40+424", size = 24, color = MUTED, font = "Helvetica")

canvas <- image_annotate(canvas, "$",
  location = paste0("+", W - 200, "+55"), size = 140, color = SYMBOL, font = "Helvetica-Bold")

cy <- 470L
canvas <- image_composite(canvas, cimg, offset = paste0("+0+", cy))

pt  <- cy + ch + 60L
ph  <- 220L
pw  <- 265L
pxs <- as.integer(c(40, 325, 610, 895))
for (x in pxs) {
  canvas <- draw_rect(canvas, x, pt, x + pw, pt + ph, PBG)
  canvas <- draw_rect(canvas, x, pt, x + 6,  pt + ph, ACCENT)
}

stats <- list(
  list(v = "230M", l = "Nigeria\nPopulation 2024"),
  list(v = "$71k", l = "USA GDP\nper Capita 2024"),
  list(v = "17pp", l = "Employment\nRange Spread"),
  list(v = "445M", l = "Nigeria+Brazil\nCombined Pop.")
)
for (i in seq_along(stats)) {
  s  <- stats[[i]]; bx <- pxs[[i]]
  canvas <- image_annotate(canvas, s$v,
    location = paste0("+", bx + 18, "+", pt + 30), size = 54, color = ACCENT, font = "Helvetica-Bold")
  canvas <- image_annotate(canvas, s$l,
    location = paste0("+", bx + 18, "+", pt + 120), size = 20, color = MUTED, font = "Helvetica")
}

iy <- pt + ph + 80L
canvas <- draw_rect(canvas, 40, iy, 47, iy + 230, ACCENT)
canvas <- image_annotate(canvas, "KEY INSIGHT",
  location = paste0("+64+", iy), size = 18, color = ACCENT, font = "Helvetica-Bold")
ins <- c(
  "Nigeria's position — large population, low GDP, below-average",
  "employment — creates a compounding challenge: the jobs deficit",
  "affects more people in absolute terms than anywhere in the sample.",
  "Brazil shares a similar structural position but with higher GDP.",
  "Bubble size reveals the human scale behind each data point."
)
for (j in seq_along(ins)) {
  canvas <- image_annotate(canvas, ins[[j]],
    location = paste0("+64+", iy + 28 + (j - 1L) * 40L), size = 25, color = WHITE, font = "Helvetica")
}

dcy <- iy + 310L
canvas <- draw_rect(canvas, 40, dcy, W - 40, dcy + 2, PBG)
canvas <- image_annotate(canvas, "2024 SNAPSHOT  |  POPULATION & EMPLOYMENT",
  location = paste0("+40+", dcy + 16), size = 18, color = MUTED, font = "Helvetica-Bold")
canvas <- image_annotate(canvas,
  "USA 335M / 73.7%    Germany 84M / 77.2%    Japan 123M / 75.8%",
  location = paste0("+40+", dcy + 48), size = 24, color = WHITE, font = "Helvetica")
canvas <- image_annotate(canvas,
  "Brazil 215M / 66.5%    Nigeria 230M / 60.4%  |  Synthetic illustrative data",
  location = paste0("+40+", dcy + 86), size = 22, color = MUTED, font = "Helvetica")

fy <- H - 100L
canvas <- draw_rect(canvas, 0, H - 10L, W, H, ACCENT)
canvas <- image_annotate(canvas, "Source: Synthetic data for illustration purposes",
  location = paste0("+40+", fy), size = 20, color = MUTED, font = "Helvetica")
canvas <- image_annotate(canvas, "github.com/papageorgiou/posts",
  location = paste0("+", W - 340L, "+", fy), size = 20, color = MUTED, font = "Helvetica")

image_write(canvas, path = "05_bubble_gdp_emp_pop.png")
message("Infographic 05 saved.")
