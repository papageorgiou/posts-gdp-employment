# 01_scatter_gdp_employment.R — Dark editorial infographic
# Palette: Deep navy + gold | BG #0B1929 | Accent #F5A623 | Sec #68ACE5

suppressPackageStartupMessages({
  library(magick)
  library(ggplot2)
  library(tidyverse)
  library(scales)
})

BG     <- "#0B1929"
ACCENT <- "#F5A623"
SEC    <- "#68ACE5"
WHITE  <- "#FFFFFF"
MUTED  <- "#99AABB"
PBG    <- "#112240"
SYMBOL <- "#695127"   # ACCENT blended into BG at 40%
W <- 1200L; H <- 2400L

draw_rect <- function(img, x1, y1, x2, y2, color) {
  w <- max(1L, as.integer(x2 - x1))
  h <- max(1L, as.integer(y2 - y1))
  image_composite(img, image_blank(w, h, color),
                  offset = paste0("+", as.integer(x1), "+", as.integer(y1)))
}

# ── Data ────────────────────────────────────────────────────────────────────
set.seed(42)
countries <- c("USA","Germany","Japan","Brazil","Nigeria")
gdp_base  <- c(55000,48000,42000,9000,2200)
emp_base  <- c(71,75,74,62,55)
gdp_trend <- c(1800,1400,800,300,150)
emp_trend <- c(0.3,0.4,0.2,0.5,0.6)

pal <- c(USA="#68ACE5",Germany="#F5A623",Japan="#88CCE8",Brazil="#4A9FD4",Nigeria="#2E7CB8")

df <- map_dfr(seq_along(countries), function(i) {
  tibble(
    country        = countries[i],
    year           = 2015:2024,
    gdp_per_capita = gdp_base[i] + gdp_trend[i]*(2015:2024-2015) + rnorm(10,0,gdp_base[i]*0.015),
    emp_rate       = emp_base[i] + emp_trend[i]*(2015:2024-2015) + rnorm(10,0,0.6)
  )
})
df_end <- filter(df, year == 2024)

# ── Chart ────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(gdp_per_capita, emp_rate, color = country)) +
  geom_path(aes(group = country), linewidth = 0.7, alpha = 0.4) +
  geom_point(aes(size  = ifelse(year==2024, 3.5, 1.8),
                 alpha = ifelse(year==2024, 1.0, 0.40))) +
  geom_text(data = df_end, aes(label = country),
            hjust = -0.2, vjust = 0.4, size = 4.2, fontface = "bold", color = "white") +
  geom_hline(yintercept = 68, linetype = "dashed", color = MUTED, linewidth = 0.5) +
  annotate("text", x = 6000, y = 68.8, label = "World avg ~68%",
           size = 3.5, color = MUTED, hjust = 0) +
  scale_color_manual(values = pal) +
  scale_size_identity() +
  scale_alpha_identity() +
  scale_x_continuous(labels = label_dollar(scale=1e-3, suffix="k"),
                     expand = expansion(mult=c(0.02,0.18))) +
  scale_y_continuous(labels = label_percent(scale=1), limits = c(50,82)) +
  labs(x = "GDP per capita (USD)", y = "Employment rate (%)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill=BG,       color=NA),
    panel.background = element_rect(fill="#0F2035", color=NA),
    panel.grid.major = element_line(color="#1A3050", linewidth=0.3),
    panel.grid.minor = element_blank(),
    axis.ticks       = element_blank(),
    axis.text        = element_text(color=MUTED, size=11),
    axis.title       = element_text(color=MUTED, size=12),
    legend.position  = "none",
    plot.margin      = margin(20,30,20,30)
  )

tmp <- tempfile(fileext=".png")
ggsave(tmp, p, width=12, height=8, units="in", dpi=100, bg=BG)
cimg <- image_read(tmp) |> image_scale("1200x")
ch   <- as.integer(image_info(cimg)$height)

# ── Canvas ───────────────────────────────────────────────────────────────────
canvas <- image_blank(W, H, color=BG)

# Top accent bar
canvas <- draw_rect(canvas, 0, 0, W, 10, ACCENT)

# Header
canvas <- image_annotate(canvas, "GDP & EMPLOYMENT SERIES  |  2015-2024",
  location="+40+40", size=20, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas, "Wealthier Economies",
  location="+40+80", size=76, color=WHITE, font="Helvetica-Bold")
canvas <- image_annotate(canvas, "Sustain Higher Employment",
  location="+40+177", size=76, color=WHITE, font="Helvetica-Bold")
canvas <- image_annotate(canvas, "But Gaps Are Narrowing",
  location="+40+274", size=76, color=ACCENT, font="Helvetica-Bold")
canvas <- image_annotate(canvas,
  "Tracking five major economies 2015-2024: wealthy nations cluster above",
  location="+40+392", size=24, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas,
  "70% employment as emerging markets accelerate to close the gap.",
  location="+40+424", size=24, color=MUTED, font="Helvetica")

# Thematic symbol at 40% opacity (blended color)
canvas <- image_annotate(canvas, "$",
  location=paste0("+",W-200,"+55"), size=140, color=SYMBOL, font="Helvetica-Bold")

# Composite chart
cy <- 470L
canvas <- image_composite(canvas, cimg, offset=paste0("+0+",cy))

# Stat panels (4 panels)
pt  <- cy + ch + 60L
ph  <- 220L
pw  <- 265L
pxs <- as.integer(c(40, 325, 610, 895))
for (x in pxs) {
  canvas <- draw_rect(canvas, x,   pt, x+pw, pt+ph, PBG)
  canvas <- draw_rect(canvas, x,   pt, x+6,  pt+ph, ACCENT)
}

stats <- list(
  list(v="77.2%", l="Germany\nTop Employment"),
  list(v="$71k",  l="USA GDP\nper Capita 2024"),
  list(v="60.4%", l="Nigeria\nEmployment 2024"),
  list(v="$53k",  l="USA-Nigeria\nGDP Gap")
)
for (i in seq_along(stats)) {
  s <- stats[[i]]; bx <- pxs[[i]]
  canvas <- image_annotate(canvas, s$v,
    location=paste0("+",bx+18,"+",pt+30), size=54, color=ACCENT, font="Helvetica-Bold")
  canvas <- image_annotate(canvas, s$l,
    location=paste0("+",bx+18,"+",pt+120), size=20, color=MUTED, font="Helvetica")
}

# Key insight
iy <- pt + ph + 80L
canvas <- draw_rect(canvas, 40, iy, 47, iy+230, ACCENT)
canvas <- image_annotate(canvas, "KEY INSIGHT",
  location=paste0("+64+",iy), size=18, color=ACCENT, font="Helvetica-Bold")
ins <- c(
  "High-income economies (USA, Germany, Japan) consistently maintain",
  "employment rates above 70%, underpinned by stronger institutions and",
  "capital-intensive industries. Emerging markets like Brazil and Nigeria",
  "are accelerating gains but have not yet closed the structural gap.",
  "The correlation between GDP and employment is real but not deterministic."
)
for (j in seq_along(ins)) {
  canvas <- image_annotate(canvas, ins[[j]],
    location=paste0("+64+",iy+28+(j-1L)*40L), size=25, color=WHITE, font="Helvetica")
}

# Data context strip
dcy <- iy + 310L
canvas <- draw_rect(canvas, 40, dcy, W-40, dcy+2, PBG)
canvas <- image_annotate(canvas, "2024 SNAPSHOT  |  EMPLOYMENT RATES",
  location=paste0("+40+",dcy+16), size=18, color=MUTED, font="Helvetica-Bold")
canvas <- image_annotate(canvas,
  "Germany 77.2%    Japan 75.8%    USA 73.7%    Brazil 66.5%    Nigeria 60.4%",
  location=paste0("+40+",dcy+48), size=24, color=WHITE, font="Helvetica")
canvas <- image_annotate(canvas,
  "World avg: 68%  |  High-income avg: ~75%  |  Emerging market avg: ~63%",
  location=paste0("+40+",dcy+86), size=22, color=MUTED, font="Helvetica")

# Footer
fy <- H - 100L
canvas <- draw_rect(canvas, 0, H-10L, W, H, ACCENT)
canvas <- image_annotate(canvas, "Source: Synthetic data for illustration purposes",
  location=paste0("+40+",fy), size=20, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas, "github.com/papageorgiou/posts",
  location=paste0("+",W-340L,"+",fy), size=20, color=MUTED, font="Helvetica")

image_write(canvas, path="01_scatter_gdp_employment.png")
message("Infographic 01 saved.")
