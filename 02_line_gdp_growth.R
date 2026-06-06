# 02_line_gdp_growth.R — Dark editorial infographic
# Palette: Near-black + teal | BG #0D1F1E | Accent #2EC4B6 | Sec #4A9E97

suppressPackageStartupMessages({
  library(magick)
  library(ggplot2)
  library(tidyverse)
  library(scales)
})

BG     <- "#0D1F1E"
ACCENT <- "#2EC4B6"
SEC    <- "#4A9E97"
WHITE  <- "#FFFFFF"
MUTED  <- "#99AABB"
PBG    <- "#0F2E2D"
SYMBOL <- "#1A615B"   # ACCENT blended into BG at 40%
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
gdp_trend <- c(1800,1400,800,300,150)

pal <- c(USA="#2EC4B6",Germany="#68D5CE",Japan="#4A9E97",Brazil="#3A7A75",Nigeria="#256360")

df <- map_dfr(seq_along(countries), function(i) {
  tibble(
    country        = countries[i],
    year           = 2015:2024,
    gdp_per_capita = gdp_base[i] + gdp_trend[i]*(2015:2024-2015) + rnorm(10,0,gdp_base[i]*0.015)
  )
})
df_end <- filter(df, year == 2024)

# ── Chart ────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(year, gdp_per_capita, color=country, group=country)) +
  geom_line(linewidth=1.8, alpha=0.9) +
  geom_point(data=df_end, size=3.5) +
  geom_text(
    data=df_end,
    aes(label=paste0(country, "  ", label_dollar(scale=1e-3,suffix="k")(gdp_per_capita))),
    hjust=-0.1, size=3.6, fontface="bold", color="white", lineheight=0.9
  ) +
  annotate("segment",
    x=2024.1, xend=2024.1,
    y=df_end$gdp_per_capita[df_end$country=="Nigeria"],
    yend=df_end$gdp_per_capita[df_end$country=="USA"],
    color=MUTED, linewidth=0.6,
    arrow=arrow(ends="both", length=unit(4,"pt"), type="open")) +
  annotate("text", x=2024.35,
    y=mean(c(df_end$gdp_per_capita[df_end$country=="Nigeria"],
             df_end$gdp_per_capita[df_end$country=="USA"])),
    label="~$53k gap", size=3.2, color=MUTED, hjust=0) +
  scale_color_manual(values=pal) +
  scale_x_continuous(breaks=seq(2015,2024,3),
                     expand=expansion(mult=c(0.02,0.24))) +
  scale_y_continuous(labels=label_dollar(scale=1e-3,suffix="k"),
                     expand=expansion(mult=c(0.05,0.05))) +
  labs(x=NULL, y="GDP per capita (USD)") +
  theme_minimal(base_size=13) +
  theme(
    plot.background  = element_rect(fill=BG,       color=NA),
    panel.background = element_rect(fill="#0A1A19", color=NA),
    panel.grid.major = element_line(color="#143030", linewidth=0.3),
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

canvas <- draw_rect(canvas, 0, 0, W, 10, ACCENT)

canvas <- image_annotate(canvas, "GDP GROWTH SERIES  |  2015-2024",
  location="+40+40", size=20, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas, "The GDP Gap Between",
  location="+40+80", size=76, color=WHITE, font="Helvetica-Bold")
canvas <- image_annotate(canvas, "Rich and Poor Nations",
  location="+40+177", size=76, color=WHITE, font="Helvetica-Bold")
canvas <- image_annotate(canvas, "Widened Through the Decade",
  location="+40+274", size=76, color=ACCENT, font="Helvetica-Bold")
canvas <- image_annotate(canvas,
  "Five economies tracked over nine years reveal how absolute wealth",
  location="+40+392", size=24, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas,
  "gaps compound over time — USA vs Nigeria now separated by ~$53k.",
  location="+40+424", size=24, color=MUTED, font="Helvetica")

canvas <- image_annotate(canvas, "$",
  location=paste0("+",W-200,"+55"), size=140, color=SYMBOL, font="Helvetica-Bold")

cy <- 470L
canvas <- image_composite(canvas, cimg, offset=paste0("+0+",cy))

pt  <- cy + ch + 60L
ph  <- 220L
pw  <- 265L
pxs <- as.integer(c(40, 325, 610, 895))
for (x in pxs) {
  canvas <- draw_rect(canvas, x, pt, x+pw, pt+ph, PBG)
  canvas <- draw_rect(canvas, x, pt, x+6,  pt+ph, ACCENT)
}

stats <- list(
  list(v="$71k",  l="USA GDP\nper Capita 2024"),
  list(v="$61k",  l="Germany GDP\nper Capita 2024"),
  list(v="$3.6k", l="Nigeria GDP\nper Capita 2024"),
  list(v="$53k",  l="USA-Nigeria\nAbsolute Gap")
)
for (i in seq_along(stats)) {
  s <- stats[[i]]; bx <- pxs[[i]]
  canvas <- image_annotate(canvas, s$v,
    location=paste0("+",bx+18,"+",pt+30), size=54, color=ACCENT, font="Helvetica-Bold")
  canvas <- image_annotate(canvas, s$l,
    location=paste0("+",bx+18,"+",pt+120), size=20, color=MUTED, font="Helvetica")
}

iy <- pt + ph + 80L
canvas <- draw_rect(canvas, 40, iy, 47, iy+230, ACCENT)
canvas <- image_annotate(canvas, "KEY INSIGHT",
  location=paste0("+64+",iy), size=18, color=ACCENT, font="Helvetica-Bold")
ins <- c(
  "The USA added roughly $16k per person in GDP over nine years, while",
  "Nigeria added only ~$1.4k — the absolute gap compounds at scale.",
  "Germany and Japan diverge in growth pace despite similar starting points.",
  "Brazil shows consistent but modest gains, constrained by structural",
  "factors that slow conversion of growth into broad prosperity."
)
for (j in seq_along(ins)) {
  canvas <- image_annotate(canvas, ins[[j]],
    location=paste0("+64+",iy+28+(j-1L)*40L), size=25, color=WHITE, font="Helvetica")
}

dcy <- iy + 310L
canvas <- draw_rect(canvas, 40, dcy, W-40, dcy+2, PBG)
canvas <- image_annotate(canvas, "GDP GROWTH  |  2015 TO 2024",
  location=paste0("+40+",dcy+16), size=18, color=MUTED, font="Helvetica-Bold")
canvas <- image_annotate(canvas,
  "USA +$16.2k    Germany +$12.6k    Japan +$7.2k    Brazil +$2.7k    Nigeria +$1.4k",
  location=paste0("+40+",dcy+48), size=24, color=WHITE, font="Helvetica")
canvas <- image_annotate(canvas,
  "Absolute gains in USD per capita over nine years  |  Synthetic data for illustration",
  location=paste0("+40+",dcy+86), size=22, color=MUTED, font="Helvetica")

fy <- H - 100L
canvas <- draw_rect(canvas, 0, H-10L, W, H, ACCENT)
canvas <- image_annotate(canvas, "Source: Synthetic data for illustration purposes",
  location=paste0("+40+",fy), size=20, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas, "github.com/papageorgiou/posts",
  location=paste0("+",W-340L,"+",fy), size=20, color=MUTED, font="Helvetica")

image_write(canvas, path="02_line_gdp_growth.png")
message("Infographic 02 saved.")
