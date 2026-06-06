# 04_slope_employment_change.R — Dark editorial infographic
# Palette: Dark forest + amber | BG #0F1F14 | Accent #F4A261 | Sec #52B788

suppressPackageStartupMessages({
  library(magick)
  library(ggplot2)
  library(tidyverse)
  library(scales)
})

BG     <- "#0F1F14"
ACCENT <- "#F4A261"
SEC    <- "#52B788"
WHITE  <- "#FFFFFF"
MUTED  <- "#99AABB"
PBG    <- "#162B1C"
SYMBOL <- "#6B5333"   # ACCENT blended into BG at 40%
W <- 1200L; H <- 2400L

draw_rect <- function(img, x1, y1, x2, y2, color) {
  w <- max(1L, as.integer(x2 - x1))
  h <- max(1L, as.integer(y2 - y1))
  image_composite(img, image_blank(w, h, color),
                  offset = paste0("+", as.integer(x1), "+", as.integer(y1)))
}

# ── Data ────────────────────────────────────────────────────────────────────
df <- tribble(
  ~country,   ~emp_2015, ~emp_2024,
  "USA",      70.8,      73.7,
  "Germany",  73.0,      77.2,
  "Japan",    73.3,      75.8,
  "Brazil",   61.5,      66.5,
  "Nigeria",  55.0,      60.4
) |>
  mutate(
    country = fct_reorder(country, emp_2024 - emp_2015),
    delta   = emp_2024 - emp_2015
  )

pal <- c(USA="#F4A261",Germany="#F4A261",Japan="#F4A261",Brazil="#52B788",Nigeria="#52B788")

# ── Chart ────────────────────────────────────────────────────────────────────
p <- ggplot(df) +
  geom_segment(aes(x=emp_2015, xend=emp_2024, y=country, yend=country, color=country),
               linewidth=1.8, alpha=0.5) +
  geom_point(aes(x=emp_2015, y=country, color=country), shape=1, size=5, stroke=1.8) +
  geom_point(aes(x=emp_2024, y=country, color=country), shape=19, size=5) +
  geom_text(aes(x=emp_2015, y=country, label=paste0(emp_2015,"%")),
            hjust=1.4, size=3.5, color=MUTED) +
  geom_text(aes(x=emp_2024, y=country,
                label=paste0(emp_2024,"%  (+",round(delta,1),"pp)")),
            hjust=-0.12, size=3.7, fontface="bold", color=WHITE) +
  scale_color_manual(values=pal) +
  scale_x_continuous(labels=label_percent(scale=1), limits=c(51,86),
                     expand=expansion(mult=c(0.02,0.12))) +
  labs(x="Employment rate (%)", y=NULL,
       subtitle="Open circle = 2015  |  Filled circle = 2024  |  Gain in pp shown") +
  theme_minimal(base_size=13) +
  theme(
    plot.background  = element_rect(fill=BG,       color=NA),
    panel.background = element_rect(fill="#0A1810", color=NA),
    panel.grid.major = element_line(color="#1A3020", linewidth=0.3),
    panel.grid.minor = element_blank(),
    axis.ticks       = element_blank(),
    axis.text        = element_text(color=MUTED, size=12),
    axis.title       = element_text(color=MUTED, size=12),
    plot.subtitle    = element_text(color=MUTED, size=11, margin=margin(b=10)),
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

canvas <- image_annotate(canvas, "EMPLOYMENT CHANGE  |  2015 VS 2024",
  location="+40+40", size=20, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas, "Emerging Economies Made",
  location="+40+80", size=76, color=WHITE, font="Helvetica-Bold")
canvas <- image_annotate(canvas, "the Biggest Employment",
  location="+40+177", size=76, color=WHITE, font="Helvetica-Bold")
canvas <- image_annotate(canvas, "Gains Over the Decade",
  location="+40+274", size=76, color=ACCENT, font="Helvetica-Bold")
canvas <- image_annotate(canvas,
  "Nigeria and Brazil outpaced advanced economies in employment growth",
  location="+40+392", size=24, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas,
  "2015-2024, though they still trail the absolute employment-rate leaders.",
  location="+40+424", size=24, color=MUTED, font="Helvetica")

canvas <- image_annotate(canvas, "%",
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
  list(v="+5.4pp", l="Nigeria\nBiggest Gain"),
  list(v="+5.0pp", l="Brazil\n2nd Biggest Gain"),
  list(v="+4.2pp", l="Germany\nStrong Growth"),
  list(v="+2.5pp", l="Japan\nSteady Progress")
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
  "Nigeria and Brazil recorded the largest employment gains 2015-2024,",
  "driven by demographic growth, expanding informal sectors, and",
  "post-recession recoveries. Advanced economies show smaller percentage",
  "gains because they were already near full employment, making each",
  "additional percentage point structurally harder to achieve."
)
for (j in seq_along(ins)) {
  canvas <- image_annotate(canvas, ins[[j]],
    location=paste0("+64+",iy+28+(j-1L)*40L), size=25, color=WHITE, font="Helvetica")
}

dcy <- iy + 310L
canvas <- draw_rect(canvas, 40, dcy, W-40, dcy+2, PBG)
canvas <- image_annotate(canvas, "EMPLOYMENT RATE  |  2015 vs 2024",
  location=paste0("+40+",dcy+16), size=18, color=MUTED, font="Helvetica-Bold")
canvas <- image_annotate(canvas,
  "Nigeria 55.0% -> 60.4%    Brazil 61.5% -> 66.5%    Germany 73.0% -> 77.2%",
  location=paste0("+40+",dcy+48), size=24, color=WHITE, font="Helvetica")
canvas <- image_annotate(canvas,
  "USA 70.8% -> 73.7%    Japan 73.3% -> 75.8%  |  All five nations improved",
  location=paste0("+40+",dcy+86), size=22, color=MUTED, font="Helvetica")

fy <- H - 100L
canvas <- draw_rect(canvas, 0, H-10L, W, H, ACCENT)
canvas <- image_annotate(canvas, "Source: Synthetic data for illustration purposes",
  location=paste0("+40+",fy), size=20, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas, "github.com/papageorgiou/posts",
  location=paste0("+",W-340L,"+",fy), size=20, color=MUTED, font="Helvetica")

image_write(canvas, path="04_slope_employment_change.png")
message("Infographic 04 saved.")
