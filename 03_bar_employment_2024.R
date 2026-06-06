# 03_bar_employment_2024.R — Dark editorial infographic
# Palette: Dark charcoal + coral | BG #1A1A2E | Accent #E63946 | Sec #A8DADC

suppressPackageStartupMessages({
  library(magick)
  library(ggplot2)
  library(tidyverse)
  library(scales)
})

BG     <- "#1A1A2E"
ACCENT <- "#E63946"
SEC    <- "#A8DADC"
WHITE  <- "#FFFFFF"
MUTED  <- "#99AABB"
PBG    <- "#252540"
SYMBOL <- "#6C2638"   # ACCENT blended into BG at 40%
W <- 1200L; H <- 2400L

draw_rect <- function(img, x1, y1, x2, y2, color) {
  w <- max(1L, as.integer(x2 - x1))
  h <- max(1L, as.integer(y2 - y1))
  image_composite(img, image_blank(w, h, color),
                  offset = paste0("+", as.integer(x1), "+", as.integer(y1)))
}

# ── Data ────────────────────────────────────────────────────────────────────
df <- tribble(
  ~country,   ~emp_rate,
  "Germany",  77.2,
  "Japan",    75.8,
  "USA",      73.7,
  "Brazil",   66.5,
  "Nigeria",  60.4
) |>
  mutate(
    country   = fct_reorder(country, emp_rate),
    bar_color = if_else(country == "Germany", ACCENT, SEC)
  )

world_avg <- 68

# ── Chart ────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(emp_rate, country)) +
  geom_col(aes(fill=bar_color), width=0.62) +
  geom_text(aes(label=paste0(emp_rate,"%")), hjust=-0.18, size=4.5,
            fontface="bold", color=WHITE) +
  geom_vline(xintercept=world_avg, linetype="dashed", color=MUTED, linewidth=0.7) +
  annotate("text", x=world_avg+0.5, y=0.55,
           label=paste0("World avg\n",world_avg,"%"),
           size=3.5, color=MUTED, hjust=0, vjust=0) +
  scale_fill_identity() +
  scale_x_continuous(labels=label_percent(scale=1), limits=c(0,90),
                     expand=expansion(mult=c(0,0.02))) +
  labs(x="Employment rate (%)", y=NULL,
       subtitle="Share of working-age population (15-64) in employment") +
  theme_minimal(base_size=13) +
  theme(
    plot.background    = element_rect(fill=BG,       color=NA),
    panel.background   = element_rect(fill="#141428", color=NA),
    panel.grid.major.x = element_line(color="#252545", linewidth=0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks         = element_blank(),
    axis.text          = element_text(color=MUTED, size=12),
    axis.title         = element_text(color=MUTED, size=12),
    plot.subtitle      = element_text(color=MUTED, size=11, margin=margin(b=10)),
    legend.position    = "none",
    plot.margin        = margin(20,30,20,30)
  )

tmp <- tempfile(fileext=".png")
ggsave(tmp, p, width=12, height=8, units="in", dpi=100, bg=BG)
cimg <- image_read(tmp) |> image_scale("1200x")
ch   <- as.integer(image_info(cimg)$height)

# ── Canvas ───────────────────────────────────────────────────────────────────
canvas <- image_blank(W, H, color=BG)

canvas <- draw_rect(canvas, 0, 0, W, 10, ACCENT)

canvas <- image_annotate(canvas, "EMPLOYMENT RATE RANKING  |  2024",
  location="+40+40", size=20, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas, "Germany Leads Employment",
  location="+40+80", size=76, color=WHITE, font="Helvetica-Bold")
canvas <- image_annotate(canvas, "While Nigeria Sits 13 Points",
  location="+40+177", size=76, color=WHITE, font="Helvetica-Bold")
canvas <- image_annotate(canvas, "Below the World Average",
  location="+40+274", size=76, color=ACCENT, font="Helvetica-Bold")
canvas <- image_annotate(canvas,
  "Employment rates across five major economies in 2024 reveal a 16.8 pp",
  location="+40+392", size=24, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas,
  "spread from Germany at the top to Nigeria at the bottom.",
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
  list(v="77.2%", l="Germany\nHighest Rate"),
  list(v="68%",   l="World Average\nEmployment"),
  list(v="60.4%", l="Nigeria\nLowest Rate"),
  list(v="16.8pp",l="Germany-Nigeria\nSpread")
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
  "Germany's 77.2% employment rate reflects strong vocational training,",
  "flexible labour markets, and industrial demand for skilled workers.",
  "Nigeria's 60.4% rate — 7.6 pp below the world average — reflects",
  "structural barriers including youth unemployment, informal sector",
  "dominance, and mismatches between education and available roles."
)
for (j in seq_along(ins)) {
  canvas <- image_annotate(canvas, ins[[j]],
    location=paste0("+64+",iy+28+(j-1L)*40L), size=25, color=WHITE, font="Helvetica")
}

dcy <- iy + 310L
canvas <- draw_rect(canvas, 40, dcy, W-40, dcy+2, PBG)
canvas <- image_annotate(canvas, "DISTANCE FROM WORLD AVERAGE  |  +/- pp",
  location=paste0("+40+",dcy+16), size=18, color=MUTED, font="Helvetica-Bold")
canvas <- image_annotate(canvas,
  "Germany +9.2    Japan +7.8    USA +5.7    Brazil -1.5    Nigeria -7.6",
  location=paste0("+40+",dcy+48), size=24, color=WHITE, font="Helvetica")
canvas <- image_annotate(canvas,
  "Percentage point difference from world average of 68%  |  2024 data",
  location=paste0("+40+",dcy+86), size=22, color=MUTED, font="Helvetica")

fy <- H - 100L
canvas <- draw_rect(canvas, 0, H-10L, W, H, ACCENT)
canvas <- image_annotate(canvas, "Source: Synthetic data for illustration purposes",
  location=paste0("+40+",fy), size=20, color=MUTED, font="Helvetica")
canvas <- image_annotate(canvas, "github.com/papageorgiou/posts",
  location=paste0("+",W-340L,"+",fy), size=20, color=MUTED, font="Helvetica")

image_write(canvas, path="03_bar_employment_2024.png")
message("Infographic 03 saved.")
