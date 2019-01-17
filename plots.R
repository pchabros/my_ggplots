# biblioteki -----------------------------

library(tidyverse)
library(lubridate)
library(wrapr)
library(grid)

# settings -------------------------------

extrafont::loadfonts(device = 'win')

roz_cz <- 12
roz_cz_txt <- roz_cz / 3.597
leg_sq <- .8

theme_set(
  theme_minimal(base_family = 'Calibri') +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = roz_cz),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )
)

colors <- c(
  rgb(66, 85, 136, maxColorValue = 255),
  rgb(171, 177, 203, maxColorValue = 255),
  rgb(0, 112, 192, maxColorValue = 255),
  rgb(0, 160, 157, maxColorValue = 255),
  rgb(102, 81, 161, maxColorValue = 255),
  rgb(137, 173, 209, maxColorValue = 255),
  rgb(162, 148, 201, maxColorValue = 255),
  rgb(40, 50, 90, maxColorValue = 255),
  rgb(82, 104, 165, maxColorValue = 255)
)

fmt <- function(x, n = 0) {
  x %>%
    round(n) %>%
    format(big.mark = " ", decimal.mark = ',', trim = TRUE)
}
  
# read-in the data -----------------------

data_list <- list()

for (i in dir('data/')) {
  data_list[[str_remove(i, '\\..*$')]] <- read_csv2(paste0('data/', i), locale = locale(encoding = 'windows-1250'))
}

# W 2.27. Wysokość dotacji celowej w latach 2013–2017: OSF ----

data_list$osf_2_27 %>%
  gather(typ, n, 2:4) %>%
  ggplot(aes(
    x = rok,
    y = n,
    fill = typ,
    label = n %>% fmt()
  )) +
  geom_col(col = 'white') +
  geom_text(
    position = position_stack(.5),
    family = 'Calibri',
    size = roz_cz_txt,
    color = 'white'
  ) +
  geom_label(aes(
    y = razem + max(razem) * .06,
    label = razem %>% fmt()),
    family = "Calibri",
    size = roz_cz_txt,
    fill = "white",
    color = podstawowy,
    label.padding = unit(0.1, "lines"),
    show.legend = FALSE
  ) +
  geom_label(aes(
    x = 2013.435,
    y = 455,
    label = '    '
  ),
  size = 2.5,
  fill = "white",
  color = podstawowy,
  label.padding = unit(0.1, "lines")
  ) +
  geom_text(aes(
    x = 2013.57,
    y = 455,
    label = 'ogółem'
  ),
  family = 'Calibri',
  size = roz_cz_txt,
  hjust = 0,
  check_overlap = TRUE
  ) +
  guides(fill = guide_legend(
    keyheight = .8,
    keywidth = .8
  )) +
  scale_fill_manual(values = paleta_ald[c(2, 1, 6)]) +
  theme(
    axis.text.y = element_blank(),
    legend.spacing.x = unit(1, 'mm'),
    legend.position = c(.5, 1.08),
    plot.margin = margin(40, 5, 5, 5)
  )

ggsave(
  filename = 'wykresy/w2_27_v1.png',
  units = 'cm',
  width = 13,
  height = 8.7,
  bg = 'transparent',
  dpi = 400
)

# W 3.4. Liczba wniosków złożonych i projektów finansowanych w konkursach NCN w 2017 roku w podziale na województwa ----

data_list$ncn_3_4 %>%
  filter(rok == 2017) %>% 
  select(-1) %>%
  gather(woj, n, -typ) %>%
  spread(typ, n) %>%
  rename_at(2:3, ~c('finansowane projekty', 'złożone wnioski')) %>%
  mutate(wsp = `finansowane projekty` / `złożone wnioski`) %>%
  gather(typ, n, c(`finansowane projekty`, `złożone wnioski`)) %>%
  mutate(
    wsp_pos = wsp * max(n) + max(n) * 1.2,
    wsp_lab = wsp_pos * 1.07,         
    txt_pos = n + max(n) * .008
  ) %>%
  ggplot(
    data = .,
    aes(
      x = woj %>% fct_rev(),
      y = n,
      fill = typ,
      label = n %>% fmt()
  )) +
  geom_segment(aes(
      xend = woj %>% fct_rev(),
      y = 0,
      yend = wsp_pos
    ),
    linetype = 'dotted',
    col = colors[4],
    alpha = .7
  ) +
  geom_col(
    position = position_dodge(.9),
    col = 'white'
  ) +
  geom_text(
    aes(y = n + 10),
    position = position_dodge(.9),
    family = 'Calibri',
    size = roz_cz_txt - .4,
    hjust = 0
  ) +
  geom_point(aes(
      y = wsp_pos,
      shape = 'współczynnik sukcesu'
    ),
    fill = colors[4],
    size = 6,
    stroke = .1,
    color = 'white'
  ) +
  geom_text(aes(
      y = wsp_pos,
      label = (wsp * 100) %>% round()
    ),
    family = 'Calibri',
    color = 'white',
    size = roz_cz_txt,
    show.legend = FALSE
  ) +
  geom_text(aes(
      y = wsp_pos,
      label = '%'
    ),
    family = 'Calibri',
    nudge_y = 170,
    color = paleta_ald[2],
    size = roz_cz_txt,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = colors[1:2]) +
  scale_shape_manual(values = 21) +
  guides(fill = guide_legend(
    keyheight = leg_sq,
    keywidth = leg_sq,
    reverse = TRUE,
    order = -1)
  ) +
  theme(
    axis.text.x = element_blank(),
    plot.margin = margin(t = 60),
    legend.position = c(.412, 1.09),
    legend.direction = 'vertical',
    legend.box = 'horizontal',
    legend.spacing.x = unit(1, 'mm'),
    legend.box.just = 'left',
    legend.margin = margin(0, 0, 0, 0)
  )

ggsave(
  filename = 'wykresy/w3_4_v1.png',
  units = 'cm',
  width = 13,
  height = 12,
  bg = 'transparent',
  dpi = 600
)

# W 3.7. Liczba wniosków złożonych w konkursach NCN w latach 2013–2017 w podziale na typy wnioskodawców ----

data_list$plot3 %>%
  mutate(total = rowSums(.[2:6])) %>%
  gather(type, n, -c(year, total)) %>%
  mutate(
    type = type %>% reorder(n),
    year_pos = -max(n) * .1
  ) %.>%
  ggplot(
    data = .,
    aes(
      x = year,
      y = n)
  ) +
  geom_line(data = tibble(
      x = c(2013:2017 - .4, 2013:2017 + .4),
      y = max(.$n) * 1.02,
      gr = rep(1:5, 2)
    ),
    aes(
      x = x,
      y = y,
      group = gr
    ),
    color = colors[1]
  ) +
  geom_curve(data = tibble(
      x = c(2013:2017 + .4, 2013:2017 - .45),
      x2 = c(2013:2017 + .45, 2013:2017 - .4),
      y = rep(c(max(.$n) * 1.02, max(.$n) * 1.01), each = 5),
      y2 = rep(c(max(.$n) * 1.01, max(.$n) * 1.02), each =  5),
      gr = 1:10
    ),
    aes(
      x = x,
      xend = x2,
      y = y,
      yend = y2,
      group = gr
    ),
    color = colors[1],
    curvature = .5
  ) +
  geom_col(
    aes(fill = type),
    position = position_dodge(.8),
    width = .1
  ) +
  geom_line(data = tibble(
      x = c(2013:2017 - .45, 2013:2017 + .45),
      y = 0,
      gr = rep(1:5, 2)
    ),
    aes(
      x = x,
      y = y,
      group = gr
    ),
    color = colors[1]
  ) +
  geom_point(
    aes(color = type),
    position = position_dodge(.8),
    size = 2
  ) +
  geom_point(
    aes(y = year_pos),
    size = 15,
    color = colors[1],
    show.legend = FALSE
  ) +
  geom_text(aes(
      y = year_pos,
      label = year
    ),
    check_overlap = TRUE,
    color = 'white',
    family = 'Calibri'
  ) +
  geom_text(aes(
      y = max(.$n) * 1.05,
      label = total %>% fmt()
    ),
    check_overlap = TRUE,
    color = colors[1],
    hjust = 0,
    size = roz_cz_txt,
    family = 'Calibri'
  ) +
  geom_label(aes(
      y = n / 2,
      group = type,
      label = n %>% fmt()
    ),
    position = position_dodge(.8),
    family = 'Calibri',
    fill = 'white',
    color = colors[1],
    label.size = .2,
    size = roz_cz_txt - .5,
    label.padding = unit(0.1, 'lines'),
    show.legend = FALSE
  ) +
  coord_flip() +
  guides(color = guide_legend(
      override.aes = list(size = 3),
      keyheight = leg_sq,
      keywidth = leg_sq,
      reverse = TRUE
    ),
    fill = FALSE
  ) +
  scale_fill_manual(values = colors[c(1, 4, 8, 7, 3)]) +
  scale_color_manual(values = colors[c(1, 4, 8, 7, 3)]) +
  theme(
    legend.spacing.x = unit(1, 'mm'),
    axis.text = element_blank(),
    legend.position = c(.5, 1.01),
    plot.margin = margin(t = 15),
    legend.direction = 'horizontal'
  ) +
  ylim(c(.$year_pos[1] * 1.1, max(.$n) * 1.12))


ggsave(
  filename = 'wykresy/w3_7_v4.png',
  units = 'cm',
  width = 13,
  height = 14,
  bg = 'transparent',
  dpi = 600
)

# W 3.10. Współczynnik sukcesu wniosków w konkursach NCN w latach 2013–2017 w podziale na typy wnioskodawców ----

# V1

ncn$W3_7 %>%
  gather("typ", "lw", -rok) %>%
  left_join(ncn$W3_9 %>%
              gather("typ", "pf", -rok),
            by = c("rok", "typ")) %>%
  filter(typ != "razem") %>%
  mutate(typ = typ %>% reorder(desc(lw)),
         wsp = pf / lw,
         lab_pos = wsp + max(wsp, na.rm = TRUE) * .025) %>%
  ggplot(aes(x = factor(rok),
             y = wsp,
             label = wsp %>% round(2) %>% scales::percent(),
             fill = typ)) +
  geom_col(position = position_dodge(1)) +
  geom_text(aes(y = lab_pos),
            position = position_dodge(1),
            family = "Calibri",
            size = roz_cz_txt - .4,
            show.legend = FALSE) +
  facet_grid(. ~ rok,
             scales = "free",
             space = "free") +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)]) %>% rev()) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             nrow = 2)) +
  theme(axis.text.y = element_blank(),
        legend.spacing.x = unit(1, "mm"),
        strip.text = element_blank())

ggsave(
  filename = 'wykresy/w3_10_v1.png',
  units = 'cm',
  width = 14,
  height = 9,
  bg = 'transparent',
  dpi = 600
)

# V2 

ncn$W3_7 %>%
  gather("typ", "lw", -rok) %>%
  left_join(ncn$W3_9 %>%
              gather("typ", "pf", -rok),
            by = c("rok", "typ")) %>%
  filter(typ != "razem") %>%
  mutate(typ = typ %>% reorder(desc(lw)),
         wsp = pf / lw,
         lab_pos = wsp) %>%
  ggplot(aes(x = factor(rok),
             y = wsp,
             label = wsp %>% round(2) %>% scales::percent(),
             fill = typ,
             color = typ)) +
  geom_col(position = position_dodge(1),
           col = "white") +
  geom_label(aes(y = lab_pos,
                 color = typ,
                 group = typ),
             fill = "white",
             hjust = .5,
             vjust = .5,
             position = position_dodge(1),
             label.padding = unit(.1, "line"),
             family = "Calibri",
             size = roz_cz_txt - .3,
             show.legend = FALSE) +
  coord_flip() +
  facet_grid(rok ~ .,
             scales = "free",
             space = "free") +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)]) %>% rev()) +
  scale_color_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)]) %>% rev()) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             nrow = 2)) +
  theme(axis.text.x = element_blank(),
        legend.spacing.x = unit(1, "mm"),
        strip.text = element_blank())

ggsave(
  filename = 'wykresy/w3_10_v2.png',
  units = 'cm',
  width = 14,
  height = 12,
  bg = 'transparent',
  dpi = 600
)

# V3

ncn$W3_7 %>%
  gather("typ", "lw", -rok) %>%
  left_join(ncn$W3_9 %>%
              gather("typ", "pf", -rok),
            by = c("rok", "typ")) %>%
  filter(typ != "razem") %>%
  mutate(wsp = pf / lw,
         typ = typ %>% factor(levels = unique(.)[c(1, 3, 4, 2, 5)]),
         lab_pos = wsp,
         xpos = case_when(typ == "uczelnie niepubliczne" &
                            rok %in% c(2012:2014, 2015) ~ rok - .15,
                          typ == "uczelnie niepubliczne" &
                            rok == 2017 ~ rok - .28,
                          typ == "uczelnie publiczne" &
                            rok %in% c(2012, 2014) ~ rok + .15,
                          typ == "pozostałe jednostki" &
                            rok == 2013 ~ rok + .15,
                          typ == "pozostałe jednostki" &
                            rok == 2017 ~ rok + .28,
                          typ == "instytuty badawcze" &
                            rok == 2015 ~ rok + .15,
                          TRUE ~ rok)) %>%
  ggplot(aes(x = xpos,
             y = wsp,
             color = typ,
             fill = typ,
             label = (wsp * 100) %>% round())) +
  geom_rect(data = tibble(xmin = c(seq(2012,2017, 2) - .5),
                          xmax = c(seq(2012,2017, 2) + .5),
                          ymin = 0,
                          ymax = .42),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = rgb(247, 249, 251, maxColorValue = 255),
            inherit.aes = FALSE) +
  geom_line(lwd = 1,
            alpha = .5) +
  geom_point(size = 5,
             shape = 21,
             stroke = 2,
             color = "white") +
  geom_text(family = "Calibri",
            color = "white",
            size = roz_cz_txt - .4) +
  geom_shadowtext(aes(label = "%",
                      x = xpos + .15),
                  family = "Calibri",
                  size = roz_cz_txt - .4,
                  bg.color = "white") +
  scale_x_continuous(breaks = 2012:2017) +
  scale_color_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)]) %>% rev()) +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)]) %>% rev()) +
  guides(color = guide_legend(nrow = 2)) +
  theme(axis.text.y = element_blank(),
        legend.position = c(.5, 1.02),
        plot.margin = margin(20, 1, 1, 1))

ggsave(
  filename = 'wykresy/w3_10_v3.png',
  units = 'cm',
  width = 13,
  height = 10,
  bg = 'transparent',
  dpi = 600
)

# V4

ncn$W3_7 %>%
  gather("typ", "lw", -rok) %>%
  left_join(ncn$W3_9 %>%
              gather("typ", "pf", -rok),
            by = c("rok", "typ")) %>%
  filter(typ != "razem") %>%
  mutate(wsp = pf / lw,
         typ = typ %>% factor(levels = unique(.)[c(1, 3, 4, 2, 5)]),
         lab_pos = wsp,
         xpos = case_when(typ == "uczelnie niepubliczne" &
                            rok %in% c(2012:2014, 2015) ~ rok - .15,
                          typ == "uczelnie niepubliczne" &
                            rok == 2017 ~ rok - .28,
                          typ == "uczelnie publiczne" &
                            rok %in% c(2012, 2014) ~ rok + .15,
                          typ == "pozostałe podmioty" &
                            rok == 2013 ~ rok + .15,
                          typ == "pozostałe podmioty" &
                            rok == 2017 ~ rok + .28,
                          typ == "instytuty badawcze" &
                            rok == 2015 ~ rok + .15,
                          TRUE ~ rok)) %>%
  ggplot(aes(x = xpos,
             y = wsp,
             color = typ,
             fill = typ,
             label = (wsp * 100) %>% round())) +
  geom_line(data = tibble(x = c(2013:2017 - .5) %>% rep(2),
                          y = c(0, .42) %>% rep(each = 5),
                          gr = 1:5 %>% rep(2)),
            aes(x = x,
                y = y,
                group = gr),
            color = podstawowy,
            alpha = .3,
            linetype = "dotted",
            lwd = .2,
            inherit.aes = FALSE) +
  geom_line(lwd = .5,
            alpha = .5) +
  geom_point(size = 5,
             shape = 21,
             stroke = 0,
             color = "white") +
  geom_text(family = "Calibri",
            color = "white",
            size = roz_cz_txt - .4) +
  geom_shadowtext(aes(label = "%",
                      x = xpos + .15),
                  family = "Calibri",
                  size = roz_cz_txt - .4,
                  bg.color = "white") +
  scale_x_continuous(breaks = 2012:2017) +
  scale_color_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)]) %>% rev()) +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)]) %>% rev()) +
  guides(color = guide_legend(nrow = 2)) +
  theme(axis.text.y = element_blank(),
        legend.position = c(.5, 1.02),
        plot.margin = margin(20, 1, 1, 1))

ggsave(
  filename = 'wykresy/w3_10_v4.png',
  units = 'cm',
  width = 13,
  height = 10,
  bg = 'transparent',
  dpi = 600
)

# W 3.15. Średnia liczba złożonych wniosków i projektów finansowanych przez NCN na jednostkę w 2017 roku w podziale na kategorie naukowe jednostek naukowych ----

ncn$W3_15 %>%
  mutate(kategoria = kategoria %>% factor(levels = paste("kategoria", c("A+", "A", "B", "C"))),
         wsp_pos = wsp * max(val) * 1.5 - max(val) * .8,
         wsp_lab = wsp_pos - max(val) * .1,
         txt_pos = case_when(typ == "średnia liczba projektów finansowanych na jednostkę" &
                               kategoria == "kategoria C" ~ val + max(val) * .07,
                             TRUE ~ val + max(val) * .05),
         typ2 = if_else(typ == "średnia liczba projektów finansowanych na jednostkę",
                        "liczba jednostek",
                        "x")) %>% 
  ggplot(aes(x = kategoria,
             y = val,
             fill = typ %>% fct_rev(),
             label = (val * 1e6) %>% fmt2())) +
  geom_col(position = "dodge",
           col = "white") +
  geom_line(aes(y = wsp_pos,
                group = 1),
            linetype = "dotted",
            color = paleta_ald[2],
            lwd = .4) +
  geom_point(aes(y = val / 2,
                 col = typ2 %>% fct_rev()),
             position = position_dodge(.9),
             shape = 21,
             fill = "white",
             size = 6.5) +
  geom_text(aes(y = val / 2,
                label = n),
            position = position_dodge(.9),
            family = "Calibri",
            size = roz_cz_txt - .6) +
  geom_point(aes(y = wsp_pos,
                 shape = "współczynnik sukcesu"),
             size = 6,
             stroke = 0,
             fill = paleta_ald[2],
             color = "white") +
  geom_text(aes(y = txt_pos),
            position = position_dodge(.9),
            family = "Calibri",
            size = roz_cz_txt) +
  geom_text(aes(y = wsp_pos,
                label = (wsp * 100) %>%
                  round()),
            family = "Calibri",
            size = roz_cz_txt,
            color = "white") +
  geom_shadowtext(aes(y = wsp_pos,
                      label = "%"),
                  size = roz_cz_txt,
                  family = "Calibri",
                  nudge_x = .13,
                  color = paleta_ald[2],
                  bg.color = "white") +
  scale_fill_manual(values = c(podstawowy2, podstawowy)) +
  scale_color_manual(breaks = "liczba jednostek",
                     values = c(podstawowy2, podstawowy)) +
  guides(color = guide_legend(override.aes = list(size = 3.3),
                              label.position = "left"),
         fill = guide_legend(label.position = "left",
                             reverse = TRUE),
         shape = guide_legend(override.aes = list(size = 4),
                              label.position = "left")) +
  scale_shape_manual(values = 21) +
  theme(axis.text.y = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.y = unit(0, "mm"),
        legend.direction = "vertical",
        legend.box = "vertical",
        axis.text.x = element_text(vjust = 43.5),
        legend.key.size = unit(leg_sq, "lines"),
        legend.box.just = "right",
        legend.position = c(.62, .85))

ggsave(
  filename = 'wykresy/w3_15_1.png',
  units = 'cm',
  width = 13,
  height = 10,
  bg = 'transparent',
  dpi = 600
)

# W 3.20 i 3.21 i 3.22 ----

# V3 z liniami kropkowanymi

ncn$W3_20 %>% 
  gather(rok, val, -obsz) %>%
  left_join(ncn$W3_21 %>%
              gather(rok, val, -obsz),
            by = c("obsz", "rok")) %>%
  rename_at(3:4, ~ c("zł", "fin")) %>%
  mutate(obsz = obsz %>% factor(levels = c("humanistyczne",
                                           "społeczne",
                                           "ścisłe",
                                           "przyrodnicze",
                                           "techniczne",
                                           "rolnicze",
                                           "medyczne") %>% rev()),
         wsp = fin / zł,
         lab_pos = zł - max(zł) * .03,
         wsp_pos = wsp * 2e3 + max(zł) * .92,
         rok_pos = -max(wsp_pos) * .3,
         wsp_lab = (wsp * 100) %>% round()) %>%
  ggplot(aes(x = obsz,
             y = zł,
             fill = rok,
             color = rok,
             label = zł)) +
  geom_segment(aes(xend = obsz,
                   y = zł,
                   yend = wsp_pos),
               linetype = 3,
               alpha = .5) +
  geom_col(aes(alpha = " liczba wniosków złożonych"),
           position = position_dodge(.9),
           col = "white") +
  geom_col(aes(y = fin,
               alpha = " liczba projektów finansowanych  "),
           width = .7) +
  geom_text(aes(y = fin / 2,
                label = fin),
            color = "white",
            size = roz_cz_txt - 1) +
  geom_point(aes(y = wsp_pos,
                 shape = "%  współczynnik sukcesu"),
             size = 4.4,
             color = "white") +
  geom_text(aes(y = wsp_pos,
                label = wsp_lab),
            color = "white",
            size = roz_cz_txt - .8,
            family = "Calibri") +
  geom_point(aes(x = "przyrodnicze",
                 y = rok_pos),
             size = 14,
             show.legend = FALSE) +
  geom_text(aes(x = "przyrodnicze",
                y = rok_pos,
                label = rok),
            family = "Calibri",
            color = "white",
            size = 4.5,
            check_overlap = TRUE) +
  geom_label(aes(y = lab_pos),
             family = "Calibri",
             color = podstawowy,
             fill = "white",
             label.r = unit(0, "lines"),
             label.padding = unit(0.1, "lines"),
             label.size = .2,
             size = roz_cz_txt - .8) +
  geom_text(aes(y = -70,
                label = obsz),
            size = roz_cz_txt,
            color = rgb(59, 58, 60, maxColorValue = 255),
            hjust = 1,
            family = "Calibri",
            check_overlap = TRUE) +
  geom_text(aes(y = wsp_pos + max(wsp_pos) * .02,
                label = "%"),
            family = "Calibri",
            hjust = 0,
            size = roz_cz_txt - .3) +
  coord_flip() +
  facet_grid(rok %>% fct_rev() ~ .) +
  scale_shape_manual(values = 21) +
  scale_alpha_manual(values = c(1, .6)) +
  scale_fill_manual(values = c(paleta_ald[c(2, 1, 3, 4, 7, 6)]) %>% rev()) +
  scale_color_manual(values = c(paleta_ald[c(2, 1, 3, 4, 7, 6)]) %>% rev()) +
  guides(color = "none",
         fill = "none",
         shape = guide_legend(override.aes = list(fill = szary_poz,
                                                  size = 3.5),
                              keyheight = leg_sq - .1,
                              keywidth = leg_sq - .1,
                              order = 2),
         alpha = guide_legend(override.aes = list(alpha = c(.6, .3)),
                              keyheight = leg_sq - .1,
                              keywidth = leg_sq - .1,
                              order = 1)) +
  theme(legend.box.margin = margin(0, 0, 0, 0),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(t = 20),
        legend.position = c(.55, 1.04),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        axis.text = element_blank(),
        strip.text = element_blank())

# legenda rozbita

legend <-
  tibble(obsz = c(rep("2", 12), rep("1", 6)),
         rok = rep("2018", 18),
         zł = c(rep(1, 6), rep(2, 6), rep(3, 6)),
         fin = rep(1:6, 3),
         wsp = c(rep(1, 6), rep(2, 6), rep(1, 6)),
         lab_pos = c(rep(-50, 6), rep(1750, 6), rep(950, 6)),
         wsp_pos = c(seq(-500, -100, length.out = 6),
                     seq(1300, 1700, length.out = 6),
                     seq(500, 900, length.out = 6)),
         rok_pos = rep(NA, 18),
         wsp_lab = c(rep("liczba projektów finansowanych", 6),
                     rep("liczba wniosków złożonych", 6),
                     rep("% współczynnik sukcesu", 6)))

ncn$W3_20 %>% 
  gather(rok, val, -obsz) %>%
  left_join(ncn$W3_21 %>%
              gather(rok, val, -obsz),
            by = c("obsz", "rok")) %>%
  rename_at(3:4, ~ c("zł", "fin")) %>%
  mutate(wsp = fin / zł,
         lab_pos = zł - max(zł) * .03,
         wsp_pos = wsp * 2e3 + max(zł) * .92,
         rok_pos = -max(wsp_pos) * .3,
         wsp_lab = (wsp * 100) %>% round() %>% as.character(),
         obsz = obsz %>% factor(levels = c(
           "1",
           "2",
           "humanistyczne",
           "społeczne",
           "ścisłe",
           "przyrodnicze",
           "techniczne",
           "rolnicze",
           "medyczne") %>% rev()),
         rok = rok %>% factor(levels = 2012:2018)) %>%
  ggplot(aes(x = obsz,
             y = zł,
             fill = rok,
             color = rok,
             label = zł)) +
  geom_segment(aes(xend = obsz,
                   y = zł,
                   yend = wsp_pos),
               linetype = 3,
               alpha = .5) +
  geom_col(position = position_dodge(.9),
           alpha = .6,
           col = "white") +
  geom_col(aes(y = fin,
               color = NULL),
           width = .7) +
  geom_text(aes(y = fin / 2,
                label = fin),
            color = "white",
            size = roz_cz_txt - 1) +
  geom_point(aes(y = wsp_pos),
             shape = 21,
             size = 4.4,
             color = "white") +
  geom_text(aes(y = wsp_pos,
                label = wsp_lab),
            color = "white",
            size = roz_cz_txt - .8,
            family = "Calibri") +
  geom_point(aes(x = "przyrodnicze",
                 y = rok_pos),
             size = 14) +
  geom_text(aes(x = "przyrodnicze",
                y = rok_pos,
                label = rok),
            family = "Calibri",
            color = "white",
            size = 4.5,
            check_overlap = TRUE) +
  geom_label(aes(y = lab_pos),
             family = "Calibri",
             color = podstawowy,
             fill = "white",
             label.r = unit(0, "lines"),
             label.padding = unit(0.1, "lines"),
             label.size = .2,
             size = roz_cz_txt - .8) +
  geom_text(aes(y = -70,
                label = obsz),
            size = roz_cz_txt,
            color = rgb(59, 58, 60, maxColorValue = 255),
            hjust = 1,
            family = "Calibri",
            check_overlap = TRUE) +
  geom_text(aes(y = wsp_pos + max(wsp_pos) * .02,
                label = "%"),
            family = "Calibri",
            hjust = 0,
            size = roz_cz_txt - .3) +
  geom_point(data = legend,
             aes(y = wsp_pos,
                 shape = factor(zł),
                 color = factor(fin),
                 alpha = factor(wsp)),
             size = 2.2) +
  geom_text(data = legend,
            aes(y = lab_pos,
                label = wsp_lab),
            family = "Calibri",
            size = roz_cz_txt - .5,
            color = rgb(59, 58, 60, maxColorValue = 255),
            check_overlap = TRUE,
            hjust = 0) +
  coord_flip() +
  facet_grid(rok %>% fct_rev() ~ .,
             scales = "free",
             space = "free") +
  scale_shape_manual(values = c(15, 15, 16)) +
  scale_alpha_manual(values = c(1, .6)) +
  scale_fill_manual(values = c(paleta_ald[c(2, 1, 5, 6, 4, 7, 2, 1, 3, 6, 7, 5)]) %>% rev()) +
  scale_color_manual(values = c(paleta_ald[c(5, 6, 4, 7, 2, 1, 3, 6, 7, 5, 2, 1) %>% rev()])) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        strip.text = element_blank())

ggsave(
  filename = 'wykresy/w3_20_v2.png',
  units = 'cm',
  width = 13.5,
  height = 15,
  bg = 'transparent',
  dpi = 600
)

# W 3.23. Liczba wniosków złożonych w konkursach NCN w latach 2013–2017 w podziale na obszary nauki i typy wnioskodawców ----

# razem

ncn$W3_23 %>%
  group_by(obsz) %>%
  mutate_if(is.double, ~ (. / sum(., na.rm = TRUE))) %>%
  gather(rok, val, `2012`:`2017`) %>%
  mutate(inst = inst %>% factor(levels = c("uczelnie publiczne",
                                           "uczelnie niepubliczne",
                                           "instytuty PAN",
                                           "instytuty badawcze",
                                           "pozostałe podmioty") %>% rev())) %>%
  group_by(obsz, rok) %>%
  arrange(inst %>% desc()) %>%
  mutate(ypos = val / 2 + c(0, cumsum(val)[-5]),
         ypos = case_when(obsz == "humanistyczne" &
                            inst == "pozostałe podmioty" &
                            rok %in% c(2014, 2017)
                          ~ ypos + .015,
                          obsz == "społeczne" &
                            inst == "pozostałe podmioty"
                          ~ ypos + .012,
                          TRUE ~ ypos)) %>%
  ggplot(aes(
    x = rok,
    y = val,
    fill = inst,
    color = inst,
    label = if_else(val == 0, NA_character_, val %>%
      fmt_pct(if_else(
      val > .0099, 0, 1)))
  )) +
  geom_col(position = "fill") +
  geom_label(aes(y = ypos),
             fill = "white",
             family = "Calibri",
             size = roz_cz_txt - .4,
             label.padding = unit(.1, "lines"),
             show.legend = FALSE) +
  geom_text(aes(y = -.01,
                label = rok),
            hjust = 1,
            size = roz_cz_txt,
            family = "Calibri",
            color = rgb(.1, .1, .1),
            angle = 90,
            check_overlap = TRUE) +
  facet_grid(~ obsz) +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(6, 5, 2, 1)])) +
  scale_color_manual(values = c(szary_poz, paleta_ald[c(6, 5, 2, 1)])) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE),
         color = FALSE) +
  theme(legend.spacing.x = unit(1, "mm"),
        axis.text.x = element_blank(),
        strip.text = element_text(face = "bold",
                                  margin = margin(0, 0, 0, 0)),
        legend.margin = margin(0, 0, 0, 0)) +
  expand_limits(y = c(-.03, NA))

ggsave(
  filename = 'wykresy/w3_23v2.png',
  units = 'cm',
  width = 32,
  height = 14,
  bg = 'transparent',
  dpi = 600
)

# humanistyczne

ncn$W3_23 %>%
  group_by(obsz) %>%
  mutate_if(is.double, ~ (. / sum(.))) %>%
  gather(rok, val, `2012`:`2017`) %>%
  mutate(rok = rok %>% as.numeric(),
         xpos = case_when(inst == "uczelnie publiczne" ~ rok,
                          inst %in% c("uczelnie niepubliczne",
                                      "instytuty badawcze") ~ rok + .22,
                          TRUE ~ rok - .22),
         inst = inst %>% factor(levels = c("uczelnie publiczne",
                                           "uczelnie niepubliczne",
                                           "instytuty PAN",
                                           "instytuty badawcze",
                                           "pozostałe instytucje") %>% rev())) %>%
  group_by(obsz, rok) %>%
  arrange(inst %>% desc()) %>%
  mutate(ypos = val / 2 + c(0, cumsum(val)[-5]) + c(0, 0, 0, 0, .033)) %>%
  filter(obsz == "humanistyczne") %>%
  ggplot(aes(x = rok,
             y = val,
             fill = inst,
             label = val %>%
               scales::percent() %>%
               str_replace("\\.", ","))) +
  geom_col(position = "fill") +
  geom_label(aes(x = xpos,
                 y = ypos),
             color = "white",
             family = "Calibri",
             size = roz_cz_txt - .4,
             label.padding = unit(0.1, "lines"),
             show.legend = FALSE,
             label.size = NA) +
  coord_flip() +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)])) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 2012:2017) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE,
                             byrow = TRUE,
                             nrow = 2)) +
  theme(legend.spacing.x = unit(1, "mm"))
  
ggsave(
  filename = 'wykresy/w3_23a_v1.png',
  units = 'cm',
  width = 13,
  height = 6.6,
  bg = 'transparent',
  dpi = 600
)

# społeczne

ncn$W3_23 %>%
  group_by(obsz) %>%
  mutate_if(is.double, ~ (. / sum(.))) %>%
  gather(rok, val, `2012`:`2017`) %>%
  mutate(rok = rok %>% as.numeric(),
         xpos = case_when(inst %in% c("uczelnie niepubliczne", "uczelnie publiczne") ~ rok,
                          inst == "instytuty badawcze" ~ rok + .22,
                          TRUE ~ rok - .22),
         inst = inst %>% factor(levels = c("uczelnie publiczne",
                                           "uczelnie niepubliczne",
                                           "instytuty PAN",
                                           "instytuty badawcze",
                                           "pozostałe instytucje") %>% rev())) %>%
  group_by(obsz, rok) %>%
  arrange(inst %>% desc()) %>%
  mutate(ypos = val / 2 + c(0, cumsum(val)[-5]) + c(0, 0, 0, 0, .033)) %>%
  filter(obsz == "społeczne") %>%
  ggplot(aes(x = rok,
             y = val,
             fill = inst,
             label = val %>%
               scales::percent() %>%
               str_replace("\\.", ","))) +
  geom_col(position = "fill") +
  geom_label(aes(x = xpos,
                 y = ypos),
             color = "white",
             family = "Calibri",
             size = roz_cz_txt - .4,
             label.padding = unit(0.1, "lines"),
             show.legend = FALSE,
             label.size = NA) +
  coord_flip() +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)])) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 2012:2017) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE,
                             byrow = TRUE,
                             nrow = 2)) +
  theme(legend.spacing.x = unit(1, "mm"),
        legend.position = "none")

ggsave(
  filename = 'wykresy/w3_23b_v1.png',
  units = 'cm',
  width = 13,
  height = 5,
  bg = 'transparent',
  dpi = 600
)

# ścisłe

ncn$W3_23 %>%
  group_by(obsz) %>%
  mutate_if(is.double, ~ (. / sum(.))) %>%
  gather(rok, val, `2012`:`2017`) %>%
  mutate(rok = rok %>% as.numeric(),
         inst = inst %>% factor(levels = c("uczelnie publiczne",
                                           "uczelnie niepubliczne",
                                           "instytuty PAN",
                                           "instytuty badawcze",
                                           "pozostałe instytucje") %>% rev())) %>%
  group_by(obsz, rok) %>%
  arrange(inst %>% desc()) %>%
  mutate(ypos = val / 2 + c(0, cumsum(val)[-5]) + c(0, 0, 0, -.01, .033)) %>%
  filter(obsz == "ścisłe") %>%
  ggplot(aes(x = rok,
             y = val,
             fill = inst,
             label = val %>%
               scales::percent() %>%
               str_replace("\\.", ","))) +
  geom_col(position = "fill") +
  geom_label(aes(y = ypos),
             color = "white",
             family = "Calibri",
             size = roz_cz_txt - .4,
             label.padding = unit(0.1, "lines"),
             show.legend = FALSE,
             label.size = NA) +
  coord_flip() +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)])) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 2012:2017) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE,
                             byrow = TRUE,
                             nrow = 2)) +
  theme(legend.spacing.x = unit(1, "mm"),
        legend.position = "none")

ggsave(
  filename = 'wykresy/w3_23c_v1.png',
  units = 'cm',
  width = 13,
  height = 5,
  bg = 'transparent',
  dpi = 600
)

# przyrodnicze

ncn$W3_23 %>%
  group_by(obsz) %>%
  mutate_if(is.double, ~ (. / sum(., na.rm = T)) %>% replace_na(0)) %>%
  gather(rok, val, `2012`:`2017`) %>%
  mutate(rok = rok %>% as.numeric(),
         inst = inst %>% factor(levels = c("uczelnie publiczne",
                                           "uczelnie niepubliczne",
                                           "instytuty PAN",
                                           "instytuty badawcze",
                                           "pozostałe instytucje") %>% rev())) %>%
  group_by(obsz, rok) %>%
  arrange(inst %>% desc()) %>%
  mutate(ypos = val / 2 + c(0, cumsum(val)[-5]) + c(0, 0, 0, -.01, .033)) %>%
  filter(obsz == "przyrodnicze") %>%
  ggplot(aes(x = rok,
             y = val,
             fill = inst,
             label = val %>%
               scales::percent() %>%
               str_replace("\\.", ","))) +
  geom_col(position = "fill") +
  geom_label(aes(y = ypos),
             color = "white",
             family = "Calibri",
             size = roz_cz_txt - .4,
             label.padding = unit(0.1, "lines"),
             show.legend = FALSE,
             label.size = NA) +
  coord_flip() +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)])) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 2012:2017) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE,
                             byrow = TRUE,
                             nrow = 2)) +
  theme(legend.spacing.x = unit(1, "mm"),
        legend.position = "none")

ggsave(
  filename = 'wykresy/w3_23d_v1.png',
  units = 'cm',
  width = 13,
  height = 5,
  bg = 'transparent',
  dpi = 600
)

# techniczne

ncn$W3_23 %>%
  group_by(obsz) %>%
  mutate_if(is.double, ~ (. / sum(.))) %>%
  gather(rok, val, `2012`:`2017`) %>%
  mutate(rok = rok %>% as.numeric(),
         xpos = case_when(inst %in% c("instytuty badawcze", "uczelnie publiczne", "pozostałe instytucje") ~ rok,
                          inst == "uczelnie niepubliczne" ~ rok + .22,
                          TRUE ~ rok - .22),
         inst = inst %>% factor(levels = c("uczelnie publiczne",
                                           "uczelnie niepubliczne",
                                           "instytuty PAN",
                                           "instytuty badawcze",
                                           "pozostałe instytucje") %>% rev())) %>%
  group_by(obsz, rok) %>%
  arrange(inst %>% desc()) %>%
  mutate(ypos = val / 2 + c(0, cumsum(val)[-5]) + c(0, 0, 0, 0, .033)) %>%
  filter(obsz == "techniczne") %>%
  ggplot(aes(x = rok,
             y = val,
             fill = inst,
             label = val %>%
               scales::percent() %>%
               str_replace("\\.", ","))) +
  geom_col(position = "fill") +
  geom_label(aes(x = xpos,
                 y = ypos),
             color = "white",
             family = "Calibri",
             size = roz_cz_txt - .4,
             label.padding = unit(0.1, "lines"),
             show.legend = FALSE,
             label.size = NA) +
  coord_flip() +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)])) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 2012:2017) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE,
                             byrow = TRUE,
                             nrow = 2)) +
  theme(legend.spacing.x = unit(1, "mm"),
        legend.position = "none")

ggsave(
  filename = 'wykresy/w3_23e_v1.png',
  units = 'cm',
  width = 13,
  height = 5,
  bg = 'transparent',
  dpi = 600
)

# rolnicze

ncn$W3_23 %>%
  group_by(obsz) %>%
  mutate_if(is.double, ~ (. / sum(., na.rm = T)) %>% replace_na(0)) %>%
  gather(rok, val, `2012`:`2017`) %>%
  mutate(rok = rok %>% as.numeric(),
         inst = inst %>% factor(levels = c("uczelnie publiczne",
                                           "uczelnie niepubliczne",
                                           "instytuty PAN",
                                           "instytuty badawcze",
                                           "pozostałe instytucje") %>% rev())) %>%
  group_by(obsz, rok) %>%
  arrange(inst %>% desc()) %>%
  mutate(ypos = val / 2 + c(0, cumsum(val)[-5]) + c(0, 0, 0, -.01, .033)) %>%
  filter(obsz == "rolnicze") %>%
  ggplot(aes(x = rok,
             y = val,
             fill = inst,
             label = val %>%
               scales::percent() %>%
               str_replace("\\.", ","))) +
  geom_col(position = "fill") +
  geom_label(aes(y = ypos),
             color = "white",
             family = "Calibri",
             size = roz_cz_txt - .4,
             label.padding = unit(0.1, "lines"),
             show.legend = FALSE,
             label.size = NA) +
  coord_flip() +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)])) +
  scale_x_continuous(breaks = 2012:2017) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE,
                             byrow = TRUE,
                             nrow = 2)) +
  theme(legend.spacing.x = unit(1, "mm"),
        legend.position = "none")

ggsave(
  filename = 'wykresy/w3_23f_v1.png',
  units = 'cm',
  width = 13,
  height = 5,
  bg = 'transparent',
  dpi = 600
)

# medyczne

ncn$W3_23 %>%
  group_by(obsz) %>%
  mutate_if(is.double, ~ (. / sum(., na.rm = T)) %>% replace_na(0)) %>%
  gather(rok, val, `2012`:`2017`) %>%
  mutate(rok = rok %>% as.numeric(),
         inst = inst %>% factor(levels = c("uczelnie publiczne",
                                           "uczelnie niepubliczne",
                                           "instytuty PAN",
                                           "instytuty badawcze",
                                           "pozostałe instytucje") %>% rev())) %>%
  group_by(obsz, rok) %>%
  arrange(inst %>% desc()) %>%
  mutate(ypos = val / 2 + c(0, cumsum(val)[-5]) + c(0, -.016, 0, -.01, .0)) %>%
  filter(obsz == "medyczne") %>%
  ggplot(aes(x = rok,
             y = val,
             fill = inst,
             label = val %>%
               scales::percent() %>%
               str_replace("\\.", ","))) +
  geom_col(position = "fill") +
  geom_label(aes(y = ypos),
             color = "white",
             family = "Calibri",
             size = roz_cz_txt - .4,
             label.padding = unit(0.1, "lines"),
             show.legend = FALSE,
             label.size = NA) +
  coord_flip() +
  scale_fill_manual(values = c(szary_poz, paleta_ald[c(2, 6, 5, 1)])) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 2012:2017) +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE,
                             byrow = TRUE,
                             nrow = 2)) +
  theme(legend.spacing.x = unit(1, "mm"),
        legend.position = "none")

ggsave(
  filename = 'wykresy/w3_23g_v1.png',
  units = 'cm',
  width = 13,
  height = 5,
  bg = 'transparent',
  dpi = 600
)

# R 5.X2. Liczba publikacji naukowych przypadających w podziale na województwa ----

spc <- .15

dane_art <-
  pbn$`R 5.X1` %>%
  group_by(typ) %>%
  summarise_at(2:17, sum) %>% 
  gather(wjw, n, -typ) %>%
  filter(!typ %like% 'Articl.*') %>%
  group_by(wjw) %>%
  mutate(sum = sum(n)) %>%
  left_join(woj_naz, c('wjw' = 'województwo')) %>%
  group_by(wjw) %>%
  mutate(
    typ = typ %>% fct_relevel('Article', after = Inf),
    lat = case_when(
      wjw == 'pomorskie' ~ lat + .11,
      wjw == 'wielkopolskie' ~ lat - .12,
      wjw == 'kujawsko-pomorskie' ~ lat + .05,
      wjw == 'opolskie' ~ lat + .07,
      wjw == 'dolnośląskie' ~ lat + .07,
      TRUE ~ lat
    ),
    long = case_when(
      wjw == 'zachodniopomorskie' ~ long - .2,
      wjw == 'warmińsko-mazurskie' ~ long - .2,
      wjw == 'kujawsko-pomorskie' ~ long - .4,
      wjw == 'świętokrzyskie' ~ long - .09,
      wjw == 'śląskie' ~ long - .07,
      TRUE ~ long
    ),
    lat_p = case_when(
      typ == 'Arti' ~ lat + spc / .8,
      typ == 'Book' ~ lat - spc / .8,
      TRUE ~ lat
    ) %>% `-`(.2)
  )

woj_df %>%
  ggplot(aes(
    x = long,
    y = lat,
    group = group
  )) +
  geom_polygon(
    fill = "white",
    col = podstawowy2 %>% alpha(.6),
    size = .2
  ) +
  geom_point(
    data = dane_art,
    aes(
      x = long - .2,
      y = lat_p + .05,
      size = n,
      color = typ,
      group = NULL
    ),
    alpha = .6
  ) +
  geom_text(
    data = dane_art,
    aes(
      y = lat_p + .05,
      label = n %>% fmt(),
      group = NULL
    ),
    family = 'Calibri',
    size = 2.5,
    hjust = 0,
    color = rgb(.2, .2, .2)
  ) +
  geom_text(
    data = dane_art,
    aes(
      x = long - .3,
      y = lat + .22,
      label = wjw,
      group = NULL
    ),
    family = 'Calibri',
    size = 3,
    hjust = 0,
    color = rgb(.2, .2, .2),
    check_overlap = TRUE
  ) +
  scale_size_area(trans = 'sqrt') +
  scale_color_manual(
    values = paleta_ald[c(1, 3, 2)],
    labels = c(
      'artykuł naukowy',
      'rozdział w monografii',
      'monografia naukowa'
  )) +
  guides(
    size = FALSE,
    color = guide_legend(override.aes = list(
      size = 4,
      color = paleta_ald[1:3]
  ))) +
  theme_map() +
  theme(
    legend.title = element_blank(),
    legend.position = c(.1, .05),
    legend.background = element_rect(color = 'transparent', fill = 'transparent')
  )

ggsave(
  filename = 'wykresy/R5_X2_v1.png',
  units = 'cm',
  width = 13 * 1.4,
  height = 12 * 1.4,
  bg = 'transparent',
  dpi = 400
)

# R 1.1. Liczba uczelni publicznych, niepublicznych i kościelnych w 2017 roku według województw ----

typ <-
  uczelnie$`R 1.1` %>%
  rename_at(vars(starts_with("liczba")),
            ~ paste("uczelnia", c("publiczna",
                                  "niepubliczna",
                                  "kościelna"))) %>%
  mutate("uczelnia kościelna" = replace_na(`uczelnia kościelna`, 0))
  
woj_sum <-
  woj_naz %>%
  left_join(typ)

n_row <- 6

main <-
  woj_df %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "white", col = podstawowy2,
               size = .2) +
  geom_text(data = woj_sum,
            aes(label = województwo, group = NULL),
            nudge_y = .14,                              # pozycja nazwy
            size = roz_cz_txt - .4,
            family = "Calibri") +
  geom_text(data = woj_sum,
            aes(label = ogółem, group = NULL),
            nudge_y = .29,                              # pozycja liczby
            size = roz_cz_txt,
            family = "Calibri") +
  theme_void() +
  xlim(c(13.8, 24.2))

subplot <-
  woj_sum %>%
  mutate(mar = ogółem / n_row) %>%
  select(-c(województwo, ogółem, id)) %>%
  pmap(function(`uczelnia publiczna`,
                `uczelnia niepubliczna`,
                `uczelnia kościelna`,
                long,
                lat,
                mar) {
    annotation_custom(
      ggplotGrob(
        waffle(
          c(`uczelnia publiczna`,
            `uczelnia niepubliczna`,
            `uczelnia kościelna`),
          legend_pos = "none",          
          size = .3,
          rows = n_row,
          colors = paleta_ald[1:3]
        )
      ),
      xmin = long - mar,
      xmax = long + mar,
      ymin = lat - .28 - .13,                            # pozycja subplotów
      ymax = lat + .28 - .13)
  })

main + subplot +
  annotation_custom(
    ggplotGrob(                                          # żeby mieć legendę
      waffle(
        c(c(`1 uczelnia publiczna` = 0,
            `1 uczelnia niepubliczna` = 0,
            `1 uczelnia kościelna` = 1)),
        legend_pos = "bottom",
        colors = paleta_ald[1:3]) +
        theme(legend.text = element_text(family = "Calibri",
                                         size = roz_cz)) +
        guides(fill = guide_legend(keywidth = leg_sq,
                                   keyheight = leg_sq))),
    ymin = 54.7                                          # pozycja legendy
  )

ggsave(
  filename = 'wykresy/R1_1_v1.png',
  units = 'cm',
  width = 13,
  height = 12,
  bg = 'transparent',
  dpi = 600
)

# W 1.7. Liczba studentów pozyskanych i utraconych przez uczelnie w wyniku otwierania wydziałów zamiejscowych w innych województwach w 2017 roku ----

## V1

uczelnie$`W 1.7 i R 1.3` %>%
  slice(-1) %>%
  mutate(województwo = reorder(województwo, `liczba studentów pozyskanych`)) %>%
  gather("poz_utr", "liczba studentów", -województwo) %>%
  ggplot(aes(
    x = województwo,
    y = `liczba studentów`,
    fill = poz_utr %>% fct_rev(),
    label = `liczba studentów` %>% fmt()
  )) +
  geom_col(position = "dodge", col = "white") +
  geom_text(position = position_dodge(.9),
            hjust = -.1,
            size = roz_cz_txt,
            family = "Calibri") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_blank()) +
  scale_fill_manual(values = plus_minus %>% rev()) +
  ylim(c(0, 11000)) +
  coord_flip() +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE))

ggsave(
  filename = 'wykresy/w1_7_v2.png',
  units = 'cm',
  width = 13,
  height = 13,
  bg = 'transparent',
  dpi = 600
)

## V2

uczelnie$`W 1.7 i R 1.3` %>%
  slice(-1) %>%
  mutate(województwo = reorder(województwo, `liczba studentów pozyskanych`),
         clr = `liczba studentów pozyskanych` > `liczba studentów utraconych`,
         poz_pos = if_else(`liczba studentów pozyskanych` > `liczba studentów utraconych`,
                           `liczba studentów pozyskanych` + 1e3,
                           `liczba studentów pozyskanych` - 1e3),
         utr_pos = if_else(`liczba studentów pozyskanych` < `liczba studentów utraconych`,
                           `liczba studentów utraconych` + 1e3,
                           `liczba studentów utraconych` - 1e3)) %>%
  gather(poz_utr, n, c(`liczba studentów pozyskanych`, `liczba studentów utraconych`)) %>%
  mutate(txt_pos = if_else(poz_utr == "liczba studentów pozyskanych",
                           poz_pos,
                           utr_pos)) %.>%
  ggplot(data = .,
         aes(x = województwo,
             y = n,
             color = poz_utr,
             label = n %>% fmt())) +
  geom_segment(data = . %>% select(województwo, poz_utr, n) %>%
                 spread(poz_utr, n),
               aes(xend = województwo,
                   y = `liczba studentów pozyskanych`,
                   yend = `liczba studentów utraconych`,
                   color = NULL,
                   label = NULL),
               color = "grey") +
  geom_point() +
  geom_text(aes(y = txt_pos),
            size = roz_cz_txt,
            family = "Calibri",
            show.legend = FALSE) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_blank()) +
  scale_fill_manual(values = plus_minus %>% rev()) +
  ylim(c(-1100, 13000)) +
  coord_flip() +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE)) +
  scale_color_manual(values = plus_minus)

ggsave(
  filename = 'wykresy/w1_7_v2.png',
  units = 'cm',
  width = 13,
  height = 13,
  bg = 'transparent',
  dpi = 600
)

# z "różnica" w legendzie

uczelnie$`W 1.7 i R 1.3` %>%
  slice(-1) %>%
  mutate(województwo = reorder(województwo, `liczba studentów pozyskanych`),
         clr = `liczba studentów pozyskanych` > `liczba studentów utraconych`,
         poz_pos = if_else(`liczba studentów pozyskanych` > `liczba studentów utraconych`,
                           `liczba studentów pozyskanych` + 1e3,
                           `liczba studentów pozyskanych` - 1e3),
         utr_pos = if_else(`liczba studentów pozyskanych` < `liczba studentów utraconych`,
                           `liczba studentów utraconych` + 1e3,
                           `liczba studentów utraconych` - 1e3)) %>%
  gather(poz_utr, n, c(`liczba studentów pozyskanych`, `liczba studentów utraconych`)) %>%
  mutate(txt_pos = if_else(poz_utr == "liczba studentów pozyskanych",
                           poz_pos,
                           utr_pos)) %.>%
  ggplot(data = .,
         aes(x = województwo,
             y = n,
             color = poz_utr,
             label = n %>% fmt())) +
  geom_segment(data = . %>% select(województwo, poz_utr, n) %>%
                 spread(poz_utr, n) %>%
                 add_row("województwo" = "",
                         "liczba studentów pozyskanych" = 5200,
                         "liczba studentów utraconych" = 5500),
               aes(xend = województwo,
                   y = `liczba studentów pozyskanych`,
                   yend = `liczba studentów utraconych`,
                   color = NULL,
                   label = NULL),
               color = "grey") +
  geom_point() +
  geom_text(aes(y = txt_pos),
            size = roz_cz_txt,
            family = "Calibri",
            show.legend = FALSE) +
  annotate(geom = "text",
           label = "różnica",
           family = "Calibri",
           size = roz_cz_txt,
           x = "",
           y = 6600) +
  theme(legend.title = element_blank(),
        legend.position = c(.5, 1.01),
        legend.direction = "horizontal",
        axis.text.x = element_blank(),
        plot.margin = margin(t = 10)) +
  scale_fill_manual(values = plus_minus %>% rev()) +
  ylim(c(-1100, 13000)) +
  coord_flip() +
  guides(fill = guide_legend(keywidth = leg_sq,
                             keyheight = leg_sq,
                             reverse = TRUE)) +
  scale_color_manual(values = plus_minus)

# W 3.22. 3.24. 3.26. Współczynnik ukończenia kształcenia (completion rate) na studiach pierwszego stopnia ----

dropout$p_st %>%
  filter(rok == '2013/2014') %>%
  mutate(
    kiedy = fct_relevel(kiedy, 'skreśl', after = 1),
    typ = case_when(
      typ == 'ogółem' ~ 'ogółem dla całej populacji studentów',
      typ == '26 lat' ~ 'wiek 26 i więcej w chwili rozpoczęcia studiów',
      typ == 'niepełnospr' ~ 'fakt pobierania stypendium dla osób niepełnosprawnych'
    ) %>% str_wrap(20),
    typ = typ %>% fct_relevel(
      'fakt pobierania\nstypendium dla osób\nniepełnosprawnych',
      after = Inf
    )) %>%
  group_by(typ) %>%
  mutate(
    razem = sum(n),
    pct = n / razem,
    skoncz = kiedy %like% '.*_mies.*'
  ) %>%
  group_by(typ, skoncz) %>%
  mutate(lab_sum = sum(n)) %>%
  nest() %>%
  mutate(xpos = row_number() %>% rev()) %>%
  unnest() %>%
  group_by(typ, skoncz) %>%
  arrange(kiedy) %>%
  mutate(
    xmin = xpos - .45,
    xmax = xpos + .45,
    ymin = c(0, cumsum(pct)[1]),
    ymin = if_else(skoncz, ymin, -ymin),
    ymax = cumsum(pct),
    ymax = if_else(skoncz, ymax, -ymax),
    lab_sum_pos = if_else(skoncz, max(ymax) * 1.03, min(ymax) * 1.03),
    lab_sum_hj = if_else(skoncz, 0, 1),
    lab_pos = (ymin + ymax) / 2,
    lab_pos = case_when(
      typ != 'wiek 26\ni więcej w chwili\nrozpoczęcia\nstudiów' &
        kiedy == 'po_6_mies' ~ lab_pos - .01,
      typ == 'wiek 26\ni więcej w chwili\nrozpoczęcia\nstudiów' &
        kiedy == 'po_6_mies' ~ lab_pos - .03,
      TRUE ~ lab_pos
    )) %>%
  group_by(typ) %>%
  mutate(typ_pos = mean(xpos)) %.>%
  ggplot(
    data = .,
    aes(
      x = xpos,
      y = pct,
      fill = kiedy,
      group = skoncz,
      label = (pct * 100) %>%
        round(if_else(pct < .01, 1, 0)) %>%
        str_c('%') %>%
        str_replace('\\.', ',')
    )) +
  geom_rect(aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  )) +
  geom_text(
    aes(y = lab_pos),
    family = 'Calibri',
    size = roz_cz_txt,
    color = 'white'
  ) +
  geom_text(aes(
    x = typ_pos,
    y = -max(pct) * 1.8,
    label = typ
  ),
  family = 'Calibri',
  size = roz_cz_txt - .2,
  color = rgb(.1, .1, .1),
  hjust = 1,
  check_overlap = TRUE,
  show.legend = FALSE
  ) +
  geom_text(aes(
    y = lab_sum_pos,
    hjust = lab_sum_hj,
    label = lab_sum %>% fmt(),
    color = skoncz
  ),
  family = 'Calibri',
  size = roz_cz_txt
  ) +
  geom_segment(aes(
    x = Inf,
    xend = -Inf,
    y = 0,
    yend = 0
  ),
  color = podstawowy2
  ) +
  geom_segment(
    data = group_by(., typ) %>%
      mutate(
        xmin = min(xmin) + .2,
        xmax = max(xmax) - .2
      ) %>%
      ungroup() %>%
      mutate(
        ymin = -max(pct) * 1.75,
        ymax = -max(pct) * 1.68
      ) -> klamra,
    aes(
      x = xmax,
      xend = xmin,
      y = ymin,
      yend = ymin
    ),
    color = podstawowy2,
    size = .5 -> lwd,
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = klamra,
    aes(
      x = xmax,
      xend = xmax + .2,
      y = ymin,
      yend = ymax
    ),
    color = podstawowy2,
    size = lwd,
    curvature = -.4,
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = klamra,
    aes(
      x = xmin,
      xend = xmin - .2,
      y = ymin,
      yend = ymax
    ),
    color = podstawowy2,
    size = lwd,
    curvature = .4,
    inherit.aes = FALSE
  ) +
  coord_flip() +
  facet_grid(
    typ ~ .,
    scales = 'free',
    space = 'free'
  ) +
  scale_fill_manual(values = c(
      '#35c2bd',
      '#f62a66',
      '#c1c0b9',
      '#00bbf0'
    ),
    labels = c(
      'odsetek studentów, którzy ukończyli studia w ciągu 6 semestrów',
      'odsetek studentów, ktorzy ukończyli studia po 6 semestrze',
      'odsetek studentów, którzy dotychczas nie ukończyli studiów',
      'odsetek studentów skreślonych'
    ),
    breaks = pull(., kiedy) %>%
      unique() %>%
      .[c(1, 4, 3, 2)]
  ) +
  scale_color_manual(
    values = plus_minus %>% rev(),
    labels = c(
      'liczba studentów, którzy nie ukończyli studiów',
      'liczba studentów, którzy ukończyli studia'
    )) +
  scale_y_continuous(
    breaks = seq(-.6, .6, length.out = 5),
    labels = (seq(-.6, .6, length.out = 5) * 100) %>% str_c('%') %>% str_remove('-'),
    limits = c(max(.$pct) * -2.55, max(.$pct) * 1.4)
  ) +
  guides(fill = guide_legend(
    keyheight = leg_sq,
    keywidth = leg_sq,
    order = 1
  ), 
  color = guide_legend(
    override.aes = list(label = 'n'),
    keyheight = leg_sq,
    keywidth = leg_sq,
    reverse = TRUE
  )) +
  theme(
    legend.text = element_text(size = 8),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.spacing.x = unit(1, 'mm'),
    strip.text = element_blank(),
    legend.direction = 'vertical',
    legend.box = 'vertical',
    legend.spacing = unit(0, 'mm'),
    legend.box.just = 0,
    legend.margin = margin()
  )

ggsave(
  filename = 'wykresy/w3_22_24_26_v1.png',
  units = 'cm',
  width = 13,
  height = 9,
  bg = 'transparent',
  dpi = 400
)

# ------------------------------

students %>%
  mutate(time = year(time)) %>%
  filter(
    iscedf13 == "Total",
    isced11 %in% c(
      "Short-cycle tertiary education",
      "Bachelor's or equivalent level",
      "Master's or equivalent level"
    ),
    sex == "Total",
    time %in% c(2013, 2016)
  ) %>%
  select(geo, time, values) %>%
  group_by(geo, time) %>%
  summarise(values = sum(values, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(eu_trans, by = c("geo" = "eng")) %>%
  select(geo.y, time, values) %>%
  rename_at(1, ~ "geo") %>%
  spread(time, values) %>%
  mutate(
    pct = `2016` / `2013` - 1,
    geo = reorder(geo, pct)
  ) %.>%
  ggplot(
    data = .,
    aes(
      x = geo,
      y = pct,
      color = pct >= 0,
      fill = pct >= 0,
      label = abs(pct * 100) %>% round()
    )) +
  geom_linerange(aes(
    ymin = min(pct) * 1.1,
    ymax = 0
  ),
  alpha = .3,
  color = rgb(.4, .4, .4),
  linetype = "dotted"
  ) +
  geom_linerange(aes(
      ymin = 0,
      ymax = pct
    ),
    color = rgb(.4, .4, .4)
  ) +
  geom_point(
    shape = 21,
    size = 5,
    color = "white"
  ) +
  geom_text(
    family = "Calibri",
    size = roz_cz_txt - .5,
    color = "white"
  ) +
  geom_shadowtext(aes(
    y = pct + max(pct) * .07,
    label = "%"
  ),
  family = "Calibri",
  size = roz_cz_txt - .2,
  bg.color = "white"
  ) +
  geom_shadowtext(aes(
    y = pct - max(pct) * .055,
    label = if_else(pct >= 0, "", "-")
  ),
  family = "Calibri",
  size = roz_cz_txt + .2,
  bg.color = "white"
  ) +
  coord_flip() +
  scale_fill_manual(values = plus_minus %>% rev()) +
  scale_color_manual(values = plus_minus %>% rev()) +
  theme(
    axis.text.x = element_blank(),
    legend.position = "none"
  ) +
  ylim(min(.$pct) * 1.1, max(.$pct) * 1.1)

ggsave(
  filename = 'wykresy/w3_X1_v2.png',
  units = 'cm',
  width = 13,
  height = 11,
  bg = 'transparent',
  dpi = 600
)

# W 3.21. Współczynnik ukończenia kształcenia (completion rate) w wybranych państwach w 2015 roku ----

W3_21 %>%
  arrange(desc(isced6)) %>%
  group_by(typ) %>%
  mutate(
    lp = row_number(),
    lp = if_else(typ == "metoda cross-cohort", lp + 17.5, lp + 0)
  ) %>%
  gather(isced, n, c(isced6, isced7)) %>%
  mutate(
    n = n / 100,
    lab = case_when(
      n == 0 & brak == "a" ~ "**",
      n == 0 & brak == "m" ~ "*",
      TRUE ~ round(n, 2) %>% scales::percent()
    ),
    śr = geo %like% ".*Śr.*"
  ) %.>%
  ggplot(
    data = .,
    aes(
      x = lp,
      y = n,
      fill = isced,
      label = lab
    )) +
  geom_rect(data = tibble(
    xmin = filter(., geo == "Średnia") %>% pull(lp) - .53,
    xmax = filter(., geo == "Średnia") %>% pull(lp) + .53,
    ymin = -Inf,
    ymax = max(.$n) * 1.13
  ),
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax
  ),
  inherit.aes = FALSE,
  fill = podstawowy2,
  alpha = .1
  ) +
  geom_col(
    position = "dodge",
    width = .8
  ) +
  geom_linerange(aes(
    x = 17.25,
    ymin = -Inf,
    ymax = max(.$n) * 1.3
  ),
  color = rgb(.5, .5, .5)
  ) +
  geom_line(data = tibble(
    x = pull(., lp) %>% range(),
    y = rep(max(.$n) * 1.15, 2)
  ),
  aes(
    x = x,
    y = y,
    group = "x"
  ),
  inherit.aes = FALSE,
  alpha = .3,
  lwd = .3
  ) +
  geom_text(data = tibble(
    x = group_by(., typ) %>% summarise(x = mean(lp)) %>% pull(x),
    y = rep(max(.$n) * 1.2, 2),
    lab = pull(., typ) %>% unique()
  ),
  aes(
    x = x,
    y = y,
    label = lab
  ),
  inherit.aes = FALSE,
  family = "Calibri",
  size = roz_cz_txt
  ) +
  geom_text(
    aes(y = n + max(n) * .005),
    position = position_dodge(.9),
    family = "Calibri",
    angle = 90,
    size = roz_cz_txt - .5,
    hjust = 0
  ) +
  geom_text(
    data = filter(., geo != "Średnia"),
    aes(
      y = -max(n) * .02,
      label = geo
    ),
    family = "Calibri",
    angle = 90,
    size = roz_cz_txt - .5,
    hjust = 1,
    check_overlap = TRUE
  ) +
  geom_text(
    data = filter(., geo == "Średnia"),
    aes(
      y = -max(n) * .02,
      label = geo
    ),
    family = "Calibri",
    angle = 90,
    fontface = "bold",
    size = roz_cz_txt,
    hjust = 1,
    check_overlap = TRUE
  ) +
  guides(fill = guide_legend(
    keyheight = leg_sq,
    keywidth = leg_sq
  )) +
  scale_y_continuous(
    breaks = seq(0, 1, length.out = 6),
    labels = scales::percent,
    limits = c(-max(.$n) * .38, max(.$n) * 1.3)
  ) +
  scale_fill_manual(
    labels = c(
      "studia licencjackie i ich odpowiednik (ISCED 6)",
      "studia magisterskie i ich odpowiednik (ISCED 7)"),
    values = c(podstawowy2, podstawowy)
  ) +
  scale_color_manual(values = c("white", paleta_ald[2])) +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.spacing.x = unit(1, "mm"),
    legend.position = c(.5, 1),
    legend.direction = "horizontal"
  )

ggsave(
  filename = 'wykresy/w3_21_v1.png',
  units = 'cm',
  width = 21,
  height = 11,
  bg = 'transparent',
  dpi = 600
)

# --------------------------------------------

umiedz$`W 7.` %>%
  rename_all(~str_remove(., 'liczba studentów ')) %>%
  slice(4) %>%
  gather(typ, n, -1) %>%
  select(-1) %>%
  separate(typ, c('typ', 'rok'), sep = ' ') %>%
  mutate(
    pct = umiedz$`W 8.` %>% slice(4) %>% gather(typ, n, -1) %>% pull(n),
    pct_pos = pct * max(n, na.rm = TRUE) * 40 + max(n, na.rm = TRUE) * .6
  ) %>%
  filter(
    !is.na(n),
    rok != '2012/2013'
  ) %>%
  group_by(rok) %>%
  nest() %>%
  mutate(xpos = row_number()) %>%
  unnest() %.>%
  ggplot(
    data = .,
    aes(
      x = xpos,
      y = n,
      fill = typ,
      color = typ,
      label = n %>% fmt()
    )) +
  geom_line(
    aes(y = pct_pos),
    position = position_dodge(.9),
    linetype = 'dotted',
    alpha = .3
  ) +
  geom_col(
    position = position_dodge(.9),
    col = 'white'
  ) +
  geom_point(
    aes(y = pct_pos),
    position = position_dodge(.9),
    size = 7
  ) +
  geom_text(
    aes(y = n + max(n) * .04),
    position = position_dodge(.9),
    family = 'Calibri',
    size = roz_cz_txt,
    color = rgb(.1, .1, .1)
  ) +
  geom_text(
    aes(
      y = pct_pos,
      label = (pct * 100) %>% round(1) %>% format(decimal.mark = ',')
    ),
    position = position_dodge(.9),
    family = 'Calibri',
    color = 'white',
    size = roz_cz_txt
  ) +
  geom_text(
    aes(
      y = pct_pos,
      x = xpos + .15,
      label = '%'
    ),
    position = position_dodge(.9),
    family = 'Calibri',
    size = roz_cz_txt
  ) +
  geom_text(
    data = tibble(x = c(1.2, 2.7)),
    aes(
      x = x,
      y = max(.$pct_pos) * 1.24,
      label = str_c(
        c('przyjeżdżający', 'wyjeżdżający'),
        ':\n      udział w ogóle studentów\n      liczba'
      )
    ),
    family = 'Calibri',
    hjust = 0,
    size = roz_cz_txt,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = tibble(x = c(1.25, 2.75, 1.25, 2.75)),
    aes(
      x = x,
      y = max(.$pct_pos) * c(1.24, 1.24, 1.16, 1.16) ,
      color = c('1', '2', '1', '2'),
      shape = c('1', '1', '2', '2')
    ),
    size = 4.5,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = plus_minus) +
  scale_shape_manual(values = 16:15) +
  scale_color_manual(values = plus_minus %>% rep(2)) +
  scale_x_continuous(
    breaks = 1:4,
    labels = pull(., rok) %>% unique()
  ) +
  theme(
    legend.position = 'none',
    axis.text.y = element_blank()
  ) +
  ylim(c(NA, max(.$pct_pos) * 1.28))

ggsave(
  filename = 'wykresy/wUX1_v1.png',
  units = 'cm',
  width = 13,
  height = 9,
  bg = 'transparent',
  dpi = 600
)

# R UZ.9. studenci z Polski na uczelniach za granicą (pełne studia) ----

mob <- read.csv('dane/mobilnosc.txt', col.names = 'all', sep = ';')

dane <- 
  mob %>%
  as_tibble() %>%
  mutate(
    n = str_extract(all, '^[0-9,]+') %>%
      str_remove(',') %>%
      as.numeric(), 
    year = str_extract(all, '\\d{4}'),
    dest = str_extract(all, 'in .*$') %>%
      str_remove('in ') %>%
      str_remove(' \\(.*$'),
    dest = case_when(
      dest == 'Bosnia/Herzegovina' ~ 'Bosnia and Herzegovina',
      dest == 'Czechia' ~ 'Czech Republic',
      dest == 'Iran' ~ 'Iran (Islamic Republic of)',
      dest == 'Korea' ~ "Korea, Democratic People's Republic of",
      dest == 'Russian Federation' ~ 'Russia',
      TRUE ~ dest
    ),
    bins = cut(
      n,
      breaks = c(-Inf, 100, 1000, 5000, Inf),
      labels = c('1 - 100', '101 - 1000', '1001 - 5000', '> 5000')
  )) %>%
  filter(
    !is.na(dest),
    !is.na(n)
  ) %>%
  group_by(dest) %>%
  slice(1) %>%
  select(-all) %>%
  filter(n > 0)

wrld_center <-
  wrld %>%
  spTransform(CRS("+proj=robin")) %>%
  coordinates() %>%
  as_tibble() %>%
  set_names(c("long", "lat")) %>%
  mutate(cntr_id = wrld@data$NAME) %>%
  left_join(dane, by = c("cntr_id" = "dest")) %>%
  mutate(
    long_ = if_else(cntr_id == "Poland", long, NA_real_),
    lat_ = if_else(cntr_id == "Poland", lat, NA_real_),
    size = case_when(
      bins == levels(bins)[1] ~ .2,
      bins == levels(bins)[2] ~ .4,
      TRUE ~ .6)) %>%
  fill(long_, lat_, .direction = "up") %>%
  fill(long_, lat_, .direction = "down") %>%
  filter(!is.na(bins))

eur_center <-
  wrld %>%
  spTransform(CRS("+init=epsg:4326")) %>%
  coordinates() %>%
  as_tibble() %>%
  set_names(c("long", "lat")) %>%
  mutate(
    cntr_id = wrld@data$NAME,
    long = case_when(
      cntr_id == "Norway" ~ long - 4,
      cntr_id == "Sweden" ~ long - 2,
      cntr_id == "United Kingdom" ~ long + 1,
      TRUE ~ long
    ),
    lat = case_when(
      cntr_id == "Norway" ~ lat - 3,
      cntr_id == "Finland" ~ lat - 2,
      TRUE ~ lat
    )) %>%
  left_join(dane, by = c("cntr_id" = "dest")) %>%
  mutate(
    long_ = if_else(cntr_id == "Poland", long, NA_real_),
    lat_ = if_else(cntr_id == "Poland", lat, NA_real_)
  ) %>%
  fill(long_, lat_, .direction = "up") %>%
  fill(long_, lat_, .direction = "down") %>%
  filter(!is.na(n)) %>%
  filter(long %>% between(-24, 36),
         lat %>% between(36, 67))

grat <-
  readOGR("mapa/ne_110m_graticules_30.shp", "ne_110m_graticules_30") %>%
  spTransform(CRS("+proj=robin")) %>%
  fortify()

clean_map <- function(shp, proj, dane) {
  shp %>%
    spTransform(CRS(proj)) %>%
    rmapshaper::ms_simplify(keep = .1) %>%
    tidy(region = "NAME") %>%
    left_join(
      dane %>% select(dest, n, bins),
      by = c("id" = "dest")
    ) %>%
    mutate(clr = if_else(id == "Poland", 0, n))
}

wrld_epg <-
  wrld %>%
  clean_map("+init=epsg:4326", dane)

wrld_rob <-
  wrld %>%
  clean_map("+proj=robin", dane)

paleta_42 <- c(
  rgb(8, 99, 146, maxColorValue = 255), # ciemny
  rgb(29, 111, 184, maxColorValue = 255),
  rgb(3, 156, 188, maxColorValue = 255),
  rgb(96, 195, 226, maxColorValue = 255)
) # jasny

wrld_rob %>%
  filter(lat > -6e6) %.>%
  ggplot(
    data = .,
    aes(
      x = long,
      y = lat
    )) +
  geom_path(data = grat,
            aes(group = group),
            linetype = "dotted",
            color = 'steelblue',
            alpha = .3) +
  geom_polygon(aes(group = group,
                   fill = clr),
               col = 'steelblue',
               size = .00001,
               show.legend = FALSE) +
  geom_curve(data = wrld_center %>% filter(long >= 1567190),
             aes(x = long_,
                 y = lat_,
                 xend = long,
                 yend = lat,
                 size = bins,
                 color = bins),
             curvature = -.3,
             arrow = arrow(type = 'closed', angle = 20, length = unit(1, 'mm')),
             alpha = .8) +
  geom_curve(data = wrld_center %>% filter(long < 1567190),
             aes(x = long_,
                 y = lat_,
                 xend = long,
                 yend = lat,
                 size = bins,
                 color = bins),
             curvature = .3,
             arrow = arrow(type = 'closed', angle = 20, length = unit(1, 'mm')),
             alpha = .8) +
  theme_map() +
  theme(legend.position = c(.37, -.22),
        legend.title = element_blank(),
        legend.text = element_text(family = "Calibri"),
        legend.text.align = 1,
        legend.margin = margin(0, 0, 0, 0)) +
  scale_color_manual(values = paleta_42[c(1, 4, 2, 3)]) +
  scale_fill_gradient(
    low = rgb(.9, .9, .9),
    high = szary_poz,
    na.value = 'white'
  ) +
  scale_size_manual(
    values = c(.4, .7, .1, 1.1) %>% rev(),
    labels = c(' 1 - 100', ' 101 - 1000', ' 1001 - 5000', ' > 5000')
  ) +
  guides(size = guide_legend(
    override.aes = list(
      color = paleta_42[4:1],
      size = c(1.1, .7, .4, .1) %>% rev())
  ),
  color = FALSE) +
  coord_equal() +
  theme(legend.text.align = 0) +
  annotation_custom(ggplotGrob(
    ggplot(
      data = subset(wrld_epg, long > -100),
      aes(
        x = long,
        y = lat
      )) +
      geom_polygon(aes(group = group,
                       fill = clr),
                   col = 'steelblue',
                   size = .00001,
                   alpha  = .8) +
      geom_segment(data = eur_center,
                   aes(x = long_,
                       y = lat_,
                       xend = long,
                       yend = lat,
                       size = bins,
                       color = bins),
                   arrow = arrow(type = 'closed', angle = 20, length = unit(1, 'mm')),
                   alpha = .8) +
      coord_map(
        xlim = c(-24, 36),
        ylim = c(36, 67)
      ) +
      theme_map() +
      theme(legend.position = "none",
            panel.border = element_rect(color = 'steelblue',
                                        fill = NA),
            panel.background = element_rect(color = "white"),
            plot.margin = margin(0, 0, 0, 0)) +
      scale_color_manual(values = paleta_42[c(4, 3, 2, 1)]) +
      scale_fill_gradient(
        low = rgb(.9, .9, .9),
        high = szary_poz,
        na.value = 'white'
      ) +
      scale_size_manual(values = c(1.1, .7, .4, .1) %>% rev())),
    xmin = -2e6,
    xmax = 14e6,
    ymin = -14e6,
    ymax = -40e5)

ggsave(
  filename = 'wykresy/rUZ9_v2.png',
  units = 'cm',
  width = 15,
  height = 14,
  bg = 'transparent',
  dpi = 400
)

# W UZD. 2. Liczba i odsetek cudzoziemców wśród studentów w podziale na studia I i II stopnia i jednolite magisterskie ----

pal <- c('#933f99', '#005792', '#278ea5') %>% rev()

umiedz$`D 3.` %>%
  as_cells() %>%
  behead('N', rok) %>%
  behead('N', ucz) %>%
  behead('W', typ) %>%
  fill(rok, ucz, typ) %>%
  filter(
    ucz == 'ogółem',
    typ != 'ogółem',
    chr != '%'
  ) %>%
  mutate(
    n = as.numeric(chr),
    pct = if_else(n > 1, 'n', 'pct')
  ) %>%
  select(c(5, 7:9)) %>%
  spread(pct, n) %>%
  mutate(
    rok = rok %>% as.numeric(),
    pct_pos = pct * max(n) * 3,
    hj = case_when(
      typ == 'studia jednolite magisterskie' ~ .35,
      typ == 'studia II stopnia' ~ .65,
      typ == 'studia I stopnia' ~ .5
    ),
    pct_pos = pct_pos * 3 + max(n) * 0.7,
    x_pos = case_when(
      typ == 'studia II stopnia' ~ rok + .2,
      typ == 'studia I stopnia' ~ rok - .2,
      TRUE ~ rok
  )) %>%
  ggplot(aes(
    x = rok,
    y = n,
    fill = typ,
    color = typ,
    label = n %>% fmt()
  )) +
  geom_col(
    position = position_dodge(.9),
    show.legend = FALSE,
    col = 'white',
    lwd = .2
  ) +
  geom_label(aes(
      y = n + max(n) * .028,
      hjust = hj
    ),
    position = position_dodge(.9),
    family = 'Calibri',
    fill = 'white',
    label.padding = unit(.07, 'lines'),
    label.size = .001,
    size = 3
  ) +
  geom_line(aes(
      y = pct_pos,
      x = x_pos,
      group = typ
    ),
    color = podstawowy2,
    linetype = 'dotted'
  ) +
  geom_point(aes(
      y = pct_pos,
      x = x_pos
    ),
    size = 6 
  ) +
  geom_text(aes(
    y = pct_pos,
    x = x_pos,
    label = (pct * 100) %>%
      round(1) %>%
      str_replace('\\.', ',')
  ),
  family = 'Calibri',
  color = 'white',
  size = 3
  ) +
  geom_shadowtext(aes(
    y = pct_pos,
    x = x_pos + .2,
    label = '%'
  ),
  family = 'Calibri',
  size = 3,
  bg.color = 'white'
  ) +
  guides(
    fill = guide_legend(override.aes = list(
      size = 4,
      shape = 15,
      color = pal),
      title = 'liczba cudzoziemców\nna studiach:',
      keyheight = leg_sq,
      keywidth = leg_sq,
      order = 1
    ),
    color = guide_legend(
      override.aes = list(size = 4),
      title = 'udział cudzoziemców wśród\nstudentów na studiach:',
      keyheight = leg_sq,
      keywidth = leg_sq,
      order = 2
    )) +
  scale_x_continuous(breaks = 2012:2017) +
  scale_fill_manual(
    values = pal,
    labels = c('I stopnia', 'II stopnia', 'jednolitych magisterskich')
  ) +
  scale_color_manual(
    values = pal,
    labels = c('I stopnia ', 'II stopnia', 'jednolitych magisterskich')
  ) +
  theme(
    axis.text.y = element_blank(),
    legend.direction = 'vertical',
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )

ggsave(
  filename = 'wykresy/wUZD_2_v2.png',
  units = 'cm',
  width = 13,
  height = 12,
  bg = 'transparent',
  dpi = 400
)

# R 4.1. Umowy dwustronne o współpracy naukowej i zakresie szkolnictwa wyższego ----

# to do szkolnictwa!

dane <-
  dwustr %>%
  select(c(1, 2, 4)) %>%
  set_names(c('geo', 'ISO2', 'n')) %>%
  filter(!n %in% c('41295', NA)) %>%
  mutate(
    lp = row_number(),
    i = 1,
    n = case_when(
      n %in% c('N/S', 'NSK', 'SN') ~ 'NS',
      n == 's' ~ 'S',
      TRUE ~ n
    )
  ) %>%
  spread(n, i, fill = 0) %>%
  group_by(geo, ISO2) %>%
  summarise_if(is.double, sum) %>%
  ungroup() %>%
  mutate(index = rowSums(.[3:5]))

wrld_rob <-
  wrld %>%
  spTransform(CRS('+proj=robin'))

wrld_naz <-
  coordinates(wrld_rob) %>%
  as_tibble() %>%
  set_names(c("long", "lat")) %>%
  mutate(ISO2 = wrld_rob@data$ISO2) %>%
  left_join(dane[dane$index > 3,], by = 'ISO2') %>%
  filter(!is.na(geo))

wrld_rob <- 
  wrld_rob %>%
  rmapshaper::ms_simplify(keep = .2) %>%
  tidy(region = "ISO2") %>%
  left_join(
    dane,
    by = c("id" = "ISO2")
  )

n_row <- 2

wrld_naz <-
  wrld_naz %>%
  mutate(
    long2 = case_when(
      geo == 'Argentyna' ~ long - 3e6,
      geo == 'Austria' ~ long - 35e5,
      geo == 'Czechy' ~ long + 1e6,
      geo == 'Niemcy' ~ long - 9e5,
      geo == 'Grecja' ~ long - 55e5,
      geo == 'Gwinea' ~ long - 15e5,
      geo == 'Republika Korei' ~ long + 3e6,
      geo == 'Słowacja' ~ long + 3e6,
      geo == 'Turcja' ~ long - 60e5,
      geo == 'Ukraina' ~ long + 45e5,
      geo == 'Wietnam' ~ long - 3e6
    ),
    lat2 = case_when(
      geo == 'Argentyna' ~ lat + 2e6,
      geo == 'Austria' ~ lat + 45e5,
      geo == 'Czechy' ~ lat + 40e5,
      geo == 'Niemcy' ~ lat + 32e5,
      geo == 'Grecja' ~ lat,
      geo == 'Gwinea' ~ lat - 3e6,
      geo == 'Republika Korei' ~ lat - 1e6,
      geo == 'Słowacja' ~ lat + 34e5,
      geo == 'Turcja' ~ lat - 2e6,
      geo == 'Ukraina' ~ lat + 40e5,
      geo == 'Wietnam' ~ lat - 3e6
    ),
    long3 = case_when(
      geo %in% c(
        'Grecja',
        'Turcja',
        'Gwinea',
        'Wietnam'
      ) ~ long2 + 1e6,
      geo == 'Republika Korei' ~ long2 - 1e6,
      TRUE ~ long2
    ),
    lat3 = if_else(
      geo %in% c(
        'Argentyna',
        'Austria',
        'Czechy',
        'Niemcy',
        'Słowacja',
        'Ukraina'
      ),
      lat2 - 1e6,
      lat2
    )
  )

subplots <-
  wrld_naz %>%
  mutate(mar = index / n_row) %>%
  select(N, S, NS, long2, lat2, mar) %>%
  pmap(function(N, S, NS, long2, lat2, mar) {
    annotation_custom(
      ggplotGrob(
        waffle(
          c(NS, N, S),
          legend_pos = "none",          
          size = .3,
          rows = n_row,
          colors = paleta_ald[c(2, 4, 7)]
        )
      ),
      xmin = long2 - mar * 1e6,
      xmax = long2 + mar * 1e6,
      ymin = lat2 - 1e6,
      ymax = lat2 + 1e6)
  })

linia <- '#9a1ba0'

main <-
  wrld_rob %>%
  filter(lat > -6e6) %.>%
  ggplot(
    data = .,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = index
    )
  ) +
  geom_path(
    data = grat,
    aes(fill = NULL),
    linetype = "dotted",
    color = rgb(.8, .8, .8)
  ) +
  geom_polygon(
    col = 'steelblue',
    lwd = .001,
    alpha = .8
  ) +
  geom_text(
    data = wrld_naz,
    aes(
      x = long2,
      y = lat2 + 7e5,
      label = geo %>% str_wrap(10)
    ),
    family = 'Calibri',
    size = 3,
    lineheight = .7,
    vjust = 0,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = wrld_naz,
    aes(
      x = long3,
      y = lat3,
      color = c('x' %>% rep(9), 'y', 'z')
    ),
    size = .9,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = wrld_naz,
    aes(
      x = long,
      y = lat
    ),
    size = .9,
    color = linia,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = wrld_naz %>%
      filter(geo %in% c(
        'Argentyna',
        'Austria',
        'Ukraina'
      )),
    aes(
      x = long3,
      xend = long3,
      y = lat3,
      yend = lat
    ),
    color = linia,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = wrld_naz %>%
      filter(geo %in% c(
        'Argentyna',
        'Austria',
        'Ukraina'
      )),
    aes(
      x = long3,
      xend = long,
      y = lat,
      yend = lat
    ),
    color = linia,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = wrld_naz %>%
      filter(!geo %in% c(
        'Argentyna',
        'Austria',
        'Ukraina'
      )),
    aes(
      x = long3,
      xend = long,
      y = lat3,
      yend = lat3
    ),
    color = linia,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = wrld_naz %>%
      filter(!geo %in% c(
        'Argentyna',
        'Austria',
        'Ukraina'
      )),
    aes(
      x = long,
      xend = long,
      y = lat3,
      yend = lat
    ),
    color = linia,
    inherit.aes = FALSE
  ) +
  theme_map() +
  scale_fill_gradientn(colors = c(
    "#f0fff3",
    "#2796cb",
    "#062743",
    'black'
  ),
  na.value = 'white'
  ) +
  scale_color_manual(
    values = linia %>% rep(3),
    breaks = c('x', 'y', 'z'),
    labels = c('nauki', 'szkolnictwa wyższego', 'nauki i szkolnictwa wyższego')
  ) +
  guides(fill = guide_colorbar(
    barheight = unit(4, "mm"),
    direction = 'horizontal',
    label.position = 'bottom',
    title.position = 'top',
    title.hjust = .5,
    title.vjust = -39,
    label.vjust = 40,
    order = 2,
    title = 'liczba podpisanych umów'
  ),
  color = guide_legend(override.aes = list(
    shape = 15,
    size = 4,
    color = paleta_ald[c(4, 7, 2)]
  ),
  keywidth = .75,
  keyheight = .75,
  order = 1,
  title = 'współpraca w zakresie:'
  )) +
  theme(
    legend.position = c(.5, .32),
    legend.title = element_text(family = 'Calibri', size = 8),
    legend.text = element_text(family = "Calibri"),
    legend.background = element_rect(fill = "transparent"),
    legend.justification = .5,
    legend.direction = 'horizontal',
    legend.box.just = .5,
    legend.margin = margin(),
    legend.spacing.y = unit(3.65, 'cm')
  ) +
  coord_equal()

main + subplots

ggsave(
  filename = 'wykresy/R_um_dwstr_v1.png',
  units = 'cm',
  width = 15,
  height = 14,
  bg = 'transparent',
  dpi = 400
)