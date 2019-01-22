# libraries --------------------------------------------------------------------

library(shadowtext)
library(DescTools)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(broom)
library(wrapr)
library(rgdal)
library(waffle)

# settings ---------------------------------------------------------------------

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
    legend.position = 'top',
    legend.direction = 'horizontal',
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
  rgb(82, 104, 165, maxColorValue = 255),
  rgb(0, 115, 124, maxColorValue = 255),
  rgb(170, 22, 82, maxColorValue = 255),
  rgb(8, 99, 146, maxColorValue = 255),
  rgb(29, 111, 184, maxColorValue = 255),
  rgb(3, 156, 188, maxColorValue = 255),
  rgb(96, 195, 226, maxColorValue = 255)
)

fmt <- function(x, n = 0) {
  x %>%
    round(n) %>%
    format(big.mark = " ", decimal.mark = ',', trim = TRUE)
}

fmt2 <- function(x, n = 1) {
  (x / 1e6) %>%
    round(n) %>%
    format(decimal.mark = ",", big.mark = ' ', trim = TRUE)
}
  
# read-in the data -------------------------------------------------------------

data_list <- list()

for (i in dir('data/')) {
  data_list[[str_remove(i, '\\..*$')]] <- read_csv2(paste0('data/', i))
}

# Stacked barplot with totals --------------------------------------------------

data_list$plot1 %>%
  gather(type, n, 2:4) %>%
  ggplot(aes(
    x = year,
    y = n,
    fill = type,
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
    y = total + max(total) * .06,
    label = total %>% fmt()),
    family = 'Calibri',
    size = roz_cz_txt,
    fill = 'white',
    color = colors[1],
    label.padding = unit(0.1, 'lines'),
    show.legend = FALSE
  ) +
  geom_label(aes(
    x = 2015.78,
    y = 500,
    label = '    '
  ),
  size = 2.5,
  fill = 'white',
  color = colors[1],
  label.padding = unit(0.1, 'lines')
  ) +
  geom_text(aes(
    x = 2015.88,
    y = 500,
    label = 'total'
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
  scale_fill_manual(values = colors[c(4, 3, 8)]) +
  theme(
    axis.text.y = element_blank(),
    legend.spacing.x = unit(1, 'mm'),
    legend.position = c(.45, .956),
    plot.margin = margin(40, 5, 5, 5)
  )

ggsave(
  filename = 'plots/plot1.png',
  units = 'cm',
  width = 15,
  height = 8.7,
  bg = 'white',
  dpi = 600
)

# Dodged barplot with percentages ----------------------------------------------

data_list$plot2 %>%
  filter(year == 2017) %>% 
  select(-1) %>%
  gather(woj, n, -type) %>%
  spread(type, n) %>%
  mutate(wsp = accepted / submitted) %>%
  gather(type, n, c(accepted, submitted)) %>%
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
      fill = type,
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
      shape = 'success rate'
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
    color = colors[4],
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
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.spacing.x = unit(1, 'mm'),
    legend.box.just = 'left',
    legend.margin = margin(0, 0, 0, 0)
  )

ggsave(
  filename = 'plots/plot2.png',
  units = 'cm',
  width = 13,
  height = 12,
  bg = 'white',
  dpi = 600
)

# Lollipop plot with totals ----------------------------------------------------

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
  geom_line(
    data = select(., year, n) %>%
      transmute(
        y = max(n) * 1.02,
        x1 = year - .4,
        x2 = year + .4
      ) %>%
      unique() %>%
      mutate(gr = row_number()) %>%
      gather('i', 'x', x1:x2),
    aes(
      x = x,
      y = y,
      group = gr
    ),
    color = colors[1]
  ) +
  geom_curve(data = tibble(
      x = c(unique(.$year) + .4, unique(.$year) - .45),
      x2 = c(unique(.$year) + .45, unique(.$year) - .4),
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
      x = c(unique(.$year) - .45, unique(.$year) + .45),
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
  filename = 'plots/plot3.png',
  units = 'cm',
  width = 13,
  height = 14,
  bg = 'white',
  dpi = 600
)

# Point-line percentage plot ---------------------------------------------------

data_list$plot4 %>%
  gather('typ', 'wsp', -year) %>%
  mutate(
    wsp = as.numeric(wsp),
    xpos = case_when(
      typ == 'group 4' &
        year %in% c(2012:2014, 2015, 2017) ~ year - .15,
      typ == 'group 1' &
        year %in% c(2012, 2014) ~ year + .15,
      typ == 'group 5' &
        year == 2013 ~ year + .15,
      typ == 'group 5' &
        year == 2017 ~ year + .28,
      typ == 'group 2' &
        year == 2015 ~ year + .15,
      TRUE ~ year
  )) %.>%
  ggplot(
    data = .,
    aes(
      x = xpos,
      y = wsp,
      color = typ,
      fill = typ,
      label = (wsp * 100) %>% round()
  )) +
  geom_line(
    data = tibble(
      x = c(unique(.$year)[-1] - .5) %>% rep(2),
      y = min(.$wsp)
    ) %>%
      mutate(
        y = c(min(y), Inf) %>% rep(each = length(x) / 2),
        gr = 1:(length(x) / 2) %>% rep(2)
      ),
    aes(
      x = x,
      y = y,
      group = gr
    ),
    color = colors[1],
    alpha = .3,
    linetype = 'dotted',
    lwd = .2,
    inherit.aes = FALSE
  ) +
  geom_line(
    lwd = .5,
    alpha = .5
  ) +
  geom_point(
    size = 5,
    shape = 21,
    stroke = 0,
    color = 'white'
  ) +
  geom_text(
    family = 'Calibri',
    color = 'white',
    size = roz_cz_txt - .4
  ) +
  geom_shadowtext(aes(
      label = '%',
      x = xpos + .15
    ),
    family = 'Calibri',
    size = roz_cz_txt - .4,
    bg.color = 'white'
  ) +
  scale_x_continuous(breaks = unique(.$year)) +
  scale_color_manual(values = colors[c(2, 4, 8, 7, 3)] %>% rev()) +
  scale_fill_manual(values = colors[c(2, 4, 8, 7, 3)] %>% rev()) +
  theme(
    axis.text.y = element_blank(),
    legend.position = c(.5, 1.02),
    plot.margin = margin(20, 1, 1, 1),
    legend.spacing.x = unit(1, 'mm'),
    legend.direction = 'horizontal'
  )

ggsave(
  filename = 'plots/plot4.png',
  units = 'cm',
  width = 13,
  height = 10,
  bg = 'white',
  dpi = 600
)

# Dodged barplot with counts and percentages -----------------------------------

data_list$plot5 %>%
  mutate(
    kategoria = kategoria %>% factor(levels = paste('category', c('A+', 'A', 'B', 'C'))),
    wsp_pos = wsp * max(val) * 1.5 - max(val) * .8,
    wsp_lab = wsp_pos - max(val) * .1,
    txt_pos = if_else(
      typ == 'mean number of accepted submissions per unit' &
        kategoria == 'kategoria C',
      val + max(val) * .07,
      val + max(val) * .05
    ),
    typ2 = if_else(
      typ == 'mean number of accepted submissions per unit',
      'number of units',
      'x')
    ) %>% 
  ggplot(aes(
    x = kategoria,
    y = val,
    fill = typ %>% fct_rev(),
    label = (val * 1e6) %>% fmt2()
  )) +
  geom_col(
    position = 'dodge',
    col = 'white'
  ) +
  geom_line(aes(
      y = wsp_pos,
      group = 1
    ),
    linetype = 'dotted',
    color = colors[4],
    lwd = .4
  ) +
  geom_point(aes(
      y = val / 2,
      col = typ2 %>% fct_rev()
    ),
    position = position_dodge(.9),
    shape = 21,
    fill = 'white',
    size = 6.5
  ) +
  geom_text(aes(
      y = val / 2,
      label = n
    ),
    position = position_dodge(.9),
    family = 'Calibri',
    size = roz_cz_txt - .6
  ) +
  geom_point(aes(
      y = wsp_pos,
      shape = 'success rate'
    ),
    size = 6,
    stroke = 0,
    fill = colors[4],
    color = 'white'
  ) +
  geom_text(
    aes(y = txt_pos),
    position = position_dodge(.9),
    family = 'Calibri',
    size = roz_cz_txt
  ) +
  geom_text(aes(
      y = wsp_pos,
      label = (wsp * 100) %>%
        round()
    ),
    family = 'Calibri',
    size = roz_cz_txt,
    color = 'white'
  ) +
  geom_shadowtext(aes(
      y = wsp_pos,
      label = '%'
    ),
    size = roz_cz_txt,
    family = 'Calibri',
    nudge_x = .13,
    color = colors[4],
    bg.color = 'white'
  ) +
  scale_fill_manual(values = colors[c(6, 8)]) +
  scale_color_manual(
    breaks = 'number of units',
    values = colors[c(6, 8)]
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3.3),
      label.position = 'left'
    ),
    fill = guide_legend(
      label.position = 'left',
      reverse = TRUE
    ),
    shape = guide_legend(
      override.aes = list(size = 4),
      label.position = 'left')
  ) +
  scale_shape_manual(values = 21) +
  theme(
    axis.text.y = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0, 'mm'),
    legend.direction = 'vertical',
    legend.box = 'vertical',
    axis.text.x = element_text(vjust = 48),
    legend.key.size = unit(leg_sq, 'lines'),
    legend.box.just = 'right',
    legend.position = c(.68, .85)
  )

ggsave(
  filename = 'plots/plot5.png',
  units = 'cm',
  width = 13,
  height = 12,
  bg = 'white',
  dpi = 600
)

# Facet dodged barplot with percentages ----------------------------------------

legend <-
  tibble(
    obsz = c(rep("2", 12), rep("1", 6)),
    rok = rep("2018", 18),
    zl = c(rep(1, 6), rep(2, 6), rep(3, 6)),
    fin = rep(1:6, 3),
    wsp = c(rep(1, 6), rep(2, 6), rep(1, 6)),
    lab_pos = c(rep(-50, 6), rep(1750, 6), rep(950, 6)),
    wsp_pos = c(
      seq(-500, -100, length.out = 6),
      seq(1300, 1700, length.out = 6),
      seq(500, 900, length.out = 6)
    ),
    rok_pos = rep(NA, 18),
    wsp_lab = c(
      rep("number of accepted projects", 6),
      rep("number of submitted projects", 6),
      rep("% success rate", 6)
    ))

data_list$plot6a %>% 
  gather(rok, val, -obsz) %>%
  left_join(
    data_list$plot6b %>%
      gather(rok, val, -obsz),
    by = c('obsz', 'rok')
  ) %>%
  rename_at(3:4, ~ c('zl', 'fin')) %>%
  mutate(
    wsp = fin / zl,
    lab_pos = zl - max(zl) * .03,
    wsp_pos = wsp * 2e3 + max(zl) * .92,
    rok_pos = -max(wsp_pos) * .25,
    wsp_lab = (wsp * 100) %>% round() %>% as.character(),
    obsz = obsz %>%
      factor(levels = c(
        '1',
        '2',
        'group 1',
        'group 2',
        'group 3',
        'group 4',
        'group 5',
        'group 6',
        'group 7'
      ) %>% rev()
    ),
    rok = rok %>% factor(levels = 2012:2018)) %>%
  ggplot(aes(
    x = obsz,
    y = zl,
    fill = rok,
    color = rok,
    label = zl)
  ) +
  geom_segment(aes(
      xend = obsz,
      y = zl,
      yend = wsp_pos
    ),
    linetype = 3,
    alpha = .5
  ) +
  geom_col(
    position = position_dodge(.9),
    alpha = .6,
    col = 'white'
  ) +
  geom_col(aes(
      y = fin,
      color = NULL
    ),
    width = .7
  ) +
  geom_text(aes(
      y = fin / 2,
      label = fin
    ),
    color = 'white',
    size = roz_cz_txt - 1
  ) +
  geom_point(
    aes(y = wsp_pos),
    shape = 21,
    size = 4.4,
    color = 'white'
  ) +
  geom_text(aes(
      y = wsp_pos,
      label = wsp_lab
    ),
    color = 'white',
    size = roz_cz_txt - .8,
    family = 'Calibri'
  ) +
  geom_point(aes(
      x = 'group 4',
      y = rok_pos
    ),
    size = 14
  ) +
  geom_text(aes(
      x = 'group 4',
      y = rok_pos,
      label = rok
    ),
    family = 'Calibri',
    color = 'white',
    size = 4.5,
    check_overlap = TRUE
  ) +
  geom_label(
    aes(y = lab_pos),
    family = 'Calibri',
    color = colors[1],
    fill = 'white',
    label.r = unit(0, 'lines'),
    label.padding = unit(0.1, 'lines'),
    label.size = .2,
    size = roz_cz_txt - .8
  ) +
  geom_text(aes(
      y = -70,
      label = obsz
    ),
    size = roz_cz_txt,
    color = rgb(59, 58, 60, maxColorValue = 255),
    hjust = 1,
    family = 'Calibri',
    check_overlap = TRUE
  ) +
  geom_text(aes(
      y = wsp_pos + max(wsp_pos) * .02,
      label = '%'
    ),
    family = 'Calibri',
    hjust = 0,
    size = roz_cz_txt - .3
  ) +
  geom_point(
    data = legend,
    aes(
      y = wsp_pos,
      shape = factor(zl),
      color = factor(fin),
      alpha = factor(wsp)
    ),
    size = 2.2
  ) +
  geom_text(
    data = legend,
    aes(
      y = lab_pos,
      label = wsp_lab
    ),
    family = 'Calibri',
    size = roz_cz_txt - .5,
    color = rgb(59, 58, 60, maxColorValue = 255),
    check_overlap = TRUE,
    hjust = 0
  ) +
  coord_flip() +
  facet_grid(
    rok %>% fct_rev() ~ .,
    scales = 'free',
    space = 'free'
  ) +
  scale_shape_manual(values = c(15, 15, 16)) +
  scale_alpha_manual(values = c(1, .6)) +
  scale_fill_manual(values = colors[c(4, 3, 7, 8, 6, 9, 4, 3, 5, 8, 9, 7)] %>% rev()) +
  scale_color_manual(values = colors[c(7, 8, 6, 9, 4, 3, 5, 8, 9, 7, 4, 3)] %>% rev()) +
  theme(
    legend.position = 'none',
    axis.text = element_blank(),
    strip.text = element_blank()
  )

ggsave(
  filename = 'plots/plot6.png',
  units = 'cm',
  width = 13.5,
  height = 15,
  bg = 'white',
  dpi = 600
)

# Bubble map -------------------------------------------------------------------

woj <-
  readOGR("maps/województwa.shp", "województwa") %>%
  spTransform(CRS("+init=epsg:4326"))

woj_naz <-
  coordinates(woj) %>%
  as_tibble() %>%
  set_names(c("long", "lat")) %>%
  mutate(
    województwo = woj@data$jpt_nazwa_,
    id = as.character(0:15)
  )

woj_df <-
  tidy(woj) %>%
  left_join(
    woj_naz %>%
      select(id, województwo)
  )

spc <- .15

dane_art <-
  data_list$map1 %>%
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
      wjw == 'dolnoslaskie' ~ lat + .07,
      TRUE ~ lat
    ),
    long = case_when(
      wjw == 'zachodniopomorskie' ~ long - .2,
      wjw == 'warminsko-mazurskie' ~ long - .2,
      wjw == 'kujawsko-pomorskie' ~ long - .4,
      wjw == 'swietokrzyskie' ~ long - .09,
      wjw == 'slaskie' ~ long - .07,
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
    fill = 'white',
    col = colors[2] %>% alpha(.6),
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
    size = 2.5,
    hjust = 0,
    color = rgb(.2, .2, .2),
    check_overlap = TRUE
  ) +
  scale_size_area(trans = 'sqrt', max_size = 4.5) +
  scale_color_manual(
    values = colors[c(3, 5, 4)],
    labels = c(
      'category 1',
      'category 2',
      'category 3'
  )) +
  guides(
    size = FALSE,
    color = guide_legend(override.aes = list(
      size = 4,
      color = colors[3:5]
  ))) +
  theme_map() +
  theme(
    legend.title = element_blank(),
    legend.position = c(.1, .05),
    legend.background = element_rect(color = 'transparent', fill = 'transparent')
  )

ggsave(
  filename = 'plots/map1.png',
  units = 'cm',
  width = 13 * 1.2,
  height = 12 * 1.2,
  bg = 'white',
  dpi = 600
)

# Waffle map -------------------------------------------------------------------

woj <-
  readOGR("maps/województwa.shp", "województwa") %>%
  spTransform(CRS("+init=epsg:4326"))

woj_naz <-
  coordinates(woj) %>%
  as_tibble() %>%
  set_names(c("long", "lat")) %>%
  mutate(
    województwo = woj@data$jpt_nazwa_,
    id = as.character(0:15)
  )

woj_df <-
  tidy(woj) %>%
  left_join(
    woj_naz %>%
      select(id, województwo)
  )

woj_sum <-
  woj_naz %>%
  left_join(data_list$map2)

n_row <- 6

main <-
  woj_df %>%
  ggplot(aes(
    x = long,
    y = lat,
    group = group
  )) +
  geom_polygon(
    fill = 'white',
    col = colors[2],
    size = .2
  ) +
  geom_text(
    data = woj_sum,
    aes(label = województwo, group = NULL),
    nudge_y = .14,
    size = roz_cz_txt - .4,
    family = 'Calibri'
  ) +
  geom_text(
    data = woj_sum,
    aes(label = total, group = NULL),
    nudge_y = .29,
    size = roz_cz_txt,
    family = 'Calibri'
  ) +
  theme_void() +
  xlim(c(13.8, 24.2))

subplot <-
  woj_sum %>%
  mutate(mar = total / n_row) %>%
  select(-c(województwo, total, id)) %>%
  pmap(function(
    `group 1`,
    `group 2`,
    `group 3`,
    long,
    lat,
    mar
    ) {
    annotation_custom(
      ggplotGrob(
        waffle(
          c(`group 1`,
            `group 2`,
            `group 3`),
          legend_pos = 'none',          
          size = .3,
          rows = n_row,
          colors = colors[c(9, 6, 4)]
        )
      ),
      xmin = long - mar,
      xmax = long + mar,
      ymin = lat - .28 - .13,
      ymax = lat + .28 - .13)
  })

main + subplot +
  annotation_custom(
    ggplotGrob(
      waffle(
        c(`1 unit of type 1` = 0,
          `1 unit of type 2` = 0,
          `1 unit of type 3` = 1),
        legend_pos = 'bottom',
        colors = colors[c(9, 6, 4)]) +
        theme(legend.text = element_text(
          family = 'Calibri',
          size = roz_cz - .5
        )) +
        guides(fill = guide_legend(
          keywidth = leg_sq,
          keyheight = leg_sq
        ))),
    ymin = 54.7
  )

ggsave(
  filename = 'plots/map2.png',
  units = 'cm',
  width = 13,
  height = 12,
  bg = 'white',
  dpi = 600
)

# Difference plot --------------------------------------------------------------

data_list$plot7 %>%
  slice(-1) %>%
  mutate(
    województwo = reorder(województwo, received),
    clr = received > lost,
    poz_pos = if_else(
      received > lost,
      received + 1e3,
      received - 1e3
    ),
    utr_pos = if_else(
      received < lost,
      lost + 1e3,
      lost - 1e3)
  ) %>%
  gather(poz_utr, n, c(received, lost)) %>%
  mutate(txt_pos = if_else(
    poz_utr == 'received',
    poz_pos,
    utr_pos
  )) %.>%
  ggplot(
    data = .,
    aes(
      x = województwo,
      y = n,
      color = poz_utr,
      label = n %>% fmt()
  )) +
  geom_segment(
    data = . %>% select(województwo, poz_utr, n) %>%
      spread(poz_utr, n),
    aes(
      xend = województwo,
      y = received,
      yend = lost,
      color = NULL,
      label = NULL
    ),
    color = 'grey'
  ) +
  geom_point() +
  geom_text(
    aes(y = txt_pos),
    size = roz_cz_txt,
    family = 'Calibri',
    show.legend = FALSE
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = 'top',
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = colors[c(11, 10)]) +
  ylim(c(-1100, 13000)) +
  coord_flip() +
  guides(fill = guide_legend(
    keywidth = leg_sq,
    keyheight = leg_sq,
    reverse = TRUE
  )) +
  scale_color_manual(values = colors[c(11, 10)])

ggsave(
  filename = 'plots/plot7.png',
  units = 'cm',
  width = 13,
  height = 13,
  bg = 'white',
  dpi = 600
)


# Percentage change lollipop plot ----------------------------------------------

data_list$plot9 %>%
  mutate(
    time = year(time),
    geo = str_remove(geo, '\\(.+$') %>%
      str_replace('Former Yugoslav Republic of Macedonia, the', 'Macedonia')
  ) %>%
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
  spread(time, values) %>%
  mutate(
    pct = `2016` / `2013` - 1,
    geo = reorder(geo, pct)
  ) %>%
  filter(!is.na(pct)) %.>%
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
  scale_fill_manual(values = colors[11:10]) +
  scale_color_manual(values = colors[11:10]) +
  theme(
    axis.text.x = element_blank(),
    legend.position = "none"
  ) +
  ylim(min(.$pct) * 1.1, max(.$pct) * 1.1)

ggsave(
  filename = 'plots/plot9.png',
  units = 'cm',
  width = 13,
  height = 13,
  bg = 'white',
  dpi = 600
)

# Panel dodged barplot ---------------------------------------------------------

data_list$plot10 %>%
  arrange(desc(isced6)) %>%
  group_by(typ) %>%
  mutate(
    lp = row_number(),
    lp = if_else(typ == 'method 2', lp + 17.5, lp + 0)
  ) %>%
  gather(isced, n, c(isced6, isced7)) %>%
  mutate(
    n = n / 100,
    lab = case_when(
      n == 0 & brak == 'a' ~ '**',
      n == 0 & brak == 'm' ~ '*',
      TRUE ~ round(n, 3) %>% scales::percent()
    ),
    śr = geo %like% '.*Śr.*'
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
      xmin = filter(., geo == 'Średnia') %>% pull(lp) - .53,
      xmax = filter(., geo == 'Średnia') %>% pull(lp) + .53,
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
    fill = colors[2],
    alpha = .1
  ) +
  geom_col(
    position = 'dodge',
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
      group = 'x'
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
    family = 'Calibri',
    size = roz_cz_txt
  ) +
  geom_text(
    aes(y = n + max(n) * .005),
    position = position_dodge(.9),
    family = 'Calibri',
    angle = 90,
    size = roz_cz_txt - .5,
    hjust = 0
  ) +
  geom_text(
    data = filter(., geo != 'Średnia'),
    aes(
      y = -max(n) * .02,
      label = geo
    ),
    family = 'Calibri',
    angle = 90,
    size = roz_cz_txt - .5,
    hjust = 1,
    check_overlap = TRUE
  ) +
  geom_text(
    data = filter(., geo == 'Średnia'),
    aes(
      y = -max(n, na.rm = TRUE) * .02,
      label = geo
    ),
    family = 'Calibri',
    angle = 90,
    fontface = 'bold',
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
      'type 1',
      'type 2'),
    values = colors[c(1, 2)]
  ) +
  scale_color_manual(values = c('white', colors[4])) +
  theme(
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.spacing.x = unit(1, 'mm'),
    legend.position = c(.5, 1),
    legend.direction = 'horizontal'
  )

ggsave(
  filename = 'plots/plot10.png',
  units = 'cm',
  width = 21,
  height = 11,
  bg = 'white',
  dpi = 600
)

# Wordl destinations map -------------------------------------------------------

wrld <- rgdal::readOGR('maps/TM_WORLD_BORDERS-0.3.shp', 'TM_WORLD_BORDERS-0.3')

grat <-
  readOGR('maps/ne_110m_graticules_30.shp', 'ne_110m_graticules_30') %>%
  spTransform(CRS('+proj=robin')) %>%
  fortify()

dane <- 
  data_list$map3 %>%
  mutate(bins = as.factor(bins))

wrld_center <-
  wrld %>%
  spTransform(CRS('+proj=robin')) %>%
  coordinates() %>%
  as_tibble() %>%
  set_names(c('long', 'lat')) %>%
  mutate(cntr_id = wrld@data$NAME) %>%
  left_join(dane, by = c('cntr_id' = 'dest')) %>%
  mutate(
    long_ = if_else(cntr_id == 'Poland', long, NA_real_),
    lat_ = if_else(cntr_id == 'Poland', lat, NA_real_),
    size = case_when(
      bins == levels(bins)[1] ~ .2,
      bins == levels(bins)[2] ~ .4,
      TRUE ~ .6)) %>%
  fill(long_, lat_, .direction = 'up') %>%
  fill(long_, lat_, .direction = 'down') %>%
  filter(!is.na(bins))

eur_center <-
  wrld %>%
  spTransform(CRS('+init=epsg:4326')) %>%
  coordinates() %>%
  as_tibble() %>%
  set_names(c('long', 'lat')) %>%
  mutate(
    cntr_id = wrld@data$NAME,
    long = case_when(
      cntr_id == 'Norway' ~ long - 4,
      cntr_id == 'Sweden' ~ long - 2,
      cntr_id == 'United Kingdom' ~ long + 1,
      TRUE ~ long
    ),
    lat = case_when(
      cntr_id == 'Norway' ~ lat - 3,
      cntr_id == 'Finland' ~ lat - 2,
      TRUE ~ lat
    )) %>%
  left_join(dane, by = c('cntr_id' = 'dest')) %>%
  mutate(
    long_ = if_else(cntr_id == 'Poland', long, NA_real_),
    lat_ = if_else(cntr_id == 'Poland', lat, NA_real_)
  ) %>%
  fill(long_, lat_, .direction = 'up') %>%
  fill(long_, lat_, .direction = 'down') %>%
  filter(!is.na(n)) %>%
  filter(long %>% between(-24, 36),
         lat %>% between(36, 67))

clean_map <- function(shp, proj, dane) {
  shp %>%
    spTransform(CRS(proj)) %>%
    rmapshaper::ms_simplify(keep = .1) %>%
    tidy(region = 'NAME') %>%
    left_join(
      dane %>% select(dest, n, bins),
      by = c('id' = 'dest')
    ) %>%
    mutate(clr = if_else(id == 'Poland', 0, n))
}

wrld_epg <-
  wrld %>%
  clean_map('+init=epsg:4326', dane)

wrld_rob <-
  wrld %>%
  clean_map('+proj=robin', dane)

wrld_rob %>%
  filter(lat > -6e6) %.>%
  ggplot(
    data = .,
    aes(
      x = long,
      y = lat
  )) +
  geom_path(
    data = grat,
    aes(group = group),
    linetype = 'dotted',
    color = 'steelblue',
    alpha = .3
  ) +
  geom_polygon(aes(
      group = group,
      fill = clr
    ),
    col = 'steelblue',
    size = .000001,
    show.legend = FALSE
  ) +
  geom_curve(
    data = wrld_center %>%
      filter(long >= 1567190),
    aes(
      x = long_,
      y = lat_,
      xend = long,
      yend = lat,
      size = bins,
      color = bins
    ),
    curvature = -.3,
    arrow = arrow(type = 'closed', angle = 20, length = unit(1, 'mm')),
    alpha = .8
  ) +
  geom_curve(
    data = wrld_center %>% filter(long < 1567190),
    aes(
      x = long_,
      y = lat_,
      xend = long,
      yend = lat,
      size = bins,
      color = bins
    ),
    curvature = .3,
    arrow = arrow(type = 'closed', angle = 20, length = unit(1, 'mm')),
    alpha = .8
  ) +
  theme_map() +
  theme(
    legend.position = c(.37, -.22),
    legend.title = element_blank(),
    legend.text = element_text(family = 'Calibri'),
    legend.text.align = 1,
    legend.margin = margin(0, 0, 0, 0)
  ) +
  scale_color_manual(values = colors[c(12, 15, 13, 14)]) +
  scale_fill_gradient(
    low = rgb(.9, .9, .9),
    high = colors[2],
    na.value = 'white'
  ) +
  scale_size_manual(
    values = c(.3, .55, .1, .9) %>% rev(),
    labels = c(' 1 - 100', ' 101 - 1000', ' 1001 - 5000', ' > 5000')
  ) +
  guides(size = guide_legend(
    override.aes = list(
      color = colors[c(15, 14, 13, 12)],
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
      geom_polygon(aes(
          group = group,
          fill = clr
        ),
        col = 'steelblue',
        size = .00001,
        alpha  = .8
      ) +
      geom_segment(
        data = eur_center,
        aes(
          x = long_,
          y = lat_,
          xend = long,
          yend = lat,
          size = bins,
          color = bins
        ),
        arrow = arrow(type = 'closed', angle = 20, length = unit(1, 'mm')),
        alpha = .8
      ) +
      coord_map(
        xlim = c(-24, 36),
        ylim = c(36, 67)
      ) +
      theme_map() +
      theme(
        legend.position = 'none',
        panel.border = element_rect(color = 'steelblue', fill = NA),
        panel.background = element_rect(color = 'white'),
        plot.margin = margin(0, 0, 0, 0)
      ) +
      scale_color_manual(values = colors[c(12, 15, 13, 14)]) +
      scale_fill_gradient(
        low = rgb(.9, .9, .9),
        high = colors[2],
        na.value = 'white'
      ) +
      scale_size_manual(values = c(.9, .1, .55, .3))),
    xmin = -2e6,
    xmax = 14e6,
    ymin = -14e6,
    ymax = -40e5
  )

ggsave(
  filename = 'plots/map3.png',
  units = 'cm',
  width = 15,
  height = 14,
  bg = 'white',
  dpi = 800
)

# World waffle map -------------------------------------------------------------

wrld <- rgdal::readOGR('maps/TM_WORLD_BORDERS-0.3.shp', 'TM_WORLD_BORDERS-0.3')

grat <-
  readOGR('maps/ne_110m_graticules_30.shp', 'ne_110m_graticules_30') %>%
  spTransform(CRS('+proj=robin')) %>%
  fortify()

dane <-
  data_list$map4 %>%
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
          colors = colors[c(4, 6, 9)]
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
    labels = c('type 1', 'type 2', 'type 3')
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
      title = 'overall'
    ),
    color = guide_legend(override.aes = list(
        shape = 15,
        size = 4,
        color = colors[c(6, 9, 4)]
      ),
      keywidth = .75,
      keyheight = .75,
      order = 1,
      title = ''
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
  filename = 'plots/map4.png',
  units = 'cm',
  width = 15,
  height = 14,
  bg = 'white',
  dpi = 800
)
