
# tema para grafaicos
# Crete theme map
theme_map1 <- function(base_size) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(0,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 6)
      # legend.key.width=unit(0.5,"cm")
      
    )
}
  
  

# sigla_munii <- 'for'; ano <- 2019
# sigla_munii <- 'bel'; ano <- 2019
# sigla_munii <- 'sgo'; ano <- 2019
# sigla_munii <- 'cgr'; ano <- 2019
# sigla_munii <- 'cam'; ano <- 2019
  
fazer_mapa_bivariate <- function(sigla_munii, ano = 2019, width = 16, height = 14, dpi=300)  {
  
  # temp_name_muni <- subset(munis_df_2019, abrev_muni==sigla_munii)$name_muni
  # 
  # # ler acess - bfca
  # acess_cmp <- read_rds(sprintf("../data/output_bfca/bfca_%s.rds", sigla_munii))
  # 
  # # ler muni limits
  # muni_sf <- geobr::read_municipality(code_muni = munis_df_2019[abrev_muni == sigla_munii]$code_muni,
  #                                     year=2010)
  # 
  # # read hex data
  # dir_hex <- sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_munii)
  # hexagonos_sf <- readr::read_rds(dir_hex)
  # 
  # # ler tiles
  # map_tiles <- read_rds(sprintf("../../../data/maptiles_crop/%s/mapbox/maptile_crop_mapbox_%s_%s.rds",
  #                               ano, sigla_munii, ano))
  # 
  # 
  # 
  # acess_cmp <- left_join(acess_cmp,
  #                       select(setDT(hexagonos_sf), id_hex, pop_total, geometry),
  #                       by=c("origin"="id_hex"))
  
  
  acess_cmp_bi <- setDT(acess_cmp_sf)
  
  # bivariate map
  # create 3 buckets for access
  quantiles_acess <- acess_cmp_bi %>%
    pull(BFCA) %>%
    Hmisc::wtd.quantile(probs = seq(0, 1, length.out = 4), weights = acess_cmp_bi$pop_total)
    # quantile(probs = seq(0, 1, length.out = 4))
  
  # ha casos em que o terceiro quintil eh igual ao valor maximo, corrigir
  if (quantiles_acess[3] == quantiles_acess[4]) {
    
    quantiles_acess[3] = quantiles_acess[4]*0.9999
    
  }
  
  
  # raiase last quantile
  quantiles_acess[4] <- 1.01*quantiles_acess[4]
  
  # create 3 buckets for mean income
  quantiles_pop <- acess_cmp_bi %>%
    pull(pop_total) %>%
    Hmisc::wtd.quantile(probs = seq(0, 1, length.out = 4), weights = acess_cmp_bi$pop_total)
    # quantile(probs = seq(0, 1, length.out = 4))
  
  # force pop quantile to start at 0
  quantiles_pop[1] <- 0
  
  # create color scale that encodes two variables
  # red for gini and blue for mean income
  # the special notation with gather is due to readibility reasons
  
  
  # default - palete 2 (rafa gostou)
  bivariate_color_scale <- tibble(
    "3 - 3" = "#e8e8e8", # high inequality, high income (1)
    "2 - 3" = "#a6d9d9", # (2)
    "1 - 3" = "#5ac8c8", # low inequality, high income (3)
    "3 - 2" = "#d3a7cb", # (4)
    "2 - 2" = "#a6a7cb", # medium inequality, medium income (5)
    "1 - 2" = "#5aa7c8", # (6)
    "3 - 1" = "#be64ac", # high inequality, low income (7)
    "2 - 1" = "#a664ac", # (8)
    "1 - 1" = "#5a64ac" # low inequality, low income (9)
  ) %>%
    gather("group", "fill")
  
  
  
  # cut into groups defined above and join fill
  acess_cmp_bi <- acess_cmp_bi %>%
    mutate(
      acess_quantiles = cut(
        BFCA,
        breaks = quantiles_acess,
        include.lowest = TRUE,
        dig.lab = 10
      ),
      pop_quantiles = cut(
        pop_total,
        breaks = quantiles_pop,
        include.lowest = TRUE,
        dig.lab = 10
      ),
      # by pasting the factors together as numbers we match the groups defined
      # in the tibble bivariate_color_scale
      group = paste(
        as.numeric(pop_quantiles)
        , "-" ,
        as.numeric(acess_quantiles)
        # (4 - as.numeric(pop_quantiles))
        # (4 - as.numeric(acess_quantiles))
      )
    ) %>%
    
    # we now join the actual hex values per "group"
    # so each municipality knows its hex value based on the his gini and avg
    # income value
    left_join(bivariate_color_scale, by = "group")
  
  
  acess_cmp_bi_sf <- acess_cmp_bi %>% st_sf()
  
  
  # make map
  map <- ggplot() +
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    geom_sf(data=st_transform(muni_sf, 3857),  fill=NA, color='gray50') +
    # new scale for map tiles
    ggnewscale::new_scale_fill() +
    
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = F) + # suppress legend
    # color municipalities according to their gini / income combination
    geom_sf(data = st_transform(acess_cmp_bi_sf, 3857), aes(fill = fill), color = NA, size = 0.1) +
    # as the sf object municipality_prod_geo has a column with name "fill" that
    # contains the literal color as hex code for each municipality, we can use
    # scale_fill_identity here
    scale_fill_identity()+
    theme_map1()+
    theme(plot.title = element_text(size = 10))+
    labs(title = "C) Bivariate Population x BFCA")
  
  
  
  # LEGEND!
  # separate the groups
  bivariate_color_scale1 <- bivariate_color_scale %>%
    separate(group, into = c("pop", "acess"), sep = " - ") %>%
    # separate(group, into = c("acess", "pop"), sep = " - ") %>%
    mutate(acess = as.integer(acess),
           pop = as.integer(pop))
  
  legend <- ggplot() +
    geom_tile(
      data = bivariate_color_scale1,
      mapping = aes(
        y = acess,
        x = pop,
        fill = fill)
    ) +
    scale_fill_identity() +
    labs(
      x = "Population \u2192",
      y = "BFCA \u2192"
    ) +
    ggthemes::theme_map() +
    # make font small enough
    theme(
      axis.title = element_text(size = 6),
      plot.margin=unit(c(0,0,0,0),"mm")
      # legend.key.size = unit(0.3, "cm")
      # axis.title.y = element_text(angle = -90)
    ) +
    # quadratic tiles
    coord_fixed()
  
  
  # calculte map aspect ratio
  bbox <- st_bbox(st_transform(muni_sf, 3857))
  ratio_map <- (bbox[3] - bbox[1])/(bbox[4] - bbox[2])
  
  library(cowplot)
  library(patchwork)
  
  
  # calcular layout
  
  
  layout <- c(area(t = 1, b = 10, l = 1, r = 10*ratio_map),
              area(t = 9, b = 10, l = 9*ratio_map, r = 10*ratio_map))


  fim <- map + legend + plot_layout(design = layout)

  
  # fim <- ggdraw(clip = "on") +
  #   draw_plot(map, 0, 0, 1, 1) +
  #   draw_plot(legend, 0.70, -0.01, 0.25, 0.25)
  
  
  # ggsave(fim,
  #        file= sprintf("../figuras/figures_en_paper/bfca_bivariate/bfca_bivariate_%s.png", sigla_munii),
  #        dpi = 300, width = width, height = height, units = "cm")
  
}



# fazer_mapa_bivariate('bel', width = 16/1.8) # 1.8 (h/w)
# fazer_mapa_bivariate('bho', width = 16/1.5) # 670 x 417 = 1.4
# fazer_mapa_bivariate('bsb', width = 16/0.7) # 624 x 954 = 0.65
# fazer_mapa_bivariate('cam', width = 16/0.98) # 602 x 628 = 0.95
# fazer_mapa_bivariate('cgr', width = 16/1.35) # 645 x 495 = 1.3
# fazer_mapa_bivariate('cur', width = 16/1.55) # 600 x 400 = 1.5
# fazer_mapa_bivariate('duq', width = 16/1.65) # 601 x 383 = 1.6
# fazer_mapa_bivariate('for', width = 16/0.6) # 567 x 625 = 0.9
# fazer_mapa_bivariate('goi', width = 16/1.27) # 615 x 483 = 1.3
# fazer_mapa_bivariate('gua', width = 16/1.3) # 523 x 400 = 1.3
# fazer_mapa_bivariate('mac', width = 16/1.5) # 600 x 400 = 1.5
# fazer_mapa_bivariate('man', width = 16/0.65) # 500 x 500 = 1
# fazer_mapa_bivariate('nat', width = 16/1.5) # 560 x 371 = 1.5
# fazer_mapa_bivariate('poa', width = 16/1.5) # 578 x 394 = 1.5
# fazer_mapa_bivariate('rec', width = 16/1.55) # 557 x 344 = 1.6
# fazer_mapa_bivariate('rio', width = 16/0.65) # 550 x 915 = 0.6
# fazer_mapa_bivariate('sal', width = 16/0.9) # 528 x 610 = 0.9
# fazer_mapa_bivariate('sgo', width = 16/1) # 440 x 440 = 1
# fazer_mapa_bivariate('slz', width = 16/1.5) # 552 x 385 x 440 = 1.4
# fazer_mapa_bivariate('spo', width = 16/1.6) # 600 x 360 x 440 = 1.6

