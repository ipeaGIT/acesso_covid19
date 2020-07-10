source("../../../acesso_oport_kaue/R/fun/setup.R")

# para manaus
ylim = c(-353979.8550, -326309.6987)
xlim = c(-6696609.8722, -6658735.3079)

# Crete theme map
theme_map1 <- function(base_size) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(4,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 6)
      # legend.key.width=unit(0.5,"cm")
      
    )
}

## cria tabela resumo
# create empty data.frame to fill with info from each city
tabela_resumo <- data.frame( code_muni = as.numeric(),
                             name_muni = as.character(),
                             abrev_muni = as.character(),
                             pop_total = as.numeric(),
                             pop_vulnrv = as.numeric(),
                             no_acess_hosp = as.numeric(),
                             no_acess_leit = as.numeric()
)






# sigla_munii <- "bel" ; ano <- 2019; dpi=200

fazer_mapa_1 <- function(sigla_munii, ano=2019, dpi=300, width = 16, height = 15){
  
  message(paste0('working on ', sigla_munii))
  
  # get muni code
  temp_code_muni <- subset(munis_df_2019, abrev_muni==sigla_munii)$code_muni
  temp_name_muni <- subset(munis_df_2019, abrev_muni==sigla_munii)$name_muni
  
  
  ### Load and filter access data --------------------------
  
  # ler dados de TMI agregado
  dir_tmi <- sprintf("../data/output_tmi_agreg/acess_tmi_agreg_%s.csv", sigla_munii)
  tmi <- data.table::fread(dir_tmi)
  
  # filtra apenas renda baixa
  tmi_decil_1_5 <- subset(tmi, decil <= 5)
  
  # filtra apenas hexagonos com pop acima de  50 anos
  tmi_pop50 <- subset(tmi_decil_1_5, idade_50  > 0)
  
  # filtra apenas hexagonos q demora mais de 30 min. caminhando para qualquer hospital
  no_acess_hosp <- subset(tmi_pop50, mode=='walk' & quant_hosp==0)
  pop_no_acess_hosp <- sum(no_acess_hosp$idade_50, na.rm=T)
  pop_no_acess_hosp <- round( pop_no_acess_hosp/1000 , digits = 1) # format value
  
  # filtra apenas hexagonos distania maior do que 5 Km para qualquer hospital com leito
  no_acess_leit <- subset(tmi_pop50, mode=='car' & quant_leit==0)
  pop_no_acess_leit <- sum(no_acess_leit$idade_50, na.rm=T)
  pop_no_acess_leit <- round( pop_no_acess_leit/1000 , digits = 1) # format value
  
  
  ## traz info de pop total do grupo vulneravel
  # read hex data
  dir_hex <- sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_munii)
  hexagonos_sf <- readr::read_rds(dir_hex)
  poptotal <- (sum(hexagonos_sf$pop_total) / 1000) %>% round(  digits = 1)
  # pop vulneravel
  hexagonos_sf <- subset(hexagonos_sf, decil <=5)
  setDT(hexagonos_sf)[, idade_50 := idade_50a59 + idade_60a69 + idade_70]
  popvulnrv <- (sum(hexagonos_sf$idade_50, na.rm=T) / 1000)  %>% round(  digits = 1)
  
  
  # update tabela resumo
  temp_resumo <- data.frame(code_muni = temp_code_muni,
                            name_muni = temp_name_muni,
                            abrev_muni = sigla_munii,
                            pop_total = poptotal,
                            pop_vulnrv = popvulnrv,
                            no_acess_hosp = pop_no_acess_hosp,
                            no_acess_leit = pop_no_acess_leit )
  
  tabela_resumo <- rbind(tabela_resumo, temp_resumo)
  
  
  ### Create maps--------------------------
  
  # ler muni limits
  muni_sf <- geobr::read_municipality(code_muni = temp_code_muni , year=2010)
  
  # let tiles
  map_tiles <- read_rds(sprintf("../../../data/maptiles_crop/%s/mapbox/maptile_crop_mapbox_%s_%s.rds", ano, sigla_munii, ano))
  
  # # ler grade de hexagonos
  # dir_hex <- sprintf("../../../data/hex_municipio/%s/hex_%s_09_%s.rds", ano, sigla_munii, ano)
  # hex_sf <- readr::read_rds(dir_hex)
  
  # merge access data
  no_acess_hosp_sf <- left_join(no_acess_hosp,
                                select(hexagonos_sf, id_hex, geometry),
                                by=c("origin"="id_hex"))
  
  no_acess_hosp_sf <- sf::st_as_sf(no_acess_hosp_sf)
  
  
  no_acess_leit_sf <- left_join(no_acess_leit,
                                select(hexagonos_sf, id_hex, geometry),
                                by=c("origin"="id_hex"))
  no_acess_leit_sf <- sf::st_as_sf(no_acess_leit_sf)
  
  
  
  if(sigla_munii == "man") {
    
    
    bbox_man <- c(xmin =xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2])
    
    no_acess_leit_sf <- no_acess_leit_sf %>% st_transform(crs = 3857) %>% 
      st_crop(bbox_man)
    
    no_acess_hosp_sf <- no_acess_hosp_sf %>% st_transform(crs = 3857) %>% 
      st_crop(bbox_man)
    
    map_tiles <- map_tiles %>%
      filter(between(x, xlim[1], xlim[2])) %>%
      filter(between(y, ylim[1], ylim[2])) 
    
    muni_sf <- muni_sf %>% st_transform(crs = 3857) %>% 
      st_crop(bbox_man)
    
  }
  
  # define breaks
  # breaks <- seq(min(no_acess_hosp_sf$idade_50, no_acess_leit_sf$idade_50), 
  #               max(no_acess_hosp_sf$idade_50, no_acess_leit_sf$idade_50), 
  #               length.out = 5)
  # 
  limits <- round(c(min(no_acess_hosp_sf$idade_50, no_acess_leit_sf$idade_50),
                    max(no_acess_hosp_sf$idade_50, no_acess_leit_sf$idade_50)))
  
  
  deserts1 <-
    ggplot() +
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    geom_sf(data=st_transform(muni_sf, 3857), fill=NA, color='gray50') +
    # nova escala
    ggnewscale::new_scale_fill() +
    geom_sf(dat = st_transform(no_acess_hosp_sf, 3857), aes(fill = idade_50 ), color = NA, alpha=.7) +
    # scale_fill_distiller(palette = "Reds", direction = 1, values = 0.5) +
    scale_fill_gradient(low = "#fc9272", high = "#67000d", limits = limits) +
    labs(fill = "Low income people above 50 years old",
         subtitle = paste0("A) ", format(pop_no_acess_hosp, big.mark = ',', decimal.mark = '.'), ' thousand people'))+
    guides(fill = guide_colourbar(title.position = "top",
                                  # title.hjust = .5,
                                  label.position = "bottom"))
  
  
  
  deserts2 <-
    ggplot() +
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    geom_sf(data=st_transform(muni_sf, 3857), fill=NA, color='gray50') +
    # nova escala
    ggnewscale::new_scale_fill() +
    geom_sf(dat = st_transform(no_acess_leit_sf, 3857), aes(fill = idade_50 ), color = NA, alpha=.7) +
    # scale_fill_distiller(palette = "Reds", direction = 1) +
    scale_fill_gradient(low = "#fc9272", high = "#67000d", limits = limits) +
    labs(fill = "Low income people above 50 years old",
         subtitle = paste0("B) ", format(pop_no_acess_leit, big.mark = ',', decimal.mark = '.'), ' thousand people'))+
    guides(fill = guide_colourbar(title.position = "top",
                                  # title.hjust = .5,
                                  label.position = "bottom"))
  
  # make plot
  if(sigla_munii %in% c('rio','bsb')) {
    
    p <- deserts1 / deserts2 +  plot_annotation( title = paste0(temp_name_muni)) +
      plot_layout(guides = "collect") &
      theme_map1() &
      theme(plot.title = element_text(hjust = 0.5))
    
    #save plot
    ggsave(p,
           file= sprintf("../figuras/figures_en_paper/map1/map1_tmi_%s.png", sigla_munii),
           dpi = dpi, width = width, height = height, units = "cm")
    
  # } else if(sigla_munii %in% c('man')) {
  #   
  #   p <- deserts1 + deserts2 +  plot_annotation( title = paste0(temp_name_muni))+
  #     plot_layout(guides = "collect") &
  #     theme_map1() &
  #     coord_sf(expand = TRUE, ylim = c(-353979.8550, -326309.6987), 
  #              xlim = c(-6696609.8722, -6658735.3079)) &
  #     theme(plot.title = element_text(hjust = 0.5)
  #           ,legend.key.size = unit(3,"cm")
  #     )
  #   
  #   #save plot
  #   ggsave(p,
  #          file= sprintf("../figuras/figures_en_paper/map1/map1_tmi_%s.png", sigla_munii),
  #          dpi = dpi, width = width, height = height, units = "cm")
    
    } else {
    
    p <- deserts1 + deserts2 +  plot_annotation( title = paste0(temp_name_muni))+
      plot_layout(guides = "collect") &
      theme_map1() &
      theme(plot.title = element_text(hjust = 0.5)
            ,legend.key.size = unit(3,"cm")
      )
    
    #save plot
    ggsave(p,
           file= sprintf("../figuras/figures_en_paper/map1/map1_tmi_%s.png", sigla_munii),
           dpi = dpi, width = width, height = height, units = "cm")
    
  }
  
  
  return(tabela_resumo)
  
}


# aplicar funcao
fazer_mapa_1('bel', width = 30/1.8) # 1.8 (h/w)
fazer_mapa_1('bho', width = 30/1.5) # 670 x 417 = 1.4
fazer_mapa_1('bsb', height = 32 * 0.7) # 624 x 954 = 0.65
fazer_mapa_1('cam', width = 30/0.98) # 602 x 628 = 0.95
fazer_mapa_1('cgr', width = 30/1.35) # 645 x 495 = 1.3
fazer_mapa_1('cur', width = 30/1.55) # 600 x 400 = 1.5
fazer_mapa_1('duq', width = 30/1.65) # 601 x 383 = 1.6
fazer_mapa_1('for', height = 16 * 0.6) # 567 x 625 = 0.9
fazer_mapa_1('goi', width = 30/1.27) # 615 x 483 = 1.3
fazer_mapa_1('gua', width = 30/1.3) # 523 x 400 = 1.3
fazer_mapa_1('mac', width = 30/1.5) # 600 x 400 = 1.5
fazer_mapa_1('man', height = 16 * 0.65) # 500 x 500 = 1
fazer_mapa_1('nat', width = 30/1.5) # 560 x 371 = 1.5
fazer_mapa_1('poa', width = 30/1.5) # 578 x 394 = 1.5
fazer_mapa_1('rec', width = 30/1.55) # 557 x 344 = 1.6
fazer_mapa_1('rio', height = 32 * 0.65) # 550 x 915 = 0.6
fazer_mapa_1('sal', width = 30/0.9) # 528 x 610 = 0.9
fazer_mapa_1('sgo', width = 30/1) # 440 x 440 = 1
fazer_mapa_1('slz', width = 30/1.5) # 552 x 385 x 440 = 1.4
fazer_mapa_1('spo', width = 30/1.6) # 600 x 360 x 440 = 1.6



# sigla_munii <- "for"; ano <- 2019; width = 16; height = 14
# sigla_munii <- "spo"; ano <- 2019; width = 16; height = 14
# sigla_munii <- "rio"; ano <- 2019; width = 16; height = 14
# sigla_munii <- "man"; ano <- 2019; width = 16; height = 14

fazer_mapa_2 <- function(sigla_munii, ano = 2019, width = 16, height = 14, dpi=300) {
  
  temp_name_muni <- subset(munis_df_2019, abrev_muni==sigla_munii)$name_muni
  
  # ler acess - ppr
  acess_ppr <- read_rds(sprintf("../data/output_ppr/ppr_%s.rds", sigla_munii))
  
  # ler acess - bfca
  acess_cmp <- read_rds(sprintf("../data/output_bfca/bfca_%s.rds", sigla_munii))
  
  # ler muni limits
  muni_sf <- geobr::read_municipality(code_muni = munis_df_2019[abrev_muni == sigla_munii]$code_muni, 
                                      year=2010)
  
  # read hex data
  dir_hex <- sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_munii)
  hexagonos_sf <- readr::read_rds(dir_hex)
  
  # ler tiles
  map_tiles <- read_rds(sprintf("../../../data/maptiles_crop/%s/mapbox/maptile_crop_mapbox_%s_%s.rds", 
                                ano, sigla_munii, ano))
  
  # refactor tipo de unidade do hospital
  acess_ppr <- acess_ppr %>%
    mutate(unidade = ifelse(TIPO_UNIDADE == 73, "Hospital de campanha", "Hospital permanente"))
  
  # merge access data
  acess_ppr <- left_join(acess_ppr, 
                         select(setDT(hexagonos_sf), id_hex, geometry),
                         by=c("id_hex"="id_hex"))
  
  
  acess_ppr_sf <- acess_ppr %>%st_sf(crs = 4326)
  
  acess_cmp <- left_join(acess_cmp, 
                         select(setDT(hexagonos_sf), id_hex, pop_total, geometry),
                         by=c("origin"="id_hex"))
  
  acess_cmp_sf <- acess_cmp %>% st_sf(crs = 4326)
  
  
  # trim map for manaus case
  if(sigla_munii == "man") {
    
    
    bbox_man <- c(xmin =xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2])
    
    acess_ppr_sf <- acess_ppr_sf %>% st_transform(crs = 3857) %>% 
      st_crop(bbox_man)
    
    acess_cmp_sf <- acess_cmp_sf %>% st_transform(crs = 3857) %>% 
      st_crop(bbox_man)
    
    map_tiles <- map_tiles %>%
      filter(between(x, xlim[1], xlim[2])) %>%
      filter(between(y, ylim[1], ylim[2])) 
    
    muni_sf <- muni_sf %>% st_transform(crs = 3857) %>% 
      st_crop(bbox_man)
    
  }
  
  
  acess_ppr_points <- acess_ppr_sf %>%
    st_centroid() %>%
    st_transform(crs = 3857) %>%
    sfc_as_cols() %>%
    # transformar leitos para mil habitantes
    mutate(ppr = ppr * 10000)
  
  # usar escala de cores diferentes para plots 1 e 2
  # 
  
  
  # make plot
  p1 <-
    
    ggplot() +
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    geom_sf(data=st_transform(muni_sf, crs = 3857), fill=NA, color='gray50') +
    # nova escala
    ggnewscale::new_scale_fill() +
    geom_point(data = subset(acess_ppr_points, unidade == 'Hospital permanente'),
               aes(x = lon, y = lat, size = ppr), alpha = 0.3, color = 'red') +
    geom_point(data = subset(acess_ppr_points, unidade == 'Hospital de campanha'),
               aes(x = lon, y = lat, size = ppr), alpha = 0.3, color = 'black', 
               show.legend = FALSE) +
    # scale_size_continuous(breaks = breaks)+
    labs(size = "UCI beds per \n10 thousand habitants",
         # title = paste0(temp_name_muni),
         title = "A) PPR of hospitals with ICU"
         # subtitle = sprintf("Hospitals with UCI: %s", nrow(acess_ppr_points))
         )+
    theme_map1()+
    theme(plot.title = element_text(size = 10))
  
  
  
  p2 <- 
    ggplot() +
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    geom_sf(data=st_transform(muni_sf, 3857), fill=NA, color='gray50') +
    # nova escala
    ggnewscale::new_scale_fill() +
    geom_sf(data = st_transform(acess_cmp_sf, 3857), aes(fill = BFCA*1000000), color = NA)+
    scale_fill_viridis_c(option = "inferno")+
    theme_map1()+
    theme(plot.title = element_text(size = 10))+
    labs(title = paste0("B) BFCA with ", round(unique(acess_cmp_sf$threshold)), " km"),
         fill = "BFCA * 10^6")

  source("7-maps_bivariate.R", local = TRUE, encoding = "UTF-8")
  p3 <- fazer_mapa_bivariate(sigla_munii)
  
  
    
  
  library(patchwork)
  
  if (sigla_munii == 'man') {
    
    fim <- p1 + p2 + p3
    
  } else if(sigla_munii == 'rio')  {
    
    fim <- (p1 | p2) / p3
    
    
    
  } else {
    
    
    fim <- p1 + p2 + p3
    
  }
  
  
  
  ggsave(fim,
         file= sprintf("../figuras/figures_en_paper/figure_en_paper_%s.png", sigla_munii),
         dpi = 300, width = width, height = height, units = "cm")
  
}


fazer_mapa_2('rio', width = 16, height = 14)
fazer_mapa_2('man', height = 7.5)
fazer_mapa_2('for', width = 16, height = 7)
fazer_mapa_2('spo', height = 10)







# # ler acess - ppr
# acess_ppr <- read_rds(sprintf("../data/output_ppr/ppr_%s.rds", sigla_munii))
# 
# # ler acess - bfca
# acess_cmp <- read_rds(sprintf("../data/output_bfca/bfca_%s.rds", sigla_munii))
# 
# 
# # read hex data
# dir_hex <- sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_munii)
# hexagonos_sf <- readr::read_rds(dir_hex)
# 
# 
# a <- left_join(hexagonos_sf, acess_cmp, by=c('id_hex'='origin'))
# 
# a <- subset(a, pop_total>0)
# ggplot() + geom_boxplot(data=a, aes(x=factor(decil), y=BFCA, weight=pop_total))
# 
# 
# ggplot() + geom_point(data=a, aes(x=factor(decil), y=weighted.mean(x=BFCA, w=pop_total)*10000))
#   # ylim(0.0000002992258, 0.0000013352451)
#   
# tolerance all.equal
# a[, 10000*weighted.mean(x=BFCA, w=pop_total), by=decil]
# 
# mean(a$BFCA)
