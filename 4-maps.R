source("../../../acesso_oport_kaue/R/fun/setup.R")





### 1) TMI access deserts ------------------------------------------------------------------------------


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


# Crete theme map
theme_map1 <- function(base_size) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4),
      strip.text = element_text(size = 6)
      # legend.key.width=unit(0.5,"cm")
      
    )
}





# sigla_munii <- "bel" ; ano <- 2019; dpi=200

fazer_mapa_1 <- function(sigla_munii, ano=2019, dpi=300){
  
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
  
  # define breaks
  breaks <- seq(min(no_acess_hosp_sf$idade_50, no_acess_leit_sf$idade_50), 
                max(no_acess_hosp_sf$idade_50, no_acess_leit_sf$idade_50), 
                length.out = 5)
  
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
    labs(fill = "Pessoas de baixa renda acima de 50 anos",
         subtitle = paste0("A) ", format(pop_no_acess_hosp, big.mark = '.', decimal.mark = ','), ' mil pessoas'))+
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
    labs(fill = "Pessoas de baixa renda acima de 50 anos",
         subtitle = paste0("B) ", format(pop_no_acess_leit, big.mark = '.', decimal.mark = ','), ' mil pessoas'))+
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
           file= sprintf("../figuras/mapa1/mapa1_tmi_dpi%s_%s.svg", dpi, sigla_munii),
           dpi = dpi, width = 16, height=23, units = "cm")
    
  } else{
    
    p <- deserts1 + deserts2 +  plot_annotation( title = paste0(temp_name_muni))+
      plot_layout(guides = "collect") &
      theme_map1() &
      theme(plot.title = element_text(hjust = 0.5)
            ,legend.key.size = unit(3,"cm")
            )
    
  #save plot
  ggsave(p,
         file= sprintf("../figuras/mapa1/mapa1_tmi_dpi%s_%s.svg", dpi, sigla_munii),
         dpi = dpi, width = 16, units = "cm")
  }

  
   return(tabela_resumo)
  
}

# aplicar funcao 
future::plan(future::multiprocess)
tabela_resumo <- furrr::future_map(.x=munis_df_2019$abrev_muni,
                                   .f=fazer_mapa_1,
                                   .progress = T) %>% rbindlist()

## single core
tabela_resumo <- lapply(X=munis_df_2019$abrev_muni, FUN=fazer_mapa_1)  %>% rbindlist()


# salva tabela resumo no servidor
fwrite(tabela_resumo, '../data/output_tmi_agreg/tabela_resumo.csv')







### 2) MAPA 2: PPR ------------------------------------------------------------------------------

# sigla_munii <- "bho"; ano <- 2019; width = 16; height = 14

fazer_mapa_2 <- function(sigla_munii, ano = 2019, width = 16, height = 14, dpi=300) {
  
  temp_name_muni <- subset(munis_df_2019, abrev_muni==sigla_munii)$name_muni
  
  # ler acess
  acess2 <- read_rds(sprintf("../data/output_acess_cmp/acess_cmp_%s.rds", sigla_munii))
  
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
  acess2 <- acess2 %>%
    mutate(unidade = ifelse(TIPO_UNIDADE == 73, "Hospital de campanha", "Hospital permanente"))
  
  # merge access data
  acess2_sf <- left_join(acess2, 
                         select(setDT(hexagonos_sf), id_hex, geometry),
                         by=c("destination"="id_hex")) %>%
    st_sf(crs = 4326)
  
  
  acess2_points <- acess2_sf %>%
    st_centroid() %>%
    st_transform(3857) %>%
    sfc_as_cols() %>%
    # transformar leitos para mil habitantes
    mutate(ppr = ppr * 10000)
  

  
  
  # make plot
  p2 <-
  
    ggplot() +
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    geom_sf(data=st_transform(muni_sf, 3857), fill=NA, color='gray50') +
    # nova escala
    ggnewscale::new_scale_fill() +
    geom_point(data = subset(acess2_points, unidade == 'Hospital permanente'),
               aes(x = lon, y = lat, size = ppr), alpha = 0.3, color = 'red') +
    geom_point(data = subset(acess2_points, unidade == 'Hospital de campanha'),
               aes(x = lon, y = lat, size = ppr), alpha = 0.3, color = 'black', 
               show.legend = FALSE) +
    # scale_size_continuous(breaks = breaks)+
    labs(size = "Leitos por \n10 mil habitantes",
         title = paste0(temp_name_muni),
         subtitle = sprintf("Hospitais com leitos de UTI: %s", nrow(acess2_points)))+
    theme_map1()
  
  
  # #save plot
  # ggsave(p2,
  #        file= sprintf("../figuras/mapa2/mapa2_ppr_%s.png", sigla_munii),
  #        dpi = 300, width = 16,  units = "cm")
  #save plot
  ggsave(p2,
         file= sprintf("../figuras/mapa2/mapa2_ppr_dpi%s_%s.svg", dpi, sigla_munii),
         dpi = 300, width = width, height = height, units = "cm")
  
}


# aplicar funcao
fazer_mapa_2('bel', width = 16/1.8) # 1.8 (h/w)
fazer_mapa_2('bho', width = 16/1.5) # 670 x 417 = 1.4
fazer_mapa_2('bsb', width = 16/0.7) # 624 x 954 = 0.65
fazer_mapa_2('cam', width = 16/0.98) # 602 x 628 = 0.95
fazer_mapa_2('cgr', width = 16/1.35) # 645 x 495 = 1.3
fazer_mapa_2('cur', width = 16/1.55) # 600 x 400 = 1.5
fazer_mapa_2('duq', width = 16/1.65) # 601 x 383 = 1.6
fazer_mapa_2('for', width = 16/0.95) # 567 x 625 = 0.9
fazer_mapa_2('goi', width = 16/1.27) # 615 x 483 = 1.3
fazer_mapa_2('gua', width = 16/1.3) # 523 x 400 = 1.3
fazer_mapa_2('mac', width = 16/1.5) # 600 x 400 = 1.5
fazer_mapa_2('man', width = 16/1.3) # 500 x 500 = 1
fazer_mapa_2('nat', width = 16/1.5) # 560 x 371 = 1.5
fazer_mapa_2('poa', width = 16/1.5) # 578 x 394 = 1.5
fazer_mapa_2('rec', width = 16/1.55) # 557 x 344 = 1.6
fazer_mapa_2('rio', width = 16/0.65) # 550 x 915 = 0.6
fazer_mapa_2('sal', width = 16/0.9) # 528 x 610 = 0.9
fazer_mapa_2('sgo', width = 16/1) # 440 x 440 = 1
fazer_mapa_2('slz', width = 16/1.5) # 552 x 385 x 440 = 1.4
fazer_mapa_2('spo', width = 16/1.6) # 600 x 360 x 440 = 1.6




# aplicar funcao - somente se quiser fazer os mapas das cidades agrupados!
mapas2 <- lapply(arrange(munis_df_2019, abrev_muni)$abrev_muni, fazer_mapa_2)


library(patchwork)
g1 <- mapas2[[1]] + mapas2[[2]] # bel + bho
g2 <- mapas2[[3]] # bsb
g3 <- mapas2[[4]] + mapas2[[5]] # cam + cgr
g4 <- mapas2[[6]] + mapas2[[7]] # cur + duq
g5 <- mapas2[[8]] + mapas2[[9]] # for + goi
g6 <- mapas2[[10]] + mapas2[[11]] # gau + mac
g7 <- mapas2[[12]] + mapas2[[13]] # man + nat
g8 <- mapas2[[14]] + mapas2[[15]] # poa + rec
g9 <- mapas2[[16]] # rio
g10 <- mapas2[[17]] + mapas2[[18]] # sal + sgo
g11 <- mapas2[[19]] + mapas2[[20]] # slz + spo


# salvar
invisible(lapply(paste0("g", seq(1:11)), 
                 function (x) {
                   
                   plot1 <- get(x)
                   
                   ggsave(filename = sprintf("../figuras/mapa2/map2_ppr_%s.png", x), plot = plot1,  
                          device = "png",
                          dpi = 300, width = 14, height = 12, units = "cm")
                 }
                 
)

)



############ Tabela 1 --------------------------------------------------------------
# atualiza tabela resumo no texto da nota tecnica

# 1) ler dados de proximidade (A) e (B)
tb <- data.table::fread('../data/output_tmi_agreg/tabela_resumo.csv')


# 2) Trazer Numero de leitos por municipios


# Open hospitals
hosp <- fread('../data/cnes/cnes_fev_hex.csv')

# subset apenas tipos de unidade que podem fazer triagem ou internar
to_keep <- '72|73|01|02|04|05|07|15|20|21'
hosp <- subset(hosp, TIPO_UNIDADE %like% to_keep)

# subset apenas com leitos & respiradores
hosp <- hosp[quant_leitos >= 1 & quant_resp >= 1]

# trunca analise combinada de leitos e respitadores
hosp[, quant_leitos := min(quant_leitos, quant_resp), by=CNES ]


# Summarize
# hosp[, saude_total := ifelse( is.na(CNES), 0, 1) ]
muni_saude <- hosp[, .(
  # total_de_hosp = sum(saude_total, na.rm = T),
  total_de_leits  = sum(quant_leitos, na.rm = T),
  total_de_resps  = sum(quant_resp, na.rm = T)
),
by = .(code_muni)]

# merge
tb$code_muni6 <- substr(tb$code_muni, 1, 6) %>% as.integer()
tb_hosp <- left_join(tb, muni_saude, by=c('code_muni6' = 'code_muni'))
setDT(tb_hosp)

# 3) Format table

# drop and reorder columns
tb_hosp[, ':='(code_muni = NULL, code_muni6=NULL, abrev_muni = NULL)] 
setcolorder(tb_hosp, c('name_muni', 'total_de_leits', 
                       'total_de_resps', 'pop_total',
                       'pop_vulnrv', 'no_acess_hosp', 
                       'no_acess_leit'))


# calcula proporcao de grupo sem leito
tb_hosp[, no_acess_leit_prop := (100 * no_acess_leit / pop_vulnrv) %>% round(1) ]
tb_hosp <- tb_hosp[order(-no_acess_leit)]

# renamte columns
tabela2 <-  dplyr::select(tb_hosp,  'Município' = name_muni,
                          # 'N. de leitos*' = total_de_leits , 
                          # 'N. de respiradores' = total_de_resps , 
                          'Pop. total' = pop_total  ,
                          'Pop. vulnerável**' = pop_vulnrv  , 
                          '(A) Pop. vulnerável com menor acesso ao SUS para triagem' = no_acess_hosp  , 
                          '(B) Pop. vulnerável com menor acesso ao SUS para internação' = no_acess_leit ,
                          '(B) / Pop. vulnerável (%)' = no_acess_leit_prop
                          )
tabela2 <- setDT(tabela2)[order(-`(B) Pop. vulnerável com menor acesso ao SUS para internação`)]

# renamte columns
tabela3 <-  dplyr::select(tb_hosp,  'Município' = name_muni,
                          'N. de leitos*' = total_de_leits ,
                          'Pop. Total (em milhares)' = pop_total)

tabela3$`N. de leitos para cada 10 mil habitantes` <- tabela3$`N. de leitos*`/ tabela3$`Pop. Total (em milhares)` *10
tabela3$`N. de leitos para cada 10 mil habitantes` <- round(tabela3$`N. de leitos para cada 10 mil habitantes`, 1)
tabela3 <- setDT(tabela3)[order(-`N. de leitos para cada 10 mil habitantes`)]

# 4) salva tabela no google sheets
# googlesheets4::sheets_create("covid19_notaIpea", sheets = list(table1 = tb ))

# atualizatabela no google sheets
googlesheets4::sheets_write( data=tabela2, ss='https://docs.google.com/spreadsheets/d/1JybBxNGy9l6wMSuMGAGN6wG83EQaMXfTIwu8R72fVjU/edit#gid=1640948460', sheet = 'table2')
googlesheets4::sheets_write( data=tabela3, ss='https://docs.google.com/spreadsheets/d/1JybBxNGy9l6wMSuMGAGN6wG83EQaMXfTIwu8R72fVjU/edit#gid=1640948460', sheet = 'table3')

