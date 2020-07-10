source("../../../acesso_oport_kaue/R/fun/setup.R")



# 1) DATA SETUP -------------------------------------------------------------------------------



# Open hospitals
hosp <- fread('../data/cnes/cnes_fev_hex.csv')

# subset apenas tipos de unidade que podem fazer triagem ou internar
to_keep <- '72|73|01|02|04|05|07|15|20|21'
hosp <- subset(hosp, TIPO_UNIDADE %like% to_keep)

# protocolo de manual de atendimento para triagem
# atencao primaria
# pronto atendimento
# pronto socorro,
# atendimento pre-hospitalar (SAMU, moveis)
# hospitais

# manter apenas que teria condicoes de fazer primeiro antendimento
# de assistencia aos casos suspeitos, confirmados, confirmados graves, 
# confirmados severos

"01 - POSTO DE SAUDE"                                    
"02 - CENTRO DE SAUDE/UNIDADE BASICA"                    
"04 - POLICLINICA"                                       
"05 - HOSPITAL GERAL"                                    
"07 - HOSPITAL ESPECIALIZADO"                            
"15 - UNIDADE MISTA"                                     
"20 - PRONTO SOCORRO GERAL"                              
"21 - PRONTO SOCORRO ESPECIALIZADO"                      
"72 - UNIDADE DE ATENCAO A SAUDE INDIGENA"               
"73 - PRONTO ATENDIMENTO"                                


# Summarize
hosp[, saude_total := ifelse( is.na(CNES), 0, 1) ]
hex_saude <- hosp[, .(saude_total = sum(saude_total, na.rm = T),
                      # saude_baixa = sum(health_low, na.rm = T),
                      # saude_media = sum(health_med, na.rm = T),
                      # saude_alta  = sum(health_high, na.rm = T),
                      quant_leitos  = sum(quant_leitos, na.rm = T),
                      quant_resp  = sum(quant_resp, na.rm = T)
                    ),
                    by = .(code_muni, id_hex)]






# FUNCTION ------------------------------------------------------------------------------------

# Calculate access

# sigla_muni <- "for" ; ano <- 2019

calcular_acess_muni <- function(sigla_muni, ano=2019) {
  
  # status message
  message('Woking on city ', sigla_muni, ' at year ', '2019',  '\n')
  
  # get muni code
  temp_code_muni <- subset(munis_df_2019, abrev_muni==sigla_muni)$code_muni
  temp_name_muni <- subset(munis_df_2019, abrev_muni==sigla_muni)$name_muni
  
  
  #  0) Subset hospitals
  hex_saude_temp <- subset(hex_saude, code_muni == substr(temp_code_muni, 1,6))
  
  
  # 1) Abrir tttmatrix ---------------------------------------------------
  ttmatrix <- read_rds(sprintf("E:/data/ttmatrix_agregada_cor/%s/ttmatrix_agregada_cor_%s_%s.rds", ano, sigla_muni, ano))
  # ttmatrix dist
  ttmatrix1 <- read_rds(sprintf("E:/data/output_distmatrix/distmatrix_%s_2019.rds", sigla_muni))
  
  # exclude od pairs with NA
  setDT(ttmatrix1)[, dist := fifelse(origin==destination , 0 , dist )]
  ttmatrix1 <- ttmatrix1[!is.na(dist)]
  # dist to km
  ttmatrix1[, dist := dist/1000]
  
  
  
  # harmonize columns and bind
  ttmatrix[, dist := NA ]
  ttmatrix1[, tt_median := NA ]
  setcolorder(ttmatrix1, names(ttmatrix))
  ttmatrix <- rbind(ttmatrix, ttmatrix1)
  
  rm(ttmatrix1)
  gc(reset=T)
  
  # 2) Agregar dados de uso do solo a ttmatrix --------------------------
  
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_muni)
  hexagonos_sf <- readr::read_rds(dir_hex)
  sum(hexagonos_sf$pop_total)
  
  # Filtrar apenas colunas com info demograficas na origem
  hex_orig <- setDT(hexagonos_sf)[, .(id_hex, pop_total, quintil, decil, renda_capita,
                                      idade_0a9, idade_10a14, idade_15a19, idade_20a29, 
                                      idade_30a39, idade_40a49, idade_50a59,    
                                      idade_60a69, idade_70, geometry)]
  

  # Merge dados de origem na matrix de tempo de viagem
  ttmatrix[hex_orig, on = c("origin" = "id_hex"),  
           c('pop_total','quintil','decil',
             'idade_0a9', 'idade_10a14', 'idade_15a19', 'idade_20a29', 
             'idade_30a39', 'idade_40a49', 'idade_50a59',    
             'idade_60a69', 'idade_70') :=
             list(i.pop_total, i.quintil, i.decil, 
                  i.idade_0a9, i.idade_10a14, i.idade_15a19, i.idade_20a29,
                  i.idade_30a39, i.idade_40a49, i.idade_50a59,
                  i.idade_60a69, i.idade_70
             )]
  
  
  # Agroupar populacao acima de 50 anos nesses hex
  ttmatrix[, idade_50 := idade_50a59 + idade_60a69 + idade_70]
  ttmatrix[, idade_60 := idade_60a69 + idade_70]
  
  
  
  # Access 1) closest facility (analise agregada) -------------------------------------------------------------------------
  
  
  # Trazer entao dados dos hospitais
  ttmatrix[hex_saude, on = c("destination" = "id_hex"),  
           c("saude_total", "quant_leitos", "quant_resp") := 
             list(i.saude_total, i.quant_leitos, i.quant_resp)
           ]
  
  
  
  # replace NAs with 0
  for (j in names(ttmatrix)){ 
    set(ttmatrix, which(is.na(ttmatrix[[j]])),j,0)  }
  
  
  
  # calculate access
  acess_tmi <- ttmatrix[, .(tmi_hosp = min(tt_median[which(saude_total >= 1)]),   # tempo min. para chegar ate hosp mais proximo
                            dmi_hosp = min(dist[which(saude_total >= 1)]),   # distancia min. para chegar ate hosp mais proximo
                            dist_leit = min(dist[which(quant_leitos >= 1)]), # distancia min. para chegar ate leito mais proximo
                            quant_hosp = sum(saude_total[which(tt_median <= 30)], na.rm=T),# quant de hosp acessiveis em menos de 30 min
                            quant_leit = sum(quant_leitos[which(dist <= 5)], na.rm=T) # leitos a uma distancia menor do que 5km
  ),
  by=.(city, mode, origin, decil, idade_50, idade_60)]
  

  # # se eh infinito, estabelecer valor maximo (no caso, 60 minutos)
  # acess_tmi <- acess_tmi[, TMI_hosp := fifelse(is.infinite(TMI_walk_hosp), 60, 
  #                                                   TMI_walk_hosp)]
  
  # # Extrair hexagonos problematicos
  # # Por enquanto, para 50 minutos
  # tt <- 60
  # 
  # hex_prob <- acess_tmi[TMI_walk_hosp >= tt]$origin
  # 
  # # Extrair hexagonos
  # hex_df_prob <- hexagonos_sf[id_hex %in% hex_prob, .(id_hex, geometry, sigla_muni,
  #                                                     decil, quintil, renda_capita,
  #                                                     idade_50a59, idade_60a69, idade_70)]
  # 
  # 
  # # Selecionar colunas
  # hex_df_prob_idade <- hex_df_prob_idade[, .(id_hex, sigla_muni, 
  #                                            renda_capita, quintil, decil, 
  #                                            idade_50, 
  #                                            geometry)]
  
  # Salvar
  path_out <- sprintf("../data/output_tmi_agreg/acess_tmi_agreg_%s.csv", sigla_muni)
  data.table::fwrite(acess_tmi, path_out)
  gc(reset = T)
  
}


# aplicar funcao --------------------------------------------------------------
future::plan(future::multiprocess)
furrr::future_map(.x=munis_df_2019$abrev_muni,
                  .f=calcular_acess_muni,
                  .progress = T)

