source("../../../acesso_oport_kaue/R/fun/setup.R")


# 1) DATA SETUP -------------------------------------------------------------------------------

# Open correction factors
slack_factor <- xlsx::read.xlsx(('../data/slack_factor/Local_resi_inter_AIHSUS_2019_edited-columns.xlsx'),
                                sheetIndex = 1)


# Open hospitals
hosp <- fread('../data/cnes/cnes_fev_hex.csv')

# subset apenas tipos de unidade que podem fazer triagem ou internar
to_keep <- '72|73|01|02|04|05|07|15|20|21'
hosp <- subset(hosp, TIPO_UNIDADE %like% to_keep)


# replace NAs with 0
hosp[, saude_total := ifelse( is.na(CNES), 0, 1) ]
hosp[, quant_leitos := ifelse( is.na(quant_leitos), 0, quant_leitos) ]
hosp[, quant_resp := ifelse( is.na(quant_resp), 0, quant_resp) ]

# subset apenas com leitos & respiradores
    hosp_covid <- hosp[quant_leitos >= 1 & quant_resp >= 1]

# trunca analise combinada de leitos e respitadores
    hosp_covid[, quant_leitos := min(quant_leitos, quant_resp), by=CNES ]

    
# Summarize
hex_saude <- hosp_covid[, .(saude_total = sum(saude_total, na.rm = T),
                            # saude_baixa = sum(health_low, na.rm = T),
                            # saude_media = sum(health_med, na.rm = T),
                            # saude_alta  = sum(health_high, na.rm = T),
                            quant_leitos  = sum(quant_leitos, na.rm = T),
                            quant_resp  = sum(quant_resp, na.rm = T)
),
by = .(code_muni, id_hex)]



# FUNCTION ------------------------------------------------------------------------------------

# sigla_munii <- "rio"; ano <- 2019

calcular_acess_comp <- function(sigla_munii, ano = 2019) {
  
  # get muni code
  temp_code_muni <- subset(munis_df_2019, abrev_muni==sigla_munii)$code_muni
  temp_name_muni <- subset(munis_df_2019, abrev_muni==sigla_munii)$name_muni
  
  
  # 0) Subset slack_factor in selected city
  slack_f <- subset(slack_factor, code_muni == substr(temp_code_muni, 1,6))$ratio
  
  # 0) Subset hospitals in selected city
  hex_saude_temp <- subset(hex_saude, code_muni == substr(temp_code_muni, 1,6) )
  
  # 1) Abrir tttmatrix
  # This one will be used to calculate 'active' access
  ttmatrix <- read_rds(sprintf("E:/data/output_distmatrix/distmatrix_%s_2019.rds", sigla_munii))
  # exclude od pairs with NA
  ttmatrix <- setDT(ttmatrix)[!is.na(dist)]
  # dist to km
  ttmatrix[, dist := dist/1000]

 #### hex id verification. O hexagono do hospital esta na ttmatrix ???
  all_dests <- unique(ttmatrix$destination)
  erro <- setdiff(hex_saude_temp$id_hex, all_dests)  
  if(length(erro) > 0){ message(paste(erro, collapse=" -- "))}
  if(length(erro) > 0){ stop()}
  
   
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_munii)
  hexagonos_sf <- readr::read_rds(dir_hex)
  
  # Filtrar apenas colunas com info demograficas na origem
  hex_orig <- setDT(hexagonos_sf)[, .(id_hex, pop_total, quintil, decil, geometry)]
  
  # corrigir populacao com slack factor
  sum(hex_orig$pop_total)
  hex_orig[, pop_total := pop_total * slack_f]
  sum(hex_orig$pop_total)
  
  # Merge dados de destino na matrix de tempo de viagem
  setDT(ttmatrix)[hex_orig, on = c("origin" = "id_hex"),
                  c('pop_total','quintil','decil') :=
                    list(i.pop_total, i.quintil, i.decil
                    )]
  
  # Trazer entao dados dos hospitais
      # ttmatrix[hex_saude_temp, on = c("destination" = "id_hex"),
      #          c("saude_total", "quant_leitos", "quant_resp") :=
      #            list(i.saude_total, i.quant_leitos, i.quant_resp)
      #          ]
  ttmatrix <- dplyr::left_join(ttmatrix, hex_saude_temp, 
                                    by = c("destination" = "id_hex"))
 

 
   
  # replace NAs with 0
  setDT(ttmatrix)
  for (j in names(ttmatrix)){ 
    set(ttmatrix, which(is.na(ttmatrix[[j]])),j,0) }
  
  
  # filtrar A matrix de viagem até os hospitais com leitos e respiradores
  ttmatrix_hosp <- ttmatrix[destination %in% hex_saude_temp$id_hex]
  
  
  # 1) PPR - WITH DECAY! ----------------------------------------------------------------------
  
  # calculate weights?
  neg_exp_f <- function(tt, b0) {
    
    exp(-b0*tt)
    
  }
  
  # gaussian
  mgaus_f <- function(t_ij,b0){exp(-t_ij^2/b0)}
  
  # calculate impedance (choose one)
  # ttmatrix_hosp[, impedance := neg_exp_f(dist, 0.45) ]
  # ttmatrix_hosp[, impedance := mgaus_f(dist, 200) ]
   ttmatrix_hosp[, impedance := fifelse(dist<=15, 1, 0) ]
  head(ttmatrix_hosp)
  # calculate weights (normalized impedance by origin)
  ttmatrix_hosp[, wi := impedance/sum(impedance), by=origin   ]
  
  # calculate weights (normalized impedance by destination)
  # ttmatrix_hosp[, wj := impedance/sum(impedance), by=dest ]
  
  
  ## Step 1
  # calcula pop e hospitais proporcionais ao peso
  
  # Demand - reaportion the demand to each each hospital
  demand1 <- ttmatrix_hosp[, .(catchment_w = sum(pop_total * wi, na.rm = TRUE),
                          quant_leitos = quant_leitos[1L]), by=destination]
  
  
  
  # calculate  ppr
  ppr <- demand1[, ppr := quant_leitos / catchment_w][]
  
  # filtrar hexagonos com leitos e respiradores
  ppr <- ppr[destination %in% hex_saude_temp$id_hex]
  
  # trazer a informacao dos hospitais (nome etc)
  ppr_hosp <- ppr %>%
    left_join(select(hosp_covid, id_hex, CNES, TIPO_UNIDADE, quant_leitos),
              by = c("destination" = "id_hex"))
  

  # para hexagonos que tem mais de 1 hospital, o PPR vai ser proporcional a quantidade de leitos
  # de cada um deles
  ppr_hosp <- ppr_hosp %>%
    group_by(destination) %>%
    mutate(prop_leitos = quant_leitos.y / quant_leitos.x) %>%
    ungroup() %>%
    # multiplicar esse prop pelo PPR
    mutate(ppr = ppr * prop_leitos) %>%
    mutate(sigla_muni = sigla_munii)
  

  # export
  write_rds(ppr_hosp,
            sprintf("../data/output_acess_cmp/acess_cmp_%s.rds", sigla_munii))
  
}


# apply function ------------------------------------------------------------------------------
plan(multiprocess)
future_lapply(munis_df_2019$abrev_muni, calcular_acess_comp)


pblapply(X=munis_df_2019$abrev_muni, FUN = calcular_acess_comp)

