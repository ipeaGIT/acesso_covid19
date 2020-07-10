source("../../../acesso_oport_kaue/R/fun/setup.R")



# 1. HOSPITAIS --------------------------------------------------------------------------------

#### Traz info de estabs geo do Projeto Acesso a Oportunidades ----------------------------------
# 20 maiores municipios do Brasil

df <- fread('../data/cnes/cnes_leitos_resp_fev.csv')

estabs2019 <- readr::read_rds("../../../data/hospitais/2019/health_facilities2019_filtered.rds")

# merge para adiciinar equipamentos
estabs2019$CNES <- as.numeric(estabs2019$CNES)
estabs2019_equip <- left_join(estabs2019[, c('CNES','code_muni', 'TIPO_UNIDADE', 'lon', 'lat')], df, by='CNES')
head(estabs2019_equip)


### Add hospitais de camapnha ------------------------

# ler hospitais de campanha
hosp_camp <- readxl::read_excel('../data/hospitais_campanha/hospitais de campanha_20200402.xlsx'
                                , sheet = 1, col_names = T)

hosp_camp <- subset(hosp_camp, code_muni %in% munis_df_2019$code_muni)


# Reorganizar dados 
setDT(hosp_camp)[, CNES := paste0(MUNICÍPIO, '-', code_muni,'-',LOCAL)]
hosp_camp <- dplyr::select(hosp_camp, 
                           CNES,
                           code_muni= code_muni,
                           lon = LONG,
                           lat = LAT,
                           CODUFMUN= code_muni,
                           COD_CEP = LOCAL,
                           quant_leitos =  leitos_uti
)


hosp_camp$TIPO_UNIDADE <- '00 - HOSPITAL DE CAMPANHA'
hosp_camp$quant_resp <- hosp_camp$quant_leitos
hosp_camp <- na.omit(hosp_camp)
hosp_camp$code_muni <- substring(hosp_camp$code_muni,1,6)

# empilhar com dados de hospitais em geral
all_estabs <- rbind(estabs2019_equip, hosp_camp )

# convert to sf
# all_estabs <- all_estabs %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

# filter estabs nos 20 munis.
estabs_20 <- subset(all_estabs, code_muni %in% substring(munis_df_2019$code_muni,1,6))


# subset apenas tipos de unidade que podem fazer triagem ou internar
to_keep <- '00|72|73|01|02|04|05|07|15|20|21'
estabs_20 <- subset(estabs_20, TIPO_UNIDADE %like% to_keep)


# replace NAs with 0
estabs_20[, quant_leitos := ifelse( is.na(quant_leitos), 0, quant_leitos) ]
estabs_20[, quant_resp := ifelse( is.na(quant_resp), 0, quant_resp) ]

# subset apenas com leitos & respiradores
hosp_covid <- estabs_20[quant_leitos >= 1 & quant_resp >= 1]

# selecionar colunas
hosp_covid <- dplyr::select(hosp_covid, 
                            CNES,
                            code_muni,
                            tipo_unidade = TIPO_UNIDADE,
                            quant_leitos,
                            quant_resp,
                            lon,
                            lat
)



# export
write_csv(hosp_covid, "../data/output_data/dados_hospitais_covid.csv")





# 2. ACCESS TMI -------------------------------------------------------------------------------

# sigla_munii <- 'for'
# sigla_munii <- 'rio'
# sigla_munii <- 'spo'


gerar_dados_tmi <- function(sigla_munii) {
  
  
  # read access data
  dir_tmi <- sprintf("../data/output_tmi_agreg/acess_tmi_agreg_%s.csv", sigla_munii)
  tmi <- data.table::fread(dir_tmi)
  
  # read hex data
  dir_hex <- sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_munii)
  hexagonos_sf <- readr::read_rds(dir_hex)
  
  
  # filtra apenas renda baixa
  tmi_decil_1_5 <- subset(tmi, decil <= 5)
  
  # filtra apenas hexagonos com pop acima de  50 anos
  tmi_pop50 <- subset(tmi_decil_1_5, idade_50  > 0)
  
  
  
  
  # 1) ACESS 30 MIN HOSPITAIS
  # filtra apenas hexagonos q demora mais de 30 min. caminhando para qualquer hospital
  no_acess_hosp <- subset(tmi_pop50, mode=='walk' & quant_hosp==0)
  
  # trazer o dist da bse de carro, que esta correto
  no_acess_hosp <- merge(select(no_acess_hosp, -dist_leit),
                         tmi_pop50 %>% filter(mode == 'car') %>% select(origin, dist_leit),
  ) %>%
    # identificar qual o tipo de problema
    mutate(acess_prob = "hosp_30min")
  
  
  
  # 2) ACESS 5 KM LEITOS
  # filtra apenas hexagonos distania maior do que 5 Km para qualquer hospital com leito
  no_acess_leit <- subset(tmi_pop50, mode=='car' & quant_leit==0)
  
  # trazer o tmi da bse de caminhada, que esta correto
  no_acess_leit <- merge(select(no_acess_leit, -tmi_hosp),
                         tmi_pop50 %>% filter(mode == 'walk') %>% select(origin, tmi_hosp),
  ) %>%
    # identificar qual o tipo de problema
    mutate(acess_prob = "leitos_5km")
  
  # bind dataframes
  no_acess_fim <- rbind(no_acess_hosp, no_acess_leit)
  
  # identify duplicates and rename them
  # no_acess_fim <- no_acess_fim %>%
  #   add_count(origin)
  
  # select columns
  no_acess_fim_select <- no_acess_fim %>%
    # trazer nome da cidade
    left_join(select(munis_df_2019, abrev_muni, name_muni), by = c("city" = "abrev_muni")) %>%
    # formatar tmi
    mutate(tmi_hosp = ifelse(is.infinite(tmi_hosp), 60, tmi_hosp)) %>%
    select(nome_muni = name_muni, id_hex = origin, decil_renda = decil, idade_50, tmi_hosp, dist_leito = dist_leit, acess_problema = acess_prob)
  
  # merge access data
  no_acess_fim_select_sf <- left_join(no_acess_fim_select,
                                      select(hexagonos_sf, id_hex, geometry),
                                      by=c("id_hex"))
  
}

# 
# #### quick check #### quick check
# no_acess_fim_select_sf <- st_sf(no_acess_fim_select_sf, crs = 4326)
# 
# a <- subset(no_acess_fim_select_sf, acess_problema=='hosp_30min')
# summary(a$tmi_hosp) # ok
# 
# b_teste <- subset(no_acess_fim_select_sf, acess_problema=='leitos_5km')
# summary(b_teste$dist_leito) # tem 30 casos abaixo de 5 Km
# 
# c <- subset(b, tmi_hosp <5)
# mapview(c)
# 
# mapview(filter(b_teste, is.infinite(dist_leito)))
# 
# 
# 
# ggplot() + 
#   geom_sf(data=no_acess_fim_select_sf, aes(fill=idade_50)) +
#   facet_grid(~acess_problema)
# 
# #### quick check #### quick check



# apply fun and transform to sf
lapply(munis_df_2019$abrev_muni, gerar_dados_tmi) %>%
  rbindlist() %>%
  st_sf(crs = 4326) %>%
  # export
  st_write("../data/output_data/dados_acesscovid.gpkg")
  










# 3. ACCESS CMP ----------------------------------------------------------------------------------

gerar_dados_cmp <- function(sigla_munii) {
  
acess2 <- read_rds(sprintf("../data/output_acess_cmp/acess_cmp_%s.rds", sigla_munii))

# read hex data
dir_hex <- sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_munii)
hexagonos_sf <- readr::read_rds(dir_hex)


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
  sfc_as_cols() %>%
  # transformar leitos para mil habitantes
  mutate(ppr = ppr * 10000)


temp <- select(acess2_points, 
            sigla_muni,
            idhex = destination,
            CNES,
            # tipo_unidade = TIPO_UNIDADE,
            # campanha = unidade,
            # leitosvent = quant_leitos.y,
            pop_captacao = catchment_w,
            leitvent_por_10kpop = ppr
            )
            
return(temp)
}

# dados de catchment de todos hospitais de todas cidades
catchments <- lapply(munis_df_2019$abrev_muni, gerar_dados_cmp) %>% rbindlist()

# traz info dos hospitais
temp_output <- left_join(catchments, hosp_covid, by='CNES')

# reorder columns
temp_output <- select(temp_output,
                      sigla_muni,
                      code_muni,
                      idhex,
                      CNES,
                      tipo_unidade,
                      pop_captacao,
                      quant_leitosuti = quant_leitos,
                      quant_resp,
                      leitvent_por_10kpop,
                      lon,
                      lat
                      )

# export
write_csv(temp_output, "../data/output_data/dados_hospitais_covid.csv")
