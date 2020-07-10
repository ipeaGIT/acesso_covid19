source("../../../acesso_oport_kaue/R/fun/setup.R")

### Leitura de equipamentos de hospitais


#### LEITOS ----------------------------------

leitos <- data.table::fread(file='\\\\storage1\\ContasSHA\\Contas\\DADOS_COVID\\CNES_LT_2020.csv')
head(leitos)

# Filter mes de fevereiro
leitos_fev <- subset(leitos, COMPETEN ==202002)


# Filter leitos UTI
leitos_fev_UTI <- subset(leitos_fev, CODLEITO %in% c(74,75,76))


# somar numero de leitos
CNES_leitos <- leitos_fev_UTI[, .(quant_leitos = sum(QT_SUS)), by=CNES]
head(CNES_leitos)

sum(CNES_leitos$quant_leitos)






#### Respiradores ----------------------------------

equips <- data.table::fread(file='\\\\storage1\\ContasSHA\\Contas\\DADOS_COVID\\CNES_EQ_2020.csv')
head(equips)

# Filter mes de fevereiro
equips_fev <- subset(equips, COMPETEN ==202002)


# Filter respiradores
equips_fev_resp <- subset(equips_fev, CODEQUIP==64)


# somar numero de leitos
CNES_resp <- equips_fev_resp[, .(quant_resp = sum(QT_USO)), by=CNES]
head(CNES_resp)

sum(CNES_resp$quant_resp)



#### Establecimentos ----------------------------------
estabs <- data.table::fread(file='\\\\storage1\\ContasSHA\\Contas\\DADOS_COVID\\CNES_ST_2020.csv')
head(estabs)

# Filter mes de fevereiro
estabs_fev <- subset(estabs, COMPETEN ==202002)

# subset columns
cnes_estabs <- estabs_fev[,.(CNES, CODUFMUN, COD_CEP)]






#### Merge equipamentos e establecimentos ----------------------------------

df <- left_join(cnes_estabs, CNES_leitos)
df <- left_join(df, CNES_resp)
df

# salva
fwrite(df, '../data/cnes/cnes_leitos_resp_fev.csv')



df <- fread('../data/cnes/cnes_leitos_resp_fev.csv')



#### Traz info de estabs geo do Projeto Acesso a Oportunidades ----------------------------------
# 20 maiores municipios do Brasil

estabs2019 <- readr::read_rds("../../../data/hospitais/2019/health_facilities2019_filtered.rds")

# merge para adicoinar equipamentos
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
                           TIPO_UNIDADE = code_muni,
                           lon = LONG,
                            lat = LAT,
                            CODUFMUN= code_muni,
                            COD_CEP = LOCAL,
                            quant_leitos =  leitos_uti
                            )


hosp_camp$TIPO_UNIDADE <- 73
hosp_camp$quant_resp <- hosp_camp$quant_leitos
hosp_camp <- na.omit(hosp_camp)
hosp_camp$code_muni <- substring(hosp_camp$code_muni,1,6)

# empilhar com dados de hospitais em geral
all_estabs <- rbind(estabs2019_equip, hosp_camp )

# convert to sf
all_estabs <- all_estabs %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

# filter estabs nos 20 munis.
estabs_20 <- subset(all_estabs, code_muni %in% substring(munis_df_2019$code_muni,1,6))






#### Identifica Hexagono de cada estab ----------------------------------

# sigla_muni <- 'rio'

fun_find_hex <- function(sigla_muni){
  
  # # status message
  # message('Woking on city ', sigla_muni, '\n')
  
  # subset hospitais do muni
  codemuni6 <-  subset(munis_df_2019, abrev_muni == sigla_muni)$code_muni %>% substr(1,6)
  temp_cnes <- subset(estabs_20, code_muni==codemuni6)


  # Pegar endereco das grades e hexagonos em todas resolucoes
  grade_file <- paste0("../../../data/hex_municipio/2019/hex_", sigla_muni, "_09_2019.rds")
  grade_sf <- readr::read_rds(grade_file) %>% dplyr::select( id_hex, geometry)

  # identify hex id
  temp_cnes <- sf::st_intersection(temp_cnes, grade_sf)
  
  #### # Corrige manualmente hospitais que caem em hexagonos q nao estao na ttmatrix

  # Verifica se todos hospitais caem em  hexagonos da ttmatrix
  # se cair nu buraco negro, imputar o hexagono mais perto

    # get possible destinations
    ttmatrix <- read_rds(sprintf("E:/data/output_distmatrix/distmatrix_%s_2019.rds", sigla_muni))
    all_dests <- unique(ttmatrix$destination)
    erro <- setdiff(temp_cnes$id_hex, all_dests)  
    rm(ttmatrix)
  
    for(i in erro){
    # keep only hex de burco negro e valid ttmatrix destinations 
    temp_grade_all <- subset(grade_sf, id_hex %in%  as.character(all_dests))
    temp_grade_i <- subset(grade_sf, id_hex == i )
    # encontra id_hex mais proximo contido na ttmatrix
    nearest <- temp_grade_all[which.min(st_distance(temp_grade_i, temp_grade_all)),]$id_hex
  
    # replace missing id_hex
    setDT(temp_cnes)
    temp_cnes[, id_hex := ifelse(id_hex==i, nearest, id_hex)]
    }
    
  # retorna 
  temp_cnes <- st_sf(temp_cnes)
  return(temp_cnes)
}



# Aplica funcao
cnes_hex <- pblapply(X=munis_df_2019$abrev_muni, FUN = fun_find_hex)
cnes_hex <- do.call('rbind', cnes_hex)
head(cnes_hex)


# save
fwrite(cnes_hex, '../data/cnes/cnes_fev_hex.csv')
