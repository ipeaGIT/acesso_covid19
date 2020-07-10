source("../../../acesso_oport_kaue/R/fun/setup.R")

library("openxlsx")
# mydf <- read.xlsx("BigExcelFile.xlsx", sheet = 1, startRow = 2, colNames = TRUE)
# xlsx::read.xlsx
## Carrega projecoes de pop

proj_pop <- openxlsx::read.xlsx('../data//pop_projecao/Projecoes_populacionais_municipais_2010_2030_revisado20200202.xlsx'
                                , sheet = 1, colNames = TRUE)

sigla_muni <- "for"

get_pop_prop_2020 <- function(sigla_muni) {
  
  
  ### get population of that muni in 2020
  setDT(proj_pop)
  temp_code_muni <- munis_df_2019[abrev_muni == sigla_muni]$code_muni
  proj_pop_muni <- proj_pop[ Ano==2020 &  Armenor == temp_code_muni, ]
  
  # drop columns
  cols_to_drop <- c(1, 2, 3, 5)
  cols_to_drop <- names(proj_pop_muni)[cols_to_drop]
  proj_pop_muni[, c(cols_to_drop) := NULL ]
  
  # sum pop by age (disregard sex diference)
  proj_pop_muni <- proj_pop_muni %>%
    mutate(Armenor = as.character(Armenor)) %>%
    group_by(Armenor) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  # change column names
  proj_pop_muni <- janitor::clean_names(proj_pop_muni)
  
  # idade_0a9, idade_10a14, idade_15a19, idade_20a29, 
  # idade_30a39, idade_40a49, idade_50a59,    
  # idade_60a69, idade_70
  
  proj_pop_muni_fim <- proj_pop_muni %>%
    mutate(sigla_muni = sigla_muni) %>%
    # # drop total column
    # select(-total) %>%
    # group ages according to previous determinationn
    group_by(sigla_muni, total) %>%
    summarise(idade_0a9   = sum(x0_a_5, x5_a_10),
              idade_10a14 = sum(x10_a_15),
              idade_15a19 = sum(x15_a_20),
              idade_20a29 = sum(x20_a_25, x25_a_30),
              idade_30a39 = sum(x30_a_35, x35_a_40),
              idade_40a49 = sum(x40_a_45, x45_a_50),
              idade_50a59 = sum(x50_a_55, x55_a_60),
              idade_60a69 = sum(x60_a_65, x65_a_70),
              idade_70    = sum(x70_a_75, x75_a_80, x80_a_85, x85_a_90, x90)
    )
  # # transform to long format
  # gather(grupo, total_idade, x0_a_5:x90)
  
  
}


# apply function
a <- lapply(munis_df_2019$abrev_muni, get_pop_prop_2020) %>% rbindlist()

# export
write_rds(a, "../data/pop_projection_2020.rds")
