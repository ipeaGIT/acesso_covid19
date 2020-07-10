source("../../../acesso_oport_kaue/R/fun/setup.R")


# open projections
proj <- read_rds("../data/pop_projection_2020.rds")

# sigla_munii <- "for"; ano <- 2019

correction_pop_hex <- function(sigla_munii, ano = 2019) {
  
  
  # open pop data
  dir_hex <- sprintf("../../../data/hex_agregados/%s/hex_agregado_%s_09_%s.rds", ano, sigla_munii, ano)
  hexagonos_sf <- readr::read_rds(dir_hex)

  # calculate totals
  total_city_2010 <- sum(hexagonos_sf$pop_total)
  total_city_2020 <- proj[sigla_muni == sigla_munii]$total
  
  hex_orig_prop <- hexagonos_sf %>%
    # calculate proportion of each hex in relation to city total
    mutate(prop_total = pop_total/total_city_2010) %>%
    # calculate proportion of each age group in each hex
    mutate_at(vars(matches("idade")), function(x) x/hexagonos_sf$pop_total) %>%
    # multiple 2020 pop total to each prop hex (this will fix pop_total)
    mutate(pop_total = total_city_2020 * prop_total)
  
  
  
  hex_orig_prop <- hex_orig_prop %>%      
    # round pop_total
    mutate(pop_total = round(pop_total)) %>%
    # apply 2010 age proportions to the new pop_total in each hexagon
    mutate_at(vars(matches("idade")), function(x) round(hex_orig_prop$pop_total * x))
  
  # save
  write_rds(hex_orig_prop, sprintf("../data/hex_agregados_proj/hex_agregados_proj_%s.rds", sigla_munii))
  
}

# apply function
lapply(munis_df_2019$abrev_muni, correction_pop_hex)
