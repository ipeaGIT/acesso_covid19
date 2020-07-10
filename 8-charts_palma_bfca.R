
####### Create inequality charts


####### read data ---------------------------------

  # list bfca files
  bfca_files <- list.files(path = '../data/output_bfca/', pattern ='bfca' , full.names = T)
  
  
  # read bfca
  bca_access <- lapply(X=bfca_files, FUN=read_rds) %>% rbindlist()
  bca_access <- subset(bca_access, threshold)
  summary(bca_access$threshold)
  table(bca_access$threshold)
  
  # list hex data
  
  # read hex data
  hex_files <- list.files(path = '../data/hex_agregados_proj/', pattern ='hex_agregados_proj_' , full.names = T)
  hexagonos_sf <- lapply(X=hex_files, FUN=read_rds) %>% rbindlist()
  
  # remove hexagons with zero pop
  hexagonos_sf <- subset(hexagonos_sf, pop_total>0)
  
  
  # merge access data
  access <- left_join(hexagonos_sf, 
                         bca_access, by=c("id_hex"="origin"))
  setDT(access)
  

  
a <- subset(access,  sigla_muni == 'spo')
head(a$pop_total)

ggplot()+ geom_boxplot(data = a, aes(x=factor(decil), y=BFCA, weight=pop_total))


####### palma chart ---------------------------------


####### palma by income
palma_income0 <- access[, .(bfca=weighted.mean(x=BFCA, w = pop_total, na.rm=T)), by=.(sigla_muni, decil)]

palma_income1 <- palma_income0[, .(palma_ratio = bfca[which(decil==10)] / mean(bfca[which(decil<5)]) ), by=sigla_muni]


chart_palma_income <- 
  
  palma_income1 %>%
  mutate(sigla_muni = factor(sigla_muni, levels = munis_df_2019$abrev_muni, labels = munis_df_2019$name_muni)) %>%
  mutate(sigla_muni = fct_reorder(sigla_muni, palma_ratio)) %>%
  ggplot()+
    geom_col(aes(y = palma_ratio, x = sigla_muni))+
    geom_hline(yintercept = 1, color = "grey90", linetype = "dashed")+
     scale_y_continuous(breaks = seq(0, 6, 2), labels = c('0', '2x', '4x', '6x'))+
    coord_flip()+
    theme_ipsum(grid = "X")+
    labs(x = "", y = "Palma Ratio\n10% wealthiest / 40% poorest") +
  labs(subtitle = 'A) by income')  + 
  theme(plot.margin=grid::unit(c(.1,.1,.1,.1), "mm"))

ggsave(chart_palma_income,
       file= "../figuras/figures_en_paper/figure_palma_income.png",
       dpi = 300, width = 18, height = 14, units = "cm")




####### palma by race

palma_race <- access[, .(palma_race = weighted.mean(BFCA , cor_branca, na.rm=T) /
                                       weighted.mean(BFCA , cor_negra, na.rm=T)), by=sigla_muni]

chart_palma_race <- 
  
palma_race %>%
  mutate(sigla_muni = factor(sigla_muni, levels = munis_df_2019$abrev_muni, labels = munis_df_2019$name_muni)) %>%
  mutate(sigla_muni = fct_reorder(sigla_muni, palma_race)) %>%
  ggplot()+
  geom_col(aes(y = palma_race, x = sigla_muni)) +
  geom_hline(yintercept = 1, color = "grey90", linetype = "dashed")+
 # scale_y_continuous(breaks = seq(0, 3, .5))+
  scale_y_continuous(breaks = seq(0, 1.5, .5), labels = c('0', '0.5x', '1x', '1.5x'))+
  coord_flip()+
  theme_ipsum(grid = "X")+
  labs(x = "", y = "Ratio White /Black pop.") +
  labs(subtitle = 'B) by color') + 
  theme(plot.margin=grid::unit(c(.1,.1,.1,.1), "mm"))

ggsave(chart_palma_race,
       file= "../figuras/figures_en_paper/figure_palma_race.png",
       dpi = 300, width = 18, height = 14, units = "cm")


library(patchwork)

chart_palma_income + chart_palma_race 

ggsave(
       file= "../figuras/figures_en_paper/figure_palma.png",
       dpi = 300, width = 16, height = 16, units = "cm")




library(cowplot)
plot_grid(chart_palma_income , chart_palma_race)  + theme(plot.margin=grid::unit(c(.5,.5,.5,.5), "mm"))
ggsave(
  file= "../figuras/figures_en_paper/figure_palma-cow.png",
  dpi = 300, width = 16, height = 16, units = "cm")
