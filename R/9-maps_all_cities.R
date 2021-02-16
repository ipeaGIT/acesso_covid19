
# carregar bibliotecas
library(ggplot2)      # visualizacao de dados
library(ggthemes)     # temas para visualizacao de dados
library(sf)           # leitura e manipulacao de dados espaciais
library(data.table)   # manipulacao de dados
library(geobr)        # dados espaciais do brasil
library(pbapply)      # progress bar
library(readr)        # rapida leitura de dados 
library(stringr)      # operacoes em strings
library(RColorBrewer) # paleta de cores
library(extrafont)    # fontes de texto
library(purrr)
library(dplyr)
library(hrbrthemes)
library(beepr)
library(patchwork)
library(rworldmap)
library(sfheaders)
library(ggmap) # geocoding

options(scipen=10000)

`%nin%` = Negate(`%in%`)

`%nlike%` = Negate(`%like%`)


# generate world map
source("./R/00_worldmap.R")


munis_df <- tribble(
  ~code_muni, ~abrev_muni, ~name_muni,        ~abrev_estado, ~modo_2017, ~modo_2018, ~modo_2019, ~modo_2020,
  2304400,    "for",       "Fortaleza",       "CE",          "todos",    "todos",    "todos",    "todos",
  3550308,    "spo",       "Sao Paulo",       "SP",          "todos",    "todos",    "todos",    "todos",
  3304557,    "rio",       "Rio de Janeiro",  "RJ",          "ativo",    "todos",    "todos",    "todos",
  4106902,    "cur",       "Curitiba",        "PR",          "todos",    "todos",    "todos",    "todos",
  4314902,    "poa",       "Porto Alegre",    "RS",          "todos",    "todos",    "todos",    "todos",
  3106200,    "bho",       "Belo Horizonte",  "MG",          "todos",    "todos",    "todos",    "todos",
  5300108,    "bsb",       "Brasilia",        "DF",          "ativo",    "ativo",    "ativo",    "ativo",
  2927408,    "sal",       "Salvador",        "BA",          "ativo",    "ativo",    "todos",    "todos",
  1302603,    "man",       "Manaus",          "AM",          "ativo",    "ativo",    "ativo",    "ativo",
  2611606,    "rec",       "Recife",          "PE",          "ativo",    "ativo",    "todos",    "todos",
  5208707,    "goi",       "Goiania",         "GO",          "ativo",    "ativo",    "todos",    "ativo",
  1501402,    "bel",       "Belem",           "PA",          "ativo",    "ativo",    "ativo",    "ativo",
  3518800,    "gua",       "Guarulhos",       "SP",          "ativo",    "ativo",    "ativo",    "ativo",
  3509502,    "cam",       "Campinas",        "SP",          "todos",    "todos",    "todos",    "todos",
  2111300,    "slz",       "Sao Luis",        "MA",          "ativo",    "ativo",    "ativo",    "ativo",
  3304904,    "sgo",       "Sao Goncalo",     "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
  2704302,    "mac",       "Maceio",          "AL",          "ativo",    "ativo",    "ativo",    "ativo",
  3301702,    "duq",       "Duque de Caxias", "RJ",          "ativo",    "ativo",    "ativo",    "ativo",
  5002704,    "cgr",       "Campo Grande",    "MS",          "ativo",    "ativo",    "ativo",    "ativo",
  2408102,    "nat",       "Natal",           "RN",          "ativo",    "ativo",    "ativo",    "ativo"
) %>% setDT()



### 0) Mapa com cidades do projeto   ---------------------------------------

# get World map
worldMap <- rworldmap::getMap(resolution = "low") %>% st_as_sf()


# load map of Brazil and municipalities
states_sf <- geobr::read_state(code_state = "all", year = 2018)
munis_sf <- lapply(munis_df$code_muni, geobr::read_municipality) %>% rbindlist() %>% st_sf()
st_crs(munis_sf) <- st_crs(states_sf)

munis_sf <- munis_sf %>%
  left_join(munis_df, by = "code_muni") %>%
  # number it according to order
  mutate(n = case_when(abrev_muni == "man" ~ 1,
                       abrev_muni == "for" ~ 2,
                       abrev_muni == "spo" ~ 3,
                       abrev_muni == "rio" ~ 4,
                       abrev_muni == "rec" ~ 5,
                       abrev_muni == "slz" ~ 6,
                       abrev_muni == "cur" ~ 7,
                       abrev_muni == "bel" ~ 8,
                       abrev_muni == "bsb" ~ 9,
                       abrev_muni == "cam" ~ 10,
                       abrev_muni == "cgr" ~ 11,
                       abrev_muni == "duq" ~ 12,
                       abrev_muni == "goi" ~ 13,
                       abrev_muni == "gua" ~ 14,
                       abrev_muni == "mac" ~ 15,
                       abrev_muni == "bho" ~ 16,
                       abrev_muni == "nat" ~ 17,
                       abrev_muni == "sal" ~ 18,
                       abrev_muni == "sgo" ~ 19,
                       abrev_muni == "poa" ~ 20
  )) %>%
  # format
  mutate(text = paste0(n, ".", " ", name_muni.x)) %>%
  mutate(type = ifelse(abrev_muni %in% c("bho", "cur", "for", "poa", "rec", "rio", "spo"), 
                       "Active and Public Transport",
                       "Active Transport")) %>%
  mutate(color = ifelse(abrev_muni %in% c("bho", "cur", "for", "poa", "rec", "rio", "spo"), 
                        "#469c77", "steelblue4"))

# get centroids of municipalities
munis_centroids <- st_centroid(munis_sf)
munis_tp_centroids <- subset(munis_centroids, code_muni %in% munis_df$code_muni[which(munis_df$modo=='todos')])

munis_tp_centroids_df <- sfheaders::sf_to_df(munis_centroids, fill=T)


# create sp map
sp <- 
  ggplot()+
  geom_sf(data=worldMap, fill="white", color="gray90") +
  geom_sf(data=states_sf, fill="gray85", colour = "gray89") +
  geom_sf(data=st_buffer(munis_centroids, dist =.1), fill="steelblue4", color="gray95", alpha=.8) +
  geom_sf(data=st_buffer(munis_tp_centroids, dist =.1), fill="#469c77", color="gray95", alpha=.8) +
  ggrepel::geom_text_repel(data = filter(munis_tp_centroids_df, abbrev_state == "SP"), aes(x = x, y = y, label = n),
                           segment.size = 3, size=2.5) +
  theme_void() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        panel.background=element_rect(fill = "gray98"),
        panel.border = element_rect(fill = NA))+
  coord_sf(expand = FALSE,
           xlim = c(filter(munis_tp_centroids_df, abrev_muni == "spo")$x-1.2, 
                    filter(munis_tp_centroids_df, abrev_muni == "spo")$x+1.2),
           ylim = c(filter(munis_tp_centroids_df, abrev_muni == "spo")$y-0.8, 
                    filter(munis_tp_centroids_df, abrev_muni == "spo")$y+1.2))

# create rio map
rio <- 
  ggplot()+
  geom_sf(data=worldMap, fill="white", color="gray90") +
  geom_sf(data=states_sf, fill="gray85", colour = "gray89") +
  geom_sf(data=st_buffer(munis_centroids, dist =.1), fill="steelblue4", color="gray95", alpha=.8) +
  geom_sf(data=st_buffer(munis_tp_centroids, dist =.1), fill="#469c77", color="gray95", alpha=.8) +
  ggrepel::geom_text_repel(data = filter(munis_tp_centroids_df, abbrev_state == "RJ"), aes(x = x, y = y, label = n),
                           segment.size = 3, size=2.5)+
  theme_void() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank(), 
        panel.background=element_rect(fill = "gray98"),
        panel.border = element_rect(fill = NA))+
  coord_sf(expand = FALSE,
           xlim = c(filter(munis_tp_centroids_df, abrev_muni == "rio")$x-1.2, 
                    filter(munis_tp_centroids_df, abrev_muni == "rio")$x+1.2),
           ylim = c(filter(munis_tp_centroids_df, abrev_muni == "rio")$y-0.5, 
                    filter(munis_tp_centroids_df, abrev_muni == "rio")$y+1))



# create map

temp_map1 <- 
  ggplot() + 
  geom_sf(data=worldMap, fill="white", color="gray90") +
  geom_sf(data=states_sf, fill="gray85", colour = "gray89") +
  geom_sf(data=st_buffer(munis_centroids, dist =.5), fill="steelblue4", color="gray95", alpha=.8) + # 'springgreen4' steelblue4
  geom_sf(data=st_buffer(munis_tp_centroids, dist =.5), fill="#469c77", color="gray95", alpha=.8) + # 'springgreen4' steelblue4
  ggrepel::geom_text_repel(data = filter(munis_tp_centroids_df, abbrev_state %nin% c("SP", "RJ")), 
                           aes(x = x, y = y, label = n),
                           segment.size = 3, size=3)+
  theme(panel.background = element_rect(fill = "gray98", colour = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid = element_blank()) + 
  labs(x = '', y = '')+
  coord_sf(expand = FALSE, 
           xlim = c(st_bbox(states_sf)[[1]], st_bbox(states_sf)[[3]]),
           ylim = c(st_bbox(states_sf)[[2]], st_bbox(states_sf)[[4]])) # coord_cartesian Coordinates Zoom
# guides(colour = guide_legend(override.aes = list(size=8)))

arrowA <- data.frame(x1 = 14.5, x2 = 10, 
                     y1 = 6.1,  y2 = 5) # 1: arrow!

arrowB <- data.frame(x1 = 16.9, x2 = 17.7, 
                     y1 = 6.1,  y2 = 5.3) # 1: arrow!

library(cowplot)
library(gridExtra)
library(grid)

t1 <- ttheme_default(core=list(
  fg_params=list(fontface=c(rep("plain", 4), "bold.italic")),
  bg_params = list(fill=c(rep(c("grey95", "grey90"),
                              length.out=4), "#6BAED6"),
                   alpha = rep(c(1,0.5), each=5))
))

tt1 <- ttheme_minimal(
  
  padding = unit(c(2, 2), "mm"),
  base_size = 8.5,
  core=list(fg_params=list(col="steelblue4", hjust=0, x=0))
  
)

tt2 <- ttheme_minimal(
  
  padding = unit(c(2, 2), "mm"),
  base_size = 8.5,
  core=list(fg_params=list(col="steelblue4", hjust=0, x=0))
  
)

# textos
t1 <- textGrob(" ",  gp=gpar(col="steelblue4", fontsize=10, fontface = "bold"),
               just = c("right"))
 t2 <- textGrob("Active\n Transport", gp=gpar(col="steelblue4", fontsize=10, fontface = "bold"), 
                just = c("right"))
# t <- textGrob("[", gp=gpar(fontsize=50))

# tabelas
table1 <- munis_tp_centroids_df %>% arrange(n) %>% .$text


fim <- ggplot() +
  coord_equal(xlim = c(0, 35), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(temp_map1), 
                    xmin = 0, xmax = 25, 
                    ymin = 0, ymax = 20) +
  annotation_custom(ggplotGrob(sp), 
                    xmin = 1.5, xmax = 9, 
                    ymin = 0, ymax = 9) +
  annotation_custom(t1, 
                    xmin = 21, xmax = 24,
                    ymin = 17, ymax = 18)+
  # annotation_custom(t2,
  #                   xmin = 21, xmax = 24,
  #                   ymin = 8, ymax = 9)+
  # annotation_custom(t, 
  #                   xmin = 22, xmax = 23,
  #                   ymin = 0, ymax = 12)+
  annotation_custom(gridExtra::tableGrob(table1,
                                         rows = NULL, cols = NULL, theme = tt1),
                    xmin = 19, xmax = Inf,
                    ymin = 0, ymax = 20.5)+
  # annotation_custom(gridExtra::tableGrob(table2, 
  #                                        rows = NULL, cols = NULL, theme = tt2),
  #                   xmin = 20.3, xmax = Inf,
  #                   ymin = 0, ymax = 12.5)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(length = unit(0.02, "npc")), lineend = "round")+
  annotation_custom(ggplotGrob(rio), 
                    xmin = 15, xmax = 21, 
                    ymin = 0, ymax = 5)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, 
               arrow = arrow(length = unit(0.02, "npc")), lineend = "round")+
  theme(panel.background = element_rect(fill = NA, colour = NA),
        axis.text = element_blank(), axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  labs(x = "", y = "")


fim2 <- 
  ggdraw() +
  draw_plot(fim) +
  draw_plot(world_map, x = -0.05, y = 0.75, width = .25, height = .25) 



# fim2

# save map
ggsave(fim2, 
       file="./figuras/mapa_anexo.png", 
       dpi = 400, width = 17, height = 12, units = "cm")
beepr::beep()
