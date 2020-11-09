### Criando mapas no R
### Autor: Matheus Cavalcanti Pestana (IESP-UERJ)
### Contato: matheus.pestana@iesp.uerj.br
### Data: 11/11

# Primeiro, é preciso carregar os pacotes necessários para a criação de mapas
# Eu, particularmente, gosto de utilizar o pacote "pacman" para carregar/baixar
# vários pacotes de uma vez. Todavia, para quem não tem muita prática, pode se ater
# à forma original (usando o library() )
# É preciso instalar antes o RTOOLS (Windows) e: :
install.packages("devtools")
install.packages("pacman")
devtools::install_github("italocegatta/brmap")
devtools::install_github("tylermorganwall/rayshader")

# OBS: Se o brmap der erro, não podendo ser instalado, rode:
load("brmap.RData")


pacman::p_load(
  sf,
  raster,
  leaflet,
  geobr,
  brmap,
  tidyverse,
  rio,
  plotly,
  osmdata,
  leaflet.extras,
  rayshader,
  gifski,
  spData,
  janitor,
  hrbrthemes
)

# O Rio no mundo
world %>%
  ggplot() +
  geom_sf() +
  geom_point(aes(x = -43.12, y = -22.54),
             size = 3,
             color = "red") +
  geom_point(aes(x = 0,
                 y = 0),
             size = 1,
             color = "black") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text(aes(x = -20,
                y = -25,
                label = "Rio de Janeiro"),
            size = 2.5) +
  labs(title = "Rio de Janeiro - 43º12'W  22º54'S",
       caption = "Matheus C. Pestana") +
  hrbrthemes::theme_ipsum_tw()

# Raster
raster <-  raster("Raster/MDE_26844no_v2.tif")
plot(raster)

# Mapa dos municípios do RJ
brmap::brmap_estado

ggplot(brmap::brmap_municipio_simples %>%
         filter(estado_cod == 33)) +
  geom_sf() +
  hrbrthemes::theme_ipsum_tw()

# Raster
raster

# Estrutura vetor SF
brmap_estado_simples %>%
  filter(estado_cod == 33) %>%
  select(geometry) %>%
  unlist() %>%
  head(15)

# Outros tipos - brmap
brmap_brasil_simples %>%
  ggplot()+
  geom_sf()

brmap_estado_simples %>%
  ggplot()+
  geom_sf()

brmap_municipio_simples %>%
  select(-municipio_nome) %>%
  filter(estado_cod == 33) %>%
  ggplot()+
  geom_sf()

# Geobr
list_geobr()

# Baixando dados de terras indígenas
terra_indigena <-  read_indigenous_land(showProgress = F)
terra_indigena %>%
  ggplot() +
  geom_sf()

# Região Sudeste - Municípios
brmap_municipio_simples %>%
  filter(estado_cod %in% c(31, 32, 33, 35)) %>%
  ggplot(aes(fill = estado_cod))+
  geom_sf(color = "white", size = 0.05)

# Idem, mas com o estado como fator
brmap_municipio_simples %>%
filter(estado_cod %in% c(31, 32, 33, 35)) %>%
  ggplot(aes(fill = as.factor(estado_cod)))+
  geom_sf(color = "white", size = 0.1)

# Melhorando as cores com outros recursos do ggplot
brmap_municipio_simples %>%
  filter(estado_cod %in% c(31, 32, 33, 35)) %>%
  ggplot(aes(fill = as.factor(estado_cod)))+
  geom_sf(color = "white", size = 0.05) +
  hrbrthemes::theme_ipsum_tw() +
  scale_fill_manual(values = c("31" = "deeppink", "32" = "forestgreen", "33" = "dodgerblue4", "35" = "darkorange"))+
  labs(title = "Região Sudeste",
       subtitle = "Divisão dos municípios",
       caption = "Matheus c. Pestana",
       fill = "Código do Estado")

# Outras variáveis - criando
brmap_municipio_simples %>%
  filter(estado_cod == 33) %>%
  mutate(visita = case_when(municipio_nome == "Rio de Janeiro" ~ "Onde moro",
                            municipio_nome %in% c("Angra dos Reis",
                                                  "Belford Roxo",
                                                  "Cabo Frio",
                                                  "Cachoeiras de Macacu",
                                                  "Guapimirim",
                                                  "Itaboraí",
                                                  "Duque de Caxias",
                                                  "Magé",
                                                  "Maricá",
                                                  "Nilópolis",
                                                  "Niterói",
                                                  "Nova Friburgo",
                                                  "Petrópolis",
                                                  "Paraty",
                                                  "Teresópolis",
                                                  "São Gonçalo") ~ "Já visitei",
                            TRUE ~ "Nunca visitei")) %>%
  ggplot(aes(fill = visita))+
  geom_sf(color = "white", size = 0.1)+
  hrbrthemes::theme_ipsum_tw()+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "Dark2")+
  labs(fill = "Legenda")

# Dados Eleitorais
segundo_turno_br <- import("votacao_candidato_munzona_2018_BR.csv",
                           encoding = "Latin-1") %>%
  filter(NR_TURNO == 2) %>%
  group_by(SG_UF, NM_URNA_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS)) %>%
  filter(SG_UF != "ZZ") %>%
  pivot_wider(names_from = NM_URNA_CANDIDATO,
              values_from = votos) %>%
  clean_names() %>%
  mutate(total_votos = fernando_haddad + jair_bolsonaro,
         pct_haddad = fernando_haddad / total_votos,
         pct_bolsonaro = jair_bolsonaro / total_votos,
         jair_ganhou = ifelse(pct_bolsonaro >= 0.5, "Jair Bolsonaro", "Fernando Haddad"),
         estado_sigla = sg_uf) %>%
  select(-sg_uf)

mapa_2t <- left_join(brmap_estado_simples, segundo_turno_br)

mapa_2t %>%
  ggplot(aes(fill = jair_ganhou))+
  geom_sf(color = "white", size = 0.1)+
  scale_fill_manual(values = c("Jair Bolsonaro" = "darkorange", "Fernando Haddad" = "firebrick"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "2º Turno - 2018",
       subtitle = "Jair Bolsonaro x Fernando Haddad",
       fill = "")

# Governadores - Partidos
governadores <- import("governadores.csv")

governadores <- governadores %>%
  rename("estado_sigla" = SIGLA_UF)

mapa_governadores <- left_join(brmap_estado_simples, governadores)

mapa_governadores %>%
  ggplot(aes(fill = SIGLA_PARTIDO, label = SIGLA_PARTIDO))+
  geom_sf(show.legend = F)+
  geom_sf_label(show.legend = F,
                size = 2,
                fill = "white",
                alpha = 0.5)+
  hrbrthemes::theme_ipsum_tw()

# Dados do Censo
pop_n_branca <- import("Populacao_Nao_Branca.xlsx")

pop_n_branca_rio <- pop_n_branca %>%
  filter(V0001 == 33) %>%
  select(mun, pct_n_branca, nome_municipio) %>%
  rename("municipio_cod" = mun) %>%
  mutate(municipio_cod = as.integer(municipio_cod))

rio <- left_join(brmap_municipio_simples %>%
                    filter(estado_cod == 33), pop_n_branca_rio)

rio %>%
  ggplot(aes(fill = pct_n_branca))+
  geom_sf(color = "white",
          size = 0.1)+
  geom_sf_text(aes(label = municipio_nome),
               size = 1.9)+
  viridis::scale_fill_viridis()

# Usando o plotly
rio %>%
  ggplot(aes(fill = pct_n_branca))+
  geom_sf(color = "white", size = 0.1)+
  geom_sf_text(aes(label = municipio_nome), size = 2)+
  viridis::scale_fill_viridis()+
  theme_minimal() -> mapa_rio_nbranco

ggplotly(mapa_rio_nbranco)

# Leaflet
rio <- rio %>%
  mutate(pct = paste(format(pct_n_branca*100, digits = 3), "%"))

pal <- colorBin("YlOrRd",
                domain = rio$pct_n_branca,
                bins = seq(0, 1, 0.1),
                alpha = T)

labels <- sprintf("<strong>%s</strong><br/> %s de não-brancos",
                  rio$nome_municipio, rio$pct) %>%
  lapply(htmltools::HTML)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = rio,
              label = labels,
              fillColor = ~pal(pct_n_branca),
              fillOpacity = 1,
              color = "black",
              weight = 1,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto"))

# OSMData
moscou_metro <- opq("Moscow",  timeout = 240, memsize = 1073741824) %>%
  add_osm_feature(key = "railway", value = "subway") %>%
  osmdata_sf()

moscou_metrostat <- opq("Moscow",  timeout = 240, memsize = 1073741824) %>%
  add_osm_feature(key = "station", value = "subway") %>%
  osmdata_sf()

moscou_city <- opq("Moscow", timeout = 240, memsize = 1073741824) %>%
  add_osm_feature(key = "name:pt", value = "Moscou") %>%
  osmdata_sf()

moscou_trem <- opq("Moscow", timeout = 240, memsize = 1073741824) %>%
  add_osm_feature(key = "railway", value = "rail") %>%
  osmdata_sf()

gps_coords <- moscou_city$bbox %>% str_split(",") %>% # Pegar as localizações
  unlist() %>% # tirar de lista
  as.numeric() # transformar em número

lng_coord <- (gps_coords[2] + gps_coords[4])/2 # Média das longitudes
lat_coord <- (gps_coords[1] + gps_coords[3])/2 # Média das latitudes

# Para linhas de trem e metro, pegamos as variáveis osm_lines
moscou_metro <- moscou_metro$osm_lines %>%
  select(name) %>%
  mutate(name = fct_drop(name),
         name = fct_explicit_na(name)) %>%
  group_by(name) %>%
  summarise()

moscou_trem <- moscou_trem$osm_lines %>%
  select(name) %>%
  mutate(name = fct_drop(name),
         name = fct_explicit_na(name)) %>%
  group_by(name) %>%
  summarise()

# Para estações, só precisamos dos pontos
moscou_metrostat <- moscou_metrostat$osm_points %>%
  select(name) %>%
  mutate(name = fct_drop(name),
         name = fct_explicit_na(name))

# Para a divisão adiministrativa, são os multipolígonos
moscou_city <- moscou_city$osm_multipolygons %>%
  select(name) %>%
  mutate(name = fct_drop(name),
         name = fct_explicit_na(name)) %>%
  group_by(name) %>%
  summarise()

ggplot()+
  geom_sf(data = moscou_city, fill = "ivory")+
  geom_sf(data = moscou_trem, color = "blue")+
  geom_sf(data = moscou_metro, color =  "forestgreen", size = 1.2)+
  geom_sf(data = moscou_metrostat, color = "black", size = 0.9)+
  theme_minimal()

leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$Stamen.TonerBackground) %>%
  addPolygons(data = moscou_city,
              opacity = 0.5, color = "black",
              fillOpacity = 0.08, weight = 1) %>%
  addPolylines(data = moscou_metro, # banco
               label = ~name, # nome das linhas
               color = "lightseagreen", # cor da linha
               opacity = 2, weight = 4, # opacidade e grossura da linha
               group = "Linhas de Metrô",  # nome do grupo
               highlight = highlightOptions(color = "red")) %>% # cor de highlight
  addPolylines(data = moscou_trem,
               label = ~name,
               color = "coral",
               opacity =  2, weight  = 3,
               group = "Linhas de Trem",
               highlight = highlightOptions(color = "red")) %>%
  addCircleMarkers(data = moscou_metrostat,
                   label = ~name,
                   radius = 0.2,
                   opacity = 1,
                   color = "midnightblue",
                   group = "Estações de Metrô") %>%
  addProviderTiles(providers$Esri.WorldStreetMap, group = "ESRI WorldStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI WorldImagery") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
  addProviderTiles(providers$MtbMap, group = "MtbMap") %>%
  addLayersControl(baseGroups = c("TonerBackground (Default)",
                                  "ESRI WorldStreetMap",
                                  "ESRI WorldImagery",
                                  "CartoDB Positron",
                                  "MtbMap"),
                   overlayGroups = c("Linhas de Metrô", "Estações de Metrô", "Linhas de Trem"),
                   options = layersControlOptions(collapsed = F),
                   position = "bottomright") %>%
  addSearchOSM() %>%
  addControlGPS() %>%
  setView(zoom = 12,
          lat = lat_coord+0.02,
          lng = lng_coord)

# Mapas 3D
source("~/Documents/Datasets/Minicurso_Mapas/functions_rayshader_gif.R")
n_frames <- 180
waterdepths <- transition_values(from = 100, to = min(montereybay)-500, steps = n_frames)
thetas <- transition_values(from = -45, to = -135, steps = n_frames)
zscale <- 50
montereybay %>%
  sphere_shade(texture = "imhof1", zscale = zscale) %>%
  add_shadow(ambient_shade(montereybay, zscale = zscale), 0.5) %>%
  add_shadow(ray_shade(montereybay, zscale = zscale, lambert = TRUE), 0.5) %>%
  save_3d_gif(montereybay, file = "montereybay.gif", duration = 6,
              solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
              watercolor = "imhof3", wateralpha = 0.8,
              waterlinecolor = "#ffffff", waterlinealpha = 0.5,
              waterdepth = waterdepths/zscale,
              theta = thetas, phi = 45)

