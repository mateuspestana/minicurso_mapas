# Exercícios

# Porcentagem de votos que determinado candidato teve nas eleições presidenciais de 2018
eleicoes <- import("votacao_candidato_munzona_2018_BR.csv",
       encoding = "Latin-1")

municipios <- import("municipios_brasileiros_tse.csv") %>%
  rename("NM_MUNICIPIO" = nome_municipio,
         "CD_MUNICIPIO" = codigo_tse)

haddad <- import("votacao_candidato_munzona_2018_BR.csv",
       encoding = "Latin-1") %>%
  filter(NR_TURNO == 2) %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NM_URNA_CANDIDATO) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS)) %>%
  filter(SG_UF != "ZZ") %>%
  pivot_wider(names_from = NM_URNA_CANDIDATO,
              values_from = votos) %>%
  clean_names() %>%
  mutate(total_votos = fernando_haddad + jair_bolsonaro,
         pct_haddad = fernando_haddad / total_votos,
         estado_sigla = sg_uf) %>%
  filter(sg_uf == "CE") %>%
  left_join(municipios, by = c("cd_municipio" = "CD_MUNICIPIO"))

mapa_haddad_CE <- left_join(brmap_municipio_simples %>%
                                 filter(municipio_cod %in% haddad$codigo_ibge),
                               haddad, by  = c("municipio_cod" = "codigo_ibge"))

mapa_haddad_CE %>%
  ggplot(aes(fill = pct_haddad))+
  geom_sf(color = "white", size = 0.1)+
  geom_sf_text(aes(label = municipio_nome, x = lon, y = lat), size = 2)+
  scale_fill_fermenter()+
  labs(title = "Votação de Fernando Haddad no 2º Turno",
       subtitle = "No Ceará",
       caption = "Matheus C. Pestana",
       fill = "% de voto")+
  theme_minimal()+
  theme(legend.position = "bottom") -> mapa_ceara

mapa_ceara

ggplotly(mapa_ceara)

# Porcentagem de população não-branca em determinado estado ou região do país

pop_n_branca <- import("Populacao_Nao_Branca.xlsx")

pop_n_branca_sul <- pop_n_branca %>%
  filter(uf %in% c("SC", "RS", "PR")) %>%
  select(mun, pct_n_branca, nome_municipio) %>%
  rename("municipio_cod" = mun) %>%
  mutate(municipio_cod = as.integer(municipio_cod))

brmap_regiao_simples
brmap_estado_simples %>% filter(regiao_cod == 4)

sul <- left_join(brmap_municipio_simples %>%
                   filter(estado_cod %in% c(41,42,43)),
                 pop_n_branca_sul)

sul %>%
  ggplot(aes(fill = pct_n_branca, group = nome_municipio))+
  geom_sf(color = "white",
          size = 0.05)+
  viridis::scale_fill_viridis() -> mapa_sul_nbranca

ggplotly(mapa_sul_nbranca)

# Nº de habitantes em cada município/estado

brmap_estado_simples

pop_n_branca <- pop_n_branca %>%
  mutate(mun = as.integer(mun))


brmap_municipio_simples %>%
  filter(estado_cod == 35) %>%
  left_join(pop_n_branca, by = c("municipio_cod" = "mun")) %>%
  ggplot()+
  geom_sf(aes(fill = log(total)),
          color = "white", size = 0.05)+
  scale_fill_fermenter()

# Terras indígenas separadas por status (declaradas, encaminhadas, regularizada, delimitada, etc)

terra_indigena <-  read_indigenous_land(showProgress = F) %>%
  filter(between(code_state, 10, 30))

ggplot() +
  geom_sf(data = world,
          fill = "lavender")+
  geom_sf(data = brmap_brasil_simples, fill = "ivory")+
  geom_sf(data = terra_indigena,
          aes(fill = fase_ti),
          color = "white",
          size = 0.1)+
  scale_fill_brewer(palette = "Paired")+
  coord_sf(xlim = c(-75, -35),
           ylim = c(-18, 5))+
  theme_minimal()+
  theme(legend.position = "bottom")

# Expectativa de vida, população ou PIB per capita (banco world)

world %>%
ggplot()+
  geom_sf(aes(fill = lifeExp))+
  viridis::scale_fill_viridis()+
  theme_minimal()
