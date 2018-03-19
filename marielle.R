### PACOTES ###

library(devtools)
library(dplyr)
library(rgdal)
library(sp)
library(spatialEco)
library(maptools)
library(tidyr)
library(data.table)
library(electionsBR)
library(stargazer)
library(broom)
library(ggplot2)
library(ggmap)

### FUNCOES ###

## Baixar dados diretamente do repositorio de dados eleitorais do TSE 
## A funcao getTse foi desenvolvida originalmente por Leonardo Barone
source_gist("https://gist.github.com/lgelape/75a663f8ea5afffdc797d8875315d210", filename =  "getTse.r")

#### 1) Spatial join dos dados dos locais de votacao para identificar suas unidades geograficas

# Abre o shape como um objeto de classe SpatialPolygonsDataFrame
# Nao se esquecer de baixar o shp no link indicado no README.md 
rj_sc <- readOGR("33SEE250GC_SIR.shp", encoding = "latin1", use_iconv = T)

# Se necessario, salve as informacoes dos poligonos como data.frame 
# Boa opcao para ver se os dados foram abertos de forma correta (tentar abrir o mapa com plot pode ser dificil, pois ele e bastante grande)
rj_dados <- rj_sc@data

## Selecionar somente o municipio do Rio de Janeiro
rio_janeiro <- rj_sc[rj_sc@data$NM_MUNICIP == "RIO DE JANEIRO",]

# Checando a projecao
rj_sc@proj4string
rio_janeiro@proj4string

# Estando tudo certo, remover objetos nao utilizados, para liberar memoria
rm(rj_sc)
rm(rj_dados)

## 1.1) Dados das Eleicoes de 2016

## Abrir banco com coordenadas geograficas

rjlv2016 <- read.csv("https://raw.githubusercontent.com/lgelape/texto_mariellefranco/master/Atributos_coordenadas_lv_RJ2016.csv", 
                     dec = ",", sep = ";", header = T, 
                     stringsAsFactors = F,
                     encoding = "latin1")

# Transformar o banco em um shp de pontos
coordinates(rjlv2016) <- ~lon+lat

# Atribuir uma projecao ao shp de pontos
rjlv2016@proj4string <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
identicalCRS(rio_janeiro, rjlv2016) # checar se as projecoes sao as mesmas

# Fazer um spatial join dos dois shp
rj2016_join <- point.in.poly(rjlv2016, rio_janeiro)

# Salvar a tabela de atributos como data.frame
rj2016 <- rj2016_join@data

#### 2) Resultados eleitorais de 2016

estado2016 <- getTse("http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2016_RJ.zip")

# Renomear e selecionar as variaveis
names(estado2016) <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", 
                       "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", 
                       "NOME_MUNICIPIO", "NUM_ZONA", "NUM_SECAO", "CODIGO_CARGO", 
                       "DESCRICAO_CARGO", "NUM_VOTAVEL", "QTDE_VOTOS")

estado2016 <- estado2016 %>%
  select(DATA_GERACAO, ANO_ELEICAO, NUM_TURNO, CODIGO_MUNICIPIO,
         NOME_MUNICIPIO, NUM_ZONA, NUM_SECAO, CODIGO_CARGO,
         DESCRICAO_CARGO, NUM_VOTAVEL, QTDE_VOTOS)

# Selecionar somente as observacoes correspondentes a cidade do RJ
votacao_rj2016 <- filter(estado2016, CODIGO_MUNICIPIO == 60011)
rm(estado2016) # exclui o objeto dos resultados estaduais, pois nao o usaremos mais

# Renomear variaveis, para evitar criacao de colunas desnecessarias, quando juntarmos as tabelas
votacao_rj2016 <- votacao_rj2016 %>%
  rename(CD_MUN_TSE = CODIGO_MUNICIPIO, NM_MUN = NOME_MUNICIPIO)

# Unir as duas bases
rio_janeiro_2016 <- left_join(votacao_rj2016, rj2016, by = c("NUM_ZONA","NUM_SECAO", "CD_MUN_TSE", "NM_MUN"))

## Preparar a base antes das agregacoes:
# Retirando os brancos e nulos
# Separando somente as observacoes correspondentes a candidatos a vereadores, pois a base de resultados eleitorais inclui candidatos a prefeito
# Retirar os votos de legenda
rio_janeiro_2016 <- rio_janeiro_2016[!rio_janeiro_2016$NUM_VOTAVEL %in% 95:97,] %>%
  filter(CODIGO_CARGO == 13) %>%
  filter(NUM_VOTAVEL > 100)

## Agregando base por diferentes niveis de agregacao

# Bairro (IBGE):
base_rj2016_bairro <- rio_janeiro_2016 %>%
  group_by(NM_BAIRRO, CD_GEOCODB, NM_MUN, CD_MUN_TSE, ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, DESCRICAO_CARGO, NUM_VOTAVEL) %>%
  summarise(QTDE_VOTOS = sum(QTDE_VOTOS))

# Subdistrito:
base_rj2016_subdistrito <- rio_janeiro_2016 %>%
  group_by(NM_SUBDIST, CD_GEOCODS, NM_MUN, CD_MUN_TSE, ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, DESCRICAO_CARGO, NUM_VOTAVEL) %>%
  summarise(QTDE_VOTOS = sum(QTDE_VOTOS))

# Bairro (TSE):
base_rj2016_bairrotse <- rio_janeiro_2016 %>%
  group_by(NM_BAIRRO_TSE, NM_MUN, CD_MUN_TSE, ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, DESCRICAO_CARGO, NUM_VOTAVEL) %>%
  summarise(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
  mutate(NUM_VOTAVEL = as.character(NUM_VOTAVEL))

## Separando os resultados somente para a Maré

# Bairro (IBGE)
base_rj2016_bmare <- base_rj2016_bairro %>%
  filter(NM_BAIRRO == "Maré") %>%
  arrange(desc(QTDE_VOTOS)) %>%
  mutate(NUM_VOTAVEL = as.character(NUM_VOTAVEL)) %>%
  mutate(PERC_VOTO = QTDE_VOTOS/sum(QTDE_VOTOS) * 100) %>%
  mutate(PERC_VOTO = round(PERC_VOTO, digits = 2))

# Bairro (TSE)
base_rj2016_btsemare <- base_rj2016_bairrotse %>%
  filter(NM_BAIRRO_TSE == "MARE") %>%
  arrange(desc(QTDE_VOTOS)) %>%
  mutate(NUM_VOTAVEL = as.character(NUM_VOTAVEL)) %>%
  mutate(PERC_VOTO = QTDE_VOTOS/sum(QTDE_VOTOS) * 100) %>%
  mutate(PERC_VOTO = round(PERC_VOTO, digits = 2))

# Subdistrito
base_rj2016_smare <- base_rj2016_subdistrito %>%
  filter(NM_SUBDIST == "MARÉ") %>%
  arrange(desc(QTDE_VOTOS)) %>%
  mutate(NUM_VOTAVEL = as.character(NUM_VOTAVEL)) %>%
  mutate(PERC_VOTO = QTDE_VOTOS/sum(QTDE_VOTOS) * 100) %>%
  mutate(PERC_VOTO = round(PERC_VOTO, digits = 2))

## Conjugando as informacoes dos candidatos para fazer as tabelas
# Baixar informacoes dos candidatos nas eleicoes de 2016
cands2016 <- candidate_local(2016)

# Selecionar somente os candidatos do Rio
rj_cands <- cands2016[cands2016$SIGLA_UE == 60011,]
rm(cands2016)

## Unir as bases de resultados com as de candidatos, selecionando somente colunas de interesse

# Bairros (TSE)
base_tse <- left_join(base_rj2016_btsemare, rj_cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO"))
base_tse <- base_tse[,c(24, 28, 1, 9, 10)] %>%
  slice(1:20)
names(base_tse) <- c("Candidato", "Partido", "Bairro (TSE)", "Votação (total)", "Votação (%)")

# Bairros (IBGE)
base_bairros <- left_join(base_rj2016_bmare, rj_cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO"))
base_bairros <- base_bairros[,c(25, 29, 1, 10, 11)] %>%
  mutate(NM_BAIRRO = as.character(NM_BAIRRO)) %>%
  slice(1:5)
names(base_bairros) <- c("Candidato", "Partido", "Bairro (IBGE)", "Votação (total)", "Votação (%)")

# Subdistritos (IBGE)
base_sub <- left_join(base_rj2016_smare, rj_cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO"))
base_sub <- base_sub[,c(25, 29, 1, 10, 11)] %>%
  mutate(NM_SUBDIST = as.character(NM_SUBDIST)) %>%
  slice(1:5)
names(base_sub) <- c("Candidato", "Partido", "Subdistrito (IBGE)", "Votação (total)", "Votação (%)")

# Gerar as tabelas
stargazer(base_tse, type = "html", summary = F, rownames = T, out = "tabela_tse.doc")
stargazer(base_bairros, type = "html", summary = F, rownames = T, out = "tabela_bairros.doc")
stargazer(base_sub, type = "html", summary = F, rownames = T, out = "tabela_sub.doc")

## Votacao individual de Marielle por bairro

# IBGE
voto_marielle_ibge <- base_rj2016_bairro %>%
  filter(NUM_VOTAVEL == "50777") %>%
  mutate(QTDE_VOTOS = as.integer(QTDE_VOTOS)) %>%
  mutate(BAIRRO = as.character(NM_BAIRRO))
voto_marielle_ibge$PERC_VOTO <- ((voto_marielle_ibge$QTDE_VOTOS/sum(voto_marielle_ibge$QTDE_VOTOS)) * 100)

voto_marielle_ibge <- voto_marielle_ibge[, c(11, 9, 10, 12)] %>%
  arrange(desc(QTDE_VOTOS)) %>%
  mutate(PERC_VOTO = round(PERC_VOTO, digits = 2)) %>%
  slice(1:20)
names(voto_marielle_ibge) <- c("Bairro (IBGE)", "N. da candidata", "Votação (total)", "Votação (%)")

# TSE
voto_marielle_tse <- base_rj2016_bairrotse %>%
  filter(NUM_VOTAVEL == "50777") %>%
  mutate(QTDE_VOTOS = as.integer(QTDE_VOTOS))
voto_marielle_tse$PERC_VOTO <- (voto_marielle_tse$QTDE_VOTOS/sum(voto_marielle_tse$QTDE_VOTOS)) * 100

voto_marielle_tse <- voto_marielle_tse[, c(1, 8, 9, 10)] %>%
  arrange(desc(QTDE_VOTOS)) %>%
  slice(1:20) %>%
  mutate(PERC_VOTO = round(PERC_VOTO, digits = 2))
names(voto_marielle_tse) <- c("Bairro (TSE)", "N. da candidata", "Votação (total)", "Votação (%)")

# Juntar as duas tabelas
voto_marielle <- cbind(voto_marielle_ibge, voto_marielle_tse)
voto_marielle <- voto_marielle[,c(1, 3, 4, 5, 7, 8)]

# Exportar tabela
stargazer(voto_marielle, type = "html", summary = F, rownames = T, out = "voto_marielle.doc")

## Comparacao IBGE e TSE
base_comparacao <- rio_janeiro_2016 %>%
  group_by(NM_BAIRRO, NM_BAIRRO_TSE, NM_MUN, CD_MUN_TSE, ANO_ELEICAO, NUM_TURNO, CODIGO_CARGO, DESCRICAO_CARGO, NUM_VOTAVEL) %>%
  summarise(QTDE_VOTOS = sum(QTDE_VOTOS)) %>%
  filter(NUM_VOTAVEL == 50777) %>%
  filter(NM_BAIRRO == "Maré")

## Mapas de votacao

# Aqui, estou juntando as informacoes de todos os candidatos com os resultados por bairro
psol <- base_rj2016_bairro %>%
  ungroup() %>%
  mutate(NUM_VOTAVEL = as.character(NUM_VOTAVEL)) %>%
  mutate(ANO_ELEICAO = as.character(ANO_ELEICAO)) %>%
  left_join(rj_cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO", "ANO_ELEICAO")) 

# Em seguida, crio um banco para transformar todos os NA onde os candidatos nao foram votados em 0
rj2016_bairro_na <- expand(psol, `CD_GEOCODB`, `NUM_VOTAVEL`)
psol <- psol %>%
  left_join(rj2016_bairro_na, by = c("CD_GEOCODB", "NUM_VOTAVEL")) %>%
  mutate(QTDE_VOTOS = ifelse(is.na(QTDE_VOTOS), 0, QTDE_VOTOS))

# Seleciono somente as variaveis de interesse e filtro pelos candidatos eleitos do PSOL
# Crio ainda uma variavel com o percentual de votos que cada bairro corresponde a sua votacao
psol <- psol[, c(1, 2, 3, 9, 23, 10, 27)] %>%
  filter(SIGLA_PARTIDO == "PSOL") %>%
  filter(NUM_VOTAVEL %in% c("50123", "50777", "50555", "50111", "50000", "50500")) %>%
  mutate(CD_GEOCODB = as.character(CD_GEOCODB)) %>%
  group_by(NUM_VOTAVEL) %>%
  mutate(PERC_VOTO = QTDE_VOTOS/sum(QTDE_VOTOS, na.rm = T) * 100)

# Abrir o shape com os limites de bairros
rj_bairros <- readOGR("rj_bairros.shp", 
                 encoding = "latin1", use_iconv = T)

# Transforma a variavel CD_GEOCODB do shp em character, para facilitar uniao
rj_bairros@data <- rj_bairros@data %>%
  mutate(CD_GEOCODB = as.character(CD_GEOCODB))

# Transformar o shape em um arquivo manipulavel para producao dos mapas (tidy)
rj_bairros_df <- tidy(rj_bairros)
rj_bairros@data$id <- 0:(nrow(rj_bairros@data)-1) # Para adicionar as variaveis que estao
rj_bairros_df$id <- as.numeric(rj_bairros_df$id)  # na tabela de atributos ao banco
rj_bairros_df <- left_join(rj_bairros_df, rj_bairros@data, by = "id")

# Aqui, juntamos os dados para os candidatos do PSOL as informacoes do mapa
# O vetor "nomes" e uma pequena gambiarra pra sumir com a categoria NA do facet_wrap
nomes <- c("DAVID MIRANDA", "LEONEL BRIZOLA NETO", "MARIELLE FRANCO", "PAULO PINHEIRO", "RENATO CINCO", "TARCÍSIO MOTTA")
rj_bairros_df <- left_join(rj_bairros_df, psol, by = c("CD_GEOCODB")) %>%
  mutate(QTDE_VOTOS = ifelse(is.na(QTDE_VOTOS), 0, QTDE_VOTOS)) %>%
  mutate(NOME_URNA_CANDIDATO = ifelse(is.na(NOME_URNA_CANDIDATO), rep(vetor), NOME_URNA_CANDIDATO))

# Produzir o mapa
ggplot() + 
  geom_polygon(data = rj_bairros_df, 
               aes(x = long, y = lat, group = group, fill = PERC_VOTO)) +
  coord_map() + 
  facet_wrap(~ NOME_URNA_CANDIDATO, ncol = 2, drop = T) +
  scale_fill_continuous(name = "% votos", low = "lightgoldenrod", high = "goldenrod4",
                        na.value = "grey90") +
  theme_nothing(legend = T) +
  theme(legend.justification = "center") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  labs(title = "Distribuição dos votos dos candidatos eleitos do PSOL (bairros)",
       caption = "Fonte: elaborado pelo autor")

# Exportar a figura
ggsave("Voto_PSOL_300dpi.png", dpi = 300)
