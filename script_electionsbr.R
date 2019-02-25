# Instala os pacotes necessários (utilizando a versão em desenvolvimento do electionsBR)
#install.packages("pacman")
# if (!require("devtools")) install.packages("devtools")
#devtools::install_github("silvadenisson/electionsBR")
pacman::p_load(gdata,dplyr,electionsBR,data.table)

#Recorte das urnas
riodaspedras <-  read.xls("http://www.tre-rj.jus.br/site/gecoi_arquivos/190920181330_arq_140088.xls") %>% filter(Bairro == 'RIO DAS PEDRAS') 
zona <- 179
DESC_UE <- "RIO DE JANEIRO"
uf <- "RJ"

#Recorte dos anos e total de candidatos por ano
deputados <- c(1998,2002,2006,2010)
vereadores <- c(2000,2004,2008)
rank <- 50 

# Cria dataframes
ranking <- data.frame() 
rfederal <- data.frame() 
restadual <- data.frame() 
rvereadores <- data.frame() 

### Organiza os dados dos deputados
for (ano in deputados){
  df <- vote_section_fed(ano, uf = uf) 
  cand <- candidate_fed(ano, uf = uf) 

## Federal
  df_fed <- df %>% filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL", NUMERO_ZONA == zona, NUMERO_SECAO %in% riodaspedras$Secao) %>%
    group_by(NUM_VOTAVEL) %>% summarise(total = sum(QTDE_VOTOS))  %>% 
    mutate(freq = total/sum(total)) %>% mutate(ano = ano)
  
  cand_fed <- cand %>% filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL", DESCRICAO_UE == DESC_UE) %>% 
    select(NUMERO_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO, DESCRICAO_CARGO)

  top_fed <- merge(x = df_fed, y = cand_fed, by.x = "NUM_VOTAVEL", by.y = "NUMERO_CANDIDATO", all.x = TRUE) %>% arrange(-total) %>% head(rank)

  rfederal <- rbind(rfederal,top_fed)

## Estadual
  df_est <- df %>% filter(DESCRICAO_CARGO == "DEPUTADO ESTADUAL", NUMERO_ZONA == zona, NUMERO_SECAO %in% riodaspedras$Secao) %>%
    group_by(NUM_VOTAVEL) %>% summarise(total = sum(QTDE_VOTOS))  %>% 
    mutate(freq = total/sum(total)) %>% mutate(ano = ano)
  
  cand_est <- cand %>% filter(DESCRICAO_CARGO == "DEPUTADO ESTADUAL", DESCRICAO_UE == DESC_UE) %>% 
    select(NUMERO_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO, DESCRICAO_CARGO)

  top_est <- merge(x = df_est, y = cand_est, by.x = "NUM_VOTAVEL", by.y = "NUMERO_CANDIDATO", all.x = TRUE) %>% arrange(-total) %>% head(rank)

  restadual <- rbind(restadual,top_est)
}

ranking <- rbind(restadual,rfederal)

### Organiza dados de veradores
for (ano in vereadores){
  df <- vote_section_local(ano, uf = uf) 
  cand <- candidate_local(ano, uf = uf) 
  
  df <- df %>% filter(DESCRICAO_CARGO == "VEREADOR", NUMERO_ZONA == zona, NUMERO_SECAO %in% riodaspedras$Secao) %>%
    group_by(NUM_VOTAVEL) %>% summarise(total = sum(QTDE_VOTOS))  %>% 
    mutate(freq = total/sum(total)) %>% mutate(ano = ano)
  
  cand <- cand %>% filter(DESCRICAO_CARGO == "VEREADOR", DESCRICAO_UE == DESC_UE) %>% 
    select(NUMERO_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO, DESCRICAO_CARGO)
  
  top <- merge(x = df, y = cand, by.x = "NUM_VOTAVEL", by.y = "NUMERO_CANDIDATO", all.x = TRUE) %>% arrange(-total) %>% head(rank)
  
  ranking <- rbind(ranking,top)
}

### Lida com os anos fora do padrão

## 2012 
# http://agencia.tse.jus.br/estatistica/sead/eleicoes/eleicoes2012/votosecao/vsec_1t_RJ.zip
cand12 <- candidate_local(2012, uf = "RJ") %>% filter(DESCRICAO_CARGO == "VEREADOR", DESCRICAO_UE == "RIO DE JANEIRO") %>%
  select(NUMERO_CANDIDATO, NOME_CANDIDATO, SIGLA_PARTIDO, DESCRICAO_CARGO)

df12 <- fread(file = "~/2012/votacao_secao_2012_RJ.txt", sep = ";", encoding = "Latin-1", header = FALSE)
colnames(df12) <- c("data","hora","ano","turno","desc_eleicao","uf","sigla_ue","sigla_uf","municipio","zona",
                    "secao","cod_cargo","cargo","NUM_VOTAVEL","QTDE_VOTOS")

df12 <- df12 %>% filter(cargo == "VEREADOR", zona == 179, secao %in% riodaspedras$Secao) %>% group_by(NUM_VOTAVEL) %>% 
  summarise(total = sum(QTDE_VOTOS)) %>% mutate(freq = total/sum(total)) %>% mutate(ano = 2012)

top12 <- merge(x = df12, y = cand12, by.x = "NUM_VOTAVEL", by.y = "NUMERO_CANDIDATO", all.x = TRUE) %>% arrange(desc(total)) %>% head(rank)

colnames(top12) <- c("NUM_VOTAVEL","total","freq","ano","NOME_CANDIDATO","SIGLA_PARTIDO","DESCRICAO_CARGO")

ranking <- rbind(ranking,top12)

## 2014
cand14 <- candidate_fed(2014, uf = "RJ")
df14 <- vote_section_fed(2014, uf = "RJ")
cargos <- c("DEPUTADO ESTADUAL", "DEPUTADO FEDERAL","SENADOR")

for(cargo in cargos){
cand <- cand14 %>% filter(DS_CARGO == cargo, NM_UE == DESC_UE) %>% 
  select(NR_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_CARGO)

df <- df14 %>% filter(DESCRICAO_CARGO == cargo, NUMERO_ZONA == zona, NUMERO_SECAO %in% riodaspedras$Secao) %>% 
    group_by(NUM_VOTAVEL) %>%    summarise(total = sum(QTDE_VOTOS)) %>% mutate(freq = total/sum(total)) %>% mutate(ano = 2014)
  
top <- merge(x = df, y = cand, by.x = "NUM_VOTAVEL", by.y = "NR_CANDIDATO", all.x = TRUE) %>% arrange(desc(total)) %>% head(rank)

colnames(top) <- c("NUM_VOTAVEL","total","freq","ano","NOME_CANDIDATO","SIGLA_PARTIDO","DESCRICAO_CARGO")
                    
ranking <- rbind(ranking,top)
}

## 2016
cand <- candidate_local(2016, uf = "RJ") %>% filter(DS_CARGO == "VEREADOR", NM_UE == "RIO DE JANEIRO") %>% 
  select(NR_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_CARGO)

df <- vote_section_local(2016, uf = "RJ") %>% filter(DESCRICAO_CARGO == "VEREADOR", NUMERO_ZONA == zona, NUMERO_SECAO %in% riodaspedras$Secao) %>% 
  group_by(NUM_VOTAVEL) %>%  summarise(total = sum(QTDE_VOTOS)) %>% mutate(freq = total/sum(total)) %>% mutate(ano = 2016)

top <- merge(x = df, y = cand, by.x = "NUM_VOTAVEL", by.y = "NR_CANDIDATO", all.x = TRUE) %>% arrange(desc(total)) %>% head(rank)

colnames(top) <- c("NUM_VOTAVEL","total","freq","ano","NOME_CANDIDATO","SIGLA_PARTIDO","DESCRICAO_CARGO")

ranking <- rbind(ranking,top)

## 2018
# http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2018.zip
# 
ranking18 <- data.frame() 
cand18 <- candidate_fed(2018, uf = "RJ")  %>% select(NR_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, DS_CARGO)   

df18 <- fread(file = "~/votacao/2018/bweb_1t_RJ_101020182014.csv", sep = ";", encoding = "Latin-1")

cargos <- c("Deputado Estadual","Deputado Federal","Senador")
for(cargo in cargos){
df <- df18  %>% filter(DS_CARGO_PERGUNTA == cargo, NR_ZONA == zona, NR_SECAO %in% riodaspedras$Secao)  %>%
    group_by(NM_VOTAVEL) %>%  summarise(total = sum(QT_VOTOS)) %>% mutate(freq = total/sum(total)) %>% mutate(ano = 2018)
  
  top18 <- merge(x = df, y = cand18, by.x = "NM_VOTAVEL", by.y = "NM_URNA_CANDIDATO", all.x = TRUE) %>% arrange(desc(total)) %>% head(rank)
  
ranking18 <- rbind(ranking18,top18)
}


# Exporta  o resultado
ranking$freq <- paste(round(ranking$freq*100,digits=1),"%",sep="")
ranking18$freq <- paste(round(ranking18$freq*100,digits=1),"%",sep="")
write.csv(ranking, "ranking-rp-final2.csv")
write.csv(ranking18, "ranking-rp-18-final2.csv")
