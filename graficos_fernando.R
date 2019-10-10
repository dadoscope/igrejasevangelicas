# Library

require(tidyverse)
require(tidytext)
require(lubridate)
require(tm)
library(stats)



# Load data
df <- read.csv("data/final/cnae_labelled_data.csv.gz")

evang <- df %>% 
  filter(is_evangelic == 1)

# Data de Abertura x Data de Cadastro

# Foi ajustado para mês // ano 

# PS: O zoo tem alguns conflitos com o tidyverse por isto não chamei na library 

evang_data <- evang %>% 
  select(data_inicio_atividade, data_situacao_cadastral) %>% 
  mutate(data_inicio = ymd(data_inicio_atividade)) %>% 
  mutate(data_sit = ymd(data_situacao_cadastral)) %>% 
  select(data_inicio, data_sit) %>% 
  mutate(data_sit = zoo::as.yearmon(data_sit)) %>% 
  mutate(data_inicio = zoo::as.yearmon(data_inicio))

# Gráfico de surgimento // cadastro das igrejas evangélicas: 

p1 <- evang_data %>%
  filter(data_inicio < zoo::as.yearmon("Jan 2019"),
         data_inicio > zoo::as.yearmon("Dez 1959")) %>%
  group_by(data_inicio) %>% 
  count() %>% 
  ggplot(aes(x = data_inicio, y = n)) +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2004, 2008, 2012, 2016)) + 
  geom_line() +
  labs(title = "Criação de Igrejas Evangélicas por mês/ano", x = "Tempo (mês/ano)", y = "Número de igrejas criadas") +
  theme_bw()


evang_freq_data<- evang_data %>%
  filter(data_inicio < zoo::as.yearmon("Jan 2019"),
         data_inicio > zoo::as.yearmon("Dez 1959")) %>%
  group_by(data_inicio) %>% 
  count()

data_vect<- zoo::as.yearmon(seq.Date(from=as.Date("1959-01-01"),to = as.Date("2019-01-31"), by = "1 month"))

data_ts<- tibble(data_inicio=data_vect)%>%
  left_join(evang_freq_data)

data_ts[is.na(data_ts$n),2] <-0 

ts(data = NA, start = 1, end = numeric(), frequency = 1,
   deltat = 1, ts.eps = getOption("ts.eps"), class = , names = )
as.ts(x, …)
is.ts(x)

ts_serie_trabalho_desp  <- ts(serie_trabalho$Valor[grep("IV. DESPESA TOTAL",serie_trabalho$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)

ts_evang<- ts(data_ts$n, start=c(1960,12), end= c(2019,1), frequency = 12)



ts_stl<- stl(ts_evang, s.window = 7) 

plot(ts_stl[,2])
  

ts_stl$time.series

df_stl<- ts_stl[["time.series"]]

season <- fit$states[,"s1"]

autolayer(df_stl)

autoplot(df_stl[,2])

plot(df_stl[,2], xlab="", ylab="", main="Tendência de criação de igrejas")

p2 <- evang_data %>%
  filter(data_inicio < zoo::as.yearmon("Jan 2019"),
         data_inicio > zoo::as.yearmon("Dec 1959")) %>%
  group_by(data_sit) %>% 
  count() %>% 
  ggplot(aes(x = data_sit, y = n)) +
  scale_x_continuous(breaks = c(1960,1980, 2000, 2004, 2008, 2012, 2016)) + 
  geom_line() +
  labs(title = "Cadastramento de Igrejas Evangélicaspor mês/ano", x = "Tempo (mês/ano)", y = "Número de igrejas criadas") +
  theme_bw()

png("criacao_igrejas_tempo.png",width=3200,height=1800,res=300)
cowplot::plot_grid(p1,nrow = 1)
dev.off()

# Distância entre criação e cadastramento: Note que a partir de 2005-2006, algo assim, 
# as igrejas já surgem e são cadastradas em menos de 2 anos

# Seria bom ajustar o grid no y

evang_data %>% 
  ggplot(aes(x = data_inicio, y = data_sit - data_inicio)) +
  geom_point() +
  ylab("Diferença em meses do cadastro para a criação") +
  xlab("Ano da criação") +
  labs(title = "Diferença ao longo do tempo da data de criação e cadastro") +
  theme_bw()

#

stop <- stopwords(kind = "pt") %>% 
  as.tibble()

df_text <- evang %>% 
  filter(is_evangelic == 1) %>% 
  select(razao_social) %>% 
  mutate(razao_social = as.character(razao_social)) %>% 
  unnest_tokens(word, razao_social) %>% 
  anti_join(stop, by = c("word" = "value")) %>% 
  group_by(word) %>% 
  count(word, sort = TRUE)

# Retornando as 20 designações mais comuns :  (Forçando a barra por que top_n nao ta funcionando)

df_text %>% 
  filter(n > 3300) %>% 
  ungroup() %>% 
  mutate(word = as.factor(word)) %>% 
  ggplot(aes(x = fct_reorder(word,n) , y = n)) +
  geom_col() +
  theme_minimal() +
  coord_flip()

# removendo igreja, deus, evangelica :

df_text %>% 
  filter(!word %in% c("igreja", "deus", "evangelica")) %>% 
  filter(n > 2500) %>% 
  ungroup() %>% 
  mutate(word = as.factor(word)) %>% 
  ggplot(aes(x = fct_reorder(word,n) , y = n)) +
  geom_col() +
  theme_minimal() +
  coord_flip() +
  xlab("Quantidade de citações") +
  ylab("Designação")


# Bonus : O crescimento das assembleias: 


# Inicio e Cadastramento de todas assembleias por mes/ano 

assembleia <- evang %>% 
  mutate(razao_social = as.character(razao_social)) %>% 
  filter(str_detect(razao_social, "ASSEMBLEIA") == TRUE) %>% 
  mutate(data_inicio = ymd(data_inicio_atividade)) %>% 
  mutate(data_sit = ymd(data_situacao_cadastral)) %>% 
  select(data_inicio, data_sit) %>% 
  mutate(data_sit = zoo::as.yearmon(data_sit)) %>% 
  mutate(data_inicio = zoo::as.yearmon(data_inicio))



p3 <- assembleia %>%
  group_by(data_inicio) %>% 
  count() %>% 
  ggplot(aes(x = data_inicio, y = n)) +
  scale_x_continuous(breaks = c(1940,1960,1980, 2000, 2004, 2008, 2012, 2016)) + 
  geom_line() +
  labs(title = "Criação das Assembleias de Deus") +
  theme_bw()


p4 <- assembleia %>%
  group_by(data_sit) %>% 
  count() %>% 
  ggplot(aes(x = data_sit, y = n)) +
  scale_x_continuous(breaks = c(1940,1960,1980, 2000, 2004, 2008, 2012, 2016)) + 
  geom_line() +
  labs(title = "Cadastramento das Assembleias de Deus") +
  theme_bw()

cowplot::plot_grid(p3,p4, nrow = 2)
  
