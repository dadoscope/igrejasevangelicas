# Library

require(tidyverse)
require(tidytext)
require(lubridate)
require(tm)

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
  group_by(data_inicio) %>% 
  count() %>% 
  ggplot(aes(x = data_inicio, y = n)) +
  scale_x_continuous(breaks = c(1940,1960,1980, 2000, 2004, 2008, 2012, 2016)) + 
  geom_line() +
  labs(title = "Início de Igrejas Evangélicas") +
  theme_bw()


p2 <- evang_data %>%
  group_by(data_sit) %>% 
  count() %>% 
  ggplot(aes(x = data_sit, y = n)) +
  scale_x_continuous(breaks = c(1940,1960,1980, 2000, 2004, 2008, 2012, 2016)) + 
  geom_line() +
  labs(title = "Cadastramento de Igrejas Evangélicas") +
  theme_bw()

cowplot::plot_grid(p1,p2, nrow = 2)

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
  
