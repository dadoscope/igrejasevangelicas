# Library

require(tidyverse)
require(tidytext)
require(lubridate)
require(tm)

# Load data


df <- read.csv("data/final/cnae_labelled_data.csv.gz")

evang <- df %>% 
  filter(is_evangelic == 1)


# Descobrindo o topo


evang %>% 
  count(cnae_fiscal) %>% 
  arrange(-n)

evang %>% 
  filter(cnae_fiscal == 9491000) %>% 
  select(data_situacao_cadastral) %>% 
  count(data_situacao_cadastral, sort = TRUE)

evang %>% 
  filter(cnae_fiscal == 9491000) %>% 
  filter(data_situacao_cadastral == "2005-11-03") %>% 
  select(razao_social, uf)
