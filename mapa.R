getTidyGDP <- function(tweets){
  tweets_tidy <- tweets %>%
    mutate(year = floor_date(ymd(data_inicio_atividade), unit = "year"))%>%
    mutate(timeline = municipio) %>%
    group_by(year, timeline) %>%
    summarise(value = n()) %>% 
    ungroup()%>%
    complete(year,timeline=unique(timeline), fill = list(value = 0)) %>% 
    ungroup()%>%group_by(timeline)%>%mutate(cumvalue = cumsum(value)) %>% select(year, timeline, cumvalue)
  #para o minuto seguinte, acumular a frase com a frase anterior. Assim vamos computar o acumulado de citacoes de cada hashtag
  names(tweets_tidy) <- c("year","country_name", "value")
  return(tweets_tidy)
}

# Library

require(tidyverse)
require(tidytext)
require(lubridate)
require(tm)
library(brazilmaps)

# Load data and filter Evangelic Churches

df <- read.csv("data/final/cnae_labelled_data.csv.gz")
evang <- df %>% 
  filter(is_evangelic == 1)

tidy_evang <- getTidyGDP(evang)

mun <- brazilmaps::get_brmap("City")
mapa1 <- plot_brmap(mun,
                    data_to_join = tidy_evang %>% filter(year == ymd("2018-01-01")),
                    join_by = c("nome" = "country_name"),
                    var = "value")
mapa1 +
  scale_fill_viridis_c(option = 2) +
  labs(title = "Número de Igrejas Evangélicas em 2018")