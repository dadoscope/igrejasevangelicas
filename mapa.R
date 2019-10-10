animateTidyWords <- function(tweets_tidy, output = "gif", filename = "animation", mysubtitle = "", mycaption = ""){
  formatted_tweets <- tweets_tidy %>%
    group_by(year) %>%
    mutate(rank = rank(-value) * 1,
           Value_rel = value/value[rank==1],
           Value_lbl = paste0(" ",value)) %>%
    group_by(country_name) %>%
    filter(rank <=10) %>%
    ungroup()
  maxvalue <- max(as.numeric(formatted_tweets$value))
  cat("Saving RDS file",sep="\n")
  saveRDS(formatted_tweets, file = "df_formatted.rds") 
  formatted_tweets <- formatted_tweets %>% filter(rank <= 10)
  formatted_tweets <- formatted_tweets %>% 
    group_by(country_name) %>%
    arrange(year) %>%
    mutate(prev.rank = lag(rank)) %>%
    ungroup() %>%
    group_by(year) %>%
    arrange(rank, prev.rank) %>%
    mutate(x = seq(1, n())) %>%
    ungroup()
  staticplot = ggplot(formatted_tweets, aes(x=x, y = value,group = country_name, fill = as.factor(country_name), color = as.factor(country_name))) + 
    #	   geom_tile(aes(y = value/2, height = value, width = 0.9)) + 
    geom_col()+ 
    geom_text(aes(y = 0, label = paste(country_name," ")),col="black",vjust = 0.2,hjust = 1, size = 11)+
    geom_text(aes(y = value, label = Value_lbl, hjust = +1),size=11, col = "black") +
    scale_y_continuous(labels = scales::comma, limits=c(0,maxvalue)) + 
    scale_x_reverse() +
    coord_flip(expand = FALSE, clip="off") + 
    theme(axis.line = element_blank(), 
          axis.text.x=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          legend.position = "none", 
          panel.background=element_blank(), 
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.title=element_text(size=36, hjust=0.5, face="bold", colour="grey", vjust=-1),
          plot.subtitle=element_text(size=30, hjust=0.5, face="italic", color="grey"),
          plot.caption =element_text(size=30, hjust=0.5, face="italic", color="grey"), 
          plot.margin = margin(2,2, 2, 10, "cm"))
  anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
    view_follow(fixed_x = FALSE)  +
    labs(title = 'Cidades com mais Igrejas Evangélicas {closest_state}',
         subtitle  =  mysubtitle,
         caption  = mycaption,
         x = "Cidades",
         y = "Número de Igrejas Evangélicas Criadas")
  
  if(output == "gif"){
    # For GIF
    animate(anim, length(formatted_tweets$year), fps = 250,  width = 3200, height = 1800,
            renderer = gifski_renderer("animation_lowres.gif"))
  }
  else{
    # For MP4
    animate(anim,length(formatted_tweets$year), fps = 250,  width = 3200, height = 1800,
            renderer = ffmpeg_renderer()) -> for_mp4
    anim_save("animation.mp4", animation = for_mp4 )
  }
}


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

cleanText <- function(vector) {
  # Process text from a given vector
  #     vector: vector of characters
  require(tm)
  require(stringr)
  vector <- iconv(vector, to = 'ASCII//TRANSLIT')
  vector <- tolower(vector)
  vector <- tm::removePunctuation(vector)
  vector <- tm::stripWhitespace(vector)
  vector <- tm::removeNumbers(vector)
  vector <- stringr::str_replace(gsub("\\s+", " ", str_trim(vector)), "B", "b")
  vector <- 
  return(vector)
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
tidy_evang$country_name <- cleanText(tidy_evang$country_name)
mun$clean_nome <- cleanText(mun$nome)
mapa1 <- plot_brmap(mun,
                    data_to_join = tidy_evang %>% filter(year == ymd("2018-01-01")),
                    join_by = c("clean_nome" = "country_name"),
                    var = "value")
mapa1 +
  scale_fill_viridis_c(option = 2) +
  labs(title = "Número de Igrejas Evangélicas em 2018")
