# Library
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
  filter(data_inicio < zoo::as.yearmon("Jan 2019"),
         data_inicio > zoo::as.yearmon("Dec 1959")) %>%
  group_by(data_inicio) %>% 
  count() %>% 
  ggplot(aes(x = data_inicio, y = n)) +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2004, 2008, 2012, 2016)) + 
  geom_line() +
  labs(title = "Criação de Igrejas Evangélicas por mês/ano", x = "Tempo (mês/ano)", y = "Número de igrejas criadas") +
  theme_bw()


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
cowplot::plot_grid(p1,p2, nrow = 2)
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



###### 

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

tidy_evang <- getTidyGDP(evang)
animateTidyWords(tidy_evang %>% filter(year >= ymd("1970-01-01")) %>% filter(year < ymd("2018-01-01")), 
                 output="gif", filename = "animation_hashtag",
                 mysubtitle = "", mycaption = "Dadoscope - https://revistaforum.com.br/blogs/dadoscope/")

p4 <- tidy_evang %>% ungroup() %>% 
  arrange(year,value) %>% tail(10) %>% 
  ggplot(aes(x= reorder(country_name, value),
             y = value, 
             group = country_name, 
             fill = as.factor(country_name), 
             color = as.factor(country_name))) + 
  #	   geom_tile(aes(y = value/2, height = value, width = 0.9)) + 
  geom_bar(stat="identity")+ 
  coord_flip(expand = FALSE, clip="off") + 
  labs(title="Cidades com mais Igrejas Evangélicas criadas no período 1960-2018",
       x = "Cidades",
       y = "Número de Igrejas Evangélicas Criadas")+
  theme(legend.position = "none")+
  geom_text(aes(x= reorder(country_name, value),
                y = value,
                label = value),
            col = "black",
            hjust=1)
png("igrejas_por_cidade.png",width=3200,height=1800,res=300)
print(p4)            
dev.off()
