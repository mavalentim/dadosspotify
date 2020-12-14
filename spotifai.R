#carregando bibliotecas
library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(ggThemeAssist)
install.packages('ggpubr')
library(ggpubr)
#usando a base de dados do kaggle - https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks

data <- read_csv("C:/Users/Matheus/Desktop/coisa x em r/spotify data/data.csv")

#COMPARANDO 1989 E FOLKLORE DA TAYLOR SWIFT
#######################
#subbase com ano 2020
datapandemia <- data%>%
  filter(year == '2020')

folklore <- datapandemia%>%
  filter(artists == "['Taylor Swift']")%>%
  mutate(album = 'folklore')

#plotting folklore

ggplot(folklore, aes(x=tempo ,y=popularity,group=1, color=name)) + geom_point(size=7)  +
  labs(x = "tempo", y = "popularidade no spotify", 
       title = "Musicas Famosas da Taylor")

##subbase com ano de lancamento de 1989
data2014<-data%>%
  filter(year == "2014")
album1989 <- data2014%>%
  filter(artists == "['Taylor Swift']")%>%
  mutate(album = '1989')
#limpando musicas duplicadas deste ultimo

album1989_2 <- album1989%>%
  group_by(name)%>%
  summarize(tempo = max(tempo), danceability = max(danceability), popularity = max(popularity),
            energy = max(energy), duration_ms = max(duration_ms), acousticness = max(acousticness),
            explicit = max(explicit), valence = max(valence), instrumentalness = max(instrumentalness),
            speechiness = max(speechiness))%>%
  ungroup()%>%
  mutate(album = '1989')


##subbase de reputation

data2017<-data%>%
  filter(year == "2017")
reputation <- data2017%>%
  filter(artists == "['Taylor Swift']")%>%
  mutate(album = 'Reputation')

##subbase de lover

data2019<-data%>%
  filter(year == "2019")
lover <- data2019%>%
  filter(artists == "['Taylor Swift']")%>%
  mutate(album = 'Lover')


##CRIANDO A BASE TAYLOR

taylor <-folklore%>%
  add_row(album1989_2)%>%
  add_row(reputation)%>%
  add_row(lover)%>%
  mutate(folklore = if_else(album=='folklore','sim','não'))

taylor2<- taylor%>%
  


#DANCEABILIDADE E TEMPO

#comparando todos os albuns
ggplot(taylor, aes(x=tempo ,y=danceability,group=1, shape=album, color= album)) + geom_point(size=7)  +
  labs(x = "tempo", y = "danceabilidade", 
       title = "Músicas dos Últimos Álbuns da Taylor") + theme(axis.line = element_line(linetype = "solid"), 
    panel.background = element_rect(fill = "white"), 
    legend.key = element_rect(fill = "white"), 
    legend.background = element_rect(fill = "white")) +labs(caption = "fonte:https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks 
elaboração: Matheus Valentim")

#comparando os álbuns
ggplot(taylor, aes(x=tempo ,y=danceability,group=1, shape= folklore,color=folklore)) + geom_point(size=7)  +
  labs(x = "tempo", y = "danceabilidade", 
       title = "Músicas dos Últimos Álbuns da Taylor") + theme(axis.line = element_line(linetype = "solid"), 
    panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"), 
    legend.key = element_rect(fill = "white"), 
    legend.background = element_rect(fill = "white")) +labs(caption = "fonte:https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks 
elaboração: Matheus Valentim")

####

#alegria da musica e quao acustica ela é

#comparando todos os albuns
ggplot(taylor, aes(x=acousticness ,y=valence,group=1, shape=album, color= album)) + geom_point(size=7)  +
  labs(x = "quão acústica é", y = "quão alegre é", 
       title = "Músicas dos Últimos Álbuns da Taylor") + theme(axis.line = element_line(linetype = "solid"), 
                                                               panel.background = element_rect(fill = "white"), 
                                                               legend.key = element_rect(fill = "white"), 
                                                               legend.background = element_rect(fill = "white")) +labs(caption = "fonte:https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks 
elaboração: Matheus Valentim")


#comparando os álbuns
ggplot(taylor, aes(x=acousticness ,y=valence,group=1,shape=folklore, color=folklore)) + geom_point(size=7)  +
  labs(x = "quão acústica é", y = "quão alegre é", 
       title = "Músicas dos Últimos Álbuns da Taylor") + theme(axis.line = element_line(linetype = "solid"), 
                                                               panel.grid.major = element_line(linetype = "blank"), 
                                                               panel.grid.minor = element_line(linetype = "blank"), 
                                                               panel.background = element_rect(fill = "white"), 
                                                               plot.background = element_rect(fill = "white"), 
                                                               legend.key = element_rect(fill = "white"), 
                                                               legend.background = element_rect(fill = "white")) +labs(caption = "fonte:https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks 
elaboração: Matheus Valentim")




####

#energy and popularity


#comparando todos os albuns
ggplot(taylor, aes(x=energy ,y=popularity,group=1, shape=album, color= album)) + geom_point(size=7)  +
  labs(x = "energia", y = "popularidade", 
       title = "Músicas dos Últimos Álbuns da Taylor") + theme(axis.line = element_line(linetype = "solid"), 
                                                               panel.background = element_rect(fill = "white"), 
                                                               legend.key = element_rect(fill = "white"), 
                                                               legend.background = element_rect(fill = "white")) +labs(caption = "fonte:https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks 
elaboração: Matheus Valentim")


#comparando os álbuns
ggplot(taylor, aes(x=energy ,y=popularity,group=1,shape=folklore, color=folklore)) + geom_point(size=7)  +
  labs(x = "energia", y = "popularidade", 
       title = "Músicas dos Últimos Álbuns da Taylor") + theme(axis.line = element_line(linetype = "solid"), 
                                                               panel.grid.major = element_line(linetype = "blank"), 
                                                               panel.grid.minor = element_line(linetype = "blank"), 
                                                               panel.background = element_rect(fill = "white"), 
                                                               plot.background = element_rect(fill = "white"), 
                                                               legend.key = element_rect(fill = "white"), 
                                                               legend.background = element_rect(fill = "white")) +labs(caption = "fonte:https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks 
elaboração: Matheus Valentim")



























#######

#COMPARANDO KPOP E HEAVY METAL

kpopsongs <- data%>%
  filter(artists == "['BLACKPINK']" | artists == "['BTS']")%>%
  mutate(estilo ='kpop')

heavymetal <- data%>%
  filter(artists == "['Iron Maiden']")%>%
  mutate(estilo = 'heavy metal')

toppop <- data%>%
  filter( artists == "['Katy Perry']"|
           artists == "['Ariana Grande']" |artists == "['Taylor Swift']")%>%
  mutate(estilo = 'pop')

  


comparacao1 <- kpopsongs%>%
  add_row(heavymetal)%>%
  add_row(toppop)%>%
  mutate(duracao = (duration_ms*0.001)/60)%>%
  group_by(estilo)%>%
  mutate(duracao_media = mean(duracao))%>%
  ungroup()

#danceabilidade e bpm
ggplot1 <-ggplot(comparacao1, aes(x=tempo ,y=danceability,group=1, color=estilo)) + geom_point(size=5, position = 'jitter')  +
  labs(x = "Batidas por Minuto", y = "Danceabilidade", 
       title = "Dá pra dançar isso aqui?")+ theme(axis.line = element_line(linetype = "dashed"), 
    axis.title = element_text(family = "mono", 
        size = 13), axis.text = element_text(family = "mono", 
        colour = "gray3"), axis.text.x = element_text(family = "mono"), 
    axis.text.y = element_text(family = "mono"), 
    plot.title = element_text(family = "mono", 
        size = 14), legend.text = element_text(size = 12, 
        family = "mono"), legend.title = element_text(size = 12, 
        family = "mono"), panel.background = element_rect(fill = NA), 
    plot.background = element_rect(colour = NA)) + theme(axis.line = element_line(linetype = "longdash"), 
    axis.text = element_text(colour = "black"), 
    legend.text = element_text(face = "bold"), 
    legend.title = element_text(colour = NA), 
    legend.key = element_rect(fill = NA), 
    legend.background = element_rect(fill = NA), 
    legend.position = "bottom", legend.direction = "horizontal")


#duracao da musica
ggplot(comparacao1, aes(x=estilo ,y=duracao_media ,group=1)) + geom_col(position = 'dodge')  +
  labs(x = "", y = "Duração (min)", 
       title = "Duração média em minutos de uma música")+ theme(axis.line = element_line(linetype = "dashed"), 
                                                  axis.title = element_text(family = "mono", 
                                                                            size = 13), axis.text = element_text(family = "mono", 
                                                                                                                 colour = "gray3"), axis.text.x = element_text(family = "mono"), 
                                                  axis.text.y = element_text(family = "mono"), 
                                                  plot.title = element_text(family = "mono", 
                                                                            size = 14), legend.text = element_text(size = 12, 
                                                                                                                   family = "mono"), legend.title = element_text(size = 12, 
                                                                                                                                                                 family = "mono"), panel.background = element_rect(fill = NA), 
                                                  plot.background = element_rect(colour = NA)) + theme(axis.line = element_line(linetype = "longdash"), 
                                                                                                       axis.text = element_text(colour = "black"), 
                                                                                                       legend.text = element_text(face = "bold"), 
                                                                                                       legend.title = element_text(colour = NA), 
                                                                                                       legend.key = element_rect(fill = NA), 
                                                                                                       legend.background = element_rect(fill = NA), 
                                                                                                       legend.position = "bottom", legend.direction = "horizontal")+labs(x = NULL)










figure <- ggarrange(ggplot1, ggplot2, 
                    labels = c("Dança?", "Duração"),
                    ncol = 1, nrow = 1)








































spotify_songs <- read_excel("C:/Users/Matheus/Desktop/coisa x em r/spotify data/spotify_songs.xlsx")

ts <- spotify_songs%>%
  filter(track_artist == 'Taylor Swift')%>%
  filter(track_popularity >0 )%>%
  filter(track_album_name=='1989' | track_album_name =='Lover' | track_album_name =='Red'
         | track_album_name == 'Speak Now' | track_album_name == 'reputation' | track_album_name == 'Taylor Swift')

ggplot(ts, aes(x=tempo ,y=track_popularity,group=1, color=track_name)) + geom_point(size=7)  +
  labs(x = "tempo", y = "popularidade no spotify", 
       title = "Musicas Famosas da Taylor")

ggplot(ts, aes(x=duration_ms ,y=track_popularity,group=1, color=track_name)) + geom_point(size=7)  +
  labs(x = "duração", y = "popularidade no spotify", 
       title = "Musicas Famosas da Taylor")


ggplot(ts, aes(x=acousticness ,y=track_popularity,group=1, color=track_name)) + geom_point(size=5)  +
  labs(x = "quão acústico é a música", y = "popularidade no spotify", 
       title = "Musicas Famosas da Taylor")



bitols <- spotify_songs%>%
  filter(track_artist == 'Katy Perry')%>%
  filter(track_album_name == 'Chained to the Ryhthm' | track_album_name == 'Never Really Over'
         | track_album_name == "Teenage Dream: The Complete Confection" | 
           track_album_name == 'One Of The Boys' | track_album_name == 'PRISM (Deluxe)'  )

ggplot(bitols, aes(x=energy ,y=track_popularity,group=1, color=track_album_name)) + geom_point(size=6)  +
  labs(x = "energia", y = "popularidade no spotify", 
       title = "Musicas da Katy Perry : energia x popularidade")



ggplot(bitols, aes(x=duration_ms ,y=track_popularity,group=1, color=track_album_name)) + geom_point(size=6)  +
  labs(x = "duração(ms)", y = "popularidade no spotify", 
       title = "Musicas da Katy Perry: duração e popularidade")











