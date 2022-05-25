#' ---
#' Title: Script - Data Analise
#' By: Izadora Santos de Carvalho
#' email: izadoraflorestal@gmail.com
#' 
#' ---



library()
rm(list = ls()) # to remove objects from environment (a character 
#ector naming objects to be removed)

#update.packages(checkBuilt=TRUE, ask=FALSE)
###Packages

library(dplyr) # summarizing the result (data) of a single analysis
library(readr) # abrir tabela de dados no formata csv
library(geobr) # carregar base de dados vetorias oficial do Brasil
library(ggplot2) # Data Visualization # to plot graphics
library(plyr) # trabalhando com dataframe
library(RStoolbox) # to work with spatial data # A Collection of Remote Sensing Tools
library(rgdal) # to work with coordinate reference system 
library(raster) # to work with raster
library(rasterVis) # visualization Methods for Raster Data
library(rgeos) # to work with spatial geometry
library(sf) # to work with spatial vectors 
library(spatial) # to work spatial data
library(tmap) # to work spatial data
library(tidyverse) #  data import, tidying, manipulation, visualisation, and programming
library(cowplot) # Data Visualization #add-on to ggplot like ggsave2
library(readxl) # abrir tabela de dados no formato excel (xlm, xls,...)

############################ Data tables############################

getwd()
dir()

# carregando o arquivo de dados

df.geral <- read_csv("data_tables/Banco_de_Dados_campo2021.csv", col_names = T)
#df_geral <- read_excel("data_tables/Banco_de_Dados_campo2021.xlsx", 
                       #sheet = "data_ok")
View(df_geral)

str(df_geral)
df_geral <- as.data.frame(df_geral)

df.geral$Vegetation <- as.factor(df.geral$Vegetation)
df.geral$Area <- as.factor(df.geral$Area)
df.geral$last_fire <- as.factor(df.geral$last_fire)
df.geral$fire_count <- as.factor(df.geral$fire_count)


### Data Summarise

df.summarise <- ddply(df.geral, .(Vegetation,Area, last_fire, fire_count), summarise, 
                      Qnt_plot=length(unique(Parcela)),
                      FvFm=mean(Fv_Fm),
                      RCABS=mean(RC_ABS),
                      FvFo=mean(Fv_Fo),
                      PI=mean(PI),
                      T_Max=mean(T_Max),
                      T_Min=mean(T_Min),
                      T_Mean=mean(T_Mean),
                      Diameter=mean(Diameter_cm),
                      Height=mean(Height_cm),
                      SPAD=mean(SPAD))

df.summarise <- ddply(df.geral, .(Vegetation, last_fire, Area), summarise, 
                      Qnt_plot=length(unique(Parcela)),
                      FvFm=mean(Fv_Fm),
                      RCABS=mean(RC_ABS),
                      FvFo=mean(Fv_Fo),
                      PI=mean(PI),
                      T_Max=mean(T_Max),
                      T_Min=mean(T_Min),
                      T_Mean=mean(T_Mean),
                      Diameter=mean(Diameter_cm),
                      Height=mean(Height_cm),
                      SPAD=mean(SPAD))
# Rodar analise de variancia
df.summarise
modelo <- aov(Diameter~Vegetation+last_fire, data = df.summarise)
anova(modelo)

# Rodar teste de comparacao de medias de Tukey
testetukey <- TukeyHSD(modelo, "Vegetation", alpha = 0.05)
testetukey

# Grafico boxplot para teste de comparacao de medias
g1 <- plot(testetukey, las = 1, col = "black")

g2 <- LSD.test(modelo, "last_fire", alpha = 0.05, group = T)
new_data <- data.frame(last_fire=rownames(g2$groups), g2$groups)
new_data <- new_data[order(new_data$last_fire)]

labels2 <- data.frame(new_data$groups)

g3 <- boxplot(Diameter ~ Vegetation, data = df.summarise, 
              #ylim=c(min(df.summarise$SPAD), 1.1*max(df.summarise$SPAD)),
              #col=c("black", "red", "blue", "green"),
              ylab="Diameter (cm)", xlab="Last Fire", main= "Boxplots", las = 1)

x11()
# adicionando letra em cima de cada boxplot
over <- 0.05*max(g3$stats[nrow(g3$stats),])
text(c(1:nlevels(df.summarise$last_fire)), g3$stats[nrow(g3$stats),]+over,
     labels2[,1], col=c("black"))
# Carregar pacote
library(agricolae)
library(multcompView)

# Definir diretorio dos dados
# setdw("....")
########
b0 <- ggplot(df.summarise) +
  aes(x = last_fire, y = Diameter, fill = last_fire) +
  geom_boxplot() +
  #geom_boxplot(show.legend = FALSE) +
  #scale_fill_manual(values=c("#A6611A","#DFC27D")) +
  ylab('Diameter (cm)') +  xlab('') + 
  guides(x = guide_axis(angle = -45)) +
  #scale_y_continuous(breaks = seq(0,100,10)) + 
  theme_bw() + 
  theme(axis.title.x = element_text(colour="black", size=20,
                                    face="bold")) +
  theme(axis.title.y = element_text(colour="black", size=20,
                                    face="bold", vjust = 2.0)) +
  theme(axis.text = element_text(colour = "black", size = 16)) +
  theme(panel.grid = element_line(color = "white")) +
  facet_wrap(vars(Vegetation))


b0
x11()

b1 <- ggplot(df.summarise) +
  aes(x = last_fire, y = Diameter, fill = last_fire) +
  geom_boxplot() +
  #geom_boxplot(show.legend = FALSE) +
  #scale_fill_manual(values=c("#A6611A","#DFC27D")) +
  ylab('Diameter (cm)') +  xlab('') + 
  guides(x = guide_axis(angle = -45)) +
  #scale_y_continuous(breaks = seq(0,100,10)) + 
  theme_bw() + 
  theme(axis.title.x = element_text(colour="black", size=20,
                                    face="bold")) +
  theme(axis.title.y = element_text(colour="black", size=20,
                                    face="bold", vjust = 2.0)) +
  theme(axis.text = element_text(colour = "black", size = 16)) +
  theme(panel.grid = element_line(color = "white")) +
  facet_wrap(vars(Vegetation))


b1
x11()
