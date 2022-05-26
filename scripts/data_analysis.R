#' ---
#' Title: Script - Data Analise
#' By: Izadora Santos de Carvalho
#' email: izadoraflorestal@gmail.com
#' 
#' ---



rm(list = ls()) # to remove objects from environment (a character 
#ector naming objects to be removed)

##Criando um projeto: dentro da pasta GitHub, crie um projeto. Esse sera o nome da pasta criada e do repositorio no GitHub.
#usethis::create_project("~/GitHub/research_data_analysis_pncm")
#usethis::use_git() #para ativar o painel "Git"
#usethis::use_github() #para criar o repositorio na sua conta do GitHub
#usethis::use_readme_md() #funcao para criar um arquivo README.md
##para criar um projeto a partir um repositorio existente:
#usethis::create_from_github("R-Ladies-Sao-Paulo/RLadies-Brasil",
                            #destdir = "C:/Users/beatr/Documents/GitHub", 
                            #fork = TRUE)

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
library(webr) # gerar graficos com os valores dos testes estatisticos
library(ggpubr) # para criar plots de forma simples (for creating easily publication ready plots)
library(rstatix) # provides pipe-friendly R functions for easy statistical analyses
library(gridExtra) # plotar tabela de dados

############################ Data tables############################

getwd()
dir()

# carregando o arquivo de dados

df_geral <- read_csv2("data_tables/Banco_de_Dados_campo2021_v2.csv", col_names = T)
#df_geral <- read_excel("data_tables/Banco_de_Dados_campo2021.xlsx", 
                       #sheet = "data_ok")
View(df_geral)

str(df_geral)
#df_geral <- as.data.frame(df_geral)

df_geral$vegetation <- as.factor(df_geral$vegetation)
df_geral$Area <- as.factor(df_geral$Area)
df_geral$last_fire <- as.factor(df_geral$last_fire)
df_geral$fire_count <- as.factor(df_geral$fire_count)


### Data Summarise

# df.summarise <- ddply(df.geral, .(Vegetation,Area, last_fire, fire_count), summarise, 
#                       Qnt_plot=length(unique(Parcela)),
#                       FvFm=mean(Fv_Fm),
#                       RCABS=mean(RC_ABS),
#                       FvFo=mean(Fv_Fo),
#                       PI=mean(PI),
#                       T_Max=mean(T_Max),
#                       T_Min=mean(T_Min),
#                       T_Mean=mean(T_Mean),
#                       Diameter=mean(Diameter_cm),
#                       Height=mean(Height_cm),
#                       SPAD=mean(SPAD))

df.summarise <- ddply(df_geral, .(vegetation, last_fire, Area), summarise, 
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

qnt_plots <- ddply(df_geral, .(vegetation, last_fire), summarise,
                      Qnt_plot=length(unique(Parcela_2)))

#Gerando tabela
sum(qnt_plots$Qnt_plot)
table_qnt_plots <- grid.table(qnt_plots)
table_qnt_plots




str(df.summarise)

##### TESTE DOS PRESSUPOSTOS DA ANALISE DE VARIANCIA #######
### Testando a homogeneidade das variancias atraves do teste de
### Bartlett. 

bartlett.test(FvFm ~ vegetation, data=df.summarise)
t.h1 <- plot(FvFm ~ vegetation, data=df.summarise)

bartlett.test(RCABS ~ vegetation, data=df.summarise)
t.h2 <- plot(RCABS ~ vegetation, data=df.summarise)

bartlett.test(FvFo ~ vegetation, data=df.summarise)
t.h3 <- plot(FvFo ~ vegetation, data=df.summarise)

bartlett.test(PI ~ vegetation, data=df.summarise)
t.h4 <- plot(PI ~ vegetation, data=df.summarise)

bartlett.test(T_Max ~ vegetation, data=df.summarise)
t.h5 <- plot(T_Max ~ vegetation, data=df.summarise)

bartlett.test(T_Min ~ vegetation, data=df.summarise)
t.h6 <- plot(T_Min ~ vegetation, data=df.summarise)

bartlett.test(T_Mean ~ vegetation, data=df.summarise)
t.h7 <- plot(T_Mean ~ vegetation, data=df.summarise)

bartlett.test(Diameter ~ vegetation, data=df.summarise)
t.h8 <- plot(Diameter ~ vegetation, data=df.summarise)

bartlett.test(Height ~ vegetation, data=df.summarise)
t.h9 <- plot(Height ~ vegetation, data=df.summarise)

bartlett.test(SPAD ~ vegetation, data=df.summarise)
t.h10 <- plot(SPAD ~ vegetation, data=df.summarise)

#Box plots with p-values
# bxp <- ggboxplot(df.summarise, x = "vegetation", y = "SPAD")
# bxp
# t_spad <- t_spad %>% add_xy_position(x = "supp")
# bxp + 
#   stat_pvalue_manual(t_spad, label = "p") +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))


#Verificao da normalidade dos dados
glimpse(df.summarise)

shapiro.test(df.summarise$FvFm)
shapiro.test(df.summarise$RCABS)
shapiro.test(df.summarise$FvFo)
shapiro.test(df.summarise$PI)
shapiro.test(df.summarise$T_Max)
shapiro.test(df.summarise$T_Min)
shapiro.test(df.summarise$T_Mean)
shapiro.test(df.summarise$Diameter)
shapiro.test(df.summarise$Height)
shapiro.test(df.summarise$SPAD)



# Rodar analise de variancia
df.summarise
modelo <- aov(Diameter~vegetation+last_fire, data = df.summarise)
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

g3 <- boxplot(Diameter ~ vegetation, data = df.summarise, 
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
  aes(x = last_fire, y = FvFm, fill = last_fire) +
  geom_boxplot() +
  #geom_boxplot(show.legend = FALSE) +
  #scale_fill_manual(values=c("#A6611A","#DFC27D")) +
  ylab('Rendimento quântico máximo do FSII (Fv/Fm)') +  xlab('Last Fire') + 
  guides(x = guide_axis(angle = -45)) +
  #scale_y_continuous(breaks = seq(0,100,10)) + 
  theme_bw() + 
  theme(axis.title.x = element_text(colour="black", size=12,
                                    face="bold")) +
  theme(axis.title.y = element_text(colour="black", size=12,
                                    face="bold", vjust = 2.0)) +
  theme(axis.text = element_text(colour = "black", size = 12)) +
  theme(panel.grid = element_line(color = "white")) +
  facet_wrap(vars(vegetation))


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
  facet_wrap(vars(vegetation))


b1
x11()
