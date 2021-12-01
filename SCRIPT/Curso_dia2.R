# Oficina R - Segundo dia 

library(PNADcIBGE)
library(dplyr)
library(ggplot2)
library(survey)
library(convey)

options(scipen = 999)

# Importação

dadosPNADc2020 <- get_pnadc(year = 2020, quarter = 4)

# Survey

CorRaça <- svytable(~V2010, design = dadosPNADc2020)%>%
  as.data.frame()

Sexo <- svytotal(~V2007, design = dadosPNADc2020)%>%
  as.data.frame()

MediaDomicilio <- svymean(~V2001, design = dadosPNADc2020, na.rm = T)%>%
  as.data.frame()

Domicilios <- svytable(~V2001, design = dadosPNADc2020)%>%
  as.data.frame()

# Interação entre duas variaveis 

Alfabetização <- svytable(~V2010 + V3001, design = dadosPNADc2020)%>%
  as.data.frame()%>%
  mutate(Prop = Freq/sum(Freq))

Alfabetização2 <- svytotal(~interaction(V2010, V3001), design = dadosPNADc2020, na.rm = T)%>%
  as.data.frame()

AlfabetizaçãoUF <- svyby(~V3001, by=~UF, design = dadosPNADc2020, FUN = svytotal, na.rm = T)%>%
  as.data.frame()

# Grafico 

ggplot(data = Alfabetização, aes(x = V3001, y = Prop, fill = V2010))+
  geom_col(position = "fill")+
  labs(x = "Sabe ler e escrever?", y= "Proporção", title = "População Brasileira", fill = "Raça ou Cor")+
  scale_y_continuous(labels = function(x) paste(100*x, "%"))

# Subcojunto

svytable(~V2010, design = subset(dadosPNADc2020, UF == "Rio Grande do Sul"))%>%
  as.data.frame()
  
DesalentoRseSC <- svytable(~VD4005 + UF, design = subset(dadosPNADc2020, UF == "Rio Grande do Sul" | UF == "Santa Catarina"))%>%
  as.data.frame()

# Mercado de trabalho

DesocupadosRMPA <- svytable(~VD4002, design = subset(dadosPNADc2020, RM_RIDE == "Região Metropolitana de Porto Alegre (RS)"))%>%
  as.data.frame()%>%
  mutate(Prop = Freq/sum(Freq))

DesalentoRMPA <- svytable(~VD4005, design = subset(dadosPNADc2020, RM_RIDE == "Região Metropolitana de Porto Alegre (RS)"))%>%
  as.data.frame()

dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 <= 14] <- "0 a 14 anos"
dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 >= 15 & dadosPNADc2020$variables$V2009 <= 20] <- "15 a 20 anos"
dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 > 20 & dadosPNADc2020$variables$V2009 <= 30] <- "21 a 30 anos"
dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 > 30 & dadosPNADc2020$variables$V2009 <= 40] <- "31 a 40 anos"
dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 > 40 & dadosPNADc2020$variables$V2009 <= 50] <- "41 a 50 anos"
dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 > 50 & dadosPNADc2020$variables$V2009 <= 60] <- "51 a 60 anos"
dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 > 60 & dadosPNADc2020$variables$V2009 <= 70] <- "61 a 70 anos"
dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 > 70 & dadosPNADc2020$variables$V2009 <= 80] <- "71 a 80 anos"
dadosPNADc2020$variables$faixa_etaria[dadosPNADc2020$variables$V2009 > 80] <- "Mais de 80 anos"

FaixaEtariaEsc <- svytable(~V3002 + faixa_etaria, design = subset(dadosPNADc2020, UF == "Rio Grande do Sul"))%>%
  as.data.frame()%>%
  mutate(Prop = Freq/sum(Freq))

# Renda

dez <- svyquantile(~VD4020, design = dadosPNADc2020, quantiles = .10, na.rm = T)
noventanove <- svyquantile(~VD4020, design = dadosPNADc2020, quantiles = .99, na.rm = T)

maispobres1 <- svymean(~VD4020, design = subset(dadosPNADc2020, VD4020 <=dez[1]), na.rm = T)%>%
  as.data.frame()

maisricos1 <- svymean(~VD4020, design = subset(dadosPNADc2020, VD4020 >= noventanove[1]), na.rm = T)%>%
  as.data.frame()

# Indice de Gini

dadosPNADc2020 <- convey_prep(dadosPNADc2020)

gini <- svygini(~VD4020, design = dadosPNADc2020, na.rm = T)%>%
  as.data.frame()

giniUF <- svyby(~VD4020, by =~UF, design = dadosPNADc2020, FUN = svygini, na.rm = T)%>%
  as.data.frame()

svylorenz(~VD4020, dadosPNADc2020, na.rm = T)
