### Oficina de R

### Calculadora

1 + 1

200 - 100

20/5

5*5

10^2

soma <- 200 + 300

rm(soma)

#Utilizar igualdades ou desigualdades:  >, >=, <, <=, ==, !=
#Mais de uma condição: & (para e), | (para ou)

### Instalar e carregar pacotes

install.packages(c("PNADcIBGE", "dplyr", "ggplot2", "survey", "convey"))

library(PNADcIBGE)
library(dplyr)
library(ggplot2)
library(survey)
library(convey)

# Importação Online

dadosPNADc2020 <- get_pnadc(year = 2020, quarter = 4)
View(dadosPNADc2020)
View(dadosPNADc2020$variables)

# Importação Offline

dadosPNADc2020_brutos <- read_pnadc("PNADC_042020.txt", "Input_PNADC_trimestral.txt")

dadosPNADc2020_brutos <- pnadc_labeller(dadosPNADc2020_brutos, "dicionario_PNADC_microdados_trimestral.xls")

dadosPNADc2020 <- pnadc_design(dadosPNADc2020_brutos)


# dplyr básico

# Arrange - Altera a ordem das linhas 

exemplo_arrange <- arrange(dadosPNADc2020_brutos, V2008)

# desc() para reordenar por uma coluna na ordem decrescente

exemplo_arrange2 <- arrange(dadosPNADc2020_brutos, desc(V2008))

# Select - selecionar variáveis

exemplo_select <- select(dadosPNADc2020_brutos, c("Ano", "UF", "V2007", "V2010", "V3001", "VD4002", "VD4016"))

# Filter - filtrar base de dados(permite que você crie um subconjunto de observações com base em seus valores.)

exemplo_filter <- filter(dadosPNADc2020_brutos, UF=="Rio Grande do Sul")
exemplo_filter2 <- filter(dadosPNADc2020_brutos, UF=="Rio Grande do Sul"|UF=="Santa Catarina"|UF=="Paraná")

# Mutate - criar variáveis a partir de outras

exemplo_mutate <- mutate(dadosPNADc2020_brutos, valor_per_capita = VD4019/V2001)

# Summarise - agrupar variáveis por soma, média, contagem, etc

exemplo_summarise <- summarise(dadosPNADc2020_brutos, media=mean(V2009, na.rm=TRUE))

# Exercicios 

# Selecionar variaveis de Sexo(V2007), UF(UF), Idade(V2009) e Sabe ler? (V3001)
variaveis <- select(dadosPNADc2020_brutos, c("V2007", "UF", "V2009", "V3001"))

#filtrar somente mulheres nos dados 
exemplo_filter <- filter(variaveis, V2007 == "Mulher")

#media de idades das mulheres
idade <- summarise(exemplo_filter, media = mean(V2009, na.rm = TRUE))

#Outras funções: variância (var), mínimo (min), máximo (max), amplitude (range), mediana (median), quantis (quantile)

#tabela com peso

tab_compeso <- svytable(~V2010, design=dadosPNADc2020)%>%
  as.data.frame()%>%
  mutate(Prop = Freq/sum(Freq))%>%
  arrange(desc(Prop))

# Criar o gráfico

ggplot(data=tab_compeso, aes(x=V2010, y=Prop))+
  geom_col()

# Colocar em ordem crescente

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col()

# Colocar em ordem decrescente

ggplot(data=tab_compeso, aes(x=reorder(V2010, -Prop), y=Prop))+
  geom_col()

#Definir cor

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col(fill="blue", color="black")

#Colocar valores

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col(fill="blue", color="black")+
  geom_text(aes(label=Prop))

#Arredondar valores

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col(fill="blue", color="black")+
  geom_text(aes(label=round(Prop, digits=3)))

#Colocar em termos percentuais

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col(fill="blue", color="black")+
  geom_text(aes(label=round(100*Prop, digits=1)))

#Adicionar o símbolo %

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col(fill="blue", color="black")+
  geom_text(aes(label=paste(round(100*Prop, digits=1), "%")))

#Ajustar posição vertical dos números

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col(fill="blue", color="black")+
  geom_text(aes(label=paste(round(100*Prop, digits=1), "%")), vjust=-0.3)

#Nomear gráfico e eixos

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col(fill="blue", color="black")+
  geom_text(aes(label=paste(round(100*Prop, digits=1), "%")), vjust=-0.3)+
  labs(x="Raça/Cor", y="Proporção", title="População")

#Ajustar escala do eixo y

ggplot(data=tab_compeso, aes(x=reorder(V2010, Prop), y=Prop))+
  geom_col(fill="blue", color="black")+
  geom_text(aes(label=paste(round(100*Prop, digits=1), "%")), vjust=-0.3)+
  labs(x="Raça/Cor", y="Proporção", title="População")+
  scale_y_continuous(labels = function(x) paste(100*x, "%"))
