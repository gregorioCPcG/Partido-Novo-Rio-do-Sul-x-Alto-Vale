# ponto bisserial
# Rio do SUl muito acima
library(readxl)
Pasta1 <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/amoedo rio do sul vs alto vale/Pasta1.xlsx")
options("scipen"=100, "digits"=4)
Pasta1$RioDoSul <- as.factor(Pasta1$RioDoSul)

by(Pasta1$Amoedo2018, Pasta1$RioDoSul, mean)
by(Pasta1$Amoedo2018, Pasta1$RioDoSul, median)
by(Pasta1$Amoedo2018, Pasta1$RioDoSul, max)
by(Pasta1$Amoedo2018, Pasta1$RioDoSul, sd)
by(Pasta1$Amoedo2018, Pasta1$RioDoSul, IQR)
library(ggplot2)
g <- ggplot(Pasta1, aes(RioDoSul, Amoedo2018))
g + geom_boxplot() +  xlab("Rio do Sul?") +
  ylab("% votos válidos Amoedo 2018 - turno 1")

ggplot(data = Pasta1, mapping = aes(x = Amoedo2018, colour = RioDoSul)) +
  geom_freqpoly(binwidth = 0.5)

barplot(Pasta1$Amoedo2018, horiz = FALSE,
        ylab = "N", xlab = "Votos Válidos Amoedo", 
        col = Pasta1$RioDoSul, main = "Votação em Amoedo 27 cidades do 
        Alto Vale e 28 bairros de Rio do Sul")
legend("topright",                                    # Add legend to barplot
       legend = c("Bairros Rio do Sul", "Cidades Alto Vale"),
       fill = c("red", "black"))

ggplot(data = Pasta1) +
  geom_point(mapping = aes(x=RioDoSul, y =Amoedo2018 , color = RioDoSul))
ggplot(data = Pasta1) +
  geom_point(mapping = aes(x = RioDoSul, y = Amoedo2018, color = Amoedo2018 > 2.5 )) # BRA



bar.m <- ggplot(data = Pasta1, aes(RioDoSul,Amoedo2018))
bar.m + geom_bar(stat = "summary", fun = "mean",
                 fill = "steelblue") + 
  stat_summary(aes(label=round(..y.., 2)),
               fun = mean, geom = "text", 
               size=4, vjust=10, color = "black")+
  stat_summary(fun.data = mean_se, geom="errorbar", 
               width = 0.6) +
  labs(title = "Médias por Grupo - Amoedo 2018", 
       subtitle = "Bairros Rio do Sul x Cidades Alto Vale", x = "Rio do Sul?", y = "Votos Válidos Amoedo", 
       caption = "Fonte: TSE") + theme_bw() + theme(text = element_text(size = 13))
cor.test(Pasta1$Amoedo2018, Pasta1$rsl)
library(polycor) #pacote exigido
polyserial(Pasta1$Amoedo2018, Pasta1$rsl)


# regressao linear
model <- lm(Amoedo2018 ~ RioDoSul, data=Pasta1)
summary(model)

# transformações e logit
ggplot(data = Pasta1) +
  geom_point(mapping = aes(x = RioDoSul, y = Amoedo2018, color = Amoedo2018 > 4.01 )) # SC


Pasta1$SC <- Pasta1$Amoedo2018 > 4.01
mean(Pasta1$SC)
summary(Pasta1$SC)
Pasta1$dep <- as.numeric(Pasta1$SC)
logit<- glm(formula = dep ~  RioDoSul, data = Pasta1, family = 'binomial')
summary(logit, odds_ratios = TRUE)
(2.86-1)*100
#ESTAR EM RIO DO SUL ELEVA 
#186 % ACHANCE DE PERTENCER AO GRUPO QUE É MAIOR DO A VOTAÇÃO DE SANTA CATARINA
# COMPARADO AS OUTRAS 27 CIDADES DO ALTO VALE

