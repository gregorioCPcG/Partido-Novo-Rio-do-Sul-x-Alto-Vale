# estatística descritiva

# alto vale 27 cidades, a maior delas tem 70 mil habitantes. Logo mudamos a estratégia
# e cadastramos os bairros dessa cidade e assim obtemos mais casos e valores similares
#POPULACIONAIS
# 51 OBS - SENDO 27 CIDADES E 24 BAIRROS DA MAIOR CIDADE DA REGIÃO
# a votação em % votos válidos para João Amoedo

library(readxl)
Pasta1 <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/amoedo rio do sul vs alto vale/Pasta1.xlsx")
summary(Pasta1$Amoedo2018)

boxplot(Pasta1$Amoedo2018)
hist(Pasta1$Amoedo2018, ylab = "N", xlab = "Votos Válidos Amoedo", col = "red", 
     main = "Votação em Amoedo 27 cidades do Alto Vale e 28 bairros de Rio do Sul")
sd(Pasta1$Amoedo2018)


str(Pasta1)
Pasta1$RioDoSul <- as.factor(Pasta1$RioDoSul)
summary(Pasta1$RioDoSul)
plot(Pasta1$RioDoSul)


        