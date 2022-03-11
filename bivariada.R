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
        ylab = "% votos válidos em Amoedo", xlab = "", 
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

table(Pasta1$RioDoSul, Pasta1$SC)
mean(Pasta1$SC)
summary(Pasta1$SC)
Pasta1$dep <- as.numeric(Pasta1$SC)
logit<- glm(formula = dep ~  RioDoSul, data = Pasta1, family = 'binomial')
summary(logit, odds_ratios = TRUE)
(2.86-1)*100
#ESTAR EM RIO DO SUL ELEVA 
#186 % ACHANCE DE PERTENCER AO GRUPO QUE É MAIOR DO A VOTAÇÃO DE SANTA CATARINA
# COMPARADO AS OUTRAS 27 CIDADES DO ALTO VALE

library(coefplot)

coefplot(model, intercept = FALSE)
coefplot(logit, intercept = FALSE)
# proceder outra estratégia


hist(Pasta1$Amoedo2018)
sim <- subset(Pasta1, RioDoSul == 'Sim', select= c(Amoedo2018, RioDoSul))
nao <- subset(Pasta1, RioDoSul == 'Não', select= c(Amoedo2018, RioDoSul))
hist(sim$Amoedo2018)
hist(nao$Amoedo2018)
# sim
sim$Amoedo2018
sd(sim$Amoedo2018)
summary(sim$Amoedo2018)
a <- rnorm(n = 44000, mean = c(5.00, 3.91, 5.58, 3.95, 3.34, 3.78, 3.45, 4.82,
                               4.32, 4.95, 6.46, 4.51, 3.06, 4.78, 7.46, 4.09,
                               3.78, 2.82, 4.69, 1.92, 6.56,
                               5.36, 4.47, 3.77), sd = 1.247)
hist(a)
hist(sim$Amoedo2018)
summary(a)
SIM <- data.frame(a)
n <- 1000
library(dplyr)
samp <- sample_n(SIM, n)
library(tidyverse)
samp %>%
  summarise (mean_samp60 = mean(a), med_s60 = median(a),
             se_s60 = sd(a))
summary(SIM$a)#para conferir
sd(SIM$a) # para conferir
sd(sim$Amoedo2018)
summary(sim$Amoedo2018)
#confidence 95%
z_star_95 <- qnorm(0.975)
z_star_95
samp %>%
  summarise(lower = mean(a) - z_star_95 * (sd(a) / sqrt(n)),
            upper = mean(a) + z_star_95 * (sd(a) / sqrt(n)))

# confidence levels
params <- SIM %>%
  summarise(mu = mean(a))

samp %>%
  summarise(samp_min = min(a), samp1_max = max(a))
SIM %>%
  summarise(pop_min = min(a), pop_max = max(a))#para conferir
sim %>%
  summarise(pop_min = min(Amoedo2018), pop_max = max(Amoedo2018))

library(statsr)
library(dplyr)
ci <- SIM %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(a) - z_star_95 * (sd(a) / sqrt(n)),
            upper = mean(a) + z_star_95 * (sd(a) / sqrt(n)))
ci %>%
  slice(1:5) # só pra ver
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))
ci %>%
  slice(1:20) # só pra ver

ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line

#não
nao$Amoedo2018
sd(nao$Amoedo2018)
summary(nao$Amoedo2018)
b <- rnorm(n = 44000, mean = c(2.20, 2.51, 2.34, 2.43, 3.64, 1.46, 4.75, 3.73, 2.69, 2.56,
                               1.06, 3.38, 2.56, 2.16, 1.19, 2.19, 2.84, 1.41, 2.62, 3.03, 3.79,
                               1.40, 2.54, 2.44, 2.87, 2.39, 4.26), sd = 0.9051)
hist(b)
hist(nao$Amoedo2018)
summary(b)
NAO <- data.frame(b)
n <- 1000
library(dplyr)
samp <- sample_n(NAO, n)
library(tidyverse)
samp %>%
  summarise (mean_samp60 = mean(b), med_s60 = median(b),
             se_s60 = sd(b))
summary(NAO$b)#para conferir
sd(NAO$b) # para conferir
sd(nao$Amoedo2018)
summary(nao$Amoedo2018)
#confidence 95%
z_star_95 <- qnorm(0.975)
z_star_95
samp %>%
  summarise(lower = mean(b) - z_star_95 * (sd(b) / sqrt(n)),
            upper = mean(b) + z_star_95 * (sd(b) / sqrt(n)))

# confidence levels
params <- NAO %>%
  summarise(mu = mean(b))

samp %>%
  summarise(samp_min = min(b), samp1_max = max(b))
NAO %>%
  summarise(pop_min = min(b), pop_max = max(b))#para conferir
nao %>%
  summarise(pop_min = min(Amoedo2018), pop_max = max(Amoedo2018))

library(statsr)
library(dplyr)
ci <- NAO %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(b) - z_star_95 * (sd(b) / sqrt(n)),
            upper = mean(b) + z_star_95 * (sd(b) / sqrt(n)))
ci %>%
  slice(1:5) # só pra ver
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))
ci %>%
  slice(1:20) # só pra ver

ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu))  +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray")  # draw vertical line
  
