library(SetMethods)
library(tidyverse)
library(QCA)


income_concentration <- read.csv('https://raw.githubusercontent.com/gpetrini/RedScare/main/clean/income_concentration.csv')
coldwar_conflicts <- read.csv('https://raw.githubusercontent.com/gpetrini/RedScare/main/clean/coldwar_concentracao_santanna_weller.csv')

teste <- as.data.frame(t(coldwar_conflicts))
colnames(teste) <- teste[1,]
teste2 <- teste[-1,] 

teste3 <- teste2
teste3[,-1] <- data.frame(sapply(teste3[,-1], function(x) as.numeric(as.character(x))))

#tirar 7 ultimas linhas e violentos/nao violentos

teste4 <- teste3[-c(5,10,11:17),]

#usar pc e decentralized. polity nao tava claro e union tem muito NA

############  exemplo indirect method

# Generate fake data
set.seed(4)
x <- runif(20, 0, 1) #20 n aleatorios de 0 a 1
# Find quantiles
quant <- quantile(x, c(.2, .4, .5, .6, .8)) 
# Theoretical calibration
x_cal <- NA
x_cal[x <= quant[1]] <- 0
x_cal[x > quant[1] & x <= quant[2]] <- .2
x_cal[x > quant[2] & x <= quant[3]] <- .4
x_cal[x > quant[3] & x <= quant[4]] <- .6
x_cal[x > quant[4] & x <= quant[5]] <- .8
x_cal[x > quant[5]] <- 1
x_cal
# Indirect calibration (binomial) #ajustando uma regressao entre x e xcal (onde eles pertencem)
a <- indirectCalibration(x, x_cal, binom = TRUE) #binom é tipo probit
#calibragem: definir ancoras qualitativas e a partir de ai ajustar uma função 

# Indirect calibration (beta regression)
b <- indirectCalibration(x, x_cal, binom = FALSE)
plot(x,a)
plot(x,a)
# Correlation
cor(a, b)
# Plot
plot(x, a); points(x, b, col = "red")

############ replicar por país #Australia
# a ideia é calibrar por país o quao proximo o conflito esteve dele. por ex conflito na europa, vai ter nota mais alta pra frança e mais baixa pra EUA

x <- as.numeric(teste4$AUS)
# Find quantiles
quant <- quantile((x), c(.2, .4, .5, .6, .8)) 
# Theoretical calibration
x_cal <- NA
x_cal[x <= quant[1]] <- 0
x_cal[x > quant[1] & x <= quant[2]] <- .2
x_cal[x > quant[2] & x <= quant[3]] <- .4
x_cal[x > quant[3] & x <= quant[4]] <- .6
x_cal[x > quant[4] & x <= quant[5]] <- .8
x_cal[x > quant[5]] <- 1
x_cal
# Indirect calibration (binomial) #ajustando uma regressao entre x e xcal (onde eles pertencem)
a <- indirectCalibration(x, x_cal, binom = TRUE) #binom é tipo probit
#calibragem: definir ancoras qualitativas e a partir de ai ajustar uma função 

# Indirect calibration (beta regression)
b <- indirectCalibration(x, x_cal, binom = FALSE)

# Correlation
cor(a, b)
# Plot
plot(x, a); points(x, b, col = "red")

############### FRANÇA
x <- as.numeric(teste4$FRA)
# Find quantiles
quant <- quantile((x), c(.2, .4, .5, .6, .8)) 
# Theoretical calibration
x_cal <- NA
x_cal[x <= quant[1]] <- 0
x_cal[x > quant[1] & x <= quant[2]] <- .2
x_cal[x > quant[2] & x <= quant[3]] <- .4
x_cal[x > quant[3] & x <= quant[4]] <- .6
x_cal[x > quant[4] & x <= quant[5]] <- .8
x_cal[x > quant[5]] <- 1
x_cal
# Indirect calibration (binomial) 
a <- indirectCalibration(x, x_cal, binom = TRUE) 

# Indirect calibration (beta regression)
b <- indirectCalibration(x, x_cal, binom = FALSE)

# Correlation
cor(a, b)
# Plot
plot(x, a); points(x, b, col = "red")

#FOR pra todos os paises
#transpoe de novo o data frame das variaveis nao utilizadas ainda: calibragem

#linhas sao os paises 
#colunas sao conflitos, as outras variaveis e o resultado
#calibrar os resultados
#diff/numero de anos 

#com o csv calibrado, vai praquela tela do QCA
teste4

#criando matriz vazia para depositar os valores do loop - calibragem
n <- 18
mat <- matrix(ncol=n, nrow=8)
colnames(mat) <- colnames(teste4)
rownames(mat) <- rownames(teste4)

#criando o loop para eventos - calibragem
for (i in 1:n) {
  x <- as.numeric(teste4[,i])
  quant <- quantile((x), c(.2, .4, .5, .6, .8))
  x_cal <- NA
  x_cal[x <= quant[1]] <- 0
  x_cal[x > quant[1] & x <= quant[2]] <- .2
  x_cal[x > quant[2] & x <= quant[3]] <- .4
  x_cal[x > quant[3] & x <= quant[4]] <- .6
  x_cal[x > quant[4] & x <= quant[5]] <- .8
  x_cal[x > quant[5]] <- 1
  x_cal
  a <- indirectCalibration(x, x_cal, binom = TRUE) 
  b <- indirectCalibration(x, x_cal, binom = FALSE)
  mat[,i] <- a
}

#calibrar pc, decentralized e diff/anos 
sem_eventos <- teste3 %>% 
  filter(rownames(teste3)== c("Diff","pc", "decentralized")) %>% 
  t() 

sem_eventos <- as.data.frame(sem_eventos)
sem_eventos$Diff <- as.numeric(sem_eventos[,1])

sem_eventos2 <- sem_eventos %>% 
  mutate(outcome = Diff/45) %>%
  select(pc, decentralized, outcome)

#calibragem dos dados - outcome, pc e decentralized

#criando matriz vazia
mat2 <- matrix(nrow= 18, ncol=3)
colnames(mat2) <- colnames(sem_eventos2)
rownames(mat2) <- rownames(sem_eventos2)

#calibrando
for (i in 1:3) {
  x <- as.numeric(sem_eventos2[,i])
  quant <- quantile((x), c(.2, .4, .5, .6, .8))
  x_cal <- NA
  x_cal[x <= quant[1]] <- 0
  x_cal[x > quant[1] & x <= quant[2]] <- .2
  x_cal[x > quant[2] & x <= quant[3]] <- .4
  x_cal[x > quant[3] & x <= quant[4]] <- .6
  x_cal[x > quant[4] & x <= quant[5]] <- .8
  x_cal[x > quant[5]] <- 1
  x_cal
  a <- indirectCalibration(x, x_cal, binom = TRUE) 
  b <- indirectCalibration(x, x_cal, binom = FALSE)
  mat2[,i] <- a
}

#Matrizes calibradas
mat #eventos
mat2 #pc, decentralized e outcome

#Transpondo e juntando as matrizes para o QCA
mat_eventos <- t(mat)
consolidated <- cbind(mat_eventos,mat2)

write.csv(consolidated, file ='~/Desktop/Monografia/dados/consolidated.csv')

#rodando QCA
QCA::runGUI()
