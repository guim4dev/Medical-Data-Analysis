### Script para Projeto da Matéria COE241 ###

## Importando os Dados ##
DadosMedicos <-
  read.table(
    "~/Projects/FinalProjectCOE241/Dados-medicos.csv",
    quote = "\"",
    comment.char = "",
    header = TRUE
  )


## Histograma e Distribuição Empírica das Variáveis##

#Determinando bin via Regra de Sturge#
bin = as.integer(1 + 3*3*log10(nrow(DadosMedicos)))

#IDADE#
hist(DadosMedicos$IDADE,
     main = "Histograma de IDADE",
     col = "darkmagenta",
     xlab = "Idade (em anos)")
idade_densidade = density(DadosMedicos$IDADE)
plot(idade_densidade, main = "PDF Empírica - IDADE")

#PESO#
hist(DadosMedicos$Peso,
     main = "Histograma de Peso",
     col = "darkmagenta",
     xlab = "Peso (em kg)")
peso_densidade = density(DadosMedicos$Peso)
plot(peso_densidade, main = "PDF Empírica - PESO")

#CargaFinal#
hist(DadosMedicos$CargaFinal,
     main = "Histograma de CargaFinal",
     col = "darkmagenta",
     xlab = "Carga Final")
CargaFinal_densidade = density(DadosMedicos$CargaFinal)
plot(CargaFinal_densidade, main = "PDF Empírica - Carga Final")

#VO2 Medido Maximo#
hist(DadosMedicos$VO2MedidoMáximo,
     main = "Histograma de VO2 Máximo",
     col = "darkmagenta",
     xlab = "VO2 Máximo (em mL/kg/min)")
VO2Max_densidade = density(DadosMedicos$VO2MedidoMáximo)
plot(VO2Max_densidade, main = "PDF - VO2 Máximo")

## Média, Variância e Boxplot das Variáveis ##

#IDADE#
media_idade = mean(DadosMedicos$IDADE)
variancia_idade = var(DadosMedicos$IDADE)
boxplot(DadosMedicos$IDADE, main = "Boxplot de Idade")

#PESO#
media_Peso = mean(DadosMedicos$Peso)
variancia_Peso = var(DadosMedicos$Peso)
boxplot(DadosMedicos$Peso, main = "Boxplot de Peso")

#CargaFinal#
media_CargaFinal = mean(DadosMedicos$CargaFinal)
variancia_CargaFinal = var(DadosMedicos$CargaFinal)
boxplot(DadosMedicos$CargaFinal, main = "Boxplot de CargaFinal")

#VO2 Medido Máximo#
media_VO2Max = mean(DadosMedicos$VO2MedidoMáximo)
variancia_VO2Max = var(DadosMedicos$VO2MedidoMáximo)
boxplot(DadosMedicos$VO2MedidoMáximo, main = "Boxplot de VO2 Máx")

## Estimando Parametros via MLE ##
## Distribuicoes: Exponencial, Gaussiana, Lognormal e Weibull ##
# Importando biblioteca EnvStats para calculo dos parametros da Weibull #
library(EnvStats)

#IDADE#
#-Exponencial-#

#Estimativa via MLE
idade_exponencial_lambda = 1/media_idade
idade_exponencial <- function(x) {
  dexp(x, rate = idade_exponencial_lambda)
}

############
#-Gaussiana-#

#Estimativas via MLE
idade_gaussiana_media = media_idade
idade_gaussiana_sd2 = variancia_idade
idade_gaussiana <- function(x) {
  dnorm(x, mean = idade_gaussiana_media, sd = sqrt(idade_gaussiana_sd2))
}

############
#-LogNormal-#

#Estimativas via MLE
idade_lognormal_mu = mean(log(DadosMedicos$IDADE, base = exp(1)))
idade_lognormal_sd2 = var(log(DadosMedicos$IDADE, base = exp(1)))
idade_lognormal <- function(x) {
  dlnorm(x, meanlog = idade_lognormal_mu, sdlog = sqrt(idade_lognormal_sd2))
}

############
#-Weibull-#

#Rodar esta linha para obter os parametros shape e scale#
eweibull(DadosMedicos$IDADE, method = "mle")
idade_weibull_shape = 4.89473
idade_weibull_scale = 58.782895
idade_weibull <- function(x) {
  dweibull(x, idade_weibull_shape, scale = idade_weibull_scale)
}

#criando grafico com todos juntos#
curve(idade_weibull, from = min(DadosMedicos$IDADE), to = max(DadosMedicos$IDADE), col = 'darkgreen', ylab = 'distribution', xlab = 'Idade')
curve(idade_lognormal, from = min(DadosMedicos$IDADE), to = max(DadosMedicos$IDADE), col = 'darkmagenta', add = TRUE)
lines(idade_densidade, col = 'black')
curve(idade_exponencial, from = min(DadosMedicos$IDADE), to = max(DadosMedicos$IDADE), col = 'red', add = TRUE)
curve(idade_gaussiana, from = min(DadosMedicos$IDADE), to = max(DadosMedicos$IDADE), col = 'blue', add = TRUE)
legend("topright", legend="idade", lty = 1, col = 'black', lwd = 2, bty = "n")
legend("right", legend="exponencial", lty = 1, col = 'red', lwd = 2, bty = "n")
legend("left", legend="gaussiana", lty = 1, col = 'blue', lwd = 2, bty = "n")
legend("topleft", legend="lognormal", lty = 1, col = 'darkmagenta', lwd = 2, bty = "n")
legend("bottom", legend="weibull", lty = 1, col = 'darkgreen', lwd = 2, bty = 'n')

############## FIM IDADE ################

#Peso#
#-Exponencial-#

#Estimativa via MLE
Peso_exponencial_lambda = 1/media_Peso
Peso_exponencial <- function(x) {
  dexp(x, rate = Peso_exponencial_lambda)
}

############
#-Gaussiana-#

#Estimativas via MLE
Peso_gaussiana_media = media_Peso
Peso_gaussiana_sd2 = variancia_Peso
Peso_gaussiana <- function(x) {
  dnorm(x, mean = Peso_gaussiana_media, sd = sqrt(Peso_gaussiana_sd2))
}

############
#-LogNormal-#

#Estimativas via MLE
Peso_lognormal_mu = mean(log(DadosMedicos$Peso, base = exp(1)))
Peso_lognormal_sd2 = var(log(DadosMedicos$Peso, base = exp(1)))
Peso_lognormal <- function(x) {
  dlnorm(x, meanlog = Peso_lognormal_mu, sdlog = sqrt(Peso_lognormal_sd2))
}

############
#-Weibull-#

#Rodar esta linha para obter os parametros shape e scale#
eweibull(DadosMedicos$Peso, method = "mle")
Peso_weibull_shape = 5.40801
Peso_weibull_scale = 92.24080
Peso_weibull <- function(x) {
  dweibull(x, Peso_weibull_shape, scale = Peso_weibull_scale)
}

#criando grafico com todos juntos#
plot(peso_densidade, col = 'black', main = '')
curve(Peso_lognormal, from = min(DadosMedicos$Peso), to = max(DadosMedicos$Peso), col = 'darkmagenta', add = TRUE)
curve(Peso_weibull, from = min(DadosMedicos$Peso), to = max(DadosMedicos$Peso), col = 'darkgreen', ylab = 'distribution', xlab = 'Peso', add = TRUE)
curve(Peso_exponencial, from = min(DadosMedicos$Peso), to = max(DadosMedicos$Peso), col = 'red', add = TRUE)
curve(Peso_gaussiana, from = min(DadosMedicos$Peso), to = max(DadosMedicos$Peso), col = 'blue', add = TRUE)
legend("topright", legend="Peso", lty = 1, col = 'black', lwd = 2, bty = "n")
legend("right", legend="exponencial", lty = 1, col = 'red', lwd = 2, bty = "n")
legend("left", legend="gaussiana", lty = 1, col = 'blue', lwd = 2, bty = "n")
legend("topleft", legend="lognormal", lty = 1, col = 'darkmagenta', lwd = 2, bty = "n")
legend("bottom", legend="weibull", lty = 1, col = 'darkgreen', lwd = 2, bty = 'n')

############ FIM PESO ##############

#CargaFinal#
#-Exponencial-#

#Estimativa via MLE
CargaFinal_exponencial_lambda = 1/media_CargaFinal
CargaFinal_exponencial <- function(x) {
  dexp(x, rate = CargaFinal_exponencial_lambda)
}

############
#-Gaussiana-#

#Estimativas via MLE
CargaFinal_gaussiana_media = media_CargaFinal
CargaFinal_gaussiana_sd2 = variancia_CargaFinal
CargaFinal_gaussiana <- function(x) {
  dnorm(x, mean = CargaFinal_gaussiana_media, sd = sqrt(CargaFinal_gaussiana_sd2))
}

############
#-LogNormal-#

#Estimativas via MLE
CargaFinal_lognormal_mu = mean(log(DadosMedicos$CargaFinal, base = exp(1)))
CargaFinal_lognormal_sd2 = var(log(DadosMedicos$CargaFinal, base = exp(1)))
CargaFinal_lognormal <- function(x) {
  dlnorm(x, meanlog = CargaFinal_lognormal_mu, sdlog = sqrt(CargaFinal_lognormal_sd2))
}

############
#-Weibull-#

#Rodar esta linha para obter os parametros shape e scale#
eweibull(DadosMedicos$CargaFinal, method = "mle")
CargaFinal_weibull_shape = 2.646977
CargaFinal_weibull_scale = 194.038877
CargaFinal_weibull <- function(x) {
  dweibull(x, CargaFinal_weibull_shape, scale = CargaFinal_weibull_scale)
}

#Criando grafico com todos juntos#
curve(CargaFinal_lognormal, from = min(DadosMedicos$CargaFinal), to = max(DadosMedicos$CargaFinal), col = 'darkmagenta', ylab = 'distribution', xlab = 'CargaFinal')
curve(CargaFinal_weibull, from = min(DadosMedicos$CargaFinal), to = max(DadosMedicos$CargaFinal), col = 'darkgreen', add = TRUE)
lines(CargaFinal_densidade, col = 'black')
curve(CargaFinal_exponencial, from = min(DadosMedicos$CargaFinal), to = max(DadosMedicos$CargaFinal), col = 'red', add = TRUE)
curve(CargaFinal_gaussiana, from = min(DadosMedicos$CargaFinal), to = max(DadosMedicos$CargaFinal), col = 'blue', add = TRUE)
legend("topright", legend="CargaFinal", lty = 1, col = 'black', lwd = 2, bty = "n")
legend("right", legend="exponencial", lty = 1, col = 'red', lwd = 2, bty = "n")
legend("left", legend="gaussiana", lty = 1, col = 'blue', lwd = 2, bty = "n")
legend("topleft", legend="lognormal", lty = 1, col = 'darkmagenta', lwd = 2, bty = "n")
legend("bottom", legend="weibull", lty = 1, col = 'darkgreen', lwd = 2, bty = 'n')

#VO2 Máximo#
#-Exponencial-#

#Estimativa via MLE
VO2Max_exponencial_lambda = 1/media_VO2Max
VO2Max_exponencial <- function(x) {
  dexp(x, rate = VO2Max_exponencial_lambda)
}

############
#-Gaussiana-#

#Estimativas via MLE
VO2Max_gaussiana_media = media_VO2Max
VO2Max_gaussiana_sd2 = variancia_VO2Max
VO2Max_gaussiana <- function(x) {
  dnorm(x, mean = VO2Max_gaussiana_media, sd = sqrt(VO2Max_gaussiana_sd2))
}

############
#-LogNormal-#

#Estimativas via MLE
VO2Max_lognormal_mu = mean(log(DadosMedicos$VO2MedidoMáximo, base = exp(1)))
VO2Max_lognormal_sd2 = var(log(DadosMedicos$VO2MedidoMáximo, base = exp(1)))
VO2Max_lognormal <- function(x) {
  dlnorm(x, meanlog = VO2Max_lognormal_mu, sd = sqrt(VO2Max_lognormal_sd2))
}

############
#-Weibull-#

#Rodar esta linha para obter os parametros shape e scale#
eweibull(DadosMedicos$VO2MedidoMáximo, method = "mle")
VO2Max_weibull_shape = 2.997817
VO2Max_weibull_scale = 32.927473
VO2Max_weibull <- function(x) {
  dweibull(x, VO2Max_weibull_shape, scale = VO2Max_weibull_scale)
}

#Criando grafico com todos juntos#
curve(VO2Max_lognormal, from = min(DadosMedicos$VO2MedidoMáximo), to = max(DadosMedicos$VO2MedidoMáximo), col = 'darkmagenta', ylab = 'distribution', xlab = 'VO2 Máximo')
curve(VO2Max_weibull, from = min(DadosMedicos$VO2MedidoMáximo), to = max(DadosMedicos$VO2MedidoMáximo), col = 'darkgreen', add = TRUE)
lines(VO2Max_densidade, col = 'black')
curve(VO2Max_exponencial, from = min(DadosMedicos$VO2MedidoMáximo), to = max(DadosMedicos$VO2MedidoMáximo), col = 'red', add = TRUE)
curve(VO2Max_gaussiana, from = min(DadosMedicos$VO2MedidoMáximo), to = max(DadosMedicos$VO2MedidoMáximo), col = 'blue', add = TRUE)
legend("topright", legend="CargaFinal", lty = 1, col = 'black', lwd = 2, bty = "n")
legend("right", legend="exponencial", lty = 1, col = 'red', lwd = 2, bty = "n")
legend("left", legend="gaussiana", lty = 1, col = 'blue', lwd = 2, bty = "n")
legend("topleft", legend="lognormal", lty = 1, col = 'darkmagenta', lwd = 2, bty = "n")
legend("bottom", legend="weibull", lty = 1, col = 'darkgreen', lwd = 2, bty = 'n')

#### QQPlot graphs ####

## IDADE ##

#exponencial#
qqPlot(DadosMedicos$IDADE, distribution = 'exp', param.list = list(rate = idade_exponencial_lambda), add.line = TRUE)

#gaussiana#
qqPlot(DadosMedicos$IDADE, distribution = 'norm', param.list = list(mean = idade_gaussiana_media, sd = sqrt(idade_gaussiana_sd2)), add.line = TRUE)

#lognormal#
qqPlot(DadosMedicos$IDADE, distribution = 'lnorm', param.list = list(meanlog = idade_lognormal_mu, sd = sqrt(idade_lognormal_sd2)), add.line = TRUE)

#weibull#
qqPlot(DadosMedicos$IDADE, distribution = 'weibull', param.list = list(shape = idade_weibull_shape, scale = idade_weibull_scale), add.line = TRUE)

## Peso ##

#exponencial#
qqPlot(DadosMedicos$Peso, distribution = 'exp', param.list = list(rate = Peso_exponencial_lambda), add.line = TRUE)

#gaussiana#
qqPlot(DadosMedicos$Peso, distribution = 'norm', param.list = list(mean = Peso_gaussiana_media, sd = sqrt(Peso_gaussiana_sd2)), add.line = TRUE)

#lognormal#
qqPlot(DadosMedicos$Peso, distribution = 'lnorm', param.list = list(meanlog = Peso_lognormal_mu, sd = sqrt(Peso_lognormal_sd2)), add.line = TRUE)

#weibull#
qqPlot(DadosMedicos$Peso, distribution = 'weibull', param.list = list(shape = Peso_weibull_shape, scale = Peso_weibull_scale), add.line = TRUE)

## CargaFinal ##

#exponencial#
qqPlot(DadosMedicos$CargaFinal, distribution = 'exp', param.list = list(rate = CargaFinal_exponencial_lambda), add.line = TRUE)

#gaussiana#
qqPlot(DadosMedicos$CargaFinal, distribution = 'norm', param.list = list(mean = CargaFinal_gaussiana_media, sd = sqrt(CargaFinal_gaussiana_sd2)), add.line = TRUE)

#lognormal#
qqPlot(DadosMedicos$CargaFinal, distribution = 'lnorm', param.list = list(meanlog = CargaFinal_lognormal_mu, sd = sqrt(CargaFinal_lognormal_sd2)), add.line = TRUE)

#weibull#
qqPlot(DadosMedicos$CargaFinal, distribution = 'weibull', param.list = list(shape = CargaFinal_weibull_shape, scale = CargaFinal_weibull_scale), add.line = TRUE)

## VO2 Máximo ##

#exponencial#
qqPlot(DadosMedicos$VO2MedidoMáximo, distribution = 'exp', param.list = list(rate = VO2Max_exponencial_lambda), add.line = TRUE)

#gaussiana#
qqPlot(DadosMedicos$VO2MedidoMáximo, distribution = 'norm', param.list = list(mean = VO2Max_gaussiana_media, sd = sqrt(VO2Max_gaussiana_sd2)), add.line = TRUE)

#lognormal#
qqPlot(DadosMedicos$VO2MedidoMáximo, distribution = 'lnorm', param.list = list(meanlog = VO2Max_lognormal_mu, sd = sqrt(VO2Max_lognormal_sd2)), add.line = TRUE)

#weibull#
qqPlot(DadosMedicos$VO2MedidoMáximo, distribution = 'weibull', param.list = list(shape = VO2Max_weibull_shape, scale = VO2Max_weibull_scale), add.line = TRUE)
