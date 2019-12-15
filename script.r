### Script para Projeto da Matéria COE241 ###

## Importando os Dados ##
DadosMedicos <-
  read.table(
    "Dados-medicos.csv",
    quote = "\"",
    comment.char = "",
    header = TRUE
  )


#### Histograma e Distribuição Empírica das Variáveis ####
bin = as.integer(1+3.3*log10(nrow(DadosMedicos)))

#IDADE
png("Histogramas/histograma_idade.png")
hist(DadosMedicos$IDADE,
     breaks = seq(min(DadosMedicos$IDADE), max(DadosMedicos$IDADE), length.out = bin+1),
     main = "Histograma de IDADE",
     col = "darkmagenta",
     xlab = "Idade (em anos)")
dev.off()

png("Densidades/densidade_idade.png")
idade_densidade = density(DadosMedicos$IDADE)
plot(idade_densidade, main = "PDF - IDADE")
dev.off()

png("CDFEmpirica/distempirica_idade.png")
plot(ecdf(DadosMedicos$IDADE), main = 'CDF Empirica - IDADE')
dev.off()

#PESO#
png("Histogramas/histograma_Peso.png")
hist(DadosMedicos$Peso,
     breaks = seq(min(DadosMedicos$Peso), max(DadosMedicos$Peso), length.out = bin+1),
     main = "Histograma de Peso",
     col = "darkmagenta",
     xlab = "Peso (em kg)")
dev.off()

png("Densidades/densidade_Peso.png")
peso_densidade = density(DadosMedicos$Peso)
plot(peso_densidade, main = "PDF - PESO")
dev.off()

png("CDFEmpirica/distempirica_Peso.png")
plot(ecdf(DadosMedicos$Peso), main = 'CDF Empirica - Peso')
dev.off()

#CargaFinal#
png("Histogramas/histograma_CargaFinal.png")
hist(DadosMedicos$CargaFinal,
     breaks = seq(min(DadosMedicos$CargaFinal), max(DadosMedicos$CargaFinal), length.out = bin+1),
     main = "Histograma de CargaFinal",
     col = "darkmagenta",
     xlab = "Carga Final")
dev.off()

png("Densidades/densidade_CargaFinal.png")
CargaFinal_densidade = density(DadosMedicos$CargaFinal)
plot(CargaFinal_densidade, main = "PDF - Carga Final")
dev.off()

png("CDFEmpirica/distempirica_CargaFinal.png")
plot(ecdf(DadosMedicos$CargaFinal), main = 'CDF Empirica - CargaFinal')
dev.off()

#VO2 Medido Maximo#
png("Histogramas/histograma_VO2Max.png")
hist(DadosMedicos$VO2MedidoMáximo,
     breaks = seq(min(DadosMedicos$VO2MedidoMáximo), max(DadosMedicos$VO2MedidoMáximo), length.out = bin+1),
     main = "Histograma de VO2 Máximo",
     col = "darkmagenta",
     xlab = "VO2 Máximo (em mL/kg/min)")
dev.off()

png("Densidades/densidade_VO2Max.png")
VO2Max_densidade = density(DadosMedicos$VO2MedidoMáximo)
plot(VO2Max_densidade, main = "PDF - VO2 Máximo")
dev.off()

png("CDFEmpirica/distempirica_VO2Max.png")
plot(ecdf(DadosMedicos$VO2MedidoMáximo), main = 'CDF Empirica - VO2Max')
dev.off()

#### Média, Variância e Boxplot das Variáveis ####

#IDADE#
media_idade = mean(DadosMedicos$IDADE)
variancia_idade = var(DadosMedicos$IDADE)
png("BoxPlots/boxplot_idade.png")
boxplot(DadosMedicos$IDADE, main = "Boxplot de Idade")
dev.off()

#PESO#
media_Peso = mean(DadosMedicos$Peso)
variancia_Peso = var(DadosMedicos$Peso)
png("BoxPlots/boxplot_peso.png")
boxplot(DadosMedicos$Peso, main = "Boxplot de Peso")
dev.off()

#CargaFinal#
media_CargaFinal = mean(DadosMedicos$CargaFinal)
variancia_CargaFinal = var(DadosMedicos$CargaFinal)
png("BoxPlots/boxplot_CargaFinal.png")
boxplot(DadosMedicos$CargaFinal, main = "Boxplot de CargaFinal")
dev.off()

#VO2 Medido Máximo#
media_VO2Max = mean(DadosMedicos$VO2MedidoMáximo)
variancia_VO2Max = var(DadosMedicos$VO2MedidoMáximo)
png("BoxPlots/boxplot_VO2Max.png")
boxplot(DadosMedicos$VO2MedidoMáximo, main = "Boxplot de VO2 Máx")
dev.off()

#### Estimando Parametros via MLE ####
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

#-Gaussiana-#

#Estimativas via MLE
idade_gaussiana_media = media_idade
idade_gaussiana_sd2 = variancia_idade
idade_gaussiana <- function(x) {
  dnorm(x, mean = idade_gaussiana_media, sd = sqrt(idade_gaussiana_sd2))
}

#-LogNormal-#

#Estimativas via MLE
idade_lognormal_mu = mean(log(DadosMedicos$IDADE, base = exp(1)))
idade_lognormal_sd2 = var(log(DadosMedicos$IDADE, base = exp(1)))
idade_lognormal <- function(x) {
  dlnorm(x, meanlog = idade_lognormal_mu, sdlog = sqrt(idade_lognormal_sd2))
}

#-Weibull-#

#Rodar esta linha para obter os parametros shape e scale#
eweibull(DadosMedicos$IDADE, method = "mle")
idade_weibull_shape = 4.089473
idade_weibull_scale = 58.782895
idade_weibull <- function(x) {
  dweibull(x, idade_weibull_shape, scale = idade_weibull_scale)
}

#criando grafico com todos juntos#
png("GraficosDistribuicoes/distribuicoes_idade.png")
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
dev.off()

#Peso#
#-Exponencial-#

#Estimativa via MLE
Peso_exponencial_lambda = 1/media_Peso
Peso_exponencial <- function(x) {
  dexp(x, rate = Peso_exponencial_lambda)
}

#-Gaussiana-#

#Estimativas via MLE
Peso_gaussiana_media = media_Peso
Peso_gaussiana_sd2 = variancia_Peso
Peso_gaussiana <- function(x) {
  dnorm(x, mean = Peso_gaussiana_media, sd = sqrt(Peso_gaussiana_sd2))
}

#-LogNormal-#

#Estimativas via MLE
Peso_lognormal_mu = mean(log(DadosMedicos$Peso, base = exp(1)))
Peso_lognormal_sd2 = var(log(DadosMedicos$Peso, base = exp(1)))
Peso_lognormal <- function(x) {
  dlnorm(x, meanlog = Peso_lognormal_mu, sdlog = sqrt(Peso_lognormal_sd2))
}

#-Weibull-#

#Rodar esta linha para obter os parametros shape e scale#
eweibull(DadosMedicos$Peso, method = "mle")
Peso_weibull_shape = 5.40801
Peso_weibull_scale = 92.24080
Peso_weibull <- function(x) {
  dweibull(x, Peso_weibull_shape, scale = Peso_weibull_scale)
}

#criando grafico com todos juntos#
png("GraficosDistribuicoes/distribuicoes_peso.png")
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
dev.off()

#CargaFinal#
#-Exponencial-#

#Estimativa via MLE
CargaFinal_exponencial_lambda = 1/media_CargaFinal
CargaFinal_exponencial <- function(x) {
  dexp(x, rate = CargaFinal_exponencial_lambda)
}

#-Gaussiana-#

#Estimativas via MLE
CargaFinal_gaussiana_media = media_CargaFinal
CargaFinal_gaussiana_sd2 = variancia_CargaFinal
CargaFinal_gaussiana <- function(x) {
  dnorm(x, mean = CargaFinal_gaussiana_media, sd = sqrt(CargaFinal_gaussiana_sd2))
}

#-LogNormal-#

#Estimativas via MLE
CargaFinal_lognormal_mu = mean(log(DadosMedicos$CargaFinal, base = exp(1)))
CargaFinal_lognormal_sd2 = var(log(DadosMedicos$CargaFinal, base = exp(1)))
CargaFinal_lognormal <- function(x) {
  dlnorm(x, meanlog = CargaFinal_lognormal_mu, sdlog = sqrt(CargaFinal_lognormal_sd2))
}

#-Weibull-#

#Rodar esta linha para obter os parametros shape e scale#
eweibull(DadosMedicos$CargaFinal, method = "mle")
CargaFinal_weibull_shape = 2.646977
CargaFinal_weibull_scale = 194.038877
CargaFinal_weibull <- function(x) {
  dweibull(x, CargaFinal_weibull_shape, scale = CargaFinal_weibull_scale)
}

#Criando grafico com todos juntos#
png("GraficosDistribuicoes/distribuicoes_CargaFinal.png")
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
dev.off()

#VO2 Máximo#
#-Exponencial-#

#Estimativa via MLE
VO2Max_exponencial_lambda = 1/media_VO2Max
VO2Max_exponencial <- function(x) {
  dexp(x, rate = VO2Max_exponencial_lambda)
}

#-Gaussiana-#

#Estimativas via MLE
VO2Max_gaussiana_media = media_VO2Max
VO2Max_gaussiana_sd2 = variancia_VO2Max
VO2Max_gaussiana <- function(x) {
  dnorm(x, mean = VO2Max_gaussiana_media, sd = sqrt(VO2Max_gaussiana_sd2))
}

#-LogNormal-#

#Estimativas via MLE
VO2Max_lognormal_mu = mean(log(DadosMedicos$VO2MedidoMáximo, base = exp(1)))
VO2Max_lognormal_sd2 = var(log(DadosMedicos$VO2MedidoMáximo, base = exp(1)))
VO2Max_lognormal <- function(x) {
  dlnorm(x, meanlog = VO2Max_lognormal_mu, sd = sqrt(VO2Max_lognormal_sd2))
}

#-Weibull-#

#Rodar esta linha para obter os parametros shape e scale#
eweibull(DadosMedicos$VO2MedidoMáximo, method = "mle")
VO2Max_weibull_shape = 2.997817
VO2Max_weibull_scale = 32.927473
VO2Max_weibull <- function(x) {
  dweibull(x, VO2Max_weibull_shape, scale = VO2Max_weibull_scale)
}

#Criando grafico com todos juntos#
png("GraficosDistribuicoes/distribuicoes_VO2Max.png")
curve(VO2Max_lognormal, from = min(DadosMedicos$VO2MedidoMáximo), to = max(DadosMedicos$VO2MedidoMáximo), col = 'darkmagenta', ylab = 'distribution', xlab = 'VO2 Máximo')
curve(VO2Max_weibull, from = min(DadosMedicos$VO2MedidoMáximo), to = max(DadosMedicos$VO2MedidoMáximo), col = 'darkgreen', add = TRUE)
lines(VO2Max_densidade, col = 'black')
curve(VO2Max_exponencial, from = min(DadosMedicos$VO2MedidoMáximo), to = max(DadosMedicos$VO2MedidoMáximo), col = 'red', add = TRUE)
curve(VO2Max_gaussiana, from = min(DadosMedicos$VO2MedidoMáximo), to = max(DadosMedicos$VO2MedidoMáximo), col = 'blue', add = TRUE)
legend("topright", legend="VO2Maximo", lty = 1, col = 'black', lwd = 2, bty = "n")
legend("right", legend="exponencial", lty = 1, col = 'red', lwd = 2, bty = "n")
legend("left", legend="gaussiana", lty = 1, col = 'blue', lwd = 2, bty = "n")
legend("topleft", legend="lognormal", lty = 1, col = 'darkmagenta', lwd = 2, bty = "n")
legend("bottom", legend="weibull", lty = 1, col = 'darkgreen', lwd = 2, bty = 'n')
dev.off()

#### QQPlot graphs ####

## IDADE ##

#exponencial#
png("QQPlots/qqplot_idade_exponencial.png")
qqPlot(DadosMedicos$IDADE, distribution = 'exp', param.list = list(rate = idade_exponencial_lambda), add.line = TRUE)
dev.off()

#gaussiana#
png("QQPlots/qqplot_idade_gaussiana.png")
qqPlot(DadosMedicos$IDADE, distribution = 'norm', param.list = list(mean = idade_gaussiana_media, sd = sqrt(idade_gaussiana_sd2)), add.line = TRUE)
dev.off()

#lognormal#
png("QQPlots/qqplot_idade_lognormal.png")
qqPlot(DadosMedicos$IDADE, distribution = 'lnorm', param.list = list(meanlog = idade_lognormal_mu, sd = sqrt(idade_lognormal_sd2)), add.line = TRUE)
dev.off()

#weibull#
png("QQPlots/qqplot_idade_weibull.png")
qqPlot(DadosMedicos$IDADE, distribution = 'weibull', param.list = list(shape = idade_weibull_shape, scale = idade_weibull_scale), add.line = TRUE)
dev.off()

## Peso ##

#exponencial#
png("QQPlots/qqplot_peso_exponencial.png")
qqPlot(DadosMedicos$Peso, distribution = 'exp', param.list = list(rate = Peso_exponencial_lambda), add.line = TRUE)
dev.off()

#gaussiana#
png("QQPlots/qqplot_peso_gaussiana.png")
qqPlot(DadosMedicos$Peso, distribution = 'norm', param.list = list(mean = Peso_gaussiana_media, sd = sqrt(Peso_gaussiana_sd2)), add.line = TRUE)
dev.off()

#lognormal#
png("QQPlots/qqplot_peso_lognormal.png")
qqPlot(DadosMedicos$Peso, distribution = 'lnorm', param.list = list(meanlog = Peso_lognormal_mu, sd = sqrt(Peso_lognormal_sd2)), add.line = TRUE)
dev.off()

#weibull#
png("QQPlots/qqplot_peso_weibull.png")
qqPlot(DadosMedicos$Peso, distribution = 'weibull', param.list = list(shape = Peso_weibull_shape, scale = Peso_weibull_scale), add.line = TRUE)
dev.off()

## CargaFinal ##

#exponencial#
png("QQPlots/qqplot_CargaFinal_exponencial.png")
qqPlot(DadosMedicos$CargaFinal, distribution = 'exp', param.list = list(rate = CargaFinal_exponencial_lambda), add.line = TRUE)
dev.off()

#gaussiana#
png("QQPlots/qqplot_CargaFinal_gaussiana.png")
qqPlot(DadosMedicos$CargaFinal, distribution = 'norm', param.list = list(mean = CargaFinal_gaussiana_media, sd = sqrt(CargaFinal_gaussiana_sd2)), add.line = TRUE)
dev.off()

#lognormal#
png("QQPlots/qqplot_CargaFinal_lognormal.png")
qqPlot(DadosMedicos$CargaFinal, distribution = 'lnorm', param.list = list(meanlog = CargaFinal_lognormal_mu, sd = sqrt(CargaFinal_lognormal_sd2)), add.line = TRUE)
dev.off()

#weibull#
png("QQPlots/qqplot_CargaFinal_weibull.png")
qqPlot(DadosMedicos$CargaFinal, distribution = 'weibull', param.list = list(shape = CargaFinal_weibull_shape, scale = CargaFinal_weibull_scale), add.line = TRUE)
dev.off()

## VO2 Máximo ##

#exponencial#
png("QQPlots/qqplot_VO2Max_exponencial.png")
qqPlot(DadosMedicos$VO2MedidoMáximo, distribution = 'exp', param.list = list(rate = VO2Max_exponencial_lambda), add.line = TRUE)
dev.off()

#gaussiana#
png("QQPlots/qqplot_VO2Max_gaussiana.png")
qqPlot(DadosMedicos$VO2MedidoMáximo, distribution = 'norm', param.list = list(mean = VO2Max_gaussiana_media, sd = sqrt(VO2Max_gaussiana_sd2)), add.line = TRUE)
dev.off()

#lognormal#
png("QQPlots/qqplot_VO2Max_lognormal.png")
qqPlot(DadosMedicos$VO2MedidoMáximo, distribution = 'lnorm', param.list = list(meanlog = VO2Max_lognormal_mu, sd = sqrt(VO2Max_lognormal_sd2)), add.line = TRUE)
dev.off()

#weibull#
png("QQPlots/qqplot_VO2Max_weibull.png")
qqPlot(DadosMedicos$VO2MedidoMáximo, distribution = 'weibull', param.list = list(shape = VO2Max_weibull_shape, scale = VO2Max_weibull_scale), add.line = TRUE)
dev.off()

#### TESTE DE HIPÓTESE ####
### KOMOLGOROV-SMIRNOV ###

## IDADE ##

#exponencial#
print ('Teste de hipótese - IDADE X Exponencial')
ks.test(DadosMedicos$IDADE, 'pexp', rate = idade_exponencial_lambda)

#gaussiana#
print ('Teste de hipótese - IDADE X Gaussiana')
ks.test(DadosMedicos$IDADE, 'pnorm', mean = idade_gaussiana_media, sd = sqrt(idade_gaussiana_sd2))

#lognormal#
print ('Teste de hipótese - IDADE X Lognormal')
ks.test(DadosMedicos$IDADE, 'plnorm', meanlog = idade_lognormal_mu, sd = sqrt(idade_lognormal_sd2))

#weibull#
print ('Teste de hipótese - IDADE X Weibull')
ks.test(DadosMedicos$IDADE, 'pweibull', shape = idade_weibull_shape, scale = idade_weibull_scale)

## Peso ##

#exponencial#
print ('Teste de hipótese - Peso X Exponencial')
ks.test(DadosMedicos$Peso, 'pexp', rate = Peso_exponencial_lambda)

#gaussiana#
print ('Teste de hipótese - Peso X Gaussiana')
ks.test(DadosMedicos$Peso, 'pnorm', mean = Peso_gaussiana_media, sd = sqrt(Peso_gaussiana_sd2))

#lognormal#
print ('Teste de hipótese - Peso X Lognormal')
ks.test(DadosMedicos$Peso, 'plnorm', meanlog = Peso_lognormal_mu, sd = sqrt(Peso_lognormal_sd2))

#weibull#
print ('Teste de hipótese - Peso X Weibull')
ks.test(DadosMedicos$Peso, 'pweibull', shape = Peso_weibull_shape, scale = Peso_weibull_scale)

## CargaFinal ##

#exponencial#
print ('Teste de hipótese - CargaFinal X Exponencial')
ks.test(DadosMedicos$CargaFinal, 'pexp', rate = CargaFinal_exponencial_lambda)

#gaussiana#
print ('Teste de hipótese - CargaFinal X Gaussiana')
ks.test(DadosMedicos$CargaFinal, 'pnorm', mean = CargaFinal_gaussiana_media, sd = sqrt(CargaFinal_gaussiana_sd2))

#lognormal#
print ('Teste de hipótese - CargaFinal X Lognormal')
ks.test(DadosMedicos$CargaFinal, 'plnorm', meanlog = CargaFinal_lognormal_mu, sd = sqrt(CargaFinal_lognormal_sd2))

#weibull#
print ('Teste de hipótese - CargaFinal X Weibull')
ks.test(DadosMedicos$CargaFinal, 'pweibull', shape = CargaFinal_weibull_shape, scale = CargaFinal_weibull_scale)

## VO2 Máximo ##

#exponencial#
print ('Teste de hipótese - VO2 Máximo X Exponencial')
ks.test(DadosMedicos$VO2MedidoMáximo, 'pexp', rate = VO2Max_exponencial_lambda)

#gaussiana#
print ('Teste de hipótese - VO2 Máximo X Gaussiana')
ks.test(DadosMedicos$VO2MedidoMáximo, 'pnorm', mean = VO2Max_gaussiana_media, sd = sqrt(VO2Max_gaussiana_sd2))

#lognormal#
print ('Teste de hipótese - VO2 Máximo X Lognormal')
ks.test(DadosMedicos$VO2MedidoMáximo, 'plnorm', meanlog = VO2Max_lognormal_mu, sd = sqrt(VO2Max_lognormal_sd2))

#weibull#
print ('Teste de hipótese - VO2 Máximo X Weibull')
ks.test(DadosMedicos$VO2MedidoMáximo, 'pweibull', shape = VO2Max_weibull_shape, scale = VO2Max_weibull_scale)

#### Coeficiente de Correlação Amostral e ScatterPlot ####

## Idade e VO2Máximo ##
correlacao_idade_VO2Max = cor(DadosMedicos$IDADE, DadosMedicos$VO2MedidoMáximo)
png('ScatterPlots/scatterplot_idade_VO2Max.png')
plot(DadosMedicos$IDADE, DadosMedicos$VO2MedidoMáximo,
     xlab = 'Idade', ylab = 'VO2 Máximo')
modelo_regressao_idade <- lm(DadosMedicos$VO2MedidoMáximo~DadosMedicos$IDADE)
abline(modelo_regressao_idade, col = 'red')
dev.off()

coeficientes_regressao_idade = modelo_regressao_idade$coefficients

## Peso e VO2Máximo ##
correlacao_Peso_VO2Max = cor(DadosMedicos$Peso, DadosMedicos$VO2MedidoMáximo)
png('ScatterPlots/scatterplot_Peso_VO2Max.png')
plot(DadosMedicos$Peso, DadosMedicos$VO2MedidoMáximo,
     xlab = 'Peso', ylab = 'VO2 Máximo')
modelo_regressao_peso <- lm(DadosMedicos$VO2MedidoMáximo~DadosMedicos$Peso)
abline(modelo_regressao_peso, col = 'red')
dev.off()

coeficientes_regressao_peso = modelo_regressao_peso$coefficients

## CargaFinal e VO2Máximo ##
correlacao_CargaFinal_VO2Max = cor(DadosMedicos$CargaFinal, DadosMedicos$VO2MedidoMáximo)
png('ScatterPlots/scatterplot_CargaFinal_VO2Max.png')
plot(DadosMedicos$CargaFinal, DadosMedicos$VO2MedidoMáximo,
     xlab = 'CargaFinal', ylab = 'VO2 Máximo')
modelo_regressao_CargaFinal <- lm(DadosMedicos$VO2MedidoMáximo~DadosMedicos$CargaFinal)
abline(modelo_regressao_CargaFinal, col = 'red')
dev.off()

coeficientes_regressao_CargaFinal = modelo_regressao_CargaFinal$coefficients

#### Bayesian Inference #####
## Variavel escolhida = Carga Final ##
# intervalos: 30.0 110.4 190.8 271.2 351.6 432.0 #

hipoteses = c('H(30.0 - 110.4)', 'H(110.4 - 190.8)', 'H(190.8 - 271.2)', 'H(271.2 - 351.6)', 'H(351.6 432.0)')
#Abaixo de 35#

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 30.000 & DadosMedicos$CargaFinal < 110.4])
com_vo2_menor = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 30.000 & DadosMedicos$CargaFinal < 110.4 & DadosMedicos$VO2MedidoMáximo < 35.0])
under35_hipotese_1 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_menor/dentro_do_intervalo,
                       com_vo2_menor/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 110.4 & DadosMedicos$CargaFinal < 190.8])
com_vo2_menor = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 110.4 & DadosMedicos$CargaFinal < 190.8 & DadosMedicos$VO2MedidoMáximo < 35.0])
under35_hipotese_2 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_menor/dentro_do_intervalo,
                       com_vo2_menor/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 190.8 & DadosMedicos$CargaFinal < 271.2])
com_vo2_menor = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 190.8 & DadosMedicos$CargaFinal < 271.2 & DadosMedicos$VO2MedidoMáximo < 35.0])
under35_hipotese_3 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_menor/dentro_do_intervalo,
                       com_vo2_menor/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 271.2 & DadosMedicos$CargaFinal < 351.6])
com_vo2_menor = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 271.2 & DadosMedicos$CargaFinal < 351.6 & DadosMedicos$VO2MedidoMáximo < 35.0])
under35_hipotese_4 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_menor/dentro_do_intervalo,
                       com_vo2_menor/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 351.6 & DadosMedicos$CargaFinal <= 432.0])
com_vo2_menor = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 351.6 & DadosMedicos$CargaFinal <= 432.0 & DadosMedicos$VO2MedidoMáximo < 35.0])
under35_hipotese_5 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_menor/dentro_do_intervalo,
                       com_vo2_menor/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

VO2Max_under35_table = matrix(
  c(under35_hipotese_1, under35_hipotese_2, under35_hipotese_3, under35_hipotese_4, under35_hipotese_5),
  nrow = 5, ncol = 4,
  byrow = TRUE, dimnames = list(
    hipoteses,
    c('Prior', 'Likelihood', 'BayesNum', 'Posterior'))
)

prob_under35 = sum(VO2Max_under35_table[11:15])

for (value in c(16:20)) {
  VO2Max_under35_table[value] = VO2Max_under35_table[value - 5]/prob_under35
}

print(VO2Max_under35_table)
print(prob_under35)

#Igual ou acima de 35#

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 30.000 & DadosMedicos$CargaFinal < 110.4])
com_vo2_maior = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 30.000 & DadosMedicos$CargaFinal < 110.4 & DadosMedicos$VO2MedidoMáximo >= 35.0])
above35_hipotese_1 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_maior/dentro_do_intervalo,
                       com_vo2_maior/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 110.4 & DadosMedicos$CargaFinal < 190.8])
com_vo2_maior = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 110.4 & DadosMedicos$CargaFinal < 190.8 & DadosMedicos$VO2MedidoMáximo >= 35.0])
above35_hipotese_2 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_maior/dentro_do_intervalo,
                       com_vo2_maior/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 190.8 & DadosMedicos$CargaFinal < 271.2])
com_vo2_maior = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 190.8 & DadosMedicos$CargaFinal < 271.2 & DadosMedicos$VO2MedidoMáximo >= 35.0])
above35_hipotese_3 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_maior/dentro_do_intervalo,
                       com_vo2_maior/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 271.2 & DadosMedicos$CargaFinal < 351.6])
com_vo2_maior = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 271.2 & DadosMedicos$CargaFinal < 351.6 & DadosMedicos$VO2MedidoMáximo >= 35.0])
above35_hipotese_4 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_maior/dentro_do_intervalo,
                       com_vo2_maior/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

dentro_do_intervalo = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 351.6 & DadosMedicos$CargaFinal <= 432.0])
com_vo2_maior = length(DadosMedicos$CargaFinal[DadosMedicos$CargaFinal >= 351.6 & DadosMedicos$CargaFinal <= 432.0 & DadosMedicos$VO2MedidoMáximo >= 35.0])
above35_hipotese_5 = c(dentro_do_intervalo/nrow(DadosMedicos),
                       com_vo2_maior/dentro_do_intervalo,
                       com_vo2_maior/dentro_do_intervalo*dentro_do_intervalo/nrow(DadosMedicos),
                       0)

VO2Max_above35_table = matrix(
  c(above35_hipotese_1, above35_hipotese_2, above35_hipotese_3, above35_hipotese_4, above35_hipotese_5),
  nrow = 5, ncol = 4,
  byrow = TRUE, dimnames = list(
    hipoteses,
    c('Prior', 'Likelihood', 'BayesNum', 'Posterior'))
)

prob_above35 = sum(VO2Max_above35_table[11:15])

for (value in c(16:20)) {
  VO2Max_above35_table[value] = VO2Max_above35_table[value - 5]/prob_above35
}

print(VO2Max_above35_table)
print(prob_above35)

## Prediction ##

priors = VO2Max_under35_table[1:5]
likelihood1 = VO2Max_under35_table[6:10]
BayesNum1 = VO2Max_under35_table[11:15]
posterior1 = VO2Max_under35_table[16:20]
Likelihood2 = VO2Max_above35_table[6:10]
predictions = posterior1 * Likelihood2
prob_to_get_better = sum(predictions)

VO2Max_prediction_table = matrix(
  c(priors, likelihood1, BayesNum1, posterior1, Likelihood2, predictions),
  nrow = 5, ncol = 6,
  dimnames = list(
    hipoteses,
    c('Prior 1', 'Likelihood1', 'BayesNum1', 'Posterior1', 'Likelihood2', 'Predict')
  )
)

print (VO2Max_prediction_table)
print(prob_to_get_better)
