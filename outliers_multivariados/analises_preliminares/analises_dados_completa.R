## Análises de dados Vivian


#pacotes
library(lme4) #modelos mistos
library(bbmle) #AICtab
library(MVN) #normalidade e transformacao
library(lattice) #graficos
library(MuMIn)

#dados completa, usando média de SOL para a última campanha (apenas pra completar), vivian proverá novos dados para análises ao término das campanhas.
dc <- read.csv("dados_completa.csv", sep=';', header=T)

str(dc)
dc$paisagem<-as.factor(dc$paisagem)
dc$coleta<-as.factor(dc$coleta)
dc$amostra<-as.factor(dc$amostra)

#excluindo as variáveis TDS e TE, que são correlação = 1 com EC e ED, respectivamente

dc<-dc[,-8]
dc<-dc[,-22]


## analisando cada variável de água e a relação as variaveis de paisagem

## TURBIDEZ
uniPlot(dc$Turb)
range(dc$Turb)
uniPlot(log(dc$Turb))
uniNorm(log(dc$Turb))
summary(log(dc$Turb))
var(log(dc$Turb))
# mesmo não dando normal, ignoramos e seguimos fazendo LMM:

#dados turbidez
turb<-cbind(dc[,1:4],turb=dc[,12],dc[,19:23])

#modelos de treinamento/teste:
mt1 <- lmer(log(turb)~1 + (1|paisagem/coleta), data=turb)
summary(mt1)

mt2 <- lmer(log(turb)~1 + (coleta|paisagem), data=turb)
summary(mt2)

mt3 <- lmer(log(turb)~coleta + (coleta|paisagem), data=turb)
summary(mt3)

mt4 <- lmer(log(turb)~coleta + (1|paisagem/coleta), data=turb)
summary(mt4)

mt5 <- lmer(log(turb)~coleta+ NumP + (coleta|paisagem), data=turb)
summary(mt5)

AICtab(mt1,mt2,mt3,mt4,mt5,base=T,weights=T)

## montando a seleção de modelos
#todos possíveis com a funcao no MuMIn
mturb.glob<-lmer(log(turb)~1+NumP+MPS+ED+MPE+MSI + (coleta|paisagem), data=turb,na.action="na.fail")
mturb <- dredge(mturb.glob)
get.models(mturb, subset=T)[1]



## visualização da colinearidade entre as variaveis explanatorias, atraves do variance inflation factor (função dada no livro zuur et al. 2009)
corvif(turb[,6:10])
#segundo o livro, pag 387 (mas tb tem coisa no appendix A), um valor de corte deve ser 5 (ou até 3) para remover variaveis colineares.
#nesse caso tiraria uma das variaveis NumP ou ED, como NumP não é padronizada por área, escolhe-se ED.
