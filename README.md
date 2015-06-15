---
title: 'Estimación de Riesgo Crediticio usando Regresión Lineal'
author: 'Jorge Martínez Ortega'
date: '15 de junio 2015'
output:
  ioslides_presentation: default
  beamer_presentation:
    fig_height: 6
    fig_width: 8
    keep_tex: yes
#logo: r_logo.png
self_contained: no
fontsize: 9pt
---

## Introducción

- Un modelo de regresión lineal asume que el valor de espectación de $Y$, dado $X$, $E(Y |X)$; es lineal en las variables de entrada $X_1, \ldots , X_p$. 
- Los modelos lineales fueron ampliamente desarrollados previo a la estadística computacional, sin embargo en estos días, donde los métodos estadísticos computacionales dominan el área, aún existen buenas razones para seguirlos usando.
- Son simples y usualmete proveen una descripción con interpretación sencilla de cómo las variables de entrada afectan a las variables de salida.
- En algunas ocaciones, tienen mejor desempeño que los métodos no lineales más sofisticados, especialmete en situaciónes con pocos datos de entrenamiento, o con un cociente de señal a ruido pequeño, o con datos con poca separación. 

## ¿Qué es una regresión lineal?

Es un modelo de predición que depende linealmente de las variables de entrada, o independientes, se escribe en la forma:
$$
\hat{Y} = \hat{\beta_0} + \hat{\beta_1}X_1 + \hat{\beta_2}X_2 + \ldots,
$$
donde $X_i$ son las variables que caracterizan a cada uno de nuestros sujetos de crédito. Por ejmeplo: 

- edad, 
- salario, 
- labora actualmete, 
- tiene otros créditos, etc.

## Ejemplo de juguete
Tenemos una muestra de 500 personas que tuvieron acceso a un crédito automotriz  aun plazo de 10 años. Al finalizar los 10 años del plazo, así se vió la incidencia del impago: 
```{r chunk1, echo=FALSE, fig.height=4.4}
options(warn=-1)
suppressMessages(library('fpp'))
data(credit)
set.seed(12)
x1 <- runif(500, 0.0, 100.0)
scores<-credit$score
defaulted<-function(testing,to_pass) 0.5*to_pass-3>testing 

default<-mapply(defaulted,scores,x1)

credit$default<-default

defaulters<-credit[credit$default, ]
paid<-credit[!(credit$default),]

my_hist<-hist(paid$income, col=rgb(0,0,1,0.5),breaks=c(10*0:26,300),ylim=c(0,80),main="Impago con respecto a Ingreso", xlab="Ingreso Anual, en miles de pesos",ylab="Número de personas/10000 pesos",freq=TRUE)
my_hist<-hist(defaulters$income, col=rgb(1,0,0,0.7), breaks=c(10*0:26,300),freq=TRUE, add=T)
legend(200,70, c("pagados", "en impago"), fill = c(rgb(0,0,1,0.5), rgb(1,0,0,0.7)))
plot(my_hist,main="500 casos de credito, ejemplo simulado.")
```

## ¿Cómo se usa la regresión lineal en este caso?

```{r chunk2, echo=FALSE}
knitr::opts_chunk$set(cache=TRUE)
library('fpp')
data(credit)
set.seed(12)
x1 <- runif(500, 0.0, 100.0)

scores<-credit$score
defaulted<-function(testing,to_pass) 0.5*to_pass-3>testing 

default<-mapply(defaulted,scores,x1)

credit$default<-default

defaulters<-credit[credit$default, ]
defaulters<-defaulters[order(defaulters$income), ]
paid<-credit[!(credit$default),]
paid<-paid[order(paid$income),]

my_hist1<-hist(paid$income, col=rgb(0,0,1,0.5), ylim=c(0,200),breaks=c(40*0:4,200,300),plot = FALSE)
my_hist2<-hist(defaulters$income, col=rgb(1,0,0,0.5), breaks=c(40*0:4,200,300), plot = FALSE)

probability<-function(paid,notpaid) paid/(paid+notpaid)
errx<-function(c,b) (b-c) 
erry<-function(fp,fnp) sqrt(fp)/(fp+fnp)

probabilidad_de_pago<-mapply(probability,my_hist1$counts,my_hist2$counts)
prob<-mapply(probability,my_hist1$counts,my_hist2$counts)
ingreso_anual<-my_hist1$mids
xerr<-mapply(errx,my_hist1$mids,my_hist1$breaks[-1])
yerr<-mapply(erry,my_hist1$counts,my_hist2$counts)
plot(my_hist1$mids,prob,ylim=c(0.0,1.0),xlim=c(0,300),main="Probabilidad de pago con respecto al ingreso. (Datos de entrenamiento)",xlab="Ingreso Anual, en miles de pesos",ylab="Fraccion de créditos pagados")
segments(my_hist1$mids, prob-yerr,my_hist1$mids, prob+yerr)
segments(my_hist1$mids-xerr, prob,my_hist1$mids+xerr, prob)
```

## Datos
```{r two-column chunk0, echo=FALSE,}
knitr::opts_chunk$set(cache=TRUE)
options(warn=-1)
suppressMessages(library('fpp'))
data(credit)
set.seed(12)
x1 <- runif(500, 0.0, 100.0)
score<-credit$score
defaulted<-function(testing,to_pass) 0.5*to_pass-3>testing 
default<-mapply(defaulted,scores,x1)

credit$default<-default
income<-credit$income
row.names(credit) <- NULL 
credit[1:8, c("income","default")]
summary(credit[, c("income","default")])
```

## Creando el modelo
```{r two-column chunk3, cache=TRUE, echo=FALSE}
options(digits=4)
modelo_lineal<-lm(!default ~ income)
int<-as.numeric(modelo_lineal$coef['(Intercept)'])
inc<-modelo_lineal$coef['income']
knitr::opts_chunk$set(echo=TRUE)
modelo_lineal<-lm(!default ~ income)
modelo_lineal
```
Es decir, nuestro modelo queda cómo
$$
Y = `r int` + `r inc` X
$$
$Y:$ Probabilidad de pago completo, $X:$ ingreso anual/(1000 pesos)

## ¿Cómo se ve nuestro modelo comparado con los datos?
```{r chunk4,echo=FALSE}
modelo_lineal<- lm(!credit$default ~ credit$income)
plot(my_hist1$mids,prob,ylim=c(0.0,1.0),xlim=c(0,300),main="Probabilidad de pago con respecto al ingreso. (Datos de entrenamiento)",xlab="Ingreso Anual, en miles de pesos",ylab="Fraccion de créditos pagados")
segments(my_hist1$mids, prob-yerr,my_hist1$mids, prob+yerr)
segments(my_hist1$mids-xerr, prob,my_hist1$mids+xerr, prob)
abline(modelo_lineal,col=c(2),lwd=c(2.5))
legend(30,0.5, c("Proyección del modelo lineal"), lty=c(1), lwd=c(2.5),col=c(2))
```

## Poniendo a prueba el modelo
```{r two-column chunk5, echo=FALSE,}
knitr::opts_chunk$set(cache=TRUE)
options(warn=-1)
suppressMessages(library('fpp'))
data(credit)
credit2<-credit
set.seed(1)
x1 <- runif(500, 0.0, 100.0)
score<-credit2$score
set.seed(11)
random1<-runif(500, -10.0, 10.)
set.seed(19)
random2<-runif(500, -20.0, 20.)
defaulted<-function(testing,to_pass,rand) 0.5*to_pass-3>testing+rand
newincome<-function(old,rand) old+rand
default<-mapply(defaulted,scores,x1,random1)

credit2$default<-default
credit2$income<-mapply(newincome,credit2$income,random2)
row.names(credit2) <- NULL 

defaulters<-credit2[credit2$default, ]
defaulters<-defaulters[order(defaulters$income), ]
paid<-credit2[!(credit2$default),]
paid<-paid[order(paid$income),]

my_hist1<-hist(paid$income, col=rgb(0,0,1,0.5), ylim=c(0,200),breaks=c(40*0:4,200,300),plot = FALSE)
my_hist2<-hist(defaulters$income, col=rgb(1,0,0,0.5), breaks=c(40*0:4,200,300), plot = FALSE)

probability<-function(paid,notpaid) paid/(paid+notpaid)
errx<-function(c,b) (b-c) 
erry<-function(fp,fnp) sqrt(fp)/(fp+fnp)

probabilidad_de_pago<-mapply(probability,my_hist1$counts,my_hist2$counts)
prob<-mapply(probability,my_hist1$counts,my_hist2$counts)
ingreso_anual<-my_hist1$mids
xerr<-mapply(errx,my_hist1$mids,my_hist1$breaks[-1])
yerr<-mapply(erry,my_hist1$counts,my_hist2$counts)
plot(my_hist1$mids,prob,ylim=c(0.0,1.0),xlim=c(0,300),main="Probabilidad de pago con respecto al ingreso. (Datos de Prueba)",xlab="Ingreso Anual, en miles de pesos",ylab="Fraccion de créditos pagados")
segments(my_hist1$mids, prob-yerr,my_hist1$mids, prob+yerr)
segments(my_hist1$mids-xerr, prob,my_hist1$mids+xerr, prob)
abline(modelo_lineal,col=c(2),lwd=c(2.5))
legend(30,0.5, c("Proyección del modelo lineal"), lty=c(1), lwd=c(2.5),col=c(2))
```


## ¿Que tan bueno para predecir es nuetro modelo?
```{r two-column chunk6, echo=FALSE}
predict<-function(value) (124/500)*(0.5-(value-0.5*value*value))/(0.5-(int-0.5*int*int))
testing<-function(value) length(credit2$default[((int+inc*credit2$income)>value & default)])/500
x<-int+0.017*0:20
y_p<-mapply(predict,x)
y_t<-mapply(testing,x)
plot(x,y_p,type="l",col="red",xlab="Valor de Y",ylab="Fracción de impago",lwd=c(2.5))
lines(x,y_t,type="l",col="blue",lwd=c(2.5))
legend(0.8,0.25, c("Predicción del Modelo","Datos de prueba"), lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue"))
```

## ¿Cómo depende nuestra tasa de impago respecto al rechazo?
```{r two-column chunk7, echo=FALSE}
predict<-function(value) (124/500)*(0.5-(value-0.5*value*value))/(0.5-(int-0.5*int*int))
testing<-function(value) length(credit2$default[((int+inc*credit2$income)>value & default)])/500
cut_p<-function(value) 1-length(credit$income[(int+inc*credit$income)>value])/500
cut_t<-function(value) 1-length(credit2$income[(int+inc*credit2$income)>value])/500
x<-int+0.017*0:20
x_p<-mapply(cut_p,x)
y_p<-mapply(predict,x)
x_t<-mapply(cut_t,x)
y_t<-mapply(testing,x)
plot(x_p,y_p,type="l",col="red",xlab="Fraccion de rechazados",ylab="Fracción de impago",lwd=c(2.5))
lines(x_p,y_t,type="l",col="blue",lwd=c(2.5))
legend(0.5,0.25, c("Datos de entrenamiento","Datos de prueba"), lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue"))
```

## Conclusiones
- El metodo de regresión lineal sigue siendo buena opción en muchos casos.
- Es un método sencillo de aplicar.
- Es fácil interpretar los resultados.

## Referencias y código

- [http://statweb.stanford.edu/~tibs/ElemStatLearn/](http://statweb.stanford.edu/~tibs/ElemStatLearn/)
- [http://www-bcf.usc.edu/~gareth/ISL/](http://www-bcf.usc.edu/~gareth/ISL/)
- [https://stat.ethz.ch/R-manual/R-patched/library/stats/html/lm.html](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/lm.html)
- [https://en.wikipedia.org/wiki/Linear_regression](https://en.wikipedia.org/wiki/Linear_regression)
- [https://github.com/iorch/clase_regresion_lineal](https://github.com/iorch/clase_regresion_lineal)

