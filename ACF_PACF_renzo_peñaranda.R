rm(list=ls())
setwd("C:/Users/Usuario/Desktop/Clases_de_R/Prácticas/Trabajo_02_ACF_PACF/")

# correlograma
#### Modelos AR(2), phi[1]>0 & phi[2]>0 ####

set.seed(3.1416)
AR2_1 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.1)), n=211, sd=0.157)
plot.ts(AR2_1, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=0.1 y phi[2]=0.1)")
acf(AR2_1, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=0.1 y phi[2]=0.1)")
pacf(AR2_1, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=0.1 y phi[2]=0.1)")
# La serie de tiempo presenta cambios bruscos y valores alrededor de cero. 
# En la ACF solo el primer desfase es significativo. Muestra tramos en positivo y negativo.
# Ninguna autocorrelación parcial es significativa.

set.seed(3.1416)
AR2_2 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.2,0.3)), n=211, sd=0.157)
plot.ts(AR2_2, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=0.2 y phi[2]=0.3)")
acf(AR2_2, main="Función de Autocorrelación Simple",
    sub="(phi[1]=0.2 y phi[2]=0.3)")
pacf(AR2_2, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=0.2 y phi[2]=0.3)")
# En la ACF, los primeros desfaces muestra niveles de autocorrelación positivos y 
# significativos, después del octavo desface la autocorrelación se hace negativa y no significativa.
# La PACF señala que el la autocorrelación parcial en el segundo desface es mayor que todas las 
# demás.

set.seed(3.1416)
AR2_3 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.5,0.4)), n=211, sd=0.157)
plot.ts(AR2_3, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=0.5 y phi[2]=0.4)")
acf(AR2_3, main="Función de Autocorrelación Simple (phi[1]=0.5 y phi[2]=0.4)")
pacf(AR2_3, main = "Función de Autocorrelación Parcial (phi[1]=0.5 y phi[2]=0.4)")
# Se puede observar una tendencia hacia abajo en el primer segemento de la serie de tiempo;
# luego, las siguientes fracciones se alternan en movimientos de subidas y bajadas. 
# Una gran porción del ACF es positivo y significativo. 

#### Modelos AR(2), phi[1]>0 & phi[2]<0 ####

set.seed(2.7183)
AR2_4 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.13,-0.21)), n=223, sd=0.163)
plot.ts(AR2_4, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=0.13 y phi[2]=-0.21)")
acf(AR2_4, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=0.13 y phi[2]=-0.21)")
pacf(AR2_4, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=0.13 y phi[2]=-0.21)")
# ACF con dos valores significativos, uno positivo y otro negativo. 
# El segundo desface, en la PACF, muestra una autocorrelación negativa y significativa.

set.seed(2.7183)
AR2_5 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.34,-0.55)), n=223, sd=0.163)
plot.ts(AR2_5, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=0.34 y phi[2]=-0.55)")
acf(AR2_5, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=0.34 y phi[2]=-0.55)")
pacf(AR2_5, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=0.34 y phi[2]=-0.55)")
# En su mayoria, los desfaces oscilan en pares positivos y negativos en el ACF.
# Autocorrelación grande y negativa en el segundo desface de la PACF.

set.seed(2.7183)
AR2_6 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.445,-0.144)), n=223, sd=0.163)
plot.ts(AR2_6, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=0.445 y phi[2]=-0.144)")
acf(AR2_6, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=0.445 y phi[2]=-0.114)")
pacf(AR2_6, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=0.445 y phi[2]=-0.114)")
#Los segmentos se alternan entre positivos y negativos en la PACF.  

set.seed(2.7183)
AR2_7 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.233,-0.377)), n=223, sd=0.163)
plot.ts(AR2_7, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=0.233 y phi[2]=-0.377)")
acf(AR2_7, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=0.233 y phi[2]=-0.377)")
pacf(AR2_7, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=0.233 y phi[2]=-0.377)")


#### Modelos AR(2), phi[1]<0 & phi[2]<0 ####
set.seed(1.61803)
AR2_8 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.3050,-0.45435)), n=227, sd=0.167)
plot.ts(AR2_8, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.305 y phi[2]=-0.4544)")
acf(AR2_8, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.305 y phi[2]=-0.4544)")
pacf(AR2_8, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.305 y phi[2]=-0.4544)")
# En la ts las primeras 100 observaciones estan comprendidas entre los valores de -0.4 y 0.4. 
# Más adelante, alrededor de la observación 183 se llega a un valor máximo postivo superior a 
# 0.6. 
# Se observa con gran valore negativo y significativo en el segundo desface de la PACF.

set.seed(1.61803)
AR2_9 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.1597,-0.2584)), n=227, sd=0.167)
plot.ts(AR2_9, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.1597 y phi[2]=-0.2584)")
acf(AR2_9, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.1597 y phi[2]=-0.2584)")
pacf(AR2_9, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.1597 y phi[2]=-0.2584)")

set.seed(1.61803)
AR2_10 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.4181,-0.335325)), n=227, sd=0.167)
plot.ts(AR2_10, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.4181 y phi[2]=-0.3353)")
acf(AR2_10, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.4181 y phi[2]=-0.3353)")
pacf(AR2_10, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.4181 y phi[2]=-0.3353)")

set.seed(1.61803)
AR2_11 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.10946,-0.17711)), n=227, sd=0.167)
plot.ts(AR2_11, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.1095 y phi[2]=-0.1771)")
acf(AR2_11, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.1095 y phi[2]=-0.1771)")
pacf(AR2_11, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.1095 y phi[2]=-0.1771)")

set.seed(1.61803)
AR2_12 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.28657,-0.46368)), n=227, sd=0.167)
plot.ts(AR2_12, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.2866 y phi[2]=-0.4637)")
acf(AR2_12, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.2866 y phi[2]=-0.4637)")
pacf(AR2_12, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.2866 y phi[2]=-0.4637)")

#### MOdelos AR(2), phi[1]<0 & phi[2]>0 ####
set.seed(0.1010010)
AR2_13 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.75025,0.121393)), n=229, sd=0.173)
plot.ts(AR2_13, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.7503 y phi[2]=0.1214)")
acf(AR2_13, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.7503 y phi[2]=0.1214)")
pacf(AR2_13, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.7503 y phi[2]=0.1214)")
# La serie de tiempo muestra tres tramos marcados con grandes alteraciones de valores negativos
# y positivos. 
# Los valores, hasta poco después del 10 desface, son significativos y fructuan en valores 
# positivos y negativos en la ACF. Luego, las autocorrelaciones vuelven a ser significativas 
# después del vigésimo segundo desface. 
# El primer desface, en la PACF, presenta un valor extremo y negativo. 

set.seed(0.1010010)
AR2_14 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.196418,0.317811)), n=229, sd=0.173)
plot.ts(AR2_14, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.1964 y phi[2]=0.3178)")
acf(AR2_14, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.1964 y phi[2]=0.3178)")
pacf(AR2_14, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.1964 y phi[2]=0.3178)")

set.seed(0.1010010)
AR2_15 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.514229,0.4151020)), n=229, sd=0.173)
plot.ts(AR2_15, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.5142 y phi[2]=0.4151)")
acf(AR2_15, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.5142 y phi[2]=0.4151)")
pacf(AR2_15, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.5142 y phi[2]=0.4151)")
# Una serie de tiempo muy parecida a AR2_13, pero con más segmentos y bastantes definidos. 
# El correlograma de la ACF enseña que todos los valores de los desfaces son significativos.

set.seed(0.1010010)
AR2_16 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.1346269,0.2178309)), n=229, sd=0.173)
plot.ts(AR2_16, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.1346 y phi[2]=0.2178)")
acf(AR2_16, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.1346 y phi[2]=0.2178)")
pacf(AR2_16, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.1346 y phi[2]=0.2178)")

set.seed(0.1010010)
AR2_17 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.3524578,0.5702887)), n=229, sd=0.173)
plot.ts(AR2_17, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.3525 y phi[2]=0.5703)")
acf(AR2_17, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.3525 y phi[2]=0.5703)")
pacf(AR2_17, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.3525 y phi[2]=0.5703)")
# Serie de tiempo bastante bien parecida a AR2_15.

set.seed(0.1010010)
AR2_18 <- arima.sim(model = list(order=c(2,0,0), ar=c(-0.4511353325,0.14930352)), n=229, sd=0.173)
plot.ts(AR2_18, main="Serie de Tiempo Simulada",
        sub="Proceso Autorregresivo (phi[1]=-0.4511 y phi[2]=0.1493)")
acf(AR2_18, main="Función de Autocorrelación Simple",
    sub="Proceso Autorregresivo (phi[1]=-0.4511 y phi[2]=0.1493)")
pacf(AR2_18, main = "Función de Autocorrelación Parcial",
     sub="Proceso Autorregresivo (phi[1]=-0.4511 y phi[2]=0.1493)")


#### Modelos MA(2), theta[1]>0 & theta[2]>0 ####

rm(list=ls())

set.seed(3.1416)
MA2_1 <- arima.sim(model=list(order=c(0,0,2), ma=c(0.2, 0.3)), sd=0.157, n=211)
plot.ts(MA2_1, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=0.2 y theta[2]=0.3)")
acf(MA2_1, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=0.2 y theta[2]=0.3))")
pacf(MA2_1, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=0.2 y theta[2]=0.3)")
# Se observa tres valores postivos y significativos en los tres primeros desfaces de la ACF.
# Los primeros desfaces presentan valores mayores a 0.2 en la PACF.

set.seed(3.1416)
MA2_2 <- arima.sim(model=list(order=c(0,0,2), ma=c(0.5, 0.7)), sd=0.157, n=211)
plot.ts(MA2_2, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=0.5 y theta[2]=0.7)")
acf(MA2_2, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=0.5 y theta[2]=0.7))")
pacf(MA2_2, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=0.5 y theta[2]=0.7)")
#Los desfaces oscilan entre valores positivos y negativos tanto para la ACF como para la PACF.

set.seed(3.1416)
MA2_3 <- arima.sim(model=list(order=c(0,0,2), ma=c(0.11, 0.13)), sd=0.157, n=211)
plot.ts(MA2_3, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=0.11 y theta[2]=0.13)")
acf(MA2_3, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=0.11 y theta[2]=0.13))")
pacf(MA2_3, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=0.11 y theta[2]=0.13)")
# Ninguna autocorrelación parcial es significativa. 

#### Modelos MA(2), theta[1]>0 & theta[2]<0 ####
set.seed(2.7183)
MA2_4 <- arima.sim(model=list(order=c(0,0,2), ma=c(0.17, -0.19)), sd=0.163, n=223)
plot.ts(MA2_4, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=0.17 y theta[2]=-0.19)")
acf(MA2_4, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=0.17 y theta[2]=-0.19))")
pacf(MA2_4, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=0.17 y theta[2]=-0.19)")

set.seed(2.7183)
MA2_5 <- arima.sim(model=list(order=c(0,0,2), ma=c(0.23, -0.29)), sd=0.163, n=223)
plot.ts(MA2_5, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=0.23 y theta[2]=-0.29)")
acf(MA2_5, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=0.23 y theta[2]=-0.29))")
pacf(MA2_5, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=0.23 y theta[2]=-0.29)")
# Solo en el segundo desface se evidencia una autocorrelación parcial significativa. 

set.seed(2.7183)
MA2_6 <- arima.sim(model=list(order=c(0,0,2), ma=c(0.31, -0.37)), sd=0.163, n=223)
plot.ts(MA2_6, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=0.31 y theta[2]=-0.37)")
acf(MA2_6, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=0.31 y theta[2]=-0.37))")
pacf(MA2_6, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=0.31 y theta[2]=-0.37)")

set.seed(2.7183)
MA2_7 <- arima.sim(model=list(order=c(0,0,2), ma=c(0.41, -0.43)), sd=0.163, n=223)
plot.ts(MA2_7, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=0.41 y theta[2]=-0.43)")
acf(MA2_7, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=0.41 y theta[2]=-0.43))")
pacf(MA2_7, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=0.41 y theta[2]=-0.43)")
# El tercer desface presenta una autocorrelación negativa y significativa. 

#### Modelos MA(2), theta[1]<0 & theta[2]<0 ####
set.seed(1.61803)
MA2_8 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.47, -0.53)), sd=0.167, n=227)
plot.ts(MA2_8, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.47 y theta[2]=-0.53)")
acf(MA2_8, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.47 y theta[2]=-0.53))")
pacf(MA2_8, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.47 y theta[2]=-0.53)")
# Todos los valores de autocorrelación parcial significativos son negativos. El mayor se encuentra
# en el segundo desface.

set.seed(1.61803)
MA2_9 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.59, -0.61)), sd=0.167, n=227)
plot.ts(MA2_9, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.59 y theta[2]=-0.61)")
acf(MA2_9, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.59 y theta[2]=-0.61))")
pacf(MA2_9, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.59 y theta[2]=-0.61)")
#Correlograma de autocorrelación parcial parecido a MA2_8, pero con menores magnitudes.  

set.seed(1.61803)
MA2_10 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.67, -0.71)), sd=0.167, n=227)
plot.ts(MA2_10, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.67 y theta[2]=-0.71)")
acf(MA2_10, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.67 y theta[2]=-0.71))")
pacf(MA2_10, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.67 y theta[2]=-0.71)")

set.seed(1.61803)
MA2_11 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.73, -0.79)), sd=0.167, n=227)
plot.ts(MA2_11, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.73 y theta[2]=-0.79)")
acf(MA2_11, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.73 y theta[2]=-0.79))")
pacf(MA2_11, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.73 y theta[2]=-0.79)")

set.seed(1.61803)
MA2_12 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.83, -0.89)), sd=0.167, n=227)
plot.ts(MA2_12, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.83 y theta[2]=-0.89)")
acf(MA2_12, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.83 y theta[2]=-0.89))")
pacf(MA2_12, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.83 y theta[2]=-0.89)")

#### Medelos MA(2), theta[1]<0 & theta[2]>0 ####
set.seed(0.1010010)
MA2_13 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.97, 0.101)), sd=0.173, n=229)
plot.ts(MA2_13, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.97 y theta[2]=0.101)")
acf(MA2_13, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.97 y theta[2]=0.101))")
pacf(MA2_13, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.97 y theta[2]=0.101)")
#Por debajo del décimo segundo desface, todas las autocorrelaciones parciales son negativas. 

set.seed(0.1010010)
MA2_14 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.103, 0.107)), sd=0.173, n=229)
plot.ts(MA2_14, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.103 y theta[2]=0.107)")
acf(MA2_14, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.103 y theta[2]=0.107))")
pacf(MA2_14, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.103 y theta[2]=0.107)")
#Ningula autocorrelación parcial es negativa.  

set.seed(0.1010010)
MA2_15 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.109, 0.113)), sd=0.173, n=229)
plot.ts(MA2_15, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.109 y theta[2]=0.113)")
acf(MA2_15, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.109 y theta[2]=0.113))")
pacf(MA2_15, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.109 y theta[2]=0.113)")

set.seed(0.1010010)
MA2_16 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.127, 0.131)), sd=0.173, n=229)
plot.ts(MA2_16, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.127 y theta[2]=0.131)")
acf(MA2_16, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.127 y theta[2]=0.131))")
pacf(MA2_16, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.127 y theta[2]=0.131)")

set.seed(0.1010010)
MA2_17 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.137, 0.139)), sd=0.173, n=229)
plot.ts(MA2_17, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.137 y theta[2]=0.139)")
acf(MA2_17, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.137 y theta[2]=0.139))")
pacf(MA2_17, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.137 y theta[2]=0.139)")

set.seed(0.1010010)
MA2_18 <- arima.sim(model=list(order=c(0,0,2), ma=c(-0.149, 0.151)), sd=0.173, n=229)
plot.ts(MA2_18, main="Serie de Tiempo Simulada",
        sub="Proceso de Medias Móviles (theta[1]=-0.149 y theta[2]=0.151)")
acf(MA2_18, main="Función de Autocorrelación Simple",
    sub="Proceso de Medias Móviles (theta[1]=-0.149 y theta[2]=0.151))")
pacf(MA2_18, main = "Función de Autocorrelación Parcial",
     sub="Proceso de Medias Móviles (theta[1]=-0.149 y theta[2]=0.151)")
