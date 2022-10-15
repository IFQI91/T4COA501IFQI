##Tarea 4
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez

#Directorio de trabajo

setwd("C:/Users/ifqi9/OneDrive - COLEGIO DE POSTGRADUADOS/Doctorado CP Entomología/Otoño 2022/Herramientas de cómputo/Tareas/Tarea4/T4_IFQI")

#a) Cree un R Project en una carpeta digital con nombre T4COA501inicialesdesunombre. Y
#guarde su script, datos y salidas correspondientes a la tarea 4.

#b) Con el archivo de datos, T4_df_auto en Excel, cree un archivo .csv con encabezados en cada
#columna.

library(readxl)
base <- read_xlsx("T4_df_auto.xlsx")
#fix(base) para corregir algun dato
View(base)

write.csv(base,"T4_df_auto.csv")

#c)Importe sus datos a R, y guárdelos en un objeto denominado “t4.df.datos”. Revise (con
#comandos o funciones de R) las características o atributos del objeto “t4.df.datos”.

t4.df.datos <- read.csv("T4_df_auto.csv")

class(t4.df.datos)
names(t4.df.datos)
summary(t4.df.datos)
str(t4.df.datos)
dim(t4.df.datos)
length(t4.df.datos)


#d)Modifique (traduzca) los nombres de las columnas del objeto “t4.df.datos” con comandos o
#funciones de R.

names(t4.df.datos)
names (t4.df.datos) = c("ID", "millasporgalon", "cilindros", "desplazamiento", 
              "caballosdefuerza", "peso", "aceleracion","anio","origen","nombre")
names (t4.df.datos)



#e)Cree cuatro subconjuntos de datos (objetos) para reducir el número de observaciones (hileras
#y/o columnas), analice los datos y defina los criterios lógicos para reducir los datos.

amplitud <- diff(range(t4.df.datos$caballosdefuerza)) # amplitud del rango. Tambien es max(muestra) - min(muestra)
amplitud

rangointervalos <- amplitud / 4
rangointervalos

#grupos por rangos de caballos de fuerza
sub1 <- subset(t4.df.datos,caballosdefuerza>=46 & caballosdefuerza <= 92)
sub2 <- subset(t4.df.datos,caballosdefuerza>=93 & caballosdefuerza <= 139)
sub3 <- subset(t4.df.datos,caballosdefuerza>=140 & caballosdefuerza <= 186)
sub4 <- subset(t4.df.datos,caballosdefuerza>=187 & caballosdefuerza <= 230)

#f) Explore los datos realizando gráficas de dispersión, histogramas, boxplots, para alguna de las
#variables. (utilice los datos completos y reducidos). Guarde sus gráficas en diferentes formatos
#(pdf, tiff etc.).

#matriz de diagramas de dispersion
tiff("T4_IFQI_1.tiff", width = 831, height = 526, units = "px",pointsize = 12)
plot(t4.df.datos[,c(-1,-8:-10)])
dev.off()

png("T4_IFQI_2.png", width = 831, height = 526, units = "px",pointsize = 12)
hist(t4.df.datos$aceleracion)
dev.off()

jpeg("T4_IFQI_3.jpeg", width = 831, height = 526)
boxplot(t4.df.datos$aceleracion~t4.df.datos$anio)
media1 <- tapply(t4.df.datos$aceleracion,t4.df.datos$anio,mean)
points(media1,pch=16,col="black")
dev.off()

jpeg("T4_IFQI_4.jpeg", width = 831, height = 526)
boxplot(t4.df.datos$aceleracion~t4.df.datos$origen)
media2 <- tapply(t4.df.datos$aceleracion,t4.df.datos$origen,mean)
points(media2,pch=16,col="black")
dev.off()


# Por subgrupo 1
pdf("T4_IFQI_5.pdf", width = 8.31, height = 5.26)
plot(sub1[,c(-1,-8:-10)])
dev.off()

tiff("T4_IFQI_6.tiff", width = 831, height = 526, units = "px",pointsize = 12)
hist(sub1$aceleracion)
dev.off()

png("T4_IFQI_7.png", width = 831, height = 526)
boxplot(sub1$aceleracion~sub1$origen)
media3 <- tapply(sub1$aceleracion,sub1$origen,mean)
points(media3,pch=16,col="black")
dev.off()

jpeg("T4_IFQI_8.jpeg", width = 831, height = 526)
boxplot(sub1$aceleracion~sub1$anio)
media4 <- tapply(sub1$aceleracion,sub1$anio,mean)
points(media4,pch=16,col="black")
dev.off()

# Por subgrupo 2
pdf("T4_IFQI_9.pdf", width = 8.31, height = 5.26)
plot(sub2[,c(-1,-8:-10)])
dev.off()

tiff("T4_IFQI_10.tiff", width = 831, height = 526, units = "px",pointsize = 12)
hist(sub2$aceleracion)
dev.off()

png("T4_IFQI_11.png", width = 831, height = 526)
boxplot(sub2$aceleracion~sub2$origen)
media4 <- tapply(sub2$aceleracion,sub2$origen,mean)
points(media4,pch=16,col="black")
dev.off()

jpeg("T4_IFQI_12.jpeg", width = 831, height = 526)
boxplot(sub2$aceleracion~sub2$anio)
media5 <- tapply(sub2$aceleracion,sub2$anio,mean)
points(media5,pch=16,col="black")
dev.off()

# Por subgrupo 3
pdf("T4_IFQI_13.pdf", width = 8.31, height = 5.26)
plot(sub3[,c(-1,-8:-10)])
dev.off()

tiff("T4_IFQI_14.tiff", width = 831, height = 526, units = "px",pointsize = 12)
hist(sub3$aceleracion)
dev.off()

png("T4_IFQI_15.png", width = 831, height = 526)
boxplot(sub3$aceleracion~sub3$origen)
media6 <- tapply(sub3$aceleracion,sub3$origen,mean)
points(media6,pch=16,col="black")
dev.off()

jpeg("T4_IFQI_16.jpeg", width = 831, height = 526)
boxplot(sub3$aceleracion~sub3$anio)
media7 <- tapply(sub3$aceleracion,sub3$anio,mean)
points(media7,pch=16,col="black")
dev.off()

# Por subgrupo 4
pdf("T4_IFQI_17.pdf", width = 8.31, height = 5.26)
plot(sub4[,c(-1,-8:-10)])
dev.off()

tiff("T4_IFQI_18.tiff", width = 831, height = 526, units = "px",pointsize = 12)
hist(sub4$aceleracion)
dev.off()

png("T4_IFQI_19.png", width = 831, height = 526)
boxplot(sub4$aceleracion~sub4$origen)
media8 <- tapply(sub4$aceleracion,sub4$origen,mean)
points(media8,pch=16,col="black")
dev.off()

jpeg("T4_IFQI_20.jpeg", width = 831, height = 526)
boxplot(sub4$aceleracion~sub4$anio)
media9 <- tapply(sub4$aceleracion,sub4$anio,mean)
points(media9,pch=16,col="black")
dev.off()

#g) Explore algunos de sus datos con las funciones de R table(), summary(), cor(), mean(),
#median(). Guarde sus resultados en objetos R.

tabla <- table(base$acceleration,base$year)
tabla

resumen <- summary(base)
resumen
correlacion <- cor(base[,c(2,3,4,5,6,7)])
correlacion

mediabase <- colMeans(base[,c(2,3,4,5,6,7)])
mediabase

mediabase2 <- tapply(base$acceleration,base$year,mean)
mediabase2

mediaac <- mean(base$acceleration)
medianac <- median(base$acceleration)
deac <- sd(base$acceleration)

#funcion para moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

modaac <- moda(base$acceleration)

#mediaac;medianac;modaac

objetoac2 <- cbind(mediaac,medianac,modaac)

#h) Reduzca el objeto t4.df.datos (data.frame) a dos subconjuntos arbitrarios A y B, de 10 hileras
#y 4 columnas, que contenga únicamente números. Luego convierta estos objetos reducidos a
#objetos matrix. Además, obtenga dos subconjuntos arbitrarios (vectores) de longitud 10, c1 y
#c2.
library(dplyr)

#A 10 x 4
A <- t4.df.datos[,c(-1,-6:-10)]
A_s <- sample_n(A,10)
names(A_s) <- NULL

#B 10 x 4
B <- t4.df.datos[,c(-1:-5,-10)]
B_s <- sample_n(B,10)
names(B_s) <- NULL

#matrices
A_s <- as.matrix(A_s)
B_s <- as.matrix(B_s)

#subconjuntos arbitrarios
c1 <- A_s[,1]
c2 <- B_s[,1]

#i) Una en horizontal los objetos matrix A y B y los objetos vector c1 y c2.

A_B <- cbind(A_s,B_s)

c1_c2 <- cbind(c1,c2)

#j) Obtenga las medias de las columnas (nones) y las desviaciones estándar de las hileras pares
#del objeto matrix A.

#Funcion para media de columnas pares o impares
m <- function(x) {  
  f <- seq_len(ncol(x))%%2
  return(colMeans(x[,f==1]))#cambiar a 0=pares,1=impares
                            #tambien se puede usar la funcion apply((,x[f==1]),2,mean)
}

#media de columnas impares o nones
m(A_s)


#Forma extendida
# numero de columnas en R
columnas<- ncol(A_s)
# extraer columnas  
p_col <- seq_len(columnas) %% 2
# filtrar para columnas pares o impares
filtrado <- as.data.frame(A_s)[,p_col == 1]
#media por columna
colMeans(filtrado)


#Funcion para desviacion estandar de columnas pares o impares
d <- function(x) {  
  f <- seq_len(nrow(x))%%2
  return(apply((x[f==0,]),1,sd))#cambiar en f== a 0=pares,1=impares
                                #cambiar en apply 1=filas, 2=columnas
}

#desviacion estandar de filas pares
d(A_s)


#k) Guarde a un archivo .csv y a un archivo .RData el objeto matrix A.

write.csv(A_s,"matrix_A.csv")
save(A_s,file="matrix_A.RData")

#dump() pueden guardar los contenidos de una función u otro objeto en un archivo externo
#sink() guarda el resultado que normalmente se muestra en la consola en un archivo externo
#source() para leer la información que has guardado previamente.

#l) Guarde a un archivo .txt y a un archivo .RData el objeto matrix B.

write.table(B_s,"matrix_B.txt")
save(B_s,file="matrix_B.RData")

#m) Defina una función llamada fmatriz( ), con un objeto matrix como argumento. Para calcular
#los totales de columnas. Luego, llame la función fmatriz( ) con su argumento para hacer el
#cálculo de los totales de columnas y guarde el resultado en un objeto vector total.columnas.

fmatriz <- function(matrix){
    return(apply(matrix,2,sum))#cambiar en f== a 0=pares,1=impares
                               #cambiar en apply 1=filas, 2=columnas
  }

#forma rapida con la funcion colSums()

total.columnas <- fmatriz(A_s)
total.columnas

#n)Efectúe una regresión lineal entre desplazamiento y peso, del conjunto de datos proporcionado
#y guardado en t4.df.datos . Elabore una gráfica entre los valores predichos y la variable
#predictor. Y gráfica de los residuales del modelo versus la variable predicha. Interprete sus
#resultados.

ml <- lm(desplazamiento~peso,t4.df.datos)
plot(t4.df.datos$peso,t4.df.datos$desplazamiento)
abline(ml,col="red")
summary(ml)

#grafica predictor vs ajustados
plot(ml$fitted.values,t4.df.datos$peso)

#grafica residuales vs predichos

plot(ml$fitted.values,ml$residuals)

plot(ml,3)

#qqplot
plot(ml,2)

#interpretacion: analizando la grafica residuales vs preichos se observa que hay
#heterocedasticidad de los residuos, se observa relación lineal entre desplazamiento y peso

#o)Cree un archivo notebook en R, para describir y ejecutar el problema del inciso n). Describa
#su notebook en tres etapas la entrada de los datos, luego el modelo de regresión y sus
#resultados, y finalmente la presentación de las gráficas y la interpretación de sus resultados.

save.image("T4_IFQI.RData")

