# Librerías que se van necesitando
source("./librerias.R")
source("./funciones.R")
# Cargando el Environment, esto se utilizó para generar el rmardown sin tener que volver
# a calucuarlo todo
# load("myData.RData")

# Exportación del dataset
accident.train <- read.csv2("./datos/accidentes-kaggle.csv",sep = ",", header = T)
accident.test <- read.csv2("./datos/accidentes-kaggle-test.csv", sep = ",", header = T)
accident.test$TIPO_ACCIDENTE <- NA
full.data <- rbind(accident.train, accident.test)

## Descripción de las variables
describe(accident.train$ANIO)
describe(accident.train$MES)
describe(accident.train$HORA)
describe(accident.train$DIASEMANA)
describe(accident.train$PROVINCIA)
describe(accident.train$COMUNIDAD_AUTONOMA)
describe(accident.train$ISLA)
describe(accident.train$TOT_VICTIMAS)
describe(accident.train$TOT_MUERTOS)
describe(accident.train$TOT_HERIDOS_GRAVES)
describe(accident.train$TOT_HERIDOS_LEVES)
describe(accident.train$TOT_VEHICULOS_IMPLICADOS)
describe(accident.train$ZONA_AGRUPADA)
describe(accident.train$RED_CARRETERA)
describe(accident.train$TIPO_VIA)
describe(accident.train$TIPO_INTERSEC)
describe(accident.train$ACOND_CALZADA)
describe(accident.train$PRIORIDAD)
describe(accident.train$SUPERFICIE_CALZADA)
describe(accident.train$LUMINOSIDAD)
describe(accident.train$FACTORES_ATMOSFERICOS)
describe(accident.train$VISIBILIDAD_RESTRINGIDA)
describe(accident.train$OTRA_CIRCUNSTANCIA)
describe(accident.train$ACERAS)
describe(accident.train$DENSIDAD_CIRCULACION)
describe(accident.train$MEDIDAS_ESPECIALES)

## Instancias clasificadas según TIPO_ACCIDENTE
plot(accident.train$TIPO_ACCIDENTE)
describe(accident.train$TIPO_ACCIDENTE)

## Eliminación de algunas variables
# Se elimina la variable CARRETERA
full.data$CARRETERA <- NULL
# Se elimina la variable ACOND_CALZADA
full.data$ACOND_CALZADA <- NULL
# Se elimina la variable TOT_VICTIMAS
full.data$TOT_VICTIMAS <- NULL

## Imputación de valores perdidos
## --- > MICE < --- ##
set.seed(179385)
# Intentar train primero y después juntar el test y volver hacer la imputación
full.train <- full.data[1:30002,]
full.test <- full.data[30003:nrow(full.data),]

# se realiza la imputacion (m = 10)
imputados <-  mice::mice(full.train, m=10, method="pmm", maxit = 5)
# se completa el conjunto de datos con las imputaciones
datosImputados <-  mice::complete(imputados)

# Pasamos a full.data el nuvo data.frame con todos los datos imputados
full.train <- datosImputados
full.data <- rbind(full.train, full.test)

#################################################################################################
# Volvemos a realizar la imputación, pero esta vez a todo el conjunto quitando la variable clase
# perdidos.

#se realiza la imputacion (m = 10)
imputados <-  mice::mice(full.data[,-27], m=10, method="pmm", maxit = 5)
# se completa el conjunto de datos con las imputaciones
datosImputados <-  mice::complete(imputados)
full.data[,1:28] <- datosImputados

## Importancia de las variables usando randomForest
# Sacado del fichero caret-randomForest.R
set.seed(74749572)
# define el control usando la funcion de seleccion mediante random forest
control <- caret::rfeControl(functions=rfFuncs, method="cv", number=10)
# ejecuta el metodo
results <- caret::rfe(trainData[,1:26], trainData[,27], sizes=c(1:26), rfeControl=control)
# muestra los resultados
print(results)
# muestra las caracteristicas elegidas
conjVariables <- predictors(results)
# realiza un grafico de los resultados.
plot(results, type=c("g", "o"), lw=2)

###### ------------ CORRELACIÓN PARA SELECCIONAR VARIABLES ------------------
matrix.all.result <- filterMethods(accident.train$TIPO_ACCIDENTE, accident.train)
head(matrix.all.result)

###### ------------ CORRELACIÓN ENTRE VARIABLES ------------------
# ZONA ~ ZONA_AGRUPADA
w.ZONA <- correlationExit(accident.train$ZONA_AGRUPADA, accident.train$ZONA, accident.train)
w.ZONA

# TRAZADO_NO_INTERSEC ~ TIPO_INTERSEC
w.INTERSEC <- correlationExit(accident.train$TRAZADO_NO_INTERSEC, accident.train$TIPO_INTERSEC, accident.train)
w.INTERSEC

##RELACION INTERSECION PROVINCIA/COMUNIDAD AUTONOMA
w.PROVINCIA.COM <- correlationExit(accident.train$PROVINCIA, accident.train$COMUNIDAD_AUTONOMA)
w.PROVINCIA.COM
# PROVINCIA ~ COMUNIDAD_AUTONOMA
comparePyC <- compareItems("PROVINCIA", "COMUNIDAD_AUTONOMA")
comparePyC

## Transformación de variables
# Pasamos la hora a entero
full.data$HORA <- trunc(full.data$HORA)

## Seleción de variables para crear los modelos
sub.full.data <- full.data[,conjVariables]
sub.full.data$TIPO_ACCIDENTE <- full.data$TIPO_ACCIDENTE
## Se selecciona el dataset para entrenar
trainData <- sub.full.data[1:30002,]

## MODELOS

# ***** Random Forest *****
# con las variables seleccionadas por este mismo algoritmo
rf.model.29M.var <- randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=trainData, ntree=500)
print(rf.model.29M.var)
rf.importancia.29M.var <- randomForest::importance(rf.model.29M.var)
rf.importancia.29M.var
varImpPlot(rf.model.29M.var)

## # ***** Random Forest *****
# con las variables seleccionadas por este mismo algoritmo y eliminando ZONA
rf.model.30M.Z <- randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=trainData, ntree=1500)
print(rf.model.30M.Z)
rf.importancia.30M.Z <- randomForest::importance(rf.model.30M.Z)
rf.importancia.30M.Z
varImpPlot(rf.model.30M.Z)

# ***** Random Forest *****
# con las variables seleccionadas por este mismo algoritmo y eliminando ZONA y COMUNIDAD_AUTONOMA
rf.model.30MZCA <- randomForest::randomForest(TIPO_ACCIDENTE ~ ., data=trainData, ntree=500)
print(rf.model.30MZCA)
rf.importancia.30MZCA <- randomForest::importance(rf.model.30MZCA)
rf.importancia.30MZCA
varImpPlot(rf.model.30MZCA)

## kaggle
# Creamos la predicción para la entrega en kaggle
prediction <- predict(rf.model.30M.Z, full.final.test)
full.final.test$id <- seq.int(nrow(full.final.test))
submit <- data.frame(id = full.final.test$id, Prediction = prediction)
write.csv(submit, file = "./resultados/result30M.Z.csv", row.names = FALSE)
