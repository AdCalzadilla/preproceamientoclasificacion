## Crear la matriz y el caluculo de los estadísticos a partir de una función
filterMethods <- function(vs, df){
  w.chi.squared <- FSelector::chi.squared(vs~., df)
  w.gain.ratio <-  gain.ratio(vs~., df)
  w.information.gain <- FSelector::information.gain(vs~., df)
  w.symetrical <-  symmetrical.uncertainty(vs~., df)
  w.oneR <- FSelector::oneR(vs~., df)

  mat <- as.matrix(w.chi.squared)
  mat <- cbind(mat, w.gain.ratio)
  mat <- cbind(mat, w.information.gain)
  mat <- cbind(mat, w.symetrical)
  mat <- cbind(mat, w.oneR)
  names(mat) <- c("chi.squared", "gain.ratio", "information.gain", "symetrical", "oneR")
  return(mat)
}

## Función para comparar la correlación existente entre dos variables
compareItems <- function(x, y){
  mat <- matrix(data = c(matrix.all.result[x,], matrix.all.result[y,]),
                nrow = 2,
                byrow = T,
                dimnames = list(c(x, y), names(matrix.all.result)))
}

## Función para calcular la correlación entre dos variables
correlationExit <- function(value, vs, dataset){
  w.chi <- FSelector::chi.squared(vs~value,dataset)
  w.gain <- FSelector::gain.ratio(vs~value,dataset)
  w.information <- FSelector::information.gain(vs~value,dataset)
  w.symetrical <- FSelector::symmetrical.uncertainty(vs~value,dataset)
  x <- c(w.chi, w.gain, w.information, w.symetrical)
  names(x) <- c("chi.squared", "gain.ratio", "information.gain", "symetrical")
  return(x)
}
