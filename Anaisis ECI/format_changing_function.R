## format changing function
change_format <- function(data) {
  
  data <- data[-(1:15),]
  
  # Puntos de Ventas
  data[,1][is.na(data[,1])] <- "ooooooooooooooooo"
  
  data["Punto de Venta Codigo"] <- rep(NA, nrow(data))
  data["Punto de Venta Centro"] <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    if (data[i,1] == "Punto de Venta") {
      data["Punto de Venta Codigo"][i,] = data[i, 2]
      data["Punto de Venta Centro"][i,] = data[i, 3]
    }
  }
  data["Punto de Venta Codigo"] <- na.locf(data["Punto de Venta Codigo"]) 
  data["Punto de Venta Centro"] <- na.locf(data["Punto de Venta Centro"])
  
  ## EAN, Cantidad, Precio y Vendedor
  data[,2][is.na(data[,2])] <- "ooooooooooooooooo"
  data[,4][is.na(data[,4])] <- "ooooooooooooooooo"
  data[,6][is.na(data[,6])] <- "ooooooooooooooooo"
  data[,8][is.na(data[,8])] <- "ooooooooooooooooo"
  
  data$...2 <- stri_trans_general(data$...2, "Latin-ASCII")
  data$...6 <- stri_trans_general(data$...6, "Latin-ASCII")
  
  data["EAN"] <- rep(NA, nrow(data))
  data["Vendida"] <- rep(NA, nrow(data))
  data["Precio"] <- rep(NA, nrow(data))
  data["Vendedor"] <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    if (data[i, 2] == "EAN") {
      if (data[i,3] == 0){
        data["EAN"][i,] = data[i+1, 3]
      } else {
        data["EAN"][i,] = data[i,3]
      }
    }
    
    if (data[i, 4] == "Vendida" | data[i, 4] == "Venta Completa") {
      data["Vendida"][i,] = data[i, 5]
    }
    if (data[i, 6] == "Neto Linea") {
      data["Precio"][i,] = data[i,7]
    }
    if (data[i, 8] == "P.O.") {
      data["Vendedor"][i,] = data[i,9]
    }
  }
  
  # Referencias
  data["Operacion"] <- rep(NA, nrow(data))
  data["ACE"] <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    if (data[i, 2] == "Operacion (Centro/Talon/Fecha)") {
      if (data[i-2, 2] == "Comprador") {
        data["Operacion"][i-3,] = data[i,4]
      } else {
        data["Operacion"][i-2,] = data[i,4]
      }
    }
    if (data[i, 2] == "ACE") {
      if (data[i-3, 2]== "Comprador") {
        data["ACE"][i-4,] = data[i,4]
      } else {
        data["ACE"][i-3,] = data[i,4]
      }
    }
  }
  
  # Fecha 
  data["Fecha"] <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    if (data[i, 6] == "Fecha Venta") {
      data["Fecha"] = data[i,7]
    }
  }
  
  # intercambiar la hora de compra y de devolucion 
  data["horaOperacion"] <- str_sub(data$Operacion, -16, -1)
  #data["horaCompra"]<-str_sub(data$Operacion, -16, -1) 
  # 需要看看hora devolucion的事
  #for (i in 1:nrow(data)) {
  #  if (!is.na(data$Vendida[i]) & data$Vendida<0)
  #    data["horaDevolucion"]<-str_sub(data$ACE, -16, -1)
  #}
  #data["horaDevolucion_backup"]<-data["horaDevolucion"]
  #data$Fecha <- str_sub(data$horaCompra, -16,-7)
  #data$Fecha <- getmode(data$Fecha)
  #
  #for (i in 1:nrow(data)) {
  #  if (!(is.na(data$horaDevolucion_backup[i]))) {
  #    data$horaDevolucion[i] =   data$horaCompra[i]
  #    data$horaCompra[i] =   data$horaDevolucion_backup[i]
  #  }
  #}
  #
  #data <- data[, -which(names(data) %in% "horaDevolucion_backup")]
  
  
  # remove the spaces in EAN column 
  data$EAN = gsub('\\s+', '', data$EAN)
  data$EAN = as.numeric(data$EAN)
  
  # changing date format
  data$Fecha <- as.numeric(data$Fecha)
  data$Fecha <- as.Date(data$Fecha, origin = "1899-12-30")
  
  data$Vendida = str_trim(data$Vendida)
  data$Vendida = as.numeric(data$Vendida)
  
  # remove columns that has nan in column vendida
  data <- data[!is.na(data$Vendida),][,10:ncol(data)]
  
}
