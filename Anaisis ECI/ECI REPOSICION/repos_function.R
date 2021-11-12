# GENERIC FUNCTION FOR REPLENISHMENT

# ONLY FOR 6 CENTERS!!!!!!!!!
# ONLY FOR 6 CENTERS!!!!!!!!!
# ONLY FOR 6 CENTERS!!!!!!!!!
# CHECK BEFORE USING!!!!!!!!!

repos <- function(df){
  
  # remove the ???errors??? generated from SERES
  df <- df[df$Vendida != 0, ]
  
  # corners ECI
  corners <- unique(filter(df, Vendida>0)$`Punto de Venta Centro`)
  
  # filter each corner
  df_goya <- df[df$`Punto de Venta Centro`== unique(corners)[1],]
  df_murcia <- df[df$`Punto de Venta Centro`== unique(corners)[2],]
  df_malaga <- df[df$`Punto de Venta Centro`== unique(corners)[3],]
  df_nervion <- df[df$`Punto de Venta Centro`== unique(corners)[4],]
  df_cordoba <- df[df$`Punto de Venta Centro`== unique(corners)[6],]
  df_salamanca <- df[df$`Punto de Venta Centro`== unique(corners)[7],]
  
  
  #####################################################################
  ###########################EXAMPLE GOYA##############################
  #####################################################################
  
  ### LOS CODIGOS SE PUEDEN MEJORAR :)
  
  # duplicates for each corner
  # sum the quantity for each repeated EANs
  d_goya <- df_goya %>% 
    group_by(EAN) %>% 
    summarise(cant_sum = sum(Vendida))
  # EANs with 0 total sum 
  ean0_goya <- d_goya[d_goya$cant_sum==0,]$EAN
  
  # find in df_goya that matches the EAN that has 0
  a <- df_goya[df_goya$EAN %in% ean0_goya,]
  
  # only keep the LAST ONE so that we know IF ITS A SELL OR RETURN 
  # >0 means sells, <0 means returns
  a <- a[!duplicated(a$EAN, fromLast = T),]
  
  # ONLY KEEP SALES!! FOR REPLENISHMENT
  # rename in order to merge
  a <- a[a$Vendida>0,] %>% rename(cant_sum = Vendida)
  
  # merge both dataframes,
  goya <- rbind(d_goya[d_goya$cant_sum>0,], a[c("EAN", "cant_sum")])
  
  # to merge 
  goya$corner <- rep("goya", nrow(goya)) 
  #####################################################################
  
  d_murcia <- df_murcia %>% 
    group_by(EAN) %>% 
    summarise(cant_sum = sum(Vendida))
  ean0_murcia <- d_murcia[d_murcia$cant_sum==0,]$EAN
  
  d_malaga <- df_malaga %>% 
    group_by(EAN) %>% 
    summarise(cant_sum = sum(Vendida))
  ean0_malaga <- d_malaga[d_malaga$cant_sum==0,]$EAN
  
  d_nervion <- df_nervion %>% 
    group_by(EAN) %>% 
    summarise(cant_sum = sum(Vendida))
  ean0_nervion <- d_nervion[d_nervion$cant_sum==0,]$EAN
  
  d_cordoba <- df_cordoba %>% 
    group_by(EAN) %>% 
    summarise(cant_sum = sum(Vendida))
  ean0_cordoba <- d_cordoba[d_cordoba$cant_sum==0,]$EAN
  
  d_salamanca <- df_salamanca %>% 
    group_by(EAN) %>% 
    summarise(cant_sum = sum(Vendida))
  ean0_salamanca <- d_salamanca[d_salamanca$cant_sum==0,]$EAN
  
  b <- df_murcia[df_murcia$EAN %in% ean0_murcia,]
  b <- b[!duplicated(b$EAN, fromLast = T),]
  b <- b[b$Vendida>0,] %>% rename(cant_sum = Vendida)
  
  c <- df_malaga[df_malaga$EAN %in% ean0_malaga,]
  c <- c[!duplicated(c$EAN, fromLast = T),]
  c <- c[c$Vendida>0,] %>% rename(cant_sum = Vendida)
  
  d <- df_nervion[df_nervion$EAN %in% ean0_nervion,]
  d <- d[!duplicated(d$EAN, fromLast = T),]
  d <- d[d$Vendida>0,] %>% rename(cant_sum = Vendida)
  
  e <- df_cordoba[df_cordoba$EAN %in% ean0_cordoba,]
  e <- e[!duplicated(e$EAN, fromLast = T),]
  e <- e[e$Vendida>0,] %>% rename(cant_sum = Vendida)
  
  f <- df_salamanca[df_salamanca$EAN %in% ean0_salamanca,]
  f <- f[!duplicated(f$EAN, fromLast = T),]
  f <- f[f$Vendida>0,] %>% rename(cant_sum = Vendida)
  
  murcia <- rbind(d_murcia[d_murcia$cant_sum>0,], b[c("EAN", "cant_sum")])
  malaga <- rbind(d_malaga[d_malaga$cant_sum>0,], c[c("EAN", "cant_sum")])
  nervion <- rbind(d_nervion[d_nervion$cant_sum>0,], d[c("EAN", "cant_sum")])
  cordoba <- rbind(d_cordoba[d_cordoba$cant_sum>0,], e[c("EAN", "cant_sum")])
  salamanca <- rbind(d_salamanca[d_salamanca$cant_sum>0,], f[c("EAN", "cant_sum")])

  murcia$corner <- rep("murcia", nrow(murcia)) 
  malaga$corner <- rep("malaga", nrow(malaga)) 
  nervion$corner <- rep("nervion", nrow(nervion)) 
  cordoba$corner <- rep("cordoba", nrow(cordoba)) 
  salamanca$corner <- rep("salamanca", nrow(salamanca)) 
  
  data <- rbind(goya, murcia, malaga, nervion, cordoba, salamanca)
  
  return(data)
}
