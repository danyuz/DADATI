# libraries used 
library(readxl)
library(dplyr)
library(zoo)
library(stringr)
library(stringi)
library(writexl)

# working directory for the function reading
setwd("C:/Users/Beatriz Garcia/Desktop/DANYU/ECI")
source("format_changing_function.R")

setwd("C:/Users/Beatriz Garcia/Desktop/DANYU/ECI/ECI_Reposicion_semanal")
source("repos_function.R")

# changing the working directory for the data reading
setwd("C:/Users/Beatriz Garcia/Desktop/DANYU/ECI/ECI_Reposicion_semanal/NOV08-NOV11/data") 
dataFiles <- lapply(Sys.glob('*.xlsx'), read_excel)

# applying functions to the files
# change format
df <- lapply(dataFiles, change_format) %>% 
  bind_rows() 

# REPLENISHMENT
# PLS CHECK BEFORE APPLYING THE REPOS FUNCTION!!!
# MADE ONLY ONLY FOR 6 CENTERS!!!
corners <- unique(filter(df, Vendida>0)$`Punto de Venta Centro`)
unique(corners)[1] # GOYA?
unique(corners)[2] # MURCIA?
unique(corners)[3] # MALAGA?
unique(corners)[4] # NERVION?
unique(corners)[6] # CORDOBA?
unique(corners)[7] # SALAMANCA?

data <- repos(df)

salamanca <- data[data$corner == "salamanca", ]
nervion <- data[data$corner == "nervion", ]
murcia <- data[data$corner == "murcia", ]
malaga <- data[data$corner == "malaga", ]
goya <- data[data$corner == "goya", ]
cordoba <- data[data$corner == "cordoba", ]

# write excels
setwd("C:/Users/Beatriz Garcia/Desktop/DANYU/ECI/ECI_Reposicion_semanal/NOV08-NOV11/reposicion")
write_xlsx(salamanca, "Salamanca_NOV08-NOV11.xlsx")
write_xlsx(nervion, "Nervion_NOV08-NOV11.xlsx")
write_xlsx(murcia, "Murcia_NOV08-NOV11.xlsx")
write_xlsx(malaga, "Malaga_NOV08-NOV11.xlsx")
write_xlsx(goya, "Goya_NOV08-NOV11.xlsx")
write_xlsx(cordoba, "Cordoba_NOV08-NOV11.xlsx")


