#-------------------------------------------------------------------------------
# This R script is part of the entitled paper: "The undervalued contributions
# and profitability of traditional agricultural systems: a case study of the
# milpa in Mexico"
# Note 1: From line 13 to line 486, the analyses were conducted within the 
# microdata laboratory. This section shows how the data was obtained, cleaned, 
# and how variables were calculated and transformed.
#
# Note 2: Starting from line 489, the analysis commands for the database 
# "microdata_prac.csv" are shown, which is provided as supplementary material 
# for this article.
# Author: Julio Díaz-José
#-------------------------------------------------------------------------------
# Packages
library(readr)
library(dplyr)
library(ggplot2) 
library(data.table)
library (FactoMineR)
library(stats)
library(factoextra)#
library(cluster)
library(tidyr)
library(scales)
library(ggpmisc)#
library(ggExtra)
library(ggside)
library(ggstatsplot)#
library(MASS)
library (robustbase)
library(ggpubr)
#-----------------------------------------------------------------------
# 1. DATA MANAGEMENT AT THE MICRODATA LABORATORY (under permissions and 
# following the protocols)
# ----------------------------------------------------------------------
# The database is composed by 5 large datasets that we must integrate in a
# sigle one: 

# a) TRD_UBICACION_UP (State, Municipality, area)
# b) TRD_TEC_AGRI_CA (Plot area, Use of inputs, fertilizers,herbicides, insecticides )
# c) TRD_TRACTORES (Use of machinery)
# d) TRD_CARAC_SOCIODEMOGRAFICAS (Age, indigenous, family size)
# e) TRD_AGRICULTURA_CIELO_ABIERTO (plot area, production)

# Import the datasets using the assisted icon in the "Environment"
# We select the variable AA111_02_C "Nombre del cultivo"
head(TRD_AGRICULTURA_CIELO_ABIERTO)
MAIZ <- TRD_AGRICULTURA_CIELO_ABIERTO %>% filter(AA111_02_C == "Maiz grano blanco"|AA111_02_C =="Maiz grano amarillo")
head(MAIZ)
# Identify a common variable
variable_comun_m1 <- "ID_CA22_UP" 
# UniÓn de las tablas 
head(TRD_UBICACION_UP)
Base_maiz <- left_join(TRD_UBICACION_UP, MAIZ, by = variable_comun_m1)
#Check
View(Base_maiz)
#Filter to delete the missing lines 

BASE_MAIZ <- filter(Base_maiz,AA111_02_C == "Maiz grano blanco"|AA111_02_C == "Maiz grano amarillo")
head(BASE_MAIZ)

### Repeat to add the next dataset #TRD_TEC_AGRI_CA

head(TRD_TEC_AGRI_CA)
variable_comun_m2 <- "ID_CA22_UP" 
Base_maiz_tec <- right_join(TRD_TEC_AGRI_CA, BASE_MAIZ, by = variable_comun_m2)
# Apply a filter again

BASE_MAIZ_TEC <- filter(Base_maiz_tec,AA111_02_C == "Maiz grano blanco"|AA111_02_C == "Maiz grano amarillo")
head(BASE_MAIZ_TEC)
# The same for the other dataset

# Rename heading to match with the common variable
TRD_TRACTORES <- LM2472_TRD_TRACTORES %>%
  rename(ID_CA22_UP = id_ca22_up
  )

head(TRD_TRACTORES)
variable_comun3 <- "ID_CA22_UP"

Base_maiz_tec_trac <- left_join(TRD_TRACTORES, BASE_MAIZ_TEC, by = variable_comun3)
head(Base_maiz_tec_trac)
BASE_MAIZ_TEC_TRAC <- filter(Base_maiz_tec_trac,AA111_02_C == "Maiz grano blanco"|AA111_02_C == "Maiz grano amarillo")
head(BASE_MAIZ_TEC_TRAC)
##Adding socioeconomic variables TRD_SOCIODEMOGRAFICAS
variable_comun4 <- "ID_CA22_UP"
Base_maiz_tec_soc <- left_join(TRD_CARAC_SOCIODEMOGRAFICAS, BASE_MAIZ_TEC_TRAC, by = variable_comun4)
BASE_MAIZ_TEC_SOC <- filter(Base_maiz_tec_soc,AA111_02_C == "Maiz grano blanco"|AA111_02_C == "Maiz grano amarillo")
head(BASE_MAIZ_TEC_SOC)
# We get a dataset contining more than 2 million of data
View(BASE_MAIZ_TEC_SOC)

#-------------------------------------------------------------------------------
# HERE WE FILTER THE MUNICIPALITIES OF INTEREST TO GET THE REGIONAL
# DATABASE (28,000 OBSERVATIONS)
# -----------------------------------------------------------------------------

# We filtered the municipalities within the study region, 
# which were previously defined in a .csv file

# 3.1.) Filter the state by name "Veracruz de Ignacio de la Llave" 
Base_veracruz_maiz <- BASE_MAIZ_TEC_SOC %>%
  filter(NOM_ENT == "Veracruz de Ignacio de la Llave")
View(Base_veracruz_maiz)
head(Base_veracruz_maiz)

# 3.2.) I loaded a vector of the municipalities of interest that grow maize,
# where fieldwork was conducted. The CSV file is named "Mun_maiz" and I will 
# call the database "Municipios". This database has a variable called "NOM_MUN".

Mun_maiz1
Municipios <- as.vector(Mun_maiz1$NOM_MUN)
Municipios

# Extracting the municipalities as a vector

municipios_interes <- Mun_maiz[[NOM_ENT]]

# 3.3.) Replace "Base_veracruz_maiz" 
base_de_datos_municipios <- Base_veracruz_maiz %>%
  filter(NOM_MUN %in% Municipios)
base_de_datos_municipios
# 3.4.)  Check the number of observations after filtering the data
num_observaciones_filtradas <- nrow(base_de_datos_municipios)
cat("Número de observaciones después de filtrar por los municipios de interés:", 
    num_observaciones_filtradas, "\n")

# We check the data containing only the municipalities of interest
head(base_de_datos_municipios)

#-------------------------------------------------------------------------------
# VARIABLES TRANSFORMATION
# This section outlines the process for calculating new variables to be used in 
# the cluster analysis. These calculations are necessary to transform the raw 
# data into meaningful metrics
################################################################################


# 4.1.) We filter to delete missing lines using a variable
base_de_datos_municipiosff1 <- filter(base_de_datos_municipiosf,SD113 == "1"|SD113 == "2")
base_de_datos_municipiosff1

head(base_de_datos_municipiosff1)
base_de_datos_municipiosff1 <- as.data.frame(base_de_datos_municipiosff1)
base_de_datos_municipiosff1

# We select the variables of interest as appear within the microdata census database 
# AA111_03_N (Plot area)
# AA111_17_N (Production maize grain)
# DA115_N    (Over 60% of the maize ther produce is sold/ if DA115_N > 60% )
# AT111_14_2 (Improved seeds )
# AT111_19  (Fertilizers)
# AT112_04  ( Draft animals)
# AT111_27_1 (Herbicides))
# AT111_28_1  (Insecticides)
# AT111_30_1 (Fungicides)
# SD115_02   (The producer identifies his/herself as indigenous)  
# SD113     (Sex )
# SD114      (age of the producer)
# TR110 (Use of tractor)
# AA111_05 (Irrigation regime) (transform 1= Riego-irrigation, 0=Temporal-rainfed)
# AT111_otra_maquinaria  Use of other machinery (planter, thresher)

#-----------------------------------------


# We want to transform variables to harmonize 1,0 and the variable DA115_N into a new one: Over 60% of the maize ther produce is sold/ if DA115_N > 60% 
# 
base_de_datos_municipiosff3 <- base_de_datos_municipiosff2 %>%
  mutate(venta = ifelse(DA115_N / AA111_17_N > 0.6, 1, 0))
base_de_datos_municipiosff3

#base_de_datos_municipiosff3$venta <- as.factor(base_de_datos_municipiosff3$venta)
base_de_datos_municipiosff3

## Transform "native seeds AT111_14_1"  1=si y 2= no 

base_de_datos_municipiosff4 <- base_de_datos_municipiosff3 %>% 
  mutate(AT111_14_1 = recode(AT111_14_1, `2` = 0,`1` = 1 ))
base_de_datos_municipiosff4

#base_de_datos_municipiosff4$AT111_14_1 <- as.factor(base_de_datos_municipiosff5$AT111_14_1)
base_de_datos_municipiosff4

## Transform improved seeds en 1=si y 2= no AT111_14_2

base_de_datos_municipiosff5<- base_de_datos_municipiosff4%>%
  mutate(AT111_14_2 = recode(AT111_14_2, `2` = 0, `1` = 1))
base_de_datos_municipiosff5


#base_de_datos_municipiosff5$AT111_14_2 <- as.factor(base_de_datos_municipiosff5$AT111_14_2)
base_de_datos_municipiosff5

## Transform fertilizers en 1=si y 2= no AT111_19

base_de_datos_municipiosff6 <- base_de_datos_municipiosff5%>%
  mutate(AT111_19 = recode(AT111_19, `2` = 0, `1` = 1 ))
#base_de_datos_municipiosff6$AT111_19 <- as.factor(base_de_datos_municipiosff6$AT111_19)
base_de_datos_municipiosff6

## Transform draft animals en 1=si y 2= no AT112_04


base_de_datos_municipiosff7 <- base_de_datos_municipiosff6%>%
  mutate(AT112_04 = recode(AT112_04, `2` = 0, `1` = 1 ))
#base_de_datos_municipiosff7$AT112_04 <- as.factor(base_de_datos_municipiosff7$AT112_04)
base_de_datos_municipiosff7


### Transform herbicides en 1=si y 2= no AT111_27_1

base_de_datos_municipiosff8 <- base_de_datos_municipiosff7%>%
  mutate(AT111_27_1 = recode(AT111_27_1, `2` = 0, `1` = 1 ))
#base_de_datos_municipiosff8$AT111_27_1 <- as.factor(base_de_datos_municipiosff8$AT111_27_1)
base_de_datos_municipiosff8


##  Transform insecticides en 1=si y 2= no AT111_28_1

base_de_datos_municipiosff9 <- base_de_datos_municipiosff8%>%
  mutate(AT111_28_1 = recode(AT111_28_1, `2` = 0,`1` = 1))
base_de_datos_municipiosff9

##  Transform fungicides  en 1=si y 2= no AT111_30_1

base_de_datos_municipiosff10 <- base_de_datos_municipiosff9%>%
  mutate(AT111_30_1 = recode(AT111_30_1, `2` = 0, `1` = 1))
base_de_datos_municipiosff10

## Match insecticides and fungicides as a single variable "Use of insecticides"

base_de_datos_municipiosff11 <- base_de_datos_municipiosff10 %>%
  mutate(AT111 = as.numeric(AT111_30_1 == 1 |AT111_28_1 == 1))

#base_de_datos_municipiosff11$AT111 <- as.factor(base_de_datos_municipiosff11$AT111)
base_de_datos_municipiosff11

#Transform variable indigenous   en 1=si y 2= no SD115_02

base_de_datos_municipiosff12 <- base_de_datos_municipiosff11 %>%
  mutate(SD115_02 = recode(SD115_02, `2` = 0,`1` = 1 ))

#base_de_datos_municipiosff12$SD115_02 <- as.factor(base_de_datos_municipiosff12$SD115_02)
base_de_datos_municipiosff12

#Transform variable tractor   en 1=si y 2= no TR110

base_de_datos_municipiosff13 <- base_de_datos_municipiosff12%>%
  mutate(tr110 = recode(tr110, `2` = 0, `1` = 1))

#base_de_datos_municipiosff13$tr110 <- as.factor(base_de_datos_municipiosff13$tr110)

base_de_datos_municipiosff13
### Transformar la variable riego en 1=si y 0= no AA111_05

base_de_datos_municipiosff14 <- base_de_datos_municipiosff13%>%
  mutate(AA111_05 = recode(AA111_05, `2` = 0, `1` = 1))


#base_de_datos_municipiosff14$AA111_05 <- as.factor(base_de_datos_municipiosff14$AA111_05)
base_de_datos_municipiosff14

#  Transform use of planter en 1=si y 2= no AT112_02

#base_de_datos_municipiosff15 <- base_de_datos_municipiosff14%>%
#  mutate(AT112_02 = recode(AT112_02, `2` = 0, `1` = 1))
#base_de_datos_municipiosff15

##  Transform thresher  en 1=si y 2= no AT112_03

base_de_datos_municipiosff16 <- base_de_datos_municipiosff14%>%
  mutate(AT112_03 = recode(AT112_03, `2` = 0,  `1` = 1))
base_de_datos_municipiosff16

##  Transform other machinery  en 1=si y 2= no AT111_32

base_de_datos_municipiosff17 <- base_de_datos_municipiosff16 %>%
  mutate(AT111_32 = recode(AT111_32, `2` = 0, `1` = 1))
base_de_datos_municipiosff17

## Joint planter, threser and other machinery

base_de_datos_municipiosff18 <- base_de_datos_municipiosff17 %>%
  mutate(AT111_otra_maquinaria = as.numeric(AT112_03 == 1|AT111_32 ==1))

#base_de_datos_municipiosff18$AT111_otra_maquinaria <- as.factor(base_de_datos_municipiosff18$AT111_otra_maquinaria)
base_de_datos_municipiosff18

# 4.2.)  Perform the formula to get a new variable named "Yield" based
# on the plot area and production

base_de_datos_municipiosff19 <- base_de_datos_municipiosff18%>%
  mutate(Rendimiento_t_ha = AA111_17_N /AA111_03_N )

# 4.3.) Perform the analysis to get the "intensification index"

# AA111_05 (Irrigated) 
# AT111_14_2 (Improved seeds)
# AT111_19  (Fertilizers)
# tr110 (Use of tractor)
# AT112_04  ( Draft animals) #several units of analysis do not use draft animals
# AT111_27_1 (Herbicides)
# AT111 (Insecticides/fungicides)
# AT111_otra_maquinaria  (Other machinery)

# Weighted variables 
#  * **Herbicides, Insecticides_fungicides, Tractor:** 

# Convert to numeric

base_de_datos_municipiosff19$AA111_05 <- as.numeric(as.character(base_de_datos_municipiosff19$AA111_05)) 
base_de_datos_municipiosff19
str(base_de_datos_municipiosff19)

#base_de_datos_municipiosff19$AT111_14_2 <- as.integer(base_de_datos_municipiosff19$AT111_14_2) 
#base_de_datos_municipiosff19
base_de_datos_municipiosff19$AT111_14_2 <- as.numeric(as.character(base_de_datos_municipiosff19$AT111_14_2)) 
base_de_datos_municipiosff19
str(base_de_datos_municipiosff19)

#base_de_datos_municipiosff19$AT111_19 <- as.integer(base_de_datos_municipiosff19$AT111_19) 
#base_de_datos_municipiosff19
base_de_datos_municipiosff19$AT111_19 <- as.numeric(as.character(base_de_datos_municipiosff19$AT111_19)) 
base_de_datos_municipiosff19
str(base_de_datos_municipiosff19)

#base_de_datos_municipiosff19$AT112_04 <- as.integer(base_de_datos_municipiosff19$AT112_04) 
#base_de_datos_municipiosff19
base_de_datos_municipiosff19$AT112_04 <- as.numeric(as.character(base_de_datos_municipiosff19$AT112_04)) 
base_de_datos_municipiosff19

#base_de_datos_municipiosff19$AT111_27_1 <- as.integer(base_de_datos_municipiosff19$AT111_27_1) 
#base_de_datos_municipiosff19

base_de_datos_municipiosff19$AT111_27_1 <- as.numeric(as.character(base_de_datos_municipiosff19$AT111_27_1)) 
base_de_datos_municipiosff19

#base_de_datos_municipiosff19$AT111 <- as.integer(base_de_datos_municipiosff19$AT111) 
#base_de_datos_municipiosff19

base_de_datos_municipiosff19$AT111 <- as.numeric(as.character(base_de_datos_municipiosff19$AT111)) 
base_de_datos_municipiosff19

#base_de_datos_municipiosff19$tr110 <- as.integer(base_de_datos_municipiosff19$tr110) 
#base_de_datos_municipiosff19

base_de_datos_municipiosff19$tr110 <- as.numeric(as.character(base_de_datos_municipiosff19$tr110)) 
base_de_datos_municipiosff19

#base_de_datos_municipiosff19$AT111_otra_maquinaria  <- as.integer(base_de_datos_municipiosff19$AT111_otra_maquinaria ) 
#base_de_datos_municipiosff19

base_de_datos_municipiosff19$AT111_otra_maquinaria <- as.numeric(as.character(base_de_datos_municipiosff19$AT111_otra_maquinaria)) 
base_de_datos_municipiosff19
str(base_de_datos_municipiosff19)
#weight
pesos <- c(
  AA111_05 = 2,
  AT111_14_2 = 1,
  AT111_19 = 1,
  AT112_04 = 1,
  AT111_27_1 = 2,            # Mayor peso
  AT111 = 2, # Mayor peso
  tr110 = 2,               # Mayor peso
  AT111_otra_maquinaria = 1
)
pesos
# We could use other method 
base_de_datos_municipiosff19$AA111_05p <- base_de_datos_municipiosff19$AA111_05*2
base_de_datos_municipiosff19

base_de_datos_municipiosff19$AT111_27_1p <- base_de_datos_municipiosff19$AT111_27_1*2
base_de_datos_municipiosff19

base_de_datos_municipiosff19$AT111p <- base_de_datos_municipiosff19$AT111*2
base_de_datos_municipiosff19

base_de_datos_municipiosff19$tr110p <- base_de_datos_municipiosff19$tr110*2
base_de_datos_municipiosff19


# Calculating the Maximum Possible Value of the Weighted Sum
# The maximum possible value of the weighted sum is the value that would be 
# obtained if all variables were 1 (presence) and multiplied by their respective
# weights.
max_valor_suma_ponderada1 <- sum(pesos) # Esto es la suma de todos los pesos definidos
max_valor_suma_ponderada1
# trasnform the number from 0 to 1.
# A value of 0 will signify the lowest possible intensification, and a value of 
# 1 will signify the highest possible intensification. This scaling creates a 
# standardized index, where each producer's score is a ratio of their actual
# weighted score to the maximum possible score. This allows for a clear, direct 
# comparison of the level of agricultural intensification across different 
# production units.
base_de_datos_municipiosff19$Indice_Intensificacion_Final <- base_de_datos_municipiosff19$suma_indi / max_valor_suma_ponderada1
base_de_datos_municipiosff19

###  Summary of the Index

print("Primeras filas del dataframe con el Índice_Intensificacion_Final ponderado:")
print(head(base_de_datos_municipiosff19 %>% select(starts_with("Indice_Intensificacion_Final"))))

print("Resumen estadístico del Indice_Intensificacion_Final ponderado:")
print(summary(base_de_datos_municipiosff19$Indice_Intensificacion_Final))

# Histogram to visualize the index distribution
hist(base_de_datos_municipiosff19$Indice_Intensificacion_Final,
     main = "Distribución del Índice de Intensificación de productores de maíz ponderado",
     xlab = "Índice de Intensificación (0 = Menos Intensified, 1 = Más Intensified)",
     ylab = "Frecuencia",
     breaks = 20, # 
     col = "darkblue",
     border = "black")

#Rename variables
resultados <- base_de_datos_municipiosff19 %>%
  rename(Superficie = AA111_03_N,
         Produccion= AA111_17_N,
         Autoconsumo =autoconsumo,
         Venta=venta,
         Semilla_criolla = AT111_14_1,
         Riego = AA111_05,
         Semilla_mejorada=AT111_14_2,
         Fertilizantes = AT111_19,
         Animales_tiro = AT112_04,
         Herbicidas = AT111_27_1,
         Insecticidas_fungicidas = AT111,
         Tractor = tr110,
         Indigena = SD115_02,
         Otra_maquinaria = AT111_otra_maquinaria,
         Rendimiento = Rendimiento_t_ha,
         Indice = Indice_Intensificacion_Final
  )
reusultados
write.csv(resultados, "microdata_prac.csv", row.names = FALSE)


# Using the resulted database we then can perform the cluster analysis to clasiffy 
# the producers
#-------------------------------------------------------------------------------
# 2. WE PERFORMED THE CLUSTER ANALYSIS USING THE CLARA ALGORITHM
# The database  "microdata_prac.csv" is provided as supplementary material 
#-------------------------------------------------------------------------------
# The cluster analysis classifies producers based on agricultural practices (through 
# an index of intensification, infrastructure such as plot area and social variables
# like indigenous and consumption patterns.
# This classification was also guided by expert knowledge ( to determine the number 
# of clusters) based on the existing maize production systems in the region. 
library(dplyr)
library(stats)
library(cluster)
library(factoextra)
# Get the data (import before running the following)
microdata_prac
# Convert the factor variables (check)
datos_clustering_nombres1 <- microdata_prac
datos_clustering_nombres1$Riego <- as.factor(as.character(datos_clustering_nombres1$Riego))
datos_clustering_nombres1$Semilla_mejorada <- as.factor(as.character(datos_clustering_nombres1$Semilla_mejorada))
datos_clustering_nombres1$Fertilizantes <- as.factor(as.character(datos_clustering_nombres1$Fertilizantes))
datos_clustering_nombres1$Animales_tiro <- as.factor(as.character(datos_clustering_nombres1$Animales_tiro))
datos_clustering_nombres1$Herbicidas <- as.factor(as.character(datos_clustering_nombres1$Herbicidas))
datos_clustering_nombres1$Insecticidas_fungicidas <- as.factor(as.character(datos_clustering_nombres1$Insecticidas_fungicidas))
datos_clustering_nombres1$Tractor <- as.factor(as.factor(datos_clustering_nombres1$Tractor))
datos_clustering_nombres1$Otra_maquinaria <- as.factor(as.character(datos_clustering_nombres1$Otra_maquinaria))
datos_clustering_nombres1$Autoconsumo <- as.factor(as.character(datos_clustering_nombres1$Autoconsumo))
datos_clustering_nombres1$Venta <- as.factor(as.factor(datos_clustering_nombres1$Venta))
datos_clustering_nombres1$Semilla_criolla <- as.factor(as.character(datos_clustering_nombres1$Semilla_criolla))
datos_clustering_nombres1$Indigena <- as.factor(as.factor(datos_clustering_nombres1$Indigena))

datos_clustering_nombres1
str(datos_clustering_nombres1)
#Select data for the analysis
datos_seleccionado_pos <- datos_clustering_nombres1[, c(1,2,3,6,13,17)]
str(datos_seleccionado_pos)

#install.packages(c("cluster", "FD", "Rcpp")) 
library(cluster)
library(FD)
datos_seleccionado_pos


# Check catergorical variables as factors
# Continuous variables as numeric.
str(datos_seleccionado_pos) # Data structure


# CLARA will perform the Gower's distance internally

# =========================================================================
# Optimal number of clusters (previously defined based on expert knowledge)
# =========================================================================

k_definido_por_expertise <- 3 
optimal_k <- k_definido_por_expertise

# =========================================================================
# Apply the CLARA algorithm


clara_fit <- clara(
  x = datos_seleccionado_pos, 
  k = optimal_k, 
  metric = "euclidean",
  samples = 50,         # Number of subsamples to extract
  sampsize = 1000,      # Size of subsample
  stand = TRUE,         # Standardize continuous variables
  trace = 0             # No details
)

# =========================================================================
# Results and analysis


#  Assigning Cluster
datos_seleccionado_pos$cluster <- clara_fit$clustering
cat("\nAsignación de las primeras observaciones a los clusters:\n")
print(head(datos_seleccionado_pos))

# CLuster size
cat("\nTamaño de cada cluster:\n")
print(table(datos_seleccionado_pos$cluster))
datos_seleccionado_pos$cluster <- clara_fit$clustering
# Medoids (Representative observations for each cluster)
cat("\nMedoides (Centros Robustos) de los Clusters:\n")
print(clara_fit$medoids)

# Descriptive analysis by cluster

# Continuous variables
aggregate(datos_seleccionado_pos[, c(2,6)], by = list(cluster = clara_fit$clustering), FUN = mean)

# Catergorical variables
cat("\nDistribución de la Variable Cualitativa Indigena por Cluster:\n")
print(prop.table(table(datos_seleccionado_pos$cluster, datos_seleccionado_pos$Indigena), margin = 1))

datos_seleccionado_pos
## Join clusters to the original dataset
datos_seleccionado_pos$ID <- datos_clustering_nombres1$ID #
datos_resultado <- data.frame(
  ID = datos_seleccionado_pos$ID,
  cluster = clara_fit$clustering
)
base_final_completa <- datos_clustering_nombres1 |>
  left_join(datos_resultado, by = "ID")

base_final_completa

cat("\nTamaño de cada cluster:\n")
print(table(base_final_completa$cluster))

##########################################
# Perform descriptive statistics for continous variables

estadisticos_cluster <- base_final_completa %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    media_Produccion = mean(Produccion, na.rm = TRUE),
    sd_Produccion = sd(Produccion, na.rm = TRUE),
    min_Produccion = min(Produccion, na.rm = TRUE),
    max_Produccion = max(Produccion, na.rm = TRUE),
    media_Rendimiento = mean(Rendimiento, na.rm = TRUE),
    sd_Rendimiento = sd(Rendimiento, na.rm = TRUE),
    min_Rendimiento = min(Rendimiento, na.rm = TRUE),
    max_Rendimiento = max(Rendimiento, na.rm = TRUE),
    media_superficie = mean(Superficie, na.rm = TRUE),
    sd_superficie = sd(Superficie, na.rm = TRUE),
    min_superficie = min(Superficie, na.rm = TRUE),
    max_superficie = max(Superficie, na.rm = TRUE),
  )


estadisticos_cluster

estadisticos_cluster_indice <- base_final_completa %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    media_indice = mean(Indice, na.rm = TRUE),
    sd_indice = sd(Indice, na.rm = TRUE),
    min_indice = min(Indice, na.rm = TRUE),
    max_indice = max(Indice, na.rm = TRUE),
  )


estadisticos_cluster_indice

# Percentages by cluster
#--Indigenous
Porcentaje_indigenas <-  base_final_completa %>%
  group_by(cluster,Indigena) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_indigenas)

#--Irrigation
Porcentaje_riego <-  base_final_completa %>%
  group_by(cluster, Riego) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_riego)


#-Selfconsumption
Porcentaje_autoconsumo <-  base_final_completa %>%
  group_by(cluster, Autoconsumo) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_autoconsumo)

#-For sale
Porcentaje_venta <-  base_final_completa %>%
  group_by(cluster, Venta) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_venta)

#-Improved seeds
Porcentaje_semilla_mejorada <-  base_final_completa %>%
  group_by(cluster, Semilla_mejorada) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_semilla_mejorada)

#-Fertilizers
Porcentaje_fertilizantes <-  base_final_completa %>%
  group_by(cluster, Fertilizantes) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_fertilizantes)

#-Draft animals
Porcentaje_animales_tiro <-  base_final_completa %>%
  group_by(cluster, Animales_tiro) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_animales_tiro)

#-Herbicides
Porcentaje_herbicidas <-  base_final_completa %>%
  group_by(cluster,Herbicidas) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_herbicidas)

#-Insecticides/fungicides
Porcentaje_insecticidas <-  base_final_completa %>%
  group_by(cluster,Insecticidas_fungicidas) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_insecticidas)

#-Use of Tractor

Porcentaje_tractor <-  base_final_completa %>%
  group_by(cluster,Tractor) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_tractor)

#-Use of other machinery
Porcentaje_otra_maquinaria <-  base_final_completa %>%
  group_by(cluster,Otra_maquinaria) %>% 
  summarise(n = n()) %>%
  mutate(porcentaje = n / sum(n) * 100)
print(Porcentaje_otra_maquinaria)


#Ccomparing groups by means

ggboxplot(base_final_completa, x = "cluster", y = "Superficie", 
          color = "cluster", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "Hectáreas", xlab = "Grupo")
ggboxplot(base_final_completa, x = "cluster", y = "Produccion", 
          color = "cluster", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "Toneladas", xlab = "Grupo")

ggboxplot(base_final_completa, x = "cluster", y = "Indice", 
          color = "cluster", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "Magnitud", xlab = "Grupo")

ggboxplot(base_final_completa, x = "cluster", y = "Rendimiento", 
          color = "cluster", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "ton/ha", xlab = "Grupo")
base_final_completa$cluster <- as.factor(as.factor(base_final_completa$cluster))
#a) plot area
res.aovS <- aov(Superficie ~ cluster, data = base_final_completa )
summary(res.aovS)
TukeyHSD(res.aovS)
plot(res.aovS, 2)#
#Kuskal
kruskal.test(Superficie ~ cluster, data = base_final_completa )
pairwise.wilcox.test(base_final_completa$Superficie, base_final_completa$cluster,
                     p.adjust.method = "BH")
# b) Production
res.aovP <- aov(Produccion ~ cluster, data = base_final_completa )
summary(res.aovP)
TukeyHSD(res.aovP)
plot(res.aovP, 2)# Kruskal
#Kuskal
kruskal.test(Produccion ~ cluster, data = base_final_completa )
pairwise.wilcox.test(base_final_completa$Produccion, base_final_completa$cluster,
                     p.adjust.method = "BH")
# b) Yield
res.aovR <- aov(Rendimiento ~ cluster, data = base_final_completa )
summary(res.aovR)
TukeyHSD(res.aovR)
plot(res.aovR, 2)# Kruskal
#Kuskal
kruskal.test(Rendimiento ~ cluster, data = base_final_completa )
pairwise.wilcox.test(base_final_completa$Rendimiento, base_final_completa$cluster,
                     p.adjust.method = "BH")
head(base_final_completa)

###############################################################################
#------------------------------------------------------------------------------
# 3. PERFORM THE SIMULATION FOR THE ARQUETYPES
#------------------------------------------------------------------------------
#We assign labels by cluster instead using numbers. This is to perform
# the analysis of arquetypes.

#
base_final_completa <- as.data.frame(base_final_completa)
base_final_completa
base_final_completa$cluster <- factor(base_final_completa$cluster,
                                             levels=c(1,2,3),
                                             labels = c("Traditional","Transitional", "Intensified"))
print(base_final_completa)
str(base_final_completa$cluster)
#rename the database

base_productores_cluster <- as.data.frame(base_final_completa)
head(base_productores_cluster)
# Install packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales) #

# We use this final data

base_productores_cluster
###############################################
# 3.1.) Simulation of other financial indicators by farmer

# Here we define the distribution for the financial variables and for each unit of 
# analysis (smallholder farmer)

resultados_simulacion <- base_productores_cluster %>%
  mutate(
    # 1. Simlation of variables
    
    #Maize price ($/kg).
    precio_venta_kg = case_when(
      cluster == "Traditional"   ~ runif(n(), min = 9, max = 12),# Precio de venta en la localidad por ser maíz criollo
      cluster == "Transitional"   ~ runif(n(), min = 7, max = 10),# precio de venta en la localidad puede ser maíz híbrido o criollo
      cluster == "Intensified" ~ runif(n(), min = 6, max = 9.20) # Precio de venta a intermediarios o en la localidad directo con el consumidor o molino
    ),
    
    # Variable costs ($/ha). +/- 15% higher in the intensive system.
    costo_variable_ha = case_when(
      cluster == "Traditional"   ~ runif(n(), min = 13991, max = 18930),
      cluster == "Transitional"   ~ runif(n(), min = 9907, max = 13404),
      cluster == "Intensified" ~ runif(n(), min = 17777, max = 28110)
    ),
    
    # Fixed costs ($/ha).
    costo_fijo_ha = case_when(
      cluster == "Traditional"   ~ runif(n(), min = 419, max = 568),
      cluster == "Transitional"   ~ runif(n(), min = 510, max = 690),
      cluster == "Intensified" ~ runif(n(), min = 901, max = 1220)
    ),
    
    #Income/maize grain/system ($/ha)
    ingreso_maiz_ha = case_when(
      cluster == "Traditional"   ~ runif(n(), min = 9600, max = 14400),
      cluster == "Transitional"   ~ runif(n(), min = 10000, max = 15000),
      cluster == "Intensified" ~ runif(n(), min = 13056, max = 19584)
    ),
    
    #Income/byproducts/system ($/ha)
    ingreso_sub_ha = case_when(
      cluster == "Traditional"   ~ runif(n(), min = 20480, max = 30721),
      cluster == "Transitional"   ~ runif(n(), min = 7920, max = 11880),
      cluster == "Intensified" ~ runif(n(), min = 6060, max = 9090)
    ),
    
    # Financial results (Income/costs/netprofit)
    ingresos_maiz=(ingreso_maiz_ha * Superficie),
    ingresos_maiz2 =(precio_venta_kg*Produccion),
    ingresos_sub =(ingreso_sub_ha * Superficie),
    ingresos_totales = (ingresos_maiz2 + ingresos_sub),
    costos_totales = (costo_variable_ha + costo_fijo_ha) * Superficie,
    utilidad_neta = ingresos_totales - costos_totales,
    utilidad_neta_maiz= ingresos_maiz2 - costos_totales,
    
    # Net profit by ha
    utilidad_neta_ha_maiz = utilidad_neta_maiz/Superficie,
    utilidad_neta_ha = utilidad_neta/Superficie
  )

head(resultados_simulacion)

#-----------------------------------------------------------------------
# Aggregate analysis
#-----------------------------------------------------------------------

# Summary (aggregate)
resumen_agregado <- resultados_simulacion %>%
  summarise(
    utilidad_total = sum(utilidad_neta),
    utilidad_promedio_productor = mean(utilidad_neta),
    mediana_utilidad = median(utilidad_neta),
    desv_est_utilidad = sd(utilidad_neta),
    productores_con_perdidas = sum(utilidad_neta < 0),
    porcentaje_con_perdidas = mean(utilidad_neta < 0) * 100,
    porcentaje_debajo_promedio= mean(utilidad_neta < 1969.084)
  )
resultados_simulacion

print("--- ANÁLISIS AGREGADO ---")
print(resumen_agregado)


# Visualization Histogram utility by unit of analysis (27,596)
g1 <- ggplot(resultados_simulacion, aes(x = utilidad_neta)) +
  geom_histogram(bins = 100, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(utilidad_neta)), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(utilidad_neta)), color = "blue", linetype = "dotted", linewidth = 1) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    x = "Utilidad Neta por Productor por ciclo",
    y = "Número de Productores"
  ) +
  theme_light()
g1
#-----------------------------------------------------------------------
# Summary by class (per ha)
resumen_segmentado <- resultados_simulacion %>%
  group_by(cluster) %>%
  summarise(
    numero_productores = n(),
    utilidad_promedio_f = mean(utilidad_neta),
    utilidad_prmedio_f_sd = sd(utilidad_neta),
    mediana_utilidad_f = median(utilidad_neta),
    porcentaje_con_perdidas_f = mean(utilidad_neta < 0) * 100,
    porcentaje_con_perdidas_maiz_f=mean(utilidad_neta_maiz < 0)*100
  ) %>%
  arrange(desc(utilidad_promedio_f))

print("--- ANÁLISIS SEGMENTADO ---")
print(resumen_segmentado)

#Summary by class (per ha)
resumen_segmentado_ha <- resultados_simulacion %>%
  group_by(cluster) %>%
  summarise(
    numero_productores = n(),
    utilidad_promedio_ha = mean(utilidad_neta_ha),
    utilidad_prmedio_ha_sd = sd (utilidad_neta_ha),
    mediana_utilidad_ha = median(utilidad_neta_ha),
    porcentaje_con_perdidas_ha = mean(utilidad_neta_ha < 0) * 100,
    porcentaje_con_perdidas_maiz_ha=mean(utilidad_neta_ha_maiz < 0)*100
  ) %>%
  arrange(desc(utilidad_promedio_ha))

print("--- ANÁLISIS SEGMENTADO ---")
print(resumen_segmentado_ha)

# Boxplot by system
g2 <- ggplot(resultados_simulacion, aes(x = cluster, y = utilidad_neta_ha, fill = cluster)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Comparación de la Utilidad Neta por Hectárea",
    subtitle = "Análisis por Sistema de Producción",
    x = "Sistema de Producción",
    y = "Utilidad Neta por Hectárea"
  ) +
  theme_light() +
  theme(legend.position = "none")
g2
#Ordering the boxplot

# Define the order
orden_personalizado <- c("Intensified", "Transitional", "Traditional")# 

# Convert 'cluster' as a factor using the previous order

resultados_simulacion$cluster <- factor(resultados_simulacion$cluster, levels = orden_personalizado)

# plot using the order
ggplot(resultados_simulacion, aes(x = cluster, y = utilidad_neta_ha, fill = cluster)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) + # Aseg?rate de cargar la librer?a 'scales'
  labs(
    title = "Comparaci?n de la Utilidad Neta por Hect?rea",
    subtitle = "An?lisis por Sistema de Producci?n",
    x = "Sistema de Producci?n",
    y = "Utilidad Neta por Hect?rea"
  ) +
  theme_light() +
  theme(legend.position = "none")


head(resultados_simulacion)

# Plot by categories of income

categorias <- resultados_simulacion[,c("cluster","utilidad_neta_ha","utilidad_neta_ha_maiz")]
categorias

utilidad_neta_ha <- categorias$utilidad_neta_ha

# Delete outliers
Q1 <- quantile(utilidad_neta_ha, 0.25)
Q3 <- quantile(utilidad_neta_ha, 0.75)
IQR_val <- IQR(utilidad_neta_ha)

# Identify outliers
limite_inferior <- Q1 - 1.5 * IQR_val
limite_superior <- Q3 + 1.5 * IQR_val

# Filter the dataframe
categorias <- subset(categorias, utilidad_neta_ha >= limite_inferior & utilidad_neta_ha <= limite_superior)
categorias

categorias1 <- categorias

### Get the boxplot

datos_long <- categorias1 %>%
  pivot_longer(
    cols = c(utilidad_neta_ha, utilidad_neta_ha_maiz), # Nombres de las columnas continuas a pivotar
    names_to = "tipo_variable",       # Nombre de la nueva columna que contendrá los nombres originales
    values_to = "valor"               # Nombre de la nueva columna que contendrá los valores
  )

# Check the data
head(datos_long)
str(datos_long)
summary(datos_long)
g3 <- ggplot(datos_long, aes(x = tipo_variable, y = valor, fill = tipo_variable)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ cluster, scales = "fixed") + # Un panel para cada categoría, con ejes Y libres
  labs(
    x = "Tipo de ingresos",
    y = "Utilidad_neta",
  ) +
  theme_light() 
g3
library(patchwork)
g1 + g3


# Comparing means 
# Utilidad neta/ha
res.aovU <- aov(utilidad_neta_ha ~ cluster, data = resultados_simulacion )
summary(res.aovU)
TukeyHSD(res.aovU)
plot(res.aovU, 2)#T Kruskal
#Kuskal
kruskal.test(utilidad_neta_ha ~ cluster, data = resultados_simulacion )
pairwise.wilcox.test(resultados_simulacion$utilidad_neta_ha, resultados_simulacion$cluster,
                     p.adjust.method = "BH")
#Netprofit only maize grain

res.aovU <- aov(utilidad_neta_ha_maiz ~ cluster, data = resultados_simulacion )
summary(res.aovU)
TukeyHSD(res.aovU)
plot(res.aovU, 2)#Tampoco tiene distribución normal, aplicamos entonces la prueba de Kruskal
#Kuskal
kruskal.test(utilidad_neta_ha_maiz ~ cluster, data = resultados_simulacion )
pairwise.wilcox.test(resultados_simulacion$utilidad_neta_ha_maiz, resultados_simulacion$cluster,
                     p.adjust.method = "BH")

#------------------------------------------------------------------------------
# 4. ANALYSIS OF INTENSIFICATION INDEX AND NETPROFIT
# -----------------------------------------------------------------------------
#I want to plot the Intensification Index and Profitability. The graph should
# show a theoretical curve to demonstrate how, in this type of traditional system, 
# profitability decreases significantly with greater intensification.

library(ggpubr)
#install.packages("ggExtra")
#install.packages("ggpmisc")
#install.packages("ggside")
#install.packages("ggstatsplot")
library(ggpmisc)
library(ggExtra)
library(ggside)
library(ggstatsplot)
library(MASS)
library (robustbase)
library(ggpubr)
library(mgcv)
library(RColorBrewer)
library(forcats)

#1. We select the list of practices considered within the intensification index

practicas <- resultados_simulacion[,c("Riego","Semilla_mejorada","Fertilizantes","Animales_tiro","Herbicidas", "Insecticidas_fungicidas", "Tractor","Otra_maquinaria" )]
practicas

#2. Data long format
datos_largos <- practicas %>%
  pivot_longer(
    cols = everything(), # Selecciona todas las columnas
    names_to = "variable", # Crea una nueva columna 'variable' con los nombres originales
    values_to = "valor"    # Crea una nueva columna 'valor' con los valores de las celdas
  ) %>%
  # The variable should be a factor
  mutate(valor = factor(valor, levels = c(0, 1), labels = c("No", "Sí")))


# 3. Filter to select the cases yes "Sí"
frecuencias_si <- datos_largos %>%
  filter(valor == "Sí") %>% # 
  group_by(variable) %>%    # Grouping by variable
  summarise(conteo = n(), .groups = 'drop')%>%
  # Order
  mutate(variable = fct_reorder(variable, conteo, .desc = TRUE)) # Ordena de mayor a menor

# 3. Barplot "Sí"

graf1 <- ggplot(frecuencias_si, aes(x = variable, y = conteo, fill = variable)) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Dark2") + # O "Set3", "Pastel1", "Paired"
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )+
  labs(
    y = "Frecuency") +
  theme_light()
graf1
# Index distribution

graf2 <- ggplot(resultados_simulacion, aes(x = Indice)) +
  geom_histogram(
    binwidth = 0.1, # Define el ancho de cada barra (bin)
    fill = "skyblue", # Color de relleno de las barras
    color = "black",  # Color del borde de las barras
    alpha = 0.7       # Transparencia de las barras (0 = transparente, 1 = opaco)
  ) +
  labs(
    x = "Intensification index 0= less intensive, 1= high intensive",
    y = "Frecuency"
  ) +
  theme_light() + # 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold") # Center and bold
  )

graf2
###
graf3 <- ggplot(resultados_simulacion, aes(x = Indice, y = Rendimiento)) +
  # Using a Generalized Additive Model function GAM
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "firebrick", linewidth = 1) +
  labs(
    x = "Intensification index",
    y = "Yield (t/ha)"
  ) +
  theme_light(base_size = 15)
graf3

graf4 <- ggplot(resultados_simulacion, aes(x = Indice, y = utilidad_neta_ha)) +
  # Using GAM
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "firebrick", linewidth = 1) +
  labs(
    x = "Íntensification index",
    y = "Net profit ($MXN)"
  ) +
  theme_light(base_size = 15)
graf4

# Combining the figures
library(patchwork)
graf2 + graf1+ graf3 + graf4


ggplot(resultados_simulacion, aes(x = Indice, y = utilidad_neta)) +
  # Ajustando un polinomio de segundo grado (una curva)
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "darkgreen", linewidth = 1) +
  labs(
    title = "Ajuste con Regresión Polinómica (Grado 2)",
    x = "Índice de intensificación",
    y = "Utilidad Neta por ha"
  )
################################### END ########################################
