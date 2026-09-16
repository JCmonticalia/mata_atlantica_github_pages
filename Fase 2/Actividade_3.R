##DATA: 31/08/2026


install.packages("rgbif")
library(rgbif)

library(tidyverse)
library(rgbif)
library(sf)
library(ggplot2)

# GBIF GENERAL


datos <- occ_search(
  scientificName = "Monticalia corymbosa",
  limit = 100000
)$data


# GBIF OCURRENCIA - COMPLEJO MONTICALIA-PENTACALIA

monticalia_gbif <- occ_data(
  scientificName = "Monticalia corymbosa",
  hasCoordinate = TRUE,
  hasGeospatialIssue = FALSE
)

monticalia_gbif$data

# dados filtrajem 1

dados <- monticalia_gbif$data


## inpeccionar dados

dim(dados)

names(dados)

##taxonomia 

unique(dados$scientificName)


#registro pelo ano

dados %>%
  count(year) %>%
  arrange(year)

# figura registros por ano

ggplot(dados, aes(x = year)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Ano",
    y = "Número de ocorrências"
  ) +
  theme_bw()



### qualidadde de dados

dados$issues

table(dados$issues)

dados_limpios <- dados %>%
  filter(
    !grepl(
      paste(issues_graves, collapse = "|"),
      issues
    )
  )

#excluir espaciales 

hasGeospatialIssue = FALSE

# verificar las coordenadas

summary(dados$decimalLatitude)
summary(dados$decimalLongitude)

# coordenadas
ggplot(dados,
       aes(x = decimalLongitude,
           y = decimalLatitude)) +
  geom_point() +
  coord_fixed() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = ""
  ) +
  theme_classic()

## MAPA COLOMBIA

library(ggplot2)
library(sf)
library(dplyr)
library(maps)

mundo <- map_data("world")

colombia <- mundo %>%
  filter(region == "Colombia")

ggplot() +
  geom_polygon(
    data = colombia,
    aes(x = long, y = lat, group = group),
    fill = "grey",
    color = "grey"
  ) +
  geom_point(
    data = dados_limpios,
    aes(
      x = decimalLongitude,
      y = decimalLatitude
    ),
    size = 2,
    alpha = 0.7
  ) +
  coord_fixed() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = ""
  ) +
  theme_classic()


# Descaragar data pentacalia corymbosa

dados_csv <- dados

dados_csv[] <- lapply(dados_csv, function(x) {
  if (is.list(x)) {
    sapply(x, function(y) {
      if (length(y) == 0 || is.null(y)) {
        NA_character_
      } else {
        paste(y, collapse = "; ")
      }
    })
  } else {
    x
  }
})

write.csv(
  dados_csv,
  "Monticalia_corymbosa_GBIF.csv",
  row.names = FALSE,
  na = ""
)

# Problemas no reportados 

library(bdc)
library(CoordinateCleaner)


# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = dados,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- dados %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", "seas", 
                                                 "zeros"))


####mapa sin problemas

ggplot() +
  geom_polygon(
    data = colombia,
    aes(x = long, y = lat, group = group),
    fill = "#C1FFC1",
    color = "#C1FFC1"
  ) +
  geom_point(
    data = cl,
    aes(
      x = decimalLongitude,
      y = decimalLatitude
    ),
    size = 2,
    alpha = 0.7
  ) +
  coord_fixed() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = ""
  ) +
  theme_classic()


#### ocurrencia por año, municipio, departamento


cl %>%
  count(stateProvince, sort = TRUE) %>%
  ggplot(aes(x = reorder(stateProvince, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Departamento",
    y = "Número de registros"
  ) +
  theme_classic()


cl %>%
  count(municipality, sort = TRUE) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(municipality, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Municipio",
    y = "Número de registros"
  ) +
  theme_classic()



