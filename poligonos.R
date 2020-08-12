rm(list=ls())
gc()


######################Lectura de Librerías###############
library(dplyr)
library(tidyr)
library(sp)
library(raster)
library(rgeos)
library(rgbif)
library(viridis)
library(gridExtra)
library(rasterVis)
library(geojsonsf)
library(rgdal)
library(geojson)
library(mapview)
library(geojsonio)


######################Lectura GeoJson de barrios catastrales de Bogotá###############

barrios <- geojson_read("barrios_catastrales/barrios_catastrales.geojson", what = "sp")

######################Pintar el Mapa###############

mapview(barrios)

######################Lectura GeoJson del shape externo deBogotá###############


Bogota <- rgdal::readOGR("barrios_catastrales/bogota_general.geojson")

######################Pintar el Mapa###############


mapview(Bogota)

######################Función para subdivir un shape en hexagonos###############

make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}


######################Ajustar el sistema de coordenadas del shape para subdividir###############
Bogota_utm <- CRS("+proj=utm +zone=44 +datum=WGS84 +units=km +no_defs") %>% 
  spTransform(Bogota, .)


######################Generar la subdivisión del shape por 500 metros###############
hex_grid <- make_grid(Bogota_utm, cell_area = 0.5, clip = TRUE)

######################Retorno al sistema de coordenadas lat+long###############

Bogota_a<-CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")%>% 
  spTransform(Bogota_utm, .)

######################Retorno al sistema de coordenadas lat+long###############
hex_grid_a<-CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")%>% 
  spTransform(hex_grid, .)

######################Crear ID por polígono###############

hex_grid_a$ID<-1:length(hex_grid_a)



######################Guarga en GeoJson###############
geojson_bogota<-as.geojson(hex_grid_a)

write(geojson_bogota, "barrios_catastrales/bogota_subdivi2.geojson")


###############################################################################

#############
##Asignación de punto a polígono
#############


############Lectura de la subdivisión creada anterior###############

dat <- geojson_read("barrios_catastrales/bogota_subdivi2.geojson", what = "sp")

## plot using mapview

mapview(dat)


############Lectura de los barrios catastrales###############

dat <- geojson_read("barrios_catastrales/barrios_catastrales.geojson", what = "sp")


mapview(dat)


######### Ejemplo de detección de polígono por puntos########## 
data <- data.frame(Longitude = c(-74.0439913,-74.0443877),
                  Latitude =c(4.6710949, 4.6694201),
                  names = c("Comercio 1", "Comercio 2"))

####################### Alinear coordenadas ###########

coordinates(data) <- ~ Longitude + Latitude
proj4string(data) <- proj4string(dat)

##############Generar tabla con los puntos encontrados ###############
A<-over(data, dat)

A
