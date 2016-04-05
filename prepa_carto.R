# On récupère les lastPops pour chaque système.
library(dplyr)
lastPops <- BRICS %>%
    semi_join({
        group_by(., system) %>% summarise(yearmax = max(year))
        }, by = c("system",  "year" = "yearmax")) %>%
    filter(pop > 10E3,!is.na(pop), system != "China (Historical)")

library(sp)
library(rgdal)

for (currSys in unique(lastPops$system)){
    currPops <- lastPops %>% filter(system == currSys)
    
    currSPDF <- SpatialPointsDataFrame(currPops %>%
                                           select(Long, Lat) %>%
                                           as.data.frame(stringsAsFactors = FALSE),
                                       currPops %>%
                                           as.data.frame(stringsAsFactors = FALSE))
    proj4string(currSPDF) <- CRS("+init=epsg:4326")
    
    writeOGR(obj = currSPDF, dsn = sprintf("prod_data/Shp/", currSys), layer = currSys, driver = "ESRI Shapefile")
}

allSPDF <- SpatialPointsDataFrame(lastPops %>%
                                      select(Long, Lat) %>%
                                      as.data.frame(stringsAsFactors = FALSE),
                                  lastPops %>%
                                      as.data.frame(stringsAsFactors = FALSE))

proj4string(allSPDF) <- CRS("+init=epsg:4326")
writeOGR(obj = allSPDF, dsn = "prod_data/Shp/allPoints", layer = "allPoints", driver = "ESRI Shapefile")