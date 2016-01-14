library(maps)
library(cartography)
library(dplyr)

currentCountry <- "Former USSR"
# Europe
# USA
# India
# China
# Brazil
# South Africa

maxyear <- BRICS %>%
    group_by(system) %>%
    summarise(yearmax = max(year))

lastPops <- BRICS %>%
    semi_join(maxyear, by = c("system",  "year" = "yearmax")) %>%
    filter(pop > 10E3,!is.na(pop))



maxPop <- max(lastPops$pop)


currentPops <- as.data.frame(lastPops %>%filter(system == currentCountry),
                             stringsAsFactors = FALSE)


mapString <- currentCountry

maxLat <- max(currentPops$Lat, na.rm = TRUE)
minLat <- min(currentPops$Lat, na.rm = TRUE)

# projCoords <- mapproj::mapproject(x = currentPops$Long,
#                                   y = currentPops$Lat,
#                                   projection = "lambert",
#                                   parameters = c(minLat, maxLat))
# 
# currentPops$projLong <- projCoords$x 
# currentPops$projLat <- projCoords$y
# 
# coordinates(currentPops) <- ~ projLong + projLat
coordinates(currentPops) <- ~Long + Lat

#projString <- sprintf("+proj=lcc +lat_1=%s +lat_0=%s +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", maxLat, minLat)
projString <- "+init=epsg:4326"
proj4string(currentPops) <- CRS(projString)



if (currentCountry == "Former USSR") {
    mapString <- "Georgia|Armenia|Azerbaijan|Belarus|Estonia|Kazakhstan|Kyrgyzstan|Latvia|Lithuania|Moldavia|Russia|Tajikistan|Turkmenistan|Ukraine|Uzbekistan"
} else if (currentCountry == "USA") {
    mapString <- "USA(?!:(Alaska|Hawaii))"
} else if (currentCountry == "Europe") {
    mapString <- "France|Austria|Belgium|Bulgaria|Switzerland|Cyprus|Czech|Germany|Denmark|Estonia|Spain|Finland|Greece|Croatia|Hungary|^Ireland|Italy|Lithuania|Luxembourg|Latvia|Malta|Netherlands|Poland|Portugal|Romania|Sweden|Slovenia|Slovakia|UK:"
} else if (currentCountry == "South Africa"){
    mapString <- "South Africa(?!:)"
}

pdf(file = paste(currentCountry, ".pdf", sep=""),   paper = "a4r")

#map(regions = mapString, projection = "lambert", param=c(minLat, maxLat))
map(regions = mapString)

propSymbolsLayer(
    spdf = currentPops, df = currentPops@data, var = "pop",
    fixmax = maxPop, symbols = "circle", border = "white",
    lwd = 0.1, legend.pos = "topleft", legend.title.txt = "Total population"
)

layoutLayer(
    title = sprintf("Cities in %s", currentCountry), author = "", sources = "",
    scale = 0, frame = TRUE, col = "#688994"
)

dev.off()