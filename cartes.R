maxyear <- BRICS %>%
    group_by(system) %>%
    summarise(yearmax = max(year))

lastPops <- BRICS %>%
    semi_join(maxyear, by = c("system",  "year" = "yearmax")) %>%
    filter(pop > 10E3,!is.na(pop))



maxPop <- max(lastPops$pop)

currentCountry <- "Former USSR"

currentPops <-
    as.data.frame(lastPops %>%filter(system == currentCountry),
                  stringsAsFactors = FALSE)

coordinates(currentPops) <- ~ Long + Lat
proj4string(currentPops) <- CRS("+init=epsg:4326")
pdf(file = paste(currentCountry,  ".pdf", sep=""), paper = "a4r")

if (currentCountry == "Former USSR") {
    map(regions="Georgia|Armenia|Azerbaijan|Belarus|Estonia|Kazakhstan|Kyrgyzstan|Latvia|Lithuania|Moldavia|Russia|Tajikistan|Turkmenistan|Ukraine|Uzbekistan")
} else if (currentCountry == "USA") {
    map(regions = "USA(?!:(Alaska|Hawaii))")
} else if (currentCountry == "Europe") {
    map(regions = "France|Austria|Belgium|Bulgaria|Switzerland|Cyprus|Czech|Germany|Denmark|Estonia|Spain|Finland|Greece|Croatia|Hungary|^Ireland|Italy|Lithuania|Luxembourg|Latvia|Malta|Netherlands|Poland|Portugal|Romania|Sweden|Slovenia|Slovakia|UK:")
} else if (currentCountry == "South Africa"){
    map(regions = "South Africa(?!:)")
} else {
    map(regions = currentCountry)
}

propSymbolsLayer(
    spdf = currentPops,
    # SpatialPolygonsDataFrame of the countries
    df = currentPops@data,
    # data frame of the regions
    var = "pop",
    # population
    fixmax = maxPop,
    # for comparability
    symbols = "circle",
    # type of symbol
    border = "white",
    # color of the symbols borders
    lwd = 0.1,
    # width of the symbols borders
    legend.pos = "topleft",
    legend.title.txt = "Total population"
)
# Layout plot
layoutLayer(
    title = sprintf("Cities in %s", currentCountry),
    scale = 0,
    frame = TRUE,
    col = "#688994"
) # color of the frame

dev.off()