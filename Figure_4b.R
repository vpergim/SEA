library(tidyverse)
library(dplyr)
library(rgdal)
library(ggplot2)
library(maptools)

resultados <- read.csv("Euskadi2020.csv",
                       sep=";",
                       header = T,
                       stringsAsFactors = F,
                       colClasses = c(rep("character",9),rep("integer",25)))

colnames(resultados)[1] <- "CUSEC"

resultados <- mutate(resultados, TP = (VOTANTES / CENSO)*100)
resultados <- resultados %>% dplyr::select(CUSEC:VOTANTES, TP, NULOS:RECORTES.CERO.LV.M)

unzip(zipfile = "seccionado_2020.zip", exdir = "./seccionado_2020")
espana <- readOGR(dsn = "seccionado_2020", layer = "SECC_CE_20200101")
euskadi <- espana[espana$CCA=="16",]

euskadi@data <- left_join(euskadi@data, resultados, by = "CUSEC")

writeOGR(euskadi, dsn = "seccionado_2020_euskadi", layer = "SECC_CE_20200101", driver = "ESRI Shapefile")

mapa_euskadi <- fortify(model = euskadi, region = "CUSEC") # convertimos el archivo shapefile en un df
info <- euskadi@data[,c(1,31)]
info <- info %>% rename(id = CUSEC)
mapa_euskadi_ext <- left_join(mapa_euskadi, info, by = "id")

pdf("Figure_4b.pdf")
ggplot(mapa_euskadi_ext, aes(x = long, y = lat, group = group, fill = TP)) +
  geom_polygon(color = NA) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(margin = margin(t = 40, b = -40)),
        legend.position = c(0.9, 0.15)) +
  scale_fill_gradient("Participation\nrate (%)",
                      low = "white",
                      high = "black",
                      limits = c(0,100))
dev.off()
