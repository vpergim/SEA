library(rgdal)
library(maptools)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)

shapefile_ccaa <- readOGR(dsn = "CCAA_ETRS89_30N", layer = "Comunidades_Autonomas_ETRS89_30N")

espana_sin_canarias <- shapefile_ccaa[shapefile_ccaa$Codigo!="05",]
canarias <- shapefile_ccaa[shapefile_ccaa$Codigo == "05",]
canarias <- elide(canarias, shift=c(600000,600000))

ccaa1 <- fortify(model = espana_sin_canarias, region = "Codigo")
ccaa2 <- fortify(model = canarias, region = "Codigo")
ccaa <- rbind(ccaa1,ccaa2)

info_ccaa <- read_excel("porcentajes información por CCAA.xlsx")
info_ccaa <- info_ccaa %>% rename(id = CCA)

mapa <- left_join(ccaa, info_ccaa, by = "id")

pdf("mapa.pdf")
ggplot(mapa, aes(x = long, y = lat, group = group, fill = Porcentaje_info)) +
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