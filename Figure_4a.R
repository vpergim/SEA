library(tidyverse)
library(dplyr)
library(rgdal)
library(ggplot2)
library(maptools)

resultados <- read.csv("sscc_EG2019abril.csv",
                       sep=";",
                       header = T,
                       stringsAsFactors = F,
                       colClasses = c(rep("character",11),rep("integer",76)))
resultados <- unite(resultados,
                    CUSEC,
                    c("COD.PROV", "COD.MUN", "DISTRITO", "SECCION"),
                    sep = "",
                    remove = FALSE)

resultados$TOTAL_VOTOS <- apply(resultados[,c("VOTOS_CANDIDATURAS", "BLANCOS", "NULOS")], 1, sum)
resultados <- mutate(resultados, TP = TOTAL_VOTOS / CENSO.CERA)
resultados <- resultados %>% dplyr::select(CUSEC, ANYO:CCAA, COD.PROV:VOTOS_CANDIDATURAS, TOTAL_VOTOS:TP, AHORA.CANAR:VOX)

espana <- readOGR(dsn = "seccionado_2019", layer = "SECC_CE_20190101")

espana_sin_canarias <- espana[espana$CCA!="05",]
canarias <- espana[espana$CCA == "05",]
canarias <- elide(canarias, shift=c(600000,600000))

ccaa1 <- fortify(model = espana_sin_canarias, region = "CUSEC")
ccaa2 <- fortify(model = canarias, region = "CUSEC")
ccaa <- rbind(ccaa1,ccaa2)

info <- resultados %>% select(CUSEC, TP)
info <- info %>% rename(id = CUSEC)
mapa <- left_join(ccaa, info, by = "id")

pdf("Figure_4a.pdf")
ggplot(mapa, aes(x = long, y = lat, group = group, fill = TP)) +
  geom_polygon(color = NA) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(margin = margin(t = 40, b = -40)),
        legend.position = "none") +
  scale_fill_gradient("",
                      low = "white",
                      high = "black",
                      limits = c(0,1))
dev.off()
