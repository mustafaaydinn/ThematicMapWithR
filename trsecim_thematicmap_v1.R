##
## R TMap kutuphanesi kullanimi ornegi
## Turkiye secim sonuclari thematicmap analizi
## Hakan SARIBIYIK * Subat 2016
##

library(readxl)    # for reading Excel
library(tmap)

## Veriyi yukleyelim.
city_fips_codes <- read_excel("./data/turkey_codes.xlsx", sheet = "turkey_city_fips_codes")

pop_data <- read_excel("./data/turkey_pop_data.xlsx", sheet = "turkey_pop_data")

haziran2015 <- read_excel("./data/2015secimleri_istatistikleri.xlsx", sheet = "2015haziran")
haziran2015$donem <- "2015-06"

kasim2015 <- read_excel("./data/2015secimleri_istatistikleri.xlsx", sheet = "2015kasim")
kasim2015$donem <- "2015-11"


## veri ile sehir kodlarini birlestirelim.
turkey_pop_data <- merge(pop_data, city_fips_codes, 'Name')
turkey_data_201506 <- merge(turkey_pop_data, haziran2015, by.x='Name', by.y="il")
turkey_data <- merge(turkey_data_201506, kasim2015, by.x='Name', by.y="il", suffixes = c("_06","_11"))
## Harita dosyasini alalim.

f <- tempfile()
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip", destfile = f)
unzip(f, exdir = "./maps")

unlink(f)

## Shape dosyasini bellege yukleyelim.
MyWorldMap <- read_shape("./maps/ne_10m_admin_1_states_provinces.shp")

## SpatialPolygonsDataFrame tipindeki harita yapisindan Turkiye yi secelim.
MyWorldMapTur <- MyWorldMap[(MyWorldMap$sov_a3 %in% c("TUR")),] 

## MyWorldMapTur@data

# Veri ile harita bilgilerini birlestirelim.

MyWorldMapTurDATA <- append_data(MyWorldMapTur, turkey_data, key.shp = "fips", key.data = "FIPS", ignore.na = TRUE)


## Artik verilerimizi harita uzerinde kolayca gosterebiliriz.
qtm(MyWorldMapTurDATA, fill = "Populationinlatestcensus_2000", text="Name", text.size="AREA", root=5, legend.show = FALSE, format="Europe")

#bos turkiye haritasi
tm_shape(MyWorldMapTurDATA) + 
  tm_fill("Red") + 
  tm_borders(alpha=.5)

## Haziran 2015
summary(turkey_data)

## 
tm_shape(MyWorldMapTurDATA) + 
  tm_fill( "Milletvekili_sayisi_06", 
           contrast = .5,
           title="Milletvekili sayisi",
           palette = "Blues",
           style="fixed", 
           breaks=c(-Inf, 2, 3, 4, 6, 8, 10, 20, 30, 40, 50, Inf)
  )   + 
  tm_text("Name", size="AREA", root=5) +
  tm_borders(alpha=.5) +
  tm_layout(asp = 3, 
            scale=.8, 
            legend.show = TRUE,
            inner.margins = c(0, 0.2, 0.025, 0.01), ##bottom, left, top, and right margin
            legend.position = c("left", "bottom"), 
            legend.frame = TRUE,
            legend.width = 0.15,
            legend.text.size = 0.8,
            earth.boundary = TRUE,
            space.color="grey90",
            legend.format = list(text.less.than = " az ",
                                 text.or.more = " den fazla ",
                                 text.separator = " - ")) +
  tm_credits("Hakan SARIBIYIK", position = c("right", "BOTTOM"))


### Dort partinin aldigi oylar,  Haziran ve Kasim secimleri karsilastirmali, turkiye ye dagilimi
partiler <- c("AKP", "CHP", "MHP", "HDP")
renkler <- c("Oranges", "Reds", "Blues", "Purples")


doMap = function(pno) {
  kolon_ismi <- c(paste0(partiler[pno],"_yuzde_06", ""), paste0(partiler[pno],"_yuzde_11", ""))
  mytitle <- c(paste0(partiler[pno],"\n2015 Haziran", ""), paste0(partiler[pno], "\n2015 Kasim", ""))
  renk <- c(paste0(renkler[pno]))
  sekil <- tm_shape(MyWorldMapTurDATA) +
    tm_fill(kolon_ismi,
            title=mytitle, 
            palette=renk,
            style="fixed", 
            breaks=c(-Inf, 1, 5, 10, 25, 30, 40, 50, 60, 70, Inf)) +
    tm_borders() +
    tm_text("Name", size="AREA", root=5) +
    tm_layout(scale=.8, 
              #aes.palette = list(seq = "Green"),
              legend.show = TRUE, 
              inner.margins = c(0, 0.2, 0.025, 0.01),
              legend.position = c("left", "bottom"),
              #legend.frame = "black",
              #legend.width = 0.15,
              legend.text.size = 0.6,
              bg.color="lightblue",
              legend.format = list(text.less.than = " az ",
                                   text.or.more = " dan fazla ",
                                   text.separator = " - ")) +
    tm_credits("Hakan SARIBIYIK", position = c("right", "BOTTOM"))
  
}

lapply(1, doMap) #AKP
lapply(2, doMap) #CHP
lapply(3, doMap) #MHP
lapply(4, doMap) #HDP



### Haziran 2015 dort parti bir seferde dort ayri Turkiye haritasi uzerinde alt alta.
tm_shape(MyWorldMapTurDATA) +
  tm_fill(c("AKP_yuzde_06", "CHP_yuzde_06", "MHP_yuzde_06", "HDP_yuzde_06"), 
          title=c("AKP", "CHP", "MHP", "HDP"), 
          style="fixed", 
          breaks=c(-Inf, 1, 5, 10, 25, 30, 40, 50, 60, 70, Inf)) +
  tm_borders() +
  tm_text("Name", size="AREA", root=5) +
  tm_layout(scale=.9, 
            legend.show = TRUE, 
            inner.margins = c(0, 0.2, 0.025, 0.01),
            legend.position = c("left", "bottom"),
            bg.color="lightblue",
            legend.format = list(text.less.than = " az ",
                                 text.or.more = " dan fazla ",
                                 text.separator = " - ")) +
  tm_credits("Hakan Sarıbıyık", position = c("right", "BOTTOM"))


#######################################################################


## var olan paletler
RColorBrewer::display.brewer.all()




