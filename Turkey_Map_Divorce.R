setwd("C:/Users/USER/Desktop/R project/Project")
install.packages("tidyverse")
install.packages("sp") #for spatial data
install.packages("map")
install.packages("mapproj")
install.packages("ggmap")
install.packages("DeducerSpatial")
install.packages("maps")
require(maps)
require(tidyverse)
library(sp)
tur<- readRDS("turkey.rds")
plot(tur)
tur@data %>% as_tibble() %>% head(10)
ggplot(tur,aes(x=long,y=lat))+geom_polygon()
ggplot(tur, aes(x = long, y = lat)) + geom_polygon(aes(group = group)) + coord_fixed()
tur_for <- fortify(tur)
head(tur_for)
ggplot(tur_for) + geom_polygon(aes(x = long, y = lat,group = group),color = "white",fill = "red") + theme_void() + coord_fixed()
data= read.csv("Divorce.csv", header=TRUE, sep=";")
t_data <-t(data)
colnames(t_data) <- t_data[1,]
t_data <- t_data[-1, ]
df = subset(t_data, select = -c(1:17) )
write.csv(df, file = "transposeddiv.csv",row.names=TRUE)
data= read.csv("transposeddiv.csv", header= TRUE, sep=",")
names(data) <- c("sehir", "bosanma")
data$sehir <- gsub("\\..*","",data$sehir)
turkceden_ingilizceye <- function(dataset){
  turkce_harfler<- c("Ç","Þ","Ð","Ý","Ü","Ö","ç","þ","ð","ý","ü","ö")
  ingilizce_harfler<- c("C","S","G","I","U","O","c","s","g","i","u","o")
  dataset=mgsub(turkce_harfler,ingilizce_harfler,dataset)
  return(dataset)
}
mgsub <- function(pattern, replacement, x, ...) {
        n = length(pattern)
        if (n != length(replacement)) {
            stop("pattern and replacement do not have the same length.")
        }
        result = x
        for (i in 1:n) {
          result <- gsub(pattern[i],replacement[i],result)
        }
        return(result)
}
tur@data$NAME_1 <- turkceden_ingilizceye(tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("K. Maras", "Kahramanmaras",tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Kinkkale","Kirikkale",tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Zinguldak", "Zonguldak", tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Afyon","Afyonkarahisar", tur@data$NAME_1 )
data$sehir=turkceden_ingilizceye(data$sehir)
data%>%as_tibble
id_and_cities<- data_frame(id = rownames(tur@data), sehir = tur@data$NAME_1) %>% left_join(data, by = "sehir")
head(id_and_cities)
final_map <- left_join(tur_for, id_and_cities, by = "id")
head(final_map)
ggplot(final_map) +geom_polygon( aes(x = long, y = lat, group = group, fill = bosanma), color = "grey") +    coord_map() +theme_void() + labs(title = "Türkiye'nin Ýllere göre Boþanma Sayýlarý",caption = "Kaynak: Türkiye Istatistik Kurumu") +
  scale_fill_distiller(name = "Boþanma Sayýlarý",palette = "Spectral", limits = c(0,30500), na.value = "white") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

