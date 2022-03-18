load(file="MRT.Routes.RData")
load(file="MRT.Stations.RData")
library(sf)
library(ggplot2)
p3 <- ggplot()+
  geom_sf(data=MRT.Routes, col="SlateBlue", size=0.6)+
  geom_sf(data=MRT.Stations, shape=22, size=3,
          color="olivedrab", fill="goldenrod")+
  theme_void()+coord_sf(ndiscr = 0)
p3
#加上站名
p4 <- ggplot()+
  geom_sf(data=MRT.Routes, col="SlateBlue", size=0.6)+
  geom_sf(data=MRT.Stations, shape=22, size=3,
          color="olivedrab", fill="goldenrod")+
  geom_sf_text(data=MRT.Stations, aes(label=站名), size=3, color="forestgreen")+
  theme_void()+coord_sf(ndiscr = 0)
p4
