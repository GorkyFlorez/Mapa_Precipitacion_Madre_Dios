#  Cargamos las Librerias ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse,rnaturalearth,rnaturalearthdata,
               sf, reticulate,maptools,maps, ggplot2 ,ggspatial, rgeos, ggmap , leaflet)

# Cargammos los SHp del Peru ---------------------------------------------------------------
Peru_Depa   <- getData('GADM', country='Peru', level=3) %>%
  st_as_sf() 
MDD         <-subset(Peru_Depa , NAME_1 == "Madre de Dios") 
lbl         <- data.frame(month_abb = month.abb, mes = 1:12)
MDD_xy <- cbind(MDD, st_coordinates(st_centroid(MDD$geometry)))
ggplot() + geom_sf(data = MDD) + theme_bw()

MDD         <- MDD %>% st_transform (crs = 4326)
leaflet(MDD) %>% addTiles() %>% addPolygons()
# Extraemos los datos raster de Precipitacion -----------------------------------------------
st_centroid(MDD)

Prec        <- getData("worldclim", var = "prec", 
                       res=0.5, lon=-74.8773, lat=-11.54012)
Prec_MDD    <- crop(Prec, MDD)
Prec_MDD    <- Prec_MDD <- mask(Prec_MDD,MDD)
PPAnual_MDD <- do.call("sum", unstack(Prec_MDD))
plot(PPAnual_MDD)
# Elaboramos los meses Para precipitacion-----------------------------------------
vls         <- rasterToPoints(Prec_MDD) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(mes = parse_number(var)) %>% 
  inner_join(., lbl, by = 'mes') %>% 
  dplyr::select(x, y, month_abb, value) %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))

vls %>% 
  filter(month_abb == 'Jan')
summary(vls$value)
# Elaboramos los mapas  ----------------------------------------------------------
colores2<- c('#9331dc', '#165dff', '#10aebe', '#00ffff', '#ffee21', '#f19e21', '#ff4223')
colores1<- c('#ff4223','#f19e21','#ffee21','#00ffff', '#10aebe', '#165dff','#9331dc')

gg          <- ggplot(vls)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  facet_wrap(~ month_abb) +
  scale_fill_gradientn(colours = colores1, 
                       na.value = 'white', limits = c(0, 870), breaks = seq(0, 870, 50)) +
  geom_sf(data = MDD, fill = NA, color = 'black', size = 0.2)+
  theme_bw() +
  geom_sf_text(data = MDD_xy , aes(x= X, y=Y, label = NAME_3), size =2.5, color="black"
               ,fontfamily = "serif",  fontface="italic")+
  scale_x_continuous(breaks = c(-72.5, -71.0, -69.5)) +
  labs(title = 'Precipitación mensual - Madre de Dios', fill = 'mm',  x = 'Longitud', y = 'Latitud', caption = "Gorky Florez") +
  theme(legend.position = 'bottom',
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(5, 'line'),
        panel.border = element_rect(size = 2, color="white"),
        axis.text.x  = element_text(face="bold", color="white", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="white", size=8),
        strip.text=element_text(family='Anton', face='bold', size=14, hjust=0, color='white'),
        strip.background=element_rect(fill='black'),
        plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "#4e4d47", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "springgreen", family="serif", face = "italic")) +
  guides(shape = guide_legend(override.aes = list(size = 10)))+
  ggtitle("Precipitación mensual - Departamento de Madre de Dios ")+
  labs(subtitle="Madre de Dios capital de la Biodiversidad del Peru", 
       caption="Fuente: https://osmdata.openstreetmap.de/")+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")


# Saving the plot
ggsave(plot = gg, filename = 'Mapa/mapas_prec_MDD1.png', width = 13, height = 13, units = 'in', dpi = 1300)
