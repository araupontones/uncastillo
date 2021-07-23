library(osmdata)
library(ggplot2)
library(png)
library(cowplot)
library(patchwork)
library(extrafont)
#loadfonts(dev = 'win')

#getbb("Uncastillo Spain")

# sort(available_tags("highway"))
# sort(available_tags("sidewalk"))




#coordinates
bbbox2 <- c(-1.1351,42.35,-1.10, 42.37  )


#caminos
secondary <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'highway', value = "secondary") %>%
  osmdata_sf()


tertiary <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'highway', value = "tertiary") %>%
  osmdata_sf()

unclassified <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'highway', value = "unclassified") %>%
  osmdata_sf()

service <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'highway', value = "service") %>%
  osmdata_sf()

pedestrian <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'highway', value = "pedestrian") %>%
  osmdata_sf()



residential <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'highway', value = "residential") %>%
  osmdata_sf()


track <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'highway', value = "track") %>%
  osmdata_sf()

#castillo
castillo <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'building') %>%
  osmdata_sf()


#campo de futbol
campo <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'sport', value = "soccer") %>%
  osmdata_sf()


#Pisci
pisci <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'sport', value = "swimming") %>%
  osmdata_sf()

#pueblo
pueblo <- opq(bbox = bbbox2) %>% # Chiswick Eyot in London, U.K.
  add_osm_feature(key = 'landuse', value = "residential") %>%
  osmdata_sf()


View(pueblo)
color_streets <- "#7fc0ff"
color_calles <- "#ffbe7f"
color_bg <- "#282828"
color_pueblo <- "#4e4e4e"
font = "Montserrat"
color_font <- "#DBDBDB"

mapa <- ggplot()+
  geom_sf(data = pueblo$osm_polygons,
          size = 1,
          color = color_pueblo,
          fill = color_pueblo)+
  #carretera --------------------------------------------------------
  geom_sf(data = secondary$osm_lines,
          size = 2,
          color = color_calles,
          alpha = .5
          )+
  #calles ----------------------------------------------------------------------
  geom_sf(data = tertiary$osm_lines,
          size = 2,
          color = color_calles,
          alpha = .5)+
  geom_sf(data = residential$osm_lines,
          size = 1,
          color = color_calles,
          alpha = .5
          )+
  geom_sf(data = unclassified$osm_lines,
          size = 1,
          color = color_calles,
          alpha = .5)+
  geom_sf(data = service$osm_lines,
          size = 1,
          color = color_calles,
          alpha = .5)+
  geom_sf(data = pedestrian$osm_polygons,
          size = .5,
          color = alpha(color_calles,.5),
          fill = NA,
          alpha = .5
          )+
  
  #pistas ----------------------------------------------------
  geom_sf(data = track$osm_lines,
          size = .5,
          linetype = "dashed",
          color = color_calles,
          alpha = .5)+
  #futbol
  geom_sf(data = campo$osm_polygons,
          size = 1,
          color = NA,
          fill = color_calles,
          alpha = .5)+
  geom_sf(data = pisci$osm_polygons,
          size = 1,
          color = NA,
          fill = color_calles,
          alpha = .5)+
  geom_sf(data = castillo$osm_polygons,
          size = 1,
          color = NA,
          fill = color_calles,
          alpha = .5)+
  #space for footer
  annotate('rect',
           xmin = -Inf,
           xmax = Inf,
           ymax = 42.355,
           ymin = 42.352,
           color = color_bg,
           fill = color_bg) +

  coord_sf(xlim = c(-1.139, -1.125), 
           ylim = c(42.352, 42.366),
           expand = FALSE) +
  theme(panel.background = element_rect(fill = color_bg, color = color_bg),
        plot.background = element_rect(fill = color_bg,color = color_bg),
        panel.border = element_rect(color = "#DBDBDB", fill = NA, size =3 ),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )


mapa
#42.355

mapa_foot <-mapa +
  geom_text(aes(x = -1.132, y =  42.3545, label = "Uncastillo"), size = 24, family = font, color = color_font) +
  geom_text(aes(x = -1.132, y =  42.3537, label = "Aragón"), size = 15, family = font, color = color_font) +
  geom_text(aes(x = -1.1255, y =  42.3525, label = "pulpodata, 2021"), size =6,hjust =1, vjust =1, family = font, color = color_font)+
  annotate("segment",
           x = -1.136, xend = -1.1345,
           y =  42.3537, yend =  42.3537,
           size = 2,
           color = color_font) +
  annotate("segment",
           x = -1.1295, xend = -1.128,
           y =  42.3537, yend =  42.3537,
           size = 2,
           color = color_font)




  
mapa_foot


exfile <- 'plots/mapa_uncas.png'
ggsave(exfile,
       mapa_foot,
       height = 15,
       width = 12,
       dpi = 360)
