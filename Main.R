library(tidyverse)
library(ggtext)
library(patchwork)


data_prop <- read_csv("data/data_propuestas.csv")

# Corregimos tipo de carrera
data_prop <- data_prop %>% 
  mutate(tipo = case_when(
    tipo=="Trayecto"~"Postítulo",
    tipo=="Profesorado"~"Profesorado",
    tipo=="Seminario"~"Curso",
    TRUE ~ "Formación\nacadémica"
  ))

# Por línea formativa
data_lf <- data_prop %>% 
  filter(cursantes_T3 > 0) %>% 
  filter(!carrera_siglas %in% c("CLINICAESCRITURA","MOD_INDIVIDUALES")) %>% 
  group_by(linea_formativa) %>% 
  summarise(cursantes = sum(cursantes_T3),
            propuestas = n_distinct(propuesta))

# Por propuesta 2
propuesta <- data_prop %>% 
  filter(cursantes_T3 > 0) %>% 
  filter(!carrera_siglas %in% c("CLINICAESCRITURA","MOD_INDIVIDUALES")) %>%
  group_by(propuesta) %>% 
  summarise(cursantes = sum(cursantes_T3))

data_lf_tipo <- data_prop %>% 
  filter(cursantes_T3 > 0) %>% 
  filter(!carrera_siglas %in% c("CLINICAESCRITURA","MOD_INDIVIDUALES")) %>% 
  group_by(linea_formativa, tipo) %>% 
  summarise(cursantes = sum(cursantes_T3),
            propuestas = n_distinct(propuesta)) %>% 
  mutate(cursantes_lf = sum(cursantes),
         propuestas_lf = sum(propuestas))


color_lf <- "#5A45A3"
color_tipo <- "#5A45A3"

color_1 <- "#594A4E"
color_2 <- "#DFB9C4"

plot_resumen_lf <- ggplot(data_lf, aes(x = fct_reorder(linea_formativa,cursantes), y = cursantes)) +
  geom_point(aes(size=propuestas), color=color_2) +
  geom_text(aes(label=cursantes), vjust= -1.8, color=color_1, fontface="bold", size=6) +
  geom_text(aes(label=propuestas, size=4*(propuestas*.03)), color="white", fontface="bold") +
  coord_flip(clip="off") +
  scale_size(range = c(6,15)) +
  labs(y="<-- Cantidad de cursantes -->", x = NULL,
       #title = "Cursantes ISEP 2021",
       #subtitle = "Cantidad de <span style='color:red;'>propuestas</span> y cantidad de 
       #<span style='color:blue;'>cursantes activos</span> del año por línea formativa y tipo de propuesta"
       ) +
  guides(size = "none") +
  theme_minimal(base_size = 18) +
  theme(
    axis.title.x = element_text(color=color_1),
    axis.text.x = element_blank(),
    plot.title = element_markdown(),
    plot.subtitle = element_markdown(),
    plot.title.position = "plot",
    axis.text.y = element_text(face="bold", color=color_lf),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

plot_resumen_lf

flechas <- data.frame(x1=c(1.8,1.8), 
                      x2=c(1,1.2), 
                      y1=c(2,2.9), 
                      y2=c(2.9,2.9))

texto <- data.frame(x1=c(1.8), 
                    x2=c(1.2), 
                    y1=c(3), 
                    y2=c(3.1))


# SIMILAR AL PLOT ANTERIOR PERO SE AGREGA LA DIM DE TIPO DE PROPUESTA
plot_resumen_lf_tipo <- ggplot(data_lf_tipo, aes(x = fct_reorder(linea_formativa,cursantes_lf), y = tipo)) +
  geom_point(aes(size=propuestas), color=color_2) +
  geom_text(aes(label=cursantes), vjust= -1.4, color=color_1, fontface="bold", size=6) +
  geom_text(aes(label=propuestas, size=4*(propuestas*.03)), color="white", fontface="bold") +
# geom_curve(
#     aes(x=x1,y=y1,xend=x2,yend=y2),
#     data=flechas,
#     arrow=arrow(length=unit(0.03, "npc")),
#     curvature = .3
#   ) +
#    annotate(
#      "text", x=1, y=3, 
#      label="Acompañamiento a las Trayectorias Escolares en la Educación Secundaria\nTrayectorias Escolares y Enseñanza en el PIT
# ", hjust=0
#    ) +
  coord_flip(clip="off") +
  scale_size(range = c(6,13)) +
  scale_y_discrete(position="right") +
  guides(size = "none") +
  theme_minimal(base_size = 18) +
  theme(
    
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.line.y = element_line(color="grey60"),
    axis.text.x = element_text(face="bold", color = color_tipo, angle = 45, hjust=0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

plot_resumen_lf_tipo


plot_final_lf_tipo <- plot_resumen_lf + plot_resumen_lf_tipo

plot_final_lf_tipo
ggsave("plot_final_lf_tipo.png", width = 19, height = 10, dpi=320)


# PLOT por IFDA
data_ifda <- data_prop %>% 
  filter(cursantes_T3 > 0) %>% 
  filter(!carrera_siglas %in% c("CLINICAESCRITURA","MOD_INDIVIDUALES")) %>% 
  group_by(ifda_sigla) %>% 
  summarise(cursantes = sum(cursantes_T3),
            propuestas = n_distinct(propuesta))


data_ifda_tipo <- data_prop %>% 
  filter(cursantes_T3 > 0) %>% 
  filter(!carrera_siglas %in% c("CLINICAESCRITURA","MOD_INDIVIDUALES")) %>% 
  group_by(ifda_sigla, tipo) %>% 
  summarise(cursantes = sum(cursantes_T3),
            propuestas = n_distinct(propuesta)) %>% 
  mutate(cursantes_ifda = sum(cursantes),
         propuestas_ifda = sum(propuestas))


# PLOT IFDA
color_lf <- "#5A45A3"
color_tipo <- "#5A45A3"

plot_resumen_ifda <- ggplot(data_ifda, aes(x = fct_reorder(ifda_sigla,cursantes), y = cursantes)) +
  geom_point(aes(size=propuestas), color=color_2) +
  geom_text(aes(label=cursantes), vjust= -1.8, color=color_1, fontface="bold", size=7) +
  geom_text(aes(label=propuestas, size=4*(propuestas*.03)), color="white", fontface="bold") +
  coord_flip(clip="off") +
  scale_size(range = c(6,15)) +
  expand_limits(x=c(1,14)) +
  labs(y = NULL, x = NULL,
       #title = "Cursantes ISEP 2021",
       #subtitle = "Cantidad de <span style='color:red;'>propuestas</span> y cantidad de 
       #<span style='color:blue;'>cursantes activos</span> del año por IFDA y tipo de propuesta"
       ) +
  guides(size = "none") +
  theme_minimal() +
  theme(
    axis.text = element_text(size=20),
    axis.title.x = element_text(color=color_1, size=15),
    axis.text.x = element_blank(),
    plot.title = element_markdown(),
    plot.subtitle = element_markdown(),
    plot.title.position = "plot",
    axis.text.y = element_text(face="bold", color=color_lf),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# PLOT IFDA TIPO
plot_resumen_ifda_tipo <- ggplot(data_ifda_tipo, aes(x = fct_reorder(ifda_sigla,cursantes_ifda), y = tipo)) +
  geom_point(aes(size=propuestas), color=color_2) +
  geom_text(aes(label=cursantes), vjust= -1.4, color=color_1, fontface="bold", size=7) +
  geom_text(aes(label=propuestas, size=4*(propuestas*.03)), color="white", fontface="bold") +
coord_flip(clip="off") +
  scale_size(range = c(6,13)) +
  scale_y_discrete(position="right") +
  expand_limits(x=c(1,14)) +
  guides(size = "none") +
  theme_minimal() +
  theme(
    axis.text = element_text(size=20),
    panel.background = element_rect(fill="#F2F2F2", color=NA),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    #axis.line.y = element_line(color="grey60"),
    axis.text.x = element_text(face="bold", color = color_tipo),
    axis.text.x.top = element_text(vjust=.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="grey80")
  )

plot_final_ifda_tipo <- plot_resumen_ifda + plot_resumen_ifda_tipo
plot_final_ifda_tipo
ggsave("plot_final_ifda_tipo.png", plot_final_ifda_tipo, width = 20, height = 13, dpi=320)

# Pruebas VIZ crecimiento 
data_anios <- read_csv("data/data_anios.csv") %>% 
  mutate(tipo = case_when(
    tipo=="Trayecto"~"Postítulo",
    tipo=="Profesorado"~"Profesorado",
    tipo=="Seminario"~"Curso",
    TRUE ~ "Formación\nacadémica"
  )) %>% 
  mutate(anio=as.factor(anio),
         anio=factor(anio, levels=c("2016","2017","2018","2019","2020","2021"))) %>% 
  filter(anio %in% c(2016,2019,2020,2021)) %>% 
  filter(cursantes_T3 > 0) %>% 
  filter(!carrera_siglas %in% c("CLINICAESCRITURA","MOD_INDIVIDUALES","TEST_TRAYECTO"))

data_anios <- data_anios %>% 
  group_by(anio) %>% 
  summarise(cursantes = sum(cursantes_T3),
            propuestas_vigentes = n_distinct(propuesta))


plot_evol <- ggplot(data_anios, aes(x=anio, y=cursantes, group=1)) +
  annotate("rect", xmin=1,xmax=4, ymin=-Inf, ymax=Inf, alpha=.2, fill="grey60") +
  annotate("rect", xmin=4,xmax=6, ymin=-Inf, ymax=Inf, alpha=.2, fill="lightgreen") +
  geom_line(linetype="dashed") +
  geom_point(aes(size=cursantes)) +
  geom_label(aes(label=cursantes, hjust=ifelse(cursantes<7000, -.5, 1.5))) +
  scale_x_discrete(drop=FALSE, labels=c("2016","","","2019","2020","2021"), expand = c(0.01,0.01)) +
  coord_cartesian(clip="off") +
  scale_size(range = c(4,12)) +
  guides(size="none") +
  labs(x=NULL,y=NULL,
       title="Crecimiento en la cantidad de cursantes desde el inicio hasta los últimos 3 años") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(face="bold"),
    plot.title = element_text(margin = margin(0,0,20,0)),
    plot.title.position = "plot",
    plot.background = element_rect(fill="white")
  )

plot_evol


ggsave("plot_evol.png", plot_evol, width = 10, height = 10, dpi=320)


# Egresados
data_egresados <- read_csv("data/data_matricula_egresos.csv")


data_egresados <- data_egresados %>% 
  select(linea_formativa, propuesta, carrera_siglas, tipo, ifda_siglas, egresados_intermedio, egresados, anio) %>% 
  mutate(tipo = case_when(
    tipo=="Trayecto"~"Postítulo",
    tipo=="Profesorado"~"Profesorado",
    tipo=="Seminario"~"Curso",
    TRUE ~ "Formación\nacadémica"
  ))


data_egresados_tipo <- data_egresados %>% 
  mutate(
    anio_actual = as.factor(anio==2021),
    anio_actual = factor(anio_actual, levels=c("TRUE", "FALSE")),
    egresados_totales = egresados + egresados_intermedio
  ) %>% 
  group_by(tipo, anio_actual) %>% 
  summarise(egresados_tipo = sum(egresados_totales))
  
  
plot_egresados_1 <- ggplot(data_egresados_tipo, aes(x=tipo, y=egresados_tipo, fill=anio_actual)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("#594A4E","#DFB9C4")) +
  theme_minimal(base_size = 16) +
  labs(
    x=NULL,
    y=NULL
  ) +
  guides(
    fill="none"
  ) +
  theme(
    panel.grid = element_blank()
  )

plot_egresados_1

ggsave("plot_egresados.png", width = 10, height = 10, dpi=320)
