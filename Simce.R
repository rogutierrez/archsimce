# Para leer el xlsx simce2m2018_rbd_publica_final
library("readxl")
Simce2do <- read_excel(file.choose(), sheet = "myexcelsheet")


library(tidyverse)

#####################################################################################################
#####################################################################################################
# Gráfico de los puntajes del Simce-Matemáticas en Talca

Talca <- filter(Simce2do, nom_com_rbd == "Talca")                       # Filtramos la ciudad de Talca

ggplot(data = Talca, mapping = aes(x = nom_rbd, y = prom_mate2m_rbd)) +
  geom_point() +
  theme(axis.text = element_text(angle = 90,                            # Rotar 90 grados la etiqueta
                                 size = 6)) +                           # Achicar tamaño de la letra
  labs(x = "Nombre Colegio", y = "Puntaje promedio Matemáticas 2018") + # Cambiar el nombre de los ejes
  ggtitle("Simce") +                                                    # Título del gráfico
  theme(plot.title = element_text(hjust = 0.5))                         # Centrar Título

#####################################################################################################
#####################################################################################################
# Comparación de dos ciudades

Curico_Talca <- filter(Simce2do, nom_com_rbd == "Curicó" | nom_com_rbd == "Talca")                     

ggplot(data = Curico_Talca, mapping = aes(x = nom_rbd, y = prom_mate2m_rbd, shape = nom_com_rbd)) +
  geom_point() +
  theme(axis.text = element_text(angle = 90,                            # Rotar 90 grados la etiqueta
                                 size = 6)) +                           # Achicar tamaño de la letra
  labs(x = "Nombre Colegio", y = "Puntaje promedio Matemáticas 2018") + # Cambiar el nombre de los ejes
  ggtitle("Simce") +                                                    # Título del gráfico
  theme(plot.title = element_text(hjust = 0.5))                         # Centrar Título


ggplot(data = Curico_Talca, mapping = aes(x = nom_rbd, y = prom_mate2m_rbd)) +
  geom_point() +
  facet_wrap(~nom_com_rbd, nrow=2)+
  theme(axis.text = element_text(angle = 90,                            # Rotar 90 grados la etiqueta
                               size = 6)) +                           # Achicar tamaño de la letra
  labs(x = "Nombre Colegio", y = "Puntaje promedio Matemáticas 2018") + # Cambiar el nombre de los ejes
  ggtitle("Simce") +                                                    # Título del gráfico
  theme(plot.title = element_text(hjust = 0.5))                         # Centrar Título

#####################################################################################################
#####################################################################################################

# Resumen por Región de los datos

Simce2do %>%
  filter(!is.na(prom_mate2m_rbd)) %>%
  group_by(nom_reg_rbd) %>%
  summarise(promedio_mate = mean(prom_mate2m_rbd),
            mediana_mate = median(prom_mate2m_rbd)) %>%
  ggplot(mapping = aes(x = nom_reg_rbd, y = promedio_mate)) +
    geom_point() +
  theme(axis.text = element_text(angle = 90,                            # Rotar 90 grados la etiqueta
                                 size = 8))




