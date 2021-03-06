library(ggplot2)
library(tidyverse)

#datos
datos <- read.csv("evolucion_edad_modificado.csv", sep = ";")

#lipieza
datos <- datos %>% 
  select(a�o, edad, poblacion, porcentaje) 
datos$edad <- as.factor(datos$edad)
datos$poblacion <- as.numeric(datos$poblacion)

#line plot
ggplot(datos, aes(x = a�o, y = poblacion, group = edad, color = edad)) +
  geom_line(size = 1.8) +
  scale_color_manual(values = c("#EE2626", "#F19A15", "grey", "black")) +
  theme_minimal() +
  labs(title = "Evoluci�n de la poblaci�n en Castilla y Le�n") +
  ylim(200, 800) +
  ylab("Poblaci�n (en miles)") +
  xlab("") +
  theme(panel.grid = element_line(linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 24, face = "bold"))

#guardar
ggsave("edades.png", device = "png", type = "cairo", width = 8, height = 6, dpi = 300)

