library(ggplot2)
library(tidyverse)

#datos
creditos <- read.csv("creditos.csv", sep = ";")

#limpieza
creditos <- creditos %>% 
  gather(credito, euros, 2:4) %>% 
  arrange(desc(euros)) %>% 
  mutate(Comunidad = fct_reorder(Comunidad, (euros)))

creditos$euros <- as.numeric(creditos$euros)

#geom point
ggplot(creditos, aes(x = Comunidad, y = euros, color = credito)) +
  geom_point(size = 4.5, stroke = 1, alpha = 0.7) +
  ylim(0, 45) +
  xlab("") +
  labs(title = "Precios públicos del crédito matriculado por primera\nvez en titulaciones de Grado (2017-18)") +
  scale_y_continuous(position = "right") +
  coord_flip() +
  scale_color_manual(values = c("#EE2626", "#F19A15", "grey"), labels = c("Máxima Exp.", "Mínima Exp.", "Precio Medio")) +
  theme_minimal() +
  theme(panel.grid = element_line(linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.8, 0.2),
        axis.title.x = element_text(hjust = 1)) +
  annotate( geom = "curve", x = "La Rioja", y = 35, xend = "Castilla y León", yend = 32.5, 
    curvature = .3, arrow = arrow(length = unit(2, "mm")), color = "#EE2626") +
  annotate(geom = "text", x = "La Rioja", y = 32.5, label = "Máxima \nexperimentalidad: \n30,30 euros", hjust = 0,
           size = 3, color = "#EE2626", fontface = "bold", vjust = 1) +
  
  annotate( geom = "segment", x = "Andalucía", y = 18, xend = "Andalucía", yend = 14.5, 
            curvature = .3, arrow = arrow(length = unit(2, "mm")), color = "#EE2626") +
  annotate(geom = "text", x = "Cantabria", y = 19, label = "Cualquier \nexperimentalidad: \n12,60 euros", hjust = 0,
           size = 3, color = "#EE2626", fontface = "bold", vjust = 1)
  

#guardar
ggsave("precios.png", device = "png", type = "cairo", width = 6, height = 8, dpi = 300)
                    
                    