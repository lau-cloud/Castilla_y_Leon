library(ggplot2)
library(tidyverse)

#datos
univ <- read.csv("numerodecentros.csv", sep = ";")
univ <- univ %>% 
  arrange(desc(numero))
  

ggplot(univ, aes(x = reorder(comunidad, -numero), y = numero, fill = centros, color = centros)) +
  geom_bar(aes(alpha = comunidad == "Castilla y Le�n"), stat = "identity") +
  scale_fill_manual(values=c("#EE2626", "#F19A15")) +
  scale_color_manual(values=c("#EE2626", "#F19A15")) +
  scale_alpha_manual(values = c("TRUE" = 0.8, "FALSE" = 0.3), guide = F) +
  labs(title = "Distribuci�n del n�mero de centros \nque imparten alguna titulaci�n oficial") +
  xlab("") +
  ylab("N� de centros") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, vjust=1.1, hjust=1),
        panel.grid = element_line(linetype = "dashed"),
        panel.grid.major.x = element_blank())

ggsave("oferta_educativa.png", device = "png", type = "cairo", width = 9, height = 6, dpi = 300)
