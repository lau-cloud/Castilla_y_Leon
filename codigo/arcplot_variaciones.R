library(ggplot2)
library(tidyverse)
library(patchwork)
library(hrbrthemes)
library(igraph)
library(ggraph)
library(devtools)
library(cowplot)


#datos
variaciones <- read.csv("variaciones_provincia_arc1.csv", sep = ";")
variaciones_provincias <- read.csv("variaciones_arc2.csv", sep = ";")

prov <- c("Ávila", "León", "Zamora", "Palencia", "Burgos", "Valladolid", "Soria", "Segovia", "Salamanca")


#limpieza
variaciones <- variaciones %>% 
  gather(destino, numero, 2:20)

por_provincias <- variaciones_provincias %>% 
  gather(destino, numero, 2:28)

por_provincias_solo <- por_provincias %>% 
  filter(destino %in% prov)
  
#rename
variaciones$destino <- as.factor(variaciones$destino)
levels(variaciones$destino)


#arc diagram GENERAL
ggraph(variaciones, layout="linear") + 
  geom_edge_arc(aes(width = numero, alpha = stat(index)), 
                color = "#F19A15", lineend = "round") +
  geom_node_point(size = 1, shape = 1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="#4E4E4E", nudge_x=-0.2) +
  theme_graph() +
  labs(edge_width = "Variaciones residenciales 2019") +
  theme(legend.position = "top")

ggsave("firstplot.png", device = "png", type = "cairo", width = 8, height = 5, dpi = 300)

#por provincias CYL
ggraph(por_provincias, layout="linear") + 
  geom_edge_arc(aes(width = numero, alpha = stat(index), edge_colour = "destino"), lineend = "round") +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black", nudge_x=-0.2) +
  theme_graph() +
  labs(edge_width = "Variaciones residenciales") +
  theme(legend.position = "top")

#por_provincias_solo
ggraph(por_provincias_solo, layout="linear") + 
  geom_edge_arc(aes(width = numero, alpha = stat(index), edge_colour = "destino"), lineend = "round") +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black", nudge_x=-0.2) +
  theme_graph() +
  labs(edge_width = "Variaciones residenciales") +
  theme(legend.position = "top")

ggsave("provincias_cyl.png", device = "png", type = "cairo", width = 10, height = 5, dpi = 300)



#avila
avila <- por_provincias %>% 
  filter(procedencia == "Ávila") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g1 <- ggraph(avila, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black", nudge_x=-0.2) +
  theme_graph() +
  labs(title = "Ávila") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin=unit(rep(2,4), "cm")
  ) 
g1
ggsave("avila.png", device = "png", type = "cairo", width = 5, height = 4, dpi = 300)


#burgos
burgos <- por_provincias %>% 
  filter(procedencia == "Burgos") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g2 <- ggraph(burgos, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black", nudge_x=-0.2) +
  theme_graph() +
  labs(title = "Burgos") +
  theme(legend.position = "none",
        plot.margin=unit(rep(2,4), "cm"),
        plot.title = element_text(hjust = 0.5)
  ) 
g2

ggsave("burgos.png", device = "png", type = "cairo", width = 5, height = 4, dpi = 300)


#leon
leon <- por_provincias %>% 
  filter(procedencia == "León") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g3 <- ggraph(leon, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black", nudge_x=-0.2) +
  theme_graph() +
  labs(title = "León") +
  theme(legend.position = "none",
        plot.margin=unit(rep(2,4), "cm"),
        plot.title = element_text(hjust = 0.5)

    ) 

g3

ggsave("leon.png", device = "png", type = "cairo", width = 5, height = 4, dpi = 300)


#zamora
zamora <- por_provincias %>% 
  filter(procedencia == "Zamora") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g4 <- ggraph(zamora, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black",nudge_x=-0.2 ) +
  theme_graph() +
  labs(title = "Zamora") +
  theme(legend.position = "none",
        plot.margin=unit(rep(2,4), "cm"),
        plot.title = element_text(hjust = 0.5)
  ) 
g4
ggsave("zamora.png", device = "png", type = "cairo", width = 5, height = 4, dpi = 300)


#segovia
segovia <- por_provincias %>% 
  filter(procedencia == "Segovia") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g5 <- ggraph(segovia, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black") +
  theme_graph() +
  labs(edge_width = "Variaciones residenciales") +
  theme(legend.position = "top",
        plot.margin=unit(rep(2,4), "cm")
  ) 

#valladolid
valladolid <- por_provincias %>% 
  filter(procedencia == "Valladolid") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g6 <- ggraph(valladolid, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black") +
  theme_graph() +
  labs(edge_width = "Variaciones residenciales") +
  theme(legend.position = "top",
        plot.margin=unit(rep(2,4), "cm")
  ) 
#Soria
soria <- por_provincias %>% 
  filter(procedencia == "Soria") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g7 <- ggraph(soria, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black") +
  theme_graph() +
  labs(edge_width = "Variaciones residenciales") +
  theme(legend.position = "top",
        plot.margin=unit(rep(2,4), "cm")
  )

#salamanca
salamanca <- por_provincias %>% 
  filter(procedencia == "Salamanca") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g8 <- ggraph(salamanca, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black") +
  theme_graph() +
  labs(edge_width = "Variaciones residenciales") +
  theme(legend.position = "top",
        plot.margin=unit(rep(2,4), "cm")
  )

#palencia
palencia <- por_provincias %>% 
  filter(procedencia == "Palencia") %>% 
  filter(!destino %in% prov) %>% 
  arrange(desc(numero)) %>%
  top_n(10)

g9 <- ggraph(palencia, layout="linear") + 
  geom_edge_arc(aes(width = numero), edge_alpha=0.5, color = "orange", lineend = "round", 
                linejoin = "bevel", label_alpha = 0.8) +
  geom_node_point( color="black", size=0.1) +
  geom_node_text( aes(label=name), repel = TRUE, size=2, color="black") +
  theme_graph() +
  labs(edge_width = "Variaciones residenciales") +
  theme(legend.position = "top",
        plot.margin=unit(rep(2,4), "cm")
  )

g9
#plot all provinces
plot_grid(g1, g2, g3, ncol = 3)
