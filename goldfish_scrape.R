library(tidyverse)
library(rvest)
library(xml2)
library(scales)
library(ggrepel)

####
leer_tabla <- function(un_html) {
  un_html %>%
    html_nodes(css = ".index-price-table-paper tbody tr") %>%
    html_text() %>%
    str_split(pattern = "\\n", simplify = T) %>%
    as.data.frame() %>%
    tbl_df() %>%
    select("Carta" = V1, "Set" = V2, "Rareza" = V3, "Precio" = V5) %>%
    mutate(Precio = as.numeric(as.character(Precio)),
           Rareza = factor(Rareza, levels = c("Basic Land","Common",
                                              "Uncommon", "Rare", "Mythic"))) %>%
    mutate_at(c("Carta", "Set"), as.character) %>%
    filter(Rareza != "Basic Land")
}

col_rarezas <- c("#ffffff", "#93b2c4", "#c0aa70", "#d66a26")

tag_outlier <- function(datos) {
  ifelse(datos < quantile(datos, .25) - IQR(datos) * 1.5 | datos > quantile(datos, .75) + IQR(datos) * 1.5, TRUE, FALSE)
}

precio_densidad <- function(precio_tabla) {
  ggplot(precio_tabla) +
    aes(x = Precio, fill = Rareza) +
    geom_density(alpha = .5) +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0), labels = dollar_format()) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = col_rarezas) +
    facet_wrap(~Rareza, scales = "free") +
    theme(legend.position = "none")
}

precio_boxplot <- function(precio_tabla) {
  precio_tabla %>%
    group_by(Rareza) %>%
    mutate(Outlier = tag_outlier(Precio),
           Outlier = ifelse(Outlier, Carta, NA)) %>%
    ggplot() +
    aes(x = Rareza, y = Precio, fill = Rareza) +
    geom_boxplot(alpha = .7) +
    geom_text_repel(aes(label = Outlier), na.rm = TRUE, size = 2.8) +
    theme_minimal() +
    scale_y_continuous(labels = dollar_format()) +
    scale_fill_manual(values = col_rarezas) +
    theme(legend.position = "none")
}

crear_booster <- function(tabla) {
  pares <- list(
    c("Rare", 1),
    c("Uncommon", 3),
    c("Common", 10)
  )

  map(pares, function(x){
    if(x[1] == "Rare") {
      x[1] <- sample(size = 1, x = c(rep("Rare", 7), "Mythic"))
    }

    tabla %>%
      filter(Rareza == x[1]) %>%
      sample_n(size = as.numeric(x[2]))
  }) %>%
    reduce(bind_rows)
}

crear_caja <- function(tabla) {
  map(1:36, ~crear_booster(tabla)) %>%
    reduce(bind_rows)
}


crear_simulacion <- function(tabla, iteraciones = 100) {
  map(1:iteraciones, function(x) {
    crear_caja(tabla) %>%
      filter(Rareza != "Common") %>%
      summarize(Total = sum(Precio)) %>%
      pull(Total)
  }) %>%
    reduce(c)
}



prob_exito <- function(precio_cajas, costo_caja) {

  media <- mean(precio_cajas)
  ds <- sd(precio_cajas)
  densidad <- density(precio_cajas)

  prob <- integrate(approxfun(densidad), lower= min(precio_cajas), upper = 100)

  exito <- paste0("Prob. de tablas:\n",
                  round(1 - prob$value, 4) * 100, "%")

  ggplot() +
    aes(x = precio_cajas) +
    geom_density(fill = "white") +
    geom_vline(xintercept = costo_caja, alpha = .75, linetype = "dashed") +
    scale_y_continuous(expand = c(0, 0)) +
    annotate(geom = "text", x = costo_caja, y = 0, label = exito, size = 3, vjust = -0.3) +
    theme_minimal()
}


#####

download_html(url = "https://www.mtggoldfish.com/index/BBD",
              file = "goldfish.html")

goldfish <- read_html("goldfish.html")

pez <- leer_tabla(goldfish)

precio_boxplot(pez)
precio_densidad(pez)

crear_caja(pez) %>%
  precio_boxplot()

set.seed(21)
escuela

crear_simulacion(pez)
prob_exito(precio_cajas = escuela, costo_caja = 120)

crear_simulacion <- function(tabla, iteraciones = 100) {
  precio_df <-
    map(1:iteraciones, function(x) {
    crear_caja(tabla) %>%
      filter(Rareza != "Common") %>%
        group_by(Rareza) %>%
        summarize(Valor = sum(Precio)) %>%
        mutate(caja = x)
  }) %>%
    reduce(bind_rows)

  precio_df
}



