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


crear_simulacion <- function(tabla, iteraciones = 100) {
  precio_df <-
    map(1:iteraciones, function(x) {
      crear_caja(tabla) %>%
        filter(Rareza != "Common") %>%
        group_by(Rareza) %>%
        summarize(Valor = sum(Precio)) %>%
        mutate(Caja = x) %>%
        group_by(Caja) %>%
        mutate(Prop = Valor / sum(Valor)) %>%
        ungroup()
    }) %>%
    reduce(bind_rows)

  precio_df
}

prop_valor <- function(res_simulacion){
  medias <-
    res_simulacion %>%
    group_by(Rareza) %>%
    summarise(Media = mean(Prop)) %>%
    mutate(Etiqueta = paste0(round(Media * 100, 1), "%"))

  tope_y <- max(medias[["Media"]])


     ggplot(medias) +
     aes(Rareza, Media, fill = Rareza, color = Rareza) +
     geom_col(alpha = .5) +
       geom_text(aes(label = Etiqueta), vjust = -.3) +
     scale_fill_manual(values = col_rarezas[-1]) +
       scale_color_manual(values = col_rarezas[-1]) +
    scale_y_continuous(expand = c(0, 0), labels = percent_format()) +
     coord_cartesian(ylim = c(0, tope_y + (tope_y / 10))) +
     theme_minimal() +
     theme(legend.position = "none") +
       labs(title = paste0("Proporción del valor de caja que aporta cada rareza"))
}

prob_exito <- function(res_simulacion, costo_caja) {
  precio_cajas <-
    res_simulacion %>%
    group_by(Caja) %>%
    summarize(Suma = sum(Valor)) %>%
    pull(Suma)

  media <- mean(precio_cajas)
  ds <- sd(precio_cajas)
  densidad <- density(precio_cajas)

  segmento <-
    densidad[c("x", "y")] %>%
    tbl_df() %>%
    mutate(Tipo = ifelse(x < costo_caja,
                         paste0("Menor que ", costo_caja, " USD"),
                         paste0("Mayor que ", costo_caja, " USD"))
    )%>%
    filter(x > min(precio_cajas) & x < max(precio_cajas))

  prob <- integrate(approxfun(densidad), lower= min(precio_cajas),
                    upper = costo_caja)

  titulo <- paste0("Probabilidad de recuperar inversión: ",
                  round(1 - prob$value, 4) * 100,
                  "%")

  subtitulo <- paste0("Simulación con ",
                      max(res_simulacion[["Caja"]]), " cajas\nValor medio: ", round(media, 1), " USD")

  ggplot(segmento) +
    aes(x, y, fill = Tipo) +
    geom_area(alpha = c(.5)) +
    geom_vline(xintercept = media, linetype = "dashed", alpha = .5) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(legend.position = "top") +
    labs(title = titulo, subtitle = subtitulo,
         x = "Valor de cajas (USD)", y = "Densidad") +
    scale_fill_manual(values = col_rarezas[c(4, 2)])
}

obtener_precios <- function(expansion, forzar_descarga = FALSE){
  goldfish_url <-
    paste0("https://www.mtggoldfish.com/index/",
           expansion)
  goldfish_html <- paste0("goldfish_", expansion, ".html")

  if(!file.exists(goldfish_html) | forzar_descarga == TRUE) {
    download_html(url = goldfish_url,
                  file = goldfish_html)
  }

  goldfish <- read_html(goldfish_html)

  precios <- leer_tabla(un_html = goldfish)

  precios
}

simular_precios <- function(precios, costo_caja, iteraciones = 100) {
  simulacion <- list()

  simulacion$sim <-
    crear_simulacion(tabla = precios, iteraciones = iteraciones)
  simulacion$prop <-
    prop_valor(res_simulacion = simulacion$sim)
  simulacion$prob <-
    prob_exito(res_simulacion = simulacion$sim, costo_caja = costo_caja)

  simulacion
}

#
analizar_set <- function(expansion, costo_caja = 100, iteraciones = 100, forzar_descarga = FALSE) {
  mi_set <- list()

  mi_set$precios <-
    obtener_precios(expansion = expansion, forzar_descarga = forzar_descarga)
  mi_set$boxplot <-
    precio_boxplot(precio_tabla = mi_set$precios)
  mi_set$densidad <-
    precio_densidad(precio_tabla = mi_set$precios)

  mi_set$simulacion <-
    simular_precios(precios = mi_set$precios, costo_caja = costo_caja, iteraciones = iteraciones)

  mi_set
}
###

amonketh <- analizar_set("AKH")
M19 <- analizar_set("M19")
hora_devastada <- analizar_set("HOU")
