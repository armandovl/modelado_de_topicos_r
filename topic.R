
##### http://www.aic.uva.es/cuentapalabras/topic-modeling.html

#Instalando las bibliotecas
library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(scales)
library(wordcloud)
library(RColorBrewer)



#cargar lista de palabras vacias
vacias <- read_tsv("https://tinyurl.com//7PartidasVacias",
                   locale = default_locale())

#Vector con nombre de los texto
titulos <- c("capsulas",
             "grano",
             "molido",
             "soluble")

#nombre de los ficheros
ficheros <- c("buenos_capsulas.txt",
              "buenos_grano.txt",
              "buenos_molido.txt",
              "buenos_soluble.txt")

#ruta de los ficheros
ruta<-"C:/Users/Armando/Desktop/modelado_topicos/"

#aquí se guardaran los 4 texto
ensayos <- tibble(texto = character(),
                  titulo = character(),
                  pagina = numeric())

######### asigno todo lo que debo de asignar #################
for (j in 1:length(ficheros)){
  texto.entrada <- read_lines(paste(ruta,
                                    ficheros[j],
                                    sep = ""),
                              locale = default_locale())
  texto.todo <- paste(texto.entrada, collapse = " ")
  por.palabras <- strsplit(texto.todo, " ")
  texto.palabras <- por.palabras[[1]]
  trozos <- split(texto.palabras,
                  ceiling(seq_along(texto.palabras)/375))
  for (i in 1:length(trozos)){
    fragmento <- trozos[i]
    fragmento.unido <- tibble(texto = paste(unlist(fragmento),
                                            collapse = " "),
                              titulo = titulos[j],
                              pagina = i)
    ensayos <- bind_rows(ensayos, fragmento.unido)
  }
}



###########################################################


#para solo conservar ensayos y vacias y limpiar environment
rm(ficheros, titulos, trozos, fragmento,
   fragmento.unido, ruta, texto.entrada,
   texto.palabras, texto.todo, por.palabras, i, j)

# dividirlo en palabras-token y guardarlas en una tabla
por_pagina_palabras <- ensayos %>%
  unite(titulo_pagina, titulo, pagina) %>%
  unnest_tokens(palabra, texto)

#reemplazar palabras
#por_pagina_palabras[por_pagina_palabras == "café"] <- "palabra reemplazadora"


#remover palabras
#por_pagina_palabras <- por_pagina_palabras[por_pagina_palabras$palabra !="sabor",]



# Eliminar palabras gramaticales
palabra_conteo <- por_pagina_palabras %>%
  anti_join(vacias) %>% 
  count(titulo_pagina, palabra, sort = TRUE) %>%
  ungroup()

#obtener una matriz con la que trabaja topic model
paginas_dtm <- palabra_conteo %>%
  cast_dtm(titulo_pagina, palabra, n)

######################## Construir el modelo LDA #####################
paginas_lda <- LDA(paginas_dtm, k = 2, control = list(seed = 1234) )

#convertirlo a otro formato necesario
paginas_lda_td <- tidy(paginas_lda, matrix = "beta")


#ver la tabla
paginas_lda_td

#guardar los 10 terminos probables para cada topico
terminos_frecuentes <- paginas_lda_td %>%
  group_by(topic) %>%
  top_n(200, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#ver el resultado
#view(terminos_frecuentes)

######################## hacer gráfico de nube de palabras
#colores aqui
#http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need

topico3 <- terminos_frecuentes[terminos_frecuentes$topic ==1,]
wordcloud(topico3$term,
          freq = terminos_frecuentes$beta,
          #min.freq = 
          #max.words = 80, 
          random.order = FALSE,
          colors=brewer.pal(name="Set1", n=2)
          #colors=brewer.pal(7, "Dark2")
          #colors=c("#0B125F" ,'green',"blue",'orange',"red","pink")
          #colors = brewer.pal(9, "Blues")[c(-1, -2, -3, -4)]
          #colors = brewer.pal(name = "Dark2", n = 8)
)
########################################################
########################################################

#Graficar los términos frecuentes
terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()




############ Clasificación por documento #############################

#nuevo modelo
paginas_lda_gamma <- tidy(paginas_lda, matrix = "gamma")


#etiquetar con el titulo del texto
paginas_lda_gamma <- paginas_lda_gamma %>%
  separate(document,
           c("titulo", "pagina"),
           sep = "_", convert = TRUE)


#para hacer el gráfico
ggplot(paginas_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() +
  facet_wrap(~ titulo, nrow = 2)


#ver que topico se asocia mejor con que página
paginas_clasificaciones <- paginas_lda_gamma %>%
  group_by(titulo, pagina) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

#ver el total de paginas, para despues ver el error
paginas_clasificaciones

# ver otra ves topico con pensador ################
topico_pensador <- paginas_clasificaciones %>%
  count(titulo, topic) %>%
  group_by(titulo) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consenso = titulo, topic)

topico_pensador

# ver las páginar erroneamente asignadas
paginas_clasificaciones %>%
  inner_join(topico_pensador, by = "topic") %>%
  filter(titulo != consenso)

#averiguar qué palabras de cada página asignó el algoritmo a cada tópico.

asignaciones <- augment(paginas_lda, data = paginas_dtm)

asignaciones

#combinar para ver que palabras no logro asignar correctamente
asignaciones <- asignaciones %>%
  separate(document, c("titulo",
                       "pagina"),
           convert = TRUE) %>%
  inner_join(topico_pensador,
             by = c(".topic" = "topic"))
asignaciones

#graficar una matriz de confusión para ver que tal funcionó el modelo
asignaciones %>%
  count(titulo, consenso, wt = count) %>%
  group_by(titulo) %>%
  mutate(porcentaje = n / sum(n)) %>%
  ggplot(aes(consenso, titulo, fill = porcentaje)) +
  geom_tile() +
  scale_fill_gradient2(high = "blue", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Asignó las palabras a.",
       y = "Las palabras procedían de.",
       fill = "% de asignaciones")
