library(readxl)
library(tidyverse)
library(ggpubr)
library(InformationValue)
library(ROCit)
library(DescTools)
library(MLmetrics)
library(broom)
library(rsample)
library(ggfortify)
# Creacion Modelo RL ------------------------------------------------------

data_p1 <- read_csv("bikes.csv")

### clean_names
data_p1 <- janitor::clean_names(data_p1)
### Transformacion variables categoricas a factor

data_p1$season <- as.factor(data_p1$season)
data_p1$holiday <- as.factor(data_p1$holiday)
data_p1$weekday <- as.factor(data_p1$weekday)
data_p1$workingday <- as.factor(data_p1$workingday)
data_p1$weather <- as.factor(data_p1$weather)

### Transformacion temperaturas

data_p1 <- data_p1 %>% 
  mutate(temp= 44 * temp - 8,
         atemp = 66 * atemp - 16)

### Transformacion humedad a porcentaje

data_p1 <- data_p1 %>% 
  mutate(humidity = humidity * 100)

### Transformacion velocidad del viento a millas por hora

data_p1 <- data_p1 %>% 
  mutate(windspeed = windspeed*67)

### Creamos el modelo sin las variables date, month y registered

data_p1_2 <- data_p1 %>% 
  select(-date, -month, -registered)

## Obtenemos un modelo nulo con solo el intercepto

modelo_nulo <- lm(target ~ 1 , data = data_p1_2)

## Creamos un modelo completo
modelo_completo <- lm(target ~ ., data = data_p1_2)
## Obtenemos la formula del modelo completo
formula_forward <- formula(modelo_completo)

modelo_p4 <- step(modelo_nulo,
                  direction="forward",
                  scope = formula_forward)




# App ---------------------------------------------------------------------



ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel(h1("Predictor demanda de bicicletas", align = "center"),
             windowTitle = "Predictor demanda de bicicletas"),
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("season", label = h5("Temporada del año"), 
                  choices = list("Invierno" = 1, "Verano" = 2, "Otoño" = 3,
                                 "Primavera" = 4), 
                  selected = 1),
      radioButtons("holiday", label = h5("¿Es día feriado?"),
                   choices = list("Sí" = 1, "No" = 0), 
                   selected = 1),
      selectInput("weekday", label = h5("Día de la semana"), 
                  choices = list("Lunes" = 1, "Martes" = 2, "Miercoles" = 3,
                                 "Jueves" = 4, "Viernes" = 5, "Sabado" = 6,
                                 "Domingo" = 0), 
                  selected = 1),
      selectInput("weather", label = h5("Clima"), 
                  choices = list("Claro a parcialmente nublado" = 1, "Niebla" = 2, 
                                 "Fuertes lluvias o nieve" = 3),
                  selected = 1),
      sliderInput("temp", label = h5("Temperatura en °C"), min = 10, 
                  max = 35, value = 15),
      "Desarrollado por Felipe Villarroel"
    ),
    mainPanel(
      titlePanel("Resultado"),
      textOutput(outputId = "test"),
      titlePanel("Explicación modelo"),
          HTML("Este es mi primer modelo, es parte de una evaluación
          para el diplomado en Data Science en la Pontificie Universidad
          Catolica de Chile. El modelo en resumen predice la demanda
          de bicicletas dependiendo que temporada del año indicamos.\n
          Observación : El modelo no esta 100% perfecto ya que predice
                   que mientras mas temperatura más demanda, por lo que le deje
                   un maximo de 35°C (lo lógico sería que hasta cierta T° la gente
                   tienda a no querer andar en bici.
                   Adicionalmente, para simpleza de la página deje algunas variables
                   constantes.")
          )

    )
  )
  
  


server <- function(input, output, session) {
  output$test = renderText({
    prediceme <-tibble(season = input$season,
                       year = 1,
                       holiday = input$holiday,
                       weekday = input$weekday,
                       workingday = 0,
                       weather = input$weather,
                       temp = input$temp,
                       atemp = input$temp,
                       humidity = 66.3,
                       windspeed = 12.5)
    
    prediceme$season <- as.factor(prediceme$season)
    prediceme$holiday <- as.factor(prediceme$holiday)
    prediceme$weekday <- as.factor(prediceme$weekday)
    prediceme$workingday <- as.factor(prediceme$workingday)
    prediceme$weather <- as.factor(prediceme$weather)
    
    paste("La demanda estimada es de : ", (round(predict(modelo_p4,newdata = prediceme),0)), 
          "bicicletas.")
    
  })
  
}

shinyApp(ui, server)