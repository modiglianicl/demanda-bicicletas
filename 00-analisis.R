
# Analisis de las bicis ---------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggrepel)
## Cargando dataset ----
data_p1 <- read_csv("bikes.csv")

### clean_names
data_p1 <- janitor::clean_names(data_p1)
### Transformacion variables categoricas a factor

data_p1$season <- as.factor(data_p1$season)
data_p1$holiday <- as.factor(data_p1$holiday)
data_p1$weekday <- as.factor(data_p1$weekday)
data_p1$workingday <- as.factor(data_p1$workingday)
data_p1$weather <- as.factor(data_p1$weather)

## Transformaciones necesarias ----

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

### Números a nombre de estaciones

data_p1 <- data_p1 %>% 
  mutate(season = case_when(season == 1 ~ "Invierno",
                            season == 2 ~ "Verano",
                            season == 3 ~ "Otoño",
                            season == 4 ~ "Primavera"))
# Plots ----

## Cantidad demanda vs Fecha----

### Para plotear año/mes creamos una variable año-mes

data_p1_v1 <- data_p1 %>% 
  mutate(anio = year(date),
         mes = month(date)) %>% 
  mutate(anio_mes = (paste(anio,"-",mes)))

ggplot(data = data_p1,
       aes(x = date,
           y = target))+
  geom_point(aes(color = season))+
  geom_smooth()+
  scale_x_date(date_breaks = "3 months")+
  labs(title = "Demanda de bicicletas",
       subtitle = "A lo largo de los años 2020 y 2021")+
  xlab("Demanda")+
  ylab("Fecha")+
  guides(color=guide_legend(title="Estación"))

### Observación : se logra ver la estacionalidad de la demanda:
### Durante finales de primavera y el inicio de invierno se logra presenciar
### la diminución de la demanda de bicicletas!

## Distribución registrados vs no regitrados ----

### Calculamos los no registrados (target - registered)

data_p1 <- data_p1 %>% 
  mutate(no_registrados = target - registered)

### Unpivoteamos los datos por tipo de cliente

data_p1_unpivot <- data_p1 %>% 
  gather(registered:no_registrados, key="tipo_cliente",
         value = "cantidad")

### Ploteamos

ggplot(data = data_p1_unpivot %>% 
         filter(tipo_cliente %in% c("no_registrados","registered")))+
  geom_freqpoly(aes(x = cantidad, color = tipo_cliente),lwd = 1.2)+
  labs(title = "Distribución de cantidad de demanda",
       subtitle = "Por categoria de cliente")+
  xlab("Demanda diaria")+
  ylab("Cantidad")+
  guides(color=guide_legend(title="Tipo de Cliente"))+
  geom_vline(xintercept = mean(data_p1_unpivot$cantidad), lwd = 1.05,
             linetype = "dashed")+
  geom_label(x =mean(data_p1_unpivot$cantidad)+800, y=100,
            label = "Promedio demanda diaria")
  
### Se observa que las cantidades se distribuyen en menor cantidad en las 
### personas registradas, pero 
