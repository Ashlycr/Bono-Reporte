# Bono-Reporte
Aqui se muestra el link de la data:
https://www.datosabiertos.gob.pe/dataset/bono-independiente-ministerio-de-trabajo-y-promocion-del-empleo-mtpe

```{r}

library(tidyverse)
data <- read.csv("D:/ASH-PROYECTO/BEST/tarea/bono.csv", sep=";")

data=rename(data,DEPARTAMENTO=DE_DEPARTAMENTO,PROVINCIA=DE_PROVINCIA,
            DISTRITO=DE_DISTRITO)

data$DEPARTAMENTO=as.factor(data$DEPARTAMENTO)
data$PROVINCIA=as.factor(data$PROVINCIA)
data$DISTRITO=as.factor(data$DISTRITO)
data$BONO_COBRADO=as.factor(data$BONO_COBRADO)
data$MEDIO_COBRO=as.factor(data$MEDIO_COBRO)

data<- data %>% 
  mutate(FECHA_COBRO= substring(FECHA_COBRO,1,10))
```
# DEPARTAMENTO 
se muestra el cuadro de los bonos presupuestados por departamento
La cantidad de personas qeue cobraron y que no.

```{r} 
  data %>% 
  group_by(DEPARTAMENTO,BONO_COBRADO) %>% 
  summarise(count=n(),
            promedio=median(PERSONAS_HOGAR),
            mediana=median(PERSONAS_HOGAR),
            max=max(PERSONAS_HOGAR),
            min=min(PERSONAS_HOGAR)) %>% 
  mutate(monto=count*760)

Ahora filtramos, sólo si han cobrado.
```{r}
DEPARTAMENTO_SI=data %>% 
group_by(DEPARTAMENTO,BONO_COBRADO) %>% 
  filter(BONO_COBRADO=="SI") %>% 
  summarise(count=n(),
            promedio=median(PERSONAS_HOGAR),
            mediana=median(PERSONAS_HOGAR),
            max=max(PERSONAS_HOGAR),
            min=min(PERSONAS_HOGAR)) %>% 
  mutate(monto=count*760)
DEPARTAMENTO_SI
A continuación, vemos el gráfico de las personas que ya cobraron el bono según departamento.

```{r}
ggplot(DEPARTAMENTO_SI, aes(x=DEPARTAMENTO, y=count,fill=DEPARTAMENTO)) + 
  geom_bar(stat="identity") +coord_flip()+
  ggtitle("Personas que cobraron el bono según departamento")
  
  Vemos el cuadro que si han cobrado por departamento
```{r}
DEPARTAMENTO_NO=data %>% 
  group_by(DEPARTAMENTO,BONO_COBRADO) %>% 
  filter(BONO_COBRADO=="NO") %>% 
  summarise(count=n(),
            promedio=median(PERSONAS_HOGAR),
            mediana=median(PERSONAS_HOGAR),
            max=max(PERSONAS_HOGAR),
            min=min(PERSONAS_HOGAR)) %>% 
  mutate(monto=count*760)
DEPARTAMENTO_NO

Ahora lo vemos en un gráfico:

```{r}
ggplot(DEPARTAMENTO_NO, aes(x=DEPARTAMENTO, y=count,fill=DEPARTAMENTO)) + 
  geom_bar(stat="identity") +coord_flip()+ #este codigo para voltear el gráfico y visualizar los nombres
  ggtitle("Personas que aun no cobran el bono según departamento")
  
 Aqui se muestra del total de hogares que SI y NO cobraron
```{r}
table(data$BONO_COBRADO)
```
El detalle de hogares de Lima que si cobraron:
```{r}
DEP_LIMA_SI=data %>% 
  group_by(DEPARTAMENTO,BONO_COBRADO,FECHA_COBRO) %>% #agrupamos por departamento, bono cobrado, y fecha de cobro
  filter(BONO_COBRADO=="SI") %>% #filtramos solo los que SI cobraron
  filter(DEPARTAMENTO=="LIMA") %>% #filtramos solo el departamento de LIMA
  summarise(count=n()) %>% 
  mutate(monto=count*760)#Y EL MONTO COBRADO POR HOGAR
DEP_LIMA_SI
```

```{r}
DEP_NO=data %>% 
  group_by(DEPARTAMENTO,BONO_COBRADO) %>% #agrupamos por departamento y segun si fue cobrado o no
  filter(BONO_COBRADO=="NO") %>% 
  summarise(count=n()) %>% 
  mutate(monto=count*760)#conocemos el monto total de bonos por cobrar
DEP_NO
```

Numero de hogares por provincia y fecha de cobro
```{r}
PROVINCIA=data %>% 
  group_by(PROVINCIA,BONO_COBRADO,FECHA_COBRO) %>% #Agrupamos por provincia, si fue cobrado y la fecha de cobro
  filter(BONO_COBRADO=="SI") %>% 
  summarise(count=n(),
            promedio=median(PERSONAS_HOGAR)) %>% 
  mutate(monto=count*760)
PROVINCIA
```
AHORA ORDENAMOS EL MONTO DE MAYOR A MENOR

```{r}
PROVINCIA%>%
  arrange(desc(monto))
```
AHORA VEMOS UN EJEMPLO SOBRE LA PROVINCIA DE AREQUIPA:
```{r}
PROVINCIA_AREQUIPA=data %>% #Solo queremos ver la provincia Arequipa
  group_by(PROVINCIA,BONO_COBRADO,FECHA_COBRO) %>% 
  filter(BONO_COBRADO=="SI") %>% 
  filter(PROVINCIA=="AREQUIPA") %>%
  summarise(count=n()) %>% 
  mutate(monto=count*760)
PROVINCIA_AREQUIPA


FINALMENTE VEMOS LOS DATOS A NIVEL DE DISTRITO
```{r}
DISTRITO_SI=data %>% 
  group_by(DISTRITO,BONO_COBRADO) %>% 
  filter(BONO_COBRADO=="SI") %>% 
  summarise(count=n()) %>% 
  mutate(monto=count*760)
DISTRITO_SI

```{r}
DISTRITO_NO=data %>% 
  group_by(DISTRITO,BONO_COBRADO) %>% 
  filter(BONO_COBRADO=="NO") %>% #filtramos por distrito y los que no han sido cobrados
  summarise(count=n()) %>% 
  mutate(monto=count*760)
DISTRITO_NO
```
AHORA VEMOS EL % SEGUN TIPO DE COBRO, COMO SE VE, LOS QUE AUN NO HAN COBRADO AUN ESTAN EN VENTANILLA
```{r}
prop.table(table(data$BONO_COBRADO,data$MEDIO_COBRO))*100
```
