### Librerias de trabajo

library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(ggpubr)
library(BSDA)

# cargando dataset cancer

raw_data <- readRDS('clean_data/cancer_data.RDS')
glimpse(raw_data)

# seleccionando variables de interés

cancer_data <- raw_data %>%
  select(age, number_of_sexual_partners,
         smokes_years, hormonal_anticonceptives, stds_hepatitis_b,
         stds_condylomatosis, dx_hpv, stds_genital_herpes, stds_syphilis,
         stds_hiv, stds_mulluscum, dx_cin, dx_cancer)

# eliminado NAs
cancer_data <- na.omit(cancer_data)

# realizando grafico de correlacion

correlations <- cor(cancer_data)

# cambiar nombre de columna y filas a español

colnames(correlations) <- c('edad', 'numero_de_parejas_sexuales',
                            'tabaquismo_años', 'uso_anticonceptivos',
                            'hepatitis_b', 'condilomatosis', 'vph',
                            'herpes_genital', 'sifilis', 'vih',
                            'molusco_contagioso', 'celulas_anormales',
                            'cancer_cervicouterino')

rownames(correlations) <- c('edad', 'numero_de_parejas_sexuales',
                            'tabaquismo_años', 'uso_anticonceptivos',
                            'hepatitis_b', 'condilomatosis', 'vph',
                            'herpes_genital', 'sifilis', 'vih',
                            'molusco_contagioso', 'celulas_anormales',
                            'cancer_cervicouterino')



corrplot(correlations, method = 'circle', tl.col = 'black', tl.cex = 0.8)

# generando modelo lineal

attach(cancer_data)

modelo_lineal <- lm(dx_cancer~dx_hpv, data = cancer_data)
summary(modelo_lineal) # y = 0.002849 + 0.934651 x
confint(modelo_lineal)

# obteniedo grupo con diagnostico de vph y sin diagnostico

dx_vph <- cancer_data %>%
  filter(dx_hpv == 1)

sano_vph <- cancer_data %>%
  filter(dx_hpv == 0)


### haciendo estadistica descriptiva

# dx de vph

dx_vph %>%
  select(dx_cancer) %>%
  summarise(promedio = sum(dx_cancer)/nrow(dx_vph))

dx_vph %>%
  select(dx_cancer) %>%
  summarise(desviacion_estandar=sd(dx_cancer))

dx_vph %>%
  select(dx_cancer) %>%
  summarise(varianza=var(dx_cancer))

# sanos

sano_vph %>%
  select(dx_cancer) %>%
  summarise(promedio = sum(dx_cancer)/nrow(sano_vph))

sano_vph %>%
  select(dx_cancer) %>%
  summarise(desviacion_estandar=sd(dx_cancer))

sano_vph %>%
  select(dx_cancer) %>%
  summarise(varianza=var(dx_cancer))



# prueba de hipotesis con z.test

z.test(x = dx_vph$dx_cancer, y = sano_vph$dx_cancer,
       sigma.x= 0.25, sigma.y = 0.0535, alternative = 'greater',
       conf.level = 0.95)





#####################
#####################
#####################


## evaluacion enfermedades hepaticas


hepatico_df <- readRDS('clean_data/hepatico.RDS')


### modelo lineal multivariable tomando 'alcoholic_beverages_per_day'
# como variable dependiente

attach(hepatico_df)
modelo_multiva <- lm(alcoholic_beverages_per_day~ gamma_glutamyl_transpeptidase + alanine_aminotransferase + alkaline_phosphotase + aspartate_aminotransferase,
                     data = hepatico_df)
summary(modelo_multiva)
confint(modelo_multiva)

# hipotesis: las personas que consumen igual o menos de 1 bebida tienen
# niveles diferentes de gamma_glutamyl_transpeptidase
# en comparacion a aquellos que toman mas de 1 bebida


# generando poblaciones

sin_bebidas <-  hepatico_df %>%
  filter(alcoholic_beverages_per_day <= 1)

con_bebida <- hepatico_df %>%
  filter(alcoholic_beverages_per_day > 1)

### hciendo estadistica descriptiva de gamma_glutamyl_transpeptidase

# media
sin_bebidas %>% summarise(promedio = mean(gamma_glutamyl_transpeptidase))

# desviacion estandar
sin_bebidas %>% summarise(de = sd(gamma_glutamyl_transpeptidase))

# varianza
sin_bebidas %>% summarise(varianza = var(gamma_glutamyl_transpeptidase))


# media
con_bebida %>% summarise(promedio = mean(gamma_glutamyl_transpeptidase))

# desviacion estandar
con_bebida %>% summarise(de = sd(gamma_glutamyl_transpeptidase))

# varianza
con_bebida %>% summarise(varianza = var(gamma_glutamyl_transpeptidase))



# prueba de hipotesis con z.test

z.test(x = sin_bebidas$gamma_glutamyl_transpeptidase,
       y = con_bebida$gamma_glutamyl_transpeptidase,
       sigma.x = 24.30219, sigma.y = 44.88475, alternative = 'two.sided',
       conf.level = 0.95)





