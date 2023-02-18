#### prueba de carga

raw_data <- read_csv('raw_data/risk_factors_cervical_cancer.csv')

is.na(raw_data) <- df == "?"
raw_data[raw_data == '?'] <- NA

write.csv(raw_data, 'clean_data/risk_factors_cervical_cancer_raw.csv',
          row.names = FALSE)

prueba <- read.csv('clean_data/risk_factors_cervical_cancer_raw.csv')



### iniciando proyecto

# cargando dataset crudo

raw_data <- read_csv('raw_data/risk_factors_cervical_cancer_raw.csv')
glimpse(raw_data)

# seleccionando variables de interés

cancer_data <- raw_data %>%
  select(age, number_of_sexual_partners,
         smokes_years, hormonal_anticonceptives, stds_hepatitis_b,
         stds_condylomatosis, dx_hpv, stds_genital_herpes, stds_syphilis,
         stds_hiv, stds_mulluscum, dx_cin, dx_cancer)

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



corrplot(correlations, method = 'circle', tl.col = 'black')

# generando modelo

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

promedio_sano <- sano_vph %>%
  select(dx_cancer) %>%
  summarise(promedio = sum(dx_cancer)/nrow(sano_vph))

sd_sano <- sano_vph %>%
  select(dx_cancer) %>%
  summarise(desviacion_estandar=sd(dx_cancer))

var_sano <- sano_vph %>%
  select(dx_cancer) %>%
  summarise(varianza=var(dx_cancer))

# prueba de hipotesis

t.test(x = dx_vph$dx_cancer, y = sano_vph$dx_cancer,
       var.equal = FALSE, alternative = 'greater',
       conf.level = 0.95)








#####################
#####################
#####################


## evaluacion enfermedades hepaticas


data <- read.table('raw_data/bupa.data', sep = ',')
nombre_bupa <- read.table('raw_data/bupa.names', sep = ',')

colnames(data) <- c('mean_corpuscular_volume', 'alkaline_phosphotase',
                    'alanine_aminotransferase', 'aspartate_aminotransferase',
                    'gamma_glutamyl_transpeptidase',
                    'alcoholic_beverages_per_day', 'bupa_field')

hepatico_df <- data %>%
  select(-bupa_field)

### modelo lineal multivariable tomando 'alcoholic_beverages_per_day'
# como variable dependiente

attach(hepatico_df)
modelo_multiva <- lm(alcoholic_beverages_per_day~ gamma_glutamyl_transpeptidase + alanine_aminotransferase + alkaline_phosphotase + aspartate_aminotransferase,
                     data = hepatico_df)
summary(modelo_multiva)
confint(modelo_multiva)

# hipotesis: las personas que consumen igual o menos de 1 bebida tienen
# niveles menores niveles de gamma_glutamyl_transpeptidase
# en comparacion a a quellos que toman mas de 1 bebida


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


#### prueba de hipotesis

t.test(x = sin_bebidas$gamma_glutamyl_transpeptidase,
       y = con_bebida$gamma_glutamyl_transpeptidase,
       var.equal = FALSE, alternative = 'less',
       conf.level = 0.95)





