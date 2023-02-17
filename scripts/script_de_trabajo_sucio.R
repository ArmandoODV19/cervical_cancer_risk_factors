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
  select(age, number_of_sexual_partners, first_sexual_intercorse,
         smokes_years, hormonal_anticonceptives, stds_hepatitis_b,
         stds_condylomatosis, dx_hpv, stds_genital_herpes, stds_syphilis,
         stds_hiv, stds_mulluscum, dx_cin, dx_cancer)

cancer_data <- na.omit(cancer_data)

# realizando grafico de correlacion


correlations <- cor(cancer_data)

# cambiar nombre de columna y filas

corrplot(correlations, method = 'circle', tl.col = 'black')

# generando modelo

attach(cancer_data)

modelo_lineal <- lm(dx_hpv~dx_cancer, data = cancer_data)
summary(modelo_lineal) # y = 0.001437 + 0.880916 x
confint(modelo_lineal)

# obteniedo grupo con diagnostico de vph y sin diagnostico

dx_vph <- cancer_data %>%
  filter(dx_hpv == 1)

sano_vph <- cancer_data %>%
  filter(dx_hpv == 0)

