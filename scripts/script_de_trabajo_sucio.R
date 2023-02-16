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
         smokes, smokes_years, smokes_packs_year, hormonal_anticonceptives,
         dx_cancer)

# realizando grafico de correlacion

