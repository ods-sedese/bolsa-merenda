library(tidyverse)
library(openxlsx)

setwd("/srv/shiny-server/bolsa-merenda")

# Read db
pobre <- read_csv("data/Por_municipio_Nova_Sintese_Alunos_e_familias_pobres_contemplados_pelo_Bolsa_Merenda.csv")
ext_pobre <- read_csv("data/Por_municipio_Nova_Sintese_Alunos_e_familias_ext_pobres_contemplados_pelo_Bolsa_Merenda.csv")
mg_dados <- read.xlsx("data/mg_dados.xlsx") %>%
  select(Código.IBGE, Município) %>%
  rename(`Código IBGE` = Código.IBGE)

# Clean ext pobre
ext_pobre <- ext_pobre %>%
  filter(!is.na(`Código IBGE`))

# Original
munic <- read_csv("data/dados_por_municipio.csv")

# Select variaveis
geral <- ext_pobre %>%
  left_join(pobre, by = c("Código IBGE" = "Código.IBGE"), suffix = c("", "_2")) %>%
  select(3,1,2, 5, 7, 17, 24, 26, 36, 6, 9, 18, 25, 28, 37, 38, 19, 38) %>%
  mutate(across(everything(), ~replace_na(., 0)),
         `Valor repassado aos beneficiários` = `Total de dinheiro repassado aos beneficiários da lista atual de extremamente pobres` + total_dinheiro_lista_atual) %>%
  select(-c(16, 17)) %>%
  left_join(mg_dados, by = "Código IBGE", suffix = c("", "_2")) %>%
  mutate(Município = Município_2) %>%
  select(-Município_2)

# Change colnames
col_names <- colnames(munic)
colnames(geral) <- col_names

# Create regional data

# Read original data
regional <- read_csv("data/dados_por_regional_sedese.csv")

regional_2 <- geral %>%
  group_by(`Diretoria Regional`) %>%
  summarise(across(c(3,4, 6,7,9,10,12,13), sum)) %>%
  group_by(`Diretoria Regional`) %>%
  transmute(`Percentual de alunos extremamente pobres que receberam o benefício` = `Número de alunos extremamente pobres que receberam o benefício` / `Número de alunos extremamente pobres potenciais beneficiários` * 100,
            `Percentual de alunos pobres que receberam o benefício` = `Número de alunos pobres que receberam o benefício` / `Número de alunos pobres potenciais beneficiários` * 100,
            `Percentual de famílias extremamente pobres que receberam o benefício` = `Número de famílias extremamente pobres que receberam o benefício` / `Número de famílias extremamente pobres potenciais beneficiárias` * 100,
            `Percentual de famílias pobres que receberam o benefício` = `Número de famílias pobres que receberam o benefício` / `Número de famílias pobres potenciais beneficiárias` * 100)

write_csv(geral, "data/dados_por_municipio.csv")
write_csv(regional_2, "data/dados_por_regional_sedese.csv")
