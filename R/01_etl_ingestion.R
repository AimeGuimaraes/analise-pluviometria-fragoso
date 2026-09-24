library(data.table)
library(lubridate)
library(arrow)

# 1. Definir diretorias relativas
caminho_bronze <- "data/bronze"
caminho_silver <- "data/silver"
dir.create(caminho_silver, showWarnings = FALSE, recursive = TRUE)

# 2. Listar ficheiros CSV na camada Bronze
arquivos <- list.files(
  path = caminho_bronze,
  pattern = "^data.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

message(paste("Ficheiros encontrados na camada Bronze:", length(arquivos)))

# Converte texto com vírgula decimal ("0,2") em número (0.2)
para_numero <- function(x) {
  if (is.character(x)) x <- as.numeric(gsub(",", ".", x, fixed = TRUE))
  as.numeric(x)
}
cols_num <- c("valorMedida", "latitude", "longitude")

# 3. Leitura em lote e tratamento de tipos de dados
lista_tabelas <- lapply(arquivos, function(f) {
  df <- fread(f, fill = TRUE)

  if ("datahora" %in% names(df)) {
    df[, datahora := as.character(datahora)]
  }

  # Padroniza as colunas numéricas em todos os ficheiros antes de empilhar
  cols_presentes <- intersect(cols_num, names(df))
  if (length(cols_presentes) > 0) {
    df[, (cols_presentes) := lapply(.SD, para_numero), .SDcols = cols_presentes]
  }
  return(df)
})

# 4. Empilhamento com alinhamento por nome de coluna
df_completo <- rbindlist(lista_tabelas, fill = TRUE, use.names = TRUE)

# 5. Limpeza, padronização temporal e ordenação cronológica
# O CEMADEN grava a telemetria em UTC; parse_date_time devolve em UTC por padrão.
df_completo[, data_padronizada := parse_date_time(
  datahora,
  orders = c("dmy HMS", "ymd HMS", "ymd HMSOS", "ymd", "dmy"),
  tz = "UTC"
)]

df_silver <- df_completo[!is.na(data_padronizada)][order(data_padronizada)]

# 6. Gravação na camada Silver (medições brutas padronizadas)
# Gravação em Apache Parquet (rápido e comprimido)
write_parquet(df_silver, file.path(caminho_silver, "chuvas_silver.parquet"))

message("Processamento Bronze -> Silver concluído com sucesso!")
print(head(df_silver))
print(tail(df_silver))

# 7. Agregação diária por estação 

# Soma que devolve NA (e não 0) quando todas as medições do dia são NA,
# para que falha de sensor não pareça dia seco no preenchimento de falhas.
soma_segura <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

# Máximo com o mesmo cuidado (max() de tudo NA daria -Inf com aviso)
max_seguro <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

# Converte de UTC para o horário local de Recife (UTC-3) antes de extrair o dia,
# para que a chuva entre 21h00 e 23h59 locais fique no dia correto.
df_silver[, data := as.Date(with_tz(data_padronizada, "America/Recife"))]

df_diario <- df_silver[, .(
  municipio      = first(municipio),
  uf             = first(uf),
  nomeEstacao    = first(nomeEstacao),
  latitude       = first(latitude),
  longitude      = first(longitude),
  chuva_total_mm = soma_segura(valorMedida),
  chuva_max_mm   = max_seguro(valorMedida),
  n_medicoes     = sum(!is.na(valorMedida))
), by = .(codEstacao, data)][order(codEstacao, data)]

# 8. Salvar a série diária padronizada na Silver
write_parquet(df_diario, file.path(caminho_silver, "chuvas_diarias_por_estacao.parquet"))
fwrite(df_diario, file.path(caminho_silver, "chuvas_diarias_por_estacao.csv"),
       sep = ";", dec = ",", bom = TRUE)

message(paste("Agregação diária concluída:", nrow(df_diario), "linhas (estação x dia)"))
print(head(df_diario))