library(data.table)
library(sf)

# Diagnóstico das estações que aparecem com mais de uma coordenada na Bronze.
# Objetivo: saber se a diferença vem da leitura (ETL) ou do dado original do CEMADEN.

# 1. Parâmetros e diretorias relativas
caminho_bronze <- "data/bronze"
caminho_silver <- "data/silver"
epsg_geo <- 4674   # SIRGAS 2000 geográfico (graus), sistema das coordenadas do CEMADEN
epsg_utm <- 31985  # SIRGAS 2000 / UTM zona 25S (metros), para medir distâncias

# Retângulo aproximado de Pernambuco continental (graus), usado só como alerta
lim_pe <- list(lat = c(-9.5, -7.25), lon = c(-41.4, -34.8))

arquivos <- list.files(
  path = caminho_bronze,
  pattern = "^data.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)
message(paste("Ficheiros encontrados na camada Bronze:", length(arquivos)))

# 2. Checagem da estrutura dos CSVs
# Conta os separadores ";" de cada linha: com 8 colunas devem ser 7. Linhas vazias
# (0 separadores) são ignoradas pelo fread e não causam problema.
estrutura <- rbindlist(lapply(arquivos, function(f) {
  linhas <- readLines(f, encoding = "UTF-8", warn = FALSE)
  n_sep <- nchar(linhas, type = "bytes") - nchar(gsub(";", "", linhas, fixed = TRUE), type = "bytes")
  data.table(
    arquivo        = basename(f),
    md5            = unname(tools::md5sum(f)),
    linhas_ruins   = sum(n_sep != n_sep[1] & n_sep > 0),
    linhas_vazias  = sum(n_sep == 0)
  )
}))

message(paste("Linhas com nº de colunas diferente do cabeçalho:", sum(estrutura$linhas_ruins)))

# Ficheiros com conteúdo idêntico (mesmo md5) duplicariam as medições
repetidos <- estrutura[, .(arquivos = paste(arquivo, collapse = ", "), .N), by = md5][N > 1]
message(paste("Grupos de ficheiros idênticos:", nrow(repetidos)))
if (nrow(repetidos) > 0) print(repetidos)

# 3. Leitura da Bronze como texto, guardando o ficheiro de origem
# colClasses = "character" preserva a coordenada exatamente como veio (casas decimais, vírgula ou ponto)
bronze <- rbindlist(lapply(arquivos, function(f) {
  df <- fread(f, sep = ";", colClasses = "character", encoding = "UTF-8",
              select = c("codEstacao", "nomeEstacao", "municipio", "latitude", "longitude", "datahora"))
  df[, arquivo := basename(f)]
  return(df)
}))

# Dia da medição com formato explícito para cada padrão ("dd/mm/aaaa ..." ou "aaaa-mm-dd ...").
# Sem formato explícito o as.IDate escolhe um só padrão e anula as datas do outro.
bronze[, dia := fifelse(
  substr(datahora, 3, 3) == "/",
  as.IDate(substr(datahora, 1, 10), format = "%d/%m/%Y"),
  as.IDate(substr(datahora, 1, 10), format = "%Y-%m-%d")
)]
bronze[, datahora := NULL]

# Coordenadas numéricas (aceita vírgula ou ponto decimal)
bronze[, `:=`(
  lat = as.numeric(chartr(",", ".", latitude)),
  lon = as.numeric(chartr(",", ".", longitude))
)]

# 4. Cobertura temporal: cada ficheiro deve trazer um mês, sem lacunas nem repetições
periodo <- bronze[, .(ini = min(dia), fim = max(dia)), by = arquivo][order(ini)]
meses_arq <- format(periodo$ini, "%Y-%m")
meses_esperados <- format(seq(min(periodo$ini), max(periodo$fim), by = "month"), "%Y-%m")
message(paste("Meses ausentes:", paste(setdiff(meses_esperados, meses_arq), collapse = ", ")))
message(paste("Meses repetidos:", paste(unique(meses_arq[duplicated(meses_arq)]), collapse = ", ")))

# 5. Uma linha por estação e coordenada distinta
coords <- bronze[, .(
  nomeEstacao = paste(unique(nomeEstacao), collapse = " | "),
  municipio   = paste(unique(municipio), collapse = " | "),
  latitude_original  = paste(unique(latitude), collapse = " | "),
  longitude_original = paste(unique(longitude), collapse = " | "),
  n_medicoes  = .N,
  n_dias      = uniqueN(dia),
  primeira    = min(dia),
  ultima      = max(dia),
  arquivos    = paste(sort(unique(arquivo)), collapse = ", ")
), by = .(codEstacao, lat, lon)]

coords[, n_coords := .N, by = codEstacao]
coords <- coords[n_coords > 1][order(codEstacao, primeira)]
message(paste("Estações com mais de uma coordenada:", uniqueN(coords$codEstacao)))

# Menor nº de casas decimais entre as grafias da coordenada (poucas casas = possível truncamento)
casas <- function(x) nchar(sub("^[^,.]*[,.]?", "", x))
coords[, casas_decimais := pmin(casas(sub(" \\|.*", "", latitude_original)),
                                casas(sub(" \\|.*", "", longitude_original)))]

# Alerta para coordenada fora de Pernambuco (ou no mar, a leste do litoral)
coords[, fora_pe := !(lat %between% lim_pe$lat & lon %between% lim_pe$lon)]

# 6. Distância máxima entre as coordenadas de uma mesma estação
# Pontos em graus (EPSG 4674) convertidos para UTM (metros) antes de medir
pontos <- st_transform(st_as_sf(coords, coords = c("lon", "lat"), crs = epsg_geo, remove = FALSE), epsg_utm)

dist_max <- rbindlist(lapply(split(seq_len(nrow(pontos)), pontos$codEstacao), function(i) {
  d <- st_distance(pontos[i, ])
  data.table(codEstacao = pontos$codEstacao[i[1]], dist_max_km = as.numeric(max(d)) / 1000)
}))

dist_max[, faixa := fcase(
  dist_max_km < 0.5, "< 0,5 km (arredondamento)",
  dist_max_km <= 5,  "0,5 a 5 km (realocação?)",
  default =          "> 5 km (suspeito)"
)]

# 7. Padrão temporal: a coordenada muda numa data (realocação/cadastro) ou alterna (erro)?
diario <- unique(bronze[codEstacao %in% coords$codEstacao, .(codEstacao, dia, cc = paste(lat, lon))])
diario <- diario[order(codEstacao, dia, cc)][, .(cc = cc[1], n_cc = .N), by = .(codEstacao, dia)]

trocas <- diario[, .(
  trocas       = sum(cc != shift(cc), na.rm = TRUE),
  dias_2coords = sum(n_cc > 1),
  n_coords     = uniqueN(cc)
), by = codEstacao]
trocas[, padrao := fifelse(trocas <= n_coords - 1 & dias_2coords == 0, "muda em data", "alterna")]

resumo <- merge(dist_max, trocas[, .(codEstacao, trocas, dias_2coords, padrao)], by = "codEstacao")
diagnostico <- merge(coords, resumo, by = "codEstacao")[order(-dist_max_km, codEstacao, primeira)]
diagnostico[, dist_max_km := round(dist_max_km, 3)]

# 8. Resumo no console
message("Estações por faixa de distância:")
print(resumo[, .N, by = faixa][order(faixa)])
message("Padrão temporal por faixa:")
print(dcast(resumo, faixa ~ padrao, fun.aggregate = length, value.var = "codEstacao"))
message(paste("Coordenadas fora do retângulo de PE:", diagnostico[fora_pe == TRUE, .N]))
message("Estações suspeitas (> 5 km):")
print(diagnostico[dist_max_km > 5, .(codEstacao, municipio, lat, lon, n_dias, primeira, ultima, dist_max_km, padrao)])

# 9. Salvar o diagnóstico na Silver
fwrite(diagnostico[, .(codEstacao, nomeEstacao, municipio, latitude = lat, longitude = lon,
                       latitude_original, longitude_original, casas_decimais, n_medicoes, n_dias,
                       primeira, ultima, arquivos, n_coords, dist_max_km, faixa, trocas,
                       dias_2coords, padrao, fora_pe)],
       file.path(caminho_silver, "diagnostico_coordenadas.csv"),
       sep = ";", dec = ",", bom = TRUE)

message("Diagnóstico gravado em data/silver/diagnostico_coordenadas.csv")
