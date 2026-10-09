library(data.table)
library(arrow)
library(sf)
library(ggplot2)

# 1. Parâmetros (fáceis de mudar)
caminho_shp      <- "data/SHP"
arquivo_bacia    <- "bacia.shp"     # contorno da Bacia do Rio Fragoso (usado no buffer)
arquivo_talvegue <- "talbegue.shp"  # rio principal (só para desenhar no mapa)
raio_m   <- 10000  # raio da área de influência em metros (10 km)
epsg_utm <- 31985  # SIRGAS 2000 / UTM zona 25S (metros)
epsg_geo <- 4674   # SIRGAS 2000 geográfico (graus), sistema das coordenadas do CEMADEN

caminho_silver <- "data/silver"

# 2. Leitura da bacia e verificação do sistema de coordenadas
bacia_bruta <- st_read(file.path(caminho_shp, arquivo_bacia), quiet = TRUE)

# Sem .prj não há como saber em que sistema estão as coordenadas: para em vez de supor
if (is.na(st_crs(bacia_bruta))) {
  stop("O shapefile da bacia não tem sistema de coordenadas (.prj ausente). Verifique o arquivo.")
}

message(paste("Bacia:", nrow(bacia_bruta), "feição(ões) | CRS:", st_crs(bacia_bruta)$Name))

# 3. Bacia em UTM, com as partes unidas e a geometria corrigida
# O polígono original tem uma autointerseção no contorno; st_make_valid corrige sem mudar a área
bacia <- bacia_bruta |>
  st_transform(epsg_utm) |>
  st_union() |>
  st_make_valid()

# 4. Área de influência: 10 km em volta do contorno da bacia
area_10km <- st_buffer(bacia, dist = raio_m)

# Talvegue só para o mapa
talvegue <- st_read(file.path(caminho_shp, arquivo_talvegue), quiet = TRUE) |>
  st_transform(epsg_utm)

# 5. Série diária da Silver e tabela de estações sem repetição
df_diario <- as.data.table(read_parquet(file.path(caminho_silver, "chuvas_diarias_por_estacao.parquet")))

# Algumas estações mudam de coordenada ao longo do tempo (atualização de cadastro do CEMADEN).
# Usa a coordenada mais recente (cadastro vigente) e guarda em n_coords quantas posições diferentes
# a estação teve, como alerta para conferência.
estacoes <- df_diario[order(codEstacao, -data)][, .(
  nomeEstacao = first(nomeEstacao),
  municipio   = first(municipio),
  latitude    = first(latitude),
  longitude   = first(longitude),
  n_coords    = uniqueN(paste(latitude, longitude))
), by = codEstacao]

message(paste("Estações na série diária:", nrow(estacoes)))

# Pontos em graus (atenção à ordem: longitude, latitude) convertidos para UTM
estacoes_sf <- st_as_sf(estacoes, coords = c("longitude", "latitude"), crs = epsg_geo, remove = FALSE) |>
  st_transform(epsg_utm)

# 6. Classificação: dentro da área de 10 km = "Aceita"
dentro <- lengths(st_intersects(estacoes_sf, area_10km)) > 0
estacoes_sf$situacao <- ifelse(dentro, "Aceita", "Rejeitada")

# Distância até a bacia em km (0 = estação dentro da bacia), para conferência
estacoes_sf$dist_bacia_km <- round(as.numeric(st_distance(estacoes_sf, bacia)) / 1000, 3)

estacoes_class <- as.data.table(st_drop_geometry(estacoes_sf))[order(dist_bacia_km)]
aceitas <- estacoes_class[situacao == "Aceita", codEstacao]

# 7. Série diária só com as estações aceitas
df_fragoso <- df_diario[codEstacao %in% aceitas]

# 8. Salvar na Silver
write_parquet(df_fragoso, file.path(caminho_silver, "chuvas_diarias_fragoso_10km.parquet"))
fwrite(df_fragoso, file.path(caminho_silver, "chuvas_diarias_fragoso_10km.csv"),
       sep = ";", dec = ",", bom = TRUE)

fwrite(estacoes_class[, .(codEstacao, nomeEstacao, municipio, latitude, longitude,
                          dist_bacia_km, situacao, n_coords)],
       file.path(caminho_silver, "estacoes_classificadas_10km.csv"),
       sep = ";", dec = ",", bom = TRUE)

# 9. Mapa: área de 10 km, bacia, talvegue e estações coloridas por situação
# Zoom na área de 10 km com uma folga de 3 km para mostrar as rejeitadas mais próximas
zoom <- st_bbox(st_buffer(area_10km, 3000))

mapa <- ggplot() +
  geom_sf(data = area_10km, fill = "#DCE9F5", color = "#4A7DB0", linetype = "dashed") +
  geom_sf(data = bacia, fill = "#9CC3A0", color = "#2E6B3A", linewidth = 0.6) +
  geom_sf(data = talvegue, color = "#1F5FA8", linewidth = 0.4) +
  geom_sf(data = estacoes_sf, aes(color = situacao, shape = situacao), size = 2.2) +
  scale_color_manual(values = c(Aceita = "#1B7F3B", Rejeitada = "#C0392B"), name = "Situação") +
  scale_shape_manual(values = c(Aceita = 16, Rejeitada = 4), name = "Situação") +
  coord_sf(xlim = zoom[c("xmin", "xmax")], ylim = zoom[c("ymin", "ymax")], crs = epsg_utm, datum = epsg_geo) +
  labs(
    title = "Estações pluviométricas a até 10 km da Bacia do Rio Fragoso",
    subtitle = paste0("Área tracejada: ", raio_m / 1000, " km em volta da bacia | Aceitas: ", length(aceitas),
                      " | Rejeitadas: ", nrow(estacoes_class) - length(aceitas)),
    caption = "Fonte: CEMADEN | SIRGAS 2000 / UTM 25S"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(caminho_silver, "mapa_estacoes_10km.png"), mapa, width = 8, height = 8, dpi = 300, bg = "white")

# 10. Resumo no console
message(paste("Estações aceitas:", length(aceitas), "| rejeitadas:", nrow(estacoes_class) - length(aceitas)))
message(paste("Linhas diárias mantidas:", nrow(df_fragoso), "de", nrow(df_diario)))

message("Estações aceitas:")
print(estacoes_class[situacao == "Aceita", .(codEstacao, nomeEstacao, municipio, dist_bacia_km, n_coords)])

# Estações perto do limite (entre 9 e 11 km), onde pequenos erros de coordenada mudam a classificação
message("Estações próximas do limite de 10 km (entre 9 e 11 km):")
print(estacoes_class[dist_bacia_km >= 9 & dist_bacia_km <= 11,
                     .(codEstacao, nomeEstacao, municipio, dist_bacia_km, situacao, n_coords)])
