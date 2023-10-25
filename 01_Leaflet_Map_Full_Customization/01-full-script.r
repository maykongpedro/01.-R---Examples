
# Load all ---------------------------------
libs <- c(
  "tidyverse", # data manipulation
  "here", # folders and path manipulation
  "leaflet", # leaflet library
  "leaflet.extras", # extras of leaflet
  "sf", # geodata manipulation
  "knitr", # for kable tables
  "BFS", # Swiss Federal Statistical Office Data
  "kableExtra" # html table
)

# checar se todos os pacotes necessários já estão instalados
installed_libs <- libs %in% rownames(
  installed.packages()
)

# instalar pacotes que não estão instalados
if(any(installed_libs == FALSE)){
  install.packages(
    libs[!installed_libs]
  )
}

# carregar pacotes (etapa opcional)
invisible(lapply(
  libs, library,
  character.only = TRUE
))


# Get data ---------------------------------

# Get commun last names by commune
tables_names <- BFS::bfs_get_catalog_tables(
  language =  "en",
  title = "last+names"
)

asset_names_commune <- tables_names |>
  dplyr::filter(stringr::str_detect(title, "commune")) |>
  dplyr::pull(number_asset)

# apenas o primeiro item da lista
# asset_names_commune <- asset_names_commune[1]
# asset_names_commune

meta_data <- BFS::bfs_get_asset_metadata(
  number_asset = asset_names_commune[1],
  language = "en"
)

# download the file
tmp <- tempfile()
BFS::bfs_download_asset(
  number_asset = asset_names_commune,
  destfile = tmp
)

# read temp file
temp_df <- readr::read_csv(file = tmp) |> janitor::clean_names()


# export temp file in rds format
temp_df |> readr::write_rds(
  here::here(
    "01_Leaflet_Map_Full_Customization",
    "data",
    "raw_data.rds"
  ),
  compress = "xz"
)


# Read data ---------------------------------

df <- readr::read_rds(
  file = here::here(
    "01_Leaflet_Map_Full_Customization",
    "data",
    "raw_data.rds"
  )
)

df


# Transform data ---------------------------------

# remove columns
df_tr <- df |>
  dplyr::select(
    -time_period,
    -obs_status
  ) |>
  print()


# Analyse data ---------------------------------

# top 5 by commune, rank and pct
df_top5 <- df_tr |>
  dplyr::arrange(dplyr::desc(pct_gde)) |>
  dplyr::group_by(gdename) |>
  dplyr::slice(1:5) |>
  dplyr::mutate(rank = dplyr::row_number()) |>
  dplyr::ungroup() |>
  print()

df_top5


# create html table for each commune
create_table <- function(name) {
  dados <- df_top5 |>
    dplyr::filter(gdename == name)

  tabela <- dados |>
    dplyr::select(
      rank,
      lastname,
      value,
      pct_gde
    ) |>
    dplyr::mutate(
     rank = paste0(rank, "."),
      pct_gde = paste0(pct_gde, "%")
    ) |>
    dplyr::rename(
      `Rank` = rank,
      `Último nome` = lastname,
      `Number` = value,
      `Percentual` = pct_gde
    ) |>
    kableExtra::kable(format = "html", align = "llrr")

  # adicionar o nome como um título em negrito acima da tabela
  tabela_final <- paste0(
    "<b>",
    unique(dados$gdename),
    "</b><br>",
    tabela
  )

  return(tabela_final)

}

# ver a tabela criada
create_table(name = "Aarau") |>
  htmltools::HTML() |>
  htmltools::html_print()



# html table code in the `table` column
df_with_tables <- df_top5 |>
  dplyr::filter(rank == 1) |>
  dplyr::mutate(
    # criar uma coluna com o código html de cada tabela de acordo com o nome
    table = purrr::map_chr(
      .x = gdename,
      .f = create_table,
      .progress = TRUE
    )
  )

df_with_tables


# Get official Swiss base maps --------------------------------------------
