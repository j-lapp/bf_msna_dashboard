process_tables <-  function (data, join_value, ind_vector) {
  
  #  supprimer colonnes
  data %<>% dplyr::select(-c(question_de_recherche, sous_question_de_recherche, groupe_de_population, indicateur, sous_ensemble_de_donnee))
  
  # transposer
  data <- t(data) %>%
    janitor::row_to_names(row_number = 1) %>% 
    as.data.frame()
  
  data <- data.table::setDT(data, keep.rownames = TRUE)[]
  
  colnames(data)[1] <- join_value
  
  # convertir les indicateurs a format numerique
  data %<>% purrr::map_if(names(.) %in% ind_vector, as.numeric, .else = as.character)
}

# reach rouge palette
palette_func <- function (select_value1, select_value2, select_color_palette) {
  # determinez max et min de le palette - combination de valeurs admin1 et admin2
  min_admin1 <- min(as.numeric(select_value1), na.rm =T)
  min_admin2 <- min(as.numeric(select_value2), na.rm =T)
  min <- ifelse(min_admin1 < min_admin2, min_admin1,  min_admin2)

  max_admin1 <- max(as.numeric(select_value1), na.rm =T)
  max_admin2 <- max(as.numeric(select_value2), na.rm =T)
  max <- ifelse(max_admin1 > max_admin2, max_admin1, max_admin2)

  colorBin(
    na.color = reach_lt_grey,
    palette =  colorRamp(
      select_color_palette,
      interpolate = "spline"
    ),
  domain  = c(min, max),
  bins = 6
  #   ifelse(
  #   count_if(NA, select_value) + count_if(0, select_value) >= length(select_value),
  #   3,
  #   10
  # )
  )
}

# admin2 tooltip quand l'utilisateur mette un hover
tooltip_admin2 <- function (select_color, select_pal, admin1, admin2, select_value, select_indicator, select_ind_type) {
  sprintf(
  '<strong><span style="font-size: 20px; color: %s;">%s </span><br>
  <span style="font-size: 15px; color: %s;">%s</span><br>
  <p>Donnees niveau province</p>
  <p style="font-size: 10px; color: %s;">%s</p>
  <span style ="font-size:%s; color:%s">%s</span></strong>',
  select_color,
  admin2, 
  reach_grey, 
  admin1,
  reach_grey, 
  paste(strwrap(select_indicator,30), collapse="<br>"),
  ifelse(is.na(select_value), "12px", "20px"),
  ifelse(is.na(select_value), reach_grey, select_pal),
  ifelse(is.na(select_value), "Donnees pas disponibles", 
         paste0(formatC(select_value,1,format="f", big.mark=",", digits=1), 
                ifelse(select_ind_type == "pc", "%", "")))) %>%
  lapply(htmltools::HTML)
  
}

# admin2 tooltip quand l'utilisateur mette un hover
tooltip_admin1 <- function (select_color, admin1, select_value, select_indicator, select_ind_type) {
  sprintf(
    '<strong><span style="font-size: 20px; color: %s;">%s </span><br>
    <p>Donnees niveau region</p>
    <p style="font-size: 10px; color: %s;">%s</p>
    <span style="font-size: %s; color: %s;">%s</span></strong>',
    reach_grey,
    admin1, 
    reach_grey,
    paste(strwrap(select_indicator,30), collapse="<br>"),
    ifelse(is.na(select_value), "12px", "20px"),
    ifelse(is.na(select_value),reach_grey, select_color), 
    ifelse(is.na(select_value), "Donnees pas disponibles", 
           paste0(formatC(select_value,1,format="f", big.mark=",", digits=1), 
                  ifelse(select_ind_type == "pc", "%", "")))) %>% 
    lapply(htmltools::HTML)
}


