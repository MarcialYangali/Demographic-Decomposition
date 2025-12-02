# =============================================================================
# Prepare data for Horiuchi decomposition
# =============================================================================

prepare_horiuchi_data <- function(df, discapacidades, age5_levels) {
    
    datos <- list()
    
    for (i in discapacidades) {
        
        # --------- State ---------
        estatal <- df |>
            dplyr::group_by(region, age5) |>
            dplyr::summarise(
                S = sum(pop, na.rm = TRUE),
                D = sum(.data[[i]], na.rm = TRUE),
                heatdays = weighted.mean(heatdays, w = pop, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::group_by(region) |>
            dplyr::mutate(
                s = S / sum(S, na.rm = TRUE)
            ) |>
            dplyr::ungroup() |>
            dplyr::mutate(
                d = D / S,
                h = heatdays / 366
            ) |>
            dplyr::group_by(region) |>
            dplyr::mutate(
                exposure = sum(d * s * h * 100, na.rm = TRUE)
            ) |>
            dplyr::ungroup() |>
            dplyr::mutate(discapacidad = i)
        
        # --------- National ---------
        nacional <- df |>
            dplyr::group_by(age5) |>
            dplyr::summarise(
                S = sum(pop, na.rm = TRUE),
                D = sum(.data[[i]], na.rm = TRUE),
                heatdays = weighted.mean(heatdays, w = pop, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::mutate(region = "Nacional") |>
            dplyr::group_by(region) |>
            dplyr::mutate(
                s = S / sum(S, na.rm = TRUE)
            ) |>
            dplyr::ungroup() |>
            dplyr::mutate(
                d = D / S,
                h = heatdays / 366
            ) |>
            dplyr::group_by(region) |>
            dplyr::mutate(
                exposure = sum(d * s * h * 100, na.rm = TRUE)
            ) |>
            dplyr::ungroup() |>
            dplyr::mutate(discapacidad = i)
        
        datos[[i]] <- dplyr::bind_rows(estatal, nacional)
    }
    
   
    datos <- dplyr::bind_rows(datos) |>
        dplyr::mutate(
            age5 = factor(age5, levels = age5_levels)
        )
    
    return(datos)
}
