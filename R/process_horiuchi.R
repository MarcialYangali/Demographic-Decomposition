# =============================================================================
# Post-process Horiuchi outputs
# =============================================================================

process_horiuchi <- function(horiuchi, datos_decomp, age5_levels) {
    
    ids <- datos_decomp |>
        dplyr::filter(region == "Nacional") |>
        dplyr::mutate(id = paste0(age5, "__", discapacidad)) |>
        dplyr::pull(id)
    
    param_names <- c(
        paste0("d__", ids),
        paste0("s__", ids),
        paste0("h__", ids)
    )
    
    for (region in names(horiuchi)) {
        names(horiuchi[[region]]) <- param_names
    }
    
    horiuchi_df <- purrr::imap_dfr(
        horiuchi,
        ~ tibble::tibble(
            region = .y,
            parameter = names(.x),
            contribution = as.numeric(.x)
        )
    ) |>
        tidyr::separate(
            parameter,
            into = c("component", "age5", "discapacidad"),
            sep = "__",
            convert = TRUE
        ) |>
        dplyr::mutate(
            age5 = factor(age5, levels = age5_levels),
            discapacidad = factor(
                discapacidad,
                levels = unique(datos_decomp$discapacidad)
            )
        )
    
    return(horiuchi_df)
}
