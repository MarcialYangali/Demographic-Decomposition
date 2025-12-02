# Das Gupta
# My version adressing 4 components
# Needs to be adjusted for 3 or x components (generic version)

Exposure_cells <- function(d, g, s, h) {
    return(d * g * s * h)
}

das_gupta <- function(data, groupvar, ref_value, target_value) {
    # Orden con i, j, k
    data <- data %>% arrange({{ groupvar }}, i, j, k)
    
    # IDs usando i_j_k
    ids <- data %>%
        dplyr::filter({{ groupvar }} == ref_value) %>%
        dplyr::arrange(i, j, k) %>%
        dplyr::mutate(id = paste0(i, "_", j, "_", k)) %>%
        dplyr::pull(id)
    
    # Extraer vectores (ya con longitud = n_i * n_j * n_k)
    xA <- list(
        d = data %>% filter({{ groupvar }} == ref_value)  %>% pull(d),
        g = data %>% filter({{ groupvar }} == ref_value)  %>% pull(g),
        s = data %>% filter({{ groupvar }} == ref_value)  %>% pull(s),
        h = data %>% filter({{ groupvar }} == ref_value)  %>% pull(h)
    )
    xB <- list(
        d = data %>% filter({{ groupvar }} == target_value) %>% pull(d),
        g = data %>% filter({{ groupvar }} == target_value) %>% pull(g),
        s = data %>% filter({{ groupvar }} == target_value) %>% pull(s),
        h = data %>% filter({{ groupvar }} == target_value) %>% pull(h)
    )
    
    # Chequeos básicos de longitudes
    stopifnot(length(xA$d) == length(xA$g), length(xA$g) == length(xA$s), length(xA$s) == length(xA$h))
    stopifnot(length(xB$d) == length(xB$g), length(xB$g) == length(xB$s), length(xB$s) == length(xB$h))
    stopifnot(length(xA$d) == length(xB$d))
    
    # Chequeo de grid idéntico (i_j_k)
    grid_ref <- data %>% filter({{ groupvar }} == ref_value)  %>%
        transmute(grid = paste0(i, "_", j, "_", k)) %>% pull(grid)
    grid_tgt <- data %>% filter({{ groupvar }} == target_value) %>%
        transmute(grid = paste0(i, "_", j, "_", k)) %>% pull(grid)
    stopifnot(identical(grid_ref, grid_tgt))
    
    # Permutaciones sobre los 4 componentes
    factores <- names(xA)               # c("d","g","s","h")
    ncomp <- length(factores)
    permutaciones <- gtools::permutations(n = ncomp, r = ncomp, v = factores)
    
    contrib_por_param <- matrix(0, nrow = length(xA[[1]]), ncol = ncomp,
                                dimnames = list(ids, factores))
    
    for (p in 1:nrow(permutaciones)) {
        orden <- permutaciones[p, ]
        x_temp <- xB
        for (kk in seq_along(orden)) {
            fac <- orden[kk]
            x_temp_A <- x_temp
            x_temp_A[[fac]] <- xA[[fac]]
            
            before <- Exposure_cells(x_temp$d,   x_temp$g,   x_temp$s,   x_temp$h)
            after  <- Exposure_cells(x_temp_A$d, x_temp_A$g, x_temp_A$s, x_temp_A$h)
            
            delta <- before - after  # misma convención de signo
            contrib_por_param[, fac] <- contrib_por_param[, fac] + delta
            
            x_temp[[fac]] <- xA[[fac]]  # fijar fac en A
        }
    }
    
    contrib_por_param <- contrib_por_param / nrow(permutaciones)
    
    # Salida tidy con i, j, k
    contrib_df <- as.data.frame(contrib_por_param) %>%
        tibble::rownames_to_column("id") %>%
        tidyr::pivot_longer(-id, names_to = "component", values_to = "contribution") %>%
        tidyr::separate(id, into = c("age", "sex", "k"), sep = "_") %>%
        dplyr::mutate(
            age = factor(age, levels = unique(data$i)),
            sex = factor(sex, levels = unique(data$j)),
            k   = factor(k,   levels = unique(data$k)),
            component = dplyr::recode(component, d = "d")  # si prefieres "dis" en vez de "d"
        ) %>%
        dplyr::select(component, age, sex, k, contribution)
    
    contrib_df
}

# Helper for multiple comparisons
das_gupta_multi <- function(data, groupvar, ref_value = NULL) {
    data <- data %>% arrange({{ groupvar }}, i, j, k)
    grupos <- data %>% pull({{ groupvar }}) %>% unique()
    if (is.null(ref_value)) ref_value <- grupos[1]
    targets <- setdiff(grupos, ref_value)
    
    purrr::map_dfr(
        targets,
        function(tg) {
            out <- das_gupta(data, {{ groupvar }}, ref_value, tg)
            out %>%
                dplyr::mutate(!!rlang::as_name(rlang::ensym(groupvar)) := tg,
                              ref = ref_value, .before = 1)
        }
    ) %>%
        dplyr::relocate(ref, .after = !!rlang::as_name(rlang::ensym(groupvar)))
}