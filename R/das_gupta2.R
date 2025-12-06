# Das gupta function
# For multiple factors
# For multiple strata variables 

# Auxiliar function for permutations based on the lenght of the factors
permute_vec <- function(x) {
    if (length(x) == 1L) return(list(x))
    res <- list()
    k <- 1L
    for (i in seq_along(x)) {
        rest <- x[-i]
        for (p in permute_vec(rest)) {
            res[[k]] <- c(x[i], p)
            k <- k + 1L
        }
    }
    res
}

# Das Gupta generalizado con contribuciones por fila
das_gupta <- function(
        df,
        ref,
        cmp,
        region_var,
        strata_vars,
        comp_vars, # en nuestro caso c("s","d","h")
        scale = 100) {
    
    region_var <- enquo(region_var)
    strata_syms <- rlang::syms(strata_vars)
    
    # Extraer referencia (A) y comparación (B)
    A <- df %>%
        filter(!!region_var == ref) %>%
        select(!!!strata_syms, all_of(comp_vars)) %>%
        rename_with(~ paste0(.x, "_A"), 
                    .cols = all_of(comp_vars))
    
    B <- df %>%
        filter(!!region_var == cmp) %>%
        select(!!!strata_syms, all_of(comp_vars)) %>%
        rename_with(~ paste0(.x, "_B"), 
                    .cols = all_of(comp_vars))
    
    # join por todas las variables de estrato
    X <- full_join(A, B, by = strata_vars) %>%
        mutate(across(where(is.numeric), 
                      ~ tidyr::replace_na(.x, 0)))
    
    # Preparar listas de componentes para A y B
    comp_A <- lapply(comp_vars, 
                     function(v) X[[paste0(v, "_A")]])
    
    names(comp_A) <- comp_vars
    
    comp_B <- lapply(comp_vars,
                     function(v) X[[paste0(v, "_B")]])
    
    names(comp_B) <- comp_vars
    
    # Número de filas (estratos) y componentes
    n <- nrow(X)
    k <- length(comp_vars)
    
    # Función "celda por celda" (sin sumar)
    cell_fun <- function(comp_list) {
        # Devuelve un vector de longitud n: scale * (s * d * h * ...)
        scale * Reduce(`*`, comp_list)
    }
    
    # Tasa total en A y B (para referencia global)
    cells_A <- cell_fun(comp_A)
    cells_B <- cell_fun(comp_B)
    R_A <- sum(cells_A)
    R_B <- sum(cells_B)
    
    # Todas las permutaciones de los componentes
    perms <- permute_vec(comp_vars) # como vectores en distinto orden
    
    # Matriz de contribuciones: filas = componentes, columnas = estratos
    contrib_cells <- matrix(
        0,
        nrow = k,
        ncol = n,
        dimnames = list(comp_vars, NULL))
    
    # Ciclo principal Das Gupta (pero vectorizado por fila)
    for (i in seq_along(perms)) {
        order_i <- perms[[i]]
        
        # Empezamos con todos los factores en el nivel A
        current <- comp_A
        R_prev_vec <- cell_fun(current) # vector de longitud n
        
        for (f in order_i) {
            # Sustituimos el factor f por su valor en B (en todas las filas)
            current[[f]] <- comp_B[[f]]
            R_new_vec <- cell_fun(current)  # vector n
            
            # Contribución del componente f en esta permutación y paso, por fila
            contrib_cells[f, ] <- contrib_cells[f, ] + (R_new_vec - R_prev_vec)
            
            # Actualizamos el "previo" para el siguiente reemplazo
            R_prev_vec <- R_new_vec
        }
    }
    
    # Promedio de contribuciones sobre todas las permutaciones
    effects_cells <- contrib_cells / length(perms)   # matriz k x n
    
    # Efectos globales por componente (sumando filas/estratos)
    effects_global <- rowSums(effects_cells)         # vector de longitud k
    
    # Tabla global
    effect_names <- paste0("effect_", comp_vars)
    effect_list  <- as.list(effects_global)
    names(effect_list) <- effect_names
    
    global_tbl <- tibble(
        region = cmp,
        R_ref = R_A,
        R_cmp = R_B,
        total_diff  = R_B - R_A,
        !!!effect_list,
        sum_effects = sum(effects_global)
    )
    
    # Tabla por fila y componente
    # Pasamos la matriz n x k a data.frame
    cell_effects_df <- as.data.frame(t(effects_cells))
    colnames(cell_effects_df) <- comp_vars
    
    cell_tbl <- bind_cols(
        X %>% select(!!!strata_syms), # estratos: age5, dis
        cell_effects_df) %>%
        mutate(contrib_total_row = rowSums(
            across(all_of(comp_vars)))) %>%
        pivot_longer(cols = all_of(comp_vars),
                     names_to = "component",
                     values_to = "effect" ) %>%
        mutate(ref = ref,
               cmp = cmp) %>%
        relocate(ref, cmp, .before = !!strata_vars)
    
    # Salida como lista con $global y $cell
    list(global = global_tbl,
        cell   = cell_tbl)
}

# EJEMPLO DE USO
# res <- das_gupta_general_detailed(
#     df = df,
#     ref = "Nacional",
#     cmp = "Yucatán",
#     region_var = region,
#     strata_vars = c("age5", "sexo"),
#     comp_vars = c("s", "d", "h"),
#     scale = 100
# )
# 
# res$global # Efectos globales
# res$cell # Efectos por fila