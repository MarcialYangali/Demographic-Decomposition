# Function for generic Kitagawa
kitagawa_generic <- function(
        data,
        group_col, # ejemplo: "region"
        stratum_col, # ejemplo: "age"
        ref, # ejemplo: "Nacional"
        cmp, # ejemplo: "Noreste"
        rate_col = NULL, # ejemplo: "d"
        struct_col = NULL, # ejemplo: "s"
        return = c("summary","by_stratum","both") # qué devolver
){
    return <- match.arg(return)
    
    # Datos por grupo y estrato
    df <- data |>
        dplyr::select(
            !!group_col := dplyr::all_of(group_col),
            !!stratum_col := dplyr::all_of(stratum_col),
            dplyr::all_of(c(rate_col, struct_col)) |> stats::na.omit()
        )
    
    # Extraemos A y B (ref y cmp)
    A <- df |>
        dplyr::filter(.data[[group_col]] == ref) |>
        dplyr::select(
            !!stratum_col := dplyr::all_of(stratum_col),
            r_A = dplyr::all_of(rate_col),
            s_A = dplyr::all_of(struct_col)
        )
    
    B <- df |>
        dplyr::filter(.data[[group_col]] == cmp) |>
        dplyr::select(
            !!stratum_col := dplyr::all_of(stratum_col),
            r_B = dplyr::all_of(rate_col),
            s_B = dplyr::all_of(struct_col)
        )
    
    merged <- dplyr::full_join(A, B, by = stratum_col) |>
        dplyr::mutate(dplyr::across(
            .cols = c(r_A, r_B, s_A, s_B),
            ~ tidyr::replace_na(.x, 0)
        )) |>
        dplyr::mutate(
            C_rate = (r_A - r_B) * (s_A + s_B) / 2,
            C_comp = (s_A - s_B) * (r_A + r_B) / 2,
            cont_R_A = r_A * s_A,
            cont_R_B = r_B * s_B
        )
    
    # Resumen global
    summary_out <- merged |>
        dplyr::summarise(
            R_A = sum(cont_R_A, na.rm = TRUE),
            R_B = sum(cont_R_B, na.rm = TRUE),
            diff_total = R_A - R_B,
            RE = sum(C_rate, na.rm = TRUE),
            CE = sum(C_comp, na.rm = TRUE)) |>
        ungroup() |> 
        dplyr::mutate(
            ref = ref,
            cmp = cmp,
            # Relativos
            pct_RE = dplyr::if_else(
                diff_total != 0, 100 * RE / diff_total, NA_real_),
            pct_CE = dplyr::if_else(
                diff_total != 0, 100 * CE / diff_total, NA_real_)
        ) |>
        dplyr::relocate(ref, cmp)
    
    # Resumen por estrato
    by_stratum_out <- merged |>
        select(-r_A, -s_A, -r_B, -s_B) |> 
        dplyr::mutate(
            !!stratum_col := .data[[stratum_col]],
            ref = ref,
            cmp = cmp) |> 
        group_by(cmp) |> 
        mutate(
            R_A = sum(cont_R_A, na.rm = TRUE),
            R_B = sum(cont_R_B, na.rm = TRUE),
            diff_total = R_A - R_B) |> 
        ungroup() |> 
        dplyr::mutate(
            diff = cont_R_A - cont_R_B) |> 
        rename(
            RE = C_rate,
            CE = C_comp) |> 
        # Relativos
        mutate(
            pct_RE = dplyr::if_else(
                diff != 0, 100 * RE / diff_total, NA_real_),
            pct_CE = dplyr::if_else(
                diff != 0, 100 * CE / diff_total, NA_real_),
            pct_STRATUM = pct_RE+pct_CE
        ) |> 
        dplyr::relocate(ref, cmp)
    
    # Opción de salida:
    if (return == "summary") return(summary_out)
    if (return == "by_stratum") return(by_stratum_out)
    list(summary = summary_out, by_stratum = by_stratum_out)
}

# Helper for multiple comparisons

kitagawa_vs_ref <- function(
        data, group_col, stratum_col, ref,
        rate_col = NULL, struct_col = NULL,
        return = c("summary","by_stratum","both")
){
    return <- match.arg(return)
    grupos <- setdiff(unique(data[[group_col]]), ref)
    purrr::map_dfr(grupos, \(g)
                   kitagawa_generic(
                       data, group_col, stratum_col, ref = ref, cmp = g,
                       rate_col = rate_col, struct_col = struct_col,
                       return = return
                   ) |>
                       dplyr::mutate(!!group_col := g)
    ) |>
        dplyr::relocate(dplyr::all_of(group_col), .after = cmp)
}