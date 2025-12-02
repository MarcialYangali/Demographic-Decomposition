# =============================================================================
# Das Gupta three-way symmetric decomposition
# =============================================================================

das_gupta_threeway <- function(df, ref_region, cmp_region) {
    
    # Extract reference and comparison regions
    A <- df |> 
        dplyr::filter(region == ref_region) |>
        dplyr::select(age5, s_A = s, d_A = d, h_A = h)
    
    B <- df |>
        dplyr::filter(region == cmp_region) |>
        dplyr::select(age5, s_B = s, d_B = d, h_B = h)
    
    # Merge age groups
    X <- dplyr::full_join(A, B, by = "age5") |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::where(is.numeric),
                .fns  = ~ tidyr::replace_na(.x, 0)
            )
        )
    
    # Rates
    R_A <- 100 * sum(X$s_A * X$d_A * X$h_A)
    R_B <- 100 * sum(X$s_B * X$d_B * X$h_B)
    
    # =============================================================================
    # Order 1: s → d → h
    # =============================================================================
    R1_1 <- 100 * sum(X$s_B * X$d_A * X$h_A)
    R1_2 <- 100 * sum(X$s_B * X$d_B * X$h_A)
    
    eff_s_1 <- R1_1 - R_A
    eff_d_1 <- R1_2 - R1_1
    eff_h_1 <- R_B  - R1_2
    
    # =============================================================================
    # Order 2: h → d → s
    # =============================================================================
    R2_1 <- 100 * sum(X$s_A * X$d_A * X$h_B)
    R2_2 <- 100 * sum(X$s_A * X$d_B * X$h_B)
    
    eff_h_2 <- R2_1 - R_A
    eff_d_2 <- R2_2 - R2_1
    eff_s_2 <- R_B  - R2_2
    
    # =============================================================================
    # Order 3: d → s → h
    # =============================================================================
    R3_1 <- 100 * sum(X$s_A * X$d_B * X$h_A)
    R3_2 <- 100 * sum(X$s_B * X$d_B * X$h_A)
    
    eff_d_3 <- R3_1 - R_A
    eff_s_3 <- R3_2 - R3_1
    eff_h_3 <- R_B  - R3_2
    
    # =============================================================================
    # Symmetric averages (Das Gupta style)
    # =============================================================================
    eff_s <- mean(c(eff_s_1, eff_s_2, eff_s_3))
    eff_d <- mean(c(eff_d_1, eff_d_2, eff_d_3))
    eff_h <- mean(c(eff_h_1, eff_h_2, eff_h_3))
    
    dplyr::tibble(
        region = cmp_region,
        R_ref = R_A,
        R_cmp = R_B,
        total_diff = R_B - R_A,
        effect_age = eff_s,
        effect_disability = eff_d,
        effect_heat = eff_h,
        sum_effects = eff_s + eff_d + eff_h
    )
}
