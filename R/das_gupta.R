# =============================================================================
# Das Gupta decomposition: s (age), d (disability), h (heat)
# =============================================================================

das_gupta <- function(df, ref_region, cmp_region) {
    
    # extract reference and comparison
    A <- df |>
        dplyr::filter(region == ref_region) |>
        dplyr::select(age5, s_A = s, d_A = d, h_A = h)
    
    B <- df |>
        dplyr::filter(region == cmp_region) |>
        dplyr::select(age5, s_B = s, d_B = d, h_B = h)
    
    X <- dplyr::full_join(A, B, by = "age5") |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::where(is.numeric),
                .fns  = ~ tidyr::replace_na(.x, 0)
            )
        )
    
    rate_fun <- function(s, d, h) {
        100 * sum(s * d * h, na.rm = TRUE)
    }
    
    R_A <- rate_fun(X$s_A, X$d_A, X$h_A)
    R_B <- rate_fun(X$s_B, X$d_B, X$h_B)
    
    comp_A <- list(
        s = X$s_A,
        d = X$d_A,
        h = X$h_A
    )
    
    comp_B <- list(
        s = X$s_B,
        d = X$d_B,
        h = X$h_B
    )
    
    perms <- list(
        c("s", "d", "h"),
        c("s", "h", "d"),
        c("d", "s", "h"),
        c("d", "h", "s"),
        c("h", "s", "d"),
        c("h", "d", "s")
    )
    
    contrib_mat <- matrix(0, nrow = length(perms), ncol = 3)
    colnames(contrib_mat) <- c("s", "d", "h")
    
    for (i in seq_along(perms)) {
        
        order_i <- perms[[i]]
        
        current <- comp_A
        R_prev  <- rate_fun(current$s, current$d, current$h)
        
        for (j in seq_along(order_i)) {
            f <- order_i[[j]]
            
            current[[f]] <- comp_B[[f]]
            R_new <- rate_fun(current$s, current$d, current$h)
            
            contrib_mat[i, f] <- contrib_mat[i, f] + (R_new - R_prev)
            
            R_prev <- R_new
        }
    }
    
    eff_s <- mean(contrib_mat[, "s"])
    eff_d <- mean(contrib_mat[, "d"])
    eff_h <- mean(contrib_mat[, "h"])
    
    tibble::tibble(
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

