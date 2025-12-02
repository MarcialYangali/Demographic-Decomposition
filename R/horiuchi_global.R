# =============================================================================
# Compute global contributions from Horiuchi results
# =============================================================================

compute_horiuchi_global <- function(datos_decomp, horiuchi_df) {
    
    global_results <- 
        cbind(
            datos_decomp |> 
                dplyr::group_by(region) |> 
                dplyr::summarise(ref_exposure = unique(exposure)) |> 
                dplyr::filter(region == "Nacional") |> 
                dplyr::rename(ref_region = region),
            
            datos_decomp |> 
                dplyr::group_by(region) |> 
                dplyr::summarise(exposure = unique(exposure)) |>
                dplyr::filter(region != "Nacional")
        )
    
    global_results <- global_results |> 
        dplyr::mutate(
            diff = exposure - ref_exposure,
            diffprop = diff / ref_exposure * 100
        )
    
    global_horiuchi <- horiuchi_df |> 
        dplyr::group_by(region, component) |> 
        dplyr::summarise(total_contribution = sum(contribution)) |>
        dplyr::ungroup()
    
    global_results <- global_results |> 
        dplyr::left_join(global_horiuchi, by = "region") |> 
        dplyr::mutate(
            prop_contribution = total_contribution / ref_exposure * 100
        )
    
    return(global_results)
}
