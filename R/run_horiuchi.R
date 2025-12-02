# =============================================================================
# Run Horiuchi decomposition for all regions
# =============================================================================

run_horiuchi <- function(datos_decomp, age5_levels) {
    
    exposure_fun <- function(pars) {
        n <- length(pars) / 3
        s <- pars[ (n+1):(2*n) ]
        d <- pars[ 1:n ]
        h <- pars[ (2*n+1):(3*n) ]
        sum(d * s * h * 100)
    }
    
    regiones <- names(table(datos_decomp$region))
    
    ref <- regiones[regiones == "Nacional"]
    regiones <- regiones[regiones != "Nacional"]
    
    data_list <- split(datos_decomp, datos_decomp$region)
    parameters <- lapply(data_list, function(df) c(df$d, df$s, df$h))
    
    horiuchi <- list()
    
    for (region in regiones) {
        horiuchi[[region]] <- DemoDecomp::horiuchi(
            func = exposure_fun,
            pars1 = parameters[[ref]],
            pars2 = parameters[[region]],
            N = 40
        )
    }
    
    return(horiuchi)
}
