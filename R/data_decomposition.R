data_decomposition <- function(df) {
    df |>
        group_by(region, age5) |>
        summarise(
            S = sum(pop * 366, na.rm = TRUE),
            D = sum(popdis * 366, na.rm = TRUE),
            H = sum(pop * heatdays, na.rm = TRUE),
            .groups = "drop"
        ) |>
        group_by(region) |>
        mutate(
            s = S / sum(S, na.rm = TRUE),
            d = D / S,
            h = H / S
        ) |>
        ungroup()
}
