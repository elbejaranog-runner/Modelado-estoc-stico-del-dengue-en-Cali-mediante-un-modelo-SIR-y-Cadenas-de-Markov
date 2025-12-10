###############################################################################
# funciones_utiles.R
# Funciones auxiliares para análisis del modelo SIR y visualización
###############################################################################

library(dplyr)
library(ggplot2)

# ---------------------------------------------------------------------------
# Resumen de posterior
# ---------------------------------------------------------------------------

resumir_posterior <- function(vec) {
  data.frame(
    mean = mean(vec),
    sd   = sd(vec),
    q2.5 = quantile(vec, 0.025),
    q50  = quantile(vec, 0.50),
    q97.5= quantile(vec, 0.975)
  )
}

# ---------------------------------------------------------------------------
# SIR discreto auxiliar
# ---------------------------------------------------------------------------

sir_step <- function(S, I, R, beta, gamma, N) {
  lambda <- beta * I / N
  new_inf <- rbinom(1, S, lambda)
  new_rec <- rbinom(1, I, gamma)
  
  S_new <- S - new_inf
  I_new <- I + new_inf - new_rec
  R_new <- R + new_rec
  
  return(c(S_new, I_new, R_new))
}

# ---------------------------------------------------------------------------
# Graficar curvas con intervalo
# ---------------------------------------------------------------------------

plot_interval <- function(df, var, color_line, color_band, titulo, yl) {
  
  ggplot(df, aes(x = semana)) +
    geom_ribbon(
      aes(ymin = q2.5, ymax = q97.5),
      fill = color_band, alpha = 0.25
    ) +
    geom_line(
      aes(y = q50),
      color = color_line, linewidth = 1.2
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = titulo,
      x = "Semana epidemiológica",
      y = yl
    )
}

message("✔ funciones_utiles cargado correctamente.")
