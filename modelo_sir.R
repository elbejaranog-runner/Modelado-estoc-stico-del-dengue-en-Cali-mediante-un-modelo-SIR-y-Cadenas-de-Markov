###############################################################################
# modelo_sir.R
# Ajuste Bayesiano de un modelo SIR discreto con JAGS
# Proyecto: Modelado estocástico del dengue en Cali
###############################################################################

library(rjags)
library(coda)
library(ggplot2)
library(bayesplot)

# ---------------------------------------------------------------------------
# 1. Cargar datos procesados
# ---------------------------------------------------------------------------

load("datos/datos_procesados.RData")

# ---------------------------------------------------------------------------
# 2. Parámetros fijos y priors
# ---------------------------------------------------------------------------

gamma <- 1        # salida por semana
rho   <- 1/6      # proporción de infecciones que llegan a SIVIGILA
N0    <- 2500000  # población aproximada de Cali
I0    <- max(C_obs[1:3])  # infectados iniciales (aprox)

beta_mu  <- 2.1
beta_sd  <- 0.7
beta_prec <- 1 / beta_sd^2

data_jags <- list(
  C         = C_obs,
  T         = T,
  gamma     = gamma,
  rho       = rho,
  N0        = N0,
  I0        = I0,
  beta_mu   = beta_mu,
  beta_prec = beta_prec
)

# ---------------------------------------------------------------------------
# 3. Inicialización
# ---------------------------------------------------------------------------

inits_function <- function() {
  list(
    S0   = runif(1, 0.2, 0.8),
    tauC = runif(1, 0.01, 0.2),
    beta = rnorm(1, beta_mu, beta_sd)
  )
}

# ---------------------------------------------------------------------------
# 4. Compilar modelo
# ---------------------------------------------------------------------------

modelo <- jags.model(
  file    = "codigo/sir_cali_nullmodel.jags",
  data    = data_jags,
  inits   = inits_function,
  n.chains = 4,
  n.adapt = 80000
)

# ---------------------------------------------------------------------------
# 5. Burn-in
# ---------------------------------------------------------------------------

update(modelo, n.iter = 50000)

# ---------------------------------------------------------------------------
# 6. MCMC
# ---------------------------------------------------------------------------

params <- c("S0", "beta", "tauC")

muestras <- coda.samples(
  modelo,
  variable.names = params,
  n.iter = 100000,
  thin   = 10
)

save(muestras, file = "codigo/muestras_parametros.RData")

# ---------------------------------------------------------------------------
# 7. Diagnósticos
# ---------------------------------------------------------------------------

print(summary(muestras))
print(gelman.diag(muestras))

png("figuras/fig_traceplots_parametros.png", width = 2800, height = 1600, res = 300)
plot(muestras)
dev.off()

# ---------------------------------------------------------------------------
# 8. Extraer posterior
# ---------------------------------------------------------------------------

posterior_mat <- as.matrix(muestras)

save(posterior_mat, file = "codigo/posterior_parametros.RData")

message("✔ Modelo SIR ajustado y resultados guardados.")
