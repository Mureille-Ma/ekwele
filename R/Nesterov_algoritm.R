#' nesterov_gradient_descent
#'
#' Implémentation de la descente de gradient avec Nesterov
#'
#' @param f
#' @param alpha
#' @param x0
#' @param iterations
#' @param beta
#'
#' @return
#' @export
#'
#' @examples gradient  <- nesterov_gradient_descent(f, alpha, x0, iterations, beta)
nesterov_gradient_descent <- function(f, alpha, x0, iterations, beta) {
  x <- x0
  history <- c(x)
  v <- 0  # Initialisation de la vitesse
  for (i in 1:iterations) {
    # Calcul du gradient anticipé
    grad <- (f(x + beta * v + 0.0001) - f(x + beta * v)) / (beta * 0.0001)
    # Mise à jour de la vitesse
    v <- beta * v + alpha * grad
    # Mise à jour de la position
    x <- x - v
    history <- c(history, x)
  }
  return(list(x_min = x, history = history))
}

# Définition de la fonction f(x)
#' function
#'
#' maintenant il faut que la faonction que l'utilisateur entre soit covex
#' genre ca admet un minimum
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
f <- function(x) {
  return ((x^2 + 3*x + 7) / (x + 2))
}

# Paramètres
alpha <- 0.01
x0 <- 50
iterations <- 2000
beta <- 0.9

#trace de la fonction f pour virsualiser

curve(f, from = -10, to = 10, col = "blue", xlab = "x", ylab = "f(x)", main = "Tracé de la fonction f(x)")


result_nesterov <- nesterov_gradient_descent(f, alpha, x0, iterations, beta)

cat("Valeur approchée du minimum avec Nesterov:", result_nesterov$x_min, "\n")

# Tracé de l'évolution du minimum
plot(result_nesterov$history, type = "l", col = "blue", xlab = "Iterations", ylab = "Valeur de x", main = "Évolution du minimum avec Nesterov")

