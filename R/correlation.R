#' Custom plot
#'
#' A customised plot used for generating the correlation
#' graphics in the handout material.
#' Not intended for other purposes.
#'
#' @param xy matrix of xy values
#' @param xlim numeric vector for x axis range
#' @param ylim numeric vector for y axis range
#' @param eps numeric tolerance for which cor not defined (don't divide by zero)
#' @importFrom stats cor sd
#' @importFrom graphics plot
#' @export
my_plot <- function(xy, xlim = c(-4, 4), ylim = c(-4, 4), eps = 1e-15) {
  title = round(cor(xy[, 1], xy[, 2]), 1)
  if (sd(xy[, 2]) < eps) title = "" # corr. coeff. is undefined
  plot(xy, main = title, xlab = "", ylab = "",
       col = "darkblue", pch = 16, cex = 0.2,
       xaxt = "n", yaxt = "n", bty = "n",
       xlim = xlim, ylim = ylim)
}

#' Multivariate normal
#'
#' Function for generating a plot of bivariate normal with a given correlation
#' between 2 variables. Created for the purpose of generating
#' correlation plots in the handout material. Not intended for other purposes.
#'
#' @param n number of point in scatter
#' @param cor correlation between the two variables
#' @importFrom mvtnorm rmvnorm
#' @export
mv_normal <- function(n = 1000, cor = 0.8) {
  for (i in cor) {
    sd = matrix(c(1, i, i, 1), ncol = 2)
    x = rmvnorm(n, c(0, 0), sd)
    my_plot(x)
  }
}

#' Apply a rotation to a matrix
#'
#' @param t angle
#' @param x matrix to rotate
#' @export
rotation <- function(t, x) {
  return(x %*% matrix(c(cos(t), sin(t), -sin(t), cos(t)), ncol = 2))
}

#' Rotated normal scatter
#'
#' For a bivariate normal apply a rotation and plot resultant scatter.
#' Created for the purpose of correlation plots in handout material.
#' Not intended for any other purpose
#'
#' @param n number of points in scatter
#' @param t angular rotation to apply
#' @importFrom mvtnorm rmvnorm
#' @export
rot_normal <- function(n = 1000, t = pi / 2) {
  sd = matrix(c(1, 1, 1, 1), ncol = 2)
  x = rmvnorm(n, c(0, 0), sd)
  for (i in t)
    my_plot(rotation(i, x))
}

#' Other correlation plots
#'
#' Generate scatter plots for handout material and slides. Gives plots
#' of correlations with assorted shapes. Created for the purpose of handout
#' material. Not intended for any other purpose
#'
#' @param n number of points in scatter
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats runif rnorm
#' @export
others <- function(n = 1000) {
  x = seq(-1, 1, length.out = n)
  y = 4 * (x ^ 2 - 1 / 2) ^ 2 + runif(n, -1, 1) / 3
  my_plot(cbind(x, y), xlim = c(-1, 1), ylim = c(-1 / 3, 1 + 1 / 3))

  y = runif(n, -1, 1)
  xy = rotation(-pi / 8, cbind(x, y))
  lim = sqrt(2 + sqrt(2)) / sqrt(2)
  my_plot(xy, xlim = c(-lim, lim), ylim = c(-lim, lim))

  xy = rotation(-pi / 8, xy)
  my_plot(xy, xlim = c(-sqrt(2), sqrt(2)), ylim = c(-sqrt(2), sqrt(2)))

  y = 2 * x ^ 2 + runif(n, -1, 1)
  my_plot(cbind(x, y), xlim = c(-1, 1), ylim = c(-1, 3))

  y = (x ^ 2 + runif(n, 0, 1 / 2)) * sample(seq(-1, 1, 2), n, replace = TRUE)
  my_plot(cbind(x, y), xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))

  y = cos(x * pi) + rnorm(n, 0, 1 / 8)
  x = sin(x * pi) + rnorm(n, 0, 1 / 8)
  my_plot(cbind(x, y), xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))

  xy1 = rmvnorm(n / 4, c(3,  3))
  xy2 = rmvnorm(n / 4, c(-3,  3))
  xy3 = rmvnorm(n / 4, c(-3, -3))
  xy4 = rmvnorm(n / 4, c(3, -3))
  my_plot(rbind(xy1, xy2, xy3, xy4), xlim = c(-3 - 4, 3 + 4),
         ylim = c(-3 - 4, 3 + 4))
}
