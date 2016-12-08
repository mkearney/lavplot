plot_blank <- function(w, h) {
    par(mar=c(0, 0, 0, 0))
    plot(1, type = "n", bty = "n",
         axes = FALSE, xlab = "", ylab = "",
         xlim = c(-w, w), ylim = c(-h, h),
         asp = 1)
}

#' plot_nodes
#'
#' @param x Vector of x coords with recommended min/max of
#'   -.75 to .75.
#' @param y Vector of y coords with recommended min/max of
#'   -.75 to .75.
#' @param cex Size of nodes defaults to .2
#' @param h height of plot defaults to 1.
#' @param w width of plot defaults to 1.
#' @param \dots Other args passed on to symbols().
#' @return Node diagram
#' @examples
#' cex <- .25
#' plot_nodes(c(-.75, -.75, -.75, 0,  .75, .75),
#'            c(-.75,    0,  .75, 0, -.5,  .5),
#'            cex = cex)
#' draw.arrow(-.75, -.75, 0, 0, cex)
#' draw.arrow(-.75, 0, 0, 0, cex)
#' draw.arrow(-.75, .75, 0, 0, cex)
#' draw.arrow(   0, 0, .75, .5, cex)
#' draw.arrow(   0, 0, .75, -.5, cex)
#' text(-.75, .75, "Education")
#' text(-.75, 0, "Age")
#' text(-.75, -.75, "Race")
#' text(0, 0, "Political\nEngagement")
#' text(.75, .5, "Political\nParticipation")
#' text(.75, -.5, "Civic\nEngagement")
#'
#' ## mediation model
#' cex <- .35
#' plot_nodes(c(-1.5, 0, 1.5), c(-.5, .5, -.5),
#'            w = 1.5, h = 1, cex)
#' draw.arrow(-1.5, -.5, 1.5, -.5, cex)
#' draw.arrow(-1.5, -.5,  .0 ,  .5, cex)
#' draw.arrow( 0, .5,   1.5,  -.5, cex)
#' text(-1.5, -.5, "x")
#' text(0, .5, "w")
#' text(1.5, -.5, "y")
#' @export
plot_nodes <- function(x, y, cex = .2, h = 1, w = 1, ...) {
    plot_blank(w, h)
    xs <- x
    ys <- y
    if (identical(length(cex), 1L)) cex <- rep(cex, length(x))
    sizes <- cex
    symbols(xs, ys, circles = sizes, bg = "#e5e5e5",
            fg = "#333333", inches = FALSE, add = TRUE, ...)   
}

prsq <- function(r) 1.05 * pi * r^2

#' draw.arrow
#'
#' @param x0 Starting x coord
#' @param y0 Starting y coord
#' @param x1 Ending x coord
#' @param y1 Ending x coord
#' @param cex Size of node (for spacing purposes).
#' @return Adds edge with closed arrow to plot.
#' @export
draw.arrow <- function(x0, y0, x1, y1, cex = .2) {
    dx <- abs(x1 - x0)
    dy <- abs(y1 - y0)
    if (cex < .2) cex <- .2 - (.2 - cex)/2
    if (cex > .2) cex <- .2 + (cex - .2)/2
    r <- cex + (cex * .5)
    slope.x <- dx / (dx + dy) 
    slope.y <- dy / (dx + dy)
    if (slope.x == 1) slope.x <- .7
    if (slope.y == 1) slope.y <- .7
    x0 <- x0 + (prsq(r) * slope.x)
    if (y1 >= y0) `%sgn%` <- function(a, b) a + b
    if (y1 < y0) `%sgn%` <- function(a, b) a - b
    y0 <- y0 %sgn% (prsq(r) * slope.y)
    x1 <- x1 - (prsq(r) * slope.x)
    if (y1 >= y0) `%sgn%` <- function(a, b) a - b
    if (y1 < y0) `%sgn%` <- function(a, b) a + b
    y1 <- y1 %sgn% (prsq(r) * slope.y)
    if (slope.y == 0L) {
        lwd <- 1.1
    } else {
        lwd <- .9
    }
    arrows(x0, y0, x1, y1, length = .075,
           angle = 30, lwd = lwd)
    arrows(x0, y0, x1, y1, length = .0725,
           angle = 24, lwd = .25)
    arrows(x0, y0, x1, y1, length = .0725,
           angle = 21, lwd = .25)
    arrows(x0, y0, x1, y1, length = .07,
           angle = 18, lwd = .25)
    arrows(x0, y0, x1, y1, length = .0675,
           angle = 15, lwd = .25)
    arrows(x0, y0, x1, y1, length = .065,
           angle = 12, lwd = .25)
    arrows(x0, y0, x1, y1, length = .0725,
           angle = 8, lwd = .25)
    arrows(x0, y0, x1, y1, length = .0625,
           angle = 5, lwd = .25)
}

