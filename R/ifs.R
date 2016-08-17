#' Generate the Heighway Dragon
#' 
#' This function is used to perform a single iteration of the
#' Heighway Dragon, which is simply the union of two functions
#' applied to the same set of points.
#'
#' @name heighway
#' @aliases heighway
#' @param x A 2-by-n matrix representing a set of points
#' @author Brian Lee Yung Rowe
#' @keywords math
#' @examples
#' library(lambda.tools)
#' # Create an instance with 12 iterations
#' x <- rbind(seq(0,1, by=0.05),0)
#' xn <- fold(1:12, function(a,b) heighway(b), x)
#
# plot(t(xn), cex=0.05, main='Heighway Dragon', axes=FALSE,xlab='',ylab='')
#
# z <- map(c(2,6,10,14), function(n) {
#   xn <- fold(1:n, function(a,b) heighway(b), x)
#   plot(t(xn), cex=0.05, main=paste('n',n,sep='='), axes=FALSE,xlab='',ylab='')
# })
#
heighway <- function(x) {
  f1 <- function(x) matrix(c(.5,.5,-.5,.5), nrow=2) %*% x
  f2 <- function(x) matrix(c(-.5,.5,-.5,-.5), nrow=2) %*% x + c(1,0)
  cbind(f1(x),f2(x))
}

#' Generate the Sierpinski Pentagon
#'
#' This function generates a single iteration of the Sierpinski Pentagon,
#' which replaces a pentagon with 5 smaller pentagons where each vertex
#' contains a pentagon.
#'
#' @name sierpinski5
#' @aliases sierpinski5
#' @param x A 2-by-n matrix representing a set of points
#' @author Brian Lee Yung Rowe
#' @keywords math
#' @seealso \code{\link{sierpinski3}} \code{\link{sierpinski4}}
#' @examples
#' library(lambda.tools)
#' x <- polygon(5)
#' xn <- fold(1:5, function(a,b) sierpinski5(b), x)
#
# plot(t(xn), cex=0.05, main='Sierpinski Pentagon', axes=FALSE,xlab='',ylab='')
#
# z <- map(c(2,3,4,6), function(n) {
#   xn <- fold(1:n, function(a,b) sierpinski5(b), x)
#   plot(t(xn), cex=0.05, main=paste('n',n,sep='='), axes=FALSE,xlab='',ylab='')
# })
sierpinski5 <- function(x) {
  m <- matrix(c(0.382,0, 0,0.382), nrow=2)
  o <- matrix(c(0,0, 0.618,0, 0.809,0.588, 0.309,0.951, -0.191,0.588), nrow=2)
  fold(o, function(a,b) cbind(m %*% x + a, b))
}

#' Generate the Sierpinski Carpet
#'
#' This function generates a single iteration of the Sierpinski Carpet,
#' which replaces a square with 8 smaller squares on the inside
#' perimeter of the original square.
#'
#' @name sierpinski4
#' @aliases sierpinski4
#' @param x A 2-by-n matrix representing a set of points
#' @author Brian Lee Yung Rowe
#' @keywords math
#' @seealso \code{\link{sierpinski3}} \code{\link{sierpinski5}}
#' @examples
#' library(lambda.tools)
#' x <- polygon(4)
#' xn <- fold(1:5, function(a,b) sierpinski4(b), x)
#
# plot(t(xn), cex=0.05, main='Sierpinski Carpet', axes=FALSE,xlab='',ylab='')
sierpinski4 <- function(x) {
  m <- matrix(c(1/3,0, 0,1/3), nrow=2)
  o <- matrix(c(0,0, 0,1/3, 0,2/3, 1/3,0, 1/3,2/3, 2/3,0, 2/3,1/3, 2/3,2/3), nrow=2)
  fold(o, function(a,b) cbind(m %*% x + a, b))
}

#' Generate the Sierpinski Gasket
#'
#' This function generates a single iteration of the Sierpinski Gasket,
#' which replaces a triangle with 3 smaller triangles, one within each
#' vertex of the original triangle.
#'
#' @name sierpinski3
#' @aliases sierpinski3
#' @param x A 2-by-n matrix representing a set of points
#' @author Brian Lee Yung Rowe
#' @keywords math
#' @seealso \code{\link{sierpinski4}} \code{\link{sierpinski5}}
#' @examples
#' library(lambda.tools)
#' x <- polygon(3)
#' xn <- fold(1:5, function(a,b) sierpinski3(b), x)
#
# plot(t(xn), cex=0.05, main='Sierpinski Gasket', axes=FALSE,xlab='',ylab='')
sierpinski3 <- function(x) {
  m <- matrix(c(0.5,0, 0,0.5), nrow=2)
  o <- matrix(c(0,0, .5,0, .25,sqrt(3)/4), nrow=2)
  fold(o, function(a,b) cbind(m %*% x + a, b))
}


#' Create a 2x2 rotation matrix
#'
#' Create a rotation matrix for the specified number of degrees.
#'
#' @name rotation
#' @aliases rotation
#' @param degrees The number of degrees to rotate counter clockwise
#' @author Brian Lee Yung Rowe
#' @keywords math
#' @examples
#' m <- rotation(135)
rotation <- function(degrees) {
  theta <- radians(degrees)
  cbind(c(cos(theta),sin(theta)), c(-sin(theta), cos(theta))) 
}

#' Get radians from degrees
#'
#' Basic conversion from degrees to radians
#'
#' @name radians
#' @aliases radians
#' @param degrees The angle in degrees
#' @author Brian Lee Yung Rowe
#' @keywords math
#' @examples
#' m <- radians(135)
radians <- function(degrees) degrees / 360 * 2 * pi


#' Construct a polygon
#'
#' Similar to \code{seq} but produces regular polygons instead of line
#' segments.
#'
#' @name polygon
#' @aliases polygon
#' @param sides The number of sides for the polygon
#' @param by The spacing between points
#' @author Brian Lee Yung Rowe
#' @keywords math
#' @examples
#' p <- polygon(6)
polygon <- function(sides=5, by=0.1) {
  theta <- 360/sides
  xo <- cumsum(c(0,cos(radians((0:(sides-1)) * theta))))
  yo <- cumsum(c(0,sin(radians((0:(sides-1)) * theta))))
  o <- rbind(xo,yo)

  x <- rbind(seq(0,1,by=by),0)
  fold(1:sides, function(a,b) cbind(b, rotation((a-1)*theta) %*% x + o[,a]))
}

#' Create random tiles
#'
#' @param x Initial starting point
#' @param segments Number of segments to create
#' @param pop Population to sample angles from
#' @examples
#' library(lambda.tools)
#' xs <- fold(1:4, function(a,b) tile(b), c(0,0))
#' xs <- fold(1:4, function(a,b) tile(b), polygon(20))
tile <- function(x=c(0,0), segments=2, pop=c(0,90,180,270)) {
  angles <- sample(pop,segments, replace=TRUE)
  xo <- cumsum(c(0,cos(radians(angles))))
  yo <- cumsum(c(0,sin(radians(angles))))
  o <- rbind(xo,yo)

  fold(1:segments, function(a,b) cbind(b, rotation(angles[a]) %*% x + o[,a]))
}
