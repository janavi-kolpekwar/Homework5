## HW5 Class/Methods


## =========================
## 1. Class Definition
## =========================
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

## =========================
## 2. Validity Method
## =========================
setValidity("sparse_numeric", function(object) {
  if (length(object@value) != length(object@pos))
    return("Slots 'value' and 'pos' must have the same length.")
  if (any(object@pos < 1L | object@pos > object@length))
    return("Slot 'pos' contains indices outside valid range.")
  if (anyDuplicated(object@pos))
    return("Slot 'pos' contains duplicate indices.")
  TRUE
})

## =========================
## 3. Coercion Methods
## =========================

## numeric -> sparse_numeric
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  new("sparse_numeric",
      value = from[nz],
      pos = as.integer(nz),
      length = as.integer(length(from)))
})

## sparse_numeric -> numeric
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos) > 0L)
    out[from@pos] <- from@value
  out
})

## =========================
## 4. Generic Function Definitions
## =========================
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

## =========================
## 5. Arithmetic Methods
## =========================

## ---- Addition ----
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            
            pos_all <- sort(unique(c(x@pos, y@pos)))
            val_x <- numeric(length(pos_all))
            val_y <- numeric(length(pos_all))
            
            if (length(x@pos) > 0)
              val_x[match(x@pos, pos_all)] <- x@value
            if (length(y@pos) > 0)
              val_y[match(y@pos, pos_all)] <- y@value
            
            vals <- val_x + val_y
            nz <- which(vals != 0)
            new("sparse_numeric",
                value = vals[nz],
                pos = as.integer(pos_all[nz]),
                length = x@length)
          })

## ---- Subtraction ----
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            
            pos_all <- sort(unique(c(x@pos, y@pos)))
            val_x <- numeric(length(pos_all))
            val_y <- numeric(length(pos_all))
            
            if (length(x@pos) > 0)
              val_x[match(x@pos, pos_all)] <- x@value
            if (length(y@pos) > 0)
              val_y[match(y@pos, pos_all)] <- y@value
            
            vals <- val_x - val_y
            nz <- which(vals != 0)
            new("sparse_numeric",
                value = vals[nz],
                pos = as.integer(pos_all[nz]),
                length = x@length)
          })

## ---- Multiplication ----
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L)
              return(new("sparse_numeric", value = numeric(0), pos = integer(0), length = x@length))
            
            x_vals <- x@value[match(common, x@pos)]
            y_vals <- y@value[match(common, y@pos)]
            prod_vals <- x_vals * y_vals
            nz <- which(prod_vals != 0)
            new("sparse_numeric",
                value = prod_vals[nz],
                pos = as.integer(common[nz]),
                length = x@length)
          })

## ---- Cross Product ----
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length.")
            common <- intersect(x@pos, y@pos)
            if (length(common) == 0L) return(0)
            sum(x@value[match(common, x@pos)] * y@value[match(common, y@pos)])
          })

## =========================
## 6. Operator Overloads
## =========================
setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))
setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

## =========================
## 7. Show & Plot Methods
## =========================
setMethod("show", "sparse_numeric", function(object) {
  cat("Formal class 'sparse_numeric'\n")
  cat("Length:", object@length, "\n")
  cat("Nonzero positions:", object@pos, "\n")
  cat("Nonzero values:", object@value, "\n")
})

setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  plot(x@pos, x@value, col = "blue", pch = 19,
       xlab = "Index", ylab = "Value", main = "Sparse Vector Comparison", ...)
  points(y@pos, y@value, col = "red", pch = 17)
  common <- intersect(x@pos, y@pos)
  if (length(common) > 0L) {
    points(common, x@value[match(common, x@pos)], col = "purple", pch = 15)
  }
  legend("topright", legend = c("x", "y", "overlap"),
         col = c("blue", "red", "purple"), pch = c(19, 17, 15))
})

## =========================
## 8. Extra Method (Density Ratio)
## =========================
setGeneric("density_ratio", function(x) standardGeneric("density_ratio"))
setMethod("density_ratio", "sparse_numeric", function(x) {
  if (x@length == 0L) return(NA_real_)
  length(x@pos) / x@length
})

################################################################################
## End of File
################################################################################



