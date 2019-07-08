##' Move Persistence Model
##'
##' Fit a random walk with time-varying move persistence to location data
##' without measurement error
##'
##' The input track is given as a dataframe where each row is a location a constant
##' time interval from the previous row, and columns:
##' \describe{
##' \item{'id'}{individual identification,}
##' \item{'date'}{observation time (POSIXct,GMT),}
##' \item{'lon'}{observed longitude,}
##' \item{'lat'}{observed latitude.}
##' }
##'
##' @title Random Walk with autocorrelation Filter
##' @param data a data frame of observations (see details)
##' @param optim numerical optimizer
##' @param verbose report progress during minimization
##' @return a list with components
##' \item{\code{fitted}}{a dataframe of fitted locations}
##' \item{\code{par}}{model parameter summmary}
##' \item{\code{data}}{input dataframe}
##' \item{\code{tmb}}{the tmb object}
##' \item{\code{opt}}{the object returned by the optimizer}
##' @useDynLib mpm
##' @importFrom TMB MakeADFun sdreport newtonOption
##' @importFrom tibble data_frame
##' @export
mpm <- function(data,
                optim = c("nlminb", "optim"),
                verbose = FALSE) {

  optim <- match.arg(optim)

  A <- length(unique(data$id))
  idx <- c(0, cumsum(as.numeric(table(data$id))))

  data.tmb <- with(data,
               list(
                 x = cbind(lon, lat),
                 A = A,
                 idx = idx
               ))


  parameters <- list(
             lg = rep(1, dim(data)[1]),
             log_sigma = c(1, 1),
             log_sigma_g = 2
           )

  ## TMB - create objective function
  if (is.null(inner.control) | !"smartsearch" %in% names(inner.control)) {
    inner.control <- list(smartsearch = TRUE)
  }
           obj <-
             MakeADFun(
               data = data.tmb,
               parameters = parameters,
               random = c("lg"),
               DLL = "mpm",
               silent = !verbose,
               inner.control = inner.control
             )


  obj$env$tracemgc <- verbose

#  obj$control <- list(trace = 0,
#                      reltol = 1e-12,
#                      maxit = 500)
#  obj$hessian <- TRUE
#  newtonOption(obj, smartsearch = TRUE)

  ## Minimize objective function
  opt <-
    suppressWarnings(switch(optim,
                            nlminb = try(nlminb(obj$par,
                                                obj$fn,
                                                obj$gr,
                                                control = control
                            ))
                            , #myfn #obj$fn
                            optim = try(do.call(
                              optim,
                              args = list(
                                par = obj$par,
                                fn = obj$fn,
                                gr = obj$gr,
                                method = "L-BFGS-B",
                                control = control
                              )
                            ))))

  ## Parameters, states and the fitted values
  rep <- suppressWarnings(try(sdreport(obj)))
  fxd <- summary(rep, "report")
  fxd_log <- summary(rep, "fixed")
  rdm <- summary(rep, "random")

  lg <- rdm[rownames(rdm) %in% "lg", ]


    fitted <- data_frame(
      id = data$id,
      date = data$date,
      g = plogis(lg[, 1]),
      g.se = lg[, 2]
    )


    if (optim == "nlminb") {
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]
    } else if (optim == "optim") {
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["value"]]
    }
    row.names(fxd)[2:3] <- c("sigma_lon", "sigma_lat")

    structure(list(
      fitted = fitted,
      par = fxd,
      data = data,
      tmb = obj,
      opt = opt,
      rep = rep,
      aic = aic
    ),
    class = "mpm"
    )

}
