### model to use the stepmix class.

### Function stepmix to create a stepmix object. Kept as an R object. The
### python package is only used when we fit some data to it.

stepmix <- function(n_components = 2, n_steps = 1,
                    measurement = "bernoulli", structural = "bernoulli",
                    assignment = "modal", correction = NULL,
                    abs_tol = 1e-3, rel_tol = 1e-10, max_iter = 100,
                    n_init = 1, init_params = "random", random_state = NULL,
                    verbose = 0, verbose_interval = 10,
                    measurement_params = NULL, structural_params = NULL){

  sm_object = list(n_components = as.integer(n_components),
                   n_steps = as.integer(n_steps),
                   measurement = measurement, 
                   structural = structural, 
                   assignment = assignment,
                   correction = correction, 
                   abs_tol = abs_tol, rel_tol = rel_tol,
                   max_iter = as.integer(max_iter),
                   n_init = as.integer(n_init),
                   init_params = init_params, random_state = random_state,
                   verbose = as.integer(verbose),
                   verbose_interval = as.integer(verbose_interval),
                   measurement_params = measurement_params, 
                   structural_params = structural_params)
  class(sm_object) <- "stepmixr"
  sm_object
}


print.stepmixr <- function(x, ..., options = 1){
  if(options == 0){
    cat("StepMix()\n")
  }
  else if(options == 1){
    cat("Stepmix object: \n")
    if(x$n_steps == 1){
      cat(sprintf(" %d components, 1 step",x$n_components))
    }
    else{
      cat(sprintf(" %d components, %d steps",
                  x$n_components, x$n_steps))
    }
  }
  else {
    cat("StepMix()\n")
    print(unlist(x))
  }
}

### fit stepmixr

fit <- function(smx, X = NULL, Y = NULL){
  ## if both x and y are null, we return smx
  if(is.null(X) & is.null(Y)){
    stop("Both X and Y aren't specified")
  }
  
  if(is.null(X)){
    stop("X must be specified")
  }
  # On fit X seulement.
  if(is.null(Y)){
    py_config <- try(reticulate::py_discover_config(required_module = "stepmix"))
    # load
    sm <- try(reticulate::import("stepmix"), silent = TRUE)
    if(inherits(stepmix,"try-error"))
          stop(paste("Unable to find stepmix library in your python repos\n",
                     "Install it using pip install stepmix.",collapse = ""))
    model <- do.call(sm$StepMix, smx)
    fit <- model$fit(as.data.frame(X))
    return(fit)
  }
  else{
    py_config <- try(reticulate::py_discover_config(required_module = "stepmix"))
    # load
    sm <- try(reticulate::import("stepmix"), silent = TRUE)
    if(inherits(stepmix,"try-error"))
      stop(paste("Unable to find stepmix library in your python repos\n",
                 "Install it using pip install stepmix.",collapse = ""))
           model <- do.call(sm$StepMix, smx)
           fit <- model$fit(as.data.frame(X), as.data.frame(Y))
           return(fit)
  }
}


require(reticulate)
use_python("C:/Python39/")
mod = stepmix()
X = data.frame(x1 = c(0,1,0,1,1,0,0,0,1,1,1,0,0,1,0,1,1,0,0,1),
               x2 = c(0,1,0,1,1,0,0,0,1,0,1,1,1,1,0,1,0,0,0,1))

fit1 = fit(mod, X)
f1 = file("fit1.pickle", "wb")
reticulate::py_save_object(fit1, "fit1.pickle")
close(f1)

fit1 = reticulate::py_load_object("fit1.pickle") 
fit1.pre
### predict stepmixr.fit
