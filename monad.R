MONAD <- function(modifier) {
    unit <- function(value) {
        monad = list()
        #monad$bind <- function (func, ...) {
        #    func_args <- as.list(match.call())[-1:-2]
        monad$bind <- function (func, args=list()) {
            # Drop the first argument, which is the function `func` itself.
            #return(do.call(func, args=append(list(value), args)))
            return(do.call(func, args=append(list(value), args)))
        };
        monad$lift <- function (func) {
            lifted_func <- function (...) {
                #func_args <- as.list(match.call())[-1:-2]
                # This mobit's value is passed (by bind) as the first argument. We don't want to pass this back to bind.
                #return(unit(do.call(monad$bind, args=append(alist(func=func),func_args))))
                return(unit(do.call(monad$bind, args=list(func=func,args=list(...)[-1]))))
            }
            return(lifted_func)
        };
        if (is.function(modifier)) {
            monad <- modifier(monad, value);
        }
        class(monad) <- append(class(monad), "monad")
        return(monad)
    }
    return(unit)
}

`%>=%` <- function(monad, f) {
  if(!is(monad, "monad")) stop("Provide a monad value left of '%>=%' !")
  #return(monad$bind(monad$lift(f)))
  return(monad$bind(f))
}

`%^>=%` <- function(monad, f) {
  if(!is(monad, "monad")) stop("Provide a monad value left of '%>=%' !")
  return(monad$bind(monad$lift(f)))
  #return(monad$bind(f))
}

just <- MONAD(function (monad, value) {
    if (is.null(value)) {
        monad$is_null = TRUE;
        monad$bind <- function (...) {
            return(monad)
        };
    }
    monad
});

state <- MONAD(function (monad, value) {
    monad$environment <- new.env()
    monad$bind <- function (func, args=list()) {
        # Drop the first argument, which is the function `func` itself.
        result <- do.call(func, args=append(list(value), args))
        monad$environment[[result$id]] <- result
        return(result)
    };
    monad$bind(function(x) x)
    monad
});

bindAll <- function(func, ...) {
  mobits <- as.list(match.call())[-1:-2]
  id <- function(x) {
    return(x)
  }
  return(do.call(func, lapply(mobits, function(mobit) { eval(mobit)$bind(id) })))
}

nothing <- just(NULL);

#monad <- just(value);
#value = "Hello, world!";
#id <- function(x) {
#    return(x)
#}
#f <- function(x) {
#    cat(x);
#    return(x)
#}
#g <- function(x) {
#    return(NULL)
#}
#
#monad$bind(id);
#out <- monad %>=% lifted_g
#out <- monad %>=% lifted_g %>=% lifted_f %>=% id
