createDropbox <- function(schemaJson, schemaFile, onInput = function() {}, redirectUrl="#") {
    dropbox <- list()
    class(dropbox) <- "dropbox"
    if (!missing(schemaJson)) {
        dropbox$schemaJson <- schemaJson
    } else if (!missing(schemaFile)) {
        dropbox$schemaJson <- readLines(schemaFile)
    } else {
        dropbox$schemaJson <- "{}"
    }
    dropbox$id <- sub("/", "", tempfile(pattern="", tmpdir=""))
    dropbox$redirectUrl <- redirectUrl
    dropbox$onInput <- onInput
    dropboxes[[dropbox$id]] <<- dropbox
    #cat("Created dropbox",dropbox$id,"\n")
    return(dropbox)
}
getDropbox <- function(id) {
    #cat("Getting dropbox",id,"\n")
    cat("Input value:", dropboxes[[id]]$input,"\n")
    return(dropboxes[[id]])
}
listDropboxes <- function(id) {
    return(ls(dropboxes))
}
submit <- function(dropboxId, input) {
    dropbox <- dropboxes[[dropboxId]]
    if (class(dropbox) != "dropbox") {
        stop("dropbox not found")
    }
    #cat("got dropbox\n")
    dropboxes[[dropboxId]]$input <<- input
    cat("input collected in dropbox",dropboxId,"\n")
    #catEnvironmentChain(environment(dropbox$onInput))
    show(dropbox$onInput)
    dropbox$onInput(dropbox)
    cat("onInput handler finished\n")
}
if(!all(sapply(ls(environment()), exists, globalenv()))) {
    (function() {
        #try(load(file="cache/dropboxes.RData"))
        if (!exists("dropboxes", inherits = FALSE)) {
            dropboxes = new.env()
        }
        wrap_all_functions <- function(f, env=parent.frame(), to=env) {
          funcs <- mget(ls(envir = env), envir = env, mode = "function",
                        ifnotfound = NA)

          for (fname in names(funcs)) {
            func <- funcs[[fname]]
            if (!is.function(func)) {
              next
            }
            #cat("wrapping",fname,"\n")
            environment(func) <- parent.frame() # So that dropboxes is accessible

            new_func <- f(func)
            #new_func <- f(substitute(func,environment())) # For some reason this seems to prevent func from being obtained from the wronge scope.
            unlockBinding(fname, env)
            assign(fname, new_func, envir = to)
          }
        }
        f <- function(func) {
            func # FIXME: For some reason this does not work if this line is removed
            new_func <- function(...) {
                #print(func)
                ret <- func(...)
                #cat("saving dropboxes\n")
                save(dropboxes, file="cache/dropboxes.RData")
                ret
            }
            return(new_func)
        }
        #environment(wrap_all_functions) <- envir
        wrap_all_functions(f, parent.frame(1), globalenv())
    })()
}
environment()
