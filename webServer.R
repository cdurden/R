library(httpuv)
library(jsonlite)
library(webutils)

PORT <- 4096

http_not_found <- list(
  status=404,
  body='404 Not Found'
)
http_method_not_allowed <- list(
  status=405,
  body='405 Method Not Allowed'
)
http_failed <- function(message) {
    return(list(
        status=500,
        body='500 Internal Server Error'
    ))
}

hello_handler <- list(
  GET = function (request) list(body="Hello world")
  # POST = function (request) { ... }
)

submission_handler <- list(
  # GET = function (request) list(body="Hello world"),
  OPTIONS = function (request) list(headers=list('Access-Control-Allow-Origin'='*','Access-Control-Allow-Methods'='POST','Access-Control-Allow-Headers'='Content-Type'), status=204),
  POST = function (request) {
      params <- parse_query(request$QUERY_STRING)
      #browser()
      if (request$CONTENT_TYPE == "application/json") {
          data <- fromJSON(request$rook.input$read_lines())
      } else {
          data <- request$rook.input$read_lines()
      }
      cat(paste0("submitting data to dropbox ",params$dropboxId,"\n"))
      submit(params$dropboxId, data)
      #cat("adding message to queue\n")
      #later(makeTask(messageQueue, "msg", params$dropboxId), delay = 0)
      cat("sending response\n")
      list(headers=list('Access-Control-Allow-Origin'='*','Content-Type'='application/json'), body=toJSON(data))
  }
)

routes <- list(
  '/' = submission_handler,
  '/hello' = hello_handler,
  # Required by App Engine.
  '/_ah/health' = list(
    GET = function (request) list()
  )
)

router <- function (routes, request) {
  # Pick the right handler for this path and method.
  # Respond with 404s and 405s if the handler isn't found.
  if (!request$PATH_INFO %in% names(routes)) {
    return(http_not_found)
  }
  path_handler <- routes[[request$PATH_INFO]]

  if (!request$REQUEST_METHOD %in% names(path_handler)) {
    return(http_method_not_allowed)
  }
  method_handler <- path_handler[[request$REQUEST_METHOD]]

  return(method_handler(request))
}

app <- list(
  call = function (request) {
    response <- router(routes, request)

    # Provide some defaults for the response
    # to make handler code simpler.
    if (!'status' %in% names(response)) {
      response$status <- 200
    }
    if (!'headers' %in% names(response)) {
      response$headers <- list()
    }
    if (request$REQUEST_METHOD != "OPTIONS" && !'Content-Type' %in% names(response$headers)) {
      response$headers[['Content-Type']] <- 'text/plain'
    }

    return(response)
  }
)

cat(paste0("Server listening on :", PORT, "...\n"))
server <- startServer("0.0.0.0", PORT, app, quiet=FALSE)
#stopServer(server)
