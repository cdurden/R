library(liteq)
library(purrr)
library(later)
messages <- list()

# Utility functions
forEachWhile <- function(xs, f) { # Call f on the elements of xs while f returns TRUE
    reduce(xs, function(acc, x) { # This function returns TRUE iff f(x) is TRUE for all x in xs.
        if (!acc) return(FALSE);
        return(f(x))
    }, .init=TRUE)
}
doWhile <- function(f) { # Call f while it returns TRUE
    if(f()) return(doWhile(f));
}
forEachUntil <- function(xs, f) !forEachWhile(xs, negate(f)) # This function returns FALSE iff f(x) is FALSE for all x in xs
doUntil <- function(xs, f) !doWhile(xs, negate(f))

# We use async.add from the background package to trigger callbacks on IO events.
# We pass the path to a file containing the message database.
# listenForMessage sets up a connection on this file, and calls onMessage when input is received.
library(background)
listenForMessage <- function(db, onMessage) {
    con <- file(db)
    open(con, open="r", blocking=FALSE)
    async.add(con, function(h, data) { onMessage(); close(con); async.rm(h); listenForMessage(db, onMessage); })
}
# These function process messages from a list of queues, passing each one on to the message handler.
# Once all messages are processed the function returns.
# They are intended for use in callback functions, which can be passed to `listenForMessage` along with a file receiving input. IO on the file with then trigger the callback.
# `queues` is a list of queues.
# Each message received on one of the queues will be passed to `messageHandler`.
# An event loop `loop` can be passed in to enable event handling.
processMessageFromQueues <- function(queues, messageHandler, loop) { # Process (at most) a single message from a list of queues...
    #cat("Processing messages\n")
    forEachUntil(queues, function(queue) { # This will return true if a message was processed.
        msg <- try_consume(queue)
        if (!is.null(msg)) {
            messageHandler(msg, queues)
            return(TRUE) # ... processing will halt once a message is processed from one of the queues
        }
        return(FALSE) # ... no messages were processed; keep trying.
    })
}
processQueues <- function(queues, messageHandler, loop) {
    #cat("Processing queues\n")
    doWhile(function() { return(processMessageFromQueues(queues, messageHandler, loop)) }) # Process messages while messages are processed successfully
}

# Example event loop
library(jsonlite)
db <- tempfile()
eventQueue <- ensure_queue("events", db = db)

eventHandlers <- new.env()

addEventHandler <- function(target, type, handler) {
    eventHandlers[[type]][[target]] <<- append(eventHandlers[[type]][[target]], handler)
}
clearEventHandlers <- function(target, type) {
    if (missing(type)) {
        lapply(names(eventHandlers), function(type) {
            eventHandlers[[type]][[target]] <<- NULL
        })
    } else {
        eventHandlers[[type]][[target]] <<- NULL
    }
}

processMessage <- function(msg, queues) {
    #cat("Message received\n")
    if (msg$queue == "events") {
        event <- fromJSON(msg$message)
        cat(paste0("Event: type=",event$type,", target=",event$target,"\n"))
        lapply(eventHandlers[[event$type]][[event$target]], function(handler) {
            handler(event)
        })
        ack(msg)
    }
}
triggerEvent <- function(event, delay=0) {
    later(function() publish(eventQueue, title = "event", message = toJSON(event)), delay=delay)
}

#eventLoop <- create_loop()
library(fileuv)
listen(db)
addFileEventHandler(db, function() processQueues(list(eventQueue), processMessage, NULL))

addEventHandler("0", "signal", function(event) {
    cat(event$type,"\n")
})
addEventHandler("0", "chain", function(event) {
    cat(event$type,"\n")
    triggerEvent(list(target="0", type="signal"))
})
function() {
    triggerEvent(list(target="0", type="signal"))
    triggerEvent(list(target="0", type="chain"))
    publish(eventQueue, title = "event", message = toJSON(list(target="0", type="chain")))
}

## Configure queues
#db <- tempfile()
#jobQueue <- ensure_queue("jobs", db = db) # Used to register jobs that must be performed
#messageQueue <- ensure_queue("messages", db = db) # Used to notify system of messages and to register listeners which handle messages.
#
#if (exists("eventLoop") && exists_loop(eventLoop)) destroy_loop(eventLoop)
#
#processMessage <- function(msg, queues) {
#    cat(paste0(msg$queue, " ", msg$message, "\n"))
#    messages <<- append(messages, paste0(msg$queue, " ", msg$message, "\n"))
#    ack(msg)
#}
#listenForMessage(db, function() processQueues(list(messageQueue,jobQueue), processMessage, eventLoop))
#
## Try it out
#makeTask <- function(queue, type, n) {
#    return(function() {
#        publish(queue, title = paste0(type," ",n,"\n"), message = paste0(type," ",n,"\n"))
#    })
#}
#out <- lapply(c(1:10), function(i) later(makeTask(jobQueue, "job", i), delay = 5))
#out <- lapply(c(1:10), function(i) later(makeTask(messageQueue, "msg", i), delay = 0))
