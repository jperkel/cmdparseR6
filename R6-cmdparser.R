suppressPackageStartupMessages( {
  library(tidyverse)
  library(R6)
})

# fn <- function(...) {
#   vars <- list(...)
#   vars
# }
# 
# fn(a = "a",b = "b",i = "--infile=xyz")

myargs <- list(
  list(lparam = "print", sparam = "p", variable = "print", default = FALSE, type = "bool"),
  list(lparam = "outfile", sparam = "o", variable = "outfile", default = NA, type = "value"),
  list(lparam = "username", sparam = "u", variable = "username", default = NA, type = "value"),
  list(lparam = "keyword", sparam = "k", variable = "keys", default = NULL, type = "multi"),
  list(lparam = "infile", sparam = "i", variable = "infile", default = NA, type = "value"),
  list(lparam = "verbose", sparam = "v", variable = "verbose", default = 0, type = "count")
)

mycmds <- list(
  list(command = "add", help = "add help text"),
  list(command = "delete", help = "delete help text"),
  list(command = "edit", help = "edit help text")
)

mysubcmds <- list(
  list(subcmd = "user", help = "operate on user"),
  list(subcmd = "account", help = "operate on account"),
  list(subcmd = "all", parent = "delete", help = "delete all")
  # list(subcmd = "user", parent = "add", help = "add user"),
  # list(subcmd = "account", parent = "add", help = "add account"),
  # list(subcmd = "user", parent = "delete", help = "delete user"),
  # list(subcmd = "account", parent = "delete", help = "delete account"),
  # list(subcmd = "user", parent = "edit", help = "edit user"),
  # list(subcmd = "account", parent = "edit", help = "edit account")
)

cmdline <- "delete account --verbose --print -o /path/to/my/file -u JeffreyPerkel -k key1 --keyword=key2 -i infile.txt -k key3 -v"
# cmdline <- "delete account --verbose"

get_element <- function(mylist, varname) {
  unlist(lapply(mylist, '[[', varname))
}

is_lparam <- function(string) {
  grepl('^--[a-zA-Z0-9._-]+$', string)
}

is_sparam <- function(string) {
  grepl('^-[a-zA-Z0-9]$', string)
}

Parser <- R6Class("Parser",
                 public = list(
                   name = NULL,
                   desc = NULL,
                   ver = NULL, 
                   initialize = function(name, desc, ver) {
                     self$name = name
                     self$desc = desc
                     self$ver = ver
                     private$cmdline = paste(commandArgs(trailingOnly = T), collapse = ' ')
                   },
                   add_arguments = function(...) {
                     arglist <- list(...)
                     for (arg in arglist) {
                       private$args <- append(private$args, arg)
                     }
                     invisible(self)
                   },
                   add_commands = function(...) {
                     cmdlist <- list(...)
                     for (cmd in cmdlist) {
                       private$cmds <- append(private$cmds, cmd)
                     }
                     invisible(self)
                   },
                   add_subcommands = function(...) {
                     subcmdlist <- list(...)
                     for (subcmd in subcmdlist) {
                       private$subcmds <- append(private$subcmds, subcmd)
                     }
                     invisible(self)
                   },
                   parse_command_line = function(cmdline = NULL) {
                     # if a cmdline is passed as a function arg, 
                     # override the result from commandArgs()
                     if (is.null(cmdline)) cmdline <- private$cmdline
                     # if there is no cmdline, return NULL
                     if (length(cmdline) == 0 || cmdline == "") return (NULL)
                     # split the cmdline into a vector of strings such that each param is its own element.
                     # if param takes the form -p=value or --p=value, split that too.
                     spl <- strsplit(cmdline, ' |=')[[1]]

                     # list to store cmdline variables
                     mydata <- vector('list', length(private$args))
                     # name each element in the list according to the variable name in args
                     names(mydata) <- get_element(private$args, 'variable')
                     # load with defaults provided in args
                     for (name in names(mydata)) {
                       record <- private$args[[which(get_element(private$args, 'variable') == name)]]
                       mydata[[name]] <- record$default
                     }

                     i <- 1 # args index
                     unk <- 0 # count of unknown args
                     l_subcmds <- private$subcmds # local copy of subcmds

                     # process command, if any
                     # if command is possible, one must be provided
                     if (length(private$cmds) > 0) {
                       if (!spl[i] %in% get_element(private$cmds, 'command')) {
                         stop(paste("Unknown command:", spl[i]))
                       }
                       mydata[["command"]] <- spl[i]
                       # filter subcmd list to only those with parent == cmd 
                       # OR no parent listed (meaning available to all cmds)
                       if (length(l_subcmds) > 0) {
                         tmp <- NULL 
                         for (j in seq_along(l_subcmds)) {
                           if (is.null(l_subcmds[[j]]$parent) || l_subcmds[[j]]$parent == spl[i]) {
                             # tmp <- append(private$subcmds[[j]], tmp)
                             tmp <- c(tmp, j)
                           }
                         }
                         # print (tmp)
                         l_subcmds <- l_subcmds[tmp]
                       }
                       i <- i + 1
                     }

                     # process sub-command, if any
                     # if subcommand is possible, one must be provided
                     if (length(l_subcmds) > 0) {
                       if (!spl[i] %in% get_element(l_subcmds, 'subcmd')) {
                         stop(paste0("\'", spl[i], "\' is not a sub-command of \'", mydata[["command"]], "\'"))
                       }
                       mydata[["subcmd"]] <- spl[i]
                       i <- i + 1
                     }

                     while (i <= length(spl)) {
                       if (is_lparam(spl[i])) {
                         lparam <- stringr::str_remove(spl[i], '^--')
                         if (!lparam %in% get_element(private$args, 'lparam')) {
                           warning(paste("Unknown parameter:", lparam))
                           mydata[["unknowns"]] <- c(mydata[["unknowns"]], lparam)
                           # move past this param
                           i <- i + 1
                           next
                         }
                         record <- private$args[[which(get_element(private$args, 'lparam') == lparam)]]
                       }
                       else if (is_sparam(spl[i])) {
                         sparam <- stringr::str_remove(spl[i], '^-')
                         if (!sparam %in% get_element(private$args, 'sparam')) {
                           warning(paste("Unknown parameter:", sparam))
                           mydata[["unknowns"]] <- c(mydata[["unknowns"]], sparam)
                           # move past this param
                           i <- i + 1
                           next
                         }
                         record <- private$args[[which(get_element(private$args, 'sparam') == sparam)]]
                       }
                       else {
                         unk <- unk + 1
                         mydata[["unknowns"]][unk] <- spl[i]
                         warning(paste("Unknown param:", spl[i]))
                         # move past this param
                         i <- i + 1
                         next
                       }

                       switch(record$type,
                              "bool" = {
                                mydata[[record$variable]] <- !record$default
                              },
                              "value" = {
                                mydata[[record$variable]] <- spl[i+1]
                                i <- i + 1
                              },
                              "multi" = {
                                idx <- ifelse(is.null(mydata[[record$variable]][1]), 1, length(mydata[[record$variable]])+1)
                                mydata[[record$variable]][idx] <- spl[i+1]
                                i <- i + 1
                              },
                              "count" = {
                                mydata[[record$variable]] <- mydata[[record$variable]] + 1
                              },
                              stop(paste("Unknown arg type:", record$type))
                       )
                       i <- i + 1
                     }
                     mydata
                   },
                   print = function() {
                     base::print(paste0("<Parser> for program \'", self$name, "\'"))
                     base::print(paste0("   desc: ", self$desc))
                     base::print(paste0("   ver: ", self$ver))
                     base::print(paste0("   Args: ", length(private$args)))
                     base::print(paste0("   Cmds: ", length(private$cmds)))
                     base::print(paste0("   Subcmds: ", length(private$subcmds)))
                   }
                 ),
                 private = list(
                   cmdline = NULL,
                   args = list(),
                   cmds = list(),
                   subcmds = list()
                 )
)

p <- Parser$new('myprog', 'myprog desc', '0.0.1')
p$add_arguments(myargs)$
  add_commands(mycmds)$
  add_subcommands(mysubcmds)

p$parse_command_line()

