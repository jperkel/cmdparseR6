suppressPackageStartupMessages( {
  library(tidyverse)
  library(R6)
})


get_element <- function(mylist, varname) {
  unlist(lapply(mylist, '[[', varname))
}

is_lparam <- function(string) {
  grepl('^--[a-zA-Z0-9._-]+$', string)
}

is_sparam <- function(string) {
  grepl('^-[a-zA-Z0-9]$', string)
}


Parser <- R6::R6Class("Parser",
public = list(
  name = NULL,
  desc = NULL,
  ver = NULL, 
  help = NULL, 

  initialize = function(name, desc, ver, help = TRUE) {
    self$name = name
    self$desc = desc
    self$ver = ver
    self$help = help
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

  usage = function() {
    lines <- vector() # container for the lines
    tablen <- 4 # tab length
    mytab <- paste(rep(' ', tablen), collapse = '')
    MAXLINELEN <- 80 

    # basic usage
    lines[(length(lines) + 1)] <- paste(
      basename(here::here()),
      if (!is.null(get_element(private$cmds, 'command'))) "[COMMAND]",
      if (!is.null(get_element(private$cmds, 'subcmd'))) "[SUBCOMMAND]",
      if (!is.null(get_element(private$args, 'lparam'))) "<OPTIONAL ARGUMENTS>")
      lines[(length(lines) + 1)] <- paste0(mytab, 'Desc: ', self$desc)
      lines[(length(lines) + 1)] <- paste0(mytab, 'Ver : ', self$ver, '\n')
      # lines[4] <- ''

    # get command names and alphabetize
    commands <- sort(get_element(private$cmds, 'command'))
    cmd_padding <- max(nchar(commands))
    if (length(private$cmds) > 0) {
      lines[length(lines) + 1] <- paste0(mytab, "COMMANDS:")
      for (cmd in commands) {
        mycmd <- private$cmds[which(get_element(private$cmds, 'command') == cmd)][[1]]
        cmd_string <- paste(stringr::str_pad(mycmd$command, width = cmd_padding, side = 'left'), ':', mycmd$help)
        lines[length(lines) + 1] <- paste0(mytab, mytab, cmd_string)

        subcmds <- sort(get_element(mycmd$subcmd, 'name'))
        if (!is.null(subcmds)) {
          sub_padding <- max(nchar(subcmds))
          lines[length(lines)+1] <- paste0(mytab, mytab, "SUBCOMMANDS:")
          for (subcmd in subcmds) {
            mysubcmd <- mycmd$subcmd[which(get_element(mycmd$subcmd, 'name') == subcmd)][[1]]
            sub_string <- paste(stringr::str_pad(mysubcmd$name, width = sub_padding, side = 'left'), ':', mysubcmd$help)
            lines[length(lines) + 1] <- paste0(mytab, mytab, mytab, sub_string)
          }
          lines[length(lines) + 1] <- ''
        }
      }
      lines[length(lines) + 1] <- ''
    } # command processing

    # l_args = local copy of private$args
    l_args <- private$args
    if (self$help) {
      # add help commands
      l_args[[length(l_args) + 1]] <- list(lparam = '--help', sparam = '-h', help = 'show help')
      l_args[[length(l_args) + 1]] <- list(lparam = '--version', sparam = '-V', help = 'show version info')
    }
    args <- sort(get_element(l_args, 'lparam'))
    # get longest element
    arg_padding <- max(nchar(args)) + 7
    lines[length(lines) + 1] <- paste0(mytab, "ARGUMENTS:")
    for (arg in args) {
      myarg <- l_args[which(get_element(l_args, 'lparam') == arg)][[1]]
      arg_string <- paste(
        myarg$lparam,
        ifelse (!is.null(myarg$sparam), paste0('(', myarg$sparam, ')'), "")
      )
      lines[length(lines) + 1] <- paste(
        mytab, mytab,
        stringr::str_pad(arg_string, width = arg_padding, side = 'right'),
        ':',
        myarg$help)
    }
    # write to screen
    if (any(nchar(lines) > MAXLINELEN)) {
      long_lines <- which(nchar(lines) > MAXLINELEN) 
      # long_lines is a vector of integers...
      for (i in long_lines) {
        myline <- lines[i]

        help_start_pos <- stringr::str_locate(myline, ': ') 
        # help_start_pos[1,2] is the position of the space in ': '
        max_help_text_line <- MAXLINELEN - help_start_pos[1,2]
        # left-side padding for indented help text
        mypadding <- paste(rep(' ', help_start_pos[1,2]), collapse = '')        

        lstr <- stringr::str_c(stringr::str_sub(myline, start = 1, end = MAXLINELEN), '\n')
        rstr <- stringr::str_sub(myline, start = MAXLINELEN+1)
        center <- NULL

        while (nchar(rstr) > max_help_text_line) {
          tmp <- stringr::str_sub(rstr, start = 1, end = max_help_text_line)          
          center <- c(center, stringr::str_trim(tmp))
          rstr <- stringr::str_sub(rstr, start = max_help_text_line + 1)
        }
        if (!is.null(center)) center <- stringr::str_c(mypadding, center, collapse = '\n')
        if(rstr != '') rstr <- stringr::str_c(mypadding, rstr)
        lines[i] <- stringr::str_c(lstr, ifelse(is.null(center), '', center), rstr)
      }
    }
    writeLines(lines)
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
        
        # process command, if any
        # if command is possible, one must be provided
        if (length(private$cmds) > 0) {
          if (!spl[i] %in% get_element(private$cmds, 'command')) {
            stop(paste("Unknown command:", spl[i]), call. = FALSE)
          }
          # add [[1]] to get a list w/ named objects
          cmd <- private$cmds[which(get_element(private$cmds, 'command') == spl[i])][[1]]
          mydata[['command']] <- cmd$command
          # advance to next element
          i <- i + 1
          # if command has listed subcmd(s), one must be used
          # assume the next element in cmdline is a subcmd
          if (!is.null(cmd$subcmd)) {
            if (!spl[i] %in% get_element(cmd$subcmd, 'name')) {
              stop(paste0("\'", spl[i], "\' is not a sub-command of \'", cmd$command, "\'"), call. = FALSE)
            }
            mydata[['subcmd']] <- spl[i]
            i <- i + 1
          }
        } # end command processing
        
        while (i <= length(spl)) {
          l_args <- NULL 
          v <- vector()
          if (is_lparam(spl[i])) {
            if (!spl[i] %in% get_element(private$args, 'lparam')) {
              warning(paste("Unknown parameter:", spl[i]), call. = FALSE)
              mydata[["unknowns"]] <- c(mydata[["unknowns"]], spl[i])
              # move past this param
              i <- i + 1
              next
            }
            for (j in seq_along(private$args)) { v[j] <- !is.null(private$args[[j]]$lparam) }
            # local copy
            l_args <- private$args[v]
            record <- l_args[[which(get_element(l_args, 'lparam') == spl[i])]]
          }
          else if (is_sparam(spl[i])) {
            if (!spl[i] %in% get_element(private$args, 'sparam')) {
              warning(paste("Unknown parameter:", spl[i]), call. = FALSE)
              mydata[["unknowns"]] <- c(mydata[["unknowns"]], spl[i])
              # move past this param
              i <- i + 1
              next
            }
            for (j in seq_along(private$args)) { v[j] <- !is.null(private$args[[j]]$sparam) }
            # local copy
            l_args <- private$args[v]
            record <- l_args[[which(get_element(l_args, 'sparam') == spl[i])]]
          }
          else {
            unk <- unk + 1
            mydata[["unknowns"]][unk] <- spl[i]
            warning(paste("Unknown param:", spl[i]), call. = FALSE)
            # move past this param
            i <- i + 1
            next
          }
          
          # print(record)
          
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
            "range" = {
              mydata[[record$variable]] <- strsplit(spl[i+1], ':')[[1]]
              # mydata[[paste0(record$variable, 1)]] <- s[1]
              # mydata[[paste0(record$variable, 2)]] <- s[2]
              i <- i + 1
            },
            stop(paste("Unknown variable type:", record$type), call. = FALSE)
          )
          i <- i + 1
        }
        mydata
      },

      print = function() {
        base::print(paste0("<Parser> for program \'", self$name, "\'"))
        base::print(paste0("   desc: ", self$desc))
        base::print(paste0("   ver: ", self$ver))
        base::print(paste0("   args: ", length(private$args)))
        base::print(paste0("   cmds: ", length(private$cmds)))
      }
    ),

    private = list(
      cmdline = NULL,
      args = list(),
      cmds = list()
    )
  )
  
  
  parse_date <- function(d) {
    year <- NA
    month <- NA
    day <- NA
    error <- FALSE 
    
    if (grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', d) == TRUE) {
      myDate <- try(as.Date (d, format = "%Y-%m-%d"))
      if (class (myDate) == "try-error" || is.na(myDate)) {
        error <- TRUE
      }
      year <- as.integer(format(myDate, "%Y"))
      month <- as.integer(format(myDate, "%m"))
      day <- as.integer(format(myDate, "%d"))
    }
    else if (grepl('^[0-9]{8}$', d) == TRUE) {
      myDate <- try(as.Date (d, format = "%Y%m%d"))
      if (class (myDate) == "try-error" || is.na(myDate)) {
        error <- TRUE
      }
      year <- as.integer(format(myDate, "%Y"))
      month <- as.integer(format(myDate, "%m"))
      day <- as.integer(format(myDate, "%d"))
    }
    else if (grepl('^[0-9]{4}-[0-9]{2}$', d) == TRUE) {
      year <- as.integer(substr(d, 1, 4))
      month <- as.integer(substr(d, 6, 7))
      if ( (is.na(year)) ||
      (is.na(month)) ||
      !(month %in% 1:12)) {
        error <- TRUE
      }
    }
    else if (grepl('^[0-9]{4}$', d) == TRUE) {
      year <- as.integer(d)
      if (is.na(year)) {
        error <- TRUE
      }
    }
    else {
      error <- TRUE
    }
    if (error) stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
    return(c(year, month, day))
  } # parse_date
  
  
  # p <- Parser$new('myprog', 'myprog desc', '0.0.1', help = T)
  # p$add_arguments(myargs)$
  #   add_commands(mycmds)
  # 
  # p$parse_command_line()
  
  