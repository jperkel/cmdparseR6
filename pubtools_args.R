# source('R6-cmdparser.R')
library(cmdparseR6)

mycmds <- list(
  list(command = "author", subcmd =
         list(
           list(name = "add", help = "Add new author"),
           list(name = "list", help = "List authors"),
           list(name = "report", help = "Get author report"),
           list(name = "summary", help = "Summarize all authors"),
           list(name = "diversity", help = "Show gender analysis of authorship")
           ),
       help = "Author tools"),
  list(command = "article", subcmd =
         list(
           list(name = "add", help = "Add new article"),
           list(name = "scheduled", help = "View upcoming content"),
           list(name = "diversity", help = "Show source diversity statistics"),
           list(name = "published", help = "View published content"),
           list(name = "top", help = "View top-performing content")
         ),
       help = "Article tools"),
  list(command = "todo", # subcmd =
         # list(
         #   list(name = "list", help = "View to-do list")
         # ),
       help = "Show upcoming due dates"))


myargs <- list(
  list(lparam = "--author", sparam = "-a", variable = "author", default = NA, type = "value", help = 'Specify author'),
  list(lparam = "--date", sparam = "-d", variable = "date", default = NA, type = 'value', help = 'YYYY-MM-DD | YYYY-MM | YYYY'),
  list(lparam = "--debug", variable = "debug", default = FALSE, type = 'bool', help = "Run in debug mode"),
  list(lparam = "--graph", variable = "graph", default = FALSE, type = 'bool', help = 'Graph output'),
  list(lparam = "--id", variable = "id", default = NA, type = 'value', help = 'Specify row Id for more details'),
  list(lparam = "--keyword", sparam = "-k", variable = "keyword", default = NULL, type = 'multi', help = "Search keywords; multiple keys are OR'd unless search_and == TRUE"),
  list(lparam = "--nolist", variable = "nolist", default = FALSE, type = 'bool', help = "Exclude article listing from author report"),
  list(lparam = "--outdir", sparam = "-o", variable = "outdir", default = 'OUTDIR', type = 'value', help = 'Store plots to outdir'),
  list(lparam = "--output-format", sparam = "-f", variable = "outputformat", default = "svg", type = 'value', help = "Select output format"),
  list(lparam = "--printonly", sparam = "-p", variable = "printonly", default = FALSE, type = 'bool', help = 'Show only print content'),
  list(lparam = "--range", sparam = "-r", variable = "daterange", default = NA, type = 'range', help = 'YYYY-MM-DD:YYYY-MM-DD'),
  list(lparam = "--sampledb", variable = "sampledb", default = FALSE, type = 'bool', help = 'Sample AuthorDB for validation'),
  list(lparam = "--search-and", variable = "search_and", default = FALSE, type = 'bool', help = "Perform Boolean AND keyword search"),
  list(lparam = "--sect", sparam = "-s", variable = "sect", default = NA, type = 'multi', help = 'Filter by section'),
  list(lparam = "--use-cache", sparam = "-u", variable = "cache", default = FALSE, type = "bool", help = "Force using cached files"),
  list(lparam = "--verbose", sparam = "-v", variable = "verbose", default = 0, "count", help = "Print verbose messages; each use increments the verbosity level")
)

p <- Parser$new('pubtools', 'cmdline publication tools', '1.0', help = T)
p$add_arguments(myargs)$add_commands(mycmds)
p$parse_command_line('author report -a Perkel')
p$usage()
