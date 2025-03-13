# cmdparseR6: A command-line argument parser based on R6 object-oriented programming

Based on the [R6 package](https://r6.r-lib.org/articles/Introduction.html), cmdparseR6 parses R-script command lines of the form `PROGRAM_NAME COMMAND SUBCOMMAND argument ...` -- equivalent to the way the Docker and Git command line interfaces work. For instance, `Rscript mycheckbook.R add deposit --amount 200` specifies the command `add` and subcommand `deposit`, with the arguments `--amount 200`. It is a refactored form of my earlier [`cmdparseR`](https://github.com/jperkel/cmdparseR) package. 

## Installation
Install from GitHub: 
```r
devtools::install_github("jperkel/cmdparseR6")
```

## Usage
The included file [`pubtools_args.R`](https://github.com/jperkel/cmdparseR6/blob/main/pubtools_args.R) contains a fully worked example: 

Specify commands -- if desired (they are not required) -- as a list of lists that includes the command `name` and `help` string: 
```r
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
  list(command = "todo", 
       help = "Show upcoming due dates"))
```
Subcommands also are not required, but if desired they should be specified as a list within the parent command, as shown above. Note that the third command, `todo`, has no subcommands. 

Arguments are also specified with a list of lists. Include a long parameter (`lparam`), optional short parameter (`sparam`), name of the `variable` to hold the value, `default` value and `help` string: 
```r
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
```
The `type` argument can be any of: `bool` (logical), `value` (e.g. a number or string), `multi` (a value that can store more than one value, for instance a list of keywords), `count` (increments each time it is used. e.g., `-v -v -v` would return `3`), and `range` (two values separated by a colon, e.g., `--range 2024:2025` would result in `mydata$daterange` == `c(2024, 2025)`). 

Initialize the command line parser with `Parser$new`, specifying the name of the tool, a short description and version number. The final `help` argument indicates if you want the parser to automatically add arguments for help (`-h`) and for version information (`-V`). `help = T` by default. 

```r
p <- Parser$new('pubtools', 'cmdline publication tools', '1.0', help = T)
```

Add arguments and commands like so:
```r
p$add_arguments(myargs)$add_commands(mycmds)
```

Finally, call `parse_command_line()` to do its work. The Parser object captures the R command line when `$new()` is called. However, you can also provide a command line string as an argument: 

```r
mydata <- p$parse_command_line()
# p$parse_command_line('author report -a Perkel')
```

The return value of `parse_command_line()` is a list of values, named according the `variable` term in `myargs`. Thus, `mydata$author` in the example above would be `Perkel`. Commands and subcommands, if used, are returned in `mydata$command` and `mydata$subcmd` respectively. Unknown arguments are captured in `mydata$unknowns`. 

`cmdparseR6` provides a built-in `usage()` function that builds a help screen from the provided arguments and help strings: 
```r
p$usage()
```
