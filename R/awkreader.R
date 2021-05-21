

translate.in.statement <- function(in.statement, the.variables, in.symbol = "%in%", envir = .GlobalEnv){

  all.characters <- strsplit(x = trimws(x = in.statement), split = "")[[1]]

  num.characters <- length(all.characters)
  num.leading.parens <- min(which(all.characters != "(")) - 1
  num.trailing.parens <- min(which(all.characters[num.characters:1] != ")")) - 1

  reduced.statement <- substring(text = trimws(x = in.statement), first = num.leading.parens + 1, last = num.characters - num.trailing.parens)

  the.pieces <- trimws(x = strsplit(x = reduced.statement, split = in.symbol)[[1]], which = "both")

  the.pieces <- gsub(pattern = "\'", replacement = "'", x = the.pieces, fixed = T)
  the.pieces <- gsub(pattern = '\"', replacement = "'", x = the.pieces, fixed = T)

  chars.the.pieces <- strsplit(x = the.pieces, split = "")

  chars.leading.parens <- lapply(X = chars.the.pieces, FUN = function(x){sum(x == "(")})

  chars.trailing.parens <- lapply(X = chars.the.pieces, FUN = function(x){sum(x == ")")})

  parens <- data.frame(leading = as.numeric(chars.leading.parens), trailing = as.numeric(chars.trailing.parens))

  for(i in 1:nrow(parens)){
    if(parens$trailing[i] < parens$leading[i]){
      the.pieces[[i]] <- sprintf("%s%s", trimws(the.pieces[[i]]), rep.int(x = ")", times = parens$leading[i] - parens$trailing[i]))
    }
  }

  rhs.values <- eval(expr = parse(text = the.pieces[2]), envir = envir)

  main.translation <- paste(sprintf("%s == '%s'", the.pieces[1], rhs.values), collapse = " || ")

  translated.statement <- sprintf("(%s)", main.translation)

  return(translated.statement)
}

translate.nin.statement <- function(nin.statement, the.variables, nin.symbol = "%nin%", in.symbol = "%in%", envir = .GlobalEnv){

  in.statement <- gsub(pattern = nin.symbol, replacement = in.symbol, x = nin.statement, fixed = T)

  in.translation <- translate.in.statement(in.statement = in.statement, the.variables = the.variables, in.symbol = in.symbol, envir = envir)

  num.characters <- nchar(in.translation)
  first.character <- substring(text = in.translation, first = 1, last = 1)
  last.character <- substring(text = in.translation, first = num.characters, last = num.characters)

  already.parens <- first.character == "(" & last.character == ")"
  if(already.parens == T){
    res <- sprintf("!%s", in.translation)
  }
  if(already.parens == F){
    res <- sprintf("!(%s)", in.translation)
  }

  return(res)
}


translate.logical.statement <- function(the.statement, the.variables, equation.symbols = c( ">=", ">", "<=", "<", "!=", "=="), envir = .GlobalEnv){

  two.sides <- FALSE
  for(i in 1:length(equation.symbols)){
    equation.pieces <- strsplit(x = the.statement, split = equation.symbols[i], fixed = TRUE)[[1]]
    if(length(equation.pieces) == 2){
      two.sides <- TRUE
      the.symbol <- equation.symbols[i]
      break;
    }
  }
  if(two.sides == FALSE){
    return(the.statement)
  }
  if(two.sides == TRUE){
    rhs.value <- eval(expr = parse(text = equation.pieces[2]), envir = envir)

    ending.value <- rhs.value
    if(is.character(rhs.value) | is.factor(rhs.value)){
      ending.value <- sprintf("'%s'", rhs.value)
    }
    res <- trimws(sprintf("%s %s %s", trimws(equation.pieces[1]), trimws(the.symbol), trimws(ending.value)))
    return(res)
  }
}

translate.filtering.statement <- function(the.filter, the.variables, envir = .GlobalEnv, and.symbol = "&", or.symbol = "|", in.symbol = "%in%", nin.symbol = "%nin%", equation.symbols = c(">=", ">", "<=", "<", "!=", "==")){

  if(is.null(the.filter)){
    return("")
  }
  if(is.na(the.filter[1])){
    return("")
  }
  if(the.filter[1] == ""){
    return("")
  }

  trimmed.filter <- trimws(the.filter)

  each.character <- strsplit(x = trimmed.filter, split = "")[[1]]
  num.characters <- length(each.character)

  w <- which(each.character %in% c(and.symbol, or.symbol))

  if(length(w) == 0){
    begin <- 1
    end <- num.characters
  }
  if(length(w) > 0){
    begin <- c(1, w + 1)
    end <- c(w-1, num.characters)
  }

  num.pieces <- length(begin)
  translated.pieces <- character(length = num.pieces)

  for(i in 1:num.pieces){
    this.piece <- paste(each.character[begin[i]:end[i]], collapse = "")

    contains.in.symbol <- length(grep(pattern = in.symbol, x = this.piece, fixed = T)) > 0
    contains.nin.symbol <- length(grep(pattern = nin.symbol, x = this.piece, fixed = T)) > 0

    intermediate.piece <- this.piece

    if(contains.in.symbol == T){
      intermediate.piece <- translate.in.statement(in.statement = this.piece, the.variables = the.variables, in.symbol = in.symbol, envir = envir)
    }
    if(contains.nin.symbol == T){
      intermediate.piece <- translate.nin.statement(nin.statement = this.piece, the.variables = the.variables, nin.symbol = nin.symbol, in.symbol = in.symbol, envir = envir)
    }
    if(contains.in.symbol == F & contains.nin.symbol == F){
      intermediate.piece <- translate.logical.statement(the.statement = this.piece, the.variables = the.variables, equation.symbols = equation.symbols, envir = envir)
    }

    for(j in 1:length(the.variables)){
      intermediate.piece <- gsub(pattern = the.variables[j], replacement = sprintf("$%d", j), x = intermediate.piece, fixed = T)
    }
    intermediate.piece <- gsub(pattern = '"', replacement = '\"', x = intermediate.piece, fixed = T)
    intermediate.piece <- gsub(pattern = "'", replacement = '\"', x = intermediate.piece, fixed = T)

    translated.pieces[i] <- intermediate.piece
  }



  translated.conjunctions <- gsub(pattern = or.symbol, replacement = "||", x = gsub(pattern = and.symbol, replacement = "&&", x = each.character[w], fixed = TRUE), fixed = TRUE)

  full.translation <- sprintf("if(%s %s)", paste(sprintf("%s %s", trimws(translated.pieces[1:(num.pieces-1)]), translated.conjunctions), collapse = ""), trimws(translated.pieces[num.pieces]))

  return(full.translation)
}


# the.files A character vector showing the names of the files.  (Note that this can either be fully specified paths or paths relative to the current working directory.)  It is assumed that all of the files have the same structure -- the same variables in the same order with the same delimiters and headers.

# The filter:  A character vector expressing a filtering step that is taken while reading in the data.  This filter is written in R's syntax -- e.g. rating == 5 | rating == 1 -- while allowing for operators of and, or, not along with subsetting operators of %in% or %nin%.

# the.variables  A character vector specifying the names of the variables to include.  When "." is specified, all of the variables are included (regardless of any other entries in the.variables).  Any values of the.variables not matching a variable's name in the files will be ignored.

# include.filename A logical value stating whether an additional column should be added to the resulting data including the filename of each row's source.  This is helpful for reading from multiple files while retaining information about their origins.

# file.header A character value.  If include.filename = TRUE is set, then file.header specifies the name of the new column of filenames.

# num.files.per.batch A numeric value specifying how many files to simultaneously process.  When the overall number of files in the.files is larger than num.files.per.batch, the reads are processed separately and then combined.  When too many files are read simultaneously, it can generate an error due to the maximum length of the command.  In that case, setting a lower value of num.files.per.batch will allow the reads to proceed in multiple batches.

# return.as A character value specifying the type of output to produce.  If return.as = "result" is set, then the data read from the files is returned as a data.table or data.frame object.  When return.as = "code" is set, then the AWK statements to read the data are returned as a character vector.  If return.as = "all" is selected, then the output is a list including both the data and the coding statements.  All other values will default to return.as = "result" for the program.

# envir Specifies the coding environment, with .GlobalEnv as a default.

# and.symbol A character value specifying the form of the logical AND operator.  R's version of & is used by default.

# or.symbol A character value specifying the form of the logical OR operator.  R's version of | is used by default.

# in.symbol A character value specifying the form of the logical IN operator.  x IN y returns TRUE if the value of x is one of the values of y.  R's version of %in% is used by default.

# nin.symbol A character value specifying the form of the logical NOT IN operator.  x NOT IN y returns TRUE if the value of x is not one of the values of y.  The operator %nin% (not standard but employed in some packages) is used by default.

# show.warnings A logical value specifying whether warnings in the reading process should be displayed (TRUE) or not (all other values).  In particular, when no data is returned (especially in batched reads), this can trigger warnings.

# return.data.table A logical value specifying whether to return the data in data.table format (TRUE) or as a data.frame (FALSE).  All other values will revert to TRUE.

# nrows An integer value specifying how many rows to read in.  When nrows is missing (NA_integer_), all relevant rows of data will be read.  Larger values of nrows will be truncated to the maximum number of rows.

# drop A numeric or character vector specifying the indices (or names) of variables that should be dropped from the data.

filtered.fread <- function(the.files, the.filter = NULL, the.variables = ".", include.filename = TRUE, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, and.symbol = "&", or.symbol = "|", in.symbol = "%in%", nin.symbol = "%nin%", show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...){
  require(data.table)

  lv.name <- "last.variable"
  value.code <- "code"
  value.all <- "all"

  if(!is.logical(return.data.table)){
    return.data.table <- TRUE
  }

  the.files <- the.files[file.exists(the.files)]

  total.files <- length(the.files)

  header.statement <- sprintf("header.dt <- fread(input = '%s', nrows = 1)", the.files[1])
  eval(expr = parse(text = header.statement))

  if(is.null(the.variables) | "." %in% the.variables){
    the.variables <- names(header.dt)
  }
  if(sum(the.variables %in% names(header.dt)) == 0){
    stop("No variables in the data were specified.")
  }

  if(!is.null(drop)){
    if(is.numeric(drop)){
      drop <- names(header.dt)[drop]
    }
    the.variables <- the.variables[!(the.variables %in% drop)]
  }

  if(length(the.variables) == 0){
    stop("All variables were dropped.")
  }

  if(!is.numeric(num.files.per.batch)){
    num.files.per.batch <- 1000
  }
  if(num.files.per.batch < 1){
    num.files.per.batch <- 1000
  }

  w <- which(names(header.dt) %in% the.variables)

  column.names.awk <- paste(sprintf("$%d", w), collapse = ",")

  awk.filter <- translate.filtering.statement(the.filter = the.filter, the.variables = names(header.dt), envir = envir, and.symbol = and.symbol, or.symbol = or.symbol, in.symbol = in.symbol, nin.symbol = nin.symbol)


  string.filename <- ""
  if(include.filename == TRUE){
    string.filename <- ",FILENAME"
  }

  list.data <- list()

  num.batches <- ceiling(total.files / num.files.per.batch)

  awk.statements <- character(length = num.batches)

  for(i in 1:num.batches){
    pasted.file.names <- paste(sprintf("'%s'", the.files[((i-1)*num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]), collapse = " ")

    awk.statements[i] <- sprintf("awk -F ',' 'FNR < 2 { next }{%s print %s%s}' %s", awk.filter, column.names.awk, string.filename, pasted.file.names)

    if(return.as != value.code){
      if(show.warnings == TRUE){
        batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows)
      }
      if(show.warnings != TRUE){
        suppressWarnings(batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows))
      }

      if(nrow(batch.data) > 0){
        if(include.filename == FALSE){
          names(batch.data) <- names(header.dt)[w]
        }
        if(include.filename == TRUE){
          nc <- ncol(batch.data)
          nv <- length(w)
          if(nc > 1 + nv){

            split.cols <- names(batch.data)[(nv+1):nc]
            batch.data[, eval(lv.name) := get(split.cols[1])]

            for(j in 2:(nc-nv)){
              batch.data[, eval(lv.name) := sprintf("%s %s", get(lv.name), get(split.cols[j]))]
            }

            batch.data[, (split.cols) := NULL]
          }
          names(batch.data) <- c(names(header.dt)[w], file.header)
        }
      }
      list.data[[i]] <- batch.data
    }
  }

  if(return.as == value.code){
    res <- awk.statements
  }
  if(return.as != value.code){
    the.result <- rbindlist(l = list.data, fill = T)
    if(return.data.table == FALSE){
      setDF(the.result)
    }
    if(nrows < nrow(the.result)){
      the.result <- the.result[1:nrows,]
    }

    if(return.as == value.all){
      res <- list(result = the.result, code = awk.statements)
    }
    if(return.as != value.all){
      res <- the.result
    }
  }

  return(res)
}

# Most inputs are the same as in filtered.fread()

combined.fread <- function(the.files, the.variables = ".", include.filename = TRUE, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...){

  return(filtered.fread(the.files = the.files, the.filter = NULL, the.variables = the.variables, include.filename = include.filename, file.header = file.header, num.files.per.batch = num.files.per.batch, return.as = return.as, envir = envir, show.warnings = show.warnings, return.data.table = return.data.table, nrows = nrows, drop = drop))
}

# Most inputs are the same as in filtered.fread unless specified below.

# the.patterns A character vector specifying patterns to search for in the data.  Any row that finds this pattern (in any variable) is returned.

# tf A logical vector specifying whether each pattern should be searched as is (TRUE) or negated (FALSE).  Negated patterns return any row that does NOT include the specified pattern.  When length(tf) < length(the.patterns), tf is replicated.  The values of FALSE may be specified using any of the following character values:  c("!", "not", "false", "F", "0").  Otherwise TRUE is used.

# connectors A character vector specifying whether multiple patterns should be connected using a logical AND or a logical OR operator.  If length(the.patterns) = k, then length(connectors) should be k-1.  Smaller vectors will be replicated.  Note that no parentheses can be specified, so the logical statement x1 AND x2 OR x3 reads in exactly that order.  For AND operators, any value in c("&", "&&", "and") maybe used, and this is not case sensitive.  Similarly, OR operators may be specified with any value in c("|", "||", "or").

pattern.fread <- function(the.files, the.patterns = NULL, tf = TRUE, connectors = "or", the.variables = ".", include.filename = TRUE, file.header = "file", num.files.per.batch = 1000, return.as = "result", envir = .GlobalEnv, show.warnings = FALSE, return.data.table = TRUE, nrows = Inf, drop = NULL, ...){

  require(data.table)

  and.symbols <- c("&", "&&", "and")
  or.symbols <- c("|", "||", "or")

  negation.symbols <- c("!", "not", "false", "F", "0")

  lv.name <- "last.variable"
  value.code <- "code"
  value.all <- "all"


  if(!is.logical(return.data.table)){
    return.data.table <- TRUE
  }

  the.files <- the.files[file.exists(the.files)]

  total.files <- length(the.files)


  if(total.files == 0){
    stop("No existing files were found.")
  }

  header.statement <- sprintf("header.dt <- fread(input = '%s', nrows = 1)", the.files[1])
  eval(expr = parse(text = header.statement))

  if(is.null(the.variables) | "." %in% the.variables){
    the.variables = names(header.dt)
  }
  if(sum(the.variables %in% names(header.dt)) == 0){
    stop("No variables in the data were specified.  Double check that the names were spelled correctly.")
  }

  if(!is.null(drop)){
    if(is.numeric(drop)){
      drop <- names(header.dt)[drop]
    }
    the.variables <- the.variables[!(the.variables %in% drop)]
  }
  if(length(the.variables) == 0){
    stop("All variables were dropped.")
  }


  if(!is.numeric(num.files.per.batch)){
    num.files.per.batch <- 1000
  }
  if(num.files.per.batch < 1){
    num.files.per.batch <- 1000
  }

  w <- which(names(header.dt) %in% the.variables)

  column.names.awk <- paste(sprintf("$%d", w), collapse = ",")


  patterns.exist <- !is.null(the.patterns)
  if(patterns.exist == FALSE){
    awk.pattern <- ""
  }
  if(patterns.exist == TRUE){

    num.patterns <- length(the.patterns)

    will.negate <- rep.int(x = tolower(as.character(tf)) %in% negation.symbols, times = ceiling(num.patterns / length(tf)))[1:num.patterns]

    logical.symbols <- rep.int(x = "", times = num.patterns)
    logical.symbols[will.negate == TRUE] <- "!"

    logical.patterns <- trimws(sprintf("%s /%s/", logical.symbols, the.patterns))

    awk.pattern <- logical.patterns[1]

    the.connections <- ""

    if(num.patterns > 1){
      raw.connections <- rep.int(x = connectors, times = (num.patterns - 1) / length(connectors))

      the.connections <- rep.int(x = " || ", times = num.patterns-1)
      the.connections[raw.connections %in% and.symbols] <- " && "

      for(j in 2:num.patterns){
        awk.pattern <- sprintf("%s %s %s", awk.pattern, the.connections[j-1], logical.patterns[j])
      }
    }
  }

  string.filename <- ""
  if(include.filename == TRUE){
    string.filename <- ",FILENAME"
  }


  list.data <- list()

  num.batches <- ceiling(total.files / num.files.per.batch)

  awk.statements <- character(length = num.batches)

  for(i in 1:num.batches){
    pasted.file.names <- paste(sprintf("'%s'", the.files[((i-1)*num.files.per.batch + 1):min(total.files, i * num.files.per.batch)]), collapse = " ")

    awk.statements[i] <- sprintf("awk -F ',' 'FNR < 2 { next } %s {print %s%s}' %s", awk.pattern, column.names.awk, string.filename, pasted.file.names)

    if(return.as != value.code){
      if(show.warnings == TRUE){
        batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows)
      }
      if(show.warnings != TRUE){
        suppressWarnings(batch.data <- fread(cmd = awk.statements[i], fill = T, nrows = nrows))
      }

      if(nrow(batch.data) > 0){
        if(include.filename == FALSE){
          names(batch.data) <- names(header.dt)[w]
        }

        if(include.filename == TRUE){
          nc <- ncol(batch.data)
          nv <- length(w)
          if(nc > 1 + nv){

            split.cols <- names(batch.data)[(nv+1):nc]
            batch.data[, eval(lv.name) := get(split.cols[1])]

            for(j in 2:(nc-nv)){
              batch.data[, eval(lv.name) := sprintf("%s %s", get(lv.name), get(split.cols[j]))]
            }

            batch.data[, (split.cols) := NULL]
          }
          names(batch.data) <- c(names(header.dt)[w], file.header)
        }

      }
      list.data[[i]] <- batch.data
    }
  }

  if(return.as == value.code){
    res <- awk.statements
  }
  if(return.as != value.code){
    the.result <- rbindlist(l = list.data, fill = T)
    if(return.data.table == FALSE){
      setDF(the.result)
    }

    if(nrows < nrow(the.result)){
      the.result <- the.result[1:nrows,]
    }
    if(return.as == value.all){
      res <- list(result = the.result, code = awk.statements)
    }
    if(return.as != value.all){
      res <- the.result
    }
  }

  return(res)
}
