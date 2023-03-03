# rmarkdown.R
# Copyright (C) 2020-2022 Posit Software, PBC

# main
.main <- function() {

  # execute knitr::spin
  spin <- function(input) {

    # read file
    text <- xfun::read_utf8(input)

    # spin and return
    knitr::spin(text = text, knit = FALSE)

  }

  # dependencies (convert knit_meta to includes)
  dependencies <- function(input, format, output, tempDir, libDir, data, quiet) {
    
    # unserialize knit_meta
    knit_meta <- base::lapply(data, jsonlite::unserializeJSON)

    # determine files_dir
    files_dir <- if (!base::is.null(libDir)) libDir else rmarkdown:::knitr_files_dir(output)

    # yield pandoc format
    list(
      includes = pandoc_includes(input, format, output, files_dir, knit_meta, tempDir)
    )
  }

  # postprocess (restore preserved)
  postprocess <- function(input, format, output, preserved_chunks) {

    # check for html output
    isHTML <-  knitr::is_html_output(
      format$pandoc$to, 
      c("markdown", "epub", "gfm", "commonmark", "commonmark_x", "markua")
    )

    # bail if we don't have any perserved chunks and aren't doing code linking
    code_link <- isHTML && base::isTRUE(format$render$`code-link`)
    if (base::length(preserved_chunks) == 0 && code_link == FALSE)
      return()

    # change to input dir and make input relative
    oldwd <- base::setwd(base::dirname(rmarkdown:::abs_path(input)))
    base::on.exit(base::setwd(oldwd), add = TRUE)
    input <- base::basename(input)

    # apply r-options (if any)
    r_options <- format$metadata$`r-options`
    if (!base::is.null(r_options)) {
      base::do.call(options, r_options)
    }
    
    # read output and initialize output_res to no-op
    output_str <- xfun::read_utf8(output)
    output_res <- output_str
    
    # perform code linking if requested
    if (base::isTRUE(code_link)) {
      if (!has_annotations(input)) {
        if (base::requireNamespace("downlit", quietly = TRUE) && base::requireNamespace("xml2", quietly = TRUE)) {

          # run downlit
          downlit::downlit_html_path(output, output)
          
          # fix xml2 induced whitespace problems that break revealjs columns 
          # (b/c they depend on inline-block behavior) then reset output_res 
          downlit_output <- base::paste(xfun::read_utf8(output), collapse = "\n")
          downlit_output <- base::gsub('(</div>)\n(<div class="column")', "\\1\\2", downlit_output)
          output_res <- base::strsplit(downlit_output, "\n", fixed = TRUE)[[1]]
          
        } else {
          base::warning("The downlit and xml2 packages are required for code linking")
        }        
      } else {
        base::warning("Since code annotations are present, code-linking has been disabled")
      }
    }
   
    # restore preserved chunks if requested
    if (base::length(preserved_chunks) > 0) {
      # convert preserved chunks to named character vector
      names <- base::names(preserved_chunks)
      preserved_chunks <- base::as.character(preserved_chunks)
      base::names(preserved_chunks) <- names


      if (isHTML) {
        # Pandoc adds an empty <p></p> around the IDs of preserved chunks, and we
        # need to remove these empty tags, otherwise we may have invalid HTML like
        # <p><div>...</div></p>. For the reason of the second base::gsub(), see
        # https://github.com/rstudio/rmarkdown/issues/133.
        for (i in base::names(preserved_chunks)) {
          output_res <- base::gsub(paste0("<p>", i, "</p>"), i, output_res,
                            fixed = TRUE, useBytes = TRUE)
          output_res <- base::gsub(paste0(' id="[^"]*?', i, '[^"]*?" '), ' ', output_res,
                            useBytes = TRUE)
        }
        output_res <- htmltools::restorePreserveChunks(output_res, preserved_chunks)

      } else {

        output_res <- knitr::restore_raw_output(output_str, preserved_chunks)
      }
    }
    
    # re-write output if necessary
    if (!base::identical(output_str, output_res)) {
      xfun::write_utf8(output_res, output)  
    }
  }

  # run shiny documents
  run <- function(input, port, host) {
    shiny_args <- list()
    if (!base::is.null(port))
      shiny_args$port <- port
    if (!base::is.null(host))
      shiny_args$host <- host

    # we already ran quarto render before the call to run
    base::Sys.setenv(RMARKDOWN_RUN_PRERENDER = "0")

    # run the doc
    rmarkdown::run(input, shiny_args = shiny_args)
  }

  # read request from stdin
  stdin <- base::file("stdin", "r")
  input <- base::readLines(stdin, warn = FALSE)
  base::close(stdin)

  # parse request and params
  request <- jsonlite::parse_json(input, simplifyVector = TRUE)
  params <- request$params

  # source in helper functions if we have a resourceDir
  if (!base::is.null(params$resourceDir)) {
    res_dir <- base::file.path(params$resourceDir, "rmd")
    base::source(base::file.path(res_dir, "patch.R"), local = TRUE)
    base::source(base::file.path(res_dir, "execute.R"), local = TRUE)
    base::source(base::file.path(res_dir, "hooks.R"), local = TRUE)
  }

  # print execute-debug message
  debug <- base::isTRUE(params$format$execute[["debug"]])
  if (debug)
    base::message("[knitr engine]: ", request$action)

  # dispatch request
  if (request$action == "spin") {
    result <- spin(params$input)
    result <- base::paste(result, collapse = '\n')
  } else if (request$action == "execute") {
    result <- execute(params$input, params$format, params$tempDir,  params$libDir, params$dependencies, params$cwd, params$params, params$resourceDir, params$handledLanguages, params$markdown)
  } else if (request$action == "dependencies") {
    result <- dependencies(params$input, params$format, params$output, params$tempDir, params$libDir, params$dependencies, params$quiet)
  } else if (request$action == "postprocess") {
    result <- postprocess(params$input, params$format, params$output, params$preserve)
  } else if (request$action == "run") {
    result <- run(params$input, params$port, params$host)
  }

  # write results
  if (debug)
    base::message("[knitr engine]: writing results")
  resultJson <- jsonlite::toJSON(auto_unbox = TRUE, result)
  xfun:::write_utf8(base::paste(resultJson, collapse = "\n"), request[["results"]])
  if (debug)
    base::message("[knitr engine]: exiting")
}

# Check to see if there are annotations in the file
has_annotations <- function(input)  {
  inputLines <- base::readLines(input)
  chunkStarts <- base::grep(knitr::all_patterns$md$chunk.begin, inputLines)
  chunkEnds <- base::grep(knitr::all_patterns$md$chunk.end, inputLines)
  annotations <- base::grep(".*\\Q#\\E\\s*<[0-9]+>\\s*", inputLines)
  hasAnnotations <- FALSE
  if (base::length(chunkStarts) > 0 && base::length(annotations) > 0) {
    lastLine <- base::max(base::max(chunkEnds), base::max(chunkStarts), base::max(annotations))

    # the chunk is a vector with a position for each line, indicating
    # whether that line is within a code chunk
    chunkMap <- base::rep(FALSE, lastLine)

    # Update the chunk mapt to mark lines that are 
    # within a code chunk
    for (x in 1:base::length(chunkStarts)) {
      start <- chunkStarts[x]
      end <- chunkEnds[x]
      for (y in start:end) {
        if (y > start && y < end) {
          chunkMap[y] = TRUE
        }
      }
    }
    
    # look for at least one annotations that is in a code chunk
    for (a in annotations) {
      if (chunkMap[a] == TRUE) {
        hasAnnotations <- TRUE
        break
      }
    }
  }
  hasAnnotations
}

if (!rmarkdown::pandoc_available(error = FALSE)) {
  # When FALSE, this means no Pandoc is found by rmarkdown, not even on PATH
  # In that case we configure rmarkdown to use Quarto bundled version
  quarto_bin_path <- base::Sys.getenv("QUARTO_BIN_PATH", NA_character_)
  # Checking env var to be safe, but should always set by Quarto
  if (!is.na(quarto_bin_path)) {
    pandoc_dir <- base::normalizePath(base::file.path(quarto_bin_path, "tools"))
    rmarkdown::find_pandoc(dir = pandoc_dir)
  }
}

# run main
.main()
