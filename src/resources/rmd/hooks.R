# hooks.R
# Copyright (C) 2020-2022 Posit Software, PBC

knitr_hooks <- function(format, resourceDir, handledLanguages) {

  knit_hooks <- list()
  opts_hooks <- list()
  
  # options in yaml (save last yaml.code for source hook)
  lastYamlCode <- NULL
  opts_hooks[["code"]] <- function(options) {
    lastYamlCode <<- options[["yaml.code"]]
    options <- knitr_options_hook(options)
    if (base::is.null(lastYamlCode)) {
      lastYamlCode <<- options[["yaml.code"]]
    }
    options
  } 


   # force eval to 'FALSE' for all chunks if execute: enabled: false
  executeEnabled <- format$execute[["enabled"]]
  if (!base::is.null(executeEnabled) && executeEnabled == FALSE) {
    opts_hooks[["eval"]] <- function(options) {
      options$eval <- FALSE
      options
    }
  }

  # propagate echo: fenced to echo: true / fenced.echo
  opts_hooks[["echo"]] <- function(options) {
    if (base::identical(options[["echo"]], "fenced")) {
      options[["echo"]] <- TRUE
      options[["fenced.echo"]] <- TRUE
    } else if (base::isTRUE(options[["chunk.echo"]])) {
      options[["fenced.echo"]] <- FALSE
    }
    # fenced.echo implies hold (if another explicit override isn't there)
    if (base::isTRUE(options[["fenced.echo"]])) {
      if (base::identical(options[["fig.show"]], "asis")) {
        options[["fig.show"]] <- "hold"
      }
      if (base::identical(options[["results"]], "markup")) {
        options[["results"]] <- "hold"
      }
    }
    options
  }

  # forward 'output' to various options. For mainline output, TRUE means flip them
  # from hide, FALSE means hide. For message/warning TRUE means use the
  # global default, FALSE means shut them off entirely)
  opts_hooks[["output"]] <- function(options) {
    output <- options[["output"]]
    if (base::isFALSE(output)) {
      options[["results"]] <- "hide"
      options[["fig.show"]] <- "hide"
    } else if (base::identical(output, "asis")) {
      options[["results"]] <- "asis"
    } else {
      if (base::identical(options[["results"]], "hide")) {
         options[["results"]] <- "markup"
      }
      if (base::identical(options[["fig.show"]], "hide")) {
         options[["fig.show"]] <- "asis"
      }
    }
    options[["message"]] <- base::ifelse(!base::isFALSE(output), knitr::opts_chunk$get("message"), FALSE)
    options[["warning"]] <- base::ifelse(!base::isFALSE(output), knitr::opts_chunk$get("warning"), FALSE)
    options
  }

  # automatically set gifski hook for fig.animate
  opts_hooks[["fig.show"]] <- function(options) {
    
    # get current value of fig.show
    fig.show <- options[["fig.show"]]
    
    # use gifski as default animation hook for non-latex output
    if (base::identical(fig.show, "animate")) {
      if (!is_latex_output(format$pandoc$to) && base::is.null(options[["animation.hook"]])) {
        options[["animation.hook"]] <- "gifski"
      }
      
    # fig.show "asis" -> "hold" for fig: labeled chunks
    } else if (base::identical(fig.show, "asis")) {
      if (is_figure_label(output_label(options))) {
        options[["fig.show"]] <- "hold"
      }
    }
    
    # return options
    options
  }

  opts_hooks[["collapse"]] <- function(options) {
    if (base::isTRUE(options[["collapse"]])) {
      comment <- options[["comment"]]
      if (base::is.null(comment) || base::is.na(comment)) {
        options[["comment"]] <- "##"
      }
    }
   
    # return options
    options
  }

  # opts hooks for implementing keep-hidden
  register_hidden_hook <- function(option, hidden = option) {
    opts_hooks[[option]] <<- function(options) {
      if (base::identical(options[[option]], FALSE)) {
        options[[option]] <- TRUE
        for (hide in hidden)
          options[[base::paste0(hide, ".hidden")]] <- TRUE
      }
      options
    }
  }

  if (base::isTRUE(format$render[["keep-hidden"]])) {
    register_hidden_hook("echo", c("source"))
    register_hidden_hook("output", c("output", "plot"))
    register_hidden_hook("include")
    register_hidden_hook("warning")
    register_hidden_hook("message")
  }

  # hooks for marking up output
  default_hooks <- knitr::hooks_markdown()

  delegating_hook <- function(name, hook) {
    function(x, options) {
      x <- default_hooks[[name]](x, options)
      hook(x, options)
    }
  }

  delegating_output_hook <- function(type, classes) {
    delegating_hook(type, function(x, options) {
      if (base::identical(options[["results"]], "asis") ||
          base::isTRUE(options[["collapse"]])) {
        x
      } else {
        # prefix for classes
        classes <- c("cell-output", base::paste0("cell-output-", classes))
        # add .hidden class if keep-hidden hook injected an option
        if (base::isTRUE(options[[base::paste0(type,".hidden")]]))
          classes <- c(classes, "hidden")
        output_div(x, NULL, classes)
      }
    })
  }

  # entire chunk
  knit_hooks$chunk <- delegating_hook("chunk", function(x, options) {
    if (base::any(base::as.logical(base::lapply(handledLanguages, function(lang) {
      prefix <- base::paste0("```{", lang, "}")
      base::startsWith(x, prefix)
    }))) && base::endsWith(x, "```")) {
      return(x)
    }

    # ojs engine should return output unadorned
    if (base::startsWith(x, "```{ojs}") && base::endsWith(x, "```")) {
      return(x)
    }

    # verbatim and comment should do nothing
    if (base::identical(options[["engine"]], "verbatim") ||
        base::identical(options[["engine"]], "comment")) {
      return(x)
    }

    # read some options
    label <- output_label(options)
    fig.cap <- options[["fig.cap"]]
    cell.cap <- NULL
    fig.subcap <- options[["fig.subcap"]]
    
    # fixup duplicate figure labels
    placeholder <- output_label_placeholder(options)
    if (!base::is.null(placeholder)) {
      figs <- base::length(base::regmatches(x, gregexpr(placeholder, x, fixed = TRUE))[[1]])
      for (i in 1:figs) {
        suffix <- base::ifelse(figs > 1, base::paste0("-", i), "")
        x <- base::sub(placeholder, base::paste0(label, suffix), fixed = TRUE, x)
      }
    }

    # caption output
    if (!base::is.null(fig.cap) && !base::is.null(fig.subcap)) {
      cell.cap <- base::paste0("\n", fig.cap, "\n")
    }  else {
      label <- NULL
    }

    # synthesize layout if we have fig.sep
    fig.sep <- options[["fig.sep"]]
    fig.ncol <- options[["fig.ncol"]]
    if (!base::is.null(fig.sep)) {
      
      # recycle fig.sep
      fig.num <- options[["fig.num"]] %||% 1L
      fig.sep <- base::rep_len(fig.sep, fig.num)
      
      # recyle out.width
      out.width <- options[["out.width"]] 
      if (base::is.null(out.width)) {
        out.width <- 1
      } 
      out.width <- base::rep_len(out.width, fig.num)
      
      # build fig.layout
      fig.layout <- list()
      fig.row <- c()
      for (i in 1:fig.num) {
        fig.row <- c(fig.row, out.width[[i]])
        if (base::nzchar(fig.sep[[i]])) {
          fig.layout[[base::length(fig.layout) + 1]] <- fig.row
          fig.row <- c()
        }
      }
      if (base::length(fig.row) > 0) {
        fig.layout[[base::length(fig.layout) + 1]] <- fig.row
      }
      options[["layout"]] <- fig.layout
      
    # populate layout-ncol from fig.ncol
    } else if (!base::is.null(fig.ncol)) {
      options[["layout-ncol"]] = fig.ncol
    }
    
    # alias fig.align to layout-align
    fig.align = options[["fig.align"]]
    if (!base::is.null(fig.align) && !base::identical(fig.align, "default")) {
      options["layout-align"] = fig.align
    }

    # alias fig.valign to layout-valign
    fig.valign = options[["fig.valign"]]
    if (!base::is.null(fig.valign) && !base::identical(fig.valign, "default")) {
      options["layout-valign"] <- fig.valign
    }

    # forward selected attributes
    forward <- c("layout", "layout-nrow", "layout-ncol", "layout-align")
    forwardAttr <- character()
    for (attr in forward) {
      value <- options[[attr]]
      if (!base::is.null(value)) {
        if (base::identical(attr, "layout")) {
          if (!base::is.character(value)) {
            value <- jsonlite::toJSON(value)
          }
        }
        if (!base::is.null(value)) {
          forwardAttr <- c(forwardAttr, base::sprintf("%s=\"%s\"", attr, value))
        }
      }
    }

    # forward any other unknown attributes
    knitr_default_opts <- base::names(knitr::opts_chunk$get())
    quarto_opts <- c("label","fig.cap","fig.subcap","fig.scap","fig.link", "fig.alt",
                     "fig.align","fig.env","fig.pos","fig.num", "lst-cap", 
                     "lst-label", "classes", "panel", "column", "fig.column", "tbl.column", "fig.cap-location", 
                     "tbl-cap-location", "cap-location", "code-fold", "code-summary", "code-overflow",
                     "code-line-numbers",
                     "layout", "layout-nrow", "layout-ncol", "layout-align", "layout-valign", 
                     "output", "include.hidden", "source.hidden", "plot.hidden", "output.hidden")
    other_opts <- c("eval", "out.width", "yaml.code", "code", "params.src", "original.params.src", 
                    "fenced.echo", "chunk.echo", "lang",
                    "out.width.px", "out.height.px", "indent", "class.source", 
                    "class.output", "class.message", "class.warning", "class.error", "attr.source", 
                    "attr.output", "attr.message", "attr.warning", "attr.error", "connection")
    known_opts <- c(knitr_default_opts, quarto_opts, other_opts)
    unknown_opts <- setdiff(base::names(options), known_opts)
    unknown_opts <- Filter(Negate(is.null), unknown_opts)
    unknown_opts <- Filter(function(opt) !base::startsWith(opt, "."), unknown_opts)
    # json encode if necessary
    unknown_values <- base::lapply(options[unknown_opts], 
                             function(value) {
                               if (!base::is.character(value) || base::length(value) > 1) {
                                 value <- jsonlite::toJSON(value, auto_unbox = TRUE)
                               } 
                               # will be enclosed in single quotes so escape
                               base::gsub("'", "\\\'", value, fixed = TRUE)
                            })
    # append to forward list
    forwardAttr <- c(forwardAttr, 
                     base::sprintf("%s='%s'", unknown_opts, unknown_values))
    if (base::length(forwardAttr) > 0)
      forwardAttr <- base::paste0(" ", base::paste(forwardAttr, collapse = " "))
    else
      forwardAttr <- ""
   
    # handle classes
    classes <- c("cell",options[["classes"]] )
    if (base::is.character(options[["panel"]]))
      classes <- c(classes, base::paste0("panel-", options[["panel"]]))
     if (base::is.character(options[["column"]]))
      classes <- c(classes, base::paste0("column-", options[["column"]]))
     if (base::is.character(options[["fig.column"]]))
      classes <- c(classes, base::paste0("fig-column-", options[["fig.column"]]))
     if (base::is.character(options[["tbl-column"]]))
      classes <- c(classes, base::paste0("tbl-column-", options[["tbl-column"]]))
     if (base::is.character(options[["cap-location"]])) 
      classes <- c(classes, base::paste0("caption-", options[["cap-location"]]))      
     if (base::is.character(options[["fig.cap-location"]])) 
      classes <- c(classes, base::paste0("fig-cap-location-", options[["fig.cap-location"]]))      
     if (base::is.character(options[["tbl-cap-location"]])) 
      classes <- c(classes, base::paste0("tbl-cap-location-", options[["tbl-cap-location"]]))      


    if (base::isTRUE(options[["include.hidden"]])) {
      classes <- c(classes, "hidden")
    }
    classes <- base::sapply(classes, function(clz) base::ifelse(base::startsWith(clz, "."), clz, base::paste0(".", clz)))

    # allow table lable through
    if (is_table_label(options[["label"]])) {
      label <- options[["label"]]
    }
    if (!base::is.null(label)) {
      label <- base::paste0(label, " ")
    }

    # if there is a label, additional classes, a forwardAttr, or a cell.cap 
    # then the user is deemed to have implicitly overridden results = "asis"
    # (as those features don't work w/o an enclosing div)
    needCell <- base::isTRUE(base::nzchar(label)) || 
                base::length(classes) > 1 ||
                base::isTRUE(base::nzchar(forwardAttr)) ||
                base::isTRUE(base::nzchar(cell.cap))
    if (base::identical(options[["results"]], "asis") && !needCell) {
      x
    } else {
      base::paste0(
        options[["indent"]], "::: {", 
        labelId(label), base::paste(classes, collapse = " ") ,forwardAttr, "}\n", x, "\n", cell.cap ,
        options[["indent"]], ":::"
      )
    }
  })

  knit_hooks$source <- function(x, options) {

    # How knitr handles the prompt option for R chunks
    x <- knitr:::hilight_source(x, "markdown", options)
    x <- knitr:::one_string(c('', x))

    # leave verbatim alone
    if (options[["engine"]] %in% c("verbatim", "embed")) {
      return(base::paste0('\n\n````', options[["lang"]] %||% 'default', x, '\n````', '\n\n'))
    }
    
    class <- options$class.source
    attr <- options$attr.source
    class <- base::paste(class, "cell-code")
    if (base::isTRUE(options[["source.hidden"]])) {
      class <- base::paste(class, "hidden")
    }
    if (!base::identical(format$metadata[["crossref"]], FALSE)) {
      id <- options[["lst-label"]]
      if (!base::is.null(options[["lst-cap"]])) {
        attr <- base::paste(attr, base::paste0('caption="', options[["lst-cap"]], '"'))
      }
    } else {
      id = NULL
    }
    if (base::identical(options[["code-overflow"]], "wrap")) {
      class <- base::paste(class, "code-overflow-wrap")
    } else if (base::identical(options[["code-overflow"]], "scroll")) {
      class <- base::paste(class, "code-overflow-scroll")
    }
    fold <- options[["code-fold"]]
    if (!base::is.null(fold)) {
      attr <- base::paste(attr, base::paste0('code-fold="', base::tolower(base::as.character(fold)), '"'))
    }
    fold <- options[["code-summary"]]
    if (!base::is.null(fold)) {
      attr <- base::paste(attr, base::paste0('code-summary="', base::as.character(fold), '"'))
    }
    lineNumbers <- options[["code-line-numbers"]]
    if (!base::is.null(lineNumbers)) {
      attr <- base::paste(attr, base::paste0('code-line-numbers="', base::tolower(base::as.character(lineNumbers)), '"'))
    }

    lang <- base::tolower(options$engine)
    if (base::isTRUE(options[["fenced.echo"]])) {
      attrs <- block_attr(
        id = id,
        lang = NULL,
        class = base::trimws(class),
        attr = attr
      )
      ticks <- "````"
      yamlCode <- lastYamlCode
      if (!base::is.null(yamlCode)) {
        yamlCode <- Filter(function(line) !base::grepl("echo:\\s+fenced", line), yamlCode)
        yamlCode <- base::paste(yamlCode, collapse = "\n")
        if (!base::nzchar(yamlCode)) {
          x <- base::trimws(x, "left")
        }
      } else {
        x <- base::trimws(x, "left")
      }
      x <- base::paste0("\n```{{", options[["original.params.src"]], "}}\n", yamlCode, x, '\n```')
    } else {
       attrs <- block_attr(
        id = id,
        lang = lang,
        class = base::trimws(class),
        attr = attr
      )
      ticks <- "```"
    }
    base::paste0('\n\n', ticks, attrs, x, '\n', ticks, '\n\n')
   
  }
  knit_hooks$output <- delegating_output_hook("output", c("stdout"))
  knit_hooks$warning <- delegating_output_hook("warning", c("stderr"))
  knit_hooks$message <- delegating_output_hook("message", c("stderr"))
  knit_hooks$plot <- knitr_plot_hook(format)
  knit_hooks$error <- delegating_output_hook("error", c("error"))
  
  list(
    knit = knit_hooks,
    opts = opts_hooks
  )
}

knitr_plot_hook <- function(format) {

  htmlOutput <- knitr:::is_html_output(format$pandoc$to)
  latexOutput <- is_latex_output(format$pandoc$to)
  defaultFigPos <- format$render[["fig-pos"]]

  function(x, options) {
    
    # are we using animation (if we are then ignore all but the last fig)
    fig.num <- options[["fig.num"]] %||% 1L
    fig.cur <- options$fig.cur %||% 1L
    tikz <- knitr:::is_tikz_dev(options)
    animate <- fig.num > 1 && options$fig.show == 'animate' && !tikz
    if (animate) {
      if (fig.cur < fig.num) {
        return ("")
      } else  {
        # if it's the gifski hook then call it directly (it will call 
        # this function back with the composed animated gif)
        hook <- knitr:::hook_animation(options)
        if (base::identical(hook, knitr:::hook_gifski)) {
          return(hook(x, options))
        }
      }
    }
  
    # classes
    classes <- base::paste0("cell-output-display")
    if (base::isTRUE(options[["plot.hidden"]])) classes <- c(classes, "hidden")

    # label
    placeholder <- output_label_placeholder(options)
    label <- base::ifelse(
      is_figure_label(placeholder),
      labelId(placeholder),
      ""
    )
    attr <- label
    
    # knitr::fix_options will convert out.width and out.height to their
    # latex equivalents, reverse this transformation so our figure layout
    # code can deal directly with percentages
    options <- latex_sizes_to_percent(options)

    # check for optional figure attributes
    keyvalue <- c()
    fig.align <- options[['fig.align']]
    if (!base::identical(fig.align, "default")) {
      keyvalue <- c(keyvalue, base::sprintf("fig-align='%s'", fig.align))
    }
    fig.env <- options[['fig.env']]
    if (!base::identical(fig.env, "figure")) {
      keyvalue <- c(keyvalue, base::sprintf("fig-env='%s'", fig.env))
    }
    fig.pos <- options[['fig.pos']] 
    if (base::nzchar(fig.pos)) {
      keyvalue <- c(keyvalue, base::sprintf("fig-pos='%s'", fig.pos))
    # if we are echoing code, there is no default fig-pos, and
    # we are not using a layout then automatically set fig-pos to 'H'
    } else if (latexOutput &&
               base::isTRUE(options[["echo"]]) &&
               base::length(base::names(options)[base::startsWith(base::names(options), "layout")]) == 0 &&
               base::is.null(defaultFigPos)) {
      keyvalue <- c(keyvalue, "fig-pos='H'")
    }
    fig.alt <- options[["fig.alt"]]
    escapeAttr <- function(x) base::gsub("'", "\\'", x, fixed = TRUE)
    if (!base::is.null(fig.alt) && base::nzchar(fig.alt)) {
       keyvalue <- c(keyvalue, base::sprintf("fig-alt='%s'", escapeAttr(fig.alt)))
    }
    fig.scap <- options[['fig.scap']]
    if (!base::is.null(fig.scap)) {
      keyvalue <- c(keyvalue, base::sprintf("fig-scap='%s'", escapeAttr(fig.scap)))
    }
    resize.width <- options[['resize.width']]
    if (!base::is.null(resize.width)) {
      keyvalue <- c(keyvalue, base::sprintf("resize.width='%s'", resize.width))
    }
    resize.height <- options[['resize.height']]
    if (!base::is.null(resize.height)) {
      keyvalue <- c(keyvalue, base::sprintf("resize.height='%s'", resize.height))
    }
    
    # add keyvalue
    keyvalue <- base::paste(
      c(
        keyvalue,
        base::sprintf('width=%s', options[['out.width']]),
        base::sprintf('height=%s', options[['out.height']]),
        options[['out.extra']]
      ),
      collapse = ' '
    )
    if (base::nzchar(keyvalue)) {
      attr <- base::paste(attr, keyvalue)
    }

    # create attributes if we have them
    if (base::nzchar(attr)) {
      attr <- base::paste0("{", base::trimws(attr), "}")
    }

    # special handling for animations
    if (animate) {
      
      # get the caption (then remove it so the hook doesn't include it)
      caption <- figure_cap(options)
      options[["fig.cap"]] <- NULL
      options[["fig.subcap"]] <- NULL
      
      # check for latex
      if (is_latex_output(format$pandoc$to)) {
        
        # include dependency on animate package
        knitr::knit_meta_add(list(
          rmarkdown::latex_dependency("animate")
        ))
        
        latexOutput <- base::paste(
          "```{=latex}",
          latex_animation(x, options),
          "```",
          sep = "\n"
        )
        
        # add the caption if we have one
        if (base::nzchar(caption)) {
          latexOutput <- base::paste0(latexOutput, "\n\n", caption, "\n")
        }
        
        # enclose in output div
        output_div(latexOutput, label, classes)
        
      # otherwise assume html
      } else {
        # render the animation
        hook <- knitr:::hook_animation(options)
        htmlOutput <- hook(x, options)
        htmlOutput <- htmlPreserve(htmlOutput)
        
        # add the caption if we have one
        if (base::nzchar(caption)) {
          htmlOutput <- base::paste0(htmlOutput, "\n\n", caption, "\n")
        }
        
        # enclose in output div
        output_div(htmlOutput, label, classes)
      }
     
    } else {
      
      # generate markdown for image
      md <- base::sprintf("![%s](%s)%s", figure_cap(options), x, attr)
      
      # enclose in link if requested
      link <- options[["fig.link"]]
      if (!base::is.null(link)) {
        md <- base::sprintf("[%s](%s)", md, link)
      }
      
      # enclose in output div
      output_div(md, NULL, classes)
    }

  }
}

knitr_options_hook <- function(options) {

  if (!knitr_has_yaml_chunk_options()) {
    # partition yaml options
    results <- partition_yaml_options(options$engine, options$code)
    if (!base::is.null(results$yaml)) {
      # convert any option with fig- into fig. and out- to out.
      # we need to do this to the yaml options prior to merging
      # so that the correctly interact with standard fig. and
      # out. options provided within knitr
      results$yaml <- normalize_options(results$yaml)
      # alias 'warning' explicitly set here to 'message'
      if (!base::is.null(results$yaml[["warning"]])) {
        options[["message"]] <- results$yaml[["warning"]]
      }
      # merge with other options
      options <- knitr:::merge_list(options, results$yaml)
      # set code
      options$code <- results$code
    } 
    options[["yaml.code"]] <- results$yamlSource
    
    # some aliases
    if (!base::is.null(options[["fig.format"]])) {
      options[["dev"]] <- options[["fig.format"]]
    }
    if (!base::is.null(options[["fig.dpi"]])) {
      options[["dpi"]] <- options[["fig.dpi"]]
    }
  } else {
    # convert any option with fig- into fig. and out- to out.
    options <- normalize_options(options)
  }
  
  # if there are line annotations in the code then we need to 
  # force disable messages/warnings
  comment_chars <- engine_comment_chars(options$engine)
  pattern <- base::paste0(".*\\Q", comment_chars[[1]], "\\E\\s*",
                    "<[0-9]+>\\s*")
  if (base::length(comment_chars) > 1) {
    pattern <- base::paste0(pattern, ".*\\Q", comment_chars[[2]], "\\E\\s*")
  }
  pattern <- base::paste0(pattern, "$")
  if (base::any(base::grepl(pattern, options$code))) {
    options$warning <- FALSE
    options$results <- "hold"
  }


  # fig.subcap: TRUE means fig.subcap: "" (more natural way 
  # to specify that empty subcaps are okay)
  if (base::isTRUE(options[["fig.subcap"]])) {
    options[["fig.subcap"]] <- ""
  }
  
  # return options  
  options
}

# convert any option with e.g. fig- into fig. 
# we do this so that all downstream code can consume a single
# variation of these functions. We support both syntaxes because
# quarto/pandoc generally uses - as a delimeter everywhere,
# however we want to support all existing knitr code as well
# as support all documented knitr chunk options without the user
# needing to replace . with -
normalize_options <- function(options) {
  base::names(options) <- base::sapply(base::names(options), function(name) {
    if (name %in% c(
                    # Text output 
                    "strip-white",
                    "class-output",
                    "class-message",
                    "class-warning",
                    "class-error",
                    "attr-output",
                    "attr-message",
                    "attr-warning",
                    "attr-error",
                    # Paged tables
                    "max-print",
                    "sql-max-print",
                    "paged-print",
                    "rows-print",
                    "cols-print",
                    "cols-min-print",
                    "pages-print",
                    "paged-print",
                    "rownames-print",
                    # Code decoration
                    "tidy-opts",
                    "class-source",
                    "attr-source",
                    # Cache
                    "cache-path",
                    "cache-vars",
                    "cache-globals",
                    "cache-lazy",
                    "cache-comments",
                    "cache-rebuild",
                    # Plots
                    "fig-path",
                    "fig-keep",
                    "fig-show",
                    "dev-args",
                    "fig-ext",
                    "fig-width",
                    "fig-height",
                    "fig-asp",
                    "fig-dim",
                    "out-width",
                    "out-height",
                    "out-extra",
                    "fig-retina",
                    "resize-width",
                    "resize-height",
                    "fig-align",
                    "fig-link",
                    "fig-env",
                    "fig-cap",
                    "fig-alt",
                    "fig-scap",
                    "fig-lp",
                    "fig-pos",
                    "fig-subcap",
                    "fig-ncol",
                    "fig-sep",
                    "fig-process",
                    "fig-showtext",
                    # Animation
                    "animation-hook",
                    "ffmpeg-bitrate",
                    "ffmpeg-format",
                    # Code chunk
                    "ref-label",
                    # Language engines
                    "engine-path",
                    "engine-opts",
                    "opts-label",
                    # Other chunk options
                    "R-options")) {
      base::sub("-", ".", name)
    } else {
      name
    }
  }, USE.NAMES = FALSE)
  options
}

# Quarto internal: Get YAML options from chunk
# for when quarto is used with older knitr
partition_yaml_options <- function(engine, code) {
  # mask out empty blocks
  if (base::length(code) == 0) {
    return(list(
      yaml = NULL,
      yamlSource = NULL,
      code = code
    ))
  }
  comment_chars <- engine_comment_chars(engine)
  comment_start <- base::paste0(comment_chars[[1]], "| ")
  comment_end <- base::ifelse(base::length(comment_chars) > 1, comment_chars[[2]], "")
  
  # check for option comments
  match_start <- base::startsWith(code, comment_start)
  match_end <- base::endsWith(base::trimws(code, "right"), comment_end)
  matched_lines <- match_start & match_end
  
  # has to have at least one matched line at the beginning
  if (base::isTRUE(matched_lines[[1]])) {
    
    # divide into yaml and code
    if (base::all(matched_lines)) {
      yamlSource <- code
      code <- c()
    } else {
      last_match <- base::which.min(matched_lines) - 1
      yamlSource <- code[1:last_match]
      code <- code[(last_match+1):base::length(code)]
    }
    
    # trim right
    if (base::any(match_end)) {
      yamlSource <- base::trimws(yamlSource, "right")
    }
  
    # extract yaml from comments, then parse it
    yaml <- base::substr(yamlSource, 
                   base::nchar(comment_start) + 1, 
                   base::nchar(yamlSource) - base::nchar(comment_end))
    yaml_options <- yaml::yaml.load(yaml, eval.expr = TRUE)
    if (!base::is.list(yaml_options) || base::length(base::names(yaml_options)) == 0) {
      warning("Invalid YAML option format in chunk: \n", base::paste(yaml, collapse = "\n"), "\n")
      yaml_options <- list()
    }
    
    # extract code
    if (base::length(code) > 0 && knitr:::is_blank(code[[1]])) {
      code <- code[-1]
      yamlSource <- c(yamlSource, "")
    }
    
    list(
      yaml = yaml_options,
      yamlSource = yamlSource,
      code = code
    )
  } else {
    list(
      yaml = NULL,
      yamlSource = NULL,
      code = code
    )
  }
}

# Quarto internal for support older knitr:
# engines and their associated comments
engine_comment_chars <- function(engine) {
  comment_chars <- list(
    r = "#",
    python = "#",
    julia = "#",
    scala = "//",
    matlab = "%",
    csharp = "//",
    fsharp = "//",
    c = c("/*",  "*/"),
    css = c("/*",  "*/"),
    sas = c("*", ";"),
    powershell = "#",
    bash = "#",
    sql = "--",
    mysql = "--",
    psql = "--",
    lua = "--",
    Rcpp = "//",
    cc = "//",
    stan = "#",
    octave = "#",
    fortran = "!",
    fortran95 = "!",
    awk = "#",
    gawk = "#",
    stata = "*",
    java = "//",
    groovy = "//",
    sed = "#",
    perl = "#",
    ruby = "#",
    tikz = "%",
    js = "//",
    d3 = "//",
    node = "//",
    sass = "//",
    coffee = "#",
    go = "//",
    asy = "//",
    haskell = "--",
    dot = "//",
    apl = "\u235D"
  )
  comment_chars[[engine]] %||% "#"
}

# helper to create an output div
output_div <- function(x, label, classes, attr = NULL) {
  div <- "::: {"
  if (!base::is.null(label) && base::nzchar(label)) {
    div <- base::paste0(div, labelId(label), " ")
  }
  base::paste0(
    div,
    base::paste(base::paste0(".", classes), collapse = " ") ,
    base::ifelse(!base::is.null(attr), base::paste0(" ", attr), ""),
    "}\n",
    base::trimws(x),
    "\n:::\n\n"
  )
}

# Quarto internal: return label as an id like #id
labelId <- function(label) {
  if (!base::is.null(label) && !base::startsWith(label, "#"))
    base::paste0("#", label)
  else
    label
}

# Quarto internal: return figure caption to use from knitr options
figure_cap <- function(options) {
  output_label <- output_label(options)
  if (base::is.null(output_label) || is_figure_label(output_label)) {
    fig.cap <- options[["fig.cap"]]
    fig.subcap <- options[["fig.subcap"]]
    if (!base::is.null(fig.subcap))
      fig.subcap
    else if (!base::is.null(fig.cap))
      fig.cap
    else
      ""
  } else {
    ""
  }
}

# Quarto internal: identify fig- prefixed label
output_label <- function(options) {
  label <- options[["label"]]
  if (!base::is.null(label) && base::grepl("^#?(fig)-", label)) {
    label
  } else {
    NULL
  }
}

# Quarto internal: Append a unique placeholder to figure label
output_label_placeholder <- function(options) {
  kPlaceholder <- "D08295A6-16DC-499D-85A8-8BA656E013A2"
  label <- output_label(options)
  if (is_figure_label(label))
    base::paste0(label, kPlaceholder)
  else
    NULL
}

# Quarto internal: identify #fig- prefix in chunk label
is_figure_label <- function(label) {
  is_label_type("fig", label)
}

# Quarto internal: identify #tbl- prefix in chunk label
is_table_label <- function(label) {
  is_label_type("tbl", label)
}

# Quarto internal: identify #type- prefix in chunk label
is_label_type <- function(type, label) {
  !base::is.null(label) && base::grepl(base::paste0("^#?", type, "-"), label)
}

# Quarto internal: Construct block attribute
block_attr <- function(id = NULL, lang = NULL, class = NULL, attr = NULL) {
  id <- labelId(id)
  if (!base::is.null(lang)) {
    lang <- base::paste0(".", lang)
  }
  if (!base::is.null(class)) {
    class <- base::paste(block_class(class))
  }
  attributes <- c(id, lang, class, attr)
  attributes <- base::paste(attributes[!base::is.null(attributes)], collapse = " ")
  if (base::nzchar(attributes)) {
    base::paste0("{", attributes, "}")
  } else {
    ""
  }
}

# Quarto internal: Construct class name with .
block_class <- function(x) {
  if (base::length(x) > 0) base::gsub('^[.]*', '.', base::unlist(base::strsplit(x, '\\s+')))
}

# Quarto internal: Convert LaTeX size to % value e.g 0.5\linewidth to 50%
latex_sizes_to_percent <- function(options) {
  #  \linewidth
  width <- options[["out.width"]]
  if (!base::is.null(width)) {
    latex_width <- base::regmatches(width, base::regexec("^([0-9\\.]+)\\\\linewidth$", width))
    if (base::length(latex_width[[1]]) > 1) {
      width <- base::paste0(base::as.numeric(latex_width[[1]][[2]]) * 100, "%")
      options[["out.width"]] <- width
    }
  }
  # \textheight
  height <- options[["out.height"]]
  if (!base::is.null(height)) {
    latex_height <- base::regmatches(height, base::regexec("^([0-9\\.]+)\\\\textheight$", height))
    if (base::length(latex_height[[1]]) > 1) {
      height <- base::paste0(base::as.numeric(latex_height[[1]][[2]]) * 100, "%")
      options[["out.height"]] <- height
    }
  }
  options
}

# ported from:
# https://github.com/yihui/knitr/blob/f8f90baad99d873202b8dc8042eab7a88fac232f/R/hooks-latex.R#L151-L171
latex_animation <- function(x, options) {
  
  fig.num = options$fig.num %||% 1L
  
  ow = options$out.width
  # maxwidth does not work with animations
  if (base::identical(ow, '\\maxwidth')) ow = NULL
  if (base::is.numeric(ow)) ow = base::paste0(ow, 'px')
  size = base::paste(c(base::sprintf('width=%s', ow),
                 base::sprintf('height=%s', options$out.height),
                 options$out.extra), collapse = ',')
  
  aniopts = options$aniopts
  aniopts = if (base::is.na(aniopts)) NULL else base::gsub(';', ',', aniopts)
  size = base::paste(c(size, base::sprintf('%s', aniopts)), collapse = ',')
  if (base::nzchar(size)) size = base::sprintf('[%s]', size)
  base::sprintf('\\animategraphics%s{%s}{%s}{%s}{%s}', size, 1 / options$interval,
          base::sub(base::sprintf('%d$', fig.num), '', xfun::sans_ext(x)), 1L, fig.num)
}

# Quarto internal: tweak knitr::is_latex_output() to handle to = "pdf"
is_latex_output <- function(to) {
  knitr:::is_latex_output() || base::identical(to, "pdf")
}


