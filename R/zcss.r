#' Create a stylesheet object
#'
#' @import magrittr
#' @export
zcss <- function(...) {
  readLines(...) %>%
    tokenize %>%
    parseCssTokens %>%
    compilePack
}



tokenize <- function(lines) {
  lines %>%
    strsplit(split = "(?!\\\\)\\s+|(?=[{}:;@.])", perl=TRUE) %>%
    unlist %>%
    Filter(f=nchar)
}


parseCssTokens <- function(tokens) {
  zcss <- list()
  rule <- list();


  i <- 0;
  state <- "PATTERN"
  while(i < length(tokens)) {
    i <- i + 1
    token <- tokens[i]

    if(state == "PATTERN") {

      if(token == "@") {
        attr(rule, "dev") <- tokens[i <- i + 1]
      }
      else if(token == ".") {
        attr(rule, "class") <- append(attr(rule, "class"), tokens[i <- i + 1])
      }
      else if(token == "{") {
        state <- "RULES"
      }
      else {
        attr(rule, "function") <- token
      }

    } else if(state == "RULES") {

      if(token == "}") {
        zcss  <- append(zcss, list(rule))
        rule <- list()
        state <- "PATTERN"
      } else if(token == ".") {
        # Hacking around tokenizer to use .s in rule keys
        tokens[i+1] <- paste(tokens[-1:1 + i], collapse="")
      }
      else if(token == ":"){
        key <- tokens[i-1]

        start <- i <- i + 1
        while(tokens[i] != ";" && tokens[i+1] != "}") i <- i + 1

        value <- paste(text=tokens[start:i], collapse="")
        value <- parse(text=value, keep.source = FALSE)

        rule[key] <- value
      }
    }
  }

  # Group rules by function
  zcss <- split(zcss, sapply(zcss, attr, "function"))
  zcss[] <- lapply(zcss,
        function(x) {
          f <- function(x, k) length(attr(x,k))
          # Order rules most to least specific
          x[order( sapply(x,f,"class"), sapply(x,f,"dev"), decreasing = TRUE)];
        })

  zcss
}



compilePack <- function(zcss){
 e <- new.env(parent=parent.frame())
  for(fn in names(zcss)) {
    .root  <- get(fn, 1)
    e[[fn]] <- local({

      .name  <- fn
      .rules <- zcss[[fn]]


      this <- function(.class){

        # Call the next highest function with the same name
        # f <- get(.name)
        # if(identical(f, this)){
          es <- search() %>% seq_along %>%
                  lapply(pos.to.env)   %>%
                  sapply(exists, x=.name, inherits=FALSE) %>%
                  which

          i <- Filter(function(i) identical(get(.name, i, inherits=FALSE), this), es)
          i <- if(length(i)) min(es[es > i]) else min(es)
          f <- get(.name, i)
        # }


        C <- match.call()

        if(!missing(.class) && !('.class' %in% formals(f))) C[".class"] <- NULL

        # Match classes
        pred <- if(missing(.class)) function(r, k=attr(r, "class")) is.null(k)
                    else function(r, k=attr(r, "class")) all(k %in% .class) || is.null(k);

        .rules <- Filter(pred, .rules);

        # Match devices
        DEV  <- names(dev.cur());
        pred <- function(r, d=attr(r, "dev")) is.null(d) || d == DEV;
        .rules <- Filter(pred, .rules)


        for(r in .rules) {
          r[names(C)] <- NULL
          C[names(r)] <- r

        }

        C[[1]] <- quote(.f)
        e <- new.env(parent=parent.frame())
        e$.f <- f

        eval(C, e)
      }

      formals(this) <- c(formals(.root), formals(this))

      this
    })
  }


  e



}


