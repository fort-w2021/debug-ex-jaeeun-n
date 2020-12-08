match.arg <- function (arg, choices, several.ok = FALSE)
{
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    # get list of arguments of the function that called match.arg()
    # by calling the parent function (sys.function) with the frame number
    # of the parent environment (sys.parent).
    # in our case: list of arguments of make_something()
    choices <- eval(formal.args[[as.character(substitute(arg))]],
                    envir = sys.frame(sysP))
    # set 'choices' to the pre-defined argument options in the parent
    # function for the argument 'arg' (in march.arg())
    # by calling the argument name of 'args' (substitute()) in the parent
    # environment (where match.arg() is called and make_something() is defined)
    # and accessing to the respective element in 'formal.args'.
    # in our case: get the options for 'something' in make_something() (=
    # 'args' in match.args())
  }
  if (is.null(arg)) return(choices[1L])
  else if(!is.character(arg))
    stop("'arg' must be NULL or a character vector")
  if (!several.ok) { # most important (default) case:
    ## the arg can be the whole of choices as a default argument.
    if(identical(arg, choices)) return(arg[1L])
    if(length(arg) > 1L) stop("'arg' must be of length 1")
  } else if(length(arg) == 0L) stop("'arg' must be of length >= 1")

  ## handle each element of arg separately
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  # look if there is a match in the 'choices' vector for each element in 'args'
  # and number the matches in i (set 0L if there is no match)
  if (all(i == 0L))
    stop(gettextf("'arg' should be one of %s",
                  paste(dQuote(choices), collapse = ", ")),
         domain = NA)
    # print the values of 'choices' and do not translate the message
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1)
    # This condition is never true.
    # If !several.ok = TRUE, the length of 'args' must be 1.
    # Otherwise the function is stopped in the upper part of the code.
    # If length(args) = 1, then length(i) = 1.
    # So this if-statement is never entered.
    stop("there is more than one match in 'match.arg'")
  choices[i]
}
