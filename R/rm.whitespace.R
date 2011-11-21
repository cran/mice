# Source by Gerko Vink, University of Utrecht, nov. 2011
# This eliminates the need for str_trim() from package stringr
##############################################################
# Stringr en Plyr are removed and all code is now refering to 
# base functions. Dependencies of the new function are below.
##############################################################
rm.whitespace <- function(string, side = "both"){
    string <- is.string(string) 							#new (own) function - checked
    stopifnot(length(side) == 1) 							#base function
    side <- match.arg(side, c("left", "right", "both")) 	#base function
    pattern <- switch(side, left = "^\\s+", right = "\\s+$",
        both = "^\\s+|\\s+$")								#base function 
    str.replace.all(string, pattern, "")					#new (own) function - checked
}

##############################################################
# DEPENDENCIES: FUNCTIONS THAT rm.whitespace REFERS TO
##############################################################
is.string <- function(string){
    if (!is.atomic(string)) 
        stop("String must be an atomic vector", call. = FALSE)
    if (!is.character(string)) 
        string <- as.character(string)
    string
}

recycle <- function (...) {
    lengths <- vapply(list(...), length, integer(1))
    lengths <- lengths[lengths != 0]
    if (length(lengths) == 0) 
        return(TRUE)
    all(max(lengths)%%lengths == 0)
}

check.pattern <- function (pattern, string, replacement = NULL) 
{
    if (!is.character(pattern)) 
        stop("Pattern must be a character vector", call. = FALSE)
    if (!recycle(string, pattern, replacement)) {
        stop("Lengths of string and pattern not compatible")
    }
    pattern
}

ignored.case <- function (string) {
    ignore.case <- attr(string, "ignore.case")
    if (is.null(ignore.case)) 
        FALSE
    else ignore.case
}

is.fixed <- function (string) {
    fixed <- attr(string, "fixed")
    if (is.null(fixed)) 
        FALSE
    else fixed
}

make.compact <- function(l){
	 Filter(Negate(is.null), l)					#origineel de compact{plyr} functie	
}

recall <- function (f, string, pattern, replacement = NULL) {
    args <- list(pattern, replacement, string, fixed = is.fixed(pattern), 
        ignore.case = ignored.case(pattern))
    do.call(f, make.compact(args))
}

re.mapply <- function (f, string, pattern, replacement = NULL) {
    args <- list(FUN = f, SIMPLIFY = FALSE, USE.NAMES = FALSE, 
        pattern, replacement, string, MoreArgs = list(fixed = is.fixed(pattern), 
            ignore.case = ignored.case(pattern)))
    do.call("mapply", compact(args))
}

str.replace.all <- function (string, pattern, replacement){
    string <- is.string(string)
    pattern <- check.pattern(pattern, string, replacement)
    if (length(pattern) == 1 && length(replacement) == 1) {
        recall("gsub", string, pattern, replacement)
    }
    else {
        unlist(re_mapply("gsub", string, pattern, replacement))
    }
}

