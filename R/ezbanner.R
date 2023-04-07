b <- function(txt, ftype, bChar, s) {

  #if bchar was not specified it's a default value I am using in this doc
  if(missing(bChar)) {
    bChar <- ":"
  }

  #if type of function is missing, assume it's banner() (for ease)
  if(missing(ftype)) {
    ftype <- 1
  }

  #main if statement
  if (ftype == 1) {
    #if snug arguemnt is missing just assume it's FALSE
    if(missing(s)) {
      s <- FALSE
    }
    result <- banner(txt, snug = s)
    cat(result)
    write_clip(result,"character","")
  } else if (ftype == 2) {
    #if snug arguemnt is missing just assume it's TRUE
    if(missing(s)) {
      s <- TRUE
    }
    result <- boxup(txt, snug = s, bandChar = bChar, rightSideHashes=2)
    cat(result)
    write_clip(result,"character","")
  }
}
