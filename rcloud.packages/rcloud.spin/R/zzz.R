rcloud.spin.caps <- NULL

.onLoad <- function(libname, pkgname)
{
  f <- function(module.name, module.path) {
    path <- system.file("javascript", module.path, package="rcloud.spin")
    caps <- rcloud.install.js.module(module.name,
                                     paste(readLines(path), collapse='\n'))
    caps
  }

  rcloud.spin.caps <<- f("rcloud.spin", "rcloud.spin.js")

  if(!is.null(rcloud.spin.caps))
    rcloud.spin.caps$init()

  if(!is.null(rcloud.spin.caps))
    rcloud.spin.caps$Msg()

# there is no gl-matrix.js so disable
#  f("matrix", "gl-matrix.js")
}
