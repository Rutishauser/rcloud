#
######################################################################## TEST

hello <- function(name="World")
{
  paste("Hello", name)
}

rwebgl <- function()
{
  return("Hello WebGL !")
}

# http://jing.cz3.nus.edu.sg/model/examples/Abacavir.mol2
# http://zinc11.docking.org/fget.pl?l=0&z=125172690&f=m
# /work/spin/tmp.mol2

#
#  install.packages('rgl')
#  library(rgl)
#  open3d()
#  a = mol2()
#  segments3d(x=a$x, y=a$y, z=a$z, col="gray", lwd=2)
#  plot3d(d1, col=rainbow(7)[d1$clr],alpha=0.7,add=F,type="s",radius=d1$r/42, box=FALSE,axes=FALSE,xlab="",ylab="",zlab="")
#  writeWebGL(dir="/work/rcloud/htdocs/tmp")
#

######################################################################## spin.Mol2()

spin.Mol2 <- function(file = "http://jing.cz3.nus.edu.sg/model/examples/Abacavir.mol2", k = 0.2)
{
  mol <- scan(file)
  data_atom <- scan_atom(mol)
  t <- scan_bond(mol)

  # print(t)
  # print( data_atom$x[t] )

  data_atom$x = data_atom$x * k
  data_atom$y = data_atom$y * k
  data_atom$z = data_atom$z * k

  #  BOND
  # spin.bond <<- list( x=data_atom$x, y=data_atom$y, z=data_atom$z, t=t )
  if( ! exists("spin.edge") )
  {
    spin.edge <<- interleave4(data_atom$x[t], data_atom$y[t], data_atom$z[t], 1.0)
  }
  else
  {
    spin.edge <<- append( spin.edge, interleave4(data_atom$x[t], data_atom$y[t], data_atom$z[t], 1.0) )
  }

  #  BALL
  spin.ball <<- list( x=data_atom$x, y=data_atom$y, z=data_atom$z, name=data_atom$name )
  # spin.ball <<- interleave3(data_atom$x, data_atom$y, data_atom$z)
}

######################################################################## scan

scan <- function(file)
{
  text <- readLines(file,encoding="UTF-8")
  sec = which(substr(text,1,1) == "@")

  return( list(text = text, sec = sec) );
}

######################################################################## atom

scan_atom <- function(data)
{
  n = which( data$text[data$sec] == "@<TRIPOS>ATOM")
  sec_a = data$text[ (data$sec[n]+1) : (data$sec[n+1]-1)]

  sss = strsplit( sec_a , " ")

  all = unlist(sss)[ which( unlist(sss) != "" ) ]

  #  id = all[ (seq_along(sss)-1)*9 + 1 ]
  x = all[ (seq_along(sss)-1)*9 + 3 ]
  y = all[ (seq_along(sss)-1)*9 + 4 ]
  z = all[ (seq_along(sss)-1)*9 + 5 ]
  name = all[ (seq_along(sss)-1)*9 + 2 ]

  reg = regexpr('[[:alpha:]]+', name)
  type =  regmatches(name ,reg)

  return( list(x=as.numeric(x), y=as.numeric(y), z=as.numeric(z), name=type) )
}

######################################################################## bond

interleave <- function(v1,v2)
{
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]
}

interleave3 <- function(v1,v2,v3)
{
  ord1 <- 3*(1:length(v1))-2
  ord2 <- 3*(1:length(v2))-1
  ord3 <- 3*(1:length(v3))
  c(v1,v2,v3)[order(c(ord1,ord2,ord3))]
}

interleave4 <- function(v1,v2,v3,v4)
{
  ord1 <- 4*(1:length(v1))-3
  ord2 <- 4*(1:length(v1))-2
  ord3 <- 4*(1:length(v2))-1
  ord4 <- 4*(1:length(v3))
  c(v1,v2,v3,v4)[order(c(ord1,ord2,ord3,ord4))]
}

scan_bond <- function(data)
{
  n = which( data$text[data$sec] == "@<TRIPOS>BOND")
  end = length( data$text ) - 1  # NA
  if( ! is.na(data$sec[n+1]) ) end = data$sec[n+1] - 1
  sec_b = data$text[ (data$sec[n]+1) : end ]

  sss = strsplit( sec_b , " ")

  all = unlist(sss)[ which( unlist(sss) != "" ) ]

  i = all[ (seq_along(sss)-1)*4 + 2 ]
  j = all[ (seq_along(sss)-1)*4 + 3 ]

  t = interleave(i, j)

  return( as.integer(t) )
}

######################################################################## END
#
