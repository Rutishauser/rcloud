#################################################################################

# there doesn't seem to be a lower-level way to customize View?

spin.Spin <- function(x, y, z)
{
  if(!is.null(rcloud.spin.caps))
  {
    deferred.rcloud.result( function() rcloud.spin.caps$Spin(x, y, z) )
  }
}

spin.Msg <- function()
{
  #  print( "Msg <- function() {} deferred" )

  if(!is.null(rcloud.spin.caps))
  {
    deferred.rcloud.result( function() rcloud.spin.caps$Msg() )
  }
}

spin.Cube <- function(r=1.0, C=1.0)
{
  # 12 edges

  x3 = matrix(0, 8, 12)

  x3[,1] = c(0, 0, 0, C,  1, 0, 0, C)
  x3[,2] = c(0, 0, 0, C,  0, 1, 0, C)
  x3[,3] = c(1, 0, 0, C,  1, 1, 0, C)
  x3[,4] = c(0, 1, 0, C,  1, 1, 0, C)

  x3[,5] = c(0, 0, 0, C,  0, 0, 1, C)
  x3[,6] = c(0, 1, 0, C,  0, 1, 1, C)
  x3[,7] = c(1, 0, 0, C,  1, 0, 1, C)
  x3[,8] = c(1, 1, 0, C,  1, 1, 1, C)

  x3[, 9] = c(0, 0, 1, C,  1, 0, 1, C)
  x3[,10] = c(0, 0, 1, C,  0, 1, 1, C)
  x3[,11] = c(1, 0, 1, C,  1, 1, 1, C)
  x3[,12] = c(0, 1, 1, C,  1, 1, 1, C)

  dim(x3) = c(4, 24)

  x3 = (x3 - 0.5) * r

  # spin.Spin( x3[1,], x3[2,], x3[3,] )

  if( ! exists("spin.edge") )
  {
    spin.edge <<- c(x3)
  }
  else
  {
    spin.edge <<- append( spin.edge,  c(x3) )
  }
}

spin.Axis <- function(r=1.0, C=0.5)
{
  # 12 edges

  x3 = matrix(0, 8, 23)
  C = C/r+0.5

  # main axes
  x3[,1] = c(-0.1, 0, 0, C,  1.1, 0, 0, C)
  x3[,2] = c(0, -0.1, 0, C,  0, 1.1, 0, C)
  x3[,3] = c(0, 0, -0.1, C,  0, 0, 1.1, C)
  # X arrow
  x3[,4] = c(1.05,  0.015,  0.015, C,  1.1, 0, 0, C)
  x3[,5] = c(1.05, -0.015, -0.015, C,  1.1, 0, 0, C)
  # X 1
  x3[,6] = c(1.0, -0.01,  0, C,  1.0, 0, 0, C)
  x3[,7] = c(0.5, -0.005, 0, C,  0.5, 0, 0, C)
  # X
  x3[,8] = c(1.15, 0.01, 0.0, C,  1.17, 0.03, 0, C)
  x3[,9] = c(1.17, 0.01, 0.0, C,  1.15, 0.03, 0, C)
  # Y arrow
  x3[,10] = c( 0.015, 1.05,  0.015, C,  0, 1.1,  0, C)
  x3[,11] = c(-0.015, 1.05, -0.015, C,  0, 1.1,  0, C)
  # Y 1
  x3[,12] = c(-0.01,  1.0, 0, C,  0, 1.0, 0, C)
  x3[,13] = c(-0.005, 0.5, 0, C,  0, 0.5, 0, C)
  # Y
  x3[,14] = c(-0.05, 1.05, 0.0, C,  -0.045, 1.065, 0, C)
  x3[,15] = c(-0.05, 1.05, 0.0, C,  -0.055, 1.065, 0, C)
  x3[,16] = c(-0.05, 1.05, 0.0, C,  -0.05,  1.035, 0, C)
  # Z arrow
  x3[,17] = c( 0.015,  0.015, 1.05, C,  0, 0, 1.1, C)
  x3[,18] = c(-0.015, -0.015, 1.05, C,  0, 0, 1.1, C)
  # Z 1
  x3[,19] = c(-0.01,  0.0, 1.0, C,  0, 0.0, 1.0, C)
  x3[,20] = c(-0.005, 0.0, 0.5, C,  0, 0.0, 0.5, C)
  # Z
  x3[,21] = c(-0.05, 0.05, 1.05, C,  -0.05, 0.05, 1.03, C)
  x3[,22] = c(-0.05, 0.02, 1.05, C,  -0.05, 0.02, 1.03, C)
  x3[,23] = c(-0.05, 0.05, 1.05, C,  -0.05, 0.02, 1.03, C)

  dim(x3) = c(4, 46)

  x3 = (x3 - 0.5) * r

  # spin.Spin( x3[1,], x3[2,], x3[3,] )

  if( ! exists("spin.edge") )
  {
    spin.edge <<- c(x3)
  }
  else
  {
    spin.edge <<- append( spin.edge,  c(x3) )
  }
}

spin.Show <- function()
{
    print("spin.Show()")

    if( ! exists("spin.edge") )
    {
      spin.edge <<- NULL
    }
    if( ! exists("spin.face") )
    {
      spin.face <<- NULL
    }
    if( ! exists("spin.ball") )
    {
      spin.ball <<- NULL
    }

    # ball

    if( exists("spin.ball" ) )
    {
      u = unique( c(spin.ball$name, c("H", "O", "C")) )
      r.scores <- numeric( length(u) )
      r.scores[] = 0.075
      names(r.scores) <- u
      r.scores["H"] = 0.05

      r <- r.scores[ spin.ball$name ]

      c.scores = sample(20:60, length(u), replace=T)
      c.scores = c.scores / 10.0
      names(c.scores) <- u
      c.scores["H"] =  5.0  #  blue
      c.scores["O"] =  2.0  #  red
      c <- c.scores[ spin.ball$name ]

      print( c( length(spin.ball$x), length(spin.ball$y), length(spin.ball$z) ) )
      if( length(spin.ball$x) != 0 )
      {
        for(I in 1:length(spin.ball$x) )
        {
	  spin.Sphere(spin.ball$x[I], spin.ball$y[I], spin.ball$z[I], R=r[I], C=c[I] )
	  cat( sprintf( "spin.Sphere( %f, %f, %f, %f, %f );\n", spin.ball$x[I], spin.ball$y[I], spin.ball$z[I], r[I], c[I]) )
        }
      }
    }


  if(!is.null(rcloud.spin.caps))
  {
    deferred.rcloud.result( function() rcloud.spin.caps$Spin(spin.edge, spin.face, spin.conf) )
  }
}

spin.ShowWeb <- function()
{
  if(!is.null(rcloud.spin.caps))
  {
    deferred.rcloud.result( function() rcloud.spin.caps$Spin(spin.edge, spin.face, spin.conf) )
  }
}

spin.ShowGL <- function()
{
  if( ! exists("spin.edge") )
  {
    spin.edge <<- NULL
  }

  require(rgl)
  open3d()
  l = length( spin.edge )
  l = l / 4
  t = 1:l
  t = t * 4
  segments3d(x=spin.edge[t-3], y=spin.edge[t-2], z=spin.edge[t-1], col="blue", lwd=1)

  if( exists("spin.ball") )
  {
    u = unique( c(spin.ball$name, c("H", "O", "C")) )
    r.scores <- numeric( length(u) )
    r.scores[] = 0.075
    names(r.scores) <- u
    r.scores["H"] = 0.05

    r <- r.scores[ spin.ball$name ]

    l = length(u)
    c.scores = sample( rainbow(l), l, replace=T)
    names(c.scores) <- u
    c.scores["H"] =  "blue"
    c.scores["O"] =  "red"
    c <- c.scores[ spin.ball$name ]

    for(I in 1:length(spin.ball$x) )
    {
	plot3d(x=spin.ball$x[I], y=spin.ball$y[I], z=spin.ball$z[I], col=c[I], alpha=1.0, add=T, type="s",radius=r[I], box=FALSE, axes=FALSE,xlab="",ylab="",zlab="")
    }
  }
}

spin.Plot <- function(aa, C=1.0)
{
  K = 1.5
  Y0 = min( aa )
  Y1 = max( aa )
  NX = dim( aa )[1]
  NZ = dim( aa )[2]
  NN = NX * NZ
  NN1 = NX * (NZ-1)
  NN2 = (NX-1) * NZ

  T = 0
  A = array( 0, dim=c( 8*(NN1+NN2) ) )
  for(I in 1:NX)
  {
    for(J in 2:NZ)
    {
      A[8*T + 1] = I/NX # X
      A[8*T + 2] = (aa[I,(J-1)] - Y0) / (Y1-Y0) # Y
      A[8*T + 3] = (J-1)/NZ # Z
      A[8*T + 4] = C
      A[8*T + 5] = I/NX # X
      A[8*T + 6] = (aa[I,J] - Y0) / (Y1-Y0) # Y
      A[8*T + 7] = J/NZ # X
      A[8*T + 8] = C
      T = T + 1
    }
  }
  for(J in 1:NZ)
  {
    for(I in 2:NX)
    {
      A[8*T + 1] = (I-1)/NX # X
      A[8*T + 2] = (aa[(I-1),J] - Y0) / (Y1-Y0) # Y
      A[8*T + 3] = J/NZ # Z
      A[8*T + 4] = C
      A[8*T + 5] = I/NX # X
      A[8*T + 6] = (aa[I,J] - Y0) / (Y1-Y0) # Y
      A[8*T + 7] = J/NZ # X
      A[8*T + 8] = C
      T = T + 1
    }
  }
  # NN1 + NN2
  if( ! exists("spin.edge") )
  {
    spin.edge <<- c( (A - 0.5) * K )
  }
  else
  {
    spin.edge <<- append( spin.edge,  c( (A - 0.5) * K ) )
  }
}

spin.Clr <- function()
{
  z = c(0, 0, 0, -1.0)
  spin.face <<- c(z, z, z, z)

  spin.edge <<- NULL
  # spin.face <<- NULL
  spin.ball <<- NULL

  spin.conf <<- list( NEGATIVE=T, DETAIL=3, FACE=T, COLOR=F, WIDTH=800, HEIGHT=600 )
}

#################################################################################

normal <- function(pP)
{
  xD = sqrt( sum( pP * pP) )
  return( pP / xD )
}

middle <- function(pA, pB)
{
  pM = (pA + pB) / 2.0
  return( normal(pM) )
}

addFace <- function(pA, pB, pC, N, R, C, pO)
{
  if(N == 0)
  {
    pA = pA*R + pO
    pB = pB*R + pO
    pC = pC*R + pO
    spin.face <<- c( spin.face, c(pA, C, pB, C, pC, C, pO, -1.0) )
  } else {
    mA = middle(pB, pC)
    mB = middle(pA, pC)
    mC = middle(pB, pA)
    addFace(pA, mB, mC, N-1, R, C, pO)
    addFace(mA, pB, mC, N-1, R, C, pO)
    addFace(mA, mB, pC, N-1, R, C, pO)
    addFace(mA, mB, mC, N-1, R, C, pO)
  }
}

addFrame <-function(pA, pB, pC, N, R, C, pO)
{
  if(N == 0)
  {
    pA = pA*R + pO
    pB = pB*R + pO
    pC = pC*R + pO
    spin.edge <<- c( spin.edge, c(pA, C, pB, C), c(pB, C, pC, C), c(pC, C, pA, C) )
  } else {
    mA = middle(pB, pC)
    mB = middle(pA, pC)
    mC = middle(pB, pA)
    addFrame(pA, mB, mC, N-1, R, C, pO)
    addFrame(mA, pB, mC, N-1, R, C, pO)
    addFrame(mA, mB, pC, N-1, R, C, pO)
    addFrame(mA, mB, mC, N-1, R, C, pO)
  }
}

spin.Edge <-function(pA, pB, C=1.0)
{
  spin.edge <<- c( spin.edge, c(pA, C, pB, C) )
}

spin.Sphere <- function(oX, oY, oZ, R=0.2, C=1.0, N=spin.conf$DETAIL)
{
  pA = c(0,1,0)
  pB = c(0,-1,0)
  pC = c(0,0,-1)
  pD = c(1,0,0)
  pE = c(0,0,1)
  pF = c(-1,0,0)
  pO = c(oX, oY, oZ)
  if( ! spin.conf$FACE )
  {
    # addFrame(pA, pC, pD, N, R, C, pO)
    # addFrame(pA, pD, pE, N, R, C, pO)
    # addFrame(pA, pE, pF, N, R, C, pO)
    # addFrame(pA, pF, pC, N, R, C, pO)
    # addFrame(pB, pC, pD, N, R, C, pO)
    # addFrame(pB, pD, pE, N, R, C, pO)
    # addFrame(pB, pE, pF, N, R, C, pO)
    # addFrame(pB, pF, pC, N, R, C, pO)
    addFrame0(pA, pD, pE, N, R, C, pO)
  }else{
    # addFace(pA, pC, pD, N, R, C, pO)
    # addFace(pA, pD, pE, N, R, C, pO)
    # addFace(pA, pE, pF, N, R, C, pO)
    # addFace(pA, pF, pC, N, R, C, pO)
    # addFace(pB, pC, pD, N, R, C, pO)
    # addFace(pB, pD, pE, N, R, C, pO)
    # addFace(pB, pE, pF, N, R, C, pO)
    # addFace(pB, pF, pC, N, R, C, pO)
    addFaceT(pA, pD, pE, N, R, C, pO)
  }
}

#
#  optimized
#
addFaceT <- function(pA, pB, pC, N, R, C, pO)
{
  if(N == 0)
  {
    pA = pA*R
    pB = pB*R
    pC = pC*R
    v1 = c(1, 1, 1)
    v2 = c(-1, 1, 1)
    v3 = c(1, 1, -1)
    v4 = c(-1, 1, -1)
    v5 = c(1, -1, 1)
    v6 = c(-1, -1, 1)
    v7 = c(1, -1, -1)
    v8 = c(-1, -1, -1)
    spin.face <<- c( spin.face, c(pA+pO, C, pB+pO, C, pC+pO, C, pO, -1.0,
				pA*v2+pO, C, pB*v2+pO, C, pC*v2+pO, C, pO, -1.0,
				pA*v3+pO, C, pB*v3+pO, C, pC*v3+pO, C, pO, -1.0,
				pA*v4+pO, C, pB*v4+pO, C, pC*v4+pO, C, pO, -1.0,
				pA*v5+pO, C, pB*v5+pO, C, pC*v5+pO, C, pO, -1.0,
				pA*v6+pO, C, pB*v6+pO, C, pC*v6+pO, C, pO, -1.0,
				pA*v7+pO, C, pB*v7+pO, C, pC*v7+pO, C, pO, -1.0,
				pA*v8+pO, C, pB*v8+pO, C, pC*v8+pO, C, pO, -1.0) )
  } else {
    mA = middle(pB, pC)
    mB = middle(pA, pC)
    mC = middle(pB, pA)
    addFaceT(pA, mB, mC, N-1, R, C, pO)
    addFaceT(mA, pB, mC, N-1, R, C, pO)
    addFaceT(mA, mB, pC, N-1, R, C, pO)
    addFaceT(mA, mB, mC, N-1, R, C, pO)
  }
}

addFrame0 <-function(pA, pB, pC, N, R, C, pO)
{
  if(N == 0)
  {
    pA = pA*R
    pB = pB*R
    pC = pC*R
    v1 = c(1, 1, 1)
    v2 = c(-1, 1, 1)
    v3 = c(1, 1, -1)
    v4 = c(-1, 1, -1)
    v5 = c(1, -1, 1)
    v6 = c(-1, -1, 1)
    v7 = c(1, -1, -1)
    v8 = c(-1, -1, -1)
    spin.edge <<- c( spin.edge, c(pA+pO, C, pB+pO, C,   pB+pO, C, pC+pO, C,   pC+pO, C, pA+pO, C,
				pA*v2+pO, C, pB*v2+pO, C,   pB*v2+pO, C, pC*v2+pO, C,   pC*v2+pO, C, pA*v2+pO, C,
				pA*v3+pO, C, pB*v3+pO, C,   pB*v3+pO, C, pC*v3+pO, C,   pC*v3+pO, C, pA*v3+pO, C,
				pA*v4+pO, C, pB*v4+pO, C,   pB*v4+pO, C, pC*v4+pO, C,   pC*v4+pO, C, pA*v4+pO, C,
				pA*v5+pO, C, pB*v5+pO, C,   pB*v5+pO, C, pC*v5+pO, C,   pC*v5+pO, C, pA*v5+pO, C,
				pA*v6+pO, C, pB*v6+pO, C,   pB*v6+pO, C, pC*v6+pO, C,   pC*v6+pO, C, pA*v6+pO, C,
				pA*v7+pO, C, pB*v7+pO, C,   pB*v7+pO, C, pC*v7+pO, C,   pC*v7+pO, C, pA*v7+pO, C,
				pA*v8+pO, C, pB*v8+pO, C,   pB*v8+pO, C, pC*v8+pO, C,   pC*v8+pO, C, pA*v8+pO, C) )
  } else {
    mA = middle(pB, pC)
    mB = middle(pA, pC)
    mC = middle(pB, pA)
    addFrame0(pA, mB, mC, N-1, R, C, pO)
    addFrame0(mA, pB, mC, N-1, R, C, pO)
    addFrame0(mA, mB, pC, N-1, R, C, pO)
    addFrame0(mA, mB, mC, N-1, R, C, pO)
  }
}


#################################################################################

spin.Star <- function(oX, oY, oZ, R=0.2, C=1.0, N=spin.conf$DETAIL)
{
  pA = c(0,1,0)
  pB = c(0,-1,0)
  pC = c(0,0,-1)
  pD = c(1,0,0)
  pE = c(0,0,1)
  pF = c(-1,0,0)
  pO = c(oX, oY, oZ)

    addFrameT(pA, pC, pD, N, R, C, pO)
    addFrameT(pA, pD, pE, N, R, C, pO)
    addFrameT(pA, pE, pF, N, R, C, pO)
    addFrameT(pA, pF, pC, N, R, C, pO)
    addFrameT(pB, pC, pD, N, R, C, pO)
    addFrameT(pB, pD, pE, N, R, C, pO)
    addFrameT(pB, pE, pF, N, R, C, pO)
    addFrameT(pB, pF, pC, N, R, C, pO)
}

#
#  a star
#
addFrameT <-function(pA, pB, pC, N, R, C, pO)
{
  if(N == 0)
  {
    pA = pA*R + pO
    pB = pB*R + pO
    pC = pC*R + pO
    spin.edge <<- c( spin.edge, c(pA, C, pO, C), c(pB, C, pO, C), c(pC, C, pO, C) )
  } else {
    mA = middle(pB, pC)
    mB = middle(pA, pC)
    mC = middle(pB, pA)
    addFrameT(pA, mB, mC, N-1, R, C, pO)
    addFrameT(mA, pB, mC, N-1, R, C, pO)
    addFrameT(mA, mB, pC, N-1, R, C, pO)
    addFrameT(mA, mB, mC, N-1, R, C, pO)
  }
}


#################################################################################
