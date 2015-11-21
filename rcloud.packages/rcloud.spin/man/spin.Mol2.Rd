\name{spin.Mol2}
\alias{spin}
\alias{Mol2}
\title{To parse Crystall Structure in MOL2 format}
\description{
Mol2 is a popular format for representing a crystall structure.
Data in the Mol2 format is available for downloading from a number of crystall database sites.
E.g. http://zinc.docking.org/results/catalogs?page.format=basic&filter.purchasability=all&catalog.code=ligand
}
\details{
A file in the Mol2 format contains a number of sections separated with the delimiters like @<TRIPOS>.
The ATOM section is a list of atom records. Each record occupies a separate line. The atom record
includes the name of the element and the three coordinates.
The BOND section is a list of the connected atom pairs.
}
\examples{
require(rcloud.spin)

# default example
spin.Clr()
spin.Mol2()
spin.Show()

# keys:
# Use the CTRL key to move forward.
# Use the ALT key to move back.
# Use the mouse (hold the right button) to rotate the structure.
spin.Clr()
spin.Sphere( 0.0, 0.0, 0.0, 0.075000, 3.600000 )
for( Y in -2:2 )  for(Z in -2:2)  spin.Edge( c(-3, Y/2, Z/2), c(3, Y/2, Z/2), 5 )
for( X in -2:2 )  for(Z in -2:2)  spin.Edge( c(X/2, -3, Z/2), c(X/2, 3, Z/2), 5 )
for( X in -2:2 )  for(Y in -2:2)  spin.Edge( c(X/2, Y/2, -3), c(X/2, Y/2, 3), 5 )
spin.Show()

# example with URL
spin.Clr()
spin.Axis(1.5)
spin.Mol2("http://zinc11.docking.org/fget.pl?l=0&z=56792163&f=m")
spin.Show()

# example with a bigger image
spin.Clr()
spin.Axis(1.5)
spin.Mol2("http://zinc11.docking.org/fget.pl?l=0&z=56335284&f=m")
spin.conf$WIDTH = 1280
spin.conf$HEIGHT = 1024
spin.Show()

# example with frames
spin.Clr()
spin.Mol2("http://zinc11.docking.org/fget.pl?l=0&z=56792163&f=m")
spin.conf$FACE = F
spin.Show()

# example of a plot
spin.Clr()
spin.Cube(1.5, 2.0)
spin.Plot(volcano, 3.25)
spin.conf$FACE = F
spin.conf$DETAIL = 3
spin.Sphere(0.75, 0.25, 0, 0.3, 2.75)
spin.Show()

# example of a program
require(rcloud.spin)
spin.Clr()
spin.Axis()
# spin.Cube()
o = c(0,0,0)
a = c(0.5 ,0.5, 0.5)
cM = c( 1, 0, 0,  0, 1, 0,  0, 0, 1 )
spin.Edge(o, a, 3)
  alpha = 1 / 10.0
  cA = c( cos(alpha), -sin(alpha), 0,  sin(alpha), cos(alpha), 0,  0, 0, 1 )
  A = matrix( cA, 3, 3, T)
  M = matrix( cM, 3, 3, T)
for( F in 1:30 )
{
  M = M %*% A
  b = M %*% a
  spin.Edge(o, b, 5)
}
  beta = 1 / 10.0
  cB = c( cos(beta), 0, -sin(beta),  0, 1, 0,   sin(beta), 0, cos(beta)  )
  B = matrix( cB, 3, 3, T)
  M = matrix( cM, 3, 3, T)
for( F in 1:30 )
{
  M = M %*% B
  b = M %*% a
  spin.Edge(o, b, 2)
}
  M = matrix( cM, 3, 3, T)
for( F in 1:30 )
{
  # M = M %*% B %*% A
  M = B %*% A %*% M
  b = M %*% a
  spin.Edge(o, b, 4)
}
spin.Show()

}
