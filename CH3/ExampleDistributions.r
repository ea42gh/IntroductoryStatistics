

# ==============================================================================================
if (F) {
    x11()
    plotnormal(0,1,200)
}

if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(5, 10, 20, 100)) plotbinom( i, 0.5)
   #title( "Binomial Distribution; Increasing n" )

   x11()
   par(mfrow=c(2,2))
   for (i in c(.1,.3,.5,.95)) plotbinom(100, i)
   #title( "Binomial Distribution; Increasing n" )
}

if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(5, 10, 20, 50)) plotnbinom( i, 100, 0.5)
   #title( "Binomial Distribution; Increasing n" )

   x11()
   par(mfrow=c(2,2))
   for (i in c(5, 10, 20, 50)) plotnbinom( i, 100, 0.1)
   #title( "Binomial Distribution; Increasing n" )
}

if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(0.1, 1, 5., 10)) plotpoisson( i)
}

if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(0.05, 1, 2, 5)) plotweibull(i,1)
   x11()
   par(mfrow=c(2,2))
   for (i in c(0.05, 1, 2, 5)) plotweibull(1,i)
   x11()
   par(mfrow=c(2,2))
   for (i in c(0.5, 5, 10, 15)) plotweibull(i,i)
}



if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(0.05, 0.1, 0.25, 0.5)) plotexponential( i)
}

if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(0.1, 1, 5, 10)) plotgamma(i,0.2)

   x11()
   par(mfrow=c(2,2))
   for (i in c(0.02, .1, .5, 2)) plotgamma(5,i)
}

if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(0.01, 0.1, 0.2, 0.5)) plotgeom( i)
}

if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(0,40,200,400)) plothyper(500-i,500+i,100)
}

if (F) {
   x11()
   par(mfrow=c(2,2))
   for (i in c(.001,.5,1., 4)) plotbeta(i,.5)
   x11()
   par(mfrow=c(2,2))
   for (i in c(.001,.5,1., 4)) plotbeta(.5,i)
   x11()
   par(mfrow=c(2,2))
   for (i in c(.001,.5,1., 4)) plotbeta(i,i)
   x11()
   par(mfrow=c(2,2))
   for (i in c(.001,.5,1., 4)) for (j in c(4.,1.,.5,.001)) plotbeta(j,i)
   x11()
   par(mfrow=c(2,2))
   for (i in c(5,50.,100, 200)) plotbeta(i,i)
}

