4.2. The t Distribution
> x <- seq(-20,20,by=.5)
> y <- dt(x,df=10)
> plot(x,y)
> y <- dt(x,df=50)
> plot(x,y)

4.4. The Chi-Squared Distribution
> x <- seq(-20,20,by=.5)
> y <- dchisq(x,df=10)
> plot(x,y)
> y <- dchisq(x,df=12)
> plot(x,y)

