##-- Production

F <- function(l,k,n,lambda,alpha,A=1){
  (l^lambda)*(k^alpha)*((A*n)^(1-lambda-alpha))
}

##-- No Growth

# Pre-Industrial

k.preind <- function(k,x,delta,beta,lambda,alpha){
  (1-delta)*k + beta*(lambda + alpha)*(1-x)*F(1,k,1,lambda,alpha)
}

kss.preind <- function(x,delta,beta,lambda,alpha){
  ((beta*(lambda+alpha)*(1-x))/delta)^(1/(1-alpha))
}

# No debt, with frictions

k.ndwf <- function(k,s,x,delta,beta,lambda,alpha,pi){
  (1-delta)*k + beta*(lambda + alpha*(1 + (pi - 1)*s))*(1-x)*F(1,k,1,lambda,alpha)
}

kss.ndwf <- function(x,delta,beta,lambda,alpha,pi){
  ((pi*beta*alpha*(1-x))/delta)^(1/(1-alpha))
}

s.ndwf <- function(k,s,x,delta,beta,lambda,alpha,pi){
  (((1-delta)*k + pi*beta*alpha*(1-x)*F(1,k,1,lambda,alpha))*s)/k.ndwf(k,s,x,delta,beta,lambda,alpha,pi)
}

sss.ndwf <- function(x,delta,beta,lambda,alpha,pi){
  1 - (lambda/(alpha*(pi-1)))
}

# For V&V only

# With debt and frictions

R.wdwf <- function(k,delta,alpha,tau){
  (1-tau)*alpha*k^(alpha-1) + 1 - delta
}

d.wdwf <- function(k,s,d,x,delta,beta,lambda,alpha,pi,tau){
  R.wdwf(k,delta,alpha,tau)*d + (x-tau)*F(1,k,1,lambda,alpha)
}

k.wdwf <- function(k,s,d,x,delta,beta,lambda,alpha,pi,tau){
  r <- R.wdwf(k,delta,alpha,tau)
  (1-delta)*k + beta*((lambda + alpha*(1 + (pi - 1)*s))*(1-tau)*F(1,k,1,lambda,alpha) + r*d) - d.wdwf(k,s,d,x,delta,beta,lambda,alpha,pi,tau)
}

kss.wdwf <- function(delta,beta,lambda,alpha,pi,tau){
  ((pi*beta*alpha*(1-tau))/delta)^(1/(1-alpha))
}

s.wdwf <- function(k,s,d,x,delta,beta,lambda,alpha,pi,tau){
  (((1-delta)*k + pi*beta*alpha*(1-tau)*F(1,k,1,lambda,alpha))*s)/k.wdwf(k,s,d,x,delta,beta,lambda,alpha,pi,tau)
}

sss.wdwf <- function(x,delta,beta,lambda,alpha,pi,tau){
  1 - (lambda - ((x-tau)/(1-tau))*(1+(pi*(1-beta))/(delta*(pi*beta - 1))))/(alpha*(pi-1))
}

# No debt, no frictions

k.ndnf <- function(k,x,delta,beta,lambda,alpha,pi){
  (1-delta)*k + pi*beta*(lambda + alpha)*(1-x)*F(1,k,1,lambda,alpha)
}

kss.ndnf <- function(x,delta,beta,lambda,alpha,pi){
  ((pi*beta*(lambda+alpha)*(1-x))/delta)^(1/(1-alpha))
}

s.ndnf <- function(k,s,x,delta,beta,lambda,alpha,pi){
  (((1-delta)*k + pi*beta*alpha*(1-x)*F(1,k,1,lambda,alpha))*s)/k.ndnf(k,x,delta,beta,lambda,alpha,pi)
}



##-- With Growth

# Pre 1982

k.gpreind <- function(k,A,g,x,delta,beta,lambda,alpha){
  (1-delta-g)*k + beta*(lambda + alpha)*(1-x)*F(1,k,1,lambda,alpha,A)
}

# With debt and frictions

R.gwdwf <- function(k,delta,alpha,tau,lambda,A,g){
  (1-tau)*alpha*(k^(alpha-1))*(A^(1-lambda-alpha)) + 1 - delta - g
}

d.gwdwf <- function(k,s,d,A,g,x,delta,beta,lambda,alpha,pi,tau){
  R.gwdwf(k,delta,alpha,tau,lambda,A,g)*d + (x-tau)*F(1,k,1,lambda,alpha,A)
}

k.gwdwf <- function(k,s,d,A,g,x,delta,beta,lambda,alpha,pi,tau){
  r <- R.gwdwf(k,delta,alpha,tau,lambda,A,g)
  (1-delta-g)*k + beta*((lambda + alpha*(1 + (pi - 1)*s))*(1-tau)*F(1,k,1,lambda,alpha,A) + r*d) - d.gwdwf(k,s,d,A,g,x,delta,beta,lambda,alpha,pi,tau)
}

s.gwdwf <- function(k,s,d,A,g,x,delta,beta,lambda,alpha,pi,tau){
  (((1-delta-g)*k + pi*beta*alpha*(1-tau)*F(1,k,1,lambda,alpha,A))*s)/k.gwdwf(k,s,d,A,g,x,delta,beta,lambda,alpha,pi,tau)
}

# No debt, no frictions

k.gndnf <- function(k,s,A,g,x,delta,beta,lambda,alpha,pi){
  (1-delta-g)*k + pi*beta*(lambda + alpha)*(1-x)*F(1,k,1,lambda,alpha,A)
}

s.gndnf <- function(k,s,A,g,x,delta,beta,lambda,alpha,pi){
  (((1-delta-g)*k + pi*beta*alpha*(1-x)*F(1,k,1,lambda,alpha,A))*s)/k.gndnf(k,s,A,g,x,delta,beta,lambda,alpha,pi)
}

# No debt, frictions

k.gndwf <- function(k,s,A,g,x,delta,beta,lambda,alpha,pi){
  (1-delta-g)*k + beta*(lambda + alpha*(1 + (pi - 1)*s))*(1-x)*F(1,k,1,lambda,alpha,A)
}

s.gndwf <- function(k,s,A,g,x,delta,beta,lambda,alpha,pi){
  (((1-delta-g)*k + pi*beta*alpha*(1-x)*F(1,k,1,lambda,alpha,A))*s)/k.gndwf(k,s,A,g,x,delta,beta,lambda,alpha,pi)
}

##-- Iterations

recur.s <- function(FUN,iters,k,s0,...){
  ret <- NULL
  
  s <- s0
  for(i in 1:iters){
    ret <- c(ret,s)
    s <- FUN(k[i],s,...)
  }
  
  return(ret)
}

recur.k <- function(FUN,iters,k0,A0 = 1,g = 0,...){
  ret <- NULL
  
  k <- k0
  A <- A0
  for(i in 1:iters){
    ret <- c(ret,k)
    
    if(g == 0){
      k <- FUN(k,...)
    } else {
      A <- (1+g)*A
      k <- FUN(k,A,g,...)
    }
  }
  
  return(ret)
}

recur.both <- function(Fk,Fs,iters,k0,s0,...){
  ret.k <- NULL
  ret.s <- NULL 
  
  k <- k0
  s <- s0
  
  for(i in 1:iters){
    ret.k <- c(ret.k,k)
    ret.s <- c(ret.s,s)
    new.k <- Fk(k,s,...)
    new.s <- Fs(k,s,...)
    k <- new.k
    s <- new.s
  }
  
  return(list(k = ret.k, s = ret.s))
}

recur.debt <- function(Fk,Fs,Fd,iters,k0,s0,d0,...){
  ret.k <- NULL
  ret.s <- NULL 
  ret.d <- NULL 
  k <- k0
  s <- s0
  d <- d0
  
  for(i in 1:iters){
    ret.k <- c(ret.k,k)
    ret.s <- c(ret.s,s)
    ret.d <- c(ret.d,d)
    new.k <- Fk(k,s,d,...)
    new.s <- Fs(k,s,d,...)
    new.d <- Fd(k,s,d,...)
    k <- new.k
    s <- new.s
    d <- new.d
  }
  
  return(list(k = ret.k, s = ret.s, d = ret.d))
}

recur.growth <- function(Fk,Fs,Fd=NULL,iters,g,k0,s0,d0=NULL,A0,...){
  ret.k <- NULL
  ret.s <- NULL 
  ret.d <- NULL 
  k <- k0
  s <- s0
  d <- d0
  A <- A0
  n <- iters
  
  if(iters < 100){
    iters = 100
  }
  
  for(i in 1:iters){
    ret.k <- c(ret.k,k)
    ret.s <- c(ret.s,s)
    ret.d <- c(ret.d,d)
    A <- (1+g)*A
    
    if(is.null(Fd)){
      new.k <- Fk(k,s,A,g,...)
      new.s <- Fs(k,s,A,g,...)
    } else {
      new.k <- Fk(k,s,d,A,g,...)
      new.s <- Fs(k,s,d,A,g,...)
      new.d <- Fd(k,s,d,A,g,...)
      d <- new.d
    }
    
    k <- new.k
    s <- new.s
    
  }
  
  return(list(k = ret.k[1:n], s = ret.s[1:n], d = ret.d[1:n], kss = ret.k[iters], sss = ret.s[iters]))
}

get.stats <- function(k,s,A0,g,x,delta,beta,lambda,alpha,pi){
  
  ret.w <- NULL
  ret.y <- NULL
  ret.c <- NULL
  ret.pk <- NULL
  ret.uk <- NULL
  ret.i <- NULL
  ret.kef <- NULL
  ret.ce <- NULL
  ret.cs <- NULL
  ret.cm <- NULL
  ret.A <- NULL
  A <- A0
  
  for(j in 1:length(k)){
    ret.A <- c(ret.A, A)
    y <- F(1,k[j],1,lambda,alpha,A)
    ret.i <- c(ret.i, beta*y*(1-x))
    ret.y <- c(ret.y, y)
    ret.w <- c(ret.w, (1-alpha-lambda)*(k[j]^alpha)*(A^(1-alpha-lambda)))
    ret.c <- c(ret.c, (1-beta)*y*(1-x))
    ret.ce <- c(ret.ce, (1-x)*y*alpha*s[j]*(1-beta))
    ret.cs <- c(ret.cs, (1-x)*y*lambda*(1-beta) + (1-x)*y*(1-s[j])*alpha*(1-beta))
    ret.cm <- c(ret.cm, (1-alpha-lambda)*y*(1-beta)*(1-x))
    ret.pk <- c(ret.pk, alpha*(A^(1-lambda-alpha))*(k[j]^(alpha-1)))
    ret.uk <- c(ret.uk, (k[j]^alpha)*(A^(1-lambda-alpha)))
    ret.kef <- c(ret.kef, k[j]/A)
    A <- (1+g)*A
  }
  
  return(list(A = ret.A, y = ret.y, w = ret.w, c = ret.c, pk = ret.pk, uk = ret.uk, i = ret.i, ce = ret.ce, cs = ret.cs, cm = ret.cm, kef = ret.kef))
}




