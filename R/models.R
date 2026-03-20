bev_holt <- nls(Recruitment~a*SSB/(1+SSB/Bhalf),data=test1,start=list(a=1,Bhalf=1e7),
                control = nls.control(maxiter = 1000))
# @import bslib, nav_panel, eller bslib::

bev_holt_log <- -nls(log(Recruitment)~log(a*SSB/(1+SSB/Bhalf)),data=test1,start=coef(mBH),lower=c(0.1,1),algorithm="port")

bev_holt1 <- srStarts(Recruitment ~ SSB, data = test1)
fit1 <- nls(log(Recruitment)~log(bev_holt1(SSB, a, b)), data = test1, start = bev_holt1)


## make these as functions?

bh_model <- function(a,b,data) {
  #includes the starting parameters?
  SSB <- data |>
    select(SSB)
  Recruitment <- data |>
    select(Recruitment)

  nls(Recruitment~a*SSB/(1+SSB/b),data=d,start=list(a=a,b=b))
}

# ui <- bslib: blahba
