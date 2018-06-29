require(gems)

tf <- generateHazardMatrix(18)

diagrate_infant <- 10
diagrate_pregwoman <- 1
diagrate_general <- 0.05

HIVmort_acute <- 0
HIVmort_chronic <- 0.1
HIVmort_treated <- 0.01
HIVmort_treated_failing <- 0.1

vlfrate <- 0.05
secondvlfrate <- 0.05

falseswitchrate <- 0.01
nonVLswitchrate <- 0.2
VLswitchrate <- 1

dropout_early <- 0.25
dropout_late <- 0.1
dropout_second <- 0.1

b2crate <- 0.33




tf[[1,2]] <- function() {0.25}
tf[[1,3]] <- function(t, bl) {
  ifelse(bl[1]+t<1990, 0, ifelse(bl[2]+t<2, diagrate_infant, ifelse(bl[3]==1 & bl[2]+t>18 & bl[2]+t<40 & bl[1]+t>=2011, diagrate_pregwoman, diagrate_general)))
}
tf[[1,18]] <- function(t, par) {0*t + HIVmort_acute + 0*par} 

tf[[2,3]] <- function(t, bl, history) {
  ifelse(bl[1]+t+sum(history)<1990, 0, ifelse(bl[2]+t+sum(history)<2, 2, ifelse(bl[3]==1 & bl[2]+t+sum(history)>18 & bl[2]+t+sum(history)<40 & bl[1]+t+sum(history)>=2011, 2, 5)))
}
tf[[2,18]] <- function(t) {0*t + HIVmort_chronic}

tf[[3,4]] <- function(t, bl, history) {
  ifelse(bl[1]+t+sum(history)<2003, 0, ifelse(bl[1]+t+sum(history)<2011, 0.3, 1))
}
tf[[3,18]] <- function(t) {0*t + HIVmort_chronic}

tf[[4,5]] <- function(t) {
  ifelse(t<0.25, 0, vlfrate)
}
tf[[4,6]] <- function(t, bl, history) {
  ifelse(bl[1]+t+sum(history)<2012, falseswitchrate, 0)
}
tf[[4,8]] <- function(t) {
  ifelse(t<1, dropout_early, dropout_late)
}
tf[[4,18]] <- function(t) {0*t + HIVmort_treated}

tf[[5,6]] <- function(t, bl, history) {
  ifelse(bl[1]+t+sum(history)<2012, nonVLswitchrate, VLswitchrate)
}
tf[[5,8]] <- function(t, history) {
  ifelse(t+history[49]<1, dropout_early, dropout_late)
}
tf[[5,18]] <- function(t) {0*t + HIVmort_treated_failing}

tf[[6,7]] <- function(t) {
  ifelse(t<0.25, 0, secondvlfrate)
}
tf[[6,8]] <- function(t, history) {
  ifelse(t+sum(history[49:63])<1, dropout_early, dropout_late)
}
tf[[6,18]] <- function(t) {0*t + HIVmort_treated}

tf[[7,8]] <- function(t, history) {
  ifelse(t+sum(history[49:76])<1, dropout_early, dropout_late)
}
tf[[7,18]] <- function(t) {0*t + HIVmort_treated_failing}

tf[[8,9]] <- function(t, history) {
  0*t+ifelse(sum(history[63:98])==0, b2crate, 0)
}
tf[[8,10]] <- function(t, history) {
  0*t+ifelse(sum(history[63:75])>0 & sum(history[76:98])==0, b2crate, 0)
}
tf[[8,11]] <- function(t, history) {
  0*t+ifelse(sum(history[76:87])>0 & sum(history[88:98])==0, b2crate, 0)
}
tf[[8,12]] <- function(t, history) {
  0*t+ifelse(sum(history[88:98])>0, b2crate, 0)
}
tf[[8,18]] <- function(t) {0*t + HIVmort_chronic}

tf[[9,10]] <- function(t) {0*t + vlfrate}
tf[[9,11]] <- function(t, bl, history) {
  ifelse(bl[1]+t+sum(history)<2012, falseswitchrate, 0)
}
tf[[9,13]] <- function(t) {0*t + dropout_second}
tf[[9,18]] <- function(t) {0*t + HIVmort_treated}

tf[[10,11]] <- function(t, bl, history) {
  ifelse(bl[1]+t+sum(history)<2012, nonVLswitchrate, VLswitchrate)
}
tf[[10,13]] <- function(t) {0*t + dropout_second}
tf[[10,18]] <- function(t) {0*t + HIVmort_treated_failing}

tf[[11,12]] <- function(t) {0*t + secondvlfrate}
tf[[11,13]] <- function(t) {0*t + dropout_second}
tf[[11,18]] <- function(t) {0*t + HIVmort_treated}

tf[[12,13]] <- function(t) {0*t + dropout_second}
tf[[12,18]] <- function(t) {0*t + HIVmort_treated_failing}

tf[[13,14]] <- function(t, history) {
  0*t+ifelse(sum(history[118:138])==0, b2crate, 0)
}
tf[[13,15]] <- function(t, history) {
  0*t+ifelse(sum(history[118:125])>0 & sum(history[126:138])==0, b2crate, 0)
}
tf[[13,16]] <- function(t, history) {
  0*t+ifelse(sum(history[126:132])>0 & sum(history[133:138])==0, b2crate, 0)
}
tf[[13,17]] <- function(t, history) {
  0*t+ifelse(sum(history[133:138])>0, b2crate, 0)
}
tf[[13,18]] <- function(t) {0*t + HIVmort_chronic}

tf[[14,15]] <- function(t) {0*t + vlfrate}
tf[[14,16]] <- function(t, bl, history) {
  ifelse(bl[1]+t+sum(history)<2012, falseswitchrate, 0)
}
tf[[14,18]] <- function(t) {0*t + HIVmort_treated}

tf[[15,16]] <- function(t, bl, history) {
  ifelse(bl[1]+t+sum(history)<2012, nonVLswitchrate, VLswitchrate)
}
tf[[15,18]] <- function(t) {0*t + HIVmort_treated_failing}

tf[[16,17]] <- function(t) {0*t + secondvlfrate}
tf[[16,18]] <- function(t) {0*t + HIVmort_treated}

tf[[17,18]] <- function(t) {0*t + HIVmort_treated_failing}


par <- generateParameterMatrix(tf)
par[[1,18]] <- list(1)



TTT <- matrix(FALSE, nrow=18, ncol=18)
TTT[1,2] <- TRUE




cohort <- simulateCohort(
  transitionFunctions = tf,
  parameters = par,
  cohortSize = sum(newinfected),
  parameterCovariances=FALSE,
  timeToTransition = TTT,
  #baseline = rbind(matrix(as.numeric(c(year,population[[i,2]],population[[i,3]],population[[i,4]],population[[i,6]])), nrow=1, ncol=6),
  #                 matrix(as.numeric(c(year,population[[i,2]],population[[i,3]],population[[i,4]],population[[i,6]])), nrow=1, ncol=6)),
  #baseline = matrix(cbind(infyear,population[which(as.logical(newinfected)),2],population[which(as.logical(newinfected)),3],population[which(as.logical(newinfected)),4],population[which(as.logical(newinfected)),6],population[which(as.logical(newinfected)),7]),nrow=sum(newinfected),ncol=6),
  baseline = cbind(as.numeric(infyear),population[which(as.logical(newinfected)),2],population[which(as.logical(newinfected)),3],population[which(as.logical(newinfected)),4],population[which(as.logical(newinfected)),6],population[which(as.logical(newinfected)),7]),
  #baseline <- baseline_fix,
  #baseline = matrix(cbind(infyear[1:2],population[c(181,1436),2],population[c(181,1436),3],population[c(181,1436),4],population[c(181,1436),6],population[c(181,1436),7]), nrow=2, ncol=6),
  #initialState = 1,
  absorbing = 18,
  to = 100,
  report.every=1
)

HIVprogression <- cbind(newpat_id,as.matrix(cohort@time.to.state)+infyear)
#HIVcurr <- c()
#for (j in 1:nrow(HIVprogression)) {
#  HIVcurr[j] <- max(which(HIVprogression[j,(2:19)]<=infyear[j]))
#}



HIVdata <- rbind(HIVdata, HIVprogression)

HIVdata <- HIVdata[order(HIVdata[,1]),]

