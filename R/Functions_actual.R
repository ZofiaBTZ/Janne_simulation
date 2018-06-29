relocate <- function(current_location, time, age, sex, factors, distancematrix) {
  relocation_probability <- 0.05 # make dependent on factors later
  r1 <- runif(1,0,1)
  if(r1<relocation_probability) {
    destinations <- distancematrix[,current_location]
    destinationvector <- cumsum(destinations)
    r2 <- sum(destinations)*runif(1,0,1)
    finaldest <- max(which(destinationvector<=r2))
    return(finaldest)
  } else {
    return(current_location)
  }
}

relocate_var1 <- function(current_location, time, age, sex, factors, distancematrix, popsize_current) {
  relocation_probability <- 0.05 # make dependent on factors later
  r1 <- runif(popsize_current,0,1)
  destinations <- distancematrix[current_location,]
  destinationmatrix <- t(apply(destinations, 1, cumsum))
  r2 <- destinationmatrix[,ncol(destinationmatrix)]*runif(popsize_current, 0, 1)
  relocates_yes <- r1<rep(relocation_probability, length=popsize_current)
  possibledestination <- t(matrix((1:ncol(distancematrix)), ncol=popsize_current, nrow=ncol(distancematrix)))*(destinationmatrix >= matrix(r2, nrow=popsize_current, ncol=ncol(destinations))) + 10000*(destinationmatrix < matrix(r2, nrow=popsize_current, ncol=ncol(destinations)))
  finaldest <- apply(possibledestination, 1, min)
  rm(destinations, destinationmatrix, possibledestination)
  return(relocates_yes*finaldest + (1-relocates_yes)*current_location)  
}

relocate_var2 <- function(current_location, time, age, sex, factors, distancematrix, popsize_current) { #CURRENTLY VALID
  relocation_probability <- 0.05*(nrow(distancematrix)>1) + 0*(nrow(distancematrix==1)) # Probability of relocation (make dependent on factors later) ALSO MAKE DEPENDANT FOR CHILDREN OF THE LOCATION OF THE MOTHER
  r1 <- runif(popsize_current,0,1) # uniform deviates for each individual
  out <- current_location # vector of locations: start with current location
  for (indiv in 1:popsize_current) {
    relocates_yes <- r1[indiv]<relocation_probability #relocates if r1 below p
    if (relocates_yes==1) {
      destination_probs <- distancematrix[current_location[indiv],] #distances to each other location
      destination_probs <- ifelse(destination_probs==0, 0, 1/destination_probs) #convert all distances to inverse; keep current destination at zero (since it would not be a relocation)
  
    
      out[indiv] <- sample((1:nrow(distancematrix)), 1, prob=destination_probs)
    }
  }
  
  return(out) #output: vector with current (new) location of each individual 
}





reassign <- function(factors, time, age, sex, current_location) {
  return(factors)
}

reassign_var1 <- function(factors, time, age, sex, current_location) { #CURRENTLY VALID
  return(factors) #no reassigning at the moment (will be done later)
}


mortality <- function(age, sex, current_location, factors) {
  malemortality <- c(0.10628225, rep(0.017151265, 5), rep(0.002972164, 5), rep(0.001575587, 5), rep(0.001647737, 5), rep(0.003228007, 5), rep(0.004122388, 5), rep(0.005162363, 5), rep(0.006770601, 5), rep(0.008960421, 5), rep(0.012085331, 5), rep(0.016749357, 5), rep(0.024602643, 5), rep(0.034169593, 5), rep(0.049545703, 5), rep(0.07485386, 5), rep(0.11179437, 5), rep(0.166417473, 5), rep(0.252979748, 5), rep(1000000, 45))
  femalemortality <- c(0.101957632, rep(0.016395388, 5), rep(0.003025214, 5), rep(0.001742667, 5), rep(0.00249521, 5), rep(0.003647901, 5), rep(0.004195402, 5), rep(0.00472919, 5), rep(0.005612752, 5), rep(0.006689759, 5), rep(0.008459057, 5), rep(0.011816835, 5), rep(0.017754043, 5), rep(0.024452891, 5), rep(0.039165862, 5), rep(0.062328318, 5), rep(0.096659067, 5), rep(0.146007583, 5), rep(0.228451656, 5), rep(1000000, 45))
  if (sex==0) {
    mortalityrate <- malemortality[age+1]
  } else{
    mortalityrate <- femalemortality[age+1]
  }
  r1 <- runif(1,0,1)
  stayalive <- ifelse(r1<mortalityrate, 0, 1)
  return(stayalive)
}

mortality_var1 <- function(age, sex, current_location, factors) { #CURRENTLY VALID
  malemortality <- c(0.10628225, rep(0.017151265, 5), rep(0.002972164, 5), rep(0.001575587, 5), rep(0.001647737, 5), rep(0.003228007, 5), rep(0.004122388, 5), rep(0.005162363, 5), rep(0.006770601, 5), rep(0.008960421, 5), rep(0.012085331, 5), rep(0.016749357, 5), rep(0.024602643, 5), rep(0.034169593, 5), rep(0.049545703, 5), rep(0.07485386, 5), rep(0.11179437, 5), rep(0.166417473, 5), rep(0.252979748, 5), rep(1000000, 45))
  femalemortality <- c(0.101957632, rep(0.016395388, 5), rep(0.003025214, 5), rep(0.001742667, 5), rep(0.00249521, 5), rep(0.003647901, 5), rep(0.004195402, 5), rep(0.00472919, 5), rep(0.005612752, 5), rep(0.006689759, 5), rep(0.008459057, 5), rep(0.011816835, 5), rep(0.017754043, 5), rep(0.024452891, 5), rep(0.039165862, 5), rep(0.062328318, 5), rep(0.096659067, 5), rep(0.146007583, 5), rep(0.228451656, 5), rep(1000000, 45))
  mortalityrate <- (sex==0)*malemortality[age+1] + (sex==1)*femalemortality[age+1] #background mortality rate
  r1 <- runif(length(age), 0, 1) #random uniform deviate
  stayalive <- ifelse(r1<mortalityrate, 0, 1) #stay alive if r1 above death rate
  return(stayalive)
}


birth <- function(age, sex, factors, hivstatus) {
  birthprobability <- ifelse((age>=18 & age<35), 0.25, 0.1)
  r1 <- runif(1,0,1)
  birth_yesno <- ifelse(r1<birthprobability, 1, 0)
}

birth_var1 <- function(age, sex) { #CURRENTLY VALID
  birthprobability <- ifelse(age>=16 & age<49 & sex==1, ifelse(age>=18 & age<35, 0.25, 0.1), 0) #Probability to give birth in a year: depends on age, zero if male/too oung/too old
r1 <- runif(popsize_current,0,1) #random uniform deviate
birth_yesno <- ifelse(r1<birthprobability, 1, 0) # Check which women will give birth
return(which(birth_yesno==1))
}



transmission <- function(totalpopulation, distancematrix, currentyear) {
  infected_vect <- totalpopulation[,8]
  for (indexperson in totalpopulation[which(totalpopulation[,8]==0),1]) {
  infected <- 0
  proxfact <- totalpopulation[,c(6,7)]
  peract_acute_mf <- (0.25*20*0.000043+0.75*0.000043)*2.45^4
  peract_chronic_mf <- 0.000043*2.45^4
  peract_treated_mf <- 0.000043
  peract_failing_mf <- 0.000043*2.45^3
  peract_acute_fm <- (0.25*20*0.000022+0.75*0.000022)*2.45^4
  peract_chronic_fm <- 0.000022*2.45^4
  peract_treated_fm <- 0.000022
  peract_failing_fm <- 0.000022*2.45^3
  peract_acute_mm <- (0.25*20*0.00043+0.75*0.00043)*2.45^4
  peract_chronic_mm <- 0.00043*2.45^4
  peract_treated_mm <- 0.00043
  peract_failing_mm <- 0.00043*2.45^3
  regular_acts <- 100*(totalpopulation[indexperson,2]>16)
  casual_acts <- ifelse(totalpopulation[indexperson,6]==0, rbinom(1,10,0.1), rbinom(1,50, 0.25))
  weights_partnertype_regular_het <- as.vector((as.numeric(totalpopulation[,3])!=as.numeric(totalpopulation[indexperson,3]))*as.numeric(totalpopulation[,2]>16))
  weights_partnertype_casual_het <- as.vector((as.numeric(totalpopulation[,3])!=as.numeric(totalpopulation[indexperson,3]))*as.numeric(totalpopulation[,2]>16))
  weights_partnertype_regular_msm <- as.vector((as.numeric(totalpopulation[indexperson,3])==0)*(as.numeric(totalpopulation[,3])==0)*(totalpopulation[,1]!=totalpopulation[indexperson,1])*as.numeric(totalpopulation[,2]>16))
  weights_partnertype_casual_msm <- as.vector((as.numeric(totalpopulation[indexperson,3])==0)*(as.numeric(totalpopulation[,3])==0)*(totalpopulation[,1]!=totalpopulation[indexperson,1])*as.numeric(totalpopulation[,2]>16))
  weights_partnertype_regular <- weights_partnertype_regular_het
  weights_partnertype_casual <- weights_partnertype_casual_het
  weights_geographic_regular <- as.vector((distancematrix[as.numeric(totalpopulation[,4]),as.numeric(totalpopulation[indexperson,4])]==0)*1+(distancematrix[as.numeric(totalpopulation[,4]),as.numeric(totalpopulation[indexperson,4])]>0)*0)
  weights_geographic_casual <- as.vector((distancematrix[as.numeric(totalpopulation[,4]),as.numeric(totalpopulation[indexperson,4])]==0)*0.5+(distancematrix[as.numeric(totalpopulation[,4]),as.numeric(totalpopulation[indexperson,4])]>0.25)*0.1)
  weights_riskbehav_regular <- as.vector((proxfact[,1]==as.numeric(totalpopulation[indexperson,6]))*1+(proxfact[,1]!=as.numeric(totalpopulation[indexperson,6]))*1)
  weights_riskbehav_casual <- as.vector((proxfact[,1]==as.numeric(totalpopulation[indexperson,6]))*1+(proxfact[,1]!=as.numeric(totalpopulation[indexperson,6]))*1)
  weights_geographic_regular_norm <- weights_geographic_regular/sum(weights_geographic_regular)
  weights_geographic_casual_norm <- weights_geographic_casual/sum(weights_geographic_casual)
  weights_riskbehav_regular_norm <- weights_riskbehav_regular/sum(weights_riskbehav_regular)
  weights_riskbehav_casual_norm <- weights_riskbehav_casual/sum(weights_riskbehav_casual)
  totalweight_regular <- cumsum(weights_partnertype_regular*(weights_geographic_regular_norm+weights_riskbehav_regular_norm)/2)
  totalweight_casual <- cumsum(weights_partnertype_casual*(weights_geographic_casual_norm+weights_riskbehav_casual_norm)/2)
  r1 <- runif(1,0,1)
  r1 <- r1*max(totalweight_regular)
  regular_partner <- max(which(totalweight_regular<r1))
  if(is.finite(regular_partner) && as.logical(totalpopulation[regular_partner,8])) {
    if (totalpopulation[regular_partner,3]==0) {
      if (totalpopulation[indexperson,3]==1) {
        if(totalpopulation[regular_partner,9]<year+1 & totalpopulation[regular_partner,10]<4) {
          infness_regular <- peract_acute_mf
        } else {
          if(totalpopulation[regular_partner,10] %in% c(4,6,9,11,14,16)){
            infness_regular <- peract_treated_mf
          } else {
            if (totalpopulation[regular_partner,10] %in% c(5,7,10,12,15,17)) {
              infness_regular <- peract_failing_mf
            } else {
              infness_regular <- peract_chronic_mf
            }
          }
        }
      } else {
        if(totalpopulation[regular_partner,9]<year+1 & totalpopulation[regular_partner,10]<4) {
          infness_regular <- peract_acute_mm
        } else {
          if(totalpopulation[regular_partner,10] %in% c(4,6,9,11,14,16)){
            infness_regular <- peract_treated_mm
          } else {
            if (totalpopulation[regular_partner,10] %in% c(5,7,10,12,15,17)) {
              infness_regular <- peract_failing_mm
            } else {
              infness_regular <- peract_chronic_mm
            }
          }
        }
      }
    } else {
      if(totalpopulation[regular_partner,9]<year+1 & totalpopulation[regular_partner,10]<4) {
        infness_regular <- peract_acute_fm
      } else {
        if(totalpopulation[regular_partner,10] %in% c(4,6,9,11,14,16)){
          infness_regular <- peract_treated_fm
        } else {
          if (totalpopulation[regular_partner,10] %in% c(5,7,10,12,15,17)) {
            infness_regular <- peract_failing_fm
          } else {
            infness_regular <- peract_chronic_fm
          }
        }
      }
    }
    r2 <- runif(1,0,1)
    infected <- r2<1-(1-infness_regular)^regular_acts
  }
  if (infected==0) {
    infness_casual <- 0
    for (act in (1:casual_acts)) {
      rx <- runif(1,0,1)
      rx <- rx*max(totalweight_casual)
      casual_partner <- max(which(totalweight_casual<rx))
      if (is.finite(casual_partner) && as.logical(totalpopulation[casual_partner,8])) {
        if (totalpopulation[casual_partner,3]==0) {
          if (totalpopulation[indexperson,3]==1) {
            if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
              infness_casual <- peract_acute_mf
            } else {
              if(totalpopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
                infness_casual <- peract_treated_mf
              } else {
                if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                  infness_casual <- peract_failing_mf
                } else {
                  infness_casual <- peract_chronic_mf
                }
              }
            }
          } else {
            if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
              infness_casual <- peract_acute_mm
            } else {
              if(totalpopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
                infness_casual <- peract_treated_mm
              } else {
                if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                  infness_casual <- peract_failing_mm
                } else {
                  infness_casual <- peract_chronic_mm
                }
              }
            }
          }
        } else {
          if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
            infness_casual <- peract_acute_fm
          } else {
            if(totalpopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
              infness_casual <- peract_treated_fm
            } else {
              if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                infness_casual <- peract_failing_fm
              } else {
                infness_casual <- peract_chronic_fm
              }
            }
          }
        }
      }
      rxx <- runif(1,0,1)
      infected <- rxx<infness_casual
      if (infected) {
        break
      }
    }
  }

  infected_vect[indexperson] <- infected*(totalpopulation[indexperson,2]>16)
  }
  return(infected_vect)
}



transmission_var1 <- function(totalpopulation, distancematrix, currentyear) {
 
  infected <- population[,8]
  proxfact <- totalpopulation[,(6:7)]
  #for (i in 1:ncol(proxfact)) {
  #  proxfact[,i] <- unlist(totalpopulation[,6])[seq(1,length(unlist(totalpopulation[,6])),length(unlist(totalpopulation[,6][1])))+i-1]
  #}
  peract_acute_mf <- (0.25*20*0.000043+0.75*0.000043)*2.45^4
  peract_chronic_mf <- 0.000043*2.45^4
  peract_treated_mf <- 0.000043
  peract_failing_mf <- 0.000043*2.45^3
  peract_acute_fm <- (0.25*20*0.000022+0.75*0.000022)*2.45^4
  peract_chronic_fm <- 0.000022*2.45^4
  peract_treated_fm <- 0.000022
  peract_failing_fm <- 0.000022*2.45^3
  peract_acute_mm <- (0.25*20*0.00043+0.75*0.00043)*2.45^4
  peract_chronic_mm <- 0.00043*2.45^4
  peract_treated_mm <- 0.00043
  peract_failing_mm <- 0.00043*2.45^3
  regular_acts <- 100
  casual_acts <- ifelse(totalpopulation[,6]==0, rbinom(popsize_current,10,0.1), rbinom(popsize_current,50, 0.25))
  weights_partnertype_regular_het <- matrix(as.numeric(kronecker(totalpopulation[,3], totalpopulation[,3], FUN="!=")), nrow=popsize_current, ncol=popsize_current)
  weights_partnertype_casual_het <- matrix(as.numeric(kronecker(totalpopulation[,3], totalpopulation[,3], FUN="!=")), nrow=popsize_current, ncol=popsize_current)
  weights_partnertype_regular_msm <- (totalpopulation[,3]==0)*matrix(as.numeric(kronecker(totalpopulation[,3], totalpopulation[,3], FUN="!=")), nrow=popsize_current, ncol=popsize_current)
  weights_partnertype_casual_msm <- (totalpopulation[,3]==0)*matrix(as.numeric(kronecker(totalpopulation[,3], totalpopulation[,3], FUN="!=")), nrow=popsize_current, ncol=popsize_current)
  weights_partnertype_regular <- weights_partnertype_regular_het
  weights_partnertype_casual <- weights_partnertype_casual_het
  distances_partners <- distancematrix[totalpopulation[,4], totalpopulation[,4]]
  weights_geographic_regular <- (distancematrix==0)
  weights_geographic_casual <- (distancematrix<0.25)
  weights_riskbehav_regular <- matrix(as.numeric(kronecker(proxfact[,1], proxfact[,1], FUN="!=")), nrow=popsize_current, ncol=popsize_current)
  weights_riskbehav_casual <- matrix(as.numeric(kronecker(proxfact[,1], proxfact[,1], FUN="!=")), nrow=popsize_current, ncol=popsize_current)
  weights_geographic_regular_norm <- weights_geographic_regular/apply(weights_geographic_regular, 1, sum)
  weights_geographic_casual_norm <- weights_geographic_casual/apply(weights_geographic_casual, 1, sum)
  weights_riskbehav_regular_norm <- weights_riskbehav_regular/apply(weights_riskbehav_regular, 1, sum)
  weights_riskbehav_casual_norm <- weights_riskbehav_casual/apply(weights_riskbehav_casual, 1, sum)
  totalweight_regular <- apply(weights_partnertype_regular*(weights_geographic_regular_norm+weights_riskbehav_regular_norm)/2, 1, cumsum)
  totalweight_casual <- apply(weights_partnertype_casual*(weights_geographic_casual_norm+weights_riskbehav_casual_norm)/2, 1, cumsum)
  r1 <- runif(popsize_current,0,1)
  r1 <- r1*apply(totalweight_regular, 1, max)
  r1 <- matrix(r1, nrow=popsize_current, ncol=1)
  regular_temp <- totalweight_regular*(totalweight_regular<r1)
  regular_partner <- apply(regular_temp, 1, which.max)
  regular_infness <- (totalpopulation[,3]==0 & totalpopulation[regularpartner,3]==1)*
    (ifelse(totalpopulation[regularpartner,9]<year+1 & totalpopulation[regularpartner,10]<4,peract_acute_mf, 
            (ifelse(totalpopulation[regularpartner,10] %in% c(4,6,9,11,14,16), peract_treated_mf, 
                    (ifelse(totalpopulation[regularpartner,10] %in% c(5,7,10,12,15,17), peract_failing_mf, peract_chronic_mf)))))) +
    (totalpopulation[,3]==1 & totalpopulation[regularpartner,3]==0)*
    (ifelse(totalpopulation[regularpartner,9]<year+1 & totalpopulation[regularpartner,10]<4,peract_acute_fm, 
            (ifelse(totalpopulation[regularpartner,10] %in% c(4,6,9,11,14,16), peract_treated_fm, 
                    (ifelse(totalpopulation[regularpartner,10] %in% c(5,7,10,12,15,17), peract_failing_fm, peract_chronic_fm)))))) +
    (totalpopulation[,3]==0 & totalpopulation[regularpartner,3]==0)*
    (ifelse(totalpopulation[regularpartner,9]<year+1 & totalpopulation[regularpartner,10]<4,peract_acute_mm, 
            (ifelse(totalpopulation[regularpartner,10] %in% c(4,6,9,11,14,16), peract_treated_mm, 
                    (ifelse(totalpopulation[regularpartner,10] %in% c(5,7,10,12,15,17), peract_failing_mm, peract_chronic_mm))))))
  r2 <- runif(popsize_current, 0, 1)  
  infected <- r2 < 1-(1-regular_infness)^regular_acts
  
  for (indexperson in which(infected==0)) {
    
      infness_casual <- 0
      for (act in (1:casual_acts[indexperson])) {
        rx <- runif(1,0,1)
        rx <- rx*max(totalweight_casual[indexperson,])
        casual_partner <- max(which(totalweight_casual[indexperson,]<rx))
        if (is.finite(casual_partner) && as.logical(totalpopulation[casual_partner,8])) {
          if (totalpopulation[casual_partner,3]==0) {
            if (totalpopulation[indexperson,3]==1) {
              if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
                infness_casual <- peract_acute_mf
              } else {
                if(totalopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
                  infness_casual <- peract_treated_mf
                } else {
                  if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                    infness_casual <- peract_failing_mf
                  } else {
                    infness_casual <- peract_chronic_mf
                  }
                }
              }
            } else {
              if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
                infness_casual <- peract_acute_mm
              } else {
                if(totalopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
                  infness_casual <- peract_treated_mm
                } else {
                  if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                    infness_casual <- peract_failing_mm
                  } else {
                    infness_casual <- peract_chronic_mm
                  }
                }
              }
            }
          } else {
            if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
              infness_casual <- peract_acute_fm
            } else {
              if(totalopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
                infness_casual <- peract_treated_fm
              } else {
                if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                  infness_casual <- peract_failing_fm
                } else {
                  infness_casual <- peract_chronic_fm
                }
              }
            }
          }
        }
        rxx <- runif(1,0,1)
        infected <- rxx<infness_casual
        if (infected[indexperson]==1) {
          break
        }
      }
    }

  return(infected)
}  


transmission_var2 <- function(totalpopulation, distancematrix, currentyear) {
  infected <- totalpopulation[,8]
  infected_temp1 <- (totalpopulation[,13]>0)*totalpopulation[,13]+(totalpopulation[,13]==0)*1
  infected_temp2 <- totalpopulation[infected_temp1,8]
  partner_infected <- infected_temp2*(totalpopulation[,13]>0)+(totalpopulation[,13]==0)*0
  peract_acute_mf <- (0.25*20*0.000043+0.75*0.000043)*2.45^4
  peract_chronic_mf <- 0.000043*2.45^4
  peract_treated_mf <- 0.000043
  peract_failing_mf <- 0.000043*2.45^3
  peract_acute_fm <- (0.25*20*0.000022+0.75*0.000022)*2.45^4
  peract_chronic_fm <- 0.000022*2.45^4
  peract_treated_fm <- 0.000022
  peract_failing_fm <- 0.000022*2.45^3
  peract_acute_mm <- (0.25*20*0.00043+0.75*0.00043)*2.45^4
  peract_chronic_mm <- 0.00043*2.45^4
  peract_treated_mm <- 0.00043
  peract_failing_mm <- 0.00043*2.45^3
  regular_acts <- 100*(totalpopulation[,2]>16)
  infness_regular <- ifelse(partner_infected,
                            ifelse(totalpopulation[,3]==1,
                                   ifelse((totalpopulation[infected_temp1,9]<currentyear+1 & totalpopulation[infected_temp1,10]<4),
                                          peract_acute_mf,
                                          ifelse((totalpopulation[infected_temp1,10]%in% c(4,6,9,11,14,16)),
                                                 peract_treated_mf,
                                                 ifelse((totalpopulation[infected_temp1,10]%in% c(5,7,10,12,15,17)),
                                                        peract_failing_mf,peract_chronic_mf))),
                                   ifelse(totalpopulation[infected_temp1,3]==0,
                                          ifelse((totalpopulation[infected_temp1,9]<currentyear+1 & totalpopulation[infected_temp1,10]<4),
                                                 peract_acute_mm,
                                                 ifelse((totalpopulation[infected_temp1,10]%in% c(4,6,9,11,14,16)),
                                                        peract_treated_mm,
                                                        ifelse((totalpopulation[infected_temp1,10]%in% c(5,7,10,12,15,17)),
                                                               peract_failing_mf,peract_chronic_mm))),
                                          ifelse((totalpopulation[infected_temp1,9]<currentyear+1 & totalpopulation[infected_temp1,10]<4),
                                                 peract_acute_fm,
                                                 ifelse((totalpopulation[infected_temp1,10]%in% c(4,6,9,11,14,16)),
                                                        peract_treated_fm,
                                                        ifelse((totalpopulation[infected_temp1,10]%in% c(5,7,10,12,15,17)),
                                                               peract_failing_mf,peract_chronic_fm))))
                                   ),0)
  r2 <- runif(nrow(totalpopulation), 0,1)
  infected <- (infected==0)*(r2<1-(1-infness_regular)^regular_acts) + infected
  proxfact <- totalpopulation[,(6:7)]
  
  casual_acts <- (totalpopulation[,6]==0)*rbinom(nrow(totalpopulation),365,0.0001) + (totalpopulation[,6]==1)*rbinom(nrow(totalpopulation),365, 0.1)
  casual_acts_het_male <- casual_acts*(totalpopulation[,3]==0 & totalpopulation[,7]==0)
  casual_acts_het_female <- casual_acts*(totalpopulation[,3]==1)
  actdiff <- sum(casual_acts_het_male)-sum(casual_acts_het_female)
  
  casual_acts_het_female <- round(casual_acts_het_female*(1+(actdiff/sum(casual_acts_het_female))))
  actdiff <- sum(casual_acts_het_male)-sum(casual_acts_het_female)
  if (actdiff>0) {
    remove_acts <- sample(which(casual_acts_het_male>0), size=abs(actdiff))
    casual_acts_het_male <- casual_acts_het_male-((1:length(casual_acts_het_male)) %in% remove_acts)
  }
  if (actdiff<0) {
    remove_acts <- sample(which(casual_acts_het_female>0), size=abs(actdiff))
    casual_acts_het_female <- casual_acts_het_female-((1:length(casual_acts_het_female)) %in% remove_acts)
  }
   
  
  
  casual_acts_msm <- casual_acts*(totalpopulation[,3]==0 & totalpopulation[,7]==1)
  if ((sum(casual_acts_msm) %% 2)==1) {
    remove_act <- sample(which(casual_acts_msm>0), size==1)
    casual_acts_msm <- casual_acts_msm-((1:length(casual_acts_msm))==remove_act)
  }
  
  
  casual_acts_het_female_current <- casual_acts_het_female
  for (indexperson in which(casual_acts_het_male>0)) {
    weight_geographic_casual <- casual_acts_het_female_current*((distancematrix[totalpopulation[,4],totalpopulation[indexperson,4]]==0)*0.5+(distancematrix[totalpopulation[,4],totalpopulation[indexperson,4]]>0.25)*0.1)
    weight_geographic_casual_norm <- weight_geographic_casual/sum(weight_geographic_casual)
    partnervect <- sample(totalpopulation[,1], size=casual_acts_het_male[indexperson], replace=TRUE, prob=weight_geographic_casual_norm)
    if (infected[indexperson]==0 & sum(infected[partnervect])>0) {
       infectprob <- ifelse(infected[partnervect]==1, ifelse((totalpopulation[partnervect,9]<currentyear+1 & totalpopulation[partnervect,10]<4),
                           peract_acute_fm,
                           ifelse((totalpopulation[partnervect,10]%in% c(4,6,9,11,14,16)),
                                  peract_treated_fm,
                                  ifelse((totalpopulation[partnervect,10]%in% c(5,7,10,12,15,17)),
                                         peract_failing_fm,peract_chronic_fm))), 0)
       rcas <- runif(length(partnervect),0,1)
       infected[indexperson] <- 1*(sum(rcas < infectprob)>0)
    } 
    if (infected[indexperson]==1) {
      infectprob <- ifelse((totalpopulation[indexperson,9]<currentyear+1 & totalpopulation[indexperson,10]<4),
                                                            peract_acute_mf,
                                                            ifelse((totalpopulation[indexperson,10]%in% c(4,6,9,11,14,16)),
                                                                   peract_treated_mf,
                                                                   ifelse((totalpopulation[indexperson,10]%in% c(5,7,10,12,15,17)),
                                                                          peract_failing_mf,peract_chronic_mf)))
      rcas <- runif(length(partnervect),0,1)
      infected[partnervect] <- (infected[partnervect]==0)*(rcas < infectprob) + infected[partnervect]
      
    }
    partnerlist <- as.data.frame(table(partnervect))
    partnertimes <- rep(0, length=nrow(totalpopulation))
    partnertimes[as.numeric(as.character(partnerlist[,1]))] <- as.numeric(as.character(partnerlist[,2]))
    casual_acts_het_female_current <- casual_acts_het_female_current - partnertimes
    print(indexperson)
  }

  
  
  
  
  for (indexperson in which(infected==0)){

      infness_casual <- 0
      casual_acts <- ifelse(totalpopulation[indexperson,6]==0, rbinom(1,10,0.1), rbinom(1,50, 0.25))
      weights_partnertype_casual_het <- as.vector((as.numeric(totalpopulation[,3])!=as.numeric(totalpopulation[indexperson,3]))*as.numeric(totalpopulation[,2]>16))
      weights_partnertype_casual_msm <- as.vector((as.numeric(totalpopulation[indexperson,3])==0)*(as.numeric(totalpopulation[,3])==0)*(totalpopulation[,1]!=totalpopulation[indexperson,1])*as.numeric(totalpopulation[,2]>16))
      weights_partnertype_casual <- weights_partnertype_casual_het
      weights_geographic_casual <- as.vector((distancematrix[as.numeric(totalpopulation[,4]),as.numeric(totalpopulation[indexperson,4])]==0)*0.5+(distancematrix[as.numeric(totalpopulation[,4]),as.numeric(totalpopulation[indexperson,4])]>0.25)*0.1)
      weights_riskbehav_casual <- as.vector((proxfact[,1]==as.numeric(totalpopulation[indexperson,6]))*1+(proxfact[,1]!=as.numeric(totalpopulation[indexperson,6]))*1)
      weights_geographic_casual_norm <- weights_geographic_casual/sum(weights_geographic_casual)
      weights_riskbehav_casual_norm <- weights_riskbehav_casual/sum(weights_riskbehav_casual)
      totalweight_casual <- cumsum(weights_partnertype_casual*(weights_geographic_casual_norm+weights_riskbehav_casual_norm)/2)
      
      act <- 1
      while (act<=casual_acts & infected==0) {
        rx <- runif(1,0,1)
        rx <- rx*max(totalweight_casual)
        casual_partner <- max(which(totalweight_casual<rx))
        if (is.finite(casual_partner) && as.logical(totalpopulation[casual_partner,8])) {
          if (totalpopulation[casual_partner,3]==0) {
            if (totalpopulation[indexperson,3]==1) {
              if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
                infness_casual <- peract_acute_mf
              } else {
                if(totalpopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
                  infness_casual <- peract_treated_mf
                } else {
                  if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                    infness_casual <- peract_failing_mf
                  } else {
                    infness_casual <- peract_chronic_mf
                  }
                }
              }
            } else {
              if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
                infness_casual <- peract_acute_mm
              } else {
                if(totalpopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
                  infness_casual <- peract_treated_mm
                } else {
                  if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                    infness_casual <- peract_failing_mm
                  } else {
                    infness_casual <- peract_chronic_mm
                  }
                }
              }
            }
          } else {
            if(totalpopulation[casual_partner,9]<year+1 & totalpopulation[casual_partner,10]<4) {
              infness_casual <- peract_acute_fm
            } else {
              if(totalpopulation[casual_partner,10] %in% c(4,6,9,11,14,16)){
                infness_casual <- peract_treated_fm
              } else {
                if (totalpopulation[casual_partner,10] %in% c(5,7,10,12,15,17)) {
                  infness_casual <- peract_failing_fm
                } else {
                  infness_casual <- peract_chronic_fm
                }
              }
            }
          }
        }
        rxx <- runif(1,0,1)
        infected <- rxx<infness_casual
      act <- act+1
      }
    print(indexperson)
    
    infected[indexperson] <- infected*(totalpopulation[indexperson,2]>16)
  }
  return(infected)
}




  
  
mtct_var1 <- function(ninfants, totalpopulation) { #CURRENTLY VALID
  mtctprob <- rep(0, length=ninfants)  
  nadults <- nrow(totalpopulation)-ninfants #Number of adults
  mtct_p_treated <- 0.05 # Probability of MTCT if mother treated with ART
  mtct_p_prophyl <- 0.20 # Probability of MTCT if mother gets prophylaxis
  mtct_p_untreated <- 0.35 # Probability of MTCT if mother gets no ART nor any prophylaxis
  pmtctcoverage <- 0.8 # Coverage of PMTCT #MAKE LATER DEPENDANT ON LOCATION
  for (i in 1:ninfants) {
    if (totalpopulation[totalpopulation[(nadults+1),12],8]==1) { #If mother of the infant infected
      if (totalpopulation[totalpopulation[(nadults+1),12],10] %in% c(4,6,9,11,14,16)) { # If the mother on successful ART
        mtctprob[i] <- mtct_p_treated #probability of MTCT: as with ART
      } else { #If the mother not on successful ART
        if (totalpopulation[totalpopulation[(nadults+1),12],10] %in% c(5,7,8,10,12,13,15,17) | year<2003) { # If the year is before 2003, or if the mother is on failing treatment or dropped out
          mtctprob[i] <- mtct_p_untreated # probability of MTCT: as with no ART nor prophylaxis
        } else { #If year is after 2003 and the mother has not started ART
          mtctprob[i] <- ifelse(runif(1,0,1)<pmtctcoverage, mtct_p_prophyl, mtct_p_untreated) # probability of MTCT: depends on PMTCT coverage
        }
      }
    }
  }
  r2 <- runif(ninfants, 0, 1) # random uniform deviate
  mtct_yesno <- ifelse(r2<mtctprob, 1, 0) # check which infants will be infected
  return(mtct_yesno)
}
    

partnerselect <- function(totalpopulation, currentyear) { #CURRENTLY VALID
has_partner <- (totalpopulation[,13]>0) # Check which individuals already have partners
partners <- totalpopulation[,13] #IDs of partners
msm_regpartn <- 0 #male regular partenr of male: currently probability 0, will be added later
wants_partner <- 0.90 #Probability that seeks partner if does not currently have one
if (currentyear==1975) {
  partnerchange <- 1 #in the first year of simulation, nobody has a partner yet
  
} else {
  partnerchange <- ifelse(totalpopulation[,2]<40, 0.05, 0.01) #in subsequent years, probability that a partnership will end 
}
r0 <- runif(nrow(totalpopulation),0,1) #random uniform deviate
changes_partners <- (r0<partnerchange)*(totalpopulation[,2]>16) #check if will end partnership
has_partner[which(changes_partners==1)] <- 0 # update: those whose partnership ends will have no partner
has_partner[partners[which(changes_partners==1)]] <- 0 # update: the partners of those whose partnership ends will also have no partner
partners[which(has_partner==0)] <- 0 # update: those whose partnership ends will have no partner


for (location in 1:max(totalpopulation[,4])) { #go through each location
  indexpool <- totalpopulation[which(has_partner==0 & totalpopulation[,2]>16 & totalpopulation[,4]==location & totalpopulation[,3]==0),] #all adult males in the location who have no partner 
  partnerpool <- totalpopulation[which(has_partner==0 & totalpopulation[,2]>16 & totalpopulation[,4]==location & totalpopulation[,3]==1),] #all adult females in the location who have no partner
  rm <- runif(nrow(indexpool), 0, 1) #random uniform deviate
  rf <- runif(nrow(partnerpool), 0, 1) #random uniform deviate
  indexpool <- indexpool[which(rm<wants_partner),] # remove from the pool of males looking for partners those who don't want a partner 
  partnerpool <- partnerpool[which(rf<wants_partner),] # remove from the pool of females looking for partners those who don't want a partner
  match <- c(nrow(indexpool),nrow(partnerpool)) # number of males and females seeking partners in current location
  
  
  if (match[1]<=match[2]) { # if equal number of males and females, or more females than males
    partner_temp <- sample(partnerpool[,1], match[1]) # sample for each male one female
    partner_temp <- cbind(indexpool[,1], partner_temp) # pairs of male-female partners
  } else { # if more males than females
    partner_temp <- sample(c(partnerpool[,1], rep(0, length=match[1]-match[2])), match[1]) # sample for each male a female (as many as there are) or leave without partner
    partner_temp <- cbind(indexpool[,1], partner_temp) #pairs of male-female partners
  }
  
  partners[indexpool[,1]] <- partner_temp[,2] # assign the males their new partner
  partners[partner_temp[(which(partner_temp[,2]>0)),2]] <- partner_temp[(which(partner_temp[,2]>0)),1] #assign the females their new partner
  print(location)
}
return(partners)
}




transmission_var3 <- function(totalpopulation, distancematrix, currentyear) { #CURRENTLY VALID
  peract_acute_mf <- (0.25*20*0.000043+0.75*0.000043)*2.45^4 # per-act transmission probabilities according to act type (m to f, f to m, m to m) and infected partner's HIV stage
  peract_chronic_mf <- 0.000043*2.45^4
  peract_treated_mf <- 0.000043
  peract_failing_mf <- 0.000043*2.45^3
  peract_acute_fm <- (0.25*20*0.000022+0.75*0.000022)*2.45^4
  peract_chronic_fm <- 0.000022*2.45^4
  peract_treated_fm <- 0.000022
  peract_failing_fm <- 0.000022*2.45^3
  peract_acute_mm <- (0.25*20*0.00043+0.75*0.00043)*2.45^4
  peract_chronic_mm <- 0.00043*2.45^4
  peract_treated_mm <- 0.00043
  peract_failing_mm <- 0.00043*2.45^3
  
  infected <- totalpopulation[,8] # infected currently
  
  regular_infness <- rep(0, length=nrow(totalpopulation)) # vector of exposure to HIV in regular partenrship
  for (i in 1:nrow(totalpopulation)) {
    if (totalpopulation[i,8]==0 && totalpopulation[i,13]>0 && totalpopulation[totalpopulation[i,13],8]==1) { # if index person not infected but has a partner who is infected
      regular_infness[i] <- (totalpopulation[i,3]==0 & totalpopulation[totalpopulation[i,13],3]==1 & totalpopulation[totalpopulation[i,13],10]==1)*peract_acute_fm +
        (totalpopulation[i,3]==1 & totalpopulation[totalpopulation[i,13],3]==0 & totalpopulation[totalpopulation[i,13],10]==1)*peract_acute_mf +
        (totalpopulation[i,3]==0 & totalpopulation[totalpopulation[i,13],3]==0 & totalpopulation[totalpopulation[i,13],10]==1)*peract_acute_mm +
        (totalpopulation[i,3]==0 & totalpopulation[totalpopulation[i,13],3]==1 & (totalpopulation[totalpopulation[i,13],10]%in%c(2,3,8,13)))*peract_chronic_fm +
        (totalpopulation[i,3]==1 & totalpopulation[totalpopulation[i,13],3]==0 & (totalpopulation[totalpopulation[i,13],10]%in%c(2,3,8,13)))*peract_chronic_mf +
        (totalpopulation[i,3]==0 & totalpopulation[totalpopulation[i,13],3]==0 & (totalpopulation[totalpopulation[i,13],10]%in%c(2,3,8,13)))*peract_chronic_mm +
        (totalpopulation[i,3]==0 & totalpopulation[totalpopulation[i,13],3]==1 & (totalpopulation[totalpopulation[i,13],10]%in%c(4,6,9,11,14,16)))*peract_treated_fm +
        (totalpopulation[i,3]==1 & totalpopulation[totalpopulation[i,13],3]==0 & (totalpopulation[totalpopulation[i,13],10]%in%c(4,6,9,11,14,16)))*peract_treated_mf +
        (totalpopulation[i,3]==0 & totalpopulation[totalpopulation[i,13],3]==0 & (totalpopulation[totalpopulation[i,13],10]%in%c(4,6,9,11,14,16)))*peract_treated_mm +
        (totalpopulation[i,3]==0 & totalpopulation[totalpopulation[i,13],3]==1 & (totalpopulation[totalpopulation[i,13],10]%in%c(5,7,10,12,15,17)))*peract_treated_fm + #CHANGE TRO FAILINTG
        (totalpopulation[i,3]==1 & totalpopulation[totalpopulation[i,13],3]==0 & (totalpopulation[totalpopulation[i,13],10]%in%c(5,7,10,12,15,17)))*peract_treated_mf +
        (totalpopulation[i,3]==0 & totalpopulation[totalpopulation[i,13],3]==0 & (totalpopulation[totalpopulation[i,13],10]%in%c(5,7,10,12,15,17)))*peract_treated_mm
    } #calcualte per-act trasnmission probability based on the infected partner's HIV status and sex
  }
  ## ADD CIRCUMCISION ETC
  regular_acts <- (totalpopulation[,13]>0)*round(exp(rnorm(nrow(totalpopulation), 3, 1))) # number of unprotected sex acts within the partnership
  regular_transmission_prob <- 1-(1-regular_infness)^regular_acts # overall transmission probability within one year within the reular partnership
  
  newinfected <- (regular_transmission_prob > runif(nrow(totalpopulation), 0, 1)) # who gets newly infected within regular partnership (random uniform deviate compared to probability)
  
  infected <- pmax(infected, as.numeric(newinfected)) # update the "infected" vector
  
  riskbehaviour_ratio <- sum(totalpopulation[,2]>16 & totalpopulation[,3]==0 & totalpopulation[,6]==1)/sum(totalpopulation[,2]>16 & totalpopulation[,3]==1 & totalpopulation[,6]==1) # ratio of high-risk behaviour men vs women
  
  
  casual_acts <- (totalpopulation[,6]==0)*(rbinom(nrow(totalpopulation), 52, 0.01)) + (totalpopulation[,3]==0)*(totalpopulation[,6]==1)*exp(rnorm(nrow(totalpopulation), 365, 0.1))*(totalpopulation[,3]==1)*(totalpopulation[,6]==1)*exp(rnorm(nrow(totalpopulation), 365, 0.1))/riskbehaviour_ratio # number of casual sex acts for each individual within the year
  casual_acts <- casual_acts*(totalpopulation[,2]>16) # restrict to aged >16
  
  casual_acts <- casual_acts*(totalpopulation[,3]==0) + casual_acts*(totalpopulation[,3]==1)*sum(casual_acts*totalpopulation[,3]==0)/sum(casual_acts*totalpopulation[,3]==1) # normalize do that men and women have same amount of acts
  casual_acts <- round(casual_acts) #round if non-integer value
  if (max(distancematrix)>0) { # if there exist more than one distinct locations
    distancematrix[which(distancematrix==0)] <- 0.5*min(distancematrix[which(distancematrix>0)]) # correct the distance matrix to avoid dividing by zero (i.e. replace the zero distance form a location to itself by a small number)
  } else { #otherwise (only one location)
    distancematrix[which(distancematrix==0)] <- 1 # change 0 to 1
  }
  
  
  acutehiv_partners <- numeric(length=nrow(totalpopulation))
  chronichiv_partners <- numeric(length=nrow(totalpopulation))
  treatedhiv_partners <- numeric(length=nrow(totalpopulation))
  failinghiv_partners <- numeric(length=nrow(totalpopulation))
  
  for (location in 1:nrow(distancematrix)) { #go through each location
    male_potential <- sum(casual_acts*(totalpopulation[,3]==0)/distancematrix[location,(totalpopulation[,4])]) # calculate total number of casual acts done by all males, weighting according to their distance to the current location
    female_potential <- sum(casual_acts*(totalpopulation[,3]==1)/distancematrix[location,(totalpopulation[,4])])  # calculate total number of casual acts done by all females, weighting according to their distance to the current location
    acutehiv_male_potential <- sum(casual_acts*(totalpopulation[,3]==0)*(totalpopulation[,10]==1)/distancematrix[location,(totalpopulation[,4])]) # number of acts by women with acute HIV, weighted by geographical distance
    acutehiv_female_potential <- sum(casual_acts*(totalpopulation[,3]==1)*(totalpopulation[,10]==1)/distancematrix[location,(totalpopulation[,4])]) # number of acts by women with acute HIV, weighted by geographical distance
    chronichiv_male_potential <- sum(casual_acts*(totalpopulation[,3]==0)*(totalpopulation[,10] %in% c(2,3,8,13))/distancematrix[location,(totalpopulation[,4])]) # number of acts by women with chronic HIV, weighted by geographical distance
    chronichiv_female_potential <- sum(casual_acts*(totalpopulation[,3]==1)*(totalpopulation[,10] %in% c(2,3,8,13))/distancematrix[location,(totalpopulation[,4])]) # number of acts by women with chronic HIV, weighted by geographical distance
    treatedhiv_male_potential <- sum(casual_acts*(totalpopulation[,3]==0)*(totalpopulation[,10] %in% c(4,6,9,11,14,16))/distancematrix[location,(totalpopulation[,4])]) # number of acts by women with treated HIV, weighted by geographical distance
    treatedhiv_female_potential <- sum(casual_acts*(totalpopulation[,3]==1)*(totalpopulation[,10] %in% c(4,6,9,11,14,16))/distancematrix[location,(totalpopulation[,4])]) # number of acts by women with treated HIV, weighted by geographical distance
    failinghiv_male_potential <- sum(casual_acts*(totalpopulation[,3]==0)*(totalpopulation[,10] %in% c(5,7,10,12,15,17))/distancematrix[location,(totalpopulation[,4])]) # number of acts by women with failing treated HIV, weighted by geographical distance
    failinghiv_female_potential <- sum(casual_acts*(totalpopulation[,3]==1)*(totalpopulation[,10] %in% c(5,7,10,12,15,17))/distancematrix[location,(totalpopulation[,4])]) # number of acts by women with failing treated HIV, weighted by geographical distance
    acutehiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)] <- rbinom(sum(totalpopulation[,3]==0 & totalpopulation[,4]==location),casual_acts[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)],acutehiv_female_potential/female_potential)
    acutehiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)] <- rbinom(sum(totalpopulation[,3]==1 & totalpopulation[,4]==location),casual_acts[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)],acutehiv_male_potential/male_potential)
    chronichiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)] <- rbinom(sum(totalpopulation[,3]==0 & totalpopulation[,4]==location),pmax(casual_acts[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)]-acutehiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)],0),chronichiv_female_potential/female_potential) # sample how many casual acts with a woman with chronic HIV
    chronichiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)] <- rbinom(sum(totalpopulation[,3]==1 & totalpopulation[,4]==location),pmax(casual_acts[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)]-acutehiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)],0),chronichiv_male_potential/male_potential) # sample how many casual acts with a woman with chronic HIV
    treatedhiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)] <- rbinom(sum(totalpopulation[,3]==0 & totalpopulation[,4]==location),pmax(casual_acts[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)]-acutehiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)]-chronichiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)],0),treatedhiv_female_potential/female_potential) # sample how many casual acts with a woman with treated HIV
    treatedhiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)] <- rbinom(sum(totalpopulation[,3]==1 & totalpopulation[,4]==location),pmax(casual_acts[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)]-acutehiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)]-chronichiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)],0),treatedhiv_male_potential/male_potential) # sample how many casual acts with a woman with treated HIV
    failinghiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)] <- rbinom(sum(totalpopulation[,3]==0 & totalpopulation[,4]==location),pmax(casual_acts[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)]-acutehiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)]-chronichiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)]-treatedhiv_partners[which(totalpopulation[,3]==0 & totalpopulation[,4]==location)],0),failinghiv_female_potential/female_potential) # sample how many casual acts with a woman with failing treated HIV
    failinghiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)] <- rbinom(sum(totalpopulation[,3]==1 & totalpopulation[,4]==location),pmax(casual_acts[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)]-acutehiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)]-chronichiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)]-treatedhiv_partners[which(totalpopulation[,3]==1 & totalpopulation[,4]==location)],0),failinghiv_male_potential/male_potential) # sample how many casual acts with a woman with failing treated HIV
   print(location) 
  }
  transmission_prob <- (totalpopulation[,3]==0)*(peract_acute_fm*acutehiv_partners+peract_chronic_fm*chronichiv_partners+peract_treated_fm*treatedhiv_partners+peract_failing_fm*failinghiv_partners) + (totalpopulation[,3]==1)*(peract_acute_mf*acutehiv_partners+peract_chronic_mf*chronichiv_partners+peract_treated_mf*treatedhiv_partners+peract_failing_mf*failinghiv_partners) # probability: sum up the per-act probabilities from all casual acts with HIV infected partners
  newinfected <- runif(nrow(totalpopulation),0,1) < transmission_prob # check who will get infected in a casual partnership
  
  
 
  infected <- pmax(infected, as.numeric(newinfected)) # update who is infected
  return(infected)
}
  
  
 
  
      
   
  

  