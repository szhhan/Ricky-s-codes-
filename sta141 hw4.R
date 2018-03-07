library(ggplot2)

# question 1
# first create a shuffle function for later usage for chance and chest
shuffle = function(x)  sample(x,length(x))
# start to write the simulation:
  simulate_monopoly = function(n = 1000, d = 6){
  
  # roll both dice n times
  dices = matrix(sample(d, n*2, replace = TRUE), ncol=2)
  # define the doubles and the rolls
  doubles = dices[,1] == dices[,2]
  rolls = apply(dices, 1, sum)
  
  # get a vector of shuffled deck for cc
  cc_deck = c(0,10,rep(NA,14))
  cc_deck = shuffle(cc_deck)
  ccdeck_i=0
  
  # get a vector of shuffled deck for chance

  ch_deck = c(0, 10, 11, 24, 39, 5, -1, -1, -2, -3,rep(NA,6))
  ch_deck = shuffle(ch_deck)
  chdeck_i=0
  
  # cc and ch locations
  cc_location = c(2, 27, 33)
  ch_location = c(7, 22, 36)
  
  # game start
  locations = rep(0, n+1)
  locations[1] = 0
  # the loop (the game)
  for (i in 1:n){
    
    # first considering double
    if (i > 2 && sum(doubles[(i-2):i]) == 3){ # double && for lazy evaluation
      locations[i+1] = 10
      next
    }
    
    # then do the simple location changes, %%40 because 40 is a round
    loc = (locations[i]+rolls[i]) %% 40
    
    # check community chest
    if (loc %in% cc_location) {
      card = cc_deck[ccdeck_i+1]
      ccdeck_i = (ccdeck_i+1) %% 16
      if (!is.na(card)){
        loc = card
      }
    }
    
    # check chance
    # adjust the position to different chance situations
    if (loc %in% ch_location){
      card2 = ch_deck[chdeck_i+1]
      chdeck_i = (chdeck_i+1) %% 16
      if (is.na(card2)){
      } else if (card2 >= 0){
        loc = card2
      } else if (loc == -1){
        if (loc == 7){
          loc = 15
        } else if (loc == 22){
          loc = 25
        } else {
          loc = 5
        }
      } else if (loc == -2){
        if (loc == 22){
          loc = 28
        } else {
          loc = 12
        }
      } else if (loc == -3){
        loc = loc - 3 # loc-3 must be > 0 or < 40 since loc = 7, 22 or 36
      }
    }
    
    # check go to jail
    if (loc == 30){
      loc = 10
    }
    
    locations[i+1] = loc 
  }
  
  locations = factor(locations, 0:39) # ensure 30 appears
  
  return(locations)
  
}

# question 2

# make the function estimate monopoly to output the prob. 
estimate_monopoly = function(sim){
  freq_table = table(sim)
  prob_table = prop.table(freq_table)
  return(prob_table)
}


#run the simulation 300000times for differnet dice sided conditions
# create a big matrix
prob_table = sapply(3:6, function(x){ estimate_monopoly(simulate_monopoly(300000, d = x)) })
# to see the 6,4-sided dices largest square probs. 
sort(prob_table[,4])
sort(prob_table[,2])

#construct the barplot for probabiliy distribution of squares for each sided dice conditions
par(mfrow=c(2,2))
barplot(prob_table[,4],main="probability distribution of squares for 6 sided dice",xlab="square number",ylab="Prob",ylim=c(0,0.08))
barplot(prob_table[,3],main="probability distribution of squares for 5 sided dice",xlab="square number",ylab="Prob",ylim=c(0,0.08))
barplot(prob_table[,2],main="probability distribution of squares for 4 sided dice",xlab="square number",ylab="Prob",ylim=c(0,0.08))
barplot(prob_table[,1],main="probability distribution of squares for 3 sided dice",xlab="square number",ylab="Prob")

# display long-term probabilities for 3, 4, 5, 6 sides dice
# other method
probs.df = data.frame(sides = rep(3:6, each = 40), location = factor(rep(0:39, 4)), probabilities = as.vector(prob_table))
ggplot(probs.df, aes(sides, location, fill = probabilities)) + scale_fill_gradient(high = "red", low = "white") + geom_tile() +
  labs(x = "Dice sides", y = "Location", title = "Monopoly location probabilities")

# question 3

#record the time
starttime = Sys.time()
#calculate the probability of go to jail for 1000 times
jail_probs = sapply(1:1000, function(x){ estimate_monopoly(simulate_monopoly(10000, d = 6))[11] })
# find the sd of it
sd(jail_probs)
#convert it to the standard error
error = sd(jail_probs)/sqrt(length(jail_probs))
error
#calculate the operation time. 
endtime = Sys.time()
duration1 = endtime - starttime

# question 4a

# same, record the time
starttime2 = Sys.time()
# run the location result of one simulation
all_locs = simulate_monopoly(n = 10000, d = 6)
# find the prob of go to jail by just sampling/shuffling the location result we just got from one simulation
# so called boostrap
boostrap_samples = sapply(1:1000, function(x) estimate_monopoly(sample(all_locs, replace = TRUE)))
# same, calculate the standard error of it 
sd(boostrap_samples[11,]) # 0.002459514 # less accurate
error4a = sd(boostrap_samples[11,])/sqrt(1000)
error4a
# check the duration time, compare with part3
endtime2 = Sys.time()
duration2 = endtime2 - starttime2
# question 4b

# bootstrap is faster, for loop take time in part 3

# question 5

#calculate the standard errors of all squares,same method as question 3, just including other squares
probsfor6 = sapply(1:1000, function(x){ estimate_monopoly(simulate_monopoly(10000, d = 6))})
# calculate the standard error, and plot
sdfor6 = apply(probsfor6,1,sd)
sdfor6final = sdfor6/sqrt(1000)
barplot(sdfor6final,main="standard error of squares for 6 sided dice",xlab="square number",ylab="standard error",ylim=c(0,8e-05))

# these are the same as above i did for the 6-sided
# i do the 5,4,3-sided
probsfor5 = sapply(1:1000, function(x){ estimate_monopoly(simulate_monopoly(10000, d = 5))})
sdfor5 = apply(probsfor5,1,sd)
sdfor5final = sdfor5/sqrt(1000)
barplot(sdfor5final,main="standard error of squares for 5 sided dice",xlab="square number",ylab="standard error",ylim=c(0,8e-05))

probsfor4 = sapply(1:1000, function(x){ estimate_monopoly(simulate_monopoly(10000, d = 4))})
sdfor4 = apply(probsfor4,1,sd)
sdfor4final = sdfor4/sqrt(1000)
barplot(sdfor4final,main="standard error of squares for 4 sided dice",xlab="square number",ylab="standard error",ylim=c(0,8e-05))


probsfor3 = sapply(1:1000, function(x){ estimate_monopoly(simulate_monopoly(10000, d = 3))})
sdfor3 = apply(probsfor3,1,sd)
sdfor3final = sdfor3/sqrt(1000)
barplot(sdfor3final,main="standard error of squares for 3 sided dice",xlab="square number",ylab="standard error",ylim=c(0,8e-05))


#6
par(mfrow=c(1,2))
#first plot the 6-sided standard error for n=10000
barplot(sdfor6final,main="standard error of squares for 6 sided dice",xlab="square number",ylab="standard error",ylim=c(0,8e-05))

# then set n=20000, compare the result with n=10000
probsfor62 = sapply(1:1000, function(x){ estimate_monopoly(simulate_monopoly(20000, d = 6))})
sdfor62 = apply(probsfor62,1,sd)
sdfor6final2 = sdfor62/sqrt(1000)
barplot(sdfor6final2,main="standard error of squares for 6 sided dice(larger n)",xlab="square number",ylab="standard error",ylim=c(0,8e-05))

