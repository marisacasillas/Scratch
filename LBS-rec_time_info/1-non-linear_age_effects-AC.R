# Base code by Middy Casillas
# defigured by Alex Cristia

library(tidyverse)

##### BASE LEVEL
input.per.unit <- 1 #this is a constant for all epochs
# this is how much input a child gets per unit of time - it is constant, assuming parents do not increase their talking

cumulated.input <-0
# this is how much input a child got over all preceding epochs

CVC_constant_skill <- 0 
CVC_variable_skill <- 0 
# this is CVC, starting at zero

input.parsing.skill <- 0.1 
#this is how input affects CVC

input.parsing.skill.increase <- 0.1 #this is a constant for all epochs
#this is how input affects parsing

n.epochs <- 100

##### BASE LEVEL

dev.tibble <- tibble(
  age = 0:n.epochs,
  cumulated.input=cumulated.input,
  CVC_constant_skill = CVC_constant_skill,
  CVC_variable_skill = CVC_variable_skill,
  input.parsing.skill = input.parsing.skill
)

# interactional growth of input and knowledge via parsing
for (i in 2:nrow(dev.tibble)) {
  # option 1: constant skill
  # the child increases CVC since the last epoch,
  dev.tibble$CVC_constant_skill[i] <- dev.tibble$CVC_constant_skill[i-1] +
    # by processing their input at the rate they can process it, which is that of the first epoch
    (input.per.unit * dev.tibble$input.parsing.skill[1]) 
  
  # option 2: input-sensitive skill
  # the child's parsing skill is cumulatively affected by the input
  dev.tibble$input.parsing.skill[i] <- dev.tibble$input.parsing.skill[i-1] +
    (input.per.unit * input.parsing.skill.increase)

  dev.tibble$CVC_variable_skill[i] <- dev.tibble$CVC_variable_skill[i-1] +
    # by processing their input at the rate they can process it, which is that of the first epoch
    (input.per.unit * dev.tibble$input.parsing.skill[i-1]) 
  
  #AC commenting this out - assuming caregiver changes input as a function of knowledge, which we don't need
  # the caregiver then increases input relative to the child's linguistic knowledge,
#  dev.tibble$input.per.unit[i] <- dev.tibble$input.per.unit[i-1] +
    # which is their new knowledge level times a stable fraction fpr converting
    # knowledge to input rate
#    (dev.tibble$knowledge[i] * input.increase.for.new.knowledge)
  
  #finally, cumulate input
  dev.tibble$cumulated.input[i] <- dev.tibble$cumulated.input[i-1] + input.per.unit
}

dev.tibble.long <- dev.tibble %>%
  gather("measure", "estimate", -age) %>%
  mutate(estimate.logged = log(estimate))

growth.plot <- ggplot(aes(x = age, y = estimate,
                          color = measure),
                      data = dev.tibble.long) +
  geom_point(size = 4)

growth.plot.log <- ggplot(aes(x = age, y = estimate.logged,
  color = measure),
  data = dev.tibble.long) +
  geom_point(size = 4)


plot(dev.tibble$CVC_variable_skill~dev.tibble$cumulated.input)

dev.tibble$cumulated.input.sq <- dev.tibble$cumulated.input^2

plot(dev.tibble$CVC_variable_skill~dev.tibble$cumulated.input.sq)


