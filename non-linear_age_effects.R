library(tidyverse)

input.rate <- 1
parsing.skill <- 0.00000001
knowledge <- 0.00000001
knowledge.increase.from.new.input <- 1.00000005
parsing.increase.from.new.knowledge <- 1.00000005
input.increase.for.new.knowledge <- 1.00000001
n.epochs <- 20

dev.tibble <- tibble(
  age = 0:n.epochs,
  knowledge = knowledge,
  input.rate = input.rate,
  parsing.skill = parsing.skill
)

# interactional growth of input and knowledge via parsing
for (i in 2:nrow(dev.tibble)) {
  # the child gains knowledge since the last epoch,
  dev.tibble$knowledge[i] <- dev.tibble$knowledge[i-1] +
    # which is their prev heard input rate times their parsing skill,
    (dev.tibble$input.rate[i-1] * dev.tibble$parsing.skill[i-1]) *
    # mediated by some stable fraction for converting parsed input to knowledge
    knowledge.increase.from.new.input
  
  # the child's increased knowledge increases their parsing skill,
  dev.tibble$parsing.skill[i] <- dev.tibble$parsing.skill[i-1] +
    # which is their new knowledge level times a stable fraction for converting
    # knowledge to parsing skill
    (dev.tibble$knowledge[i] * parsing.increase.from.new.knowledge)
  
  # the caregiver then increases input relative to the child's linguistic knowledge,
  dev.tibble$input.rate[i] <- dev.tibble$input.rate[i-1] +
    # which is their new knowledge level times a stable fraction fpr converting
    # knowledge to input rate
    (dev.tibble$knowledge[i] * input.increase.for.new.knowledge)
}

dev.tibble.long <- dev.tibble %>%
  gather("measure", "estimate", -age)

growth.plot <- ggplot(aes(x = age, y = estimate,
                          color = measure),
                      data = dev.tibble.long) +
  geom_point(size = 4)
