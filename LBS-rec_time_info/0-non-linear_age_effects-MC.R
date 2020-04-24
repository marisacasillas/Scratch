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


input.knowledge.plot <- ggplot(aes(x = input.rate, y = knowledge),
                      data = dev.tibble) +
  geom_point(size = 4) +
  geom_smooth(method = "lm")

dev.tibble$input.rate.sq <- dev.tibble$input.rate^2
input.knowledge.plot <- ggplot(aes(x = input.rate.sq, y = knowledge),
  data = dev.tibble) +
  geom_point(size = 4) +
  geom_smooth(formula = y ~ log(x))


# input.age.knowledge.plot <- ggplot(aes(x = input.rate, y = age, size = knowledge),
#                       data = dev.tibble) +
#   geom_point()

input.age.knowledge.plot <- ggplot(aes(x = input.rate, y = knowledge,
                                        color = age, alpha = age, size = age),
                      data = dev.tibble) +
  geom_point()

dev.tibble$age.sq <- dev.tibble$age^2
# input.agesq.knowledge.plot <- ggplot(aes(x = input.rate, y = age.sq, size = knowledge),
#                       data = dev.tibble) +
#   geom_point()

input.agesq.knowledge.plot <- ggplot(aes(x = input.rate, y = knowledge,
                                        color = age.sq, alpha = age.sq, size = age.sq),
                      data = dev.tibble) +
  geom_point()


# models

model1 <- lm(knowledge ~ age * input.rate, data = dev.tibble)
summary(model1)
plot(model1)

model2 <- lm(knowledge ~ age.sq * input.rate, data = dev.tibble)
summary(model2)
plot(model2)

anova(model1, model2)

model1.log <- lm(knowledge ~ age * input.rate.log, data = dev.tibble)
summary(model1.log)
plot(model1.log)

model2.log <- lm(knowledge ~ age.sq * input.rate.log, data = dev.tibble)
summary(model2.log)
plot(model2.log)

anova(model1, model2)
