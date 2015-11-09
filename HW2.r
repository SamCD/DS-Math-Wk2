#Q1
# (a) What is the probability of rolling a pair of dice and obtaining 
#a total score of
#9 or more? 
combos <- 6 * 6
successes <- 10 #([3,6],[4,5],[4,6],[5,4],[5,5],[5,6],[6,3],[6,4],[6,5],[6,6])
p <- successes / combos
# p = 0.27777778

#(b) What is the probability of rolling a pair of dice and obtaining a
#total score of 7
successes <- 6 #([1,6],[2,5],[3,4],[4,3],[5,2],[6,1])
p <- successes / combos
# p = 0.1666667

#Q2 A box contains four black pieces of cloth, two striped pieces, and six dotted
# pieces. A piece is selected randomly and then placed back in the box. A second
# piece is selected randomly. What is the probability that:
blk <- 4
str <- 2
dot <- 6
tot <- blk + str + dot
#a. both pieces are dotted?
p <- (dot / tot) * (dot / tot) #independent events
# p = 0.25
#b. the first piece is black and the second piece is dotted?
p <- (blk / tot) * (dot / tot)
# p = 0.1666667
#c. one piece is black and one piece is striped?
combos <- 12 * 12
successes <- 16 
p <- successes / combos
p
# p = 0.1111111

#Q3
# A card is drawn at random from a deck.
tot <- 52
#(a) What is the probability that it is an ace or a king?
p <- (4/tot) + (4/tot) #Mutually exclusive; P(A or B) = P(A) + P(B)
# p = 0.1538462
#(b) What is the probability that it is either a red card or a black card?
p <- (26 / tot) + (26 / tot)
# p = 1.0

#Q4
#The probability that you will win a game is 0.45.
psucc <- 0.45
N <- 80
# (a) If you play the game 80
# times, what is the most likely number of wins? 
mean <- psucc * N
# most likely number of wins = mean = 36

# (b) What are the mean and
# variance of a binomial distribution with p = 0.45 and N = 80?
var <- psucc * N * (1 - psucc)
# mean = 36, variance = 19.8

#Q5
#  A fair coin is flipped 9 times.
#What is the probability of getting exactly 6 heads?
p <- dbinom(6,9,0.5)
# p = 0.1640625

#Q6
# When Susan and Jessica play a card game, Susan wins 60% of the time. If they
# play 9 games, what is the probability that Jessica will have won more games than
# Susan?
psucc <- 0.6
N <- 9
mean <- psucc * N
p <- ppois(4,mean) #the probability of Susan winning 4 or less games, given her expected amount
# p = 0.3733108

#Q7
# You flip a coin three times. 
# (a) What is the probability of getting heads on only one of your flips? 
psucc <- 0.5
N <- 3
p <- dbinom(1,N,psucc)
# p = 0.375

# (b) What is the probability of getting heads on at least one flip?
mean <- psucc * N
p <- ppois(1,mean,lower=FALSE)
# p = 0.4421746

#Q8
# A test correctly identifies a disease in 95% of people who have it. It correctly
# identifies no disease in 94% of people who do not have it. In the population, 3%
# of the people have the disease. What is the probability that you have the disease
# if you tested positive?
pB_A1 = 0.95
pA1 = 0.03
pB_A2 = 1 - 0.94
pA2 = 0.94
pA1_B = (pB_A1 * pA1) / (pB_A1*pA1 + pB_A2*pA2)
# p = 0.335689

#Q9
# A jar contains 10 blue marbles, 5 red marbles, 4 green marbles, and 1 yellow
# marble. Two marbles are chosen (without replacement). 
blu <- 10
red <- 5
grn <- 4
yel <- 1
tot <- blu + red + grn + yel
# (a) What is the probability that one will be green and the other red?
p <- ((grn / tot) * (red / (tot - 1))) + ((red / tot) * (grn / (tot - 1))) #Mutually Exclusive
# p = 0.1052632

# (b) What is the probability that one will be blue and the other yellow?
p <- ((blu / tot) * (yel / (tot - 1))) + ((yel / tot) * (blu / (tot - 1)))
# p = 0.05263158

#Q10
# You roll a fair die five times, and you get a 6 each time. What is the probability
# that you get a 6 on the next roll?
p <- 1/6
# p = 0.1666667

#Q11
# You win a game if you roll a die and get a 2 or a 5. You play this game 60 times.
psucc <- 1/3
N <- 60
# a. What is the probability that you win between 5 and 10 times (inclusive)? 
p <- (dbinom(5,N,psucc) 
+ dbinom(6,N,psucc) 
+ dbinom(7,N,psucc) 
+ dbinom(8,N,psucc) 
+ dbinom(9,N,psucc) 
+ dbinom(10,N,psucc))
# p = 0.003167526

# b. What is the probability that you will win the game at least 15 times?
mean <- psucc * N
p <- ppois(15,mean,lower=FALSE)
# p = 0.8434869

# c. What is the probability that you will win the game at least 40 times?
p <- ppois(40,mean,lower=FALSE)
# p = 0.0000254

# d. What is the most likely number of wins.
# most likely number of wins = mean = 20

# e. What is the probability of obtaining the number of wins in d?
p <- dbinom(mean,N,psucc)
# p = 0.1087251

#Q12
# In a baseball game, Tommy gets a hit 30% of the time when facing this pitcher.
# Joey gets a hit 25% of the time. They are both coming up to bat this inning.
# a. What is the probability that Joey or Tommy will get a hit? 
tom <- .3
joe <- .25
p <- tom + joe #independent
# p = 0.55

# b. What is the probability that neither player gets a hit?
p <- (1 - (tom + joe))
# p = 0.45

# c. What is the probability that they both get a hit?
p <- tom * joe
# p = 0.075

#Q13
# An unfair coin has a probability of coming up heads of 0.65. The coin is flipped
# 50 times. What is the probability it will come up heads 25 or fewer times? 
psucc <- 0.65
N <- 50
mean <- psucc * N
p <- ppois(25,mean)
# p = 0.1063967

#Q14
#You draw two cards from a deck, what is the probability that:
N <- 52 #Population size
n <- 2 #Sample size
x <- 2 #number of successes
face <- 12
hearts <- 13
# p = 0.04977376
#Hypergeometric Distribution
k <- face
p <- 
(
  (
  (fac(k)/(fac(k-x)*fac(x)) * fac(N-k)/(fac((N-k)-fac(n-x))*fac(n-x))))
  /(fac(N)/fac(N-n)*fac(n))
)

# b. you draw two cards from a deck and both of them are hearts?
k <- hearts
p <- 
  (
    (
      (fac(k)/(fac(k-x)*fac(x)) * fac(N-k)/(fac((N-k)-fac(n-x))*fac(n-x))))
    /(fac(N)/fac(N-n)*fac(n))
  )

#Q15
# True/False: You are more likely to get a pattern of HTHHHTHTTH than
# HHHHHHHHTT when you flip a coin 10 times.
N <- 10
x1 <- 4
x2 <- 2
psucc <- 0.5
p1 <- (fac(N) / (fac(x1)*fac(N-x1))) * (psucc^x1) * ((1 - psucc)^(N-x1))
p2 <- (fac(N) / (fac(x2)*fac(N-x2))) * (psucc^x2) * ((1 - psucc)^(N-x2))
# (p1 = p2) = FALSE

#PT2
#Q1
a <- 0
b <- 4
#a Find c
c <- 0.25
fx <- function(x){0.25 * x}
dfx <- antiD(0.25 * x ~ x) 

#b
X <- dfx(1) - dfx(-1) #integrate on the range
# 0

#c
X <- pexp(2,0.25*x,lower.tail=FALSE ~ x)
# 0.6321206

#d
X <- dfx(3) - dfx(1)
#1

#e
EX <- dfx(1) - dfx(0)
# 0.125

#f
var <- EX - EX^2
sd <- sqrt(var)
#0.109375, 0.3307189

#Q2
fx <- function(x){(3/2) * x^2}
dfx <- antiD((3/2) * x^2 ~ x)
#a
X <- dfx(0) - dfx(-2)
integrate(dfx,-2,0)

#b
X <- pexp(-0.5,(3/2) * x^2,lower.tail=FALSE ~ x)
# 0

#c
X <- dfx(.5) - dfx(-.5)
# 0.125

#d
EX <- dfx(1) - dfx(0)
# 0.5

#e
var <- EX - EX^2
sd <- sqrt(var)
# 0.25,0.5

