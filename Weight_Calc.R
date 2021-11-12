# Determine weights between measures, if they are equally important

# Average of each measure
# Multiply each measure by both of the others averages

# Example
# x <- 1:10
# y <- seq(0.1, 1, 0.1)
# z <- seq(0.01, 0.1, 0.01)
# 
# x2 <- mean(y)*mean(z)*x
# y2 <- mean(x)*mean(z)*y
# z2 <- mean(x)*mean(y)*z

# Ajusted to have the same mean and in this case all the same values

# The weight according to equal importance would then be
# the product of the means of the other measures.

w1 <- mean(TOT)*mean(ARV)
w2 <- mean(SD)*mean(ARV)
w3 <- mean(SD)*mean(TOT)

obj <- w1*SD+w2*TOT+w3*ARV

# Example of weigthing with importance

obj_i <- 0.5*w1*SD+0.25*w2*TOT+0.25*w3*ARV






