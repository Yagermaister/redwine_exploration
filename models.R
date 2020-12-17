library("readr")
library("faraway")
library("lmtest")

data <- read_csv2("winequality-red.csv", col_types = cols(
      fixed_acidity = col_double(),
      volatile_acidity = col_number(),
      citric_acid = col_number(),
      residual_sugar = col_number(),
      chlorides = col_number(),
      free_sulfur_dioxide = col_number(),
      total_sulfur_dioxide = col_number(),
      density = col_number(),
      pH = col_number(),
      sulphates = col_number(),
      alcohol = col_number(),
      quality = col_number()
))

attach(data)

#plot(quality, fixed_acidity)
#plot(quality, volatile_acidity)
#plot(quality, citric_acid)
#plot(quality, residual_sugar)
#plot(quality, chlorides)
#plot(quality, free_sulfur_dioxide)
#plot(quality, total_sulfur_dioxide)
#plot(quality, density)
#plot(quality, pH)
#plot(quality, sulphates)
#plot(quality, alcohol)

model <- lm(quality ~ fixed_acidity+volatile_acidity+citric_acid+
                  residual_sugar+chlorides+free_sulfur_dioxide+
                  total_sulfur_dioxide+density+pH+sulphates+alcohol)

print(cor(model))
#autocorrelation test Darbin-Wotson
print(dwtest(model))
#autocorrelation test of different stages
print(bgtest(model, 2))
print(bgtest(model, 3))
print(bgtest(model, 4))
#test of geteroscedastity
print(bptest(model))
#model summary
print(summary(model))
#multicollinearity
print(vif(model))