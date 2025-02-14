# read the txt file
crop_data <- read.delim("crops.txt", sep = " ")

# now conduct two way ANOVA with country and related
# recode the variables
cropframe <- cropframe %>% mutate(related = recode(related, 'yes' = '1', 'no' = '0'))

cropframe = data.frame(
  crops = crop_data$Crops, 
  county = factor(crop_data$County), 
  related = factor(crop_data$Related))

# draw the interaction plot
par(mfrow=c(1,2))
interaction.plot(cropframe$related,cropframe$county,cropframe$crops)
interaction.plot(cropframe$county,cropframe$related,cropframe$crops)

# find out if county and related are factors
is.factor(cropframe$county)
is.factor(cropframe$related)

# conduct a two-way ANOVA now
cropanov=lm(crops~county*related); anova(cropanov)
summary(cropanov)  # non-significant interaction and main effect

# see if assumptions of normality are met
qqnorm(residuals(cropanov)); plot(fitted(cropanov),residuals(cropanov))

# remove interaction and fit the additive model
cropanov2=lm(crops~county+related,data=cropframe); anova(cropanov2)
summary(cropanov2)  # non-significant main effects

# 2(b) ANCOVA with the influence of the Size
# Two ANCOVA test: (1) county + size
cropframe = data.frame(
  crops = crop_data$Crops, 
  size = crop_data$Size,
  county = factor(crop_data$County), 
  related = factor(crop_data$Related))

cropanov3=lm(crops~size+county,data=cropframe);anova(cropanov3)
summary(cropanov3) # significant effect of size

# test normality
qqnorm(residuals(cropanov3)); plot(fitted(cropanov3),residuals(cropanov3))

# test interaction effect size*county
cropanov4=lm(crops~size*county,data=cropframe);anova(cropanov4)
summary(cropanov4)   # significant interaction effect


# (2) related + size
cropanov5=lm(crops~size+related,data=cropframe);anova(cropanov5)
summary(cropanov5)  # significant effect of size

# test normality
qqnorm(residuals(cropanov5)); plot(fitted(cropanov5),residuals(cropanov5))

# test interaction effect
cropanov6=lm(crops~size*related,data=cropframe);anova(cropanov6)
summary(cropanov6)  # no significant interaction effect

# (3) county + size + related
cropanov7=lm(crops~county*related+size,data=cropframe);anova(cropanov7)
summary(cropanov7) 

# test normality
qqnorm(residuals(cropanov7)); plot(fitted(cropanov7),residuals(cropanov7)) # normality may be violated
