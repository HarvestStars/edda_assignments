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

# now ANCOVA with the influence of the Size
