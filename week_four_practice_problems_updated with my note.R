library(tidyverse)
library(ggpubr)


## Hands-on activity One ##

View(mtcars)

# Filter mtcars for cars where "cyl" is 6 or more
mtcars %>% 
  filter(cyl >= 6)

# Select the "disp" and "wt" and "mpg" columns from mtcars
mtcars %>% 
  select(disp, wt, mpg)

# Group mtcars by number of cylinders, "cyl"
mtcars %>% 
  group_by(cyl)

# Add up the number of cars in each cylinder group
# method 1
mtcars %>% 
  group_by(cyl) %>% 
  summarise(num_cars = n())
# method 2
mtcars %>% 
  group_by(cyl) %>% 
  tally()

?tally() # Count observations by group

# Calculate the median weight, "wt", in each cylinder group
# calculate the median within each group not all tibble 
mtcars %>% 
  group_by(cyl) %>%
  mutate(median_wt = median(wt))
  

# Create a new column which contains the ratio of horse power, "hp", to "mpg"
# AND only keep the cars with "wt" greater than 3

mtcars %>% 
  mutate(hp_ratio = hp / mpg) %>% 
  filter(wt > 3)


# Group mtcars by "gear" and then find the average "hp" to "wt" ratio
# summarize() is the same as summarise()
# method 1
mtcars %>% 
  mutate(hp_ratio = hp / wt) %>% 
  group_by(gear) %>% 
  summarize(avg_hp_ratio = mean(hp_ratio))

# method 2 (More Efficient way)
mtcars %>% 
  group_by(gear) %>% 
  summarize(avg_hp_ratio = mean(hp / wt))

#----------------------#

as_tibble(iris)  # This is your data set for these problems
View(iris)  # View it in RStudio

### dplyr problems ###

# NOTE: Don't forget, you there's a cheat sheet available on Box under "Resources"

# A. Complete the filter so that it only returns the "virginica" species
iris %>% 
  filter(Species == "virginica")

# B. Use summarise to get the mean sepal length for each species
iris %>% 
  group_by(Species) %>% 
  summarise(mean_SepalLength = mean(Sepal.Length))

# C. Get the ratio of sepal length to sepal width for all samples

iris %>% 
  mutate(sepalratio = Sepal.Length / Sepal.Width)

# D. Get the average ratio of petal length to petal width for each species
iris %>% 
  group_by(Species) %>% 
  summarise(len_ratio = mean(Sepal.Length / Petal.Length))

### ggplot problems ###

# Pipe the above into ggplot2, set x axis to "species",  y ot the ratio column
iris %>% 
  group_by(Species) %>% 
  summarise(len_ratio = mean(Sepal.Length / Petal.Length)) %>% 
  ggplot(mapping = aes(x = Species, y = len_ratio, fill = Species)) +
  geom_bar(stat = "identity") +
  ylab("Average of Sepal to Petal Length(cm)")+
  labs(title = "Iris flower analysis")
  
# A. Make a scatter plot comparing sepal length to petal length

ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()
# B. Color the plot by the Species 
ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(stat = "identity")

# C. Change to theme_classic(), set the base font size to 16, 
# set the x label to "Sepal Length (cm)", and set the y label to "Petal Length (cm)",
# set the title to "Sepal vs Petal Length in Iris Flowers"
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  xlab("Sepal Length (cm)") +
  ylab("Petal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16)

# D. Plot the linear correlation between Sepal and Petal length stat_smooth 
# method 1: linear correlation with CI
iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ylab("Petal Length (cm)") +
  xlab("Sepal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16) +
  stat_smooth(method = "lm")

# method 2: linear correlation without CI
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  xlab("Sepal Length (cm)") +
  ylab("Petal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16)+
  geom_line(stat = "smooth")


# E. Add the pearson correlation coefficient using ggpubr's stat_cor()

iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ylab("Petal Length (cm)") +
  xlab("Sepal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16) +
  stat_smooth(method = "lm") +
  stat_cor()

# method 2
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  xlab("Sepal Length (cm)") +
  ylab("Petal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16)+
  geom_line(stat = "smooth") +
  stat_cor()

# F. Save the plot to a pdf file or can save to png() or jpeg()
# method 1: for ggsave(), the default width and height are the size of the current graphics device. Plot size in units ("in", "cm", "mm", or "px")

iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ylab("Petal Length (cm)") +
  xlab("Sepal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16) +
  stat_smooth(method = "lm") +
  stat_cor()

ggsave(filename = "my_figure.pdf")

# method 2: for pdf(), the default width and height of the graphics are 7 inches (fixed).
pdf()
iris %>%
  ggplot(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ylab("Petal Length (cm)") +
  xlab("Sepal Length (cm)") +
  labs(title = "Sepal vs Petal Length in Iris Flowers") +
  theme_classic(base_size = 16) +
  stat_smooth(method = "lm") +
  stat_cor()
dev.off()



### Challenge Problems ###

# Answer the following using dplyr and ggplot2 (with significance testing via ggpubr):

library(tidyverse)
library(ggpubr)

# 1. Do 6 cylinder cars have better MPG than 8 cylinder cars?

# Part 1:
View(mtcars)
mtcars %>% 
  group_by(cyl) %>% 
  select(mpg) %>% 
  summarise(mpg.median = median(mpg))

# Answers 1:  the median of mpg for cars with 6 cyl is 19.7 which is higher than that with 8 cyl (15.2)

#------------#
# Part 2:

# aes(x, y, color = factor(cyl)), it shows boxplot with colored boxes
# aes(x, y, fill = factor(cyl)), it shows boxplot with black boxes with filled colored
###
mtcars %>% 
  filter(cyl == 6 | cyl == 8) %>%
  ggplot(mapping = aes(x = factor(cyl), y = mpg, color = factor(cyl))) +
  geom_boxplot()+
  stat_compare_means(method = "t.test", label = "p.signif", 
                     comparisons = list(c("6","8")))+
  theme_classic(base_size = 16) +
  theme(legend.position = "none", plot.title = element_text(hjust =0.5)) +
  ylab("Miles per Gallon (MPG)") +
  xlab("Number of Cylinders") +
  labs(title = "Number of Cylinders and MPG")

###
mtcars %>%
  filter(cyl == 6 | cyl == 8) %>%
  ggplot(mapping = aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  stat_compare_means(method = "t.test", label = "p.signif",
                     comparisons = list(c("6","8"))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none", plot.title = element_text(hjust =0.5)) +
  ylab("Miles per Gallon (MPG)") +
  xlab("Number of Cylinders") +
  labs(title = "Number of Cylinders and MPG")
ggsave(filename = "challenge 1-effect of cyl number on mpg.boxplot.jpg")


mtcars %>%
  filter(cyl == 6 | cyl == 8) %>%
  ggplot(mapping = aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  stat_compare_means(method = "t.test", label = "p.format",
                     comparisons = list(c("6","8"))) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none", plot.title = element_text(hjust =0.5)) +
  ylab("Miles per Gallon (MPG)") +
  xlab("Number of Cylinders") +
  labs(title = "Number of Cylinders and MPG")

ggsave(filename = "challenge 1-effect of cyl number on mpg.boxplot-p value shown.jpg")

# Answer 2: the p value is 4.5e-5, which shows that 6 cyl cars differs significantly from cyl cars in mpg

# side note:
# filter (cyl == c(6, 8)) only extract part of the data. Why?

# Because filter() runs logical tests on the rows of our dataset, 
# we should make use of R’s 
# (1) logical operators (&, |, !), 
# (2) relational operators (<, >, <=, >=, ==, !=), 
# (3) and other functions that return logical values (%in%, is.na(), etc.).

?"%in%"
# %in% is:
# used to do value matching, it returns logical vector (TRUE or FALSE but never NA) 
# Output logical vector has the same length as left operand.
# works only with vectors
# %in% Example:
x <- c(1,2,4)
y <- c(5,4,3)
x %in% y


#----------------#
# 2. Is there a correlation between urban population and Murder rate?
?USArrests
View(USArrests)
str(USArrests)
summary(USArrests)
USArrests %>% 
  ggplot(mapping = aes(x = UrbanPop, y = Murder)) +
  ylab("Murder arrested (per 100k)") +
  xlab("Percentage of urban population") +
  labs(title = "Urban pop and per capita murder rate") +
  theme_classic() + 
  geom_point() +
  stat_smooth(method = "lm") +
  stat_cor()
ggsave(filename = "challenge 2-urben pop and murder rate correlation.jpg")
dev.off()
# Answer: 
# (1) Pearson correlation (r) is close to 0, showing that the linear dependence between two variables (x and y) is very low.
# (2) on the scatter plot, there is no other correlation patterns observed.


# 3. Did the treatment #1 or #2 make the plants grow more or less than control?
View(PlantGrowth)
str(PlantGrowth)
unique(PlantGrowth$group)

# Multiple pairwise test against a reference group
# more info for stat_compare_mean(), check: http://rpkgs.datanovia.com/ggpubr/reference/stat_compare_means.html
# method 1
PlantGrowth %>%
ggplot(mapping = aes(x = factor(group), y = weight, fill = factor(group))) +
  geom_boxplot() +
  stat_compare_means(method = "t.test", label = "p.signif", comparisons = list(c("ctrl","trt1")), label.y =6.5) +
  stat_compare_means(method = "t.test", label = "p.signif", comparisons = list(c("ctrl", "trt2")), label.y = 6.8) +
  stat_compare_means(label.y = 7.3) + #add global one way ANOVA p-value
  theme_classic(base_size = 16) +
  theme(legend.position = "none") +
  xlab(NULL) 

ggsave(filename = "challenge 3 -effect of trt1 and trt2 on plant wt-method-1.jpg")
dev.off()
# method 2

PlantGrowth %>%
  ggplot(mapping = aes(x = factor(group), y = weight, fill = factor(group))) +
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y =6.5) + #add global p-value
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "ctrl") + 
  theme_classic(base_size = 16) +
  theme(legend.position = "none") +
  xlab(NULL) 

ggsave(filename = "challenge 3 -effect of trt1 and trt2 on plant wt-method-2.jpg")
dev.off()
# method 3: specify 2 lists: trt1 vs control and trt2 vs control in one call of stat_compare_means()
PlantGrowth %>%
  group_by(group) %>%
  ggplot(mapping = aes(x = factor(group, labels = c("Control", "Treatment 1", "Treatment 2")),
                       y = weight, fill = group)) +
  geom_boxplot() + 
  xlab(NULL) +
  ylab("Plant weight") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(title = "Plant Growth With Treatment") + 
  stat_compare_means(method = "t.test", label = "p.signif", 
                     comparisons = list(c("Control", "Treatment 1"), c("Control", "Treatment 2"))) 

ggsave(filename = "challenge 3 -effect of trt1 and trt2 on plant wt-method-3.jpg")
dev.off()
# Answer: trt 2 shows to significantly increase plant weight, trt 1 did not show significant effect on plant weight

# 4. Does Vitamin C supplementation improve tooth growth? Is OJ better than Vitamin C at dose of 2?
View(ToothGrowth)
?ToothGrowth
# [,1]	len	numeric	Tooth length
# [,2]	supp	factor	Supplement type (VC or OJ).
# [,3]	dose	numeric	Dose in milligrams/day

## Part 1 ##
ToothGrowth %>% 
  ggplot(mapping = aes(x = factor(supp), y = len, fill = supp)) +
  geom_boxplot() +
  xlab("Supplement type") +
  ylab("Tooth Length") +
  stat_compare_means(method = "t.test", label = "p.signif", comparisons = list(c("VC", "OJ")))+
  theme_classic(base_size = 16)
dev.off()
# Answer 1: Vitamin C supplementation does NOT show improved tooth growth as compared to OJ supp

## Part 2 ##
# Method 1: no grid and individual box for each facet
ToothGrowth %>% 
  ggplot(mapping = aes(x = factor(supp), y = len, fill = supp)) +
  geom_boxplot() +
  facet_wrap(~paste0(dose, " mg/day")) +
  xlab(NULL) +
  ylab("Tooth Length") +
  labs(title = "Tooth Growth With Treatment") +
  stat_compare_means(method = "t.test", label = "p.signif", comparisons = list(c("VC", "OJ")))+
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
ggsave(filename = "challenge 4 -Tooth Growth With Treatment-method 1.jpg")

# Method 2: with grid and individual box for each facet (prefered)
ToothGrowth %>%
  ggplot(aes(x = factor(supp), y = len, fill = supp)) +
  geom_boxplot() +
  facet_wrap(~ paste0(dose, " mg/day")) +
  stat_compare_means(method = "t.test",
                     label = "p.signif", 
                     comparisons = list(c("OJ", "VC"))) +
  xlab(NULL) +
  ylab("Tooth Length") +
  labs(title = "Tooth Growth With Treatment") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") 

ggsave(filename = "challenge 4 -Tooth Growth With Treatment-method 2.jpg")
dev.off()
# Answer 2: Vitamin C supplementation does NOT show improved tooth growth as compared to OJ supp at dose of 2
# However, Vitamin C significantly reduced tooth length at dose of 0.5 and 1