surveys <- read.csv(file="data/portal_data_joined.csv")


### Creating objects in R### Vectors and data types
## ## We’ve seen that atomic vectors can be of type character, numeric, integer, and
## ## logical. But what happens if we try to mix these types in a single
## ## vector?
## 
## ## What will happen in each of these examples? (hint: use `class()` to
## ## check the data type of your object)
num_char <- c(1, 2, 3, 'a')
 
num_logical <- c(1, 2, 3, TRUE)
 
char_logical <- c('a', 'b', 'c', TRUE)
 
tricky <- c(1, 2, 3, '4')

animals <- c("mouse", "rat", "dog", "cat")

class(num_char)
class(num_logical)
class(char_logical)
class(tricky)
class(animals)

animals[2]

animals[c(2,3)]

weight_g <- c(21,34,39,54,50)

weight_g[c(T, F, T, F, F)]

weight_g>50

O50 <-weight_g[weight_g <= 30 | weight_g >= 50] 

O50

O51 <-weight_g[weight_g <= 30 & weight_g >= 50] 

O51

O52 <-weight_g[weight_g <= 30 & weight_g !=20] 

O52

animals[animals %in% c("cat", "rat", "duck")]

animals %in% c("cat", "rat", "duck")


HIGH<-c(2,4,4,NA,6)

mean(HIGH, na.rm=T)

HIGH[HIGH <=mean(HIGH, na.rm=T)]

HIGH[HIGH <=mean(HIGH, na.rm=T) & !is.na(HIGH)]


mean(HIGH >=4, na.rm=T)


## ## Why do you think it happens?
## 
## ## Can you draw a diagram that represents the hierarchy of the data
## ## types?
# * Can you figure out why `"four" > "five"` returns `TRUE`?





### Presentation of the survey data

download.file("https://ndownloader.figshare.com/files/2292169",
               "data/portal_data_joined.csv")

str(survey)

survey

unique(survey$plot_type)

unique(survey$species)


#DATA: Logical, Numeric, Integer, Character





## Challenge
## Based on the output of `str(surveys)`, can you answer the following questions?
## * What is the class of the object `surveys`?
## * How many rows and how many columns are in this object?
## * How many species have been recorded during these surveys?

### Factors
sex <- factor(c("male", "female", "female", "male"))

food <- factor(c("low", "high", "medium", "high", "low", "medium", "high"))

levels(food)

food <- factor(food, levels=c("low", "medium", "high"))

levels(food)

min(food) ## doesn't work




food <- factor(food, levels=c("low", "medium", "high"), ordered=TRUE)
levels(food)
min(food)




f <- factor(c(1, 5, 10, 2))

as.numeric(f)               ## wrong! and there is no warning...
as.numeric(as.character(f)) ## works...
as.numeric(levels(f))[f]    ## The recommended way.************************************




## Challenge
##
## * In which order are the treatments listed?
##
## * How can you recreate this plot with "control" listed
## last instead of first?

food <- factor(food, levels=c("low", "medium", "high"), ordered=TRUE)

******
exprmt <- factor(c("treat1", "treat2", "treat1", "treat3", "treat1", "control",
                 "treat1", "treat2", "treat3"))

exprmt<-factor(exprmt, levels=c("treat1", "treat2", "treat3", "control"))

table(exprmt)

******

barplot(table(factor(exprmt, levels=c("control", "treat1", "treat2", "treat3"))))

******
  
## The data.frame class
## Compare the output of these examples, and compare the difference between when
## the data are being read as `character`, and when they are being read as
## `factor`.
example_data <- data.frame(animal=c("dog", "cat", "sea cucumber", "sea urchin"),
                           feel=c("furry", "furry", "squishy", "spiny"),
                           weight=c(45, 8, 1.1, 0.8))
str(example_data)
example_data <- data.frame(animal=c("dog", "cat", "sea cucumber", "sea urchin"),
                           feel=c("furry", "furry", "squishy", "spiny"),
                           weight=c(45, 8, 1.1, 0.8), stringsAsFactors=FALSE)
str(example_data)
## ## Challenge
## ##  There are a few mistakes in this hand crafted `data.frame`,
## ##  can you spot and fix them? Don't hesitate to experiment!
author_book <- data.frame(author_first=c("Charles", "Ernst", "Theodosius"),
                               author_last=c("Darwin", "Mayr", "Dobzhansky"),
                               year=c(NA, 1942, 1970))
## ## Challenge:
## ##   Can you predict the class for each of the columns in the following
## ##   example?
## ##   Check your guesses using `str(country_climate)`:
## ##   * Are they what you expected? Why? why not?
## ##   * What would have been different if we had added `stringsAsFactors = FALSE`
## ##     to this call?
## ##   * What would you need to change to ensure that each column had the
## ##     accurate data type?
 country_climate <- data.frame(country=c("Canada", "Panama", "South Africa", "Australia"),
                                climate=c("cold", "hot", "temperate", "hot/temperate"),
                                temperature=c(10, 30, 18, 15),
                                northern_hemisphere=c(TRUE, TRUE, FALSE, FALSE),
                                has_kangaroo=c(FALSE, FALSE, FALSE, TRUE), stringsAsFactors  =T)

## Sequences and Subsetting data frames

### 1. The function `nrow()` on a `data.frame` returns the number of
### rows. Use it, in conjuction with `seq()` to create a new
### `data.frame` called `surveys_by_10` that includes every 10th row
### of the survey data frame starting at row 10 (10, 20, 30, ...)
###
### 2. Create a data.frame containing only the observation from 1999 of the -->
### surveys dataset.


yol<-select(surveys, plot_id, species_id, weight)

y95<- filter(surveys, year ==1995)

surveys %>%
  filter (year == 1995) %>% 
  select(species_id, sex, weight, year) %>%
  head()


sur<- surveys %>% filter (weight >15, year <1993) %>% select(year, sex, weight) %>% arrange(year)

nrow(surveys) - nrow(sur)

surN <- sur %>% mutate(weight_kg = weight / 1000) 

newnewsur <- surveys %>% 
  mutate(hindfoot_half = hindfoot_length/2) %>% 
  filter(!is.na(hindfoot_half), hindfoot_half <30) %>% 
  select(species_id, hindfoot_half) %>% 
  arrange(hindfoot_half)
View(newnewsur)

write.csv(newnewsur, file="newnewSur.csv", row.names=F)


surveys %>% 
  group_by(sex) %>% 
  tally
  
surveys %>% 
  group_by(sex, species_id) %>% 
  summarize (mean_weight= mean(weight, na.rm=T)) %>% 
  filter(!is.nan(mean_weight), sex != "")


#1.
surveys %>% 
  group_by(plot_type) %>% 
  tally

#2.
sum<- surveys %>% 
  group_by(genus, species) %>%
  filter(!is.na(hindfoot_length)) %>% 
  summarize_each(c("mean", "min", "max"), hindfoot_length) %>%
  arrange(genus)
View(sum)


#3. The fatty of the year
fat <- surveys %>% 
  group_by(year) %>% 
  filter(weight==max(weight, na.rm=T)) %>% 
  select(year,genus, species, weight) %>% 
  arrange(year)
View(fat)


surveys_complete <- surveys %>% 
  filter(species_id !="",
         !is.na(weight),
         !is.na(hindfoot_length),
         sex %in% c("M", "F"))


species_count <- surveys_complete %>% 
  group_by(species_id) %>% 
  tally

frequent_species <- species_count %>% 
  filter(n >=10) %>% 
  select(species_id)

surveys_complete2 <- surveys_complete %>% 
  filter(species_id %in% frequent_species$species_id)

***********
  
ggplot(data= surveys_complete2,
       aes(x=weight, y=hindfoot_length)) + geom_point(alpha=0.1, col="red")

***********

ggplot(data= surveys_complete2, 
       aes(x= species_id, y=hindfoot_length)) + 
  geom_boxplot() +
  geom_point(color = "tomato", alpha= 0.3, position= "jitter") +
  geom_boxplot()
   

yearly_counts <- surveys_complete2 %>% 
  group_by(year, species_id) %>% 
  tally

**********************************
  
ggplot(yearly_counts,
       aes(x=year, y=n,
           group=species_id,
           color=species_id)) +
  geom_line()

**********************************
  
  
  
  ggplot(yearly_counts,
         aes(x=year, y=n,
             group=species_id,
             color=species_id)) + 
  geom_line() +
  facet_wrap(~species_id)

yearly_sex_counts <- surveys_complete2 %>%
  group_by(year, species_id, sex) %>%
  tally
  
ggplot(yearly_sex_counts,
       aes(x=year, y=n,
           group= sex,
           color= sex)) +
  geom_line() +
  facet_wrap(~species_id)

****************************************************************

surveys_complete2 %>% 
  group_by(species_id, year, sex) %>% 
  summarize(mean_weight= mean(weight)) %>% 
  ggplot(aes(x=year, y=mean_weight, group=sex, color=sex)) +
  geom_line() +
facet_wrap(~species_id)

ggsave("save.png", width = 15, height = 10)

*****************************************************************