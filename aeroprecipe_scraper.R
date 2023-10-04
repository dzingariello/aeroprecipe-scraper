# Scraping aeroprecipes.com

library('rvest')
library('RSelenium')
library('tidyr')
library('dplyr')
library('ggplot2')
library('RMySQL')
library('wordcloud')
library('tibble')

url <- 'https://www.aeroprecipe.com'
#Starting Selenium
rD <- rsDriver(port = 4450L, browser = c("firefox"))
remDr <- rD[["client"]]
remDr$navigate(url)
# find button to close pop-up modal
closeModal <- remDr$findElement(using = 'css selector', "button.justify-center")
# click button
closeModal$clickElement()
remDr$screenshot(display=TRUE)
# scroll down to load all recipes
for(i in 1:3){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(2)    
}
#get the page html
webpage <- read_html(remDr$getPageSource()[[1]])
#get all recipe paths
recipe_urls <- html_nodes(webpage, 'div#recipes a') %>% html_attr('href')
recipe <- data.frame(matrix(ncol = 11, nrow = length(recipe_urls)))
colnames(recipe) <- c('name', 'upvotes', 'category', 'method', 'time', 'filter',
                      'weight', 'grind', 'roast', 'temperature', 'water')

for(r in 1:length(recipe_urls)){
  recipe_url <- paste0(url, recipe_urls[r])
  print(paste0('Scraping ',r,'/', length(recipe_urls)))
  recipe_page <- read_html(recipe_url)
  # Info I want to get:
  recipe[r,1] <- html_nodes(recipe_page, "h1")
                  %>% html_text() #Name
  recipe[r,2] <- html_nodes(recipe_page, "button[title='Click to upvote']")
                  %>% html_attr('data-numvotes') %>% strtoi() #Upvotes
  recipe[r,3] <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[1]/div[2]/div[1]/a/text()')
                  %>% html_text() #Category
  recipe[r,4] <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[3]/div/div[2]/div[1]/span')
                  %>% html_text() #Method
  time <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[3]/div/div[2]/div[2]/span')
                  %>% html_text() #Time
  min <- time %>% str_extract("^\\d+") %>% strtoi() *60
  sec <- time %>% str_extract("[^:](\\d+)") %>% strtoi()
  recipe[r,5] <- min + sec
  recipe[r,6] <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[3]/div/div[2]/div[3]/span') %>% html_text() %>% str_extract("^\\w+")#Filter
  recipe[r,7] <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[3]/div/div[3]/div/div[1]/span') %>% html_text() %>% str_extract("^\\d+") %>% strtoi()#Weight
  recipe[r,8] <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[3]/div/div[3]/div/div[2]/span') %>% html_text() #Grind
  recipe[r,9] <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[3]/div/div[3]/div/div[3]/span') %>% html_text() #Roast
  recipe[r,10] <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[3]/div/div[4]/div/div[1]/span') %>% html_text() %>% str_extract("^\\d+") %>% strtoi()#Temperature
  recipe[r,11] <- html_nodes(recipe_page, xpath='/html/body/div/div[1]/div[1]/div/div[1]/div[3]/div/div[4]/div/div[2]/span') %>% html_text() %>% str_extract("^\\d+") %>% strtoi()#Water
}

write.csv(recipe, "aeroprecipes.csv", row.names=FALSE)
recipe <- read.csv('aeroprecipes.csv', sep = ',')

#############################

ggplot(data=recipe, aes(x=time, fill=category)) + geom_histogram() + xlim(c(0,2000)) + facet_wrap(~category) + theme(legend.position="none")

lm(time ~ weight + water, data=recipe)
ggplot(data=recipe) + geom_point(mapping=aes(x=weight, y=upvotes)) + geom_smooth(mapping=aes(x=weight, y=upvotes))

# Remove 0 Upvote Recipes
champion_recipe <- recipe %>% filter(category ==" Championship")
ggplot(data=liked_recipes) + geom_point(mapping=aes(x=weight, y=upvotes)) + geom_abline()

boxplot(temperature~category, recipe, xlab="Category", ylab="Water Temperature")
boxplot(time~category, log="y", recipe, xlab="Category", ylab="Brew Time")
?boxplot()

fifty_highest <- recipe %>% arrange(desc(upvotes)) %>% head(50)
ggplot() + geom_point(data=fifty_highest, aes(x=method, y=upvotes, color=category), size = 3) + scale_color_manual(values = c(" Championship" = "red", " Experimental" = "grey", " From a Barista"="grey", " From an Enthusiast"="grey"))

lm_params <- lm(upvotes ~ method + time, data = recipe)
summary(lm_params)

ggplot() + qplot(y = water, x = weight, data = recipe, facets = ~ category) + qplot(y = water, x = weight, data = fifty_highest, facets = ~category)

combo_top <- fifty_highest %>% count(method, filter) %>% mutate(new_column=c('Most Upvoted'))
combo_champion <- champion_recipe %>% count(method, filter) %>% mutate(new_column=c('Championship'))
combo_all <- recipe %>% count(method, filter) %>% mutate(new_column=c('All Recipes'))
combos <- bind_rows(combo_all, combo_champion, combo_top, .id=NULL)
combos_nototal <- combos %>% filter(new_column!= 'All Recipes')
ggplot(data=combos_nototal) + geom_col(mapping=aes(x=method, y=n, fill=filter)) + facet_wrap(~new_column)

wordcloud()
