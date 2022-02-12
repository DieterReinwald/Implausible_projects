#################################################
## Dieter Reinwald
## HarvardX - Data Science - Implausible projects - Code
## https://github.com/DieterReinwald
## 2022
#################################################

#################################################
# Preparations
#################################################

# Load required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(naniar)) install.packages("naniar", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(timeR)) install.packages("timeR", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(udpipe)) install.packages("udpipe", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(boot)) install.packages("boot", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(Rfast)) install.packages("Rfast", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(pander)) install.packages("pander", repos = "http://cran.us.r-project.org", dependencies = TRUE)
library(tidyverse)
library(data.table)
library(caret)
library(randomForest)
library(dplyr)
library(lubridate)
library(stringr)
library(bpa)
library(e1071)
library(rpart)
library(boot)
library(ggplot2)
library(readr)
library(naniar)
library(timeR)
library(stringi)
library(scales)
library(gridExtra)
library(corrplot)
library(udpipe)
library(knitr)
library(pROC)
library(ranger)
library(rmarkdown)
library(pander)

# define global font size of ggplot objects
global_size <- 9

#################################################
# Import data
#################################################

#Create timer object to track duration for different sections
mytimer <- createTimer(verbose = FALSE)
# Start timer
mytimer$start("Import data")

##############################
# service projects data
urlfile_projects = "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/projects.csv"
projects <- read_csv2(url(urlfile_projects))

##############################
# service project invoices data 2015 to 2021
urlfile_inv2015 <- "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/invoices_2015.csv"
invoices_2015 <- read_csv2(url(urlfile_inv2015))

urlfile_inv2016 <- "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/invoices_2016.csv"
invoices_2016 <- read_csv2(url(urlfile_inv2016))

urlfile_inv2017 <- "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/invoices_2017.csv"
invoices_2017 <- read_csv2(url(urlfile_inv2017))

urlfile_inv2018 <- "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/invoices_2018.csv"
invoices_2018 <- read_csv2(url(urlfile_inv2018))

urlfile_inv2019 <- "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/invoices_2019.csv"
invoices_2019 <- read_csv2(url(urlfile_inv2019))

urlfile_inv2020 <- "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/invoices_2020.csv"
invoices_2020 <- read_csv2(url(urlfile_inv2020))

urlfile_inv2021 <- "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/invoices_2021.csv"
invoices_2021 <- read_csv2(url(urlfile_inv2021))

# merge invoice data
invoices <- bind_rows(invoices_2015, invoices_2016, invoices_2017, invoices_2018, invoices_2019, invoices_2020, invoices_2021)

# remove temp objects
rm(invoices_2015, invoices_2016, invoices_2017, invoices_2018, invoices_2019, invoices_2020, invoices_2021)

##############################
# flagged projects data
urlfile_flagged <- "https://raw.githubusercontent.com/DieterReinwald/Implausible_projects/main/data/flagged_projects.csv"
flagged_projects <- read_csv2(url(urlfile_flagged))

#Stop timer
mytimer$stop("Import data")

#################################################
# Explore, clean and visualize data
#################################################

#Start timer
mytimer$start(" Explore, clean and visualize data")

## Exploratory data analysis

#############################################
### Projects data
#### Data structure

# dimension of projects data
dim(projects)

# head of project data
head(projects) %>% pander(split.tables = 100, style = "rmarkdown", caption = "Head data of table projects")

#### Initial data types

# show data types 
sapply(projects, class) %>% 
  as.data.frame() %>% 
  pander(split.tables = 100, style = "rmarkdown", caption = "Initial data types projects")

#### Mutations

# show summary project_start_date
projects %>% 
  dplyr::select(project_start_date) %>% 
  group_by(project_start_date) %>% 
  summarise(count = n()) %>% 
  pander()

# remove project_start_date 
 projects <- 
   projects %>% 
   dplyr::select(-project_start_date)

# data mutation of projects
projects <- 
   projects %>% 
     mutate(
      channel = as.factor(channel),
      correction = as.integer(correction),
      fix = as.integer(fix),
      project_create_date = as_date(project_create_date),
      pcd_year = as.integer(year(project_create_date)),
      pcd_month = as.integer(month(project_create_date)),
      pcd_day = as.integer(wday(project_create_date)),
      project_end_date = as_date(project_end_date),
      ped_year = as.integer(year(project_end_date)),
      ped_month = as.integer(month(project_end_date)),
      ped_day = as.integer(wday(project_end_date)),
      project_status = as.factor(project_status),
      blocked_for_invoice = as.integer(blocked_for_invoice),
      warranty_claim = as.integer(warranty_claim),
      flag = as.integer(0)
   )

# show year, month, and day per date types
projects %>% 
  dplyr::select(pcd_year, pcd_month, pcd_day, ped_year, ped_month, ped_day) %>% 
  head(6) %>% 
  pander(split.tables = 100, style = "rmarkdown")

# get patterns for proj_id 
 projects$proj_id %>% 
  bpa(unique_only = TRUE) %>% 
  pander()

#remove cancellation projects based on pattern
projects <- 
  projects %>%  
  filter(!str_detect(proj_id, '\\.'))

# get pattern for channel
projects$channel %>% bpa(unique_only = TRUE) %>% pander()

# show patterns where frequency > 0 
match_pattern(projects$channel, pattern = "AAAA", unique_only = FALSE) %>% 
  table %>% 
  as.data.frame() %>% 
  filter(Freq > 0) %>%
  pander()

# remove NULL value from channels
projects <- 
  projects %>% 
  filter(!str_detect(channel, 'NULL'))

#### Summary

# data exploration projects
summary(projects) %>% pander(split.tables = 100, style = "rmarkdown", caption = "Head data of table projects (after mutation)")

#############################################
### Invoices data
#### Data structure

# dimension of invoices data
dim(invoices)

# head of invoices data
head(invoices) %>% pander(split.tables = 100, style = "rmarkdown", caption = "Head data of table invoices")

#### Initial data types

# data types
sapply(invoices, class) %>% 
  as.data.frame() %>% 
  distinct() %>% 
  pander(split.tables = 100, style = "rmarkdown", caption = "Initial data types invoices")

#### Mutation

# data mutation of invoices
invoices <- 
   invoices %>% 
      mutate(
        order_account = as.integer(order_account),
        invoice_create_date = as_date(invoice_create_date),
        icd_year = as.integer(year(invoice_create_date)),
        icd_month = as.integer(month(invoice_create_date)),
        icd_day = as.integer(wday(invoice_create_date)),
        invoice_date = as_date(invoice_date), 
        invd_year = as.integer(year(invoice_date)),
        invd_month = as.integer(month(invoice_date)),
        invd_day = as.integer(wday(invoice_date)),
        due_date = as_date(due_date), 
        dd_year = as.integer(year(due_date)),
        dd_month = as.integer(month(due_date)),
        dd_day = as.integer(wday(due_date)),
        qty = as.numeric(qty),
        line_amount = as.numeric(line_amount),
        line_discount = as.numeric(line_discount),
        net_amount = as.numeric(net_amount),
        total_discount = as.numeric(total_discount),
        markup = as.numeric(markup),
        tax = as.numeric(tax),
        gross_amount = as.numeric(gross_amount),
        weight = as.numeric(weight),
        type = as.factor(type),
        category = as.factor(category),
        empl_item_detail = as.factor(empl_item_detail)
      ) %>% 
    dplyr::select(-volume)

# show year, month, and day per date types
invoices %>% 
  dplyr::select(icd_year, icd_month, icd_day, invd_year, invd_month, invd_day, dd_year, dd_month, dd_day) %>% 
  head(6) %>% 
  pander(split.tables = 100, style = "rmarkdown")

#basic pattern analysis proj_invoice_id
invoices$proj_invoice_id %>% 
  bpa(unique = TRUE) %>% 
  pander()

# remove cases from proj_invoice_id and category
invoices <- 
  invoices %>%   
  filter(!str_detect(proj_invoice_id, 'NULL')) %>% # remove value NULL
  filter(!stri_isempty(proj_invoice_id) == TRUE) %>% # remove empty strings
  filter(!str_detect(proj_invoice_id, 'GS-')) %>% # remove credit notes
  filter(!str_detect(category, 'NULL')) # remove value NULL for category

# get all unique empl_item_detail values and create an Id column
empl_item_detail_id <- 
  invoices %>% 
  dplyr::select(empl_item_detail) %>% 
  distinct() %>% 
  mutate(empl_item_detail_id = row_number())

# join with invoices with new table
invoices <- inner_join(invoices, empl_item_detail_id, by = "empl_item_detail")

# replace NAs with value 0 for integer and numeric values
invoices <- 
  invoices %>% 
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

#### Summary

# data exploration invoices
summary(invoices) %>% pander(split.tables = 100, style = "rmarkdown", caption = "Head data of table invoices (after mutation)")

#############################################
### Flagged projects data
#### Data structure

# dimension of flagged projects data
dim(flagged_projects)

# head of flagged_projects data
head(flagged_projects) %>% pander(split.tables = 100, style = "rmarkdown")

#### Initial data types
class(flagged_projects$proj_id)

#### Mutation
# no mutations done 

#### Summary
unique_flagged <- 
  flagged_projects %>% 
  dplyr::select(proj_id) %>% 
  distinct() %>% 
  nrow()

# data exploration flagged_projects
summary(flagged_projects) %>% 
  pander(split.tables = 100, style = "rmarkdown")

#############################################
## Preprocessing

# create unique flagged project ids
unique_flagged_project_ids <- 
  flagged_projects %>% 
  dplyr::select(proj_id) %>% 
  distinct()

# flag projects in list of all projects based on flagged proj_ids
for (i in projects$proj_id) {
  if (i %in% unique_flagged_project_ids$proj_id) {
    projects$flag[projects$proj_id == i] <- 1
  }
}

# result table after flagging
table(projects$flag) %>% 
  pander()

# define number of flagged and unflagged projects 
nb_flagged_projects <- sum(projects$flag == 1)
nb_unflagged_projects <- sum(projects$flag == 0)

# plot flagged projects over years
projects %>% filter(flag == 1) %>% 
  group_by(pcd_year) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(pcd_year, count)) +
  geom_bar(stat="identity") +
  labs(title = "Flagged projects over years", x = "year", y = "count") + 
  theme(text = element_text(size=global_size))

# join projects with invoice data
projects_invoices <- 
  inner_join(projects, invoices, by = "proj_invoice_projid") %>% distinct()

#############################################
## Data visualization 

### project_id
# create plot for projects and invoices relationship
projects_invoices %>% 
  dplyr::select(proj_id, proj_invoice_id) %>% 
  group_by(proj_id) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(count)) + 
  geom_bar(color = "black", stat = "count") + 
  labs(title = "Distribution invoices per projects", x = "invoice per project", y = "count") + 
  theme(text = element_text(size=global_size))

### channel 
# create plot for channel distribution
projects_invoices %>% 
  group_by(channel) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(channel = reorder(channel, count)) %>%
  ggplot(aes(channel, count)) + #, y = ..prop..)) +
  geom_bar(color = "black", stat = "identity") + 
  coord_flip() +
  labs(title = "Distribution of projects per channel", x = "channel", y = "count") + 
  theme(text = element_text(size=global_size))

### correction and fix projects
# create plot for correction projects
p1 <- 
  projects_invoices %>% 
  ggplot(aes(correction)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  scale_x_discrete(limits = c(0,1)) + 
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribution of correction projects", x = "correction", y = "percentage") + 
  theme(text = element_text(size=global_size))

# create plot for fix projects
p2 <- projects_invoices %>% 
  ggplot(aes(fix)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  scale_x_discrete(limits = c(0,1)) + 
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribution of fix projects", x = "fix", y = "percentage") + 
  theme(text = element_text(size=global_size))

# plot combined plots
grid.arrange(p1, p2, nrow = 2)

### project_create_date and project_end_date
# projects created - date perspective
projects_invoices %>% 
  group_by(project_create_date) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(project_create_date, n)) +
  geom_line() +
  labs(title = "Development of projects created", x = "date", y = "count") + 
  theme(text = element_text(size=global_size))

# projects starting from 2015 based on explanations
projects_invoices <- 
  projects_invoices %>% 
  filter(project_create_date >= '2015-01-01')

# projects created - monthly perspective
projects_invoices %>% 
  ggplot(aes(pcd_month)) +
  geom_bar(stat="count")+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  labs(title = "Comparison of projects created per month", x = "month", y = "count") + 
  theme(text = element_text(size=global_size))

# projects end - year perspective
projects_invoices %>% 
  group_by(ped_year) %>% 
  summarize(count = n()) %>% 
  pander(split.tables = 100, style = "rmarkdown")

# projects end - year perspective
projects_invoices %>% 
  filter(ped_year >= 2015) %>% 
  ggplot(aes(ped_year)) +
  geom_bar() + 
  labs(title = "Project end dates per year", x = "year", y = "count") + 
  theme(text = element_text(size=global_size))

# projects end - monthly perspective
projects_invoices %>% 
  ggplot(aes(ped_month)) +
  geom_bar(stat="count")+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  labs(title = "Project end dates per month", x = "month", y = "count") + 
  theme(text = element_text(size=global_size))

# projects end - daily perspective for January
projects_invoices %>%
  filter(ped_month == 1 & ped_year >= 2015) %>%
  mutate(project_end_day_jan = day(project_end_date)) %>% 
  ggplot(aes(project_end_day_jan)) +
  geom_bar(stat="count") +
  labs(title = "Projects end daily analysis for January", x = "day", y = "count") + 
  theme(text = element_text(size=global_size))

### project_status
# plot project status
projects_invoices %>%
  dplyr::select(project_status) %>% 
  ggplot(aes(project_status)) +
  geom_bar() +
  labs(title = "Project status", x = "proejct status", y = "count") +
  coord_flip() + 
  theme(text = element_text(size=global_size))

### cust_account
# top 20 customer accounts
projects_invoices %>% 
  group_by(cust_account) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20, count) %>% 
  ggplot(aes(count, reorder(cust_account, count))) +
  geom_bar(color = "black", stat = "identity") +
  labs(title = "Top 20 customer accounts", x = "cust_account", y = "count") + 
  theme(text = element_text(size=global_size))

# define top 20 cust_accounts
top_20_cust_accounts <- projects_invoices %>% 
  group_by(cust_account) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20, count) %>% 
  dplyr::select(cust_account)

# plot relationship between top 20 cust_accounts and channel usage
top_20_cust_accounts %>%
  inner_join(projects_invoices, by = "cust_account") %>% 
  dplyr::select(cust_account, channel) %>% 
  group_by(cust_account, channel) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(channel, count)) + 
  geom_bar(stat="identity") + 
  facet_wrap(. ~ cust_account) +
  scale_x_discrete(label=abbreviate) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 20 customer accounts and channel usage", x = "cust_account", y = "count") + 
  theme(text = element_text(size=global_size))

### order_account
# plot top 20 order accounts
projects_invoices %>% 
  group_by(order_account) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20, count) %>% 
  ggplot(aes(count, reorder(order_account, count))) +
  geom_bar(color = "black", stat = "identity") +
  labs(title = "Top 20 order accounts", x = "order_account", y = "count") + 
  theme(text = element_text(size=global_size))

# define top 20 order accounts
top_20_order_accounts <- projects_invoices %>% 
  group_by(order_account) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20, count) %>% 
  dplyr::select(order_account)

# plot relationship between top 20 order accounts and channel usage
top_20_order_accounts %>%
  inner_join(projects_invoices, by = "order_account") %>% 
  dplyr::select(order_account, channel) %>% 
  group_by(order_account, channel) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(channel, count)) + 
  geom_bar(stat="identity") + 
  facet_wrap(. ~ order_account) +
  scale_x_discrete(label=abbreviate) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Top 20 order accounts and channel usage", x = "order_account", y = "count") + 
  theme(text = element_text(size=global_size))

### invoice_account
top_20_inv_accounts <- projects_invoices %>% 
  group_by(invoice_account) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20, count) %>% 
  dplyr::select(invoice_account)

# top 20 invoice_accounts
projects_invoices %>% 
  group_by(invoice_account) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20, count) %>% 
  ggplot(aes(count, reorder(invoice_account, count))) +
  geom_bar(color = "black", stat = "identity") +
  labs(title = "Top 20 invoice accounts", x = "invoice_account", y = "count") + 
  theme(text = element_text(size=global_size))

# plot top 20 invoice_account and channels, fig.width=8, fig.height=6}
top_20_inv_accounts %>%
  inner_join(projects_invoices, by = "invoice_account") %>% 
  dplyr::select(invoice_account, channel) %>% 
  group_by(invoice_account, channel) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(channel, count)) + 
  geom_bar(stat="identity") + 
  facet_wrap(. ~ invoice_account) +
  scale_x_discrete(label=abbreviate) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 20 invoices accounts and channel usage", x = "invoice_account", y = "count") + 
  theme(text = element_text(size=global_size))

### blocked_for_invoice
# table blocked_for_invoice 
projects_invoices %>%
  dplyr::select(blocked_for_invoice) %>% 
  group_by(blocked_for_invoice) %>% 
  summarise(count = n()) %>% 
  pander(split.tables = 100, style = "rmarkdown")

### warranty_claim
# table warranty_claim
projects_invoices %>%
  dplyr::select(warranty_claim) %>% 
  group_by(warranty_claim) %>% 
  summarise(count = n()) %>% 
  pander(split.tables = 100, style = "rmarkdown")

### proj_invoice_projid and proj_invoice_id
#proj_invoice_projid pattern
projects_invoices$proj_invoice_projid %>% 
  bpa(unique = TRUE) %>% 
  pander(split.tables = 100, style = "rmarkdown")

#proj_invoice_id pattern
projects_invoices$proj_invoice_id %>% 
  bpa(unique = TRUE) %>% 
  pander(split.tables = 100, style = "rmarkdown")

### invoice_create_date
# invoices created - yearly perspective
projects_invoices %>% 
  filter(icd_year >= 2015) %>% 
  ggplot(aes(icd_year)) +
  geom_bar() +
  labs(title = "Development of invoices created per year ", x = "year", y = "count") + 
  theme(text = element_text(size=global_size))

# invoices created - monthly perspective
projects_invoices %>% 
  ggplot(aes(icd_month)) +
  geom_bar(stat="count")+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  labs(title = "Development of invoice created per month", x = "month", y = "count") + 
  theme(text = element_text(size=global_size))

### invoice_date
# invoice date - yearly perspective
projects_invoices %>% 
  filter(invd_year >= 2015) %>% 
  ggplot(aes(invd_year)) +
  geom_bar() +
  labs(title = "Development of invoice dates per year", x = "year", y = "count") + 
  theme(text = element_text(size=global_size))

# invoice dates - monthly perspective
projects_invoices %>% 
  ggplot(aes(invd_month)) +
  geom_bar(stat="count")+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  labs(title = "Development of invoice dates per month", x = "month", y = "count") + 
  theme(text = element_text(size=global_size))

### due_date
# due date - yearly perspective
projects_invoices %>% 
  filter(dd_year >= 2015) %>% 
  ggplot(aes(dd_year)) +
  geom_bar() +
  labs(title = "Development of due date per year", x = "year", y = "count") + 
  theme(text = element_text(size=global_size))

# due dates - monthly perspective
projects_invoices %>% 
  ggplot(aes(dd_month)) +
  geom_bar(stat="count")+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(title = "Development of due date per month",x = "month", y = "count") + 
  theme(text = element_text(size=global_size))

### invoice amounts
#### General analysis 
# create data frame for correlation based on invoice amounts
d <- data.frame(
  qty = projects_invoices$qty,
  line_discount = projects_invoices$line_discount,
  line_amount = projects_invoices$line_amount,
  net_amount = projects_invoices$net_amount,
  total_discount = projects_invoices$total_discount,
  markup = projects_invoices$markup,
  tax = projects_invoices$tax, 
  gross_amount = projects_invoices$gross_amount
)

#create corrplot for invoice amounts
corrplot(cor(d, use = "pairwise.complete.obs"), method = "number") 

#### (a) qty
# summary qty
summary(projects_invoices$qty) %>% 
  pander(split.tables = 100, style = "rmarkdown")

# histogram qty
projects_invoices %>% 
  dplyr::select(qty) %>%
  ggplot(aes(qty)) +
  geom_histogram(color = "black") + 
  labs(title = "Histogram quantity") + 
  theme(text = element_text(size=global_size))

#### (b) line_discount
# histogram line discount
projects_invoices %>% 
  dplyr::select(line_discount) %>%
  ggplot(aes(line_discount)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram line_discount") + 
  theme(text = element_text(size=global_size))

#### (c) line_amount
# histogram line_amount
projects_invoices %>% 
  dplyr::select(line_amount) %>%
  ggplot(aes(line_amount)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram line_amount") + 
  theme(text = element_text(size=global_size))

#### (d) net_amount
# histogram net_amount
projects_invoices %>% 
  dplyr::select(net_amount) %>%
  ggplot(aes(net_amount)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram net_amount") + 
  theme(text = element_text(size=global_size))

# histogram net_amount > 0
projects_invoices %>% 
  dplyr::select(net_amount) %>%
  filter(net_amount > 0) %>% 
  ggplot(aes(net_amount)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram net_amount > 0") + 
  theme(text = element_text(size=global_size))

# define variables
net_amount_mean <- mean(projects_invoices$net_amount, na.rm = TRUE)
net_amount_sd <- sd(projects_invoices$net_amount, na.rm = TRUE)

#### (e) total_discount
# histogram total_discount
projects_invoices %>% 
  dplyr::select(total_discount) %>%
  ggplot(aes(total_discount)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram total_discount") + 
  theme(text = element_text(size=global_size))

projects_invoices %>% 
  group_by(total_discount) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10) %>% 
  pander(split.tables = 100, style = "rmarkdown")

#### (f) markup
# histogram total_discount
projects_invoices %>% 
  dplyr::select(markup) %>%
  ggplot(aes(markup)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram markup") + 
  theme(text = element_text(size=global_size))

# summary markup
summary(projects_invoices$markup) %>% 
  pander(split.tables = 100, style = "rmarkdown")

#### (g) tax
# histogram tax
projects_invoices %>% 
  dplyr::select(tax) %>%
  ggplot(aes(tax)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram tax") + 
  theme(text = element_text(size=global_size))

#### (h) gross_amount
# histogram net_amount
projects_invoices %>% 
  dplyr::select(gross_amount) %>%
  ggplot(aes(gross_amount)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram gross_amount") + 
  theme(text = element_text(size=global_size))

### weight
# histogram weight
projects_invoices %>% 
  dplyr::select(weight) %>%
  ggplot(aes(weight)) +
  geom_histogram(color = "black", binwidth = 100) + 
  labs(title = "Histogram weight") + 
  theme(text = element_text(size=global_size))

# summary weight
summary(projects_invoices$weight) %>% 
  pander(split.tables = 100, style = "rmarkdown")

# remove weight from data 
projects_invoices <- 
  projects_invoices %>% 
  dplyr::select(-weight)

### type
# plot type
projects_invoices %>% 
  dplyr::select(type) %>% 
  ggplot(aes(type)) +
  geom_bar(color = "black") + 
  coord_flip() + 
  labs(title = "Distribution of type", x = "type", y = "count") + 
  theme(text = element_text(size=global_size))

### category
# plot category
projects_invoices %>% 
  dplyr::select(category, type) %>% 
  ggplot(aes(category)) +
  geom_bar(color = "black") +
  scale_x_discrete(label=abbreviate) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(type ~ . ) +
  labs(title = "Distribution of categories per type") + 
  theme(text = element_text(size=global_size))

### empl_item_detail
# plot top 20 empl_item_details - type empl
p1 <- projects_invoices %>% 
  filter(type == "EMPL") %>% 
  group_by(empl_item_detail) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20, count) %>% 
  ggplot(aes(count, reorder(empl_item_detail, count))) +
  geom_bar(color = "black", stat = "identity") +
  labs(title = "Top 20 workers", y = "worker_id") + 
  theme(text = element_text(size=global_size))

# plot top 20 empl_item_details - type item
p2 <- projects_invoices %>% 
  filter(type == "ITEM") %>% 
  group_by(empl_item_detail) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20, count) %>% 
  ggplot(aes(count, reorder(empl_item_detail, count))) +
  geom_bar(color = "black", stat = "identity") +
  scale_x_continuous(breaks = c(100000, 200000)) +
  labs(title = "Top 20 items", y = "item_id") + 
  theme(text = element_text(size=global_size))

# plot total plot
grid.arrange(p1, p2, nrow=1)

#Stop timer
mytimer$stop(" Explore, clean and visualize data")

######################################
# Modeling approach
######################################

#Start timer
mytimer$start("Modeling approach")

# clean data not required for modeling approach
rm(d, flagged_projects, invoices, p1, p2, projects, top_20_cust_accounts, top_20_inv_accounts, top_20_order_accounts, unique_flagged_project_ids)

#############################################
## Initialization of modeling

# clean data from date and character types
model_data <- 
  projects_invoices %>% 
  dplyr::select(-proj_id, -proj_invoice_projid, -proj_invoice_id, -project_end_date, -invoice_date, -due_date, -invoice_create_date, -empl_item_detail, -cust_account, -invoice_account, -order_account)

#############################################
## Smoothplots for single predictors
# create channel id from row_number 
channel_id <- 
  model_data %>% 
  dplyr::select(channel) %>% 
  distinct() %>% 
  mutate(channel_id = row_number())

# replace channel with channel id in data and remove variable channel 
model_data <- 
  model_data %>% 
  inner_join(channel_id, by = "channel") %>% 
  dplyr::select(-channel)

# plot smoothplot channel
sp_channel <- 
  model_data %>% 
  group_by(channel_id) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = channel_id, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot channel") + 
  theme(text = element_text(size=global_size))

# plot smoothplot correction
sp_correction <- 
  model_data %>% 
  group_by(correction) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = correction, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot correction") + 
  theme(text = element_text(size=global_size))

# plot smoothplot fix
sp_fix <-
  model_data %>% 
  group_by(fix) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = fix, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot fix") + 
  theme(text = element_text(size=global_size))

# create project status id from row_number 
project_status_id <- 
  model_data %>% 
  dplyr::select(project_status) %>% 
  distinct() %>% 
  mutate(project_status_id = row_number())

# replace project status with project status id in data and remove variable project status
model_data <- 
  model_data %>% 
  inner_join(project_status_id, by = "project_status") %>% 
  dplyr::select(-project_status)

# plot smoothplot project status 
sp_project_status <-
  model_data %>% 
  group_by(project_status_id) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = project_status_id, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot project status") + 
  theme(text = element_text(size=global_size))

# create type id from row_number 
type_id <- 
  model_data %>% 
  dplyr::select(type) %>% 
  distinct() %>% 
  mutate(type_id = row_number())

# replace type with type id in data and remove variable type 
model_data <- 
  model_data %>% 
  inner_join(type_id, by = "type") %>% 
  dplyr::select(-type)

# plot smoothplot type
sp_type <- 
  model_data %>% 
  group_by(type_id) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = type_id, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot type") + 
  theme(text = element_text(size=global_size))

# create category id from row_number 
category_id <- 
  model_data %>% 
  dplyr::select(category) %>% 
  distinct() %>% 
  mutate(category_id = row_number())

# replace category with category id in data and remove variable category
model_data <- 
  model_data %>% 
  inner_join(category_id, by = "category") %>% 
  dplyr::select(-category)

# plot smoothplot category
sp_category <- 
  model_data %>% 
  group_by(category_id) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = category_id, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot category") + 
  theme(text = element_text(size=global_size))

# plot smoothplot blocked for invoice
sp_blocked <- 
  model_data %>% 
  group_by(blocked_for_invoice) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = blocked_for_invoice, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot blocked_for_invoice") + 
  theme(text = element_text(size=global_size))

# plot smoothplot warranty claim
sp_warranty <- 
  model_data %>% 
  group_by(warranty_claim) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = warranty_claim, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot warranty_claim") + 
  theme(text = element_text(size=global_size))

# plot smoothplot pcd_year
sp_pcd_year <-
  model_data %>% 
  group_by(pcd_year) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = pcd_year, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot pcd_year") + 
  theme(text = element_text(size=global_size))

# plot smoothplot pcd_month
sp_pcd_month <-model_data %>% 
  group_by(pcd_month) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = pcd_month, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot pcd_month") + 
  theme(text = element_text(size=global_size))

# plot smoothplot pcd_day
sp_pcd_day <- 
  model_data %>% 
  group_by(pcd_day) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = pcd_day, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot pcd_day") + 
  theme(text = element_text(size=global_size))

# plot smoothplot ped_year
sp_ped_year <- 
  model_data %>% 
  filter(ped_year >= 2015) %>% 
  group_by(ped_year) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = ped_year, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot ped_year") + 
  theme(text = element_text(size=global_size))

# plot smoothplot ped_month
sp_ped_month <- 
  model_data %>% 
  group_by(ped_month) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = ped_month, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot ped_month") + 
  theme(text = element_text(size=global_size))

# plot smoothplot ped_day
sp_ped_day <- 
  model_data %>% 
  group_by(ped_day) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = ped_day, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot ped_day") + 
  theme(text = element_text(size=global_size))

# plot smoothplot net_amount
sp_net_amount <- 
  model_data %>% 
  group_by(net_amount) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = net_amount, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot net_amount") + 
  theme(text = element_text(size=global_size))

# plot smoothplot line_discount
sp_line_discount <-
  model_data %>% 
  group_by(line_discount) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = line_discount, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot line_discount") + 
  theme(text = element_text(size=global_size))

# plot smoothplot total_discount
sp_total_discount <- 
  model_data %>% 
  group_by(total_discount) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = total_discount, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot total_discount") + 
  theme(text = element_text(size=global_size))

# plot smoothplot markup
sp_markup <- 
  model_data %>% 
  group_by(markup) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = markup, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot markup") + 
  theme(text = element_text(size=global_size))

# plot smoothplot tax
sp_tax <- 
  model_data %>% 
  group_by(tax) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = tax, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot tax") + 
  theme(text = element_text(size=global_size))

# plot smoothplot gross_amount
sp_gross_amount <- 
  model_data %>% 
  group_by(gross_amount) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = gross_amount, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot gross_amount") + 
  theme(text = element_text(size=global_size))

# plot smoothplot qty
sp_qty <- 
  model_data %>% 
  group_by(qty) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = qty, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot qty") + 
  theme(text = element_text(size=global_size))

# plot smoothplot line_amount
sp_line_amount <- 
  model_data %>% 
  group_by(line_amount) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = line_amount, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot line_amount") + 
  theme(text = element_text(size=global_size))

# plot smoothplot icd_year
sp_icd_year <- 
  model_data %>% 
  group_by(icd_year) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = icd_year, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot icd_year") + 
  theme(text = element_text(size=global_size))

# plot smoothplot icd_month
sp_icd_month <- 
  model_data %>% 
  group_by(icd_month) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = icd_month, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot icd_month") + 
  theme(text = element_text(size=global_size))

# plot smoothplot icd_day
sp_icd_day <-
  model_data %>% 
  group_by(icd_day) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = icd_day, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot icd_day") + 
  theme(text = element_text(size=global_size))

# plot smoothplot invd_year
sp_invd_year <- 
  model_data %>% 
  group_by(invd_year) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = invd_year, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot invd_year") + 
  theme(text = element_text(size=global_size))

# plot smoothplot invd_month
sp_invd_month <- 
  model_data %>% 
  group_by(invd_month) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = invd_month, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot invd_month") + 
  theme(text = element_text(size=global_size))

# plot smoothplot invd_day
sp_invd_day <- 
  model_data %>% 
  group_by(invd_day) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = invd_day, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot invd_day") + 
  theme(text = element_text(size=global_size))

# plot smoothplot dd_year
sp_due_year <-
  model_data %>% 
  group_by(dd_year) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = dd_year, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot dd_year") + 
  theme(text = element_text(size=global_size))

# plot smoothplot dd_month
sp_due_month <-
  model_data %>% 
  group_by(dd_month) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = dd_month, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot dd_month") + 
  theme(text = element_text(size=global_size))

# plot smoothplot dd_day
sp_due_day <- 
  model_data %>% 
  group_by(dd_day) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = dd_day, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot dd_day") + 
  theme(text = element_text(size=global_size))

# plot smoothplot empl_item_detail_id
sp_empl_item_detail <- 
  model_data %>% 
  group_by(empl_item_detail_id) %>% 
  summarize(n = n(), 
            avg = mean(flag), 
            med = median(flag),
            se = sd(flag)) %>% 
  ggplot(aes(x = empl_item_detail_id, y = avg, ymin = avg - se, ymax = avg + se, col="avg")) + 
  geom_point() +
  geom_point(aes(y=med, col = "median")) + 
  geom_errorbar(aes(alpha= 0.3), show.legend = FALSE) + 
  geom_smooth() +
  labs(title = "Smoothplot empl_item_detail_id") + 
  theme(text = element_text(size=global_size))

# plot combined plots
grid.arrange(sp_channel, sp_correction, sp_fix, sp_project_status, sp_type, sp_category, sp_blocked, sp_warranty, sp_empl_item_detail, sp_qty, sp_line_discount, sp_line_amount, ncol = 2)

grid.arrange(sp_net_amount, sp_total_discount, sp_markup, sp_tax, sp_gross_amount, sp_pcd_year, sp_pcd_month, sp_pcd_day, sp_ped_year, sp_ped_month, sp_ped_day, sp_icd_year, ncol = 2)

grid.arrange(sp_icd_month, sp_icd_day, sp_invd_year, sp_invd_month, sp_invd_day, sp_due_year, sp_due_month, sp_due_day, ncol = 2)

# clean objects 
rm(sp_channel, sp_correction, sp_fix, sp_project_status, sp_type, sp_category, sp_blocked, sp_warranty, sp_empl_item_detail, sp_pcd_year, sp_pcd_month, sp_pcd_day, sp_ped_year, sp_ped_month, sp_ped_day, sp_icd_year, sp_icd_month, sp_icd_day, sp_invd_year, sp_invd_month, sp_invd_day, sp_due_year, sp_due_month, sp_due_day, sp_qty, sp_line_discount, sp_line_amount, sp_net_amount, sp_total_discount, sp_markup, sp_tax, sp_gross_amount)

model_data_selection <- 
  model_data %>% 
  dplyr::select(channel_id, category_id, net_amount, line_discount, total_discount, markup, tax, qty, line_amount, empl_item_detail_id, flag, project_create_date)

#############################################
## Feature scaling - normalization
#change data type flag to factor
model_data <- 
  model_data_selection %>% 
  mutate(flag = as.factor(flag))

# normalize the numerical data
model_data_norm.pre <- 
  model_data %>% 
  dplyr::select(-flag) %>% 
  preProcess(method = "range")

# get normalized values
model_data_norm <- predict(model_data_norm.pre, model_data)

# remove duplicate values
model_data_norm <- model_data_norm %>% distinct()

# remove temp variables
rm(model_data_norm.pre)

# show normalized numerical data
model_data_norm %>% 
  dplyr::select(-project_create_date, -flag) %>% 
  head(10) %>% 
  pander(split.tables = 100, style = "rmarkdown", caption = "normalized data")

#############################################
## Feature selection
#near zero variation analysis
nzv <- 
  model_data_norm %>% 
  dplyr::select(-flag, -project_create_date) %>% 
  nearZeroVar(saveMetrics = TRUE, names = TRUE) 

nzv %>% 
  dplyr::filter(nzv == TRUE) %>% 
  pander()

model_data_norm <- 
  model_data_norm %>% dplyr::select(-net_amount, -line_discount, -total_discount, -markup, -tax)

#############################################
## Data splitting
# define train data set (all projects before 2021)
base <- 
  model_data_norm %>% 
  filter(project_create_date < '2021-01-01') %>% 
  dplyr::select(-project_create_date)

# split base data set in train and test set
test_index <- createDataPartition(y = base$flag, p = 0.1, times = 1, list = FALSE)
train <- base[-test_index,]
test <- base[test_index,]

# define validation data set
validation <- 
  model_data_norm %>% 
  filter(project_create_date >= '2021-01-01') %>% 
  dplyr::select(-project_create_date)

#Stop timer
mytimer$stop("Modeling approach")

#####################################################
# Train and compare different models
#####################################################

#Start timer
mytimer$start("Train and compare different models")

# Cross validation 
trControl <- trainControl(method = "cv", number = 10)

#############################################
## Model 1 - Naive bayes

#Start timer
mytimer$start("Naive bayes")

# define data frame for results
results <- data.frame(Model = integer(), 
                      Approach = character(), 
                      Accuracy = numeric(),
                      Sensitivity = numeric(),
                      Specificity = numeric(),
                      Precision = numeric(),
                      Recall = numeric(), 
                      F1 = numeric())

# train model 
model_nb <- naiveBayes(flag ~ ., data = train, trControl = trcontrol)

# predict values
test$pred_nb <- predict(model_nb, test)

# create confusion matrix 
cm_nb <- confusionMatrix(test$flag, test$pred_nb)

# gather results
temp_model_accuracy <- cm_nb$overall["Accuracy"]
temp_model_sensitivity <- cm_nb$byClass["Sensitivity"]
temp_model_specificity <- cm_nb$byClass["Specificity"]
temp_model_precision <- cm_nb$byClass["Precision"]
temp_model_recall <- cm_nb$byClass["Recall"]
temp_model_f1 <- cm_nb$byClass["F1"]

results <- bind_rows(results, 
                     data.frame(Model = 1,
                                Approach = "Naive Bayes",
                                Accuracy = temp_model_accuracy,
                                Sensitivity = temp_model_sensitivity, 
                                Specificity = temp_model_specificity,
                                Precision = temp_model_precision, 
                                Recall = temp_model_recall,
                                F1 = temp_model_f1
                     ))

results %>% as.tbl() %>% as.data.frame()
pander(split.tables = 130, style = "rmarkdown")

# plot the confusion matrix results
table <- data.frame(cm_nb$table)

plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

cm_nb_plot <- 
  ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference))) + 
  labs(title = "confusion matrix - naive bayes") + 
  theme(text = element_text(size=global_size))
cm_nb_plot

#Stop timer
mytimer$stop("Naive bayes")

#############################################
## Model 2- Logistic regression

#Start timer
mytimer$start("Logistic regression")

# train model
model_logm <- glm(flag ~ ., data = train, family = "binomial")

# cross validation
model_logm_cv <- cv.glm(train, model_logm, K = 10)

# predict values on test set
predict_model_logm_prob <- predict(model_logm, test, type = "response")

# changing probabilities
test$pred_logm <- as.factor(ifelse(predict_model_logm_prob > 0.5, 1, 0))

# create the confusion matrix
cm_logm <- confusionMatrix(test$flag, test$pred_logm)

# gather results
temp_model_accuracy <- cm_logm$overall["Accuracy"]
temp_model_sensitivity <- cm_logm$byClass["Sensitivity"]
temp_model_specificity <- cm_logm$byClass["Specificity"]
temp_model_precision <- cm_logm$byClass["Precision"]
temp_model_recall <- cm_logm$byClass["Recall"]
temp_model_f1 <- cm_logm$byClass["F1"]

results <- bind_rows(results, 
                     data.frame(Model = 2,
                                Approach = "Logistic Regression",
                                Accuracy = temp_model_accuracy,
                                Sensitivity = temp_model_sensitivity, 
                                Specificity = temp_model_specificity,
                                Precision = temp_model_precision, 
                                Recall = temp_model_recall,
                                F1 = temp_model_f1
                     ))

results %>% as.tbl() %>% as.data.frame() %>% 
  pander(split.tables = 130, style = "rmarkdown")

# calculate probs for train model, show ROC curve and calculate auc 
predict_model_logm_prob <- predict(model_logm, test, type = "response")

# calculate ROC
ROC <- roc(test$flag, predict_model_logm_prob)

#calculate auc value
auc_logm <- auc(ROC)

# plot ROC curve
plot(ROC, col = "blue") + 
  theme(text = element_text(size=global_size))

# create data frame
table <- data.frame(cm_logm$table)

# preparations
plotTable <- table %>%
  mutate(good_bad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

# plot confusion matrix
cm_logm_plot <- 
  ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = good_bad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference))) + 
  labs(title = "confusion matrix - logistic regression") + 
  theme(text = element_text(size=global_size))
cm_logm_plot

#############################################
# specify a null model with no predictors
null_model <- glm(flag ~ 1, data = train, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(flag ~ ., data = train, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the stepwise flag probability
step_prob <- predict(step_model, test, type = "response")

# Plot the ROC of the stepwise model
ROC <- roc(test$flag, step_prob)

plot(ROC, col = "red") + 
  theme(text = element_text(size=global_size))

#calculate auc value
auc_logm2 <- auc(ROC)

#Stop timer
mytimer$stop("Logistic regression")

#############################################
## Model 3 - Decision tree
# train model
model_dt <- rpart(flag ~ ., data = train, method = "class", control = rpart.control(cp = 0))

# predict values
test$pred_dt <- predict(model_dt, test, type = "class")

# create the confusion matrix
cm_dt <- confusionMatrix(test$flag, test$pred_dt)

# gather results
temp_model_accuracy <- cm_dt$overall["Accuracy"]
temp_model_sensitivity <- cm_dt$byClass["Sensitivity"]
temp_model_specificity <- cm_dt$byClass["Specificity"]
temp_model_precision <- cm_dt$byClass["Precision"]
temp_model_recall <- cm_dt$byClass["Recall"]
temp_model_f1 <- cm_dt$byClass["F1"]

results <- bind_rows(results, 
                     data.frame(Model = 3,
                                Approach = "Decision Tree",
                                Accuracy = temp_model_accuracy,
                                Sensitivity = temp_model_sensitivity, 
                                Specificity = temp_model_specificity,
                                Precision = temp_model_precision, 
                                Recall = temp_model_recall,
                                F1 = temp_model_f1
                     ))

results %>% as.tbl() %>% as.data.frame() %>% 
  pander(split.tables = 130, style = "rmarkdown")

# create data frame
table <- data.frame(cm_dt$table)

# preparations
plotTable <- table %>%
  mutate(good_bad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

# plot confusion matrix
cm_dt_plot <- 
  ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = good_bad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference))) + 
  labs(title = "confusion matrix - decision tree") + 
  theme(text = element_text(size=global_size))
cm_dt_plot

#############################################
## Model 4 - Random forest
#Start timer
mytimer$start("Random forest")

# train model
model_rf <- ranger(flag ~ ., data = train, importance = "impurity")

# predict values
predict_model_rf <- predict(model_rf, test)
test$pred_rf <- predict_model_rf$predictions

# create confusion matrix
cm_rf <- confusionMatrix(test$flag, test$pred_rf)

# gather results
temp_model_accuracy <- cm_rf$overall["Accuracy"]
temp_model_sensitivity <- cm_rf$byClass["Sensitivity"]
temp_model_specificity <- cm_rf$byClass["Specificity"]
temp_model_precision <- cm_rf$byClass["Precision"]
temp_model_recall <- cm_rf$byClass["Recall"]
temp_model_f1 <- cm_rf$byClass["F1"]

results <- bind_rows(results, 
                     data.frame(Model = 4,
                                Approach = "Random Forest",
                                Accuracy = temp_model_accuracy,
                                Sensitivity = temp_model_sensitivity, 
                                Specificity = temp_model_specificity,
                                Precision = temp_model_precision, 
                                Recall = temp_model_recall,
                                F1 = temp_model_f1
                     ))

results %>% as.tbl() %>% as.data.frame() %>% 
  pander(split.tables = 130, style = "rmarkdown")

# plot the confusion matrix results
table <- data.frame(cm_rf$table)

plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

cm_rf_plot <- 
  ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference))) +
  labs(title = "confusion matrix - random forest") + 
  theme(text = element_text(size=global_size))
cm_rf_plot

#Stop timer
mytimer$stop("Random forest")

# show importance of predictors
importance(model_rf) %>% 
  as.data.frame() %>% 
  pander()

#############################################
## Model 5 - K-nearest neighbours 
## Summary 
# plot combined plots
grid.arrange(cm_nb_plot, cm_logm_plot, cm_dt_plot, cm_rf_plot, nrow=2)

# show all results
results %>% as.tbl() %>% as.data.frame() %>% 
  pander(split.tables = 130, style = "rmarkdown")

#Stop timer
mytimer$stop("Train and compare different models")

#############################################
# Model prediction 
#############################################
#Start timer
mytimer$start(" Model validation and results")

# Predicting values 
#set best value on cross-validation of model x

# remove flag variable (which is 0) on validation set
validation <- 
  validation %>% 
  dplyr::select(-flag)

# predict implausible projects 2021 - decision tree
validation_predicitions_dt <- predict(model_dt, validation)#, type = "response")

validation_flagged_dt_mean <- mean(validation_predicitions_dt[,2] > 0.5) # flagged projects
validation_flagged_dt_sum <- sum(validation_predicitions_dt[,2] > 0.5) # flagged projects

# predict implausible projects 2021 - random forest
validation_predicitions_rf <- predict(model_rf, validation)#, type = "response")

validation_flagged_rf_mean <- mean(validation_predicitions_rf$predictions == 1) # flagged projects
validation_flagged_rf_sum <- sum(validation_predicitions_rf$predictions == 1) # flagged projects

#Stop timer
mytimer$stop(" Model validation and results")

#############################################
# Appendix 
#Get timer summary
getTimer(mytimer) %>% 
  mutate(timeElapsed_min = round(as.numeric(timeElapsed/60),2)) %>%
  dplyr::select(event, start, end, timeElapsed_min) %>% 
  pander(split.tables = 120, style = "rmarkdown")
