library("tidyverse")

data <- read.csv("~/Downloads/Master Alumni List_Updated_7-16-25_by_Bohyoon.csv")
betsi_contact <- read.csv("~/Downloads/Book2.csv")

data = data %>% rename("First Name" = First.Name,
                       "Last Name" = Last.Name,
                       "Mobile Phone" = Cell.Phone)  

betsi_contact = betsi_contact %>% rename("First Name" = First.Name,
                       "Last Name" = Last.Name,
                       "Mobile Phone" = Mobile.Phone,
                       "Home Phone" = Home.Phone,
                       "Business Phone" = Business.Phone,
                       "E-mail Address" = E.mail.Address)  %>% 
                filter(!`Mobile Phone` %in% c("", "No contact from Betsi", "Do Not Contact")) %>% 
                select(`First Name`, `Last Name`, `Mobile Phone`,
                       `Home Phone`, `Business Phone`, `E-mail Address`,
                       `Company`) %>% 
                mutate(across(everything(), ~ as.character(.)))

data = data %>% select(-starts_with("X")) %>% 
  select(`First Name`, `Last Name`, `Mobile Phone`, Company) %>% 
  filter(!`Mobile Phone` %in% c("", "No contact from Betsi", "Do Not Contact")) %>% 
  mutate(`Home Phone` = NA_real_, `Business Phone`=NA_real_, `E-mail Address`=NA_real_) %>% 
  mutate(across(everything(), ~ as.character(.)))

combined = bind_rows(data, betsi_contact) %>% 
  distinct(`First Name`, `Last Name`, .keep_all = TRUE) %>% 
  mutate(Company = case_when(
    Company == "" ~ NA,
    TRUE ~ Company
  ),
  `Home Phone` = case_when(
    `Home Phone` == "" ~ NA,
    TRUE ~ `Home Phone`
  ),
  `Business Phone` = case_when(
    `Business Phone` == "" ~ NA,
    TRUE ~ `Business Phone`),
  `E-mail Address` = case_when(
    `E-mail Address` == "" ~ NA,
    TRUE ~ `E-mail Address`))

write.csv(combined, "~/Downloads/Alumni_Contact.csv", row.names = FALSE)



