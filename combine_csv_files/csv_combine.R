library(tidyverse)
data <- read.csv("~/Downloads/csv-ovarianneo-set.csv") %>% 
  mutate(PMID = as.character(PMID))
missing_data <- read.csv("~/Downloads/errors.csv") %>% 
  mutate(PMID = as.character(PMID))
i=1
while (i <= nrow(data)) {
  data.i= data[c(i: (i+169)), ]
  write.csv(data.i, paste0("~/Desktop/CSV FILES/csv-ovarianneo-set_", i, ".csv"))
  i = i+130
}
k=1
while (k <= nrow(missing_data)) {
  data.k= missing_data[c(k: (k+100)), ]
  write.csv(data.k, paste0("~/Desktop/CSV FILES/csv-ovarianneo-missing-set_", k, ".csv"))
  k = k+80
}

files=paste0("~/Downloads/screened_", 1:40, ".csv")
combined = NULL


# combine 40 screened files
for (i in files) {
  csv = read.csv(i, stringsAsFactors = FALSE)
  combined <- rbind(combined, csv)
  print(paste0(i," processed"))
}

# remove duplicates (PMID or title)
cleaned_csv = combined %>% 
  distinct(PMID, .keep_all = TRUE) %>% 
  distinct(Title, .keep_all = TRUE) %>% 
  mutate(PMID = as.character(PMID))

# update the original data
updated_data = data %>% 
  left_join(
    cleaned_csv %>% 
      select(PMID, Inclusion.Exclusion.Reason),
    by = "PMID"
  )
updated_data = updated_data %>% 
  left_join(
    cleaned_csv %>% 
      select(Title, Inclusion.Exclusion.Reason) %>% 
      distinct(),
    by = "Title",
    suffix = c("",".byTitle")
  ) %>% 
  mutate(Inclusion.Exclusion.Reason = 
           coalesce(Inclusion.Exclusion.Reason, 
                    Inclusion.Exclusion.Reason.byTitle)
         ) %>% 
  select(-Inclusion.Exclusion.Reason.byTitle)

sum(is.na(updated_data$Inclusion.Exclusion.Reason))

write.csv(updated_data,"~/Desktop/CSV FILES/updated_csv.csv")

