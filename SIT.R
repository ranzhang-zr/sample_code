library(tidyverse)
library(rstatix)
library(ggpubr)

#***************READ AND MERGE CSV FILES***************


# first, separate pre- and post files
# and put them in separate folders: [pre_folder], and [post_folder].
# then import and merge to 1 pre file, and 2 post file

allpre <- list.files(path = "C:\\Users\\599352rz\\Desktop\\pre_folder",
                  full.names = TRUE) %>%
  lapply(read_csv, 
         col_types = "ccccccccccccccccccccccccccccccccccccccccccccccccccccc") %>%
  lapply(head, n = -1) %>%
  bind_rows()


allpost <- list.files(path = "C:\\Users\\599352rz\\Desktop\\post_folder",
                     full.names = TRUE) %>%
  lapply(read_csv, 
         col_types = "ccccccccccccccccccccccccccccccccccccccccccccccccccccc") %>%
  lapply(head, n = -1) %>%
  bind_rows()



#***************PROCESSING PRE DATA***************



# Keep the columns and rows needed: 

names(allpre)<-str_replace_all(names(allpre), c(" " = "." , "," = "" ))

pre <- allpre %>%
  select("Participant.Public.ID", 
         "Screen.Name", 
         "Zone.Type", 
         "Response", 
         "image", 
         "Name") %>%
  filter(Zone.Type == "response_slider_endValue")

# extract feedback, 
# rename variables, 
# create an indicator of feedback types

# after inspecting, there were 6 cases with abnormal data
# that only have screen 1. instead of both screen 2 and screen 3
# also there were 2 cases with more data somehow
# so I drop them. in total, 452 at this time

pre <- filter(pre,
              Screen.Name == "Screen 2" | Screen.Name == "Screen 3" )

pre <- pre[!(pre$Participant.Public.ID == "R_1reHEZI2qmRP9VZ" | pre$Participant.Public.ID == "R_vMlcK9WtK1Efny1"), ]


widepre <- pre %>%
  pivot_wider(names_from = Screen.Name, 
              values_from = Response) %>%
  rename(rate_pre = "Screen 2",
         fb_pre = "Screen 3",
         cnd_pre = Name,
         img_pre = image,
         id_pre = Participant.Public.ID)



widepre$rate_pre <- as.numeric(widepre$rate_pre)
widepre$fb_pre <- as.numeric(widepre$fb_pre)

widepre <- mutate(widepre,
                  fb_type = case_when(fb_pre > rate_pre ~ "high",
                                      fb_pre < rate_pre ~ "low",
                                      fb_pre == rate_pre ~ "same"))



#***************PROCESSING PRE DATA***************



names(allpost)<-str_replace_all(names(allpost), c(" " = "." , "," = "" ))

post <- allpost %>%
  select("Participant.Public.ID", 
         "Zone.Type", 
         "Response", 
         "image", 
         "Name") %>%
  filter(Zone.Type == "response_slider_endValue") %>%
  rename(rate_post = Response,
         cnd_post = Name,
         img_post = image,
         id_post = Participant.Public.ID)


# again, after inspecting, there were 6 with more data...

post <- post[!(post$id_post == "R_1reHEZI2qmRP9VZ" | post$id_post == "R_3eh8fBUCEzcl55x" |
               post$id_post == "R_3gZrx996r17rapk" | post$id_post == "R_3HNOAeup6q9mgOm" |
                 post$id_post == "R_3Li0Ee6LwUe2lBt" | post$id_post == "R_vMlcK9WtK1Efny1"), ]




#***************MERGE THE PRE AND POST***************



# only the ones with both pre and post would be presented
# in total 448 cases
# memo: group_by() and tally()

pre_plus_post <- merge(x = widepre, y = post,
                       by.x = c("id_pre","img_pre"),
                       by.y = c("id_post","img_post"))

# after inspecting, the data looked fine.
# so drop redundant variables, calculate the block 2 - block 1

pre_plus_post <- select(pre_plus_post,
                        id_pre,
                        img_pre,
                        cnd_pre,
                        rate_pre,
                        fb_pre,
                        fb_type,
                        rate_post)

pre_plus_post$rate_post <- as.numeric(pre_plus_post$rate_post)

pre_plus_post$diff <- pre_plus_post$rate_post - pre_plus_post$rate_pre



#***************PROCESSING THE FINAL***************


# aggregate the diff and rating

summary <- pre_plus_post %>%
  group_by(id_pre, cnd_pre, fb_type) %>%
  dplyr::summarise(meandiff = mean(diff), meanrate = mean(rate_pre))

# extract summary to the csv file, in case want to use other software

write_csv(summary, "C:\\Users\\599352rz\\Desktop\\summary.csv")


# SIMPLE TASK BEHAVIOR

# (1) descriptive

summary %>%
  group_by(cnd_pre, fb_type) %>%
  summarize(diff = mean(meandiff),
            rate = mean(meanrate))

summary %>%
  group_by(cnd_pre) %>%
  summarize(rate = mean(meanrate))

# (2) comparing the willingness to drink between conditions
# RM ANOVA: condition on rate_pre

sum_onlycnd <- summary %>%
  group_by(id_pre, cnd_pre) %>%
  summarize(rate = mean(meanrate))

write_csv(sum_onlycnd, "C:\\Users\\599352rz\\Desktop\\sum_cnd_rate.csv")

sum_onlycnd <- sum_onlycnd %>%
  ungroup()

res_1way <- sum_onlycnd %>%
  anova_test(dv = rate,
             wid = id_pre,
             within = cnd_pre)

get_anova_table(res_1way, correction = "auto")

pairwise_t_test(sum_onlycnd,rate ~ cnd_pre)

# plot mean rate

ggerrorplot(sum_onlycnd, x = "cnd_pre", y = "rate",
            desc_stat = "mean_se",
            error.plot = "errorbar",
            add = "violin",add.params = list(color = "black", fill = "darkgray"))



# (3) comparing mean diff
# two way RM ANOVA: condition and feedback type on mean diff


sum_wide_diff <- summary %>%
  select(-meanrate) %>%
  pivot_wider(names_from = c(cnd_pre, fb_type),
              values_from = meandiff)

write_csv(sum_wide_diff, "C:\\Users\\599352rz\\Desktop\\sum_wide_diff.csv")


### QUESTION: could be that some people very low so have no feedback...

summary <- summary %>%
  ungroup()

res_rm <- summary %>%
  anova_test(dv = meandiff, 
             wid = id_pre,
             within = c(cnd_pre, fb_type))

get_anova_table(res_rm, correction = "auto")

# plot mean diff

ggplot(data = summary) + 
  geom_boxplot(mapping = aes(x = fb_type, y = meandiff), 
               outlier.shape = NA, notch = TRUE) +
  facet_wrap(~cnd_pre) + theme_bw() +
  scale_y_continuous(limits = c(-3, 3))


# CALCULATE SI SCORE
# since condition doesn't really change the diff
# I'll only calculate postive influence, negative influence, and total influence


# first, make an only_fbtype_wide data format

sum_fb_wide <- summary %>%
  group_by(id_pre, fb_type) %>%
  summarize(diff = mean(meandiff)) %>%
  pivot_wider(names_from = fb_type,
              values_from = diff)

# second, calculate PosIn: high diff - same diff
# and NegIn: low diff* (-1) - same diff


sum_fb_wide$PosIn <- sum_fb_wide$high - sum_fb_wide$same
sum_fb_wide$NegIn <- sum_fb_wide$low*(-1) - sum_fb_wide$same


# Finally, TotalIn: [high diff + low diff * (-1)]/2 - same diff

sum_fb_wide$TotalIn <- (sum_fb_wide$high + (sum_fb_wide$low * (-1))) * 1/2 - 
  sum_fb_wide$same


write_csv(sum_fb_wide, "C:\\Users\\599352rz\\Desktop\\sum_fb_wide.csv")

