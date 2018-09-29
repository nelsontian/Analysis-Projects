# this file is another analysis using language background variables (lbq)
# this is a continuation of the three-part ratings analysis

setwd('C:/Users/Nelson/Desktop/BMBLAB')

# bring lbq (language background variables) into analysis
lbq <- read.csv("lbq_data_cleaned.csv")
colnames(lbq) = c("subject","thinking",
                  "l1read","l1write","l1listen","l1speak","l1vocab","l1grammar","l1avg",
                  "l2read","l2write","l2listen","l2speak","l2vocab","l2grammar","l2avg",
                  "l3","multiYN","multi","CultureUS","CultureH","MoveUS","EngAoA",
                  "TVSp","MovSp","MediaSp","AvgSp","TVEn","MovEn","MediaEn","AvgEn",
                  "TVSp_En","MovSp_En","MediaSp_En","AvgSp_En")

# read in all if not in environment
if(!exists("all", inherits = FALSE))
{
  all <- read.csv("ratings_full.csv")
  all$group <- factor(all$group, levels = c("Heritage Speakers","Early Bilinguals",
                                            "Late Bilinguals","Spanish Controls",
                                            "Ecuador Spanish","English Controls"))
}

if(!exists("allverbn", inherits = FALSE))
{
  allverbn <- read.csv("all_verbs.csv")
  allverbn <- within(allverbn, {
    engverb <- factor(engverb)
    spnverb <- factor(spnverb)
  })
}

# subject-level analysis
subjects <- all %>%
  filter(condition == "Causative") %>%
  group_by(subject, group) %>%
  summarise(avgrating = mean(rating, na.rm = T),
            sdrating = sd(rating, na.rm = T))
subjects <- merge(subjects, lbq, by = "subject")

# make categorical versions of exposure variable
subjects$TV <- ifelse(subjects$TVSp_En > 0, 'Sp', 'En')
subjects$Mov <- ifelse(subjects$MovSp_En > 0, 'Sp', 'En')
subjects$Media <- ifelse(subjects$MediaSp_En > 0, 'Sp', 'En')

# make categories for how exposure dist (3Sp, 2En, etc.)
subjects$spfreq <- rowSums(subjects == 'Sp')
subjects$totalexp <- ifelse(subjects$spfreq == 3, '3Sp',
                            ifelse(subjects$spfreq == 2, '2Sp',
                                   ifelse(subjects$spfreq == 1, '2En',
                                          '3En')))
subjects <- subset(subjects, select = -spfreq)

# plot distribution of all exposure variables (TVSp, ..., AvgEn)
pdf("exposurevarsdiff.pdf", onefile = TRUE)
# i for column ids of exp. var
for(i in 35:38)
{
  # histogram
  x <- subjects[[i]]
  x <- x[!is.na(x)]
  h <- hist(x, breaks = 10, col = "grey",
            main = c(paste(colnames(subjects)[i]), " Exposure"))
  xfit <- seq(min(x), max(x), length = 40) 
  yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col = "black", lty = 2, lwd = 2)
  # density plot
  plot(density(subjects[[i]], na.rm = T),
       main = c(paste(colnames(subjects)[i]), " Exposure"))
}
# saved to pdf
dev.off()

pdf("exposurevarsgrouped.pdf", onefile = TRUE)
# i for column ids of exp. var
# for(i in 35:38)
# {
#   # histogram
#   x <- subjects[[i]]
#   x <- x[!is.na(x)]
#   h <- hist(x, breaks = 10, col = "grey",
#             main = c(paste(colnames(subjects)[i]), " Exposure"))
#   xfit <- seq(min(x), max(x), length = 40) 
#   yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
#   yfit <- yfit*diff(h$mids[1:2])*length(x) 
#   lines(xfit, yfit, col = "black", lty = 2, lwd = 2)
#   # density plot
#   plot(density(subjects[[i]], na.rm = T),
#        main = c(paste(colnames(subjects)[i]), " Exposure"))
# }
for(i in 35:38)
{
  cname = colnames(subjects[i])
  g = ggplot(data = subjects, aes(get(cname), color = group)) +
    geom_density() +
    xlab(cname) +
    theme_classic()
  print(g)
}
# saved to pdf
dev.off()

# create a new categorical variable for language proficiency
attach(subjects)
subjects$proflevel[l1avg >= 6] <- "Strong"
subjects$proflevel[l1avg >= 3 & l1avg < 6] <- "Medium"
subjects$proflevel[l1avg < 3] <- "Weak"
detach(subjects)
# same with cultural affiliation
attach(subjects)
subjects$culturehlevel[CultureH >= 6] <- "Strong"
subjects$culturehlevel[CultureH >= 3 & CultureH < 6] <- "Medium"
subjects$culturehlevel[CultureH < 3] <- "Weak"
detach(subjects)

# visualize data
ggplot(subjects) +
  geom_bar(aes(x = group, y = avgrating, fill = proflevel),
           position = "dodge", stat = "summary", fun.y = "mean")

pdf("findings.pdf", onefile = T)
ggplot(subjects) +
  geom_point(aes(x = l1avg, y = avgrating, color = group))

ggplot(subjects) +
  geom_bar(aes(x = group, y = avgrating, fill = proflevel),
           position = "dodge", stat = "summary", fun.y = "mean")


ggplot(subjects) +
  geom_bar(aes(group, y = avgrating, fill = thinking),
           position = "dodge", stat = "summary", fun.y = "mean")

ggplot(subjects) + 
  geom_col(aes(x = factor(CultureUS), y = avgrating), position = "dodge") +
  facet_grid(. ~ group) +
  ggtitle("Cultural affiliation bar chart")

ggplot(subjects, aes(x=l1avg)) + geom_histogram(binwidth=.5) +
  facet_grid(. ~ group) +
  ggtitle("L1 proficiency by group")

ggplot(subjects, aes(x=l2avg)) + geom_histogram(binwidth=.5) +
  facet_grid(. ~ group) +
  ggtitle("L1 proficiency by group")
dev.off()

# # trim out fat from lbq
# modellbq <- lbq %>%
#   select(subject,MoveUS,EngAoA,l1avg,l2avg,
#          TVSp,MovSp,MediaSp,TVEn,MovEn,MediaEn)

# merge trial level data with verb data
all_new <- merge(x = all, y = allverbn, by = "verb", all.x = TRUE)
# only caus
caus_new <- all_new %>%
  filter(condition == "Causative")
caus_bil <- caus_new %>%
  filter(group %in% c("Heritage Speakers", "Early Bilinguals", "Late Bilinguals"))
# only pscaus
pscaus_new <- all_new %>%
  filter(condition == "Pseudo-causative")


# correlation between stimNum and rating PER GROUP for caus
stim_caus <- data.table(caus_new)
# one missing value for rating in HS group, remove for now
stim_caus <- stim_caus[!is.na(stim_caus$rating), ]
stim_causcorr <- stim_caus[, .(scorr = cor(stimNum, rating)), by = group]
# stim_causcorr

# correlation between stimNum and rating PER GROUP for pscaus
stim_pscaus <- data.table(pscaus_new)
# one missing value for rating in HS group, remove for now
stim_pscaus <- stim_pscaus[!is.na(stim_pscaus$rating), ]
stim_pscauscorr <- stim_pscaus[, .(scorr = cor(stimNum, rating)), by = group]
stim_pscauscorr

# # no lbq for Ecuador, will remove by merging
# caus_new <- merge(caus_new, lbq, by = "subject")
# caus_bil <- merge(caus_bil, lbq, by = "subject")

# correlations between different variables
caus_corr <- caus_bil %>%
  select(zipf,stimNum,EngAoA,MoveUS,l1avg,l2avg,rating)
caus_res <- cor(caus_corr, use = "complete.obs")
round(caus_res, 2)

# another one for language exposure
caus_correxp <- caus_bil %>%
  select(TVSp,MovSp,MediaSp,TVEn,MovEn,MediaEn,rating)
caus_resexp <- cor(caus_correxp, use = "complete.obs")
round(caus_resexp, 2)

# grouped exposure correlations
caus_corrdiff <- caus_new %>%
  select(group,TVSp_En,MovSp_En,MediaSp_En,AvgSp_En)
for(i in 1:4)
{
  c <- caus_corrdiff %>%
    filter(group == levels(caus_new$group)[i]) %>%
    select(-group)
  print(levels(caus_new$group)[i])
  print(round(cor(c, use = "complete.obs"), 2))
}


# linear mixed effects models
zipf_lm <- lmer(rating ~ group + (1|verb) + (1|subject), data = caus_new)
zipf_full <- lmer(rating ~ zipf + group + (1|verb) + (1|subject), data = caus_new)
anova(zipf_lm, zipf_full)

zipf_inter <- lmer(rating ~ zipf * group + (1|verb) + (zipf|subject), data = caus_new)
summary(zipf_lm)
summary(zipf_full)

anova(zipf_full, zipf_inter)
zipf_only <- glmer(rating ~ zipf + (1|verb) + (1|subject), family = poisson, data = caus_new)

# instead of using averaged ratings, treat ratings as an ordinal categorical with 5 levels
caus_new$rating <- factor(caus_new$rating, ordered = TRUE, levels = c("1","2","3","4","5"))
caus_bil$rating <- factor(caus_bil$rating, ordered = TRUE, levels = c("1","2","3","4","5"))

caus_new <- within(caus_new, group <- relevel(group, ref = "Heritage Speakers"))

# rating is now a categorical variable, use clmm for ordinal mixed effects regression
zipf_clmm <- clmm(rating ~ group + (1|verb) + (1|subject), data = caus_new)
zipf_clmm <- clmm(rating ~ zipf + group + (1|verb) + (1|subject), data = caus_new)
zipf_clmm <- clmm(rating ~ zipf + stimNum + group + (1|verb) + (1|subject), data = caus_new)
summary(zipf_clmm)

# remove group labels, look closer at bilingual groups
zipf_clmm <- clmm(rating ~ zipf + stimNum + EngAoA + MoveUS + (1|verb) + (1|subject), data = caus_bil)
zipf_clmm <- clmm(rating ~ l1avg + l2avg + (1|verb) + (1|subject), data = caus_bil)
zipf_clmm <- clmm(rating ~ zipf + stimNum + MoveUS + l1avg + l2avg + (1|verb) + (1|subject), data = caus_bil)
# language exposure
zipf_clmm <- clmm(rating ~ TVSp + (1|verb) + (1|subject), data = caus_bil)
summary(zipf_clmm)
zipf_clmm <- clmm(rating ~ MovSp + (1|verb) + (1|subject), data = caus_bil)
summary(zipf_clmm)



# snippet for error checking
clmm_data <- caus_new %>%
  select(rating, zipf, stimNum, group, verb, subject)
clmm_ex <- clmm(rating ~ zipf + stimNum + group + (1|verb) + (1|subject), data = clmm_data)
write.csv(clmm_data, "clmm_data.csv", row.names = FALSE)

write.csv(subjects, "subject_data.csv", row.names = FALSE)
