# Welcome to the supplemental code for The signed mental lexicon: Effect of phonological neighborhood density, 
# iconicity, and childhood language experience.

# For the below code to work, three subdirectories named 'data', 'iconicity', and 'results' must exist your current working directory.
# Correspondence concerning this article and/or code should be addressed to Naomi K. Caselli (nkc@bu.edu).

# LOAD PACKAGES
###########################################################################################
library(tidyverse)
library(apaTables)
library(sjPlot)
library(lme4)
library(data.table)
library(gridExtra)

# DATA INGEST
###########################################################################################
# Download data from OSF
osfr::osf_retrieve_file("tj7f2") %>%
     osfr::osf_download(path = "./data", conflicts = "overwrite", progress = TRUE) 

osfr::osf_retrieve_file("9nygd") %>%
     osfr::osf_download(path = "./data", conflicts = "overwrite", progress = TRUE)

osfr::osf_retrieve_file("q46gj") %>%
     osfr::osf_download(path = "./iconicity", conflicts = "overwrite", progress = TRUE)

# Import data
lexdecdata <-tibble(
     read.csv("./data/StudyData.csv"))

ASLLEX <-tibble(
     read.csv("./data/SignData.csv"))

# DATA WRANGLING
###########################################################################################
# Recode Flexion to categorical variable
ASLLEX$Flexion <- recode(ASLLEX$Flexion,  
                         "1" ="Fully Open", 
                         "2" = "Bent",
                         "3" = "Flat Open", 
                         "4" = "Flat Closed",
                         "5" = "Curved (C)",
                         "6" = "Curved (O)",
                         "7" = "Fully Closed")

# Recode for ID consistency
ASLLEX$EntryID <- toupper(ASLLEX$EntryID)

# SET GRAPHIC AESTHETICS
###########################################################################################
lifeaquatic <-
        c("#c7e9b4",
          "#41b6c4",
          "#1d91c0",
          "#225ea8",
          "#253494",
          "#081d58",
          "white")
short_lifeaquatic <- c("#3B9AB2", "#F21A00", "#EBCC2A")
very_short_lifeaquatic <- c("#3B9AB2",  "#EBCC2A")

set_theme(
        base = theme_classic(),
        axis.title.size = .9,
        axis.textsize = .9,
        legend.size = .7,
        legend.title.size = .8,
        geom.label.size = 3,
        plot.margin = unit(c(1,1,1.5, 1.2), "cm")
)

# DEMOGRAPHICS  
###########################################################################################
###  Plot the distribution of age and quantity of ASL exposure
demographics <- lexdecdata %>% 
        select(Participant, ASLAge, ASLExposure, ParentHearingStatus) %>% 
        distinct()

ASLExposurePlot<-qplot(factor(round(ASLExposure, digits=1)), data=demographics, geom="bar", fill=factor(ParentHearingStatus))+ 
        labs(y="Number of Participants",x="Quantity of \n ASL Exposure",fill=" ")+
        scale_x_discrete(breaks = scales::pretty_breaks(n = 6))+
        theme(axis.title.y=element_blank(),
              legend.direction = "horizontal",
              legend.position = c(-0.1, 1.05),
              text=element_text(size=14),
              axis.text.x = element_text(angle = 45, hjust = 1,size=14), 
              axis.line = element_line(colour = "grey"),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank())+ 
        scale_fill_manual(values = short_lifeaquatic)


ASLAgePlot<-qplot(ASLAge, data=demographics, geom="histogram", fill=factor(ParentHearingStatus),binwidth=1)+ 
        labs(y="Number of Participants",x="Age of \n First ASL Exposure",fill=" ")+
        theme(legend.position="none",
              text=element_text(size=14),
              axis.text.x = element_text(angle = 45, hjust = 1,size=14), 
              axis.line = element_line(colour = "grey"))+ 
        scale_fill_manual(values = short_lifeaquatic)




pdf(
        file = "./results/Figure3.pdf",
        width = 7,
        height = 3
)
grid.arrange(ASLAgePlot,ASLExposurePlot,ncol=2)
dev.off()
###########################################################################################
### ANALYSIS #1: Plot the distribution of phonological features and lexical class for real signs, non signs, and ASL-LEX

# Create %notin% operator
`%notin%` <- Negate(`%in%`)

# Create table to pull from for charting
asllexphon <- ASLLEX %>% select(  EntryID,
                                  SignType,
                                  MajorLocation,
                                  SelectedFingers,
                                  Flexion,
                                  Movement,
                                  LexicalClass) %>%
     mutate(CorrectResponse = as.factor("ASL-LEX")) %>%
     filter(EntryID %notin% lexdecdata$EntryID)

# Rename asllexphon column names to match lexdecdata column names we will be working with for this analysis
setnames(asllexphon, old = names(asllexphon), new = c('EntryID','SignType', 'MajorLocation', 'SelectedFingers',
                                                      'Flexion', 'Movement', 'LexicalClass', 'CorrectResponse'))

# Create empty pdf to place chart in
pdf(
     file = "./results/Appendix1.pdf",
     width = 10,
     height = 6
)

# Plot distribution of phonological features
lexdecdata %>% select(
     EntryID,
     SignType,
     MajorLocation,
     SelectedFingers,
     Flexion,
     Movement,
     LexicalClass,
     CorrectResponse
) %>%
     bind_rows(asllexphon) %>%
     gather(Feature, "value", -c(EntryID, CorrectResponse)) %>%
     distinct() %>%
     group_by(CorrectResponse, Feature) %>%
     dplyr::count(value) %>%
     transmute(value, percent = n / sum(n)) %>%
     transform(Feature=factor(Feature,levels=c("SignType","Flexion","Movement", "SelectedFingers", "MajorLocation", "LexicalClass"))) %>%
     ggplot(aes(x = value, y = percent, fill = CorrectResponse)) +
     geom_bar(position = "dodge", stat = "identity") +
     guides(fill = guide_legend(title = NULL)) +
     coord_flip() +
     facet_wrap(~ Feature , scales = "free") +
     theme(axis.title.y = element_blank(),
           legend.position = c(-.15, 0.05)) +
     scale_fill_manual(values = short_lifeaquatic) +
     scale_y_continuous(expand = c(0, 0))
dev.off()

### ANALYSIS #1a: Compare iconicity of study items to other items in ASL-LEX

StudyItems <- lexdecdata %>%
     select(EntryID, Iconicity.Z.) %>%
     distinct() %>%
     filter(EntryID %in% ASLLEX$EntryID)

StudyItems %>%
     summarise(mean = mean(Iconicity.Z.), sd = sd(Iconicity.Z.))

OtherASLLEXItems <- ASLLEX %>%
     select(EntryID, Iconicity.Z.) %>%
     filter(EntryID %notin% lexdecdata$EntryID)

OtherASLLEXItems %>%
     summarise(
          mean = mean(Iconicity.Z., na.rm = TRUE),
          sd = sd(Iconicity.Z., na.rm = TRUE)
     )

wilcox.test(StudyItems$Iconicity.Z., OtherASLLEXItems$Iconicity.Z.)

### ANALYSIS #1b: Make a correlation matrix summarizing the lexical information
lexdecdata %>% select(
     EntryID,
     MaximalNeighborhoodDensity,
     MinimalNeighborhoodDensity,
     Parameter.BasedNeighborhoodDensity,
     SignFrequency.Z.,
     Iconicity.Z.
) %>%
     distinct() %>%
     select(-EntryID) %>%
     apa.cor.table(
          show.conf.interval = FALSE,
          filename = "./results/Table1.doc",
          landscape = TRUE
     )


### Analysis #1: Results
nrow(lexdecdata)
lexdecdata %>% group_by(CorrectResponse) %>% count()

### Analysis #2: Remove Outliers
# Identify the error rate for each sign, and identify signs with more than 40% errors
HighErrors <-
     lexdecdata %>% group_by(EntryID) %>% 
     filter(CorrectResponse == "nonsign") %>% summarise(
          NumberCorrect = sum(Error == "C", na.rm = TRUE),
          NumberIncorrect = sum(Error == "E", na.rm = TRUE)
     ) %>% 
     mutate(PropErrors = NumberIncorrect/(NumberCorrect + NumberIncorrect)) %>%
     filter(PropErrors > .4)

# Remove signs with higher than 40% errors
lexdecdata <-
     lexdecdata %>% filter(!EntryID %in% HighErrors$EntryID)

# Find items faster than the sign onset
lexdecdata %>% filter(RT < min(SignOnset)) %>% count()

# Find 2 SDs above the mean RT for each participant
RTSD <-
     lexdecdata %>% 
     dplyr::group_by(Participant) %>% 
     dplyr::summarise (
          sdRT = sd(RTFromSignOnset, na.rm = TRUE),
          mRT = mean(RTFromSignOnset, na.rm = TRUE)
     ) %>% 
     mutate(upperbound = 2*sdRT + mRT)

lexdecdata <- merge(RTSD, lexdecdata, by = "Participant")

lexdecdata %>% filter(RT >= upperbound) %>% count()

# Remove items below the sign onset and above 2sds above the mean for that participant
lexdecdata <- lexdecdata %>% filter(RT < upperbound)
lexdecdata <- lexdecdata %>% filter(RT > min(SignOnset))

# Summarise accuracy rates and RTs 
lexdecdata %>% group_by(Error) %>% count()

# Main summary table
lexdecdata %>%
     dplyr::group_by(Participant, CorrectResponse) %>%
     dplyr::summarise(
          NumberCorrect = sum(Error == "C", na.rm = TRUE),
          NumberIncorrect = sum(Error == "E", na.rm = TRUE),
          MeanRT = mean(RTFromSignOnset, na.rm = TRUE)
     ) %>%
     dplyr::mutate(PercentCorrect = NumberCorrect / (NumberCorrect + NumberIncorrect)) %>%
     dplyr::group_by(CorrectResponse) %>%
     dplyr::summarise(
          MeanCorrectRate = mean(PercentCorrect),
          SDCorrectRate = sd(PercentCorrect),
          meanRT2 = mean(MeanRT),
          sdRT2 = sd(MeanRT)
     )

### Analysis #3: Plot the timecourse of sign onsets, offsets, and RTs
timecoursedata <- lexdecdata %>%
        select(EntryID, CorrectResponse, SignOffset, SignOnset, RT) %>%
        gather(Event, "Time",-c(CorrectResponse, EntryID)) %>%
        mutate(Event = as.factor(Event)) %>%
        mutate(Event = fct_relevel(Event, c("SignOffset", "SignOnset", "RT"))) 

#separate the reaction time data (which should be plotted at the trial level) 
#from the sign onset/offset data. Reduce the sign onset/offset trial so there is only one value per item.
timecourseSignDuration <- timecoursedata %>%
        filter(Event == "SignOffset" | Event == "SignOnset") %>%
        distinct() 

timecourseRT <- timecoursedata %>% 
        filter(Event == "RT")

timecoursedata <- rbind(timecourseSignDuration, timecourseRT)

# Establish median RT
medianRT <- timecoursedata %>%
        filter(Event == "RT") %>%
        dplyr::group_by(CorrectResponse) %>%
        dplyr::summarize(Int = median(Time, na.rm = TRUE))

# Create empty pdf to place chart in
pdf(
     file = "./results/Figure4.pdf",
     width = 5,
     height = 3
)

# Plot timecourse of sign onsets, offsets, and RTs
ggplot(data = timecoursedata, aes(x = Time, fill = Event)) +
        xlab("Time (ms)") +
        ylab("Density") +
        guides(fill = guide_legend(title = NULL)) +
        geom_density() +
        scale_color_manual(values = sort(short_lifeaquatic)) +
        scale_fill_manual(values = sort(short_lifeaquatic)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0),  limits = c(0, 4000)) +
        facet_grid(CorrectResponse ~ .) +
        geom_vline(data = medianRT, aes(xintercept = Int))
dev.off()


### Analysis #4: Lexicality and Accuracy
# Examine mean/sd of RTs by error
lexdecdata  %>%
     group_by(Error) %>%
     summarise(mean = mean(RT), sd = sd(RT))

lexicalityRT <-
        lmer(logRTFromSignOnset ~ CorrectResponse + Error + ParentHearingStatus  + Age + ASLAge   + ASLExposure +
                        SignOnset + SignOffset + TrialNum + preverror + logprevRT +  
                        (1 | EntryID) + (1 | Participant),
                data = lexdecdata,
                REML = FALSE
        )
summary(lexicalityRT)

lexicalityAccuracy <- 
        glmer(Error ~ CorrectResponse +ParentHearingStatus  + Age + ASLAge++ASLExposure + SignOnset + SignOffset + TrialNum +
                        preverror + logprevRT  + 
                        (1 | EntryID) + (1 | Participant),
                family = binomial,
                data = lexdecdata,
                nAGQ = 0,
                control = glmerControl(optimizer = "nloptwrap")
        )
summary(lexicalityAccuracy)



### Analysis 5: Regressions
# Prepare for regressions
variabs <-
     c(
          "SignOnset",
          "SignOffset",
          "logprevRT" ,
          "TrialNum",
          "Age",
          "ASLExposure",
          "MaximalNeighborhoodDensity",
          "MinimalNeighborhoodDensity",
          "Parameter.BasedNeighborhoodDensity",
          "ASLAge",
          "MajorLocationFrequency",
          "HandshapeFrequency"
     )

# Transform necessary variables
lexdecdata [variabs] <-
     lapply(lexdecdata[variabs],  scale)

# Create different subsets of dataset
# Correct lexical decision table
correctlexdecdata <-
     lexdecdata %>% 
     filter(Error == "C" & CorrectResponse == "realsign") %>%
     filter(complete.cases(.))

# Deaf of deaf only table
DoDOnly <-
     correctlexdecdata %>% filter(ParentHearingStatus == "DeafParents")

# Deaf of deaf only accuracy table
AccuracyDoDOnly <-
     lexdecdata %>% filter(ParentHearingStatus == "DeafParents")

# Deaf of hearing only table
DoHOnly <-
     correctlexdecdata %>% filter(ParentHearingStatus == "HearingParents")

# Deaf of hearing only accuracy table
AccuracyDoHOnly <-
     lexdecdata %>% filter(ParentHearingStatus == "HearingParents")

# High frequency
HF <-
     correctlexdecdata %>% filter(SignFrequency.Z. > median(SignFrequency.Z.))

# Low frequency
LF <-
     correctlexdecdata %>% filter(SignFrequency.Z. < median(SignFrequency.Z.))

### Analysis 5a: Reaction time models
# Select a measure of neighborhood density
MaxND <-
     lmer(
          logRTFromSignOnset ~ ParentHearingStatus  + Age + ASLAge  + ASLExposure +
               SignOnset + SignOffset + TrialNum + preverror + logprevRT + MaximalNeighborhoodDensity *
               SignFrequency.Z. * ParentHearingStatus  + Iconicity.Z. * SignFrequency.Z. * ParentHearingStatus  + (1 |
               EntryID) + (1 | Participant),
          data = correctlexdecdata,
          REML = FALSE
     )

MinND <-
     lmer(
          logRTFromSignOnset ~ ParentHearingStatus  + Age + ASLAge   + ASLExposure +
               SignOnset + SignOffset + TrialNum + preverror + logprevRT + MinimalNeighborhoodDensity *
               SignFrequency.Z. * ParentHearingStatus  + Iconicity.Z. * SignFrequency.Z. * ParentHearingStatus  + 
               (1 |EntryID) + (1 |Participant),
          data = correctlexdecdata,
          REML = FALSE
     )
PBND <-
     lmer(
          logRTFromSignOnset ~ ParentHearingStatus  + Age + ASLAge   + ASLExposure +
               SignOnset + SignOffset + TrialNum + preverror + logprevRT + Parameter.BasedNeighborhoodDensity *
               SignFrequency.Z. * ParentHearingStatus  + Iconicity.Z. * SignFrequency.Z. * ParentHearingStatus  + 
               (1 | EntryID) + (1 | Participant),
          data = correctlexdecdata,
          REML = FALSE
     )

# View fit
AIC(MaxND)
AIC(MinND)
AIC(PBND)

# View summarization
summary(MaxND)
summary(MinND)
summary(PBND)


# Plot the interaction between neighborhood density, parent hearing status, and frequency
# Create empty pdf to place charts in
pdf(
     file = "./results/Figure5.pdf",
     width = 6,
     height = 3
)

# Plot model
plot_model(
     PBND,
     type = "pred",
     terms = c(
          "Parameter.BasedNeighborhoodDensity",
          "SignFrequency.Z. [-0.56, -0.01, 0.48]",
          "ParentHearingStatus"
     ),
     mdrt.values = "quart",
     ci.style = "whisker",
     title = " "
) + scale_colour_manual(
     values = short_lifeaquatic,
     name = "Sign Frequency (quartiles)",
     labels = c("Lower", "Median", "Upper")
) +
     scale_fill_manual(values = c("grey", "grey", "grey")) +
     labs(y = "Log Reaction Time", x = "Parameter Based Neighborhood Density")
dev.off()

# Analysis 5b: Additional models examining subsets of data
DoDOnlyModel <-
     lmer(
          logRTFromSignOnset ~  Age + SignOnset + SignOffset + TrialNum + preverror + logprevRT + Parameter.BasedNeighborhoodDensity *
               SignFrequency.Z.  + Iconicity.Z. * SignFrequency.Z.  + 
               (1 | EntryID) + (1 | Participant),
          data = DoDOnly,
          REML = FALSE
     )

DoHOnlyModel <-
     lmer(
          logRTFromSignOnset ~  Age++SignOnset + SignOffset + TrialNum + preverror + logprevRT + Parameter.BasedNeighborhoodDensity *
               SignFrequency.Z.  + Iconicity.Z. * SignFrequency.Z.  + 
               (1 | EntryID) + (1 |  Participant),
          data = DoHOnly,
          REML = FALSE
     )


HighFreq <-
     lmer(
          logRTFromSignOnset ~ ParentHearingStatus  + Age++SignOnset + SignOffset + TrialNum +
               preverror + logprevRT + Parameter.BasedNeighborhoodDensity * ParentHearingStatus  +
               Iconicity.Z. * ParentHearingStatus  + 
               (1 | EntryID) + (1 | Participant),
          data = HF,
          REML = FALSE
     )

LowFreq <-
     lmer(
          logRTFromSignOnset ~ ParentHearingStatus  + Age++SignOnset + SignOffset + TrialNum +
               preverror + logprevRT + Parameter.BasedNeighborhoodDensity * ParentHearingStatus  +
               Iconicity.Z. * ParentHearingStatus  + 
               (1 | EntryID) + (1 | Participant),
          data = LF,
          REML = FALSE
     )

# Create table of all RT models
tab_model(
     PBND,
     DoDOnlyModel,
     DoHOnlyModel,
     HighFreq,
     LowFreq,
     file = "./results/Table2.doc",
     p.style = "stars",
     collapse.se = TRUE,
     linebreak = FALSE,
     show.re.var = FALSE,
     show.aic = FALSE,
     show.ci= FALSE,
     show.icc = FALSE,
     wrap.labels = 55,
     show.ngroups = FALSE,
     dv.labels = c(
          "Full Model",
          "Deaf Parents",
          "Hearing Parents",
          "High Frequency",
          "Low Frequency"
     ),
     pred.labels = c(
          "(intercept)",
          "Hearing Parents",
          "Age",
          "ASL Age",
          "ASL Exposure",
          "Sign Onset",
          "Sign Offset",
          "Trial Number",
          "Previous Trial Accuracy (error)",
          "Previous Trial Reaction Time",
          "Parameter ND",
          "Frequency",
          "Iconicity",
          "Parameter ND: Frequency",
          "Hearing Parents : Parameter ND",
          "Hearing Parents : Frequency",
          "Frequency : Iconicity",
          "Hearing Parents : Iconicity",
          "Hearing Parents : Parameter ND : Frequency",
          "Hearing Parents : Frequency : Iconicity"
     )
)

# Create table of log likelihood comparisons
PBNDLRT <- drop1(PBND, test = "Chisq", scope = ~ .)

# Write to results folder
write.csv(PBNDLRT, "./results/TableS1.csv")

### Analysis 5c: Accuracy Models
# Create table based on accuracy
accuracylexdecdata <-
     lexdecdata %>% 
     filter(CorrectResponse == "realsign") %>% 
     na.omit(Age)

# Select a measure of neighborhood density
AccuracyMaxND <-
     glmer(
          Error ~ ParentHearingStatus  + Age + ASLAge++ASLExposure + SignOnset + SignOffset + TrialNum +
               preverror + logprevRT + MaximalNeighborhoodDensity * SignFrequency.Z. * ParentHearingStatus  +
               Iconicity.Z. * SignFrequency.Z. * ParentHearingStatus  + 
               (1 | EntryID) + (1 | Participant),
          family = binomial,
          data = accuracylexdecdata,
          nAGQ = 0,
          control = glmerControl(optimizer = "nloptwrap")
     )

AccuracyMinND <-
     glmer(
          Error ~ ParentHearingStatus  + Age + ASLAge++ASLExposure + SignOnset + SignOffset + TrialNum +
               preverror + logprevRT + MinimalNeighborhoodDensity * SignFrequency.Z. * ParentHearingStatus  +
               Iconicity.Z. * SignFrequency.Z. * ParentHearingStatus  + 
               (1 | EntryID) + (1 | Participant),
          family = binomial,
          data = accuracylexdecdata,
          nAGQ = 0,
          control = glmerControl(optimizer = "nloptwrap")
     )

AccuracyPBND <-
     glmer(
          Error ~ ParentHearingStatus  + Age + ASLAge++ASLExposure + SignOnset + SignOffset + TrialNum +
               preverror + logprevRT + Parameter.BasedNeighborhoodDensity * SignFrequency.Z. * ParentHearingStatus  +
               Iconicity.Z. * SignFrequency.Z. * ParentHearingStatus  + 
               (1 | EntryID) + (1 | Participant),
          family = binomial,
          data = accuracylexdecdata,
          nAGQ = 0,
          control = glmerControl(optimizer = "nloptwrap")
     )

NoIntAccuracyPBND <-
     glmer(
          Error ~ ParentHearingStatus  + Age + ASLAge + ASLExposure + SignOnset + SignOffset + TrialNum +
               preverror + logprevRT + Parameter.BasedNeighborhoodDensity * SignFrequency.Z. +  ParentHearingStatus * Parameter.BasedNeighborhoodDensity  + ParentHearingStatus *
               SignFrequency.Z.  + Iconicity.Z. * SignFrequency.Z. + Iconicity.Z. * ParentHearingStatus   + (1 | EntryID) + (1 | Participant),
          family = binomial,
          data = accuracylexdecdata,
          nAGQ = 0,
          control = glmerControl(optimizer = "nloptwrap")
     )

# View fit
AIC(AccuracyMaxND)
AIC(AccuracyMinND)
AIC(AccuracyPBND)

# View summarization of models
summary(AccuracyMaxND)
summary(AccuracyMinND)
summary(AccuracyPBND)



# Create neighborhood density interaction graph
pdf(
     file = "./results/Figure6.pdf",
     width = 5,
     height = 4
)

plot_model(
     NoIntAccuracyPBND,
     type = "eff",
     terms = c("Iconicity.Z.", "ParentHearingStatus"),
     transformation = NULL) +   
     scale_y_continuous(name = "logits") +
     scale_colour_manual(
          values = very_short_lifeaquatic,
          name = " ",
          labels = c("Deaf Parents", "Hearing Parents")
     ) +
     scale_fill_manual(values = c("grey", "grey", "grey")) +
     labs(x = "Iconicity", title = "") +
     guides(color = guide_legend(override.aes = list(fill = NA)))
dev.off()

# Create accuracy models table
tab_model(
     AccuracyPBND,
     file = "./results/Table3.doc",
     p.style = "stars",
     collapse.se = TRUE,
     linebreak = FALSE,
     show.re.var = FALSE,
     show.aic = FALSE,
     show.ci= FALSE,
     show.icc = FALSE,
     wrap.labels = 55,
     show.ngroups = FALSE,
     pred.labels = c(
          "(intercept)",
          "Hearing Parents",
          "Age",
          "ASL Age",
          "ASL Exposure",
          "Sign Onset",
          "Sign Offset",
          "Trial Number",
          "Previous Trial Accuracy (error)",
          "Previous Trial Reaction Time",
          "Parameter ND",
          "Frequency",
          "Iconicity",
          "Parameter ND: Frequency",
          "Hearing Parents : Parameter ND",
          "Hearing Parents : Frequency",
          "Frequency : Iconicity",
          "Hearing Parents : Iconicity",
          "Hearing Parents : Parameter ND : Frequency",
          "Hearing Parents : Frequency : Iconicity"
     )
)

# Create log likelihood test table
NoIntAccuracyPBNDDrop1 <-
     drop1(NoIntAccuracyPBND, test = "Chisq", scope = ~ .)

write.csv(
     NoIntAccuracyPBNDDrop1,
     "./results/TableS2.csv"
)
### END OF LINE
