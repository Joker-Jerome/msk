# library
library(ggplot2)
library(MASS)

# msk-impact
msk_matrix <- read.table("/Users/jerome/Projects/msk/datasets/msk_impact_2017_v2/mut_matrix.txt", sep = "\t", header = T)
sample_id <- msk_matrix$Sample_ID
patient_id <- substring(sample_id,1,9)
uniq_patient_id <- unique(patient_id)
pm_patient_id <- setdiff(patient_id,uniq_patient_id) 

# freq
freq_vec <- sapply(uniq_patient_id, function(x) sum(patient_id == x))
pm_index <- which(freq_vec > 1)
pm_id_1 <- uniq_patient_id[pm_index]


# for 
sapply(pm_id_1, function(x) ifelse(patien))
pm_all_index <- match(pm_id_1,patient_id)
all_pm_map <- match(patient_id,pm_id_1)
all_pm_nnaindex <- which(!is.na(all_pm_map))
msk_sub <- msk_matrix[all_pm_nnaindex,]

# primary metastasis
primary_sub <- msk_sub[msk_sub$Tumor == "Primary",]
metastasis_sub  <- msk_sub[msk_sub$Tumor == "Metastasis",]

primary_pid <- substring(primary_sub$Sample_ID,1,9)
metastasis_pid <- substring(metastasis_sub$Sample_ID,1,9)
shared_id <- intersect(primary_pid,metastasis_pid)
pp_index <- match(shared_id,primary_pid)
mp_index <- match(shared_id,metastasis_pid)

cp_sub <- primary_sub[pp_index,]
cm_sub <- metastasis_sub[mp_index,]


diff_mut <- function(i) {
    cat(colnames(cp_sub[which(cp_sub[i,9:1128] > 0) + 8]))
    cat("\n")
    cat(colnames(cm_sub[which(cm_sub[i,9:1128] > 0) + 8]))
    
    
}

diff_mut_list <- function(i) {
    p_set <- colnames(cp_sub[which(cp_sub[i,9:1128] > 0) + 8])
    m_set <- colnames(cm_sub[which(cm_sub[i,9:1128] > 0) + 8])
    inter <- intersect(p_set,m_set)
    return(setdiff(m_set,inter))
}

diff_list <- c()
total_count <- 0
for (t in 1:length(shared_id)) {
    diff_list <- c(diff_list,diff_mut_list(t))
    total_count <- total_count + length(diff_mut_list(t))
}

mut_count <- function(df) {
    vec <- sapply(1:nrow(df),function(x) sum(df[x,9:1128]))
    return(vec)
}

## breast paired sub
breast_cp <- cp_sub[as.character(cp_sub$Site) == "Breast",]
breast_cm <- cm_sub[as.character(cp_sub$Site) == "Breast",]


## lung paired sub
lung_cp <- cp_sub[as.character(cp_sub$Site) == "Lung",]
lung_cm <- cm_sub[as.character(cp_sub$Site) == "Lung",]

## prostate paired sub
prostate_cp <- cp_sub[as.character(cp_sub$Site) == "Prostate",]
prostate_cm <- cm_sub[as.character(cp_sub$Site) == "Prostate",]

# breast plot 
breast_cp_count <- mut_count(breast_cp)
breast_cm_count <- mut_count(breast_cm)
truehist(breast_cp_count, col = "green", nbins = 10)
truehist(breast_cm_count, col = "blue", nbins = 10, add = T)

hist(breast_cp_count, breaks = max(breast_cp_count)/2,col=rgb(1,0,0,0.5),xlim=c(0,130), ylim=c(0,30), main="Overlapping Histogram", xlab="counts")
hist(breast_cm_count, breaks = max(breast_cm_count)/2,col=rgb(0,0,1,0.5), add=T)

tmp_len <- length(breast_cp_count)
b_df <- data.frame(mut = c(breast_cp_count,breast_cm_count), pm = c(rep("primary",tmp_len), rep("metastasis", tmp_len)),group = c(1:tmp_len, 1:tmp_len))
ggplot(data = b_df, mapping = aes(x = factor(group), y = mut, fill = pm)) + geom_bar(stat = 'identity', position = 'dodge') + ylab("Number of mutations") + xlab("Sample Number") + 
    guides(fill=guide_legend(title="group"))+ ggtitle("Breast Cancer")

# lung plot
lung_cp_count <- mut_count(lung_cp)
lung_cm_count <- mut_count(lung_cm)

hist(lung_cp_count, breaks = max(lung_cp_count)/2,col=rgb(1,0,0,0.5),xlim=c(0,130), ylim=c(0,30), main="Overlapping Histogram", xlab="counts")
hist(lung_cm_count, breaks = max(lung_cm_count)/2,col=rgb(0,0,1,0.5), add=T)

tmp_len <- length(lung_cp_count)
l_df <- data.frame(mut = c(lung_cp_count,lung_cm_count), pm = c(rep("primary",tmp_len), rep("metastasis", tmp_len)),group = c(1:tmp_len, 1:tmp_len))
ggplot(data = l_df, mapping = aes(x = factor(group), y = mut, fill = pm)) + geom_bar(stat = 'identity', position = 'dodge') + ylab("Number of mutations") + xlab("Sample Number") + 
    guides(fill=guide_legend(title="group"))+ ggtitle("Lung Cancer")
## prostate 
prostate_cp_count <- mut_count(prostate_cp)
prostate_cm_count <- mut_count(prostate_cm)

hist(prostate_cp_count, breaks = max(prostate_cp_count)/2,col=rgb(1,0,0,0.5),xlim=c(0,130), ylim=c(0,30), main="Overlapping Histogram", xlab="counts")
hist(prostate_cm_count, breaks = max(prostate_cm_count)/2,col=rgb(0,0,1,0.5), add=T)

tmp_len <- length(prostate_cp_count)
p_df <- data.frame(mut = c(prostate_cp_count,prostate_cm_count), pm = c(rep("primary",tmp_len), rep("metastasis", tmp_len)),group = c(1:tmp_len, 1:tmp_len))
ggplot(data = p_df, mapping = aes(x = factor(group), y = mut, fill = pm)) + geom_bar(stat = 'identity', position = 'dodge') + ylab("Number of mutations") + xlab("Sample Number") + 
    guides(fill=guide_legend(title="group"))+ ggtitle("Prostate Cancer") + theme(axis.text.x =element_text(size=6) )
