# msk-impact"
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

