# MSK_IMPACT

msk_impact <- read.table("/Users/jerome/Downloads/msk_impact_2017/data_mutations_annotated.txt",header =T)
raw_file <- "/Users/jerome/Downloads/msk_impact_2017/data_mutations_annotated.txt"
text <- readLines(raw_file, n = 1)
text_vec <- strsplit(text, " ")[[1]]

text2 <- readLines(raw_file, n = 2)
text2_vec <- strsplit(text2, " ")[[1]]

