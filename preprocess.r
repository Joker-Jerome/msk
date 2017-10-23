# MSK_IMPACT

msk_impact <- read.table("/Users/jerome/Downloads/msk_impact_2017/data_mutations_annotated.txt",header =T)
raw_file <- "/Users/jerome/Downloads/msk_impact_2017/data_mutations_annotated.txt"
con <- file('/Users/jerome/Downloads/msk_impact_2017/data_mutations_annotated.txt')

text <- readLines(raw_file, n = 1)
text_vec <- strsplit(text, " ")[[1]]
text_vec[2]

text2 <- readLines(raw_file, n = 2)
text2_vec <- strsplit(text2, " ")[[1]]

read.table(con, skip = 0, nrows = 1)
read.table(con, skip = 1, nrows = 1)

con <- file('/Users/jerome/Downloads/msk_impact_2017/data_mutations_annotated.txt')
line1 <- scan(file=con, nlines=1, quiet=TRUE)


msk_impact <- read.table("/Users/jerome/Downloads/msk_impact_2017/data_mutations_annotated.txt", skip = 2,header =T)

