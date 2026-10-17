#********************************************************************************
# Purpose: Read SVF measurements, extract station and direction from `id.
#
# Input:
#   - File: data/SVF/namexy.csv  (tab-delimited)
#
# Output:
#   - Boxplot
#********************************************************************************
svf <- read.csv2("data/SVF/svf_2025-09-01.csv", sep = ";", dec = ".", stringsAsFactors = FALSE)

cols <- names(svf)[3:length(names(svf))]

lst <- lapply(seq_len(nrow(svf)), function(i) as.numeric(svf[i, cols]))
names(lst) <- svf$id

boxplot(lst, las = 2, xlab = "Station", ylab = "SVF")