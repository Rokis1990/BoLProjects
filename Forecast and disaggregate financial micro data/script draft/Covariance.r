## Covariance and Correlation

# read data
data2019 <- readxl::read_xlsx("Duomenys/Dataset_v3.1.xlsx", sheet = 5)

## Covariance and Correlation

# 110 Quarterly TUI-01 + F-06 vs B-09-04

a <- data2019$`F-06_13xx+TUI-01_22xx+TUI-01_32xx`
b <- data2019$`B-09-04 srautų suma (raw) 110`

print(cov(a, b, method = "spearman")) # Covariance
# 15
print(cor(a, b, method = "spearman")) # Correlation
# 0.75

# 120 
a <- data2019$`srautas F-06 120`
b <- data2019$`B-09-04 srautų suma (raw) 120`

print(cov(a, b, method = "spearman")) # Covariance
# -7.42
print(cor(a, b, method = "spearman")) # Correlation
# -0.37

# 150
a <- data2019$`F-06_23xx+TUI-01_12xx+TUI-01_42xx`
b <- data2019$`B09-04 srautų suma (raw) 150`

print(cov(a, b, method = "spearman")) # Covariance
# 6.64
print(cor(a, b, method = "spearman")) # Correlation
# 0.33






