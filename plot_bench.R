## This script was modified to calculate Correlation, RMSE, ME and plot scatter plot ###
## The original is by Dr Isaac Mugume ####

        setwd("~/Desktop/benchmarking/comp/entebbe/solar/")
        # 
        # # Let me read Jinja temperature data
        # 
        jja_1 = read.csv("comp_sol.csv", header = T); head(jja_1)
        # 
            jja_2 = c()
            for (i in seq(1, length(jja_1$Date_Time), 2)){
              x = cbind(as.character(jja_1[i,1]), as.character(jja_1[i,2]), as.character(jja_1[i+1,2]))
              row.names(x) = c()
              colnames(x) = c("Date_Time", "sol_wimea", "sol_unma")
              jja_2 = rbind(jja_2, x)
            }
            # 
            head(jja_2)
            # 
             write.csv(jja_2, "comp_sol1.csv", row.names = F)
    # 
        jja_3 = read.csv("comp_sol1.csv", header = T); head(jja_3)
    
    plot(jja_3$sol_unma, jja_3$sol_wimea, 
         #main = "Kamuli All Temperature Values (deg. C) -Scatter plot", 
         xlab = "UNMA_ADCON_AWS Solar Insolation (Watts per sq. meter)", ylab = "WIMEA-ICT_AWS Solar Insolation (Watts per sq. meter)",
         xlim = c(0, 1200), ylim = c(0, 820))
    x_val = c(1, 750)
    y_val = x_val
    lines(x_val, y_val, lwd=4, col = 2)
    cor.test(jja_3$sol_unma, jja_3$sol_wimea)
# #
legend("topleft", legend = c("Correlation = 0.594"), 
       bty = "n",
       text.col = c(2, 1))
#
error <- jja_3$sol_wimea-jja_3$sol_unma
rmse <- sqrt(mean(error^2))
me <- mean(error)
rmse
me
#   
# # Plotting hourly variations ####
jja_00 = read.csv("comp_sol_00.csv", header = F) # reading 0:00hr temp
# 
plot(jja_00[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 00:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_00[,2], col = 2, type = "o", lwd=2)
cor.test(jja_00[,3], jja_00[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
                             ), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_01 = read.csv("comp_sol_01.csv", header = F) # reading 0:00hr temp
# 
plot(jja_01[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 01:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_01[,2], col = 2, type = "o", lwd=2)
cor.test(jja_01[,3], jja_01[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
                             ), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_02 = read.csv("comp_sol_02.csv", header = F) # reading 0:00hr temp
# 
plot(jja_02[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 02:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_02[,2], col = 2, type = "o", lwd=2)
cor.test(jja_02[,3], jja_02[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_03 = read.csv("comp_sol_03.csv", header = F) # reading 0:00hr temp
# 
plot(jja_03[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 03:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_03[,2], col = 2, type = "o", lwd=2)
cor.test(jja_03[,3], jja_03[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)


jja_04 = read.csv("comp_sol_04.csv", header = F) # reading 0:00hr temp
# 
plot(jja_04[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 04:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_04[,2], col = 2, type = "o", lwd=2)
cor.test(jja_04[,3], jja_04[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)


jja_05 = read.csv("comp_sol_05.csv", header = F) # reading 0:00hr temp
# 
plot(jja_05[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 05:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_05[,2], col = 2, type = "o", lwd=2)
cor.test(jja_05[,3], jja_05[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_06 = read.csv("comp_sol_06.csv", header = F) # reading 0:00hr temp
# 
plot(jja_06[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 06:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_06[,2], col = 2, type = "o", lwd=2)
cor.test(jja_06[,3], jja_06[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_07 = read.csv("comp_sol_07.csv", header = F) # reading 0:00hr temp
# 
plot(jja_07[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 07:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_07[,2], col = 2, type = "o", lwd=2)
cor.test(jja_07[,3], jja_07[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
"Correlation = -0.770"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_08 = read.csv("comp_sol_08.csv", header = F) # reading 0:00hr temp
# 
plot(jja_08[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 08:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_08[,2], col = 2, type = "o", lwd=2)
cor.test(jja_08[,3], jja_08[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = -0.403"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_09 = read.csv("comp_sol_09.csv", header = F) # reading 0:00hr temp
# 
plot(jja_09[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 09:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_09[,2], col = 2, type = "o", lwd=2)
cor.test(jja_09[,3], jja_09[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = 0.800"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_10 = read.csv("comp_sol_10.csv", header = F) # reading 0:00hr temp
# 
plot(jja_10[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 10:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_10[,2], col = 2, type = "o", lwd=2)
cor.test(jja_10[,3], jja_10[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = 0.304"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_11 = read.csv("comp_sol_11.csv", header = F) # reading 0:00hr temp
# 
plot(jja_11[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 11:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_11[,2], col = 2, type = "o", lwd=2)
cor.test(jja_11[,3], jja_11[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = 0.535"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_12 = read.csv("comp_sol_12.csv", header = F) # reading 0:00hr temp
# 
plot(jja_12[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 12:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_12[,2], col = 2, type = "o", lwd=2)
cor.test(jja_12[,3], jja_12[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = 0.668"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_13 = read.csv("comp_sol_13.csv", header = F) # reading 0:00hr temp
# 
plot(jja_13[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 13:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_13[,2], col = 2, type = "o", lwd=2)
cor.test(jja_13[,3], jja_13[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = 0.208"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_14 = read.csv("comp_sol_14.csv", header = F) # reading 0:00hr temp
# 
plot(jja_14[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 14:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_14[,2], col = 2, type = "o", lwd=2)
cor.test(jja_14[,3], jja_14[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = 0.073"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_15 = read.csv("comp_sol_15.csv", header = F) # reading 0:00hr temp
# 
plot(jja_15[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 15:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_15[,2], col = 2, type = "o", lwd=2)
cor.test(jja_15[,3], jja_15[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = 0.912"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_16 = read.csv("comp_sol_16.csv", header = F) # reading 0:00hr temp
# 
plot(jja_16[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 16:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_16[,2], col = 2, type = "o", lwd=2)
cor.test(jja_16[,3], jja_16[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = -0.164"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_17 = read.csv("comp_sol_17.csv", header = F) # reading 0:00hr temp
# 
plot(jja_17[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 17:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_17[,2], col = 2, type = "o", lwd=2)
cor.test(jja_17[,3], jja_17[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = -0.410"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_18 = read.csv("comp_sol_18.csv", header = F) # reading 0:00hr temp
# 
plot(jja_18[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 18:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_18[,2], col = 2, type = "o", lwd=2)
cor.test(jja_18[,3], jja_18[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation", 
                             "Correlation = 0.207"), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_19 = read.csv("comp_sol_19.csv", header = F) # reading 0:00hr temp
# 
plot(jja_19[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 19:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_19[,2], col = 2, type = "o", lwd=2)
cor.test(jja_19[,3], jja_19[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
                             ), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_20 = read.csv("comp_sol_20.csv", header = F) # reading 0:00hr temp
# 
plot(jja_20[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 20:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_20[,2], col = 2, type = "o", lwd=2)
cor.test(jja_20[,3], jja_20[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
                             ), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
       col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_21 = read.csv("comp_sol_21.csv", header = F) # reading 0:00hr temp
# 
plot(jja_21[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 21:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_21[,2], col = 2, type = "o", lwd=2)
cor.test(jja_21[,3], jja_21[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_22 = read.csv("comp_sol_22.csv", header = F) # reading 0:00hr temp
# 
plot(jja_22[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 22:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_22[,2], col = 2, type = "o", lwd=2)
cor.test(jja_22[,3], jja_22[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)

jja_23 = read.csv("comp_sol_23.csv", header = F) # reading 0:00hr temp
# 
plot(jja_23[,3], type = "o", ylim = c(0, 1400), 
     lwd=2, xlab = "Count of Records", ylab = "Solar Insolation (Watts per sq. meter) at 23:00 Hrs")   # This is unma temp in the 3rd column
lines(jja_23[,2], col = 2, type = "o", lwd=2)
cor.test(jja_23[,3], jja_23[,2])
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation" 
), bty = "n", lwd = c(2, 2, 0), pch = c("o", "o", NA), 
col = c(2, 1), text.col = c(2, 1, 1), y.intersp = 0.4)


### Let us get daily variation by using average hourly values

avg_00 = c(mean(jja_00[,3], na.rm = T), mean(jja_00[,2], na.rm = T)); avg_00
avg_01 = c(mean(jja_01[,3], na.rm = T), mean(jja_01[,2], na.rm = T)); avg_01
avg_02 = c(mean(jja_02[,3], na.rm = T), mean(jja_02[,2], na.rm = T)); avg_02
avg_03 = c(mean(jja_03[,3], na.rm = T), mean(jja_03[,2], na.rm = T)); avg_03
avg_04 = c(mean(jja_04[,3], na.rm = T), mean(jja_04[,2], na.rm = T)); avg_04
avg_05 = c(mean(jja_05[,3], na.rm = T), mean(jja_05[,2], na.rm = T)); avg_05
avg_06 = c(mean(jja_06[,3], na.rm = T), mean(jja_06[,2], na.rm = T)); avg_06
avg_07 = c(mean(jja_07[,3], na.rm = T), mean(jja_07[,2], na.rm = T)); avg_07
avg_08 = c(mean(jja_08[,3], na.rm = T), mean(jja_08[,2], na.rm = T)); avg_08
avg_09 = c(mean(jja_09[,3], na.rm = T), mean(jja_09[,2], na.rm = T)); avg_09
avg_10 = c(mean(jja_10[,3], na.rm = T), mean(jja_10[,2], na.rm = T)); avg_10
avg_11 = c(mean(jja_11[,3], na.rm = T), mean(jja_11[,2], na.rm = T)); avg_11
avg_12 = c(mean(jja_12[,3], na.rm = T), mean(jja_12[,2], na.rm = T)); avg_12
avg_13 = c(mean(jja_13[,3], na.rm = T), mean(jja_13[,2], na.rm = T)); avg_13
avg_14 = c(mean(jja_14[,3], na.rm = T), mean(jja_14[,2], na.rm = T)); avg_14
avg_15 = c(mean(jja_15[,3], na.rm = T), mean(jja_15[,2], na.rm = T)); avg_15
avg_16 = c(mean(jja_16[,3], na.rm = T), mean(jja_16[,2], na.rm = T)); avg_16
avg_17 = c(mean(jja_17[,3], na.rm = T), mean(jja_17[,2], na.rm = T)); avg_17
avg_18 = c(mean(jja_18[,3], na.rm = T), mean(jja_18[,2], na.rm = T)); avg_18
avg_19 = c(mean(jja_19[,3], na.rm = T), mean(jja_19[,2], na.rm = T)); avg_19
avg_20 = c(mean(jja_20[,3], na.rm = T), mean(jja_20[,2], na.rm = T)); avg_20
avg_21 = c(mean(jja_21[,3], na.rm = T), mean(jja_21[,2], na.rm = T)); avg_21
avg_22 = c(mean(jja_22[,3], na.rm = T), mean(jja_22[,2], na.rm = T)); avg_22
avg_23 = c(mean(jja_23[,3], na.rm = T), mean(jja_23[,2], na.rm = T)); avg_23


hr_avg = rbind(avg_00, avg_01, avg_02, avg_03, avg_04, avg_05, avg_06, avg_07, avg_08, avg_09,
               avg_10, avg_11, avg_12, avg_13, avg_14, avg_15, avg_16, avg_17, avg_18, avg_19,
               avg_20, avg_21, avg_22, avg_23)
row.names(hr_avg) = c("00hrs", "01hrs", "02hrs", "03hrs", "04hrs", "05hrs", "06hrs", 
                      "07hrs", "08hrs", "09hrs", "10hrs", "11hrs", "12hrs", "13hrs",
                      "14hrs", "15hrs", "16hrs", "17hrs", "18hrs", "19hrs", "20hrs",
                      "21hrs", "22hrs", "23hrs")
colnames(hr_avg) = c("sol_unma", "sol_wimea")
head(hr_avg)

hrs = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)     # This gives the time in local time

plot(hrs, hr_avg[,1], type = "l", lwd=2, ylim = c(0, 1300), 
     xlab = "Hours in Local Time", 
     ylab = "Average Hourly Solar Insolation (Watts per sq. meter)")
lines(hrs, hr_avg[,2], lwd=2, col = 2)
legend("topleft", legend = c("WIMEA-ICT AWS Solar Insolation", "UNMA_ADCON AWS Solar Insolation"), 
       bty = "n", lwd = c(2, 2, 0), col = c(2, 1), text.col = c(2, 1), y.intersp = 0.4)
# 
