# Multivariate analysis lab 
# Written by S. Oh
# Version date: Nov 25, 2024

## Section 5.6 Multivariate Quality Control Chart. 센터 표시. upper bound, lower bound 표시하고 그 밖으로 나가는 이상점을 표시하는 정도의 그림.

## Monotoring the stability of a given sample of multivariate observations
## Setting a control region for future observations

#plot을 똑같이 그려 보는 수준에서 준비.

library(ggplot2)
library(MASS)

data <- data.frame(
  x1 = c(3387, 3109, 2670, 3125, 3469, 3120, 3671, 4531, 3678, 3238, 3135, 5217, 3728, 3506, 3824, 3516),
  x2 = c(2200, 875, 957, 1758, 868, 398, 1603, 523, 2034, 1136, 5326, 1658, 1945, 344, 807, 1223),
  x3 = c(1181, 3532, 2502, 4510, 3032, 2130, 1982, 4675, 2354, 4606, 3044, 3340, 2111, 1291, 1365, 1175),
  x4 = c(14861, 11367, 13329, 12328, 12847, 13979, 13528, 12699, 13534, 11609, 14189, 15052, 12236, 15482, 14900, 15078),
  x5 = c(236, 310, 1182, 1208, 1385, 1053, 1046, 1100, 1349, 1150, 1216, 660, 299, 206, 239, 161)
)

mean_vec = colMeans(data[,c(1,2)])
Sigma_12 = cov(data[,c(1,2)])
n <- nrow(data)
p <- 2


## Ellipse format chart : 타원 그리기 차트 

ggplot(data, aes(x = x1, y = x2)) +
  geom_point(color = "black", size = 3) +  # Scatter plot points
  stat_ellipse(level = 0.95, type = "norm", color = "blue", linewidth = 1) +  # Ellipse
  geom_point(aes(x = mean_vec[1], y = mean_vec[2]), shape = 3, size = 4) + # mean vector 부분에 십자가 그리기 
  theme_minimal() + #미학적 추가 
  labs(
    title = "Ellipse Format Chart of x1 and x2",
    x = "x1",
    y = "x2"
  )

### Univariate Control Chart - x2를 기준으로 다시 그림 그려 보기! (왜냐? 타원에서 x2 쪽으로 먼 이상점 있었으니)

x2bar = mean_vec[2]
CI_len = qnorm(0.975) * sqrt(Sigma_12[2,2])
UCL = x2bar + CI_len; LCL = x2bar - CI_len

# Plot the x2 series with horizontal lines
plot(
  data[, 2], 
  type = 'l', 
  main = "Univariate quality control chart", 
  xlab = "Index", 
  ylab = expression(x[2]),
  ylim = c(-1000,6000)
)
abline(h = x2bar, col = "blue", lty = 2, lwd = 2)  # Horizontal line for x2bar
abline(h = UCL, col = "red", lty = 2, lwd = 2)     # Horizontal line for UCL
abline(h = LCL, col = "red", lty = 2, lwd = 2)     # Horizontal line for LCL

# Add annotations
legend(
  "topright", 
  legend = c(expression(bar(x[2])), "UCL", "LCL"), 
  col = c("blue", "red", "red"), 
  lty = 2, 
  lwd = 2, 
  box.lty = 0
)


## T^2-chart : 타원 : 2차원까지만 가능. 그 이상 차원이면 Hotelling's T^2 통해 시각화해야 함. 

# Calculate Hotelling's T^2 statistic for each observation

inv_cov_matrix <- solve(Sigma_12)
T2 <- apply(data[,c(1,2)], 1, function(row) {
  diff <- as.matrix(row - mean_vec)
  as.numeric(t(diff) %*% inv_cov_matrix %*% diff)
})

# Control limit for T^2-chart
alpha <- 0.05
control_limit <- (p * (n - 1) / (n - p)) * qf(1 - alpha, p, n - p) # follows F distribution

# Create a data frame for plotting
T2_data <- data.frame(
  Observation = 1:n,
  T2 = T2
)

# Plot the T^2-chart
ggplot(T2_data, aes(x = Observation, y = T2)) +
  geom_point(color = "black", size = 3) +  # Line plot of T^2 values
  geom_hline(yintercept = control_limit, color = "blue", linetype = "dashed", size = 1) +  # Control limit
  theme_minimal() +
  labs(
    title = "Hotelling's T^2 Control Chart",
    x = "Observation",
    y = expression(T^2)
  )

#궁금하면? filtering해서 idx 확인.
