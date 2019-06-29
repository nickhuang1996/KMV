#导入nleqslv
library(nleqslv);
#输入无风险利率
print("无风险利率:")
r <- 0.0225;
print(paste("r=", r))
#输入公司债务期限
print("公司债务期限:")
T <- 1;
print(paste("T=", T))
#输入流动负债、非流动负债
print("流动负债、非流动负债:")
SD <- 1e8;
LD <- 0.5 * 1e8;
print(paste("SD=", SD))
print(paste("LD=", LD))
#计算违约点
print("违约点:")
D0 <- SD + 0.5 * LD;
print(paste("D0=", D0))
#根据fair value修改违约点
D <- D0;
#输入股权波动率
print("股权波动率:")
PriceTheta <- 0.2893;
print(paste("PriceTheta=", PriceTheta))
#月波动
EquityTheta <- PriceTheta * sqrt(12);
print(paste("EquityTheta=", EquityTheta))
#输入股权价值
print("股权价值:")
E <- 214512867;
print(paste("E=", E))
if (FALSE) {
    "KMV模型变形求解"
}
cat('\n')

print("KMV模型变形求解")

x0 <- c(1, 1);
KMV_fun <- function(x) {
    y <- numeric(2);
    d1 <- (log(x[1] * E / D) + (r + 0.5 * x[2] ^ 2) * T) / (x[2] * sqrt(T));
    d2 <- d1 - x[2] * sqrt(T);
    y[1] <- x[1] * pnorm(d1) - D * exp(-r * T) * pnorm(d2) / E - 1;
    y[2] <- pnorm(d1) * x[1] * x[2] - EquityTheta;
    y
}
#求解KMV
z <- nleqslv(x0, KMV_fun, method = "Newton")
print(paste("z=", z))
cat('\n')

#VA
print("公司资产价值:")
V0 <- z$x[1] * E
print(paste("V0=", V0))
#AssetTheta
print("公司资产价值的波动率:")
ThetaV <- z$x[2]
print(paste("ThetaV=", ThetaV))
cat('\n')

#计算违约距离
print("违约距离:")
DD <- (V0 - D0) / (V0 * ThetaV)
print(paste("DD=", DD))
#计算违约率
print("违约率:")
EDF <- pnorm(DD)
print(paste("EDF=", EDF))
