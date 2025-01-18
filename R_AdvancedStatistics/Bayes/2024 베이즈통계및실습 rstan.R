## Rstan 공식 페이지, Rstan 전반에 대한 소개 (https://mc-stan.org/users/interfaces/rstan)


##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #####
# "RStan Getting Started" - (https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) 
# 위 링크에서 Rstan의 설치에 대한 전반적인 가이드를 확인할 수 있음
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #####




### 단계는 크게 다음과 같음
# (0. R 및 Rstudio 버전 확인)
# 1. Configuring C++ Toolchain (필수적, 상당 시간 소요, OS별 매뉴얼이 상이함에 유의)
# 2. RStan의 설치
# 3. Example 1: Eight schools (작동 확인을 위함)



### 0. R 및 Rstudio 버전 확인
# R version 4.2.0 이상 사용 권장 (https://www.r-project.org/)
# RStudio version 1.4.x 이상 사용 권장 (https://posit.co/products/open-source/rstudio/)



### 1. Configuring C++ Toolchain
# "RStan Getting Started"에 안내된, 본인 환경에 맞는 방법대로 매뉴얼을 따라 정확하게 수행하기 바람 



### 2. Rstan의 설치

## (이전에 rstan을 사용한 적이 있는 경우) 다음 lines를 실행하여 제거 후 최신 버전으로 재설치 권장
# remove.packages(c("StanHeaders", "rstan"))
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

## 'Configuring C++ Toolchain' 단계를 마친 Windows의 기준으로는 다음과 같이 진행
remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)



## (Mac의 경우) rstan 설치 과정에서 compilation이 요구되는 패키지를 소스로부터 바로 설치하기를 
# 원하느냐고 묻는 질문이 나올 때, Yes를 입력한 후 "non-zero exit status" 오류가 발생할 경우
# : 단계 2.의 처음으로 돌아가 rstan 제거 -> R 재시작 -> compilation 물음에 no로 진행한 후 오류 재확인



### 3. Example 1: Eight schools

library(rstan)
# rstan version 2.32.6 / Stan version 2.32.2

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

example(stan_model, run.dontrun = TRUE) # 시간 소요

## 만약 다음과 같은 에러가 발생하는 경우
# : "prep_call_sampler(object)에서 다음과 같은 에러가 발생했습니다: 함수 "prep_call_sampler"를 찾을 수 없습니다"

## 1. 단계 2.의 remove.packages("rstan")와 if (file.exists(".RData")) file.remove(".RData")를 실행하고
## 2. library("rstan") 또는 fit <- stan(file = 'schools.stan', data = schools_dat) 등과 같은 line을 실행해보고
## 3. rstan이라는 패키지를 찾을 수 없다거나 stan이라는 함수를 찾을 수 없다는 문구가 출력되면
## 4. rm(list=ls())으로 R 환경을 비운 다음 R을 재시작하고,
## 5. 이 파일의 lines 38-40를 실행(rstan package 제거 및 재설치)한 다음,
## 6. 단계 3.을 다시 시도




### Example 1: Eight schools

# : 여전히 (https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)를 참고하며 진행하는 것입니다.
# 상단탭(File -> New File -> Stan File)을 통해서 새로운 (stan) text editor을 열고,
# 위 링크의 "Example 1: Eight Schools" 부분에 있는 약 18줄의 코드를 복사 및 붙여넣기하여 작성한 프로그램을
# getwd()를 실행했을 때 출력되는 경로에 파일명 "schools.stan"으로 저장해야 합니다.

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
schools_fit <- stan(file = 'schools.stan', data = schools_dat) # 시간 소요

# "hash mismatch so recompiling; make sure Stan code ends with a blank line"라 나올 수도 있음
# - "schools.stan"의 첫 줄 및 마지막 줄 확인
# "경고메시지(들): 1: There were (-----) divergent transitions after warmup."라 나올 수 있음 
# - 우선 진행해도 무방함 (참고: https://mc-stan.org/misc/warnings.html)
# "Tail Effective Samples Size (ESS) is too low, ---"라 나올 수 있음
# - 우선 진행해도 무방함

plot(schools_fit, pars=c("mu", "tau", "eta", "theta"))
print(schools_fit) # 사후표본의 요약통계량
plot(schools_fit)
pairs(schools_fit, pars = c("mu", "tau", "lp__")) # 경고메시지 무시