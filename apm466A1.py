# -*- coding: utf-8 -*-
from math import log,exp, ceil
import scipy.optimize as optimize
import scipy.interpolate as interpolate
import pylab as pl
import numpy as np
#ttm: list of time to maturities for each date, it is all rounded to months 
#as from Feb 1, 2021

date_label = ['JAN 18','JAN 19','JAN 20','JAN 21','JAN 22','JAN 25','JAN 26','JAN 27','JAN 28','JAN 29']
#prices: list of closing prices for selected bonds from Jan 18 to Jan 29 
#ttm: list of time to maturities for each date, it is all rounded to months 
#as from Feb 1, 2021
ttm = [1/12,7/12,13/12,18/12,25/12,28/12,37/12,43/12,49/12,55/12,61/12]
#prices: list of closing prices for selected bonds from Jan 18 to Jan 29 
prices =[[100.08,100.4,100.44,100.2,103.29,103.13,106.21,104.27,103.6,100.33,\
          98.78],[100.08,100.4,100.43,100.19,103.29,103.14,106.21,104.28,103.6,\
          100.32,98.77],[100.08,100.39,100.41,100.16,103.25,103.1,106.17,\
          104.25,103.59,100.3,98.76],[100.07,100.38,100.4,100.16,103.24,103.09,\
          106.15,104.19,103.51,100.22,98.66],[100.07,100.38,100.41,100.16,\
          103.25,103.09,106.17,104.24,103.54,100.27,98.71],[100.06,100.37,\
          100.39,100.14,103.22,103.04,106.12,104.29,103.62,100.37,98.84],[\
          100.06,100.37,100.39,100.15,103.22,103.06,106.18,104.26,103.59,\
          100.33,98.83],[100.06,100.38,100.41,100.17,103.25,103.1,106.22,\
          104.32,103.66,100.42,98.93],[100.06,100.37,100.41,100.18,103.26,\
          103.08,106.19,104.24,103.61,100.35,98.86],[100.06,100.37,100.41,\
         100.18,103.24,103.06,106.18,104.24,103.57,100.33,98.81]]
#coupon rates for the 11 bonds
couponRate = [0.75,0.75,0.55,0.25,1.75,1.5,2.25,1.5,1.25,0.5,0.25]
# yr: list of raw (not interpolated) yields for dates from Jan 18 to Jan 29
# yr[0]= Jan 18's yields... yr[9]= Jan29's yields without interpolations
yr = [] 
#caculate ytm, price is the bond price, notional is the face value, 
#time is time to maturity,coupon_rate is coupon rate
#freq is the frequency of coupon in a year,2 is semi-annual
def estimate_ytm(price, notional, time, coupon, freq=2):
    return (coupon+(notional-price)/time)/((notional+price)/freq)
def bond_ytm(price, notional, time, coupon, freq=2, guess=0.05):
    periods = time*freq
    coupon_each = coupon/freq
    time_mod = time%0.5
    if(time_mod==0):
        time_mod+=0.2/12
    dirty_price = price+(0.5-time_mod)*coupon
    dt = [ time_mod*2+i for i in range(ceil(periods))]
    ytm_func = lambda y : sum([coupon_each/((1+y/freq)**(t)) for t in dt]) + \
        (notional)/((1+y/freq)**(periods))  - dirty_price
    return optimize.newton(ytm_func, guess)

#caculate all the raw yields
for i in range(10):
    ytms = []#ytms for ith date
    for j in range(11):
        price = prices[i][j]#j-th bond price at i-th date
        time = ttm[j]#time to maturity for j-th bond
        coupon_rate =couponRate[j] #coupon rate  for j-th bond
        ytm = estimate_ytm(price,100,time,coupon_rate)
        ytms.append(ytm)
    yr.append(ytms)
    
#interpolating time points(0-5 years) for each month and plot the yield curve
t_points=[]
for i in range(60):
    t_points.append((i+1)/12)
y = []
for j in range(10):
    interp = interpolate.CubicSpline(ttm, yr[j])
    yj = interp(t_points)
    y.append(yj)
    pl.plot(t_points,y[j],label=date_label[j])
pl.ylabel('yields')
pl.xlabel('years')
pl.title('0-5Year Yield Curve')
pl.legend()
pl.show()

#bootstrapping spot curve
sr_all=[]
for i in range(10):
    spot_rates = []#ytms for ith date
    for j in range(11):
        price = prices[i][j]#j-th bond price at i-th date
        time = ttm[j]#time to maturity for j-th bond
        coupon_each =couponRate[j]/2 #coupon rate  for j-th bond
        coupon_pv = 0# present value for coupons
        time_mod = time%0.5
        interp_sp =[]
        if(time_mod==0):
            time_mod+=0.2/12
        dirty_price = price+(0.5-time_mod)*couponRate[j]# caculate dirty price
        dt = [ time_mod+i/2 for i in range(int(time*2))]#caculate coupon times 
        if len(spot_rates)>1:
            interp= interpolate.CubicSpline(ttm[0:len(spot_rates)],spot_rates)
            interp_sp = interp(dt)
        elif len(spot_rates)>0:
            interp_sp=spot_rates
        for t in range(len(dt)):
            coupon_pv += coupon_each*exp(-interp_sp[t]*dt[t])
        rest_price = dirty_price - coupon_pv
        spot_rate = -log(rest_price/(100+coupon_each))/time
        spot_rates.append(spot_rate)
    sr_all.append(spot_rates)

s=[]
for j in range(10):
    interp = interpolate.CubicSpline(ttm, sr_all[j])
    sj = interp(t_points[11:])
    s.append(sj)
    pl.plot(t_points[11:],sj,label=date_label[j])
pl.ylabel('spot rates')
pl.xlabel('years')
pl.title('1-5Year Spot Curve')
pl.legend()
pl.show()
         
#Calculate Forward Curve
firsty_rs=[]
for i in range(10):
    firsty_rs.append(s[i][0])

fr_all=[]
for i in range(10):
    forward_rates=[]
    first_r= firsty_rs[i]
    for j in range(len(s[0])-12):
        future_r=s[i][j+12]
        forward_rate = (1+future_r)**(j/12+2)/(1+first_r)-1
        forward_rates.append(forward_rate)
    fr_all.append(forward_rates)
    pl.plot(t_points[23:],forward_rates,label=date_label[i])
pl.ylabel('1yr to x yr forward rates')
pl.xlabel('years')
pl.title('2-5Year forward Curve')
pl.legend()
pl.show()
         
#Calculate cov matrix log-returns for yields 
log_return_all=[]
for i in range(5):
    log_returns=[]
    for j in range(9):
        r_iyear_j = y[j][12*i-1]
        r_iyear_j1 = y[j+1][12*i-1]
        log_return = log(r_iyear_j/r_iyear_j1)
        log_returns.append(log_return)
    log_return_all.append(log_returns)
data = np.array(log_return_all)
covMatrix_logs = np.cov(data,bias=True)
print ('covMatrix_logs:',covMatrix_logs)

#calculate cov matrix for foward rates
fr_all1=[]
for i in range(4):
    frs=[]
    for j in range(9):
        fr_iyear_j=fr_all[j][12*(i-1)]
        frs.append(fr_iyear_j)
    fr_all1.append(frs)
data1 = np.array(fr_all1)
covMatrix_fr = np.cov(data1,bias=True)
print('covMatrix_fr:',covMatrix_fr)

#Calculate eigenvalue and eigenvectors
eig_logr = np.linalg.eig(covMatrix_logs)
print('eigenvalues logr-cov:',eig_logr[0])
print('eigenvectors logr-cov:',eig_logr[1])
eig_fr = np.linalg.eig(covMatrix_fr)
print('eigenvalues fr-cov',eig_fr[0])
print('eigenvectors fr-cov',eig_fr[1])