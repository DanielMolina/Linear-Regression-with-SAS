options nodate nocenter linesize=75 pagesize=66;

* input data and rename variables;

FILENAME CSV "/folders/myfolders/PopularitySubset.csv";

PROC IMPORT DATAFILE = CSV
		    OUT = Popularity
		    DBMS = CSV
		    REPLACE;

proc datasets LIB = work MEMTYPE = data;
	MODIFY Popularity;
		RENAME _n_tokens_title = x1 
				_n_tokens_content = x2 
				_num_hrefs = x3 
				_num_imgs = x4 
				_num_videos = x5 
				_LDA_01 = x6 
				_LDA_02 = x7 
				_global_rate_positive_words = x8 
				_global_rate_negative_words = x9 
				_is_weekend = x10 
				_num_shares = y 
				_time_delta = td 
				virality_index = y_v;

* correlation matrix;

proc corr data = Popularity;
	var x1 - x10 y_v;

* backward elimination procedure;

proc reg data = Popularity;
	model y_v = x1 - x10 / selection = backward slstay = 0.05;

* lack of fit test and VIF to check multicollinearity;

proc reg data = Popularity plots(maxpoints = 50000);
	model y_v = x1 x2 x3 x6 x7 x8 x9 x10 / vif lackfit alpha = 0.05;
	ods output anova = pop_anova;
	output out = pop_out p = yhat r = e;

* tests for normality of error terms;

proc univariate plots normal data = pop_out;
	var e;

* breuch-pagan test to check constant varience of error terms;

data pop_out;
  set pop_out nobs=total;
  e2 = e**2;

proc reg data = pop_out;
  model e2 = x1 x2 x3 x6 x7 x8 x9 x10;
  ods output anova = pop_anova2;

data pop_anova;
  set pop_anova;
  if source='Error' then call symput ('sse' , ss);

data pop_anova2;
  set pop_anova2;
  if source='Model' then call symput ('ssr', ss);

data pop_BP;
  BP_statistic = (&ssr/9)/(&sse/39644)**2;
  pvalue = 1 - probchi(BP_statistic, 8);
  ssr = &ssr;
  sse = &sse;

proc print data = pop_BP;

* box-cox procedure on Y;

proc transreg data = Popularity ss2 detail;
	model boxcox(y_v / geo) = identity(x1 x2 x3 x6 x7 x8 x9 x10);


* transformation on Y suggested by box-cox;

data Popularity2;
	set Popularity;
	y_t = y_v ** -0.25;

* lack of fit test and VIF to check multicollinearity;

proc reg data = Popularity2 plots(maxpoints = 50000);
	model y_t = x1 x2 x3 x6 x7 x8 x9 x10 / vif lackfit alpha = 0.05;
	ods output anova = pop_anova_t;
	output out = pop_out_t p = yhat r = e;

* tests for normality of error terms;

proc univariate plots normal data = pop_out_t;
	var e;

* breuch-pagan test to check constant varience of error terms;

data pop_out_t;
  set pop_out_t nobs=total;
  e2 = e**2;

proc reg data = pop_out_t;
  model e2 = x1 x2 x3 x6 x7 x8 x9 x10;
  ods output anova = pop_anova2_t;

data pop_anova_t;
  set pop_anova_t;
  if source='Error' then call symput ('sse' , ss);

data pop_anova2_t;
  set pop_anova2_t;
  if source='Model' then call symput ('ssr', ss);

data pop_BP_t;
  BP_statistic = (&ssr/9)/(&sse/39644)**2;
  pvalue = 1 - probchi(BP_statistic, 8);
  ssr = &ssr;
  sse = &sse;

proc print data = pop_BP_t;





