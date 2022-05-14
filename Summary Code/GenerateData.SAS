libname lib "C:\Users\jluu\Dropbox\Research\Groshen\SAS Code\Run 6\Dose Level 9";
Options MPRINT;

/*Import all 9 experimental conditions*/
/*Low=1, Medium=2, High=3*/
%macro retrieveData(dataName, fileName, betaNum, pnNum);
data &dataName;
	infile "C:\Users\jluu\Dropbox\Research\Groshen\SAS Code\Run 6\Dose Level 9\&fileName" delimiter=',' firstobs=2;
	input simNum finalMTD dosesTested dltCount ptCount ptCount1 ptCount2 ptCount3 pn beta gamma p1 p2 p3 p4 p5 p6 p7 p8;

	catBeta = &betaNum;
	catPN = &pnNum;
run;
%mend retrieveData;

%retrieveData(hBhP, High Beta_High P(D).csv, 3, 3);
%retrieveData(hBmP, High Beta_Medium P(D).csv, 3, 2);
%retrieveData(hBlP, High Beta_Low P(D).csv, 3, 1);
%retrieveData(mBhP, Medium Beta_High P(D).csv, 2, 3);
%retrieveData(mBmP, Medium Beta_Medium P(D).csv, 2, 2);
%retrieveData(mBlP, Medium Beta_Low P(D).csv, 2, 1);
%retrieveData(lBhP, Low Beta_High P(D).csv, 1, 3);
%retrieveData(lBmP, Medium Beta_Medium P(D).csv, 1, 2);
%retrieveData(lBlP, Low Beta_Low P(D).csv, 1, 1);

/*Combine 9 experimental conditions into 1 data step and save it as a permanent dataset*/
data lib.combinedData;
	set hBhP hBmP hBlP mBhP mBmP mBlP lBhP lBmP lBlP;
run;

data combinedData;
	set lib.combinedData;

	/*arcMTD = arsin(sqrt(finalMTD));
	logMTD = log(finalMTD);*/

	sqrtMTD = sqrt(finalMTD);
	pn_catBeta = pn*catBeta;
	catPN_beta = catPN*beta;
	catPN_catBeta = catPN*catBeta;
run;


/********Linear regression for final MTD********/
proc glm data=combinedData;
	where finalMTD > 0;
	class catPN catBeta;
	model sqrtMTD = pn beta catPN catBeta / clparm solution;
	output out=modeltst r=resid p=pred;
run;

/*Test for normality in the model*/
proc univariate plot normal data=modeltst;
	var resid;
run;

/*Test for linearity/homoscedasticity in the model*/
proc gplot data=modeltst;
	symbol1 v=star;
	plot resid*pred=dosesTested;
run;

/*SqrtMTD vs PN for each category*/
proc sort data=combinedData;
	by catBeta catPN;
run;

proc sgpanel data=modeltst;
	panelby catBeta catPN / columns=3 rows=3;
	scatter y=resid x=pred;
run;

/*Forwards, backwards, and stepwise prediction modeling*/
proc glmselect data=combinedData;
	model sqrtMTD = pn beta catBeta catPN pn*catBeta catPN*beta catPN*catBeta  / selection=backward sls=0.10 hierarchy=single showpvalues select=sl stop=sl;
run;

proc glmselect data=combinedData;
	model sqrtMTD = pn beta catBeta catPN pn*catBeta catPN*beta catPN*catBeta  / selection=forward sle=0.10 details=all hierarchy=single showpvalues select=sl stop=sl;
run;

proc glmselect data=combinedData;
	model sqrtMTD = pn beta catBeta catPN pn*catBeta catPN*beta catPN*catBeta  / selection=stepwise sls=0.10 sle=0.10 details=all hierarchy=single showpvalues select=sl stop=sl;
run;


