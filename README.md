### Lendingclub
Lending loan data

----

> The LendingClub makes several datasets avaliable on there website, it's a real world data   set with a nice mix of categorical and continuous variables. i am going to use the 2007 to 2011 file (LoanStats3a.csv), and my goal was to check the characteristics we use to check for customers that are more likely to default on there loans

After loading the data into R and look at the status columns. i notice that there where several different statuses which seem to indicate good repayment behaviour and several more that indicate less than perfect repayment behaviour, and we have 42,536 customers which are also loan applicants

> I am going to use a binary classification problem, so the first step is to decide what statuses i can consider as a good and bad customers. For the loan status if a loan was charge-off, default, does not meet the credit policy.status:charged off, late (16-30 days), late (31-120 days) i consider this as "bad customers" on the other way i considered fully paid off loans and current as "good customers". And if a status does not recieved loan categorized it as NA.

> I compared the default rates and the distributions of each variables, and i visualize  the distributions of each variable.from there i selected out a few variables that could appear to be significant differences in the bad and good customers.

 Similarly, the issue date of the loan application  was increasing by the year from 2007 to 2011. I also looked at variables that missing variables and i did mean   imputations on the variables so that i don't loose meaningful information from the data.

> Fitting the model using the library caret package in r i build the model using training set predicting using the test data set. in this to predict which class a customers belongs to "bad" or "good" they are different ways to check for testing if the model worked really or to select the best models, in this case i am going to use resampling bootstrapped with 25 replicate that comes  with potential bias that may come from bootstrapped sampling

----
## Thanks
* [markdown-js](https://github.com/kcmbonu15-js)
