USE H_Market;



/* Q1 GROUP BY/HAVING
Find the average household income of customers 
who have more or equal 3 family members in thier family.
(rounded average income to the integer)
The result should be 3 columns & 2 rows.
*/
SELECT (hh_adults+hh_children) AS members,ROUND(AVG(hh_income)) AS avg_income
FROM customers
WHERE(hh_adults+ hh_children)>=3
GROUP BY members;

#SOLUTION, 3 columns & 2 rows
#3 members avg_income 106755
#4 members avg_income 78165
#5 members avg_income 68928

/* Q2 Subqueries
How many tea products and the average price (rounded to the integer) of tea products measured in ounces (oz). 
*/
SELECT COUNT(subcategory) AS num_tea,
       (SELECT ROUND(AVG(regular_price)) FROM products WHERE volume_of_measurement ='oz' AND subcategory= 'tea' ) AS avg_price
FROM products
WHERE subcategory= 'tea' 
GROUP BY subcategory ;
#SOLUTION, 2 columns & 1 rows#
#How many tea :30 Average price:6

/* Q3 CASE
Determine the count of customers categorized by income levels. 
The income levels are defined as follows: 
'Low Income': less than or equal to $80,000
'Medium Income' : between $80,001 and $150,000, 
'High Income' : between $150,001 and $200,000, 
'Super High Income' beyond $200,000. 
The results should include the count of customers in each income category, 
and the final output should be ordered by the customer count in ascending order.
The result should be 2 columns & 4 rows.
*/

SELECT COUNT(*) AS num_customer,
      CASE 
	  WHEN hh_income<=80000 THEN 'Low Income'
      WHEN hh_income >80000 AND hh_income < 150000 THEN 'Medium Income'
      WHEN hh_income > 150000 AND hh_income < 200000 THEN 'High Income'
      ELSE 'Supre_High_Income'END 
      AS income_category
FROM customers
GROUP BY income_category
ORDER BY num_customer;

#SOLUTION, 2 columns & 4 rows
#High Income:2
#Super High Income:5
#Low Income:19
#Medium Income:24


/* Q4 INNER JOIN
Find the count of products, the average regular price (rounded to one decimal place), and the category for products priced over $7.00. 
The calculation should only consider products in the 'food' category (excluding 'seafood') and purchased on '2023-09-26'. 
The result should be 3 columns & 2 rows.
*/

SELECT 
COUNT(a.product_id) AS num_product,
a.category,
ROUND(AVG(regular_price),1) AS avg_regular_price
FROM products AS a
INNER JOIN purchases AS b
ON a.product_id= b.prod_id
WHERE (a.regular_price>7.0
AND b.shopping_date='2023-09-26'
AND a.category LIKE'%food%'AND a.category NOT LIKE 'seafood')
GROUP BY a.category ;

#SOLUTION, 3 columns & 2 rows
#frozen foods:3, average regular price $8.6
#prepared foods:5, average regular price $10.3

/* Q5 LEFT/RIGHT JOIN
How many distinct customers who made purchases on '2023-09-26' for products in the 'food' category(excluding 'seafood'). 
Utilize a combination of RIGHT JOIN for customer purchases and LEFT JOIN for product details. 
The results should represent the unique customer count for this specific shopping date and category(1 columns & 1 rows).
*/

SELECT COUNT(DISTINCT a.cust_id) AS num_customer
FROM customers AS a
LEFT JOIN purchases AS b
ON a.cust_id = b.cust_id
RIGHT JOIN products AS c
ON b.prod_id=c.product_id
WHERE b.shopping_date='2023-09-26'
AND c.category LIKE'%food%'AND c.category NOT LIKE 'seafood';

#SOLUTION, 1 columns & 1 rows
# How many :5





        
        



      
      







