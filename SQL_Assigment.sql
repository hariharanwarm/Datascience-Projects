-- ************************************************************************************************************************************

--                                                                 Environment Setup

-- ************************************************************************************************************************************

USE superstoresDB;
SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
select @@GLOBAL.sql_mode;

-- ************************************************************************************************************************************

--                                                                 Run Basic Queries to understand the data

-- ************************************************************************************************************************************
-- Run DESCRIBE to understand the column names and data types 

DESCRIBE market_fact;
DESCRIBE cust_dimen;
DESCRIBE prod_dimen;
DESCRIBE shipping_dimen;
DESCRIBE orders_dimen;

-- Below Queries are run to understand number of rows and NA if present in a table for all 5 tables

-- 1.market_fact table

SELECT COUNT(*) FROM market_fact;

SELECT SUM(Ord_id IS NULL)+SUM(Prod_id IS NULL)+SUM(Ship_id IS NULL)+SUM(Cust_id IS NULL)+SUM(Sales IS NULL)+
SUM(DisCOUNT IS NULL)+SUM(Order_Quantity IS NULL)+SUM(Profit IS NULL)+SUM(Shipping_Cost IS NULL)+SUM(Product_Base_Margin IS NULL) AS null_count
FROM market_fact;

-- 2.cust_dimen table

SELECT COUNT(*) FROM cust_dimen;

SELECT SUM(Customer_Name IS NULL)+SUM(Province IS NULL)+SUM(Region IS NULL)+SUM(Customer_Segment IS NULL)+SUM(Cust_id IS NULL) 
	AS null_count FROM cust_dimen;

-- 3.prod_dimen table

SELECT COUNT(*) FROM prod_dimen;

SELECT  SUM(Product_Category IS NULL) + SUM(Product_Sub_Category IS NULL) + SUM(Prod_id IS NULL) 
AS null_COUNT FROM prod_dimen;

-- 4.shipping_dimen

SELECT COUNT(*) FROM shipping_dimen;

SELECT SUM(Order_ID IS NULL)+ SUM(Ship_Mode IS NULL)+SUM(Ship_Date IS NULL)+SUM(Ship_id IS NULL) 
AS null_count
FROM shipping_dimen;

-- 5.orders_dimen

SELECT COUNT(*) FROM orders_dimen;

SELECT SUM(Order_ID IS NULL) + SUM(Order_Date IS NULL) + SUM(Order_Priority IS NULL)+ SUM(Ord_id IS NULL) 
AS null_COUNT FROM orders_dimen;



--                                                   ASSIGMENT for 15th April 2018 Submission

--                                                                          START of Assignment

-- ************************************************************************************************************************************

--                                                                 Task 1 Understanding the Data

-- ************************************************************************************************************************************

/*    A. Describe the data in hand in your own words. (Word Limit is 500) 

Answer :
-----------
There are five datasets related to superstore provided which is imported into Mysql for analyses ,of which 
a)The market_fact table details the sales, discount, profit, shipping cost and product base margin for products against order id, product id, shipping id and Cust id.
This table has 8399 rows with 63 NAs.63 NA are part of the Product_Base_Margin column.
b)The cust_dimen table details the customer details such their name, province and region they belong to along with thier customer segment and customer id.
This table has 1832 rows with no null values.
c)The prod_dimen table details the product id, its category and sub category.
This table has 17 rows with no null values.
d)The shipping_dimen table details the shipping related information against order id such as shipping mode,shipping date and its id.
This table has 7701 rows with no null values.
e)The orders_dimen table details the orders related information such as order id, order date, order priority and order id.
This table has 5506 rows with no null values.

The market_fact table has columns such as ord_id,prod_id,ship_id,cust_id that can be used to link with the other tables such as orders_dimen,
prod-dimen,shipping_dimen and cust_dimen respectively to retrieve informations that are not in market_facts table.

 
     B. Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table don ’ t have Primary Key or Foreign Key, then specifically mention it in your answer. ) 

Answer:
----------
a)market_fact table
Primary Key : None
Foreign keys : Ord_id,Prod_id,Ship_id,Cust_id

b)cust_dimen table
Primary Key : Cust_id
Foreign Keys : None

c)prod_dimen table
Primary Key : Prod_id
Foreign Keys : None

d)shipping_dimen table
Primary Key : Ship_id
Foreign Keys : None

e)orders_dimen table
Primary Key : Ord_id
Foreign Keys : None

*/


-- ************************************************************************************************************************************

--                                                                 Task 2 Baic Analysis

-- ************************************************************************************************************************************

/* A. Find the total and the average sales (display total_sales and avg_sales)  
   Result : Total_Sales = 14915600.82400002, Average_Sales = 1775.8781788308156 */

SELECT 
	SUM(sales) Total_Sales,
	AVG(sales) Average_Sales FROM market_fact;
    
/* B.  Display the number of customers in each region in decreasing order of no_of_customers. 
  The result should contain columns Region, no_of_customers */

SELECT 
	Region,
    COUNT(*) No_of_Customers FROM cust_dimen 
    GROUP BY Region
    ORDER BY No_of_Customers DESC;
    
-- C. Find the region having maximum customers (display the region name and max(no_of_customers) 

SELECT 
	Region,
    COUNT(*) No_of_Customers FROM cust_dimen 
    GROUP BY Region
    ORDER BY No_of_Customers DESC
    LIMIT 1;
    ;
    
-- D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold) 

 SELECT 
	Prod_id,COUNT(*) No_of_products_sold
    FROM market_fact
    GROUP BY Prod_id 
    ORDER BY No_of_products_sold DESC
    ;

-- E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer name, no_of_tables purchased)  

SELECT cd.Customer_Name,COUNT(mf.Sales) No_of_Tables_Purchase
	FROM cust_dimen cd
    INNER JOIN market_fact mf
    ON cd.Cust_id = mf.Cust_id
    INNER JOIN prod_dimen pd
    ON pd.Prod_id = pd.Prod_id
	WHERE pd.Product_Sub_Category = 'TABLES'
	AND cd.Region = 'ATLANTIC'
	GROUP BY cd.Customer_Name
    ;

-- ************************************************************************************************************************************

--                                                                 Task 3 Advanced Analysis Write sql queries for the following: 

-- ************************************************************************************************************************************

-- A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)? 

SELECT pd.Product_Category,SUM(mf.Profit) Profits
	FROM market_fact mf
    INNER JOIN prod_dimen pd
    ON mf.Prod_id = pd.Prod_id
    GROUP BY pd.Product_Category
    ORDER BY Profits DESC;
    
-- B. Display the product category, product sub-category and the profit within each sub-category in three columns.  

SELECT pd.Product_Category,pd.Product_Sub_Category,SUM(mf.Profit) Profits
	FROM market_fact mf
    INNER JOIN prod_dimen pd
    ON mf.Prod_id = pd.Prod_id
    GROUP BY pd.Product_Category,pd.Product_Sub_Category
    ORDER BY pd.Product_Category DESC;
    
/* C. Where is the least profitable product subcategory shipped the most? 

Answer - Ontario

Step 1 : Below query identifies the least profitable product which is 'Tables' 
*/
SELECT 
 pd.Product_Sub_Category,COUNT(mf.ship_id) No_of_shipment,SUM(mf.Profit) Profits
	FROM market_fact mf
    INNER JOIN prod_dimen pd
    ON mf.Prod_id = pd.Prod_id
    GROUP BY pd.Product_Sub_Category
    ORDER BY Profits  ASC
    LIMIT 1
    ;
-- Step2 : finding the region where the least profitable prod sub category is shipped   which is 'Ontario'
SELECT 
	cd.Region,pd.Product_Sub_Category,COUNT(Ship_id) No_of_shipment ,sum(Profit) Profit FROM  prod_dimen pd
    INNER JOIN market_fact mf
    ON pd.Prod_id = mf.Prod_id
    INNER JOIN cust_dimen cd
    ON cd.Cust_id = mf.Cust_id
    WHERE pd.Product_Sub_Category = 'TABLES'
    GROUP BY cd.Region,pd.Product_Sub_Category
    ORDER BY Profit ASC
    LIMIT 1
    ;
/* Second part of Question C : For the least profitable product sub-category, display the  region-wise no_of_shipments and the profit made in
 each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)  Note: You can hardcode the name of the least profitable product sub-category  

 Answer : Below query lists regions where TABLES are shipped with its profit listed in descending order*/

SELECT 
cd.Region,COUNT(Ship_id) No_of_shipment,sum(Profit) Profit FROM  prod_dimen pd
    INNER JOIN market_fact mf
    ON pd.Prod_id = mf.Prod_id
    INNER JOIN cust_dimen cd
    ON cd.Cust_id = mf.Cust_id
    WHERE pd.Product_Sub_Category = 'TABLES'
    GROUP BY cd.Region
    ORDER BY Profit DESC
    ;

-- ************************************************************************************************************************************

--                                                               END of Assignment

-- ************************************************************************************************************************************


