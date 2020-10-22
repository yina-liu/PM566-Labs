Lab 10 - SQL
================

# Setup

``` r
knitr::opts_chunk$set(eval = FALSE)
library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")
```

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)
```

``` r
dbListTables(con)
```

TIP: Use can use the following QUERY to see the structure of a table

``` sql
PRAGMA table_info(actor)
```

SQL references:

<https://www.w3schools.com/sql/>

# Exercise 1

Retrive the actor ID, first name and last name for all actors using the
`actor` table. Sort by last name and then by first name.

``` sql
SELECT actor_id, first_name, last_name
FROM actor
ORDER by last_name, first_name
```

# Exercise 2

Retrive the actor ID, first name, and last name for actors whose last
name equals ‘WILLIAMS’ or ‘DAVIS’.

``` sql
SELECT actor_id,first_name,last_name
FROM actor
WHERE last_name IN ('WILLIAMS','DAVIS') 
```

# Exercise 3

Write a query against the `rental` table that returns the IDs of the
customers who rented a film on July 5, 2005 (use the rental.rental\_date
column, and you can use the date() function to ignore the time
component). Include a single row for each distinct customer ID.

``` sql
SELECT DISTINCT customer_id
FROM rental
WHERE date(rental_date) = '2005-07-05'
```

# Exercise 4

## Exercise 4.1

Construct a query that retrives all rows from the `payment` table where
the amount is either 1.99, 7.99, 9.99.

``` sql
SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)
```

## Exercise 4.2

Construct a query that retrives all rows from the `payment` table where
the amount is greater then 5

``` sql
SELECT *
FROM payment
WHERE amount > 5 
```

## Exercise 4.2

Construct a query that retrives all rows from the `payment` table where
the amount is greater then 5 and less then 8

``` sql
SELECT *
FROM payment
WHERE amount>5 AND amount<8
```

# Exercise 5

Retrive all the payment IDs and their amount from the customers whose
last name is ‘DAVIS’.

``` sql
SELECT p.payment_id, p.amount, c.last_name
FROM customer AS c
  INNER JOIN payment AS p
ON c.customer_id = p.customer_id
WHERE c.last_name = 'DAVIS' 
```

# Exercise 6

## Exercise 6.1

Use `COUNT(*)` to count the number of rows in `rental`

``` sql
SELECT COUNT(*) AS n_rows
FROM rental
```

## Exercise 6.2

Use `COUNT(*)` and `GROUP BY` to count the number of rentals for each
`customer_id`

``` sql
SELECT customer_id,COUNT(*)
FROM rental
GROUP BY customer_id
```

## Exercise 6.3

Repeat the previous query and sort by the count in descending order

``` sql
SELECT customer_id,COUNT(*) AS count
FROM rental
GROUP BY customer_id
ORDER BY count DESC
```

## Exercise 6.4

Repeat the previous query but use `HAVING` to only keep the groups with
40 or more.

``` sql
SELECT customer_id,COUNT(*) AS count
FROM rental
GROUP BY customer_id
HAVING count>=40
ORDER BY count DESC
```

# Exercise 7

The following query calculates a number of summary statistics for the
payment table using `MAX`, `MIN`, `AVG` and `SUM`

``` sql
SELECT 
   MAX(amount) AS max_amount,
   MIN(amount) AS min_amount,
   AVG(amount) AS avg_amount,
   SUM(amount) AS sum_amount
FROM payment
```

## Exercise 7.1

Modify the above query to do those calculations for each `customer_id`

``` sql
SELECT customer_id,
MAX(amount) AS max_amount,
MIN(amount) AS min_amount,
AVG(amount) AS avg_amount,
SUM(amount) AS sum_amount
FROM payment
GROUP BY customer_id
```

## Exercise 7.2

Modify the above query to only keep the `customer_id`s that have more
then 5 payments

``` sql
SELECT customer_id,
   MAX(amount) AS max_amount,
   MIN(amount) AS min_amount,
   AVG(amount) AS avg_amount,
   SUM(amount) AS sum_amount,
   COUNT(*) AS count
FROM payment
GROUP BY customer_id
HAVING count >5
```

# Cleanup

Run the following chunk to disconnect from the connection.

``` r
# clean up
dbDisconnect(con)
```
