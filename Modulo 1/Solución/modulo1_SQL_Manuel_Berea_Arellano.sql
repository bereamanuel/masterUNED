-- Databricks notebook source
-- MAGIC %md
-- MAGIC # Evalución 2025 - Módulo 1 
-- MAGIC Manuel Berea Arellano
-- MAGIC En el siguiente cuaderno estarán todos los ejercicios pedidos en SQL.

-- COMMAND ----------

-- MAGIC %md
-- MAGIC ## Manejo de bases de datos
-- MAGIC Se piden las siguientes cuestiones:
-- MAGIC
-- MAGIC - Obtén una vista de todos los pedidos, mostrando el nombre del cliente, la fecha del pedido, el nombre del producto, la cantidad ordenada y el precio del producto

-- COMMAND ----------

SELECT 
   C.`First Name` AS CLIENTE
  ,O.`date` AS FECHA
  ,P.name AS PRODUCTO
  ,OT.quantity AS CANTIDAD
  ,P.price AS PRECIO
FROM orders1 as O
JOIN customers1 AS C ON O.customer_id=C.ID
JOIN orderdetails1 AS OT on O.id = OT.order_id
JOIN products1 AS P ON P.id=OT.product_id
ORDER BY CANTIDAD DESC

-- COMMAND ----------

-- MAGIC %md
-- MAGIC - Encuentre las cinco categorías que tienen el mayor número de productos asociados, ordenándolas de mayor a menor

-- COMMAND ----------

 SELECT 
   C.NAME AS CATEGORIA
  ,COUNT( DISTINCT P.id ) AS N_PRODUCT
 FROM categories1 AS C
 JOIN products1 AS P ON P.category_id=C.id
 GROUP BY CATEGORIA
 ORDER BY N_PRODUCT DESC

-- COMMAND ----------

-- MAGIC %md
-- MAGIC - Determine cuál es el empleado que ha gestionado el mayor número de pedidos y muestra su nombre completo junto con el total de pedidos asignados

-- COMMAND ----------

SELECT 
   E.first_name AS NOMBRE
  ,E.last_name AS APELLIDO
  , COUNT(O.customer_id) AS N_PEDIDOS
FROM orders1 AS O 
JOIN employees1 AS E ON E.employee_id = O.employee_id
GROUP BY NOMBRE, APELLIDO
ORDER BY N_PEDIDOS DESC
LIMIT 1

-- COMMAND ----------

-- MAGIC %md
-- MAGIC - Calcule el total de ingresos generados por cada producto (multiplicando el precio por la cantidad ordenada), mostrando el nombre del producto, la cantidad total vendida y el ingreso total. Ordena los resultados por ingreso total en orden descendente

-- COMMAND ----------

SELECT
  PRODUCTO
  ,CANTIDAD
  ,CANTIDAD*PRECIO AS INGRESO_TOTAL
FROM (
  SELECT 
   P.name AS PRODUCTO
  ,P.price AS PRECIO
  ,COUNT(OT.order_id) AS CANTIDAD
  FROM orderdetails1 AS OT
  JOIN products1 AS P ON OT.product_id = P.id
  GROUP BY PRODUCTO, PRECIO)
ORDER BY INGRESO_TOTAL

