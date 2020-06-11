;Read File String & Converting into List by Split-Lines 
(def customerTable_list (list* (clojure.string/split-lines (slurp "cust.txt"))))
(def productTable_list (list* (clojure.string/split-lines (slurp "prod.txt"))))
(def salesTable_list (list* (clojure.string/split-lines (slurp "sales.txt"))))

;Maps For Customer
(def customer_ID_Info_Map {})
(def customer_ID_Name_Map {})

;Maps For Product
(def product_ID_Name_Map {})
(def product_ID_Cost_Map {})

;Maps For Sales
(def sales_ID_customerID_Map {})
(def sales_ID_productID_Map {})
(def sales_ID_ItemCount_Map {})

;Maps For Storing Total Sales for Customer & Total Count for Product
(def partial_Sales_Customer_Map {})
(def partial_Product_Count_Map {})
(def total_Sales_Customer_Map {})
(def total_Product_Count_Map {})

;Customer Map Data Storing
(defn insert_Into_C_Map [list_line]
						(def line (clojure.string/split list_line #"\|"))
						(def customer_ID_Name_Map (assoc customer_ID_Name_Map (get line 0) (get line 1)))
						(def customerInfo (str ":[" (get line 1) "," (get line 2) "," (get line 3) "]"))
  				(def customer_ID_Info_Map (assoc customer_ID_Info_Map (get line 0) customerInfo))
)

(defn customer_add [full_list]
						(if (not= (empty? (first (list* full_list))) true)
										(do
										   (insert_Into_C_Map (first (list* full_list)))
													(customer_add (rest (list* full_list)))
										)
						)
)

(customer_add customerTable_list)
(def sorted_customer_ID_Info (into (sorted-map) customer_ID_Info_Map))

;Product Map Data Storing
(defn insert_Into_P_Map [list_line1]
						(def line (clojure.string/split list_line1 #"\|"))
						(def product_ID_Name_Map (assoc product_ID_Name_Map (get line 0) (get line 1)))
				  (def product_ID_Cost_Map (assoc product_ID_Cost_Map (get line 0) (read-string (get line 2))))
)

(defn product_add [full_list1]
						(if (not= (empty? (first (list* full_list1))) true)
										(do
										   (insert_Into_P_Map (first (list* full_list1)))
													(product_add (rest (list* full_list1)))
										)
						)
)

(product_add productTable_list)
(def sorted_product_ID_Name (into (sorted-map) product_ID_Name_Map))

;Sales Map Data Storing
(defn insert_Into_S_Map [list_line2]
						(def line (clojure.string/split list_line2 #"\|"))
						(def sales_ID_customerID_Map (assoc sales_ID_customerID_Map (get line 0) (get line 1)))
      (def sales_ID_productID_Map (assoc sales_ID_productID_Map (get line 0) (get line 2)))
      (def sales_ID_ItemCount_Map (assoc sales_ID_ItemCount_Map (get line 0) (read-string (get line 3))))
)

(defn sales_add [full_list2]
						(if (not= (empty? (first (list* full_list2))) true)
										(do
										   (insert_Into_S_Map (first (list* full_list2)))
													(sales_add (rest (list* full_list2)))
										)
						)
)

(sales_add salesTable_list)
(def sorted_sales_ID_CustomerID (into (sorted-map) sales_ID_customerID_Map))

;Print Customer Table
(defn printOption1 [sorted_customer_ID_Info_Copy]
						(if (not= (empty? (first sorted_customer_ID_Info_Copy)) true)
										(do
													(def k (first (keys sorted_customer_ID_Info_Copy)))
													(def v (first (vals sorted_customer_ID_Info_Copy)))
													(println " "k v)
													(printOption1 (rest sorted_customer_ID_Info_Copy))
										)
						)
)

;Print Product Table
(defn printOption2 [sorted_product_ID_Name_Copy]
						(if (not= (empty? (first sorted_product_ID_Name_Copy)) true)
										(do
													(def k (first (keys sorted_product_ID_Name_Copy)))
													(def v (first (vals sorted_product_ID_Name_Copy)))
													(println " "k ":[" v "," (get product_ID_Cost_Map k) "]")
													(printOption2 (rest sorted_product_ID_Name_Copy))
										)
						)
)

;Print Sales Table
(defn printOption3 [sorted_sales_ID_CustomerID_Copy]
						(if (not= (empty? (first sorted_sales_ID_CustomerID_Copy)) true)
										(do
													(def k (first (keys sorted_sales_ID_CustomerID_Copy)))
													(def v (first (vals sorted_sales_ID_CustomerID_Copy)))
             (def productName (get product_ID_Name_Map (get sales_ID_productID_Map k)))
             (println " "k ":[" (get customer_ID_Name_Map v) "," productName "," (get sales_ID_ItemCount_Map k) "]")
													(printOption3 (rest sorted_sales_ID_CustomerID_Copy))
										)
						)
)

;Function For Counting Total Sales for Customer & Total Count for Product
;This Function will give us partial details.It will not give data of Customer
;who has not purchased anything and Product which no one has bought.
(defn partialOption45 [sorted_sales_ID_CustomerID_Copy]
						(if (not= (empty? (first sorted_sales_ID_CustomerID_Copy)) true)
										(do
													(def k (first (keys sorted_sales_ID_CustomerID_Copy)))
													(def v (first (vals sorted_sales_ID_CustomerID_Copy)))
             (def productName (get product_ID_Name_Map (get sales_ID_productID_Map k)))
             (def customerName (get customer_ID_Name_Map v))
             (def productPrice (get product_ID_Cost_Map (get sales_ID_productID_Map k)))
  											(def productCount (get sales_ID_ItemCount_Map k))
  											(def purchase (* productPrice productCount))
  											(def totalPurchases (+ purchase (get partial_Sales_Customer_Map customerName 0)))
  											(def partial_Sales_Customer_Map (assoc partial_Sales_Customer_Map customerName totalPurchases))
  											(def partial_Product_Count (+ productCount (get partial_Product_Count_Map productName 0)))
  											(def partial_Product_Count_Map (assoc partial_Product_Count_Map productName partial_Product_Count))
  											(partialOption45 (rest sorted_sales_ID_CustomerID_Copy))
										)
						)
)

(partialOption45 sorted_sales_ID_CustomerID)

;Function for adding missing data in partial Total Sales for customer
(defn totalCustomerSalesCount [customer_ID_Name_Map_Copy] 
						(if (not= (empty? (first customer_ID_Name_Map_Copy)) true)
										(do
													(def k (first (keys customer_ID_Name_Map_Copy)))
													(def v (first (vals customer_ID_Name_Map_Copy)))
													(def total_Sales_Customer_Map (assoc total_Sales_Customer_Map v (get partial_Sales_Customer_Map v 0)))
  											(totalCustomerSalesCount (rest customer_ID_Name_Map_Copy))
										)
						)
)

(totalCustomerSalesCount customer_ID_Name_Map)

;Function for adding missing data in partial Total Count For Product
(defn totalProductCount [product_ID_Name_Map_Copy] 
						(if (not= (empty? (first product_ID_Name_Map_Copy)) true)
										(do
													(def k (first (keys product_ID_Name_Map_Copy)))
													(def v (first (vals product_ID_Name_Map_Copy)))
													(def total_Product_Count_Map (assoc total_Product_Count_Map v (get partial_Product_Count_Map v 0)))
  											(totalProductCount (rest product_ID_Name_Map_Copy))
										)
						)
)

(totalProductCount product_ID_Name_Map)

;Function For 1. Display Customer Table
(defn printCustomerTable []
						(println "\n================== Customer Table ==================\n")
						(printOption1 sorted_customer_ID_Info)
						(println "\n====================================================\n")
)

;Function For 2. Display Product Table
(defn printProductTable []
						(println "\n=========== Product Table ===========\n")
						(printOption2 sorted_product_ID_Name)
						(println "\n=====================================\n")
)

;Function For 3. Display Sales Table
(defn printSalesTable []
						(println "\n============== Sales Table ==============\n")
						(printOption3 sorted_sales_ID_CustomerID)
						(println "\n=========================================\n")
)

;Function For 4. Total Sales for Customer
(defn totalCustomerSales []
    		(println "Please Enter Customer Name : ")
    		(def customerName (read-line))
    
    		(if (contains? total_Sales_Customer_Map customerName)
        		(do 
        					(println (str customerName ": $"(get total_Sales_Customer_Map customerName)))
        		)
        		(do 
        				(println "Customer Not Found!!!")
        				(println "Total Sales : $ 0.0")
          )
    )
)

;Function For 5. Total Count for Product
(defn totalProductCount []
    		(println "Please Enter Product Name : ")
    		(def productName (read-line))
    
    		(if (contains? total_Product_Count_Map productName)
        		(do 
        					(println (str productName ": "(get total_Product_Count_Map productName)))
        		)
        		(do 
        				(println "Product Not Found!!!")
        				(println "Total Product Count: 0")
          )
    )
)

;Function For 6.Exit
(defn exit []
    (println " Good Bye!!!")
    (System/exit 0)
)


;Function To Display Sales Menu
(defn displayMenu []
						(println "\n******** Sales Menu ********")
						(println "----------------------------")
						(println " 1. Display Customer Table")
						(println " 2. Display Product Table")
						(println " 3. Display Sales Table")
						(println " 4. Total Sales for Customer")
						(println " 5. Total Count for Product")
						(println " 6. Exit")
						(println "\n Enter an Option : ")

						(let [enteredOption (read-line)]
											(cond
																(= enteredOption "1")
																																				(do
																																				   (printCustomerTable)
																																				   (displayMenu)
																																				)
																(= enteredOption "2")
																																				(do
																																				   (printProductTable)
																																				   (displayMenu)
																																				)
																(= enteredOption "3")
																																				(do
																																				   (printSalesTable)
																																				   (displayMenu)
																																				)
																(= enteredOption "4")
																																				(do
																																				   (totalCustomerSales)
																																				   (displayMenu)
																																				)
																(= enteredOption "5")
																																				(do
																																				   (totalProductCount)
																																				   (displayMenu)
																																				)
																(= enteredOption "6")
																																				(do
																																				   (exit)
																																				)
																:else
																					(do
																					   (println "\n You Have Entered Wrong option!!! Please Enter Correct Option From 1 to 6.")
																					   (displayMenu)
																					)
											)
    	 )
)

(displayMenu)