#load "ShoppingCart/Index.fs"

open ShoppingCart

let cart = { Items = Map.empty; Discount = Discount 10M }

let prd1, prd2, prd3, prd4, prd5 =
  { Id = "1"
    Name = "Headphones"
    Price = 59.99M
    Category = "Electronics" },
  { Id = "2"
    Name = "Running Shoes"
    Price = 89.99M
    Category = "Footwear" },
  { Id = "3"
    Name = "Coffee Mug"
    Price = 12.99M
    Category = "Kitchen" },
  { Id = "4"
    Name = "Premium Laptop"
    Price = 1299.99M
    Category = "Electronics" },
  { Id = "5"
    Name = "Winter Jacket"
    Price = 149.99M
    Category = "Clothing" }

cart
|> Cart.Item.add (prd1, Discount.Zero)
|> Cart.Item.add (prd2, Discount 15M)
|> Cart.Item.add (prd3, Discount.Zero)
|> Cart.Item.add (prd4, Discount.Zero)
|> Cart.Item.add (prd5, Discount.Zero)
|> Cart.Item.updateQuantity ("1", 3)
|> Cart.Item.remove "5"
|> Cart.calculateTotal
|> printfn "Total Price: %.2f"
