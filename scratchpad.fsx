#load "ShoppingCart/Index.fs"

open ShoppingCart

let cart = Map.empty

cart
|> Cart.Item.add
  { Id = "p1"
    Name = "Product 1"
    Price = 10.0M
    Category = "Category A" }
|> Cart.Item.add
  { Id = "p2"
    Name = "Product 2"
    Price = 20.0M
    Category = "Category B" }
|> Cart.Item.updateQuantity ("p1", 3)
|> Cart.Item.remove "p2"
|> Cart.calculateTotal
|> printfn "Total Price: %M"
