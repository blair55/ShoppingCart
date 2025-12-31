open ShoppingCart.Client.Catalog
open ShoppingCart.Client.Console
open ShoppingCart.Client.LoggedCart

System.Console.Clear()

loggedCart logger {
  add prd1
  // add prd2 10M
}
|> ignore
