open ShoppingCart
open ShoppingCart.Client.Catalog
open ShoppingCart.Client.Console
open ShoppingCart.Client.LoggedCart

System.Console.Clear()

// printfn "=== Demo: Bundle Discounts ==="
// printfn ""

// // Demo 1: Coffee mug bundle (2 mugs get 10%% off each)
// printfn "Demo 1: Buy 4 coffee mugs (2x bundle of 2 mugs @ 10%% off each)"
// let cart1 =
//   loggedCart logger {
//     add prd3
//     add prd3
//     add prd3
//     add prd3
//     bundle_discount [(prd3.Id, 2)] 10M
//   }

// printfn "Total: $%.2f" (Cart.calculateTotal cart1)
// printfn "  (Expected: All 4 mugs at $%.2f each = $%.2f)" (prd3.Price * 0.90M) (prd3.Price * 0.90M * 4M)
// printfn ""

// // Demo 2: Headphones + Running Shoes bundle
// printfn "Demo 2: Buy 3 headphones + 2 running shoes (bundle: 1 headphone + 1 shoe @ 10%% off each)"
// let cart2 =
//   loggedCart logger {
//     add prd1
//     add prd1
//     add prd1
//     add prd2
//     add prd2
//     bundle_discount [prd1.Id, 1; prd2.Id, 1] 10M
//   }

// printfn "Total: $%.2f" (Cart.calculateTotal cart2)
// printfn "  (Expected: 2 bundles applied + 1 headphone at full price)"
// printfn "  - 2 headphones @ $%.2f = $%.2f" (prd1.Price * 0.90M) (prd1.Price * 0.90M * 2M)
// printfn "  - 2 shoes @ $%.2f = $%.2f" (prd2.Price * 0.90M) (prd2.Price * 0.90M * 2M)
// printfn "  - 1 headphone @ $%.2f = $%.2f" prd1.Price prd1.Price
// printfn "  - Total: $%.2f" (prd1.Price * 0.90M * 2M + prd2.Price * 0.90M * 2M + prd1.Price)
// printfn ""

// Demo 3: Multiple bundles at once
printfn "Demo 3: Buy 2 coffee mugs + 1 headphone + 1 running shoe (both bundles apply)"
let cart3 =
  loggedCart logger {
    add prd3
    add prd3
    add prd1
    add prd2
    bundle_discount [(prd3.Id, 2)] 10M
    bundle_discount [(prd1.Id, 1); (prd2.Id, 1)] 10M
  }

printfn "Total: $%.2f" (Cart.calculateTotal cart3)
printfn "  (Expected: Bundle 1 + Bundle 2 both applied)"
printfn "  - 2 mugs @ $%.2f = $%.2f" (prd3.Price * 0.90M) (prd3.Price * 0.90M * 2M)
printfn "  - 1 headphone @ $%.2f = $%.2f" (prd1.Price * 0.90M) (prd1.Price * 0.90M)
printfn "  - 1 shoe @ $%.2f = $%.2f" (prd2.Price * 0.90M) (prd2.Price * 0.90M)
printfn "  - Total: $%.2f" (prd3.Price * 0.90M * 2M + prd1.Price * 0.90M + prd2.Price * 0.90M)
printfn ""
