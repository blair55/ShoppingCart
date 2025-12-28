open System
open Expecto
open Expecto.Flip
open ShoppingCart


let d0 = Discount.Zero

let icart items =
  { Items = Map.ofList items; Discount = d0 }

let prd =
  { Id = "p1"
    Name = "Product"
    Price = 0M
    Category = "Category" }


let cartItemTests =

  testList
    "Cart Item Tests"
    [ test "add item to empty cart" {
        let cart = icart []
        let expected = icart [ prd.Id, (prd, 1, d0) ]
        Cart.Item.add (prd, d0) cart |> Expect.equal "adds item with qty 1" expected
      }

      test "add existing item increments quantity" {
        let cart = icart [ prd.Id, (prd, 2, d0) ]
        let expected = icart [ prd.Id, (prd, 3, d0) ]
        Cart.Item.add (prd, d0) cart |> Expect.equal "increments item quantity" expected
      }

      test "add existing item takes latest discount" {
        let dsc = Discount 10M
        let cart = icart [ prd.Id, (prd, 2, d0) ]
        let expected = icart [ prd.Id, (prd, 3, dsc) ]
        Cart.Item.add (prd, dsc) cart |> Expect.equal "updates item discount" expected
      }

      test "remove only item from cart" {
        let cart = icart [ prd.Id, (prd, 2, d0) ]
        let expected = icart []
        Cart.Item.remove prd.Id cart |> Expect.equal "returns empty cart" expected
      }

      test "remove item from cart with multiple items" {
        let prd2 = { prd with Id = "p2" }
        let cart = icart [ prd.Id, (prd, 2, d0); prd2.Id, (prd2, 1, d0) ]
        let expected = icart [ prd2.Id, (prd2, 1, d0) ]

        Cart.Item.remove prd.Id cart
        |> Expect.equal "returns cart without removed item" expected
      }

      test "remove item that doesn't exist in cart" {
        let cart = icart [ prd.Id, (prd, 2, d0) ]
        let expected = cart
        Cart.Item.remove "123" cart |> Expect.equal "returns unchanged cart" expected
      }

      test "update item quantity in cart" {
        let cart = icart [ prd.Id, (prd, 2, d0) ]
        let expected = icart [ prd.Id, (prd, 5, d0) ]

        Cart.Item.updateQuantity (prd.Id, 5) cart
        |> Expect.equal "updates item quantity" expected
      }

      test "update item quantity to zero" {
        let cart = icart [ prd.Id, (prd, 2, d0) ]
        let expected = icart []

        Cart.Item.updateQuantity (prd.Id, 0) cart
        |> Expect.equal "removes item from cart" expected
      }

      test "update item with negative quantity" {
        let cart = icart [ prd.Id, (prd, 2, d0) ]
        let expected = icart []

        Cart.Item.updateQuantity (prd.Id, -3) cart
        |> Expect.equal "removes item from cart" expected
      }

      test "update item that doesn't exist in cart" {
        let cart = icart [ prd.Id, (prd, 2, d0) ]
        let expected = cart

        Cart.Item.updateQuantity ("123", 5) cart
        |> Expect.equal "returns unchanged cart" expected
      }

      ]


let cartTotalTests =

  testList
    "Cart Total Tests"
    [ test "calculate total for empty cart" {
        let cart = icart []
        Cart.calculateTotal cart |> Expect.equal "total is zero" 0.0M
      }

      test "calculate total for cart with items" {
        let prd1, p1Qty = { prd with Id = "p1"; Price = 10.0M }, 2
        let prd2, p2Qty = { prd with Id = "p2"; Price = 15.0M }, 3
        let cart = icart [ prd1.Id, (prd1, p1Qty, d0); prd2.Id, (prd2, p2Qty, d0) ]
        let expectedTotal = prd1.Price * decimal p1Qty + prd2.Price * decimal p2Qty

        Cart.calculateTotal cart
        |> Expect.equal "calculates correct total" expectedTotal
      }

      test "calculate total for cart with items with discounts" {
        let prd1, p1Qty, p1Dsc = { prd with Id = "p1"; Price = 10.0M }, 2, Discount 10M
        let prd2, p2Qty, p2Dsc = { prd with Id = "p2"; Price = 15.0M }, 3, Discount 20M
        let prd3, p3Qty, p3Dsc = { prd with Id = "p3"; Price = 30.0M }, 3, Discount 35M

        let cart =
          { icart
              [ prd1.Id, (prd1, p1Qty, p1Dsc)
                prd2.Id, (prd2, p2Qty, p2Dsc)
                prd3.Id, (prd3, p3Qty, p3Dsc) ] with
              Discount = Discount 5M }

        let expectedTotal =
          prd1.Price * (1M - (p1Dsc + cart.Discount).Value / 100M) * decimal p1Qty
          + prd2.Price * (1M - (p2Dsc + cart.Discount).Value / 100M) * decimal p2Qty
          + prd3.Price * (1M - (p3Dsc + cart.Discount).Value / 100M) * decimal p3Qty

        Cart.calculateTotal cart
        |> Expect.equal "calculates correct total" expectedTotal
      }

      ]


let discountTests =

  testList
    "Discount Tests"
    [ test "discount value clamped between 0 and 100" {
        Expect.equal "clamps negative discount to 0" (Discount -10M).Value 0M
        Expect.equal "keeps valid discount unchanged" (Discount 50M).Value 50M
        Expect.equal "clamps discount over 100 to 100" (Discount 150M).Value 100M

        Expect.equal
          "clamps stacked discounts over 100 to 100"
          (Discount 75M + Discount 50M).Value
          100M
      }

      test "applying discount to price" {
        let price = 200.0M
        let discount = Discount 25M
        let expected = price * 0.75M
        discount.Apply price |> Expect.equal "applies discount correctly" expected
      }

      ]


let sampleDataTest =
  testList
    "Sample Data Tests"
    [ test "running shoes discount calculation" {
        let cart =
          { icart
              [ "2",
                ({ Id = "2"
                   Name = "Running Shoes"
                   Price = 89.99M
                   Category = "Footwear" },
                 1,
                 Discount 15M) ] with

              Discount = Discount 10M }

        let expectedTotal = 89.99M * 0.75M

        Cart.calculateTotal cart
        |> Expect.equal "calculates correct total" expectedTotal
      }

      test "full sample data cart total calculation" {
        let cart =
          { icart
              [ "1",
                ({ Id = "1"
                   Name = "Headphones"
                   Price = 59.99M
                   Category = "Electronics" },
                 1,
                 d0)

                "2",
                ({ Id = "2"
                   Name = "Running Shoes"
                   Price = 89.99M
                   Category = "Footwear" },
                 1,
                 Discount 15M)

                "3",
                ({ Id = "3"
                   Name = "Coffee Mug"
                   Price = 12.99M
                   Category = "Kitchen" },
                 1,
                 d0)

                "4",
                ({ Id = "4"
                   Name = "Premium Laptop"
                   Price = 1299.99M
                   Category = "Electronics" },
                 1,
                 d0)

                "5",
                ({ Id = "5"
                   Name = "Winter Jacket"
                   Price = 149.99M
                   Category = "Clothing" },
                 1,
                 d0)

                ] with

              Discount = Discount 10M }

        let expectedTotal = 1438.16M

        Math.Round(Cart.calculateTotal cart, 2)
        |> Expect.equal "calculates correct total" expectedTotal
      }

      ]


testList "" [ cartItemTests; cartTotalTests; discountTests; sampleDataTest ]
|> runTestsWithCLIArgs [] [||]
|> exit
