open System
open Expecto
open Expecto.Flip
open ShoppingCart


let d0 = Discount.Zero

let icart items =
  { Items = items
    Discount = d0
    Bundles = [] }

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
        let expected = icart [ (prd, d0) ]
        Cart.Item.add (prd, d0) cart |> Expect.equal "adds item" expected
      }

      test "add existing item adds another instance" {
        let cart = icart [ (prd, d0); (prd, d0) ]
        let expected = icart [ (prd, d0); (prd, d0); (prd, d0) ]
        Cart.Item.add (prd, d0) cart |> Expect.equal "adds another instance" expected
      }

      test "add existing item takes latest discount for new instance" {
        let dsc = Discount 10M
        let cart = icart [ (prd, d0); (prd, d0) ]
        let expected = icart [ (prd, dsc); (prd, d0); (prd, d0) ]
        Cart.Item.add (prd, dsc) cart |> Expect.equal "adds with new discount" expected
      }

      test "remove only item from cart" {
        let cart = icart [ (prd, d0) ]
        let expected = icart []
        Cart.Item.remove prd.Id cart |> Expect.equal "returns empty cart" expected
      }

      test "remove one item from cart with multiple instances" {
        let cart = icart [ (prd, d0); (prd, d0) ]
        let expected = icart [ (prd, d0) ]

        Cart.Item.remove prd.Id cart
        |> Expect.equal "returns cart with one instance removed" expected
      }

      test "remove item from cart with multiple products" {
        let prd2 = { prd with Id = "p2" }
        let cart = icart [ (prd, d0); (prd2, d0) ]
        let expected = icart [ (prd2, d0) ]

        Cart.Item.remove prd.Id cart
        |> Expect.equal "returns cart without removed item" expected
      }

      test "remove item that doesn't exist in cart" {
        let cart = icart [ (prd, d0) ]
        let expected = cart
        Cart.Item.remove "123" cart |> Expect.equal "returns unchanged cart" expected
      }

      test "update item quantity in cart" {
        let cart = icart [ (prd, d0); (prd, d0) ]
        let expected = icart [ (prd, d0); (prd, d0); (prd, d0); (prd, d0); (prd, d0) ]

        Cart.Item.updateQuantity (prd.Id, 5) cart
        |> Expect.equal "updates item quantity" expected
      }

      test "update item quantity to zero" {
        let cart = icart [ (prd, d0); (prd, d0) ]
        let expected = icart []

        Cart.Item.updateQuantity (prd.Id, 0) cart
        |> Expect.equal "removes item from cart" expected
      }

      test "update item with negative quantity" {
        let cart = icart [ (prd, d0); (prd, d0) ]
        let expected = icart []

        Cart.Item.updateQuantity (prd.Id, -3) cart
        |> Expect.equal "removes item from cart" expected
      }

      test "update item that doesn't exist in cart" {
        let cart = icart [ (prd, d0) ]
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
        let prd1 = { prd with Id = "p1"; Price = 10.0M }
        let prd2 = { prd with Id = "p2"; Price = 15.0M }

        let cart =
          icart
            [ (prd1, d0)
              (prd1, d0)
              (prd2, d0)
              (prd2, d0)
              (prd2, d0) ]

        let expectedTotal = prd1.Price * 2M + prd2.Price * 3M

        Cart.calculateTotal cart
        |> Expect.equal "calculates correct total" expectedTotal
      }

      test "calculate total for cart with items with discounts" {
        let prd1, p1Dsc = { prd with Id = "p1"; Price = 10.0M }, Discount 10M
        let prd2, p2Dsc = { prd with Id = "p2"; Price = 15.0M }, Discount 20M
        let prd3, p3Dsc = { prd with Id = "p3"; Price = 30.0M }, Discount 35M

        let cart =
          { icart
              [ (prd1, p1Dsc)
                (prd1, p1Dsc)
                (prd2, p2Dsc)
                (prd2, p2Dsc)
                (prd2, p2Dsc)
                (prd3, p3Dsc)
                (prd3, p3Dsc)
                (prd3, p3Dsc) ] with
              Discount = Discount 5M }

        let expectedTotal =
          prd1.Price * (1M - (p1Dsc + cart.Discount).Value / 100M) * 2M
          + prd2.Price * (1M - (p2Dsc + cart.Discount).Value / 100M) * 3M
          + prd3.Price * (1M - (p3Dsc + cart.Discount).Value / 100M) * 3M

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
              [ ({ Id = "2"
                   Name = "Running Shoes"
                   Price = 89.99M
                   Category = "Footwear" },
                 Discount 15M) ] with

              Discount = Discount 10M }

        let expectedTotal = 89.99M * 0.75M

        Cart.calculateTotal cart
        |> Expect.equal "calculates correct total" expectedTotal
      }

      test "full sample data cart total calculation" {
        let cart =
          { icart
              [ ({ Id = "1"
                   Name = "Headphones"
                   Price = 59.99M
                   Category = "Electronics" },
                 d0)

                ({ Id = "2"
                   Name = "Running Shoes"
                   Price = 89.99M
                   Category = "Footwear" },
                 Discount 15M)

                ({ Id = "3"
                   Name = "Coffee Mug"
                   Price = 12.99M
                   Category = "Kitchen" },
                 d0)

                ({ Id = "4"
                   Name = "Premium Laptop"
                   Price = 1299.99M
                   Category = "Electronics" },
                 d0)

                ({ Id = "5"
                   Name = "Winter Jacket"
                   Price = 149.99M
                   Category = "Clothing" },
                 d0)

                ] with

              Discount = Discount 10M }

        let expectedTotal = 1438.16M

        Math.Round(Cart.calculateTotal cart, 2)
        |> Expect.equal "calculates correct total" expectedTotal
      }

      ]


let bundleDiscountTests =

  testList
    "Bundle Discount Tests"
    [ test "apply bundle discount to 2 coffee mugs" {
        let coffeeMug = { prd with Id = "p1"; Price = 12.99M }

        let bundle =
          { Items = [ (coffeeMug.Id, 2) ]; Discount = Discount 10M }

        let cart =
          { icart [ (coffeeMug, d0); (coffeeMug, d0) ] with Bundles = [ bundle ] }

        // Each mug: 12.99 * 0.90 = 11.691
        // Total: 11.691 * 2 = 23.382
        let expectedTotal = 12.99M * 0.90M * 2M

        Cart.calculateTotal cart
        |> Expect.equal "applies 10% discount to both mugs" expectedTotal
      }

      test "apply bundle discount exhaustively - 4 coffee mugs (2 bundles)" {
        let coffeeMug = { prd with Id = "p1"; Price = 12.99M }

        let bundle =
          { Items = [ (coffeeMug.Id, 2) ]; Discount = Discount 10M }

        let cart =
          { icart [ (coffeeMug, d0); (coffeeMug, d0); (coffeeMug, d0); (coffeeMug, d0) ] with
              Bundles = [ bundle ] }

        // All 4 mugs get discount (2 bundle applications)
        let expectedTotal = 12.99M * 0.90M * 4M

        Cart.calculateTotal cart
        |> Expect.equal "applies discount to all 4 mugs" expectedTotal
      }

      test "bundle discount with remaining items - 3 coffee mugs" {
        let coffeeMug = { prd with Id = "p1"; Price = 12.99M }

        let bundle =
          { Items = [ (coffeeMug.Id, 2) ]; Discount = Discount 10M }

        let cart =
          { icart [ (coffeeMug, d0); (coffeeMug, d0); (coffeeMug, d0) ] with
              Bundles = [ bundle ] }

        // 2 mugs with discount: 12.99 * 0.90 * 2 = 23.382
        // 1 mug at full price: 12.99
        let expectedTotal = 12.99M * 0.90M * 2M + 12.99M

        Cart.calculateTotal cart
        |> Expect.equal "applies discount to 2 mugs, 1 at full price" expectedTotal
      }

      test "apply cross-product bundle - headphones + running shoes" {
        let headphones = { prd with Id = "p1"; Price = 59.99M }
        let runningShoes = { prd with Id = "p2"; Price = 89.99M }

        let bundle =
          { Items = [ (headphones.Id, 1); (runningShoes.Id, 1) ]
            Discount = Discount 10M }

        let cart =
          { icart [ (headphones, d0); (runningShoes, d0) ] with Bundles = [ bundle ] }

        // Each item: price * 0.90
        let expectedTotal = 59.99M * 0.90M + 89.99M * 0.90M

        Cart.calculateTotal cart
        |> Expect.equal "applies 10% discount to both bundle items" expectedTotal
      }

      test "exhaustive bundle matching - 3 headphones + 2 running shoes" {
        let headphones = { prd with Id = "p1"; Price = 59.99M }
        let runningShoes = { prd with Id = "p2"; Price = 89.99M }

        let bundle =
          { Items = [ (headphones.Id, 1); (runningShoes.Id, 1) ]
            Discount = Discount 10M }

        let cart =
          { icart
              [ (headphones, d0)
                (headphones, d0)
                (headphones, d0)
                (runningShoes, d0)
                (runningShoes, d0) ] with
              Bundles = [ bundle ] }

        // Bundle applies 2 times (limited by 2 running shoes)
        // 2 headphones with discount: 59.99 * 0.90 * 2 = 107.982
        // 2 running shoes with discount: 89.99 * 0.90 * 2 = 161.982
        // 1 headphones at full price: 59.99
        let expectedTotal = 59.99M * 0.90M * 2M + 89.99M * 0.90M * 2M + 59.99M

        Cart.calculateTotal cart
        |> Expect.equal
          "applies bundle 2 times, leaves 1 headphones at full price"
          expectedTotal
      }

      test "bundle discount with cart-level discount" {
        let headphones = { prd with Id = "p1"; Price = 59.99M }
        let runningShoes = { prd with Id = "p2"; Price = 89.99M }

        let bundle =
          { Items = [ (headphones.Id, 1); (runningShoes.Id, 1) ]
            Discount = Discount 10M }

        let cart =
          { icart [ (headphones, d0); (runningShoes, d0) ] with
              Bundles = [ bundle ]
              Discount = Discount 5M }

        // Bundle discount: 10%, Cart discount: 5%, Total: 15%
        // Each item: price * 0.85
        let expectedTotal = 59.99M * 0.85M + 89.99M * 0.85M

        Cart.calculateTotal cart
        |> Expect.equal "stacks bundle and cart discounts" expectedTotal
      }

      test "bundle discount with item-level discount" {
        let headphones = { prd with Id = "p1"; Price = 59.99M }
        let runningShoes = { prd with Id = "p2"; Price = 89.99M }

        let bundle =
          { Items = [ (headphones.Id, 1); (runningShoes.Id, 1) ]
            Discount = Discount 10M }

        let cart =
          { icart [ (headphones, Discount 5M); (runningShoes, Discount 5M) ] with
              Bundles = [ bundle ] }

        // Bundle discount: 10%, Item discount: 5%, Total: 15%
        let expectedTotal = 59.99M * 0.85M + 89.99M * 0.85M

        Cart.calculateTotal cart
        |> Expect.equal "stacks bundle and item discounts" expectedTotal
      }

      test "no bundle applied when insufficient items" {
        let coffeeMug = { prd with Id = "p1"; Price = 12.99M }

        let bundle =
          { Items = [ (coffeeMug.Id, 2) ]; Discount = Discount 10M }

        let cart =
          { icart [ (coffeeMug, d0) ] with Bundles = [ bundle ] }

        // Only 1 mug, bundle requires 2, so no discount applied
        let expectedTotal = 12.99M

        Cart.calculateTotal cart
        |> Expect.equal "no discount when bundle requirements not met" expectedTotal
      }

      test "multiple different bundles" {
        let coffeeMug = { prd with Id = "p1"; Price = 12.99M }
        let headphones = { prd with Id = "p2"; Price = 59.99M }
        let runningShoes = { prd with Id = "p3"; Price = 89.99M }

        let bundle1 =
          { Items = [ (coffeeMug.Id, 2) ]; Discount = Discount 10M }

        let bundle2 =
          { Items = [ (headphones.Id, 1); (runningShoes.Id, 1) ]
            Discount = Discount 10M }

        let cart =
          { icart
              [ (coffeeMug, d0)
                (coffeeMug, d0)
                (headphones, d0)
                (runningShoes, d0) ] with
              Bundles = [ bundle1; bundle2 ] }

        // Coffee mugs: 12.99 * 0.90 * 2 = 23.382
        // Headphones: 59.99 * 0.90 = 53.991
        // Running shoes: 89.99 * 0.90 = 80.991
        let expectedTotal = 12.99M * 0.90M * 2M + 59.99M * 0.90M + 89.99M * 0.90M

        Cart.calculateTotal cart
        |> Expect.equal "applies multiple independent bundles" expectedTotal
      }

      ]


testList "" [ cartItemTests; cartTotalTests; discountTests; sampleDataTest; bundleDiscountTests ]
|> runTestsWithCLIArgs [] [||]
|> exit
