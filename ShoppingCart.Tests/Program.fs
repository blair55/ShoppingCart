open Expecto
open Expecto.Flip
open ShoppingCart


let cartItemTests =

  let product =
    { Id = "p1"
      Name = "Sample Product"
      Price = 20.0M
      Category = "Sample Category" }

  testList
    "Cart Item Tests"
    [ test "add item to empty cart" {
        let cart = Map.empty
        let expected = Map.ofList [ product.Id, Item(product, 1) ]
        Cart.Item.add product cart |> Expect.equal "adds item with qty 1" expected
      }

      test "add item to cart with existing item" {
        let cart = Map.ofList [ product.Id, Item(product, 2) ]
        let expected = Map.ofList [ product.Id, Item(product, 3) ]
        Cart.Item.add product cart |> Expect.equal "increments item quantity" expected
      }

      test "remove item from cart" {
        let cart = Map.ofList [ product.Id, Item(product, 2) ]
        let expected = Map.empty

        Cart.Item.remove product.Id cart
        |> Expect.equal "removes item from cart" expected
      }

      test "remove item that doesn't exist in cart" {
        let cart = Map.empty
        let expected = Map.empty

        Cart.Item.remove product.Id cart |> Expect.equal "returns empty cart" expected
      }

      test "update item quantity in cart" {
        let cart = Map.ofList [ product.Id, Item(product, 2) ]
        let expected = Map.ofList [ product.Id, Item(product, 5) ]

        Cart.Item.updateQuantity (product.Id, 5) cart
        |> Expect.equal "updates item quantity" expected
      }

      test "update item quantity to zero" {
        let cart = Map.ofList [ product.Id, Item(product, 2) ]
        let expected = Map.empty

        Cart.Item.updateQuantity (product.Id, 0) cart
        |> Expect.equal "removes item from cart" expected
      }

      test "update item with negative quantity" {
        let cart = Map.ofList [ product.Id, Item(product, 2) ]
        let expected = Map.empty

        Cart.Item.updateQuantity (product.Id, -3) cart
        |> Expect.equal "removes item from cart" expected
      }

      test "update item that doesn't exist in cart" {
        let cart = Map.ofList [ product.Id, Item(product, 2) ]
        let expected = cart

        Cart.Item.updateQuantity ("123", 5) cart
        |> Expect.equal "returns unchanged cart" expected
      }

      ]

let cartTotalTests =

  testList
    "Cart Total Tests"
    [ test "calculate total for empty cart" {
        let cart = Map.empty
        Cart.calculateTotal cart |> Expect.equal "total is zero" 0.0M
      }

      test "calculate total for cart with items" {
        let product1, p1Qty =
          { Id = "p1"
            Name = "Product 1"
            Price = 10.0M
            Category = "Category 1" },
          2

        let product2, p2Qty =
          { Id = "p2"
            Name = "Product 2"
            Price = 15.0M
            Category = "Category 2" },
          3

        let cart =
          Map.ofList [ product1.Id, Item(product1, p1Qty); product2.Id, Item(product2, p2Qty) ]

        let expectedTotal = product1.Price * decimal p1Qty + product2.Price * decimal p2Qty

        Cart.calculateTotal cart
        |> Expect.equal "calculates correct total" expectedTotal
      }

      ]

runTestsWithCLIArgs [] [||] (testList "tests" [ cartItemTests; cartTotalTests ])
|> exit
