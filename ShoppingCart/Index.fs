namespace rec ShoppingCart

type ProductId = string

type Price = decimal

type Product =
  { Id: ProductId
    Name: string
    Price: Price
    Category: string }

type Quantity = int

type Item = Product * Quantity

type Cart = Map<ProductId, Item>


module Cart =

  let calculateTotal (cart: Cart) : Price =
    cart
    |> Map.toList
    |> List.sumBy (fun (_, (product, qty)) -> product.Price * decimal qty)


  module Item =

    let add (product: Product) (cart: Cart) : Cart =
      cart
      |> Map.change product.Id (function
        | Some(_, qty) -> Some(product, qty + 1)
        | None -> Some(product, 1))

    let remove (productId: ProductId) (cart: Cart) : Cart = cart |> Map.remove productId

    let updateQuantity (productId: ProductId, qty: Quantity) (cart: Cart) : Cart =
      match Map.tryFind productId cart with
      | Some(product, _) when qty > 0 -> cart |> Map.add productId (product, qty)
      | Some _ -> cart |> Map.remove productId
      | None -> cart
