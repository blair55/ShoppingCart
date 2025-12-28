namespace rec ShoppingCart

open System

type ProductId = string

type Price = decimal

type Product =
  { Id: ProductId
    Name: string
    Price: Price
    Category: string }

type Quantity = int

type Discount(d: decimal) =
  member _.Value = Math.Clamp(d, 0M, 100M)
  static member Zero = Discount 0M
  static member (+)(a: Discount, b: Discount) = Discount(a.Value + b.Value)
  member this.Apply(price: Price) : Price = price * (1M - this.Value / 100M)

type Item = Product * Quantity * Discount

type Cart = { Items: Map<ProductId, Item>; Discount: Discount }


module Cart =

  let calculateTotal (cart: Cart) : Price =
    cart.Items
    |> Map.toList
    |> List.sumBy (fun (_, (prd, qty, dsc)) ->
      let totalDiscount = cart.Discount + dsc
      totalDiscount.Apply prd.Price * decimal qty)


  module Item =

    let add (prd: Product, dsc: Discount) (cart: Cart) : Cart =
      { cart with
          Items =
            cart.Items
            |> Map.change prd.Id (function
              | Some(_, qty, _) -> Some(prd, qty + 1, dsc)
              | None -> Some(prd, 1, dsc)) }

    let remove (pId: ProductId) (cart: Cart) : Cart =
      { cart with Items = cart.Items |> Map.remove pId }

    let updateQuantity (pId: ProductId, qty: Quantity) (cart: Cart) : Cart =
      match Map.tryFind pId cart.Items with
      | Some(prd, _, dsc) when qty > 0 ->
        { cart with Items = cart.Items |> Map.add pId (prd, qty, dsc) }
      | Some _ -> { cart with Items = cart.Items |> Map.remove pId }
      | None -> cart
