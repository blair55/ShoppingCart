module ShoppingCart.Client.Console

open ShoppingCart.Client.LoggedCart
open ShoppingCart
open SpectreCoff

type Formatter = { Price: Price -> string; Discount: Discount -> string }

let private logOp (fmt: Formatter) op =
  match op with
  | Add(prd, dsc) -> sprintf "Add %s with %s discount" prd.Name (fmt.Discount dsc)
  | Remove pId -> sprintf "Remove productId %s" pId
  | UpdateQuantity(pId, qty) -> sprintf "Update productId %s quantity to %i" pId qty
  | SetDiscount dsc -> sprintf "Set cart discount to %s" (fmt.Discount dsc)
  |> Edgy
  |> toConsole

let private logCart (fmt: Formatter) cart =
  let alignLeft = { defaultColumnLayout with Alignment = Left }
  let alignRight = { defaultColumnLayout with Alignment = Right }

  let columns =
    [ column (Calm "Product") |> withLayout alignLeft
      column (Calm "Qty") |> withLayout alignRight
      column (Calm "Price") |> withLayout alignRight
      column (Calm "Discount") |> withLayout alignRight
      column (Calm "Total")
      |> withLayout alignRight
      |> withFooter (Pumped(fmt.Price(Cart.calculateTotal cart))) ]

  let row (prd, qty, dsc as item) =
    Payloads
      [ Many [ Calm prd.Id; Raw prd.Name ]
        Raw(string qty)
        Raw(fmt.Price prd.Price)
        Raw(fmt.Discount(cart.Discount + dsc))
        Raw(fmt.Price(Cart.Item.total item cart.Discount)) ]

  Map.toList cart.Items
  |> List.map (snd >> row)
  |> table columns
  |> toOutputPayload
  |> toConsole

let fmt = { Price = sprintf "%.2f"; Discount = fun d -> sprintf "%M%%" d.Value }

let logger = { Op = logOp fmt; Cart = logCart fmt; }
