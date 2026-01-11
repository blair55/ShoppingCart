module ShoppingCart.Client.Console

open ShoppingCart.Client.LoggedCart
open ShoppingCart
open SpectreCoff

type Formatter = { Price: Price -> string; Discount: Discount -> string }

let private logOp (fmt: Formatter) op =
  let message =
    match op with
    | Add(prd, dsc) -> sprintf "Add %s with %s discount" prd.Name (fmt.Discount dsc)
    | Remove pId -> sprintf "Remove productId %s" pId
    | UpdateQuantity(pId, qty) -> sprintf "Update productId %s quantity to %i" pId qty
    | SetDiscount dsc -> sprintf "Set cart discount to %s" (fmt.Discount dsc)
    | AddBundle bundle ->
      let items =
        bundle.Items
        |> List.map (fun (pId, qty) -> sprintf "%dx %s" qty pId)
        |> String.concat " + "

      sprintf "Add bundle: (%s) @ %s off" items (fmt.Discount bundle.Discount)

  message |> Edgy |> toConsole

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

  // Group items by (ProductId, Discount) - items with different discounts appear separately
  let groupedItems =
    Cart.applyBundles cart.Bundles cart.Items
    |> List.groupBy (fun (prd, dsc) -> prd.Id, dsc.Value)
    |> List.map (fun ((_, _), items) ->
      let prd, dsc = items.Head
      let qty = items.Length
      prd, qty, dsc)

  let row (prd: Product, qty, dsc) =
    let itemTotal = (cart.Discount + dsc).Apply prd.Price * decimal qty

    Payloads
      [ Many [ Calm prd.Id; Raw prd.Name ]
        Raw(string qty)
        Raw(fmt.Price prd.Price)
        Raw(fmt.Discount(cart.Discount + dsc))
        Raw(fmt.Price itemTotal) ]

  groupedItems |> List.map row |> table columns |> toOutputPayload |> toConsole

let fmt = { Price = sprintf "%.2f"; Discount = fun d -> sprintf "%M%%" d.Value }

let logger = { Op = logOp fmt; Cart = logCart fmt }
