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

type Item = Product * Discount

type BundleItem = ProductId * Quantity

type Bundle = { Items: BundleItem list; Discount: Discount }

type Cart = { Items: Item list; Discount: Discount; Bundles: Bundle list }


module Cart =

  // Apply bundle discounts exhaustively to items in the cart
  let applyBundles (bundles: Bundle list) (items: Item list) : Item list =

    // Try to match a single bundle against remaining items
    let rec tryMatchBundle
      (bundle: Bundle)
      (remaining: Item list)
      (discounted: Item list)
      : Item list * Item list =
      // Try to find items matching the bundle requirements
      let tryFindMatches (remaining: Item list) =
        bundle.Items
        |> List.fold
          (fun state (pId, qty) ->
            match state with
            | None -> None
            | Some(matched, rem) ->
              let found = rem |> List.filter (fun (prd, _) -> prd.Id = pId) |> List.truncate qty

              if found.Length = qty then
                let newRem =
                  found
                  |> List.fold
                    (fun acc item ->
                      let idx = List.findIndex ((=) item) acc
                      List.removeAt idx acc)
                    rem

                Some(matched @ found, newRem)
              else
                None)
          (Some([], remaining))

      match tryFindMatches remaining with
      | Some(matchedItems, newRemaining) ->
        let withBundleDiscount =
          matchedItems |> List.map (fun (prd, dsc) -> prd, dsc + bundle.Discount)

        // Recursively try to match the bundle again
        tryMatchBundle bundle newRemaining (discounted @ withBundleDiscount)
      | None -> discounted, remaining

    // Apply all bundles sequentially
    let rec applyAllBundles
      (bundles: Bundle list)
      (discounted: Item list)
      (remaining: Item list)
      : Item list =
      match bundles with
      | [] -> discounted @ remaining
      | bundle :: restBundles ->
        let newDiscounted, newRemaining = tryMatchBundle bundle remaining discounted
        applyAllBundles restBundles newDiscounted newRemaining

    applyAllBundles bundles [] items

  let calculateTotal (cart: Cart) : Price =
    applyBundles cart.Bundles cart.Items
    |> List.sumBy (fun (prd, dsc) -> (cart.Discount + dsc).Apply prd.Price)


  module Item =

    let add (prd: Product, dsc: Discount) (cart: Cart) : Cart =
      { cart with Items = (prd, dsc) :: cart.Items }

    let remove (pId: ProductId) (cart: Cart) : Cart =
      match cart.Items |> List.tryFindIndex (fun (prd, _) -> prd.Id = pId) with
      | Some idx -> { cart with Items = cart.Items |> List.removeAt idx }
      | None -> cart

    let updateQuantity (pId: ProductId, qty: Quantity) (cart: Cart) : Cart =
      let currentItems = cart.Items |> List.filter (fun (prd, _) -> prd.Id = pId)
      let otherItems = cart.Items |> List.filter (fun (prd, _) -> prd.Id <> pId)

      if qty <= 0 then
        { cart with Items = otherItems }
      elif currentItems.IsEmpty then
        cart
      else
        // Keep the discount from the first matching item
        let newItems = List.init qty (fun _ -> currentItems.Head)
        { cart with Items = otherItems @ newItems }

  module Bundle =

    let add (bundle: Bundle) (cart: Cart) : Cart =
      { cart with Bundles = bundle :: cart.Bundles }
