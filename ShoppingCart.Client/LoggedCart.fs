module ShoppingCart.Client.LoggedCart

open ShoppingCart

type Op =
  | Add of Product * Discount
  | Remove of ProductId
  | UpdateQuantity of ProductId * Quantity
  | SetDiscount of Discount
  | AddBundle of Bundle

type Logger = { Op: Op -> Unit; Cart: Cart -> Unit }

type LoggedCartBuilder(logger: Logger) =

  let perform cart =
    function
    | Add(prd, dsc) -> Cart.Item.add (prd, dsc) cart
    | Remove pId -> Cart.Item.remove pId cart
    | UpdateQuantity(pId, qty) -> Cart.Item.updateQuantity (pId, qty) cart
    | SetDiscount dsc -> { cart with Discount = dsc }
    | AddBundle bundle -> Cart.Bundle.add bundle cart

  let (<+>) cart op =
    logger.Op op
    let cart = perform cart op
    logger.Cart cart
    cart

  member _.Yield _ =
    { Items = []; Discount = Discount.Zero; Bundles = [] }

  member _.Zero _ =
    { Items = []; Discount = Discount.Zero; Bundles = [] }

  [<CustomOperation("add")>]
  member _.Add(cart, prd, ?dsc) =
    cart <+> Add(prd, Discount(Option.defaultValue 0M dsc))

  [<CustomOperation("remove")>]
  member _.Remove(cart, pId) = cart <+> Remove pId

  [<CustomOperation("update_quantity")>]
  member _.UpdateQuantity(cart, pId, qty) = cart <+> UpdateQuantity(pId, qty)

  [<CustomOperation("discount")>]
  member _.SetDiscount(cart, dsc) = cart <+> SetDiscount(Discount dsc)

  [<CustomOperation("bundle_discount")>]
  member _.AddBundleDiscount(cart, items: (ProductId * Quantity) list, discount: decimal) =
    cart <+> AddBundle { Items = items; Discount = Discount discount }

let loggedCart logger = LoggedCartBuilder logger
