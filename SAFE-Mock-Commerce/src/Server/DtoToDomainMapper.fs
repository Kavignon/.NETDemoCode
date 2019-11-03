module DtoToDomainMapper

open System
open Shared

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let (|Suffix|_|) (p:string) (s:string) =
    if s.EndsWith(p) then
        Some(String.Empty)
    else
        None

let getHeadphoneShape (headphoneDto: ProductCatalogue.Headphone) =
    match headphoneDto.Fit.Value with
    | Prefix "In" _ -> ``In ear``
    | Prefix "On" _ -> ``On ear``
    | _ -> ``Over ear``

let convertToProductColor color =
    match color with
    | "Red" -> Some Red
    | "Black" -> Some Black
    | "White" -> Some White
    | "Gray" -> Some Gray
    | "Blue" -> Some Blue
    | "Green" -> Some Green
    | _ -> None
    |> Option.defaultValue ProductColor.NotSupportedByStore

let convertToBrand brand =
    match brand with
    | "Toshiba" -> Some Toshiba
    | "Sony" -> Some Sony
    | "Microsoft" -> Some Microsoft
    | "Intel" -> Some Brand.Intel
    | "AMD" -> Some Brand.AMD
    | "Nintendo" -> Some Nintendo
    | "Bose" -> Some Bose
    | "Asus" -> Some Asus
    | "Apple" -> Some Apple
    | _ -> None
    |> Option.defaultValue Brand.NotSupportedByStore

let makeProductInformationFrom productDto =
    let productDimensions =
        match productDto with
        | Headphones h ->
            {
                Height = float h.Height.Value
                Width = float h.Width.Value
                Depth = Some (float h.Depth.Value)
            }
        | Computer c ->
            {
                Height = float c.Height.Value
                Width = float c.Width.Value
                Depth = Some (float c.Depth.Value)
            }
        | ReadingMaterial rm ->
            {
                Height = float rm.Height.Value
                Width = float rm.Width.Value
                Depth = Some (float rm.Depth.Value)
            }

    match productDto with
    | Headphones headphoneDto ->
        {
            Name = headphoneDto.Model.Name
            Weight = float headphoneDto.Weigth.Value
            ShippingWeight = float headphoneDto.Weigth.Value
            AverageReviews = float headphoneDto.ReviewAverage.Value
            Dimensions = productDimensions
            Price = float headphoneDto.Price.Value
            Color = convertToProductColor headphoneDto.Color.Value
            Brand = convertToBrand headphoneDto.Manufacturer.Name
        }
    | ReadingMaterial readingDto ->
        {
            Name = readingDto.Name.Value
            Weight = float readingDto.ShippingWeight.Value
            ShippingWeight = float readingDto.ShippingWeight.Value
            AverageReviews = float readingDto.ReviewAverage.Value
            Dimensions = productDimensions
            Price = float readingDto.Price.Value
            Color = Red // Provide book color in definition
            Brand = Toshiba //Waiting for up book publisher companies in definition
        }
    | Computer computerDto ->
        {
            Name = computerDto.Model.Series + " " + computerDto.Model.Number
            Weight = float computerDto.Weight.Value
            ShippingWeight = float computerDto.Weight.Value
            AverageReviews = float computerDto.ReviewAverage.Value
            Dimensions = productDimensions
            Price = float computerDto.Price.Value
            Color = convertToProductColor computerDto.Color.Value
            Brand = convertToBrand computerDto.Manufacturer
        }