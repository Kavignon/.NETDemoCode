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

let generateProductSku() = Guid.NewGuid().ToString()

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

let convertDtoToStoreProduct productDto =
    match productDto with
    | HeadphoneDto h ->
        let headphonesInfo = makeProductInformationFrom h
        let product = {
            Details = headphonesInfo
            Shape = getHeadphoneShape h
            BatteryLife = Some(h.BatteryLife.Value)
            ReleaseDate = h.ReleaseDate.Value
            AreWireless = h.IsWireless.Value
            IsNoiseCancelActive = h.IsNoiseCancelled.Value
        }
        WirelessHeadphones(product, generateProductSku())

// // let getStoreBooks books storeProductList =
// //     books
// //     |> Array.map(fun b ->
// //         let bookInfo = makeProductInformationFrom (ReadingMaterial b)
// //         let product = {
// //             Details = bookInfo
// //             AuthorName = b.AuthorName.Value
// //             Format = KindleVersion // TODO: Missing format from XML document & function to convert
// //             ISBN = "" // TODO: Missing ISBN from XML document & function to convert
// //             Summary = "" // TODO: Missing Summary from XML document & function to convert
// //             PageCount = b.PageCount.Value // TODO: Missing function to convert from XML
// //             Language = English // TODO: Misssing supported languages from XML document & function to convert
// //             Category = Fantasy // TODO: Missing Missing function to convert from the XML document & function to convert
// //             ReleasedDate = DateTime.Now // Missing release date from XML document & function to convert
// //         }
// //         Book(product, generateProductSku())
// //     )
// //     |> Array.append storeProductList

// // let getStoreComputers computers storeProductList =
// //     computers
// //     |> Array.map(fun computer ->
// //         let computerInfo = makeProductInformationFrom (Computer computer)
// //         let computerCpu = {
// //             Details = computerInfo // TODO: Need function to retrieve it from XmlProvider<...>.Computer
// //             CoreCount = 4 // TODO: Need function to retrieve it from XmlProvider<...>.Computer
// //             Series = Intel(IntelCorei7) // TODO: Need function to retrieve it from XmlProvider<...>.Computer
// //             ProcessorSpeed = 3.2 // TODO: Need function to retrieve it from XmlProvider<...>.Computer
// //             OverclockedSpeed = 5.2 // TODO: Need function to retrieve it from XmlProvider<...>.Computer
// //             Wattage = 80 // TODO: Need function to retrieve it from XmlProvider<...>.Computer
// //             YearModel = DateTime.Today // TODO: Need function to retrieve it from XmlProvider<...>.Computer + Should be int, not DateTime
// //         }
// //         let product = {
// //             Details = computerInfo
// //             Resolution = HighDefinition 1920
// //             Cpu = computerCpu
// //             Ram = 8192 // TODO: Need to specify RAM from XML
// //             CacheMemory = None // TODO: Need to specify field in computer + function to retrieve it from the generated type
// //             DdrRam = None // TODO: Need to specify from XML
// //             RunningOperatingSystem = Windows10 // TODO: Need to specify from XML
// //             DeviceInputs = [] // TODO: Need to specify from XML
// //         }
// //         Laptop(product, generateProductSku())
// //     )
// //     |> Array.append storeProductList