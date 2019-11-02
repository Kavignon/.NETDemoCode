module CatalogApiWorkflows

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

type CatalogueUtils =
    static member getFitFromHeadphones (h: ProductCatalogue.Headphone) =
        match h.Fit.Value with
            | Prefix "In" _ -> ``In ear``
            | Prefix "On" _ -> ``On ear``
            | _ -> ``Over ear``

    static member getColorFromDbProduct colorString =
       match colorString with
        | "Red" -> Some Red
        | "Black" -> Some Black
        | "White" -> Some White
        | "Gray" -> Some Gray
        | "Blue" -> Some Blue
        | "Green" -> Some Green
        | _ -> None
        |> Option.defaultValue ProductColor.NotSupportedByStore

    static member getBrandFromDbProduct brandString =
        match brandString with
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

let extractProductInformationFromSerialization serializedProduct =
        match serializedProduct with
        | Headphones dbHeadphones ->
            {
                Name = dbHeadphones.Model.Name
                Weight = float dbHeadphones.Weigth.Value
                ShippingWeight = float dbHeadphones.Weigth.Value
                AverageReviews = 4.2 // TODO: Add AverageReview field in the XML
                Dimensions = {
                    Heigth = float dbHeadphones.Heigth.Value
                    Width = float dbHeadphones.Width.Value
                    Depth = Some (float dbHeadphones.Depth.Value)
                }
                Price = float dbHeadphones.Price.Value
                Color = CatalogueUtils.getColorFromDbProduct dbHeadphones.Color.Value
                Brand = CatalogueUtils.getBrandFromDbProduct dbHeadphones.Manufacturer.Name
            }
        | ReadingMaterial dbBook ->
            {
                Name = dbBook.Name.Value
                Weight = float dbBook.ShippingWeight.Value
                ShippingWeight = float dbBook.ShippingWeight.Value
                AverageReviews = float dbBook.ReviewAverage.Value
                Dimensions = {
                    // TODO: Add dimensions to book definition
                    Heigth = 1.00
                    Width = 1.00
                    Depth = Some 1.00
                }
                Price = float dbBook.Price.Value
                Color = Red // Provide book color in definition
                Brand = Toshiba //Waiting for up book publisher companies in definition
            }
        | Computer computerDb ->
            {
                Name = computerDb.Model.Series + " " + computerDb.Model.Number
                Weight = float computerDb.Weight.Value
                ShippingWeight = float computerDb.Weight.Value
                AverageReviews = 4.5 // TODO: Add AverageReview field in the XML
                Dimensions = {
                    Heigth = float computerDb.Height.Value
                    Width = float computerDb.Height.Value
                    Depth = Some (float computerDb.Height.Value)
                }
                Price = float computerDb.Price.Value
                Color = computerDb.Color.Value |> CatalogueUtils.getColorFromDbProduct // Provide book color in definition
                Brand = computerDb.Manufacturer |> CatalogueUtils.getBrandFromDbProduct
            }


let getStoreHeadphones headphones storeProductList =
    headphones
    |> Array.map(fun h ->
        let headphonesInfo = extractProductInformationFromSerialization (Headphones h)
        let product = {
            Details = headphonesInfo
            Fit = CatalogueUtils.getFitFromHeadphones h
            BatteryLife = Some(h.BatteryLife.Value)
            ReleaseDate = h.ReleaseDate.Value
            AreWireless = h.IsWireless.Value
            IsNoiseCancelActive = h.IsNoiseCancelled.Value
        }
        WirelessHeadphones(product, "WireLessId")
    )
    |> Array.append storeProductList

let getStoreBooks books storeProductList =
    books
    |> Array.map(fun b ->
        let bookInfo = ReadingMaterial(b) |> extractProductInformationFromSerialization
        let product = {
            Details = bookInfo
            AuthorName = b.AuthorName.Value
            Format = KindleVersion // Missing format from XML document
            ISBN = "" // Missing ISBN from XML document
            Summary = "" // Missing Summary from XML document
            PageCount = b.PageCount.Value
            Language = English
            Category = Fantasy // Missing function to convert from the XML document
            ReleasedDate = DateTime.Now // Missing release date from XML document
        }
        Book(product, "SomeBookId")
    )
    |> Array.append storeProductList

let getStoreComputers computers storeProductList =
    computers
    |> Array.map(fun computer ->
        let computerInfo = Computer(computer) |> extractProductInformationFromSerialization
        let computerCpu = {
            Details = computerInfo // TODO: Need function to retrieve it from XmlProvider<...>.Computer
            CoreCount = 4 // TODO: Need function to retrieve it from XmlProvider<...>.Computer
            Series = Intel(IntelCorei7) // TODO: Need function to retrieve it from XmlProvider<...>.Computer
            ProcessorSpeed = 3.2 // TODO: Need function to retrieve it from XmlProvider<...>.Computer
            OverclockedSpeed = 5.2 // TODO: Need function to retrieve it from XmlProvider<...>.Computer
            Wattage = 80 // TODO: Need function to retrieve it from XmlProvider<...>.Computer
            YearModel = DateTime.Today // TODO: Need function to retrieve it from XmlProvider<...>.Computer + Should be int, not DateTime
        }
        let product = {
            Details = computerInfo
            Resolution = HighDefinition 1920 
            Cpu = computerCpu
            Ram = 8192 // TODO: Need to specify RAM from XML 
            CacheMemory = None // TODO: Need to specify field in computer + function to retrieve it from the generated type
            DdrRam = None // TODO: Need to specify from XML 
            RunningOperatingSystem = Windows10 // TODO: Need to specify from XML 
            DeviceInputs = [] // TODO: Need to specify from XML
        }
        Laptop(product, "LaptopId")
    )
    |> Array.append storeProductList

let loadStoreProducts (storeItems: ProductCatalogue.Items)  =
    [||]
    |> getStoreHeadphones storeItems.Headphones
    |> getStoreBooks storeItems.Books
    |> getStoreComputers storeItems.Computers