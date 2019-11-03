module CatalogApiWorkflows

open System

open Shared
open DtoToDomainMapper

// TODO: Missing function to generate the SkuID of a store product...
let getStoreHeadphones headphones storeProductList =
    headphones
    |> Array.map(fun h ->
        let headphonesInfo = makeProductInformationFrom (Headphones h)
        let product = {
            Details = headphonesInfo
            Shape = getHeadphoneShape h
            BatteryLife = Some(h.BatteryLife.Value)
            ReleaseDate = h.ReleaseDate.Value
            AreWireless = h.IsWireless.Value
            IsNoiseCancelActive = h.IsNoiseCancelled.Value
        }
        WirelessHeadphones(product, generateProductSku())
    )
    |> Array.append storeProductList

let getStoreBooks books storeProductList =
    books
    |> Array.map(fun b ->
        let bookInfo = makeProductInformationFrom (ReadingMaterial b)
        let product = {
            Details = bookInfo
            AuthorName = b.AuthorName.Value
            Format = KindleVersion // TODO: Missing format from XML document & function to convert
            ISBN = "" // TODO: Missing ISBN from XML document & function to convert
            Summary = "" // TODO: Missing Summary from XML document & function to convert
            PageCount = b.PageCount.Value // TODO: Missing function to convert from XML
            Language = English // TODO: Misssing supported languages from XML document & function to convert
            Category = Fantasy // TODO: Missing Missing function to convert from the XML document & function to convert
            ReleasedDate = DateTime.Now // Missing release date from XML document & function to convert
        }
        Book(product, generateProductSku())
    )
    |> Array.append storeProductList

let getStoreComputers computers storeProductList =
    computers
    |> Array.map(fun computer ->
        let computerInfo = makeProductInformationFrom (Computer computer)
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
        Laptop(product, generateProductSku())
    )
    |> Array.append storeProductList

let loadStoreProducts (storeItems: ProductCatalogue.Items)  =
    [||]
    |> getStoreHeadphones storeItems.Headphones
    |> getStoreBooks storeItems.Books
    |> getStoreComputers storeItems.Computers