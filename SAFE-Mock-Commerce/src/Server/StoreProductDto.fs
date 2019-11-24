module CatalogueDto

open FSharp.Data

type ProductCatalogue = XmlProvider<"./StoreProducts.xml"> // TODO: Enhancement - Move the data to a SQL db in Azure and load data from there.

let productsFromDatabase = ProductCatalogue.GetSample()

type StoreProductDto =
    | HeadphoneDto of ProductCatalogue.Headphone
    | ReadingMaterialDto of ProductCatalogue.Book
    | ComputerDto of ProductCatalogue.Computer