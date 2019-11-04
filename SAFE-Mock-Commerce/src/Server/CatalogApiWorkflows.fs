module CatalogApiWorkflows

open Shared
open DtoToDomainMapper

let hydrateProductSeq (storeItems: ProductCatalogue.Items)  =
    let headphones = storeItems.Headphones |> Seq.map (HeadphoneDto >> convertDtoToStoreProduct)
    let computers = storeItems.Computers |> Seq.map (ComputerDto >> convertDtoToStoreProduct)
    let books = storeItems.Books |> Seq.map (ReadingMaterialDto >> convertDtoToStoreProduct)

    computers
    |> Seq.append books
    |> Seq.append headphones