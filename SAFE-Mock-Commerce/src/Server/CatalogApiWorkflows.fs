module CatalogApiWorkflows

open System

open Shared
open DtoToDomainMapper

let getHeadphonesFromCatalogue (headphoneDtos: ProductCatalogue.Headphone seq) productList =
    headphoneDtos
    |> Seq.map(fun x -> convertDtoToStoreProduct (HeadphoneDto x))
    |> Seq.append productList

let getComputersFromCatalogue (headphoneDtos: ProductCatalogue.Computer seq) productList =
    headphoneDtos
    |> Seq.map(fun x -> convertDtoToStoreProduct (ComputerDto x))
    |> Seq.append productList

let getBooksFromCatalogue (headphoneDtos: ProductCatalogue.Book seq) productList =
    headphoneDtos
    |> Seq.map(fun x -> convertDtoToStoreProduct (ReadingMaterialDto x))
    |> Seq.append productList

let loadStoreProducts (storeItems: ProductCatalogue.Items)  =
    Seq.empty
    |> getHeadphonesFromCatalogue storeItems.Headphones
    |> getBooksFromCatalogue storeItems.Books
    |> getComputersFromCatalogue storeItems.Computers