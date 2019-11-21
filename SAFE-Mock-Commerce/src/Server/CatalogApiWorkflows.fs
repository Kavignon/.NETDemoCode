module CatalogApiWorkflows

open CatalogueDto
open DtoToDomainMapper

let hydrateProductSeq (storeItems: ProductCatalogue.Items)  =
    let headphones = storeItems.Headphones |> Seq.map (HeadphoneDto >> convertDtoToStoreProduct)
    let books = storeItems.Books |> Seq.map (ReadingMaterialDto >> convertDtoToStoreProduct)
    (headphones, books) ||> Seq.append |> Seq.toList