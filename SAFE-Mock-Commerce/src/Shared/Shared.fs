namespace Shared
open System
open FSharp.Data

type Counter = { Value : int }

type ProductDimension = {
    Height: float
    Width: float
    Depth: float option
}

type ProductColor =
    | Red
    | Black
    | White
    | Gray
    | Blue
    | Green
    | NotSupportedByStore

type Brand =
    | Toshiba
    | Sony
    | Microsoft
    | Intel
    | AMD
    | Nintendo
    | Bose
    | Asus
    | Apple
    | NotSupportedByStore

type SupportedLanguage =
    | English
    | French
    | NotSupportedByStore

type CommonProductInformation  = {
    Name:           string
    Weight:         float
    ShippingWeight: float
    AverageReviews: float
    Dimensions:     ProductDimension
    Price:          float
    Color:          ProductColor
    Brand:          Brand
}

type DeviceDisplay =
    | StandardDefinition    of int
    | EnhanceDefinition     of int
    | HighDefinition        of int
    | UltraHighDefinition   of int

type OperatingSystem =
    | Windows7
    | Windows8
    | Windows10
    | MacOS
    | Linux
    | XboxOne
    | Playstation4
    | Switch

type CableConnection =
    | USB1
    | USB2
    | USB3
    | USBC
    | HDMI
    | MiniHDMI
    | PowerAdapter

type Device = {
    ProductDetails:     CommonProductInformation
    ModelNumber:        string
    IsWireless:         bool
    SupportedOS:        OperatingSystem list
    HardwareInterfaces: CableConnection list
    Resolution:         DeviceDisplay
}

type Keyboard = {
    DeviceDefinition:   Device
    IsMechanical:       bool
    IsGamingKeyboard:   bool
    KeyCount:           int
}

type FightingPad = {
    DeviceDescription:      Device;
    AreBatteriesRequired:   bool
    HasProgrammableButtons: bool
}

type DeviceInput =
    | Keyboard  of Keyboard
    | Gamepad   of FightingPad

type IntelProcessorSeries =
    | IntelCorei3
    | IntelCorei5
    | IntelCorei7
    | IntelCorei9

type AmdProcessorSeries =
    | Ryden
    | Athlon
    | AthlonII
    | ASeries
    | ESeries
    | FSeries

type ProcessorSeries =
    | Intel     of IntelProcessorSeries
    | AMD       of AmdProcessorSeries

type DDR =
    | DDR2
    | DDR3
    | DDR4

type CPU = {
    Details:            CommonProductInformation
    CoreCount:          int
    Series:             ProcessorSeries
    ProcessorSpeed:     float
    OverclockedSpeed:   float
    Wattage:            int
    YearModel:          DateTime
}

type Computer = {
    Details:                CommonProductInformation
    Resolution:             DeviceDisplay
    Cpu:                    CPU
    Ram:                    int
    CacheMemory:            int option
    DdrRam:                 DDR option
    RunningOperatingSystem: OperatingSystem
    DeviceInputs:           DeviceInput list
}

type GameConsole = {
    Hardware:               Computer
    SupportedResolutions:   DeviceDisplay list
    Inputs:                 CableConnection list
    IsHandHandledDevice:    bool
    MaxControllerSupported: int
}

type BookCategory =
    | Fantasy
    | ``Computer Science``
    | ``Graphic Novel``
    | CategoryNotSupportedByStore


type BookFormat =
    | MassPaperBack
    | Paperback
    | Hardcover
    | KindleVersion
    | NotSupportedByStore

type Book = {
    AuthorName:     string
    SupportedFormats : BookFormat list // using seq removes the structural equality
    Summary:        string
    Details:        CommonProductInformation
    Category:       BookCategory
    PageCount:      int
    ISBN:           string
    SupportedLanguages : SupportedLanguage list // using seq removes the structural equality
    Publisher: string
    ReleasedDate:   DateTime
}

type HeadphoneShape =
    | ``In ear``
    | ``On ear``
    | ``Over ear``

type HeadphoneProduct = {
    Details:                CommonProductInformation
    Shape:                  HeadphoneShape
    BatteryLife:            int option
    ReleaseDate:            DateTime;
    AreWireless:            bool
    IsNoiseCancelActive:    bool
}

type StoreProduct =
    | Book                  of novel:       Book * SkuId: string
    | WirelessHeadphones    of headphones:  HeadphoneProduct * SkuId: string
    | Television            of television:  Device * SkuId: string
    | Laptop                of laptop:      Computer * SkuId: string
    | GameConsole           of console:     GameConsole * SkuId: string
with
    member x.ProductId =
        match x with
        | Book (_, id) -> id
        | WirelessHeadphones (_, id) -> id
        | Television (_,id) -> id
        | Laptop (_, id) -> id
        | GameConsole (_, id) -> id

    member x.ProductPrice =
        match x with
        | Book (b, _) -> b.Details.Price
        | WirelessHeadphones (wh, _) -> wh.Details.Price
        | Television (t, _) -> t.ProductDetails.Price
        | Laptop (l, _) -> l.Details.Price
        | GameConsole (gc, _) -> gc.Hardware.Details.Price

module CatalogueDto =
    type ProductCatalogue = XmlProvider<"./StoreProducts.xml"> // TODO: Enhancement - Move the data to a SQL db in Azure and load data from there.

    let productsFromDatabase = ProductCatalogue.GetSample()

    type StoreProductDto =
        | HeadphoneDto of ProductCatalogue.Headphone
        | ReadingMaterialDto of ProductCatalogue.Book
        | ComputerDto of ProductCatalogue.Computer
        
module ShoppingCart =

    type PaymentMethod =
        | Visa of cardOwnerName: string * cardNumber: string * expirationDate: DateTime * cardSecurityCode: int * wasCloned: bool
        | Debit of cardOwnerName: string * cardNumber: string * BankName: string * wasCloned: bool

        member x.IsCardValid =
            match x with
            | Visa(_, _, exprDate, _, wasCloned) -> DateTime.Now < exprDate && not wasCloned
            | Debit(_, _, _, wasCloned) -> not wasCloned

        member x.CardOwnerName =
            match x with
            | Visa (ownerName, _, _, _, _) -> ownerName
            | Debit (ownerName, _, _, _) -> ownerName

    type ShoppingCart = {
        SelectedPaymentMethod: PaymentMethod
        SelectedItems: Map<StoreProduct, int> option
    }
    with

    member x.IsCartEmpty = x.SelectedItems |> Option.toList |> List.isEmpty

    member x.GetCartSubtotal =
        match x.SelectedItems with
        | None -> 0.00
        | Some storeProducts ->
            (0.00, storeProducts)
            ||> Map.fold(fun accumulatedSubtotal product qty -> accumulatedSubtotal + (product.ProductPrice * float qty))

module CustomerInfo =
    type PersonalName = {
        FirstName:      string;
        MiddleName:     string option
        LastName:       string
    }
    with
        member x.Fullname =
            match x.MiddleName with
            | Some middle -> [x.FirstName; middle; x.LastName] |> List.fold (+) ""
            | None -> [x.FirstName; x.LastName] |>  List.fold (+) ""

    type PostalCode =  PostalCode of string
    type City = City of string
    type Province = Province of string

    // Address
    type StreetAddress = {
        CivicNumber:int;
        StreetName:string;
        PostalCode: PostalCode;
    }

    type CanadaAddress = {
        Street: StreetAddress;
        City: City;
        Province: Province
    }

    // Email
    type Email = UnverifiedEmail of string

    type CountryPrefix = CountryPrefix of int
    type Phone = { CountryPrefix:CountryPrefix; LocalNumber:string }

    type Contact = {
        CustomerName: PersonalName
        BillingAddress: CanadaAddress
        ShippingAddress: CanadaAddress option
        IsShippingToBillingAddress: bool
        Email: Email
        Phone: Phone option
    }

    // Put it all together into a CustomerAccount type
    type CustomerAccountId  = AccountId of string

    // override equality and deny comparison
    [<CustomEquality; NoComparison>]
    type CustomerAccount =
      {
        AccountId: CustomerAccountId
        ContactInfo: Contact
        }

        override this.Equals(other) =
            match other with
            | :? CustomerAccount as otherCust -> (this.AccountId = otherCust.AccountId)
            | _ -> false

        override this.GetHashCode() = hash this.AccountId

module DtoToDomain =
    open CatalogueDto
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
            | HeadphoneDto h ->
                {
                    Height = float h.Height.Value
                    Width = float h.Width.Value
                    Depth = Some (float h.Depth.Value)
                }
            | ComputerDto c ->
                {
                    Height = float c.Height.Value
                    Width = float c.Width.Value
                    Depth = Some (float c.Depth.Value)
                }
            | ReadingMaterialDto rm ->
                {
                    Height = float rm.Height.Value
                    Width = float rm.Width.Value
                    Depth = Some (float rm.Depth.Value)
                }

        match productDto with
        | HeadphoneDto headphoneDto ->
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
        | ReadingMaterialDto readingDto ->
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
        | ComputerDto computerDto ->
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

    let convertToWirelessHeadphones productCommonInfo headphoneDetails =
        let product = {
            Details = productCommonInfo
            Shape = getHeadphoneShape headphoneDetails
            BatteryLife = Some(headphoneDetails.BatteryLife.Value)
            ReleaseDate = headphoneDetails.ReleaseDate.Value
            AreWireless = headphoneDetails.IsWireless.Value
            IsNoiseCancelActive = headphoneDetails.IsNoiseCancelled.Value
        }

        WirelessHeadphones(product, generateProductSku())

    let convertToBook productCommonInfo (bookDetails: ProductCatalogue.Book) =
        let convertToSupportedLanguage languageStr =
            match languageStr with
            | "English" -> English
            | "French" -> French
            | _ -> SupportedLanguage.NotSupportedByStore

        let convertToFormat formatStr =
            match formatStr with
            | "Mass paper back" -> MassPaperBack
            | "Paperback" -> Paperback
            | "Hardcover" -> Hardcover
            | "Kindle" -> KindleVersion
            | _ -> NotSupportedByStore

        let convertToBookCategory categoryStr =
            match categoryStr with
            | Prefix "Fan" _ -> Fantasy
            | Prefix "Com" _ & Suffix "Sci" _ -> ``Computer Science``
            | Prefix "Grap" _ & Suffix "Nov" _ -> ``Graphic Novel``
            | _ ->  CategoryNotSupportedByStore

        let product = {
            Details = productCommonInfo
            AuthorName = bookDetails.AuthorName.Value
            SupportedFormats = bookDetails.Formats |> Array.map(fun f -> convertToFormat f.Value) |> Array.toList
            ISBN = bookDetails.Isbn.Value
            Summary = bookDetails.Summary.Value
            PageCount = bookDetails.PageCount.Value
            SupportedLanguages =  [ convertToSupportedLanguage bookDetails.Language.Value ]
            Publisher = bookDetails.Publisher.Value
            Category = convertToBookCategory bookDetails.Category.Value
            ReleasedDate = bookDetails.ReleaseDate.Value
        }

        Book(product, generateProductSku())

    let convertDtoToStoreProduct productDto =
        let productCommonInfo = makeProductInformationFrom productDto
        match productDto with
        | HeadphoneDto details -> convertToWirelessHeadphones productCommonInfo details
        | ReadingMaterialDto details -> convertToBook productCommonInfo details

    let hydrateProductSeq (storeItems: ProductCatalogue.Items)  =
        let headphones = storeItems.Headphones |> Seq.map (HeadphoneDto >> convertDtoToStoreProduct)
        let books = storeItems.Books |> Seq.map (ReadingMaterialDto >> convertDtoToStoreProduct)

        Seq.append books headphones

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ICounterApi = { initialCounter : unit -> Async<Counter> }