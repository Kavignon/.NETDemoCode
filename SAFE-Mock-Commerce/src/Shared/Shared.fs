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

// override equality and deny comparison
// // [<CustomEquality; CustomComparison>]

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

    member x.isCartEmpty = x.SelectedItems |> Option.toList |> List.isEmpty

    member x.getCartSubtotal =
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
    type Email = Email of string

    // Phone
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

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ICounterApi = { initialCounter : unit -> Async<Counter> }