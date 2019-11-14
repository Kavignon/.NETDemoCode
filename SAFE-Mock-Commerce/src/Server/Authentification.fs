module JWT

open System
open System.IO
open System.Security
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt

open FsToolkit.ErrorHandling

open Shared.EmailAddress
open Shared.UserCredentials
open Shared.TypeCreator

let usernamesFromDb : string list = []

let private createPassPhrase() =
    let crypto = System.Security.Cryptography.RandomNumberGenerator.Create()
    let randomNumber = Array.init 32 byte
    crypto.GetBytes(randomNumber)
    randomNumber

let secret =
    let fi = FileInfo("./temp/token.txt") //  TODO: Provide missing text file.
    if not fi.Exists then
        let passPhrase = createPassPhrase()
        if not fi.Directory.Exists then
            fi.Directory.Create()
        File.WriteAllBytes(fi.FullName,passPhrase)
    File.ReadAllBytes(fi.FullName)
    |> System.Text.Encoding.UTF8.GetString

let issuer = "mock-ecommerce.io"

let private algorithm = Microsoft.IdentityModel.Tokens.SecurityAlgorithms.HmacSha256

let generateToken username =
    [ Claim(JwtRegisteredClaimNames.Sub, username);
      Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) ]
    |> Saturn.Auth.generateJWT (secret, algorithm) issuer (DateTime.UtcNow.AddHours(1.0))

let createSessionCredentials username password  =
    match username, password with
    | empty, _ when String.IsNullOrEmpty(empty) -> Error MissingUsername
    | _, empty when String.IsNullOrEmpty(empty) -> Error MissingPassword
    | potentialEmailString, pwd ->
        if potentialEmailString.Contains("@") <> true then
            Error (ErrorWhileValidatingEmail (NotAnEmail potentialEmailString))
        else
            match usernamesFromDb |> List.tryFind(fun x -> potentialEmailString = x) with
            | None -> Error(UsernameNotFound potentialEmailString)
            | Some foundVerifiedEmailString ->
                result {
                    let! verifiedEmail = createEmail foundVerifiedEmailString
                    let secureToken = generateToken verifiedEmail.Value
                    return { UserName = verifiedEmail; Password = pwd; Token = secureToken }
                }
