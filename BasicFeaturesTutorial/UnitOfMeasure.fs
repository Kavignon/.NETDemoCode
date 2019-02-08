module UnitOfMeasure

[<AutoOpen>]
module Units =
    //Used to represent coordinates in the map
    [<Measure>] type abscissa
    [<Measure>] type ordinate

    //Represents unit for weight in kilograms
    [<Measure>] type kg

    //Represents unit for currency
    [<Measure>] type usd

    // units of measure representing the character and equipment stats
    [<Measure>] type dmg // damage
    [<Measure>] type def // defense
    [<Measure>] type str // strength
    [<Measure>] type spd // speed
    [<Measure>] type intel // intelligence
    [<Measure>] type res // resistance
    [<Measure>] type mgpwr  // magic power
    [<Measure>] type mgres // magic resistance
    [<Measure>] type evd    // evade
    [<Measure>] type lck  // luck
    [<Measure>] type ctr // critical
    [<Measure>] type hl // hit limit
    [<Measure>] type hp // health points
    [<Measure>] type mp // mana points
    [<Measure>] type eu // equipment usage

[<AutoOpen>]
module Energy =

    type IStats =
        abstract member showStat : unit -> string

    type IEnergyPoint =
        abstract capPoints : unit -> IEnergyPoint
        abstract raisePoints : float -> IEnergyPoint
        abstract reducePoints : float -> IEnergyPoint

    type HealthPoint =
        { CurrentLife: float<hp>
          MaxLife : float<hp> }
        interface IEnergyPoint with
            member x.capPoints () =
                if x.CurrentLife > x.MaxLife then { CurrentLife = x.MaxLife; MaxLife = x.MaxLife } :> IEnergyPoint else x :> IEnergyPoint

            member x.raisePoints (raisePoint:float) =
                let lifePoints = raisePoint * 1.0<hp>
                let raisedHealth = { x with CurrentLife = x.CurrentLife + lifePoints }
                (raisedHealth :> IEnergyPoint).capPoints()

            member x.reducePoints (hitPoint: float) =
                let lifePoints = hitPoint * 1.0<hp>
                let reducedLife = { x with CurrentLife = x.CurrentLife - lifePoints }
                (reducedLife :> IEnergyPoint)
        member x.raiseMaxLife (healthPoints: float<hp>) =
            { CurrentLife = x.CurrentLife + healthPoints; MaxLife = x.MaxLife + healthPoints }
        member x.isCharacterDead() =
            x.CurrentLife <= 0.0<hp>

    type ManaPoint =
        { CurrentMana: float<mp>
          MaxMana : float<mp> }
        interface IEnergyPoint with
            member x.capPoints () =
                if x.CurrentMana > x.MaxMana then { ManaPoint.CurrentMana = x.MaxMana; ManaPoint.MaxMana = x.MaxMana } :> IEnergyPoint else x :> IEnergyPoint

            member x.raisePoints (raisePoint:float) =
                let manaPoints = raisePoint * 1.0<mp>
                let raisedHealth = { x with CurrentMana = x.CurrentMana + manaPoints }
                (raisedHealth :> IEnergyPoint).capPoints()

            member x.reducePoints (reducePoint: float) =
                let manaPoints = reducePoint * 1.0<mp>
                let reducedLife = { x with CurrentMana = x.CurrentMana - manaPoints }
                (reducedLife :> IEnergyPoint)
        member x.canCharacterPerformMagic() =
             x.CurrentMana > 0.00<mp>
        member x.capManaAtZero() : ManaPoint =
            if x.CurrentMana < 0.00<mp> then
                { x with CurrentMana = 0.00<mp>}
            else
                x
        member x.raiseMaxMana (manaPoints: float<mp>) =
            { CurrentMana = x.CurrentMana + manaPoints; MaxMana = x.MaxMana + manaPoints }

    type CharacterStats = {
        Health          : HealthPoint
        Mana            : ManaPoint
        Speed           : float<spd>
        Strength        : float<str>
        MagicPower      : float<mgpwr> option
        Defense         : int<def>
        Resistance      : int<res>
        MagicResist     : float<mgres>
        Evade           : int<evd>
        Luck            : int<lck>
    }
    with
        interface IStats with
            member x.showStat() =
                sprintf "Max health : %O - Max mana : %O - Strenght : %O - Defense : %O - Magic power : %O - Resistance : %O - Magic resistance : %O - Evade : %O - Luck : %O" x.Health.MaxLife x.Mana.MaxMana x.Strength x.Defense x.MagicPower x.Resistance x.MagicResist x.Evade x.Luck
        member x.applyTemporaryDefense (tPoints: int<def>) =
            { x with Defense = x.Defense + tPoints }

        static member InitialStats =
        {
            Health = { MaxLife = 100.00<hp>; CurrentLife = 100.00<hp> }
            Mana = { MaxMana = 50.00<mp>; CurrentMana = 50.00<mp> }
            Speed = 1.00<spd>
            Strength = 11.00<str>
            MagicPower = None
            Defense = 12<def>
            Resistance = 6<res>
            MagicResist = 3.00<mgres>
            Evade = 0<evd>
            Luck = 2<lck>
        }