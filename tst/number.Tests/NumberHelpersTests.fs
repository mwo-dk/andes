namespace Andes.Tests

module NumberHelpersTests =

    open System
    open Andes.NumberHelpers
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open System.Globalization

    let private english = CultureInfo("en-US")
    let private englishNumber = english.NumberFormat

    module isInfTests =

        [<Property>]
        let ``isInfinity should return true for positive infinity`` () =
            isInf Double.PositiveInfinity

        [<Property>]
        let ``isInfinity should return true for negative infinity`` () =
            isInf Double.NegativeInfinity

        [<Property>]
        let ``isInfinity should return false for finite numbers`` (x: NormalFloat) =
            let x = x.Get
            isInf x |> not

    module isNanTests =
        [<Property>]
        let ``isNaN should return true for NaN`` () =
            isNaN Double.NaN
    
        [<Property>]
        let ``isNaN should return false for finite numbers`` (x: NormalFloat) =
            let x = x.Get
            isNaN x |> not

    module isIntegerTests =
        [<Theory>]
        [<InlineData(null)>]
        [<InlineData("")>]
        [<InlineData(" ")>]
        [<InlineData(" \t ")>]
        let ``isInteger should return false for null and white space`` (x: string) =
            Assert.False(isInteger x)

        [<Theory>]
        [<InlineData("a")>]
        [<InlineData("1 0")>]
        [<InlineData("1.0")>]
        [<InlineData("1-0")>]
        let ``isInteger should return false for invalid input`` (x: string) =
            Assert.False(isInteger x)

        [<Property>]
        let ``isInteger should return true for integers`` (x: int) =
            isInteger (x.ToString(englishNumber))

        [<Property>]
        let ``isInteger should return false for non-integers`` (x: NormalFloat) =
            let x = x.Get
            (isInteger (x.ToString(englishNumber))) |> not

    module isRealTests =
        [<Theory>]
        [<InlineData(null)>]
        [<InlineData("")>]
        [<InlineData(" ")>]
        [<InlineData(" \t ")>]
        let ``isReal should return false for null and white space`` (x: string) =
            Assert.False(isReal x)

        [<Theory>]
        [<InlineData("a")>]
        [<InlineData("1 0")>]
        [<InlineData("1?0")>]
        [<InlineData("1-0")>]
        let ``isReal should return false for invalid input`` (x: string) =
            Assert.False(isReal x)

        [<Property>]
        let ``isReal should return true for real numbers`` (x: NormalFloat) =
            let x = x.Get
            isReal (x.ToString(englishNumber))

        [<Property>]
        let ``isReal should return false for non-real numbers`` (x: string) =
            isReal $"a{x}" |> not

    module isImaginaryTests =

        [<Theory>]
        [<InlineData(null)>]
        [<InlineData("")>]
        [<InlineData(" ")>]
        [<InlineData(" \t ")>]
        let ``isImaginary should return false for null and white space`` (x: string) =
            Assert.False(isImaginary x)

        [<Theory>]
        [<InlineData("a")>]
        [<InlineData("1 0")>]
        [<InlineData("1?0")>]
        [<InlineData("1-0")>]
        let ``isImaginary should return false for invalid input`` (x: string) =
            Assert.False(isImaginary x)

        [<Property>]
        let ``isImaginary should return false for real numbers`` (x: NormalFloat) =
            let x = x.Get
            isImaginary (x.ToString(englishNumber)) |> not

        [<Property>]
        let ``isImaginary should return true for "positive" imaginary numbers with i as prefix`` (x: NormalFloat) =
            let x = x.Get
            (isImaginary $"i{(abs x).ToString(englishNumber)}")

        [<Property>]
        let ``isImaginary should return true for "negative" imaginary numbers with i as prefix`` (x: NormalFloat) =
            let x = x.Get
            (isImaginary $"-i{(abs x).ToString(englishNumber)}")

        [<Property>]
        let ``isImaginary should return true for "positive" imaginary numbers with i as suffix`` (x: NormalFloat) =
            let x = x.Get
            (isImaginary $"{(abs x).ToString(englishNumber)}i")

        [<Property>]
        let ``isImaginary should return true for "negative" imaginary numbers with i as suffix`` (x: NormalFloat) =
            let x = x.Get
            (isImaginary $"-{(abs x).ToString(englishNumber)}i")

        [<Property>]
        let ``isImaginary should return false for non-imaginary numbers`` (x: string) =
            isImaginary $"a{x}" |> not
