module Primes
  let rec primes = 
    let isPrime number primes =
      let sqrtn = float >> sqrt >> int
      primes
        |> Seq.takeWhile (fun n -> n <= (sqrtn number))
        |> Seq.exists (fun n -> number % n = 0)
        |> not

    let rec primes' current =
      seq {
        if primes |> isPrime current then
          yield current
        yield! primes' (current + 2)
      }
    seq {
      yield 2
      yield! primes' 3
    } |> Seq.cache

  let solution = lazy ( primes |> Seq.nth(10000) ) // PE007
  ProjectEuler.Exercises.Utilities.measure_execution_time solution |> ignore
