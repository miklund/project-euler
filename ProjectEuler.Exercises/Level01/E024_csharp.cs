namespace ProjectEuler24
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public static class Program
    {
        static void Main(string[] args)
        {
            var alphabet = new List<string> { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" };
            Console.WriteLine("{0}", alphabet.FindPermutationAt(1000000));
        }

        /// <summary>
        /// Find specific permutation by index
        /// </summary>
        /// <param name="alphabet">The alphabet, example [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] must be in ordered</param>
        /// <param name="searchIndex">The index in the permutation we're looking for</param>
        private static string FindPermutationAt(this List<string> alphabet, int searchIndex)
        {
            return FindPermutationAt(alphabet, 0, searchIndex);
        }

        /// <summary>
        /// Inner calculation, don't call this directly
        /// </summary>
        private static string FindPermutationAt(List<string> alphabet, int currentIndex, int searchIndex)
        {
            // Factorial computation, does this exist in .NET framework already?
            Func<int, int> factorial = n => Enumerable.Range(1, n).Aggregate((acc, x) => acc * x);

            // Exit condition
            if (alphabet.Count == 1)
            {
                return alphabet.Single();
            }

            // Number of combinations for each sybil in the alphabet
            int combinations = factorial(alphabet.Count - 1);

            // foreach sybil in alphabet
            for (int i = 0, lowIndex = currentIndex; i < alphabet.Count; i++, lowIndex += combinations)
            {
                int highIndex = lowIndex + combinations;

                // Search index should be between lowIndex and highIndex 
                if (searchIndex >= lowIndex && searchIndex <= highIndex)
                {
                    var found = alphabet[i];

                    // Remove found sybil from alphabet
                    var newAlphabet = alphabet.Except(new[] { found }).ToList();

                    // Add and recurse
                    return found + FindPermutationAt(newAlphabet, lowIndex, searchIndex);
                }
            }

            // Should only end up here if we ask for searchIndex more than max
            throw new IndexOutOfRangeException("No such index exist in permutation: " + searchIndex);
        }
    }
}