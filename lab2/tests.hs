import Lab1
import Test.HUnit

testSum = TestCase $ assertEqual "10 + 5 = 15" 15 (add 10 5)

testGaussSimple =
  TestCase
    ( do
        let mA = ([[1, 1, 0], [0, 1, 1], [1, 0, 1]] :: Matrix)
        let mb = [2, 3, 4] :: Vector
        let x = gauss mA mb
        let ans = [1.5, 0.5, 2.5] :: Vector
        assertEqual "some explanation i guess" ans x
    )

testGaussHarder =
  TestCase
    ( do
        let mA = ([[1, -2, 1], [2, 1, -3], [4, -7, 1]] :: Matrix)
        let mb = ([0, 5, -1] :: Vector)
        let x = gauss mA mb
        let ans = [3.0, 2.0, 1.0] :: Vector
        assertEqual "some explanation i guess" ans x
    )

testGaussEvenMoreHarder =
  TestCase
    ( do
        let mA = ([[1, 1, 1, 1], [2, 3, 0, -1], [-3, 4, 1, 2], [1, 2, -1, 1]] :: Matrix)
        let mb = ([13, -1, 10, 1] :: Vector)
        let x = gauss mA mb
        let ans = [2, 0, 6, 5] :: Vector
        assertEqual "some explanation i guess" ans x
    )

testlist =
  TestList
    [ TestLabel "testGaussSimple" testGaussSimple,
      TestLabel "testGaussHarder" testGaussHarder,
      TestLabel "testGaussEvenMoreHarder" testGaussEvenMoreHarder
    ]


main :: IO ()
main = do
  runTestTT testlist
  return ()