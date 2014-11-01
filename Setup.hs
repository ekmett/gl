import Distribution.Simple
import Gen.Generator
import Gen.Parser
import Gen.Registry

generateAPI :: IO ()
generateAPI = do
  registry <- parseFile "gl.xml"
  -- TODO: check the date on gl.xml, Gen.*, Setup.hs against dist/build/autogen/Graphics?
  generateSource (deshenaniganize registry)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
    generateAPI
    buildHook simpleUserHooks pkg lbi hooks flags
  }
