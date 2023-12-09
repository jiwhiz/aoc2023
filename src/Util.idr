module Util

import System.File

export covering
parseFile : (path : String) -> IO (List String)
parseFile path =
    let
        covering go : (List String) -> File -> IO (Either FileError (List String))
        go acc file = do
            False <- fEOF file | True => pure (Right acc)
            Right line <- fGetLine file
                | Left err => pure (Left err)
            go (line :: acc) file
    in do
        result <- withFile path Read pure (go [])
        case result of
            (Left err) => pure []
            (Right lines) => pure $ reverse lines
