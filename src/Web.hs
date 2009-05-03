module Web where

import Network.Shed.Httpd
import Network.URI
import Text.Printf

start port = do
    printf "Listening to %d\nvisit http://localhost:%d/" port port
    initServer port handler

handler req = do
    print req

    let uri = reqURI req
    putStr "query string: "
    print $ uriQuery uri

    let query_str = uriQuery uri
    putStr "query: "
    print $ queryToArguments query_str

    let path_str = uriPath uri
    if path_str == "/"
        then
            return $ Response 200 [] path_str
        else
            do
                content <- readFile ('.' : path_str)
                return $ Response 200 [] content
