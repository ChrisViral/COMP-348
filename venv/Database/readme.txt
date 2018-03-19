Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018

The server is run with server.py
The default database text file is data.txt and must be in the root folder
Another text file path can be provided as the first argument (python server.py path.txt)

The client is run with client.py
It will fail if the server is not running when passing a server request
Upon exiting, the client can optionally close the server it is connected to (only tested on Windows, but should work on other platforms)
Otherwise, the PID of the server is printed by the server on launch