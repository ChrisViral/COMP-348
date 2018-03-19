"""
Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018
"""

if __name__ == "__main__":

    import sys
    from Server.database_server import DatabaseServer, DatabaseHandler

    # Server data
    HOST: str = "localhost"
    PORT: int = 9999

    if len(sys.argv) > 1:
        FILE = sys.argv[1]
    else:
        FILE = "data.txt"

    with DatabaseServer(FILE, (HOST, PORT), DatabaseHandler) as server:
        # Start server
        print("Starting server...")
        print("Database file is: " + FILE)
        print("Server host is: " + HOST)
        print("Server port is: " + str(PORT))
        print("Server PID: " + str(server.getpid))
        server.serve_forever()