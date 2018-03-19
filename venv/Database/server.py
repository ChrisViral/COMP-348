"""
Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018
"""

if __name__ == "__main__":

    from Server.database_server import DatabaseServer, DatabaseHandler

    # Server data
    FILE: str = "data.txt"
    HOST: str = "localhost"
    PORT: int = 9999

    with DatabaseServer(FILE, (HOST, PORT), DatabaseHandler) as server:
        # Start server
        print("Server PID: " + str(server.pid))
        server.serve_forever()