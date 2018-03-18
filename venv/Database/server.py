"""
Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018
"""

if __name__ == "__main__":

    import os
    from Server.database_server import DatabaseServer, DatabaseHandler

    # Server address
    HOST = "localhost"
    PORT = 9999

    with DatabaseServer((HOST, PORT), DatabaseHandler) as server:
        if server.database.valid:
            # Start server
            server.pid = os.getpid()
            print("Server PID: " + str(server.pid))
            server.serve_forever()
        else:
            # Let user know the server failed to start
            print("Database loaded is invalid, shutting down server")