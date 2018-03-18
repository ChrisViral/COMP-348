
if __name__ == "__main__":

    import os
    from Server.database_server import DatabaseServer, DatabaseHandler

    HOST = "localhost"
    PORT = 9999

    with DatabaseServer((HOST, PORT), DatabaseHandler) as server:
        server.pid = os.getpid()
        print("Server PID: " + str(server.pid))
        server.serve_forever()