
if __name__ == "__main__":

    from Client.database_client import Client

    HOST = "localhost"
    PORT = 9999
    client = Client((HOST, PORT))

    while client.active:
        client.request()