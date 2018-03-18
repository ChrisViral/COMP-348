"""
Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018
"""

if __name__ == "__main__":

    from Client.database_client import Client

    # Client address
    HOST = "localhost"
    PORT = 9999
    client = Client((HOST, PORT))

    # Keep looping until client shuts down
    while client.active:
        client.request()