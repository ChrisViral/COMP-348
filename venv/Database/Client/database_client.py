"""
Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018
"""

import os
import signal
import socket
from typing import Tuple

class Client:
    """
    Database server client
    """

    # Database query prompt
    prompt: str = """\
=== Database Menu ===

1. Find customer
2. Add customer
3. Delete customer
4. Update customer age
5. Update customer address
6. Update customer phone
7. Print report
8. Exit
                
Select: """
    # Database response buffer size
    buffer: int = 1024

    def __init__(self, server_address: Tuple) -> None:
        """
        Initializes this Client
        :param server_address: The server address
        """

        # Inititate fields
        self.active: bool = True
        self.address: Tuple = server_address
        self.sock: socket.socket = None

    @staticmethod
    def ask_command() -> int:
        """
        Asks the user for a database query to make
        :return: The command number chosen by the user (1 to 8)
        """

        # Loop until resolved
        while True:

            # Ask for value
            value = input(Client.prompt).strip()

            #Try and parse value
            try:
                num = int(value)

                # See if the value is in the valid range
                if num >= 1 and num <= 8:
                    return num
                else:
                    print("Please enter a number between 1 and 8")
            except:
                print("Please enter a valid integer")

    @staticmethod
    def ask_yesno(prompt: str) -> bool:
        """
        Asks the user a yes or no question
        :param prompt: The question to ask the user
        :return: True if the user answered yes, False otherwise
        """

        # Loop until resolved
        while True:

            # Ask for user input
            yn = input(prompt).strip().upper()

            # Test for result
            if yn == "Y":
                return True
            elif yn == "N":
                return False
            else:
                print("Please enter Y or N")

    @staticmethod
    def ask_str(prompt: str) -> str:
        """
        Asks the uiser for a text input
        :param prompt: Question to ask the user
        :return:       The user's answer
        """

        # Loop until resolved
        while True:
            # Ask user for input
            name = input(prompt).strip()

            # Test to see if the string is empty
            if len(name) != 0:
                return name
            else:
                print("Please enter a string of nonzero length")

    @staticmethod
    def ask_int(prompt: str) -> int:
        """
        Ask the user for an integer input
        :param prompt: Question to ask the user
        :return:       Integer value entered by the user
        """

        # Loop until resolved
        while True:

            #Ask user for value
            value = input(prompt).strip()

            # Try to parse as integer
            try:
                num = int(value)
                return num
            except:
                print("Please enter a valid integer")

    @staticmethod
    def println(message: str):
        """
        Prints a given message to the console with a newline character
        :param message: Message to print
        """

        print(message + "\n")

    def connect(self) -> bool:
        """
        Connects the client to the server through a socket connection. The client shuts down if it cannot connect
        :return: True if the client could connect, false otherwise
        """

        # Try and connect to the server
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect(self.address)
            return True

        except:
            # Let the user know the connection failed
            Client.println("Could not connect to the server")
            self.active = False
            return False

    def send_request(self, request: str) -> None:
        """
        Sends a request to the server
        :param request: String request to send
        """

        self.sock.sendall((request + "\n").encode())

    def get_server_string(self) -> str:
        """
        Gets the direct string response from the server
        :return: The server's response
        """

        return self.sock.recv(Client.buffer).decode().strip()

    def receive_response(self) -> str:
        """
        Gets a formatted response from the server
        :return: The server response
        """

        return "Server response: " + self.get_server_string()

    def request(self) -> None:
        """
        Initializes a server request
        """

        # Try and connect to the server
        if self.connect():

            # Ask the user for a command
            command = Client.ask_command()

            # Take action according to demand
            if command == 1:
                self.find_customer()
            elif command == 2:
                self.add_customer()
            elif command == 3:
                self.delete_customer()
            elif command == 4:
                self.update_age()
            elif command == 5:
                self.update_address()
            elif command == 6:
                self.update_phone()
            elif command == 7:
                self.print_report()
            elif command == 8:
                self.exit()
            else:
                # Should not happen
                print("Invalid request")

            # Finalize the connection
            self.close()

    def find_customer(self) -> None:
        """
        Queries the server to find a customer
        """

        # Get client name
        name = Client.ask_str("Enter the customer's name to find: ")

        # Send request to server then print response
        self.send_request("\n".join(("find_customer", name)))
        Client.println(self.receive_response())

    def add_customer(self) -> None:
        """
        Queries the server to add a customer
        """

        # Ask the user for the customer's info
        name = Client.ask_str("Enter the new customer's name: ")
        age = Client.ask_int("Enter the new customer's age: ")
        address = Client.ask_str("Enter the new customer's address: ")
        phone = Client.ask_str("Enter the new customer's phone: ")

        # Send request to server then print response
        self.send_request("\n".join(("add_customer", "{}|{}|{}|{}".format(name, age, address, phone))))
        Client.println(self.receive_response())

    def delete_customer(self) -> None:
        """
        Queries the server to delete a customer
        """

        # Ask the customer's name
        name = Client.ask_str("Enter the customer's name to delete: ")

        # Send request to the server then print response
        self.send_request("\n".join(("delete_customer", name)))
        Client.println(self.receive_response())

    def update_age(self) -> None:
        """
        Queries the server to update a customer's age
        """

        # Ask for the customer's name and new age
        name = Client.ask_str("Enter the customer's name to update: ")
        age = Client.ask_int("Enter the customer's new age: ")

        # Send request to the server and print response
        self.send_request("\n".join(("update_age", name, str(age))))
        Client.println(self.receive_response())

    def update_address(self) -> None:
        """
        Queries the server to update a customer's address
        """

        # Ask for the customer's name and new address
        name = Client.ask_str("Enter the customer's name to update: ")
        address = Client.ask_str("Enter the customer's new address: ")

        # Send request to server and print response
        self.send_request("\n".join(("update_address", name, address)))
        Client.println(self.receive_response())

    def update_phone(self) -> None:
        """
        Queries the server to update a customer's phone
        """

        # Ask for the customer's name and new phone
        name = Client.ask_str("Enter the customer's name to update: ")
        phone = Client.ask_str("Enter the customer's new phone: ")

        # Send the request to the server and print response
        self.send_request("\n".join(("update_phone", name, phone)))
        Client.println(self.receive_response())

    def print_report(self) -> None:
        """
        Queries the server for a full report of the database
        """

        # Send request to server and print response
        self.send_request("print_report")
        Client.println("\n=== Database records ===\n" + self.get_server_string())

    def exit(self) -> None:
        """
        Closes the client
        """
        # Ask the user if the server should also close
        self.active = False
        terminate = self.ask_yesno("Do you also want to terminate the server? ")

        if terminate:
            # If so, get the server's PID
            self.send_request("get_pid")
            pid = self.get_server_string()

            # Try and shut down the server
            try:
                # On Windows
                if os.name == "nt":
                    os.system("taskkill /F /PID " + pid)
                else:
                    os.kill(int(pid), signal.SIGTERM)

            except:
                Client.println("Could not kill server from program, please manually kill the PID " + pid + " if you wish to kill the server")

    def close(self) -> None:
        """
        Closes the socket connection to the server
        """

        self.sock.close()