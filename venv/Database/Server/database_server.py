"""
Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018
"""

import os
from typing import Tuple
from .database import Database
from socket import socket
from socketserver import TCPServer, BaseServer, StreamRequestHandler, BaseRequestHandler

class DatabaseServer(TCPServer):
    """
    Database Server object
    """

    @property
    def getpid(self) -> int:
        """
        PID of the server
        :return: PID of the server
        """
        return self.pid

    def __init__(self, file: str, server_address: Tuple, handler_class: BaseRequestHandler, bind_and_activate: bool = True) -> None:
        """
        Creates a new DatabaseServer with the given Database file location
        :param file:              Database file location
        :param server_address:    Address and port of the server
        :param handler_class:     The request handler class
        :param bind_and_activate: If the server should activate on initialization
        """

        self.pid: int = os.getpid()
        self.file: str = file
        self.database: Database = Database(file)
        TCPServer.__init__(self, server_address, handler_class, bind_and_activate)


class DatabaseHandler(StreamRequestHandler):
    """
    Database server request handler
    """

    def __init__(self, request: socket, client_address: Tuple, server: BaseServer) -> None:
        """
        Creates a new handler for the given client request
        :param request:        Client socket requesting
        :param client_address: Client address
        :param server:         Server requested
        """

        self.server: BaseServer = server
        self.database: Database = server.database
        StreamRequestHandler.__init__(self, request, client_address, server)

    def readline(self) -> str:
        """
        Reads the next line from the request message
        :return: The string line extracted from the request
        """

        return self.rfile.readline().decode().strip()

    def writeline(self, response: str) -> None:
        """
        Writes the given line to the server response
        :param response: Text to write to the server response
        """

        self.wfile.write((response + "\n").encode())

    def handle(self) -> None:
        """
        Handles the request from the client
        """

        # Get request type
        request_type = self.readline()

        # Take action according to the request type
        if request_type == "find_customer":
            self.find_customer()
        elif request_type == "add_customer":
            self.add_customer()
        elif request_type == "delete_customer":
            self.delete_customer()
        elif request_type == "update_age":
            self.update_age()
        elif request_type == "update_address":
            self.update_address()
        elif request_type == "update_phone":
            self.update_phone()
        elif request_type == "print_report":
            self.print_report()
        elif request_type == "get_pid":
            self.get_pid()
        else:
            self.writeline("Invalid request made")

    def find_customer(self) -> None:
        """
        Handles a find_customer request
        """

        # Get name from request
        name = self.readline()
        # Get customer from database
        customer = self.database.get_customer(name)

        # Handle non existing customer
        if customer != None:
            self.writeline(customer)
        else:
            self.writeline("Customer not found")

    def add_customer(self) -> None:
        """
        Handles add_customer request
        """

        # Get new customer info string from request
        info = self.readline()
        # Parse customer info
        data = Database.process_customer(info)

        # Handle invalid info
        if data != None:
            #Try and add customer to database
            if self.database.add_customer(data):
                self.writeline("Customer successfully added")
            else:
                self.writeline("Customer already in database")
        else:
            self.writeline("Customer data invalid")

    def delete_customer(self) -> None:
        """
        Handles delete_customer request
        """

        # Get customer name from request
        name = self.readline()

        # Try and delete customer
        if self.database.delete_customer(name):
            self.writeline("Customer sucessfully deleted")
        else:
            self.writeline("Customer not in database")

    def update_age(self) -> None:
        """
        Handle update_age request
        """

        # Get customer name from request
        name = self.readline()

        # Try and parse age to int
        try:
            age = int(self.readline())
            # Try and update age
            if self.database.update_age(name, age):
                self.writeline("Customer's age successfully updated")
            else:
                self.writeline("Customer is not in database")
        except:
            self.writeline("Invalid age provided")

    def update_address(self) -> None:
        """
        Handles update_address request
        """

        # Get name and address from request
        name = self.readline()
        address = self.readline()

        # Try and update address
        if self.database.update_address(name, address):
            self.writeline("Customer's address successfully updated")
        else:
            self.writeline("Customer not in database")

    def update_phone(self) -> None:
        """
        Handles update_phone request
        """

        # Get name and phone from request
        name = self.readline()
        phone = self.readline()

        # Try and update phone
        if self.database.update_phone(name, phone):
            self.writeline("Customer's phone successfully updated")
        else:
            self.writeline("Customer not in database")

    def print_report(self) -> None:
        """
        Handles print_report request
        """

        # Send database report as response
        self.writeline("\n" + self.database.report())

    def get_pid(self) -> None:
        """
        Handles get_pid request
        """

        # Send server PID as response
        self.writeline(str(self.server.pid))
