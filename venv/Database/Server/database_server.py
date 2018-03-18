"""
Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018
"""

from .database import Database
from socketserver import TCPServer, StreamRequestHandler

class DatabaseHandler(StreamRequestHandler):

    def __init__(self, request, client_address, server):
        self.server = server
        self.database = server.database
        StreamRequestHandler.__init__(self, request, client_address, server)

    def readline(self):
        return self.rfile.readline().decode().strip()

    def writeline(self, response):
        self.wfile.write(response.encode())

    def handle(self):
        request_type = self.readline()

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

    def find_customer(self):
        name = self.readline()
        customer = self.database.get_customer(name)
        if customer == None:
            self.writeline("Customer not found")
        else:
            self.writeline(customer)

    def add_customer(self):
        info = self.readline()
        data = Database.process_customer(info)
        if data != None:
            if self.database.add_customer(data):
                self.writeline("Customer successfully added")
            else:
                self.writeline("Customer already in database")
        else:
            self.writeline("Customer data invalid")

    def delete_customer(self):
        name = self.readline()

        if self.database.delete_customer(name):
            self.writeline("Customer sucessfully deleted")
        else:
            self.writeline("Customer not in database")

    def update_age(self):
        name = self.readline()

        try:
            age = int(self.readline())
            if self.database.update_age(name, age):
                self.writeline("Customer's age successfully updated")
            else:
                self.writeline("Customer is not in database")
        except:
            self.writeline("Invalid age provided")

    def update_address(self):
        name = self.readline()
        address = self.readline()
        if self.database.update_address(name, address):
            self.writeline("Customer's address successfully updated")
        else:
            self.writeline("Customer not in database")

    def update_phone(self):
        name = self.readline()
        phone = self.readline()

        if self.database.update_phone(name, phone):
            self.writeline("Customer's phone successfully updated")
        else:
            self.writeline("Customer not in database")

    def print_report(self):
        self.writeline("\n" + self.database.report())

    def get_pid(self):
        self.writeline(str(self.server.pid))


class DatabaseServer(TCPServer):

    def __init__(self, server_address, handler_class, bind_and_activate = True):
        self.pid = 0
        self.database = Database("data.txt")
        TCPServer.__init__(self, server_address, handler_class, bind_and_activate)
