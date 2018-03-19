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
    buffer: int = 1024

    def __init__(self, server_address):
        self.active: bool = True
        self.address: Tuple = server_address

    @staticmethod
    def ask_command():

        while True:
            value = input(Client.prompt)
            try:
                num = int(value)
                if num < 1 or num > 8:
                    print("Please enter a number between 1 and 8")
                else:
                    return num
            except:
                print("Please enter a valid integer")

    @staticmethod
    def ask_yesno(prompt):

        while True:
            yn = input(prompt).upper()
            if yn == "Y":
                return True
            elif yn == "N":
                return False
            else:
                print("Please enter Y or N")

    @staticmethod
    def ask_str(prompt):

        while True:
            name = input(prompt)
            if len(name) == 0:
                print("Please enter a string of nonzero length")
            else:
                return name

    @staticmethod
    def ask_int(prompt):

        while True:
            value = input(prompt)
            try:
                num = int(value)
                return num
            except:
                print("Please enter a valid integer")

    @staticmethod
    def println(message):
        print(message + "\n")

    def connect(self):
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect(self.address)
            return True
        except:
            Client.println("Could not connect to the server")
            self.active = False
            return False

    def send_request(self, request):
        self.sock.sendall((request + "\n").encode())

    def receive_response(self):
        return "Server response: " + self.sock.recv(Client.buffer).decode().strip()

    def request(self):
        command = Client.ask_command()

        if self.connect():
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
                print("Invalid request")

            self.close()

    def find_customer(self):
        name = Client.ask_str("Enter the customer's name to find: ")

        self.send_request("\n".join(("find_customer", name)))
        Client.println(self.receive_response())

    def add_customer(self):
        name = Client.ask_str("Enter the new customer's name: ")
        age = Client.ask_int("Enter the new customer's age: ")
        address = Client.ask_str("Enter the new customer's address: ")
        phone = Client.ask_str("Enter the new customer's phone: ")

        self.send_request("\n".join(("add_customer", "{}|{}|{}|{}".format(name, age, address, phone))))
        Client.println(self.receive_response())

    def delete_customer(self):
        name = Client.ask_str("Enter the customer's name to delete: ")

        self.send_request("\n".join(("delete_customer", name)))
        Client.println(self.receive_response())

    def update_age(self):
        name = Client.ask_str("Enter the customer's name to update: ")
        age = Client.ask_int("Enter the customer's new age: ")

        self.send_request("\n".join(("update_age", name, str(age))))
        Client.println(self.receive_response())

    def update_address(self):
        name = Client.ask_str("Enter the customer's name to update: ")
        address = Client.ask_str("Enter the customer's new address: ")

        self.send_request("\n".join(("update_address", name, address)))
        Client.println(self.receive_response())

    def update_phone(self):
        name = Client.ask_str("Enter the customer's name to update: ")
        phone = Client.ask_str("Enter the customer's new phone: ")

        self.send_request("\n".join(("update_phone", name, phone)))
        Client.println(self.receive_response())

    def print_report(self):
        self.send_request("print_report")
        Client.println(self.receive_response())

    def exit(self):
        self.active = False
        terminate = self.ask_yesno("Do you also want to terminate the server? ")

        if terminate:
            self.send_request("get_pid")
            pid = self.sock.recv(1024).decode().strip()

            try:
                # On Windows
                if os.name == "nt":
                    os.system("taskkill /F /PID " + pid)
                else:
                    os.kill(int(pid), signal.SIGTERM)

            except:
                Client.println("Could not kill server from program, please manually kill the PID " + pid + " if you wish to kill the server")

    def close(self):
        self.sock.close()