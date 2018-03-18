"""
Christophe Savard
40017812
COMP-348 E
Assignment 2
March 18th 2018
"""

from typing import List

class Database:
    """
    Database storage structure
    """

    def __init__(self, file: str) -> Database:
        """
        Initializes the database in the given text file
        :param file: File to Initialize the database with
        """

        # Initialise database
        self.fileName = file
        self.database = { }
        self.valid = True

        try:

            # Read data from file
            with open(file) as f:
                lines = f.readlines()

            # Process one customer per line
            for line in lines:
                customer = Database.process_customer(line)
                # Add customer if valid
                if customer != None:
                    self.add_customer(customer, False)

            # Update file at end of loading
            self.update_file()
        except:
            # If invalid, let the use know
            self.valid = False
            print("Database file invalid")

    @staticmethod
    def process_customer(line: str) -> List:
        """
        Processes a customer database string into usable data
        :param line: Customer database string
        :return:     A list containing the customer data, or None if it could not be processed
        """

        # Split the database string
        data = line.strip().split('|')

        # If of incorrect length, return
        if len(data) != 4:
            return None

        # Trim all whitespace from data
        data = [d.strip() for d in data]

        # If name is of size 0, return
        if len(data[0]) == 0:
            return None

        try:
            # Try and parse age into number
            data[1] = int(data[1])
        except:
            #If fails, put 0 as the age
            data[1] = 0

        # Return the resulting data
        return data

    @staticmethod
    def format_customer(name: str, data: List) -> str:
        """
        Formats customer data into a database string
        :param name: Customer's name
        :param data: Customer's other data
        :return:     The resulting database string
        """

        return "{}|{}|{}|{}".format(name, data[0], data[1], data[2])

    def has_customer(self, name: str) -> bool:
        """
        Determine if the customer is present within the database
        :param name: Name of the customer to find
        :return:     True if the customer is in the database, false otherwise
        """

        return name in self.database

    def get_customer(self, name: str) -> str:
        """
        Gets the associated customer database string to a given customer
        :param name: Name of the customer
        :return:     The customer database string of this customer, or None if it could not be found
        """

        if self.has_customer(name):
            return Database.format_customer(name, self.database[name])
        else:
            return None

    def add_customer(self, data: List, update: bool = True) -> bool:
        """
        Adds a customer to the database
        :param data:   Customer data to add
        :param update: If the database text file should be updated
        :return:       True if the customer has been added to the database, false otherwise
        """

        # Only add if the customer is not found within the database
        if not self.has_customer(data[0]):

            # Add the customer using the name as the key, other fields serve as data
            self.database[data[0]] = data[1:]

            # Update the file if necessary
            if update:
                self.update_file()
            return True

        return False

    def delete_customer(self, name: str) -> bool:
        """
        Deletes the given customer from the database
        :param name: Customer to delete
        :return:     True if the customer has been deleted, false otherwise
        """
        # Only delete if the customer is in the database
        if self.has_customer(name):
            # Delete customer, then update database file
            del self.database[name]
            self.update_file()
            return True

        return False

    def update_age(self, name: str, age: int) -> bool:
        """
        Updates the age of a given customer
        :param name: Name of the customer to update
        :param age:  New age of the customer
        :return:     True if the customer has been updated, false otherwise
        """

        # Only update if the customer is within the database
        if self.has_customer(name):
            # Update age then update database file
            self.database[name][0] = age
            self.update_file()
            return True

        return False

    def update_address(self, name: str, address: str) -> bool:
        """
        Updates the address of a given customer
        :param name:    Name of the customer to update
        :param address: New address of the customer
        :return:        True if the customer has been updated, false otherwise
        """

        # Only update if the customer is in the database
        if self.has_customer(name):
            # Update address then update database file
            self.database[name][1] = address
            self.update_file()
            return True

        return False

    def update_phone(self, name: str, phone: str) -> bool:
        """
        Updates the phone of a given customer
        :param name:  Name of the customer to update
        :param phone: New phone of the customer
        :return:      True if the customer has been updated, false otherwise
        """

        # Only update if the customer is in the database
        if self.has_customer(name):
            # Update phone then update database file
            self.database[name][2] = phone
            self.update_file()
            return True

        return False

    def report(self) -> str:
        """
        Creates a string report of all the customers in the database, by alphabetical name order
        :return: Report of all the customers in the database
        """

        # Sort keys by alphabetical order
        lines = ""
        keys = sorted(self.database.keys())

        # Add them line by line
        for key in keys:
            lines += Database.format_customer(key, self.database[key]) + "\n"

        return lines

    def update_file(self) -> None:
        """
        Updates the Database text file on the disk
        """

        if self.valid:
            with open(self.fileName, "w") as f:
                f.write(self.report())