
class Database:

    def __init__(self, file):

        # Initialise dictionary
        self.fileName = file
        self.database = { }

        # Read all data from file
        with open(file) as f:
            lines = f.readlines()

        # Add one customer per line
        for line in lines:
            customer = Database.process_customer(line)
            if customer != None:
                self.add_customer(customer, False)

        self.update_file()

    @staticmethod
    def process_customer(line):
        data = line.strip().split('|')
        if len(data) != 4:
            return None

        data = [d.strip() for d in data]
        if len(data[0]) == 0:
            return None

        try:
            data[1] = int(data[1])
        except:
            data[1] = 0

        return data

    @staticmethod
    def format_customer(name, data):
        return "{}|{}|{}|{}".format(name, data[0], data[1], data[2])

    def has_customer(self, name):
        return name in self.database

    def get_customer(self, name):
        if not self.has_customer(name):
            return None

        customer = self.database[name]
        return Database.format_customer(name, customer)

    def add_customer(self, data, update = True):
        if not self.has_customer(data[0]):
            self.database[data[0]] = data[1:]
            if update:
                self.update_file()
            return True
        return False

    def delete_customer(self, name):
        if self.has_customer(name):
            del self.database[name]
            self.update_file()
            return True
        return False

    def update_age(self, name, age):
        if self.has_customer(name):
            self.database[name][0] = age
            self.update_file()
            return True
        return False

    def update_address(self, name, address):
        if self.has_customer(name):
            self.database[name][1] = address
            self.update_file()
            return True
        return False

    def update_phone(self, name, phone):
        if self.has_customer(name):
            self.database[name][2] = phone
            self.update_file()
            return True
        return False

    def report(self):
        lines = ""
        for key in self.database:
            lines += Database.format_customer(key, self.database[key]) + "\n"

        return lines

    def update_file(self):
        with open(self.fileName, "w") as f:
            f.write(self.report())