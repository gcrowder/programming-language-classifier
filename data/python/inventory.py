class Inventory:
    def __init__(self, name="Albums"):
        self.name = name

    def sell_album(self):
        pass

    def add_album(self):
        pass

    def discount_album(self, album):
        pass

    def discount_artist_catalog(self,artist):
        pass
        
class Artist:
    def __init__(self, name):
        self.name = name
        self.albums = []

    def add_album(self, album):
        self.albums.append(album)

class Album:
    def __init__(self, name, quantity=0, normal_price=1):
        self.name = name
        self.quantity = quantity
        self.normal_price = normal_price
        self.discount = 0
