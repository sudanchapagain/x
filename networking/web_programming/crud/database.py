import aiosqlite
from typing import List, Optional
from dataclasses import dataclass


@dataclass
class Product:
    code: str
    name: str
    price: float
    image: str


@dataclass
class CartItem:
    code: str
    name: str
    price: float
    image: str
    quantity: int


class Database:

    def __init__(self, db_path: str = "store.db"):
        self.db_path = db_path

    async def init_db(self):
        async with aiosqlite.connect(self.db_path) as db:
            await db.execute(
                """
                CREATE TABLE IF NOT EXISTS products (
                    code TEXT PRIMARY KEY,
                    name TEXT NOT NULL,
                    price REAL NOT NULL,
                    image TEXT NOT NULL
                )
            """
            )

            await db.execute(
                """
                CREATE TABLE IF NOT EXISTS cart_items (
                    session_id TEXT,
                    product_code TEXT,
                    quantity INTEGER,
                    PRIMARY KEY (session_id, product_code),
                    FOREIGN KEY (product_code) REFERENCES products (code)
                )
            """
            )

            sample_products = [
                ("1", "B Complex", 250.0, "bcomplex.png"),
                ("2", "Betadine", 50.0, "betadine.png"),
                ("3", "Cetrizine", 80.0, "cetrizine.png"),
                ("4", "diclofenac", 20.0, "diclofenac.png"),
                ("5", "gauze", 20.0, "gauze.png"),
                ("6", "paracetamol", 20.0, "paracetamol.png"),
                ("7", "thermometer", 200.0, "thermometer.png"),
            ]

            for code, name, price, image in sample_products:
                await db.execute(
                    "INSERT OR IGNORE INTO products (code, name, price, image) VALUES (?, ?, ?, ?)",
                    (code, name, price, image),
                )

            await db.commit()

    async def get_all_products(self) -> List[Product]:
        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute("SELECT code, name, price, image FROM products")
            rows = await cursor.fetchall()
            return [
                Product(code=row[0], name=row[1], price=row[2], image=row[3])
                for row in rows
            ]

    async def get_product(self, code: str) -> Optional[Product]:
        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute(
                "SELECT code, name, price, image FROM products WHERE code = ?", (code,)
            )
            row = await cursor.fetchone()
            if row:
                return Product(code=row[0], name=row[1], price=row[2], image=row[3])
            return None


db = Database()
