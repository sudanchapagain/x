from typing import Dict, List
from database import CartItem, Product


class CartManager:
    
    def __init__(self):
        self.carts: Dict[str, Dict[str, CartItem]] = {}
    
    def get_cart(self, session_id: str) -> Dict[str, CartItem]:
        if session_id not in self.carts:
            self.carts[session_id] = {}
        return self.carts[session_id]
    
    def add_to_cart(self, session_id: str, product: Product, quantity: int = 1):
        cart = self.get_cart(session_id)
        
        if product.code in cart:
            cart[product.code].quantity += quantity
        else:
            cart[product.code] = CartItem(
                code=product.code,
                name=product.name,
                price=product.price,
                image=product.image,
                quantity=quantity
            )
    
    def update_quantity(self, session_id: str, product_code: str, quantity: int):
        cart = self.get_cart(session_id)
        if product_code in cart and quantity > 0:
            cart[product_code].quantity = quantity
    
    def remove_from_cart(self, session_id: str, product_code: str):
        cart = self.get_cart(session_id)
        if product_code in cart:
            del cart[product_code]
    
    def increase_quantity(self, session_id: str, product_code: str):
        cart = self.get_cart(session_id)
        if product_code in cart:
            cart[product_code].quantity += 1
    
    def decrease_quantity(self, session_id: str, product_code: str):
        cart = self.get_cart(session_id)
        if product_code in cart:
            if cart[product_code].quantity > 1:
                cart[product_code].quantity -= 1
            else:
                self.remove_from_cart(session_id, product_code)
    
    def clear_cart(self, session_id: str):
        self.carts[session_id] = {}
    
    def get_cart_total(self, session_id: str) -> float:
        cart = self.get_cart(session_id)
        return sum(item.price * item.quantity for item in cart.values())
    
    def get_cart_items(self, session_id: str) -> List[CartItem]:
        cart = self.get_cart(session_id)
        return list(cart.values())


cart_manager = CartManager()
