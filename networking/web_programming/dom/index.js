const box = document.getElementById("box")
const swap = document.getElementById("swap")

const products = [{
    name: "product1",
    price: 10
}, {
    name: "product2",
    price: 20
}, {
    name: "product3",
    price: 30
}, {
    name: "product4",
    price: 40
}]
let cart = []
let showCart = false

function showProducts() {
    box.innerHTML = ""

    products.forEach((p, i) => {
        const row = document.createElement("div")
        row.style.padding = "8px"
        row.textContent = p.name + " NPR" + p.price + " "
        const add = document.createElement("button")
        add.textContent = "add"
        add.style.padding = "6px"
        add.onclick = () => {
            cart.push(p)
        }
        row.appendChild(add)
        box.appendChild(row)
    })
}

function drawCart() {
    box.innerHTML = ""
    if (!cart.length) {
        box.textContent = "empty";
        return
    }
    cart.forEach((p, i) => {
        const row = document.createElement("div")
        row.style.padding = "8px"
        row.textContent = p.name + " NPR" + p.price + " "
        const rm = document.createElement("button")
        rm.textContent = "remove"
        rm.style.padding = "6px"
        rm.onclick = () => {
            cart.splice(i, 1);
            drawCart()
        }
        row.appendChild(rm)
        box.appendChild(row)
    })
}

swap.onclick = () => {
    showCart = !showCart
    if (showCart) {
        drawCart();
        swap.textContent = "products"
    } else {
        showProducts();
        swap.textContent = "cart"
    }
}

showProducts()
