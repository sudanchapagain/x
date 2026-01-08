from fastapi import FastAPI, Request, Form, HTTPException
from fastapi.templating import Jinja2Templates
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse, RedirectResponse
from starlette.middleware.sessions import SessionMiddleware
import secrets
from contextlib import asynccontextmanager

from database import db
from cart import cart_manager


@asynccontextmanager
async def lifespan(app: FastAPI):
    await db.init_db()
    yield


app = FastAPI(
    title="medical store",
    description="Online medical store",
    lifespan=lifespan
)

app.add_middleware(SessionMiddleware, secret_key=secrets.token_hex(32))

app.mount("/static", StaticFiles(directory="static"), name="static")

templates = Jinja2Templates(directory="templates")


def get_session_id(request: Request) -> str:
    if "session_id" not in request.session:
        request.session["session_id"] = secrets.token_hex(16)
    return request.session["session_id"]


@app.get("/", response_class=HTMLResponse)
async def index(request: Request):
    products = await db.get_all_products()
    return templates.TemplateResponse(
        "index.html", {"request": request, "products": products}
    )


@app.post("/add-to-cart")
async def add_to_cart(request: Request, code: str = Form(...)):
    session_id = get_session_id(request)
    product = await db.get_product(code)

    if not product:
        raise HTTPException(status_code=404, detail="Product not found")

    cart_manager.add_to_cart(session_id, product)
    return RedirectResponse(url="/", status_code=303)


@app.get("/cart", response_class=HTMLResponse)
async def view_cart(request: Request):
    session_id = get_session_id(request)
    cart_items = cart_manager.get_cart_items(session_id)
    total = cart_manager.get_cart_total(session_id)

    return templates.TemplateResponse(
        "cart.html",
        {
            "request": request,
            "cart_items": cart_items,
            "total": total,
            "cart_empty": len(cart_items) == 0,
        },
    )


@app.post("/cart/update")
async def update_cart(request: Request, code: str = Form(...), action: str = Form(...)):
    session_id = get_session_id(request)

    if action == "add":
        cart_manager.increase_quantity(session_id, code)
    elif action == "subtract":
        cart_manager.decrease_quantity(session_id, code)
    elif action == "remove":
        cart_manager.remove_from_cart(session_id, code)
    elif action == "clear":
        cart_manager.clear_cart(session_id)

    return RedirectResponse(url="/cart", status_code=303)


def main():
    import uvicorn

    uvicorn.run("main:app")


if __name__ == "__main__":
    main()
