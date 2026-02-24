use axum::{
    Router,
    extract::ConnectInfo,
    extract::ws::{Message, WebSocket, WebSocketUpgrade},
    response::IntoResponse,
    routing::get,
};
use futures::StreamExt;
use std::net::SocketAddr;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() {
    let app = Router::new().route("/ws", get(ws_handler));

    let listener = TcpListener::bind("127.0.0.1:3000").await.unwrap();
    println!("listening on ws://localhost:3000/ws");

    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .await
    .unwrap();
}

async fn ws_handler(
    (ws, ConnectInfo(addr)): (WebSocketUpgrade, ConnectInfo<SocketAddr>),
) -> impl IntoResponse {
    println!("New WS connection from {}", addr);
    ws.on_upgrade(handle_socket)
}

async fn handle_socket(mut socket: WebSocket) {
    while let Some(Ok(msg)) = socket.next().await {
        match msg {
            Message::Text(text) => {
                println!("> {}", text);
                let _ = socket
                    .send(Message::Text(format!("echo: {}", text).into()))
                    .await;
            }
            Message::Close(_) => {
                println!("connection closed");
                break;
            }
            _ => {}
        }
    }
}
