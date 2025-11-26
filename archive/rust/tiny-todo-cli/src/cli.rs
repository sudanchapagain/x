use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "td")]
#[command(about = "simple tiny todo cli", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    Add {
        title: String,
    },
    List,
    Delete {
        id: u32,
    },
    Update {
        id: u32,
        title: String,
    },
}
