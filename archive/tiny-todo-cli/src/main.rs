mod cli;
mod todo;
mod file;

use anyhow::Result;
use clap::Parser;
use crate::cli::{Cli, Commands};
use crate::todo::Todo;
use crate::file::{load, save};

fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut todos = load()?;

    match cli.command {
        Commands::Add { title } => {
            let id = todos.last().map(|t| t.id + 1).unwrap_or(1);
            let todo = Todo {
                id,
                title,
                done: false,
            };
            todos.push(todo);
            save(&todos)?;
            println!("Todo added.");
        }
        Commands::List => {
            for todo in &todos {
                println!(
                    "{} [{}] - {}",
                    todo.id,
                    if todo.done { "x" } else { " " },
                    todo.title
                );
            }
        }
        Commands::Delete { id } => {
            let len_before = todos.len();
            todos.retain(|todo| todo.id != id);
            if todos.len() < len_before {
                save(&todos)?;
                println!("Todo deleted.");
            } else {
                println!("Todo with id {} not found.", id);
            }
        }
        Commands::Update { id, title } => {
            let mut found = false;
            for todo in &mut todos {
                if todo.id == id {
                    todo.title = title.clone();
                    found = true;
                    break;
                }
            }
            if found {
                save(&todos)?;
                println!("Todo updated.");
            } else {
                println!("Todo with id {} not found.", id);
            }
        }
    }

    Ok(())
}

