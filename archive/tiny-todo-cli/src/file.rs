use std::{fs::{File, OpenOptions}, io::{BufReader, BufWriter}, path::PathBuf};
use anyhow::Result;
use crate::todo::Todo;
use dirs::home_dir;

fn get_path() -> PathBuf {
    let mut path = home_dir().expect("could not find home directory");
    path.push("Documents/.todo.json");
    path
}

pub fn load() -> Result<Vec<Todo>> {
    let path = get_path();

    if !path.exists() {
        return Ok(Vec::new());
    }

    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let todos = serde_json::from_reader(reader)?;
    Ok(todos)
}

pub fn save(todos: &[Todo]) -> Result<()> {
    let path = get_path();
    let file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)?;

    let writer = BufWriter::new(file);
    serde_json::to_writer_pretty(writer, &todos)?;
    Ok(())
}

