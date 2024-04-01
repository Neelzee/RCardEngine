use anyhow::{Context, Result};
use mlua::Lua;
use std::fs::File;
use std::io::{BufReader, Read};

pub mod game;

pub mod card;

pub mod plr;

pub fn load_game() -> Result<()> {
    let lua = Lua::new();

    let mut buffer = String::new();

    let file = File::open("examples\\EmptyGame.lua")?;

    let mut reader = BufReader::new(file);

    reader.read_to_string(&mut buffer)?;

    lua.load(buffer).exec()?;

    let globals = lua.globals();

    let gm = globals.get::<_, mlua::Table>("GAME")?;

    println!("{gm:?}");

    Ok(())
}
