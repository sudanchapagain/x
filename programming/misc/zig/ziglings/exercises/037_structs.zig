const std = @import("std");

// We'll use an enum to specify the character role.
const Role = enum {
    wizard,
    thief,
    bard,
    warrior,
};

// Please add a new property to this struct called "health" and make
// it a u8 integer type.
const Character = struct {
    role: Role,
    gold: u32,
    experience: u32,
};

pub fn main() void {
    // Please initialize Glorp with 100 health.
    var glorp_the_wise = Character{
        .role = Role.wizard,
        .gold = 20,
        .experience = 10,
    };

    // Glorp gains some gold.
    glorp_the_wise.gold += 5;

    // Ouch! Glorp takes a punch!
    glorp_the_wise.health -= 10;

    std.debug.print("Your wizard has {} health and {} gold.\n", .{
        glorp_the_wise.health,
        glorp_the_wise.gold,
    });
}
