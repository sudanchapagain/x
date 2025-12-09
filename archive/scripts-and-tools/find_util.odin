package findutil

import "core:os"
import "core:fmt"
import "core:strings"

FindConfig :: struct {
    directory: string,
    name_filter: string,
    print_only: bool,
}

main :: proc() {
    if len(os.args) < 2 {
        fmt.println("usage: find <directory> [options]")
        return
    }

    dir := os.args[1]
    options := os.args[2:]

    if !os.is_dir(dir) {
        fmt.println("Error: Specified path is not a directory.")
        return
    }

    find_config := FindConfig{
        directory = dir,
        name_filter = "",
        print_only = true,
    }

    for option in options {
        if strings.has_prefix(option, "--name=") {
            find_config.name_filter = strings.trim_prefix(option, "--name=")
        } else if option == "--print" {
            find_config.print_only = true
        } else {
            fmt.println("unknown option:", option)
            return
        }
    }

    err := find_files(find_config)
    if err != nil {
        fmt.println("Error:", err)
    }
}

find_files :: proc(config: FindConfig) -> os.Error {
    dir_handle := os.open(config.directory) or_return
    defer os.close(dir_handle)

    dirents := os.read_dir(dir_handle, -1) or_return

    for dirent in dirents {
        if config.name_filter != "" && dirent.name != config.name_filter {
            continue
        }

        if config.print_only {
            file_path := strings.join({config.directory, dirent.name}, "/") or_return
            fmt.println(file_path)
        }
    }

    return nil
}
