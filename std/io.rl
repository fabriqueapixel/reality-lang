import std.string;
import std.tuple;
import std.option;

extern fn read_file_ext(path: string) -> string;
extern fn write_file_ext(path: string, content: string) -> int;
extern fn get_current_working_directory() -> string;
extern fn file_exists(path: string) -> bool;

mod File {
    fn open(path: String) -> String {
        let content = read_file_ext(path.data);
    
        return String.init(content);
    }

    fn exists(path: String) -> bool {
        return file_exists(path.data);
    }

    fn write(path: String, content: String) -> unit {
        let result = write_file_ext(path.data, content.data);

        if result != 0 {
            GC.panic(f"Could not write to file: {path}");
        }
        
        return unit;
    }
}

mod System {
    extern fn get_env(var: string) -> string;
    extern fn execute_command(command: string) -> int;

    mod Env {
        fn get(var: String, _ default: String = "") -> String {
            let value = get_env(var.data);
            if value != "".data {
                return String.init(value);
            } else {
                return default;
            }
        }

        fn set(var: String, value: String) -> unit {
            // Dummy implementation; setting environment variables is not supported in this example.
            return unit;
        }

        fn unset(var: String) -> unit {
            // Dummy implementation; unsetting environment variables is not supported in this example.
            return unit;
        }

        fn exists(var: String) -> bool {
            let value = get_env(var.data);
            return value != "".data;
        }
    }

    fn execute(command: String) -> int {
        return execute_command(command.data);
    }

    fn cwd() -> String {
        let path = get_current_working_directory();
        return String.init(path);
    }
}
