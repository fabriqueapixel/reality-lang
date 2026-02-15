import std.string;
import std.parser;
import std.io;
import std.iterator;
import source.parser;
import source.configuration;
import source.cli;
import source.color;

// Commands
import source.commands.build;
import source.commands.help;
import source.commands.init;

fn get_cwd() -> String {
    let cwd_str = get_current_working_directory();
    return {
        data: cwd_str,
        length: strlen(cwd_str)
    };
}

type Command = fn(String, List<CLI>) -> unit;
type Commands = Map<String, Command>;

impl fn (l: List<A>) first<A>(pred: fn(A) -> bool) -> Option<A> {
    let i = 0;
    while i < l.length {
        let value = l[i];
        if pred(value) {
            return Some(value);
        };
        i = i + 1;
    };

    None
}

impl fn (l: List<A>) first_index<A>(pred: fn(A) -> bool) -> Option<(A, int)> {
    let i = 0;
    while i < l.length {
        let value = l[i];
        if pred(value) {
            return Some((value, i));
        };
        i = i + 1;
    };

    None
}

fn run_command(cwd: String, args: List<CLI>, config: List<*Configuration>) -> unit {
    let build_output = build_command(cwd, args, config);

    let exec_args_idx = args.first_index(|cli| 
        cli is PositionalArg(let arg) && arg == "--"
    );
    let exec_args = if exec_args_idx is Some((_, let idx)) {
        args.slice(idx + 1, args.length)
    } else {
        []
    };

    if build_output is Some(let binary_path) {
        LF.log("Running program...");

        let run_exit_code = execute_command(f"{binary_path} {exec_args.map(show).join(" ")}".data);

        if run_exit_code != 0 {
            LF.error("Program exited with code " + (run_exit_code as int).show_prec(0) + ".");
        }
    } else {
        LF.error("Build failed; cannot run program.");
    }

    unit
}

fn fetch_config(cwd: String) -> List<*Configuration> {
    let config = Configuration.parse_file(cwd + "/config.toml");

    let final_config = if config is Ok(let conf) {
        conf
    } else if config is Err(let errMsg) {
        LF.error(errMsg);
        GC.exit(1);
    } else {
        LF.error("Unknown error while parsing configuration.");
        GC.exit(1);
    }

    final_config
}

fn get_commands(cwd: String) -> Commands {
    return [
        ("build", |cwd, args| {
            let config = fetch_config(cwd);
            build_command(cwd, args, config);
            return unit
        }),
        ("help", help_command),
        ("run", |cwd, args| {
            let config = fetch_config(cwd);
            run_command(cwd, args, config);
            return unit
        }),
        ("init", init_command)
    ];
}

pub fn main(args: List<String>) -> int {
    let cli_args = args.slice(1, args.length).parse_as_cli();

    let command = cli_args.get_first_positional();

    let cwd = get_cwd();
    if command is Some(let cmd1) && cmd1 == "init" {
        init_command(cwd, cli_args.slice(1, cli_args.length));
        return 0;
    };

    if command is Some(let cmd) {
        let commands = get_commands(cwd);
        let cmd_fn = commands.get(cmd);

        if cmd_fn is Some(let f) {
            f(cwd, cli_args.slice(1, cli_args.length));

            0
        } else {
            print("Unknown command: " + cmd);
        }
    } else {
        print("No positional argument (command) provided.");
    }

    return 0;
}
