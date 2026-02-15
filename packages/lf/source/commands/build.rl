import core.source.configuration;
import core.source.cli;
import core.source.color;
import core.source.parser;
import core.source.commands;
import std.map;

enum Mode {
    Dev,
    Release
}

enum Dependency {
    Dependency(
        Map<String, String>, // aliases
        List<String>,        // libraries
        List<String>         // headers
    )
}

impl fn (m: Mode) show_prec(prec: int) -> String {
    if m is Dev {
        return "Development Mode";
    } else if m is Release {
        return "Release Mode";
    } else {
        return "Unknown Mode";
    }
}

impl fn (d: Dependency) show_prec(prec: int) -> String {
    if d is Dependency(let aliases, let libraries, let headers) {
        let result = "Dependency:\n";

        result = result + "  Aliases:\n";
        let alias_keys = aliases.keys();
        let i = 0;
        while i < alias_keys.length {
            let key = alias_keys[i];
            let value = aliases.get(key).get_or_else("");
            result = result + "    " + key + " => " + value + "\n";
            i = i + 1;
        };

        result = result + "  Libraries:\n";
        let j = 0;
        while j < libraries.length {
            result = result + "    " + libraries[j] + "\n";
            j = j + 1;
        };

        result = result + "  Headers:\n";
        let k = 0;
        while k < headers.length {
            result = result + "    " + headers[k] + "\n";
            k = k + 1;
        };

        return result;
    } else {
        return "Unknown Dependency";
    }
}

fn get_section_as_map(configs: List<*Configuration>) -> Map<String, String> {
    let result = &Map.empty<String, String>();

    let i = 0;
    while i < configs.length {
        let config = *configs[i];
        if config is KeyValue(let key, Str(let value)) {
            result.set(key, value);
        };  

        i = i + 1;
    };

    return *result;
}

fn add_cwd(path: String, cwd: String) -> String {
    if path.starts_with("$") || path.starts_with("<") {
        return path
    } else {
        return cwd + "/" + path;
    }
}

fn get_packages(configs: List<*Configuration>) -> Map<String, String> {
    let packages_section = configs
        .get_section(["features", "dependencies"])
        .get_or_else(Section([], []));
    
    if packages_section is Section(_, let package_configs) {
        return get_section_as_map(package_configs);
    } else {
        return Map.empty<String, String>();
    }
}

fn parse_packages(config: Map<String, String>, cwd: String) -> Map<String, Dependency> {
    let result = &Map.empty<String, Dependency>();
    
    let packages = config.map(
        |(let key, let value)| {
            // Computing packages inside the mapped configuration
            let package_config = Configuration.parse_file(value + "/config.toml");

            let new_cwd = cwd + "/" + value;

            if package_config is Ok(let pc) {
                let Section(_, let aliases) = pc
                    .get_section(["features", "aliases"])
                    .get_or_else(Section([], []));
                
                let Section(_, let lib_c) = pc
                    .get_section(["features", "libc"])
                    .get_or_else(Section([], []));

                let Arr(let libraries) = lib_c
                    .get_key_value("libraries")
                    .get_or_else(Arr([]));

                let Arr(let headers) = lib_c
                    .get_key_value("headers")
                    .get_or_else(Arr([]));

                let Section(_, let package_information) = pc
                    .get_section(["package"])
                    .get_or_else(Section([], []));
                
                let Str(let main) = package_information
                    .get_key_value("main")
                    .get_or_else(Str("source/main.rl"));

                let dependency_item = Dependency(
                    get_section_as_map(aliases).extend([
                        (key, new_cwd + "/" + main) 
                    ]),
                    libraries.map(|lib| add_cwd(lib, new_cwd)),
                    headers.map(|header| add_cwd(header, new_cwd))
                )

                result.set(key, dependency_item);
                let mapping = get_section_as_map(pc);
                let packages = parse_packages(mapping, new_cwd);

                result.extend_mut(packages);
            } else {
                LF.error("Could not parse package configuration for " + key);
                GC.exit(1);
            }
        }
    );

    return *result;
}

fn merge_dependencies(d: Map<String, Dependency>) -> Dependency {
    let merged_aliases = &Map.empty<String, String>();
    let merged_libraries = &List.init<String>();
    let merged_headers = &List.init<String>();

    let i = 0;
    while i < d.length {
        if d[i].snd() is Dependency(let aliases, let libraries, let headers) {
            merged_aliases.extend_mut(aliases);

            merged_libraries.extend_mut(libraries);

            merged_headers.extend_mut(headers);
        };

        i = i + 1;
    };

    return Dependency(*merged_aliases, *merged_libraries, *merged_headers);
}

fn build_path_aliases(m: Map<String, String>) -> String {
    let result = "";

    let i = 0;
    while i < m.length {
        let key = (m.keys())[i];
        let value = m.get(key).get_or_else("");
        result = result + f"-p {key}={value} ";
        i = i + 1;
    };

    return result;
}

fn parse_main_package(dep: Dependency, config: List<*Configuration>) -> Dependency {
    let Dependency(let aliases, let libraries, let headers) = dep;

    let Section(_, let package_aliases) = config
        .get_section(["features", "aliases"])
        .get_or_else(Section([], []));
    let main_aliases = get_section_as_map(package_aliases);

    let Section(_, let package_libc) = config
        .get_section(["features", "libc"])
        .get_or_else(Section([], []));
    let Arr(let main_libraries) = package_libc
        .get_key_value("libraries")
        .get_or_else(Arr([]));
    
    let Arr(let main_headers) = package_libc
        .get_key_value("headers")
        .get_or_else(Arr([]));  

    let merged_aliases = &Map.empty<String, String>();
    merged_aliases.extend_mut(aliases);
    merged_aliases.extend_mut(main_aliases);

    let merged_libraries = &List.init<String>();
    merged_libraries.extend_mut(libraries);
    merged_libraries.extend_mut(main_libraries);

    let merged_headers = &List.init<String>();
    merged_headers.extend_mut(headers);
    merged_headers.extend_mut(main_headers);    

    return Dependency(*merged_aliases, *merged_libraries, *merged_headers);
}

fn build_command(cwd: String, args: List<CLI>, config: List<*Configuration>) -> Option<String> {
    let mode = if args.has_flag("dev") { Dev } else { Release };

    let Section(_, let package_information) = config
        .get_section(["package"])
        .get_or_else(Section([], []));

    let Str(let main_file) = package_information
        .get_key_value("main")
        .get_or_else(Str("source/main.rl"));
    
    let packages_map = get_packages(config);
    let all_packages = parse_packages(packages_map, cwd);

    let Dependency(let aliases, let libraries, let headers) = all_packages
        .merge_dependencies()
        .parse_main_package(config);

    LF.log("Building project in " + mode.show_prec(0) + "...");

    // Creating RLC command
    let headers_str = if headers.length > 0 {
        "-I " + headers.map(|h| if h.starts_with("<") {
            h.show_prec(1)
        } else {
            h
        }).join(" -I ")
    } else {
        ""
    }
    let rlc_command = f"rlc {main_file} {build_path_aliases(aliases)} -I \"<stdio.h>\" -I \"<string.h>\" {headers_str}";

    LF.log("Executing command: " + color(Black) + rlc_command + reset_code());

    let rlc_exit_code = System.execute(rlc_command);

    if rlc_exit_code != 0 {
        LF.error("Build failed with exit code " + (rlc_exit_code as int).show_prec(0) + ".");
        GC.exit(1);
    }

    let main_file_split = main_file.split('.');
    let c_file = main_file_split.slice(0, main_file_split.length - 1).join(".") + ".c";

    let c_file_location = cwd + "/" + c_file;
    let base_gcc_command = f"gcc {c_file_location} {libraries.join(" ")} $REALITY_DIR/libc/String.c $REALITY_DIR/libc/Actor.c $LIBGC/lib/libgc.a -I$LIBGC/include/ -lm -w -o {cwd}/output/program";

    let gcc_command = if mode is Dev {
        base_gcc_command + " -DDEV_MODE -g"
    } else {
        base_gcc_command + " -O3 -DRELEASE_MODE"
    };

    LF.log("Compiling with command: " + color(Black) + gcc_command + reset_code());

    if not(File.exists(cwd + "/output")) {
        System.execute("mkdir " + cwd + "/output");
    }

    let gcc_exit_code = System.execute(gcc_command);

    if gcc_exit_code != 0 {
        LF.error("GCC compilation failed with exit code " + (gcc_exit_code as int).show_prec(0) + ".");

        GC.exit(1);
    }

    LF.success("Build succeeded! Binary located at " + color(Black) + cwd + "/output/program" + reset_code());

    return Some(cwd + "/output/program");
}
