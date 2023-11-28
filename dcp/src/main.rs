#![feature(let_chains)]

use std::collections::{HashMap, HashSet};

use ir::{write_code, CodeFormatConfig};

struct Args {
    path: String,
    filter: HashSet<String>
}

impl Args {
    pub fn is_filtered(&self, name: Option<&String>) -> bool {
        if self.filter.is_empty() {
            return false
        }

        let Some(name) = name else {
            return true
        };

        !self.filter.contains(name)
    }
}

fn print_help_exit() -> ! {
    eprintln!("Usage: dcp <path> [options]");
    eprintln!("Options:");
    eprintln!("       -h, --help    Display this help message");
    eprintln!("       -f <name>     Decompile only the given function(s)");
    std::process::exit(0);
}

const STDLIB: &'static str = include_str!("../../stdlib.csv");

fn parse_args() -> Args {
    let mut path = None;
    let mut filter = HashSet::new();

    let mut iter = std::env::args().skip(1);
    while let Some(arg) = iter.next() {
        if arg.starts_with("--") {
            match &arg[2..] {
                "help" => print_help_exit(),
                _ => {
                    eprintln!("Unknown option `{arg}`. Use `dcp -h` for help.");
                    std::process::exit(1);
                }
            }
        } else if arg.starts_with('-') {
            match &arg[1..] {
                "h" => print_help_exit(),
                "f" => {
                    let Some(name) = iter.next() else {
                        eprintln!("Expected function name. Use `dcp -h` for help.");
                        std::process::exit(1);
                    };
                    filter.insert(name);
                }
                _ => {
                    eprintln!("Unknown option `{arg}`. Use `dcp -h` for help.");
                    std::process::exit(1);
                }
            }
        } else if path.is_none() {
            path = Some(arg);
        }
    }

    if path.is_none() {
        print_help_exit();
    }

    Args {
        path: path.unwrap(),
        filter
    }
}

fn parse_stdlib() -> (HashMap<ir::Name, usize>, HashMap<ir::Name, &'static str>, HashMap<&'static str, ir::Name>, HashSet<ir::Name>) {
    let mut counts = HashMap::new();
    let mut names = HashMap::new();
    let mut rev_names = HashMap::new();
    let mut variadics = HashSet::new();
    let mut external_idx = 0;
    for line in STDLIB.lines() {
        let mut split = line.split(",");
        let str_name = split.next().unwrap();
        let count: usize = split.next().unwrap().parse().unwrap();
        let variadic = split.next().unwrap() == "T";

        let name = ir::Name(external_idx, ir::Namespace::External);
        counts.insert(name, count);
        names.insert(name, str_name);
        rev_names.insert(str_name, name);
        if variadic {
            variadics.insert(name);
        }

        external_idx += 1;
    }
    (counts, names, rev_names, variadics)
}

struct FunctionSet {
    func_locations: HashMap<u64, ir::Name>,
    func_args: HashMap<ir::Name, (usize, bool)>,
    got: HashMap<u64, ir::Name>,
    entry_func: Option<ir::Name>
}

impl FunctionSet {
    pub fn name_of_location(&self, location: u64) -> ir::Name {
        *self.func_locations.get(&location).expect("Don't have an ir name for this location")
    }
}

fn get_bin_symbols(bin: &bin::FuncsBinary, long_names: &mut HashMap<ir::Name, String>) -> FunctionSet {
    let mut funcs = FunctionSet {
        func_locations: HashMap::new(),
        func_args: HashMap::new(),
        got: HashMap::new(),
        entry_func: None
    };

    let (stdlib_arities, stdlib_names, stdlib_rev_names, stdlib_variadic) = parse_stdlib();

    let mut global_idx = 0;

    for meta in &bin.meta {
        if let bin::BinMeta::Name { location, name } = meta {
            if *location == 0 {
                continue
            }

            let short_name = ir::Name(global_idx, ir::Namespace::Global);
            if *name == "main" {
                funcs.entry_func = Some(short_name);
            }
            funcs.func_locations.insert(*location, short_name);
            long_names.insert(short_name, name.clone());

            global_idx += 1;
        } else if let bin::BinMeta::PltElement { location, name } = meta {
            if let Some(name) = stdlib_rev_names.get(name.as_str()) {
                funcs.got.insert(*location, *name);
                funcs.func_args.insert(*name, (
                    *stdlib_arities.get(name).unwrap(),
                    stdlib_variadic.contains(name)
                ));
                long_names.insert(*name, stdlib_names.get(name).unwrap().to_string());
            } else {
                todo!("Unknown PLT entries")
            }
        }
    }

    funcs
}

fn should_attempt(func_name: &str) -> bool {
    for name in &[
        "_start", "deregister_tm_clones", "register_tm_clones",
        "frame_dummy", "__do_global_dtors_aux", "__libc_csu_init",
        "__libc_csu_fini"
    ] {
        if func_name == *name {
            return false
        }
    }

    true
}

struct DataFlowAnalysisFunc {
    func: ir::Func,
    cfg: ir::Cfg,
    blocks: Vec<ir::CfgBlock>,
    frame: ir::Frame
}

fn main() {
    let args = parse_args();

    let x86_abi = ir::Abi {
        caller_read: x86::caller_read(),
        fp: x86::frame_ptr_name(),
        func_args: x86::func_args()
    };

    let bin = elf::read(&args.path).expect("Could not open ELF");
    let bin = bin.functions_from_meta();

    let mut long_names = HashMap::new();

    let mut df_funcs = HashMap::new();

    let mut symbols = get_bin_symbols(&bin, &mut long_names);
    let mut callgraph = symbols.entry_func.map(ir::CallGraph::new);

    let plt_call_map = x86::gen_plt_data(bin.plt.as_ref(), &symbols.got).expect("could not make plt data");
    for (addr, name) in plt_call_map {
        symbols.func_locations.insert(addr, name);
    }

    for func in bin.funcs.iter() {
        if let Some(name) = func.name.as_deref() && !should_attempt(name) {
            continue
        }

        let short_name = symbols.name_of_location(func.addr);
        let mut func = x86::gen_ir_func(func, short_name).expect("Could not make initial translation");

        // Generate and apply stack frame
        ir::clean_dead_labels(&mut func.code);
        ir::move_constants_right(&mut func.code);
        ir::reduce_binop_constants(&mut func.code);
        let frame = ir::gen_frame(&func.code, &x86_abi);
        ir::apply_frame_names(&x86_abi, &mut func.code, &frame);

        // Update with names of known entities
        ir::replace_names(&mut func.code, &symbols.func_locations);
        if let Some(rodata) = bin.rodata.as_ref() {
            ir::insert_string_lits(&mut func.code, &rodata.data, rodata.base_addr);
        }

        if let Some(callgraph) = &mut callgraph {
            ir::insert_in_callgraph(callgraph, &func);
        }

        let (blocks, mut cfg) = ir::drain_code_to_cfg(&mut func.code);
        cfg.generate_backward_edges();

        func.args = ir::infer_func_args(&x86_abi, &cfg, &blocks);
        symbols.func_args.insert(short_name, (func.args.len(), false)); // TODO: Allow variadics

        df_funcs.insert(short_name, DataFlowAnalysisFunc {
            func, cfg, blocks, frame
        });
    }

    // Now we know the number of arguments for each function we can update the call-sites,
    // and deal with format strings too
    for DataFlowAnalysisFunc { func, blocks, .. } in df_funcs.values_mut() {
        if args.is_filtered(long_names.get(&func.short_name)) {
            continue
        }

        ir::insert_args(&x86_abi, blocks, &symbols.func_args);
        ir::no_remove_inline_strings(blocks);
        ir::update_format_string_args(&x86_abi, blocks, &symbols.func_args);
    }

    // We can now safely assume that if something is unused, it's actually unused, so let's
    // figure out which functions return something
    if let Some(callgraph) = &mut callgraph {
        callgraph.trim_unreachable();
        callgraph.generate_backward_edges();
        let sorted = callgraph.topological_sort();

        let mut sometimes_read = HashSet::new();
        for name in &sorted {
            let Some(func) = df_funcs.get_mut(name) else {
                continue
            };
            ir::demote_dead_calls(&x86_abi, &func.cfg, &mut func.blocks[..], &mut sometimes_read);
        }

        for name in df_funcs.keys().cloned().collect::<Vec<_>>() {
            if !sometimes_read.contains(&name) {
                ir::demote_to_return_void(&mut df_funcs.get_mut(&name).unwrap().blocks);
            }
        }
    }

    // Now everything is in place we can aggressively optimise this function
    for DataFlowAnalysisFunc { mut func, mut blocks, cfg, frame } in df_funcs.into_values() {
        if args.is_filtered(long_names.get(&func.short_name)) {
            continue
        }

        ir::inline_single_use_pairs(&x86_abi, &cfg, &mut blocks[..]);
        ir::remove_dead_writes(&x86_abi, &cfg, &mut blocks[..]);
        ir::demote_dead_calls(&x86_abi, &cfg, &mut blocks[..], &mut HashSet::new());

        for block in blocks.iter_mut() {
            // ir::Instr::dump_block(&block.code);
            ir::reduce_cmp(&mut block.code);
            ir::reduce_binop_assoc(&mut block.code);
            ir::reduce_binop_constants(&mut block.code);
            ir::reduce_binop_identities(&mut block.code);
            ir::clean_self_writes(&mut block.code);
        }

        // for block in blocks.iter() {
        //     println!("> {}", block.label);
        //     ir::Instr::dump_block(&block.code);
        // }

        let code = ir::generate_ifs(&mut blocks, &cfg);
        func.code = code;

        // ir::Instr::dump_block(&func.code);

        ir::clean_dead_jumps(&mut func.code);
        ir::clean_dead_labels(&mut func.code);
        ir::clean_unreachable_elses(&mut func.code);
        ir::clean_empty_true_then(&mut func.code);
        ir::loop_gen(&mut func.code);
        ir::loop_jump_to_continue(&mut func.code);
        ir::loop_jump_to_break(&mut func.code);
        ir::clean_dead_labels(&mut func.code);
        ir::if_break_negate(&mut func.code);
        ir::final_continue(&mut func.code);
        ir::while_gen(&mut func.code);
        ir::for_gen(&mut func.code);
        ir::for_init_search(&mut func.code);
        ir::clean_unreachable(&mut func.code);
        ir::generate_else_if(&mut func.code);
        ir::clean_dead_fallthrough_jumps(&mut func.code);
        ir::clean_dead_labels(&mut func.code);

        pretty_print(func.short_name, &func, &long_names, &frame)
    }
}

fn pretty_print(name: ir::Name, func: &ir::Func, func_long_names: &HashMap<ir::Name, String>, frame: &ir::Frame) {
    let mut name_map = func_long_names.clone();
    name_map.extend(x86::name_map());
    for local in frame.locals() {
        if local.offset < 0 {
            name_map.insert(local.name, format!("local{:x}", -local.offset));
        }
    }

    if let Some(name) = &func_long_names.get(&name) {
        print!("func {name}(");
    } else {
        print!("func @{:x}(", func.addr);
    }
    for (a, arg) in func.args.iter().enumerate() {
        if a != 0 {
            print!(", ");
        }
        if let Some(name) = name_map.get(&arg.name) {
            print!("{name}: ");
        } else {
            print!("r{}: ", arg.name.0);
        }
        print!("{}", arg.typ);
    }
    println!(")");

    let mut string = String::new();
    write_code(&mut string, &func.code, &CodeFormatConfig {
        name_map,
        ..Default::default()
    }).expect("Could not write");
    println!("{}", string);
}