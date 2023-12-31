#![feature(let_chains)]

use std::collections::{HashMap, HashSet};

use ir::{write_code, CodeFormatConfig};

struct Args {
    path: String,
    main: String,
    filter: HashSet<String>,
    removed_passes: HashSet<&'static str>,
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

    pub fn has_pass(&self, name: &str) -> bool {
        !self.removed_passes.contains(name)
    }
}

fn print_help_exit() -> ! {
    eprintln!("Usage: dcp <path> [options]");
    eprintln!("Options:");
    eprintln!("       -h, --help                          Display this help message");
    eprintln!("       -f <name>       [default: all]      Decompile only the given function(s)");
    eprintln!("       -m <name>       [default: main]     Use the given function as main");
    eprintln!("Pass disabling (all enabled by default):");
    eprintln!("       --nocheckvoid                       Prevent void return analysis");
    eprintln!("       --nolocalnames                      Prevent transformation of local locations into names");
    eprintln!("       --noifgen                           Prevent if-else generation, stop after MIR translation");
    eprintln!("       --noloopgen                         Prevent loop generation, stop after if generation");
    eprintln!("       --noinline                          Prevent expression inlining");
    std::process::exit(0);
}

const STDLIB: &'static str = include_str!("../../stdlib.csv");

const CHECK_VOID: &'static str = "checkvoid";
const LOCAL_NAMES: &'static str = "localnames";
const IF_GEN: &'static str = "ifgen";
const LOOP_GEN: &'static str = "loopgen";
const INLINE: &'static str = "inline";

fn parse_args() -> Args {
    let mut path = None;
    let mut filter = HashSet::new();
    let mut main = "main".to_string();
    let mut removed_passes = HashSet::new();

    let mut iter = std::env::args().skip(1);
    while let Some(arg) = iter.next() {
        match &arg[..] {
            "-h" | "--help" => print_help_exit(),
            "--nocheckvoid" => { removed_passes.insert(CHECK_VOID); }
            "--nolocalnames" => { removed_passes.insert(LOCAL_NAMES); }
            "--noifgen" => { removed_passes.insert(IF_GEN); }
            "--noloopgen" => { removed_passes.insert(LOOP_GEN); }
            "--noinline" => { removed_passes.insert(INLINE); }
            "-f" => {
                let Some(name) = iter.next() else {
                    eprintln!("Expected function name. Use `dcp -h` for help.");
                    std::process::exit(1);
                };
                filter.insert(name);
            }
            "-m" => {
                let Some(name) = iter.next() else {
                    eprintln!("Expected function name. Use `dcp -h` for help.");
                    std::process::exit(1);
                };
                main = name;
            }
            _ if path.is_none() => {
                path = Some(arg);
            }
            _ => {
                eprintln!("Unknown option `{arg}`. Use `dcp -h` for help.");
                std::process::exit(1);
            }
        }
    }

    if path.is_none() {
        print_help_exit();
    }

    Args {
        path: path.unwrap(),
        filter,
        main,
        removed_passes
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
    obj_locations: HashMap<u64, ir::Name>,
    got: HashMap<u64, ir::Name>,
    entry_func: Option<ir::Name>
}

fn get_bin_symbols(bin: &bin::RawBinary, long_names: &mut HashMap<ir::Name, String>, main_name: &str, global_idx: &mut u32) -> FunctionSet {
    let mut funcs = FunctionSet {
        func_locations: HashMap::new(),
        func_args: HashMap::new(),
        obj_locations: HashMap::new(),
        got: HashMap::new(),
        entry_func: None
    };

    let (stdlib_arities, _stdlib_names, stdlib_rev_names, stdlib_variadic) = parse_stdlib();

    for meta in &bin.meta {
        match meta {
            bin::BinMeta::Region { .. } => {}
            bin::BinMeta::FuncName { location, name } => {
                if *location == 0 {
                    continue
                }

                // if *name == main_name && let Some(entry_func) = funcs.entry_func {
                //     assert!(!long_names.contains_key(&entry_func));
                //     long_names.insert(entry_func, name.clone());
                //     assert_eq!(funcs.func_locations.get(location), Some(&entry_func));
                //     continue
                // }

                let short_name = ir::Name(*global_idx, ir::Namespace::Global);
                // FIXME: If we already have an entry_func as indicated by the Entry bin meta, just overwrite
                //        it with this one. Eventually we hope to be good enough to actually use _start, but
                //        our function detection is having a bad time with it at the moment.
                if *name == main_name {
                    funcs.entry_func = Some(short_name);
                }
                funcs.func_locations.insert(*location, short_name);
                long_names.insert(short_name, name.clone());

                *global_idx += 1;
            }
            bin::BinMeta::PltElement { location, name } => {
                let short_name = match stdlib_rev_names.get(name.as_str()) {
                    Some(name) => {
                        funcs.func_args.insert(*name, (
                            *stdlib_arities.get(name).unwrap(),
                            stdlib_variadic.contains(name)
                        ));
                        *name
                    },
                    None => {
                        eprintln!("Warning: unrecognised PLT/GOT entry: {name}");
                        *global_idx += 1;
                        ir::Name(*global_idx - 1, ir::Namespace::Global)
                    }
                };

                funcs.got.insert(*location, short_name);
                long_names.insert(short_name, name.clone());
            }
            bin::BinMeta::Entry { location } => {
                if let Some(entry_func) = funcs.entry_func {
                    assert_eq!(funcs.func_locations.get(location), Some(&entry_func));
                    continue
                }

                if let Some(func) = funcs.func_locations.get(location) {
                    funcs.entry_func = Some(*func);
                } else {
                    let short_name = ir::Name(*global_idx, ir::Namespace::Global);
                    funcs.entry_func = Some(short_name);
                    funcs.func_locations.insert(*location, short_name);
                    *global_idx += 1;
                    // long_names.insert(short_name, main_name.clone());
                }
            }
            bin::BinMeta::InitArrFunc { location } => {
                funcs.func_locations.entry(*location).or_insert_with(|| {
                    *global_idx += 1;
                    let name = ir::Name(*global_idx - 1, ir::Namespace::Global);
                    long_names.insert(name, format!("initarr@{location:x}"));
                    name
                });
            }
            bin::BinMeta::FiniArrFunc { location } => {
                funcs.func_locations.entry(*location).or_insert_with(|| {
                    *global_idx += 1;
                    let name = ir::Name(*global_idx - 1, ir::Namespace::Global);
                    long_names.insert(name, format!("finiarr@{location:x}"));
                    name
                });
            }
            bin::BinMeta::ObjName { location, name } => {
                let short_name = ir::Name(*global_idx, ir::Namespace::Global);
                *global_idx += 1;

                funcs.obj_locations.insert(*location, short_name);
                long_names.insert(short_name, name.clone());
            }
        }
    }

    funcs
}

fn should_attempt(func_name: &str) -> bool {
    for name in &[
        "deregister_tm_clones", "register_tm_clones",  "frame_dummy",
        "__do_global_dtors_aux", "__libc_csu_init", "__libc_csu_fini", "_dl_relocate_static_pie"
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
        called_read: x86::called_read(),
        fp: x86::frame_ptr_name(),
        func_args: x86::func_args(),
    };

    let bin = elf::read(&args.path).expect("Could not open ELF");

    let all_code = x86::gen_ir(&bin.code, bin.base_addr).expect("Could not make initial translation");

    let mut long_names = HashMap::new();

    let mut df_funcs = HashMap::new();
    let mut global_idx = 0;
    let mut symbols = get_bin_symbols(&bin, &mut long_names, &args.main, &mut global_idx);
    let mut callgraph = symbols.entry_func.map(ir::CallGraph::new);

    let plt_call_map = x86::gen_plt_data(bin.plt.as_ref(), &symbols.got).expect("could not make plt data");
    for (addr, name) in plt_call_map {
        symbols.func_locations.insert(addr, name);
    }

    let raw_funcs = ir::gen_funcs(all_code, &symbols.func_locations, &mut global_idx);

    for func in &raw_funcs {
        symbols.func_locations.insert(func.addr, func.short_name);
        long_names.entry(func.short_name).or_insert_with(|| format!("fun{:x}", func.addr));
    }

    for mut func in raw_funcs {
        if let Some(name) = long_names.get(&func.short_name) && !should_attempt(name) {
            continue
        }

        // println!("{:?}", func.short_name);
        // ir::Instr::dump_block(&func.code);
        // println!();

        assert!(!func.code.is_empty());

        // Generate and apply stack frame
        ir::clean_dead_labels(&mut func.code);
        ir::move_constants_right(&mut func.code);
        ir::reduce_binop_constants(&mut func.code);

        let frame;
        if args.has_pass(LOCAL_NAMES) {
            frame = ir::gen_frame(&func.code, &x86_abi);
            ir::apply_frame_names(&x86_abi, &mut func.code, &frame);
        } else {
            frame = ir::Frame::new();
        }

        ir::tail_call_to_call_return(&mut func.code);

        // Update with names of known entities
        ir::replace_func_names(&mut func.code, &symbols.func_locations);
        ir::replace_obj_names(&mut func.code, &symbols.obj_locations);
        if let Some(rodata) = bin.rodata.as_ref() {
            ir::insert_string_lits(&mut func.code, &rodata.data, rodata.base_addr);
        }

        if let Some(callgraph) = &mut callgraph && args.has_pass(CHECK_VOID) {
            ir::insert_in_callgraph(callgraph, &func);
        }

        let (blocks, mut cfg) = ir::drain_code_to_cfg(&mut func.code);
        cfg.generate_backward_edges();

        func.args = ir::infer_func_args(&x86_abi, &cfg, &blocks);
        symbols.func_args.insert(func.short_name, (func.args.len(), false)); // TODO: Variadics (somehow)

        df_funcs.insert(func.short_name, DataFlowAnalysisFunc {
            func, cfg, blocks, frame
        });
    }

    // Now we know the number of arguments for each function we can update the call-sites,
    // and deal with format strings too
    for DataFlowAnalysisFunc { blocks, func, cfg, .. } in df_funcs.values_mut() {
        ir::insert_args(&x86_abi, cfg, blocks, &func.args, &symbols.func_args);
        ir::inline_constants(&cfg, blocks);
        ir::update_format_string_args(&x86_abi, blocks, &symbols.func_args);

        // for block in blocks {
        //     ir::Instr::dump_block(&block.code);
        //     println!();
        // }
    }

    // We can now safely assume that if something is unused, it's actually unused, so let's
    // figure out which functions return something
    if let Some(callgraph) = &mut callgraph && args.has_pass(CHECK_VOID) {
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

        // Assume the entry function returns something
        sometimes_read.insert(symbols.entry_func.unwrap());

        for name in df_funcs.keys().cloned().collect::<Vec<_>>() {
            if !sometimes_read.contains(&name) {
                ir::demote_to_return_void(&mut df_funcs.get_mut(&name).unwrap().blocks);
            }
        }
    }

    // Now everything is in place we can aggressively optimise this function
    let mut values = df_funcs.into_values().collect::<Vec<_>>();
    values.sort_by_cached_key(|el| el.func.addr);
    for DataFlowAnalysisFunc { mut func, mut blocks, cfg, frame } in values {
        if args.is_filtered(long_names.get(&func.short_name)) {
            continue
        }

        // reduce_cmp can change ref-counts, so need to run again
        for _ in 0..2 {
            // At this stage:
            // - Calls must appear either as Instr::Expr { Expr::Call } or Instr::Store { src: Expr::Call } but nowhere else
            // - The only types of instrs are Store, Expr, Branch, Return
            if args.has_pass(INLINE) {
                ir::inline_single_use_pairs(&x86_abi, &cfg, &mut blocks[..]);
                ir::remove_dead_writes(&x86_abi, &cfg, &mut blocks[..]);
                ir::demote_dead_calls(&x86_abi, &cfg, &mut blocks[..], &mut HashSet::new());
            }

            for block in blocks.iter_mut() {
                ir::reduce_cmp(&mut block.code);
                ir::reduce_ref_deref(&mut block.code);
                ir::reduce_binop_to_identities(&mut block.code);
                ir::reduce_binop_assoc_all(&mut block.code);
                ir::reduce_binop_constants(&mut block.code);
                ir::reduce_binop_identities(&mut block.code);
                ir::clean_self_writes(&mut block.code);
            }
        }

        'end: {
            if !args.has_pass(IF_GEN) {
                for block in blocks.iter_mut() {
                    func.code.extend(block.code.drain(..));
                }

                break 'end
            }

            let code = ir::generate_ifs(&mut blocks, &cfg);
            func.code = code;

            ir::clean_dead_jumps(&mut func.code);
            ir::clean_dead_labels(&mut func.code);
            ir::clean_dead_fallthrough_jumps(&mut func.code);
            ir::clean_dead_labels(&mut func.code);
            ir::clean_else_to_fallthrough(&mut func.code);
            ir::clean_empty_true_then(&mut func.code);

            if !args.has_pass(LOOP_GEN) {
                break 'end
            }

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
            ir::generate_elseif(&mut func.code);
            ir::clean_dead_fallthrough_jumps(&mut func.code);
            ir::clean_dead_labels(&mut func.code);
        }

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