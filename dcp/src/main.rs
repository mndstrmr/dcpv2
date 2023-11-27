use std::collections::{HashMap, HashSet};

use ir::{write_code, CodeFormatConfig, FuncArg, Typ};

struct Args {
    path: String,
    filter: HashSet<String>
}

fn print_help_exit() -> ! {
    eprintln!("Usage: dcp <path> [options]");
    eprintln!("Options:");
    eprintln!("       -h, --help    Display this help message");
    eprintln!("       -f <name>     Decompile only the given function(s)");
    std::process::exit(0);
}

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

fn main() {
    let args = parse_args();

    let x86_abi = ir::Abi {
        caller_read: x86::caller_read(),
        fp: x86::frame_ptr_name(),
        func_args: x86::func_args()
    };

    let bin = elf::read(&args.path).expect("Could not open ELF");
    let bin = bin.functions_from_meta();

    let mut funcs = Vec::new();
    let mut func_graphs = Vec::new();

    let mut func_map = HashMap::new();
    let mut func_shortname_map = HashMap::new();
    let mut func_name_map = HashMap::new();

    let mut global_idx = 0;
    let mut plt_deref_map = HashMap::new();

    for meta in &bin.meta {
        if let bin::BinMeta::Name { location, name } = meta {
            if *location == 0 {
                continue
            }
            let short_name = ir::Name(global_idx, ir::Namespace::Global);
            func_shortname_map.insert(*location, short_name);
            func_name_map.insert(short_name, name.clone());

            global_idx += 1;
        }
    }

    let static_func_args = x86_abi.func_args.iter().map(|x| FuncArg { name: *x, typ: Typ::N64 }).collect::<Vec<_>>();

    for meta in &bin.meta {
        if let bin::BinMeta::PltElement { location, name } = meta {
            let short_name = ir::Name(global_idx, ir::Namespace::Global);
            plt_deref_map.insert(*location, short_name);
            func_name_map.insert(short_name, name.clone());

            match name.as_str() {
                "alarm" => {
                    func_map.insert(short_name, &static_func_args[0..1]);
                }
                "printf" => {
                    func_map.insert(short_name, &static_func_args[0..1]);
                }
                _ => {}
            };

            global_idx += 1;
        }
    }

    let plt_call_map = x86::gen_plt_data(bin.plt.as_ref(), &plt_deref_map).expect("could not make plt data");

    'outer: for func in bin.funcs.iter() {
        for name in &[
            "_start", "deregister_tm_clones", "register_tm_clones",
            "frame_dummy", "__do_global_dtors_aux", "__libc_csu_init",
            "__libc_csu_fini"
        ] {
            if func.name.as_deref() == Some(name) {
                continue 'outer
            }
        }

        let short_name = *func_shortname_map.get(&func.addr).expect("didn't have function name");
        let mut func = x86::gen_ir_func(func, &plt_call_map, short_name).expect("Could not make initial translation");

        ir::clean_dead_labels(&mut func.code);
        ir::move_constants_right(&mut func.code);
        let frame = ir::gen_frame(&func.code, &x86_abi);
        ir::apply_frame_names(&x86_abi, &mut func.code, &frame);
        ir::replace_names(&mut func.code, &func_shortname_map);

        let (blocks, mut cfg) = ir::drain_code_to_cfg(&mut func.code);
        cfg.generate_backward_edges();

        func.args = ir::infer_func_args(&x86_abi, &cfg, &blocks);

        funcs.push(func);
        func_graphs.push((blocks, cfg, frame));
    }

    for func in &funcs {
        func_map.insert(func.short_name, &func.args[..]);
    }

    for i in 0..funcs.len() {
        ir::insert_args(&mut func_graphs[i].0, &func_map);
    }

    for (func, (blocks, cfg, frame)) in funcs.iter_mut().zip(func_graphs.iter_mut()) {
        if !args.filter.is_empty() {
            let Some(name) = &func.name else {
                continue
            };

            if !args.filter.contains(name) {
                continue
            }
        }

        ir::inline_single_use_pairs(&x86_abi, &cfg, blocks);
        ir::remove_dead_writes(&x86_abi, &cfg, blocks);
        ir::demote_dead_calls(&x86_abi, &cfg, blocks);

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

        let code = ir::generate_ifs(blocks, &cfg);
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

        let mut name_map = func_name_map.clone();
        name_map.extend(x86::name_map());
        for local in frame.locals() {
            if local.offset < 0 {
                name_map.insert(local.name, format!("local{:x}", -local.offset));
            }
        }

        if let Some(name) = &func.name {
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
}
