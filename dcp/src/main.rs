use std::collections::{HashMap, HashSet};

use ir::{write_code, CodeFormatConfig};

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

    'outer: for (f, func) in bin.funcs.iter().enumerate() {
        for name in &[
            "_start", "deregister_tm_clones", "register_tm_clones",
            "frame_dummy", "__do_global_dtors_aux", "__libc_csu_init",
            "__libc_csu_fini"
        ] {
            if func.name.as_deref() == Some(name) {
                continue 'outer
            }
        }

        let mut func = x86::gen_ir_func(func, ir::Name(100 + f)).expect("Could not make initial translation");

        ir::clean_dead_labels(&mut func.code);
        ir::move_constants_right(&mut func.code);
        let frame = ir::gen_frame(&func.code, &x86_abi, 100 + bin.funcs.len());
        ir::apply_frame_names(&x86_abi, &mut func.code, &frame);
      
        let (blocks, mut cfg) = ir::drain_code_to_cfg(&mut func.code);
        cfg.generate_backward_edges();

        func.args = ir::infer_func_args(&x86_abi, &cfg, &blocks);

        funcs.push(func);
        func_graphs.push((blocks, cfg, frame));
    }

    let mut func_map = HashMap::new();
    let mut func_name_map = HashMap::new();
    for func in &funcs {
        func_map.insert(func.addr, func);

        if let Some(name) = &func.name {
            func_name_map.insert(func.short_name, name.clone());
        }
    }
    
    for i in 0..funcs.len() {
        ir::insert_args(&mut func_graphs[i].0, &func_map);
        ir::replace_names(&mut func_graphs[i].0, &func_map);
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
        ir::loop_gen(&mut func.code);
        ir::loop_jump_to_continue(&mut func.code);
        ir::loop_jump_to_break(&mut func.code);
        ir::clean_dead_labels(&mut func.code);
        ir::if_break_negate(&mut func.code);
        ir::final_continue(&mut func.code);
        ir::while_gen(&mut func.code);
        ir::for_gen(&mut func.code);
        ir::for_init_search(&mut func.code);
        
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
