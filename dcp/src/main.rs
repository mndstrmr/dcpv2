use std::collections::HashMap;

use ir::{write_code, CodeFormatConfig};


fn main() {
    let x86_abi = ir::Abi {
        caller_read: x86::caller_read(),
        fp: x86::frame_ptr_name(),
        func_args: x86::func_args()
    };

    let bin = elf::read("samples/prec.elf").expect("Could not open ELF");
    let bin = bin.functions_from_meta();

    let mut funcs = Vec::new();
    let mut func_graphs = Vec::new();

    for (f, func) in bin.funcs.iter().enumerate() {
        let mut func = x86::gen_ir_func(func, ir::Name(100 + f)).expect("Could not make initial translation");
        
        ir::clean_dead_labels(&mut func.code);
        ir::move_constants_right(&mut func.code);
        let frame = ir::gen_frame(&func.code, &x86_abi, 100 + bin.funcs.len());
        ir::apply_frame_names(&x86_abi, &mut func.code, &frame);
      
        let (blocks, mut cfg) = ir::drain_code_to_cfg(&mut func.code);
        cfg.generate_backward_edges();

        func.args = ir::infer_func_args(&x86_abi, &cfg, &blocks);

        funcs.push(func);
        func_graphs.push((blocks, cfg));
    }

    let mut func_map = HashMap::new();
    for func in &funcs {
        func_map.insert(func.addr, func);
    }
    
    for i in 0..funcs.len() {
        ir::insert_args(&mut func_graphs[i].0, &func_map);
        ir::replace_names(&mut func_graphs[i].0, &func_map);
    }

    for (func, (blocks, cfg)) in funcs.iter_mut().zip(func_graphs.iter_mut()) {
        ir::remove_dead_writes(&x86_abi, &cfg, blocks);
        ir::inline_single_use_pairs(&x86_abi, &cfg, blocks);
        for block in blocks.iter_mut() {
            ir::reduce_cmp(&mut block.code);
            ir::reduce_binop_assoc(&mut block.code);
            ir::reduce_binop_identities(&mut block.code);
            ir::clean_self_writes(&mut block.code);
        }

        let code = ir::generate_ifs(blocks, &cfg);
        func.code = code;

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
        
        println!("\nfunc {} (r{}) {:?}:", func.name.as_deref().unwrap_or("??"), func.short_name.0, func.args.iter().map(|x| x.name.0).collect::<Vec<_>>());
        // ir::Instr::dump_block(&func.code);
        // println!("{:?}", frame);
        let mut string = String::new();
        write_code(&mut string, &func.code, &CodeFormatConfig {
            indent: "    ".to_string(),
            indent_lab: false,
            lit_types: false, mem_types: false, return_types: false, write_types: false,
            loc: true,
            loc_gap: 4,
            name_map: HashMap::new()
        }).expect("Could not write");
        println!("{}", string);
    }
}
