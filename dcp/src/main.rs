
fn main() {
    let bin = elf::read("samples/add.elf").expect("Could not open ELF");
    let bin = bin.functions_from_meta();

    for func in &bin.funcs {
        let mut func = x86::gen_ir_func(func).expect("Could not make initial translation");
        ir::clean_dead_labels(&mut func.code);
        let (mut blocks, mut cfg) = ir::drain_code_to_cfg(&mut func.code);

        cfg.generate_backward_edges();

        ir::inline_single_use_pairs(&cfg, &mut blocks);
        for block in &mut blocks {
            ir::reduce_cmp(&mut block.code);
        }

        let code = ir::generate_ifs(&mut blocks, &cfg);
        func.code = code;

        ir::clean_dead_jumps(&mut func.code);
        ir::clean_dead_labels(&mut func.code);

        ir::clean_unreachable_elses(&mut func.code);
        ir::loop_gen(&mut func.code);
        ir::loop_jump_to_continue(&mut func.code);
        ir::clean_dead_labels(&mut func.code);
        ir::if_break_negate(&mut func.code);
        ir::final_continue(&mut func.code);
        ir::while_gen(&mut func.code);

        ir::Instr::dump_block(&func.code);
    }
}
