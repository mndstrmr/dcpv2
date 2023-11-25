use std::collections::{VecDeque, HashMap};

use crate::{CfgBlock, Instr, Cfg, Label};

pub fn drain_code_to_cfg(code: &mut Vec<Instr>) -> (Vec<CfgBlock>, Cfg) {
    let mut blocks = Vec::new();

    let mut curr_block: Vec<Instr> = Vec::new();
    let mut node_id = 0;

    let mut dq: VecDeque<Instr> = code.drain(..).collect();
    let mut label_map = HashMap::new();

    while let Some(instr) = dq.pop_front() {
        match instr {
            instr @ Instr::Store { .. } => curr_block.push(instr),
            instr @ Instr::Return { .. } => {
                curr_block.push(instr);
                blocks.push(CfgBlock {
                    loc: curr_block.first().unwrap().loc(),
                    label: node_id,
                    code: curr_block
                });

                curr_block = Vec::new();
                node_id += 1;
            }
            Instr::Label { label, .. } => {
                if curr_block.is_empty() {
                    label_map.insert(label, node_id);
                    continue;
                }
                label_map.insert(label, node_id + 1);

                blocks.push(CfgBlock {
                    loc: curr_block.first().unwrap().loc(),
                    label: node_id,
                    code: curr_block
                });
                curr_block = Vec::new();
                node_id += 1;
            }
            instr @ Instr::Branch { .. } => {
                curr_block.push(instr);
                blocks.push(CfgBlock {
                    loc: curr_block.first().unwrap().loc(),
                    label: node_id,
                    code: curr_block
                });
                curr_block = Vec::new();
                node_id += 1;
            }
            Instr::If { .. } | Instr::Loop { .. } | Instr::Break(_) |
            Instr::Continue(_) | Instr::While { .. } => panic!("if or loop stmt in cfg gen"),
        }
    }

    let mut cfg = Cfg::new(0);

    for b in 0..blocks.len() {
        cfg.add_node(b);
    }

    for (b, block) in blocks.iter_mut().enumerate() {
        match block.code.last_mut().unwrap() {
            Instr::Branch { label, cond, .. } => {
                let dst = *label_map.get(&label).expect("Bad jump");
                *label = Label(dst);
                cfg.add_edge(b, dst);

                if cond.is_none() {
                    continue;
                }
            }
            Instr::Return { .. } => continue,
            _ => {}
        }

        cfg.add_edge(b, b + 1);
    }

    (blocks, cfg)
}
