use crossterm::style::Stylize;
use scan_fmt_xml::{
    scan_core::{channel_system, Val},
    ScxmlModel,
};

pub fn print_state(scxml_model: &ScxmlModel, state: Vec<bool>) {
    if !state.is_empty() {
        let mut first = true;
        for (pred, val) in scxml_model.predicates.iter().zip(&state) {
            if first {
                print!("[{pred}: {val:5}");
                first = false;
            } else {
                print!(" | {pred}: {val:5}");
            }
        }
        println!("]");
    }
}

pub fn print_event(scxml_model: &ScxmlModel, event: channel_system::Event) {
    print!(
        "{}",
        scxml_model
            .fsm_names
            .get(&event.pg_id)
            .unwrap()
            .as_str()
            .bold()
    );
    if let Some((src, trg, event_idx, param)) = scxml_model.parameters.get(&event.channel) {
        let event_name = scxml_model.events.get(event_idx).unwrap();
        match event.event_type {
            channel_system::EventType::Send(val) => println!(
                " sends param {}={val:?} of event {} to {}",
                param.as_str().bold().blue(),
                event_name.as_str().bold().red(),
                scxml_model.fsm_names.get(trg).unwrap().as_str().bold()
            ),
            channel_system::EventType::Receive(val) => println!(
                " receives param {}={val:?} of event {} from {}",
                param.as_str().bold().blue(),
                event_name.as_str().bold().red(),
                scxml_model.fsm_names.get(src).unwrap().as_str().bold()
            ),
            _ => unreachable!(),
        }
    } else if let Some(pg_id) = scxml_model.ext_queues.get(&event.channel) {
        match event.event_type {
            channel_system::EventType::Send(val) => {
                if let Val::Tuple(e) = val {
                    if let (Val::Integer(sent_event), Val::Integer(_origin)) = (&e[0], &e[1]) {
                        println!(
                            " sends event {} to {}",
                            scxml_model
                                .events
                                .get(&(*sent_event as usize))
                                .unwrap()
                                .as_str()
                                .bold()
                                .red(),
                            scxml_model.fsm_names.get(pg_id).unwrap().as_str().bold()
                        );
                    } else {
                        panic!("events should be pairs");
                    }
                } else {
                    panic!("events should be pairs");
                }
            }
            channel_system::EventType::Receive(val) => {
                if let Val::Tuple(e) = val {
                    if let (Val::Integer(sent_event), Val::Integer(origin)) = (&e[0], &e[1]) {
                        println!(
                            " receives event {} from {}",
                            scxml_model
                                .events
                                .get(&(*sent_event as usize))
                                .unwrap()
                                .as_str()
                                .bold()
                                .red(),
                            scxml_model
                                .fsm_indexes
                                .get(&(*origin as usize))
                                .unwrap()
                                .as_str()
                                .bold(),
                        );
                    } else {
                        panic!("events should be pairs");
                    }
                } else {
                    panic!("events should be pairs");
                }
            }
            _ => unreachable!(),
        }
    } else if scxml_model.int_queues.contains(&event.channel) {
        match event.event_type {
            channel_system::EventType::Send(val) => {
                println!(" sends event {val:?} to its internal queue")
            }
            channel_system::EventType::Receive(val) => {
                println!(" processes event {val:?} from its internal queue")
            }
            channel_system::EventType::ProbeEmptyQueue => {
                println!("'s internal queue is empty")
            }
            _ => unreachable!(),
        }
    } else {
        match event.event_type {
            channel_system::EventType::Send(val) => {
                println!(" sends {val:?} to {:?}", event.channel)
            }
            channel_system::EventType::Receive(val) => {
                println!(" receives {val:?} from {:?}", event.channel)
            }
            channel_system::EventType::ProbeEmptyQueue => {
                println!(" probes that {:?} is empty", event.channel)
            }
            channel_system::EventType::ProbeFullQueue => {
                println!(" probes that {:?} is full", event.channel)
            }
        }
    }
}
