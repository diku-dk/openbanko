use std::env;
use std::fs::{File, OpenOptions};
use std::io;
use std::io::Error;
use std::io::prelude::*;

extern crate rustbox;

use rustbox::{Color, RustBox};
use rustbox::Key;

static BANKOCOLS: usize = 9;
static BANKOROWS: usize = 3;

#[derive(Clone, Copy, PartialEq)]
enum Mode {
	Normal,
	Moving,
}

struct Video {
	rustbox: RustBox,
	mode: Mode,
	bigskip: usize,
}

impl Video {
	fn init(bigskip: usize) -> Video {
		let rustbox = match RustBox::init(Default::default()) {
			Ok(val) => val,
			Err(e) => panic!("{}", e),
		};
		
		let v = Video {
			rustbox: rustbox,
			mode: Mode::Normal,
			bigskip: bigskip,
		};
		
		v.draw_top();
		
		v
	}
		
	fn draw_top(&self) {
		let text = "L: Luk  G: Gem  F: Flyttetilstand";
		let mid = self.rustbox.width()/2;
		let x = mid - text.len()/2;
		
		self.rustbox.print(x, 2, rustbox::RB_BOLD, Color::Default, Color::Default, "L");
		self.rustbox.print(x+1, 2, rustbox::RB_NORMAL, Color::Default, Color::Default, ": Luk");
		self.rustbox.print(x+8, 2, rustbox::RB_BOLD, Color::Default, Color::Default, "G");
		self.rustbox.print(x+9, 2, rustbox::RB_NORMAL, Color::Default, Color::Default, ": Gem");
		self.rustbox.print(x+16, 2, rustbox::RB_BOLD, Color::Default, Color::Default, "F");
		self.rustbox.print(x+17, 2, rustbox::RB_NORMAL, Color::Default, Color::Default, ": Flyttetilstand");
		
		let text2 = "Venstre/Hojre";
		let x2 = mid - text2.len();
		
		self.rustbox.print(x2, 3, rustbox::RB_BOLD, Color::Default, Color::Default, "Venstre");
		self.rustbox.print(x2+7, 3, rustbox::RB_NORMAL, Color::Default, Color::Default, "/");
		self.rustbox.print(x2+8, 3, rustbox::RB_BOLD, Color::Default, Color::Default, "Højre");
		match self.mode {
			Mode::Normal => self.rustbox.print(x2+13, 3, rustbox::RB_NORMAL, Color::Default, Color::Default, ": Skift"),
			Mode::Moving => self.rustbox.print(x2+13, 3, rustbox::RB_NORMAL, Color::Default, Color::Default, ": Flyt "),
		}
		
		let text3 = "Op/Ned";
		let x3 = mid - text3.len();
		
		self.rustbox.print(x3, 4, rustbox::RB_BOLD, Color::Default, Color::Default, "Op");
		self.rustbox.print(x3+2, 4, rustbox::RB_NORMAL, Color::Default, Color::Default, "/");
		self.rustbox.print(x3+3, 4, rustbox::RB_BOLD, Color::Default, Color::Default, "Ned");
		match self.mode {
			Mode::Normal => self.rustbox.print(x3+6, 4, rustbox::RB_NORMAL, Color::Default, Color::Default, &format!(": Skift {} felter", self.bigskip)),
			Mode::Moving => self.rustbox.print(x3+6, 4, rustbox::RB_NORMAL, Color::Default, Color::Default, &format!(": Flyt  {} felter", self.bigskip)),
		}
		self.rustbox.present();
	}
	
	fn display_status(&self, text: &str) {
		self.rustbox.print(2, 0, rustbox::RB_BOLD, Color::Default, Color::Default, text);
		
		self.rustbox.present();
	}
	
	fn clear_status(&self) {
		self.rustbox.print(0, 0, rustbox::RB_NORMAL, Color::Default, Color::Default, &(0..self.rustbox.width()).map(|_| " ").collect::<String>());
		
		self.rustbox.present();
	}
	
	fn draw_previous_plade(&self, plade: [[u16; 9]; 3]) {
		let endx = self.rustbox.width()/2 - (BANKOCOLS * 5)/2 - 3;
		let y = 6;
		
		let mut col = BANKOCOLS - 1;
		
		for dy in 0..BANKOROWS*4+1 {
			self.rustbox.print(endx, y + dy, rustbox::RB_NORMAL, Color::Default, Color::Default, if dy % 4 == 0 { "+" } else { "|" });
		}
		
		for rcol in 0..BANKOCOLS-1 {
			if endx - 5 < rcol*5 {
				break;
			}
			let colx = endx - rcol*5 - 5;
			for row in 0..plade.len() {
				let ff = plade[row][col];
				let text = if ff == 0 {
					"|    ".to_string()
				} else if ff < 10 {
					format!("|  {} ", ff)
				} else {
					format!("| {}", ff)
				};
				self.rustbox.print(colx, y + row*4, rustbox::RB_NORMAL, Color::Default, Color::Default, "+----");
				self.rustbox.print(colx, y + row*4 + 1, rustbox::RB_NORMAL, Color::Default, Color::Default, "|    ");
				self.rustbox.print(colx, y + row*4 + 2, rustbox::RB_NORMAL, Color::Default, Color::Default, &text);
				self.rustbox.print(colx, y + row*4 + 3, rustbox::RB_NORMAL, Color::Default, Color::Default, "|    ");
			}
			self.rustbox.print(colx, y + (plade.len()-1)*4 + 4, rustbox::RB_NORMAL, Color::Default, Color::Default, "+----");
			col -= 1;
		}
		
		self.rustbox.present();
	}
	
	fn draw_next_plade(&self, plade: [[u16; 9]; 3]) {
		let x = self.rustbox.width()/2 + (BANKOCOLS * 5)/2 + 3;
		let y = 6;
		
		for dy in 0..BANKOROWS*4+1 {
			self.rustbox.print(x - 1, y + dy, rustbox::RB_NORMAL, Color::Default, Color::Default, if dy % 4 == 0 { "+" } else { "|" });
		}
		
		for col in 0..BANKOCOLS-1 {
			if x + col*5 > self.rustbox.width() {
				break;
			}
			let colx = x + col*5;
			for row in 0..plade.len() {
				let ff = plade[row][col];
				let text = if ff == 0 {
					"    |".to_string()
				} else if ff < 10 {
					format!("  {} |", ff)
				} else {
					format!(" {} |", ff)
				};
				self.rustbox.print(colx, y + row*4, rustbox::RB_NORMAL, Color::Default, Color::Default, "----+");
				self.rustbox.print(colx, y + row*4 + 1, rustbox::RB_NORMAL, Color::Default, Color::Default, "    |");
				self.rustbox.print(colx, y + row*4 + 2, rustbox::RB_NORMAL, Color::Default, Color::Default, &text);
				self.rustbox.print(colx, y + row*4 + 3, rustbox::RB_NORMAL, Color::Default, Color::Default, "    |");
			}
			self.rustbox.print(colx, y + (plade.len()-1)*4 + 4, rustbox::RB_NORMAL, Color::Default, Color::Default, "----+");
		}
		
		self.rustbox.present();
	}
	
	fn draw_plade(&self, n: usize, total: usize,
			      plade: [[u16; 9]; 3]) {
		// clean up!
		for y in 0..BANKOROWS*4 + 1 {
			self.rustbox.print(0, 6+y, rustbox::RB_NORMAL, Color::Default, Color::Default, &(0..self.rustbox.width()).map(|_| " ").collect::<String>());
		}
		let toptext = if n == 0 {
			format!("  {}/{} >", n+1, total)
		} else if n == total - 1 {
			format!("< {}/{}  ", n+1, total)
		} else {
			format!("< {}/{} >", n+1, total)
		};
		let x = self.rustbox.width()/2 - toptext.len()/2;
		self.rustbox.print(1, 1, rustbox::RB_NORMAL, Color::Default, Color::Default, &(0..self.rustbox.width()).map(|_| " ").collect::<String>());
		self.rustbox.print(x, 1, rustbox::RB_NORMAL, Color::Default, Color::Default, &toptext);
		
		let line = "+----+----+----+----+----+----+----+----+----+";
		let space = "|    |    |    |    |    |    |    |    |    |";
		let nonumber = "|    ";
		
		let x = self.rustbox.width()/2 - line.len()/2;
		let y = 6;
		
		self.rustbox.print(x, y, rustbox::RB_BOLD, Color::Default, Color::Default, line);
		for row in 0..plade.len() {
		self.rustbox.print(x, y + row*4 + 1, rustbox::RB_BOLD, Color::Default, Color::Default, space);
			for col in 0..plade[row].len() {
				let ff = plade[row][col];
				let text = if ff == 0 {
					nonumber.to_string()
				} else if ff < 10 {
					format!("|  {} ", ff)
				} else {
					format!("| {}", ff)
				};
				self.rustbox.print(x + nonumber.len()*col, y + row*4 + 2, rustbox::RB_BOLD, Color::Default, Color::Default, &text);
			}
			self.rustbox.print(x + nonumber.len()*plade[row].len(), y + row*4 + 2, rustbox::RB_BOLD, Color::Default, Color::Default, "|");
			self.rustbox.print(x, y + row*4 + 3, rustbox::RB_BOLD, Color::Default, Color::Default, space);
			self.rustbox.print(x, y + row*4 + 4, rustbox::RB_BOLD, Color::Default, Color::Default, line);
		}
		
		self.rustbox.present();
	}
	
	fn set_mode(&mut self, mode: Mode) {
		self.mode = mode;
		self.draw_top();
	}
}

fn save_plader(filename: &str, plader: &Vec<[[u16; 9]; 3]>) -> Result<usize, io::Error> {
	let mut f = try!(OpenOptions::new().write(true).open(filename));
	
	for plade in plader {
		for row in 0..BANKOROWS {
			for col in 0..BANKOCOLS {
				if col + 1 == BANKOCOLS {
					try!(write!(f, "{:02}", plade[row][col]));
				} else {
					try!(write!(f, "{:02} ", plade[row][col]));
				}
			}
			try!(f.write(b"\n"));
		}
		try!(f.write(b"\n"));
	}
	
	try!(f.flush());
	
	Ok(plader.len() as usize)
}

fn parse_bankopladeformat(filename: &str) -> Vec<[[u16; 9]; 3]> {
	let mut f = match File::open(&filename) {
		Ok(val) => val,
		Err(_) => panic!("Den fil kan jeg altså ikke åbne, Preben."),
	};
	
	let mut buffer = String::new();
	
	match f.read_to_string(&mut buffer) {
		Ok(_) => {},
		Err(_) => panic!("Det er sgu' da ikke til at læse..."),
	}
	
	let mut plader: Vec<[[u16; 9]; 3]> = Vec::new();
	
	let mut plade: [[u16; 9]; 3] = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
	                                [0, 0, 0, 0, 0, 0, 0, 0, 0],
	                                [0, 0, 0, 0, 0, 0, 0, 0, 0]];
	let mut row = 0;
	let mut col = 0;
	let mut current_field: i16 = -1;
	
	for c in buffer.chars() {
		if col >= BANKOCOLS {
			panic!("Slet format: For mange felter! ({})", c);
		}
		if row > BANKOROWS {
			panic!("Slet format: For mange rækker! ({})", c);
		}
		if c == '\n' {
			if current_field >= 0 {
				plade[row][col] = current_field as u16;
				current_field = -1;
				col = 0;
				row += 1;
			} else {
				plader.push(plade);
				plade = [[0, 0, 0, 0, 0, 0, 0, 0, 0],
				         [0, 0, 0, 0, 0, 0, 0, 0, 0],
				         [0, 0, 0, 0, 0, 0, 0, 0, 0]];
				row = 0;
			}
		} else if c == ' ' {
			plade[row][col] = current_field as u16;
			current_field = -1;
			col += 1;
		} else {
			if current_field == -1 {
				current_field = match c.to_digit(10) {
					Some(val) => val as i16 * 10,
					None      => 0,
				};
			} else {
				current_field += match c.to_digit(10) {
					Some(val) => val as i16,
					None      => 0,
				};
			}
		}
	}
	
	plader
}

fn main() {
	let filename = match env::args().nth(1) {
		Some(val) => val,
		None      => panic!("Du glemte vist at angive en fil, dit kvaj!"),
	};
	
	let mut plader = parse_bankopladeformat(&filename);
	
	let total = plader.len();
	let bigskip = if total/100 < 10 { 10 } else { total/100 };
	
	let mut v = Video::init(bigskip);
	
	let mut current: usize = 0;
	let mut mode = Mode::Normal;
	let mut till_clear = 0;
	
	'main: loop {
		v.draw_plade(current, plader.len(), plader[current]);
		if current > 0 {
			v.draw_previous_plade(plader[current-1]);
		}
		if current + 1 < plader.len() {
			v.draw_next_plade(plader[current+1]);
		}
		if till_clear > 1 {
			till_clear -= 1;
		} else if till_clear == 1 {
			v.clear_status();
			till_clear -= 1;
		}
		
		match v.rustbox.poll_event(false) {
			Ok(rustbox::Event::KeyEvent(key)) => {
				match key {
					Some(Key::Char('q')) => { break 'main; },
					Some(Key::Char('l')) => { break 'main; },
					Some(Key::Char('g')) => {
						v.clear_status();
						till_clear = 5;
						match save_plader(&filename, &plader) {
							Ok(_) => {
								v.display_status("Gemt!");
							},
							Err(e) => {
								v.display_status(&format!("En fejl! {}", e));
							},
						}
					},
					Some(Key::Char('f')) => {
						mode = match mode { 
							Mode::Normal => Mode::Moving,
							Mode::Moving => Mode::Normal,
						};
						v.set_mode(mode);
					},
					Some(Key::Right) => {
						if mode == Mode::Moving && current + 1 < total {
							plader.swap(current, current+1);
						}
						current = if current + 1 < total { current + 1 } else { current };
					},
					Some(Key::Left) => {
						if mode == Mode::Moving && current > 0 {
							plader.swap(current-1, current);
						}
						current = if current > 0 { current - 1 } else { current };
					},
					Some(Key::Up) => {
						if mode == Mode::Moving && current + 1 < total {
							plader.swap(current, if current + bigskip >= total {
								total - 1 
							} else {
								current+bigskip
							});
						}
						current = if current + 1 < total { if current + bigskip >= total { total - 1 } else { current + bigskip } } else { current };
					},
					Some(Key::Down) => {
						if mode == Mode::Moving && current > 0 {
							plader.swap(if current < bigskip { 0 } else { current - bigskip }, current);
						}
						current = if current > 0 { if current < bigskip { 0 } else { current - bigskip } } else { current };
					},
					_ => {},
				}
			},
			Err(e) => panic!("{}", e),
			_ => {}
		}
	}
}
