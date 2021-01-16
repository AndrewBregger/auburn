pub struct CodeGen {}

// pub fn write_byte(&mut self, byte: u8) {
//     self.data.push(byte);
// }

// pub fn write_arg(&mut self, op: OpCode, index: u8) {
//     self.write_op(op);
//     self.write_byte(index);
// }

// pub fn write_op(&mut self, op: OpCode) {
//     self.data.push(op as u8);
// }

// pub fn write_bytes(&mut self, bytes: &[u8]) {
//     self.data.extend_from_slice(bytes)
// }

// // returns the first byte of the jmp operand.
// pub fn write_jmp(&mut self, op: OpCode) -> usize {
//     self.write_op(op);
//     self.write_bytes(&[0xff, 0xff]);
//     self.len() - 2
// }

// pub fn patch_jmp(&mut self, offset: usize) {
//     println!("patch_jmp: {} {}", self.len(), offset);
//     let jump: u16 = (self.len() - offset - 2)
//         .try_into()
//         .expect("attempting to jump too far");
//     self.data[offset] = ((jump >> 8) & 0xff) as u8;
//     self.data[offset + 1] = (jump & 0xff) as u8;
// }

// pub fn write_loop(&mut self, start: usize) {
//     self.write_op(OpCode::Loop);
//     let offset = (self.len() - start + 2) as u16;
//     self.write_bytes(&offset.to_be_bytes());
// }

// /// writes a new label and returns the opcode index after the label.
// pub fn write_label(&mut self, bytes: &str) -> usize {
//     // let len: u8 = u8::try_from(bytes.len()).expect("label is too long");
//     let len = bytes.len().try_into().expect("label is too long");
//     self.write_op(OpCode::Label);
//     self.write_byte(len);
//     self.write_bytes(bytes.as_bytes());
//     self.data.len()
// }
