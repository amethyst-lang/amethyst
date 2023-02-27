pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum Ast {
    Integer(u128),
    //Bool(bool),
    Binary {
        op: BinaryOp,
        left: Box<Ast>,
        right: Box<Ast>,
    },
}
