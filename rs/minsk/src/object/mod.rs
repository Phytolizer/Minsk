use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Number(i64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum ObjectKind {
    Number,
    Bool,
}

impl Object {
    pub fn kind(&self) -> ObjectKind {
        match self {
            Self::Number(_) => ObjectKind::Number,
            Self::Bool(_) => ObjectKind::Bool,
        }
    }

    pub fn as_number(&self) -> Option<&i64> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<&bool> {
        if let Self::Bool(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn try_into_bool(self) -> Result<bool, Self> {
        if let Self::Bool(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
}
