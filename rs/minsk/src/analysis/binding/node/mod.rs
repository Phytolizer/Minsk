use self::kind::BoundNodeKind;

pub(crate) mod expression;
pub(crate) mod kind;
pub(crate) mod operator;

pub(crate) trait BoundNode {
    fn kind(&self) -> BoundNodeKind;
}
