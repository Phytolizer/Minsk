use std::collections::HashMap;

use crate::object::Object;

use super::binding::node::expression::assignment::BoundAssignmentExpression;
use super::binding::node::expression::binary::BoundBinaryExpression;
use super::binding::node::expression::literal::BoundLiteralExpression;
use super::binding::node::expression::unary::BoundUnaryExpression;
use super::binding::node::expression::variable::BoundVariableExpression;
use super::binding::node::expression::BoundExpression;
use super::binding::node::operator::binary::kind::BoundBinaryOperatorKind;
use super::binding::node::operator::unary::kind::BoundUnaryOperatorKind;
use super::variable_symbol::VariableSymbol;

pub(crate) struct Evaluator<'v> {
    variables: &'v mut HashMap<VariableSymbol, Object>,
}

impl<'v> Evaluator<'v> {
    pub(crate) fn new(variables: &'v mut HashMap<VariableSymbol, Object>) -> Self {
        Self { variables }
    }

    pub(crate) fn evaluate(mut self, root: BoundExpression) -> Object {
        self.evaluate_expression(root)
    }

    fn evaluate_expression(&mut self, root: BoundExpression) -> Object {
        match root {
            BoundExpression::Assignment(a) => self.evaluate_assignment_expression(a),
            BoundExpression::Binary(b) => self.evaluate_binary_expression(b),
            BoundExpression::Literal(lit) => self.evaluate_literal_expression(lit),
            BoundExpression::Unary(u) => self.evaluate_unary_expression(u),
            BoundExpression::Variable(v) => self.evaluate_variable_expression(v),
        }
    }

    fn evaluate_assignment_expression(&mut self, root: BoundAssignmentExpression) -> Object {
        let value = self.evaluate_expression(*root.expression);
        self.variables.insert(root.variable, value.clone());
        value
    }

    fn evaluate_binary_expression(&mut self, root: BoundBinaryExpression) -> Object {
        let left = self.evaluate_expression(*root.left);
        let right = self.evaluate_expression(*root.right);
        // unwrap: safe bc the binder checked the types for us
        match root.operator.kind {
            BoundBinaryOperatorKind::Addition => {
                Object::Number(left.as_number().unwrap() + right.as_number().unwrap())
            }
            BoundBinaryOperatorKind::Subtraction => {
                Object::Number(left.as_number().unwrap() - right.as_number().unwrap())
            }
            BoundBinaryOperatorKind::Multiplication => {
                Object::Number(left.as_number().unwrap() * right.as_number().unwrap())
            }
            BoundBinaryOperatorKind::Division => {
                Object::Number(left.as_number().unwrap() / right.as_number().unwrap())
            }
            BoundBinaryOperatorKind::LogicalAnd => {
                Object::Bool(left.try_into_bool().unwrap() && right.try_into_bool().unwrap())
            }
            BoundBinaryOperatorKind::LogicalOr => {
                Object::Bool(left.try_into_bool().unwrap() || right.try_into_bool().unwrap())
            }
            BoundBinaryOperatorKind::Equality => Object::Bool(left == right),
            BoundBinaryOperatorKind::Inequality => Object::Bool(left != right),
        }
    }

    fn evaluate_literal_expression(&mut self, root: BoundLiteralExpression) -> Object {
        root.value
    }

    fn evaluate_unary_expression(&mut self, root: BoundUnaryExpression) -> Object {
        let operand = self.evaluate_expression(*root.operand);
        match root.operator.kind {
            BoundUnaryOperatorKind::Identity => operand,
            BoundUnaryOperatorKind::Negation => Object::Number(-operand.as_number().unwrap()),
            BoundUnaryOperatorKind::LogicalNegation => {
                Object::Bool(!operand.try_into_bool().unwrap())
            }
        }
    }

    fn evaluate_variable_expression(&mut self, root: BoundVariableExpression) -> Object {
        self.variables.get(&root.variable).unwrap().clone()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::analysis::compilation::Compilation;
    use crate::analysis::syntax::tree::SyntaxTree;
    use crate::analysis::variable_symbol::VariableSymbol;
    use crate::object::Object;

    struct Test {
        text: &'static str,
        expected: Object,
    }

    impl Test {
        const fn new(text: &'static str, expected: Object) -> Self {
            Self { text, expected }
        }
    }

    const TESTS: &[Test] = &[
        Test::new("1", Object::Number(1)),
        Test::new("+1", Object::Number(1)),
        Test::new("-1", Object::Number(-1)),
        Test::new("14 + 12", Object::Number(26)),
        Test::new("12 - 3", Object::Number(9)),
        Test::new("4 * 2", Object::Number(8)),
        Test::new("9 / 3", Object::Number(3)),
        Test::new("(10)", Object::Number(10)),
        Test::new("12 == 3", Object::Bool(false)),
        Test::new("3 == 3", Object::Bool(true)),
        Test::new("12 != 3", Object::Bool(true)),
        Test::new("3 != 3", Object::Bool(false)),
        Test::new("false == false", Object::Bool(true)),
        Test::new("true == false", Object::Bool(false)),
        Test::new("false != false", Object::Bool(false)),
        Test::new("true != false", Object::Bool(true)),
        Test::new("true", Object::Bool(true)),
        Test::new("false", Object::Bool(false)),
        Test::new("!true", Object::Bool(false)),
        Test::new("!false", Object::Bool(true)),
        Test::new("(a = 10) * a", Object::Number(100)),
    ];

    #[test]
    fn evaluates_correct_value() {
        for test in TESTS {
            let Test { text, expected } = test;
            let mut syntax_tree = SyntaxTree::parse(text);
            let compilation = Compilation::new(&mut syntax_tree);
            let mut variables = HashMap::<VariableSymbol, Object>::new();
            let result = compilation.evaluate(&mut variables).unwrap();
            assert_eq!(*expected, result);
        }
    }
}
