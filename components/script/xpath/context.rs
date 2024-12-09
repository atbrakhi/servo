/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::iter::Enumerate;
use std::vec::IntoIter;

use super::Node;
use crate::dom::bindings::root::DomRoot;

/// The context during evaluation of an XPath expression.
pub struct EvaluationCtx {
    /// Where we started at
    pub starting_node: DomRoot<Node>,
    /// The "current" node in the evaluation
    pub context_node: DomRoot<Node>,
    /// Details needed for evaluating a predicate list
    pub predicate_ctx: Option<PredicateCtx>,
    /// The nodes we're currently matching against
    pub predicate_nodes: Option<Vec<DomRoot<Node>>>,
}

#[derive(Clone, Copy)]
pub struct PredicateCtx {
    pub index: usize,
    pub size: usize,
}

impl EvaluationCtx {
    /// Prepares the context used while evaluating the XPath expression
    pub fn new(context_node: &Node) -> EvaluationCtx {
        EvaluationCtx {
            starting_node: DomRoot::from_ref(context_node),
            context_node: DomRoot::from_ref(context_node),
            predicate_ctx: None,
            predicate_nodes: None,
        }
    }

    /// Creates a new context using the provided node as the context node
    pub fn subcontext_for_node(&self, node: &Node) -> EvaluationCtx {
        EvaluationCtx {
            starting_node: self.starting_node.clone(),
            context_node: DomRoot::from_ref(node),
            predicate_ctx: self.predicate_ctx,
            predicate_nodes: self.predicate_nodes.clone(),
        }
    }

    pub fn update_predicate_nodes(&self, nodes: Vec<&Node>) -> EvaluationCtx {
        EvaluationCtx {
            starting_node: self.starting_node.clone(),
            context_node: self.context_node.clone(),
            predicate_ctx: None,
            predicate_nodes: Some(nodes.into_iter().map(DomRoot::from_ref).collect()),
        }
    }

    pub fn subcontext_iter_for_nodes(&self) -> EvalNodesetIter {
        let size = self.predicate_nodes.as_ref().map_or(0, |v| v.len());
        EvalNodesetIter {
            ctx: self,
            nodes_iter: self
                .predicate_nodes
                .as_ref()
                .map_or_else(|| Vec::new().into_iter(), |v| v.clone().into_iter())
                .enumerate(),
            size,
        }
    }
}

/// When evaluating predicates, we need to keep track of the current node being evaluated and
/// the index of that node in the nodeset we're operating on.
pub struct EvalNodesetIter<'a> {
    ctx: &'a EvaluationCtx,
    nodes_iter: Enumerate<IntoIter<DomRoot<Node>>>,
    size: usize,
}

impl<'a> Iterator for EvalNodesetIter<'a> {
    type Item = EvaluationCtx;

    fn next(&mut self) -> Option<EvaluationCtx> {
        self.nodes_iter.next().map(|(idx, node)| EvaluationCtx {
            starting_node: self.ctx.starting_node.clone(),
            context_node: node.clone(),
            predicate_nodes: self.ctx.predicate_nodes.clone(),
            predicate_ctx: Some(PredicateCtx {
                index: idx + 1,
                size: self.size,
            }),
        })
    }
}