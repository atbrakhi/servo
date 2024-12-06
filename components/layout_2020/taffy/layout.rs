/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use app_units::Au;
use atomic_refcell::{AtomicRef, AtomicRefCell, AtomicRefMut};
use style::properties::ComputedValues;
use style::values::generics::length::{GenericLengthPercentageOrAuto, LengthPercentageOrAuto};
use style::values::specified::align::AlignFlags;
use style::values::specified::box_::DisplayInside;
use style::Zero;
use taffy::style_helpers::{TaffyMaxContent, TaffyMinContent};
use taffy::{AvailableSpace, MaybeMath, RequestedAxis, RunMode};

use super::{TaffyContainer, TaffyItemBox, TaffyItemBoxInner, TaffyStyloStyle};
use crate::cell::ArcRefCell;
use crate::context::LayoutContext;
use crate::formatting_contexts::{Baselines, IndependentFormattingContext, IndependentLayout};
use crate::fragment_tree::{BoxFragment, CollapsedBlockMargins, Fragment};
use crate::geom::{
    LogicalSides, LogicalVec2, PhysicalPoint, PhysicalRect, PhysicalSides, PhysicalSize, Size,
    SizeConstraint,
};
use crate::positioned::{AbsolutelyPositionedBox, PositioningContext, PositioningContextLength};
use crate::sizing::{ContentSizes, InlineContentSizesResult};
use crate::style_ext::ComputedValuesExt;
use crate::{ConstraintSpace, ContainingBlock};

const DUMMY_NODE_ID: taffy::NodeId = taffy::NodeId::new(u64::MAX);

fn resolve_content_size(constraint: AvailableSpace, content_sizes: ContentSizes) -> f32 {
    match constraint {
        AvailableSpace::Definite(limit) => {
            let min = content_sizes.min_content.to_f32_px();
            let max = content_sizes.max_content.to_f32_px();
            limit.min(max).max(min)
        },
        AvailableSpace::MinContent => content_sizes.min_content.to_f32_px(),
        AvailableSpace::MaxContent => content_sizes.max_content.to_f32_px(),
    }
}

#[inline(always)]
fn with_independant_formatting_context<T>(
    item: &mut TaffyItemBoxInner,
    cb: impl FnOnce(&mut IndependentFormattingContext) -> T,
) -> T {
    match item {
        TaffyItemBoxInner::InFlowBox(ref mut context) => cb(context),
        TaffyItemBoxInner::OutOfFlowAbsolutelyPositionedBox(ref abspos_box) => {
            let mut abspos_box = AtomicRefCell::borrow_mut(abspos_box);
            cb(&mut abspos_box.context)
        },
    }
}

/// Layout parameters and intermediate results about a taffy container,
/// grouped to avoid passing around many parameters
struct TaffyContainerContext<'a> {
    source_child_nodes: &'a [ArcRefCell<TaffyItemBox>],
    layout_context: &'a LayoutContext<'a>,
    positioning_context: &'a mut PositioningContext,
    content_box_size_override: &'a ContainingBlock<'a>,
    style: &'a ComputedValues,
}

struct ChildIter(std::ops::Range<usize>);
impl Iterator for ChildIter {
    type Item = taffy::NodeId;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(taffy::NodeId::from)
    }
}

impl taffy::TraversePartialTree for TaffyContainerContext<'_> {
    type ChildIter<'a> = ChildIter where Self: 'a;

    fn child_ids(&self, _node_id: taffy::NodeId) -> Self::ChildIter<'_> {
        ChildIter(0..self.source_child_nodes.len())
    }

    fn child_count(&self, _node_id: taffy::NodeId) -> usize {
        self.source_child_nodes.len()
    }

    fn get_child_id(&self, _node_id: taffy::NodeId, index: usize) -> taffy::NodeId {
        taffy::NodeId::from(index)
    }
}

impl taffy::LayoutPartialTree for TaffyContainerContext<'_> {
    type CoreContainerStyle<'a> = TaffyStyloStyle<&'a ComputedValues> where Self: 'a;
    type CacheMut<'b> = AtomicRefMut<'b, taffy::Cache> where Self: 'b;

    fn get_core_container_style(&self, _node_id: taffy::NodeId) -> Self::CoreContainerStyle<'_> {
        TaffyStyloStyle(self.style)
    }

    fn set_unrounded_layout(&mut self, node_id: taffy::NodeId, layout: &taffy::Layout) {
        let id = usize::from(node_id);
        (*self.source_child_nodes[id]).borrow_mut().taffy_layout = *layout;
    }

    fn get_cache_mut(&mut self, node_id: taffy::NodeId) -> AtomicRefMut<'_, taffy::Cache> {
        let id = usize::from(node_id);
        let mut_ref: AtomicRefMut<'_, _> = (*self.source_child_nodes[id]).borrow_mut();
        AtomicRefMut::map(mut_ref, |node| &mut node.taffy_layout_cache)
    }

    fn compute_child_layout(
        &mut self,
        node_id: taffy::NodeId,
        inputs: taffy::LayoutInput,
    ) -> taffy::LayoutOutput {
        let mut child = (*self.source_child_nodes[usize::from(node_id)]).borrow_mut();
        let child = &mut *child;

        fn option_f32_to_lpa(input: Option<f32>) -> LengthPercentageOrAuto<Au> {
            match input {
                None => LengthPercentageOrAuto::Auto,
                Some(length) => LengthPercentageOrAuto::LengthPercentage(Au::from_f32_px(length)),
            }
        }

        fn option_f32_to_size(input: Option<f32>) -> Size<Au> {
            match input {
                None => Size::Initial,
                Some(length) => Size::Numeric(Au::from_f32_px(length)),
            }
        }

        with_independant_formatting_context(
            &mut child.taffy_level_box,
            |independent_context| -> taffy::LayoutOutput {
                match independent_context {
                    IndependentFormattingContext::Replaced(replaced) => {
                        // TODO: re-evaluate sizing constraint conversions in light of recent layout_2020 changes
                        let containing_block = &self.content_box_size_override;

                        // Adjust known_dimensions from border box to content box
                        let pbm = replaced.style.padding_border_margin(containing_block);
                        let pb_sum = pbm.padding_border_sums.map(|v| v.to_f32_px());
                        let content_box_known_dimensions = taffy::Size {
                            width: inputs
                                .known_dimensions
                                .width
                                .map(|width| width - pb_sum.inline),
                            height: inputs
                                .known_dimensions
                                .height
                                .map(|height| height - pb_sum.block),
                        };

                        let content_box_size = replaced
                            .contents
                            .used_size_as_if_inline_element_from_content_box_sizes(
                                containing_block,
                                &replaced.style,
                                replaced.preferred_aspect_ratio(&pbm.padding_border_sums),
                                LogicalVec2 {
                                    inline: option_f32_to_size(content_box_known_dimensions.width),
                                    block: option_f32_to_size(content_box_known_dimensions.height),
                                },
                                LogicalVec2 {
                                    inline: Size::Numeric(Au::zero()),
                                    block: Size::Numeric(Au::zero()),
                                },
                                LogicalVec2 {
                                    inline: Size::Initial,
                                    block: Size::Initial,
                                },
                                pbm.padding_border_sums + pbm.margin.auto_is(Au::zero).sum(),
                            )
                            .to_physical_size(self.style.writing_mode);

                        // Create fragments if the RunMode if PerformLayout
                        // If the RunMode is ComputeSize then only the returned size will be used
                        if inputs.run_mode == RunMode::PerformLayout {
                            child.child_fragments = replaced
                                .contents
                                .make_fragments(&replaced.style, content_box_size);
                        }

                        let computed_size = taffy::Size {
                            width: inputs.known_dimensions.width.unwrap_or_else(|| {
                                content_box_size.width.to_f32_px() +
                                    pbm.padding_border_sums.inline.to_f32_px()
                            }),
                            height: inputs.known_dimensions.height.unwrap_or_else(|| {
                                content_box_size.height.to_f32_px() +
                                    pbm.padding_border_sums.block.to_f32_px()
                            }),
                        };
                        let size = inputs.known_dimensions.unwrap_or(computed_size);
                        taffy::LayoutOutput {
                            size,
                            ..taffy::LayoutOutput::DEFAULT
                        }
                    },

                    IndependentFormattingContext::NonReplaced(non_replaced) => {
                        // TODO: re-evaluate sizing constraint conversions in light of recent layout_2020 changes
                        let containing_block = &self.content_box_size_override;

                        // Adjust known_dimensions from border box to content box
                        let pbm = non_replaced.style.padding_border_margin(containing_block);
                        let margin_sum = pbm.margin.auto_is(Au::zero).sum();
                        let content_box_inset =
                            (pbm.padding_border_sums + margin_sum).map(|v| v.to_f32_px());
                        let content_box_known_dimensions =
                            taffy::Size {
                                width: inputs.known_dimensions.width.map(|width| {
                                    width - pbm.padding_border_sums.inline.to_f32_px()
                                }),
                                height: inputs.known_dimensions.height.map(|height| {
                                    height - pbm.padding_border_sums.block.to_f32_px()
                                }),
                            };

                        // Compute inline size
                        let inline_size = content_box_known_dimensions.width.unwrap_or_else(|| {
                            let constraint_space = ConstraintSpace {
                                // TODO: pass min- and max- size
                                block_size: SizeConstraint::new(
                                    inputs.parent_size.height.map(Au::from_f32_px),
                                    Au::zero(),
                                    None,
                                ),
                                writing_mode: self.style.writing_mode,
                                preferred_aspect_ratio: non_replaced.preferred_aspect_ratio(),
                            };

                            let result = non_replaced
                                .inline_content_sizes(self.layout_context, &constraint_space);
                            let adjusted_available_space = inputs
                                .available_space
                                .width
                                .map_definite_value(|width| width - content_box_inset.inline);

                            resolve_content_size(adjusted_available_space, result.sizes)
                        });

                        // Return early if only inline content sizes are requested
                        if inputs.run_mode == RunMode::ComputeSize &&
                            inputs.axis == RequestedAxis::Horizontal
                        {
                            return taffy::LayoutOutput::from_outer_size(taffy::Size {
                                width: inline_size + pbm.padding_border_sums.inline.to_f32_px(),
                                // If RequestedAxis is Horizontal then height will be ignored.
                                height: 0.0,
                            });
                        }

                        let maybe_block_size =
                            option_f32_to_lpa(content_box_known_dimensions.height);
                        let content_box_size_override = ContainingBlock {
                            inline_size: Au::from_f32_px(inline_size),
                            block_size: maybe_block_size,
                            style: &non_replaced.style,
                        };

                        let layout = {
                            let mut child_positioning_context = PositioningContext::new_for_subtree(
                                self.positioning_context
                                    .collects_for_nearest_positioned_ancestor(),
                            );
                            let layout = non_replaced.layout(
                                self.layout_context,
                                &mut child_positioning_context,
                                &content_box_size_override,
                                containing_block,
                            );

                            // Store layout data on child for later access
                            child.positioning_context = child_positioning_context;

                            layout
                        };

                        child.child_fragments = layout.fragments;

                        let block_size = layout.content_block_size.to_f32_px();

                        let computed_size = taffy::Size {
                            width: inline_size + pbm.padding_border_sums.inline.to_f32_px(),
                            height: block_size + pbm.padding_border_sums.block.to_f32_px(),
                        };
                        let size = inputs.known_dimensions.unwrap_or(computed_size);

                        taffy::LayoutOutput {
                            size,
                            first_baselines: taffy::Point {
                                x: None,
                                y: layout.baselines.first.map(|au| au.to_f32_px()),
                            },
                            ..taffy::LayoutOutput::DEFAULT
                        }
                    },
                }
            },
        )
    }
}

impl taffy::LayoutGridContainer for TaffyContainerContext<'_> {
    type GridContainerStyle<'a> = TaffyStyloStyle<&'a ComputedValues>
    where
        Self: 'a;

    type GridItemStyle<'a> = TaffyStyloStyle<AtomicRef<'a, ComputedValues>>
    where
        Self: 'a;

    fn get_grid_container_style(
        &self,
        _node_id: taffy::prelude::NodeId,
    ) -> Self::GridContainerStyle<'_> {
        TaffyStyloStyle(self.style)
    }

    fn get_grid_child_style(
        &self,
        child_node_id: taffy::prelude::NodeId,
    ) -> Self::GridItemStyle<'_> {
        let id = usize::from(child_node_id);
        let child = (*self.source_child_nodes[id]).borrow();
        TaffyStyloStyle(AtomicRef::map(child, |c| &*c.style))
    }
}

impl TaffyContainer {
    pub fn inline_content_sizes(
        &self,
        layout_context: &LayoutContext,
        _constraint_space: &ConstraintSpace,
    ) -> InlineContentSizesResult {
        let style = &self.style;

        let max_content_inputs = taffy::LayoutInput {
            run_mode: taffy::RunMode::ComputeSize,
            sizing_mode: taffy::SizingMode::InherentSize,
            axis: taffy::RequestedAxis::Horizontal,
            vertical_margins_are_collapsible: taffy::Line::FALSE,

            known_dimensions: taffy::Size::NONE,
            parent_size: taffy::Size::NONE,
            available_space: taffy::Size::MAX_CONTENT,
        };

        let min_content_inputs = taffy::LayoutInput {
            available_space: taffy::Size::MIN_CONTENT,
            ..max_content_inputs
        };

        let containing_block = &ContainingBlock {
            inline_size: Au::zero(),
            block_size: GenericLengthPercentageOrAuto::Auto,
            style,
        };

        let mut grid_context = TaffyContainerContext {
            layout_context,
            positioning_context:
                &mut PositioningContext::new_for_containing_block_for_all_descendants(),
            content_box_size_override: containing_block,
            style,
            source_child_nodes: &self.children,
        };

        let (max_content_output, min_content_output) = match style.clone_display().inside() {
            DisplayInside::Grid => {
                let max_content_output = taffy::compute_grid_layout(
                    &mut grid_context,
                    DUMMY_NODE_ID,
                    max_content_inputs,
                );
                let min_content_output = taffy::compute_grid_layout(
                    &mut grid_context,
                    DUMMY_NODE_ID,
                    min_content_inputs,
                );
                (max_content_output, min_content_output)
            },
            _ => panic!("Servo is only configured to use Taffy for CSS Grid layout"),
        };

        let pb_sums = style
            .padding_border_margin(containing_block)
            .padding_border_sums;

        InlineContentSizesResult {
            sizes: ContentSizes {
                max_content: Au::from_f32_px(max_content_output.size.width) - pb_sums.inline,
                min_content: Au::from_f32_px(min_content_output.size.width) - pb_sums.inline,
            },

            // TODO: determine this accurately
            //
            // "true" is a safe default as it will prevent Servo from performing optimizations based
            // on the assumption that the node's size does not depend on block constraints.
            depends_on_block_constraints: true,
        }
    }

    /// <https://drafts.csswg.org/css-grid/#layout-algorithm>
    pub(crate) fn layout(
        &self,
        layout_context: &LayoutContext,
        positioning_context: &mut PositioningContext,
        content_box_size_override: &ContainingBlock,
        containing_block: &ContainingBlock,
    ) -> IndependentLayout {
        let mut container_ctx = TaffyContainerContext {
            layout_context,
            positioning_context,
            content_box_size_override,
            style: content_box_size_override.style,
            source_child_nodes: &self.children,
        };

        fn auto_or_to_option<T>(input: GenericLengthPercentageOrAuto<T>) -> Option<T> {
            match input {
                LengthPercentageOrAuto::LengthPercentage(val) => Some(val),
                LengthPercentageOrAuto::Auto => None,
            }
        }

        let container_style = &content_box_size_override.style;
        let align_items = container_style.clone_align_items();
        let justify_items = container_style.clone_justify_items();
        let pbm = container_style.padding_border_margin(containing_block);

        let known_dimensions = taffy::Size {
            width: Some(
                (content_box_size_override.inline_size + pbm.padding_border_sums.inline)
                    .to_f32_px(),
            ),
            height: auto_or_to_option(content_box_size_override.block_size)
                .map(Au::to_f32_px)
                .maybe_add(pbm.padding_border_sums.block.to_f32_px()),
        };

        let taffy_containing_block = taffy::Size {
            width: Some(containing_block.inline_size.to_f32_px()),
            height: auto_or_to_option(containing_block.block_size).map(Au::to_f32_px),
        };

        let layout_input = taffy::LayoutInput {
            run_mode: taffy::RunMode::PerformLayout,
            sizing_mode: taffy::SizingMode::InherentSize,
            axis: taffy::RequestedAxis::Vertical,
            vertical_margins_are_collapsible: taffy::Line::FALSE,

            known_dimensions,
            parent_size: taffy_containing_block,
            available_space: taffy_containing_block.map(AvailableSpace::from),
        };

        let output = match container_ctx.style.clone_display().inside() {
            DisplayInside::Grid => {
                taffy::compute_grid_layout(&mut container_ctx, DUMMY_NODE_ID, layout_input)
            },
            _ => panic!("Servo is only configured to use Taffy for CSS Grid layout"),
        };

        // Convert `taffy::Layout` into Servo `Fragment`s
        let fragments: Vec<Fragment> = self
            .children
            .iter()
            .map(|child| (**child).borrow_mut())
            .map(|mut child| {
                fn rect_to_logical_sides<T>(rect: taffy::Rect<T>) -> LogicalSides<T> {
                    LogicalSides {
                        inline_start: rect.left,
                        inline_end: rect.right,
                        block_start: rect.top,
                        block_end: rect.bottom,
                    }
                }

                fn rect_to_physical_sides<T>(rect: taffy::Rect<T>) -> PhysicalSides<T> {
                    PhysicalSides::new(rect.top, rect.right, rect.bottom, rect.left)
                }

                fn size_and_pos_to_logical_rect<T: Default>(
                    position: taffy::Point<T>,
                    size: taffy::Size<T>,
                ) -> PhysicalRect<T> {
                    PhysicalRect::new(
                        PhysicalPoint::new(position.x, position.y),
                        PhysicalSize::new(size.width, size.height),
                    )
                }

                let layout = &child.taffy_layout;

                let padding = rect_to_physical_sides(layout.padding.map(Au::from_f32_px));
                let border = rect_to_physical_sides(layout.border.map(Au::from_f32_px));
                let margin = rect_to_physical_sides(layout.margin.map(Au::from_f32_px));
                let logical_margin = rect_to_logical_sides(layout.margin.map(Au::from_f32_px));
                let collapsed_margin = CollapsedBlockMargins::from_margin(&logical_margin);

                // Compute content box size and position.
                //
                // For the x/y position we have to correct for the difference between the
                // content box and the border box for both the parent and the child.
                let content_size = size_and_pos_to_logical_rect(
                    taffy::Point {
                        x: Au::from_f32_px(
                            layout.location.x + layout.padding.left + layout.border.left,
                        ) - pbm.padding.inline_start -
                            pbm.border.inline_start,
                        y: Au::from_f32_px(
                            layout.location.y + layout.padding.top + layout.border.top,
                        ) - pbm.padding.block_start -
                            pbm.border.block_start,
                    },
                    taffy::Size {
                        width: layout.size.width -
                            layout.padding.left -
                            layout.padding.right -
                            layout.border.left -
                            layout.border.right,
                        height: layout.size.height -
                            layout.padding.top -
                            layout.padding.bottom -
                            layout.border.top -
                            layout.border.bottom,
                    }
                    .map(Au::from_f32_px),
                );

                match &mut child.taffy_level_box {
                    TaffyItemBoxInner::InFlowBox(independent_box) => {
                        let fragment = Fragment::Box(
                            BoxFragment::new(
                                independent_box.base_fragment_info(),
                                independent_box.style().clone(),
                                std::mem::take(&mut child.child_fragments),
                                content_size,
                                padding,
                                border,
                                margin,
                                None, /* clearance */
                                collapsed_margin,
                            )
                            .with_baselines(Baselines {
                                first: output.first_baselines.y.map(Au::from_f32_px),
                                last: None,
                            }),
                        );

                        child
                            .positioning_context
                            .adjust_static_position_of_hoisted_fragments(
                                &fragment,
                                PositioningContextLength::zero(),
                            );
                        let child_positioning_context = std::mem::replace(
                            &mut child.positioning_context,
                            PositioningContext::new_for_containing_block_for_all_descendants(),
                        );
                        container_ctx
                            .positioning_context
                            .append(child_positioning_context);

                        fragment
                    },
                    TaffyItemBoxInner::OutOfFlowAbsolutelyPositionedBox(abs_pos_box) => {
                        fn resolve_alignment(value: AlignFlags, auto: AlignFlags) -> AlignFlags {
                            match value {
                                AlignFlags::AUTO => auto,
                                AlignFlags::NORMAL => AlignFlags::STRETCH,
                                value => value,
                            }
                        }

                        let hoisted_box = AbsolutelyPositionedBox::to_hoisted(
                            abs_pos_box.clone(),
                            PhysicalRect::from_size(PhysicalSize::new(
                                Au::from_f32_px(output.size.width),
                                Au::from_f32_px(output.size.height),
                            )),
                            LogicalVec2 {
                                inline: resolve_alignment(
                                    child.style.clone_align_self().0 .0,
                                    align_items.0,
                                ),
                                block: resolve_alignment(
                                    child.style.clone_justify_self().0 .0,
                                    justify_items.computed.0,
                                ),
                            },
                            container_ctx.style.writing_mode,
                        );
                        let hoisted_fragment = hoisted_box.fragment.clone();
                        container_ctx.positioning_context.push(hoisted_box);
                        Fragment::AbsoluteOrFixedPositioned(hoisted_fragment)
                    },
                }
            })
            .collect();

        IndependentLayout {
            fragments,
            content_block_size: Au::from_f32_px(output.size.height) - pbm.padding_border_sums.block,
            content_inline_size_for_table: Some(
                Au::from_f32_px(output.size.width) - pbm.padding_border_sums.inline,
            ),
            baselines: Baselines::default(),

            // TODO: determine this accurately
            //
            // "true" is a safe default as it will prevent Servo from performing optimizations based
            // on the assumption that the node's size does not depend on block constraints.
            depends_on_block_constraints: true,
        }
    }
}