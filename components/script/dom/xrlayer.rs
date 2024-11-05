/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use canvas_traits::webgl::WebGLContextId;
use dom_struct::dom_struct;
#[cfg(feature = "webxr")]
use webxr_api::LayerId;

use crate::dom::bindings::inheritance::Castable;
use crate::dom::bindings::root::Dom;
use crate::dom::eventtarget::EventTarget;
use crate::dom::webglrenderingcontext::WebGLRenderingContext;
#[cfg(feature = "webxr")]
use crate::dom::xrframe::XRFrame;
#[cfg(feature = "webxr")]
use crate::dom::xrsession::XRSession;
#[cfg(feature = "webxr")]
use crate::dom::xrwebgllayer::XRWebGLLayer;

#[cfg(feature = "webxr")]
#[dom_struct]
pub struct XRLayer {
    event_target: EventTarget,
    session: Dom<XRSession>,
    context: Dom<WebGLRenderingContext>,
    /// If none, the session is inline (the composition disabled flag is true)
    /// and this is a XRWebGLLayer.
    #[ignore_malloc_size_of = "Layer ids don't heap-allocate"]
    #[no_trace]
    layer_id: Option<LayerId>,
}

#[cfg(feature = "webxr")]
impl XRLayer {
    #[allow(dead_code)]
    pub fn new_inherited(
        session: &XRSession,
        context: &WebGLRenderingContext,
        layer_id: Option<LayerId>,
    ) -> XRLayer {
        XRLayer {
            event_target: EventTarget::new_inherited(),
            session: Dom::from_ref(session),
            context: Dom::from_ref(context),
            layer_id,
        }
    }

    pub(crate) fn layer_id(&self) -> Option<LayerId> {
        self.layer_id
    }

    pub(crate) fn context_id(&self) -> WebGLContextId {
        self.context.context_id()
    }

    pub(crate) fn context(&self) -> &WebGLRenderingContext {
        &self.context
    }

    pub(crate) fn session(&self) -> &XRSession {
        &self.session
    }

    pub fn begin_frame(&self, frame: &XRFrame) -> Option<()> {
        // TODO: Implement this for other layer types
        if let Some(this) = self.downcast::<XRWebGLLayer>() {
            this.begin_frame(frame)
        } else {
            unimplemented!()
        }
    }

    pub fn end_frame(&self, frame: &XRFrame) -> Option<()> {
        // TODO: Implement this for other layer types
        if let Some(this) = self.downcast::<XRWebGLLayer>() {
            this.end_frame(frame)
        } else {
            unimplemented!()
        }
    }
}
