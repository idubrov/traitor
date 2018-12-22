use std::sync::atomic::{AtomicUsize, Ordering};

struct Metadata;

#[traitor::shadow]
trait Annotated {
    fn hello<'data>(&'data self) -> &'static str;
}

unsafe impl AnnotatedShadow for Metadata {
    type Data = ValueWithDrop;

    fn hello<'data>(_data: &'data ValueWithDrop, _meta: &'data Metadata) -> &'static str {
        "hello"
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

struct ValueWithDrop {
}

impl Drop for ValueWithDrop {
    fn drop(&mut self) {
        COUNTER.fetch_add(1, Ordering::SeqCst);
    }
}

#[test]
fn test_drop() {
    let traitor = traitor::Traitor::new();
    let binder = traitor.declare(AnnotatedShadowInfo::new(Metadata));

    let bound: Box<Annotated> = bind_Annotated_box(Box::new(ValueWithDrop {}), binder);

    assert_eq!("hello", bound.hello());
    std::mem::drop(bound);

    assert_eq!(COUNTER.load(Ordering::SeqCst), 1);
}
