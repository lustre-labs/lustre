import lustre/dom/html
import lustre/dom/html/attr

pub fn html_test () {
    html.div([ attr.class("foo") ], [
        html.p([], [
            html.text("testing")
        ]),
        html.input([ attr.value("hello") ])
    ]) 
}
