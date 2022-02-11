import gleam/float
import gleam/int

// TYPES -----------------------------------------------------------------------

pub external type Attr

// GENERIC CONSTRUCTORS --------------------------------------------------------

external fn create_attr (String, String) -> Attr =
    "./attr.ffi.mjs" "createAttr"

pub fn from_string (name: String, val: String) -> Attr {
    create_attr(name, val)
}

pub fn from_float (name: String, val: Float) -> Attr {
    create_attr(name, float.to_string(val))
}

pub fn from_int (name: String, val: Int) -> Attr {
    create_attr(name, int.to_string(val))
}

pub fn from_bool (name: String, val: Bool) -> Attr {
    create_attr(name, case val {
        True -> 
            "true"

        False -> 
            "false"
    })
}

// COMMON ATTRIBUTES -----------------------------------------------------------

pub fn style (styles: String) -> Attr {
    from_string("style", styles)
}

pub fn class (class: String) -> Attr {
    from_string("class", class)
}

pub fn id (id: String) -> Attr {
    from_string("id", id)
}

// INPUT ATTRIBUTES ------------------------------------------------------------

pub fn type_ (type_: String) -> Attr {
    from_string("type", type_)
}

pub fn value (value: String) -> Attr {
    from_string("value", value)
}

pub fn checked (checked: Bool) -> Attr {
    from_bool("checked", checked)
}
