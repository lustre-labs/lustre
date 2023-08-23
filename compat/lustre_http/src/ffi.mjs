export const do_request = (method, url, body, on_success, on_error) => {
    let xhr = new XMLHttpRequest()
    xhr.onerror = _evt => {
        console.log("error for", method, url, ":", xhr)
        on_error(888, "oops!")
    }
    xhr.onloadend = _evt => {
        switch (xhr.status) {
            case 200:
            case 204: on_success(xhr.responseText); break;
            default: on_error(xhr.status, xhr.responseText)
        }
    }
    xhr.open(method, url)
    xhr.send(body);
}
