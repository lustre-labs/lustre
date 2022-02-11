export const createNode = (tag, namespace, attrs, children) => {
    const el = namespace == ''
        ? document.createElement(tag)
        : document.createElementNS(namespace, tag)

    for (const attr of attrs) el.setAttributeNode(attr)
    for (const node of children) el.append(node)

    return el
}

export const createText = (content) => {
    return document.createTextNode(content)
}