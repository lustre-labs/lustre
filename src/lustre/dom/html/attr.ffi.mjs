export const createAttr = (name, value) => {
    const attr = document.createAttribute(name)
    attr.value = value

    return attr
}