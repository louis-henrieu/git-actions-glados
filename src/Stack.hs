module Stack (
    initStack
) where
    import Info

    initStack :: Stack
    initStack = Stack { fast = [], global = [], const_value = [], bytecode = [] }