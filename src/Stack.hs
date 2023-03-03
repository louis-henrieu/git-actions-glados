module Stack (
    initStack
) where
    import Info

    initStack :: Stack
    initStack = Stack { fast = [], global = [], constValue = [], bytecode = [], end = False }