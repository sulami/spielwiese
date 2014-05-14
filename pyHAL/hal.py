class Memory:
    pass

class MemoryController:
    def __init__(self):
        self.memories = []
        self.axises = []

    def new_memory(self, **kwargs):
        m = Memory()
        for axis in kwargs:
            if axis not in self.axises:
                self.axises.append(axis)
            for mem in self.memories:
                if axis not in mem.__dict__:
                    mem.__dict__[axis] = 0
        for axis in self.axises:
            if axis not in kwargs:
                m.__dict__[axis] = 0
        m.__dict__.update(kwargs)
        self.memories.append(m)
        return m

