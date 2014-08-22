class Node
{
    uint key;
    uint data;
    Node l;
    Node r;

    this(uint key, uint data)
    {
        this.key = key;
        this.data = data;
    }

    void insert(uint key, typeof(Node.data) data)
    {
        if (key <= this.key) {
            if (this.l is null)
                this.l = new Node(key, data);
            else
                this.l.insert(key, data);
        } else {
            if (this.r is null)
                this.r = new Node(key, data);
            else
                this.r.insert(key, data);
        }
    }

    uint size()
    {
        uint retval = 1;
        if (this.l !is null)
            retval += this.l.size();
        if (this.r !is null)
            retval += this.r.size();
        return retval;
    }

    Node lookup(uint key)
    {
        if (key == this.key)
            return this;
        if (key < this.key && this.l !is null)
            return this.l.lookup(key);
        if (this.r !is null)
            return this.r.lookup(key);
        return null;
    }
}

unittest {
    Node root = new Node(6, 36);
    assert(root !is null);
    root.insert(5, 25);
    root.insert(2, 4);
    root.insert(7, 49);
    root.insert(3, 9);
    root.insert(9, 81);
    root.insert(1, 1);
    assert(root.lookup(9) !is null);
    assert(root.lookup(9).data == 81);
    assert(root.lookup(6) !is null);
    assert(root.lookup(6).data == 36);
    assert(root.lookup(15) is null);
    assert(root.size() == 7);
}

int main()
{
    return 0;
}

