module btree;

class Node
{
    uint key;
    uint data;
    Node p;
    Node l;
    Node r;

    this(uint key, uint data)
    {
        this.key = key;
        this.data = data;
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

    void insert(uint key, typeof(Node.data) data)
    {
        if (key <= this.key) {
            if (this.l is null) {
                this.l = new Node(key, data);
                this.l.p = this;
            } else {
                this.l.insert(key, data);
            }
        } else {
            if (this.r is null) {
                this.r = new Node(key, data);
                this.r.p = this;
            } else {
                this.r.insert(key, data);
            }
        }
    }

    void remove(uint key)
    {
        Node node = this.lookup(key);
        if (node is null)
            return;
        Node parent = node.p;
        bool leftshort = (node._left_path() <= node._right_path());
        bool isleft = (parent.l == node);
        if (node.l !is null && node.r !is null) {
            /* relocate both */
            isleft ? parent.l : parent.r = leftshort ? node.l : node.r;
            Node tmp = leftshort ? node.l : node.r;
            while ((leftshort ? tmp.r : tmp.l) !is null)
                tmp = leftshort ? tmp.r : tmp.l;
            leftshort ? tmp.l : tmp.r = leftshort ? node.r : node.l;
        } else if (node.l !is null) {
            /* relocate just left */
            isleft ? parent.l : parent.r = node.l;
        } else if (node.r !is null) {
            /* relocate just right */
            isleft ? parent.l : parent.r = node.r;
        }
        destroy(node);
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

    protected uint _left_path()
    {
        if (this.r !is null)
            return (1 + this.r._left_path());
        return 0;
    }

    protected uint _right_path()
    {
        if (this.l !is null)
            return (1 + this.l._right_path());
        return 0;
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
    root.remove(5);
    assert(root.size() == 6);
    assert(root.lookup(5) is null);
}

