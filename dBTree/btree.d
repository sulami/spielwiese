import std.stdio;

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

void test(Node root, uint key)
{
    Node node = root.lookup(key);
    if (node !is null)
        writefln("Results for %d: %3d", key, node.data);
    else
        writefln("No result found for key %d", key);
}

int main()
{
    Node root = new Node(6, 36);
    root.insert(5, 25);
    root.insert(3, 9);
    root.insert(2, 4);
    root.insert(7, 49);
    root.insert(9, 81);
    root.insert(1, 1);
    test(root, 3);
    test(root, 5);
    test(root, 7);
    test(root, 6);
    test(root, 15);
    writefln("Tree size: %d", root.size());
    return 0;
}

