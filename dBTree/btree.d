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

Node new_node(Node root, uint key, typeof(Node.data) data)
{
    if (root is null)
        return new Node(key, data);
    if (key <= root.key)
        root.l = new_node(root.l, key, data);
    else
        root.r = new_node(root.r, key, data);
    return root;
}



void test(Node root, uint key)
{
    writefln("Results for %d: %3d", key, root.lookup(key).data);
}

int main()
{
    Node root = new_node(null, 0, 0);
    new_node(root, 5, 25);
    new_node(root, 3, 9);
    new_node(root, 2, 4);
    new_node(root, 7, 49);
    new_node(root, 9, 81);
    new_node(root, 1, 1);
    test(root, 3);
    test(root, 5);
    test(root, 7);
    writefln("Tree size: %d", root.size());
    writeln("Success!");
    return 0;
}

