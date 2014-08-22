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

Node lookup(Node root, uint key)
{
    if (root is null)
        return null;
    if (key == root.key)
        return root;
    if (key < root.key)
        return lookup(root.l, key);
    return lookup(root.r, key);
}

void test(Node root, uint key)
{
    writefln("Results for %d: %3d", key, lookup(root, key).data);
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
    writeln("Success!");
    return 0;
}

