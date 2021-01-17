package linked_list_set;

//import kotlinx.atomicfu.AtomicRef;

import kotlinx.atomicfu.AtomicRef;

import java.lang.annotation.AnnotationTypeMismatchException;

//import java.util.concurrent.atomic.AtomicReference;

public class SetImpl implements Set {
    static void print(Set set) {
        for (int j = 0; j < 40; ++j) {
            if (set.contains(j)) {
                System.out.print(j);
                System.out.print(" ");
            }
        }
        System.out.println();
    }

    public static void main(String[] args) {
        Set s = new SetImpl();
//        for (int i = 0; i < 4; ++i) {
//            System.out.println(s.remove(i));
//        }
        System.out.println(s.contains(1));
        System.out.println(s.add(1));
        System.out.println(s.contains(1));
//        print(s);
//        for (int i = 0; i < 4; ++i) {
//            System.out.println("remove#1 " + s.remove(i));
//        }
//        for (int i = 0; i < 30; ++i) {
//            System.out.println("contains#1 " + s.contains(i));
//        }
//        for (int i = 0; i < 4; ++i) {
//            System.out.println("remove#2 " + s.remove(i));
//        }
//        for (int i = 0; i < 4; ++i) {
//            System.out.println("contains#2 " + s.contains(i));
//        }
//        for (int i = 0; i < 30; ++i) {
//            System.out.println("remove#3 " + s.remove(i));
//        }
//        print(s);
    }

    private interface INode {
    }

    static private class Node implements INode {
        final AtomicRef<INode> next;
        final int x;

        Node(int x, INode next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }

    static private class Removed implements INode {
        Removed(Node n) {
            assert (n != null);
            next = n;
        }

        final Node next;
    }

    static private class Window {
        Window(Node c, Node n) {
            left = c;
            right = n;
        }

        Node left, right;
    }

    private final Node head = new Node(Integer.MIN_VALUE, new Node(Integer.MAX_VALUE, null));

    /**
     * Returns the {@link Window}, where cur.x < x <= next.x
     */
    private Window findWindow(int x) {
        retry:
        while (true) { // retry operation
            Node cur = head;
            Node next = (Node)cur.next.getValue();            // *** READ
            while (true) { // find window
                INode overNext = next.next.getValue();
                if (overNext instanceof Removed) {
                    Node realOverNext = ((Removed) overNext).next;
                    if (!cur.next.compareAndSet(next, realOverNext)) {
                        continue retry;
                    }
                    next = realOverNext;
                } else {
                    Node realNext = next;
                    int key = realNext.x;
                    if (x <= key) {
                        return new Window(cur, realNext);
                    }
                    cur = realNext;
                    // maybe fail ; next = (Node)cur.next.getValue();     // *** READ
                    INode n = cur.next.getValue();
                    if (n instanceof Removed) {
                        next = ((Removed) n).next;
                    } else {
                        next = (Node)n;
                    }
                }
            }
        }
    }

    @Override
    public boolean add(int x) {
        while (true) {
            Window w = findWindow(x);
            if (w.right.x == x) {
                return false;
            } else {
                Node node = new Node(x, w.right);
                if (w.left.next.compareAndSet(w.right, node)) {
                    return true;
                }
            }
        }
    }

    @Override
    public boolean remove(int x) {
        while (true) {
            Window w = findWindow(x);
            if (w.right.x != x) {
                return false;
            } else {
                INode node = w.right.next.getValue(); // *** READ
                if (node instanceof Removed) {
                    continue;
                }
                // по идее этой строкой мы теряем изменения, если они произошли между созданием removed
                // и касом
                INode removed = new Removed((Node)node);
                // создание removed и cas должна быть атомарны...
                if (w.right.next.compareAndSet(node, removed)) {
//                    w.left.next.compareAndSet(removed, node); // ??
                    return true;
                }
//                if (w.cur.next.compareAndSet(w.next, node)) {
//                    return true;
//                }
            }
        }
    }

    @Override
    public boolean contains(int x) {
        Window w = findWindow(x);
        return w.right.x == x;
    }
}