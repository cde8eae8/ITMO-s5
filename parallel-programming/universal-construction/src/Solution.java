/**
 * @author Дроздов Никита
 */
public class Solution implements AtomicCounter {
    // объявите здесь нужные вам поля
    final Node first = new Node(0);
    final ThreadLocal<Node> current = new ThreadLocal<>();

    public int getAndAdd(int x) {
        // напишите здесь код
        if (current.get() == null)
            current.set(first);
        Node local_next;
        int v;
        while (true) {
            v = current.get().value;
            local_next = new Node(v + x);
            Node next = current.get().cons.decide(local_next);
            current.set(next);
            if (next == local_next) {
                break;
            }
        }
        return v;
    }

    // вам наверняка потребуется дополнительный класс
    private static class Node {
        Node(int n) {
            value = n;
            cons = new Consensus<>();
        }
        final int value;
        final Consensus<Node> cons;
    }
}
