import java.util.concurrent.atomic.*;

public class Solution implements Lock<Solution.Node> {
    private final Environment env;

    // todo: необходимые поля (final, используем AtomicReference)
    final AtomicReference<Node> tail = new AtomicReference<>(null);

    public Solution(Environment env) {
        this.env = env;
    }

    @Override
    public Node lock() {
        Node my = new Node(); // сделали узел
        my.locked.set(true);
        Node pred = tail.getAndSet(my);
        if (pred != null) {
            pred.next.set(my);
            while (my.locked.get()) env.park();
        }
        return my; // вернули узел
    }

    @Override
    public void unlock(Node my) {
        Node next = my.next.get();
        if (next != null) {
            next.locked.set(false);
            env.unpark(next.thread);
        } else {
            if (!tail.compareAndSet(my, null)) {
                while (next == null) {
                    next = my.next.get();
                }
                next.locked.set(false);
                env.unpark(next.thread);
            }
        }
    }

    static class Node {
        final AtomicReference<Boolean> locked = new AtomicReference<>(false);
        final AtomicReference<Node> next = new AtomicReference<>(null);
        final Thread thread = Thread.currentThread(); // запоминаем поток, которые создал узел
        // todo: необходимые поля (final, используем AtomicReference)
    }
}
