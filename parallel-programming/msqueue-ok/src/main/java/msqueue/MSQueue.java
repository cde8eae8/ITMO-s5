package msqueue;

import kotlinx.atomicfu.AtomicRef;

public class MSQueue implements Queue {
    private AtomicRef<Node> head;
    private AtomicRef<Node> tail;

    public MSQueue() {
        Node dummy = new Node(0);
        this.head = new AtomicRef<>(dummy);
        this.tail = new AtomicRef<>(dummy);
    }

    @Override
    public void enqueue(int x) {
        Node newTail = new Node(x);
        while (true) {
            Node curTail = tail.getValue();
            if (curTail.next.compareAndSet(null, newTail)) {
                tail.compareAndSet(curTail, newTail);
                break;
            } else {
                tail.compareAndSet(curTail, curTail.next.getValue());
            }
        }
    }

    @Override
    public int dequeue() {
        while (true) {
            Node curHead = head.getValue();
            Node nextNode = curHead.next.getValue();
            Node localTail = tail.getValue();
            if (nextNode == null) {
                return Integer.MIN_VALUE;
            }
            if (localTail == curHead) {
                tail.compareAndSet(localTail, nextNode);
            } else {
                if (head.compareAndSet(curHead, nextNode)) {
                    return nextNode.x;
                }
            }
        }
    }

    @Override
    public int peek() {
        Node curHead = head.getValue();
        Node next = curHead.next.getValue();
        if (next == null)
            return Integer.MIN_VALUE;
        return next.x;
    }

    private static class Node {
        final int x;
        AtomicRef<Node> next;

        Node(int x) {
            this.x = x;
            this.next = new AtomicRef<Node>(null);
        }
    }
}