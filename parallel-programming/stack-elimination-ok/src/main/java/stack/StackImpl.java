package stack;

import kotlinx.atomicfu.AtomicBoolean;
import kotlinx.atomicfu.AtomicRef;

import java.util.*;

public class StackImpl implements Stack {

    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(final int x, final Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }

    private static class EliminationNode {
        EliminationNode(final int x) {
            hasValue = new AtomicBoolean(true);
            value = x;
        }
        final AtomicBoolean hasValue;
        final int value;
    }

    private static final int ELIMINTAION_ARRAY_SIZE = 4;
    private static final int ELIMINTAION_LOOPS = 1000;
    private static final int ELIMINATION_NEIGHBOURS = 2;
    private final Random random = new Random();

    // head pointer
    private final AtomicRef<Node> head = new AtomicRef<>(null);
    private final List<AtomicRef<EliminationNode>> eliminationArray =
            new ArrayList<AtomicRef<EliminationNode>>(Collections.nCopies(ELIMINTAION_ARRAY_SIZE,
                    new AtomicRef<EliminationNode>(null)));

    @Override
    public void push(final int x) {
        if (eliminatePush(x)) {
            return;
        }
        while (true) {
            final Node h = head.getValue();
            final Node node = new Node(x, h);
            if (head.compareAndSet(h, node)) {
                return;
            }
        }
    }

    @Override
    public int pop() {
        final int start_idx = random.nextInt(eliminationArray.size());
        for (int j = 0; j < ELIMINATION_NEIGHBOURS; ++j) {
            final int idx = (start_idx + j) % eliminationArray.size();
            final EliminationNode node = eliminationArray.get(idx).getValue();
            if (node != null) {
                if (node.hasValue.compareAndSet(true, false)) {
                    return node.value;
                }
            }
        }
        while (true) {
            final Node curHead = head.getValue();
            if (curHead == null) return Integer.MIN_VALUE;
            final Node next = curHead.next.getValue();
            if (head.compareAndSet(curHead, next))
                return curHead.x;
        }
    }

    private boolean eliminatePush(final int x) {
        final int start_idx = random.nextInt(eliminationArray.size());
        final EliminationNode node = new EliminationNode(x);
        for (int j = 0; j < ELIMINATION_NEIGHBOURS; ++j) {
            final int idx = (start_idx + j) % eliminationArray.size();
            final AtomicRef<EliminationNode> ref = eliminationArray.get(idx);
            if (ref.compareAndSet(null, node)) {
                int iter = ELIMINTAION_LOOPS;
                while (iter-- != 0) {
                    if (!node.hasValue.getValue()) {
                        break;
                    }
                }
                ref.setValue(null);
                return !node.hasValue.compareAndSet(true, false);
            }
        }
        return false;
    }
}
