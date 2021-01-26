import kotlinx.atomicfu.atomic
import java.util.*
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

enum class OperationType {
    ADD, PEEK, POLL
}

class Operation<E>(v: E, opType: OperationType) {
    val type = opType;
    val value = AtomicReference<E>(v)
    val executed = AtomicBoolean(false)
}

class FCPriorityQueue<E : Comparable<E>> {
    private val q = PriorityQueue<E>()
    private val lock = atomic(false)
    private val rand = Random()
    private val operations = AtomicReferenceArray<Operation<E?>>(3)

    /**
     * Retrieves the element with the highest priority
     * and returns it as the result of this function;
     * returns `null` if the queue is empty.
     */
    fun poll(): E? {
        val op = Operation<E?>(null, OperationType.POLL);
        run(op)
//        assert(op.ex
//        ecuted.value)
        return op.value.get()
    }

    /**
     * Returns the element with the highest priority
     * or `null` if the queue is empty.
     */
    fun peek(): E? {
        val op = Operation<E?>(null, OperationType.PEEK);
        run(op)
//        assert(op.executed.value)
        return op.value.get()
    }

    /**
     * Adds the specified element to the queue.
     */
    fun add(element: E) {
        val op = Operation<E?>(element, OperationType.ADD);
        run(op)
//        assert(op.executed.value)
    }

    private fun run(op: Operation<E?>) {
        var operationIdx = -1;
        while (true) {
            if (lock.compareAndSet(false, true)) {
                try {
                    if (operationIdx != -1) {
                        operations.set(operationIdx, null)
                    }
                    if (!op.executed.get()) {
                        executeTask(op)
                    }
                    combine()
                } finally {
                    lock.value = false;
                    // lock.unlock()
                }
                return
            } else {
                if (operationIdx == -1) {
                    val idx = rand.nextInt(operations.length())
                    if (operations.compareAndSet(idx, null, op)) {
                        operationIdx = idx;
                    }
                } else {
                    if (op.executed.get()) {
                        operations.set(operationIdx, null);
                        return
                    } else {
                        Thread.yield()
                    }
                }
            }
        }
    }

    private fun combine() {
        for (i in 0 until operations.length()) {
            val op = operations.get(i);
            if (op != null && !op.executed.get()) {
                executeTask(op)
            }
        }
    }

    private fun executeTask(operation: Operation<E?>) {
        assert(!operation.executed.get());
//        if (operation.executed.get()) return
        if (operation.type == OperationType.ADD) {
            q.add(operation.value.get())
        } else if (operation.type == OperationType.PEEK) {
            operation.value.set(q.peek())
        } else if (operation.type == OperationType.POLL) {
            operation.value.set(q.poll())
        }
        operation.executed.set(true)
    }
}