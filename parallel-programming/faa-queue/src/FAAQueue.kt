import kotlinx.atomicfu.*
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

class FAAQueue<T> {
    private val head: AtomicRef<Segment> // Head pointer, similarly to the Michael-Scott queue (but the first node is _not_ sentinel)
    private val tail: AtomicRef<Segment> // Tail pointer, similarly to the Michael-Scott queue

    init {
        val firstNode = Segment()
        head = atomic(firstNode)
        tail = atomic(firstNode)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(x: T) {
        while (true) {
            val localTail = tail.value;
            // val enqIdx = localTail.enqIdx.incrementAndGet();
            val enqIdx = localTail.enqIdx.getAndIncrement();
            if (enqIdx >= SEGMENT_SIZE) {
                val newTail = Segment(x)
                if (localTail.next.compareAndSet(null, newTail)) {
                    tail.compareAndSet(localTail, newTail)
                    return
                } else {
                    tail.compareAndSet(localTail, localTail.next.get())
                }
            } else {
                if (localTail.elements.compareAndSet(enqIdx, null, x)) {
                    return
                }
            }
        }
    }

    /**
     * Retrieves the first element from the queue
     * and returns it; returns `null` if the queue
     * is empty.
     */
    fun dequeue(): T? {
        while (true) {
            val localHead = head.value;
            val deqIdx = localHead.deqIdx.getAndIncrement();
            if (deqIdx >= SEGMENT_SIZE) {
                val nextNode = localHead.next.get()
                val localTail = tail.value
                if (nextNode == null) {
                    return null
                }
                if (localTail == localHead) {
                    tail.compareAndSet(localTail, nextNode)
                } else {
                    head.compareAndSet(localHead, nextNode);
                }
//                if (head.value.next == null) return null
//                head.value = head.value.next!!
//                continue
                // TODO
            } else {
                val res = localHead.elements.getAndSet(deqIdx, DONE);
                if (res != null) {
                    return res as T?
                }
            }
        }
    }

    /**
     * Returns `true` if this queue is empty;
     * `false` otherwise.
     */
    val isEmpty: Boolean get() {
        while (true) {
            val localHead = head.value
            if (localHead.isEmpty) {
                if (localHead.next.get() == null) return true
                head.compareAndSet(localHead, localHead.next.get());
                continue
            } else {
                return false
            }
        }
    }
}

private class Segment {
    val next = AtomicReference<Segment>(null)
    val enqIdx = atomic(0) // index for the next enqueue operation
    val deqIdx = atomic(0) // index for the next dequeue operation
    val elements = AtomicReferenceArray<Any?>(SEGMENT_SIZE)

    constructor() // for the first segment creation

    constructor(x: Any?) { // each next new segment should be constructed with an element
        enqIdx.value = 1
        elements[0] = x
    }

    val isEmpty: Boolean get() {
        val local = deqIdx.value
        return local >= enqIdx.value || local >= SEGMENT_SIZE
    }
}

private val DONE = Any() // Marker for the "DONE" slot state; to avoid memory leaks
const val SEGMENT_SIZE = 2 // DO NOT CHANGE, IMPORTANT FOR TESTS

