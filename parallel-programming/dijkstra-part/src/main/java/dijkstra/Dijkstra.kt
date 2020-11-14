package dijkstra

import kotlinx.atomicfu.AtomicInt
import kotlinx.atomicfu.atomic
import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.PriorityBlockingQueue
import java.util.concurrent.atomic.AtomicInteger
import kotlin.Comparator
import kotlin.collections.ArrayList
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

class MultiQueue(nQueues: Int, private var comparator: java.util.Comparator<Node>) {
    private var queues: List<PriorityBlockingQueue<Node>>
    private var r = Random();

    init {
        val qs = ArrayList<PriorityBlockingQueue<Node>>(nQueues)
        for (i in 0..nQueues) {
            qs.add(PriorityBlockingQueue<Node>(1000, comparator));
        }
        queues = qs;
    }

    fun add(v : Node) {
        val queue = queues.get(r.nextInt(queues.size));
        queue.add(v);
    }

    fun poll() : Node? {
        val m1 = queues[r.nextInt(queues.size)].poll()
        val m2 = queues[r.nextInt(queues.size)].poll()
        if (m1 == null) return m2
        if (m2 == null) return m1
        var ret = m1
        var back = m2
        if (comparator.compare(m2, m1) == -1) {
            val tmp = ret
            ret = back
            back = tmp
        }
        add(back);
        return ret;
    }
}

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = MultiQueue(2 * workers, NODE_DISTANCE_COMPARATOR) // TODO replace me with a multi-queue based PQ!
    // val q = PriorityBlockingQueue<Node>(1000, NODE_DISTANCE_COMPARATOR);
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    val activeNodes = AtomicInteger(1)
    repeat(workers) {
        thread {
            while (activeNodes.get() > 0) {
                // TODO Write the required algorithm here,
                // TODO break from this loop when there is no more node to process.
                // TODO Be careful, "empty queue" != "all nodes are processed".
                val cur = q.poll()
                if (cur == null) {
                    // if (workIsDone) break else continue
                    continue;
                }
                for (e in cur.outgoingEdges) {
                    while (true) {
                        val curDist = cur.distance
                        val oldDist = e.to.distance
                        if (oldDist > curDist + e.weight) {
                            // oldDist = curDist + e.weight
                            if (e.to.casDistance(oldDist, curDist + e.weight)) {
                                activeNodes.incrementAndGet();
                                q.add(e.to);
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                activeNodes.decrementAndGet();
//                val cur: Node? = synchronized(q) { q.poll() }
//                if (cur == null) {
//                    if (workIsDone) break else continue
//                }
//                for (e in cur.outgoingEdges) {
//                    if (e.to.distance > cur.distance + e.weight) {
//                        e.to.distance = cur.distance + e.weight
//                        q.addOrDecreaseKey(e.to)
//                    }
//                }
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}