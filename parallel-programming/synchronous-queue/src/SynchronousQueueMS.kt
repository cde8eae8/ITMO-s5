import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

class SynchronousQueueMS<E> : SynchronousQueue<E> {
    private val head : AtomicRef<Node<E?>?> = atomic(null)
    private val tail : AtomicRef<Node<E?>?> = atomic(null)

    init {
        val dummy = Node<E?>(null, null, Type.DUMMY)
        head.value = dummy
        tail.value = dummy
    }

    override suspend fun send(element: E) : E {
        while (true) {
            val localHead = head.value
            val nextNode = localHead!!.next.value
            val localTail = tail.value
            if (localHead == localTail || localTail!!.type == Type.SENDER || localTail.type == Type.DUMMY) {
                // TODO
                val res = suspendCoroutine<Any?> sc@ { cont ->
                    val newNode = Node<E?>(element, cont, Type.SENDER)
                    if (tryEnqueue(newNode, localTail) == null) {
                        cont.resume(RETRY)
                        return@sc
                    }
                }
                if (res === RETRY) continue
                // assert(res == element)
                return res as E
            } else {
                val node = tryDequeue(localHead, nextNode, localTail)
                if (node == null) {
                    continue
                }
                assert(node.type != Type.DUMMY)
                assert(node.type != Type.SENDER)
                node.cor!!.resume(element as Any)
                return element
            }
        }
    }

    override suspend fun receive(): E {
        while (true) {
            val localHead = head.value
            val nextNode = localHead!!.next.value
            val localTail = tail.value
            if (localHead == localTail || localTail!!.type == Type.RECEIVER || localTail.type == Type.DUMMY) {
                val res = suspendCoroutine<Any?> sc@ { cont ->
                    val newNode = Node<E?>(null, cont, Type.RECEIVER)
                    if (tryEnqueue(newNode, localTail) == null) {
                        cont.resume(RETRY)
                        return@sc
                    }
                }
                if (res === RETRY) {
                    continue
                }
                return res as E
            } else {
                // if (nextNode!!.type == Type.RECEIVER) continue
                val node = tryDequeue(localHead, nextNode, localTail)
                if (node == null) {
                    continue
                }
                assert(node.type != Type.DUMMY)
                assert(node.type == Type.SENDER)
                node.cor!!.resume(node.value!!)
                return node.value!!
            }
        }
    }

    private fun tryEnqueue(newTail : Node<E?>, localTail: Node<E?>?) : Node<E?>? {
        if (localTail!!.next.compareAndSet(null, newTail)) {
            tail.compareAndSet(localTail, newTail)
            return newTail
        } else {
            tail.compareAndSet(localTail, localTail.next.value)
        }
        return null
    }

    private fun tryDequeue(localHead: Node<E?>?, nextNode : Node<E?>?, localTail: Node<E?>?) : Node<E?>? {
        // val nextNode = localHead!!.next.value
        if (nextNode == null) {
            return null
        }
        if (localTail === localHead) {
            tail.compareAndSet(localTail, nextNode)
        } else {
            if (head.compareAndSet(localHead, nextNode)) {
                return nextNode
            }
        }
        return null
    }

    private enum class Type {
        RECEIVER, SENDER, DUMMY
    }

    private class Node<E> constructor(x: E, cont: Continuation<Any?>?, t: Type) {
        val next : AtomicRef<Node<E?>?> = atomic(null)
        val value = x
        val cor : Continuation<Any?>? = cont
        val type = t
    }
}

private val RETRY = Any()