import kotlinx.atomicfu.atomic
import java.lang.IllegalArgumentException
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY))
    private val mSize = atomic(0)

    override fun get(index: Int): E {
        var version = findVersionWithIndex(index)
        while (true) {
            val value = version.array.get(index)
//            assert(value != null)
            if (value == MOVED) {
                val newVersion = version.next.get()
                assert(newVersion != null)
                version = version.next.get()!!
            } else if (value == null) {
                continue
            } else {
                return value as E
            }
        }
    }

    override fun put(index: Int, element: E) {
        var version = findVersionWithIndex(index)
        while (true) {
            val value = version.array.get(index)
            if (value == MOVED) {
                version = version.next.get()!!
            } else {
                if (version.array.compareAndSet(index, value, element)) {
                    return
                }
            }
        }
    }

    override fun pushBack(element: E) {
        var version : Core<E> = core.value;
//        println(element)
        while (true) {
            val idx = mSize.value
            if (!(idx < version.cap)) {
                val nextVersion = version.next.get()
                if (nextVersion != null) {
//                    println("next")
                    version = nextVersion
                    continue
                } else {
//                    println("move")
                    val newVersion = Core<E>(version.cap * 2)
                    newVersion.array.set(version.cap, element)
                    if (version.next.compareAndSet(null, newVersion)) {
//                        println("new version" + newVersion.cap)
                        // move
                        for (i in 0 until version.cap) {
                            val value = version.array.getAndSet(i, MOVED)
                            if (value == MOVED) {
                                assert(false)
                            } else {
                                newVersion.array.compareAndSet(i, null, value)
                            }
                        }
//                        println("exit" + newVersion.cap)
//                        core.compareAndSet(version, newVersion)
                        mSize.getAndIncrement()
                        return
                    }
                }
            } else {
//                println("insert" + size + version.cap)
                if (version.array.compareAndSet(idx, null, element)) {
                    mSize.getAndIncrement()
                    return
                } else {
                    // was moved...
                    val nextVersion = version.next.get()
                    if (nextVersion != null) {
                        version = nextVersion
                        continue
                    }
                }
            }
        }
    }

    override val size: Int
        get() {
            return mSize.value
        }

    private fun findVersionWithIndex(index : Int) : Core<E> {
        if (index >= mSize.value) throw IllegalArgumentException()
        var version : Core<E> = core.value;
        while (!(index < version.cap)) {
            val nextVersion = version.next.get()
            if (nextVersion != null) {
                version = nextVersion
            }
        }
        return version
    }
}

private val MOVED = Any()

private class Core<E>(capacity: Int) {
    val cap = capacity
    val next = AtomicReference<Core<E>?>(null)
    val array = AtomicReferenceArray<Any?>(capacity)
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME