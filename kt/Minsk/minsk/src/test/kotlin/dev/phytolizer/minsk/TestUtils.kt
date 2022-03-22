package dev.phytolizer.minsk

object TestUtils {
    fun <T> cartesianProduct(a: List<T>, b: List<T>, vararg lsts: List<T>): List<List<T>> =
        (listOf(a, b).plus(lsts)).fold(listOf(listOf())) { acc, lst ->
            acc.flatMap { sublst -> lst.map { sublst + it } }
        }
}
