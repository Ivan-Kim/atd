package org.example

import junit.framework.Assert.assertEquals
import kotlinx.serialization.json.JsonElement
import org.junit.Test

class EverythingTest {
    private val testRecursiveClass = RecursiveClass(
        id = 0,
        flag = true,
        children = (1..10).map { RecursiveClass(id = it, flag = it % 2 == 0, children = listOf()) }
    )

    @Test
    fun test_recursive_class_json_string() {
        // GIVEN
        val target = testRecursiveClass
        // WHEN
        val encodedString: String = target.toJsonString()
        val decodedType: RecursiveClass = RecursiveClass.fromJsonString(encodedString)
        // THEN
        assertEquals(target, decodedType)
    }

    @Test
    fun test_recursive_class_json() {
        // GIVEN
        val target = testRecursiveClass
        // WHEN
        val encodedJson: JsonElement = target.toJson()
        val decodedType: RecursiveClass = RecursiveClass.fromJson(encodedJson)
        // THEN
        assertEquals(target, decodedType)
    }
}