package org.example

import junit.framework.Assert.assertEquals
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.json.*
import org.junit.Test

class EverythingTest {
    private val testRecursiveClass = RecursiveClass(
        id = 0,
        flag = true,
        children = (1..10).map { RecursiveClass(id = it, flag = it % 2 == 0, children = listOf()) }
    )

    @OptIn(ExperimentalSerializationApi::class)
    private val testRoot = Root(
        id = "id long",
        await = false,
        xInit = 3.14,
        items = listOf(listOf(1,2), listOf(-1,-2)),
        maybe = 422,
        extras = listOf(34, 12),
        answer = 12,
        aliased = Alias(listOf(55, 44)),
        point = Pair(4.4, 1.1),
        kinds = listOf(Kind.Amaze(listOf("one", "two")), Kind.Root_, Kind.Thing(1)),
        assoc1 = listOf(Pair(4.12, 1), Pair(2.2, 2)),
        assoc2 = listOf(Pair("first", 1), Pair("second", 2)),
        assoc3 = mapOf(Pair(1.1, 1), Pair(2.2, 2)),
        assoc4 = mapOf(Pair("firstt", 1), Pair("secondd", 2)),
        nullables = listOf(1, null, 3),
        options = listOf(1, 2, null),
        parametrizedRecord = IntFloatParametrizedRecord(2, listOf(1.0, 1.1)),
        parametrizedTuple = KindParametrizedTuple(Triple(Kind.Root_, Kind.WOW, 9)),
        untypedThings = listOf(buildJsonObject { put("object1", true) }, buildJsonObject { putJsonArray("object2") { addAll(listOf(1,2,3)) } })
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


    @Test
    fun test_root_class_json_string() {
        // GIVEN
        val target = testRoot
        // WHEN
        val encodedString: String = target.toJsonString()
        val decodedType: Root = Root.fromJsonString(encodedString)
        // THEN
        assertEquals(target, decodedType)
    }

    @Test
    fun test_root_class_json() {
        // GIVEN
        val target = testRoot
        // WHEN
        val encodedJson: JsonElement = target.toJson()
        val decodedType: Root = Root.fromJson(encodedJson)
        // THEN
        assertEquals(target, decodedType)
    }

    @Test
    fun test_enum_class_json_string() {
        // GIVEN
        val target = EnumSumtype.A
        // WHEN
        val encodedString: String = target.toJsonString()
        val decodedType: EnumSumtype = EnumSumtype.fromJsonString(encodedString)
        // THEN
        assertEquals(target, decodedType)
    }

    @Test
    fun test_enum_class_json() {
        // GIVEN
        val target = EnumSumtype.A
        // WHEN
        val encodedJson: JsonElement = target.toJson()
        val decodedType: EnumSumtype = EnumSumtype.fromJson(encodedJson)
        // THEN
        assertEquals(target, decodedType)
    }
}
