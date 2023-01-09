module minsk.runtime.object;

import std.conv : to;

enum Type {
    Integer,
    Boolean,
}

interface Obj {
    Type type() const @safe pure nothrow;
    string toString() const @safe pure nothrow;
}

class Integer : Obj {
    int value;

    this(int value) {
        this.value = value;
    }

    override Type type() const @safe pure nothrow {
        return Type.Integer;
    }

    override string toString() const @safe pure nothrow {
        return value.to!string;
    }

    override bool opEquals(Object b) {
        const ib = cast(Integer) b;
        return ib !is null && ib.value == value;
    }
}

class Boolean : Obj {
    bool value;

    this(bool value) {
        this.value = value;
    }

    override Type type() const @safe pure nothrow {
        return Type.Boolean;
    }

    override string toString() const @safe pure nothrow {
        return value ? "true" : "false";
    }

    override bool opEquals(Object b) {
        const bb = cast(Boolean) b;
        return bb !is null && bb.value == value;
    }
}
