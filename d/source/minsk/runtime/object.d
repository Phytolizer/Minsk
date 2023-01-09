module minsk.runtime.object;

import std.conv : to;

enum Type {
    Integer,
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
}
