module minsk.runtime.object;

import std.conv : to;

interface Obj {
    string toString() const @safe pure nothrow;
}

class Integer : Obj {
    int value;

    this(int value) {
        this.value = value;
    }

    override string toString() const @safe pure nothrow {
        return value.to!string;
    }
}
