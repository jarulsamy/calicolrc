Vector = {}
Vector.__index = Vector            --> Allows vector.add to find add in Vector

function Vector.new (x, y)
    local vector = {}
    setmetatable(vector, Vector)   --> Allows the look up of __add to be found in Vector
    vector.x = x;
    vector.y = y;
    return vector;
end

function Vector:__add (v2)
     return Vector.new(self.x + v2.x, self.y + v2.y);
end

function Vector:add (v2)
     return Vector.new(self.x + v2.x, self.y + v2.y);
end

--[[

v1 = Vector.new(0, 0)
v2 = Vector.new(10, -4)

v3 = v1 + v2
v4 = v1:add(v3)

--]]