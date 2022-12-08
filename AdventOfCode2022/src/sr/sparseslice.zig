fn SparseSlice(comptime T: type) type {
    return struct {
        const Self = @This();

        base: [*]T,
        stride: usize,
        len: usize,

        pub fn fromSlice(slice: []T) Self {
            return Self{
                .base = slice.ptr,
                .stride = 1,
                .len = slice.len,
            };
        }

        const Iterator = struct {
            slice: Self,
            i: usize = 0,

            pub fn next(self: *Iterator) ?*T {
                if (self.i < self.slice.len) {
                    const ptr = &self.slice.base[self.i * self.slice.stride];
                    self.i += 1;
                    return ptr;
                } else {
                    return null;
                }
            }
        };

        pub fn iterate(self: Self) Iterator {
            return Iterator{
                .slice = self,
            };
        }
    };
}
