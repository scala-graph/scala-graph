package scalax.collection.concurrent;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;

final class ArrayTreeIntrinsics {
    private static final MethodHandles.Lookup lookup = MethodHandles.lookup();

    public static final class SizeH {
        private static final VarHandle HANDLE;

        static {
            try {
                // noinspection JavaLangInvokeHandleSignature
                HANDLE = MethodHandles.privateLookupIn(ArrayTree.class, lookup)
                        .findVarHandle(ArrayTree.class, "_size", int.class);
            } catch (ReflectiveOperationException e) {
                throw new Error("ArrayTree lookup failed.", e);
            }
        }

        public static int incrementAndGet(ArrayTree<?> instance) {
            return (int) HANDLE.getAndAdd(instance, 1) + 1;
        }

        public static int get(ArrayTree<?> instance) {
            return (int) HANDLE.getVolatile(instance);
        }
    }

/*
    public static final class MultiLeafUsedH {
        private static final VarHandle HANDLE;

        static {
            try {
                // noinspection JavaLangInvokeHandleSignature
                HANDLE = MethodHandles.privateLookupIn(ArrayTree.MultiLeaf.class, lookup)
                        .findVarHandle(ArrayTree.MultiLeaf.class, "_used", int.class);
            } catch (ReflectiveOperationException e) {
                throw new Error("MultiLeaf lookup failed.", e);
            }
        }

        @SuppressWarnings("ClassEscapesDefinedScope")
        public static int incrementAndGet(ArrayTree.MultiLeaf<?> instance) {
            return (int) HANDLE.getAndAdd(instance, 1) + 1;
        }

        @SuppressWarnings("ClassEscapesDefinedScope")
        public static int get(ArrayTree.MultiLeaf<?> instance) {
            return (int) HANDLE.getVolatile(instance);
        }
    }
*/
}
