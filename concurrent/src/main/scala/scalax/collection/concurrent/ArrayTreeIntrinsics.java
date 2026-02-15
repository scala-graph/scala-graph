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

        public static int incrAndGet(final ArrayTree<?> instance) {
            return (int) HANDLE.getAndAdd(instance, 1) + 1;
        }

        public static int get(final ArrayTree<?> instance) {
            return (int) HANDLE.getVolatile(instance);
        }
    }

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
        public static int get(final ArrayTree.MultiLeaf<?> instance) {
            return (int) HANDLE.getVolatile(instance);
        }

        @SuppressWarnings("ClassEscapesDefinedScope")
        public static boolean compareAndSet(final ArrayTree.MultiLeaf<?> instance, final int current, final int newValue) {
            return (boolean) HANDLE.compareAndSet(instance, current, newValue);
        }

        @SuppressWarnings("ClassEscapesDefinedScope")
        public static boolean compareAndIncr(final ArrayTree.MultiLeaf<?> instance, final int current) {
            return (boolean) HANDLE.compareAndSet(instance, current, current + 1);
        }
    }

    public static final class UpperNodeUsedH {
        private static final VarHandle HANDLE;

        static {
            try {
                // noinspection JavaLangInvokeHandleSignature
                HANDLE = MethodHandles.privateLookupIn(ArrayTree.UpperNode.class, lookup)
                        .findVarHandle(ArrayTree.UpperNode.class, "_used", int.class);
            } catch (ReflectiveOperationException e) {
                throw new Error("UpperNode lookup failed.", e);
            }
        }

        @SuppressWarnings("ClassEscapesDefinedScope")
        public static int get(final ArrayTree.UpperNode<?> instance) {
            return (int) HANDLE.getVolatile(instance);
        }

        @SuppressWarnings("ClassEscapesDefinedScope")
        public static boolean compareAndIncr(final ArrayTree.UpperNode<?> instance, final int current) {
            return (boolean) HANDLE.compareAndSet(instance, current, current + 1);
        }
    }

    public static final class LeafParentNodeUsedH {
        private static final VarHandle HANDLE;

        static {
            try {
                // noinspection JavaLangInvokeHandleSignature
                HANDLE = MethodHandles.privateLookupIn(ArrayTree.LeafParentNode.class, lookup)
                        .findVarHandle(ArrayTree.LeafParentNode.class, "_used", int.class);
            } catch (ReflectiveOperationException e) {
                throw new Error("LeafParentNode lookup failed.", e);
            }
        }

        @SuppressWarnings("ClassEscapesDefinedScope")
        public static int get(final ArrayTree.LeafParentNode<?> instance) {
            return (int) HANDLE.getVolatile(instance);
        }

        @SuppressWarnings("ClassEscapesDefinedScope")
        public static boolean compareAndIncr(final ArrayTree.LeafParentNode<?> instance, final int current) {
            return (boolean) HANDLE.compareAndSet(instance, current, current + 1);
        }
    }
}
