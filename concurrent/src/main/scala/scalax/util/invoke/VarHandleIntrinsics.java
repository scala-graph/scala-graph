package scalax.util.invoke;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;

/* Java-native calls of `VarHandle` methods to avoid Object[] allocation and more.
 */
final class VarHandleIntrinsics {
    private static final VarHandle INT_ARRAY_HANDLE = MethodHandles.arrayElementVarHandle(int[].class);
    private static final VarHandle LONG_ARRAY_HANDLE = MethodHandles.arrayElementVarHandle(long[].class);
    private static final VarHandle OBJECT_ARRAY_HANDLE = MethodHandles.arrayElementVarHandle(Object[].class);

    static int getInt(int[] array, int index) {
        return (int) INT_ARRAY_HANDLE.get(array, index);
    }

    static int getAcquireInt(int[] array, int index) {
        return (int) INT_ARRAY_HANDLE.getAcquire(array, index);
    }

    static void setReleaseInt(int[] array, int index, int value) {
        INT_ARRAY_HANDLE.setRelease(array, index, value);
    }

    static long getLong(long[] array, int index) {
        return (long) LONG_ARRAY_HANDLE.get(array, index);
    }

    static long getAcquireLong(long[] array, int index) {
        return (long) LONG_ARRAY_HANDLE.getAcquire(array, index);
    }

    static void setReleaseLong(long[] array, int index, long value) {
        LONG_ARRAY_HANDLE.setRelease(array, index, value);
    }

    static Object getObject(Object[] array, int index) {
        return (Object) OBJECT_ARRAY_HANDLE.get(array, index);
    }

    static Object getAcquireObject(Object[] array, int index) {
        return (Object) OBJECT_ARRAY_HANDLE.getAcquire(array, index);
    }

    static void setReleaseObject(Object[] array, int index, Object value) {
        OBJECT_ARRAY_HANDLE.setRelease(array, index, value);
    }
}
