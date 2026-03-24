import java.util.Random;

public class QuickSort {

    // main sort method
    public static void sort(Comparable[] a) {
        shuffle(a);
        sort(a, 0, a.length - 1);
    }

    // recursive sort method
    private static void sort(Comparable[] a, int lo, int hi) {
        if (hi <= lo) return;
        int j = partition(a, lo, hi);
        sort(a, lo, j - 1);
        sort(a, j + 1, hi);
    }

    // partition method
    private static int partition(Comparable[] a, int lo, int hi) {
        int i = lo;
        int j = hi + 1;
        Comparable v = a[lo]; // Partitioning item

        while (true) {
            // Scan right, stop if item < v
            while (less(a[++i], v)) {
                if (i == hi) break;
            }
            // Scan left, stop if item > v
            while (less(v, a[--j])) {
                if (j == lo) break;
            }
            // Check if pointers cross
            if (i >= j) break;

            exch(a, i, j);
        }
        exch(a, lo, j); // Put partitioning item v at a[j]
        return j;
    }

    //helper functions
    private static void shuffle(Comparable[] a) {
        Random random = new Random();
        int n = a.length;
        for (int i = 0; i < n; i++) {
            int r = i + random.nextInt(n - i);
            exch(a, i, r);
        }
    }

    private static boolean less(Comparable v, Comparable w) {
        return v.compareTo(w) < 0;
    }

    private static void exch(Comparable[] a, int i, int j) {
        Comparable swap = a[i];
        a[i] = a[j];
        a[j] = swap;
    }

    public static void show(Comparable[] a) {
        for (Comparable c : a) {
            System.out.print(c + " ");
        }
        System.out.println();
    }
}
