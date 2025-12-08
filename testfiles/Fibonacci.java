public class Fibonacci {
    public static int fib(int n) {
        if (n <= 1) {
            return n;
        }
        int a = 0;
        int b = 1;
        int i = 2;
        while (i <= n) {
            int temp = a + b;
            a = b;
            b = temp;
            i = i + 1;
        }
        return b;
    }

    public static int compute() {
        return fib(7);
    }
}
