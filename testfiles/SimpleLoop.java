public class SimpleLoop {
    public static int sumToN() {
        int i = 1;
        int sum = 0;
        int n = 5;

        while (i <= n) {
            sum = sum + i;
            i = i + 1;
        }

        return sum;
    }
}
