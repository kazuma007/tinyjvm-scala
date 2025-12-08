public class NestedLoop {
    public static int compute() {
        int sum = 0;
        int i = 1;
        while (i <= 4) {
            int j = 1;
            while (j <= 4) {
                sum = sum + i * j;
                j = j + 1;
            }
            i = i + 1;
        }
        return sum;
    }
}
