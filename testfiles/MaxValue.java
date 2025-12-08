public class MaxValue {
    public static int max(int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    public static int findMax() {
        return max(15, 23);
    }
}
