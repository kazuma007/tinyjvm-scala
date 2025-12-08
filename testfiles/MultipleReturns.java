public class MultipleReturns {
    public static int checkCondition() {
        int x = 20;
        if (x > 15) {
            return 42;
        }
        int y = x * 2;
        if (y > 30) {
            return 99;
        }
        return 0;
    }
}
