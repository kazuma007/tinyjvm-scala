public class Program {

    public static int square(int x) {
        return x * x;
    }

    public static int sumToN(int n) {
        int i = 1;
        int sum = 0;

        while (i <= n) {
            sum = sum + i;
            i = i + 1;
        }

        if (sum >= 10) {
            return sum;
        } else {
            return 0;
        }
    }

    // tiny JVM から呼ぶエントリ
    public static int main() {
        int n = 5;
        int s = sumToN(n);    // invokestatic
        return square(s);     // さらに invokestatic
    }
}
