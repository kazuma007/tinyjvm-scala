public class ArrayTest {
    // Test 1: Create array and set/get values
    public static int basicArrayTest() {
        int[] arr = new int[5];
        arr[0] = 10;
        arr[1] = 20;
        arr[2] = 30;
        return arr[0] + arr[1] + arr[2]; // Should return 60
    }

    // Test 2: Sum array elements
    public static int sumArray() {
        int[] numbers = new int[4];
        numbers[0] = 5;
        numbers[1] = 10;
        numbers[2] = 15;
        numbers[3] = 20;

        int sum = 0;
        int i = 0;
        while (i < 4) {
            sum = sum + numbers[i];
            i = i + 1;
        }
        return sum; // Should return 50
    }

    // Test 3: Array length
    public static int arrayLengthTest() {
        int[] arr = new int[7];
        return arr.length; // Should return 7
    }

    // Test 4: Find maximum in array
    public static int findMax() {
        int[] values = new int[5];
        values[0] = 3;
        values[1] = 17;
        values[2] = 8;
        values[3] = 25;
        values[4] = 12;

        int max = values[0];
        int i = 1;
        while (i < 5) {
            if (values[i] > max) {
                max = values[i];
            }
            i = i + 1;
        }
        return max; // Should return 25
    }

    // Test 5: Array initialization with loop
    public static int initializeAndSum() {
        int[] arr = new int[10];
        int i = 0;
        while (i < 10) {
            arr[i] = i * 2;
            i = i + 1;
        }

        // Sum: 0 + 2 + 4 + 6 + 8 + 10 + 12 + 14 + 16 + 18 = 90
        int sum = 0;
        int j = 0;
        while (j < 10) {
            sum = sum + arr[j];
            j = j + 1;
        }
        return sum; // Should return 90
    }

    // Test 6: Reverse array elements
    public static int reverseArray() {
        int[] arr = new int[5];
        arr[0] = 1;
        arr[1] = 2;
        arr[2] = 3;
        arr[3] = 4;
        arr[4] = 5;

        // Reverse in place
        int i = 0;
        int j = 4;
        while (i < j) {
            int temp = arr[i];
            arr[i] = arr[j];
            arr[j] = temp;
            i = i + 1;
            j = j - 1;
        }

        // Now arr = [5, 4, 3, 2, 1]
        return arr[0] * 10 + arr[4]; // Should return 51
    }
}
