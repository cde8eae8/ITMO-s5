

public class Main {
    public static void main(String[] args) {
        FCPriorityQueue<Integer> queue = new FCPriorityQueue<Integer>();
        queue.add(1);
        queue.add(2);
        queue.add(3);
        queue.add(4);
        for (int i = 0; i < 4; ++i) {
            System.out.println(queue.peek());
            queue.add(-1);
        }

    }
}
