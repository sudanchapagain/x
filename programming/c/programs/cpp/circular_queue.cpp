#include <print>
#include <iostream>

class CircularQueue {
private:
    int size;
    int front;
    int rear;
    int count;
    int* arr;

public:
    CircularQueue(int s)
        : size{s}, front{0}, rear{0}, count{0} {
        arr = new int[size];
    }

    ~CircularQueue() {
        delete[] arr;
    }

    bool isFull() const {
        return count == size;
    }

    bool isEmpty() const {
        return count == 0;
    }

    void enqueue(int value) {
        if (isFull()) {
            std::println("queue overflow!");
            return;
        }

        arr[rear] = value;
        rear = (rear + 1) % size;
        ++count;

        std::println("{} enqueued.", value);
    }

    int dequeue() {
        if (isEmpty()) {
            std::println("queue underflow!");
            return -1;
        }

        int value = arr[front];
        front = (front + 1) % size;
        --count;

        std::println("{} dequeued.", value);
        return value;
    }

    int peek() const {
        if (isEmpty()) {
            std::println("queue is empty.");
            return -1;
        }
        return arr[front];
    }

    void display() const {
        if (isEmpty()) {
            std::println("queue is empty.");
            return;
        }

        std::print("queue: ");

        int idx = front;
        for (int i = 0; i < count; ++i) {
            std::print("{} ", arr[idx]);
            idx = (idx + 1) % size;
        }

        std::print("\n");
    }
};

int main() {
    CircularQueue q(5);
    int choice, value;

    while (true) {
        std::println("1. enqueue\t2. dequeue\t3. peek\t4. display\t5. exit");

        std::print("enter choice: ");
        std::cin >> choice;

        if (!std::cin) {
            std::println("invalid input!");
            return 0;
        }

        switch (choice) {
            case 1:
                std::print("enter value: ");
                std::cin >> value;
                q.enqueue(value);
                break;

            case 2:
                q.dequeue();
                break;

            case 3:
                value = q.peek();
                if (value != -1)
                    std::println("front element: {}", value);
                break;

            case 4:
                q.display();
                break;

            case 5: return 0;

            default: std::println("invalid choice!");
        }
    }
}
