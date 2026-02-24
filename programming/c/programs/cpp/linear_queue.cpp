#include <print>
#include <iostream>

class Queue {
private:
    int size;
    int front;
    int rear;
    int* arr;

public:
    Queue(int s)
        : size{s}, front{-1}, rear{-1} {
        arr = new int[size];
    }

    ~Queue() {
        delete[] arr;
    }

    bool isFull() const {
        return rear == size - 1;
    }

    bool isEmpty() const {
        return (front == -1 || front > rear);
    }

    void enqueue(int value) {
        if (isFull()) {
            std::println("queue overflow!");
            return;
        }

        if (front == -1) {
            front = 0;
        }

        arr[++rear] = value;
        std::println("{} enqueued.", value);
    }

    int dequeue() {
        if (isEmpty()) {
            std::println("queue underflow!");
            return -1;
        }

        int value = arr[front++];
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
        for (int i = front; i <= rear; ++i) {
            std::print("{} ", arr[i]);
        }
        std::print("\n");
    }
};

int main() {
    Queue q(5);
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
