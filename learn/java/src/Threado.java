package src;

/*
 * Single-threaded systems use an approach called an event loop with polling.
 * In this model, a single thread of control runs in an infinite loop, polling
 * a single event queue to decide what to do next. Once this polling mechanism
 * returns with, say, a signal that a network file is ready to be read, then
 * the event loop dispatches control to the appropriate event handler. Until
 * this event handler returns, nothing else can happen in the program. This
 * wastes CPU time. It can also result in one part of a program dominating the
 * system and preventing any other events from being processed. In general, in
 * a single-threaded environment, when a thread blocks (that is, suspends
 * execution) because it is waiting for some resource, the entire program
 * stops running.

 * The benefit of Java's multithreading is that the main loop/polling
 * mechanism is eliminated. One thread can pause without stopping other
 * parts of your program. For example, the idle time created when a thread
 * reads data from a network or waits for user input can be utilized
 * elsewhere. Multithreading allows animation loops to sleep for a second
 * between each frame without causing the whole system to pause. When a
 * thread blocks in a Java program, only the single thread that is blocked
 * pauses. All other threads continue to run.

 * over the past few years, multicore systems have become commonplace. Of
 * course, single-core systems are still in widespread use. It is important
 * to understand that Java's multithreading features work in both types
 * of systems. In a single-core system, concurrently executing threads share
 * the CPU, with each thread receiving a slice of CPU time. Therefore, in a
 * single-core system, two or more threads do not actually run at the same
 * time, but idle CPU time is utilized. However, in multicore systems, it is
 * possible for two or more threads to actually execute simultaneously. In
 * many cases, this can further improve program efficiency and increase the
 * speed of certain operations.

 * Threads exist in several states. Here is a general description. A thread
 * can be running. It can be ready to run as soon as it gets CPU time. A
 * running thread can be suspended, which temporarily halts its activity. A
 * suspended thread can then be resumed, allowing it to pick up where it left
 * off. A thread can be blocked when waiting for a resource. At any time,
 * a thread can be terminated, which halts its execution immediately. Once
 * terminated, a thread cannot be resumed.

 * Java assigns to each thread a priority that determines how that thread should
 * be treated with respect to the others. Thread priorities are integers that
 * specify the relative priority of one thread to another. As an absolute
 * value, a priority is meaningless; a higher-priority thread doesn't
 * run any faster than a lower-priority thread if it is the only thread
 * running. Instead, a thread's priority is used to decide when to switch
 * from one running thread to the next. This is called a context switch. The
 * rules that determine when a context switch takes place are simple:
 * 
 * - A thread can voluntarily relinquish control. This occurs when explicitly
 *   yielding, sleeping, or when blocked. In this scenario, all other threads
 *   are examined, and the highest-priority thread that is ready to run is
 *   given the CPU.
 * 
 * - A thread can be preempted by a higher-priority thread. In this
 *   case, a lower-priority thread that does not yield the processor is
 *   simply preempted—no matter what it is doing—by a higher-priority
 *   thread. Basically, as soon as a higher-priority thread wants to run,
 *   it does. This is called preemptive multitasking.
 * 
 * In cases where two threads with the same priority are competing for CPU
 * cycles, the situation is a bit complicated. For some operating systems,
 * threads of equal priority are time-sliced automatically in round-robin
 * fashion. For other types of operating systems, threads of equal priority
 * must voluntarily yield control to their peers. If they don't, the other
 * threads will not run.

 * to enforce synchronicity:
 * =========================
 * For this purpose, Java implements an elegant twist on an age-old model
 * of interprocess synchronization: the monitor. The monitor is a control
 * mechanism first defined by C.A.R. Hoare. You can think of a monitor as
 * a very small box that can hold only one thread. Once a thread enters
 * a monitor, all other threads must wait until that thread exits the
 * monitor. In this way, a monitor can be used to protect a shared asset
 * from being manipulated by more than one thread at a time.
 *
 * In Java, there is no class "Monitor"; instead, each object has its own
 * implicit monitor that is automatically entered when one of the object's
 * synchronized methods is called. Once a thread is inside a synchronized
 * method, no other thread can call any other synchronized method on the same
 * object. This enables you to write very clear and concise multithreaded code,
 * because synchronization support is built into the language.

 * for messaging between two threads
 * =================================
 * After you divide your program into separate threads, you need to define
 * how they will communicate with each other. When programming with some other
 * languages, you must depend on the operating system to establish
 * communication between threads. This, of course, adds overhead. By contrast,
 * Java provides a clean, low-cost way for two or more threads to talk to each
 * other, via calls to predefined methods that all objects have. Java's
 * messaging system allows a thread to enter a synchronized method on an
 * object, and then wait there until some other thread explicitly notifies it
 * to come out.
 *
 *
 * Thread can be implement by either
 * extending Thread class or by using Runnable interface
 * Implementing Runnable interface
 * `public class MyRunnable implements Runnable {`
 * in myThread `Thread thread = new Thread(new MyRunnable());`

 */

public class Threado extends Thread {
    @Override
    public void run() {
        System.out.println("Thread is running.");
    }

    public void myThread() {
        Threado thread = new Threado();
        thread.start();
    }

    // Synchronization
    private int count = 0;

    public synchronized void increment() {
        count++;
    }

    public void syncThread() {
        Threado example = new Threado();

        Thread t1 = new Thread(() -> example.increment());
        Thread t2 = new Thread(() -> example.increment());

        t1.start();
        t2.start();

        try {
            t1.join();
            t2.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        System.out.println("Count: " + example.count);
    }
}
