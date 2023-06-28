package cp2022.solution;

import java.util.Map;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

public class AntiStarvationGuard {
    final Semaphore mutex;
    final Semaphore waiting;
    Integer capacity;
    Map<Thread, AtomicInteger> workersPassed;


    public AntiStarvationGuard(Integer capacity) {
        this.capacity = capacity;
        this.workersPassed = new ConcurrentHashMap<>();

        this.mutex = new Semaphore(1, true);
        this.waiting = new Semaphore(1, true);
    }

    int findLongestWaiting(){
        int max = 0;
        for (var atomicInteger : workersPassed.values()){
            int val = atomicInteger.intValue();
            if (val > max){
                max = val;
            }
        }
        return max;
    }

    public void startTryingToEnter() throws InterruptedException {
        waiting.acquire();
        mutex.acquire();

        int timeWaited = findLongestWaiting() + 1;

        workersPassed.put(Thread.currentThread(), new AtomicInteger(0));

        // Kaskadowo budzimy kolejny watek czekajacy w kolejce waiting.
        if (timeWaited < capacity) waiting.release();

        for (var atomicInteger : workersPassed.values()){
            atomicInteger.incrementAndGet();
        }

        mutex.release();
    }

    public void startTryingToSwitch() throws InterruptedException {
        mutex.acquire();

        workersPassed.put(Thread.currentThread(), new AtomicInteger(1));

        mutex.release();
    }

    public void finishTrying() throws InterruptedException {
        mutex.acquire();

        int removedValue = workersPassed.get(Thread.currentThread()).intValue();
        workersPassed.remove(Thread.currentThread());
        int newLongestWaiting =  findLongestWaiting();

        if (removedValue == capacity && newLongestWaiting < capacity)
            // Means we can let at least one worker perform their action.
            waiting.release();

        mutex.release();
    }
}
