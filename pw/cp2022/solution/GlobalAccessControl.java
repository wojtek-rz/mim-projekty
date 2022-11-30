package cp2022.solution;

import java.util.Map;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

public class GlobalAccessControl {
    final Semaphore mutex;
    final Semaphore waiting;
    int capacity;
    Map<Thread, AtomicInteger> workersPassed;


    public GlobalAccessControl(int capacity) {
        this.capacity = capacity;
        this.mutex = new Semaphore(1, true);
        this.waiting = new Semaphore(1, true);
        this.workersPassed = new ConcurrentHashMap<>();
    }

    void incrementMap(){
        for (var atomicInteger : workersPassed.values()){
            atomicInteger.incrementAndGet();
        }
    }

    int findMaxInMap(){
        int max = 0;
        for (var atomicInteger : workersPassed.values()){
            int val = atomicInteger.intValue();
            if (val > max){
                max = val;
            }
        }
        return max;
    }

    public void startTryingToEnter(Thread thread) throws InterruptedException {
        waiting.acquire();
        mutex.acquire();

        incrementMap();
        int max = findMaxInMap();

        workersPassed.put(thread, new AtomicInteger(1));

        // Kaskadowo budzimy kolejny wątek czekający w kolejce waiting.
        if (max < capacity){
            waiting.release();
        }

        mutex.release();
    }

    public void startTryingToSwitch(Thread thread) throws InterruptedException {
        mutex.acquire();

        workersPassed.put(thread, new AtomicInteger(1));

        mutex.release();
    }

    public void finishTrying(Thread thread) throws InterruptedException {
        mutex.acquire();

        int removedValue = workersPassed.get(thread).intValue();
        workersPassed.remove(thread);

        int new_max =  findMaxInMap();
        if (new_max < capacity && removedValue == capacity){
            // Means we can let at lease one worker perform their action.
            waiting.release();
        }

        mutex.release();
    }
}
