package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Semaphore;

public class WorkplaceGuard {

    private final ConcurrentMap<WorkplaceId, Boolean> occupied;
    private final Map<WorkplaceId, Map<Thread, Semaphore>> waiting;
    private final Map<WorkplaceId, Map<Thread, WorkplaceId>> switchWaiting;
    private final ConcurrentMap<WorkplaceId, WorkplaceId> waitsFor;
    private final Semaphore mutex;

    public WorkplaceGuard(Collection<Workplace> workplaces) {
        this.mutex = new Semaphore(1, true);

        this.occupied = new ConcurrentHashMap<>();
        this.waitsFor = new ConcurrentHashMap<>();

        this.waiting = new ConcurrentHashMap<>();
        this.switchWaiting = new ConcurrentHashMap<>();

        for (var workplace : workplaces){
            this.occupied.put(workplace.getId(), false);

            this.waiting.put(workplace.getId(), new ConcurrentHashMap<>());
            this.switchWaiting.put(workplace.getId(), new ConcurrentHashMap<>());
        }
    }

    private Thread findKeyFromValue(Map<Thread, WorkplaceId> map, WorkplaceId value){
        for (var entry : map.entrySet()){
            if (entry.getValue() == value){
                return entry.getKey();
            }
        }
        return null;
    }

    public void acquire(WorkplaceId workplaceId) throws InterruptedException {
        mutex.acquire();

        if (occupied.get(workplaceId)){
            Semaphore wait = new Semaphore(0, true);

            waiting.get(workplaceId).put(Thread.currentThread(), wait);

            mutex.release();

            wait.acquire();
        } else {
            occupied.replace(workplaceId, true);
            mutex.release();
        }
    }

    public void releaseAnyone(WorkplaceId workplaceId){
        // Mutex musi byc zapewniony.
        if (!waiting.get(workplaceId).isEmpty()){
            Map.Entry<Thread, Semaphore> entry = waiting.get(workplaceId).entrySet().iterator().next();
            Thread thread = entry.getKey();
            Semaphore semaphore = entry.getValue();

            switchWaiting.get(workplaceId).remove(thread);
            waiting.get(workplaceId).remove(thread);

            semaphore.release();
        } else {
            occupied.replace(workplaceId, false);
        }
    }

    public void release(WorkplaceId workplaceId) {
        try {
            mutex.acquire();

            releaseAnyone(workplaceId);

            mutex.release();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    public void releaseFromSwitch(WorkplaceId prevWorkplaceId, WorkplaceId newWorkplaceId){
        // Mutex musi byc zapewniony.
        WorkplaceId cycle = newWorkplaceId;
        while (cycle != null && !switchWaiting.get(prevWorkplaceId).containsValue(cycle)){
            cycle = waitsFor.getOrDefault(cycle, null);
        }
        if (cycle != null){
            Thread threadToAwake = findKeyFromValue(switchWaiting.get(prevWorkplaceId), cycle);

            switchWaiting.get(prevWorkplaceId).remove(threadToAwake);

            waiting.get(prevWorkplaceId).get(threadToAwake).release();
            waiting.get(prevWorkplaceId).remove(threadToAwake);
        } else {
            releaseAnyone(prevWorkplaceId);
        }
    }

    public void releaseAndAcquire(WorkplaceId prevWorkplaceId, WorkplaceId newWorkplaceId) throws InterruptedException {
        mutex.acquire();

        releaseFromSwitch(prevWorkplaceId, newWorkplaceId);

        if (occupied.get(newWorkplaceId)){
            Semaphore wait = new Semaphore(0, true);

            waiting.get(newWorkplaceId).put(Thread.currentThread(), wait);
            switchWaiting.get(newWorkplaceId).put(Thread.currentThread(), prevWorkplaceId);

            waitsFor.put(prevWorkplaceId, newWorkplaceId);
            mutex.release();

            wait.acquire();
            waitsFor.remove(prevWorkplaceId);
        } else {
            occupied.replace(newWorkplaceId, true);

            mutex.release();
        }
    }

}
