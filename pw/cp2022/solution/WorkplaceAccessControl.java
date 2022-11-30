package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;

import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;

public class WorkplaceAccessControl {

    private final ConcurrentMap<WorkplaceId, ConcurrentLinkedDeque<Semaphore> > waitingFromEnter;
    private final ConcurrentMap<WorkplaceId, ConcurrentMap<WorkplaceId, Semaphore> > waitingFromSwitch;
    private final ConcurrentMap<WorkplaceId, AtomicBoolean> occupied;

    private final ConcurrentMap<WorkplaceId, WorkplaceId> waitsFor;
    private final Semaphore mutex;

    public WorkplaceAccessControl(Collection<Workplace> workplaces) {
        this.mutex = new Semaphore(1, true);

        this.waitingFromSwitch = new ConcurrentHashMap<>();
        this.waitingFromEnter = new ConcurrentHashMap<>();
        this.occupied = new ConcurrentHashMap<>();
        this.waitsFor = new ConcurrentHashMap<>();

        for (var workplace : workplaces){

            this.waitingFromSwitch.put(workplace.getId(), new ConcurrentHashMap<>());
            this.waitingFromEnter.put(workplace.getId(), new ConcurrentLinkedDeque<>());
            this.occupied.put(workplace.getId(), new AtomicBoolean(false));
        }
    }

    void enterAcquire(WorkplaceId wid) throws InterruptedException {
        mutex.acquire();
        if (occupied.get(wid).get()){
            Semaphore wait = new Semaphore(0, true);
            waitingFromEnter.get(wid).add(wait);
            mutex.release();
            wait.acquire();
        } else {
            occupied.get(wid).set(true);
            mutex.release();
        }
    }

    void releaseAnyone(WorkplaceId wid){
        if (!waitingFromSwitch.get(wid).isEmpty()){
            // We pick random semaphore from the waiting ones.
            for (WorkplaceId key : waitingFromSwitch.get(wid).keySet()){
                waitingFromSwitch.get(wid).get(key).release();
                waitingFromSwitch.get(wid).remove(key);
                break;
            }
        } else if (!waitingFromEnter.get(wid).isEmpty()){
            waitingFromEnter.get(wid).removeFirst().release();
        } else {
            occupied.get(wid).set(false);
        }
    }

    void leaveRelease(WorkplaceId wid) throws InterruptedException {
        mutex.acquire();

        releaseAnyone(wid);

        mutex.release();
    }

    void switchRelease(WorkplaceId wid, WorkplaceId newWid){
        WorkplaceId cycle = newWid;
        while (cycle != null && !waitingFromSwitch.get(wid).containsKey(cycle)){
            cycle = waitsFor.getOrDefault(cycle, null);
        }
        if (cycle != null){
            // Waiting from switch contains workplace that we want to switch to,
            // so to prevent blocking the cycle we have to release that worker first.
            waitingFromSwitch.get(wid).get(cycle).release();
            waitingFromSwitch.get(wid).remove(cycle);
        } else {
            // We can release anyone.
            releaseAnyone(wid);
        }
    }

    void switchReleaseAndAcquire(WorkplaceId wid, WorkplaceId newWid) throws InterruptedException {
        mutex.acquire();

        // Release
        switchRelease(wid, newWid);

        // Acquire
        if (occupied.get(newWid).get()){
            Semaphore wait = new Semaphore(0, true);

            waitingFromSwitch.get(newWid).put(wid, wait);

            waitsFor.put(wid, newWid);
            mutex.release();

            wait.acquire();
            waitsFor.remove(wid);
        } else {
            occupied.get(newWid).set(true);
            mutex.release();
        }
    }
}
