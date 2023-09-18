package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;

import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Semaphore;

public class UseSafetyGuard {
    private final ConcurrentMap<WorkplaceId, Semaphore> postSemaphoresByWId;
    private final ConcurrentMap<Thread, Semaphore> postSemaphoresByThreads;

    public UseSafetyGuard(Collection<Workplace> workplaces) {
        this.postSemaphoresByWId = new ConcurrentHashMap<>();
        this.postSemaphoresByThreads = new ConcurrentHashMap<>();

        for (Workplace workplace : workplaces){
            postSemaphoresByWId.put(workplace.getId(), new Semaphore(1, true));
        }
    }

    public void startBlockingUse(WorkplaceId wId) throws InterruptedException {
        // Zablokuj potencjalne uzycia use() przez zajecie semafora postSemaphore.
        postSemaphoresByWId.get(wId).acquire();

        // Odblokuje to stanowisko dopiero wywolanie use() przez tego pracownika na nowym stanowisku.
        postSemaphoresByThreads.put(Thread.currentThread(), postSemaphoresByWId.get(wId));
    }

    public void stopBlockingUse(){
        if (postSemaphoresByThreads.containsKey(Thread.currentThread())){
            postSemaphoresByThreads.get(Thread.currentThread()).release();
            postSemaphoresByThreads.remove(Thread.currentThread());
        }
    }

    public void acquire(WorkplaceId wId) throws InterruptedException {
        postSemaphoresByWId.get(wId).acquire();
    }

    public void release(WorkplaceId wId){
        postSemaphoresByWId.get(wId).release();
    }
}
