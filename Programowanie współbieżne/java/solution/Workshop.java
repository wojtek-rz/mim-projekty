package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;

import java.util.*;
import java.util.concurrent.*;

public class Workshop implements cp2022.base.Workshop {

    final UseSafetyGuard useSafetyGuard;
    final AntiStarvationGuard antiStarvationGuard;
    final WorkplaceGuard workplaceGuard;
    final ConcurrentMap<Thread, WorkplaceId> currentWorkplaceId;
    final ConcurrentMap<WorkplaceId, Workplace> workplaces;

    public Workshop(Collection<Workplace> workplaces) {

        this.useSafetyGuard = new UseSafetyGuard(workplaces);
        this.antiStarvationGuard = new AntiStarvationGuard(workplaces.size() * 2);
        this.workplaceGuard = new WorkplaceGuard(workplaces);

        this.workplaces = new ConcurrentHashMap<>();
        this.currentWorkplaceId = new ConcurrentHashMap<>();

        for (Workplace workplace : workplaces) {
            this.workplaces.put(workplace.getId(), new WorkplaceDecorator(workplace, useSafetyGuard));
        }
    }

    @Override
    public Workplace enter(WorkplaceId wId) {
        Thread thread = Thread.currentThread();

        currentWorkplaceId.put(thread, wId);

        try {
            antiStarvationGuard.startTryingToEnter();

            workplaceGuard.acquire(wId);

            antiStarvationGuard.finishTrying();

            return workplaces.get(wId);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Workplace switchTo(WorkplaceId wId) {
        Thread thread = Thread.currentThread();

        // Update maps.
        WorkplaceId prevwId = currentWorkplaceId.get(thread);
        currentWorkplaceId.remove(thread);
        currentWorkplaceId.put(thread, wId);

        if (prevwId == wId) {
            return workplaces.get(wId);
        }

        try {
            useSafetyGuard.startBlockingUse(prevwId);

            antiStarvationGuard.startTryingToSwitch();

            workplaceGuard.releaseAndAcquire(prevwId, wId);

            antiStarvationGuard.finishTrying();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        return workplaces.get(wId);
    }

    @Override
    public void leave() {
        Thread thread = Thread.currentThread();

        // Update maps.
        WorkplaceId prevWid = currentWorkplaceId.get(thread);
        currentWorkplaceId.remove(thread);

        // Release workplace.
        workplaceGuard.release(prevWid);
    }
}
