package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;

import java.util.*;
import java.util.concurrent.*;

public class Workshop implements cp2022.base.Workshop {
    final ConcurrentMap<WorkplaceId, Workplace> workplaces;
    final ConcurrentMap<Thread, WorkplaceId> wIdByThreads;

    final UseAccessControl useAccessControl;
    final WorkplaceAccessControl workplaceAccess;
    GlobalAccessControl globalAccess;

    public Workshop(Collection<Workplace> workplaces){
        this.workplaces = new ConcurrentHashMap<>();
        this.wIdByThreads = new ConcurrentHashMap<>();

        this.useAccessControl = new UseAccessControl(workplaces);
        this.globalAccess = new GlobalAccessControl(workplaces.size() * 2);
        this.workplaceAccess = new WorkplaceAccessControl(workplaces);

        for (Workplace workplace : workplaces){
            this.workplaces.put(workplace.getId(), new WorkplaceDecorator(workplace, useAccessControl));
        }
    }
    @Override
    public Workplace enter(WorkplaceId wId) {
        // Dodajemy na którym semaforze wątek się wykonuje.
        wIdByThreads.put(Thread.currentThread(), wId);

        try {
            globalAccess.startTryingToEnter(Thread.currentThread());

                workplaceAccess.enterAcquire(wId);

            globalAccess.finishTrying(Thread.currentThread());

            return workplaces.get(wId);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Workplace switchTo(WorkplaceId wId) {
        WorkplaceId prevWId = wIdByThreads.get(Thread.currentThread());
        if (prevWId == wId){
            return workplaces.get(wId);
        }
        wIdByThreads.remove(Thread.currentThread());
        wIdByThreads.put(Thread.currentThread(), wId);

        try {
            // Blokuje wywołania "use" na poprzednim stanowisku.
            useAccessControl.startBlockingUse(prevWId);

            globalAccess.startTryingToSwitch(Thread.currentThread());

                // Zwalnia stanowisko dla jednej osoby i chce zająć nowe stanowisko.
                workplaceAccess.switchReleaseAndAcquire(prevWId, wId);

            globalAccess.finishTrying(Thread.currentThread());

            return workplaces.get(wId);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void leave() {
        WorkplaceId prevWid = wIdByThreads.get(Thread.currentThread());
        wIdByThreads.remove(Thread.currentThread());
        try {
            workplaceAccess.leaveRelease(prevWid);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
