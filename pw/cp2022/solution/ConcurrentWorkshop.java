package cp2022.solution;

import cp2022.base.Workplace;
import cp2022.base.WorkplaceId;
import cp2022.base.Workshop;
import cp2022.tests.pggp_tests.utility.WorkerId;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;

/*
  Wywołanie metod enter lub switchTo Twojej implementacji interfejsu cp2022.base.Workshop może
  zwrócić oryginalny obiekt stanowiska o żądanym identyfikatorze (tj. ten przekazany ww. metodzie
  newWorkshop klasy cp2022.solution.WorkshopFactory) lub obiekt innej klasy dziedziczącej po
  klasie cp2022.base.Workplace. Jednakże w tym drugim przypadku każde wywołanie metody use musi
  kiedyś doprowadzić do dokładnie jednego wywołania metody use oryginalnego obiektu stanowiska.
  Innymi słowy, zwrócony obiekt stanowiska może być co najwyżej dekoratorem obiektu oryginalnego,
  a gwarancje bezpieczeństwa aplikują się do oryginalnego obiektu.
*/

public class ConcurrentWorkshop implements Workshop {

    final Map<WorkplaceId, WorkplaceWrapper> workplaces =
            new HashMap<>();

    final Semaphore mutex = new Semaphore(1, true);

    /** Queues of workers waiting for entry per workplace */
    final Map<WorkplaceId, Queue<Long>> wpQueue = new ConcurrentHashMap<>();

    /** One semaphore per worker identified by ThreadId */
    final Map<Long, Semaphore> woSemaphore = new ConcurrentHashMap<>();

    /** Maps workers to workplace they want to switch from */
    final Map<Long, WorkplaceWrapper> switchingFrom = new ConcurrentHashMap<>();

    /** Maps workplace to a list of workers currently occupying it */
    final Map<WorkplaceId, Long> occupiedBy = new ConcurrentHashMap<>();

    /** Maps workplace to a use() permit semaphore */
    final Map<WorkplaceId, Semaphore> usePermits = new ConcurrentHashMap<>();

    /** Maps workers to workplaces they currently occupy */
    final Map<Long, WorkplaceWrapper> occupies = new ConcurrentHashMap<>();

    /** Graph representing current switch state. Useful for detecting cycles */
    final SwitchGraph switchGraph = new SwitchGraph();

    /** Utility that guards compliance with 2*N requirement */
    final WorkshopQueue workshopQueue;

    public ConcurrentWorkshop(Collection<Workplace> workplaces) {
        this.workshopQueue = new WorkshopQueue(2 * workplaces.size() - 1);

        for (var w : workplaces) {
            this.workplaces.put(w.getId(), new WorkplaceWrapper(w, this));
            this.wpQueue.put(w.getId(), new LinkedList<>());
            this.occupiedBy.put(w.getId(), (long) -1);
            this.usePermits.put(w.getId(), new Semaphore(1, true));
        }
    }

    @Override
    public Workplace enter(WorkplaceId wid) {
        long workerId = Thread.currentThread().getId();
        WorkplaceWrapper to = workplaces.get(wid);
        try {
            workshopQueue.enter();

            mutex.acquire();

            if (!woSemaphore.containsKey(workerId))
                woSemaphore.put(workerId, new Semaphore(0, true));

            if (occupiedBy.get(to.getId()) != -1) {

                wpQueue.get(to.getId()).add(workerId);

                mutex.release();
                woSemaphore.get(workerId).acquire();

                mutex.acquire();
            }
            occupies.put(workerId, to);
            occupiedBy.put(to.getId(), workerId);
            mutex.release();

            workshopQueue.leave();

        } catch (InterruptedException e) {
            throw new RuntimeException("panic: unexpected thread interruption");
        } finally {
        }

        return to;
    }

    @Override
    public Workplace switchTo(WorkplaceId wid) {
        long workerId = Thread.currentThread().getId();
        WorkplaceWrapper from = occupies.get(workerId);
        WorkplaceWrapper to = workplaces.get(wid);

        if (from.equals(to))
            return workplaces.get(wid);

        try {
            workshopQueue.switchTo();

            mutex.acquire();
//            System.out.println("from: " + from + "to: " + to);
            // release
            switchingFrom.put(workerId, from);
            occupies.remove(workerId);

//            System.out.println( "acquire usePermits" + Thread.currentThread().getName());
            usePermits.get(from.getId()).acquire(); // dopiero wykonanie use zwolni usePermit

            wakeAnyWaiting(from);
            if (occupiedBy.get(to.getId()) == -1) {
                occupiedBy.put(to.getId(), workerId);
                occupies.put(workerId, to);
            } // else: workplace [to] is occupied by someone
            else {
                wpQueue.get(to.getId()).add(workerId);

                mutex.release();

                woSemaphore.get(workerId).acquire();

                mutex.acquire();
                // ktoś został occupiedBy != null
                occupiedBy.put(to.getId(), workerId);
                occupies.put(workerId, to);
            }

            mutex.release();
            workshopQueue.leave();

//
//            // bez cykli
//            WorkplaceWrapper waitingOn = switchGraph.traversePath(from);
//
//            boolean isCycle = waitingOn != null && waitingOn.equals(from);
//            // isCycle=true if current [to] owner is waiting for us to switch from [from] - so
//            // indeed a cycle
//            // isCycle=false means that we are not a part of a problem and can just wait for [this]
//            // to become unoccupied or enter freely if it's empty.
//
//            // Since we will be waiting, let's add ourselves to the graph. We might be accidentally
//            // forming a cycle that someone else will detect (and be forced to fix) in the future.
//            switchGraph.add(from, to);
//
//
//            if (isCycle) {
//                // If we have just formed a cycle, we must break it by waking the worker involved in
//                // a cycle that waits directly for [form] to become unoccupied. It is not *yet* free,
//                // but he's supposed to move to [form], traverse through cycle and find his own
//                // predecessor.
//                // This predecessor will then move to our predecessor's [from], traverse through cycle
//                // and so on. Last one awaken, when traversing, will find that he has no predecessors.
//                // This means that he is the first caller and only thing left in solving the cycle
//                // is moving immediately from [from] to [to].
//
//                WorkplaceWrapper predecessor = switchGraph.cyclePredecessor(from);
//                wakeReadyToSwitch(predecessor, from);
//                mutex.acquire();
//
//                // After waking up the predecessor, cycle-former leaves switchToHere() and waits
//                // in completeEventualSwitch() until he acquires this.occupied.
//                // When he does so, he then releases from.usePermit.
//            } else {
//                // If no cycle was found, leave previous workplace and wake anyone waiting in
//                // queue for it to become available.
//                wakeAnyWaitingForSwitch(from);
//
//                // We will now wait until [to] becomes empty.
//                mutex.release();
//                woSemaphore.get(workerId).acquire();
//
//                // Now we are permitted to enter [to], but will need to wait with working until
//                // its previous owner leaves it completely.
//            }

            // these two lines will be executed in an appropriate moment during use() call in
            // the WorkplaceWrapper:
            //      occupies.put(workerId, to); occupiedBy.get(to).add(workerId);
            // Calling use() guarantees that switchTo() ended.

        } catch (InterruptedException e) {
            throw new RuntimeException("panic: unexpected thread interruption");
        }

        return to;
    }

    @Override
    public void leave() {
        long workerId = Thread.currentThread().getId();
        WorkplaceWrapper from = occupies.get(workerId);

        try {
            mutex.acquire();
            occupies.remove(workerId);

            wakeAnyWaiting(from);
            mutex.release();

        } catch (Exception e) {
            throw new RuntimeException("panic: unexpected thread interruption");
        }
    }

    private void wakeAnyWaiting(WorkplaceWrapper workplace) throws InterruptedException {
        if (!wpQueue.get(workplace.getId()).isEmpty()){
            Long workerToWake = wpQueue.get(workplace.getId()).poll();
            woSemaphore.get(workerToWake).release();
        } else {
            occupiedBy.put(workplace.getId(), (long)-1);
        }
    }
}
