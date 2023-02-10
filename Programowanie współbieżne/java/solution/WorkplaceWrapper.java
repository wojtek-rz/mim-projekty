package cp2022.solution;

import cp2022.base.Workplace;

import java.util.Objects;
import java.util.concurrent.Semaphore;

public class WorkplaceWrapper extends Workplace implements Comparable<Workplace> {
    private final Workplace workplace;
    private final ConcurrentWorkshop workshop;

    public WorkplaceWrapper(Workplace w, ConcurrentWorkshop workshop) {
        super(w.getId());
        this.workplace = w;
        this.workshop = workshop;
    }

    @Override
    public void use() {
        try {
            workshop.mutex.acquire();
            long workerId = Thread.currentThread().getId();

            WorkplaceWrapper switchingFrom = workshop.switchingFrom.getOrDefault(workerId, null);

            if (switchingFrom != null) {
                // pending switchTo() resolution
//                workshop.occupies.remove(workerId);


                workshop.usePermits.get(switchingFrom.getId()).release();

//                workshop.switchGraph.remove(switchingFrom, this);
//                workshop.switchingFrom.remove(workerId);
            }
            workshop.mutex.release();

            Semaphore thisUsePermit = workshop.usePermits.get(this.getId());
            thisUsePermit.acquire();
            workplace.use();
            thisUsePermit.release();

        } catch (InterruptedException e) {
            throw new RuntimeException("panic: unexpected thread interruption");
        }
    }

    @Override
    public int compareTo(Workplace other) {
        return this.workplace.getId().compareTo(other.getId());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Workplace that = (Workplace) o;
        return workplace.getId().equals(that.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(workplace.getId());
    }

    @Override
    public String toString() {
        return String.valueOf(workplace.getId());
    }
}
