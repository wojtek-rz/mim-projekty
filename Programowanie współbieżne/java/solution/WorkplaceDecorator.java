package cp2022.solution;

import cp2022.base.Workplace;

public class WorkplaceDecorator extends Workplace {
    private final Workplace workplace;
    private final UseSafetyGuard useSafetyGuard;

    public WorkplaceDecorator(Workplace workplace, UseSafetyGuard useSafetyGuard) {
        super(workplace.getId());
        this.workplace = workplace;
        this.useSafetyGuard = useSafetyGuard;
    }

    @Override
    public void use() {
        try {
            // Zakoncz blokowanie wywolywan "use" na poprzednim stanowisku, jesli takowe istnieje.
            useSafetyGuard.stopBlockingUse();

            useSafetyGuard.acquire(workplace.getId());
            workplace.use();
            useSafetyGuard.release(workplace.getId());
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
