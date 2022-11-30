package cp2022.solution;

import cp2022.base.Workplace;

public class WorkplaceDecorator extends Workplace {
    private final Workplace workplace;
    private final UseAccessControl useAccessControl;

    public WorkplaceDecorator(Workplace workplace, UseAccessControl useAccessControl) {
        super(workplace.getId());
        this.workplace = workplace;
        this.useAccessControl = useAccessControl;
    }

    @Override
    public void use() {
        try {
            // Zakończ blokowanie wywoływań "use" na poprzednim stanowisku, jeśli takowe istnieje.
            useAccessControl.stopBlockingUse();

            useAccessControl.acquire(workplace.getId());
            workplace.use();
            useAccessControl.release(workplace.getId());
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
