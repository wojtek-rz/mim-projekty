/*
 * University of Warsaw
 * Concurrent Programming Course 2022/2023
 * Java Assignment
 *
 * Author: Konrad Iwanicki (iwanicki@mimuw.edu.pl)
 */
package cp2022.solution;

import java.util.Collection;

import cp2022.base.Workplace;
import cp2022.base.Workshop;



public final class WorkshopFactory {

    public final static Workshop newWorkshop(
            Collection<Workplace> workplaces
    ) {
        return new cp2022.solution.ConcurrentWorkshop(workplaces);
    }


    
}
