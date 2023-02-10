package cp2022.solution;

import java.util.HashMap;

public class SwitchGraph {

    private final HashMap<WorkplaceWrapper, WorkplaceWrapper> edges = new HashMap<>();

    public SwitchGraph() {
    }

    public void add(WorkplaceWrapper from, WorkplaceWrapper to) {
        edges.put(from, to);
    }

    public void remove(WorkplaceWrapper from, WorkplaceWrapper to) {
        edges.remove(from, to);
    }

    public WorkplaceWrapper traversePath(WorkplaceWrapper from) {
        WorkplaceWrapper curr = null;
        WorkplaceWrapper next = edges.get(from);
        int count = 0;
        while (next != null && !next.equals(from)) {
            curr = next;
            next = edges.get(curr);
            count++;
            if (count > 2 * edges.size())
                // there's a cycle, but we're not a part of it
                return null;
        }
        return curr;
    }

    // Assumes that cw is a part of a cycle.
    public WorkplaceWrapper cyclePredecessor(WorkplaceWrapper cw) {
        WorkplaceWrapper pred = null;
        WorkplaceWrapper next = edges.get(cw);
        while (next != null) {
            if (next.equals(cw) && pred != null) {
                assert edges.get(pred).equals(cw);
                return pred;
            }
            pred = next;
            next = edges.get(next);
        }
        if (pred == null)
            for (WorkplaceWrapper dir_pred : edges.keySet())
                if (edges.get(dir_pred).equals(cw))
                    return dir_pred;
        return pred;
    }
}
