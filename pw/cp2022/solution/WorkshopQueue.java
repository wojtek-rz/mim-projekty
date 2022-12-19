package cp2022.solution;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;
import java.util.logging.Level;
import java.util.logging.Logger;

public class WorkshopQueue {

    private static final Logger logger = Logger.getAnonymousLogger();

    private long oldest_id = -1;
    private int bound;

    private final Queue<Long> ids = new LinkedList<>();
    private final HashMap<Long, Integer> enteredAfter = new HashMap<>();

    public WorkshopQueue() {
        logger.setLevel(Level.OFF);
    }

    public WorkshopQueue(int bound) {
        this();
        this.bound = bound;
    }

    public void setBound(int bound) {
        this.bound = bound;
    }


    public synchronized void enter() throws InterruptedException {
        long id = Thread.currentThread().getId();
        logThreadAction("wants to enter()");

        if (oldest_id != -1)
            while (enteredAfter.containsKey(oldest_id) && enteredAfter.get(oldest_id) >= bound) {
                logThreadAction("waits." + enteredAfter.get(oldest_id) + " inside. Bound=" + bound);
                wait();
            }
        if (oldest_id == -1) oldest_id = id;

        enteredAfter.replaceAll((w, c) -> c + 1);
        ids.add(id);
        enteredAfter.put(id, 0);
    }

    public synchronized void switchTo() throws InterruptedException {
        long id = Thread.currentThread().getId();
        logThreadAction("wants to switchTo()");

        if (oldest_id == -1) oldest_id = id;

        ids.add(id);
        enteredAfter.put(id, 0);
    }

    public synchronized void leave() {
        long id = Thread.currentThread().getId();
        logThreadAction("leaves the queue");

        assert ids.contains(id);

        if (id == oldest_id) {
            logThreadAction("was the oldest");
            ids.poll();
            if (!ids.isEmpty()) {
                Long next_oldest_id = ids.peek();
                assert next_oldest_id != null;

                oldest_id = next_oldest_id;
            } else oldest_id = -1;

            notify();
        } else ids.remove(id);
        enteredAfter.remove(id);
    }

    private void logThreadAction(String message) {
        logger.info(Thread.currentThread().getName() + " " + message);
    }
}
