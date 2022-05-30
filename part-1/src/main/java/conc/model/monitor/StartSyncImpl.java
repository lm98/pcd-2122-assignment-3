package conc.model.monitor;

public class StartSyncImpl implements StartSync {
    private boolean started;

    @Override
    public synchronized void waitStart() {
        while (!started){
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        started = false;
    }

    @Override
    public synchronized void notifyStart() {
        this.started = true;
        notifyAll();
    }
}
