package conc.model.monitor;

public class StopFlagImpl implements StopFlag {
    private boolean flag;

    public StopFlagImpl() {
        flag = false;
    }

    @Override
    public synchronized void reset() {
        flag = false;
    }

    @Override
    public synchronized void set() {
        flag = true;
    }

    @Override
    public synchronized boolean isSet() {
        return flag;
    }
}
