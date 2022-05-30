package conc.model.monitor;

public interface StopFlag {
    void reset();

    void set();

    boolean isSet();
}
