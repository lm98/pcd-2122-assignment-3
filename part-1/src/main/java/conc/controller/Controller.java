package conc.controller;

import conc.model.InputListener;
import conc.model.monitor.StartSync;
import conc.model.monitor.StopFlag;

public class Controller implements InputListener {

    private final StartSync sync;
    private final StopFlag stopFlag;

    public Controller(StartSync sync, StopFlag stopFlag){
        this.sync = sync;
        this.stopFlag = stopFlag;
    }

    public synchronized void started(){
        stopFlag.reset();
        sync.notifyStart();
    }

    public synchronized void stopped() {
        stopFlag.set();
    }

}