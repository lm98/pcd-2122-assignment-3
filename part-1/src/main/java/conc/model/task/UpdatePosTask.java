package conc.model.task;

import conc.model.Body;

import java.util.List;
import java.util.concurrent.Callable;

public final class UpdatePosTask implements Callable<Body> {
    private final Body subject;
    private final double dt;

    public UpdatePosTask(Body subject, double dt){
        this.subject = subject;
        this.dt = dt;
    }

    @Override
    public Body call() {
        subject.updatePos(dt);
        return subject;
    }
}
