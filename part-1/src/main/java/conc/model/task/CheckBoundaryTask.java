package conc.model.task;

import conc.model.Body;
import conc.model.Boundary;

import java.util.List;
import java.util.concurrent.Callable;

public final class CheckBoundaryTask implements Callable<Body> {
    private final Body subject;
    private final Boundary boundary;

    public CheckBoundaryTask(Body subject, Boundary boundary){
        this.subject = subject;
        this.boundary = boundary;
    }

    @Override
    public Body call(){
        subject.checkAndSolveBoundaryCollision(boundary);
        return subject;
    }
}
