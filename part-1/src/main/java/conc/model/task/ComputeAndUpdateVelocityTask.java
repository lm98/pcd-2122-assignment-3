package conc.model.task;

import conc.model.Body;
import conc.model.V2d;

import java.util.List;
import java.util.concurrent.Callable;

public final class ComputeAndUpdateVelocityTask implements Callable<Body> {
    private final List<Body> bodies;
    private final Body subject;
    private final double dt;
    
    public ComputeAndUpdateVelocityTask(List<Body> bodies, Body subject, double dt){
        this.bodies = bodies;
        this.subject = subject;
        this.dt = dt;
    }

    @Override
    public Body call() {
        /* compute total force on bodies */
        V2d totalForce = computeTotalForceOnBody(subject);

        /* compute instant acceleration */
        V2d acc = new V2d(totalForce).scalarMul(1.0 / subject.getMass());

        /* update velocity */
        subject.updateVelocity(acc, dt);
        return subject;
    }

    private V2d computeTotalForceOnBody(Body b) {

        V2d totalForce = new V2d(0, 0);

        /* compute total repulsive force */

        for (Body otherBody : bodies) {
            if (!b.equals(otherBody)) {
                try {
                    V2d forceByOtherBody = b.computeRepulsiveForceBy(otherBody);
                    totalForce.sum(forceByOtherBody);
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        }

        /* add friction force */
        totalForce.sum(b.getCurrentFrictionForce());

        return totalForce;
    }
}
