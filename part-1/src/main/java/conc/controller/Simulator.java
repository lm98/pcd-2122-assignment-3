package conc.controller;

import conc.model.Body;
import conc.model.Boundary;
import conc.model.P2d;
import conc.model.V2d;
import conc.model.monitor.StartSync;
import conc.model.monitor.StartSyncImpl;
import conc.model.monitor.StopFlag;
import conc.model.monitor.StopFlagImpl;
import conc.view.SimulationView;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.*;

public class Simulator {
	/* bodies in the field */
	private ArrayList<Body> bodies;
	/* boundary of the field */
	private Boundary bounds;

	private final int nBodies, nSteps, nWorkers;

	public Simulator(int nBodies, int nSteps, int nWorkers) {
		this.nBodies = nBodies;
		this.nSteps = nSteps;
		this.nWorkers = nWorkers;
		testWithNumBodies();
	}
	
	public void execute() {
		/** SETUP **/
		StartSync sync = new StartSyncImpl();
		StopFlag flag = new StopFlagImpl();
		Controller controller = new Controller(sync, flag);
		SimulationView viewer = new SimulationView(620,620);
		viewer.registerListener(controller);

		/** EXECUTE **/
		var vt = 0.0;
		var dt = 0.001;
		var iter = 0;
		var exec = Executors.newFixedThreadPool(nWorkers);

		sync.waitStart();

		while(iter < nSteps){

			var results = new ArrayList<Future<Body>>();

			bodies.forEach(body -> {
				//var res = exec.submit(new ComputeAndUpdateVelocityTask(bodies, body, dt));
				//results.add(res);
			});

			var step = new ArrayList<Body>();
			results.forEach(bodyFuture -> {
				try {
					step.add(bodyFuture.get());
				} catch (Exception e) {
					e.printStackTrace();
				}
			});

			bodies = step;
			results.clear();

			bodies.forEach(body -> {
				//var res = exec.submit(new UpdatePosTask(body, dt));
				//results.add(res);
			});

			step.clear();
			results.forEach(bodyFuture -> {
				try {
					step.add(bodyFuture.get());
				} catch (Exception e) {
					e.printStackTrace();
				}
			});

			bodies = step;
			results.clear();

			bodies.forEach(body -> {
				//var res = exec.submit(new CheckBoundaryTask(body, bounds));
				//results.add(res);
			});

			step.clear();
			results.forEach(bodyFuture -> {
				try {
					step.add(bodyFuture.get());
				} catch (Exception e) {
					e.printStackTrace();
				}
			});

			bodies = step;
			results.clear();

			vt = vt + dt;
			iter++;

			if(!flag.isSet()){
				viewer.display(bodies, vt, iter, bounds);
			}
		}
	}

	private void testWithNumBodies() {
		bounds = new Boundary(-4.0, -4.0, 4.0, 4.0);

		Random rand = new Random(System.currentTimeMillis());
		bodies = new ArrayList<>();
		for (int i = 0; i < nBodies; i++) {
			double x = bounds.getX0()*0.25 + rand.nextDouble() * (bounds.getX1() - bounds.getX0()) * 0.25;
			double y = bounds.getY0()*0.25 + rand.nextDouble() * (bounds.getY1() - bounds.getY0()) * 0.25;
			Body b = new Body(i, new P2d(x, y), new V2d(0, 0), 10);
			bodies.add(b);
		}
	}
}
