package conc.view;



import conc.model.Body;
import conc.model.Boundary;
import conc.model.InputListener;
import conc.model.P2d;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Simulation view
 *
 * @author aricci
 *
 */
public class SimulationView {
        
	private final VisualiserFrame frame;
	
    /**
     * Creates a view of the specified size (in pixels)
     * 
     * @param w width.
     * @param h height.
     */
    public SimulationView(int w, int h){
    	frame = new VisualiserFrame(w,h);
    }
        
    public void display(List<Body> bodies, double vt, long iter, Boundary bounds){
 	   frame.display(bodies, vt, iter, bounds); 
    }

	public void registerListener(InputListener l){
		frame.registerListener(l);
	}
    
    public static class VisualiserFrame extends JFrame implements ActionListener {
        private final VisualiserPanel panel;
		private final List<InputListener> listeners = new ArrayList<>();

		public VisualiserFrame(int w, int h){
            setTitle("Bodies Simulation");
            setSize(w+100,h+100);
            setResizable(false);

            panel = new VisualiserPanel(w,h);
			JPanel controlPanel = new JPanel();
			JButton start = new JButton("start");
			controlPanel.add(start);
			JButton stop = new JButton("stop");
			controlPanel.add(stop);
			LayoutManager layout = new BorderLayout();
			JPanel cp = new JPanel();
			cp.setLayout(layout);
			cp.add(BorderLayout.NORTH, controlPanel);
			cp.add(BorderLayout.CENTER, panel);
			setContentPane(cp);

			start.addActionListener(this);
			stop.addActionListener(this);
            addWindowListener(new WindowAdapter(){
    			public void windowClosing(WindowEvent ev){
    				System.exit(-1);
    			}
    			public void windowClosed(WindowEvent ev){
    				System.exit(-1);
    			}
    		});
    		this.setVisible(true);
        }
        
        public void display(List<Body> bodies, double vt, long iter, Boundary bounds){
        	try {
	        	SwingUtilities.invokeLater(() -> {
	        		panel.display(bodies, vt, iter, bounds);
	            	repaint();
	        	});
        	} catch (Exception ex) {ex.printStackTrace();}
        }

        public void updateScale(double k) {
        	panel.updateScale(k);
        }

		@Override
		public void actionPerformed(ActionEvent e) {
			String cmd = e.getActionCommand();
			if (cmd.equals("start")){
				notifyStarted();
			} else if (cmd.equals("stop")){
				notifyStopped();
			}
		}

		public void registerListener(InputListener listener){
			this.listeners.add(listener);
		}

		public void notifyStarted(){
			listeners.forEach(InputListener::started);
		}

		public void notifyStopped(){
			listeners.forEach(InputListener::stopped);
		}
    }

    public static class VisualiserPanel extends JPanel implements KeyListener {
        
    	private List<Body> bodies;
    	private Boundary bounds;
    	
    	private long nIter;
    	private double vt;
    	private double scale = 1;
    	
        private final long dx;
        private final long dy;
        
        public VisualiserPanel(int w, int h){
            setSize(w,h);
            dx = w/2 - 20;
            dy = h/2 - 20;
			this.addKeyListener(this);
			setFocusable(true);
			setFocusTraversalKeysEnabled(false);
			requestFocusInWindow(); 
        }

        public void paint(Graphics g){    		    		
    		if (bodies != null) {
        		Graphics2D g2 = (Graphics2D) g;
        		
        		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
        		          RenderingHints.VALUE_ANTIALIAS_ON);
        		g2.setRenderingHint(RenderingHints.KEY_RENDERING,
        		          RenderingHints.VALUE_RENDER_QUALITY);
        		g2.clearRect(0,0,this.getWidth(),this.getHeight());

        		
        		int x0 = getXcoord(bounds.getX0());
        		int y0 = getYcoord(bounds.getY0());
        		
        		int wd = getXcoord(bounds.getX1()) - x0;
        		int ht = y0 - getYcoord(bounds.getY1());
        		
    			g2.drawRect(x0, y0 - ht, wd, ht);
    			
	    		bodies.forEach( b -> {
	    			P2d p = b.getPos();
			        int radius = (int) (10*scale);
			        if (radius < 1) {
			        	radius = 1;
			        }
			        g2.drawOval(getXcoord(p.getX()),getYcoord(p.getY()), radius, radius); 
			    });		    
	    		String time = String.format("%.2f", vt);
	    		g2.drawString("Bodies: " + bodies.size() + " - vt: " + time + " - nIter: " + nIter + " (UP for zoom in, DOWN for zoom out)", 2, 20);
    		}
        }
        
        private int getXcoord(double x) {
        	return (int)(dx + x*dx*scale);
        }

        private int getYcoord(double y) {
        	return (int)(dy - y*dy*scale);
        }
        
        public void display(List<Body> bodies, double vt, long iter, Boundary bounds){
            this.bodies = bodies;
            this.bounds = bounds;
            this.vt = vt;
            this.nIter = iter;
        }
        
        public void updateScale(double k) {
        	scale *= k;
        }

		@Override
		public void keyPressed(KeyEvent e) {
				if (e.getKeyCode() == 38){  		/* KEY UP */
					scale *= 1.1;
				} else if (e.getKeyCode() == 40){  	/* KEY DOWN */
					scale *= 0.9;  
				} 
		}

		public void keyReleased(KeyEvent e) {}
		public void keyTyped(KeyEvent e) {}
	}
}
