import Myro;
sim = new Myro.Simulation();
robot = (Myro.Robot)Myro.makeRobot("SimScribbler", sim);
robot.forward(1,1);