// Ladybug
import Processing;

Processing.window(400, 400);
Processing.background(255);
Processing.smooth();

// Head
Processing.fill(0);
Processing.ellipseMode(Processing.CENTER);
Processing.ellipse(200, 160, 60, 80);

// eyes
Processing.fill(255, 255, 0);
Processing.ellipse(185, 130, 5, 5);
Processing.ellipse(215, 130, 5, 5);

// antlers
Processing.fill(0);
Processing.strokeWeight(3);
Processing.line(190, 125, 170, 95);
Processing.ellipse(167, 95, 5, 5);
Processing.line(210, 125, 230, 95);
Processing.ellipse(233, 95, 5, 5);

// Body
Processing.fill(255, 0, 0);
Processing.ellipse(200, 200, 140, 120);

Processing.fill(0);
Processing.strokeWeight(5);
Processing.line(200, 140, 200, 260);
Processing.strokeWeight(1);

Processing.triangle(200, 250, 205, 260, 195, 260);

// left dots
Processing.fill(0);
Processing.ellipse(140, 200, 8, 8);
Processing.ellipse(155, 180, 7, 7);
Processing.ellipse(165, 160, 8, 8);
Processing.ellipse(158, 210, 6, 6);
Processing.ellipse(172, 197, 8, 8);
Processing.ellipse(185, 172, 9, 9);
Processing.ellipse(180, 222, 8, 8);
Processing.ellipse(164, 236, 8, 8);

// right dots
Processing.ellipse(210, 160, 8, 8);
Processing.ellipse(222, 178, 7, 7);
Processing.ellipse(235, 166, 6, 6);
Processing.ellipse(213, 240, 8, 8);
Processing.ellipse(210, 206, 9, 9);
Processing.ellipse(240, 198, 7, 7);
Processing.ellipse(252, 212, 8, 8);
Processing.ellipse(232, 222, 8, 8);
