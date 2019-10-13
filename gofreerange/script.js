// @ts-check
const canvas = document.querySelector("canvas");
const cxt = canvas.getContext("2d");
const canvasSize = 384,
  Xcenter = canvasSize/2,
  Ycenter = canvasSize/2;

cxt.strokeStyle = "#000000";
cxt.lineWidth = 2;

const numberOfSides = 6;

/**
 * Draw a hexagon
 * @param {number} size Distance from center
 */
function hexagon(size) {
  cxt.beginPath();
  // Move to the first corner of the hexagon
  cxt.moveTo(Xcenter + size * Math.cos(0), Ycenter + size * Math.sin(0));
  for (let i = 1; i <= numberOfSides; i++) {
    // Draw lines between 6 points on the hexagon
    const theta = i * 2 * Math.PI / numberOfSides;
    cxt.lineTo(Xcenter + size * Math.cos(theta), Ycenter + size * Math.sin(theta));
  }
  cxt.stroke();
}

/**
 * Draws spurs (lines branching off from spoke).
 * Spurs are draw on a straight upward spoke, but later translated to a different location.
 * @param {number} rootStart Starting Y position of the spur
 * @param {number} length How long the spur will be.
 * @param {number} spread Ending X position of the spur.
 */
function spurs(rootStart, length, spread) {
  // Start at a point in the middle of an upward spoke.
  cxt.moveTo(0, rootStart);
  // Draw a line coming off of the stroke towards the right.
  cxt.lineTo(spread, rootStart + length);
  cxt.stroke();
  // Repeat, but mirrored on the left side.
  cxt.moveTo(0, rootStart);
  cxt.lineTo(-spread, rootStart + length);
  cxt.stroke();
}

/**
 * Random number between `low` and `high`.
 * @param {number} low
 * @param {number} high
 */
function randomBetween(low, high) {
  return Math.floor((Math.random()*high)+low);
}

/**
 * Change origin of rotating to be center of canvas,
 * then perform the rotation. (By default the origin is 0,0.)
 * @param {CanvasRenderingContext2D} cxt
 * @param {number} angle
 */
function rotateFromCenter(cxt, angle) {
  cxt.translate(Xcenter, Ycenter);
  cxt.rotate(angle);
  cxt.translate(-Xcenter, -Ycenter);
}

const spurData = [
  {
    rootStart: randomBetween(30,35),
    length: randomBetween(50,55),
    spread: randomBetween(40,50),
  },
  {
    rootStart: randomBetween(65,70),
    length: randomBetween(35,40),
    spread: randomBetween(20,30),
  },
  {
    rootStart: randomBetween(90,100),
    length: randomBetween(25,35),
    spread: randomBetween(15,20),
  },
  {
    rootStart: randomBetween(130,135),
    length: randomBetween(10,20),
    spread: randomBetween(5,10),
  },
  {
    rootStart: randomBetween(145,150),
    length: randomBetween(15,20),
    spread: randomBetween(5,7),
  },
];

/**
 * Draw a spoke with 5 spurs.
 * The spoke is draw as an upward line from the center,
 * but gets rotated later on.
 */
function spoke() {
  cxt.beginPath();
  cxt.moveTo(0, 0);
  cxt.lineTo(0, Ycenter);
  cxt.stroke();

  spurData.forEach(data => spurs(data.rootStart, data.length, data.spread));
}

function multiSpoke() {
  for (let i = 1; i <= numberOfSides; i++) {
    cxt.translate(Xcenter, Ycenter);
    cxt.rotate(Math.PI / (numberOfSides / 2));
    spoke();
    cxt.translate(-Xcenter, -Ycenter);
  }
}

/**
 * Draws a random hexagon with an outline on the outer and inner edges.
 * The inner edge is some random size from the center (+- 5 from `size`),
 * and the outer edge is 3-10 points away from that.
 * @param {number} size
 */
function randomHexagon(size) {
  const a = randomBetween(size-5,size+5);
  const b = a + randomBetween(3,10);
  hexagon(a);
  hexagon(b);
}


multiSpoke();

// Rotate the hexagons to line up with the spokes
rotateFromCenter(cxt, Math.PI / numberOfSides);

// Draw 4 random hexagons
randomHexagon(30);
randomHexagon(70);
randomHexagon(80);
randomHexagon(90);
