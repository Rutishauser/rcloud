((function() {

    var shaderProgram;
    var vertexBuffer;
    var colorBuffer;

    var edgeL, edgeP, edgeM;
    var faceL, faceP, faceM, faceP2;

    var M, V, dV, dS;

	function mxm( A, B ) {
	    var T = new Array(9);
	    for(var I=0; I != 3; I++) for(var J=0; J != 3; J++) {
		T[3*I+J] = 0.0;
		for(var K=0; K != 3; K++) T[3*I+J] = T[3*I+J] + A[3*I+K] * B[3*K+J];
	    }
	    return T;
	}

	function e() {
	    var T = new Array(9);
	    for(var I=0; I != 3; I++) for(var J=0; J != 3; J++) {
		T[3*I+J] = 0.0;
		if( I == J ) T[3*I+J] = 1.0;
	    }
	    return T;
	}

	function rr( M, J, K, beta ) {
	    if( J > 2 ) return;
	    if( K > 2 ) return;
	    M[3*J+J] = Math.cos(beta);
	    M[3*J+K] = Math.sin(beta);
	    M[3*K+J] = - Math.sin(beta);
	    M[3*K+K] = Math.cos(beta);
	}

	function z( shift ) {
	    var T = new Array(3);
	    for(var I=0; I != 3; I++) T[I] = 0.0;
	    T[2] = shift;
	    return T;
	}

	function vav( A, B ) {
	    var T = new Array(3);
	    for(var I=0; I != 3; I++) T[I] = A[I] + B[I];
	    return T;
	}

	function vsv( A, B ) {
	    var T = new Array(3);
	    for(var I=0; I != 3; I++) T[I] = A[I] - B[I];
	    return T;
	}

	function vxn( A, N ) {
	    var T = new Array(3);
	    for(var I=0; I != 3; I++) T[I] = A[I] * N;
	    return T;
	}

	function mxv( A, B ) {
	    var T = new Array(3);
	    for(var I=0; I != 3; I++)
	    {
		T[I] = 0.0;
		// A[3*J+I] is important
		for(var J=0; J != 3; J++) T[I] = T[I] + A[3*I+J] * B[J];
		// for(var J=0; J != 3; J++) T[I] = T[I] + A[3*J+I] * B[J];
	    }
	    return T;
	}

	function inv( a ) {
	    var T = new Array(9);
	    var det = a[0]*(a[4]*a[8]-a[7]*a[5]) - a[1]*(a[3]*a[8]-a[5]*a[6]) + a[2]*(a[3]*a[7]-a[4]*a[6]); // adjoin
	    T[0] =  (a[4]*a[8] - a[7]*a[5]) / det;
	    T[1] = -(a[3]*a[8] - a[5]*a[6]) / det;
	    T[2] =  (a[3]*a[7] - a[6]*a[4]) / det;
	    T[3] = -(a[1]*a[8] - a[2]*a[7]) / det;
	    T[4] =  (a[0]*a[8] - a[2]*a[6]) / det;
	    T[5] = -(a[0]*a[7] - a[6]*a[1]) / det;
	    T[6] =  (a[1]*a[5] - a[2]*a[4]) / det;
	    T[7] = -(a[0]*a[5] - a[3]*a[2]) / det;
	    T[8] =  (a[0]*a[4] - a[3]*a[1]) / det;
	    return T;
	}

    CoreSpin = function(x, y, f)
    {
	edgeL = x.length;  edgeP = edgeL / 4;  edgeM = edgeP / 2;
	faceL = y.length;  faceP = faceL / 4;  faceM = faceP / 4;  faceP2 = faceM * 3;
	console.log("edgeL = " + edgeL + ", edgeP = " + edgeP + ", faceP2 = " + faceP2 );
	var coor = new Array( (edgeP+faceP2)*4 );
	var clr = new Array( (edgeP+faceP2)*3 );
	for(var j=0; j<edgeP; j++)
	{
	    coor[j*4 + 0] = x[j*4 + 0];  // x
	    coor[j*4 + 1] = x[j*4 + 1];  // y
	    coor[j*4 + 2] = x[j*4 + 2];  // z
	    coor[j*4 + 3] = x[j*4 + 3];  // color

	    clr[3*j + 0] = x[j*4 + 0];
	    clr[3*j + 1] = x[j*4 + 1];
	    clr[3*j + 2] = x[j*4 + 2];
	}
	for(var k=0; k<faceM; k++) // M triangles
	{
	    for(var j=0; j!=3; j++) // 3 out of 4 points
	    {
		coor[edgeL + (k*3 + j)*4 + 0] = y[ (k*4 + j) * 4 + 0 ];  // x
		coor[edgeL + (k*3 + j)*4 + 1] = y[ (k*4 + j) * 4 + 1 ];  // y
		coor[edgeL + (k*3 + j)*4 + 2] = y[ (k*4 + j) * 4 + 2 ];  // z
		coor[edgeL + (k*3 + j)*4 + 3] = y[ (k*4 + j) * 4 + 3 ];  // color
	    }
	    for(var j=0; j!=3; j++) // the fourth point
	    {
		clr[edgeP*3 + (k*3 + j)*3 + 0] = y[ (k*4 + 3) * 4 + 0 ];  // x
		clr[edgeP*3 + (k*3 + j)*3 + 1] = y[ (k*4 + 3) * 4 + 1 ];  // y
		clr[edgeP*3 + (k*3 + j)*3 + 2] = y[ (k*4 + 3) * 4 + 2 ];  // z

		// console.log("clr[" + (edgeL + (k*3 + j)*3 + 0) + "]  k=" + k + " j=" + j);
		// console.log("  y[" + ( (k*4 + 4) * 4 + 0 ) + "]  k=" + k + " j=" + j + " == " + y[ (k*4 + 3) * 4 + 0 ] )

		if( y[ (k*4 + 3) * 4 + 3 ] != -1.0 )
		{
		    console.log("  y[" + ( (k*4 + 3) * 4 + 3 ) + "]  k=" + k + " j=" + j + " == " + y[ (k*4 + 3) * 4 + 3 ] + " != -1.0  // n clr");
		}
	    }
	}
	for(var t=0; t!=edgeP; t++)
	{
	    // console.log("EDGE " + t + "(" + t + ") " + clr[t] + "," + clr[t+1] + "," + clr[t+2] );
	}
	for(var t=0; t!=faceP2; t++)
	{
	    var b = edgeP*3 + t*3;
	    // console.log("FACE " + t + "(" + b + ") " + clr[b] + "," + clr[b+1] + "," + clr[b+2] );
	}

        var width = f.WIDTH, height = f.HEIGHT;
	var canvas = $("<canvas id='abc'></canvas>")[0]; // width='" + width + "' height='" + height + "'></canvas>")[0];
	canvas.width = width;
	canvas.height = height;

	var gl;
        try {
          gl = canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
          gl.viewportWidth = canvas.width;
          gl.viewportHeight = canvas.height;
        } catch(e) {
        }
        if (!gl) {
          return "Could not initialise WebGL,";
        }

	console.log("*** We have GL ***");


	// mouse events
        canvas.onmousedown = handleMouseDown;
        document.onmouseup = handleMouseUp;
        document.onmousemove = handleMouseMove;

        gl.viewportWidth = canvas.width;
        gl.viewportHeight = canvas.height;

        initShaders(gl);

        initBuffers(gl, coor, clr);

        gl.clearColor(0.05, 0.05, 0.05, 1.0);
	gl.enable(gl.DEPTH_TEST);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        // draw(gl);

	var requestAnimFrame = (function(){
          return  window.requestAnimationFrame   ||
              window.webkitRequestAnimationFrame ||
              window.mozRequestAnimationFrame    ||
              window.oRequestAnimationFrame      ||
              window.msRequestAnimationFrame     ||
             function(callback, element) {
               return window.setTimeout(callback, 1000/60);
             };
	})();

	var delta0 = 0.002;
	var delta1 = 0.02;
	var delta = 0;        // init with delta0

	var Rx  = e();   rr(Rx,  1, 2, delta1 );
	var RxT = e();   rr(RxT, 2, 1, delta1 );
	var Ry  = e();   rr(Ry,  0, 2, delta1 );
	var RyT = e();   rr(RyT, 2, 0, delta1 );
	var S = z(0.02);
	var dS0 = z(0.0025);
	var dS00 = vsv( z(0.0), dS0 );

	dV = z(0.0);
	dS = z(0.0);

	function handleKeyUp(e){
	    switch(e.keyCode)
	    {
		case 16:  // shift
	            break;
		case 17:  // ctrl
	            dV = z(0.0);
	            dS = z(0.0);
	            break;
		case 18:  // alt
	            dV = z(0.0);
	            dS = z(0.0);
	            break;
	    }
	}
	function handleKeyDown(e){
	    switch(e.keyCode)
	    {
	        case 39:  // right
		    M = mxm( M, RyT );
		    break;
	        case 37:  // left
		    M = mxm( M, Ry );
		    break;
	        case 40:  // down
		    M = mxm( M, Rx );
		    break;
	        case 38:  // up
		    M = mxm( M, RxT );
		    break;
		case 32:  // space
	            V = z(0.0);
	            dV = z(0.0);
	            break;
		case 13:  // enter
		    var MI = inv(M);
		    var T = mxv( MI, S );
	            V = vav( V, T );
	            break;
		case 16:  // shift
	            break;
		case 17:  // ctrl
		    dS = dS0;
		    var MI = inv(M);
		    dV = mxv( MI, dS );
	            break;
		case 18:  // alt
		    dS = dS00;
		    var MI = inv(M);
		    dV = mxv( MI, dS );
	            break;
	    }
	}

	document.addEventListener('keydown', handleKeyDown, false);
	document.addEventListener('keyup', handleKeyUp, false);

      var mouseDown = false;
      var lastMouseX = null;
      var lastMouseY = null;

      function handleMouseDown(event) {
        mouseDown = true;
        lastMouseX = event.clientX;
        lastMouseY = event.clientY;
      }

      function handleMouseUp(event) {
        mouseDown = false;
      }

      function handleMouseMove(event) {
        if (!mouseDown) {
          return;
        }
        var newX = event.clientX;
        var newY = event.clientY;

        var deltaX = newX - lastMouseX;
        var deltaY = newY - lastMouseY;

        // mat4.rotate(newRotationMatrix, degToRad(deltaY / 10), [1, 0, 0]);

	var s = 250.0;

	    var Ry = e();
	    rr(Ry, 1, 2, deltaY / s );
	    M = mxm( M, Ry );

	    var Rx = e();
	    rr(Rx, 2, 0, deltaX / s );
	    M = mxm( M, Rx );

	    var MI = inv(M);
	    dV = mxv( MI, dS );

        lastMouseX = newX
        lastMouseY = newY;
      }

	//  L O O P

	var currentTime = Date.now();
	(function animloop(){

	    var now = Date.now();
	    var deltaT = now - currentTime;
	    currentTime = now;

	    var deltaV = vxn( dV, deltaT / 10.0 );

	    V = vav( V, deltaV );
	    gl.uniformMatrix3fv(shaderProgram.RotationMatrix, false, M);
	    gl.uniform3fv(shaderProgram.ShiftVector, V);

	    draw(gl);
	    requestAnimFrame(animloop, canvas);
	})();

	return canvas;
    }

    function initShaders(gl)
    {
	var fSh =   "varying highp vec4 vColor;" +
		    "  void main(void) {" +
		    "    gl_FragColor = vColor;" +
		    "  }" ;
        var fragmentShader = getShader(gl, gl.FRAGMENT_SHADER, fSh);

	var vSh =   "attribute vec4 aVertexPosition;" +
		    "attribute vec3 aVertexNormal;" +
		    "uniform mat4 uPMatrix;" +
		    "uniform mat3 uRMatrix;" +
		    "uniform vec3 uMVector;" +
		    "varying highp vec4 vColor;" +
		    "vec4 color(float w, float u);" +
		    "float up(float w, float a);" +
		    "float down(float w, float a);" +
		    "  void main(void) {" +
		    "    vec3 pos = vec3(aVertexPosition.x, aVertexPosition.y, aVertexPosition.z);" +
		    "    float w = aVertexPosition.w;" +
		    "    {" +
		    "      pos = pos + uMVector;" +
		    "      pos = uRMatrix * pos;" +
		    "      pos = pos + vec3( 0, 0, -1.5 );" +
		    "    }" +
		    "    gl_Position = uPMatrix * vec4(pos, 1.0);" +
		    "    float clr = 0.333*(4.0-gl_Position.z);" +
		    "    vColor = color(w, clr);" +
		    "    vec3 delta = vec3(aVertexPosition.x, aVertexPosition.y, aVertexPosition.z) - aVertexNormal;" +
		    "    if( length(delta) == 0.0 ) /* edge */ return;" +
		    "    /* light */" +
		    "    vec3 light = vec3(1.0, 2.0, 3.0);" +
		    "    light = normalize(light);" +
		    "    delta = uRMatrix * delta;" +
		    "    delta = normalize(delta);" +
		    "    float ref = dot(light, delta);" +
		    "    ref = ref * 0.75 + 0.25;" +
		    "    vColor = vColor * ref;" +
		    "    vColor.w = 1.0;" +
		    "  }" +
		    "  vec4 color(float w, float clr) {" +
		    "      vColor = vec4( clr, clr, clr, 1.0 );" +
		    "      if( w <= 1.0 ) { vColor = vec4( up(w,clr),       up(w,clr),       up(w,clr),       1.0 );  return vColor; }" +
		    "      if( w <= 2.0 ) { vColor = vec4( clr,             down(w-1.0,clr), down(w-1.0,clr), 1.0 );  return vColor; }" +
		    "      if( w <= 3.0 ) { vColor = vec4( clr,             up(w-2.0,clr),   0.0,             1.0 );  return vColor; }" +
		    "      if( w <= 4.0 ) { vColor = vec4( down(w-3.0,clr), clr,             0.0,             1.0 );  return vColor; }" +
		    "      if( w <= 5.0 ) { vColor = vec4( 0.0,             down(w-4.0,clr), up(w-4.0,clr),   1.0 );  return vColor; }" +
		    "      if( w <= 6.0 ) { vColor = vec4( up(w-5.0,clr),   0.0,             down(w-5.0,clr), 1.0 );  return vColor; }" +
		    "      if( w <= 7.0 ) { vColor = vec4( down(w-6.0,clr), 0.0,             0.0,             1.0 );  return vColor; }" +
		    "      return vColor;" +
		    "  }" +
		    "  float down(float w, float clr) {" +
		    "    return clr * (1.0 - w);" +
		    "  }" +
		    "  float up(float w, float clr) {" +
		    "    return clr * w;" +
		    "  }" ;
        var vertexShader = getShader(gl, gl.VERTEX_SHADER, vSh);

        shaderProgram = gl.createProgram();
        gl.attachShader(shaderProgram, vertexShader);
        gl.attachShader(shaderProgram, fragmentShader);
        gl.linkProgram(shaderProgram);

        if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
	    alert("The shader is not linked.");
        }

        gl.useProgram(shaderProgram);
        // to set shader program attributes
        shaderProgram.vertexPositionAttribute = gl.getAttribLocation(shaderProgram, "aVertexPosition");
        gl.enableVertexAttribArray(shaderProgram.vertexPositionAttribute);
        shaderProgram.vertexColorAttribute = gl.getAttribLocation(shaderProgram, "aVertexNormal");
        gl.enableVertexAttribArray(shaderProgram.vertexColorAttribute);
        // to create uniform variables for shader program
        shaderProgram.ProjMatrix = gl.getUniformLocation(shaderProgram, "uPMatrix");
        shaderProgram.Color = gl.getUniformLocation(shaderProgram, "uColor");
        shaderProgram.RotationMatrix = gl.getUniformLocation(shaderProgram, "uRMatrix");
        shaderProgram.ShiftVector = gl.getUniformLocation(shaderProgram, "uMVector");
    }

    // to create a shader
    function getShader(gl,type,source) {
        var shader = gl.createShader(type);
        gl.shaderSource(shader, source);
        // to compile the shader
        gl.compileShader(shader);

        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            alert("SHADER COMPILATION ERROR: " + gl.getShaderInfoLog(shader));
            gl.deleteShader(shader);
            return null;
        }
        return shader;
    }

    function initBuffers(gl, coor, clr) {
      // to set vertex buffer
      vertexBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(coor), gl.STATIC_DRAW);
      vertexBuffer.itemSize = 4;
      vertexBuffer.numberOfItems = coor.length / 4;
      // to set color buffer
      colorBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(clr), gl.STATIC_DRAW);
      colorBuffer.itemSize = 3;
      colorBuffer.numberOfItems = clr.length / 3;

      var pMatrix = new Array(16);
      pMatrix[0] = 1.3099021911621094;
      pMatrix[1] = 0;
      pMatrix[2] = 0;
      pMatrix[3] = 0;
      pMatrix[4] = 0;
      pMatrix[5] = 1.7465362548828125;
      pMatrix[6] = 0;
      pMatrix[7] = 0;
      pMatrix[8] = 0;
      pMatrix[9] = 0;
      pMatrix[10] = -1.0020020008087158;
      pMatrix[11] = -1;
      pMatrix[12] = 0;
      pMatrix[13] = 0;
      pMatrix[14] = -0.20020020008087158;
      pMatrix[15] = 0;

      M = e();
      V = z(0.0);
      dV = z(0.0);
      dS = z(0.0);

      gl.uniformMatrix4fv(shaderProgram.ProjMatrix, false, pMatrix);
      gl.uniformMatrix3fv(shaderProgram.RotationMatrix, false, M);
      gl.uniform3fv(shaderProgram.ShiftVector, V);
    }

    function draw(gl)
    {
        gl.viewport(0, 0, gl.viewportWidth, gl.viewportHeight);

        gl.clear(gl.COLOR_BUFFER_BIT);

        gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
        gl.vertexAttribPointer(shaderProgram.vertexPositionAttribute, vertexBuffer.itemSize, gl.FLOAT, false, 0, 0);
        gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
        gl.vertexAttribPointer(shaderProgram.vertexColorAttribute, colorBuffer.itemSize, gl.FLOAT, false, 0, 0);

        // to draw segments and triangles
        // gl.drawElements(gl.LINES, indexBuffer.numberOfItems, gl.UNSIGNED_SHORT,0);
	gl.drawArrays(gl.LINES, 0, edgeP);
        gl.drawArrays(gl.TRIANGLES, edgeP, faceP2);
	gl.flush();
    }

return {
    init: function(k) {
        k();
    },
    Msg: function(k) {
	// alert("Spin v 1.0");
	console.log("*** Spin v 1.0 ***");
	k();
    },
    Spin: function(x, y, f, k) {
	k(function() { return CoreSpin(x, y, f); });
    }
};

})()) /*jshint -W033 */
