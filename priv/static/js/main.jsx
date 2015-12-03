
function compile_cpp(cpp_code, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/js_compile');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json.js_code, json.errors);
        }
    };
    xhr.send(JSON.stringify({
        cpp_code: cpp_code
    }));
}

function load_lib(src) {
    // from the top of loadDynamicLibrary
    var libModule = eval(src)(
      Runtime.alignFunctionTables(),
      Module
    );
    
    return libModule;
}

function load_module(src) {
    var module = load_lib(src);
    
    var num_leds = module._get_num_leds();
    
    var ledColors_ptr = module._get_ledColors_out();
    var ledColors = new Uint32Array(Module.HEAPU32.buffer, ledColors_ptr, num_leds);
    
    var leds_updated_ptr = module._get_leds_updated();
    var leds_updated = new Uint8Array(Module.HEAPU8.buffer, leds_updated_ptr, 1);
    
    var leds_positions_ptr = module._get_led_positions();
    var leds_positions_stride = module._get_led_positions_stride();
    var leds_positions = new Int8Array(Module.HEAP8.buffer, leds_positions_ptr,
            leds_positions_stride * num_leds);
    
    module._c_setup();
    
    return {
        dispose: function() {
            module.cleanups.forEach(function(cleanup) { cleanup() });
        },
        num_leds: num_leds,
        get_colors: function() {
            module._c_loop();

            if (leds_updated[0]) {
                leds_updated[0] = 0;
                return ledColors;
            } else {
                return null;
            }
        },
        get_positions: function() {
            var positions = [];
            for (var i = 0; i < num_leds; i++) {
                var base = leds_positions_stride * i;
                var led_position = new THREE.Vector3(
                        leds_positions[base + 0],
                        leds_positions[base + 2],
                        leds_positions[base + 1]);
                positions.push(led_position);
            }
            return positions;
        }
    };
}

var Preview = React.createClass({
    componentDidMount: function() {
        // set the scene size
        var WIDTH = 400;
        var HEIGHT = 300;

        // set some camera attributes
        var VIEW_ANGLE = 45;
        var ASPECT = WIDTH / HEIGHT;
        var NEAR = 0.1;
        var FAR = 10000;

        // create a WebGL renderer, camera
        // and a scene
        var renderer = new THREE.WebGLRenderer();
        
        var camera = new THREE.PerspectiveCamera( 45, WIDTH / HEIGHT, 1, 10000 );

        var controls = new THREE.OrbitControls( camera, this._container );

        var scene = new THREE.Scene();
        scene.fog = new THREE.FogExp2( 0x000000, 0.001 );

        // the camera starts at 0,0,0 so pull it back
        camera.position.z = 500;

        // start the renderer - set the clear colour
        // to a full black
        renderer.setClearColor(new THREE.Color(0, 1));
        renderer.setPixelRatio( window.devicePixelRatio );
	    renderer.setSize(WIDTH, HEIGHT);
	    this._container.appendChild( renderer.domElement );


        // create the particle variables
        var particles = new THREE.Geometry();
        var texture = THREE.ImageUtils.loadTexture("/assets/img/particle.png")
        var material = new THREE.PointsMaterial({
            color: 0xFFFFFF,
            size: 50,
            map: texture,
            blending: THREE.AdditiveBlending,
            depthTest: false,
            transparent: true,
            vertexColors: THREE.VertexColors
        });
        
        particles.vertices = this.props.module.get_positions();

        // Add a random color for each particle
        for(var p = 0; p < particles.vertices.length; p++) {
            var color = new THREE.Color();
            color.setHSL( Math.random(), 1.0, 0.5 );
            particles.colors.push(color);
        }

        // create the particle system
        var points = new THREE.Points(
                particles,
                material);

        points.sortParticles = true;

        // add it to the scene
        scene.add(points);

        var animationCallback = (function() {
            var colors = this.props.module.get_colors();
            if (colors !== null) {
                for (var i = 0; i < colors.length; i++)
                    particles.colors[i].set(colors[i]);
            }
            
            particles.colorsNeedUpdate = true;
            
            renderer.render(scene, camera);
            
            this._animationCallbackReq = requestAnimationFrame(animationCallback);
        }).bind(this);
        
        animationCallback();
        
        this.setState({
            scene: scene,
            points: points,
            particles: particles,
            material: material,
            texture: texture,
            controls: controls,
            renderer: renderer,
        });
    },
    
    componentWillUnmount: function() {
        if (this._animationCallbackReq !== null) {
            cancelAnimationFrame(this._animationCallbackReq);
        }
        
        // this.state.scene.dispose();
        // this.state.points.dispose();
        this.state.particles.dispose();
        this.state.material.dispose();
        this.state.texture.dispose();
        this.state.controls.dispose();
        this.state.renderer.dispose();
    },
    
    render: function() {
        return (<div ref={(function(container) {
            this._container = container;
        }).bind(this)} />);
    }
});

var EditorPreview = React.createClass({
    getInitialState: function() {
        return {
            cpp_code: this.props.initial_cpp_code,
            state: "initial",
            errors: null,
            module: null,
        };
    },
    compile: function() {
        if (this.state.state == "compiling")
            return;
        
        if (this.state.module != null) {
            this.state.module.dispose();
            this.setState({module: null});
        }
        
        compile_cpp(this.state.cpp_code, (function(js_code, errors) {
            if (js_code === null) {
                this.setState({state: "error"});
            } else {
                this.setState({
                    state: "compiled",
                    module: load_module(js_code),
                });
            }
            this.setState({errors: errors});
        }).bind(this));
        
        this.setState({state: "compiling"});
    },
    update_cpp_code: function(new_cpp_code) {
        this.setState({cpp_code: new_cpp_code});
    },
    render: function() {
        var preview = (this.state.state == "compiled"
            ? <Preview module={this.state.module} />
            : <div>{this.state.state}</div>);
        
        var cm_options = {
            lineNumbers: true,
            matchBrackets: true,
            mode: "text/x-c++src",
        };
        
        var errors;
        if (this.state.errors == null)
            errors = <div/>;
        else {
            var html = {__html: ansi_up.ansi_to_html(this.state.errors)};
            errors = <pre dangerouslySetInnerHTML={html} />;
        }
        
        return (<div>
                    {preview}
                    <button onClick={this.compile} >Preview</button>
                    <ReactCodeMirror value={this.state.cpp_code} onChange={this.update_cpp_code} options={cm_options} />
                    {errors}
                </div>
                );
    },
});



function get_test_mod(callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/assets/test.cpp');
    xhr.onload = function() {
        if (xhr.status === 200) {
            callback(xhr.responseText);
        }
    };
    xhr.send();
}

function test() {
    get_test_mod(function (cpp_code) {
        ReactDOM.render(
                <EditorPreview initial_cpp_code={cpp_code} />,
                document.getElementById('container')
                );
    });
}

var Module = {
    onRuntimeInitialized: function() {
        test();
    }
};
