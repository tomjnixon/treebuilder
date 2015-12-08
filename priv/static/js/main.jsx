var RBS = ReactBootstrap;

function compile_cpp(cpp_code, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/js_compile');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json.cpp_code, json.js_code, json.errors);
        }
    };
    xhr.send(JSON.stringify({
        cpp_code: cpp_code
    }));
}

function list_sketches(callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/sketches/list_sketches');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json);
        }
    };
    xhr.send();
}

function get_state(name, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/sketches/get_state');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json[0], json[1]);
        }
    };
    xhr.send(JSON.stringify({
        name: name
    }));
}

function save(name, cpp_code, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/sketches/save');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json[0], json[1]);
        }
    };
    xhr.send(JSON.stringify({
        cpp_code: cpp_code,
        name: name
    }));
}


function enable(name, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/sketches/enable');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json[0], json[1]);
        }
    };
    xhr.send(JSON.stringify({
        name: name
    }));
}

function disable(name, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/sketches/disable');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json[0], json[1]);
        }
    };
    xhr.send(JSON.stringify({
        name: name
    }));
}

function delete_sketch(name, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/sketches/delete');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json[0]);
        }
    };
    xhr.send(JSON.stringify({
        name: name
    }));
}

function show_sketch(name, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/sketches/show_sketch');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json[0], json[1]);
        }
    };
    xhr.send(JSON.stringify({
        name: name
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
    
    try {
        module._c_setup();
        module._c_loop();
    } catch (error) {
        return {error: "Failed to call setup or loop."};
        console.log(error);
    }
    
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
        var WIDTH = this._container.clientWidth;
        var HEIGHT = this._container.clientHeight;
        console.log(WIDTH);
        console.log(HEIGHT);

        // set some camera attributes
        var VIEW_ANGLE = 45;
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
        return (<div className="previewcontainer" ref={(function(container) {
            this._container = container;
        }).bind(this)} />);
    }
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

var EditWindow = React.createClass({
    getDefaultProps: function() {
        return {initial_app_state: {}};;
    },
    getInitialState: function() {
        var state = {
            name: null,
            cpp_code: null,
            editor_cpp_code: "",
            preview_cpp_code: null,
            state: "unsaved",
            errors: null,
            module: null,
            command_running: false,
        };
        _.assign(state, this.props.initial_app_state);
        if (state.cpp_code != null)
            state.editor_cpp_code = state.cpp_code;
        if (state.js_code != null)
            state.preview_cpp_code = state.cpp_code;
        
        // state.errors = "";
        // for (var i = 0; i < 50; i++)
        //     state.errors += "foo\n";
        return state;
    },
    
    componentDidMount: function() {
        this.update_preview(this.state.preview_cpp_code, this.state.js_code, this.state.errors);
    },
    
    update_cpp_code: function(new_cpp_code) {
        this.setState({
            editor_cpp_code: new_cpp_code
        });
    },
    
    update_preview: function(cpp_code, js_code, errors) {
        var module = js_code != null ? load_module(js_code) : null;
        if (module != null && "error" in module) {
            errors = module.error;
            module = null;
        }
        this.setState({
            errors: errors,
            module: module,
            preview_cpp_code: cpp_code,
        });
    },
    
    on_preview: function() {
        compile_cpp(this.state.editor_cpp_code,
                function(cpp_code, js_code, errors) {
                    this.update_preview(cpp_code, js_code, errors);
                    this.setState({command_running: false});
                }.bind(this));
        this.setState({
            command_running: true
        });
    },
    
    on_save: function() {
        var name = this.state.name
        while (name == null) {
            name = prompt("Give it a name");
            
            if (name == null)
                return;
            if (name == "")
                continue;
        }
        this.setState({name: name});
        
        save(name, this.state.editor_cpp_code, function(status, new_state) {
            this.update_preview(new_state.cpp_code, new_state.js_code, new_state.errors);
            this.setState({
                state: new_state.state,
                cpp_code: new_state.cpp_code,
                command_running: false,
            });
        }.bind(this));
        
        this.setState({command_running: true});
    },
    
    on_enable: function() {
        enable(this.state.name, function(status, new_state) {
            this.update_preview(new_state.cpp_code, new_state.js_code, new_state.errors);
            this.setState({
                state: new_state.state,
                cpp_code: new_state.cpp_code,
                command_running: false,
            });
        }.bind(this));
        
        this.setState({command_running: true});
    },
    
    on_disable: function() {
        disable(this.state.name, function(status, new_state) {
            console.log(new_state);
            this.update_preview(new_state.cpp_code, new_state.js_code, new_state.errors);
            this.setState({
                state: new_state.state,
                cpp_code: new_state.cpp_code,
                command_running: false,
            });
        }.bind(this));
        
        this.setState({command_running: true});
    },
    
    on_done: function() {
        if (this.state.editor_cpp_code != this.state.cpp_code) {
            if (!confirm("Exit without saving? Pressing OK will discard any changes."))
                return;
        }
        
        this.props.on_done_editing();
    },
    
    on_delete: function() {
        if (!confirm("Really delete this sketch?"))
            return;
        
        delete_sketch(this.state.name, function(status) {
            this.props.on_done_editing();
        }.bind(this));
    },
    
    render: function() {
        var cm_options = {
            lineNumbers: true,
            matchBrackets: true,
            mode: "text/x-c++src",
        };
        
        var code = <ReactCodeMirror value={this.state.editor_cpp_code}
                     onChange={this.update_cpp_code}
                    options={cm_options} />;
        
        var errors;
        if (this.state.errors == null)
            errors = <div/>;
        else {
            var html = {__html: ansi_up.ansi_to_html(this.state.errors)};
            errors = <pre className="errors" dangerouslySetInnerHTML={html} />;
        }
        
        var unsaved = this.state.state == "unsaved";
        var can_preview = this.state.preview_cpp_code != this.state.editor_cpp_code;
        var can_save = this.state.cpp_code != this.state.editor_cpp_code;
        
        var enable_disabled = (this.state.command_running
                                || !_.contains(["compiles", "enabled"], this.state.state));
        var enable_text = this.state.state == "enabled" ? "Disable" : "Enable";
        var enable_callback = _.get({compiles: this.on_enable, enabled: this.on_disable},
                                    this.state.state);
        var enable_button = (
                <RBS.Button
                    disabled={enable_disabled}
                    onClick={enable_callback}
                    >{enable_text}</RBS.Button>
                );
        
        var buttons = (
                <div>
                    <RBS.Button
                            disabled={this.state.command_running || !can_preview}
                            onClick={can_preview ? this.on_preview : null} >
                        Preview
                    </RBS.Button>
                    <RBS.Button
                            disabled={!can_save}
                            onClick={can_save ? this.on_save : null} >
                        Save
                    </RBS.Button>
                    <RBS.Button
                            onClick={this.on_done} >
                        Done
                    </RBS.Button>
                    <RBS.Button
                            disabled={unsaved}
                            onClick={this.on_delete} >
                        Delete
                    </RBS.Button>
                    {enable_button}
                </div>
                );
        
        var preview = (this.state.module != null
            ? <Preview module={this.state.module} />
            : null);
        
        return (
                    <div className="editor">
                        <div className="leftcol">
                            <div className="preview">
                                {preview}
                            </div>
                            {errors}
                        </div>
                        <div className="rightcol">
                            <div className="buttons">
                                {buttons}
                            </div>
                            <div className="code">
                                {code}
                            </div>
                        </div>
                    </div>
                );
    }
});

var ListWindow = React.createClass({
    getInitialState: function() {
        return {
            sketches: [],
            busy: false,
        };
    },
    componentDidMount: function() {
        this.refresh();
    },
    refresh: function() {
        list_sketches(function(sketches) {
            this.setState({
                sketches: sketches,
                busy: true
            });
        }.bind(this));
        this.setState({busy: true});
    },
    show_sketch: function(name) {
        console.log("show " + name);
        show_sketch(name, function(status) {
        });
    },
    render_sketch: function(sketch) {
        return (
            <RBS.Panel header={<h3>{sketch.name}</h3>} >
                <RBS.Button onClick={this.props.on_edit_sketch.bind(this, sketch.name)}>
                    Edit
                </RBS.Button>
                <RBS.Button
                        disabled={sketch.state != "enabled"}
                        onClick={this.show_sketch.bind(this, sketch.name)}>
                    Show on Tree
                </RBS.Button>
            </RBS.Panel>
        );
    },
    render: function() {
        return (
            <div>
                <RBS.Button onClick={this.props.on_new_sketch}>
                    New
                </RBS.Button>
                <RBS.Button onClick={this.refresh}>
                    Refresh
                </RBS.Button>
                {this.state.sketches.map(this.render_sketch)}
            </div>
        );
    }
});

var App = React.createClass({
    getInitialState: function() {
        return {
            editing: null
        };
    },
    edit_sketch: function(name) {
        get_state(name, function(status, state) {
            this.setState({editing: state});
        }.bind(this));
    },
    new_sketch: function() {
        get_test_mod(function (cpp_code) {
            this.setState({editing: {editor_cpp_code: cpp_code}});
        }.bind(this));
    },
    done_editing: function() {
        get_test_mod(function (cpp_code) {
            this.setState({editing: null});
        }.bind(this));
    },
    render: function() {
        var panel;
        if (this.state.editing == null)
            panel = <ListWindow
                        on_edit_sketch={this.edit_sketch}
                        on_new_sketch={this.new_sketch} />;
        else
            panel = <EditWindow
                        initial_app_state={this.state.editing}
                        on_done_editing={this.done_editing} />;
        
        return (<div className="top">
                    <header>treebuilder</header>
                    {panel}
                </div>);
    }
});

var Module = {
    onRuntimeInitialized: function() {
        
        ReactDOM.render(<App/>, document.getElementById('container'));
        // get_test_mod(function (cpp_code) {
        //     var element = (<div className="top">
        //                     <header>treebuilder</header>
        //                     <EditWindow initial_app_state={{editor_cpp_code: cpp_code}} />
        //                     </div>);
        //     ReactDOM.render(element, document.getElementById('container'));
        // });
    }
};
