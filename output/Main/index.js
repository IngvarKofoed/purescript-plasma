"use strict";
var Color = require("../Color");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var DOM = require("../DOM");
var DOM_HTML = require("../DOM.HTML");
var DOM_HTML_Window = require("../DOM.HTML.Window");
var Data_Array = require("../Data.Array");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Number_Format = require("../Data.Number.Format");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Tuple = require("../Data.Tuple");
var Graphics_Canvas = require("../Graphics.Canvas");
var $$Math = require("../Math");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var Signal_DOM = require("../Signal.DOM");
var Signal_Time = require("../Signal.Time");

// | Represents rgb values in the 0..1 range
var Value = (function () {
    function Value(value0) {
        this.value0 = value0;
    };
    Value.create = function (value0) {
        return new Value(value0);
    };
    return Value;
})();

// | Represents a pixel with position and RGB value
var Pixel = (function () {
    function Pixel(value0) {
        this.value0 = value0;
    };
    Pixel.create = function (value0) {
        return new Pixel(value0);
    };
    return Pixel;
})();

// | Represents a 2D position
var Coord = (function () {
    function Coord(value0) {
        this.value0 = value0;
    };
    Coord.create = function (value0) {
        return new Coord(value0);
    };
    return Coord;
})();

// | Converts Number into a byte [0;255] Int
var toByte = function (v) {
    return Data_Maybe.fromJust()(Data_Int.fromNumber($$Math.floor(v * 255.0)));
};

// | Converts an RGB value into a color
var toColor = function (v) {
    return Color.rgb(toByte(v.value0.r))(toByte(v.value0.g))(toByte(v.value0.b));
};
var showValue = new Data_Show.Show(function (v) {
    return "(" + (Data_Number_Format.toString(v.value0.r) + (", " + (Data_Number_Format.toString(v.value0.g) + (", " + (Data_Number_Format.toString(v.value0.b) + ")")))));
});
var showPixel = new Data_Show.Show(function (v) {
    return "(" + (Data_Number_Format.toString(v.value0.x) + (", " + (Data_Number_Format.toString(v.value0.y) + (", " + (Data_Show.show(showValue)(v.value0.value) + ")")))));
});
var showCoord = new Data_Show.Show(function (v) {
    return "(" + (Data_Number_Format.toString(v.value0.x) + (", " + (Data_Number_Format.toString(v.value0.y) + ")")));
});

// | Returns the intensity in the range [0..1] 
var rotateAndZoomSinusoid = function (x) {
    return function (y) {
        return function (time) {
            return $$Math.sin(4.0 * (x * $$Math.sin((time * 1.0e-3) / 2.0) + y * $$Math.cos((time * 1.0e-3) / 3.0)) + time * 1.0e-3) / 2.0 + 0.5;
        };
    };
};

// | Returns the intensity in the range [0..1] 
var ringsSinusoid = function (x) {
    return function (y) {
        return function (time) {
            var cy = y + 0.5 * $$Math.cos((time * 1.0e-3) / 3.0);
            var cx = x + 0.5 * $$Math.sin((time * 1.0e-3) / 5.0);
            return $$Math.sin($$Math.sqrt(100.0 * (cx * cx + cy * cy) + 1.0) + time * 1.0e-3) / 2.0 + 0.5;
        };
    };
};

// | Renders an arrays of pixels given a pixel render function
var renderPixels = function (rp) {
    return function (pixels) {
        return Control_Monad_Eff.foreachE(pixels)(function (v) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(rp(v.value0.x)(v.value0.y)(Color.toHexString(toColor(v.value0.value))));
        });
    };
};

// | Render a single pixel given a canvas context, width and height of pixel, position and color
var renderPixel = function (ctx) {
    return function (w) {
        return function (h) {
            return function (x) {
                return function (y) {
                    return function (color) {
                        return function __do() {
                            Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.setFillStyle(color)(ctx))();
                            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.fillRect(ctx)({
                                x: w * x, 
                                y: w * y, 
                                w: w, 
                                h: h
                            }))();
                        };
                    };
                };
            };
        };
    };
};

// | Returns the intensity in the range [0..1] 
var liniarSinusoid = function (x) {
    return function (y) {
        return function (time) {
            return $$Math.sin(x * 2.0 - time * 1.0e-3) / 2.0 + 0.5;
        };
    };
};

// | FPS signal
var fps = Signal_Time.every(50.0);

// | Creates all descrete coordinates for a bounded 2D area given width and height.
var createCoords = function (width) {
    return function (height) {
        return Control_Bind.bind(Control_Bind.bindArray)(Data_Array.range(0)(width))(function (v) {
            return Control_Bind.bind(Control_Bind.bindArray)(Data_Array.range(0)(height))(function (v1) {
                return [ new Coord({
                    x: Data_Int.toNumber(v), 
                    y: Data_Int.toNumber(v1)
                }) ];
            });
        });
    };
};

// | Normalizes the coord (>= 0) value into the range [-0.5; 0.5] given the max
var coordNorm = function (max) {
    return function (coord) {
        return coord / max - 0.5;
    };
};

// | Calculates pixels given coordinates for the pixels and a plasma generator
var calculatePixels = function (coords) {
    return function (plasma) {
        return function (normX) {
            return function (normY) {
                return function (time) {
                    return Data_Functor.map(Data_Functor.functorArray)(function (v) {
                        return new Pixel({
                            x: v.value0.x, 
                            y: v.value0.y, 
                            value: plasma(normX(v.value0.x))(normY(v.value0.y))(time)
                        });
                    })(coords);
                };
            };
        };
    };
};
var main = function __do() {
    var v = Graphics_Canvas.getCanvasElementById("canvas")();
    var canvas = Data_Maybe.fromJust()(v);
    var v1 = Graphics_Canvas.getContext2D(canvas)();
    var v2 = Graphics_Canvas.getCanvasWidth(canvas)();
    var v3 = Graphics_Canvas.getCanvasHeight(canvas)();
    var coordNormWidth = coordNorm(v2 / 10.0);
    var coordNormHeight = coordNorm(v3 / 10.0);
    var myRenderPixel = renderPixel(v1)(10.0)(10.0);
    var myRenderPixels = renderPixels(myRenderPixel);
    var coords = createCoords(50)(50);
    var plasma1 = function (x) {
        return function (y) {
            return function (time) {
                var v4 = liniarSinusoid(x)(y)(time);
                return new Value({
                    r: v4, 
                    g: v4, 
                    b: v4
                });
            };
        };
    };
    var plasma2 = function (x) {
        return function (y) {
            return function (time) {
                var v4 = rotateAndZoomSinusoid(x)(y)(time);
                return new Value({
                    r: v4, 
                    g: v4, 
                    b: v4
                });
            };
        };
    };
    var plasma3 = function (x) {
        return function (y) {
            return function (time) {
                var v4 = ringsSinusoid(x)(y)(time);
                return new Value({
                    r: v4, 
                    g: v4, 
                    b: v4
                });
            };
        };
    };
    var plasma4 = function (x) {
        return function (y) {
            return function (time) {
                var r = liniarSinusoid(x)(y)(time);
                var g = rotateAndZoomSinusoid(x)(y)(time);
                var b = ringsSinusoid(x)(y)(time);
                return new Value({
                    r: r, 
                    g: g, 
                    b: b
                });
            };
        };
    };
    var plasma5 = function (x) {
        return function (y) {
            return function (time) {
                var r = rotateAndZoomSinusoid(x)(y)(time);
                var g = rotateAndZoomSinusoid(x)(y)(time * 1.2 + 1000.0);
                var b = rotateAndZoomSinusoid(x)(y)(time * 0.8 + 5000.0);
                return new Value({
                    r: r, 
                    g: g, 
                    b: b
                });
            };
        };
    };
    var simplePixelCalculator = calculatePixels(coords)(plasma5)(coordNormWidth)(coordNormHeight);
    var loop = Signal.flippedMap(Signal.functorSignal)(fps)(function (t) {
        return myRenderPixels(simplePixelCalculator(t));
    });
    return Signal.runSignal(loop)();
};
module.exports = {
    Coord: Coord, 
    Pixel: Pixel, 
    Value: Value, 
    calculatePixels: calculatePixels, 
    coordNorm: coordNorm, 
    createCoords: createCoords, 
    fps: fps, 
    liniarSinusoid: liniarSinusoid, 
    main: main, 
    renderPixel: renderPixel, 
    renderPixels: renderPixels, 
    ringsSinusoid: ringsSinusoid, 
    rotateAndZoomSinusoid: rotateAndZoomSinusoid, 
    toByte: toByte, 
    toColor: toColor, 
    showPixel: showPixel, 
    showValue: showValue, 
    showCoord: showCoord
};
