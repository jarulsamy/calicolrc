using System;
using System.Collections.Generic;

public static class Extensions {
  public static T[] Slice<T>(this T[] source, int start, int end) {
    if (start == 0 && end == source.Length)
        return source;
        // Handles negative ends.
        if (end < 0) {
          end = source.Length + end;
        }
        int len = end - start;
        // Return new array.
        T[] res = new T[len];
        for (int i = 0; i < len; i++) {
          res[i] = source[i + start];
        }
        return res;
  }
  public static bool isspace(this string source) {
		return (source == " ");
  }
}

public class XDotAttrParser {
    // """Parser for xdot drawing attributes.
    // See also:
    // - http://www.graphviz.org/doc/info/output.html#d:xdot
    
	int pos;
	DotParser parser;
	string buf;
	Pen pen;
	List<Shape> shapes;
	
    public XDotAttrParser(DotParser parser, string buf) {
        this.parser = parser;
        this.buf = buf;
        this.pos = 0;
        
        this.pen = new Pen();
        this.shapes = new List<Shape>();
	}

    public void __nonzero__() {
        return this.pos < this.buf.Length;
	}

    public string read_code() {
        int pos = this.buf.IndexOf(' ', this.pos);
        string res = this.buf.Slice(this.pos, pos);
        this.pos++;
        while (this.pos < this.buf.Length && this.buf.Substring(this.pos, 1).isspace()) {
            this.pos++;
		}
        return res;
	}

    public int read_number() {
        return Convert.ToInt32(this.read_code());
	}

    public double read_float() {
        return Convert.ToDouble(this.read_code());
	}

    public Point read_point() {
        int x = this.read_number();
        int y = this.read_number();
        return this.transform(x, y);
	}

    public string read_text() {
        int num = this.read_number();
        int pos = this.buf.find("-", this.pos) + 1;
        this.pos = pos + num;
        string res = this.buf.Slice(pos, this.pos);
        while (this.pos < this.buf.Length && this.buf.Substring(this.pos, 1).isspace()) {
            this.pos++;
		}
        return res;
	}

    public void read_polygon() {
        int n = this.read_number();
        p = [];
        for i in range(n) {
            x, y = this.read_point()
            p.append((x, y))
        return p

    public void read_color() {
        // See http://www.graphviz.org/doc/info/attrs.html#k:color
        c = this.read_text()
        c1 = c[:1]
        if c1 == '#':
            hex2float = lambda h: float(int(h, 16)/255.0)
            r = hex2float(c[1:3])
            g = hex2float(c[3:5])
            b = hex2float(c[5:7])
            try:
                a = hex2float(c[7:9])
            except (IndexError, ValueError) {
                a = 1.0
            return r, g, b, a
        elif c1.isdigit() or c1 == ".":
            // "H,S,V" or "H S V" or "H, S, V" or any other variation
            h, s, v = map(float, c.replace(",", " ").split())
            r, g, b = colorsys.hsv_to_rgb(h, s, v)
            a = 1.0
            return r, g, b, a
        else:
            return this.lookup_color(c)

    public void lookup_color(c) {
        try:
            color = gtk.gdk.color_parse(c)
        except ValueError:
            pass
        else:
            s = 1.0/65535.0
            r = color.red*s
            g = color.green*s
            b = color.blue*s
            a = 1.0
            return r, g, b, a

        try:
            dummy, scheme, index = c.split('/')
            r, g, b = brewer_colors[scheme][int(index)]
        except (ValueError, KeyError) {
            pass
        else:
            s = 1.0/255.0
            r = r*s
            g = g*s
            b = b*s
            a = 1.0
            return r, g, b, a
                
        sys.stderr.write("unknown color '%s'\n" % c)
        return None

    public void parse() {
        s = self

        while s:
            op = s.read_code()
            if op == "c":
                color = s.read_color()
                if color is not None:
                    this.handle_color(color, filled=False)
            elif op == "C":
                color = s.read_color()
                if color is not None:
                    this.handle_color(color, filled=True)
            elif op == "S":
                // http://www.graphviz.org/doc/info/attrs.html#k:style
                style = s.read_text()
                if style.startswith("setlinewidth(") {
                    lw = style.split("(")[1].split(")")[0]
                    lw = float(lw)
                    this.handle_linewidth(lw)
                elif style in ("solid", "dashed", "dotted") {
                    this.handle_linestyle(style)
            elif op == "F":
                size = s.read_float()
                name = s.read_text()
                this.handle_font(size, name)
            elif op == "T":
                x, y = s.read_point()
                j = s.read_number()
                w = s.read_number()
                t = s.read_text()
                this.handle_text(x, y, j, w, t)
            elif op == "E":
                x0, y0 = s.read_point()
                w = s.read_number()
                h = s.read_number()
                this.handle_ellipse(x0, y0, w, h, filled=True)
            elif op == "e":
                x0, y0 = s.read_point()
                w = s.read_number()
                h = s.read_number()
                this.handle_ellipse(x0, y0, w, h, filled=False)
            elif op == "L":
                points = this.read_polygon()
                this.handle_line(points)
            elif op == "B":
                points = this.read_polygon()
                this.handle_bezier(points, filled=False)
            elif op == "b":
                points = this.read_polygon()
                this.handle_bezier(points, filled=True)
            elif op == "P":
                points = this.read_polygon()
                this.handle_polygon(points, filled=True)
            elif op == "p":
                points = this.read_polygon()
                this.handle_polygon(points, filled=False)
            elif op == "I":
                x0, y0 = s.read_point()
                w = s.read_number()
                h = s.read_number()
                path = s.read_text()
                this.handle_image(x0, y0, w, h, path)
            else:
                sys.stderr.write("unknown xdot opcode '%s'\n" % op)
                break

        return this.shapes
    
    public Point transform(x, y) {
        return this.parser.transform(x, y);
	}

    public void handle_color(color, filled=False) {
        if filled:
            this.pen.fillcolor = color
        else:
            this.pen.color = color

    public void handle_linewidth(linewidth) {
        this.pen.linewidth = linewidth

    public void handle_linestyle(style) {
        if style == "solid":
            this.pen.dash = ()
        elif style == "dashed":
            this.pen.dash = (6, )       // 6pt on, 6pt off
        elif style == "dotted":
            this.pen.dash = (2, 4)       // 2pt on, 4pt off

    public void handle_font(size, name) {
        this.pen.fontsize = size
        this.pen.fontname = name

    public void handle_text(x, y, j, w, t) {
        this.shapes.append(TextShape(this.pen, x, y, j, w, t))

    public void handle_ellipse(x0, y0, w, h, filled=False) {
        if filled:
            // xdot uses this to mean "draw a filled shape with an outline"
            this.shapes.append(EllipseShape(this.pen, x0, y0, w, h, filled=True))
        this.shapes.append(EllipseShape(this.pen, x0, y0, w, h))

    public void handle_image(x0, y0, w, h, path) {
        this.shapes.append(ImageShape(this.pen, x0, y0, w, h, path))

    public void handle_line(points) {
        this.shapes.append(LineShape(this.pen, points))

    public void handle_bezier(points, filled=False) {
        if filled:
            // xdot uses this to mean "draw a filled shape with an outline"
            this.shapes.append(BezierShape(this.pen, points, filled=True))
        this.shapes.append(BezierShape(this.pen, points))

    public void handle_polygon(points, filled=False) {
        if filled:
            // xdot uses this to mean "draw a filled shape with an outline"
            this.shapes.append(PolygonShape(this.pen, points, filled=True))
        this.shapes.append(PolygonShape(this.pen, points))


EOF = -1
SKIP = -2


class Scanner:
    """Stateless scanner."""

    // should be overriden by derived classes
    tokens = []
    symbols = {}
    literals = {}
    ignorecase = False

    public void __init__() {
        flags = re.DOTALL
        if this.ignorecase:
            flags |= re.IGNORECASE
        this.tokens_re = re.compile(
            '|'.join(['(' + regexp + ')' for type, regexp, test_lit in this.tokens]),
             flags
        )

    public void next(buf, pos) {
        if pos >= len(buf) {
            return EOF, '', pos
        mo = this.tokens_re.match(buf, pos)
        if mo:
            text = mo.group()
            type, regexp, test_lit = this.tokens[mo.lastindex - 1]
            pos = mo.end()
            if test_lit:
                type = this.literals.get(text, type)
            return type, text, pos
        else:
            c = buf[pos]
            return this.symbols.get(c, None), c, pos + 1


class Token:

    public void __init__(type, text, line, col) {
        this.type = type
        this.text = text
        this.line = line
        this.col = col


class Lexer:

    // should be overriden by derived classes
    scanner = None
    tabsize = 8

    newline_re = re.compile(r'\r\n?|\n')

    public void __init__(buf = None, pos = 0, filename = None, fp = None) {
        if fp is not None:
            try:
                fileno = fp.fileno()
                length = os.path.getsize(fp.name)
                import mmap
            except:
                // read whole file into memory
                buf = fp.read()
                pos = 0
            else:
                // map the whole file into memory
                if length:
                    // length must not be zero
                    buf = mmap.mmap(fileno, length, access = mmap.ACCESS_READ)
                    pos = os.lseek(fileno, 0, 1)
                else:
                    buf = ''
                    pos = 0

            if filename is None:
                try:
                    filename = fp.name
                except AttributeError:
                    filename = None

        this.buf = buf
        this.pos = pos
        this.line = 1
        this.col = 1
        this.filename = filename

    public void next() {
        while True:
            // save state
            pos = this.pos
            line = this.line
            col = this.col

            type, text, endpos = this.scanner.next(this.buf, pos)
            assert pos + len(text) == endpos
            this.consume(text)
            type, text = this.filter(type, text)
            this.pos = endpos

            if type == SKIP:
                continue
            elif type is None:
                msg = 'unexpected char '
                if text >= ' ' and text <= '~':
                    msg += "'%s'" % text
                else:
                    msg += "0x%X" % ord(text)
                raise ParseError(msg, this.filename, line, col)
            else:
                break
        return Token(type = type, text = text, line = line, col = col)

    public void consume(text) {
        // update line number
        pos = 0
        for mo in this.newline_re.finditer(text, pos) {
            this.line += 1
            this.col = 1
            pos = mo.end()

        // update column number
        while True:
            tabpos = text.find('\t', pos)
            if tabpos == -1:
                break
            this.col += tabpos - pos
            this.col = ((this.col - 1)//this.tabsize + 1)*this.tabsize + 1
            pos = tabpos + 1
        this.col += len(text) - pos


class Parser:

    public void __init__(lexer) {
        this.lexer = lexer
        this.lookahead = this.lexer.next()

    public void match(type) {
        if this.lookahead.type != type:
            raise ParseError(
                msg = 'unexpected token %r' % this.lookahead.text, 
                filename = this.lexer.filename, 
                line = this.lookahead.line, 
                col = this.lookahead.col)

    public void skip(type) {
        while this.lookahead.type != type:
            this.consume()

    public void consume() {
        token = this.lookahead
        this.lookahead = this.lexer.next()
        return token


ID = 0
STR_ID = 1
HTML_ID = 2
EDGE_OP = 3

LSQUARE = 4
RSQUARE = 5
LCURLY = 6
RCURLY = 7
COMMA = 8
COLON = 9
SEMI = 10
EQUAL = 11
PLUS = 12

STRICT = 13
GRAPH = 14
DIGRAPH = 15
NODE = 16
EDGE = 17
SUBGRAPH = 18


class DotScanner(Scanner) {

    // token regular expression table
    tokens = [
        // whitespace and comments
        (SKIP,
            r'[ \t\f\r\n\v]+|'
            r'//[^\r\n]*|'
            r'/\*.*?\*/|'
            r'#[^\r\n]*',
        False),

        // Alphanumeric IDs
        (ID, r'[a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*', True),

        // Numeric IDs
        (ID, r'-?(?:\.[0-9]+|[0-9]+(?:\.[0-9]*)?)', False),

        // String IDs
        (STR_ID, r'"[^"\\]*(?:\\.[^"\\]*)*"', False),

        // HTML IDs
        (HTML_ID, r'<[^<>]*(?:<[^<>]*>[^<>]*)*>', False),

        // Edge operators
        (EDGE_OP, r'-[>-]', False),
    ]

    // symbol table
    symbols = {
        '[': LSQUARE,
        ']': RSQUARE,
        '{': LCURLY,
        '}': RCURLY,
        ',': COMMA,
        ':': COLON,
        ';': SEMI,
        '=': EQUAL,
        '+': PLUS,
    }

    // literal table
    literals = {
        'strict': STRICT,
        'graph': GRAPH,
        'digraph': DIGRAPH,
        'node': NODE,
        'edge': EDGE,
        'subgraph': SUBGRAPH,
    }

    ignorecase = True


class DotLexer(Lexer) {

    scanner = DotScanner()

    public void filter(type, text) {
        // TODO: handle charset
        if type == STR_ID:
            text = text[1:-1]

            // line continuations
            text = text.replace('\\\r\n', '')
            text = text.replace('\\\r', '')
            text = text.replace('\\\n', '')
            
            // quotes
            text = text.replace('\\"', '"')

            // layout engines recognize other escape codes (many non-standard)
            // but we don't translate them here

            type = ID

        elif type == HTML_ID:
            text = text[1:-1]
            type = ID

        return type, text


class DotParser(Parser) {

    public void __init__(lexer) {
        Parser.__init__(lexer)
        this.graph_attrs = {}
        this.node_attrs = {}
        this.edge_attrs = {}

    public void parse() {
        this.parse_graph()
        this.match(EOF)

    public void parse_graph() {
        if this.lookahead.type == STRICT:
            this.consume()
        this.skip(LCURLY)
        this.consume()
        while this.lookahead.type != RCURLY:
            this.parse_stmt()
        this.consume()

    public void parse_subgraph() {
        id = None
        if this.lookahead.type == SUBGRAPH:
            this.consume()
            if this.lookahead.type == ID:
                id = this.lookahead.text
                this.consume()
        if this.lookahead.type == LCURLY:
            this.consume()
            while this.lookahead.type != RCURLY:
                this.parse_stmt()
            this.consume()
        return id

    public void parse_stmt() {
        if this.lookahead.type == GRAPH:
            this.consume()
            attrs = this.parse_attrs()
            this.graph_attrs.update(attrs)
            this.handle_graph(attrs)
        elif this.lookahead.type == NODE:
            this.consume()
            this.node_attrs.update(this.parse_attrs())
        elif this.lookahead.type == EDGE:
            this.consume()
            this.edge_attrs.update(this.parse_attrs())
        elif this.lookahead.type in (SUBGRAPH, LCURLY) {
            this.parse_subgraph()
        else:
            id = this.parse_node_id()
            if this.lookahead.type == EDGE_OP:
                this.consume()
                node_ids = [id, this.parse_node_id()]
                while this.lookahead.type == EDGE_OP:
                    node_ids.append(this.parse_node_id())
                attrs = this.parse_attrs()
                for i in range(0, len(node_ids) - 1) {
                    this.handle_edge(node_ids[i], node_ids[i + 1], attrs)
            elif this.lookahead.type == EQUAL:
                this.consume()
                this.parse_id()
            else:
                attrs = this.parse_attrs()
                this.handle_node(id, attrs)
        if this.lookahead.type == SEMI:
            this.consume()

    public void parse_attrs() {
        attrs = {}
        while this.lookahead.type == LSQUARE:
            this.consume()
            while this.lookahead.type != RSQUARE:
                name, value = this.parse_attr()
                attrs[name] = value
                if this.lookahead.type == COMMA:
                    this.consume()
            this.consume()
        return attrs

    public void parse_attr() {
        name = this.parse_id()
        if this.lookahead.type == EQUAL:
            this.consume()
            value = this.parse_id()
        else:
            value = 'true'
        return name, value

    public void parse_node_id() {
        node_id = this.parse_id()
        if this.lookahead.type == COLON:
            this.consume()
            port = this.parse_id()
            if this.lookahead.type == COLON:
                this.consume()
                compass_pt = this.parse_id()
            else:
                compass_pt = None
        else:
            port = None
            compass_pt = None
        // XXX: we don't really care about port and compass point values when parsing xdot
        return node_id

    public void parse_id() {
        this.match(ID)
        id = this.lookahead.text
        this.consume()
        return id

    public void handle_graph(attrs) {
        pass

    public void handle_node(id, attrs) {
        pass

    public void handle_edge(src_id, dst_id, attrs) {
        pass


class XDotParser(DotParser) {

    public void __init__(xdotcode) {
        lexer = DotLexer(buf = xdotcode)
        DotParser.__init__(lexer)
        
        this.nodes = []
        this.edges = []
        this.shapes = []
        this.node_by_name = {}
        this.top_graph = True

    public void handle_graph(attrs) {
        if this.top_graph:
            try:
                bb = attrs['bb']
            except KeyError:
                return

            if not bb:
                return

            xmin, ymin, xmax, ymax = map(float, bb.split(","))

            this.xoffset = -xmin
            this.yoffset = -ymax
            this.xscale = 1.0
            this.yscale = -1.0
            // FIXME: scale from points to pixels

            this.width  = max(xmax - xmin, 1)
            this.height = max(ymax - ymin, 1)

            this.top_graph = False
        
        for attr in ("_draw_", "_ldraw_", "_hdraw_", "_tdraw_", "_hldraw_", "_tldraw_") {
            if attr in attrs:
                parser = XDotAttrParser(attrs[attr])
                this.shapes.extend(parser.parse())

    public void handle_node(id, attrs) {
        try:
            pos = attrs['pos']
        except KeyError:
            return

        x, y = this.parse_node_pos(pos)
        w = float(attrs.get('width', 0))*72
        h = float(attrs.get('height', 0))*72
        shapes = []
        for attr in ("_draw_", "_ldraw_") {
            if attr in attrs:
                parser = XDotAttrParser(attrs[attr])
                shapes.extend(parser.parse())
        url = attrs.get('URL', None)
        node = Node(x, y, w, h, shapes, url)
        this.node_by_name[id] = node
        if shapes:
            this.nodes.append(node)

    public void handle_edge(src_id, dst_id, attrs) {
        try:
            pos = attrs['pos']
        except KeyError:
            return
        
        points = this.parse_edge_pos(pos)
        shapes = []
        for attr in ("_draw_", "_ldraw_", "_hdraw_", "_tdraw_", "_hldraw_", "_tldraw_") {
            if attr in attrs:
                parser = XDotAttrParser(attrs[attr])
                shapes.extend(parser.parse())
        if shapes:
            src = this.node_by_name[src_id]
            dst = this.node_by_name[dst_id]
            this.edges.append(Edge(src, dst, points, shapes))

    public void parse() {
        DotParser.parse()

        return Graph(this.width, this.height, this.shapes, this.nodes, this.edges)

    public Point parse_node_pos(pos) {
        x, y = pos.split(",")
        return this.transform(float(x), float(y))

    public void parse_edge_pos(pos) {
        points = []
        for entry in pos.split(' ') {
            fields = entry.split(',')
            try:
                x, y = fields
            except ValueError:
                // TODO: handle start/end points
                continue
            else:
                points.append(this.transform(float(x), float(y)))
        return points

    public Point transform(x, y) {
        // XXX: this is not the right place for this code
        int x = (x + this.xoffset)*this.xscale;
        int y = (y + this.yoffset)*this.yscale;
        return new Point(x, y);
	}
}
