import sys, sqlite, _sqlite, re
from functions import *

class Dataset:
    """
    This is the object that a db.q (query) will return. It can format cols, or just
    iterate over the columns.
    """
    def __init__(self, table, data, cols, formats = {}):
        self.tableName = table
        self.data = data
        self.cols = cols
        self.formats = formats
    def __len__(self):
        return len(self.data)
    def __getitem__(self, index):
        if index >= 0 and index < len(self.data):
            return self.data[index]
        raise StopIteration
    def __iter__(self):
        for c in self.data:
            yield c
    def applyFormat(self, row, colName):
        """
        Apply the format from the db to a row/col
        """
        if (self.tableName, colName) in self.formats:
            table, format, test = self.formats[(self.tableName, colName)]
            if self.tableName != table:
                return row[colName]
            if test == None or eval(test % row):
                retval = format % row
            else:
                retval = row[colName]
            if type(retval) == str and retval.startswith("eval:"):
                retval = eval(retval[5:])
            return retval
        else:
            return row[colName]
    def replace(self, row):
        """ Replace data in replaceable things """
        retval = {}
        for key in row:
            if (self.tableName, key) in self.formats:
                try:
                    retval[key] = self.applyFormat(row, key)
                except KeyError, e:
                    retval[key] = row[key]
            else:
                retval[key] = row[key]
        return retval
    def makeTable(self, headers=None, sort = None, altHeaders = {}, border = 0,
                  width=None, showHeaders=1, groupBy = None, numbering = 1, toc = 1):
        """ Takes formatting for the table and returns html table. """
        retval = ""
        if headers == None:
            headers = [x[0] for x in self.cols]
        if len(self.data) < 50:
            toc = 0
        if numbering == 0:
            toc = 0
        if toc:
            print begin("P")
            for letter in "ABCDEFGHIJKLMNOPQRSTUVWXYZ":
                print link(letter, "#" + letter),
            print end("P")
        if showHeaders:
            for h in headers:
                if h in altHeaders:
                    h = altHeaders[h]
                retval += format("TH align=left", h.title())
            retval = format("TR", retval)
        if sort:
            self.data.sort(makeComparison(*sort))
        lastGroup = None
        self.currLetter = "A"
        self.rowNum = 1
        for row in self.data:
            rowdata = ""
            # replace items with formatting options
            # new row of data, replaced with formatting
            orig = row.copy()
            row = self.replace(row)
            if groupBy != None and lastGroup != row[groupBy]:
                rowdata += format("TR", format("TH align=left colspan=%d" % len(headers),
                                               row[groupBy]))
                lastGroup = row[groupBy]
            self.first = 1
            for h in headers:
                if h in row:
                    if type(row[h]) == type("") and len(row[h]) > 1 and row[h][-1] == ":":
                        rowdata += format("TD", self.num(row[h], orig[h]), align="right")
                    else:
                        if not isempty(row[h]):
                            rowdata += format("TD", self.num(row[h], orig[h]))
                        else:
                            rowdata += format("TD", self.num("&nbsp;", ""))
                else:
                    rowdata += format("TD")
            retval += format("TR", rowdata)
        # add the rest of the alphabet <a name> tags:
        retval += begin("TR") + begin("TD")
        for c in range(ord(self.currLetter), ord('Z') + 1):
            retval += '<A name="%s"></A>' % chr(c)
        retval += end("TD") + end("TR")
        # wrap the table up:
        if width:
            return format("TABLE", retval, border = border, width = width)
        else:
            return format("TABLE", retval, border = border)
    def num(self, data, orig):
        """ Does the numbering and toc alpha bookmarks """
        retval = ""
        if self.first:
            if type(orig) == str and len(orig) > 0 and orig[0].isalpha():
                if orig[0] >= self.currLetter:
                    for c in range(ord(self.currLetter), ord(orig[0]) + 1):
                        retval += '<A name="%s"></A>' % chr(c)
                    self.currLetter = chr(ord(orig[0]) + 1)
            retval += "%d) %s" % (self.rowNum, data)
            self.rowNum += 1
            self.first = 0
        else:
            retval += str(data)
        return retval
    def makeList(self, headers = None, sort = None):
        """ Takes a list of cols, and list of dicts
        (optional: list of fields to sort by)
        """
        if sort:
            self.data.sort(makeComparison(*sort))
        if headers == None:
            headers = [col[0] for col in self.cols]
        retval = begin("BLOCKQUOTE", class_id="%s-list" % self.tableName)
        for row in self.data:
            row = self.replace(row)
            for h in headers:
                if h in row:
                    if row[h] != None and row[h] != "":
                        retval += format("LI", bold(h.title() + ": ") + str(row[h]))
        retval += end("BLOCKQUOTE")
        return retval

class Database:
    """
    The db connection.
    """
    def __init__(self, database):
        self.database = database
        self.db = sqlite.connect(self.database)
        self.cursor = self.db.cursor()
        self.formats = {}
    def addFormat(self, tableName, field, format, test = None):
        self.formats[(tableName, field)] = (tableName, format, test)
    def q(self, query, *args):
        """ Given a query, returns a Dataset """
        data = self.query(query, *args)
        retval = []
        for row in data:
            dictionary = {}
            colNum = 0
            for col in self.col_names:
                dictionary[col] = row[colNum]
                colNum += 1
            retval.append(dictionary)
        return Dataset(self.table, retval, self.cols, self.formats)
    def query(self, q, *args):
        """ Given a query, returns all rows """
        self.table = self.getTable(q)
        if q == None:
            return []
        elif "drop" in q:
            try:
                self.cursor.execute(q, *args)
                self.db.commit()
                return None
            except _sqlite.DatabaseError, e:
                print >> sys.stderr, "warning: %s" % e
                return None
        self.cursor.execute(q, *args)
        self.db.commit()
        self.cols = self.cursor.rs.col_defs
        self.col_names = [col[0] for col in self.cols]
        return self.cursor.fetchall()
    def close(self):
        """ Closes and writes out tables """
        self.cursor.close()
        self.db.close()
    def getTable(self, query):
        """
        Get table name from the SQL query
        Based on http://schulermanager.at/pear/DB_cache_pgsql.phps
        """
        # -- check for generall SQL operation type
        if query == None:
            query = ""
        query = query.upper()
        if (re.match('^\s*?(SELECT|DELETE)\s', query) != None):
            #-- scan for operation table in query
            result = re.match(".*FROM\s*([A-Za-z_0-9]+)\s*", query)
            groups = result.groups()
            #-- check for scan success
            if (len(groups) == 0): return None
            #-- return table name
            return groups[0].lower()
        elif (re.match('^\s*?INSERT\s', query) != None):
            #-- scan for operation table in query
            result = re.match("^\s*INSERT INTO\s*([A-Za-z_0-9]+)\s*", query)
            groups = result.groups()
            #-- check for scan success
            if len(groups) == 0: return None
            #-- return table name
            return groups[0].lower()
        elif (re.match('^\s*?UPDATE\s', query) != None):
            #-- scan for operation table in query
            result = re.match("^\s*UPDATE\s*([A-Za-z_0-9]+)\s*", query)
            groups = result.groups()
            #-- check for scan success
            if len(groups) == 0: return None
            #-- return table name
            return groups[0].lower()
        else: #-- no table operation, like BEGIN; COMMIT;
            #-- Such query should not be cached and always executed
            #-- so NULL will be returned
            return None
