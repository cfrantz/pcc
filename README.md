pcc
===

PCC is a simple C compiler experiment.

The C parser is a Python variant of a Parsing Expression Grammar (PEG),
created with the ultra-cool MacroPy package (https://github.com/lihaoyi/macropy).

The original C99 PEG is from http://www.romanredz.se/papers/C.peg.

Currently, the parser translates a C source file into a parse tree
that can be dumped to an XML file.  The parse tree structures are defined by an
XML schema and translated to/from python objects by the bubbles SOAP/XML
library (https://github.com/cfrantz/bubbles).

Example
=======

Input:
------
    int main(void)
    {
        int i=7;
        printf("Hello %d\n", i);
    }

Output:
-------

    <c:nodes xmlns:c="urn:c-ast" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <c:node>
        <c:function>
          <c:signature>
            <c:name>main</c:name>
            <c:type>function</c:type>
            <c:signature>
              <c:sig>
                <c:type>void</c:type>
              </c:sig>
            </c:signature>
          </c:signature>
          <c:rtype>
            <c:type>int</c:type>
          </c:rtype>
          <c:body>
            <c:stmt xsi:type="ns0:Declarator">
              <c:name>i</c:name>
              <c:type>int</c:type>
              <c:initializer xsi:type="ns0:Integer">
                <c:value>7</c:value>
              </c:initializer>
            </c:stmt>
            <c:stmt xsi:type="ns0:Call">
              <c:function xsi:type="ns0:Identifier">
                <c:name>printf</c:name>
              </c:function>
              <c:arguments xsi:type="ns0:String">
                <c:value>Hello %d\n</c:value>
              </c:arguments>
              <c:arguments xsi:type="ns0:Identifier">
                <c:name>i</c:name>
              </c:arguments>
            </c:stmt>
          </c:body>
        </c:function>
      </c:node>
    </c:nodes>


TODO
====

* Test/fix the parser by testing lots of C code
* Error conditions
* Consume parse tree and translate to another form (e.g. assembly)

vim: ts=4 sts=4 sw=4 expandtab:
