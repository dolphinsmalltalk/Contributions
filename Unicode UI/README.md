# Unicode UI

This package adds experimental Unicode support to the Dolphin Smalltalk UI.

This may not be a complete or optimally efficient implementation.  This started as an experimental prototype to explore the concept of adding Unicode UI support to Dolphin Smalltalk.

## Example Shell:
![Example Shell](http://www.mitchellscientific.com/smalltalk/osds/unicode-ui/exp2/Exp2ScrnCap.png)

## How it Works
This is done in a two stages:
1. The package loads code as normal.
2. After a prompt, automated transforms are applied to the code base.  This approach was taken to facilitate the ease of applying Unicode UI support to a independently evolving Dolphin core.  The results of these transforms are not captured into new packages. See the package scripts for more information.

## Areas in Need of Improvement:
1. Expand coverage of Unicode support as needed.  Presently most of the UI, and ODBC Databases have decent Unicode support.
2. Some of the transforms are simple text replacements.  It may be desirable to have more sophisticated parse tree aware transforms.
3. I haven't used this, "for real", if someone does then further areas in need of improvement will come up.

Initially developed by Mitchell Scientific, Inc. in 2016 .

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of th??Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
