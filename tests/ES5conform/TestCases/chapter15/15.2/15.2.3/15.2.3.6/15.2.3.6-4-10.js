/// Copyright (c) 2009 Microsoft Corporation 
/// 
/// Redistribution and use in source and binary forms, with or without modification, are permitted provided
/// that the following conditions are met: 
///    * Redistributions of source code must retain the above copyright notice, this list of conditions and
///      the following disclaimer. 
///    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and 
///      the following disclaimer in the documentation and/or other materials provided with the distribution.  
///    * Neither the name of Microsoft nor the names of its contributors may be used to
///      endorse or promote products derived from this software without specific prior written permission.
/// 
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
/// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
/// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
/// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
/// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
/// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
/// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
/// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

/*
Step 4 of defineProperty calls the [[DefineOwnProperty]] internal method
of O to define the property. Step 7b of [[DefineOwnProperty]] rejects if
current.[[Enumerable]] and desc.[[Enumerable]] are the boolean negations
of each other.
*/

ES5Harness.registerTest( {
id: "15.2.3.6-4-10",

path: "TestCases/chapter15/15.2/15.2.3/15.2.3.6/15.2.3.6-4-10.js",

description: "Object.defineProperty throws TypeError when changing [[Enumerable]] from false to true on non-configurable accessor properties",

test: function testcase() {
  var o = {};

  // create an accessor property; all other attributes default to false.
  // dummy getter
  var getter = function () { return 1; };
  var d1 = { get: getter, enumerable: false, configurable: false };
  Object.defineProperty(o, "foo", d1);
  // now, setting enumerable to true should fail, since [[Configurable]]
  // on the original property will be false.
  var desc = { get: getter, enumerable: true };

  try {
    Object.defineProperty(o, "foo", desc);
      print(Object.getOwnPropertyDescriptor(o,"foo")["enumerable"]);
  }
  catch (e) {
    if (e instanceof TypeError) {
      // the property should remain unchanged.
      var d2 = Object.getOwnPropertyDescriptor(o, "foo");
      if (d2.get === getter &&
          d2.enumerable === false &&
          d2.configurable === false) {
        return true;
      }
    }
  }
 },

precondition: function prereq() {
  return fnExists(Object.defineProperty) && fnExists(Object.getOwnPropertyDescriptor);
 }
});
