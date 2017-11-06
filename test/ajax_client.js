#!/usr/bin/env node
'use strict';
var async = require('async');
var Thrift = require("thrift");
var TestService = require("./gen-nodejs/TestService.js");
var TestTypes = require("./gen-nodejs/test_types.js");
const protocols = ["TBinaryProtocol", "TCompactProtocol", "TJSONProtocol"];
var num_replies = 0;
async.each(
    protocols,
    function(uri, callback) {
        console.error("testing " + uri);
        var protocol = eval("Thrift." + uri);
        var connection = Thrift.createHttpConnection("127.0.0.1", 9099, {
            path : "/" + uri,
            protocol : eval("Thrift." + uri)
        });
        var client = Thrift.createClient(TestService, connection);
        return client.test_v2(new TestTypes.Vec2({x: 2.0, y: 3.0}))
            .then(function(result) {
                console.error(uri, "Got answer:", result);
                if(Math.abs(result - 5) > 0.01) {
                    console.error(uri, "Wrong answer");
                    process.exit(1);
                }
                ++num_replies;
                callback();
            })
            .catch(function(err) {
                console.error("Thrift call failed", err);
                process.exit(1);
            });
    },
    function(err) {
        console.error("Got", num_replies, "/", protocols.length, "replies");
        if(num_replies /= protocols.length) {
            console.error("Got", num_replies, "instead of", protocols.length);
            process.exit(1);
        }
    });
