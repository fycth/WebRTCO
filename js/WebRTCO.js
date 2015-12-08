
// Description: Free WebRTC Framework
// Author: Andrii Sergiienko
// Created: Sun Apr  5 15:15:20 2015 (+0300)
// Version: 1.12

/*
 options.localVideoID = string ID of the video tag for showing local video
 options.remoteVideoID = string ID of the video tag for showing remote video 
 options.getRemoteVideoID = call back function which is called when we need to get remote video tag ID from the client
 options.roomID = string word that identifies room parameter in the GET request. If not set, equal to 'room'
 options.roomName = string that that identifies room name. If not set, room name will be requested from the signaling server
 options.pc_config
 options.constraints
 options.sdp_constraints
 options.sandbox = if set, don't make calls, just get user media and stop
 options.own_signaler = URL to the WebRTCO signaler installed on the customer's side
 options.call_back = it is a function that will be called when we need to notify client on an event (error happened, created room etc.)
 options.no_data_channel - if set, don't create data channel (created by default)
 options.hq_audio - if set, enable high-quality audio (stereo, 44100, 256kb/s)
 */

var WebRTCO = function(options) {
    var API = {};
    var signaling_url = typeof options.own_signaler !== 'undefined' ? options.own_signaler : "wss://www.oslikas.com/WebRTCO-app/signaling/1.12/";
    if (typeof options === 'undefined') {
	      console.error("You should pass options. Please, refer to the documentation.");
	      return;
    }
    // we store in here set of PeerConnection objects (one per peer)
    var Peers = [];
    // we use basic default contraints when custom constraints are not provided
    var default_constraints = {
        audio: {
            mandatory: {
            },
            optional: []
        },
	      video: {
	          mandatory: {
                minWidth: 640,
                minHeight: 480,
                maxWidth: 1080,
                maxHeight: 720
            },
	          optional: []
	      }
    };
    var default_pc_config = {
	      iceServers: [
	          {url: 'stun:stun1.l.google.com:19302'},
	          {url: 'stun:srun2.l.google.com:19302'},
	          {url: 'stun:stun.l.google.com:19302'}
	      ]
    };
    var default_sdp_constraints = {
        mandatory: {
            OfferToReceiveAudio: true,
            OfferToReceiveVideo: true
        },
        optional: []
    };

    /* screen casting constraints */
    var screen_casting_constraints = {
        firefox: {
            audio: false,
            video: {
                mozMediaSource: 'screen',
                mediaSource: 'screen'
            }
        },
        chrome: {
            audio: false,
            video: {
            }
        }
    };
    /* end of screencasting constraints */

    /* Look at the options and do initialization */
    var pc_config = typeof options.pc_config !== 'undefined' ? options.pc_config : default_pc_config;
    var constraints = typeof options.constraints !== 'undefined' ? options.constraints : default_constraints;
    var sdpConstraints = typeof options.sdp_constraints !== 'undefined' ? options.sdp_constraints : default_sdp_constraints;
    var localStream = null;
    var localScreenStream = null;
    var roomID = typeof options.roomID !== 'undefined' ? options.roomID : 'room';
    var sandbox = typeof options.sandbox !== 'undefined' ? true : false;
    var call_back = typeof options.call_back !== 'undefined' ? options.call_back : undefined;

    function getRemoteVideo() {
        if (typeof options.remoteVideoID !== 'undefined') return options.remoteVideoID;
        if (typeof options.getRemoteVideoID !== 'undefined') return options.getRemoteVideoID();
        pushCallBack(ErrorCodes.REMOTE_ID);
        return null;
    };

    /*
     * Error codes and call back
     */
    var ErrorCodes = {
        // New room created, got the room URL
        ENTERED_ROOM: 100,
        // Created new peer connection
        NEW_PEER_CONNECTION: 101,
        // Unknown error
        UNKNOWN: 10001,
        // Can't create peer connection
        PEER_CONNECTION: 1002,
        // Can't access user media
        MEDIA: 1003,
        // Can't reach signaling server
        SIGNALING: 1004,
        // Room is full
        ROOM_IS_FULL: 1005,
        // Remote Video ID is not defined
        REMOTE_ID: 1006,
        // Can't create data channel
        DATA_CHANNEL: 1007,
        // public message from peer
        PUBLIC_MESSAGE: 1008
    };
    API.ErrorCodes = ErrorCodes;

    var BERT = new WebRTCO_BERT();

    function pushCallback(code, message) {
        if (undefined === call_back) return;
        var msg = {code: code};
        if (typeof message !== undefined) msg.message = message;
        call_back(msg);
    };
    /* end of ErrorCodes */

    /*
     * Utilities
     */
    var Utils = (function() {
	      var API = {};
	      API.getRoomName = function() {
	          var s = window.location.search;
	          var o = {};
	          s.replace(
		            new RegExp ("([^?=&]+)(=([^&]*))?", "g"),
		            function ($0, $1, $2, $3) { o[$1] = $3; }
	          );
	          if (o[roomID]) return o[roomID];
            return undefined;
	      };
        API.makeRandID = function(limit) {
            var rand_text = "";
            var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
            for(var i = 0; i < limit; i++)
                rand_text += possible.charAt(Math.floor(Math.random() * possible.length));
	          return rand_text;
        };
	      return API;
    }());
    /* end of Utils object */

    /*
     * Peer Connection object
     *
     *
     */
    var PeerConnection = function(remoteVideoID) {
        var API = {};
        var remoteVideo = document.getElementById(remoteVideoID);
	      var pc_constraints = {};
        var peer;
        var pc;
        API.setPeer = function(p) {
            peer = p;
        };
	      function onIceCandidate(e) {
	          if (e.candidate) send2peer({type: 'candidate', label: e.candidate.sdpMLineIndex,
					                              id: e.candidate.sdpMid, candidate: e.candidate.candidate});
            //	        else console.debug("End of candidates");
	      };

        function send2peer(message) {
            if (typeof options.signaling !== 'undefined') {
                Signaling.Send(message);
            } else {
                Signaling.Send({type: 'TO', value: peer, message: message});
            }
        };

	      var onRemoteStreamAdded = function(e) {
	          //TODO: calling of attachstream should be in media object (a callback?)
	          attachMediaStream(remoteVideo, e.stream);
            //	        console.debug("Remote stream added");
	      };

	      function onRemoteStreamRemoved(e) {
            //	        console.debug("Remote stream removed");
	      };

	      API.doCall = function() {
            //	        console.debug("Sending offer to the peer. Constraints: " + JSON.stringify(pc_constraints));
            if (typeof options.no_data_channel === 'undefined') createDataChannel();
	          pc.createOffer(setLocalAndSendMessage, onError, pc_constraints);
	      };

	      function doAnswer() {
            //	        console.debug("Sending answer to the peer");
	          pc.createAnswer(setLocalAndSendMessage, onError, sdpConstraints); //TODO: sdpConstraints should be passed here?
	      };

	      function onError(e) {
            pushCallback(ErrorCodes.PEER_CONNECTION, e.toString());
            //	        console.debug("Something happened when sending an answer or an offer: " + e.toString());
	      };

	      function setLocalAndSendMessage(sdp) {
            if (typeof options.hq_audio !== 'undefined') {
                sdp.sdp = setSDPBand(sdp.sdp);
                sdp.sdp = setSDPStereo(sdp.sdp);
            }
            pc.setLocalDescription(sdp);
            send2peer(sdp);
	      };

        function setSDPBand(sdp) {
            //    sdp = sdp.replace( /b=AS([^\r\n]+\r\n)/g , '');
            sdp = sdp.replace( /a=mid:audio\r\n/g , 'a=mid:audio\r\nb=AS:320\r\n');
            //    sdp = sdp.replace( /a=mid:video\r\n/g , 'a=mid:video\r\nb=AS:4096\r\n');
            //    sdp = sdp.replace( /a=mid:data\r\n/g , 'a=mid:data\r\nb=AS:163840\r\n');
            return sdp;
        }
        function setSDPStereo(sdp) {
            var sdpLines = sdp.split('\r\n');
            var fmtpLineIndex = null;
            for (var i = 0; i < sdpLines.length; i++) {
                if (sdpLines[i].search('opus/48000') !== -1) {
                    var opusPayload = extractSdp(sdpLines[i], /:(\d+) opus\/48000/i);
                    break;
                }
            }
            for (var i = 0; i < sdpLines.length; i++) {
                if (sdpLines[i].search('a=fmtp') !== -1) {
                    var payload = extractSdp(sdpLines[i], /a=fmtp:(\d+)/ );
                    if (payload === opusPayload) {
                        fmtpLineIndex = i;
                        break;
                    }
                }
            }
            if (fmtpLineIndex === null) return sdp;
            //stereo - ability to receive stereo
            //sprop-stereo - ability to produce stereo
            //usedtx - don't send audio signal if nothing comes from the source
            sdpLines[fmtpLineIndex] = sdpLines[fmtpLineIndex].concat('; stereo=1; sprop-stereo=1; usedtx=0; x-google-min-bitrate=256; x-google-max-bitrate=320; maxaveragebitrate=' + (320 * 1024) + ';');
            //maxaveragebitrate=' + (128 * 1024) + '; maxplaybackrate=48000');
            sdp = sdpLines.join('\r\n');
            return sdp;
        }

	      API.onOffer = function(m) {
	          pc.setRemoteDescription(new RTCSessionDescription(m));
	          doAnswer();
	      };

	      API.onAnswer = function(m) {
	          pc.setRemoteDescription(new RTCSessionDescription(m));
	      };

	      API.onCandidate = function(m) {
	          var candidate = new RTCIceCandidate({sdpMLineIndex: m.label, candidate: m.candidate});
	          pc.addIceCandidate(candidate);
	      };

        API.addStream = function(stream) {
            pc.addStream(stream);
        };

        // Data Channel
        var data_channel = null;
        function onDataChannel(event) {
            console.info("Received data channel creating request");
            data_channel = event.channel;
            initDataChannel();
        };
        function createDataChannel() {
            try {
                data_channel = pc.createDataChannel("datachannel_" + peer);
            } catch (e) {
                pushCallback(ErrorCodes.DATA_CHANNEL,e);
                data_channel = null;
                //                console.error("Error creating data channel " + e);
                return;
            }
            initDataChannel();
        };
        function initDataChannel() {
            data_channel.onopen = onDataChannelOpen;
            data_channel.onclose = onDataChannelClose;
            data_channel.onmessage = onReceiveMessageCallback;
        };
        function send2dataChannel(message) {
            if (null !== data_channel) data_channel.send(message);
        };
        function onDataChannelOpen(event) {
            //            console.info("Data channel opened");
            //            send2dataChannel("hello to " + peer + " !");
        };
        function onDataChannelClose(event) {
            data_channel = null;
            //            console.info("Data channel closed");
        };
        function onReceiveMessageCallback(event) {
            //            console.info("data message: " + event.data);
        };
        // end of Data Channel

	      try {
	          pc = new RTCPeerConnection(pc_config, {});
	          pc.onicecandidate = onIceCandidate;
            pc.onaddstream = onRemoteStreamAdded;
            pc.onremovestream = onRemoteStreamRemoved;
            pc.ondatachannel = onDataChannel;
            //	        console.debug("Peer connection created");
	      } catch (e) {
	          pc = null;
            pushCallback(ErrorCodes.PEER_CONNECTION, e.message);
            //	        console.debug("Failed to create peer connection: " + e.message);
	      }
        return API;
    };
    /* end of peer connection object */

    API.Adapter = new WebRTCOAdapter();
    var browserName = API.Adapter.name;

    /*
     * WebRTC media object
     *
     * localVideoID - (mandatory) - ID of the video tag
     * _constraints - (optional) - media access constraints
     *
     */
    var WebRTCOMedia = function(localVideoID, _constraints, _stream_callback) {
        var API = {};
	      var localVideo = document.getElementById(localVideoID);
        if (typeof options.hq_audio !== 'undefined' && browserName === 'Chrome') {
            _constraints.audio.mandatory.echoCancellation = false;
            _constraints.audio.mandatory.googEchoCancellation = false;
        }
	      function doGetUserMedia() {
	          try {
                //		        console.debug("Requesting access to media...");
		            getUserMedia(_constraints, onSuccess, onError);
	          } catch (e) {
                pushCallback(ErrorCodes.MEDIA, e.message);
                //		        console.error("Media access failed with error: " + e.message);
		            return;
	          }
	      };
	      function onSuccess(stream) {
            //	        console.debug("Media access granted");
            localVideo.muted = true;
	          _stream_callback(stream);
	          attachMediaStream(localVideo, stream);
	      };
	      function onError(e) {
            pushCallback(ErrorCodes.MEDIA, e.toString());
            //	        console.error("Failed to get media access: " + e);
	      };
	      doGetUserMedia();
        return API;
    };
    /* end of media object */

    var Media = new WebRTCOMedia(options.localVideoID, constraints, function(stream) { localStream = stream; });

    /*
     * Signaling object
     *
     *
     */
    var Signaling;
    if (!sandbox) {
        if (typeof options.signaling !== 'undefined') {
            Signaling = options.signaling;
        } else {
            Signaling = (function() {
	              var API = {};
	              var ready = false;
                var channel = null;

	              function onOpened() {
                    //	        console.debug("Signaling channel opened");
	                  ready = true;
	                  var room = Utils.getRoomName();
	                  if (room !== undefined) {
		                    initiator = true;
	                  } else {
                        room = Utils.makeRandID(5);
                        //                console.debug("Room name: " + room);
		                    initiator = false;
	                  }
                    API.Send({type: 'ROOM_ENTER', value: room + ''});
	              };

	              function onMessage(m) {
                    //	        console.debug('S->C: ' + m.data);
	                  processMessage(m.data);
	              };

	              function onError(e) {
                    pushCallback(ErrorCodes.SIGNALING, e.message);
                    //	        console.debug("Error on signaling channel");
	              };

	              function onClosed() {
                    //	        console.debug("Signaling channel has been closed");
	                  ready = false;
	              };

	              function processMessage(m) {
                    var r = new FileReader();
                    r.addEventListener("loadend", function() {
                        var msg = BERT.decode(r.result);
                        // look at the message type
                        switch (BERT.ab2str(msg.v[0].v)) {
                        case "ROOM_IS_FULL":
                            pushCallback(ErrorCodes.ROOM_IS_FULL);
                            break;
                        case "ENTERED_ROOM":
                            var room_url = window.location + "?" + roomID + "=" + BERT.ab2str(msg.v[1].v);
                            pushCallback(ErrorCodes.ENTERED_ROOM, room_url);
                            break;
                        case "NEW_PEER":
                            var FromPid = BERT.ab2str(msg.v[1].v);
                            Peers[FromPid] = new PeerConnection(getRemoteVideo());
                            Peers[FromPid].addStream(localStream);
                            Peers[FromPid].setPeer(FromPid);
                            Peers[FromPid].doCall();
                            break;
                        case "NEW_PUBLIC":
                            pushCallback(ErrorCodes.PUBLIC_MESSAGE, {from: BERT.ab2str(msg.v[1].v), msg: BERT.ab2str(msg.v[2].v)});
                        case "FROM":
                            var peer = BERT.ab2str(msg.v[1].v);
                            msg = JSON.parse(BERT.ab2str(msg.v[2].v));
                            if (msg.type == 'offer') {
                                if (Peers.indexOf(peer) <= -1) {
                                    Peers[peer] = new PeerConnection(getRemoteVideo());
                                    Peers[peer].addStream(localStream);
                                    Peers[peer].setPeer(peer);
                                    Peers[peer].onOffer(msg);
                                }
                            } else if (msg.type == 'answer') {
                                Peers[peer].onAnswer(msg);
                            } else if (msg.type == 'candidate') {
                                Peers[peer].onCandidate(msg);
                            };
                            break;
                        default:
                            break;
                        };
                    });
                    r.readAsArrayBuffer(m);
	              };

	              API.Send = function(m) {
	                  if (!ready) return;
	                  var s = typeof m.message !== 'undefined' ? JSON.stringify(m.message) : "";
                    var b = BERT.encode(BERT.tuple(BERT.bin(m.type),BERT.tuple(BERT.bin(m.value),BERT.bin(s))));
                    //	        console.debug('C->S: ' + s);
	                  channel.send(b);
	              };

                function init() {
                    if (null !== localStream) {
	                      channel = new WebSocket(signaling_url);
	                      channel.onopen = onOpened;
	                      channel.onerror = onError;
	                      channel.onmessage = onMessage;
	                      channel.onclose = onClosed;
                    } else setTimeout(init, 1000);
                };
                init();
	              return API;
            }());
        }
    }
    /* end of signaling object */

    // send broadcasting message to all peers via server
    API.send2Public = function(msg) {
        Signaling.Send({type: "TO_PUBLIC", value: msg});
    };

    // Return local stream
    API.getLocalStream = function() {
        return localStream;
    };

    // mute/unmute local audio tracks
    API.muteAudio = function(mute) {
        var audioTracks = localStream.getAudioTracks();
        for (i = 0; i < audioTracks.length; i++) audioTracks[i].enabled = mute;
    };

    // pause/resume local video
    API.pauseVideo = function(pause) {
        var videoTracks = localStream.getVideoTracks();
        for (i = 0; i < videoTracks.length; i++) videoTracks[i].enabled = pause;
    };

    function getScreenCastingConstraints() {
        if (API.Adapter.name === 'Firefox') return screen_casting_constraints.firefox;
        else if (API.Adapter.name === 'Chrome') return screen_casting_constraints.chrome;
        else return null;
    };

    API.doScreenCasting = function(screenCastingVideoID) {
        var constraints = getScreenCastingConstraints();
        API.Media = new WebRTCOMedia(screenCastingVideoID, constraints, function(stream) { localScreenStream = stream; });
    };

    /* for 3rd-party signaling */
    API.getPeerConnection = function(remoteVideoID) {
        var pc = new PeerConnection(remoteVideoID);
        pc.addStream(localStream);
        return pc;
    };
    /* end of */

    return API;
};
/* end of WebRTCO object */

/*
 * WebRTC Adapter
 *
 */
var WebRTCOAdapter = function() {
    var API = {};
    if (navigator.mozGetUserMedia) {
        window.RTCPeerConnection = mozRTCPeerConnection;
        window.RTCSessionDescription = mozRTCSessionDescription;
        window.RTCIceCandidate = mozRTCIceCandidate;
        window.getUserMedia = navigator.mozGetUserMedia.bind(navigator);
        window.attachMediaStream = function(element, stream) {
            element.mozSrcObject = stream;
            element.play();
        };
        window.reattachMediaStream = function(to, from) {
            to.mozSrcObject = from.mozSrcObject;
            to.play();
        };
    } else if (navigator.webkitGetUserMedia) {
        window.RTCPeerConnection = webkitRTCPeerConnection;
        window.getUserMedia = navigator.webkitGetUserMedia.bind(navigator);
        window.attachMediaStream = function(element, stream) {
            element.src = URL.createObjectURL(stream);
        };
        window.reattachMediaStream = function(to, from) {
            to.src = from.src;
        };
        if (!webkitMediaStream.prototype.getVideoTracks) {
            webkitMediaStream.prototype.getVideoTracks = function() {
                return this.videoTracks;
            };
            webkitMediaStream.prototype.getAudioTracks = function() {
                return this.audioTracks;
            };
        }
        if (!webkitRTCPeerConnection.prototype.getLocalStreams) {
            webkitRTCPeerConnection.prototype.getLocalStreams = function() {
                return this.localStreams;
            };
            webkitRTCPeerConnection.prototype.getRemoteStreams = function() {
                return this.remoteStreams;
            };
        }
    }

    var ua=navigator.userAgent,tem,M=ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\/))\/?\s*(\d+)/i) || [];
    if(/trident/i.test(M[1])) {
        tem = /\brv[ :]+(\d+)/g.exec(ua) || [];
        API.name = 'IE ';
        API.version = (tem[1]||'');
    } else if(M[1] === 'Chrome') {
        tem = ua.match(/\bOPR\/(\d+)/);
        if (tem != null) {
            API.name = 'Opera';
            API.version = tem[1]
        }
    }
    if (typeof API.name === 'undefined') {
        M = M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
        if ((tem = ua.match(/version\/(\d+)/i)) != null) {
            M.splice(1,1,tem[1]);
        }
        API.name = M[0];
        API.version = M[1];
    }
    return API;
};
/* end of Adapter object */

/*
 * WebRTC BERT Encoder/Decoder
 * The code is taken from https://github.com/synrc/n2o
 * Credentials: Maxim Sokhatsky
 */
var WebRTCO_BERT = function() {
    var API = {};

    function utf8_toByteArray(str) {
        var byteArray = [];
        if (str !== undefined && str !== null)
            for (var i = 0; i < str.length; i++)
                if (str.charCodeAt(i) <= 0x7F) byteArray.push(str.charCodeAt(i));
        else {
            var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
            for (var j = 0; j < h.length; j++) byteArray.push(parseInt(h[j], 16)); }
        return {t:107,v:byteArray}; };

    function utf8_dec(ab) {
        if (!(ab instanceof ArrayBuffer)) ab = new Uint8Array(utf8_toByteArray(ab).v).buffer;
        var t=new DataView(ab),i=c=c1=c2=0,itoa=String.fromCharCode,s=[]; while (i<t.byteLength ) {
            c=t.getUint8(i); if (c<128) { s+=itoa(c); i++; } else
            if ((c>191) && (c<224)) { c2=t.getUint8(i+1); s+=itoa(((c&31)<<6)|(c2&63)); i+=2; }
            else { c2=t.getUint8(i+1); c3=t.getUint8(i+2); s+=itoa(((c&15)<<12)|((c2&63)<<6)|(c3&63));
                   i+=3; } } return s; }

    // BERT Encoder

    function uc(u1,u2) { if (u1.byteLength == 0) return u2; if (u2.byteLength == 0) return u1;
                         var a = new Uint8Array(u1.byteLength + u2.byteLength);
                         a.set(u1, 0); a.set(u2, u1.byteLength); return a; };
    function ar(o)     { return o.v instanceof ArrayBuffer ? new Uint8Array(o.v) : o.v instanceof Uint8Array ? o.v :
                         Array.isArray(o.v) ? new Uint8Array(o.v) : new Uint8Array(utf8_toByteArray(o.v).v);}
    function fl(a)     { return a.reduce(function(f,t){ return uc(f, t instanceof Uint8Array ? t :
                                                                  Array.isArray(t) ? fl(t) : new Uint8Array([t]) ); }, new Uint8Array()); }
    API.atom = function(o)   { return {t:100,v:utf8_toByteArray(o).v}; };
    API.bin = function(o)    { return {t:109,v:o instanceof ArrayBuffer ? new Uint8Array(o) : o instanceof Uint8Array ? o : utf8_toByteArray(o).v}; };
    API.tuple = function()   { return {t:104,v:Array.apply(null,arguments)}; };
    API.list = function()    { return {t:108,v:Array.apply(null,arguments)}; };
    API.number = function(o) { return {t:98,v:o}; };

    function enc(o)    { return fl([131,ein(o)]); }
    function ein(o)    { return Array.isArray(o)?en_108({t:108,v:o}):eval('en_'+o.t)(o); }
    function en_undefined(o) { return [106]; }
    function en_98(o)  { return [98,o.v>>>24,(o.v>>>16)&255,(o.v>>>8)&255,o.v&255]; }
    function en_97(o)  { return [97,o.v];}
    function en_106(o) { return [106];}
    function en_100(o) { return [100,o.v.length>>>8,o.v.length&255,ar(o)]; }
    function en_107(o) { return [107,o.v.length>>>8,o.v.length&255,ar(o)];}
    function en_104(o) { var l=o.v.length,r=[]; for(var i=0;i<l;i++)r[i]=ein(o.v[i]); return [104,l,r]; }
    function en_109(o) { var l=o.v instanceof ArrayBuffer ? o.v.byteLength : o.v.length;
                         return[109,l>>>24,(l>>>16)&255,(l>>>8)&255,l&255,ar(o)]; }
    function en_108(o) { var l=o.v.length,r=[]; for(var i=0;i<l;i++)r.push(ein(o.v[i]));
                         return o.v.length==0?[106]:[108,l>>>24,(l>>>16)&255,(l>>>8)&255,l&255,r,106]; }
    // BERT Decoder

    function nop(b) { return []; };
    function big(b) { var sk=b==1?sx.getUint8(ix++):sx.getInt32((a=ix,ix+=4,a)); ix+=sk+1; return []; };
    function int(b) { return b==1?sx.getUint8(ix++):sx.getInt32((a=ix,ix+=4,a)); };
    function dec(d) { sx=new DataView(d);ix=0; if(sx.getUint8(ix++)!==131)throw("BERT?"); return din(); };
    function str(b) { var dv,sz=(b==2?sx.getUint16(ix):sx.getInt32(ix));ix+=b;
                      var r=sx.buffer.slice(ix,ix+=sz); return b==2?utf8_dec(r):r; };
    function run(b) { var sz=(b==1?sx.getUint8(ix):sx.getUint32(ix)),r=[]; ix+=b;
                      for(var i=0;i<sz;i++) r.push(din()); if(b==4)ix++; return r; };
    function din()  { var c=sx.getUint8(ix++),x; switch(c) { case 97: x=[int,1];break;
                                                           case 98:  x=[int,4]; break; case 100: x=[str,2]; break;
                                                           case 110: x=[big,1]; break; case 111: x=[big,4]; break;
                                                           case 104: x=[run,1]; break; case 107: x=[str,2]; break;
                                                           case 108: x=[run,4]; break; case 109: x=[str,4]; break;
                                                           default:  x=[nop,0]; } return {t:c,v:x[0](x[1])};};

    API.encode = function(t) {
        return enc(t);
    };

    API.decode = function(b) {
        return dec(b);
    };

    // array buffer to string
    API.ab2str = function(buf) {
        return String.fromCharCode.apply(null, new Uint8Array(buf));
    };

    return API;
};
/* End of WebRTCO BERT object */
