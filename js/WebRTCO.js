/**
 * User: Andrey Sergienko
 * OSLIKAS http://www.oslikas.com
 */

var WebRTCO = function(signallingURL, localVideo, onRoom, onChat, getRem, onBye) {
    // <WebRTC Adapter>
    var RTCPeerConnection = null;
    var getUserMedia = null;
    var attachMediaStream = null;
    var reattachMediaStream = null;
    var webrtcDetectedBrowser = null;
    //  </WebRTC Adapter>

    var debug = false;
    var room = null;
    var initiator;

    var _sc = null;
    var _media = null;

    this.API_sendPutChatMsg = function(msg) {
        _sc.sendPublicChatMessage(msg);
    };

    this.API_isMicMuted = function() {
        return _sc.isMicMuted();
    };

    this.API_muteMic = function(mute) {
        _sc.muteMic(mute);
    };

    window.onbeforeunload = function() {
        _sc.sendByeMessage();
    };

    var sdpConstraints = {'mandatory': {'OfferToReceiveAudio':true, 'OfferToReceiveVideo':true }};

    var clog = function(str) {
        if (debug) { window.console.log(str); }
    };

    this.setDebug = function(b) {
        debug = b;
    };

//    var mergeConstraints = function(cons1, cons2) {
//        var merged = cons1;
//        for (var name in cons2.mandatory) merged.mandatory[name] = cons2.mandatory[name];
//        merged.optional.concat(cons2.optional);
//        return merged;
//    };

    // SDP stuff
    /**
     *
     * @param sdp
     * @returns {*}
     */
    var preferOpus = function(sdp) {
        var sdpLines = sdp.split('\r\n');

        for (var i = 0; i < sdpLines.length; i++) {
            if (sdpLines[i].search('m=audio') !== -1) {
                var mLineIndex = i;
                break;
            }
        }

        if (mLineIndex === null) return sdp;

        for (i = 0; i < sdpLines.length; i++) {
            if (sdpLines[i].search('opus/48000') !== -1) {
                var opusPayload = extractSdp(sdpLines[i], /:(\d+) opus\/48000/i);
                if (opusPayload) sdpLines[mLineIndex] = setDefaultCodec(sdpLines[mLineIndex], opusPayload);
                break;
            }
        }

        sdpLines = removeCN(sdpLines, mLineIndex);

        sdp = sdpLines.join('\r\n');
        return sdp;
    };

    var extractSdp = function(sdpLine, pattern) {
        var result = sdpLine.match(pattern);
        return (result && result.length == 2)? result[1]: null;
    };

    var setDefaultCodec = function(mLine, payload) {
        var elements = mLine.split(' ');
        var newLine = new Array();
        var index = 0;
        for (var i = 0; i < elements.length; i++) {
            if (index === 3) newLine[index++] = payload;
            if (elements[i] !== payload) newLine[index++] = elements[i];
        }
        return newLine.join(' ');
    };

    var removeCN = function(sdpLines, mLineIndex) {
        var mLineElements = sdpLines[mLineIndex].split(' ');
        for (var i = sdpLines.length-1; i >= 0; i--) {
            var payload = extractSdp(sdpLines[i], /a=rtpmap:(\d+) CN\/\d+/i);
            if (payload) {
                var cnPos = mLineElements.indexOf(payload);
                if (cnPos !== -1) mLineElements.splice(cnPos, 1);
                sdpLines.splice(i, 1);
            }
        }
        sdpLines[mLineIndex] = mLineElements.join(' ');
        return sdpLines;
    };

    /*
     Signalling Connection object
     */
    var WebRTCO_SC = function(signallingURL,onRoom,onChat,getRem,onBye) {
        var channelReady;
        var channel;
        var localStream = null;
        var peers = {};
        var myPID = null;

        var callback_onPubChatMsgRecv = onChat;
        var callback_onRoomRecv = onRoom;
        var callback_getRemoteVideo = getRem;
        var callback_onBye = onBye;

        this.muteMic = function(mute) {
            var audiotracks = localStream.getAudioTracks();
            for (var i = 0, l = audiotracks.length; i < l; i++) {
                audiotracks[i].enabled = !mute;
            };
        };

        this.isMicMuted = function() {
            var audiotracks = localStream.getAudioTracks();
            return !(audiotracks[0].enabled);
        };

        var onChannelOpened = function() {
            clog('Channel opened...');
            channelReady = true;

            if(location.search.substring(1,5) == "room") {
                room = location.search.substring(6);
                sendMessage({"type" : "ROOM_ENTER", "value" : room * 1});
                initiator = true;
            } else {
                sendMessage({"type" : "ROOM_CREATE", "value" : ""});
                initiator = false;
            }
//        doGetUserMedia();
        };

        var onChannelMessage = function(message) {
            clog('S->C: ' + message.data);
            processSignalingMessage(message.data);
        };

        var onChannelError = function() {
            clog('Channel error.');
        };

        var onChannelClosed = function() {
            clog('Channel closed.');
            channelReady = false;
        };

        this.sendByeMessage = function() {
            sendMessage({type: 'bye', from: myPID});
        };

        var sendMessage = function(message) {
            if (!channelReady) return;
            var msgString = JSON.stringify(message);
            clog('C->S: ' + msgString);
            channel.send(msgString);
        };

        var processSignalingMessage = function(message) {
            var msg = JSON.parse(message);
//            clog("received packed: " + msg);
            if (msg.type === 'TOPEER') {
                var peer_from = msg.from;
                var peer_to = msg.to;
                msg = msg.packet;
                var pc = peers[peer_from];
                if (undefined === pc) {
                    clog("undefined pid " + peer_from);
                    if (msg.type === 'PLEASE_CALL') {
                        clog("pid " + peer_from + "asked for call");
                        var rv = callback_getRemoteVideo(peer_from);
                        var pc = new WebRTCO_PC(rv, localStream, peer_from, myPID, sendMessage);
                        peers[peer_from] = pc;
                        pc.doCall();
                    } else return;
                }
                if (msg.type === 'offer') {
                    pc.onOffer(msg);
                } else if (msg.type === 'answer') {
                    pc.onAnswer(msg);
                } else if (msg.type === 'candidate') {
                    pc.onCandidate(msg);
                }
            } else {
                if (msg.type === 'CHATMSG') {
                    if (null === callback_onPubChatMsgRecv) return;
                    callback_onPubChatMsgRecv(msg.value);
                } else if (msg.type === 'bye') {
                    if (null !== callback_onBye) callback_onBye(msg.from);
                } else if (msg.type === 'ROOM_ENTERED') {
                    room = msg.value;
                    myPID = msg.pid;
                    clog("received room number: " + room);
                    if (null !== callback_onRoomRecv && ! initiator) callback_onRoomRecv(room);
                    initiator = false;
                } else if (msg.type === 'ROOM_LEAVE') {
                    window.location.href = "/";
                } else if (msg.type === 'PEER_JOINED') {
                        var pid = msg.pid;
                        clog("peer joined: " + pid);
                        var rv = callback_getRemoteVideo(pid);
                        var pc = new WebRTCO_PC(rv, localStream, pid, myPID, sendMessage);
                        peers[pid] = pc;
                        var msg = {type: 'PLEASE_CALL'};
                        sendMessage({type: 'TOPEER', from: myPID, to: pid, packet: msg});
                }
            }
        };

        // send public chat message via signalling server
        this.sendPublicChatMessage = function(chatMsg) {
            if (!channelReady) return;
            sendMessage({"type" : "CHATMSG", "value" : chatMsg});
        };

        this.go = function(ls) {
            localStream = ls;
        channelReady = false;
        channel = new WebSocket(signallingURL);
        channel.onopen = onChannelOpened;
        channel.onerror = onChannelError;
        channel.onmessage = onChannelMessage;
        channel.onclose = onChannelClosed;
        }
    };

    /*
     Media object
     */
    var WebRTCO_Media = function(localVideo, func) {
        var localStream = null;
        var localV = null;

        var doGetUserMedia = function() {
            var constraints = {"audio": true, "video": {"mandatory": {}, "optional": []}};
            try {
                clog("Requested access to local media with mediaConstraints:\n \"" + JSON.stringify(constraints) + "\"");
                getUserMedia(constraints, onUserMediaSuccess, onUserMediaError);
            } catch (e) {
                clog("getUserMedia failed with exception: " + e.message);
            }
        };

        var onUserMediaSuccess = function(stream) {
            clog("User has granted access to local media.");
            attachMediaStream(localV, stream);
            localV.muted = true;
            localV.play();
            localStream = stream;
            func(stream);

//            clog("Creating PeerConnection.");
//            createPeerConnection();
//            clog("Adding local stream.");
//            pc.addStream(localStream);

//            if (initiator) doCall();
        };

        var onUserMediaError = function(error) {
            clog("Failed to get access to local media. Error code was " + error.code);
        };

        localV = localVideo;
        doGetUserMedia();
    };

    /*
    Peer Connection Object
     */
    var WebRTCO_PC = function(rv, ls, rpid, lpid, func) {
        var sendSignallingMessage = func;
        var localStream = ls;
        var remoteVideo = rv;
        var pid_remote = rpid;
        var pid_my = lpid;
        var pc_config = {"iceServers":
            [{url:'stun:23.21.150.121'},
                {url:'stun:stun.l.google.com:19302'},
                {url:'stun:stun01.sipphone.com'},
                {url:'stun:stun.ekiga.net'},
                {url:'stun:stun.fwdnet.net'},
                {url:'stun:stun.ideasip.com'},
                {url:'stun:stun.iptel.org'},
                {url:'stun:stun.rixtelecom.se'},
                {url:'stun:stun.schlund.de'},
                {url:'stun:stun.l.google.com:19302'},
                {url:'stun:stun1.l.google.com:19302'},
                {url:'stun:stun2.l.google.com:19302'},
                {url:'stun:stun3.l.google.com:19302'},
                {url:'stun:stun4.l.google.com:19302'},
                {url:'stun:stunserver.org'},
                {url:'stun:stun.softjoys.com'},
                {url:'stun:stun.voiparound.com'},
                {url:'stun:stun.voipbuster.com'},
                {url:'stun:stun.voipstunt.com'},
                {url:'stun:stun.voxgratia.org'},
                {url:'stun:stun.xten.com'},
                {
                    url: 'turn:numb.viagenie.ca',
                    credential: 'muazkh',
                    username: 'webrtc@live.com'
                },
                {
                    url: 'turn:192.158.29.39:3478?transport=udp',
                    credential: 'JZEOEt2V3Qb0y27GRntt2u2PAYA=',
                    username: '28224511:1379330808'
                },
                {
                    url: 'turn:192.158.29.39:3478?transport=tcp',
                    credential: 'JZEOEt2V3Qb0y27GRntt2u2PAYA=',
                    username: '28224511:1379330808'
                }]};

        var sendMessage = function(msg) {
            sendSignallingMessage({type: 'TOPEER', from: pid_my, to: pid_remote, packet: msg});
        }

        var onIceCandidate = function(event) {
            if (event.candidate)
                sendMessage({type: 'candidate', label: event.candidate.sdpMLineIndex, id: event.candidate.sdpMid,
                    candidate: event.candidate.candidate});

            else clog("End of candidates.");
        };

        var onRemoteStreamAdded = function(event) {
            clog("Remote stream added.");
            attachMediaStream(remoteVideo, event.stream);
            remoteVideo.play();
//            remoteStream = event.stream;
        };

        var onRemoteStreamRemoved = function(event) {
            clog("Remote stream removed.");
        };

        this.doCall = function() {
            var constraints = {"optional": [], "mandatory": []};
//        var constraints = {"optional": [], "mandatory": {"MozDontOfferDataChannel": true}};
//        if (webrtcDetectedBrowser === "chrome")
//            for (var prop in constraints.mandatory) if (prop.indexOf("Moz") != -1) delete constraints.mandatory[prop];

//        constraints = mergeConstraints(constraints, sdpConstraints);
            clog("Sending offer to peer, with constraints: \n  \"" + JSON.stringify(constraints) + "\".")
            pc.createOffer(setLocalAndSendMessage, onErrorCallBack, constraints);
        };

        var doAnswer = function() {
            clog("Sending answer to peer.");
            pc.createAnswer(setLocalAndSendMessage, onErrorCallBack, sdpConstraints);
        };

        var onErrorCallBack = function(e) {
            clog("Something wrong happened when answer or offer " + e.toString());
        };

//        var remoteHangup = function() {
//            clog('Session terminated.');
//            pc.close();
//        };

        var setLocalAndSendMessage = function(sessionDescription) {
            sessionDescription.sdp = preferOpus(sessionDescription.sdp);
            pc.setLocalDescription(sessionDescription);
            sendMessage(sessionDescription);
        };

        this.onOffer = function(msg) {
            pc.setRemoteDescription(new RTCSessionDescription(msg));
            doAnswer();
        };

        this.onAnswer = function(msg) {
            pc.setRemoteDescription(new RTCSessionDescription(msg));
        };

        this.onCandidate = function(msg) {
            var candidate = new RTCIceCandidate({sdpMLineIndex:msg.label, candidate:msg.candidate});
            pc.addIceCandidate(candidate);
        };

//        var pc_config = {"iceServers": [{"url": "stun:stun.l.google.com:19302"}]};
//            var pc_constraints = {"optional": [{"DtlsSrtpKeyAgreement": true}]};
//        if (webrtcDetectedBrowser == "firefox") pc_config = {"iceServers":[{"url":"stun:23.21.150.121"}]};

        var pc_constraints = null;
        var pc = null;

        try {
                pc = new RTCPeerConnection(pc_config, pc_constraints);
                pc.onicecandidate = onIceCandidate;
                clog("Created RTCPeerConnnection with:\n" +
                    "  config: \"" + JSON.stringify(pc_config) + "\";\n" +
                    "  constraints: \"" + JSON.stringify(pc_constraints) + "\".");
            } catch (e) {
                pc = null;
                clog("Failed to create PeerConnection, exception: " + e.message);
                return;
            }
            pc.onaddstream = onRemoteStreamAdded;
            pc.onremovestream = onRemoteStreamRemoved;
        clog("my stream is: " + localStream);
            pc.addStream(localStream);
    };

    /*
     * WebRTC Adapter
     */
    var initWebRTCAdapter = function() {
        if (navigator.mozGetUserMedia) {
            webrtcDetectedBrowser = "firefox";

            RTCPeerConnection = mozRTCPeerConnection;
            RTCSessionDescription = mozRTCSessionDescription;
            RTCIceCandidate = mozRTCIceCandidate;
            getUserMedia = navigator.mozGetUserMedia.bind(navigator);

            attachMediaStream =
                function(element, stream) {
                    element.mozSrcObject = stream;
                    element.play();
                };

            reattachMediaStream =
                function(to, from) {
                    to.mozSrcObject = from.mozSrcObject;
                    to.play();
                };
            return true;
        } else if (navigator.webkitGetUserMedia) {
            webrtcDetectedBrowser = "chrome";

            RTCPeerConnection = webkitRTCPeerConnection;
            getUserMedia = navigator.webkitGetUserMedia.bind(navigator);
            attachMediaStream =
                function(element, stream) {
                    element.src = webkitURL.createObjectURL(stream);
                };

            reattachMediaStream =
                function(to, from) {
                    to.src = from.src;
                };

            if (!webkitMediaStream.prototype.getVideoTracks) {
                webkitMediaStream.prototype.getVideoTracks =
                    function() {
                        return this.videoTracks;
                    };
                webkitMediaStream.prototype.getAudioTracks =
                    function() {
                        return this.audioTracks;
                    };
            }

            if (!webkitRTCPeerConnection.prototype.getLocalStreams) {
                webkitRTCPeerConnection.prototype.getLocalStreams =
                    function() {
                        return this.localStreams;
                    };
                webkitRTCPeerConnection.prototype.getRemoteStreams =
                    function() {
                        return this.remoteStreams;
                    };
            }
            return true;
        } else return false;
    };

    initWebRTCAdapter();

    _sc = new WebRTCO_SC(signallingURL,onRoom,onChat,getRem,onBye);
    _media = new WebRTCO_Media(localVideo, _sc.go);

};

