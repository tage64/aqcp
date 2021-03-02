* Wed Feb 17: Research and coding with AudioIO
* Thu Feb 18: More coding and research with AudioIO.hs and portaudio.
* Fri Feb 19: Even more coding with AudioIO.hs and also coding with Socket.hs.
* Sat Feb 20: Coding with Socket related tests.
* Sun Feb 21: Coding and researching about crypto. Found saltine which is a haskell library using libsodium as backend. It is a very good idea to use an existing and well tested crypto library rather than trying to figure out the best algorithms yourself. That way, we are relying on experts that our cryptography is safe.
* Mon Feb 22: Searched for good audio compression algorithms. Didn't found any good haskell bindings or implementations. Found opus which is a very good audio codec and exactly what we need, but there are no haskell bindings. Looked at the c-library and thought that it wouldn't be too hard to use FFI to call it in haskell. But it is better to get a working program first and we'll therefore skip the audio compression at first.
* Tue Feb 23: Tested the tcp module with a group member. We successfully sent information about the local weather at our respective places in plain text over the tcp connection.
            Also wrote documentation for the entire Crypto module as well as its tests.
* Wed Feb 24: Finnished the AudioIO module in src/AudioIO.hs.
            It uses portaudio via bindings from haskell's portaudio package.
            It involved quite a bit of IO monads and some coding with vectors and raw pointers when interfacing to portaudio's functions.
* Thu Feb 25: Started to implement the code in app/ which is more of a main-code and not lib-code.
              Began to implement an interface for command line parsing but handed over the implementation to an other group member.
              Then started to implement the server and needed to change withServer in TCP.hs a litle bit.
              Read about how to terminate threads and thus also the server and came up with a solution based on the async package.
* Fri Feb 26: Finished the implementation of the server.
              Split out a module about audio streaming so that it can be used by both server and client.
* Sat Feb 27: Implemented the client in app/Client.hs.
              It was quite similar to implementing the server in app/Server.hs.
              However, also made exception handling safer by introducing brackets from Control.Exception in src/TCP.hs and app/Server.hs.
              The code looks a bit more uggly with those bracket-functions, but it is needed to make sure all threads are canceled properly.
* Mon Mar 1: Added possibility to select input and output audio device by implementing wrappers around functions and types from portaudio in AudioIO.hs.
             Also learnt a bit about monad transformers and introduced the ExceptT transformer in a few functions. It was funny and the code looks even cleaner.
* Tue Mar 2: Improved functionality of the tcp sending and receiving mechanism.
             It now encodes byte strings with the binary package before sending so that the length of the sent message is tagged in the sent byte string.
             The receiving end then reads as many bytes as needed to get just the sent byte string and stops then.
             Then all byte strings sent will arrive with exactly the same length and content.
             It took roughly the whole day but at the end my computer crashed and all my saved work was lost.
             I needed to rewrite  everything in the evening but it actually became better code then.
