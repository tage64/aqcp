* Wed Feb 17: Research and coding with AudioIO
* Thu Feb 18: More coding and research with AudioIO.hs and portaudio.
* Fri Feb 19: Even more coding with AudioIO.hs and also coding with Socket.hs.
* Sat Feb 20: Coding with Socket related tests.
* Sun Feb 21: Coding and researching about crypto. Found saltine which is a haskell library using libsodium as backend. It is a very good idea to use an existing and well tested crypto library rather than trying to figure out the best algorithms yourself. That way, we are relying on experts that our cryptography is safe.
* Mon Feb 22: Searched for good audio compression algorithms. Didn't found any good haskell bindings or implementations. Found opus which is a very good audio codec and exactly what we need, but there are no haskell bindings. Looked at the c-library and thought that it wouldn't be too hard to use FFI to call it in haskell. But it is better to get a working program first and we'll therefore skip the audio compression at first.
Tue Feb 23: Tested the tcp module with a group member. We successfully sent information about the local weather at our respective places in plain text over the tcp connection.
            Also wrote documentation for the entire Crypto module as well as its tests.
