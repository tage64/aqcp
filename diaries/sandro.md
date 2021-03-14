* Wed Feb 17: General research on Network Programming and some reading through the Network.Socket library
* Thu Feb 18: More research on Network and Socket Programming
* Fri Feb 19: Coded Socket.hs prototype 1 (later changed to TCP.hs)
* Sat Feb 20: Added some build instructions for linux (ubuntu) and Mac OS X to readme
* Mon Feb 22: Started researching on Audio compression and the Vorbis project
* Tue Feb 23: Tested implementation of TCP.hs and wrote docs for TCP.hs and TCPTest.hs
* Thu Feb 25: Researched on System.Console.GetOpt library and coded Options.hs prototype 1
* Fri Feb 25: After coding a prototype with System.Console.GetOpt I decided to switch to another
library namely System.Console.ArgParser which seemed to have more of what we needed for this project. Finished coding Options.hs
* Sun Feb 28: Added docs to Options.hs and testet whole application with Tage. We ran into some issues regarding portaudio not finding
soundcard device on the server laptop. The plan now is to build an executable on the Windows platform instead.
* Mon Mar 1: Started writing on the report, more specifically on the external libaries part. What libraries we used and how we used them in the context of our project. Started drawing some flowcharts that explains in a abstract way how the program works.
* Tue Mar 2: Continued writing on the report, finished writing the function explanations for network and audio.
* Wed Mar 3: Modified Options, added two new optional flag arguments to the constructors Server and Client namely input and output device aswell as extended makeParser. Also added new constructor ListAllDevices to Options. Continued writing on the report, polished the explanation part for AudioIO and added some new text to the discussion part. Lastly I wrote the example usage part of the report.
* Thu Mar 4: More writing on the report, mostly some finishing touches here and there. Added the build instructions and the program interface to the report. Control read the report toghether with Karl making sure there aren't any misspelling of any kind and so on.
* Sat 13 Mar: Added explanation of why we used the TCP protocol in the program to the report.
* Sun 14 Mar: Removed centralserver skeleton from Options.hs and Main.hs, also did some final polish on the report.
