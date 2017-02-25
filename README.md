# README #

This repository is in bitbucket being used for free and as such it has a maximum of 5 users. I only see it being used at most by 3 users.

### What is this repository for? ###

* Quick summary

  It hold the current sources for the port from COBOL application CTOS to running as GNUCobol (aka OpenCobol) application hosted on LINUX.

* Version
 
  Initial

* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### How do I get set up? ###

* Installing the Server
  
  1. Install Linux Mint Cinnamon (17.1 - current latest) making the main user ctools.
     From Update Manager install all the outstanding updates. This is shown by the 
     installing updates having an exclamation ! in the shield next to clock in the bottom
     left of the screen.
 
  2. From a terminal install the following using apt-get
    - sudo apt-get install git git-gui g++ cmake open-cobol

  3. Using Software Manager install
    
     - putty
     - openssh  
     - sublime text
     - doublecmd gtk 

  4. Set up to retrieve the repository - note you must supply <user> and <password> in git clone below.

     - sudo mkdir /ctools
     - sudo chown ctools:ctools /ctools
     - mkdir /ctools/dev
     - cd /ctools/dev
     - git clone https://<user>:<password>@bitbucket.org/VincentRisi/ctos4ctools source
     - mkdir build
     - cd build
     - cmake ../source
     - make install

  5. Change the key bindings using dconf-editor - you must first install
 
     - sudo apt-get install dconf-tools
     - then run the dconf-editor in a terminal session
     - org
     - Cinamon
     - desktop
     - keybindings
     - wm
     - change all the entries in [ ] brackets so that the system does not interfere with the CTOS key strokes




