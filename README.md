# README #

This repository is in bitbucket being used for free and as such it has a maximum of 5 users. I only see it being used at most by 3 users.

### What is this repository for? ###

* Quick summary

  It holds the current sources for the port from COBOL application CTOS to running as GNUCobol (aka OpenCobol) application hosted on LINUX.

* Version
 
  Initial 17.2 and later

* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### How do I get set up? ###

* Installing the Server
	
  1. Install Linux Mint Cinnamon (17.2 - our current version Sep 2016) making the main user ctools.
     From Update Manager install all the outstanding updates. This is shown by the 
     installing updates having an exclamation ! in the shield next to clock in the bottom
     right of the screen.
	
  2. From a terminal install the following using apt-get install
    - sudo apt-get install git git-gui g++ cmake open-cobol pdftk
	
	- For PUtty & pdftk see new instructions - 8 & 9. below - for installing on Linux mint 19.1 & greater
	  as the apt-get does not bring back the latest software for Mint 19.1 as far as these two packages are concerned
	
  3. Using Software Manager install
    
     - putty
     - openssh server
     - sublime text
     - doublecmd gtk 
     - Geany
	 - Hylafax
	
  4. Set up to retrieve the repository - note you must supply <user> and <password> in git clone below.
	
     - sudo mkdir /ctools
	
    4.1  Set all folders in ctools to ctools ownership.
      - cd /ctools
      - sudo chown -R ctools:ctools *
      - Also can do like this:    sudo chown ctools:ctools /ctools
  
     - mkdir /ctools/dev
     - cd /ctools/dev
     - git config --global user.email "Steve@Christensen.co.za"
     - git config --global user.name "Steve Christensen"
                         <username>   <pswd>
     - sudo git clone https://VincentRisi:Atl45514n@bitbucket.org/VincentRisi/ctos4ctools source
     - mkdir build
     - cd build
     - sudo cmake ../source
     - sudo make -k
     - sudo make install
	
  5. Change the key bindings using dconf-editor - you must first install:
	
     - sudo apt-get install dconf-tools
     - then run the dconf-editor in a terminal session and go to:
     - org
     - Cinamon
     - desktop
     - keybindings
     - wm
     - change all the entries in [ ] brackets so that the system does not interfere with the CTOS key strokes
	
  6. Setup putty.
	
     - in /ctools/dev/source/putty there are a number of files and folders that can be copied into /home/.putty to get the current config 
	 
  7. .bashrc needs to be copied to the users home directory with ALL the CTOS dependent entries in it.  
       A copy is found in /ctools/dev/source/installs   Three types of file for auto-exit of putty or not ...
    
  8.  Installing pdftk Linux Mint 19.1 > - installation instructions also found here:  https://linuxhint.com/install_pdftk_ubuntu/
	- sudo add-apt-repository ppa:malteworld/ppa
	- sudo apt update
	- sudo apt install pdftk
	
  9.  Installing PUtty on Linux Mint 19.1 >
	- Download  putty-0.72.tar.gz from the PUtty website
	- Put the `putty-0.72.tar.gz` into the directory  `/home/ctools/Downloads`
	- cd /home/ctools/Downloads
	- tar vxf putty-0.72.tar.gz
	
	- use menu on the pc by pressing the windows button and select Administrator/Synaptic Package Manager
	- Select Search - it will pop up a Find dialog
	- Look for Gtk selecting Look in Name Search
	- Scroll down to libgtk-3-dev, mark it and the libgtk-3-doc for installation then hit the apply button.
	
	- In a Terminal
	- cd /home/ctools/Downloads/putty-0.72
	- ./configure
	- sudo make
	- sudo make install
	
	- putty should now work properly on the Linux box

  10. Setup all the new users 
    - Press windows key 
    - go to System Settings
    - go to Users & Groups
    - Add user with a temp password.  Make sure to add in ctools, lpadmin, plugdev,sambashare
        to the user groups...
	
  11. Set user passwords by 
    - su <enter>  this to enter the root user which has powers to change user passwords
	- ctos 
	-
    - passwd <username>
    - enter the temp password then enter a new password

  12. Add all the users into the same group namely ctools
    - sudo usermod -g ctools <username>

  13. Change the WORKGROUP in Samba to allow printers to work in the group  
    - edit /etc/samba/smb.conf using sublime-text editor 
	- look for the WORKGROUP entry and change it to CTJ
	- change the Printer entries to allow browsing and guest ok = yes
	- change the entry by removing the ; in front of the entry usershare max shares = 100
	
	- *END*
