# AUG Mods Checker

The program for synchronization of game modpacks.

## Download binaries:
[Download](https://github.com/Augmeneco/AugModsChecker/releases/download/1.1/AugMC.zip)

## Usage:
  - Install AugMC
  - Input path to mods directory and User/Repository
  - Click "Synchronize"

## Hosting:
For hosting modpacks you must create repository on [GitHub](https://github.com) and and just upload the mod files. With AugMC you must create md5 checksums and upload it to your repository too. Without "md5list.json" modpack cannot be updated.

## Dependencies
On Windows you need:
```
libcrypto-1_1-x64.dll
libcurl.dll
libssl-1_1-x64.dll
```

## Building from source:

Open augmc.lpr with [Lazarus IDE](https://www.lazarus-ide.org) and build or in terminal:
```sh
lazbuild augmc.lpr
```
