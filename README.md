# Contributions
Third party contributions to Dolphin that are not part of the core image.

* This repository should be cloned/downloaded directly under the Dolphin root directory (e.g, Dolphin/Contributions).
* Contributions should be under the MIT licence.
* Contributions should be split into sub-directories by functional grouping. You may choose to add packages under a diretory with the original author's name as we did in the past. However, bear in mind that since these are now entirely public and likely to be edited by many people, this may not be a good idea. If in doubt, for the time being, add them under the author's name and we can move them later.
* Contributions directories should contain a README.md and may include an optional modified MIT licence with a specific copyright.
* Contributions should be in the Dolphin .PAX format (not .PAC).
* Contributions must all coexist with one another. They must be loadable alongside all others, without clashes, into a DPRO image.
* Contribution changes must pass all tests before submission (i.e. TestAll.cmd should run without errors).
