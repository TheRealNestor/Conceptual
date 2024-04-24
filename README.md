# Conceptual DSL

This is a small project implementing tool support for *Conceptual*, a custom domain specific language (DSL) for modelling and translating *concepts*  to [Alloy](https://alloytools.org/documentation.html), a formal modeling language. The notion of concepts is based on research by [Daniel Jackson (2021)](https://essenceofsoftware.com/). The DSL reflects ideas of the developer and may or may not completely align with Professor Daniel Jackson's intended language design.  

## Overview
Conceptual DSL allows users to model concepts within a specific domain, providing a structured representation. This project provides a simple compiler that translates Conceptual DSL code into Alloy code, enabling further analysis and verification.

Example usage of  *Conceptual* can be found in the `test/progs` directory. Please refer to this directory to get a sense of the language's syntax. 

## Disclaimer
This project is experimental and research-grade. It is not intended for production use and comes with no guarantees. 

## Installation
To install ConceptDSL and its dependencies, ensure you have [OPAM](https://opam.ocaml.org/) installed and execute the following commands:

```bash
opam update
opam install . --deps-only
```
or simply type 
```bash
make
```

Notably, not a lot of time has been spent scrutinizing the project, specifying the dependencies, on versioning, etc. It was developed using the `ocaml-base-compiler.5.0.0` switch and Dune version `3.14`. A few of the depencies can also be identified in the `.opam` file at the root.  

## Usage 
To execute the compiler, use the following command:
```bash 
make run <input_file>
```
where `<input_file>` is the path to the Conceptual DSL code to compile. 

## Language Support
Language support for integrated development enviroments (IDEs) has not been explored extensively or made public in these ecosystems. However, some preliminary work was done to establish language support for Visual Studio Code. The support is lightweight as no external language server is used, although many quality-of-life features are omitted as a result. 

While not publicly available as an extension, the package has been included in this repository. Enthusiasts can thus install the extensions locally, allowing the editor to recognize `.con` files and provide syntax highlighting among other things. 

Concretely, follow these steps to enable the language support:
1. Open the command palette (Command+Shift+P on macOS, Ctrl+Shift+P on Windows).
2. Search for "Developer: Install Extension from Location".
3. Navigate to the directory containing the extension (e.g., plug-ins/vscode-lang-support) and select it to install.

Finally, reload the IDE or execute "Developer: Reload Window" on the command palette. 


