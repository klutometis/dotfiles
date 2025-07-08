# Building tree-sitter-markdown

## Overview
Instructions for building tree-sitter-markdown from source, including a necessary patch to fix build issues.

## Steps

### 1. Clone the repository
```bash
cd build
git clone https://github.com/tree-sitter-grammars/tree-sitter-markdown.git
cd tree-sitter-markdown
```

### 2. Apply build fix patch
The repository has a build issue where it looks for `grammar.js` in the wrong location. Apply this patch to `common/common.mak`:

```diff
diff --git a/common/common.mak b/common/common.mak
index b59a443..c03b27a 100644
--- a/common/common.mak
+++ b/common/common.mak
@@ -66,7 +66,7 @@ $(LANGUAGE_NAME).pc: bindings/c/$(LANGUAGE_NAME).pc.in
 		-e 's|@CMAKE_INSTALL_PREFIX@|$(PREFIX)|' \
 		-e 's|@TS_REQUIRES@|$(REQUIRES)|' $< > $@
 
-$(PARSER): $(SRC_DIR)/grammar.js
+$(PARSER): grammar.js
 	$(TS) generate $^
 
 install: all
```

### 3. Build
```bash
make
```

### 4. Copy libraries to Emacs
```bash
cd build && cp tree-sitter-markdown/libtree-sitter-markdown.so tree-sitter-markdown-inline/libtree-sitter-markdown-inline.so ~/.emacs.d/tree-sitter
```

## Expected Output
The build should complete successfully and create:
- `tree-sitter-markdown/libtree-sitter-markdown.a`
- `tree-sitter-markdown/libtree-sitter-markdown.so`
- `tree-sitter-markdown-inline/libtree-sitter-markdown-inline.a`  
- `tree-sitter-markdown-inline/libtree-sitter-markdown-inline.so`

## Notes
- The patch fixes the parser dependency path in the makefile
- Both the main markdown parser and inline parser are built
- Build tested successfully on Linux
