# Huffman Compression

A program that performs lossless compression of text files using Huffman compression.

## Usage

Make sure you have Stack and GHC installed.

Build the binaries using stack:

```
stack build
```

This should print out a message saying where your executable is located. For example:

```
Installing executable huffman-compression-exe in /.../huffman-compression/.stack-work/install/aarch64-osx/610bb86a05f2fc0e26985848186ecf209bbaaa1bffb7b2d745ceb354fe1f98d1/9.6.5/bin
```

Run the program as follows

```
/path/to/executable <encode|decode> <input file> <output file>
```

## Performance

The program was tested on the War and Peace text from Project Gutenberg. We recorded the following results:

```
4.0M    data/warandpeace.txt -- actual file
2.0M    data/warandpeace.enc -- encoded file
```

So, we achieved a compression ratio of ~0.50.
