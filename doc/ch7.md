Chapter 7. Input and Output

    All input and output operations are performed through ports. 
    A port is a pointer into a (possibly infinite) stream of data (often a file), 
    an opening through which programs may draw bytes or characters from the stream or 
    place bytes or characters into the stream. 
    A port may be an input port, an output port, or both simultaneously.

    Ports are first-class objects, like any other object in Scheme. 
    Like procedures, ports do not have a printed representation the way strings and numbers do. 
    There are initially three ports: the current input port, 
                                            current output port, 
                                        and current error port, 
    which are textual ports connected to the process's standard input, standard output, and standard error streams. 
    Several ways to open new ports are provided.

    An input port often points to a finite stream, e.g., an input file stored on disk. 
    If one of the input operations, e.g., get-u8, get-char, or get-datum, is asked to read from a port that has reached the end of a finite stream, 
    it returns a special eof (end of file) object. 
    The predicate eof-object? may be used to determine if the value returned from the input operation is the eof object.

    Ports are either binary or textual. 
    A binary port allows a program to read or write 8-bit unsigned bytes, or "octets," from or to the underlying stream. 
    A textual port allows a program to read or write characters.

    In many cases, the underlying stream is organized as a sequence of bytes, but these bytes should be treated as encodings for characters. 
    In this case, a textual port may be created with a transcoder to decode bytes to characters (for input) or encode characters to bytes (for output). 
    A transcoder encapsulates a codec that determines how characters are represented as bytes. 
    Three standard codecs are provided: a latin-1 codec, a Unicode utf-8 codec, and a Unicode utf-16 codec. 
    For the latin-1 encoding, each character is represented by exactly one byte. 
    For utf-8, each character is represented by from one to four bytes, 
    and for utf-16, each character is represented by two or four bytes.

    A transcoder also encapsulates an eol style that determines whether and how line endings are recognized. 
    If the eol style is none, no line endings are recognized. The six other standard eol styles are the following:

    lf:	line-feed character
    cr:	carriage-return character
    nel:	Unicode next-line character
    ls:	Unicode line-separator character
    crlf:	carriage return followed by line feed, and
    crnel:	carriage return followed by next line

    The eol style affects input and output operations differently. 
    For input, any eol style except none causes each of the line-ending characters or two-character sequences to be converted into a single line-feed character. 
    For output, any eol style except none causes line-feed characters to be converted into the specific one- or two-character sequence associated with the eol style. 
    In the input direction, all eol styles except none are equivalent, while in the output direction, the eol styles none and lf are equivalent.

    In addition to the codec and eol style, a transcoder encapsulates just one other piece of information: an error-handling mode that determines what happens if a decoding or encoding error occurs, 
    i.e., if a sequence of bytes cannot be converted to a character with the encapsulated codec in the input direction or a character cannot be converted to a sequence of bytes with the encapsulated codec in the output direction. 
    The error-handling mode is ignore, raise, or replace. If the error-handling mode is ignore, the offending sequence of bytes or the character is ignored. 
    If the error-handling mode is raise, an exception with condition type i/o-decoding or i/o-encoding is raised; 
        in the input direction, the port is positioned beyond the sequence of bytes. 
    If the error-handling mode is replace, a replacement character or character encoding is produced: 
    in the input direction, the replacement character is U+FFFD, 
    while in the output direction, the replacement is either the encoding of U+FFFD for utf-8 and utf-16 codecs or the encoding of the question-mark character ( ? ) for the latin-1 codec.

    A port may be buffered for efficiency, to eliminate the overhead of a call into the operating system for each byte or character. 
    Three standard buffer modes are supported: block, line, and none. 
    With block buffering, input is drawn from a stream and output is sent to the stream in chunks of some implementation-dependent size. 
    With line buffering, buffering is performed on a line-by-line basis or on some other implementation-dependent basis. 
    Line buffering is typically distinguished from block buffering only for textual output ports; there are no line divisions in binary ports, and input is likely to be drawn from a stream as it becomes available. 
    With buffer-mode none, no buffering is performed, so output is sent immediately to the stream and input is drawn only as needed.

    The remainder of this chapter covers operations on transcoders, file ports, standard ports, string and bytevector ports, custom ports, general port operations, input operations, output operations, 
    convenience I/O, filesystem operations, and conversions between bytevectors and strings.

  Section 7.1. Transcoders


