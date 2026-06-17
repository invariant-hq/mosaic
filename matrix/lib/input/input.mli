(** Terminal input.

    This module parses raw terminal byte streams into structured events:
    keyboard input with modifiers, mouse actions, scroll wheel, bracketed paste,
    terminal resize, and focus tracking. Terminal responses (device attributes,
    mode reports, cursor position, OSC replies) are separated into their own
    stream.

    The module handles multiple terminal protocols transparently: Kitty keyboard
    protocol, SGR and URXVT mouse tracking, X10/Normal mouse tracking, and
    bracketed paste.

    {1:event_model Event model}

    The event type {!t} covers user-facing input. Terminal responses are
    reported separately as {!Response.t} values so applications can handle input
    and terminal protocol replies independently. Clipboard replies, OSC
    responses, capability reports, and unknown terminal replies are delivered on
    the response stream, not as ordinary user input.

    Keyboard input follows common terminal conventions:
    - Ctrl letters normalize to lowercase character keys.
    - ESC-prefixed Alt/Option input sets both {!Modifier.alt} and
      {!Modifier.meta}.
    - invalid legacy high bytes are interpreted as Meta/Alt bytes when they time
      out or fail UTF-8 continuation.

    Bracketed paste is emitted as {!Paste} with the exact payload between the
    markers, including empty payloads and embedded escape bytes.

    {1:parsing Parsing}

    Create a {!Parser} and feed it raw terminal bytes:
    {[
    let p = Input.Parser.create () in
    Input.Parser.feed p buf 0 len ~now ~on_event:handle_event
      ~on_response:handle_response
    ]}

    Escape sequences and UTF-8 byte sequences may arrive fragmented across
    reads; the parser buffers partial sequences until they complete or time out.
    Ambiguous sequences (lone Escape vs. Alt+key) use a 50ms timeout; clearly
    incomplete sequences (CSI, OSC, UTF-8) use 100ms. Call {!Parser.drain} after
    {!Parser.deadline} to emit pending events.

    {b Warning.} The parser is not thread-safe; use one instance per input
    source. *)

include module type of Event
(** @inline *)

module Parser = Parser
(** Incremental input parser. See {!Parser}. *)
