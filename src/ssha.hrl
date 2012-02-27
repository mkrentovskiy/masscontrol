-define(TIMEOUT, 600000).
-define(BANNER_TIMEOUT, 6000).
-define(PROMPT_TIMEOUT, 1000).
-define(COMMAND_TIMEOUT, 600000).

-define(TERM_WIDTH, 500).
-define(TERM_HEIGHT, 32000).

-define(PACKET_SIZE, 32768).
-define(WINDOW_SIZE, 1024*?PACKET_SIZE).

-define(LOG(F,I), io:format(F,I)).
