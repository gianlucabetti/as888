000100090710     D socket...
000200090710     D                 PR            10I 0 ExtProc('socket')
000300090710     D  addr_family                  10I 0 value
000400090710     D  type                         10I 0 value
000500090710     D  protocol                     10I 0 value
000600090710     D*
000700090710     D close...
000800090710     D                 PR            10I 0 ExtProc('close')
000900090710     D  sock_desc                    10I 0 value
001000090710     D*
001100090710     D send...
001200090710     D                 PR            10I 0 ExtProc('send')
001300090710     D  sock_desc                    10I 0 value
001400090710     D  buffer                         *   value
001500090710     D  buffer_len                   10I 0 value
001600090710     D  flags                        10I 0 value
001700151126     D*
001800151126     D sendTo...
001900151126     D                 PR            10I 0 ExtProc('sendto')
002000151126     D  sock_desc                    10I 0 value
002100151126     D  buffer                         *   value
002200151126     D  buffer_len                   10I 0 value
002300151126     D  flags                        10I 0 value
002400151126     D  dest_addr                      *   value
002500151126     D  addr_len                     10I 0 value
002600110225     D*
002700110225     D recv...
002800110225     D                 PR            10I 0 ExtProc('recv')
002900110225     D  sock_desc                    10I 0 value
003000110225     D  buffer                         *   value
003100110225     D  buffer_len                   10I 0 value
003200110225     D  flags                        10I 0 value
003300151126     D*
003400151126     D recvFrom...
003500151126     D                 PR            10I 0 ExtProc('recvfrom')
003600151126     D  sock_desc                    10I 0 value
003700151126     D  buffer                         *   value
003800151126     D  buffer_len                   10I 0 value
003900151126     D  flags                        10I 0 value
004000151126     D  from_addr                      *   value
004100151126     D  addr_len                     10I 0
004200090710     D*
004300090710     D connect...
004400090710     D                 PR            10I 0 ExtProc('connect')
004500090710     D  sock_desc                    10I 0 value
004600090710     D  dest_addr                      *   value
004700090710     D  addr_len                     10I 0 value
004800151009     D*
004900151009     D bind...
005000151009     D                 PR            10I 0 ExtProc('bind')
005100151009     D  sock_desc                    10I 0 value
005200151009     D  local_addr                     *   value
005300151009     D  addr_len                     10I 0 value
005400151009     D*
005500151009     D listen...
005600151009     D                 PR            10I 0 ExtProc('listen')
005700151009     D  sock_desc                    10I 0 value
005800151009     D  back_log                     10I 0 value
005900151012     D*
006000151012     D accept...
006100151012     D                 PR            10I 0 ExtProc('accept')
006200151012     D  sock_desc                    10I 0 value
006300151012     D  address                        *   value
006400151012     D  addr_len                     10I 0
006500151126     D*
006600151126     D setSockOpt...
006700151126     D                 PR            10I 0 ExtProc('setsockopt')
006800151126     D  sock_desc                    10I 0 value
006900151126     D  level                        10I 0 value
007000151126     D  option_name                  10I 0 value
007100151126     D  option_value                   *   value
007200151126     D  option_len                   10I 0 value
007300151127     D*
007400151127     D select...
007500151127     D                 PR            10I 0 ExtProc('select')
007600151127     D  max_desc                     10I 0 value
007700151127     D  read_set                       *   value
007800151127     D  write_set                      *   value
007900151127     D  except_set                     *   value
008000151127     D  wait_time                      *   value
008100151127     D*
008200151127     D/if not defined(FCNTL_PROTOTYPE)
008300151127     D fcntl...
008400151127     D                 PR            10I 0 ExtProc('fcntl')
008500151127     D   SocketDesc                  10I 0 value
008600151127     D   Command                     10I 0 value
008700151127     D   Arg                         10I 0 value Options(*NOPASS)
008800151127     D/define FCNTL_PROTOTYPE
008900151127     D/endif
009000151127     D*
