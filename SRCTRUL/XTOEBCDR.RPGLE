000100990908     H dftactgrp(*yes)
000200990908
000300011129     D*------------
000400060516     D* SCHIERE A PROGRAMMA
000500011129     D*------------
000600060517     D skASCIDec       S              3  0 DIM(140) CTDATA PERRCD(7)
000700060517     D skASCIHex       S              2    DIM(140) CTDATA PERRCD(7)
000800060517     D skASCIChr       S              3    DIM(140) CTDATA PERRCD(7)
000900060517     D skEBCDDec       S              3  0 DIM(140) CTDATA PERRCD(7)
001000060517     D skEBCDHex       S              2    DIM(140) CTDATA PERRCD(7)
001100060517     D skEBCDChr       S              3    DIM(140) CTDATA PERRCD(7)
001200060517     D*------------
001300060517     D* DS D PROCEDURA
001400060517     D*------------
001500060517     D XTOEBCDDS     E DS
001600011129     D*------------
001700011129     D* VARIABILI DI WRK
001800011129     D*------------
001900060517     D jSk             S              3  0 INZ
002000060517     D wDec            S              3  0 INZ
002100060517     D wFlgCvt         S              1    INZ
002200000626
002300000626
002400060517     C*
002500060517     C* Verifico subito il tipo lancio richiesto
002600060517     C                   if        §XTETLA = 'C'                                * solo chiusura
002700060517     C                   seton                                        lr
002800060517     C                   endif
002900060517     C*
003000060517     C                   if        §XTETLA = 'E'                                * solo elaborazione
003100060517     C                   exsr      converti
003200060517     C                   seton                                        rt
003300060517     C                   endif
003400060517     C*
003500060517     C                   if        §XTETLA = *blanks                            * elabora e chiude
003600060517     C                   exsr      converti
003700060517     C                   seton                                        lr
003800060517     C                   endif
003900000626     C*
004000060516
004100060517
004200060517
004300060517     C*------------------------------------------------------------------------*
004400060517     C* CONVERTI - Routine di reperimento PORTO e VARIE fatturate
004500060517     C*------------------------------------------------------------------------*
004600060517     C     CONVERTI      BEGSR
004700060517     C*
004800060517     C* Inizializzo flag d procedura
004900060517     C                   EVAL      §XTEERR = *blanks
005000060517     C                   EVAL      wFlgCvt = *blanks
005100060517     C*
005200060517     C* Precedenza alla conversione da ASCII a EBCDIC
005300060517     C*
005400060517     C* - Do precedenza 1, se indicato, al valore decimale
005500060517     C                   IF        wFlgCvt = *blanks
005600060517     C                   IF        §XTEADEC <> *blanks
005700060517     C                   MOVE(P)   §XTEADEC      wDec
005800060517     C                   Z-ADD     1             jSk
005900060517     C     wDec          LOOKUP    skASCIDec(jSk)                         55
006000060517     C                   IF        %equal
006100060517     C                   EVAL      §XTEAHEX = skASCIHex(jSk)
006200060517     C                   EVAL      §XTEACHR = skASCIChr(jSk)
006300060517     C* Reperisco i controvalori EBCDIC del carattere trovato
006400060517     C                   Z-ADD     1             jSk
006500060517     C     §XTEACHR      LOOKUP    skEBCDChr(jSk)                         55
006600060517     C                   IF        %equal
006700060517     C                   MOVE(P)   skEBCDDec(jSk)§XTEEDEC
006800060517     C                   EVAL      §XTEEHEX = skEBCDHex(jSk)
006900060517     C                   EVAL      §XTEECHR = skEBCDChr(jSk)
007000060517     C                   ELSE
007100060517     C                   EVAL      §XTEERR = '1'
007200060517     C                   ENDIF
007300060517     C*
007400060517     C                   ELSE
007500060517     C                   EVAL      §XTEERR = '1'
007600060517     C                   ENDIF
007700060517     C                   ENDIF
007800060517     C                   ENDIF
007900060517     C*
008000060517     C* - Do precedenza 2, se indicato, al valore esadecimale
008100060517     C                   IF        wFlgCvt = *blanks
008200060517     C                   IF        §XTEAHEX <> *blanks
008300060517     C                   Z-ADD     1             jSk
008400060517     C     §XTEAHEX      LOOKUP    skASCIHex(jSk)                         55
008500060517     C                   IF        %equal
008600060517     C                   MOVE(P)   skASCIDec(jSk)§XTEADEC
008700060517     C                   EVAL      §XTEACHR = skASCIChr(jSk)
008800060517     C* Reperisco i controvalori EBCDIC del carattere trovato
008900060517     C                   Z-ADD     1             jSk
009000060517     C     §XTEACHR      LOOKUP    skEBCDChr(jSk)                         55
009100060517     C                   IF        %equal
009200060517     C                   MOVE(P)   skEBCDDec(jSk)§XTEEDEC
009300060517     C                   EVAL      §XTEEHEX = skEBCDHex(jSk)
009400060517     C                   EVAL      §XTEECHR = skEBCDChr(jSk)
009500060517     C                   ELSE
009600060517     C                   EVAL      §XTEERR = '1'
009700060517     C                   ENDIF
009800060517     C*
009900060517     C                   ELSE
010000060517     C                   EVAL      §XTEERR = '1'
010100060517     C                   ENDIF
010200060517     C                   ENDIF
010300060517     C                   ENDIF
010400060517     C*
010500060517     C* - Do precedenza 3, se indicato, al valore carattere
010600060517     C                   IF        wFlgCvt = *blanks
010700060517     C                   IF        §XTEACHR <> *blanks
010800060517     C                   Z-ADD     1             jSk
010900060517     C     §XTEACHR      LOOKUP    skASCIChr(jSk)                         55
011000060517     C                   IF        %equal
011100060517     C                   MOVE(P)   skASCIDec(jSk)§XTEADEC
011200060517     C                   EVAL      §XTEAHEX = skASCIHex(jSk)
011300060517     C* Reperisco i controvalori EBCDIC del carattere trovato
011400060517     C                   Z-ADD     1             jSk
011500060517     C     §XTEACHR      LOOKUP    skEBCDChr(jSk)                         55
011600060517     C                   IF        %equal
011700060517     C                   MOVE(P)   skEBCDDec(jSk)§XTEEDEC
011800060517     C                   EVAL      §XTEEHEX = skEBCDHex(jSk)
011900060517     C                   EVAL      §XTEECHR = skEBCDChr(jSk)
012000060517     C                   ELSE
012100060517     C                   EVAL      §XTEERR = '1'
012200060517     C                   ENDIF
012300060517     C*
012400060517     C                   ELSE
012500060517     C                   EVAL      §XTEERR = '1'
012600060517     C                   ENDIF
012700060517     C                   ENDIF
012800060517     C                   ENDIF
012900060517     C*
013000060517     C* Eventualmente anche conversione da EBCDIC a ASCII
013100060517     C*
013200060517     C* - Do precedenza 1, se indicato, al valore decimale
013300060517     C                   IF        wFlgCvt = *blanks
013400060517     C                   IF        §XTEEDEC <> *blanks
013500060517     C                   MOVE(P)   §XTEEDEC      wDec
013600060517     C                   Z-ADD     1             jSk
013700060517     C     wDec          LOOKUP    skEBCDDec(jSk)                         55
013800060517     C                   IF        %equal
013900060517     C                   EVAL      §XTEEHEX = skEBCDHex(jSk)
014000060517     C                   EVAL      §XTEECHR = skEBCDChr(jSk)
014100060517     C* Reperisco i controvalori ASCII del carattere trovato
014200060517     C                   Z-ADD     1             jSk
014300060517     C     §XTEECHR      LOOKUP    skASCIChr(jSk)                         55
014400060517     C                   IF        %equal
014500060517     C                   MOVE(P)   skASCIDec(jSk)§XTEADEC
014600060517     C                   EVAL      §XTEAHEX = skASCIHex(jSk)
014700060517     C                   EVAL      §XTEACHR = skASCIChr(jSk)
014800060517     C                   ELSE
014900060517     C                   EVAL      §XTEERR = '1'
015000060517     C                   ENDIF
015100060517     C*
015200060517     C                   ELSE
015300060517     C                   EVAL      §XTEERR = '1'
015400060517     C                   ENDIF
015500060517     C                   ENDIF
015600060517     C                   ENDIF
015700060517     C*
015800060517     C* - Do precedenza 2, se indicato, al valore esadecimale
015900060517     C                   IF        wFlgCvt = *blanks
016000060517     C                   IF        §XTEEHEX <> *blanks
016100060517     C                   Z-ADD     1             jSk
016200060517     C     §XTEEHEX      LOOKUP    skEBCDHex(jSk)                         55
016300060517     C                   IF        %equal
016400060517     C                   MOVE(P)   skEBCDDec(jSk)§XTEEDEC
016500060517     C                   EVAL      §XTEECHR = skEBCDChr(jSk)
016600060517     C* Reperisco i controvalori ASCII del carattere trovato
016700060517     C                   Z-ADD     1             jSk
016800060517     C     §XTEECHR      LOOKUP    skASCIChr(jSk)                         55
016900060517     C                   IF        %equal
017000060517     C                   MOVE(P)   skASCIDec(jSk)§XTEADEC
017100060517     C                   EVAL      §XTEAHEX = skASCIHex(jSk)
017200060517     C                   EVAL      §XTEACHR = skASCIChr(jSk)
017300060517     C                   ELSE
017400060517     C                   EVAL      §XTEERR = '1'
017500060517     C                   ENDIF
017600060517     C*
017700060517     C                   ELSE
017800060517     C                   EVAL      §XTEERR = '1'
017900060517     C                   ENDIF
018000060517     C                   ENDIF
018100060517     C                   ENDIF
018200060517     C*
018300060517     C* - Do precedenza 3, se indicato, al valore carattere
018400060517     C                   IF        wFlgCvt = *blanks
018500060517     C                   IF        §XTEECHR <> *blanks
018600060517     C                   Z-ADD     1             jSk
018700060517     C     §XTEECHR      LOOKUP    skEBCDChr(jSk)                         55
018800060517     C                   IF        %equal
018900060517     C                   MOVE(P)   skEBCDDec(jSk)§XTEEDEC
019000060517     C                   EVAL      §XTEEHEX = skEBCDHex(jSk)
019100060517     C* Reperisco i controvalori ASCII del carattere trovato
019200060517     C                   Z-ADD     1             jSk
019300060517     C     §XTEECHR      LOOKUP    skASCIChr(jSk)                         55
019400060517     C                   IF        %equal
019500060517     C                   MOVE(P)   skASCIDec(jSk)§XTEADEC
019600060517     C                   EVAL      §XTEAHEX = skASCIHex(jSk)
019700060517     C                   EVAL      §XTEACHR = skASCIChr(jSk)
019800060517     C                   ELSE
019900060517     C                   EVAL      §XTEERR = '1'
020000060517     C                   ENDIF
020100060517     C*
020200060517     C                   ELSE
020300060517     C                   EVAL      §XTEERR = '1'
020400060517     C                   ENDIF
020500060517     C                   ENDIF
020600060517     C                   ENDIF
020700060517     C*
020800060517     C                   ENDSR
020900060516
021000060517
021100060517
021200000626
021300990908      /TITLE Operazioni iniziali.
021400011129     C     *inzsr        BEGSR
021500990908     C*
021600060517     C     *entry        PLIST
021700060517     C                   PARM                    XTOEBCDDS
021800060517     C*
021900060517     C                   ENDSR
022000060517     C*------------------------------------------------------------------
022100060517** skASCIDec **
022200060516000020040060080100120
022300060516001021041061081101121
022400060516002022042062082102122
022500060516003023043063083103123
022600060516004024044064084104124
022700060516005025045065085105125
022800060516006026046066086106126
022900060516007027047067087107127
023000060517008028048068088108128
023100060516009029049069089109000
023200060516010030050070090110000
023300060516011031051071091111000
023400060516012032052072092112000
023500060516013033053073093113000
023600060516014034054074094114000
023700060516015035055075095115000
023800060516016036056076096116000
023900060516017037057077097117000
024000060516018038058078098118000
024100060516019039059079099119000
024200060517** skASCIHex **
0243000605160014283C506478
0244000605160115293D516579
02450006051602162A3E52667A
02460006051603172B3F53677B
02470006051604182C4054687C
02480006051605192D4155697D
024900060516061A2E42566A7E
025000060516071B2F43576B7F
025100060517081C3044586C80
025200060516091D3145596D00
0253000605160A1E32465A6E00
0254000605160B1F33475B6F00
0255000605160C2034485C7000
0256000605160D2135495D7100
0257000605160E22364A5E7200
0258000605160F23374B5F7300
0259000605161024384C607400
0260000605161125394D617500
02610006051612263A4E627600
02620006051613273B4F637700
026300060517** skASCIChr **
026400060516NULDC4  (  <  P  d  x
026500060516SOHNAK  )  =  Q  e  y
026600060516STXSYN  *  >  R  f  z
026700060516ETXETB  +  ?  S  g  {
026800060516EOTCAN  ,  @  T  h  |
026900060516ENQ EM  -  A  U  i  }
027000060516ACKSUB  .  B  V  j  ~
027100060516BELESC  /  C  W  kDEL
027200060517 BS FS  0  D  X  l  ç
027300060516 HT GS  1  E  Y  mNUL
027400060516 LF RS  2  F  Z  nNUL
027500060516 VT US  3  G  [  oNUL
027600060516 FF SP  4  H  \  pNUL
027700060516 CR  !  5  I  ]  qNUL
027800060516 SO  "  6  J  ^  rNUL
027900060516 SI  #  7  K  _  sNUL
028000060516DLE  $  8  L  `  tNUL
028100060516DC1  %  9  M  a  uNUL
028200060516DC2  &  :  N  b  vNUL
028300060516DC3  '  ;  O  c  wNUL
028400060517** skEBCDDec **
028500060517000020040060080100120
028600060517001021041061081101121
028700060517002022042062082102122
028800060517003023043063083103123
028900060517004024044064084104124
029000060517005025045065085105125
029100060517006026046066086106126
029200060517007027047067087107127
029300060517008028048068088108128
029400060517009029049069089109000
029500060517010030050070090110000
029600060517011031051071091111000
029700060517012032052072092112000
029800060517013033053073093113000
029900060517014034054074094114000
030000060517015035055075095115000
030100060517016036056076096116000
030200060517017037057077097117000
030300060517018038058078098118000
030400060517019039059079099119000
030500060517** skEBCDHex **
0306000605170014283C506478
0307000605170115293D516579
03080006051702162A3E52667A
03090006051703172B3F53677B
03100006051704182C4054687C
03110006051705192D4155697D
031200060517061A2E42566A7E
031300060517071B2F43576B7F
031400060517081C3044586C80
031500060517091D3145596D00
0316000605170A1E32465A6E00
0317000605170B1F33475B6F00
0318000605170C2034485C7000
0319000605170D2135495D7100
0320000605170E22364A5E7200
0321000605170F23374B5F7300
0322000605171024384C607400
0323000605171125394D617500
03240006051712263A4E627600
03250006051713273B4F637700
032600060517** skEBCDChr **
032700060517NUL§§§§§§§§§  &§§§§§§
032800060517SOH§§§§§§NAK§§§§§§  '
032900060517STX BS§§§§§§§§§§§§  :
033000060517ETX§§§§§§SUB§§§§§§  #
033100060517§§§CAN§§§ SP§§§§§§  @
033200060517 HT EMENQ§§§§§§§§§  '
033300060517§§§§§§ACK§§§§§§  |  =
033400060517DEL§§§BEL§§§§§§  ,  "
033500060517§§§IFS§§§§§§§§§  %§§§
033600060517§§§IGS§§§§§§§§§  _NUL
033700060517§§§IRSSYN§§§  ]  >NUL
033800060517 VTIUS§§§§§§  $  ?NUL
033900060517 FF§§§§§§§§§  *§§§NUL
034000060517 CR§§§§§§§§§  )§§§NUL
034100060517 SO§§§§§§  [  ;§§§NUL
034200060517 SI§§§EOT  .  ^§§§NUL
034300060517DLE§§§§§§  <  -§§§NUL
034400060517§§§ LF§§§  (  /§§§NUL
034500060517§§§ETB§§§  +§§§§§§NUL
034600060517§§§ESC§§§  !§§§§§§NUL
