000100990226             PGM
000200990213             DCL        VAR(&SYSNAME) TYPE(*CHAR) LEN(8)
000300990226             DCL        VAR(&CTLDEV) TYPE(*CHAR) LEN(8)
000400990226             DCL        VAR(&REMSYS) TYPE(*CHAR) LEN(8)
000500990226             MONMSG     MSGID(CPF0000)
000600990213             RTVNETA    SYSNAME(&SYSNAME)
000700990226             CRTNWIISDN NWID(&SYSNAME) RSRCNAME(CMN11) ONLINE(*YES) +
000800990226                          NETTYPE(*ETSI) PCLENTRY((*IDLC *LOAD 2))
000900990226             CRTLINIDLC LIND(LINEAISDN) ONLINE(*YES) +
001000990226                          VRYWAIT(*NOWAIT) CNN(*SWT) SWTCNN(*BOTH) +
001100990226                          EXCHID(*SYSGEN) THRESHOLD(*OFF) +
001200990226                          MAXFRAME(2048) SWTNWISLCT(*FIRST) +
001300990226                          TEXT(*BLANK) IDLCWDWSIZ(7) IDLCFRMRTY(7) +
001400990226                          IDLCRSPTMR(10) IDLCCNNRTY(7) +
001500990226                          LINKSPEED(4800) COSTCNN(0) COSTBYTE(0) +
001600990226                          SECURITY(*NONSECURE) PRPDLY(*TELEPHONE) +
001700990226                          USRDFN1(128) USRDFN2(128) USRDFN3(128) +
001800990226                          CMNRCYLMT(2 5)
001900990226             CHGLINIDLC LIND(LINEAISDN) SWTNWILST((&SYSNAME *B *CALC))
002000990226
002100990301             CHGCOSD    COSD(COSRFD) ROW1LINE(*SAME *SAME 4800) +
002200990301                          ROW2LINE(*SAME *SAME 4800) TEXT('x ISDN')
002300990301
002400990226             CRTCNNL    CNNL(SETRASISDL) NETTYPE(*ETSI)
002500990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS089) +
002600990301                          RMTNBR('0266045116')
002700990301             CHGVAR     VAR(&CTLDEV) VALUE('AS089I')
002800990301             CHGVAR     VAR(&REMSYS) VALUE('AS089')
002900990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
003000990301
003100990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS050) +
003200990301                          RMTNBR('0290310170')
003300990301             CHGVAR     VAR(&CTLDEV) VALUE('AS050I')
003400990301             CHGVAR     VAR(&REMSYS) VALUE('AS050')
003500990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
003600990301
003700990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS005) +
003800990301                          RMTNBR('0258018024')
003900990301             CHGVAR     VAR(&CTLDEV) VALUE('AS005I')
004000990301             CHGVAR     VAR(&REMSYS) VALUE('AS005')
004100990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
004200990301
004300990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS093) +
004400990301                          RMTNBR('0303583626')
004500990301             CHGVAR     VAR(&CTLDEV) VALUE('AS093I')
004600990301             CHGVAR     VAR(&REMSYS) VALUE('AS093')
004700990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
004800990301
004900990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS002) +
005000990301                          RMTNBR('0107261670')
005100990301             CHGVAR     VAR(&CTLDEV) VALUE('AS002I')
005200990301             CHGVAR     VAR(&REMSYS) VALUE('AS002')
005300990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
005400990301
005500990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS006) +
005600990301                          RMTNBR('0498089112')
005700990301             CHGVAR     VAR(&CTLDEV) VALUE('AS006I')
005800990301             CHGVAR     VAR(&REMSYS) VALUE('AS006')
005900990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
006000990301
006100990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS059) +
006200990301                          RMTNBR('0422910063')
006300990301             CHGVAR     VAR(&CTLDEV) VALUE('AS059I')
006400990301             CHGVAR     VAR(&REMSYS) VALUE('AS059')
006500990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
006600990301
006700990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS043) +
006800990301                          RMTNBR('0458751405')
006900990301             CHGVAR     VAR(&CTLDEV) VALUE('AS043I')
007000990301             CHGVAR     VAR(&REMSYS) VALUE('AS043')
007100990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
007200990301
007300990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS001) +
007400990301                          RMTNBR('0516651833')
007500990301             CHGVAR     VAR(&CTLDEV) VALUE('AS001I')
007600990301             CHGVAR     VAR(&REMSYS) VALUE('AS001')
007700990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
007800990301
007900990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(SETRAS) +
008000990301                          RMTNBR('0516013790')
008100990301             CHGVAR     VAR(&CTLDEV) VALUE('SETRASI')
008200990301             CHGVAR     VAR(&REMSYS) VALUE('SETRAS')
008300990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
008400990301
008500990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS018) +
008600990301                          RMTNBR('059528256')
008700990301             CHGVAR     VAR(&CTLDEV) VALUE('AS018I')
008800990301             CHGVAR     VAR(&REMSYS) VALUE('AS018')
008900990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
009000990301
009100990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS020) +
009200990301                          RMTNBR('0521615170')
009300990301             CHGVAR     VAR(&CTLDEV) VALUE('AS020I')
009400990301             CHGVAR     VAR(&REMSYS) VALUE('AS020')
009500990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
009600990301
009700990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS028) +
009800990301                          RMTNBR('0553024847')
009900990301             CHGVAR     VAR(&CTLDEV) VALUE('AS028I')
010000990301             CHGVAR     VAR(&REMSYS) VALUE('AS028')
010100990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
010200990301
010300990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS066) +
010400990301                          RMTNBR('0712901118')
010500990301             CHGVAR     VAR(&CTLDEV) VALUE('AS066I')
010600990301             CHGVAR     VAR(&REMSYS) VALUE('AS066')
010700990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
010800990301
010900990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS053) +
011000990301                          RMTNBR('0735757083')
011100990301             CHGVAR     VAR(&CTLDEV) VALUE('AS053I')
011200990301             CHGVAR     VAR(&REMSYS) VALUE('AS053')
011300990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
011400990301
011500990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS062) +
011600990301                          RMTNBR('0854315521')
011700990301             CHGVAR     VAR(&CTLDEV) VALUE('AS062I')
011800990301             CHGVAR     VAR(&REMSYS) VALUE('AS062')
011900990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
012000990301
012100990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS026) +
012200990301                          RMTNBR('0755294022')
012300990301             CHGVAR     VAR(&CTLDEV) VALUE('AS026I')
012400990301             CHGVAR     VAR(&REMSYS) VALUE('AS026')
012500990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
012600990301
012700990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS063) +
012800990301                          RMTNBR('0765453078')
012900990301             CHGVAR     VAR(&CTLDEV) VALUE('AS063I')
013000990301             CHGVAR     VAR(&REMSYS) VALUE('AS063')
013100990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
013200990226
013300990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS082) +
013400990301                          RMTNBR('070212091')
013500990301             CHGVAR     VAR(&CTLDEV) VALUE('AS082I')
013600990301             CHGVAR     VAR(&REMSYS) VALUE('AS082')
013700990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
013800990301
013900990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS067) +
014000990301                          RMTNBR('0823821694')
014100990301             CHGVAR     VAR(&CTLDEV) VALUE('AS067I')
014200990301             CHGVAR     VAR(&REMSYS) VALUE('AS067')
014300990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
014400990301
014500990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS007) +
014600990301                          RMTNBR('0805375489')
014700990301             CHGVAR     VAR(&CTLDEV) VALUE('AS007I')
014800990301             CHGVAR     VAR(&REMSYS) VALUE('AS007')
014900990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
015000990301
015100990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS015) +
015200990301                          RMTNBR('0961903820')
015300990301             CHGVAR     VAR(&CTLDEV) VALUE('AS015I')
015400990301             CHGVAR     VAR(&REMSYS) VALUE('AS015')
015500990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
015600990301
015700990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS032) +
015800990301                          RMTNBR('0916680762')
015900990301             CHGVAR     VAR(&CTLDEV) VALUE('AS032I')
016000990301             CHGVAR     VAR(&REMSYS) VALUE('AS032')
016100990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
016200990301
016300990301             ADDCNNLE   CNNL(SETRASISDL) ENTRY(AS031) +
016400990301                          RMTNBR('0957132543')
016500990301             CHGVAR     VAR(&CTLDEV) VALUE('AS031I')
016600990301             CHGVAR     VAR(&REMSYS) VALUE('AS031')
016700990301             CALL       PGM(LINEAISDNB) PARM(&CTLDEV &REMSYS)
016800990301
016900990213ENDPGM
