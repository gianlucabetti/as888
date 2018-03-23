000100000000     h*--------------------------------------------------------------------------------------------*
000200060921     h* Controllo CAP utilizzi particolari
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     f*--------------------------------------------------------------------------------------------*
000600080521     fazcpp02l  IF   E           K DISK                                         *versioni cappario
000700060921     ftabel00f  IF   E           K DISK                                         *versioni cappario
000800060921     f
000900000000     d*--------------------------------------------------------------------------------------------*
001000060921     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
001100060921     D CCP             S              1    DIM(9)                               CARATT.CONTROLLO
001200060921     D CAP             S              1    DIM(9)                               CAP DA CONTROLLAR
001300060921     D CAF             S              1    DIM(9)                               CAP FITTIZIO
001400060921     D LET             S              1    DIM(32) CTDATA PERRCD(32)            LETTERE ALFABETO
001500060921     D LEM             S              1    DIM(32) CTDATA PERRCD(32)            LETTERE ALFAB. MINUS
001600060921     D NUM             S              1    DIM(10) CTDATA PERRCD(10)            NUMERI
001700060921     d
001800060921     d tisi95ds      E ds
001900060921     d tisi97ds      E ds
002000061030     d dcppflo       E ds
002100060921     d
002200060921     D DS15          E DS
002300060921     d
002400060921     d KCAP            S                   like(i95cap)
002500080408     d Knar            S                   like(i95nar)
002600060921     d Ktbute          S                   like(tblkut) inz(1)
002700060921     d Ktbcod          S                   like(tblcod)
002800060921     d Ktbkey          S                   like(tblkey)
002900000000     c*--------------------------------------------------------------------------------------------*
003000000000     c* Main lines
003100000000     c*--------------------------------------------------------------------------------------------*
003200060921     c     *entry        plist
003300060921     c                   parm                    tisi95ds
003400060921     c                   parm                    tisi97ds
003500060921     c
003600060921     C* TIPO LANCIO, I13TLA "C" -           CHIUSO CON LR
003700060921     C* TIPO LANCIO, I13TLA "L" - ELABORO E CHIUDO CON LR
003800060921     C* TIPO LANCIO, I13TLA " " - ELABORO E CHIUDO IN RETRN
003900060921    1C     I95TLA        IFNE      'C'
004000060921     c* Pulisco i campi di output
004100060921     c                   clear                   o97err
004200060921     c                   clear                   o97msg
004300060921     c                   clear                   o97esi
004400060921     c                   clear                   o97fit
004500060921     c                   clear                   o97uti
004600060921     c                   clear                   o97flo
004700080411     c                   clear                   o97liv
004800060921     c
004900060921     c                   exsr      CERCAP
005000060921     c                   endif
005100000000     c*
005200060921     c                   if        i95tla<>' '
005300000000     c                   SETON                                            lr
005400060921     c                   else
005500060921     c                   SETON                                            RT
005600060921     c                   endif
005700000000     c*--------------------------------------------------------------------------------------------*
005800000000     C* *inzsr - operazioni iniziali
005900000000     c*--------------------------------------------------------------------------------------------*
006000000000     C     *inzsr        BEGSR
006100060921     c
006200060921     C     KEYTAB        KLIST
006300060921     C                   KFLD                    KTBUTE
006400060921     C                   KFLD                    KTBCOD
006500060921     C                   KFLD                    KTBKEY
006600060921     c     kcpp          klist
006700080521     c                   kfld                    i97ntw
006800080411     c                   kfld                    knar
006900060921     c                   kfld                    kcap
007000000000     c                   ENDSR
007100060921     c*--------------------------------------------------------------------------------------------*
007200060921     C* Cerco record con cap esteso
007300060921     c*--------------------------------------------------------------------------------------------*
007400060921     C     CERCAP        BEGSR
007500060921     c                   eval      kcap=i95cap
007600080408     c                   eval      knar=i95nar
007700060921     c                   exsr      LETCAP
007800060921     c
007900060921     c                   if        o97esi='S'
008000060921     c                   eval      o97fit='N'
008100080411     c                   eval      o97liv='C'
008200060921     c                   else
008300060921     c
008400060921     c* Se non trovato provo col fittizio
008500060921     c                   exsr      CHKFOR
008600060921     c
008700060921     c* Se trovato senza errori
008800060921     c                   if        o97err=' '
008900060921     c                   eval      kcap=wcapfi
009000080408     c                   eval      knar=i95nar
009100060921     c                   exsr      LETCAP
009200060921     c
009300060921     c                   if        o97esi='S'
009400060921     c                   eval      o97fit='S'
009500080423     c                   eval      o97liv='C'
009600080408     c                   else
009700080408     c* Se non trovato provo con cap nullo "*" + nazione + network
009800080408     c                   eval      kcap='*        '
009900080408     c                   eval      knar=i95nar
010000080408     c                   exsr      LETCAP
010100080408     c                   if        o97esi='S'
010200080408     c                   eval      o97fit='N'
010300080411     c                   eval      o97liv='N'
010400080408     c                   else
010500080408     c* Se non trovato ancora provo con cap e nazione nulli "*" + network
010600080408     c                   eval      knar='*  '
010700080408     c                   exsr      LETCAP
010800080411     c                   if        o97esi='S'
010900080411     c                   eval      o97liv='W'
011000080411     c                   endif
011100080408     c                   endif
011200060921     c                   endif
011300080410     c                   endif
011400060921     c
011500060921     c                   endif
011600060921     c*
011700060921     c* Imposto gli altri campi di output
011800060921     c                   if        o97esi='S'
011900060921     c                   eval      o97uti=cpputi
012000060921     c                   eval      o97flo=cppflo
012100060921     c* Se non trovato recorde niente errori, imposto comunque
012200060921     c*  che è utilizzabile
012300060921     c                   else
012400060921     c                   if        o97err=' '
012500060921     c                   eval      o97uti='S'
012600060921     c                   endif
012700060921     c                   endif
012800060921     c
012900060921     c                   ENDSR
013000060921     c*--------------------------------------------------------------------------------------------*
013100060921     C* Lettura con cap su utilizzi particolari
013200060921     c*--------------------------------------------------------------------------------------------*
013300060921     c     LETCAP        BEGSR
013400080521     c     kcpp          setll     azcpp02l
013500080521     c     kcpp          reade     azcpp02l
013600080521     c                   dow       not %eof(azcpp02l)
013700060921     c* Verifico decorrenza e scadenza
013800060921     c                   if        i95dat>=cppdde and i95dat<=cppdsc
013900060921     c* trovato record
014000060921     c                   eval      o97esi='S'
014100060921     c                   leave
014200060921     c                   endif
014300080521     c     kcpp          reade     azcpp02l
014400060921     c                   enddo
014500060921     c                   ENDSR
014600060921     C*------------------------------------------------------------------------*
014700060921     C* CHKFOR - CONTROLLO FORMALE DEL CAP IN INPUT
014800060921     C*------------------------------------------------------------------------*
014900060921     C     CHKFOR        BEGSR
015000060921     c* cerco la nazione
015100060921     c                   EXSR      chknar
015200060921     c
015300060921     c* se non ci sono errori (non dovrebbe in quanto prima si deve
015400060921     c*  controllare il cap col tisi95r)
015500060921     c                   if        o97err=' '
015600060921     C*---
015700060921     C* RIASSUMO I CARATTERI DI CONTROLLO PREVISTI;
015800060921     C*  "N" - SOLO NUMERI
015900060921     C*  "L" - SOLO LETTERE
016000060921     C*  "A" - CAMPO ALFANUMERICO     (NUMERI LETTERE)
016100060921     C*  " " - CAMPO DA NON IMMETTERE
016200060921     C*  "O" - OPZIONALE ALFANUMERICO (NUMERI LETTERE E BLANK)
016300060921     C*  "Q" - OPZIONALE SOLO LETTERE (       LETTERE E BLANK)
016400060921     C*  "0" - OPZIONALE NUMERICO     (NUMERI         E BLANK)
016500060921     C*---
016600060921     C                   MOVEA     I95CAP        CAP
016700060921     C                   MOVEA     §15CAP        CCP
016800060921     C*
016900060921     C* RECUPERO CAP FITTIZIO PER EVENTUALE TENTATIVO SUCCESSIVO
017000060921     C                   Z-ADD     §15PCF        P                 2 0
017100060921     C                   CLEAR                   CAF
017200060921     C                   MOVEA     CAP(P)        CAF(P)
017300060921     C                   ADD       §15ECF        P                              *CARATT.DA TENERE
017400060921     C     P             IFLT      10
017500060921     C                   MOVEA     *BLANKS       CAF(P)
017600060921     C                   ENDIF
017700060921     C                   MOVEA     CAF           WCAPFI            9            *MEM CAP FITTIZIO
017800060921     C*
017900060921     C*
018000060921     C                   Z-ADD     1             X                 2 0
018100060921     C                   SETOFF                                       90
018200060921     C*
018300060921    1C     X             DOWLE     9
018400060921     C*
018500060921     C* CARATTERE  D A   N O N   I M M E T T E R E
018600060921    2C     CCP(X)        IFEQ      ' '
018700060921     C     CAP(X)        ANDNE     ' '
018800060921     C                   SETON                                        90
018900060921    2C                   ENDIF
019000060921     C*
019100060921     C* CARATTERE  N U M E R I C O
019200060921    2C     CCP(X)        IFEQ      'N'
019300060921     C     CCP(X)        OREQ      'A'
019400060921     C     CCP(X)        OREQ      'O'
019500060921     C     CCP(X)        OREQ      '0'
019600060921     C     CAP(X)        LOOKUP    NUM                                    30
019700060921     C*
019800060921     C* 30 OFF- NON TROVATO: PER "N" ERRORE SUBITO
019900060921     C  N30              SETON                                        90
020000060921     C*
020100060921     C* 30 OFF- NON TROVATO: PER "0" ERRORE SE NON E' NEMMENO ' '
020200060921    3C     *IN90         IFEQ      *ON
020300060921     C     CCP(X)        ANDEQ     '0'
020400060921     C     CAP(X)        ANDEQ     ' '
020500060921     C                   SETOFF                                       90
020600060921    3C                   ENDIF
020700060921    2C                   ENDIF
020800060921     C*
020900060921     C* CARATTERE  A L F A N U M E R I C O
021000060921    2C     CCP(X)        IFEQ      'L'
021100060921     C     CCP(X)        OREQ      'A'
021200060921     C     *IN90         ANDEQ     *ON
021300060921     C     CCP(X)        OREQ      'O'
021400060921     C     *IN90         ANDEQ     *ON
021500060921     C     CCP(X)        OREQ      'Q'
021600060921     C     CAP(X)        LOOKUP    LET                                    30
021700060921     C*---
021800060921     C* 30 OFF- NON TROVATO: PER "L" ERRORE SUBITO
021900060921     C* 30 OFF- NON TROVATO: PER "A" ERRORE SUBITO
022000060921     C*                      XCHE' NON TROVATO NEMMENO NEI NUMERICI
022100060921     C*---
022200060921     C  N30              SETON                                        90
022300060921     C   30              SETOFF                                       90
022400060921     C*
022500060921     C* 30 OFF- NON TROVATO: PER "O" E "Q" ERRORE SE NON E' NEMMENO ' '
022600060921    3C     *IN90         IFEQ      *ON
022700060921     C     CAP(X)        ANDEQ     ' '
022800060921     C*
022900060921    4C     CCP(X)        IFEQ      'O'
023000060921     C     CCP(X)        OREQ      'Q'
023100060921     C                   SETOFF                                       90
023200060921    4C                   ENDIF
023300060921    3C                   ENDIF
023400060921    2C                   ENDIF
023500060921     C*
023600060921    2C     *IN90         IFEQ      *ON
023700060921     C                   Z-ADD     10            X
023800061030     C                   MOVEL     MSG(1)        o97msg                         *CAP FORMALMENTE ERR
023900060921     c                   movel     'E'           o97err
024000060921    2C                   ENDIF
024100060921     C                   ADD       1             X                 2 0
024200060921    1C                   ENDDO
024300060921     c
024400060921     c                   endif
024500060921     C*
024600060921     C                   ENDSR
024700060921     C*------------------------------------------------------------------------*
024800060921     C* CHKNAR - CONTROLLA NAZIONE
024900060921     C*------------------------------------------------------------------------*
025000060921     C     CHKNAR        BEGSR
025100060921     C*
025200060921     C* CONTROLLO ESISTENZA NAZIONE SU TABELLA 15
025300060921     C                   MOVEL     '15'          KTBCOD
025400060921     C                   MOVEL(P)  I95NAR        KTBKEY
025500060921     C     KEYTAB        CHAIN     TABEL00F                           99
025600060921     C     *IN99         IFEQ      '1'
025700060921     C     TBLFLG        ORNE      *BLANKS
025800060921     C                   MOVEL     MSG(2)        o97msg                         *NAZIONE INESISTENTE
025900060921     c                   movel     'E'           o97err
026000060921     C                   ELSE
026100060921     C                   MOVEL     TBLUNI        DS15
026200060921     C                   ENDIF
026300060921     C*
026400060921     C                   ENDSR
026500060921     C*------------------------------------------------------------------------*
026600060921**
026700060921TISI97R- CAP formalmente errato.                                             2 2
026800060921TISI97R- Nazione inesistente.                                                1 1
026900060921**
027000060921ABCDEFGHIJKLMNOPQRSTUVWXYZOAUEEI
027100060921**
027200060921abcdefghijklmnopqrstuvwxyzòàùèéì
027300060921**
0274000609210123456789
