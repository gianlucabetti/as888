000100960131     H
000200960131     H*--------------------------------------------------------------*
000300960131     H* PGM. TRUL42R : Routine che controlla la validità del numero  *
000400960208     H*                Telefono/fax                                  *
000500960131     H*--------------------------------------------------------------*
000600960131     H*                PARAMETRI:                                    *
000700960208     H*                          D42FAX=Numero     da controllare    *
000800960131     H*                          D42ERR=ERRORE: 1=errore             *
000900960131     H*                          D42MSG=Se errore = 1 --> tipo err.  *
001000960131     H*--------------------------------------------------------------*
001100960131     FTABEL00F  IF   E           K DISK
001200960131     D*--------------------------------------------------------------*
001300960131     D* SCHIERE
001400960131     D*--------------------------------------------------------------*
001500960201     D ERR             S             70    DIM(8) CTDATA PERRCD(1)              Errori
001600960201     D SCH             S              1    DIM(16)                              Sch.appogg.
001700960201     D KEY             S              1    DIM(8)                               Key x posiz.
001800960131     D*--------------------------------------------------------------*
001900960131     D* DS
002000960131     D*--------------------------------------------------------------*
002100960201     D DS4X          E DS
002200960201     D TRUL42        E DS                  EXTNAME(TRUL42DS)
002300960131      *--------------------------------------------------------------*
002400960131      *  Ciclo Principale
002500960131      *--------------------------------------------------------------*
002600960201     C     *ENTRY        PLIST
002700960201     C                   PARM                    TRUL42
002800960201     C                   MOVEL     *BLANKS       D42ERR
002900960201     C                   MOVEL     *BLANKS       D42MSG
003000090424      *  Controllo Prefisso se presente barra non più obbligatoria
003100960201     C                   EXSR      CTRPRE
003200960201      *  Controllo Numero FAX
003300090424     c                   if        d42err=' '
003400960201     C                   EXSR      CTRFAX
003500090424     c                   endif
003600090424     c
003700960201      *  Fine Lavoro
003800960201     C     FINE          TAG
003900960201     C                   SETON                                        RT
004000960201      *--------------------------------------------------------------*
004100960201      *  CTRPRE : Controllo prefisso                                 *
004200960201      *--------------------------------------------------------------*
004300960201     C     CTRPRE        BEGSR
004400960201      *
004500960201      *  Calcolo lunghezza prefisso
004600960131     C     '/'           SCAN      D42FAX        WLUNGH            2 0
004700960201     C     WLUNGH        SUB       1             WCHPRE            2 0          Lungh.prefisso
004800090424      *  Manca la barra --> esco dalla routine
004900090424      *                 perchè non più obbligatoria
005000960131     C     WCHPRE        IFLE      0
005100090424     c                   goto      EndPRE
005200090424     c                   endif
005300090424     c
005400090424     C***                MOVEL     '1'           D42ERR
005500090424     C***                MOVEL     ERR(1)        D42MSG                         Manca prefisso
005600090424     C***                GOTO      FINE
005700090424     C***                END
005800090424     c
005900960201      *  Prefisso troppo lungo
006000960201     C     WCHPRE        IFGT      8
006100960201     C                   MOVEL     '1'           D42ERR
006200960201     C                   MOVEL     ERR(2)        D42MSG                         troppo lungo
006300090424     C                   GOTO      EndPRE
006400960201     C                   END
006500960131      *  Estraggo prefisso da D42FAX
006600960201     C                   Z-ADD     1             X                 2 0
006700960201     C                   MOVEA     *BLANKS       KEY
006800960201     C                   MOVEA     D42FAX        SCH
006900960201     C     X             DOWLE     WCHPRE
007000960201     C     SCH(X)        ANDGE     '0'
007100960201     C     SCH(X)        ANDLE     '9'
007200960201     C                   MOVEL     SCH(X)        KEY(X)
007300960201     C                   ADD       1             X
007400960201     C                   END
007500960201     C*  Prefisso non numerico
007600960201     C     X             IFLE      WCHPRE
007700960201     C                   MOVEL     '1'           D42ERR
007800960201     C                   MOVEL     ERR(3)        D42MSG                         non numerico
007900090424     C                   GOTO      EndPRE
008000960201     C                   END
008100090424     c* ES --> 24/04/09 non controlliamo più il prefisso nella tabella
008200090424     c*        4X perchè ne vengono sempre creati soprattutto per i cell.
008300090424     c                   goto      EndPRE
008400090424     c
008500090424     c
008600960131      *  Controllo se prefisso è valido (in tabella)
008700960131     C                   Z-ADD     1             KKUT
008800960131     C                   MOVEL     '4X'          KCOD
008900960131     C                   MOVEL     *BLANKS       KKEY
009000960201     C                   MOVEA     KEY           KKEY
009100960131     C     KTAB          CHAIN     TABEL00F                           30
009200960131      *  Prefisso non esistente in tabella
009300960131     C     *IN30         IFEQ      '1'
009400960131     C                   MOVEL     '1'           D42ERR
009500960201     C                   MOVEL     ERR(4)        D42MSG                         non esiste tab.
009600960131     C                   GOTO      FINE
009700960131     C                   END
009800960131      *  Prefisso annullato
009900960201     C     TBLFLG        IFNE      ' '
010000960131     C                   MOVEL     '1'           D42ERR
010100960201     C                   MOVEL     ERR(5)        D42MSG                         Annullato
010200960131     C                   GOTO      FINE
010300960131     C                   ELSE
010400960131     C                   MOVEL     TBLUNI        DS4X
010500960131     C                   END
010600960201      *
010700090424     C     EndPRE        ENDSR
010800960201      *--------------------------------------------------------------*
010900960208      *  CTRFAX : Controllo numero Fax/Tel                           *
011000960201      *--------------------------------------------------------------*
011100960201     C     CTRFAX        BEGSR
011200960201      *
011300960208      *  estraggo la sottostringa per determinare il numero fax/tel
011400960131      *  della lunghezza 16 - (chr. prefisso + barra)
011500960131      *  dalla posizione prefisso + barra + 1
011600090424     c                   clear                   WSTRIG
011700090424     c                   if        WLUNGH>0
011800960201     C     WLUNGH        ADD       1             WP                2 0
011900960201     C     16            SUB       WLUNGH        WLUNG1            2 0
012000960201     C     WLUNG1        SUBST(P)  D42FAX:WP     WSTRIG           16
012100960131      *  sposto numero fax in schiera e retrocedo a ritroso
012200960131      *  fino a che non trovo un numero per determinare la lungh.
012300960201     C                   MOVEA     WSTRIG        SCH
012400090424     c                   else
012500090424     c* se non c'e' la / metto tutto il numero nella schiera
012600090424     c                   movea     D42FAX        SCH
012700090424     c                   endif
012800090424     c
012900960131     C                   Z-ADD     16            X
013000960131     C     X             DOWGT     0
013100960201     C     SCH(X)        ANDEQ     *BLANKS
013200960131     C                   SUB       1             X
013300960131     C                   END
013400960131      *  Manca numero fax
013500960201     C     X             IFEQ      0
013600960131     C                   MOVEL     '1'           D42ERR
013700960201     C                   MOVEL     ERR(6)        D42MSG                         Manca nr.fax
013800960131     C                   GOTO      FINE
013900960131     C                   END
014000090424      *  Se lunghezza nr.fax < di 4 con / o 6 senza barra --> errore
014100090424     c                   if        (WLUNGH>0 and x<4)   or
014200090424     c                             (WLUNGh<=0 and x<6)
014300960131     C                   MOVEL     '1'           D42ERR
014400960201     C                   MOVEL     ERR(7)        D42MSG                         Nr.fax corto
014500960131     C                   GOTO      FINE
014600960131     C                   END
014700090424     c
014800960201      *  Controllo se numerico
014900960201     C                   Z-ADD     1             Y                 3 0
015000960201     C     Y             DOWLE     X
015100960201     C     SCH(Y)        ANDGE     '0'
015200960201     C     SCH(Y)        ANDLE     '9'
015300960201     C                   ADD       1             Y
015400960201     C                   END
015500960201     C*  FAX non numerico
015600960201     C     Y             IFLE      X
015700960201     C                   MOVEL     '1'           D42ERR
015800960201     C                   MOVEL     ERR(8)        D42MSG                         non numerico
015900960201     C                   GOTO      FINE
016000960201     C                   END
016100960201      *
016200960201     C                   ENDSR
016300960201      *--------------------------------------------------------------*
016400960201      *  *INZSR : Operazioni iniziali                                *
016500960201      *--------------------------------------------------------------*
016600960201     C     *INZSR        BEGSR
016700960201      *
016800960201      * Definisco chiave per lettura TABEL
016900960201     C     KTAB          KLIST
017000960201     C                   KFLD                    KKUT
017100960201     C                   KFLD                    KCOD
017200960201     C                   KFLD                    KKEY
017300960201      *--------------------------------------------------------------*
017400960201      * Definisco variabili
017500960201     C     *LIKE         DEFINE    TBLKUT        KKUT
017600960201     C     *LIKE         DEFINE    TBLKEY        KKEY
017700960201     C     *LIKE         DEFINE    TBLCOD        KCOD
017800960201      *--------------------------------------------------------------*
017900960201     C                   ENDSR
018000960208**
018100960208Il prefisso non è corretto o manca (deve essere : prefisso/numero)
018200960208Il numero del prefisso è superiore a 8 caratteri
018300960208Numero del prefisso alfanumerico
018400960208Numero del prefisso non esiste
018500960208Prefisso annullato
018600960208Non è stato immesso il numero del telefono o del fax
018700090424I numeri immessi sono troppo pochi.
018800960208Il numero contiene dei caratteri non validi (non numerici)
