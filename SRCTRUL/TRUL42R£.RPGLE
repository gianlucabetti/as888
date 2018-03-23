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
003000960201      *  Controllo Prefisso
003100960201     C                   EXSR      CTRPRE
003200960201      *  Controllo Numero FAX
003300960201     C                   EXSR      CTRFAX
003400960201      *  Fine Lavoro
003500960201     C     FINE          TAG
003600960201     C                   SETON                                        RT
003700960201      *--------------------------------------------------------------*
003800960201      *  CTRPRE : Controllo prefisso                                 *
003900960201      *--------------------------------------------------------------*
004000960201     C     CTRPRE        BEGSR
004100960201      *
004200960201      *  Calcolo lunghezza prefisso
004300960131     C     '/'           SCAN      D42FAX        WLUNGH            2 0
004400960201     C     WLUNGH        SUB       1             WCHPRE            2 0          Lungh.prefisso
004500960131      *  Manca prefisso
004600960131     C     WCHPRE        IFLE      0
004700960131     C                   MOVEL     '1'           D42ERR
004800960201     C                   MOVEL     ERR(1)        D42MSG                         Manca prefisso
004900960131     C                   GOTO      FINE
005000960131     C                   END
005100960201      *  Prefisso troppo lungo
005200960201     C     WCHPRE        IFGT      8
005300960201     C                   MOVEL     '1'           D42ERR
005400960201     C                   MOVEL     ERR(2)        D42MSG                         troppo lungo
005500960201     C                   GOTO      FINE
005600960201     C                   END
005700960131      *  Estraggo prefisso da D42FAX
005800960201     C                   Z-ADD     1             X                 2 0
005900960201     C                   MOVEA     *BLANKS       KEY
006000960201     C                   MOVEA     D42FAX        SCH
006100960201     C     X             DOWLE     WCHPRE
006200960201     C     SCH(X)        ANDGE     '0'
006300960201     C     SCH(X)        ANDLE     '9'
006400960201     C                   MOVEL     SCH(X)        KEY(X)
006500960201     C                   ADD       1             X
006600960201     C                   END
006700960201     C*  Prefisso non numerico
006800960201     C     X             IFLE      WCHPRE
006900960201     C                   MOVEL     '1'           D42ERR
007000960201     C                   MOVEL     ERR(3)        D42MSG                         non numerico
007100960201     C                   GOTO      FINE
007200960201     C                   END
007300960131      *  Controllo se prefisso è valido (in tabella)
007400960131     C                   Z-ADD     1             KKUT
007500960131     C                   MOVEL     '4X'          KCOD
007600960131     C                   MOVEL     *BLANKS       KKEY
007700960201     C                   MOVEA     KEY           KKEY
007800960131     C     KTAB          CHAIN     TABEL00F                           30
007900960131      *  Prefisso non esistente in tabella
008000960131     C     *IN30         IFEQ      '1'
008100960131     C                   MOVEL     '1'           D42ERR
008200960201     C                   MOVEL     ERR(4)        D42MSG                         non esiste tab.
008300960131     C                   GOTO      FINE
008400960131     C                   END
008500960131      *  Prefisso annullato
008600960201     C     TBLFLG        IFNE      ' '
008700960131     C                   MOVEL     '1'           D42ERR
008800960201     C                   MOVEL     ERR(5)        D42MSG                         Annullato
008900960131     C                   GOTO      FINE
009000960131     C                   ELSE
009100960131     C                   MOVEL     TBLUNI        DS4X
009200960131     C                   END
009300960201      *
009400960201     C                   ENDSR
009500960201      *--------------------------------------------------------------*
009600960208      *  CTRFAX : Controllo numero Fax/Tel                           *
009700960201      *--------------------------------------------------------------*
009800960201     C     CTRFAX        BEGSR
009900960201      *
010000960208      *  estraggo la sottostringa per determinare il numero fax/tel
010100960131      *  della lunghezza 16 - (chr. prefisso + barra)
010200960131      *  dalla posizione prefisso + barra + 1
010300960201     C     WLUNGH        ADD       1             WP                2 0
010400960201     C     16            SUB       WLUNGH        WLUNG1            2 0
010500960201     C     WLUNG1        SUBST(P)  D42FAX:WP     WSTRIG           16
010600960131      *  sposto numero fax in schiera e retrocedo a ritroso
010700960131      *  fino a che non trovo un numero per determinare la lungh.
010800960201     C                   MOVEA     WSTRIG        SCH
010900960131     C                   Z-ADD     16            X
011000960131     C     X             DOWGT     0
011100960201     C     SCH(X)        ANDEQ     *BLANKS
011200960131     C                   SUB       1             X
011300960131     C                   END
011400960131      *  Manca numero fax
011500960201     C     X             IFEQ      0
011600960131     C                   MOVEL     '1'           D42ERR
011700960201     C                   MOVEL     ERR(6)        D42MSG                         Manca nr.fax
011800960131     C                   GOTO      FINE
011900960131     C                   END
012000960131      *  Se lunghezza nr.fax < chr previsti in tabella do errore
012100960208     C     X             IFLT      §4XCHR
012200960131     C                   MOVEL     '1'           D42ERR
012300960201     C                   MOVEL     ERR(7)        D42MSG                         Nr.fax corto
012400960131     C                   GOTO      FINE
012500960131     C                   END
012600960201      *  Controllo se numerico
012700960201     C                   Z-ADD     1             Y                 3 0
012800960201     C     Y             DOWLE     X
012900960201     C     SCH(Y)        ANDGE     '0'
013000960201     C     SCH(Y)        ANDLE     '9'
013100960201     C                   ADD       1             Y
013200960201     C                   END
013300960201     C*  FAX non numerico
013400960201     C     Y             IFLE      X
013500960201     C                   MOVEL     '1'           D42ERR
013600960201     C                   MOVEL     ERR(8)        D42MSG                         non numerico
013700960201     C                   GOTO      FINE
013800960201     C                   END
013900960201      *
014000960201     C                   ENDSR
014100960201      *--------------------------------------------------------------*
014200960201      *  *INZSR : Operazioni iniziali                                *
014300960201      *--------------------------------------------------------------*
014400960201     C     *INZSR        BEGSR
014500960201      *
014600960201      * Definisco chiave per lettura TABEL
014700960201     C     KTAB          KLIST
014800960201     C                   KFLD                    KKUT
014900960201     C                   KFLD                    KCOD
015000960201     C                   KFLD                    KKEY
015100960201      *--------------------------------------------------------------*
015200960201      * Definisco variabili
015300960201     C     *LIKE         DEFINE    TBLKUT        KKUT
015400960201     C     *LIKE         DEFINE    TBLKEY        KKEY
015500960201     C     *LIKE         DEFINE    TBLCOD        KCOD
015600960201      *--------------------------------------------------------------*
015700960201     C                   ENDSR
015800960208**
015900960208Il prefisso non è corretto o manca (deve essere : prefisso/numero)
016000960208Il numero del prefisso è superiore a 8 caratteri
016100960208Numero del prefisso alfanumerico
016200960208Numero del prefisso non esiste
016300960208Prefisso annullato
016400960208Non è stato immesso il numero del telefono o del fax
016500960208I numeri immessi sono inferiori a quelli previsti dal prefisso
016600960208Il numero contiene dei caratteri non validi (non numerici)
