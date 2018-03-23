000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400060320      *  Descrizione   :  Stampa tariffe trazionisti
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700060320     Ftntl80D   CF   E             WORKSTN
000800060320     Fanfrn01L  IF   E           K DISK
000900060320     Ftabel00f  IF   E           K DISK
001000000000      *
001100960913     D* SCHIERE PER CARICAMENTO L1
001200940926     D*
001300000000     D KPJBA         E DS
001400060320     D ds3i          E DS
001500000000     D CNCR80        E DS
001600000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001700020307     D dsparam         DS
001800060320     D  datae                         8s 0
001900060320     D  traz1                         8  0
002000060320     D  socie                         3
002100090512     D  dataCerta                     8s 0
002200090115     D* DS "TRUL16R" - RICERCA ALFABETICA TRAZIONISTI
002300090115     D PARAM           DS                  INZ
002400090115     D* TIPOLOGIA T_=TRAZIONISTA
002500090115     D  PARTIP                 1      2
002600090115     D* RAGIONE SOCIALE TRAZIONISTA
002700090115     D  PARRSC                 3     37
002800090115     D* S.I. TRAZIONISTA
002900090115     D  PARCSF                38     40
003000090115     D* CODICE TRAZIONISTA
003100090115     D  PARPDR                41     47  0
003200090115     D* PARFLG = "3" --> NON ESISTONO TRAZIONISTI PER LA CHIAVE
003300090115     D*                  ALFABETICA RICHIESTA
003400090115     D  PARFLG                48     48
003500000000     D WLBDAT          DS
003600940926     D  G02DAT                 1      8  0
003700940926     D  G02INV                 9     16  0
003800940926     D  G02ERR                17     17
003900940926     D  G02TGI                18     22  0
004000060320      *__________________________________________________________________
004100000000     C     *ENTRY        PLIST
004200000000     C                   PARM                    KPJBA
004300060320     c     ktab          klist
004400060320     c                   kfld                    codut             1 0
004500060320     c                   kfld                    tab1              2
004600060320     c                   kfld                    tab2              8
004700060320     c                   z-add     1             codut
004800060320     c                   move      '3I'          tab1
004900060320     c                   movel(p)  '1'           tab2
005000060320     c     kfrn          klist
005100060320     c                   kfld                    socie
005200060320     c                   kfld                    fornit            8
005300060320     c                   move      *all'0'       fornit
005400060320      * ricerca in tabella società gestione traini
005500060320     c     ktab          chain     tabel00f
005600060320     c                   if        %found(tabel00f)
005700060320     c                   movel     tbluni        tab76            76
005800060320     c                   move      tab76         socie
005900060320     c                   endif
006000000000      *
006100000000      *--------------------------------------------------------------*
006200000000      *             M A I N      L I N E
006300000000      *--------------------------------------------------------------*
006400000000     C                   Z-ADD     1             CODUT
006500000000     C                   CALL      'X§PARUT'
006600000000     C                   PARM                    UT§DSE
006700000000     C                   MOVEL     RAGUT         RSUT             20
006800000000     C                   MOVEL     RAGUT         RSUT2            20
006900000000     C                   MOVEL     REC80         CNCR80
007000000000      *
007100000000      * Inizializzo i campi a video
007200000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
007300090512     C                   Z-ADD     *ZERO         VIDDTCERTA
007400020307     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
007500000000      *
007600000000      * Emetto il formato video
007700060324     c                   do        *hival
007800000000     C     EMETTO        TAG
007900020308     C                   SETOFF                                       90
008000060320     C                   EXFMT     tn80D01
008100000000      *
008200000000     C   KC              GOTO      FINE                                         F3=USCITA
008300090115     C   Kg              exsr      rictraz
008400000000      *
008500000000      * Eseguo i  controlli sui campi immessi a video
008600000000     C                   EXSR      CONTR
008700000000      *
008800020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008900020308     C  N90
009000020308     CanNkf              SETON                                          90      F6=CONFERMA
009100000000      *
009200000000      * Per errore generico riemetto il formato video
009300000000     C   90              GOTO      EMETTO
009400000000      *
009500020307      * Richiamo programma di stampa
009600020308     c                   if        *inkf
009700060320     c                   z-add     dalrib        datae
009800060320     c                   move      fornit        traz1
009900020307     c                   movel     dsparam       kpjbu
010000060320     C                   CALL      'TNTL81R'
010100000000     C                   PARM                    KPJBA
010200020308     c                   endif
010300060324     c                   enddo
010400000000      *
010500000000     C     FINE          TAG
010600000000     C                   SETON                                        LR
010700000000      *--------------------------------------------------------------*
010800000000      * Routine per i controlli sui campi del formato video          *
010900000000      *--------------------------------------------------------------*
011000000000     C     CONTR         BEGSR
011100000000      *
011200020308     c                   movea     '000000'      *in(41)
011300000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
011400020307     C     VIDDAL        ifeq      0
011500020308     C                   SETON                                        4390
011600000000     C                   GOTO      ENDCTL
011700000000     C                   END
011800000000      * Controllo data iniziale
011900000000     C     VIDDAL        IFGT      0
012000940926     C                   Z-ADD     VIDDAL        G02DAT
012100000000     C                   MOVEL     *BLANK        G02ERR
012200940926     C                   CALL      'XSRDA8'
012300000000     C                   PARM                    WLBDAT
012400000000     C     G02ERR        IFEQ      '1'
012500020308     C                   SETON                                        44  90
012600000000     C                   GOTO      ENDCTL
012700000000     C                   ENDIF
012800020307     C                   Z-ADD     G02INV        DALRIB            8 0
012900020308     C                   Z-ADD     G02dat        viddal
013000020307     c                   endif
013100090512      *
013200160420     C                   Z-ADD     0             dataCerta
013300160420     c                   goto      no_CTRL_DCERTA
013400090512     C     VIDDTCERTA    ifeq      0
013500090512     C                   SETON                                        4190
013600090512     C                   GOTO      ENDCTL
013700090512     C                   END
013800090512      *
013900090512     C     VIDDTCERTA    IFGT      0
014000090512     C                   Z-ADD     VIDDTCERTA    G02DAT
014100090512     C                   MOVEL     *BLANK        G02ERR
014200090512     C                   CALL      'XSRDA8'
014300090512     C                   PARM                    WLBDAT
014400090512     C     G02ERR        IFEQ      '1'
014500090512     C                   SETON                                        42  90
014600090512     C                   GOTO      ENDCTL
014700090512     C                   ENDIF
014800090512     C                   Z-ADD     G02INV        dataCerta
014900090512     C                   Z-ADD     G02dat        vidDtCerta
015000090512     c                   endif
015100160420     c     no_CTRL_DCERTAtag
015200060320      * trazionista
015300060320     c                   setoff                                       45
015400060320     C                   move      cdpa1v        fornit
015500060324     C                   IF        cdpa1v < 9999999 and cdpa1v > 0
015600060320     C     kfrn          chain     anfrn01l
015700060320     c                   if        %found(anfrn01l)
015800060320     C                   MOVEL     frndesbrev    cdpa1vd
015900060320     c                   else
016000060320     C                   SETON                                        45  90
016100060320     c                   endif
016200060321     c                   endif
016300000000      *
016400000000     C     ENDCTL        ENDSR
016500090115      *--------------------------------------------------------------*
016600090115      * Routine per ricerca codici trazionisti                       *
016700090115      *--------------------------------------------------------------*
016800090115     C     rictraz       BEGSR
016900090115     C*  deposita la KPJBU
017000090115     C                   MOVEL     KPJBU         DKPJBU          256
017100090115     C                   MOVEL     *blank        PARRSC
017200090115     C                   Z-ADD     *ZEROS        PARPDR
017300090115     C                   eval      partip = 'T '
017400090115     C                   clear                   parcsf
017500090115     C                   MOVEL     *BLANKS       PARFLG
017600090115     C                   MOVEL     PARAM         KPJBU
017700090115     C                   CALL      'TRUL16R'
017800090115     C                   PARM                    KPJBA
017900090115     C                   MOVEL     KPJBU         PARAM
018000090115IF  2C     PARFLG        IFEQ      '3'
018100090115     C                   MOVEL     *zeros        cdpa1v
018200090115X   2C                   ELSE
018300090115     C                   MOVEL     PARPDR        cdpa1v
018400090115     C                   MOVEL     PARRSC        cdpa1vd
018500090115E   2C                   ENDIF
018600090115     C* REIMPOSTA LA KPJBU
018700090115     C                   MOVEL     DKPJBU        KPJBU
018800090115     C                   ENDSR
