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
002100000000     D WLBDAT          DS
002200940926     D  G02DAT                 1      8  0
002300940926     D  G02INV                 9     16  0
002400940926     D  G02ERR                17     17
002500940926     D  G02TGI                18     22  0
002600060320      *__________________________________________________________________
002700000000     C     *ENTRY        PLIST
002800000000     C                   PARM                    KPJBA
002900060320     c     ktab          klist
003000060320     c                   kfld                    codut             1 0
003100060320     c                   kfld                    tab1              2
003200060320     c                   kfld                    tab2              8
003300060320     c                   z-add     1             codut
003400060320     c                   move      '3I'          tab1
003500060320     c                   movel(p)  '1'           tab2
003600060320     c     kfrn          klist
003700060320     c                   kfld                    socie
003800060320     c                   kfld                    fornit            8
003900060320     c                   move      *all'0'       fornit
004000060320      * ricerca in tabella società gestione traini
004100060320     c     ktab          chain     tabel00f
004200060320     c                   if        %found(tabel00f)
004300060320     c                   movel     tbluni        tab76            76
004400060320     c                   move      tab76         socie
004500060320     c                   endif
004600000000      *
004700000000      *--------------------------------------------------------------*
004800000000      *             M A I N      L I N E
004900000000      *--------------------------------------------------------------*
005000000000     C                   Z-ADD     1             CODUT
005100000000     C                   CALL      'X§PARUT'
005200000000     C                   PARM                    UT§DSE
005300000000     C                   MOVEL     RAGUT         RSUT             20
005400000000     C                   MOVEL     RAGUT         RSUT2            20
005500000000     C                   MOVEL     REC80         CNCR80
005600000000      *
005700000000      * Inizializzo i campi a video
005800000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
005900020307     C                   Z-ADD     0             CDPA1V                         1' COD.PADRONC.
006000000000      *
006100000000      * Emetto il formato video
006200060324     c                   do        *hival
006300000000     C     EMETTO        TAG
006400020308     C                   SETOFF                                       90
006500060320     C                   EXFMT     tn80D01
006600000000      *
006700000000     C   KC              GOTO      FINE                                         F3=USCITA
006800000000      *
006900000000      * Eseguo i  controlli sui campi immessi a video
007000000000     C                   EXSR      CONTR
007100000000      *
007200020308      * Se non vi sono errori ma non e' premuto F6 emetto video
007300020308     C  N90
007400020308     CanNkf              SETON                                          90      F6=CONFERMA
007500000000      *
007600000000      * Per errore generico riemetto il formato video
007700000000     C   90              GOTO      EMETTO
007800000000      *
007900020307      * Richiamo programma di stampa
008000020308     c                   if        *inkf
008100060320     c                   z-add     dalrib        datae
008200060320     c                   move      fornit        traz1
008300020307     c                   movel     dsparam       kpjbu
008400060320     C                   CALL      'TNTL81R'
008500000000     C                   PARM                    KPJBA
008600020308     c                   endif
008700060324     c                   enddo
008800000000      *
008900000000     C     FINE          TAG
009000000000     C                   SETON                                        LR
009100000000      *--------------------------------------------------------------*
009200000000      * Routine per i controlli sui campi del formato video          *
009300000000      *--------------------------------------------------------------*
009400000000     C     CONTR         BEGSR
009500000000      *
009600020308     c                   movea     '000000'      *in(41)
009700000000      * SE IMMESSE SCELTE IMMETTERE ANCHE DATA E VICEVERSA
009800020307     C     VIDDAL        ifeq      0
009900020308     C                   SETON                                        4390
010000000000     C                   GOTO      ENDCTL
010100000000     C                   END
010200000000      * Controllo data iniziale
010300000000     C     VIDDAL        IFGT      0
010400940926     C                   Z-ADD     VIDDAL        G02DAT
010500000000     C                   MOVEL     *BLANK        G02ERR
010600940926     C                   CALL      'XSRDA8'
010700000000     C                   PARM                    WLBDAT
010800000000     C     G02ERR        IFEQ      '1'
010900020308     C                   SETON                                        44  90
011000000000     C                   GOTO      ENDCTL
011100000000     C                   ENDIF
011200020307     C                   Z-ADD     G02INV        DALRIB            8 0
011300020308     C                   Z-ADD     G02dat        viddal
011400020307     c                   endif
011401080918     c* non permetto la stampa se data <= 31/8/2008 frutto della l.133
011402080918     c                   setoff                                       4490
011403080918     c                   if        dalrib < 20080901
011404080918     C                   SETON                                        44  90
011405080918     C                   GOTO      ENDCTL
011406080918     C                   ENDIF
011407080918     c* non permetto la stampa se data > 31/12/2008 frutto della l.133
011408080918     c                   setoff                                       4490
011409080918     c                   if        dalrib > 20081231
011410080918     C                   SETON                                        44  90
011411080918     C                   GOTO      ENDCTL
011412080918     C                   ENDIF
011500060320      * trazionista
011600060320     c                   setoff                                       45
011700060320     C                   move      cdpa1v        fornit
011800060324     C                   IF        cdpa1v < 9999999 and cdpa1v > 0
011900060320     C     kfrn          chain     anfrn01l
012000060320     c                   if        %found(anfrn01l)
012100060320     C                   MOVEL     frndesbrev    cdpa1vd
012200060320     c                   else
012300060320     C                   SETON                                        45  90
012400060320     c                   endif
012500060321     c                   endif
012600000000      *
012700000000     C     ENDCTL        ENDSR
