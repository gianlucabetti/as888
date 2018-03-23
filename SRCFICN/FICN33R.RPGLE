000100130731     H DECEDIT('0,') DATEDIT(*ymd.)
000200000000      *****************************************************************
000300050506      *
000400020624      *  Descrizione   :  rielaborazione C.E. giornaliero
000500020624      *                   Programma di lancio
000600000000      *****************************************************************
000700020624     Fficn33D   CF   E             WORKSTN
000800000000      *
000900940926     D*
001000000000     D KPJBA         E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300020307     D dsparam         DS
001400020624     D  datadal                       8s 0
001500020624     D  dataal                        8s 0
001600151016     d  dist                          1    dim(10)
002600030403     d  vOvr                          1
002700050506     d  vfrig                         1
002800050506     d  vfudate                       8s 0
002900051123     d  vriep                         1
003000940926     D*
003100940926     D*
003200000000     D WLBDAT          DS
003300940926     D  G02DAT                 1      8  0
003400940926     D  G02INV                 9     16  0
003500940926     D  G02ERR                17     17
003600940926     D  G02TGI                18     22  0
003601130731     d Wdata8          DS
003602130731     d  dadata                 1      8  0
003603130731     d  adata                  9     16  0
003604130731     d  GioLav                17     21  0
003605130731      * - Parametri per pgm. XGIOSE1
003606130731     d Wggmmaa         s              6    inz
003607130731     d Wgioset         s              1    inz
003608130731      *
003609130731     d wData_6         s              6  0 inz
003610130731     d wData_Dmy       s               d   inz  datfmt(*dmy)
003611130731     d dataeur         s               d   inz  datfmt(*eur)
003612130731     d dataiso         s               d   inz  datfmt(*iso)
003613130731     d dataiso2        s               d   inz  datfmt(*iso)
003700000000      *
003800000000     C     *ENTRY        PLIST
003900000000     C                   PARM                    KPJBA
004000000000      *
004100000000      *--------------------------------------------------------------*
004200000000      *             M A I N      L I N E
004300000000      *--------------------------------------------------------------*
004400000000     C                   Z-ADD     1             CODUT
004500000000     C                   CALL      'X§PARUT'
004600000000     C                   PARM                    UT§DSE
004700000000     C                   MOVEL     RAGUT         RSUT             20
004800000000     C                   MOVEL     RAGUT         RSUT2            20
004900000000     C                   MOVEL     REC80         CNCR80
005000000000      *
005100130731      * Inizializzo i campi a video per profili EDP
005101130731     c                   if        %subst(knmus:1:3) = 'EDP'
005200020624     C                   Z-ADD     *ZERO         VIDal                          COD.FILIALE
005300000000     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
005400100204     c                   move      ' '           vdis1
005500100204     c                   move      ' '           vdis2
005600100204     c                   move      ' '           vdis3
005700100204     c                   move      ' '           vdis4
005800100204     c                   move      ' '           vdis5
005900100204     c                   move      ' '           vdis6
006000030403     c                   move      'S'           vOvr
006100050506     c                   move      *blank        vfrig
006200050506     C                   Z-ADD     *ZERO         Vfudate                        DATA INIZIALE
006300051123     c                   move      'N'           vriep
006301130731     c                   else
006303130731     c                   seton                                        21
006304130731     c                   move      *date         data8
006305130731     c                   move      01            data8             8 0
006306130731     c                   move      data8         dataiso
006307130731      *ricavo ultimo giorno mese precedente
006308130731     c     dataiso       subdur    1:*d          dataiso2
006309130731     c                   move      *date         dataiso
006310130731     c                   move      dataiso       dadata
006311130731     c                   move      dataiso2      adata
006312130731     c                   CALL      'XSRLAV8'
006313130731     c                   PARM                    Wdata8
006314130731      * se diverso da 3 vuol dire che non è il terzo giorno lavorativo dopo il fine mese
006315130731     c                   if        giolav <> 3
006316130802     c                   exfmt     v1
006317130731     c                   goto      fine
006318130731     c                   endif
006319130731     c                   move      dataiso2      dataeur
006320130731     c                   move      dataiso2      wdata_dmy
006321130731     c                   move      wData_Dmy     wData_6
006322130731     c                   move      wData_6       Wggmmaa
006323130731     c                   call      'XGIOSE1'
006324130731     c                   parm                    Wggmmaa           6
006325130731     c                   parm      *blanks       Wgioset           1
006326130731     c                   if        wgioset <> '6' and wgioset <> '7'
006327130802     c                   exfmt     v1
006328130731     c                   goto      fine
006329130731     c                   endif
006330130731     c                   move      dataeur       vidal
006331130731     c                   move      dataeur       viddal
006332130731     c                   movel     01            viddal
006333130731     c                   move      'S'           vOvr
006334130802     c                   move      'S'           vriep
006335130802     c                   move      *blank        vfrig
006336130802     C                   move      *zero         Vfudate
006337130731     c                   endif
006400000000      * Emetto il formato video
006500000000     C     EMETTO        TAG
006600020308     C                   SETOFF                                       90
006700020624     C                   EXFMT     cn33D01
006800000000      *
006900000000     C   KC              GOTO      FINE                                         F3=USCITA
007000000000      *
007100000000      * Eseguo i  controlli sui campi immessi a video
007200000000     C                   EXSR      CONTR
007300000000      *
007400020308      * Se non vi sono errori ma non e' premuto F6 emetto video
007500020308     C  N90
007600020308     CanNkf              SETON                                          90      F6=CONFERMA
007700000000      *
007800000000      * Per errore generico riemetto il formato video
007900000000     C   90              GOTO      EMETTO
008000000000      *
008100020307      * Richiamo programma di stampa
008200020308     c                   if        *inkf
008300020624     c                   z-add     dalribal      dataal
008400020624     c                   z-add     dalrib        datadal
008401151016     c                   move      vdis1         dist(1)
008402151016     c                   move      vdis2         dist(2)
008403151016     c                   move      vdis3         dist(3)
008404151016     c                   move      vdis4         dist(4)
008405151016     c                   move      vdis5         dist(5)
008406151016     c                   move      vdis6         dist(6)
008407151016     c                   move      vdis7         dist(7)
008408151016     c                   move      vdis8         dist(8)
008409151016     c                   move      vdis9         dist(9)
008410151016     c                   move      vdis10        dist(10)
008500020307     c                   movel     dsparam       kpjbu
008600020624      * richiama creazione saldi batch
008700020624     c                   move      'FN34'        kcoaz
008800050506     C                   CALL      'FICN34R'
008900050506     C*                  CALL      'BCH10'
009000000000     C                   PARM                    KPJBA
009100020624     C                   GOTO      FINE                                         F3=USCITA
009200020308     c                   endif
009300000000      *
009400000000     C     FINE          TAG
009500000000     C                   SETON                                        LR
009600000000      *--------------------------------------------------------------*
009700000000      * Routine per i controlli sui campi del formato video          *
009800000000      *--------------------------------------------------------------*
009900000000     C     CONTR         BEGSR
010000000000      *
010100020308     c                   movea     '000000'      *in(41)
010200050506      * se gestita forzatura per rigenerazione file controllo di gestione
010300050506      * mantengo valorizzati solo il flag e la forzatura dell'udate
010400050506      *
010500050506     c                   if        vfrig = 'S'
010600050506      * Controllo forzatura udate
010700050506     C     Vfudate       IFGT      0
010800050506     C                   Z-ADD     Vfudate       G02DAT
010900050506     C                   MOVEL     *BLANK        G02ERR
011000050506     C                   CALL      'XSRDA8'
011100050506     C                   PARM                    WLBDAT
011200050506     C     G02ERR        IFEQ      '1'
011300050506     C                   SETON                                        46  90
011400050506     C                   GOTO      ENDCTL
011500050506     C                   ENDIF
011600050506     C                   Z-ADD     G02dat        vfudate
011700050506     c                   endif
011800050506      * azzeramento altri parametri
011900050506     C                   Z-ADD     *ZERO         VIDal                          COD.FILIALE
012000050506     C                   Z-ADD     *ZERO         VIDDAL                         DATA INIZIALE
012100050506     c                   move      ' '           vdis1
012200050506     c                   move      ' '           vdis2
012300050506     c                   move      ' '           vdis3
012400050506     c                   move      ' '           vdis4
012500050506     c                   move      ' '           vdis5
012600050506     c                   move      ' '           vdis6
012700050506     c                   move      ' '           vdis7
012800050506     c                   move      ' '           vdis8
012900050506     c                   move      ' '           vdis9
013000050506     c                   move      ' '           vdis10
013100050506     c                   move      ' '           vOvr
013200050506     C                   Z-ADD     *ZERO         dalrib                         COD.FILIALE
013300050506     C                   Z-ADD     *ZERO         dalribal                       DATA INIZIALE
013400051123     c                   move      'N'           vriep
013500050506      * nel caso ci sia la forzatura dell'udate salto gli altri controlli
013600050506      * gli altri campi devono essere tutti azzerati
013700050506     C                   GOTO      ENDCTL
013800050506     c                   else
013900050506     C                   Z-ADD     *ZERO         Vfudate                        COD.FILIALE
014000050506     c                   end
014100000000      *
014200020624      * controllo immissione delle date
014300020307     C     VIDDAL        ifeq      0
014400020308     C                   SETON                                        4390
014500000000     C                   GOTO      ENDCTL
014600000000     C                   END
014700020624     C     VIDal         ifeq      0
014800020624     C                   SETON                                        4190
014900020624     C                   GOTO      ENDCTL
015000020624     C                   END
015100000000      * Controllo data iniziale
015200000000     C     VIDDAL        IFGT      0
015300940926     C                   Z-ADD     VIDDAL        G02DAT
015400000000     C                   MOVEL     *BLANK        G02ERR
015500940926     C                   CALL      'XSRDA8'
015600000000     C                   PARM                    WLBDAT
015700000000     C     G02ERR        IFEQ      '1'
015800020308     C                   SETON                                        44  90
015900000000     C                   GOTO      ENDCTL
016000000000     C                   ENDIF
016100020307     C                   Z-ADD     G02INV        DALRIB            8 0
016200020308     C                   Z-ADD     G02dat        viddal
016300020307     c                   endif
016400020624      * Controllo data finale
016500020624     C     VIDal         IFGT      0
016600020624     C                   Z-ADD     VIDal         G02DAT
016700020624     C                   MOVEL     *BLANK        G02ERR
016800020624     C                   CALL      'XSRDA8'
016900020624     C                   PARM                    WLBDAT
017000020624     C     G02ERR        IFEQ      '1'
017100020624     C                   SETON                                        42  90
017200020624     C                   GOTO      ENDCTL
017300020624     C                   ENDIF
017400020624     C                   Z-ADD     G02INV        DALRIBal          8 0
017500020624     C                   Z-ADD     G02dat        vidal
017600020624     c                   endif
017700020624     C     dalrib        ifgt      dalribal
017800020624     C                   SETON                                        4590
017900020624     C                   GOTO      ENDCTL
018000020624     C                   END
018100000000      *
018200000000     C     ENDCTL        ENDSR
