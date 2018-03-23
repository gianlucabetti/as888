000100981008      *-------------------------------------------------------------------------*
000200981106      *    ELENCO C.A. IN LIQUIDAZIONE TRANSATTIVA DA CONTABILIZZARE            *
000300981008      *-------------------------------------------------------------------------*
000400980721
000500980721      ****************************************************************
000600981026      *  FASI C.A. FISSATE A PROGRAMMA
000700981201      *_______________________________________________________________
000800981106      * 900 - Chiusura CA    (WCHCA)
000900980721      ***************************************************************
001000981201      *  CAUSALE CHIUSURA  FISSATA A PROGRAMMA
001100981201      *_______________________________________________________________
001200981202      * 05  - Chiusura dopo contabilizzazione ca     wcch
001300981201      ***************************************************************
001400980623
001500980521      ****************************************************************
001600980521      *  RIEPILOGO INDICATORI
001700980521      ***************************************************************
001800980904      * 30 - COMODO
001900980904      * 31 - COMODO
002000980904      ***************************************************************
002100980623
002200980521     H DECEDIT('0,') DATEDIT(*DMY.)
002300980623
002400981027     FFIDN00D   CF   E             WORKSTN  sfile(FI00S02:nrr1)
002500981027     FFNDCT01L  UF   E           K DISK
002600981106     FFNDCD01L  UF   E           K DISK
002700080626     FFNDCL01L  UF A E           K DISK
002800980929     FFNDCF01L  UF A E           K DISK
002900980521      *------------------------------------------------------------------------*
003000981106     D WCHCA           s                   like(dctfca)
003100981106     D                                     inz(900)
003200981201     d wcch            s                   like(dctcch)
003300981201     d                                     inz('05')
003400981106     D W0040           S              4  0
003500980721     D W0140           S             14  0
003600980521     D Wdtgio          S              8  0
003700980521     D dateu           S              8  0
003800980529     D nrr1            S              4  0
003900980623     D nrr2            S              4  0
004000980929     D WORA            S              4  0
004100981106     d wtcksm          S                   like(dctksc)
004200981106     d wtcksd          S                   like(dctksc)
004300981027
004400980521      *   D S   I N T E R N E / E S T E R N E
004500980521     D WLBDAT          DS                  INZ
004600980521     D  G02DAT                 1      8  0
004700980521     D  G02INV                 9     16  0
004800980521     D  G02ERR                17     17
004900980521     D  G02TGI                18     22  0
005000980525      *
005100980929     D FIDN00DS      E DS
005200981230     D DDCT01        E DS
005300980521     D KPJBA         E DS
005400981106     D UT§DSE0F      E DS
005500981106     d  tcu                  398    697
005600981106     D                                     DIM(50)
005700981106     d  kcu                  698    847P 0
005800981106     D                                     DIM(50)
005900981106     D                                     PACKEVEN
006000981106      *--------------------------------------------
006100980930      *
006200980521     D                SDS
006300980521     D  VTCPGM                 1     10
006400980623      *------------------------------------------------------------------------*
006500980521      *---------------  C A L C O L O  ----------------------------------------*
006600980521      *------------------------------------------------------------------------*
006700980623
006800981026      * Aggiorno testata C.A. e file fasi
006900981027     c                   IF        i00nce = 1
007000081219      * singola CA
007100981106     C     KDCT00        CHAIN     FNDCT000                           30
007200981230     c*
007300981230     c                   movel     dctflo        ddct01
007400981230     c*
007500081219     c  n30              exsr      SUB_SINGOLA
007600981106      *
007700981027     c                   ELSE
007800981106      * Stampo tutte le CA selezionate
007900981027     c                   exsr      CA_multi
008000981027     c                   ENDIF
008100981026
008200980907     c                   EVAL      *INLR = *ON
008300980910      *****************************************************************
008400081219      * Routine  singola CA ed aggiornamento dei files
008500980910      *****************************************************************
008600081219     C     SUB_SINGOLA   BEGSR
008700981106
008800081219      * Aggiorno file importi da liquidare
008900081219     c                   EXSR      UPDDCL
009000081219
009100981106      * Imposto campi per trasmissione dati  e chiusura C.A.
009200981027     c                   EXSR      RIEfndct
009300981027
009400981106      * Aggiorno testata C.A.
009500981106     c                   EXCEPT    UPDDCT
009600981106
009700981106      * Imposto e Aggiorno dettaglio C.A. come da testata CA
009800981106     c                   EXSR      UPDDCD
009900981027
010000981027      * Scrivo fasi C.A.
010100981027     c                   EXSR      WRTDCF
010200981027
010300981027     c                   ENDSR
010400981027      *****************************************************************
010500981027      * AGGIORNO FILES LEGGENDO SUBFILE
010600981027      *****************************************************************
010700981027     C     CA_multi      BEGSR
010800981027
010900981026      * Leggo  subfile
011000980929     c                   z-add     1             nrr2
011100981027     c     NRR2          CHAIN     FI00S02                            31
011200981026      *
011300981027      * Elaboro se C.A. non esclusa e scelta non bloccata
011400981027     c     *IN31         DOWEQ     *OFF
011500981027     c                   IF        V2Csce <> 'E'  and  V2Hi82 <> '1'
011600981027      *
011700981027     C     KDCT          CHAIN     FNDCT000                           30
011800981027      *
011900081219      * aggiorno i dati della singola CA
012000081219     c                   EXSR      SUB_SINGOLA
012100981027      *
012200981027     c                   ENDIF
012300981027      *
012400980929     c                   add       1             nrr2
012500981027     c     NRR2          CHAIN     FI00S02                            31
012600980929     c                   ENDDO
012700980929      *
012800980910     c                   ENDSR
012900081219      *****************************************************************
013000081219      * AGGIORNO FNDCL Importi da loiquidare CA
013100081219      *****************************************************************
013200081219     C     UPDDCL        BEGSR
013300081219     c*
013400981106      * recupero l'importo liquidato e trattenuto
013500981106     c* aggancio il file liquidazione ca
013600981106     c     kdcd          chain     fndcl01l                           30
013700080626      * se non lo trovo lo valorizzo
013800080626     c                   if        not %Found(fndcl01l)
013900080626     c                   clear                   fndcl000
014000080626     c                   movel     dctaac        Dclaac
014100080626     c                   movel     dctfil        Dclfil
014200080626     c                   z-add     dctnca        Dclnca
014300080626     c* importo concordato
014400080626     c                   z-add     dctipv        dclipr
014500080626     c                   movel     dctvpv        dclvpr
014600080626     c                   z-add     dctipv        dclipl
014700080626     c                   movel     dctvpv        dclvpl
014800080626     c                   clear                   dclftr
014900080626     c                   write     fndcl000
015000080626     c                   else
015100080626     c                   if        dclipr = 0
015200080626     c                   z-add     dctipv        dclipr
015300080626     c                   movel     dctvpv        dclvpr
015400080626     c                   endif
015500080626     c                   if        dclipl = 0
015600080626     c                   z-add     dclipr        dclipl
015700080626     c                   movel     dclvpr        dclvpl
015800080626     c                   endif
015900080626
016000080626     c                   update    fndcl000
016100080626     c                   endif
016200081219
016300981106     C                   endsr
016400981027      *****************************************************************
016500981027      * IMPOSTO CAMPI DI FNDCT
016600981027      *****************************************************************
016700981027     C     RIEfndct      BEGSR
016800981027      *
016900981201     c                   movel     wcch          DCTcch
017000981106     c                   z-add     dateu         DCTdch
017100981106     c                   z-add     WCHCA         DCTfca
017200981222     c                   clear                   DCTft1
017300981222     c                   clear                   DCTft2
017400981027      *
017500981027     c                   ENDSR
017600981106      *****************************************************************
017700981106      * AGGIORNO FNDCD Dettaglio C.A.
017800981106      *****************************************************************
017900981106     C     UPDDCD        BEGSR
018000981106     c*
018100981106     c     kdcd          setll     fndcd01l
018200981106     c     kdcd          reade     fndcd01l                               31
018300981106     c*
018400981106      * Imposto campi per trasmissione dati e pulisco i dati della chiusura C A
018500981106     c                   dow       *in31 = *off
018600981201     c                   eval      DCdcch = dctcch
018700981106     c                   eval      DCddch = dctdch
018800981222     c                   eval      DCdftr = dctft1
018900981106     c                   update    fndcd000
019000981106     c*
019100981106     c     kdcd          reade     fndcd01l                               31
019200981106     c*
019300981106     c                   enddo
019400981106     c*
019500981106     c                   endsr
019600981027      *****************************************************************
019700981106      * AGGIORNO FNDCF FASI C.A.  sia la fase che eseguo + la chiusura
019800981027      *****************************************************************
019900981027     C     WRTDCF        BEGSR
020000981027      *
020100980929     C                   z-add     DCTaaC        DCFaac
020200980929     C                   z-add     DCTfil        DCFfil
020300980929     C                   z-add     DCTnca        DCFnca
020400981106     c                   z-add     i00fca        DCFfca
020500980929     c                   z-add     dateu         DCFdfc
020600980929     c                   z-add     wora          DCFhfc
020700981027     c                   z-add     I00fgs        DCFfev
020800980929     c                   movel     knmus         DCFpru
020900981211     c                   movel     dctptr        DCFptr
021000980929      *
021100980929     c                   WRITE     FNDCF000
021200981106      * chiusura C.A.
021300981106     c                   z-add     wchca         DCFfca
021400981106     c                   WRITE     FNDCF000
021500980929      *
021600980929     c                   ENDSR
021700980910      *****************************************************************
021800980527      * ROUTINE INIZIALE
021900980527      *****************************************************************
022000980527     C     *INZSR        BEGSR
022100980615      *
022200980521     C     *ENTRY        PLIST
022300980521     C                   PARM                    KPJBA
022400981026      *
022500981026     C                   MOVEL     KPJBU         FIDN00DS
022600981106      *
022700980721      * reperisco data e ora
022800980721     C                   TIME                    W0140
022900980521      * UDATE IN GGMMAAAA
023000980521     C                   MOVE      W0140         WDTGIO
023100980929     C                   MOVEL     W0140         WORA
023200980521      * UDATE IN AAAAMMGG
023300980521     C                   Z-ADD     WDTGIO        G02DAT
023400980521     C                   MOVEL     *BLANK        G02ERR
023500980521     C                   CALL      'XSRDA8'
023600980521     C                   PARM                    WLBDAT
023700980521     C                   MOVEL     G02INV        DATEU
023800981027      *
023900981026     C     KDCT00        KLIST
024000981026     C                   KFLD                    I00AAC
024100981026     C                   KFLD                    I00FIL
024200981026     C                   KFLD                    I00NCA
024300980522      *
024400980929     C     KDCT          KLIST
024500980929     C                   KFLD                    V2HAAC
024600980929     C                   KFLD                    V2CFIL
024700980929     C                   KFLD                    V2CNCA
024800981106      *
024900981106     C     KDCD          KLIST
025000981106     C                   KFLD                    DCTAAC
025100981106     C                   KFLD                    DCTFIL
025200981106     C                   KFLD                    DCTNCA
025300980521     C                   ENDSR
025400980521      *---------------------------------------------------------------------------------------------
025500981027     OFNDCT000  E            UPDDCT
025600981106     O                       DCTdch
025700981202     O                       DCTcch
025800980929     O                       DCTfca
025900981222     O                       DCTft1
026000981222     O                       DCTft2
