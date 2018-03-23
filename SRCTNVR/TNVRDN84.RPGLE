000100020913      *--------------------------------------------------------------------------------------------*
000200020913      * Trasmissione C.A Da P.O a P.O. a SEDE
000300020913      *--------------------------------------------------------------------------------------------*
000400020920      * Se non riesco ad allocare un membro di una filiale
000500020920      * non trasmetto a quella filiale e non aggiorno la trasmissione
000600020920      * così andrà via con la trasmissione successiva
000700020913      *--------------------------------------------------------------------------------------------*
000800980828     H DECEDIT('0,') DATEDIT(*DMY.)
000900980929      ****************************************************************
001000980929      *  RIEPILOGO INDICATORI
001100980929      ***************************************************************
001200020920      * 21   - Comodo
001300980929      * 30   - CHAIN FNARB/FNBLP
001400981223      * 81   - chain FNLBL02   Se spento Bolla Mamma
001500981223      * 82   - chain FNLBL01   Se spento Bolla Figlia
001600980929      * 90   - Lettura Testata C.A.
001700980930      * 91   - Mancata allocazione file
001800021004      * 92   - P.O. locale (stesso AS400)
001900021004      * 93   - P.O. locale (stesso AS400) utilizzato x flag calcolo responsabilità
002000021014      * 94   - P.O. locale (stesso S.I.)
002100980929      * 98   - Lettura File legati alle C.A.
002200020913      *--------------------------------------------------------------------------------------------*
002300020913      * Data base
002400020913      *--------------------------------------------------------------------------------------------*
002500981019     Ffndcl01l  UF   E           K DISK
002600980831     Ffndcf01l  UF   E           K DISK
002700980831     Ffndka01l  UF   E           K DISK
002800980831     Ffndcd01l  UF   E           K DISK
002900021015     Ffndct06l  IF   E           K DISK
003000020913      *--------------------------------------------------------------------------------------------*
003100020913      * Data structure
003200020913      *--------------------------------------------------------------------------------------------*
003300020913      *----------------
003400020913      * Schiere
003500020913      *----------------
003600020913     d $RecDCT         s              7  0 dim(250)                             *num rec FNDCT
003700020913     d $RecDCD         s              7  0 dim(250)                             *num rec FNDCD
003800020913     d $RecDKA         s              7  0 dim(250)                             *num rec FNDKA
003900020913     d $RecDCF         s              7  0 dim(250)                             *num rec FNDCF
004000020913     d $RecDCC         s              7  0 dim(250)                             *num rec FNDCC
004100020913     d $RecDCL         s              7  0 dim(250)                             *num rec FNDCL
004200020913     D SKok            s              1    dim(250) inz(*all'0')
004300021014      * schiera dei P.O. presenti sull'AS/400
004400021014     d skiPOA          s              3  0 dim(250) inz
004500021014      * schiera dei P.O. presenti nel Sistema Informativo
004600021014     d skiPOS          s              3  0 dim(250) inz
004700020913      *----------------
004800020913      * Ds
004900020913      *----------------
005000020913     D KPJBA         E DS                                                       *architettura
005100020912     D DSCMZ         E DS                                                       *ds per "TIBS53C"
005200020913     D UT§DSE0F      E DS
005300020913     D CNCR80        E DS
005400020912     D TIBS55DS      E DS                                                       *reper.librerie job
005500020913     d TIBS56DS      e ds
005600020913     d  ski                    7    756  0 dim(250)
005700020913     d TRUL26DS      e ds
005800020913     d  skpo                  11    760  0 dim(250)
005900020913     d  skpot                761   1510  0 dim(250)
006000020925     d FNLV55ds      e ds                  inz
006100020925     d  D55tpt       e                     inz('P')
006200020913      *
006300020913     D wlbda8          DS                  INZ                                  *ds "XSRDA8"
006400020913     D  g08dat                        8  0                                       -data g/m/aa
006500020913     D  g08inv                        8  0                                       -data aa/m/g
006600020913     D  g08err                        1                                          -errore
006700020913     D  g08tgi                        5  0                                       -n°giorni data
006800020913      *
006900020913     D DSmembro        ds             7    inz
007000020913     D  MbrM                   1      1    inz('M')
007100020913     D  MbrPOP                 2      4  0 inz
007200020913     D  MbrPOA                 5      7  0 inz
007300020913      *----------------
007400020913      * Variabili riferite al data base
007500020913      *----------------
007600020913     d SAVfil          S                   LIKE(DCTfl1)                         *fil. trasmissione
007700020920     d wPOT            S                   LIKE(DCTfl1)                         *fil. trasmissione
007800020913     d wFLx            S                   LIKE(DCTfl1)                         *fil. trasmissione
007900021014     d wDTx            S                   LIKE(DCTdt1)                         *fil. trasmissione
008000020913     d SVaas           S                   LIKE(dctaas)                         *anno     spedizione
008100020913     d SVlnp           S                   LIKE(dctlnp)                         *l.parten.spedizione
008200020913     d SVnrs           S                   LIKE(dctnrs)                         *n.serie  spedizione
008300020913     d SVnsp           S                   LIKE(dctnsp)                         *numero   spedizione
008400020913     d wDCTfil         s                   like(DCTfil) inz
008500020920     d TPOapCA         s                   like(DCTfil) inz
008600020913      *----------------
008700020913      * Variabili a programma
008800020913      *----------------
008900020913     D Giro            S              1  0 inz                                  *giro trasmissione
009000020920     D $ARBmd          s              1    inz(*off)
009100020920     D XF              S              3  0 inz                                  *indice
009200020920     D XT              S              3  0 inz                                  *indice
009300020920     D XX              S              3  0 inz                                  *indice
009400980828     D wn14            S             14  0                                      *numerico 14
009500980828     D wn8             S              8  0                                      *numerico 8
009600020913     D ora6            S              6  0                                      *ora corrente di 6
009700020913     D DatCor          S              8  0                                      *d.corrente aa/m/g
009800020913     d ChiSono         s              1                                         * A=ARR P=PART
009900020913     d Legame          s              1                                         * bolla con legami
010000020913      *----------------
010100020913      * Costanti
010200020913      *----------------
010300020913     D $FileDCT        c                   const('FNDCT00T  ')
010400020913     D $FileDCD        c                   const('FNDCD00T  ')
010500020913     D $FileDKA        c                   const('FNDKA00T  ')
010600020913     D $FileDCF        c                   const('FNDCF00T  ')
010700020913     D $FileDCC        c                   const('FNDCC00T  ')
010800020913     D $FileDCL        c                   const('FNDCL00T  ')
010900020913     D $FileArbM       c                   const('FNARBM0T  ')
011000020913     D $FileArbD       c                   const('FNARBD0T  ')
011100020913     D $FileDCTr       c                   const('FNDCT00R  ')
011200020913     D $FileDCDr       c                   const('FNDCD00R  ')
011300020913     D $FileDKAr       c                   const('FNDKA00R  ')
011400020913     D $FileDCFr       c                   const('FNDCF00R  ')
011500020913     D $FileDCCr       c                   const('FNDCC00R  ')
011600020913     D $FileDCLr       c                   const('FNDCL00R  ')
011700020913      *
011800020913      *--------------------------------------------------------------------------------------------*
011900020913      * Main lines
012000020913      *--------------------------------------------------------------------------------------------*
012100021014      *
012200020913     c     ' '           setll     FNDCT06L
012300020913     c     ' '           reade     FNDCT06L                               90
012400020913      *
012500021014do  1c                   dow       not *in90
012600020913      *
012700020913      * leggo i records legati a questa C.A.
012800020913     c                   exsr      LETDET
012900020913     c     ' '           reade     FNDCT06L                               90
013000021014      *
013100021014e   1c                   enddo                                                  *fine file fisico
013200020913      *
013300021014     C                   movel     *on           *INLR
013400020913      *--------------------------------------------------------------------------------------------*
013500020913      * LETDET - Scrittura archivi legati alla C.A. da trasmettere
013600020913      *--------------------------------------------------------------------------------------------*
013700020913     C     LETDET        BEGSR
013800020913      *
013900020913      * leggo i records di dettaglio C.A.
014000020913     c     keynca        setll     FNDCD01L
014100020913     c     keynca        reade     FNDCD01L                               98
014200020913do  1c                   DOW       NOT *in98                                    *finchè fine file
014300021014     c                   clear                   DCDftr                         *flag trasmissione
014400021014     c                   clear                   DCDdtr                         *data trasmissione
014500980911     C                   EXCEPT    upddcd
014600020913     c     keynca        reade     FNDCD01L                               98
014700020913e   1c                   ENDDO                                                  *fine file fisico
014800020913      *
014900020913      * leggo i records dell'anagrafico clienti danni
015000020913     c     keynca        setll     FNDKA01L
015100020913     c     keynca        reade     FNDKA01L                               98
015200020913do  1c                   DOW       NOT *in98                                    *finchè fine file
015300020913     c                   movel     'T'           DKAftr                         *flag trasmissione
015400020913     c                   z-add     DatCor        DKAdtr                         *data trasmissione
015500980911     C                   EXCEPT    upddka
015600020913      *
015700020913     c     keynca        reade     FNDKA01L                               98
015800020913e   1c                   ENDDO                                                  *fine file fisico
015900020913      *
016000020913      * leggo i records Storico Fasi  C.A.
016100020913     c     keynca        setll     FNDCF01L
016200020913     c     keynca        reade     FNDCF01L                               98
016300020913do  1c                   DOW       NOT *in98                                    *finchè fine file
016400020913     c                   movel     'T'           DCFftr                         *flag trasmissione
016500020913     c                   z-add     DatCor        DCFdtr                         *data trasmissione
016600980911     C                   EXCEPT    upddcf
016700020913     c     keynca        reade     FNDCF01L                               98
016800020913e   1c                   ENDDO                                                  *fine file fisico
016900020913      *
017000020913      * leggo i records DAti relativi alla liquidazione per C. A.
017100020913     c     keynca        chain     FNDCL01L                           98
017200020913if  1c                   if        NOT *in98                                    *se trovato record
017300020913     c                   movel     'T'           DCLftr                         *flag trasmissione
017400020913     c                   z-add     DatCor        DCLdtr                         *data trasmissione
017500981021     C                   EXCEPT    upddcl
017600020913e   1C                   endif                                                  *fine file fisico
017700980911      *
017800020913     C                   ENDSR
017900020913      *--------------------------------------------------------------------------------------------*
018000020913      * *inzsr - operazioni iniziali
018100020913      *--------------------------------------------------------------------------------------------*
018200980828     C     *inzsr        BEGSR
018300020913      *
018400020913      * chiavi di lettura
018500981021     C     keynca        KLIST                                                  *fndcd00f
018600980911     C                   KFLD                    dctaac                          -anno C.A.
018700980911     C                   KFLD                    dctfil                          -P.O. C.A.
018800980911     C                   KFLD                    dctnca                          -numero C.A.
018900990203      * spedizione
019000990203     c     kbol01        klist
019100990203     c                   kfld                    dctaas
019200990203     c                   kfld                    dctlnp
019300990203     c                   kfld                    dctnrs
019400990203     c                   kfld                    dctnsp
019500990203      * spedizione save
019600990203     c     kbolsv        klist
019700990203     c                   kfld                    svaas
019800990203     c                   kfld                    svlnp
019900990203     c                   kfld                    svnrs
020000990203     c                   kfld                    svnsp
020100981008      * legame bolla spedizione
020200981008     c     klbl          klist
020300981008     c                   kfld                    dctaas
020400981008     c                   kfld                    dctlnp
020500981008     c                   kfld                    dctnrs
020600981008     c                   kfld                    dctnsp
020700981222      * legame bolla spedizione
020800981222     c     kdct          klist
020900981222     c                   kfld                    w001a             1
021000981222     c                   kfld                    savfil
021100981222     c                   clear                   w001a
021200020913      *
021300980828     C                   ENDSR
021400020913      *--------------------------------------------------------------------------------------------*
021500020913      * Aggiornamento file
021600020913      *--------------------------------------------------------------------------------------------*
021700980911     OFNDCD000  E            upddcd
021800980911     O                       dcdftr
021900980911     O                       dcddtr
022000980911     OFNDKA000  E            upddka
022100980911     O                       dkaftr
022200980911     O                       dkadtr
022300980911     OFNDCF000  E            upddcf
022400980911     O                       dcfftr
022500980911     O                       dcfdtr
022600981019     OFNDCL000  E            upddcl
022700981019     O                       dclftr
022800981019     O                       dcldtr
