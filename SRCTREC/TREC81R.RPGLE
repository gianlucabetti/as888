000100950519     H*------------------------------------------------------------------------*
000200020521     H*  Nome programma:  TREC81R
000300950519     H*  Descrizione   :  Stampa competenze di conto economico.
000400950519     H*                   Programma di stampa batch.
000500950519     H*  Data creazione:  14 GIU 1994
000600950519     H*------------------------------------------------------------------------*
000700000000     H DECEDIT('0,') DATEDIT(*DMY.)
000800000000     F*------------------------------------------------------------------------*
000900000000     F* DATA BASE
001000000000     F*------------------------------------------------------------------------*
001100031212     Ftntbe01L  IF   E           K DISK
001101031212     FECECT01L  IF   E           K DISK
001200000000     FECECD01L  IF   E           K DISK
001300020520     FANENT01L  IF   E           K DISK
001400000000     FTABEL00F  IF   E           K DISK
001500000000     FTREC81P   O    E             PRINTER OFLIND(*IN30)
001600000000     F                                     USROPN
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D CMD             S             80    DIM(1) CTDATA PERRCD(1)
002100940830     D*-------------------
002200940830     D* RICERCA PARAMETRI UTENTE
002300940830     D*-------------------
002400000000     D*------------------------------------------------------------------------*
002500000000     D* INPUT
002600000000     D*------------------------------------------------------------------------*
002700000000     D*------------------
002800020923     D* DS REPERIMENTO DATI UTENTE
002900020923     D*-------------------
003000020923     D TIBS34DS      E DS
003100020923     D DDATIUTE      E DS
003200020923     D AZUTEDS       E DS                  extname(AZUTE00F)
003204031212     D des             S             18    inz
003205031212     D                                     DIM(18)
003300000000     D*-------------------
003400000000     D* ARCHITETTURA
003500000000     D*-------------------
003600000000     D KPJBA         E DS
003700000000     D*-------------------
003800000000     D* PARAMETRI PASSATI DAL FILTRO
003900000000     D*-------------------
004000000000     D PARAM           DS
004100000224     D  PA1CMP                 1      3  0
004200000224     D  PA2CMP                 4      6  0
004300000224     D  PA1DDV                 7     14  0
004400000224     D  PA2DDV                15     22  0
004500000224     D  PARFCR                23     23
004600991223     D*------------------
004700991223     D* CALL XSRDA8 - CONTROLLO DATE (8)
004800991223     D*------------------
004900991223     D WLBDA8          DS                  INZ
005000991223     D  G08DAT                 1      8  0
005100991223     D  G08INV                 9     16  0
005200991223     D  G08ERR                17     17
005300991223     D  G08TGI                18     22  0
005400991223     D*------------------
005500991223     D* S/COMPOSIZIONE DATA
005600991223     D*------------------
005700991223     D                 DS
005800991223     D  DSANN8                 1      4  0
005900991223     D  DSMES8                 5      6  0
006000991223     D  DSGIO8                 7      8  0
006100991223     D  DSDAT8                 1      8  0
006200991223     D                 DS
006300991223     D  DSMES2                 1      2  0
006400991223     D  DSANN2                 3      4  0
006500991223     D  DSDAT2                 1      4  0
006600000000     C*------------------------------------------------------------------------*
006700000000     C* MAIN LINE
006800000000     C*------------------------------------------------------------------------*
006900000000     C                   MOVEL     *BLANKS       COMMAN
007000000000     C                   MOVEA     CMD(1)        COMMAN
007100000000     C                   Z-ADD     80            LUNG
007200000000     C                   CALL      'QCMDEXC'                            99
007300000000     C                   PARM                    COMMAN           80
007400000000     C                   PARM                    LUNG             15 5
007500001102     C*
007600001102     C* APRO IL FILE DI STAMPA
007700001102     C                   OPEN      TREC81P
007701031212     c*
007702031212     c                   exsr      srtabco1
007800001102     C*
007900001102     C* IMPOSTO IN STAMPA I PARAMETRI DI LANCIO
008000001102     C                   MOVEL     PA1CMP        PARCCD
008100001102     C                   MOVEL     PA2CMP        PARCCA
008200001102     C                   MOVEL     PARFCR        PARTIP
008300000000     C*
008400001102     C* EMETTO LE INTESTAZIONE E I PARAMETRI
008500000000     C                   WRITE     EC81T1
008600001102     C                   WRITE     EC81PA
008700000000     C                   WRITE     EC81T2
008800000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
008900000000     C                   SETOFF                                       31
009000000000     C*
009100000000     C* DEVO REPERIRE IL NUMERO BLOCCO, L'ULTIMO IN ARCHIVIO
009200000000     C     *HIVAL        SETLL     ECECT01L
009300000000     C                   READP     ECECT01L                               99
009400000000     C   99              Z-ADD     1             $NBC
009500000000     C  N99              Z-ADD     ECTNBC        $NBC
009600000000     C*
009700000000     C* LEGGO TUTTI I RECORD DAL FILE TESTATE COMPETENZE
009800000706     C     KE2ECT        SETLL     ECECT01L
009900000000     C                   READ      ECECT01L                               20
010000000000DO  1C     *IN20         DOWEQ     *OFF
010100000000     C*
010200001102     C* VERIFICO I LIMITI
010300000000IF  2C     ECTCMP        IFLE      PA2CMP
010400000224     C     ECTCMP        ANDGE     PA1CMP
010500001102     C     ECTDDC        ANDLE     PA1DDV
010600001102     C     ECTDSC        ANDGE     PA2DDV
010700940727     C     ECTATB        ANDEQ     *BLANKS
010800000000     C*
010900000000IF  3C     PARFCR        IFEQ      *BLANKS
011000000000     C     PARFCR        OREQ      ECTFCR
011100000000     C*
011200000000     C                   EXSR      DETTAG
011300000000     C*
011400000000E   3C                   ENDIF
011500000000E   2C                   ENDIF
011600000000     C*
011700000000     C                   READ      ECECT01L                               20
011800000000E   1C                   ENDDO
011900000000     C*
012000000000     C* VERIFICO SE HO STAMPATO QUALCOSA
012100000000     C     $ALMEN        IFGT      *ZERO
012200000000     C                   WRITE     EC81E1
012300000000     C                   ELSE
012400000000     C                   WRITE     EC81E2
012500000000     C                   ENDIF
012600000000     C                   CLOSE     TREC81P
012700000000     C*
012800000000     C* FINE PGM
012900000000     C                   SETON                                        LR
013000000000     C*------------------------------------------------------------------------*
013100000000     C* DETTAG - ESAMINA IL DETTAGLIO COMPETENZE (FILE ECECD00F)
013200000000     C*------------------------------------------------------------------------*
013300000000     C     DETTAG        BEGSR
013400000000     C*
013500000000     C* RIEMPO I CAMPI DI STAMPA
013600000000     C                   MOVEL     ECTATB        P1CATB
013700000000     C                   Z-ADD     ECTCMP        P1CCMP
013800000000     C                   MOVEL     ECTCMD        P1CCMD
013900991223     C                   Z-ADD     ECTDDC        DSDAT8
014000991223     C                   Z-ADD     DSANN8        DSANN2
014100991223     C                   Z-ADD     DSMES8        DSMES2
014200991223     C                   Z-ADD     DSDAT2        P1CDDC
014300991223     C                   Z-ADD     ECTDSC        DSDAT8
014400991223     C                   Z-ADD     DSANN8        DSANN2
014500991223     C                   Z-ADD     DSMES8        DSMES2
014600991223     C                   Z-ADD     DSDAT2        P1CDSC
014700000000     C                   MOVEL     ECTFCR        P1CFCR
014800980312     C                   MOVEL     ECTRIC        P1CRIC
014900020130     C                   MOVEL     ECTFDR        P1CFDR
015000000000     C     *IN30         IFEQ      *ON
015100000000     C                   WRITE     EC81T1
015200000000     C                   WRITE     EC81T2
015300940727     C                   SETOFF                                       30  31
015400000000     C                   ENDIF
015500000000     C                   WRITE     EC81D1
015600000000     C                   Z-ADD     1             $ALMEN
015700000000     C                   SETOFF                                       31
015800940803     C                   SETON                                        32
015900000000     C*
016000000000     C* DEVO EMETTERE SOLO I RECORD DI ECECD CHE HANNO:
016100000000     C* ..  STESSO COD.COMPETENZA DI ECECT
016200000000     C* ..  PERIODO DI VALIDITA' COMPRESO (O UGUALE) ALLA VALIDITA' SU ECECT
016300000000     C*
016400000000     C     KEYECD        SETLL     ECECD01L
016500000000     C     KEYECD        READE     ECECD01L                               21
016600000000     C     *IN21         DOWEQ     *OFF
016700000000     C*
016800000000     C* VERIFICO IL PERIODO VALIDITA'
016900001102   **C*    ECDDDO        IFGE      ECTDDC
017000001102   **C*    ECDDSO        ANDLE     ECTDSC
017100001102     C     ECDDDO        IFLE      PA1DDV
017200001102     C     ECDDSO        ANDGE     PA2DDV
017300940727     C     ECDATB        ANDEQ     *BLANKS
017400000000     C                   EXSR      STAMPA
017500000000     C                   ENDIF
017600000000     C*
017700000000     C     KEYECD        READE     ECECD01L                               21
017800000000     C                   ENDDO
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* STAMPA - STAMPA DEL DETTAGLIO
018300000000     C*------------------------------------------------------------------------*
018400000000     C     STAMPA        BEGSR
018500000000     C*
018600000000     C     *IN30         IFEQ      *ON
018700000000     C                   WRITE     EC81T1
018800000000     C                   WRITE     EC81T2
018900940727     C                   SETOFF                                       30  32
019000000000     C                   ENDIF
019100000000     C*
019200000000     C* RIEMPO I CAMPI DI STAMPA
019300000000     C                   MOVEL     ECDATB        P1DATB
019400940830     C                   MOVEL     ECDFRA        P1CFRA
019500940830     C                   MOVEL     ECDIOE        P1CIOE
019600000222     C                   MOVEL     ECDSEG        P1CSEG
019700000222     C                   Z-ADD     ECDPER        P1CPER
019800000000     C                   Z-ADD     ECDKCC        P1CKCC
019900981019IF  1C     ECDKCC        IFGT      *ZEROS
020000981019     C                   MOVEL     '201'         YSOC              3
020100981019     C                   Z-ADD     ECDKCC        YVON              4 0
020200981019     C                   MOVEL     *BLANKS       YVOA              8
020300981019     C                   Z-ADD     DATCOR        YDAT              8 0
020400981019     C                   MOVEL     *BLANKS       YDES             20
020500981019     C                   MOVEL     *BLANKS       YERR              1
020600981019     C                   MOVEL     *BLANKS       YMSG             80
020700981019     C                   CALL      'YCO900R'
020800981019     C                   PARM                    YSOC
020900981019     C                   PARM                    YVON
021000981019     C                   PARM                    YVOA
021100981019     C                   PARM                    YDAT
021200981019     C                   PARM                    YDES
021300981019     C                   PARM                    YERR
021400981019     C                   PARM                    YMSG
021500981019IF  2C     YERR          IFNE      'S'
021600981019     C                   MOVEL     YDES          P1DKCC
021700981019E   2C                   ENDIF
021800981019E   1C                   ENDIF
021900000000     C                   Z-ADD     ECDKSC        P1CKSC
022000000000     C                   MOVEL     ECDRIC        P1CRIC
022100940919     C                   MOVEL     *BLANKS       P1CRID
022200940919     C     ECDRIC        IFGT      *BLANKS
022300000000     C                   MOVEL     *BLANKS       TBLCOD
022400000000     C                   MOVEL     '2T'          TBLCOD
022500000000     C                   MOVEL     ECDRIC        TBLKEY
022600000000     C     KEYTAB        CHAIN     TABEL00F                           99
022700000000     C  N99              MOVEL     TBLUNI        P1CRID
022800940919     C   99              MOVEL     *ALL'*'       P1CRID
022900940919     C                   ENDIF
023000940830     C                   MOVEL     ECDCCF        P1CCCF
023100940919     C                   MOVEL     *BLANKS       P1CCCD
023200940919     C     ECDCCF        IFGT      *ZEROS
023300020520     C*
023400020520     C* COSTRUISCO LA CHIAVE X ANENT01L
023500020520     C                   MOVEL     ECDCCF        ENTENTITA
023600020520     C                   EVAL      ENTSOCIETA = YSOC
023700020520     C                   EVAL      ENTTPRIFAN = 'CDC'
023800020520     C                   EVAL      ENTENTITA  = %subst(ENTENTITA:3:3)
023900020520     C*
024000020520     C* REPERISCO LA DESCRIZIONE DEL CAPOCONTO
024100020520     C     KEYENT        CHAIN     ANENT01L
024200020520     C                   IF        %found(ANENT01L)
024300020520     C                   EVAL      P1CCCD = ENTDESCR
024400020520     C                   ELSE
024500020520     C                   MOVEL     *ALL'*'       P1CCCD
024600020520     C                   ENDIF
024700020520     C                   ENDIF
024800020520     C*
024900991223     C                   Z-ADD     ECDDDO        DSDAT8
025000991223     C                   Z-ADD     DSANN8        DSANN2
025100991223     C                   Z-ADD     DSMES8        DSMES2
025200991223     C                   Z-ADD     DSDAT2        P1CDDO
025300991223     C                   Z-ADD     ECDDSO        DSDAT8
025400991223     C                   Z-ADD     DSANN8        DSANN2
025500991223     C                   Z-ADD     DSMES8        DSMES2
025600991223     C                   Z-ADD     DSDAT2        P1CDSO
025700000000     C                   WRITE     EC81D2
025800940803     C                   SETON                                        31  32
025900981019     C*
026000000000     C                   ENDSR
026100020923     C*-------------------------------------------------------------------------
026200020923     C* REPERISCE I DATI UTENTE
026300020923     C*-------------------------------------------------------------------------
026400020923     C     REPDATIUTE    BEGSR
026500020923     C*
026600020923     C* INIZIALIZZA VARIABILI DI WRK
026700020923     C                   CLEAR                   TIBS34DS
026800020923     C                   CLEAR                   AZUTEDS
026900020923     C                   CLEAR                   DDATIUTE
027000020923     C*
027100020923     C     *DTAARA       DEFINE    §azute        azuteds
027200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
027300020923     C                   IN(E)     *DTAARA
027400020923if  1C                   IF        %Error
027500020923     c                   EVAL      I34Tla = 'L'
027600020923     C                   CALL      'TIBS34R'
027700020923     C                   PARM                    Tibs34Ds
027800020923     C                   IN        *DTAARA
027900020923e   1C                   ENDIF
028000020923     C*
028100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
028200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
028300020923     C*
028400020923     C                   ENDSR
028500000000     C*------------------------------------------------------------------------*
028501031212     c* riempo la leggenda in stampa x decodificare il campo data riferi-
028502031212     c* mento (ectfdr)
028700000000     C*------------------------------------------------------------------------*
028800031212     C     srtabco1      BEGSR
028801031212     c*
028802031212     c     'CO1'         setll     tntbe01l
028803031212     c                   do        *hival
028804031212     c     'CO1'         reade     tntbe01l
028805031212     c                   if        %eof(tntbe01l)
028806031212     c                   leave
028807031212     c                   end
028808031212     c                   add       1             x                 2 0
028809031212     c* massimo 18 elementi
028810031212     c                   if        x > 18
028811031212     c                   leave
028812031212     c                   end
028813031212     c                   eval      des(x) = %subst(tbeke1: 1: 1) + '=' +
028814031212     c                             %subst(tbeuni: 1: 15)
028815031212     c                   enddo
028816031212     c* se ho ancora elementi vuoti decodifico anche 0 e 1
028817031212     c                   if        x < 18
028818031212     c                   add       1             x
028819031212     c                   eval      des(x) = '0=MM spediz. OLD '
028820031212     c                   end
028821031212     c                   if        x < 18
028822031212     c                   add       1             x
028823031212     c                   eval      des(x) = '1=MM succes. OLD '
028824031212     c                   end
028825031212     c*
028826031212     c                   eval      dsco1a = des(1)+des(2)+des(3)+des(4)+
028827031212     c                             des(5)+des(6)+des(7)+des(8)+des(9)
028828031212     c                   eval      dsco1b = des(10)+des(11)+des(12)+
028829031212     c                             des(13)+des(14)+des(15)+des(16)+des(17)+
028830031212     c                             des(18)
028831031212     C*
028832031212     C                   ENDSR
028833031212     C*------------------------------------------------------------------------*
028834031212     C* *INZSR - IMPOSTAZIONI INIZIALI
028835031212     C*------------------------------------------------------------------------*
028836031212     C     *INZSR        BEGSR
028900000000     C*
029000000000     C* PARAMETRI IN INPUT AL PGM
029100000000     C     *ENTRY        PLIST
029200000000     C                   PARM                    KPJBA
029300000000     C                   MOVEL     KPJBU         PARAM
029400000224     C*
029500000224     C* RIGIRA LA 1° DATA PASSATA NEL PARAMETRO
029600001102     C                   Z-ADD     PA1DDV        PARDAT
029700000224     C     PA1DDV        IFGT      *ZEROS
029800000224     C                   Z-ADD     PA1DDV        G08DAT
029900000224     C                   Z-ADD     *ZEROS        G08INV
030000000224     C                   MOVEL     '0'           G08ERR
030100000224     C                   CALL      'XSRDA8'
030200000224     C                   PARM                    WLBDA8
030300000224     C                   Z-ADD     G08INV        PA1DDV
030400000224     C                   ENDIF
030500000224     C*
030600000224     C* RIGIRA LA 2° DATA PASSATA NEL PARAMETRO
030700000224     C     PA2DDV        IFGT      *ZEROS
030800000224     C                   Z-ADD     PA2DDV        G08DAT
030900000224     C                   Z-ADD     *ZEROS        G08INV
031000000224     C                   MOVEL     '0'           G08ERR
031100000224     C                   CALL      'XSRDA8'
031200000224     C                   PARM                    WLBDA8
031300000224     C                   Z-ADD     G08INV        PA2DDV
031400000224     C                   ENDIF
031500940830     C*
031600940830     C     *LIKE         DEFINE    ECTNBC        $NBC
031700000000     C*
031800000000     C* RECUPERO DATI DELL'UTENTE
031900020923     C                   EXSR      REPDATIUTE
032000000000     C                   MOVEL     DSFIRS        PTCRSU
032100020923     C                   Z-ADD     1             TBLKUT
032200981019     C*
032300981019     C* CALCOLA LA DATA CORRENTE
032400981019     C                   TIME                    N14              14 0
032500981019     C                   MOVE      N14           N8                8 0
032600981019     C                   Z-ADD     N8            G08DAT
032700981019     C                   Z-ADD     *ZEROS        G08INV
032800981019     C                   MOVEL     '0'           G08ERR
032900981019     C                   CALL      'XSRDA8'
033000981019     C                   PARM                    WLBDA8
033100981019     C                   Z-ADD     G08INV        DATCOR            8 0
033200000000     C*
033300000000     C     KEYECT        KLIST
033400000000     C                   KFLD                    $NBC
033500000000     C                   KFLD                    PA1CMP
033600000224     C                   KFLD                    PA1DDV
033700000706     C*
033800000706     C     KE2ECT        KLIST
033900000706     C                   KFLD                    $NBC
034000000706     C                   KFLD                    PA1CMP
034100000000     C*
034200000000     C     KEYECD        KLIST
034300000000     C                   KFLD                    $NBC
034400000000     C                   KFLD                    ECTCMP
034500000000     C*
034600940830     C     KEYTAB        KLIST
034700020923     C                   KFLD                    TBLKUT
034800000000     C                   KFLD                    TBLCOD
034900000000     C                   KFLD                    TBLKEY
035000020520     C*
035100020520     C* CHIAVE LETTURA ANENT01L - COMPLETA
035200020520     C     KEYENT        KLIST
035300020520     C                   KFLD                    ENTSOCIETA
035400020520     C                   KFLD                    ENTTPRIFAN
035500020520     C                   KFLD                    ENTENTITA
035600000000     C*
035700000000     C* SE I PARAMETRI INIZIALE/FINALE SONO AMBEDUE VUOTI, IMPOSTO IL FINALE
035800000000     C     PA1CMP        IFEQ      *ZERO
035900000000     C     PA2CMP        ANDEQ     *ZERO
036000000000     C                   Z-ADD     *HIVAL        PA2CMP
036100000000     C                   ENDIF
036200000000     C*
036300000000     C                   ENDSR
036400000000     C*------------------------------------------------------------------------*
036500000000** OVERRIDE SUL FILE DI STAMPA
036600000000 OVRPRTF FILE(TREC81P) OVRFLW(62) USRDTA('Competenze')
