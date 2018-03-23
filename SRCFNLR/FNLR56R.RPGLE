000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941111     H* FNLR56R *----------------------------------------------------*
000300911129     H*         - GESTIONE RIPRISTINO SPEDIZIONI IN CONSEGNA -       *
000400000000     H*--------------------------------------------------------------*
000500941111     FFNLR56D   CF   E             WORKSTN
000600911129     FTABEL00F  IF   E           K DISK
000700021204     Ffiapd01L  IF   E           K DISK
000800941111     FFNARB01L  UF   E           K DISK
000900041202     FFNANM02L  IF   E           K DISK
001000990519     F                                     RENAME(FNANM000:FNANM2)
001100960527     F                                     INFDS(ANM02)
001200000113     FFNARBX2L  UF   E           K DISK
001300000113     FFNART01L  UF   E           K DISK
001400161122     FFiART01L  UF   E           K DISK    prefix(new:3)
001500060214     FFiAR401L  UF   E           K DISK
001600130725     FFiAR501L  UF   E           K DISK
001700990921     FFIAR601L  IF   E           K DISK
001800051115     FFIAR901L  IF   E           K DISK
001900990921     FFIARI01L  UF   E           K DISK
002000140303     fTigcp21L  IF   E           K DISK
002100941111     FCNASS01L  UF   E           K DISK
002200120321     Ffncss01L  iF   E           K DISK
002300120321     F                                     RENAME(FNcss000:FNcss001)
002400120321     Ffncss03L  uF   E           K DISK
002500120321     Ftncsm03L  uF   E           K DISK
002600941111     FFNLBL01L  IF   E           K DISK
002700941111     FFNLBL02L  IF   E           K DISK
002800941111     F                                     RENAME(FNLBL000:FNLBLMAM)
002900020827     FFNFVV01L  IF   E           K DISK
003000140303     FFidst01L  IF   E           K DISK
003100011121     Ffiftt01L  UF   E           K DISK
003200060126     Ffiftd22L  UF   E           K DISK
003300071106     Ffifce01L  UF   E           K DISK
003400011121     Ffifst01L  UF   E           K DISK
003500011121     F                                     RENAME(fiftt000:fifst000)
003600011121     Ffifsd02L  UF   E           K DISK
003700011121     F                                     RENAME(fiftd000:fifsd000)
003800940324     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003900040825     Ffnblp01l  UF   E           K DISK
004000040909     Ffnblt01l  UF   E           K DISK
004100040825     FTITAS30C  UF   E           K DISK    USROPN
004200040826     Ffnevb05l  UF   E           K DISK    rename(fnevb000:fnevb5) prefix(x)
004300990803     FFNEVB00F  O    E             DISK
004400940324     F*----------------------------------------------------
004500170522     D ERR             S             75    DIM(18) CTDATA PERRCD(1)             ERRORI A VIDEO
004600040826     d cmd             s             60    dim(01) ctdata perrcd(1)
004700040826     d comman          s             80
004800040826     d lenght          s             15  5 inz(80)
004900040826     d wlib            s             10    inz('GAITRAGRU ')
005000040910     d wlibp           s             10    inz('GAITRAGRPS')
005100941111     D C3A             S              2    DIM(50)                              CODICE TABELLA 3A
005200941111     D D3A             S             71    DIM(50)                              PARAM. TABELLA 3A
005300050104     D CLI             S              7  0 DIM(9990)                            COD.CLI DA TRASM.
005400971007     D*
005500960527     D ANM02           DS
005600960527     D  ANMNR2               397    400B 0
005700900517     D KPJBA         E DS
005800900511     D  LIBSYS                92    100
005900940324     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006000940324     D  DTU                  848    895P 0
006100940324     D                                     DIM(12)                              DATE PARAM
006200940324     D                                     PACKEVEN
006300020826     D ddatiute      e ds
006400020826     D azuteds       e ds                  extname(AZUTE00F)
006500020826     D tibs34ds      E DS                  inz
006600120321     D tnsc30ds      E DS
006700140303     D trul06ds      E DS
006800960508     D  LIN                    1     90  0
006900960508     D                                     DIM(30)                              FIL. COMODO
007000960527     D DSLR33        E DS                  EXTNAME(FNLR33DS)
007100990921     D YEURDS        E DS                  EXTNAME(YEURCODS)
007200960507     D DS7A2         E DS
007300960507     D DS2A          E DS
007400911209     D DS3A          E DS
007500040825     D DS3K          E DS
007600040909     D DSbl4j        E DS
007700130725     D dar5cog       e ds
007800911209      *
007900941111     D WLBDA8          DS
008000941111     D  G02DAT                 1      8  0
008100941111     D  G02INV                 9     16  0
008200941111     D  G02ERR                17     17
008300941111     D  G02TGI                18     22  0
008400940324     D                 DS                  INZ
008500941111     D  V01ANS                 1      4  0
008600941111     D  V01LNP                 5      7  0
008700941111     D  V01NRS                 8      9  0
008800941111     D  V01NSP                10     16  0
008900941111     D  NEWSPE                 1     16  0
009000960131     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
009100960131     D PARAM3          DS
009200960131     D  PA3PDR                 1      7  0
009300960131     D  PA3TSR                 8      8
009400960131     D  PA3NDC                 9     15  0
009500960131     D  PA3FCT                16     16
009600960131     D  PA3FVD                17     17
009700960131     D  PA3FVT                18     18
009800960131     D  PA3DDC                19     26  0
009900041122     D  PA3sml               256    256
010000960508     D PARAM           DS
010100960508     D  PARNFV                 2      6  0
010200960508     D  PARNPG                 7      7  0
010300960508     D  LNP                    8     10  0
010400960508     D  LNA                   11     13  0
010500960508     D  NRS                   14     15  0
010600960508     D  NSC                   16     22  0
010700960508     D  CAN                   23     23
010800960508     D  VLS                   24     30  6
010900960508     D  FLG                   31     31
011000960508     D  FEL                   92     94  0
011100960508     D  NPS                   95     96  0
011200960508     D  PARFGS                97     99  0
011300970606     D  PARFLF               100    102  0
011400031128     D  pes                  103    105  0
011500131104     c*
011600131104     d dEVB01        e ds
011700131104     c*
011800040715     d Fidn12ds      e ds
011900040715     d  skKey                 26    305    dim(20)
012000040715     d  skAnn                306    325    dim(20)
012100040715     d  skDca                326    485    dim(20)
012200040715     d  skFca                486    545  0 dim(20)
012300040715     d  skDch                546    705  0 dim(20)
012400040715     d  skCch                706    745    dim(20)
012500120321     D COMNRA          S                   LIKE(ASSNRA)
012600040715
012700940323     I*----------------------------------------------------
012800940324     IAZORG01L  AA
012900940324     I                             P    4    5 0ORGFIL
013000920226     I/SPACE 3
013100040825     ITITAS000      10
013200040825     ITITAS010      11
013300040825     ITITASP00      12
013400900702     C*****************************************************************
013500900509     C     FOR01         TAG
013600941111     C                   Z-ADD     NEWSPE        SAVSPE           16 0
013700940324     C                   MOVEL     $MSG          PREMSG           75
013800941111     C                   EXFMT     LR56D01
013900020924?    C* F18 - Cambio Filiale
014000020924     C     *INKS         IFEQ      '1'
014100020924     c                   seton                                        79
014200140303     c                   goto      for01
014300020924     c                   end
014400020924      *
014500940324     C                   SETOFF                                       901318
014600940324     C                   SETOFF                                       1412
014700940324     C                   MOVEL     *BLANKS       $MSG
014800940324     C                   MOVE      ' '           RIPCON            1
014900910320     C** CMD3 - FINE LAVORO
015000900509     C   KC              GOTO      FINE
015100911129     C*
015200140303      * Controllo se esiste la filiale da me digitata
015300140303     C     V01fgs        LOOKUP    LIN                                    96
015400140303     C     *IN96         IFEQ      '0'
015500140303     C                   SETON                                        2890
015600140303     C                   MOVEL     ERR(17)       $MSG
015700140303     C                   END
015800140303     c
015900930421     C* Verifico che la data impostata sia formalmente esatta
016000930421     C  N90              DO
016100930421     C                   Z-ADD     V01DCM        G02DAT
016200930421     C                   MOVEL     *BLANKS       G02ERR
016300941111     C                   CALL      'XSRDA8'
016400941111     C                   PARM                    WLBDA8
016500930421     C     G02ERR        IFEQ      '1'
016600940324     C                   MOVEL     ERR(2)        $MSG
016700930421     C                   SETON                                        1890
016800930421     C                   ELSE
016900941111     C                   Z-ADD     G02INV        DCM               8 0
017000941111     C                   Z-ADD     G02DAT        V01DCM
017100940218     C                   Z-ADD     G02TGI        DATDT             5 0
017200930421     C                   END
017300941111     C                   END
017400941111     C*
017500000104     C     §7AGGR        IFNE      0
017600000104     C     OGGI          SUB       §7AGGR        WGG15             5 0
017700941111     C  N90WGG15         IFGT      DATDT
017800000104     C                   MOVEL     ERR(11)       $MSG
017900000104     C                   SETON                                        1890
018000941111     C                   END
018100000104     C                   END
018200930421     C* Controllo se la spedizione è esistente o valida su video
018300911129     C  N90V01LNP        IFEQ      *ZEROS
018400911129     C                   SETON                                        1490
018500940324     C                   MOVEL     ERR(4)        $MSG
018600911129     C                   END
018700911129     C  N90V01NSP        IFEQ      *ZEROS
018800940324     C                   MOVEL     ERR(8)        $MSG
018900940324     C                   SETON                                        1290
019000911129     C                   END
019100941111     C* Controllo l'anno della spedizione
019200941111     C  N90V01ANS        IFNE      *ZEROS
019300941111     C                   MOVEL     V01ANS        WCOM2             2 0
019400941111     C     WCOM2         IFEQ      *ZERO
019500941111     C     V01ANS        IFGT      60
019600941111     C                   ADD       1900          V01ANS
019700941111     C                   ELSE
019800941111     C                   ADD       2000          V01ANS
019900941111     C                   END
020000941111     C                   END
020100941111     C                   END
020200941111     C* N90 CERCO LA SPEDIZIONE NELL'ARCHIVIO ARB00
020300911129     C*
020400911129     C  N90              DO
020500911129     C                   Z-ADD     V01ANS        ARBAAS
020600911129     C                   Z-ADD     V01LNP        ARBLNP
020700911129     C                   Z-ADD     V01NRS        ARBNRS
020800911129     C                   Z-ADD     V01NSP        ARBNSP
020900941111     C* AGGANCIO FNARB00
021000941111     C     KARB1         CHAIN(N)  FNARB01L                           15
021100940324     C     *IN15         IFEQ      '1'
021200940324     C                   MOVEL     ERR(6)        $MSG
021300940324     C                   SETON                                        9012
021400940324     C                   ELSE
021500940324     C* Controllo tipi bolla per vedere se bolla di recupero
021600940324     C* assegnato: tipo bolla 'A3'
021700940323     C                   Z-ADD     1             X                 2 0
021800941111     C                   CLEAR                   DS3A
021900940323     C                   MOVEL     *BLANKS       VIDCBL
022000940323     C                   MOVEL     *BLANKS       PORTO             1
022100940323     C                   MOVEL     *BLANKS       TIPBOL            2
022200941111     C     ARBCBO        LOOKUP    C3A(X)                                 11
022300940324     C     *IN11         IFEQ      '1'
022400941111     C                   MOVEL     D3A(X)        DS3A
022500941111     C                   MOVEL     §3ADES        VIDCBL
022600941111     C                   MOVEL     §3ATB1        PORTO
022700941111     C                   MOVEL     §3ATB1        TIPBOL
022800940323     C                   END
022900940324     C*
023000020827     C* Controllo se la distinta è esistente e chiusa
023100020826     C     V01NDC        IFEQ      0
023200020826     C                   MOVEL     ERR(1)        $MSG
023300020826     C                   SETON                                        1390
023400020826     C                   END
023500940218     C*
023600120127     C* Se bolla di recupero assegnato non eseguo chain su FNFVV
023700940218     C     ARBNDC        IFNE      9999999
023800940218     C     ARBNMI        ORNE      9999999
023900940323     C     TIPBOL        ORNE      'A3'
024000940218     C*
024100940218     C  N90              DO
024200020826     C                   Z-ADD     V01fgs        FVVfgs
024300020826     C                   Z-ADD     V01NDC        FVVNFV
024400020826     C     KFVV          CHAIN     FNFVV01L                           13
024500940324     C     *IN13         IFEQ      '1'
024600940324     C                   SETON                                        90
024700940324     C                   MOVEL     ERR(1)        $MSG
024800940324     C                   ELSE
024900140303     c                   z-add     fvvnfv        dstnfv
025000140303     c                   z-add     fvvfgs        dstfgs
025100140303     C     Kdst          CHAIN     Fidst01l
025200940324     C     FVVFCF        IFNE      'S'
025300940324     C                   MOVEL     ERR(1)        $MSG
025400930423     C                   SETON                                        1390
025500930423     C                   END
025600940324     C                   END
025700940218     C*
025800940218     C                   END
025900940218     C                   END
026000140611     c* se non essiete più la distinta su arb verifico l'ultima distinta
026100140911     c* (ritorno all'incasso) evento mic
026200140911     c                   if        (arbicc= 'R' or arbica ='R')
026300140611     c                   exsr      srultmic
026400140611     c                   end
026500140912     c* se non esiste più la distinta su arb verifico l'ultima distinta
026600140911     c* (Parziale ) evento P
026700140912     C                   SETOff                                       66
026800140911     c                   if        arbdcm = 0 and arbdcp > 0
026900140911     c                   if        arbndc <> 0 and arbndc <> v01ndc
027000140911     C                   SETON                                        1390
027100140911     C                   MOVEL     ERR(5)        $MSG
027200140911     c                   else
027300140911     c                   exsr      srultp
027400140912     C                   SETON                                        66
027500140911     c                   end
027600140911     c                   end
027700140611     C* CONTROLLO SE E' DELLA STESSA DISTINTA DEL VIDEO
027800020826     c                   if        not *in90  and (arbifp <> v01fgs or
027900020826     c                             arbndc <> v01ndc)
028000940324     C                   SETON                                        1390
028100940324     C                   MOVEL     ERR(5)        $MSG
028200911129     C                   END
028300010806      * E SE HA IL FLAG DI DISTINTA CHIUSA = S
028400010817     C  N90ARBFDC        IFNE      'S'
028500140911     c     arbdcm        andgt     0
028600010806     C                   SETON                                        1390
028700010806     C                   MOVEL     ERR(15)       $MSG
028800010806     C                   ENDIF
028900941111     C                   END
029000941111     C                   END
029100940218     C*
029200940218     C* Se bolla di recupero assegnato non controllo legami
029300940218     C* dettaglio segnacolli ne flag di incasso
029400170522b1   C  N90ARBDCM        IFEQ      0
029500940218     C     ARBNDC        ORNE      9999999
029600940218     C     ARBNMI        ORNE      9999999
029700940323     C     TIPBOL        ORNE      'A3'
029800930421     C*  C'E' LEGAME E LA LINEA DI ARRIVO MAMMA E FIGLIA E' UGUALE
029900930421     C*  NON AMMESSO IL RIPRISTINO  (CONTROLLO SE C'E' MAMMA)
030000941111     C     KARB1         CHAIN     FNLBL01L                           54
030100170522b2   C  N54LBLLAN        IFEQ      LBLLAP
030200940414     C                   MOVEL     ERR(7)        $MSG
030300940324     C                   SETON                                        1290
030400170522e2   C                   END
030500930421     C*  C'E' LEGAME E LA LINEA DI ARRIVO MAMMA E FIGLIA E' UGUALE
030600930421     C*  NON AMMESSO IL RIPRISTINO  (CONTROLLO SE C'E' FIGLIA)
030700941111     C     KARB1         CHAIN     FNLBL02L                           54
030800170522     C* N54
030900170522     C*NN90LBLLAN        IFEQ      LBLLAP
031000170522     C*                  MOVEL     ERR(7)        $MSG
031100170522     C*                  SETON                                        1290
031200170522     C*                  END
031201170522b2   C                   IF        not *in54 and not *in90
031202170522     C                   MOVEL     ERR(18)       $MSG
031203170522     C                   SETON                                        1290
031204170522e2   C                   END
031300930421     C* CONTROLLO DETTAGLIO SEGNACOLLI X VERIFICARE SE C'E' ALMENO UN
031400930421     C* SEGNACOLLO CON QUELLA DATA CONSEGNA
031500941207     C                   Z-ADD     0             WNCOL             5 0
031600941207     C     KARB1         CHAIN     FNART01L                           88
031700170522b2   C     *IN90         IFEQ      '0'
031800170522b3   C     *IN88         DOWEQ     '0'
031900170522b4   C     ARTDCM        IFEQ      DCM
032000941207     C                   ADD       1             WNCOL
032100930421     C                   SETON                                        88
032200170522e4   C                   END
032300941207     C     KARB1         READE     FNART01L                               88
032400170522e3   C                   END
032500170522b2   C  N90WNCOL         IFEQ      0
032600941207     C                   MOVEL     ERR(3)        $MSG
032700941207     C                   SETON                                        1890
032800170522e2   C                   END
032900170522e1   C                   END
033000941207     C*
033100930421     C* SE GIA' CONTABILIZZATO
033200170522b1   c                   if        not *in90
033300170522b2   C     ARBNMI        IFNE      0
033400160523     C     ARBNMI        oreq      0
033500160523     C     ARBdcp        andgt     0
033600920220     C* GUARDO SE SI PUO' RIPRISTINARE , NON SI PUO' SE:
033700920220     C*  C'E' CONTRASSEGNO GIA' INCASSATO
033800170522b3   C     §3AFCA        IFGT      0
033900941111     C     ARBICC        ANDNE     'R'
034000941111     C     ARBICC        ANDNE     ' '
034100051115     C     KARB1         CHAIN     FIAR901L                           15
034200170522b4   C  N15AR9CAS        IFGT      *ZEROS
034300940324     C                   MOVEL     ERR(7)        $MSG
034400940324     C                   SETON                                        1290
034500170522e4   C                   END
034600170522e3   C                   END
034700920220     C*  E' UN ASSEGNATO GIA' INCASSATO
034800170522b3   C     ARBICA        IFNE      'R'
034900941111     C     ARBICA        ANDNE     ' '
035000990922     C* SE PRIMA BOLLA IN ASSEGNATO IMPOSTO TIPO RECORD '1'
035100990922     C                   MOVEL     §3ATB1        WPORTO            1
035200170522b4   C     PORTO         IFEQ      'A'
035300990922     C                   MOVEL     '1'           KTRC
035400170522x4   C                   ELSE
035500990922     C                   MOVEL     '2'           KTRC
035600170522e4   C                   END
035700140911     C     KAR6          CHAIN     FIAR601L                           15
035800920220     C* E' ASSEGNATO
035900170522b4   C  N15AR6IFT        IFGT      0
036000000119     C     AR6DFT        ANDGT     0
036100940324     C                   SETON                                        1290
036200940324     C                   MOVEL     ERR(7)        $MSG
036300170522e4   C                   END
036400930421     C*  C'E' INCASSO PARZIALE
036500990921     C                   MOVEL     'A'           KTIP
036600990921     C     KARI          CHAIN     FIARI01L                           15
036700170522b4   C     ARIIPP        IFGT      0
036800940324     C                   SETON                                        1290
036900940324     C                   MOVEL     ERR(7)        $MSG
037000170522e4   C                   END
037100170522e3   C                   END
037200170522e2   C                   END
037300170522e1   C                   END
037400940218     C*
037500170522x2   C                   ELSE
037600170522b3   C     ARBDMI        IFLT      WDTU10
037700940324     C                   SETON                                        1290
037800940324     C                   MOVEL     ERR(7)        $MSG
037900170522e3   C                   END
038000170522e2   C                   END
038100950113     C** Controllo se spedizione con giacenza in fase di chiusura
038200170522b2   C     *IN90         IFEQ      '0'
038300050309     C     KGCA          CHAIN     tigcp21L                           54
038400170522b3   C     *IN54         IFEQ      '0'
038500050309     C     GCPFAS        ANDGE     40
038600950113     C                   SETON                                        1290
038700950113     C                   MOVEL     ERR(12)       $MSG
038800170522e3   C                   END
038900170522e2   C                   END
039000990204     C** CONTROLLO SE spedizione con pratica C.A. Aperta
039100170522b2   C     *IN90         IFEQ      '0'
039200040715      *
039300040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALLA SPEDIZIONE
039400040715      * SIA COME MAMMA CHE COME FIGLIA
039500040715     c                   clear                   fidn12ds
039600040715     c                   eval      i12aas = arbaas
039700040715     c                   eval      i12lnp = arblnp
039800040715     c                   eval      i12nrs = arbnrs
039900040715     c                   eval      i12nsp = arbnsp
040000040715     c                   eval      i12fel = 'E'
040100040715     c                   eval      i12fan = 'E'
040200040715     c                   eval      i12fch = 'E'
040300040715     c                   call      'FIDN12R'
040400040715     c                   parm                    fidn12ds
040500040715      **
040600040715      * se c'è almeno una C.A. aperta
040700040715      **
040800170522b3   c     o12nca        ifgt      0
040900040715      **
041000170522b4   C     SAVSPE        IFNE      NEWSPE
041100990204     C     PREMSG        ORNE      ERR(14)
041200990419     C     PREMSG        ANDNE     ERR(9)
041300990204     C                   SETON                                        1290
041400990204     C                   MOVEL     ERR(14)       $MSG
041500990204     C                   Z-ADD     NEWSPE        SAVSPE
041600170522e4   C                   END
041700040715      **
041800170522e3   C                   END
041900170522e2   C                   END
042000931110     C** CONTROLLO SE DISTINTA GIA' STATA CONTEGGIATA
042100140611     C* Controllo se padroncino abilitato ai conteggi
042200140611     C                   MOVEL     'N'           CNTPDR            1
042300140611     C                   MOVEL     arbPDC        ORGFIL
042400140611     C     kapd          CHAIN     fiapd01L                           11
042500140611     C  N11ORGFIL        CHAIN     AZORG01L                           11
042600170522b2   C     *IN11         IFEQ      '0'
042700140611     C     APDPDD        ANDNE     'S'
042800140611     C                   MOVE      'S'           CNTPDR
042900170522e2   C                   END
043000170522b2   C     *IN90         IFEQ      '0'
043100940324     C     CNTPDR        ANDEQ     'S'
043200140611     C                   MOVE      arbPDC        FTTPDR
043300931110     C                   MOVE      'C'           FTTTSR
043400140611     C                   MOVE      arbNDC        FTTNDC
043500020826     c* leggo la distinta del po scelto a video
043600171031     C     KFTT          CHAIN(N)  fiftt01L                           02
043700170522b3   C     FTTFVL        IFNE      'B'
043800931110     C     FTTFVL        ANDNE     ' '
043900020826     C     *IN02         OREQ      '1'
044000170522b4   C     SAVSPE        IFNE      NEWSPE
044100940324     C     PREMSG        ORNE      ERR(9)
044200931110     C                   SETON                                        1290
044300940324     C                   MOVEL     ERR(9)        $MSG
044400940324     C                   Z-ADD     NEWSPE        SAVSPE
044500170522x4   C                   ELSE
044600940218     C                   MOVE      'S'           RIPCON
044700170522e4   C                   END
044800170522e3   C                   END
044900170522e2   C                   END
045000911211     C** MANDATO DI INTROITO DEVE ESSERE = 0
045100911129     C   90              GOTO      FOR01
045200930422?    C                   EXSR      GESFM2
045300911209     C                   GOTO      FOR01
045400910320     C*
045500000000     C     FINE          TAG
045600960527     C* CHIUDO PGM FNLR33R SE RICHIAMATO
045700960527     C     WCLR33        IFEQ      'S'
045800960527     C                   MOVEL     'C'           D33TLA
045900960527     C                   CALL      'FNLR33R'
046000960527     C                   PARM                    DSLR33
046100960527     C                   ENDIF
046200960527     C*
046300970423     C     WRISTA        IFEQ      'S'
046400970423     C                   MOVEL     'C'           SA2TLA            1
046500970423     C                   MOVEL     'C'           SA2TRC            1
046600970423     C                   CALL      'FNLSA2R3'
046700970423     C                   PARM                    SA2TLA            1
046800970423     C                   PARM                    SA2TRC            1
046900970423     C                   PARM                    SA2DTS            8 0
047000970423     C                   PARM                    SA2LNA            3 0
047100970423     C                   END
047200000000     C                   SETON                                        LR
047300930422     C*========================================================================*
047400930422     C*-----------
047500930422     C* GESTIONE FORMATO DELLA SPEDIZIONE
047600930422     C*-----------
047700930422?    C     GESFM2        BEGSR
047800911129     C* VALORIZZO I CAMPI DEL VIDEO
047900930422?    C                   EXSR      VALOR
048000941111     C                   WRITE     LR56Z02
048100911129     C*
048200911129     C                   DO        *HIVAL
048300911129     C*
048400941111     C                   EXFMT     LR56D02
048500120322     c   kl              leave
048600120321     c* interroga multiassegni
048700120321     c   ki              exsr      srmulti
048800120322     c* forzatura se multiassegno
048900120322     c                   if        *in09 and *inkf
049000120322     C                   SETON                                        90
049100120322     C                   MOVEL     ERR(16)       $MSG
049200120322     C                   iter
049300120322     c                   end
049400120322     C*
049500120322     c                   if        not *inkj and not *inkf
049600120322     C                   iter
049700120322     c                   end
049800930203     C* PER ANNULLAMENTO CHIEDO CONFERMA
049900911129     C*
050000911129     C                   MOVEL     'NO'          V08SNO
050100120322     c                   do        *hival
050200941111     C                   EXFMT     LR56D03
050300120322     c   kl              clear                   $msg
050400120322     c   kl              leavesr
050500010817     C     V08SNO        IFEQ      'SI'
050600930422?    C                   EXSR      RIPRIS
050700930422?    C                   EXSR      EVENTO
050800120322     c                   clear                   $msg
050900120322     c                   leavesr
051000911129     C                   END
051100120322     C                   ENDdo
051200120322     C                   ENDdo
051300120322     c                   clear                   $msg
051400930422?    C                   ENDSR
051500140611      *-----------------------------------------------------***********
051600140611     c     srultmic      begsr
051700140611     c     kbol          setgt     fnevb05l
051800140611     c                   do        *hival
051900140611     c     kbol          readpe(n) fnevb05l                               21
052000140611     C   21              leave
052100140611     c                   if        xevbcev <> 'MIC'
052200140611     c                   iter
052300140611     c                   endif
052400140611     c                   if        xevbnot <> *blank
052500140611     c                   movel     xevbnot       devb01
052600140611     c                   move      §NOTpdc       arbpdc
052700140611     c                   movel     arbpdc        fil               3 0
052800140611     c                   move      fil           arbifp
052900140611     c                   move      §NOTndc       comndc            7 0
053000140611     c                   move      comndc        arbndc
053100140611     c                   eval      arbfdc = 'S'
053200140611     c                   leavesr
053300140611     c                   end
053400140611     c                   enddo
053500140611?    C                   ENDSR
053600140911      *-----------------------------------------------------***********
053700140911     c     srultp        begsr
053800140911     c     kbol          setgt     fnevb05l
053900140911     c                   do        *hival
054000140911     c     kbol          readpe(n) fnevb05l                               21
054100140911     C   21              leave
054200140911     c                   if        xevbcev <> 'P  '
054300140911     c                   iter
054400140911     c                   endif
054500140911     c                   if        xevbnot <> *blank
054600140911     c                   movel     xevbnot       devb01
054700140911     c                   move      §NOTpdc       arbpdc
054800140911     c                   movel     arbpdc        fil               3 0
054900140911     c                   move      fil           arbifp
055000140911     c                   move      §NOTndc       comndc            7 0
055100140911     c                   move      comndc        arbndc
055200140911     c                   eval      arbfdc = 'S'
055300140911     c                   leavesr
055400140911     c                   end
055500140911     c                   enddo
055600140911?    C                   ENDSR
055700120321      *-----------------------------------------------------***********
055800120321      * richiamo pgm multiassegni
055900120321      *-----------------------------------------------------***********
056000120321     C     srmulti       BEGSR
056100120321     c* interroga multiassegni
056200120321     c                   clear                   tnsc30ds
056300120321     c                   eval      sc30CMTI = 'N'
056400120321     c                   eval      sc30TPCI = 'V'
056500120321     c                   eval      sc30AASI = v01ans
056600120321     c                   eval      sc30LNPI = v01lnp
056700120321     c                   eval      sc30NRSI = v01nrs
056800120321     c                   eval      sc30NSPI = v01nsp
056900120321     c                   movel(p)  tnsc30ds      kpjbu
057000120321     c                   call      'TNSC30R'
057100120321     c                   parm                    kpjba
057200120321     C*
057300120321     c                   endsr
057400930422     C*-----------
057500930422     C* VALORIZZO I CAMPI DELLA SPEDIZIONE A VIDEO
057600930422     C*-----------
057700930422?    C     VALOR         BEGSR
057800930422     C*
057900911129     C* CONTROLLO SE LA BOLLA IN QUESTIONE HA LEGAME CON SECONDA BOLLA
058000911129     C     ARBFAN        IFEQ      'S'
058100911209     C                   MOVEL     'ANOM.'       VIDANO            5
058200911129     C                   ELSE
058300911129     C                   MOVEL     *BLANKS       VIDANO
058400911129     C                   END
058500911129     C*
058600941207     C                   Z-ADD     0             AR9CAS
058700941207     C                   MOVEL     *BLANKS       AR9VCA
058800911129     C                   MOVEL     ARBMGS        MM                2 0
058900911129     C                   MOVE      ARBMGS        GG                2 0
059000911209     C                   MOVEL     GG            V04MGS            4 0
059100911129     C                   MOVE      MM            V04MGS
059200911209     C                   MOVEL     V04MGS        V04GMA
059300911209     C                   MOVE      ARBAAS        V04GMA
059400911129     C                   Z-ADD     ARBNCL        V04COL
059500941111     C                   Z-ADD     ARBPKF        V04PES
059600941111     C                   Z-ADD     ARBVLF        V04VOL
059700911129     C                   MOVE      ARBDCR        MMGG              4 0
059800911129     C                   MOVEL     MMGG          MM                2 0
059900911129     C                   MOVE      MMGG          GG                2 0
060000911209     C                   MOVE      MM            V04GMC            4 0
060100911129     C                   MOVEL     GG            V04GMC
060200911129     C                   Z-ADD     *ZEROS        V04IMA
060300941111     C*
060400941111     C     §3ATB2        IFNE      *BLANKS
060500990921     C     PORTO         OREQ      'A'
060600990922     C     PORTO         IFEQ      'A'
060700990922     C                   MOVEL     '1'           KTRC
060800990922     C                   ELSE
060900990922     C                   MOVEL     '2'           KTRC
061000990922     C                   END
061100140911     C     KAR6          CHAIN     FIAR601L                           74
061200990921     C                   MOVEL     'A'           KTIP
061300990921     C                   MOVEL     AR6DIV        ARIDIV
061400990921     C                   Z-ADD     0             ARIIPP
061500990921     C                   Z-ADD     0             ARIIMP
061600010817      * VISUALIZZO INTANTO L'IMPORTO DELLA FATTURA ... E POI CERCO
061700010817      * IN FIARI01L SE SONO STATI FATTI INCASSI PARZIALI O IN DIVISA <>
061800010817      * E LI SOSTITUISCO ALL'IMPORTO DELLA FATTURA
061900990921     C  N74AR6IFT        IFNE      *ZEROS
062000990921     C                   Z-ADD     AR6IFT        V04IMA
062100010817     C     KARI          CHAIN     FIARI01L                           74
062200010817     C  N74              CLEAR                   V04IMA
062300990921     C     *IN74         DOWEQ     '0'
062400991115     C     ARIIMP        IFGT      0
062500990921     C     AR6DIV        IFEQ      ARIDIV
062600010817     C                   ADD       ARIIMP        V04IMA
062700990921     C* Se la divisa di incasso è diversa converto importo già
062800990921     C* incassato nella divisa della bolla
062900990921     C                   ELSE
063000990921     C                   CLEAR                   YEURDS
063100990921     C                   MOVEL     ARIDIV        YECDVI
063200991115     C                   Z-ADD     ARIIMP        YECIMI
063300990921     C                   MOVEL     AR6DIV        YECDCO
063400990921     C                   MOVEL     'H'           YECTAR
063500990921     C                   CALL      'YEURCO'
063600990921     C                   PARM                    YEURDS
063700010817     C                   ADD       YECIMO        V04IMA
063800911209     C                   END
063900991115     C                   END
064000990921     C     KARI          READE     FIARI01L                               74
064100990921     C                   END
064200990921     C                   END
064300941111     C                   END
064400941111     C*
064500941207     C     §3AFCA        IFGT      0
064600051115     C     KARB1         CHAIN     FIAR901L                           15
064700941207     C                   END
064800941207     C*
064900941111     C                   MOVEL     *BLANKS       AR4NOT
065000970401     C                   MOVEL     '0'           KTRC
065100060214     C     KAR4          CHAIN     FiAR401L                           74
065200941111     C                   MOVEL     AR4NOT        ARBNOT
065300970401     C                   MOVEL     '1'           KTRC
065400941111     C                   MOVEL     *BLANKS       AR4NOT
065500060214     C     KAR4          READE     FiAR401L                               74
065600941111     C                   MOVEL     AR4NOT        ARBNO2
065700941111     C*
065800941111     C     ARBPRD        COMP      *BLANKS                            05
065900941111     C     ARBNZD        COMP      *BLANKS                            06
066000941111     C     ARBPRM        COMP      *BLANKS                            07
066100941111     C     ARBNZM        COMP      *BLANKS                            08
066200120321     C* per multiassegni abilito il cmd di visualizzazione
066300120321     c     karb1         setll     fncss01l                               09
066400120321     C*
066500930422?    C                   ENDSR
066600930422     C*-----------
066700930422     C* RIPRISTINO I CAMPI DELLA SPEDIZIONE
066800930422     C*-----------
066900930422?    C     RIPRIS        BEGSR
067000911129     C*
067100960508     C     KARB1         CHAIN(N)  FNARB01L                           71
067200911129     C*
067300911129     C  N71              DO
067400951204     C     KARB1         SETLL     FNART01L
067500951204     C*
067600970604     C                   MOVE      *BLANKS       WNPS85            1
067700951204     C*  AGGIORNO DETTAGLIO SEGNACOLLI
067800951204     C                   DO        *HIVAL                                       *B005
067900951204     C     KARB1         READE     FNART01L                               72
068000951204     C  N72ARTDCM        IFEQ      DCM
068100951204     C                   Z-ADD     *ZEROS        ARTDCM
068200970902     C                   MOVEL     *BLANKS       ARTFL1
068300960508     C                   UPDATE    FNART000
068400161122     c
068500161122      *
068600161122     c     kart          chain     fiart01l
068700161122     c                   if        %found(fiart01l)
068800161122     c                   clear                   newdcm
068900161128     c                   clear                   newhcm
069000161122     c                   clear                   newcca
069100161122     c                   if        artnap=85  and artdam=dcm
069200161122     c                   clear                   newdam
069300161122     c                   clear                   newdia
069400161122     c                   clear                   newflnap
069500161122     c                   clear                   newham
069600161122     c                   clear                   newnap
069700161122     c                   endif
069800161122     c                   if        newnapf=85 and newdamf=dcm
069900161122     c                   clear                   newnapf
070000161122     c                   clear                   newdamf
070100161122     c                   clear                   newdiaf
070200161122     c                   clear                   newhamf
070300161122     c                   endif
070400161122     c                   update    fiart000
070500161122     c                   endif
070600161122     c
070700951204     C     ARTDAM        IFEQ      DCM
070800951204     C     ARTNAP        ANDEQ     85
070900970604     C                   MOVE      '1'           WNPS85
071000960508     C                   CLEAR                   PARAM
071100960508     C                   Z-ADD     V01NDC        PARNFV
071200960508     C                   MOVEL     '4'           PARNPG
071300970206     C                   Z-ADD     ARTFLS        LNP
071400960508     C                   Z-ADD     ARTLNA        LNA
071500960508     C                   Z-ADD     ARTNRS        NRS
071600960508     C                   Z-ADD     ARTNSC        NSC
071700960508     C                   MOVEL     '1'           FLG
071800020916     C                   Z-ADD     simfel        FEL
071900960508     C                   Z-ADD     85            NPS
072000960508     C                   Z-ADD     FVVFGS        PARFGS
072100970606     C                   Z-ADD     FVVFLE        PARFLF
072200031128     C                   Z-ADD     FVVFGS        pes
072300960508     C                   MOVEL     PARAM         KPJBU
072400960508     C                   CALL      'FNLSA8R'
072500960508     C                   PARM                    KPJBA
072600951204     C                   END
072700960527     C*  RIAPRO ANOMALIE CHIUSE CON CAUSALE 'CO' E RIAPRO LE 138
072800960508     C                   Z-ADD     ARTFLS        KFLS
072900960508     C                   Z-ADD     ARTLNA        KLNA
073000960508     C                   Z-ADD     ARTNRS        KNRS
073100960508     C                   Z-ADD     ARTNSC        KSCN
073200990519     C     KANM2         CHAIN     FNANM02L                           73
073300960527    2C     *IN73         DOWEQ     '0'
073400041202     C* Cancello solo le anomalie 138 e 139: la riapertura di anomalie|
073500041202     c*  chiuse con la consegna la lascio al pgm fnlsa8r che fa
073600041202     c*  controllo più precisi . questo era un doppione gestito non beniss
073700960527    4C     ANMCAA        IFEQ      138                                          CHIUDO
073800000620    4C     ANMCAA        OREQ      139
073900960527     C                   Z-ADD     ANMNR2        D33NRR
074000960527     C                   MOVEL     'M'           D33FSC
074100960527     C                   MOVEL     DATEU         D33DCH
074200960527     C                   CALL      'FNLR33R'
074300960527     C                   PARM                    DSLR33
074400960527     C* MEMORIZZO CHE HO APERTO QUESTO PROGRAMMA
074500960527     C                   MOVEL     'S'           WCLR33            1
074600960527ES  4C                   ENDIF
074700990519     C     KANM2         READE     FNANM02L                               73
074800960527    2C                   END
074900960507     C*
075000951204     C                   END
075100951204     C  N72              END
075200990921     C*
075300990921     C* Aggiorno file degli incassi
075400990921     C     KARB1         CHAIN     FIARI01L                           75
075500990921     C     *IN75         DOWEQ     '0'
075600990921     C* Assegnato:
075700990921     C     ARITIP        IFEQ      'A'
075800990921     C* ... devo considerare solo i record con l'importo incassato
075900990921     C*     impostato
076000990921     C     ARIIMP        IFGT      0
076100990921     C* Se progressivo incassato > 0 update record azzerando
076200990921     C* ultimo importo incassato
076300990921     C     ARIIPP        IFGT      0
076400990921     C                   Z-ADD     0             ARIIMP
076500990921     C                   UPDATE    FIARI000
076600990921     C* ... se progressivo incassato = 0 deleto record (non
076700990921     C* ci sono altri incassi)
076800990921     C                   ELSE
076900990921     C                   DELETE    FIARI000
077000990921     C                   END
077100990921     C                   END
077200990921     C* ...C/Assegno --> solo se flag di incasso <> 'Y'--> dlt record
077300990921     C                   ELSE
077400990921     C     ARBICC        IFNE      'Y'
077500990921     C                   DELETE    FIARI000
077600990921     C                   END
077700990921     C                   END
077800990921     C* New Rcd
077900990921     C     KARB1         READE     FIARI01L                               75
078000990921     C                   END
078100000113     C*
078200000113     C* Aggiorno file stp bolle restituzione Bam/lettera Vettura
078300000113     C     KARB1         CHAIN     FNARBX2L                           75
078400000113     C     *IN75         DOWEQ     '0'
078500000113     C                   DELETE    FNARBX00
078600000113     C* New Rcd
078700000113     C     KARB1         READE     FNARBX2L                               75
078800000113     C                   END
078900990921     C*
079000990921     C* Posiz. bolla x aggiornamento
079100960527     C     KARB1         CHAIN     FNARB01L                           71
079200911129     C                   SUB       1             ARBNTC
079300911129     C                   Z-ADD     *ZEROS        ARBHMC
079400911129     C                   Z-ADD     *ZEROS        ARBDCM
079500911129     C                   Z-ADD     *ZEROS        ARBSTP
079600930423     C     ARBDCP        IFEQ      DCM
079700930423     C                   Z-ADD     *ZEROS        ARBDCP
079800930423     C                   END
079900911129     C                   Z-ADD     *ZEROS        ARBNGD
080000950214     C                   Z-ADD     *ZEROS        ARBNSS
080100020827     C                   Z-ADD     *ZEROS        ARBIFP
080200020827     c* memorizzo il po di chi ha chiuso la distinta per chainare in seguito
080300020827     c* gli assegni
080400020827     C                   Z-ADD     ARBIPP        klna
080500020827     C                   Z-ADD     *ZEROS        ARBIPP
080600911129     C                   Z-ADD     *ZEROS        ARBCUC
080700911129     C                   Z-ADD     *ZEROS        ARBNDC
080800010806      * Memorizzo il codice del padroncino per scriverlo nell'evento
080900010806     C                   Z-ADD     ARBPDC        SAVPDC
081000010806      *
081100941111     C                   Z-ADD     *ZEROS        ARBPDC
081200911129     C                   Z-ADD     *ZEROS        ARBDDC
081300911129     C                   MOVE      *BLANKS       ARBFIP
081400930203     C                   MOVE      *BLANKS       ARBCCA
081500951124     C     ARBICC        IFNE      'Y'
081600911129     C                   MOVE      *BLANKS       ARBICC
081700951124     C                   END
081800950727     C     PORTO         IFEQ      'A'
081900951124     C     ARBICA        ANDNE     'Y'
082000961218     C     PORTO         ORNE      'A'
082100961218     C     §3ATB2        ANDNE     *BLANK
082200961218     C     ARBICA        ANDNE     'Y'
082300971023     C     PORTO         OREQ      'A'
082400971023     C     ARBNMI        ANDEQ     8888888
082500971023     C     ARBICA        ANDEQ     'Y'
082600971023     C     PORTO         ORNE      'A'
082700971023     C     §3ATB2        ANDNE     *BLANK
082800971023     C     ARBNMI        ANDEQ     8888888
082900971023     C     ARBICA        ANDEQ     'Y'
083000911129     C                   MOVE      *BLANKS       ARBICA
083100950727     C                   END
083200911129     C                   MOVE      *BLANKS       ARBFDC
083300920522     C                   Z-ADD     *ZEROS        ARBNMI
083400941111     C                   Z-ADD     *ZEROS        ARBNCI
083500930615     C                   Z-ADD     *ZEROS        ARBDCE
083600941111     C                   Z-ADD     *ZEROS        ARBNRC
083700941111     C                   Z-ADD     *ZEROS        ARBNPI
083800941111     C                   UPDATE    FNARB000
083900040825      *
084000040825     c                   exsr      bollepar
084100040825     c                   exsr      bollesede
084200040825     c                   exsr      eventomic
084300970423     C*
084400970423     C*  RICHIAMO PGM PER CONTROLLO RIELAVORAZ. STATISTICHE
084500970630     C* ESCLUDO LE BOLLE DI RECUPERO
084600970630     C     §3ARBL        IFNE      'R'
084700970423     C                   MOVEL     'S'           WRISTA            1
084800970423     C                   MOVEL     *BLANKS       SA2TLA            1
084900970423     C                   MOVEL     'C'           SA2TRC            1
085000970423     C                   Z-ADD     DCM           SA2DTS            8 0
085100970423     C                   Z-ADD     FVVFGS        SA2LNA            3 0
085200970423     C                   CALL      'FNLSA2R3'
085300970423     C                   PARM                    SA2TLA            1
085400970423     C                   PARM                    SA2TRC            1
085500970423     C                   PARM                    SA2DTS            8 0
085600970423     C                   PARM                    SA2LNA            3 0
085700970604     C*
085800970604     C     WNPS85        IFEQ      '1'
085900970604     C                   CLEAR                   SA2TLA
086000970604     C                   MOVEL     'A'           SA2TRC            1
086100970604     C                   Z-ADD     DCM           SA2DTS            8 0
086200970604     C                   CLEAR                   SA2LNA
086300970604     C                   CALL      'FNLSA2R3'
086400970604     C                   PARM                    SA2TLA            1
086500970604     C                   PARM                    SA2TRC            1
086600970604     C                   PARM                    SA2DTS            8 0
086700970604     C                   PARM                    SA2LNA            3 0
086800970604     C                   END
086900970630     C                   END
087000931110     C*
087100931110     C*  SE ESISTE TESTATA TASSAZIONE PADRONCINI CANCELLO
087200931110     C*  CANCELLO RIGA DETTAGLIO SPEDIZIONE
087201171031     C     KFTT          CHAIN     fiftt01L                           02
087300940324     C     CNTPDR        IFEQ      'S'
087400140303     C     *IN02         IFEQ      '0'
087500960130     C     FTTFVL        ANDNE     'C'
087600940324     C*
087700120127     C*  SE DISTINTA CON UNA SOLA SPEDIZIONE CANCELLO FIFTT
087800940324     C     FTTSET        IFEQ      1
087900011121     C                   DELETE    fiftt01L
088000960131     C                   MOVEL     *BLANKS       FTTFVL
088100940324     C                   ELSE
088200940324     C                   SUB       1             FTTSET
088300011121     C                   UPDATE    fiftt000
088400960131     C                   END
088500140303     C                   ENDif
088600140303     C                   ENDif
088700960131     C*  CANCELLO DETTAGLIO
088800960131     C                   MOVE      ARBAAS        FTDAAS
088900960131     C                   MOVE      ARBLNP        FTDLNP
089000960131     C                   MOVE      ARBNRS        FTDNRS
089100960131     C                   MOVE      ARBNSP        FTDNSP
089200140303     C                   MOVE      dstnfv        FTDndc
089300140304     C                   MOVE      dstpdr        FTDpdr
089400060126     /* ?Dopo Aver portato in UNI i Padroncini la vista logica 02L è stata
089500060126     /* ?sostituita con la 22L che seleziona solo i records con ATB = *blank
089600060126     C     KFTD          CHAIN     fiftd22L                           02
089700060126     C  N02              DELETE    fiftd22L
089800071106     /* ?se ha eliminato FTD deve anche togliere gli eventuali FCE correlati
089900071106     C  N02              do
090000071106     C     KFce          setll     fifce01L
090100071106     C     KFce          reade     fifce01L
090200071106     c                   dow       not %Eof(fifce01L)
090300071106     c                   delete    fifce000
090400071106     C     KFce          reade     fifce01L
090500071106     c                   endDO
090600071106     c                   endDO
090700060126      *
090800960130     C* SOLO SE VALORIZZATA
090900960130    6C     FTTFVL        IFEQ      'V'
091000960130     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
091100960130     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
091200960130     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
091300960130     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
091400960130     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
091500960130     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
091600960130     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
091700041122     c                   move      ' '           Pa3sml
091800960130     C                   MOVEL     PARAM3        KPJBU
091900011121     C                   CALL      'FICNE2R'
092000960130     C                   PARM                    KPJBA
092100140303    6C                   END
092200140303    5C                   END
092300950317     C*
092400011121     C     KFTT          CHAIN     fifst01L                           02
092500950317     C*  SE DISTINTA CON UNA SOLA SPEDIZIONE CANCELLO FLFST
092600140303    5C     FTTFVL        IFNE      'C'
092700140303     C*mm  *IN02         ANDEQ     '0'
092800140303    6C  N02FTTSET        IFEQ      1
092900011121     C                   DELETE    fifst01L
093000960131     C                   MOVEL     *BLANKS       FTTFVL
093100950317     C                   ELSE
093200950317     C                   SUB       1             FTTSET
093300011121     C                   UPDATE    fifst000
093400140303    6C                   END
093500140303    6C                   ENdif
093600960131     C*  CANCELLO DETTAGLIO
093700960131     C                   MOVE      ARBAAS        FTDAAS
093800960131     C                   MOVE      ARBLNP        FTDLNP
093900960131     C                   MOVE      ARBNRS        FTDNRS
094000960131     C                   MOVE      ARBNSP        FTDNSP
094100011121     C     KFTD          CHAIN     fifsd02L                           02
094200011121     C  N02              DELETE    fifsd02L
094300960130    6C     FTTFVL        IFEQ      'V'
094400960130     C                   MOVEL     FTDPDR        PA3PDR                         *COD. PADRONCINO
094500960130     C                   MOVEL     FTDTSR        PA3TSR                         *TIPO PRESTAZIONE
094600960130     C                   Z-ADD     FTDNDC        PA3NDC                         *NUMERO DISTINTA
094700960130     C                   Z-ADD     FTDDDC        PA3DDC                         *DATA   DISTINTA
094800960130     C                   MOVEL     'N'           PA3FCT                         *FLG COD.TARIFFA
094900960130     C                   MOVEL     'N'           PA3FVD                         *FLG VAL.MANUAL.DETT
095000960130     C                   MOVEL     'N'           PA3FVT                         *FLG VAL.MANUAL.TEST
095100041122     c                   move      'S'           Pa3sml
095200960130     C                   MOVEL     PARAM3        KPJBU
095300011121     C                   CALL      'FICNE9C'
095400960130     C                   PARM                    KPJBA
095500140303    6C                   END
095600140303    5C*mm                END
095700140303    4C*mm                END
095800931110     C*
095900140303    3C*mm                END
096000130725     C*cerco eventuale record su fiar5 contenente coordinate geostazionarie
096100130725      * se già trasmesse le pulisce abblencando i campi se non ancora trasmessi in sede
096200130725      * cancella il rekord
096300130725     C                   MOVEL     'COG'         KTRd
096400130725     C     KAR5          CHAIN     FiAR501L
096500130725     c                   if        %found(fiar501l)
096600130725     c                   if        ar5ft2 = *blank
096700130725     C                   DELETE    FiAR501L
096800130725     c                   else
096900130725     c                   clear                   §AR5XLO
097000130725     c                   clear                   §AR5YLA
097100130725     c                   clear                   ar5ft2
097200130725     c                   clear                   ar5dt2
097300130725     c                   movel     dar5cog       ar5uni
097400130725     c                   update    fiar5000
097500130725     c                   endif
097600130725     c                   endif
097700950317     C*
097800941111     C* CONTROLLO SE CI SONO DELLE NOTE
097900970401     C                   MOVEL     '0'           KTRC
098000060214     C     KAR4          CHAIN     FiAR401L                           74
098100060214     C  N74              DELETE    FiAR401L
098200970401     C                   MOVEL     '1'           KTRC
098300060214     C     KAR4          CHAIN     FiAR401L                           74
098400060214     C  N74              DELETE    FiAR401L
098500911209     C*
098600020827     C* CONTROLLO SE CI SONO ASSEGNI
098700941111     C     KCNA          SETLL     CNASS01L
098800960108     C     KCNA          READE     CNASS01L                               74
098900960108     C* Cerco Assegno con linea di arrivo
099000960108     C     *IN74         DOWEQ     '0'
099100120321     C  N74              DELETE    CNASS000
099200110921     c* se incassato con + assegni
099300120127     c     ' '           scan      assnra                                 52
099400120127     c                   if        not *in52 and assabi = 0
099500120127     c                   exsr      srasssped
099600120127     c                   end
099700110921     c*
099800960108     C     KCNA          READE     CNASS01L                               74
099900911209     C  N74              END                                                    *E005
100000930422?    C                   ENDSR
100100930422     C*-----------
100200930422     C* SCRITTURA RIPRISTINO SU ARCHIVIO EVENTI
100300930422     C*-----------
100400930422?    C     EVENTO        BEGSR
100500940324     c*
100600960913     C     WRTRIP        IFEQ      'S'
100700940324     C     RIPCON        ANDEQ     ' '
100800940324     C     WRTRDC        OREQ      'S'
100900940324     C     RIPCON        ANDEQ     'S'
101000930422     C                   MOVEL     *BLANKS       EVBATB
101100930422     C                   Z-ADD     DATEU         EVBDTV
101200930422     C                   TIME                    EVBORV
101300020826     C                   Z-ADD     dutpou        EVBFLE
101400020826     C                   Z-ADD     dutcou        EVBCDU
101500930422     C                   Z-ADD     ARBLNP        EVBLNP
101600930422     C                   Z-ADD     ARBAAS        EVBAAS
101700930422     C                   Z-ADD     ARBNRS        EVBNRS
101800930422     C                   Z-ADD     ARBNSP        EVBNSP
101900930422     C                   Z-ADD     DATEU         EVBDEV
102000930422     C                   MOVEL     EVBORV        EVBHEV
102100930422     C                   MOVEL     'RIP'         EVBCEV
102200940324     C     RIPCON        IFEQ      'S'
102300940324     C                   MOVEL     'RDC'         EVBCEV
102400940324     C                   END
102500040825     C                   MOVEL     'T'           EVBFTR
102600040825     C                   Z-ADD     dateu         EVBDTR
102700990803     C                   MOVEL     *BLANKS       EVBNOT
102800010806      * Memorizzo in EVBNOT il nr. distinta ripristinato e il codice
102900010806      * padroncino per essere sicuri, in fase di ricezione, di ripristinare
103000010806      * la distinta giusta
103100131104     c                   clear                   Devb01
103200131104     C                   move      *zeros        §NOTNDC
103300131104     C                   move      *zeros        §NOTPDC
103400131104     C                   move      V01NDC        §NOTNDC
103500140611     C                   move      arbPDC        §NOTPDC
103600131104     C                   MOVEL     Devb01        EVBNOT
103700990803     C                   Z-ADD     0             EVBDTC
103800990803     C                   WRITE     FNEVB000
103900930423     C                   END
104000930422     C*
104100930422?    C                   ENDSR
104200120127     C*-----------------------------------------------------*
104300120127     C* aggiorno le spedizioni come ritorno all'incasso c/ass
104400120127     C* o cancello i più assegni per la stessa spedizione
104500120127     C*-----------------------------------------------------*
104600120127     C     srasssped     BEGSR
104700120321     C                   EVAL      COMNRA =ASSNRA
104800120127     c* spedizioni incassate con multi assegni
104900120321     c     COMNRA        setll     fncss03l
105000120127     c                   do        *hival
105100120321     c     COMNRA        reade     fncss03l
105200120321     c                   if        %eof(fncss03l)
105300120127     c                   leave
105400120127     c                   end
105500120127     c* cancello gli assegni
105600120127     c     csskey        setll     tncsm03l
105700120127     c                   do        *hival
105800120127     c     csskey        reade     tncsm03l
105900120127     c                   if        %eof(tncsm03l)
106000120127     c                   leave
106100120127     c                   end
106200120127     c                   delete    tncsm000
106300120127     c                   enddo
106400120127     c* CNASS
106500120321     C     KCSSOa        CHAIN     cnass01L
1066001201272    c                   if        %found(cnass01l)
106700120127     C                   delete    cnass000
1068001201272    c                   end
106900120127     c* FNARB spedizione incassata
107000120127     C     Kcss          CHAIN     FNARB01L
1071001201272    c                   if        %found(fnarb01l)
1072001203213    c                   if        arbicc <> ' '  and arbicc <> 'Y'
107300120321     c                   eval      arbicc = 'R'
1074001201273    c                   end
107500120127     C                   UPDATE    FNARB000
1076001201272    c                   end
107700120127     c* FNARB spedizione originale
107800120127     c                   if        cssaao <> 0 and cssaao <> cssaas
107900120127     C     Kcsso         CHAIN     FNARB01L
1080001201272    c                   if        %found(fnarb01l)
1081001203213    c                   if        arbicc <> ' ' and arbicc <> 'Y'
108200120321     c                   eval      arbicc = 'R'
1083001201273    c                   end
108400120127     C                   UPDATE    FNARB000
1085001201272    c                   end
1086001201272    c                   end
108700120127     C*
108800120321     c                   delete    fncss000
108900120127     c                   enddo
109000120127     c                   endsr
109100040825     C**---------------------------------------------------------
109200040825     C** AGGIORNO BOLLA IN PARTENZA PER RIPRISTINO SPEDIZIONE
109300040825     C**---------------------------------------------------------
109400040825     C     bollepar      BEGSR
109500040825      *
109600040825      * ESEGUE IL RIPRISTINO SOLO SE LA DISTINTA È >(0)
109700040825      *  ED È UGUALE A QUELLA DELLA PARTENZA.
109800040825      *   ALTRIMENTI NON DEVE ESEGUIRE IL RIPRISTINO.
109900040825      *
110000040825     c     kbol          chain     fnblp01l
110100040825     c                   if        %found(fnblp01l)
110200040826     C     blpndc        IFGT      0
110300040826     C     blpndc        ANDEQ     v01NDC
110400040825      *
110500040825     C*  Aggiorno dati di consegna
110600040825     C                   MOVE      ARBDCE        BLPDCE
110700040825     C                   MOVE      ARBNPI        BLPNPI
110800040825     C                   MOVE      ARBNCI        BLPNCI
110900040825     C                   MOVE      ARBNRC        BLPNRC
111000040825      *
111100040825     C                   CLEAR                   BLPNDC
111200040825     C                   CLEAR                   BLPDDC
111300040825      *
111400040825     C                   CLEAR                   BLPDCM
111500040825     C                   CLEAR                   BLPHMC
111600040825      *
111700040825     C                   MOVE      ARBDCP        BLPDCP
111800040825     C                   MOVE      ARBTMC        BLPTMC
111900040825      *
112000040825     C                   MOVE      ARBCCA        BLPCCA
112100040909      *bolle poste
112200040909     c                   if        blpsop = '&'
112300040909     c                   exsr      bolleposte
112400040909     c                   endif
112500040909      *  aggiornamento dettaglio segnacolli
112600040909     C                   EXSR      DeTsegn
112700040825      *  Controllo se trasmettere dati cliente
112800040825     C                   EXSR      DATCLI
112900040825
113000040825     c                   update    fnblp000
113100040825
113200040825     c                   END
113300040825     C                   endif
113400040825     C*
113500040825     C                   ENDSR
113600040909     C**---------------------------------------------------------
113700040909     C** trattamento bolle poste
113800040909     C**---------------------------------------------------------
113900040909     C     bolleposte    begsr
114000051115     C     kbol          chain     fiar901L
114100051115     C                   if        %found(fiar901l)
114200040909     C*  SE C'È IL CONTRASSEGNO CONTROLLO SE PRESENTE IL TIPO RECORD "J"
114300040909     c                   move      'J'           trec4
114400060214     C     kbl4          chain     fiar401l
114500060214     C                   IF        %found(fiar401l)
114600040909     C*  ABBLANKA IL FLAG DI BOLLA AUTOGENERATA CON C/A SENZA INCASSO
114700060214     C                   movel     ar4not        dsbl4j
114800040909     C                   move      *BLANK        §b4csb
114900060214     C                   movel     dsbl4j        ar4not
115000060214     C                   update    fiar4000
115100040909     C                   endif
115200040909     C*
115300040909     C                   endif
115400040909      *
115500040909     c                   endsr
115600040909     C**---------------------------------------------------------
115700040909     C** aggiornamento dettaglio segnacolli
115800040909     C**---------------------------------------------------------
115900040909     C     DeTsegn       BEGSR
116000040909      *
116100040909     C     Kbol          setll     fnblt01l
116200040909     C                   do        *hival
116300040909     C     Kbol          reade     fnblt01l                               22
116400040909     C*
116500040909     C   22              leave
116600040909      *
116700040909     C                   clear                   bltdcm
116800050215      *
116900050215      * x Estero : (da TRTC81R in partenza)
117000050215      *   se inviati gli Stati  IFTSTA di consegna il flag di trasm. è =>"C"
117100050215      *   e deve essere reimpostato =>"R"
117200050215     c                   if        bltftr = 'C'
117300050215     C                   move      'R'           bltftr
117400050215     c                   end
117500050215      *
117600040909     c                   update    fnblt000
117700040909      *
117800040909     C                   enddo
117900040909     C                   endsr
118000040825     C**---------------------------------------------------------
118100040825     C** CONTROLLO SE DEVO TRASMETTERE I DATI AL CLIENTE
118200040825     C**---------------------------------------------------------
118300040825     C     DATCLI        BEGSR
118400040825      * SE IL CLIENTE VUOLE RICEVERE DATI CONSEGNA SFLEGGO CAMPI TRASMISS.
118500040825     C                   SETOFF                                       32
118600040825     C     BLPCCM        IFNE      0
118700040825     C                   Z-ADD     1             W
118800040825     C     BLPCCM        LOOKUP    CLI(W)                                 32
118900040825     C                   END
119000040825     C  N32BLPKSC        IFNE      0
119100040825     C                   Z-ADD     1             W
119200040825     C     BLPKSC        LOOKUP    CLI(W)                                 32
119300040825     C                   END
119400060214     C* SE ANCORA NON TROVATO CHAIN SU ar4 PER VERIFICARE SE ESISTE IL
119500040825      *CODICE DEL CLIENTE NEL CAMPO NOTE
119600040825     C  N32              DO
119700040909     c                   move      'Z'           trec4
119800060214     C     KBL4          CHAIN     Fiar401L                           66
119900040825     C                   IF        not *in66
120000060214     C                   MOVEL     ar4NOT        KSCJ              7 0
120100040825     C                   Z-ADD     1             W
120200040825     C     KSCJ          LoOKUP    CLi(W)                                 32
120300040825     C                   END
120400040825     C                   END
120500040825      *
120600040825     C   32              MOVEL     *BLANK        BLPFT3
120700040825     C   32              MOVE      *ZEROS        BLPDT3
120800040825     C*
120900040825     C                   ENDSR
121000040825     C**---------------------------------------------------------
121100040825     C** AGGIORNO BOLLA IN sede PER RIPRISTINO SPEDIZIONE
121200040825     C**---------------------------------------------------------
121300040825     C     bollesede     BEGSR
121400040825     c     kbol          chain     titas30c                           21
121500040909      *  SE LE DISTINTE COINCIDONO.
121600040825     C     *in21         IFEQ      '0'
121700041213     C*    tasndc        ANDEQ     v01ndc
121800040825     C                   Z-ADD     0             TASDCM                         DATA CONSEGNA
121900040825     C                   Z-ADD     0             TASHMC                         ORA CONSEGNA
122000040825     C                   Z-ADD     0             TASNDC                         NR.DIST.CONS.
122100040825     C                   Z-ADD     0             TASPDC                         NR.DIST.CONS.
122200040825     C                   Z-ADD     0             TASDDC                         DATA.DIST.CONS.
122300040825     C* AGGIORNO TITAS000
122400040825     C   10              EXCEPT    AGGTAS
122500040825     C* AGGIORNO TITAS010
122600040825     C   11              EXCEPT    AGGTA1
122700040825     C* AGGIORNO TITASP00
122800040825     C   12              EXCEPT    AGGTAP
122900040825     C                   SETOFF                                       101112
123000040825     C                   END
123100040825     C*
123200040825     C                   ENDSR
123300040825     C**---------------------------------------------------------
123400040825     C** verifica se evento mic è già trasmesso altrimenti flegga
123500040825     C**---------------------------------------------------------
123600040825     C     eventomic     BEGSR
123700040825     c     kbol          setgt     fnevb05l
123800040825     c                   do        *hival
123900040825     c     kbol          readpe    fnevb05l                               21
124000040825     C   21              leave
124100040825     c                   if        xevbcev <> 'MIC'
124200040825     c                   iter
124300040825     c                   endif
124400040825     c                   if        xevbnot <> *blank
124500131104     c                   movel     xevbnot       devb01
124600040825      * se esiste il numero distinta ed è uguale a quello di ARB e non è
124700040825      * ancora trasmesso lo fleggo come se lo fosse stato x non aggiornare
124800040825      * tas e blp in seguito
124900131104     c                   if        §notNDC = %editc(v01ndc:'X')
125000131104     c                                  and xevbdtr = 0
125100040825     c                   z-add     dateu         xevbdtr
125200040825     c                   move      'T'           xevbftr
125300040825     c                   except    aggeve5
125400040825     c                   endif
125500040825     c                   endif
125600040825
125700040825     c                   enddo
125800040825
125900040825     C                   ENDSR
126000040825     C*-----------------------------------------------------------
126100940218     C* OPERAZIONI INIZIALI
126200040825     C*-----------------------------------------------------------
126300940218?    C     *INZSR        BEGSR
126400940218     C*
126500940218     C     *ENTRY        PLIST
126600940218     C                   PARM                    KPJBA
126700020826     c*
126800020826     c     *dtaara       define    §azute        azuteds
126900020826     c     *dtaara       define    §datiute      ddatiute
127000020826     C                   in(E)     *dtaara
127100020826     C                   IF        %Error  or  RSUT = *blanks
127200020826     C                   call      'TIBS34R'
127300020826     C                   parm                    Tibs34Ds
127400020826     C                   in        *dtaara
127500020826     c                   ENDIF
127600020827     c                   z-add     dutpou        v01fgs
127700020924      * se primo livello accende il 78 per il cambio di P.O. a video con F18
127800020924     c     dutlpo        comp      '1'                                    78
127900020826     C*----------------------------------------------------*
128000940218     C                   Z-ADD     1             CODUT             1 0
128100940324     C                   CALL      'X§PARUT'
128200940324     C                   PARM                    UT§DSE
128300940324     C                   MOVEL     RAGUT         RSUT
128400000104     C* DATA A 8
128500000104     C                   MOVEL     DTU(10)       W0020             2 0
128600000104     C                   Z-ADD     DTU(10)       WDTU10
128700000104     C     W0020         IFGE      60
128800000104     C                   MOVEL     19            WDTU10
128900000104     C                   ELSE
129000000104     C                   MOVEL     20            WDTU10            8 0
129100000104     C                   ENDIF
129200940218     C*---------------------------------------------------------------*
129300940218     C* GIRO UDATE
129400941111     C                   TIME                    WHHDAT           14 0
129500941111     C                   MOVE      WHHDAT        G02DAT
129600940218     C                   MOVEL     *BLANK        G02ERR
129700941111     C                   CALL      'XSRDA8'
129800941111     C                   PARM                    WLBDA8
129900941111     C                   Z-ADD     G02INV        DATEU             8 0
130000940218     C                   Z-ADD     G02TGI        OGGI              5 0
130100950113     C                   Z-ADD     0             KFRG
130200941111     C                   MOVEL     'FNLR56R'     VIDPGM
130300940218      *---------------------------------------------------------------*
130400140611     C     KCSS          KLIST
130500120321     C                   KFLD                    CSSAAS
130600120321     C                   KFLD                    CSSLNP
130700120321     C                   KFLD                    CSSNRS
130800120321     C                   KFLD                    CSSNSP
130900120321     C     KCSSOa        KLIST
131000120321     C                   KFLD                    KLNA              3 0
131100120321     C                   KFLD                    CSSLNO
131200120321     C                   KFLD                    CSSAAO
131300120321     C                   KFLD                    CSSNRO
131400120321     C                   KFLD                    CSSNSO
131500120321     C     KCSSO         KLIST
131600120321     C                   KFLD                    CSSAAO
131700120321     C                   KFLD                    CSSLNO
131800120321     C                   KFLD                    CSSNRO
131900120321     C                   KFLD                    CSSNSO
132000951211     C     KANM          KLIST
132100951211     C                   KFLD                    KAAS
132200951211     C                   KFLD                    KLNP
132300951211     C                   KFLD                    KNRS
132400951211     C                   KFLD                    KNSP
132500951211     C                   KFLD                    KCAA
132600960507     C     KANM2         KLIST
132700960507     C                   KFLD                    KFLS
132800960507     C                   KFLD                    KLNA
132900960507     C                   KFLD                    KNRS
133000960507     C                   KFLD                    KSCN
133100940218     C     KTAB          KLIST
133200940218     C                   KFLD                    CODUT
133300940218     C                   KFLD                    TBLCOD
133400940218     C                   KFLD                    TBLKEY
133500940323     C     KTAB1         KLIST
133600940323     C                   KFLD                    CODUT
133700940323     C                   KFLD                    TBLCOD
133800940218     C     KARB1         KLIST
133900940218     C                   KFLD                    ARBAAS
134000940218     C                   KFLD                    ARBLNP
134100940218     C                   KFLD                    ARBNRS
134200940218     C                   KFLD                    ARBNSP
134300161122     C     KARt          KLIST
134400161122     C                   KFLD                    ARtAAS
134500161122     C                   KFLD                    ARtLNP
134600161122     C                   KFLD                    ARtNRS
134700161122     C                   KFLD                    ARtNSP
134800161122     C                   KFLD                    ARtNSc
134900140911     C     KAR6          KLIST
135000140911     C                   KFLD                    ARBAAS
135100140911     C                   KFLD                    ARBLNP
135200140911     C                   KFLD                    ARBNRS
135300140911     C                   KFLD                    ARBNSP
135400140911     c                   kfld                    ktrc
135500990921     C     KARI          KLIST
135600990921     C                   KFLD                    ARBAAS
135700990921     C                   KFLD                    ARBLNP
135800990921     C                   KFLD                    ARBNRS
135900990921     C                   KFLD                    ARBNSP
136000990921     C                   KFLD                    KTIP
136100990204     C     KDCT          KLIST
136200990204     C                   KFLD                    ARBAAS
136300990204     C                   KFLD                    ARBLNP
136400990204     C                   KFLD                    ARBNRS
136500990204     C                   KFLD                    ARBNSP
136600950113     C     KGCA          KLIST
136700950113     C                   KFLD                    ARBAAS
136800950113     C                   KFLD                    ARBLNP
136900950113     C                   KFLD                    ARBNRS
137000950113     C                   KFLD                    ARBNSP
137100950113     C                   KFLD                    KFRG
137200940218     C     KCNA          KLIST
137300960108     C                   KFLD                    KLNA              3 0
137400940218     C                   KFLD                    ARBLNP
137500940218     C                   KFLD                    ARBAAS
137600940218     C                   KFLD                    ARBNRS
137700940218     C                   KFLD                    ARBNSP
137800940218     C     KFVV          KLIST
137900940218     C                   KFLD                    FVVNPG
138000940218     C                   KFLD                    FVVNFV
138100020826     C                   KFLD                    v01fgs
138200940218     C                   Z-ADD     4             FVVNPG
138300140303     C     Kdst          KLIST
138400140303     C                   KFLD                    fvvnpg
138500140303     C                   KFLD                    dstNFV
138600140303     C                   KFLD                    dstfgs
138700940218     C*
138800940218     C     KLBL          KLIST
138900940218     C                   KFLD                    LBLAAP
139000940218     C                   KFLD                    LBLLPP
139100940218     C                   KFLD                    LBLNRP
139200940218     C                   KFLD                    LBLNSP
139300940218     C*
139400940218     C     KARB2         KLIST
139500940218     C                   KFLD                    LBLAAN
139600940218     C                   KFLD                    LBLLPN
139700940218     C                   KFLD                    LBLNRN
139800940218     C                   KFLD                    LBLNSN
139900940218     C*
140000940218     C     KFTT          KLIST
140100940218     C                   KFLD                    FTTPDR
140200940218     C                   KFLD                    FTTTSR
140300940218     C                   KFLD                    FTTNDC
140400940218     C*
140500940218     C     KFTD          KLIST
140600940218     C                   KFLD                    FTDAAS
140700940218     C                   KFLD                    FTDLNP
140800940218     C                   KFLD                    FTDNRS
140900940218     C                   KFLD                    FTDNSP
141000140303     C                   KFLD                    wTSR              1
141100140303     C                   KFLD                    ftdPDr
141200140303     C                   KFLD                    ftdndc
141300140303     C                   move      'C'           wTSR
141400071106     C*
141500071106     C     KFce          KLIST
141600071106     C                   KFLD                    FtdFGS
141700071106     C                   KFLD                    FtdPDR
141800071106     C                   KFLD                    FtdTSR
141900071106     C                   KFLD                    FtdNDC
142000071106     C                   KFLD                    FtdDDC
142100071106     C                   KFLD                    FtdAAS
142200071106     C                   KFLD                    FtdLNP
142300071106     C                   KFLD                    FtdNRS
142400071106     C                   KFLD                    FtdNSP
142500021204     C*
142600970401     C     KAR4          KLIST
142700970401     C                   KFLD                    ARBAAS
142800970401     C                   KFLD                    ARBLNP
142900970401     C                   KFLD                    ARBNRS
143000970401     C                   KFLD                    ARBNSP
143100970401     C                   KFLD                    KTRC
143200130725     C*
143300130725     C     KAR5          KLIST
143400130725     C                   KFLD                    ARBAAS
143500130725     C                   KFLD                    ARBLNP
143600130725     C                   KFLD                    ARBNRS
143700130725     C                   KFLD                    ARBNSP
143800130725     C                   KFLD                    KTRd              3
143900021204     C*
144000021204     C     KApd          KLIST
144100021204     C                   KFLD                    apdtip
144200140611     C                   KFLD                    arbpdc
144300021204     c                   movel     'A'           apdtip
144400040825     C*
144500040825     C     kbl4          klist
144600040825     C                   kfld                    blpaas
144700040825     C                   kfld                    blplnp
144800040825     C                   kfld                    blpnrs
144900040825     C                   kfld                    blpnsp
145000040909     C                   kfld                    trec4             1
145100040909
145200040825     C     Kbol          klist
145300040825     C                   kfld                    arbaas
145400040825     C                   kfld                    arblnp
145500040825     C                   kfld                    arbnrs
145600040825     C                   kfld                    arbnsp
145700941214     C*-------------------------------------------------------*
145800140303     C*  Caricamento £1
145900140303     c                   clear                   trul06ds
146000140303     c                   eval      d06cod = '£1'
146100140303     c                   movel     dutpou        d06key
146200140303     c                   movel     trul06ds      kpjbu
146300140303     c                   call      'TRUL06R'
146400140303     c                   parm                    kpjba
146500140303     c                   movel     kpjbu         trul06ds
146600940324     C* Verifico se evento Ripristino da memorizzare
146700940323     C                   MOVEL     '2A'          TBLCOD
146800940323     C                   MOVEL     *BLANKS       TBLKEY
146900940323     C                   MOVEL     'RIP'         TBLKEY
147000940324     C                   MOVEL     ' '           WRTRIP            1
147100940323     C                   CLEAR                   TBLUNI
147200940324     C     KTAB          CHAIN     TABEL                              11
147300940323     C                   MOVEL     TBLUNI        DS2A
147400940324     C     §2AEVB        IFEQ      'S'
147500940324     C                   MOVEL     'S'           WRTRIP
147600940324     C                   END
147700940324     C* Verifico se evento Ripristino dopo conteggi da memorizzare
147800940324     C                   MOVEL     'RDC'         TBLKEY
147900940324     C                   MOVEL     ' '           WRTRDC            1
148000940324     C                   CLEAR                   TBLUNI
148100940324     C     KTAB          CHAIN     TABEL                              11
148200940324     C                   MOVEL     TBLUNI        DS2A
148300940324     C     §2AEVB        IFEQ      'S'
148400940324     C                   MOVEL     'S'           WRTRDC
148500940324     C                   END
148600940323     C*
148700940324     C*  Carico dati tabella 3a
148800940323     C                   MOVEL     '3A'          TBLCOD
148900940323     C                   Z-ADD     0             X
149000940324     C     KTAB1         CHAIN     TABEL00F                           11
149100940324     C     *IN11         DOWEQ     '0'
149200940323     C     X             ANDLT     50
149300940323     C                   ADD       1             X
149400941111     C                   MOVEL     TBLUNI        D3A(X)
149500940323     C                   MOVEL     TBLKEY        C3A(X)
149600940324     C     KTAB1         READE     TABEL00F                               11
149700940323     C                   END
149800971007     C*
149900960507     C* Verifico se evento Ripristino da memorizzare
150000960507     C                   MOVEL     '7A'          TBLCOD
150100960507     C                   MOVEL     *BLANKS       TBLKEY
150200960507     C                   MOVEL     '2'           TBLKEY
150300960507     C                   CLEAR                   TBLUNI
150400960507     C     KTAB          CHAIN     TABEL                              11
150500960507     C                   MOVEL     TBLUNI        DS7A2
150600040825     C*                                                    *
150700040825     C* CARICO I CODICI CLIENTI
150800040825     C                   MOVE      1             TBLKUT
150900040825     C                   MOVEL     '3K'          TBLCOD
151000040825     C                   MOVEL     *BLANKS       TBLKEY
151100040825     C                   MOVE      *ZEROS        W                 4 0
151200040825     C     Ktab1         SETLL     TABEL00F
151300040825     C     *IN31         DOUEQ     '1'
151400040825     C     Ktab1         READE     TABEL00F                               31
151500040825     C  N31              DO
151600040825     C                   MOVEL     TBLUNI        DS3K
151700040825     C                   ADD       1             W
151800040825     C                   MOVEL     TBLKEY        CLI(W)
151900040825      *
152000040825     C                   END
152100040825     C                   END
152200940324     C*
152300940324     C*  Definisco varibile per accesso AZORG
152400940324     C     *LIKE         DEFINE    ARBLNP        WLIN
152500950113     C     *LIKE         DEFINE    GCPFRG        KFRG
152600951211     C     *LIKE         DEFINE    ANMAAS        KAAS
152700951211     C     *LIKE         DEFINE    ANMLNP        KLNP
152800951211     C     *LIKE         DEFINE    ANMNRS        KNRS
152900951211     C     *LIKE         DEFINE    ANMNSP        KNSP
153000971007     C     *LIKE         DEFINE    ANMCAA        KCAA
153100960507     C     *LIKE         DEFINE    ANMFLS        KFLS
153200960507     C     *LIKE         DEFINE    ANMSCN        KSCN
153300970401     C     *LIKE         DEFINE    AR4TRC        KTRC
153400990921     C     *LIKE         DEFINE    ARITIP        KTIP
153500010806     C     *LIKE         DEFINE    ARBPDC        SAVPDC
153600940323     C*-------------------------------------------------------*
153700040825      * ovr per titas
153800040825     c                   Clear                   comman
153900040825     c                   Movel(p)  cmd(1)        comman
154000040825     c                   Eval      %Subst(comman:13:8) = 'TITAS30C'
154100040910      *test sistema informativo di prova o buono x libreria TITAS
154200040910     c                   if        %subst(knsif:7:1) = 'P'
154300040910     c                   Eval      %Subst(comman:30:10) = wlibp
154400040910     c                   else
154500040910     c                   Eval      %Subst(comman:30:10) = wlib
154600040910     c                   endif
154700040910
154800040825     c                   Eval      comman =
154900040825     c                             %trim(comman) + '/TITAS30C)'
155000040825     c                   Call      'QCMDEXC'                            99
155100040825     c                   Parm                    comman
155200040825     c                   Parm                    lenght
155300040825     c                   If        *In99
155400040825     c                   seton                                        lr
155500040825     c                   Else
155600040825     c                   Open      TITAS30C
155700040825     c                   EndIf
155800040825     c
155900940218?    C                   ENDSR
156000930422     C*========================================================================*
156100040825     OTITAS000  E            AGGTAS
156200040825     O                       TASDCM
156300040825     O                       TASHMC
156400040825     O                       TASNDC
156500040825     O                       TASPDC
156600040825     O                       TASDDC
156700040825     OTITAS010  E            AGGTA1
156800040825     O                       TASDCM
156900040825     O                       TASHMC
157000040825     O                       TASNDC
157100040825     O                       TASPDC
157200040825     O                       TASDDC
157300040825     OTITASP00  E            AGGTAP
157400040825     O                       TASDCM
157500040825     O                       TASHMC
157600040825     O                       TASNDC
157700040825     O                       TASPDC
157800040825     O                       TASDDC
157900040825     Ofnevb5    E            AGGeve5
158000040825     O                       xevbdtr
158100040825     O                       xevbftr
158200010806**
158300950419Numero distinta inesistente oppure non chiusa                                01
158400950419Data non valida                                                              02
158500950419Nessun collo consegnato in questa data !!!                                   03
158600950419Linea errata !!!                                                             04
158700950419Spedizione consegnata/in consegna in altra distinta !!!                      05
158800950419Spedizione inesistente !!!                                                   06
158900950419Spedizione già contabilizzata o legata ad altra bolla                        07
159000950419Immettere numero spedizione !!!                                              08
159100950419Attenzione !! Richiesto ripristino spedizione in distinta già conteggiata    09
159200950419Spedizione con rimborso impossibile effettuare riprisitino !!!               10
159300950419Spedizione consegnata da oltre 15 gg. impossibile effettuare ripristino      11
159400950419Spedizione con giacenza già in fase di chiusura:impossibile ripristino       12
159500970228Spedizione non ripristinabile: colli chiusi con pratica IDD                  13
159600990204Attenzione !! Richiesto ripristino spedizione con pratica C.A. aperta        14
159700010806Spedizione che non ha il flag di distinta chiusa                             15
159800120322Sono presenti altre spedizioni/assegni. F10 per forzare                      16
159900140303Filiale non in gestione                                                      17
159901170522Spedizione legata a bolla seguente non ripristinabile                        18
160000040825** cmd
160100040825OVRDBF FILE(        ) TOFILE(
