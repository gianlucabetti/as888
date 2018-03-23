000100950927     H DECEDIT('0,') DATEDIT(*DMY.)
000200950927?     *--------------------------------------------------------------*
000300041122      * FIDN69R                                                      *
000400950927      *     *----------------------------------------------*         *
000500041122      *         STATISTICA NR INVII PREAVVISI DI DANNO               *
000600041122      *                 nel periodo richiesto                        *
000700950927      *     *---------------------------------------------*          *
000800950927      *                                                              *
000900950927?     *--------------------------------------------------------------*
001000950927      * KC - FINE LAVORO                                             *
001100950927      * KF - CONFERMA                                                *
001200950927?     *--------------------------------------------------------------*
001300950928      * 28 - Visualizzazione messaggio di errore                     *
001400950928      * 30 - Indicatore per chain generiche                          *
001500950929      * 31 - Call TRUL49C                                            *
001600041122      * 40 - Reverse Immage su data da                               *
001700041122      * 41 - Reverse Immage su data a                                *
001800950928      * 42 - Reverse Immage su codice cliente                        *
001900950928?     *--------------------------------------------------------------*
002000040927     fkfazn11l  if   E           k disk
002100040927     F*--------
002200950927     FCNACO00F  IF   E           K DISK
002300960209     F*--------
002400960209     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002500950927     F*--------
002600041122     FFIDN69D   CF   E             WORKSTN
002700950927     D*---------------------------------------------------------------*
002800950927     D* SCHIERA
002900950927     D*---------------------------------------------------------------*
003000020502     D £1              S              3  0 DIM(30)
003100960209     D ERR             S             70    DIM(17) CTDATA PERRCD(1)
003200950928     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
003300950927?     *--------------------------------------------------------------*
003400950927?     *  DS                                                          *
003500950927?     *--------------------------------------------------------------*
003600950927     D KPJBA         E DS
003700950927     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003800950927     D  TCU                  398    697
003900950928     D                                     DIM(50)
004000950927     D  KCU                  698    847P 0
004100950928     D                                     DIM(50)
004200950928     D                                     PACKEVEN
004300950927     D TCUDS           DS
004400950927     D  F34                    3      4
004500950927     D  F56                    5      6
004600950927     D CNCR80        E DS
004700950927      *
004800041122     D FIDN69ds      E DS
004900041122     D  lnpd69                24    113  0 dim(30)
005000040928     D
005100020502     d trul06ds      e ds
005200020502     d  lin                    1     90  0 dim(30)
005300040928     D Tibs36ds      E DS
005400040927      *
005500040927      *
005600950928     D WLBDA8          DS
005700950928     D  G02DAT                 1      8  0
005800950928     D  G02INV                 9     16  0
005900950928     D  G02ERR                17     17
006000950928     D  G02TGI                18     22  0
006100040927      *
006200950929     D WGIDAT          DS
006300950929     D  GIODAT                 1      8  0
006400950929     D  GIOINV                 9     16  0
006500950929     D  GIOTGI                17     21  0
006600950927     D                SDS
006700950927     D  NOMPGM                 1     10
006800960209?     *--------------------------------------------------------------*
006900960209?     *  Organigramma di filiale                                     *
007000960209?     *--------------------------------------------------------------*
007100960209     IAZORG01L  AA
007200960209     I                             P    4    5 0ORGFIL
007300960209     I                                 14   33  ORGDES
007400950927?     *--------------------------------------------------------------*
007500950927?     *  CICLO PRINCIPALE                                            *
007600950927?     *--------------------------------------------------------------*
007700950927      *  Impostazione parametri prima videata
007800950927     C                   EXSR      INZ01
007900040927      *
008000950927      *  Loop gestione videata
008100950927     C     WFINE         DOUEQ     'S'
008200950927     C                   EXSR      GESD01
008300950927     C                   END
008400950927      *  Fine Lavoro
008500950927     C     FINE          TAG
008600950927     C                   SETON                                        LR
008700950927?     *--------------------------------------------------------------*
008800950927?     *  INZ01: Inizializzazione prima videata                       *
008900950927?     *--------------------------------------------------------------*
009000950927     C     INZ01         BEGSR
009100950927      *
009200040928     C                   setoff                                       404142
009300040928     C                   SETOFF                                       284344
009400040928     C                   MOVEL     RAGUT         RSUT
009500040928     c                   move      *zeros        d1ddat
009600040928     c                   move      *zeros        d1adat
009700040928     c                   move      *zeros        d1csup
009800040928     c                   move      *all'9'       d1clnp
009900040928     c                   move      *all'9'       d1cksc
010000040928     c                   move      *all'9'       d1ct01
010100040928     c                   move      *blank        d1ct02
010200040928     c                   move      *blank        d1ct03
010300950928     C                   MOVEL     '1'           WTPVID            1
010400041122     C                   clear                   FIDN69ds
010500950928     C*
010600950928     C                   ENDSR
010700950928?     *--------------------------------------------------------------*
010800950928?     *  GESD01: Gestione prima videata                              *
010900950928?     *--------------------------------------------------------------*
011000950928     C     GESD01        BEGSR
011100950928      *
011200041122     C                   EXFMT     DN69d01
011300950928     C                   SETOFF                                       404142
011400040928     C                   SETOFF                                       434428
011500950928      *  Fine Lavoro
011600950928     C     *INKC         IFEQ      '1'
011700950928     C                   MOVEL     'S'           WFINE
011800950928     C                   GOTO      FINVD1
011900950928     C                   END
012000950928      *  Controlli
012100950928     C                   EXSR      CTR01
012200950928     C   28              GOTO      FINVD1
012300040927      *
012400041122      *  Conferma: Sbm. FIDN70R
012500951002     C     *INKF         IFEQ      '1'
012600041122     C                   EXSR      SBMP70
012700950928     C                   EXSR      INZ01
012800950928     C                   END
012900950928      *
013000950928     C     FINVD1        ENDSR
013100950928?     *--------------------------------------------------------------*
013200950928?     *  CTR01: Controlli prima videata                              *
013300950928?     *--------------------------------------------------------------*
013400950928     C     CTR01         BEGSR
013500950928      *
013600040928      *  Controllo data dal
013700040928     C     d1ddat        IFEQ      0
013800950928     C                   SETON                                        4028
013900950928     C                   MOVEL     ERR(1)        $MSG
014000950928     C                   GOTO      FINCT1
014100950928     C                   ELSE
014200040928     C                   Z-ADD     d1ddat        G02DAT
014300950928     C                   Z-ADD     0             G02INV
014400950928     C                   MOVEL     *BLANKS       G02ERR
014500950928     C                   CALL      'XSRDA8'
014600950928     C                   PARM                    WLBDA8
014700950928     C     G02ERR        IFEQ      '1'
014800950928     C                   SETON                                        4028
014900950928     C                   MOVEL     ERR(2)        $MSG
015000950928     C                   GOTO      FINCT1
015100950928     C                   ELSE
015200950928     C                   Z-ADD     G02INV        WDATDA
015300040928     C                   Z-ADD     G02DAT        d1ddat
015400950928     C                   END
015500950928     C                   END
015600950928     C     WDATDA        IFGT      WOGGI
015700950928     C                   SETON                                        4028
015800950928     C                   MOVEL     ERR(3)        $MSG
015900950928     C                   GOTO      FINCT1
016000950928     C                   END
016100040928      *  Controllo data al
016200040928     C     d1adat        IFEQ      0
016300950928     C                   SETON                                        4128
016400950928     C                   MOVEL     ERR(1)        $MSG
016500950928     C                   GOTO      FINCT1
016600950928     C                   ELSE
016700040928     C                   Z-ADD     d1adat        G02DAT
016800950928     C                   Z-ADD     0             G02INV
016900950928     C                   MOVEL     *BLANKS       G02ERR
017000950928     C                   CALL      'XSRDA8'
017100950928     C                   PARM                    WLBDA8
017200950928     C     G02ERR        IFEQ      '1'
017300950928     C                   SETON                                        4128
017400950928     C                   MOVEL     ERR(2)        $MSG
017500950928     C                   GOTO      FINCT1
017600950928     C                   ELSE
017700950928     C                   Z-ADD     G02INV        WDATAA
017800040928     C                   Z-ADD     G02DAT        d1adat
017900950928     C                   END
018000950928     C                   END
018100950928     C     WDATAA        IFGT      WOGGI
018200950928     C                   SETON                                        4128
018300950928     C                   MOVEL     ERR(3)        $MSG
018400950928     C                   GOTO      FINCT1
018500950928     C                   END
018600040927      *
018700950928      *  Controllo limiti fra due date
018800950928     C     WDATDA        IFGT      WDATAA
018900950928     C                   SETON                                        404128
019000950928     C                   MOVEL     ERR(4)        $MSG
019100950928     C                   GOTO      FINCT1
019200950928     C                   END
019300040927      *
019400040927      * ---------------------------
019500040928     c                   if        d1clnp=0 or d1clnp=999
019600040928     c                   eval      d1clnp=999
019700040928     c                   eval      d1dlnp=*blank
019800040928     c                   end
019900040928      *
020000040928     c                   if        d1cksc=0
020100040928     c                   eval      d1cksc=*all'9'
020200040928     c                   eval      d1dksc=*blank
020300040928     c                   end
020400040928      *
020500951002     C                   SETOFF                                       42
020600040928      *
020700040928      *  Controllo se immessa lnp
020800040928     C     d1clnp        ifne      999
020900040928     C     d1clnp        andne     0
021000040928     C                   EXSR      CTRLNP
021100040928     C                   END
021200040928     C   28              GOTO      FINCT1
021300040928      *
021400040928      *  Controllo se immesso cliente
021500040928     C     d1cKSC        ifne      *all'9'
021600040928     C     d1cKSC        andne     0
021700040928     C     d1dKSC        orne      *blank
021800960209     C                   EXSR      CTRCLI
021900960209     C                   END
022000040930     C*
022100040930     C*
022200950928     C     FINCT1        ENDSR
022300960209?     *--------------------------------------------------------------*
022400960209?     *  CTRCLI: Controllo dati cliente                              *
022500960209?     *--------------------------------------------------------------*
022600960209     C     CTRCLI        BEGSR
022700960209      *
022800960209      *  Controllo se richiesta ricerca alfabetica cliente
022900040928     C     d1CKSC        IFEQ      0
023000040928     C     d1CKSC        oreq      *all'9'
023100040928     C     d1DKSC        ANDNE     *BLANKS
023200040928     C                   MOVEL     d1DKSC        ACORAG
023300960209     C                   Z-ADD     CLIITA        ACOKCC
023400960209     C                   Z-ADD     0             ACOKSC
023500960209     C                   CALL      'XALFA1'
023600960209     C                   PARM                    PARDUT
023700960209     C                   PARM                    PARKUT
023800960209     C                   PARM                    ACORAG
023900960209     C                   PARM                    ACOKCC
024000960209     C                   PARM                    ACOKSC
024100960209     C                   PARM                    PARSTA            1 0
024200961016     C                   PARM                    PARFLR           90
024300960209     C     PARSTA        IFEQ      0
024400040928     C                   Z-ADD     ACOKSC        d1CKSC
024500040928     C                   eval      d1DKSC = ACORAG
024600960209     C                   END
024700960209     C                   END
024800040927     **
024900960209      *  Controllo validità conto cliente
025000040928     C                   MOVEL     *BLANKS       d1DKSC
025100040928     C     d1CKSC        IFNE      0
025200960209     C                   Z-ADD     1             KKUT
025300960209     C                   Z-ADD     CLIITA        KKCC
025400040928     C                   Z-ADD     d1CKSC        KKSC
025500960209     C     KACO          CHAIN     CNACO00F                           30
025600960209     C     *IN30         IFNE      '0'
025700960209     C     ACOFLG        ORNE      *BLANKS
025800960209     C     ACOABL        ORNE      *BLANKS
025900130315     C***  ACOABL        ANDNE     *ZEROS
026000960209     C  N28              MOVEL     ERR(6)        $MSG
026100960209     C                   SETON                                        4228
026200960209     C                   END
026300040928     C                   MOVEL     ACORAG        d1DKSC
026400960209     C                   END
026500960209     C*
026600960209     C                   ENDSR
026700960209?     *--------------------------------------------------------------*
026800960209?     *  CTRLNP: Controllo dati linea di partenza                    *
026900960209?     *--------------------------------------------------------------*
027000960209     C     CTRLNP        BEGSR
027100960209      *
027200040928      *  Controllo validità liNea di partenza
027300040928     C     d1Clnp        IFNE      0
027400040928     C                   Z-ADD     D1Clnp        ORGFIL
027500960209     C     ORGFIL        CHAIN     AZORG01L                           30
027600960209     C     *IN30         IFNE      '0'
027700960209     C  N28              MOVEL     ERR(16)       $MSG
027800040928     C                   SETON                                        4328
027900960209     C                   END
028000040928     C                   MOVEL     ORGDES        d1Dlnp
028100960212     C     ORGFIL        LOOKUP    £1                                     30
028200040928      **
028300960212     C     *IN30         IFEQ      '0'
028400040928     C     O36POU        ANDNE     046
028500960209     C  N28              MOVEL     ERR(17)       $MSG
028600040928     C                   SETON                                        4328
028700960209     C                   END
028800960209     C                   END
028900960209     C*
029000960209     C                   ENDSR
029100950928?     *--------------------------------------------------------------*
029200041122?     *  SBMP70: Sottometto stampa statistica                        *
029300950928?     *--------------------------------------------------------------*
029400041122     C     SBMP70        BEGSR
029500040927      *
029600041122     c                   clear                   fidn69ds
029700041122     C                   Z-ADD     WDATDA        D69dsd
029800041122     C                   Z-ADD     WDATAA        D69dsa
029900040928     c                   if        d1clnp <> 999
030000041122     C                   z-add     d1clnp        D69l01
030100040928     c                   else
030200040928     c                   if        o36pou <> 046
030300041122     c                   movea     £1            lnpd69
030400040928     c                   else
030500041122     C                   z-add     046           D69l01
030600040928     c                   end
030700040928     c                   end
030800041122     C                   z-add     d1cksc        D69ksc
030900041122     C                   move      d1dksc        D69rag
031000041122     C                   move      d1ct01        D69t01
031100041122     C                   move      d1ct02        D69t02
031200041122     C                   move      d1ct03        D69t03
031300041122     C                   move      d1csup        D69sup
031400041122     C   77              move      'S'           D69sed
031500041122     C  N77              move      'F'           D69sed
031600041122     C                   MOVEL     Fidn69ds      KPJBU
031700041005     c                   move      prtsql        kpjbu
031800040927      *
031900041122     C                   MOVEL     'DN70'        KCOAZ
032000040927     c     kcoaz         chain     kfazn11l
032100040927     c                   if        not %Found(kfazn11l) or
032200040927     c                             knmus = *all'1'
032300041122     C                   CALL      'FIDN70C'
032400040927     C                   PARM                    KPJBA
032500040927     C                   else
032600950928     C                   CALL      'BCH10'
032700950928     C                   PARM                    KPJBA
032800950928     C                   END
032900950928     C*
033000950928     C                   ENDSR
033100950928?     *--------------------------------------------------------------*
033200950928?     *  *INZSR: Operazioni iniziali                                 *
033300950928?     *--------------------------------------------------------------*
033400950928     C     *INZSR        BEGSR
033500950928     C*
033600950928     C     *ENTRY        PLIST
033700950928     C                   PARM                    KPJBA
033800041005     C                   move      kpjbu         prtsql            1
033900950928     C* Richiamo XPARUT
034000950928     C                   Z-ADD     1             CODUT
034100950928     C                   CALL      'X§PARUT'
034200950928     C                   PARM                    UTEDSE
034300960209     C                   MOVEL     REC80         CNCR80
034400040928      *
034500040928      *  In base all'utente prende il P.O.
034600040928     C                   CLEAR                   Tibs36Ds
034700040928     c                   EVAL      I36ute = Knmus
034800040928     c                   EVAL      I36Tla = 'L'
034900040928     C                   CALL      'TIBS36R'
035000040928     C                   PARM                    Tibs36Ds
035100040928      * se utente di sede
035200040928     c                   if        o36pou = 046
035300040928     c                   seton                                        77
035400040928     c                   end
035500040928     C*
035600950928     C*--- Ricerca Capoconti -----------------------------------------*
035700040927     C                   DO        50            X                 3 0
035800950928     C                   MOVE      TCU(X)        TCUDS
035900950928     C     F56           IFEQ      'CG'
036000950928     C     F34           ANDEQ     '01'
036100950928     C                   Z-ADD     KCU(X)        CLIITA            4 0
036200950928     C                   END
036300950928     C                   END
036400950928     C*  KLIST
036500950928     C*
036600950928     C     KACO          KLIST
036700950928     C                   KFLD                    KKUT
036800950928     C                   KFLD                    KKCC
036900950928     C                   KFLD                    KKSC
037000950928     C*  Definizione delle variabili
037100950928     C     *LIKE         DEFINE    ACOKUT        KKUT
037200950928     C     *LIKE         DEFINE    ACOKCC        KKCC
037300950928     C     *LIKE         DEFINE    ACOKSC        KKSC
037400040928     C     *LIKE         DEFINE    d1ddat        WDATDA
037500040928     C     *LIKE         DEFINE    d1adat        WDATAA
037600950928     C*  Inizializzazione
037700950928     C                   MOVEL     'N'           WFINE             1
037800950928     C                   MOVEL     RAGUT         PARDUT           30
037900950928     C                   Z-ADD     CODUT         PARKUT            1 0
038000950928     C                   TIME                    WHHDAT           14 0
038100040927      *
038200950928     C                   MOVE      WHHDAT        G02DAT
038300950928     C                   Z-ADD     0             G02INV
038400950928     C                   MOVEL     *BLANKS       G02ERR
038500950928     C                   CALL      'XSRDA8'
038600950928     C                   PARM                    WLBDA8
038700950928     C                   Z-ADD     G02INV        WOGGI             8 0
038800950929     C                   Z-ADD     G02TGI        WTODAY            5 0
038900040927      *
039000960209      * Reperisco filiali su AS
039100020730     C     SimTpP        IFNE      '2'
039200960209     C                   Z-ADD     1             KKUT
039300020502     c                   clear                   trul06ds
039400020502     c                   eval      d06cod = '£1'
039500040928     c                   movel     o36POU        d06key
039600020502     c                   movel     trul06ds      kpjbu
039700020502     c                   call      'TRUL06R'
039800020502     c                   parm                    kpjba
039900020502     c                   movel     kpjbu         trul06ds
040000020502     c                   movea     lin           £1
040100960212     C                   ELSE
040200040928     C                   MOVE      o36POU        £1(1)
040300960212     C                   END
040400950928      *
040500950928     C                   ENDSR
040600950928**
040700041122Data obbligatoria                                                     01
040800040927Data formalmente errata                                               02
040900040927Data maggiore della data del giorno                                   03
041000040927Limiti incompatibili: data iniziale maggiore della data finale        04
041100040928                                                                      05
041200950928Codice cliente inesistente o annullato                                06
041300041122                                                                      07
041400041122                                                                      08
041500041122Indicare un codice cliente e/o un punto operativo                     09
041600040927                                                                      10
041700040927                                                                      11
041800040927                                                                      12
041900040927                                                                      13
042000040927                                                                      14
042100040927                                                                      15
042200041122Punto operativo   inesitente                                          16
042300041122Punto operativo   non gestibile                                       17
042400950928**
042500950928312831303130313130313031
