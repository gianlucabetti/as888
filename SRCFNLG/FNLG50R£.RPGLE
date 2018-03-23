000100950927     H DECEDIT('0,') DATEDIT(*DMY.)
000200950927?     *--------------------------------------------------------------*
000300040927      * FNLG50R                                                      *
000400950927      *     *----------------------------------------------*         *
000500040927      *         STATISTICA NR GIACENZE APERTE PER CLIENTE            *
000600040927      *                 nel periodo di spedizione                    *
000700950927      *     *---------------------------------------------*          *
000800950927      *                                                              *
000900950927?     *--------------------------------------------------------------*
001000950927      * KC - FINE LAVORO                                             *
001100950927      * KF - CONFERMA                                                *
001200950927?     *--------------------------------------------------------------*
001300950928      * 28 - Visualizzazione messaggio di errore                     *
001400950928      * 30 - Indicatore per chain generiche                          *
001500950929      * 31 - Call TRUL49C                                            *
001600950928      * 40 - Reverse Immage su data spedizione da                    *
001700950928      * 41 - Reverse Immage su data spedizione a                     *
001800950928      * 42 - Reverse Immage su codice cliente                        *
001900950928?     *--------------------------------------------------------------*
002000040927     fkfazn11l  if   E           k disk
002100040927     F*--------
002200950927     FCNACO00F  IF   E           K DISK
002300950927     F*--------
002400950928     FTABEL00F  IF   E           K DISK    USROPN
002500960209     F*--------
002600960209     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002700950927     F*--------
002800040927     FFNLG50D   CF   E             WORKSTN
002900950927     D*---------------------------------------------------------------*
003000950927     D* SCHIERA
003100950927     D*---------------------------------------------------------------*
003200020502     D £1              S              3  0 DIM(30)
003300960209     D ERR             S             70    DIM(17) CTDATA PERRCD(1)
003400950928     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
003500950927?     *--------------------------------------------------------------*
003600950927?     *  DS                                                          *
003700950927?     *--------------------------------------------------------------*
003800950927     D KPJBA         E DS
003900950927     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004000950927     D  TCU                  398    697
004100950928     D                                     DIM(50)
004200950927     D  KCU                  698    847P 0
004300950928     D                                     DIM(50)
004400950928     D                                     PACKEVEN
004500950927     D TCUDS           DS
004600950927     D  F34                    3      4
004700950927     D  F56                    5      6
004800950927     D CNCR80        E DS
004900950927      *
005000040928     D FNLG50ds      E DS
005100040928     D  lnpd50                24    113  0 dim(30)
005200040928     D  tcm                  114    123    Dim(10)
005300040928     D
005400020502     d trul06ds      e ds
005500020502     d  lin                    1     90  0 dim(30)
005600040928     D Tibs36ds      E DS
005700040927      *
005800010726     D DS5A          E DS
005900040928     D DS$3          E DS
006000040927      *
006100950928     D WLBDA8          DS
006200950928     D  G02DAT                 1      8  0
006300950928     D  G02INV                 9     16  0
006400950928     D  G02ERR                17     17
006500950928     D  G02TGI                18     22  0
006600040927      *
006700950929     D WGIDAT          DS
006800950929     D  GIODAT                 1      8  0
006900950929     D  GIOINV                 9     16  0
007000950929     D  GIOTGI                17     21  0
007100950927     D                SDS
007200950927     D  NOMPGM                 1     10
007300960209?     *--------------------------------------------------------------*
007400960209?     *  Organigramma di filiale                                     *
007500960209?     *--------------------------------------------------------------*
007600960209     IAZORG01L  AA
007700960209     I                             P    4    5 0ORGFIL
007800960209     I                                 14   33  ORGDES
007900950927?     *--------------------------------------------------------------*
008000950927?     *  CICLO PRINCIPALE                                            *
008100950927?     *--------------------------------------------------------------*
008200950927      *  Impostazione parametri prima videata
008300950927     C                   EXSR      INZ01
008400040927      *
008500950927      *  Loop gestione videata
008600950927     C     WFINE         DOUEQ     'S'
008700950927     C                   EXSR      GESD01
008800950927     C                   END
008900950927      *  Fine Lavoro
009000950927     C     FINE          TAG
009100950927     C                   SETON                                        LR
009200950927?     *--------------------------------------------------------------*
009300950927?     *  INZ01: Inizializzazione prima videata                       *
009400950927?     *--------------------------------------------------------------*
009500950927     C     INZ01         BEGSR
009600950927      *
009700040928     C                   setoff                                       404142
009800040928     C                   SETOFF                                       284344
009900040928     C                   MOVEL     RAGUT         RSUT
010000040928     C                   MOVEL     o36dpo        d1DOVE
010100040928     c                   move      *zeros        d1ddat
010200040928     c                   move      *zeros        d1adat
010300040928     c                   move      *zeros        d1csup
010400040928     c                   move      *all'9'       d1clnp
010500040928     c                   move      *all'9'       d1cksc
010600040928     c                   move      *all'9'       d1ct01
010700040928     c                   move      *blank        d1ct02
010800040928     c                   move      *blank        d1ct03
010900040928     c                   move      *blank        d1ct04
011000040928     c                   move      *blank        d1ct05
011100040928     c                   move      *blank        d1ct06
011200040928     c                   move      *blank        d1ct07
011300040928     c                   move      *blank        d1ct08
011400040928     c                   move      *blank        d1ct09
011500040928     c                   move      *blank        d1ct10
011600950928     C                   MOVEL     '1'           WTPVID            1
011700040928     C                   clear                   FNLG50ds
011800950928     C*
011900950928     C                   ENDSR
012000950928?     *--------------------------------------------------------------*
012100950928?     *  GESD01: Gestione prima videata                              *
012200950928?     *--------------------------------------------------------------*
012300950928     C     GESD01        BEGSR
012400950928      *
012500040927     C                   EXFMT     LG50d01
012600950928     C                   SETOFF                                       404142
012700040928     C                   SETOFF                                       434428
012800950928      *  Fine Lavoro
012900950928     C     *INKC         IFEQ      '1'
013000950928     C                   MOVEL     'S'           WFINE
013100950928     C                   GOTO      FINVD1
013200950928     C                   END
013300950928      *  Controlli
013400950928     C                   EXSR      CTR01
013500950928     C   28              GOTO      FINVD1
013600040927      *
013700040927      *  Conferma: Sbm. FNLG51R
013800951002     C     *INKF         IFEQ      '1'
013900040927     C                   EXSR      SBMP51
014000950928     C                   EXSR      INZ01
014100950928     C                   END
014200950928      *
014300950928     C     FINVD1        ENDSR
014400950928?     *--------------------------------------------------------------*
014500950928?     *  CTR01: Controlli prima videata                              *
014600950928?     *--------------------------------------------------------------*
014700950928     C     CTR01         BEGSR
014800950928      *
014900040928      *  Controllo data dal
015000040928     C     d1ddat        IFEQ      0
015100950928     C                   SETON                                        4028
015200950928     C                   MOVEL     ERR(1)        $MSG
015300950928     C                   GOTO      FINCT1
015400950928     C                   ELSE
015500040928     C                   Z-ADD     d1ddat        G02DAT
015600950928     C                   Z-ADD     0             G02INV
015700950928     C                   MOVEL     *BLANKS       G02ERR
015800950928     C                   CALL      'XSRDA8'
015900950928     C                   PARM                    WLBDA8
016000950928     C     G02ERR        IFEQ      '1'
016100950928     C                   SETON                                        4028
016200950928     C                   MOVEL     ERR(2)        $MSG
016300950928     C                   GOTO      FINCT1
016400950928     C                   ELSE
016500950928     C                   Z-ADD     G02INV        WDATDA
016600040928     C                   Z-ADD     G02DAT        d1ddat
016700950928     C                   END
016800950928     C                   END
016900950928     C     WDATDA        IFGT      WOGGI
017000950928     C                   SETON                                        4028
017100950928     C                   MOVEL     ERR(3)        $MSG
017200950928     C                   GOTO      FINCT1
017300950928     C                   END
017400040928      *  Controllo data al
017500040928     C     d1adat        IFEQ      0
017600950928     C                   SETON                                        4128
017700950928     C                   MOVEL     ERR(1)        $MSG
017800950928     C                   GOTO      FINCT1
017900950928     C                   ELSE
018000040928     C                   Z-ADD     d1adat        G02DAT
018100950928     C                   Z-ADD     0             G02INV
018200950928     C                   MOVEL     *BLANKS       G02ERR
018300950928     C                   CALL      'XSRDA8'
018400950928     C                   PARM                    WLBDA8
018500950928     C     G02ERR        IFEQ      '1'
018600950928     C                   SETON                                        4128
018700950928     C                   MOVEL     ERR(2)        $MSG
018800950928     C                   GOTO      FINCT1
018900950928     C                   ELSE
019000950928     C                   Z-ADD     G02INV        WDATAA
019100040928     C                   Z-ADD     G02DAT        d1adat
019200950928     C                   END
019300950928     C                   END
019400950928     C     WDATAA        IFGT      WOGGI
019500950928     C                   SETON                                        4128
019600950928     C                   MOVEL     ERR(3)        $MSG
019700950928     C                   GOTO      FINCT1
019800950928     C                   END
019900040927      *
020000950928      *  Controllo limiti fra due date
020100950928     C     WDATDA        IFGT      WDATAA
020200950928     C                   SETON                                        404128
020300950928     C                   MOVEL     ERR(4)        $MSG
020400950928     C                   GOTO      FINCT1
020500950928     C                   END
020600040927      *
020700950929      * Reperisco data ultimo utilizzo pgm pulizia bolle partenze
020800040928     c                   if        o36pou <> 046
020900951002     C     WDATDA        IFNE      WSVDAT
021000951002     C                   Z-ADD     WDATDA        WSVDAT            8 0
021100950929     C                   MOVE      *BLANKS       $PGM
021200010726     C                   MOVEL     'FNLS61R'     $PGM
021300950929     C                   MOVE      *BLANKS       $DAT
021400950929     C                   MOVE      *BLANKS       $ERR
021500950929     C                   CALL      'TRUL49C'                            31
021600950929     C                   PARM                    $PGM             10
021700000103     C                   PARM                    $DAT              8
021800950929     C                   PARM                    $ERR              1
021900951002      * programma in uso data ultimo utilizzo = data del giorno
022000950929     C     $ERR          IFEQ      '1'
022100950929     C     *IN31         OREQ      '1'
022200951002     C     $DAT          OREQ      *BLANKS
022300951002     C     $DAT          OREQ      *ZEROS
022400951002     C                   SETON                                        4028
022500951002     C                   MOVEL     ERR(8)        $MSG
022600951002     C                   GOTO      FINCT1
022700950929     C                   ELSE
022800950929     C                   MOVE      $DAT          G02INV
022900950929     C                   Z-ADD     *ZEROS        G02DAT
023000950929     C                   Z-ADD     *ZEROS        G02TGI
023100950929     C                   MOVEL     '3'           G02ERR
023200950929     C                   CALL      'XSRDA8'
023300950929     C                   PARM                    WLBDA8
023400951002     C                   Z-ADD     G02TGI        WDATA1            8 0
023500010726     C     WDATA1        SUB       §5ABLP        GIOTGI
023600950929     C                   CALL      'XSRGI8'
023700950929     C                   PARM                    WGIDAT
023800950929     C                   Z-ADD     GIOINV        WDTPUL            8 0
023900040928     C                   ENDIF
024000040928     C                   ENDIF
024100040928     c                   else
024200040928     C                   Z-ADD     §$3DP0        WDTPUL
024300040928     c                   end
024400950929     C     WDATDA        IFLT      WDTPUL
024500950929     C                   SETON                                        4028
024600951002     C                   MOVEL     ERR(7)        $MSG
024700040928     C                   MOVE      wdtpul        $MSG
024800950929     C                   GOTO      FINCT1
024900950929     C                   END
025000040927      *
025100040927      * ---------------------------
025200040928     c                   if        d1clnp=0 or d1clnp=999
025300040928     c                   eval      d1clnp=999
025400040928     c                   eval      d1dlnp=*blank
025500040928     c                   end
025600040928      *
025700040928     c                   if        d1cksc=0
025800040928     c                   eval      d1cksc=*all'9'
025900040928     c                   eval      d1dksc=*blank
026000040928     c                   end
026100040928      *
026200951002     C                   SETOFF                                       42
026300040928      *
026400040928      *  Controllo se immessa lnp
026500040928     C     d1clnp        ifne      999
026600040928     C     d1clnp        andne     0
026700040928     C                   EXSR      CTRLNP
026800040928     C                   END
026900040928     C   28              GOTO      FINCT1
027000040928      *
027100040928      *  Controllo se immesso cliente
027200040928     C     d1cKSC        ifne      *all'9'
027300040928     C     d1cKSC        andne     0
027400040928     C     d1dKSC        orne      *blank
027500960209     C                   EXSR      CTRCLI
027600960209     C                   END
027700951002     C   28              GOTO      FINCT1
027800950928     C*
027900040930     C*  ricerca x tipo comunicazione
028000040930     C*
028100040930     c                   if        d1ct01 ='?'
028200040930     c                   clear                   kpjbu
028300040930     c                   movel     '?'           kpjbu
028400040930     c                   call      'TRTB87R'
028500040930     c                   parm                    kpjba
028600040930     c                   movel     kpjbu         d1ct01
028700040930     c                   end
028800040930     C*
028900040930     c                   if        d1ct02 ='?'
029000040930     c                   clear                   kpjbu
029100040930     c                   movel     '?'           kpjbu
029200040930     c                   call      'TRTB87R'
029300040930     c                   parm                    kpjba
029400040930     c                   movel     kpjbu         d1ct02
029500040930     c                   end
029600040930     C*
029700040930     c                   if        d1ct03 ='?'
029800040930     c                   clear                   kpjbu
029900040930     c                   movel     '?'           kpjbu
030000040930     c                   call      'TRTB87R'
030100040930     c                   parm                    kpjba
030200040930     c                   movel     kpjbu         d1ct03
030300040930     c                   end
030400040930     C*
030500040930     c                   if        d1ct04 ='?'
030600040930     c                   clear                   kpjbu
030700040930     c                   movel     '?'           kpjbu
030800040930     c                   call      'TRTB87R'
030900040930     c                   parm                    kpjba
031000040930     c                   movel     kpjbu         d1ct04
031100040930     c                   end
031200040930     C*
031300040930     c                   if        d1ct05 ='?'
031400040930     c                   clear                   kpjbu
031500040930     c                   movel     '?'           kpjbu
031600040930     c                   call      'TRTB87R'
031700040930     c                   parm                    kpjba
031800040930     c                   movel     kpjbu         d1ct05
031900040930     c                   end
032000040930     C*
032100040930     c                   if        d1ct06 ='?'
032200040930     c                   clear                   kpjbu
032300040930     c                   movel     '?'           kpjbu
032400040930     c                   call      'TRTB87R'
032500040930     c                   parm                    kpjba
032600040930     c                   movel     kpjbu         d1ct06
032700040930     c                   end
032800040930     C*
032900040930     c                   if        d1ct07 ='?'
033000040930     c                   clear                   kpjbu
033100040930     c                   movel     '?'           kpjbu
033200040930     c                   call      'TRTB87R'
033300040930     c                   parm                    kpjba
033400040930     c                   movel     kpjbu         d1ct07
033500040930     c                   end
033600040930     C*
033700040930     c                   if        d1ct08 ='?'
033800040930     c                   clear                   kpjbu
033900040930     c                   movel     '?'           kpjbu
034000040930     c                   call      'TRTB87R'
034100040930     c                   parm                    kpjba
034200040930     c                   movel     kpjbu         d1ct08
034300040930     c                   end
034400040930     C*
034500040930     c                   if        d1ct09 ='?'
034600040930     c                   clear                   kpjbu
034700040930     c                   movel     '?'           kpjbu
034800040930     c                   call      'TRTB87R'
034900040930     c                   parm                    kpjba
035000040930     c                   movel     kpjbu         d1ct09
035100040930     c                   end
035200040930     C*
035300040930     c                   if        d1ct10 ='?'
035400040930     c                   clear                   kpjbu
035500040930     c                   movel     '?'           kpjbu
035600040930     c                   call      'TRTB87R'
035700040930     c                   parm                    kpjba
035800040930     c                   movel     kpjbu         d1ct10
035900040930     c                   end
036000040930     C*
036100040930     C*
036200950928     C     FINCT1        ENDSR
036300960209?     *--------------------------------------------------------------*
036400960209?     *  CTRCLI: Controllo dati cliente                              *
036500960209?     *--------------------------------------------------------------*
036600960209     C     CTRCLI        BEGSR
036700960209      *
036800960209      *  Controllo se richiesta ricerca alfabetica cliente
036900040928     C     d1CKSC        IFEQ      0
037000040928     C     d1CKSC        oreq      *all'9'
037100040928     C     d1DKSC        ANDNE     *BLANKS
037200040928     C                   MOVEL     d1DKSC        ACORAG
037300960209     C                   Z-ADD     CLIITA        ACOKCC
037400960209     C                   Z-ADD     0             ACOKSC
037500960209     C                   CALL      'XALFA1'
037600960209     C                   PARM                    PARDUT
037700960209     C                   PARM                    PARKUT
037800960209     C                   PARM                    ACORAG
037900960209     C                   PARM                    ACOKCC
038000960209     C                   PARM                    ACOKSC
038100960209     C                   PARM                    PARSTA            1 0
038200961016     C                   PARM                    PARFLR           90
038300960209     C     PARSTA        IFEQ      0
038400040928     C                   Z-ADD     ACOKSC        d1CKSC
038500040928     C                   eval      d1DKSC = ACORAG
038600960209     C                   END
038700960209     C                   END
038800040927     **
038900960209      *  Controllo validità conto cliente
039000040928     C                   MOVEL     *BLANKS       d1DKSC
039100040928     C     d1CKSC        IFNE      0
039200960209     C                   Z-ADD     1             KKUT
039300960209     C                   Z-ADD     CLIITA        KKCC
039400040928     C                   Z-ADD     d1CKSC        KKSC
039500960209     C     KACO          CHAIN     CNACO00F                           30
039600960209     C     *IN30         IFNE      '0'
039700960209     C     ACOFLG        ORNE      *BLANKS
039800960209     C     ACOABL        ORNE      *BLANKS
039900960209     C     ACOABL        ANDNE     *ZEROS
040000960209     C  N28              MOVEL     ERR(6)        $MSG
040100960209     C                   SETON                                        4228
040200960209     C                   END
040300040928     C                   MOVEL     ACORAG        d1DKSC
040400960209     C                   END
040500960209     C*
040600960209     C                   ENDSR
040700960209?     *--------------------------------------------------------------*
040800960209?     *  CTRLNP: Controllo dati linea di partenza                    *
040900960209?     *--------------------------------------------------------------*
041000960209     C     CTRLNP        BEGSR
041100960209      *
041200040928      *  Controllo validità liNea di partenza
041300040928     C     d1Clnp        IFNE      0
041400040928     C                   Z-ADD     D1Clnp        ORGFIL
041500960209     C     ORGFIL        CHAIN     AZORG01L                           30
041600960209     C     *IN30         IFNE      '0'
041700960209     C  N28              MOVEL     ERR(16)       $MSG
041800040928     C                   SETON                                        4328
041900960209     C                   END
042000040928     C                   MOVEL     ORGDES        d1Dlnp
042100960212     C     ORGFIL        LOOKUP    £1                                     30
042200040928      **
042300960212     C     *IN30         IFEQ      '0'
042400040928     C     O36POU        ANDNE     046
042500960209     C  N28              MOVEL     ERR(17)       $MSG
042600040928     C                   SETON                                        4328
042700960209     C                   END
042800960209     C                   END
042900960209     C*
043000960209     C                   ENDSR
043100950928?     *--------------------------------------------------------------*
043200040927?     *  SBMP51: Sottometto stampa statistica                        *
043300950928?     *--------------------------------------------------------------*
043400040927     C     SBMP51        BEGSR
043500040927      *
043600040928     c                   clear                   fnlg50ds
043700040927     C                   Z-ADD     WDATDA        D50dsd
043800040927     C                   Z-ADD     WDATAA        D50dsa
043900040928     c                   if        d1clnp <> 999
044000040928     C                   z-add     d1clnp        D50l01
044100040928     c                   else
044200040928     c                   if        o36pou <> 046
044300040928     c                   movea     £1            lnpd50
044400040928     c                   else
044500040928     C                   z-add     046           D50l01
044600040928     c                   end
044700040928     c                   end
044800040928     C                   z-add     d1cksc        D50ksc
044900040929     C                   move      d1dksc        D50rag
045000040928     C                   move      d1ct01        D50t01
045100040928     C                   move      d1ct02        D50t02
045200040928     C                   move      d1ct03        D50t03
045300040928     C                   move      d1ct04        D50t04
045400040928     C                   move      d1ct05        D50t05
045500040928     C                   move      d1ct06        D50t06
045600040928     C                   move      d1ct07        D50t07
045700040928     C                   move      d1ct08        D50t08
045800040928     C                   move      d1ct09        D50t09
045900040928     C                   move      d1ct10        D50t10
046000040928     C                   move      d1csup        D50sup
046100040929     C   77              move      'S'           D50sed
046200040929     C  N77              move      'F'           D50sed
046300040928     C                   MOVEL     FNLG50ds      KPJBU
046400041005     c                   move      prtsql        kpjbu
046500040927      *
046600040927     C                   MOVEL     'LG51'        KCOAZ
046700040927     c     kcoaz         chain     kfazn11l
046800040927     c                   if        not %Found(kfazn11l) or
046900040927     c                             knmus = *all'1'
047000040929     C                   CALL      'FNLG51C'
047100040927     C                   PARM                    KPJBA
047200040927     C                   else
047300950928     C                   CALL      'BCH10'
047400950928     C                   PARM                    KPJBA
047500950928     C                   END
047600950928     C*
047700950928     C                   ENDSR
047800950928?     *--------------------------------------------------------------*
047900950928?     *  *INZSR: Operazioni iniziali                                 *
048000950928?     *--------------------------------------------------------------*
048100950928     C     *INZSR        BEGSR
048200950928     C*
048300950928     C     *ENTRY        PLIST
048400950928     C                   PARM                    KPJBA
048500041005     C                   move      kpjbu         prtsql            1
048600950928     C* Richiamo XPARUT
048700950928     C                   Z-ADD     1             CODUT
048800950928     C                   CALL      'X§PARUT'
048900950928     C                   PARM                    UTEDSE
049000960209     C                   MOVEL     REC80         CNCR80
049100040928      *
049200040928      *  In base all'utente prende il P.O.
049300040928     C                   CLEAR                   Tibs36Ds
049400040928     c                   EVAL      I36ute = Knmus
049500040928     c                   EVAL      I36Tla = 'L'
049600040928     C                   CALL      'TIBS36R'
049700040928     C                   PARM                    Tibs36Ds
049800040928      * se utente di sede
049900040928     c                   if        o36pou = 046
050000040928     c                   seton                                        77
050100040928     c                   end
050200040928     C*
050300950928     C*--- Ricerca Capoconti -----------------------------------------*
050400040927     C                   DO        50            X                 3 0
050500950928     C                   MOVE      TCU(X)        TCUDS
050600950928     C     F56           IFEQ      'CG'
050700950928     C     F34           ANDEQ     '01'
050800950928     C                   Z-ADD     KCU(X)        CLIITA            4 0
050900950928     C                   END
051000950928     C                   END
051100950928     C*  KLIST
051200960209     C     KTAB          KLIST
051300950928     C                   KFLD                    KKUT
051400950928     C                   KFLD                    KCOD
051500950928     C                   KFLD                    KKEY
051600950928     C*
051700950928     C     KACO          KLIST
051800950928     C                   KFLD                    KKUT
051900950928     C                   KFLD                    KKCC
052000950928     C                   KFLD                    KKSC
052100950928     C*  Definizione delle variabili
052200950928     C     *LIKE         DEFINE    ACOKUT        KKUT
052300950928     C     *LIKE         DEFINE    ACOKCC        KKCC
052400950928     C     *LIKE         DEFINE    ACOKSC        KKSC
052500950928     C     *LIKE         DEFINE    TBLCOD        KCOD
052600950928     C     *LIKE         DEFINE    TBLKEY        KKEY
052700040928     C     *LIKE         DEFINE    d1ddat        WDATDA
052800040928     C     *LIKE         DEFINE    d1adat        WDATAA
052900950928     C*  Inizializzazione
053000950928     C                   MOVEL     'N'           WFINE             1
053100950928     C                   MOVEL     RAGUT         PARDUT           30
053200950928     C                   Z-ADD     CODUT         PARKUT            1 0
053300950928     C                   TIME                    WHHDAT           14 0
053400040927      *
053500950928     C                   MOVE      WHHDAT        G02DAT
053600950928     C                   Z-ADD     0             G02INV
053700950928     C                   MOVEL     *BLANKS       G02ERR
053800950928     C                   CALL      'XSRDA8'
053900950928     C                   PARM                    WLBDA8
054000950928     C                   Z-ADD     G02INV        WOGGI             8 0
054100950929     C                   Z-ADD     G02TGI        WTODAY            5 0
054200040927      *
054300960209      * Reperisco filiali su AS
054400951002     C                   OPEN      TABEL00F
054500020730     C     SimTpP        IFNE      '2'
054600960209     C                   Z-ADD     1             KKUT
054700020502     c                   clear                   trul06ds
054800020502     c                   eval      d06cod = '£1'
054900040928     c                   movel     o36POU        d06key
055000020502     c                   movel     trul06ds      kpjbu
055100020502     c                   call      'TRUL06R'
055200020502     c                   parm                    kpjba
055300020502     c                   movel     kpjbu         trul06ds
055400020502     c                   movea     lin           £1
055500960212     C                   ELSE
055600040928     C                   MOVE      o36POU        £1(1)
055700960212     C                   END
055800040927      *
055900040928      * Lettura giorni di pulizia filiale
056000080403     C                   Z-ADD     1             KKUT
056100970718     C                   MOVEL     '5A'          KCOD
056200040213     C                   MOVEL     '1       '    KKEY
056300960209     C     KTAB          CHAIN     TABEL00F                           30
056400010726     C  N30              MOVEL     TBLUNI        DS5A
056500010726     C   30              CLEAR                   DS5A
056600040928      *
056700040928      * Lettura giorni di pulizia sede
056800040928     C                   MOVEL     '$3'          KCOD
056900040928     C                   MOVEL     '1       '    KKEY
057000040928     C     KTAB          CHAIN     TABEL00F                           30
057100040928     C  N30              MOVEL     TBLUNI        DS$3
057200040928     C   30              CLEAR                   DS$3
057300010726     C                   CLOSE     TABEL00F
057400950928      *
057500950928     C                   ENDSR
057600950928**
057700040927Data giacenza obbligatoria                                            01
057800040927Data formalmente errata                                               02
057900040927Data maggiore della data del giorno                                   03
058000040927Limiti incompatibili: data iniziale maggiore della data finale        04
058100040928                                                                      05
058200950928Codice cliente inesistente o annullato                                06
058300040928Attenzione: data precedente all'ultima data pulizia bolle del         07
058400951002Data pulizia bolle non reperibile i dati potrebbero essere imprecisi  08
058500040927Indicare un codice cliente e/o una linea di partenza                  09
058600040927                                                                      10
058700040927                                                                      11
058800040927                                                                      12
058900040927                                                                      13
059000040927                                                                      14
059100040927                                                                      15
059200960209Linea di partenza inesitente                                          16
059300960212Linea di partenza non gestibile                                       17
059400950928**
059500950928312831303130313130313031
