000100931029     H DECEDIT('0,') DATEDIT(*DMY.)
000200931029      *---------*----------------------------------------------------*
000300020315      * FICNB6R *                                                    *
000400931029      *         - STAMPA CHIUSURA DISTINTA DI CONSEGNA -             *
000500931029      *         - DA ARCHIVI TASSAZIONE PADRONCINI.    -             *
000600931108      *--------------------------------------------------------------*
000700931108      * LEGGENDA DEGLI INDICATORI                                    *
000800931108      *--------------------------------------------------------------*
000900931108      * 01 - PAGAMENTO C/A O ASSEGNATO AVVENUTO CON ASSEGNO          *
001000931108      * 02 - SIMULAZIONE                                             *
001100931108      * 03 - CONSEGNE                                                *
001200931108      * 04 - RITIRI                                                  *
001300931108      * 05 - OVERFLOW                                                *
001400931111      * 07 - ESISTE DELTA PER ARRIVARE AL MINIMO SUl TOTALE GIORN.   *
001500931203      * 08 - ESISTE BONUS                                            *
001600931207      * 09 - ESISTONO GLI IMPORTI AGGIUNTI MANUALMENTE               *
001700931108      * 15 - INDICATORE DI COMODO X LOKUP                            *
001800931109      * 70 - LETTURA FLFTT02L                                        *
001900931109      * 80 - LETTURA FLFTD03L/FLFTD04L                               *
002000931108      * 89 - CHAIN/READE SU TABEL00F                                 *
002100931108      * 90 - ERRORE: DIMENSIONAMENTO SCH.INSUFF. X CARICARE TAB.'2A' *
002200931108      * 92 - ERRORE: DIMENSIONAMENTO SCH.INSUFF. X CARICARE TAB.'1A' *
002300941201      * 93 - ERRORE: DIMENSIONAMENTO SCH.INSUFF. X CARICARE TAB.'CV' *
002400931029      *--------------------------------------------------------------*
002500000623     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002600000623      *---------
002700011114     Ffiftt02L  IF   E           K DISK
002800931104      *---------
002900011114     Ffiftd03L  IF   E           K DISK
003000931029      *---------
003100030123     Ffifre02L  IF   E           K DISK
003200020318      *---------
003300931104     FTABEL00F  IF   E           K DISK    USROPN
003400020506      *---------
003500020529     FTntbe01l  iF   E           K DISK
003600941201      *---------
003700021202     Ffiapd01L  IF   E           K DISK
003800941201      *---------
003900020515     FTNTLZ01L  IF   E           K DISK
004000020515      *---------
004100020506     FFNARB01L  IF   E           K DISK    usropn
004200020506     Ftitas30c  IF   E           K DISK    usropn
004300101112     F******ansif01l  IF   E           K DISK    usropn
004400931105      *---------
004500020315     FFICNB6P   O    E             PRINTER INFDS(STAMP)
004600941201     F                                     OFLIND(*IN05)
004700931108      *---------
004800931108     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)
004900931029      *--------------------------------------------------------------*
005000931029      *  SCHIERE                                                     *
005100931104      *--------------------------------------------------------------*
005200931108     D STT             S             66    DIM(2) CTDATA PERRCD(1)              STAMPA
005300011121     D STP             S              5  0 DIM(999)                             STOP
005400931104      *--------------------------------------------------------------*
005500931105     D D2A             S             20    DIM(150)                             DESCRiZ. 2A
005600931105     D C2A             S              3    DIM(150)                             CHIAVI 2A
005700941201      *--------------------------------------------------------------*
005800941201     D ITA             S              1    DIM(100)                             DESCRiZ. CV
005900941201     D CCV             S              3    DIM(100)                             CHIAVI CV
006000931104      *--------------------------------------------------------------*
006100931104     D CDT             S              2    DIM(20)                              COD DESCR.TITOLO
006200931104     D DST             S             20    DIM(20)                              DESCR.TITOLO
006300000623      *--------------------------------------------------------------*
006400000623     D FIP             S              3  0 DIM(100)                             FILIALI POSTE
006500931104      *--------------------------------------------------------------*
006600931102      *  DS                                                          *
006700931102      *--------------------------------------------------------------*
006800931102     D KPJBA         E DS
006900000623      *----------------------------------------------------------------
007000931102     D*
007100941201     D WLBDA8          DS
007200941201     D  G02DAT                 1      8  0
007300941201     D  G02INV                 9     16  0
007400941201     D  G02ERR                17     17
007500941201     D  G02TGI                18     22  0
007600931102     D*  DS x gestione contatore di righe
007700931105     D STAMP           DS
007800931102     D  OVR                  188    189B 0
007900931102     D  LIN                  367    368B 0
008000931102     D  PAG                  369    372B 0
008100020506     D*
008200020506     D OG143         E DS
008300000623     D*
008400941201     D DSCV          E DS
008500941201     D*
008600020315     D ficnb6        E DS                  EXTNAME(ficnB6DS)
008700931108     D*
008800931109     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
008900931109     D*
009000931109     D CNCR80        E DS
009100110511     D CNCR42        E DS
009200931109     D*
009300931108     D                SDS
009400931108     D  NOMPG                  1     10
009500020506     D*
009600110509     d dataiso         s               d   datfmt(*iso)
009700110509     d dataeur         s               d   datfmt(*eur)
009800090916     F*--------------------------------------------------------------*
009900090916     D   DCTP        E DS
010000090916     D KCliPar         S              7s 0 DIM(9999)                            Tar.Part.sul cliente
010100090916     D KCliParTSR      S              8a   DIM(9999)                            Tar.Part.sul cliente
010200090916     D KTarStdNew      S              7A   DIM(9999)                            Tar.Part.sul cliente
010300090916     D KTarStd         S              4A   DIM(9999)                            Tar.Part.sul cliente
010400090916     D KTarNew         S              3A   DIM(9999)                            Tar.Part.sul cliente
010500090916     D KTarObb         S              1A   DIM(9999)                            Tar.Part.sul cliente
010600090916     D KTarUgu         S              1A   DIM(9999)                            Tar.Part.sul cliente
010700101112     ***************************************************************************
010800101112     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
010900101112     D ESITO_OK...
011000101112     D                 C                   0
011100101112     ***************************************************************************
011200101112     **
011300101112     ** Dichiarazione prototipi procedure esterne.
011400101112     **
011500101112     ***************************************************************************
011600101112      /DEFINE DFTACTGRP_YES
011700101112     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
011800101112      /UNDEFINE DFTACTGRP_YES
011900101112     ***************************************************************************
012000101112     **
012100101112     ** Definizione strutture dati.
012200101112     **
012300101112      **************************************************************************
012400101112     D tibsSocI0...
012500101112     D               E DS                  QUALIFIED
012600101112     D                                     INZ
012700101112     D tibsSocO0...
012800101112     D               E DS                  QUALIFIED
012900101112     D                                     INZ
013000101112     ***************************************************************************
013100101112     **
013200101112     ** Definizione variabili modulo/programma.
013300101112     **
013400101112     ***************************************************************************
013500101112     D prmRqsOpCode...
013600101112     D                 S             10A
013700101112     D prmRpyOpCode...
013800101112     D                 S             10A
013900101112     D prmRpyIdMsg...
014000101112     D                 S             10I 0
014100101112     D prmRqsFormato...
014200101112     D                 S             10A
014300101112     D prmRqsDataSize...
014400101112     D                 S             10I 0
014500101112     D prmRpyFormato...
014600101112     D                 S             10A
014700101112     D prmRpyDataSize...
014800101112     D                 S             10I 0
014900101112
015000000623      *----------------------------------------------------------------
015100000623     IAZORG01L  AA
015200000623     I                             P    4    5 0ORGFIL
015300000623     I                                  7    7  ORGFAG
015400030108     I                               3776 3800  ORGDE3
015500020506     Ifnarb000
015600020506     i              arbtc2                      tastc2
015700020506     i              arbcad                      tascad
015800020506     i              arbrsd                      tasrsd
015900020506     i              arbmgs                      tasmgs
016000020506     i              arbhcr                      tashcr
016100020506     i              arbdcr                      tasdcr
016200020506     i              arbtcr                      tastcr
016300161214     i              arbksc                      tasksc
016400161214     i              arbtc1                      tasftc
016500931102      *--------------------------------------------------------------*
016600931102      *  CICLO PRINCIPALE:                                           *
016700931102      *--------------------------------------------------------------*
016800020529     c                   exsr      defcam
016900020506     C*   GESTIONE CONSEGNE
017000941201     C     DD0TSR        IFNE      'R'
017100941201     C                   MOVE      'C'           KTSR
017200931108     C                   SETON                                        03
017300020507     C*  Azzero campi per stampa totali
017400020507     C                   EXSR      AZZFIN
017500931104     C*  Loop di letture distinte padroncini nella data considerata
017600011114     C     KFTT          CHAIN     fiftt02L                           70
017700931104     C*
017800931109     C     *IN70         IFEQ      '0'
017900931104     C     *IN70         DOWEQ     '0'
018000020506     c                   EXSR      DOCCON
018100020506     c                   z-add     1             ok
018200950109     C*  Leggo la distinta successiva
018300011114     C     KFTT          READE     fiftt02L                               70
018400931104     C                   END
018500931108     C*--------------------------------------------------------------*
018600931109     C                   EXSR      TOTFIN
018700931109     C                   END
018800931109     C*--------------------------------------------------------------*
018900931108     C                   END
019000931108     C*--------------------------------------------------------------*
019100020506     C*   GESTIONE RITIRI
019200941201     C     DD0TSR        IFNE      'C'
019300931125     C                   SETOFF                                       03
019400931109     C                   SETON                                        04
019500950110     C*  Azzero campi per stampa prospetto totale ritiri
019600950110     C                   EXSR      AZZTOG
019700950110     C*
019800941201     C                   MOVE      'R'           KTSR
019900011114     C     KFTT          CHAIN     fiftt02L                           70
020000950109     C     *IN70         IFEQ      '0'
020100950109     C     *IN70         DOWEQ     '0'
020200931109     C                   EXSR      DOCRIT
020300950109     C*  Leggo la distinta successiva
020400011114     C     KFTT          READE     fiftt02L                               70
020500931109     C                   END
020600950109     C*  Totale finale ritiri
020700950110     C                   EXSR      WRTTOG
020800950109     C                   END
020900950109     C*
021000950109     C                   END
021100931108     C*--------------------------------------------------------------*
021200931110     C*   Stampo totale complessivo se richiesta sia la stampa dei
021300931110     C*   dati relativi alle consegne che quelle relative ai ritiri
021400941201     C     DD0TSR        IFEQ      ' '
021500931111     C                   SETOFF                                       0304
021600931110     C                   EXSR      WRTGEN
021700931110     C                   END
021800020529      *--------------------------------------------------------------*
021900020529     C     FINE          TAG
022000101112      *
022100101112     ** Chiudo il programma.
022200101112     C                   CALL      'TIBSSOCR'
022300101112     C                   PARM      'FINALIZE'    prmRqsOpCode
022400101112     C                   PARM                    prmRpyOpCode
022500101112     C                   PARM                    prmRpyIdMsg
022600101112
022700020529     c                   if        simfel <> 0
022800020529     C                   SETON                                        LR
022900020529     c                   else
023000020529     C                   SETON                                        RT
023100020529     c                   end
023200950110      *--------------------------------------------------------------*
023300020515      *  srfor : Decodifico il fornitore                             *
023400950110      *--------------------------------------------------------------*
023500020515     C     srfor         BEGSR
023600110613      *
023700020515     c                   movel     fttsoc        socp
023800020529     c                   clear                   dessoc
023900110613      *
024000110613     c****************   if        simfel = 0
024100110613      *
024200101112     c**** fttsoc        chain     ansif01l
024300101112     c***********        if        %found(ansif01l)
024400101112     c***********        movel     sifdesbrev    dessoc
024500101112     c***********        end
024600101112      *
024700101112      **
024800101112     c                   clear                   dessoc
024900101112     c                   exsr      decod_societa
025000101112      **
025100101112      *
025200110613     c**********         end
025300110613      *
025400020515     c                   move      fttcdf        forp
025500020529     c                   move      fttnff        nrfatp
025600020529     C                   MOVE      FTTDft        G02INV
025700020529     C                   MOVEL     '3'           G02ERR
025800020529     C                   CALL      'XSRDA8'
025900020529     C                   PARM                    WLBDA8
026000020529     C                   MOVE      G02DAT        dtfatp
026100020515     c                   move      fttcdf        tlzpdr
026200020515     c     ktlz          chain     tntlz01l
026300020515     c                   if        %found(tntlz01l)
026400020515     c                   movel     tlzrsc        desfor
026500020515     c                   else
026600020515     c                   movel     *all'?'       desfor
026700020515     c                   end
026800020515     C                   ENDSR
026900020515      *--------------------------------------------------------------*
027000020515      *  AZZTOG: Azzero totali generali ritiri                       *
027100020515      *--------------------------------------------------------------*
027200020515     C     AZZTOG        BEGSR
027300950110     C*
027400950110     C                   Z-ADD     0             TOGSNA
027500950110     C                   Z-ADD     0             TOGCLA
027600950110     C                   Z-ADD     0             TOGKFA
027700950110     C                   Z-ADD     0             TOGTVL
027800950110     C                   Z-ADD     0             TOGSPE
027900950110     C                   Z-ADD     0             TOGNCL
028000950110     C                   Z-ADD     0             TOGVOL
028100950110     C                   Z-ADD     0             TOGCLI
028200020507     C                   Z-ADD     0             TOGPKG
028300950110     C*
028400950110     C                   ENDSR
028500931104      *--------------------------------------------------------------*
028600931109      *  DOCCON: Lettura distinte x numero documento                 *
028700931104      *--------------------------------------------------------------*
028800931109     C     DOCCON        BEGSR
028900931104     C*
029000940318     C*  Incremeto totali sommapesi giornata
029100940318     C                   ADD       FTTSNA        TOTSNA
029200940318     C                   ADD       FTTCLA        TOTCLA
029300940318     C                   ADD       FTTKFA        TOTKFA
029400940318     C                   ADD       FTTTVL        TOTTVL
029500941201     C                   MOVE      FTTNDC        KNDC
029600950113     C                   MOVE      FTTDDC        KDDCT
029700931104     C*  Posizionamento iniziale x loop letture dettaglio distinta
029800011114     C     KFTD          CHAIN     fiftd03L                           80
029900931104     C*
030000931104     C     *IN80         IFEQ      '0'
030100931104     C*  Scrivo dati testate distinta
030200931109     C                   EXSR      TESCON
030300931104     C     *IN80         DOWEQ     '0'
030400931104     C*  Scrittura dati di dettaglio distinta
030500931109     C                   EXSR      DETCON
030600011114     C     KFTD          READE     fiftd03L                               80
030700931104     C                   END
030800931104     C*
030900931111     C*  Scrivo dati totale distinta consegna
031000931111     C                   EXSR      TOTCON
031100931104     C                   END
031200931105     C*
031300931104     C                   ENDSR
031400931109      *--------------------------------------------------------------*
031500931109      *  DOCRIT: Lettura distinte ritiri                             *
031600931109      *--------------------------------------------------------------*
031700931109     C     DOCRIT        BEGSR
031800950109     C*
031900950109     C                   MOVE      FTTNDC        KNDC
032000950113     C                   MOVE      FTTDDC        KDDCT
032100011121     c                   move      fttdiv        coddiv
032200011121     C*
032300931109     C*  Posizionamento iniziale x loop letture dettaglio distinta
032400011114     C     KFTD          CHAIN     fiftd03L                           80
032500931109     C*
032600931109     C     *IN80         IFEQ      '0'
032700931109     C*  Scrivo dati testate distinta
032800931109     C                   EXSR      TESRIT
032900020315      * cambiato il ciclo per poter gestire i nuovi ritiri
033000931109     C     *IN80         DOWEQ     '0'
033100020315      *
033200020315      * salva per eseguire a rottura di stop
033300020315     c                   z-add     ftdnss        savnss
033400020315     c                   z-add     ftdstp        savstp
033500020315      *   salva il nome e il codice tariffa
033600020315     C                   movel     ftdRSC        savrsc
033700020315     C                   MOVE      FTDCTP        savCTP
033800020315     C                   MOVE      FTDfit        savFIT
033900020315     c                   MOVE      FTDfia        savFIA
034000020315     C*  da totalizzare
034100020315     C                   z-add     0             addSET
034200020315     C                   z-add     0             addNCL
034300020315     C                   z-add     0             addpkl
034400020315     C                   z-add     0             addvlu
034500020315     C                   z-add     0             additt
034600020315     C                   z-add     0             addita
034700020315      *
034800020315      *  x stop di ritiro
034900020315     C     *in80         doweq     *off
035000020315     c     ftdnss        andeq     savnss
035100020315     c     ftdstp        andeq     savstp
035200020315      *
035300020315     C*  cumula i campi per stop
035400020315     C                   EXSR      AddRIT
035500020315      *
035600011114     C     KFTD          READE     fiftd03L                               80
035700020315     c                   enddo
035800020315      *
035900020315     C*  Scrittura dati di dettaglio distinta
036000020315     C                   EXSR      DETRIT
036100020315      *
036200020315     C                   ENDdo
036300931109     C*
036400950109     C*  Scrivo dati totale numero ritiro
036500950109     C                   EXSR      TOTRIT
036600931109     C                   END
036700950109     C*
036800931109     C                   ENDSR
036900931105      *--------------------------------------------------------------*
037000020507      *  AZZFIN:Azzera dati di riepilogo distinte                    *
037100931105      *--------------------------------------------------------------*
037200020507     C     AZZFIN        BEGSR
037300931105     C*
037400020507     C*
037500020507     c                   z-add     0             spetot
037600020507     c                   z-add     0             spenoc
037700020507     c                   z-add     0             TOTP
037800020507     c                   z-add     0             TOTCOL
037900020507     c                   z-add     0             TOTPKG
038000020507     c                   z-add     0             TOTVOL
038100020507     c                   z-add     0             TOTVOL
038200020507     c                   z-add     0             NUMSTP
038300020507     c                   z-add     0             TOTCP
038400020507     c                   z-add     0             TOTNF
038500020507     c                   z-add     0             TOTGC
038600020507     c                   z-add     0             TOTAV
038700020507     c                   z-add     0             TOTNCP
038800020507     c                   z-add     0             TOTESG
038900020507     c                   z-add     0             inccnt
039000020507     c                   z-add     0             nsass
039100020507     c                   z-add     0             dirass
039200020507     c                   z-add     0             TOTEV
039300020507     c                   z-add     0             inccnV
039400020507     c                   z-add     0             nsassV
039500020507     c                   z-add     0             dirasV
039600020507     C                   Z-ADD     0             TOTSNA
039700020507     C                   Z-ADD     0             TOTCLA
039800020507     C                   Z-ADD     0             TOTKFA
039900020507     C                   Z-ADD     0             TOTTVL
040000020507     C*
040100020507     C                   ENDSR
040200020507      *--------------------------------------------------------------*
040300020507      *  TOTFIN:stampa dati di riepilogo distinte                    *
040400020507      *--------------------------------------------------------------*
040500020507     C     TOTFIN        BEGSR
040600020507     C*
040700020507     C*  Valorizzo i campi di riepilogo
040800020507     C     SPETOT        SUB       SPENOC        SPECON
040900020507     c                   if        spetot <> 0
041000020507     C                   Z-ADD     SPENOC        SPENCO
041100020507     C*  Totali in lire
041200020507     C                   Z-ADD     INCCNT        TOTG              9 0
041300020507     C                   ADD       NSASS         TOTG
041400020507     C                   ADD       DIRASS        TOTG
041500020507     C                   Z-ADD     TOTG          TOTESG
041600020507     C*  Totali in valuta
041700020507     C                   Z-ADD     INCCNV        TOTEV
041800020507     C                   ADD       NSASSV        TOTEV
041900020507     C                   ADD       DIRASV        TOTEV
042000020507     C*
042100020507     C*  Controlla se ci sono valori nei totali
042200020507     C                   SETOFF                                       1112
042300020507     C*
042400020507     C*  Imposta l'ovrflow da testare
042500020507     C                   Z-ADD     55            OVRLIN            3 0
042600020507     C     TOTCP         IFGT      0
042700020507     C                   SETON                                        11
042800020507     C                   SUB       1             OVRLIN
042900020507     C                   END
043000020507     C     TOTNCP        IFGT      0
043100020507     C                   SETON                                        12
043200020507     C                   SUB       1             OVRLIN
043300020507     C                   END
043400020507     C*
043500020507     C*  Scrittura dati di totale
043600020507     C     LIN           IFGE      OVRLIN
043700020507     C                   WRITE     TES00
043800020507     C                   END
043900020507     C*
044000020507     C                   WRITE     TOTAL
044100020507     C*
044200020507     C*  Controllo se devo scrivere il totale in lire
044300020507     C     INCCNT        IFNE      0
044400020507     C     NSASS         ORNE      0
044500020507     C     DIRASS        ORNE      0
044600020507     C     TOTESG        ORNE      0
044700020507     C                   WRITE     TOTESL
044800020507     C                   END
044900020507     C*  Controllo se devo scrivere il totale in valuta
045000020507     C     INCCNV        IFNE      0
045100020507     C     NSASSV        ORNE      0
045200020507     C     DIRASV        ORNE      0
045300020507     C     TOTEV         ORNE      0
045400020507     C                   WRITE     TOTESV
045500020507     C                   END
045600020507     C                   WRITE     TOTSMP
045700020507     c                   end
045800020507     C*
045900020507     C                   ENDSR
046000931104      *--------------------------------------------------------------*
046100931109      *  TESCON: Preparo la scrittura dei dati di testata x consegne *
046200931104      *--------------------------------------------------------------*
046300931109     C     TESCON        BEGSR
046400931104     C*
046500960214     C                   Z-ADD     0             STP
046600960214     C                   Z-ADD     0             F
046700931108     C                   MOVE      FTTCUC        CODUTE
046800931104     C                   MOVE      FTDNDC        NUMDOC
046900941201     C                   MOVE      FTTDDC        G02INV
047000941213     C                   MOVEL     '3'           G02ERR
047100941201     C                   CALL      'XSRDA8'
047200941201     C                   PARM                    WLBDA8
047300941201     C                   MOVE      G02DAT        STPDDC
047400011121     c                   move      fttdiv        coddiv
047500931108     C*
047600020515     C*  Scrittura testa  totale
047700940321     C     LIN           IFGE      61
047800940321     C                   WRITE     TES00
047900020515     C                   WRITE     TES01
048000940321     C                   SETOFF                                       05
048100940321     C                   ELSE
048200931108     C                   WRITE     SALTO
048300940321     C                   END
048400020515     C                   WRITE     TES03
048500931104     C*
048600931104     C                   ENDSR
048700931109      *--------------------------------------------------------------*
048800931109      *  TOTRIT:Stampa dati di riepilogo x ritiri                    *
048900931109      *--------------------------------------------------------------*
049000931109     C     TOTRIT        BEGSR
049100931111     C*
049200931111     C*  Calcolo totali ritiro
049300931111     C                   MOVE      FTTSET        TOPSPE
049400950110     C                   ADD       FTTSET        TOGSPE
049500931203     C                   MOVE      NUMSTP        TOPCLI
049600950110     C                   ADD       NUMSTP        TOGCLI
049700030107     C*
049800030107      * Valore Ritiri Annullati
049900030107     c                   setoff                                       77
050000030107     c                   if        ftttbs > 0
050100030107     c                   seton                                        77
050200030131     C                   z-add     ftttbs        topTBS            7 0
050300030107     c                   end
050400931111     C*
050500931111     C*  Preparo Stampa dati di tassazione
050600931111     C                   MOVE      FTTITT        TOPTAR
050700931111     C                   MOVE      FTTITA        TOPACC
050800011121     C                   move      fttdiv        TOPdiv
050900011114     C* >>>>>>>>> divisa
051000931111     C                   MOVE      *BLANKS       TOPTAF
051100931111     C                   MOVE      *BLANKS       TOPACF
051200931111     C     FTTFIT        IFNE      *BLANK
051300931111     C                   MOVE      'MAN'         TOPTAF                         Inserimento man.
051400931111     C                   END
051500931111     C     FTTFIA        IFNE      *BLANK
051600931111     C                   MOVE      'MAN'         TOPACF                         Inserimento man.
051700931111     C                   END
051800931111     C*
051900931111     C*  Scrittura dati di totale padroncino
052000940321     C     LIN           IFGE      63
052100940321     C                   WRITE     TES00
052200940321     C                   SETOFF                                       05
052300940321     C                   END
052400931111     C                   WRITE     TOTPD
052500950110     C*  Incremento totali generali ritiri
052600950110     C                   ADD       FTTSNA        TOGSNA
052700950110     C                   ADD       FTTCLA        TOGCLA
052800950110     C                   ADD       FTTKFA        TOGKFA
052900950110     C                   ADD       FTTTVL        TOGTVL
053000931109     C*
053100931109     C                   ENDSR
053200020507      *--------------------------------------------------------------*
053300020507      *  WRTTOG:Stampa dati totali ritiri                            *
053400020507      *--------------------------------------------------------------*
053500020507     C     WRTTOG        BEGSR
053600020507     C*
053700020507     c                   if        togspe <> 0
053800020507     C*  Scrittura dati di totale
053900020507     C     LIN           IFGE      57
054000020507     C                   WRITE     TES00
054100020507     C                   END
054200020507     C                   WRITE     TOTSPG
054300020507     c                   end
054400020507     C*
054500020507     C                   ENDSR
054600931111      *--------------------------------------------------------------*
054700931111      *  TOTCON:Stampa dati di riepilogo x distinta consegna         *
054800931111      *--------------------------------------------------------------*
054900931111     C     TOTCON        BEGSR
055000931111     C*
055100931111     C*  Preparo Stampa dati di tassazione
055200931111     C                   MOVE      FTTITT        TASTAD
055300931111     C                   MOVE      FTTITA        TASACD
055400011121     C                   move      fttdiv        tasdiv
055500011114      * >>>>>>>> divisa
055600931111     C                   MOVE      *BLANKS       FLGTAD
055700931111     C                   MOVE      *BLANKS       FLGACD
055800931111     C     FTTFIT        IFNE      *BLANK
055900931111     C                   MOVE      'MAN'         FLGTAD                         Inserimento man.
056000931111     C                   END
056100931111     C     FTTFIA        IFNE      *BLANK
056200931111     C                   MOVE      'MAN'         FLGACD                         Inserimento man.
056300931111     C                   END
056400931111     C*
056500931111     C*
056600931111     C*  Scrittura dati di totale padroncino
056700940321     C     LIN           IFGE      64
056800940321     C                   WRITE     TES00
056900940321     C                   SETOFF                                       05
057000940321     C                   END
057100940321     C   05              SETOFF                                       05
057200931111     C                   WRITE     TOT01
057300931111     C*
057400931111     C*  Aggiorno dati di riepilogo
057500931111     C                   ADD       FTTSET        SPETOT
057600931111     C                   ADD       FTTSNT        SPENOC
057700931111     C*
057800931111     C                   ENDSR
057900931109      *--------------------------------------------------------------*
058000931109      *  TESRIT: Preparo la scrittura dei dati di testata x ritiri   *
058100931109      *--------------------------------------------------------------*
058200931109     C     TESRIT        BEGSR
058300931109     C*
058400960214     C                   Z-ADD     0             STP
058500960214     C                   Z-ADD     0             F
058600020506     C*  Scrittura testa totale
058700020613     C     LIN           IFGE      60
058800020506     c     ok            oreq      0
058900940321     C                   WRITE     TES00
059000940321     C                   WRITE     TES02
059100931109     C                   SETOFF                                       05
059200940321     C                   ELSE
059300931109     C                   WRITE     SALT2
059400940321     C                   END
059500931109     C*
059600931109     C*  Giro la data distinta
059700931203     C                   Z-ADD     0             STP
059800931203     C                   Z-ADD     0             NUMSTP
059900941213     C                   MOVE      KDDC          G02INV
060000941202     C                   MOVEL     '3'           G02ERR
060100941202     C                   CALL      'XSRDA8'
060200941202     C                   PARM                    WLBDA8
060300941202     C                   MOVE      G02DAT        DRT
060400950109     C                   MOVE      FTTNDC        NRT
060500950110     C                   Z-ADD     0             TOPSPE
060600950110     C                   Z-ADD     0             TOPNCL
060700950110     C                   Z-ADD     0             TOPPKG
060800950110     C                   Z-ADD     0             TOPVOL
060900950110     C                   Z-ADD     0             TOPCLI
061000950110     C                   Z-ADD     0             TOPTAR
061100950110     C                   Z-ADD     0             TOPACC
061200950110     C                   MOVEL     *BLANKS       TOPTAF
061300950110     C                   MOVEL     *BLANKS       TOPACF
061400011121     C                   movel     *BLANKS       TOPdiv
061500931109     C*
061600931109     C                   ENDSR
061700931104      *--------------------------------------------------------------*
061800931109      *  DETCON: Scrittura dati dettaglio distinta consegna          *
061900931104      *--------------------------------------------------------------*
062000931109     C     DETCON        BEGSR
062100160503     C* tolto dal PRTF Bocchi
062200160503     C*********          MOVE      FTDHMC        ORACON
062300941202     C                   MOVE      FTDDCM        G02INV
062400941202     C                   MOVEL     '3'           G02ERR
062500941202     C                   CALL      'XSRDA8'
062600941202     C                   PARM                    WLBDA8
062700011121     C                   MOVE      G02DAT        DATCON            8 0
062800011121     c                   movel     datcon        datco6
062900011121     c                   move      datcon        fld2              2
063000011121     c                   move      fld2          datco6                         ggmmaa
063100931108     C                   MOVE      FTDAAS        AAS
063200931104     C                   MOVE      FTDLNP        LNP
063300931105     C                   MOVE      FTDNRS        NRS
063400931104     C                   MOVE      FTDNSP        NSP
063500931104     C                   MOVE      FTDNCL        NCL
063600941213     C                   MOVE      FTDPKL        PKG
063700941213     C                   MOVE      FTDVLU        VOL
063800931104     C                   MOVE      FTDCRC        RISER
063900931104     C                   MOVE      FTDTMC        TMC
064000931104     C*----------------------------------------------------*
064100931104     C*  Controllo se consegna parziale
064200931105     C*----------------------------------------------------*
064300011121     C                   MOVEL     *BLANKS       DESCON           19
064400931104     C     FTDCMC        IFEQ      'P  '
064500931104     C     FTDCMC        OREQ      '   '
064600931105     C                   MOVEL     FTDCMC        CPZ               1
064700931104     C                   MOVE      *BLANKS       CMC
064800931105     C*  Incremento totale colli, peso, volume x consegnati
064900931105     C                   ADD       1             TOTF              3 0          Fatte
065000931105     C                   ADD       FTDNCL        TOTCOL
065100941213     C                   ADD       FTDPKL        TOTPKG
065200941213     C                   ADD       FTDVLU        TOTVOL
065300931105     C*----------------------------------------------------*
065400931104     C                   ELSE
065500931105     C*----------------------------------------------------*
065600931105     C                   Z-ADD     1             Q
065700931105     C     FTDCMC        LOOKUP    C2A(Q)                                 15
065800931105     C   15              MOVEL     D2A(Q)        DESCON
065900931105     C*  Controllo il tipo di mancata consegna
066000931105     C     FTDTMC        IFEQ      'N'
066100931105     C                   ADD       1             TOTNF             3 0           Non fatte
066200931105     C                   ELSE
066300931105     C     FTDTMC        IFEQ      'A'
066400931105     C                   ADD       1             TOTAV             3 0           Lasciato avv.
066500931105     C                   ELSE
066600931105     C     FTDTMC        IFEQ      'G'
066700931105     C                   ADD       1             TOTGC             3 0           Giacenti
066800931105     C                   END
066900931105     C                   END
067000931105     C                   END
067100931104     C                   MOVE      FTDCMC        CMC
067200931104     C                   MOVE      *BLANKS       CPZ
067300931105     C*----------------------------------------------------*
067400931104     C                   END
067500931105     C*----------------------------------------------------*
067600931105     C*  Controllo se consegna parziale
067700931105     C     CPZ           IFEQ      'P'
067800931105     C                   ADD       1             TOTP              3 0
067900931105     C                   END
068000931108     C*----------------------------------------------------*
068100931108     C     FTDCMC        IFEQ      'T  '
068200931108     C     FTDCMC        OREQ      'N  '
068300931108     C                   MOVE      *BLANKS       CMC
068400931108     C                   END
068500931104     C*----------------------------------------------------*
068600941202     C                   MOVE      FTDAAS        KAAS
068700941202     C                   MOVE      FTDLNP        KLNP
068800941202     C                   MOVE      FTDNRS        KNRS
068900941202     C                   MOVE      FTDNSP        KNSP
069000941213     C                   MOVEL     FTDRSC        RSD
069100941202     C*  Reperisco i dati del destinatario
069200020506     C*                  MOVEL     *BLANKS       ARBCAD
069300020506     C*                  MOVE      *ZEROS        ARBMGS
069400020506     C                   MOVEL     *BLANKS       tasCAD
069500020506     C                   MOVE      *ZEROS        tasMGS
069600020506     c                   if        simfel = 0
069700020506     C     KARB          CHAIN     titas30c                           72
069800020506     c                   else
069900941202     C     KARB          CHAIN     FNARB01L                           72
070000161214     C******             MOVE      ARBTC1        CONCP1
070100020506     c                   end
070200090916      ****  Identifica TTL con un Asterisco in Stampa prima dell'importo
070300090916     c                   clear                   cliTTL
070400161214     c     tasksc        lookup    KCliPar                                33
070500090916     c   33              move      'T'           cliTTL
070600090916      ****
070700020506     C*                  MOVE      ARBTC2        CONCP2
070800020506     C*                  MOVEL     ARBCAD        CAP
070900020506     C*                  MOVEL     ARBRSD        RSD
071000020506     C*                  MOVE      ARBMGS        GG                2 0
071100020506     C*                  MOVEL     ARBMGS        MM                2 0
071200161214     C                   MOVE      tasFTC        CONCP1
071300020506     C                   MOVE      tasTC2        CONCP2
071400020506     C                   MOVEL     tasCAD        CAP
071500020506     C                   MOVEL     tasRSD        RSD
071600020506     C                   MOVE      tasMGS        GG                2 0
071700020506     C                   MOVEL     tasMGS        MM                2 0
071800941213     C                   MOVEL     GG            STPGMS
071900941213     C                   MOVE      MM            STPGMS
072000931104     C*----------------------------------------------------*
072100931108     C*  Controllo se spedizione non consegnata o consegnata
072200931108     C     FTDCMC        IFEQ      'P  '
072300931108     C     FTDCMC        OREQ      '   '
072400931116     C* Calcolo il n mero di stop
072500931108     C     FTDSTP        IFEQ      *ZEROS
072600011121     C                   ADD       1             NUMSTP            5 0
072700931108     C                   ELSE
072800931116     C     FTDSTP        LOOKUP    STP                                    15
072900931108     C  N15              DO
073000931108     C                   ADD       1             NUMSTP
073100931108     C                   ADD       1             F                 3 0
073200931108     C                   MOVE      FTDSTP        STP(F)
073300931108     C                   END
073400931108     C                   END
073500931108     C                   END
073600931108     C*----------------------------------------------------*
073700931108     C*  Stampo i dati di tassazione dei padroncini
073800931109     C                   MOVE      FTDCTP        CODTAR
073900931108     C                   MOVE      FTDITT        TASTAR
074000931109     C                   MOVE      FTDITA        TASACC
074100011114      *>>>>>>>> divisa
074200931109     C                   MOVE      *BLANKS       FLGTAR
074300931109     C                   MOVE      *BLANKS       FLGACC
074400931109     C     FTDFIT        IFNE      *BLANK
074500931109     C                   MOVE      'MAN'         FLGTAR                         Inserimento man.
074600931109     C                   END
074700931109     C     FTDFIA        IFNE      *BLANK
074800931109     C                   MOVE      'MAN'         FLGACC                         Inserimento man.
074900000623     C                   END
075000000623      *----------------------------------------------------*
075100000623      *TESTO SE FILIALE POSTE
075200000623     C     LNP           LOOKUP    FIP                                    88
075300000623     C     *IN88         IFEQ      *ON
075400000623     C     CMC           IFEQ      *BLANK
075500000703     C     TMC           ANDEQ     *BLANK
075600000623      * CONSEGNATO
075700000623     C                   ADD       1             TOTCP             3 0           Fatte
075800000623     C                   ELSE
075900000623      * NON CONSEGNATO
076000000623     C                   ADD       1             TOTNCP            3 0           Non fatte
076100000623     C                   END
076200000623     C                   ENDIF
076300170112     C*----------------------------------------------------*
076400170112      *  per i SOLO INCASSO  imposto segnalazione al posto
076500170112      *  della mancata Consegna (Bocchi)
076600170112     C*----------------------------------------------------*
076700170112     c                   if        FTDSIC ='S'
076800170112     c                   eval        descon ='Solo Incasso'
076900170112     c                   end
077000000623     C*----------------------------------------------------*
077100020515     c                   exsr      salpag
077200931108     C                   WRITE     DET01
077300950109     C*----------------------------------------------------*
077400950109     C*  Stampa dati consegna richiesta
077500950110     C                   MOVEL     'N'           STSAL             1
077600950109     C                   EXSR      STPRIC
077700931104     C*----------------------------------------------------*
077800931104     C*  Controllo se ci sono importi da stampare
077900931104     C                   EXSR      GESIMP
078000950110     C*----------------------------------------------------*
078100950110     C     STSAL         IFEQ      'S'
078200020515     c                   exsr      salpag
078300950110     C                   WRITE     SALRIG
078400950110     C                   END
078500931104     C*----------------------------------------------------*
078600931104     C                   ENDSR
078700950109      *--------------------------------------------------------------*
078800950109      *  STPRIC: Scrittura dati consegna richiesta                   *
078900950109      *--------------------------------------------------------------*
079000950109     C     STPRIC        BEGSR
079100950109     C                   MOVEL     *BLANKS       TIPRIC
079200950109     C                   Z-ADD     0             DATRIC
079300020506     C*                  Z-ADD     ARBHCR        ORARIC
079400950109     C*  Controllo se devo gestire tipo di consegna richiesta
079500020506     C*    ARBDCR        IFNE      *ZEROS
079600020506     C*    ARBHCR        ORNE      *ZEROS
079700020506     C*    ARBTCR        ORNE      *BLANKS
079800950109     C*  Giro la data consegna richiesta
079900020506     C*    ARBDCR        IFNE      0
080000020506     C*                  MOVE      ARBDCR        G02INV
080100020506     c                   Z-ADD     tasHCR        ORARIC
080200020506     C*  Controllo se devo gestire tipo di consegna richiesta
080300020506     C     tasDCR        IFNE      *ZEROS
080400020506     C     tasHCR        ORNE      *ZEROS
080500020506     C     tasTCR        ORNE      *BLANKS
080600020506     C*  Giro la data consegna richiesta
080700020506     C     tasDCR        IFNE      0
080800020506     C                   MOVE      tasDCR        G02INV
080900950109     C                   MOVE      '3'           G02ERR
081000950109     C                   CALL      'XSRDA8'
081100950109     C                   PARM                    WLBDA8
081200011122     C                   MOVE      G02DAT        fld80             8 0
081300011122     C                   movel     G02DAT        DATRIC
081400011122     C                   move      G02DAT        fld20             2 0
081500011122     C                   move      fld20         DATRIC
081600950109     C                   END
081700950109     C*  Consegna richiesta il  gg./alle ore
081800020506     C*    ARBTCR        IFEQ      ' '
081900020506     C*    ARBDCR        IFNE      *ZERO
082000020506     C     tasTCR        IFEQ      ' '
082100020506     C     tasDCR        IFNE      *ZERO
082200950109     C                   MOVEL     'IL'          TIPRIC
082300950109     C                   ELSE
082400950109     C                   MOVE      'ALLE ORE'    TIPRIC
082500950109     C                   END
082600950109     C*  Consegna richiesta prima del/prima delle
082700950109     C                   ELSE
082800020506     C*    ARBTCR        IFEQ      'P'
082900020506     C*    ARBDCR        IFNE      *ZERO
083000020506     C     tasTCR        IFEQ      'P'
083100020506     C     tasDCR        IFNE      *ZERO
083200950109     C                   MOVEL     'PRIMA DE'    TIPRIC
083300950109     C                   MOVE      'L  '         TIPRIC
083400950109     C                   ELSE
083500950109     C                   MOVE      'PRIMA DE'    TIPRIC
083600950109     C                   MOVE      'LLE'         TIPRIC
083700950109     C                   END
083800950109     C*  Consegna richiesta dopo le/dopo il
083900950109     C                   ELSE
084000020506     C*    ARBTCR        IFEQ      'D'
084100020506     C*    ARBDCR        IFNE      *ZERO
084200020506     C     tasTCR        IFEQ      'D'
084300020506     C     tasDCR        IFNE      *ZERO
084400950109     C                   MOVEL     'DOPO  IL'    TIPRIC
084500950109     C                   ELSE
084600950109     C                   MOVE      'DOPO  LE'    TIPRIC
084700950109     C                   END
084800950109     C                   END
084900950109     C*
085000950109     C                   END
085100950109     C                   END
085200950109     C*----------------------------------------------------*
085300020515     c                   exsr      salpag
085400950110     C                   MOVEL     'S'           STSAL
085500950109     C                   WRITE     DETRIC
085600950109     C*----------------------------------------------------*
085700950109     C                   END
085800950109     C*----------------------------------------------------*
085900950109     C                   ENDSR
086000020315      *--------------------------------------------------------------*
086100020315      *  AddRIT: Cumula per Stop i ritiri dello stesso cliente       *
086200020315      *--------------------------------------------------------------*
086300020315     C     AddRIT        BEGSR
086400020315     C*
086500020315     C                   add       FTDSET        addSET
086600020315     C                   add       FTDNCL        addNCL
086700020315     C                   add       FTDPKL        addpkl
086800020315     C                   add       FTDVLU        addvlu
086900020315     C                   add       FTDITT        additt
087000020315     C                   add       FTDITA        addita
087100020315     C*
087200020315     C                   MOVE      FTDAAS        KAAS
087300020315     C                   MOVE      FTDLNP        KLNP
087400020315     C                   MOVE      FTDNRS        KNRS
087500020315     C                   MOVE      FTDNSP        KNSP
087600020315     C*
087700020315     C                   ENDSR
087800931109      *--------------------------------------------------------------*
087900931109      *  DETRIT: Scrittura dati dettaglio distinta ritiri            *
088000931109      *--------------------------------------------------------------*
088100931109     C     DETRIT        BEGSR
088200931109     C*
088300020315     C                   MOVE      savSTP        STOP
088400020315     C                   MOVEL     savRSC        RSM
088500020315     C*
088600020315     C                   z-add     addSET        SPE
088700020315     C                   z-add     addNCL        NCL
088800020315     C                   z-add     addPKL        PKG
088900020315     C                   z-add     addVLU        VOL
089000931109     C*
089100931109     C*  Preparo Stampa dati di tassazione
089200020315     C                   MOVE      savCTP        CODTAR
089300020315     C                   z-add     additt        TASTAR
089400020315     C                   MOVE      addita        TASACC
089500020315      *
089600931109     C                   MOVE      *BLANKS       FLGTAR
089700931109     C                   MOVE      *BLANKS       FLGACC
089800020315     C     savFIT        IFNE      *BLANK
089900931109     C                   MOVE      'MAN'         FLGTAR                         Inserimento man.
090000931109     C                   END
090100020315     C     savFIA        IFNE      *BLANK
090200931109     C                   MOVE      'MAN'         FLGACC                         Inserimento man.
090300931109     C                   END
090400931203     C*----------------------------------------------------*
090500931203     C* Calcolo il n mero di stop
090600020315     C     savSTP        IFEQ      *ZEROS
090700011121     C                   ADD       1             NUMSTP
090800931203     C                   ELSE
090900020315     C     savSTP        LOOKUP    STP                                    15
091000931203     C  N15              DO
091100931203     C                   ADD       1             NUMSTP
091200931203     C                   ADD       1             F                 3 0
091300020315     C                   MOVE      savSTP        STP(F)
091400931203     C                   END
091500931203     C                   END
091600931109     C*----------------------------------------------------*
091700940321     C     LIN           IFGE      65
091800931109     C                   WRITE     TES00
091900931109     C                   WRITE     TES02
092000931109     C                   SETOFF                                       05
092100931109     C                   END
092200931109     C                   WRITE     DET02
092300931109     C                   MOVE      *ZERO         DRT
092400950109     C                   MOVE      *ZERO         NRT
092500931109     C*----------------------------------------------------*
092600931111     C*  Incremento totali padroncino
092700931111     C                   ADD       NCL           TOPNCL
092800950110     C                   ADD       NCL           TOGNCL
092900931111     C                   ADD       PKG           TOPPKG
093000950110     C                   ADD       PKG           TOGPKG
093100931111     C                   ADD       VOL           TOPVOL
093200950110     C                   ADD       VOL           TOGVOL
093300931109     C*
093400931109     C                   ENDSR
093500931104      *--------------------------------------------------------------*
093600931104      *  GESIMP: Gestione scrittura importi C/A e assegnato          *
093700931104      *--------------------------------------------------------------*
093800931104     C     GESIMP        BEGSR
093900941213     C*----------------------------------------------------*
094000941213     C     FTDCAS        IFNE      *ZERO                                        C/A
094100941213     C*  Se esiste contrassegno controllo se in lire o in valuta
094200941213     C                   MOVE      'I'           WITA              1
094300941213     C     FTDVCA        IFNE      *BLANKS
094400941213     C                   Z-ADD     1             X                 3 0
094500941213     C     FTDVCA        LOOKUP    CCV(X)                                 32
094600941213     C   32              MOVEL     ITA(X)        WITA
094700941213     C                   END
094800941213     C                   END
094900941213     C*----------------------------------------------------*
095000931104     C*  Controllo se esiste importo C/A o Ass. e se si e se i dati
095100931104     C*  di pagamento sono uguali stampo una sola riga di totale
095200941202     C     FTDCAS        IFNE      *ZERO                                        C/A + Assegnato
095300941213     C     WITA          ANDEQ     'I'                                          C/A Italia
095400931104     C     FTDIFP        ANDNE     *ZERO
095500941202     C*  Dati di pagamento uguali
095600931104     C     FTDTIC        ANDEQ     FTDMIF
095700931104     C     FTDAS1        ANDEQ     FTDAS2
095800931104     C     FTDAB1        ANDEQ     FTDAB2
095900931104     C     FTDCA1        ANDEQ     FTDCA2
096000941213     C                   MOVE      FTDVCA        VALUTA
096100941213     C                   MOVE      *BLANKS       DESTIT           20
096200931104     C                   MOVE      FTDAS1        NUMASS
096300931104     C                   MOVE      FTDAB1        ABI
096400931104     C                   MOVE      FTDCA1        CAB
096500931105     C                   MOVE      FTDTIC        TIT               2
096600950110     C     FTDCAS        ADD       FTDIFP        IMPOR            13 3
096700991119     C                   MOVEL     'ITL'         VCA               3
096800931104     C*  Controllo se assegno
096900931104     C     FTDAS1        COMP      *BLANKS                            0101
097000931104     C* Ricerco descrizione tabella codici rimbordo
097100931104     C                   EXSR      DECTIT
097200931104     C* Scrittura record totale C/A + Assegnato
097300020515     c                   exsr      salpag
097400950110     C                   MOVE      IMPOR         COM3              3 0
097500950110     C                   Z-ADD     IMPOR         IMPOR1
097600950110     C                   Z-ADD     IMPOR         IMPOR2
097700950110     C     COM3          COMP      0                                      06
097800950110     C                   MOVEL     'N'           STSAL
097900931108     C                   WRITE     DETIMP
098000931104     C                   GOTO      FINIMP
098100931104     C                   END
098200931104     C*----------------------------------------------------*
098300931104     C*  Controllo se esiste importo C/A
098400941213     C     FTDCAS        IFNE      *ZERO                                        C/A + Assegnato
098500941213     C                   MOVE      FTDVCA        VALUTA
098600931104     C                   MOVE      *BLANKS       DESTIT
098700931104     C                   MOVE      FTDAS1        NUMASS
098800931104     C                   MOVE      FTDAB1        ABI
098900931104     C                   MOVE      FTDCA1        CAB
099000931105     C                   MOVE      FTDTIC        TIT
099100941213     C                   Z-ADD     FTDCAS        IMPOR
099200991119     C                   MOVEL     FTDVCA        VCA               3
099300931104     C*  Controllo se assegno
099400931104     C     FTDAS1        COMP      *BLANKS                            0101
099500931104     C* Ricerco descrizione tabella codici rimbordo
099600931104     C                   EXSR      DECTIT
099700931104     C* Scrittura record totale C/A + Assegnato
099800020515     c                   exsr      salpag
099900950110     C                   MOVE      IMPOR         COM3              3 0
100000950110     C                   Z-ADD     IMPOR         IMPOR1
100100950110     C                   Z-ADD     IMPOR         IMPOR2
100200950110     C     COM3          COMP      0                                      06
100300950110     C                   MOVEL     'N'           STSAL
100400931108     C                   WRITE     DETIMP
100500931104     C                   END
100600931104     C*----------------------------------------------------*
100700931104     C*  Controllo se esiste importo Assegnato
100800931104     C     FTDIFP        IFNE      *ZERO                                        C/A + Assegnato
100900931104     C                   MOVE      *BLANKS       DESTIT
101000941213     C                   MOVE      *BLANKS       VALUTA
101100931104     C                   MOVE      FTDAS2        NUMASS
101200931104     C                   MOVE      FTDAB2        ABI
101300931104     C                   MOVE      FTDCA2        CAB
101400931104     C                   MOVE      FTDMIF        TIT
101500931104     C                   Z-ADD     FTDIFP        IMPOR
101600020725     C                   IF        FTDDIV <> ' '
101700020725     C                   MOVEL     ftddiv        VCA
101800020725     C                   ELSE
101900020725     C                   MOVEL     'EUR'         VCA
102000020725     C                   END
102100931104     C*  Controllo se assegno
102200931104     C     FTDAS2        COMP      *BLANKS                            0101
102300931104     C* Ricerco descrizione tabella codici rimbordo
102400931104     C                   EXSR      DECTIT
102500931104     C* Scrittura record totale C/A + Assegnato
102600020515     c                   exsr      salpag
102700950110     C                   MOVE      IMPOR         COM3              3 0
102800950110     C                   Z-ADD     IMPOR         IMPOR1
102900950110     C                   Z-ADD     IMPOR         IMPOR2
103000950110     C     COM3          COMP      0                                      06
103100950110     C                   MOVEL     'N'           STSAL
103200931108     C                   WRITE     DETIMP
103300931104     C                   END
103400931104     C*----------------------------------------------------*
103500931104     C     FINIMP        ENDSR
103600931104      *--------------------------------------------------------------*
103700931104      *  DECTIT: Decodifico il titolo                                *
103800931104      *--------------------------------------------------------------*
103900931104     C     DECTIT        BEGSR
104000931104     C*----------------------------------------------------*
104100931104     C                   Z-ADD     1             Q
104200931105     C                   MOVE      *BLANKS       DESTIT
104300931105     C     TIT           IFNE      *BLANKS
104400931116     C     TIT           ANDNE     'SC'
104500931104     C     TIT           LOOKUP    CDT(Q)                                 15
104600931104     C   15              MOVEL     DST(Q)        DESTIT
104700931105     C                   MOVE      TIT           TPMB              1
104800931105     C*  Controllo se tipo di assegno mittente o bartolini e
104900931105     C*  incremento i relativi totali
105000991119     C     TPMB          IFEQ      'M'                                          02
105100991119     C     VCA           IFEQ      'ITL'                                        03
105200991119     C     VCA           OREQ      *BLANKS                                      03
105300931105     C                   ADD       IMPOR         DIRASS
105400991119     C*... EURO
105500991119     C                   ELSE
105600991119     C     VCA           IFEQ      'EUR'
105700991119     C                   ADD       IMPOR         DIRASV
105800991119     C                   END
105900991119     C                   END                                                    03
106000931105     C                   ELSE
106100991119     C     VCA           IFEQ      'ITL'
106200991119     C     VCA           OREQ      *BLANKS                                      03
106300931105     C                   ADD       IMPOR         NSASS
106400991119     C*... EURO
106500991119     C                   ELSE
106600991119     C     VCA           IFEQ      'EUR'
106700991119     C                   ADD       IMPOR         NSASSV
106800991119     C                   END
106900991119     C                   END
107000931105     C                   END
107100931105     C                   ELSE
107200931105     C*  pagamento in contanti
107300991119     C     VCA           IFEQ      'ITL'
107400991119     C     VCA           OREQ      *BLANKS                                      03
107500931105     C                   ADD       IMPOR         INCCNT
107600991119     C*... EURO
107700991119     C                   ELSE
107800991119     C     VCA           IFEQ      'EUR'
107900991119     C                   ADD       IMPOR         INCCNV
108000931105     C                   END
108100991119     C                   END
108200991119     C                   END
108300931104     C*----------------------------------------------------*
108400931104     C                   ENDSR
108500931110      *--------------------------------------------------------------*
108600931110      *  WRTGEN: Stampa totali generali tassazione                   *
108700931110      *--------------------------------------------------------------*
108800931110     C     WRTGEN        BEGSR
108900931110      *--------------------------------------------------------------*
109000941213     C                   MOVE      ' '           KTSR
109100011114     C     KFTT          CHAIN     fiftt02L                           80
109200931110     C     *IN80         IFEQ      '0'
109300931110     C* Totali generali tassazione
109400030108     C*  x i Ritiri Annullati li espone a parte e li toglie
109500030108     C*   dalla base in cui sono accorpati.
109600140429      * il campo PDU è stato liberato dal valore che era stato precedentemente
109700140429      * inserito x i RITIRI ANNULLATI (poichè equivaleva a VRANx1000)
109800140429      ** a questo punto
109900140429     c******             move      fttPDU        genRAN
110000140429     c                   move      fttVRAN       genRAN
110100030108     c                   z-add     fttTBS        genTBS
110200030108     c                   setoff                                       77
110300140429     c******             if        fttpdu > 0
110400140429     c                   if        fttVRAN >0
110500030108     c                   seton                                        77
110600030108     c                   end
110700931110     C                   Z-ADD     FTTITT        GENTAR
110800030108     c   77              sub       genran        gentar
110900931110     C                   Z-ADD     FTTITA        GENACC
111000011114      * >>>>>>>>> divisa
111100931110     C                   Z-ADD     FTTTPE        GENPCK
111200931110     C                   Z-ADD     FTTTBN        GENBON
111300931110     C                   Z-ADD     FTTTIM        GENIAM
111400931111     C                   Z-ADD     0             GENMIN
111500931111     C                   ADD       FTTITT        GENTO1
111600931111     C                   ADD       FTTITA        GENTO1
111700011114      * >>>>>>>> divisa
111800931111     C                   ADD       FTTTPE        GENTO1
111900931111     C     FTTMNT        ADD       GENTO1        GENTO2
112000931207     C                   ADD       FTTTIM        GENTO2
112100931203     C                   ADD       FTTTBN        GENTO2
112200020318      *
112300020318      * a rettifica dell'importo addizionale, occorre riportare anche altre righe
112400030123      *  di rettifica impostate sul FIFRE02L e impostate come riporta il FTTTIM.
112500020318     C                   movel     'N'           rett              1
112600030123     C     KFre          Chain     fifre02L
112700030123     c                   if        %Found(fifre02l)
112800020318     C                   movel     'S'           rett
112900020318     c                   end
113000020318      *
113100940401     C     FTTTBN        COMP      0                                  0808
113200940401     C     FTTMNT        COMP      0                                  0707
113300940401     C     FTTTIM        COMP      0                                  0909
113400931203     C   07FTTMNT        ADD       GENTO1        GENMIN
113500940321     C     LIN           IFGE      61
113600931111     C                   WRITE     TES00
113700940321     C                   END
113800090713      *
113900090713      * stampa PICKING FORFETTARIO solo il campo è valorizzato
114000090713     C     GENpck        COMP      0                                  76
114100090713      *
114200931111     C                   WRITE     TOTGEN
114300020506     c                   exsr      azzgen
114400020318      *
114500020318      * solo se ci sono delle rettifiche
114600020318     C                   if        rett = 'S'
114700060720     c                   clear                   savcre
114800020318     c                   seton                                        51
114900020507     c                   z-add     0             okret             1 0
115000030123     C     KFre          Setll     fifre02L
115100030123     C     KFre          reade     fifre02l
115200030123     c                   dow       not %eof(fifre02l)
115300020613     C     LIN           IFGE      65
115400020506     c                   WRITE     TES00
115500020318     c                   seton                                        51
115600020318     C                   END
115700020409     c                   if        savcre <> frecre
115800020409     c                   move      FRECRE        savCRE
115900020409     c                   move      FRECRE        detCRE
116000020318     c                   movel     'CRE'         tbecod
116100020318     C                   movel(P)  frecre        tbeke1
116200020318     c                   clear                   detDES
116300020318     C     Ktbe          chain     tntbe01l
116400020318     C                   if        %Found(tntbe01l) and tbeatb = *blank
116500020318     c                   movel     tbeuni        detDES
116600020318     c                   end
116700020409     c                   else
116800020409     c                   clear                   detCRE
116900020409     c                   clear                   detDES
117000020409     c                   end
117100020318     c                   move      FRENOT        detNOT
117200020318     c                   z-add     FRETIM        detTIM
117300020318     C                   WRITE     DETRET
117400020507     c                   z-add     1             okret
117500020318     c                   setoff                                       51
117600030123     C     KFre          reade     fifre02l
117700020318     c                   enddo
117800020507     c                   if        okret = 1
117900020318     C                   WRITE     ENDRET
118000020318     C                   end
118100020507     C                   end
118200020613     C     LIN           IFGE      59
118300020515     C                   WRITE     TES00
118400020515     C                   END
118500020515     C                   WRITE     TOTGE1
118600020515     c                   exsr      azzge1
118700020515     C                   END
118800020318      *
118900951025     C*  Se ci sono errori stampo segnalazione
119000951025     C     DD0ERR        IFEQ      'S'
119100951025     C     LIN           IFGE      59
119200951025     C                   WRITE     TES00
119300951025     C                   END
119400951025     C                   WRITE     ERRORE
119500951025     C                   END
119600931110     C*
119700931110     C                   ENDSR
119800020506      *--------------------------------------------------------------*
119900020506      *  AZZgen: Azzero totali generali                              *
120000020506      *--------------------------------------------------------------*
120100020506     C     AZZgen        BEGSR
120200020506     C*
120300020506     C                   Z-ADD     0             gentar
120400020506     C                   Z-ADD     0             genacc
120500020506     C                   Z-ADD     0             genpck
120600020506     C                   Z-ADD     0             gento1
120700020506     C                   Z-ADD     0             genmin
120800020506     C                   Z-ADD     0             genbon
120900020506     C*
121000020506     C                   ENDSR
121100020515      *--------------------------------------------------------------*
121200020515      *  AZZgen: Azzero totali generali                              *
121300020515      *--------------------------------------------------------------*
121400020515     C     AZZge1        BEGSR
121500020515     C*
121600020515     C                   Z-ADD     0             geniam
121700020515     C                   Z-ADD     0             gento2
121800020613     C                   Z-ADD     0             PAGE
121900020515     c                   seton                                        05
122000020515     C*
122100020515     C                   ENDSR
122200931104      *--------------------------------------------------------------*
122300020515      *  salto pagina per consegne                                   *
122400931104      *--------------------------------------------------------------*
122500020515     C     salpag        BEGSR
122600020515     C     LIN           IFGE      65
122700020515     C                   WRITE     TES00
122800020515     C                   WRITE     TES01
122900020515     C                   WRITE     TES03
123000020515     C                   SETOFF                                       05
123100020515     C                   END
123200020515     C                   ENDSR
123300020515      *--------------------------------------------------------------*
123400020529      *  Operazioni iniziali                                 *
123500020515      *--------------------------------------------------------------*
123600020529     C     defcam        BEGSR
123700931104      *--------------------------------------------------------------*
123800020315     C                   MOVEL     KPJBU         ficnB6
123900020529     c* chiudo file
124000020529     c                   if        dd0pdr = 0
124100020529     c                   seton                                        lr
124200020529     c                   return
124300020529     c                   end
124400020529     c*
124500941201     C     DD0SIM        COMP      'S'                                    02
124600020529     C* Carico dati del padroncino
124700030306     C                   MOVEL     *BLANKS       APDRSF
124800021202     C     kapd          CHAIN     fiapd01L                           50
124900030131     C                   MOVEL     APDRSF        DESPAD
125000020529     C                   MOVE      DD0PDR        KPDR
125100020529     C                   MOVE      DD0DDT        KDDC
125200020529     C                   MOVE      DD0PDR        CODCPC
125300020529     C                   CLEAR                   WLBDA8
125400020529     C                   MOVE      KDDC          G02INV
125500020529     C                   MOVEL     '3'           G02ERR
125600020529     C                   CALL      'XSRDA8'
125700020529     C                   PARM                    WLBDA8
125800020529     C                   MOVE      G02DAT        STPDDC
125900020529     c                   z-add     0             ok                1 0
126000020529     C                   TIME                    TIMES             6 0
126100020529     C                   SETON                                        05
126200020529     C                   SETON                                        OA
126300020529     c                   z-add     61            lin
126400020529     C                   MOVE      ' '           KTSR
126500020529     C     KFTT          CHAIN     fiftt02L                           80
126600020529     c                   if        *in80
126700020529     c                   return
126800020529     c                   else
126900020529     c                   exsr      srfor
127000020529     c                   end
127100020529     C                   ENDSR
127200020529      *--------------------------------------------------------------*
127300020529      *  *INZSR: Operazioni iniziali                                 *
127400020529      *--------------------------------------------------------------*
127500020529     C     *INZSR        BEGSR
127600110511      *
127700020529     C     *ENTRY        PLIST
127800020529     C                   PARM                    KPJBA
127900931109     C* Richiamo XPARUT
128000931109     C                   Z-ADD     1             CODUT
128100931109     C                   CALL      'X§PARUT'
128200931109     C                   PARM                    UT§DSE
128300110511     C****               MOVEL     RAGUT         RSUT             20
128400931109     C                   MOVEL     REC80         CNCR80
128500110511     C                   MOVEL     REC42         CNCR42
128600110511     C                   MOVEL     simRGS        RSUT             20
128700110511      *
128800020506     c                   if        simfel = 0
128900020530     c                   seton                                        20
129000020506     c                   open      titas30c
129100101112     c**********         open      ansif01l
129200101112
129300020506     c                   else
129400020530     c                   setoff                                       20
129500020506     c                   open      fnarb01l
129600020506     c                   end
129700020506     C*                  Z-ADD     SIMFEL        FILIA             3 0
129800931104     C*  Definizione delle chiavi
129900020515     C     Ktlz          KLIST
130000020515     C                   KFLD                    tlztip
130100020515     C                   KFLD                    tlzpdr
130200020515     C                   KFLD                    socp
130300020515     C                   movel(p)  'P'           tlztip
130400020515     C*
130500020515     C     KFTT          KLIST
130600020515     C                   KFLD                    KPDR
130700020515     C                   KFLD                    KFVL
130800020515     C                   KFLD                    KDDC
130900020529     C                   KFLD                    KTSR
131000020318     C*
131100941201     C     KFTD          KLIST
131200941201     C                   KFLD                    KPDR
131300941201     C                   KFLD                    KTSR
131400950113     C                   KFLD                    KNDC
131500950113     C                   KFLD                    KDDCT
131600020318     C*
131700020318     C     KFRE          KLIST
131800030123     C                   KFLD                    KTip              1
131900020318     C                   KFLD                    KPDR
132000020318     C                   KFLD                    KDDC
132100030123     C                   move      'A'           KTip              1
132200941201     C*
132300941201     C     KARB          KLIST
132400941201     C                   KFLD                    KAAS
132500941201     C                   KFLD                    KLNP
132600941201     C                   KFLD                    KNRS
132700941201     C                   KFLD                    KNSP
132800931105     C*
132900941201     C     KTAB          KLIST
133000941201     C                   KFLD                    KKUT
133100941201     C                   KFLD                    KCOD
133200020318     C*
133300020318     C     KTbe          KLIST
133400020318     C                   KFLD                    tbecod
133500020318     C                   KFLD                    tbeke1
133600021202     C*
133700021202     C     Kapd          KLIST
133800021202     C                   KFLD                    apdtip
133900021202     C                   KFLD                    dd0pdr
134000021202     c                   movel     'A'           apdtip
134100020318     c*
134200931105     C* Definizione delle variabili
134300020506     C*    *LIKE         DEFINE    ARBAAS        KAAS
134400020506     C*    *LIKE         DEFINE    ARBLNP        KLNP
134500020506     C*    *LIKE         DEFINE    ARBNRS        KNRS
134600020506     C*    *LIKE         DEFINE    ARBNSP        KNSP
134700020506     C     *LIKE         DEFINE    tasAAS        KAAS
134800020506     C     *LIKE         DEFINE    tasLNP        KLNP
134900020506     C     *LIKE         DEFINE    tasNRS        KNRS
135000020506     C     *LIKE         DEFINE    tasNSP        KNSP
135100941201     C     *LIKE         DEFINE    FTTPDR        KPDR
135200941201     C     *LIKE         DEFINE    FTTFVL        KFVL
135300941201     C     *LIKE         DEFINE    FTTTSR        KTSR
135400941201     C     *LIKE         DEFINE    FTTDDC        KDDC
135500950113     C     *LIKE         DEFINE    FTTDDC        KDDCT
135600941201     C     *LIKE         DEFINE    FTTNDC        KNDC
135700941201     C     *LIKE         DEFINE    TBLKUT        KKUT
135800941201     C     *LIKE         DEFINE    TBLCOD        KCOD
135900941201     C     *LIKE         DEFINE    FTTSET        SPETOT
136000931108     C     *LIKE         DEFINE    FTTSNT        SPENOC
136100020315     C     *like         define    ftdnss        savnss
136200020315     C     *like         define    ftdstp        savstp
136300020315     C     *like         define    FTDRSC        savrsc
136400020315     C     *like         define    FTDSET        addSET
136500020315     C     *like         define    FTDNCL        addNCL
136600020315     C     *like         define    FTDPKL        addpkl
136700071121     C     *like         define    FTDVLU        addvlu           +2
136800020315     C     *like         define    FTDCTP        savCTP
136900020315     C     *like         define    FTDITT        additt
137000020315     C     *like         define    FTDITA        addita
137100020315     C     *like         define    FTDFIA        savFIA
137200020315     C     *like         define    FTDFIT        savFIT
137300020409     c     *like         define    FRECRE        savCRE
137400000623     C* Inizializza  schiera delle Filiali POSTE
137500000623     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
137600000623     C                   CLEAR                   FIP
137700000623     C     *LOVAL        SETLL     AZORG01L
137800000623     C                   READ      AZORG01L                               89
137900000623     C     *IN89         DOWEQ     *OFF
138000000623     C                   MOVEL     ORGDE3        OG143
138100020717     C**** §OGPT         IFEQ      'S'
138200020717     C     §OgNTW        IFEQ      'PPT'
138300000623     C                   Z-ADD     1             X
138400000623     C     *ZEROS        LOOKUP    FIP(X)                                 89
138500000623     C   89              Z-ADD     ORGFIL        FIP(X)
138600000623     C                   END
138700000623     C                   READ      AZORG01L                               89
138800000623     C                   END
138900000623     C* Inizializzazione  variabili
139000950623     C                   MOVE      SIMFEL        CODFIL            3 0
139100020529     C                   MOVE      'C'           KFVL
139200020529     C                   MOVE      1             KKUT
139300020529     C                   MOVEL     NOMPG         NOMPGM
139400020529     C                   MOVEL     *ALL'-'       CMP
139500020529     C                   MOVEL     *ALL'-'       CM1
139600020529     C                   MOVEL     *ALL'*'       CM3
139700931105     C* Caricamento dati tabelle nelle schiere
139800931105     C                   OPEN      TABEL00F
139900931105     C* Carico in schiera la Tab. 2A: descrizione tipo consegna
140000941201     C                   MOVEL     '2A'          KCOD
140100931104     C                   Z-ADD     0             Q                 3 0
140200941201     C     KTAB          CHAIN     TABEL00F                           89
140300931104     C     *IN89         DOWEQ     '0'
140400931105     C     TBLFLG        IFEQ      *BLANKS
140500931105     C     Q             IFLT      150
140600931104     C                   ADD       1             Q
140700931108     C                   MOVEL     TBLKEY        C2A(Q)
140800931108     C                   MOVEL     TBLUNI        D2A(Q)
140900931105     C                   ELSE
141000931105     C                   SETON                                        9089
141100931105     C   OA              EXCEPT    TESERR
141200931105     C                   EXCEPT    DETERR
141300931104     C                   END
141400931105     C                   END
141500941201     C     KTAB          READE     TABEL00F                               89
141600931104     C                   END
141700931105     C*
141800931105     C* Carico in schiera la Tab. 1A: descrizione titolo
141900941201     C                   MOVEL     '1A'          KCOD
142000931105     C                   Z-ADD     0             Q                 3 0
142100941213     C     KTAB          CHAIN     TABEL00F                           89
142200931105     C     *IN89         DOWEQ     '0'
142300931105     C     TBLFLG        IFEQ      *BLANKS
142400931105     C     Q             IFLT      20
142500931105     C                   ADD       1             Q
142600931105     C                   MOVEL     TBLUNI        DST(Q)
142700931105     C                   MOVEL     TBLKEY        CDT(Q)
142800931105     C                   ELSE
142900931105     C                   SETON                                        9289
143000931105     C   OA              EXCEPT    TESERR
143100931105     C                   EXCEPT    DETERR
143200931105     C                   END
143300931105     C                   END
143400941201     C     KTAB          READE     TABEL00F                               89
143500931105     C                   END
143600931105     C*
143700941201     C* Carico dati valute
143800941201     C                   CLEAR                   DSCV
143900941201     C                   Z-ADD     0             Q                 3 0
144000941201     C                   MOVEL     'CV'          KCOD
144100941201     C     KTAB          CHAIN     TABEL00F                           89
144200941201     C     *IN89         DOWEQ     '0'
144300941201     C     TBLFLG        IFEQ      *BLANKS
144400941201     C     Q             IFLT      100
144500941201     C                   ADD       1             Q
144600941201     C                   MOVEL     TBLUNI        DSCV
144700941201     C                   MOVEL     TBLKEY        CCV(Q)
144800941201     C                   MOVEL     §CVITA        ITA(Q)
144900941201     C                   ELSE
145000941201     C                   SETON                                        9389
145100941201     C   OA              EXCEPT    TESERR
145200941201     C                   EXCEPT    DETERR
145300941201     C                   END
145400931105     C                   END
145500941201     C     KTAB          READE     TABEL00F                               89
145600941201     C                   END
145700931105     C* Fine caricamento:
145800931105     C                   CLOSE     TABEL00F
145900090916      *
146000090916      *----
146100090916     C                   Z-ADD     0             CP                5 0
146200090916     C                   movel(P)  'CTP'         tbeCOD
146300090916     c                   clear                   KCliPar
146400090916     c                   clear                   KCliParTSR
146500090916     c                   clear                   KTarStdNew
146600090916     c                   clear                   KTarStd
146700090916     c                   clear                   KTarNew
146800090916     c                   clear                   KTarObb
146900090916     c                   clear                   KTarUgu
147000090916      *----
147100090916     C     tbeCOD        setll     tntbe01l
147200090916     C     tbeCOD        reade     tntbe01l
147300090916      *
147400090916     c                   dow       not %eof(tntbe01l)
147500090916     C                   add       1             CP
147600090916     C                   MOVEL     TBeKE1        KCliPar(CP)
147700090916     C                   MOVEL     TBeKE1        KCliParTSR(CP)
147800090916     C                   MOVEL     TBeKE2        primoByte         1
147900090916      * il tipo servizio alla destra del codice cliente
148000090916     C                   MOVE      primoByte     KCliParTSR(CP)
148100090916     C                   MOVEL     TBeKE2        KTarStd(CP)
148200090916     C                   movel     tbeuni        dCTP
148300090916     C                   MOVEL     TBeKE2        KTarStdNew(CP)
148400090916     C                   MOVE      §CTPCTD       KTarStdNew(CP)
148500090916     C                   MOVEL     §CTPCTD       KTarnew(CP)
148600090916     C                   MOVEL     §CTPOBB       KTarobb(CP)
148700090916     C                   MOVEL     §CTPUGU       KTarugu(CP)
148800090916     C     tbeCOD        reade     tntbe01l
148900090916     c                   enddo
149000090916      *----
149100090916      *
149200931108     C                   ENDSR
149300101112     C**************************************************************************
149400101112     C* decodifica la società
149500101112     C************************************************************
149600101112     C     Decod_societa begSR
149700110613     C*
149800170105     C                   clear                   cfisc            20
149900110613     ** Inizializzo il programma.
150000110613     C                   CALL      'TIBSSOCR'
150100110613     C                   PARM      'INIT'        prmRqsOpCode
150200110613     C                   PARM                    prmRpyOpCode
150300110613     C                   PARM                    prmRpyIdMsg
150400101112      *
150500110613      *
150600101112     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
150700101112     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
150800101112     C                   RESET                   tibsSocI0
150900101112     C                   EVAL      tibsSocI0.idSocieta = fttsoc
151000110509      * x data
151100110509     c                   move      Fttddc        dataiso
151200110509     C                   EVAL      tibsSocI0.dtvalidita = dataiso
151300101112     C
151400101112     C                   CALL      'TIBSSOCR'
151500101112     C                   PARM      'GETANAGRAF'  prmRqsOpCode
151600101112     C                   PARM      *BLANK        prmRpyOpCode
151700101112     C                   PARM      *ZERO         prmRpyIdMsg
151800101112     C                   PARM      'TIBSSOCI0'   prmRqsFormato
151900101112     C                   PARM                    tibsSocI0
152000101112     C                   PARM                    prmRqsDataSize
152100101112     C                   PARM      'TIBSSOCO0'   prmRpyFormato
152200101112     C                   PARM                    tibsSocO0
152300101112     C                   PARM                    prmRpyDataSize
152400101112     c                   if         PRMRPYIDMSG >= 0
152500101112     c                              and TIBSSOCO0.IDSOCIETA <> *blank
152600101112     c                   eval       dessoc   = tibsSocO0.RAGSOCIALE
152700170105     C                   eval       cfisc    = tibsSocO0.CODFISCALE
152800170105     c                   exsr      RETRIEVE_PEC
152900170105     c                   end
153000101112      *
153100101112     C                   ENDSR
153200170105      *----------------------------------------------------------------------
153300170105      *   reperisce la PEC se c'è
153400170105      *----------------------------------------------------------------------
153500170105     C     RETRIEVE_PEC  BEGSR
153600170105      *
153700170105     C                   if        cfisc <> ' '
153800170105     C                   eval       prmI_CFisc = cFisc
153900170105     C                   clear                   prmO_PECEML
154000170105     C                   CALL      'FICNPECR'
154100170105     C                   PARM                    prmI_CFisc       20
154200170105     C                   PARM                    prmO_PECEML     100
154300170105      *
154400170105     C                   eval       indPEC = 'PEC: ' + %trim(prmO_PECEML)
154500170105     c                   end
154600170105      *
154700170105     C                   ENDSR
154800931105      *--------------------------------------------------------------*
154900931105     O*  STAMPA ERRORI !!!!!!!!
155000931105      *--------------------------------------------------------------*
155100931105     OSYSPRT    E            TESERR           01
155200931105     O                       STT(1)              66
155300931105     O                       STT(2)             132
155400931105     O                       RSUT                20
155500931105     O                       CODFIL              32
155600931105     O                       UDATE              118 '  /  /  '
155700931105     O                       PAGE          Z    132
155800931105     O          E            TESERR           03
155900931105     O                       TIMES              118 '  .  .  '
156000931105     O          E            TESERR           06
156100931105     O                                           24 'SI RACCOMANDA DI CONSEGN'
156200931105     O                                           48 'ARE QUESTA STAMPA ALL''U'
156300931105     O                                           72 'FFICIO  C.E.D.!!!!!!    '
156400931105     O          E            TESERR      0  2
156500931105     O                                           24 'SI RACCOMANDA DI CONSEGN'
156600931105     O                                           48 'ARE QUESTA STAMPA ALL''U'
156700931105     O                                           72 'FFICIO  C.E.D.!!!!!!    '
156800931105     O          E            DETERR         1
156900931105     O                                           24 'NON CARICATI TUTTI GLI E'
157000931105     O                                           48 'LEMENTI DELLA TABELLA '
157100931105     O               90                             '2A'
157200931105     O               92                             '1A'
157300941201     O               93                             'CV'
157400931105      *--------------------------------------------------------------*
157500931108** STT
157600931029                                    ***  C H I U S U R A     D I S
157700931108 T I N T A    C O N S E G N A  ***  FLFRD0R              PAG.
