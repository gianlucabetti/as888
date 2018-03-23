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
016300931102      *--------------------------------------------------------------*
016400931102      *  CICLO PRINCIPALE:                                           *
016500931102      *--------------------------------------------------------------*
016600020529     c                   exsr      defcam
016700020506     C*   GESTIONE CONSEGNE
016800941201     C     DD0TSR        IFNE      'R'
016900941201     C                   MOVE      'C'           KTSR
017000931108     C                   SETON                                        03
017100020507     C*  Azzero campi per stampa totali
017200020507     C                   EXSR      AZZFIN
017300931104     C*  Loop di letture distinte padroncini nella data considerata
017400011114     C     KFTT          CHAIN     fiftt02L                           70
017500931104     C*
017600931109     C     *IN70         IFEQ      '0'
017700931104     C     *IN70         DOWEQ     '0'
017800020506     c                   EXSR      DOCCON
017900020506     c                   z-add     1             ok
018000950109     C*  Leggo la distinta successiva
018100011114     C     KFTT          READE     fiftt02L                               70
018200931104     C                   END
018300931108     C*--------------------------------------------------------------*
018400931109     C                   EXSR      TOTFIN
018500931109     C                   END
018600931109     C*--------------------------------------------------------------*
018700931108     C                   END
018800931108     C*--------------------------------------------------------------*
018900020506     C*   GESTIONE RITIRI
019000941201     C     DD0TSR        IFNE      'C'
019100931125     C                   SETOFF                                       03
019200931109     C                   SETON                                        04
019300950110     C*  Azzero campi per stampa prospetto totale ritiri
019400950110     C                   EXSR      AZZTOG
019500950110     C*
019600941201     C                   MOVE      'R'           KTSR
019700011114     C     KFTT          CHAIN     fiftt02L                           70
019800950109     C     *IN70         IFEQ      '0'
019900950109     C     *IN70         DOWEQ     '0'
020000931109     C                   EXSR      DOCRIT
020100950109     C*  Leggo la distinta successiva
020200011114     C     KFTT          READE     fiftt02L                               70
020300931109     C                   END
020400950109     C*  Totale finale ritiri
020500950110     C                   EXSR      WRTTOG
020600950109     C                   END
020700950109     C*
020800950109     C                   END
020900931108     C*--------------------------------------------------------------*
021000931110     C*   Stampo totale complessivo se richiesta sia la stampa dei
021100931110     C*   dati relativi alle consegne che quelle relative ai ritiri
021200941201     C     DD0TSR        IFEQ      ' '
021300931111     C                   SETOFF                                       0304
021400931110     C                   EXSR      WRTGEN
021500931110     C                   END
021600020529      *--------------------------------------------------------------*
021700020529     C     FINE          TAG
021800101112      *
021900101112     ** Chiudo il programma.
022000101112     C                   CALL      'TIBSSOCR'
022100101112     C                   PARM      'FINALIZE'    prmRqsOpCode
022200101112     C                   PARM                    prmRpyOpCode
022300101112     C                   PARM                    prmRpyIdMsg
022400101112
022500020529     c                   if        simfel <> 0
022600020529     C                   SETON                                        LR
022700020529     c                   else
022800020529     C                   SETON                                        RT
022900020529     c                   end
023000950110      *--------------------------------------------------------------*
023100020515      *  srfor : Decodifico il fornitore                             *
023200950110      *--------------------------------------------------------------*
023300020515     C     srfor         BEGSR
023400110613      *
023500020515     c                   movel     fttsoc        socp
023600020529     c                   clear                   dessoc
023700110613      *
023800110613     c****************   if        simfel = 0
023900110613      *
024000101112     c**** fttsoc        chain     ansif01l
024100101112     c***********        if        %found(ansif01l)
024200101112     c***********        movel     sifdesbrev    dessoc
024300101112     c***********        end
024400101112      *
024500101112      **
024600101112     c                   clear                   dessoc
024700101112     c                   exsr      decod_societa
024800101112      **
024900101112      *
025000110613     c**********         end
025100110613      *
025200020515     c                   move      fttcdf        forp
025300020529     c                   move      fttnff        nrfatp
025400020529     C                   MOVE      FTTDft        G02INV
025500020529     C                   MOVEL     '3'           G02ERR
025600020529     C                   CALL      'XSRDA8'
025700020529     C                   PARM                    WLBDA8
025800020529     C                   MOVE      G02DAT        dtfatp
025900020515     c                   move      fttcdf        tlzpdr
026000020515     c     ktlz          chain     tntlz01l
026100020515     c                   if        %found(tntlz01l)
026200020515     c                   movel     tlzrsc        desfor
026300020515     c                   else
026400020515     c                   movel     *all'?'       desfor
026500020515     c                   end
026600020515     C                   ENDSR
026700020515      *--------------------------------------------------------------*
026800020515      *  AZZTOG: Azzero totali generali ritiri                       *
026900020515      *--------------------------------------------------------------*
027000020515     C     AZZTOG        BEGSR
027100950110     C*
027200950110     C                   Z-ADD     0             TOGSNA
027300950110     C                   Z-ADD     0             TOGCLA
027400950110     C                   Z-ADD     0             TOGKFA
027500950110     C                   Z-ADD     0             TOGTVL
027600950110     C                   Z-ADD     0             TOGSPE
027700950110     C                   Z-ADD     0             TOGNCL
027800950110     C                   Z-ADD     0             TOGVOL
027900950110     C                   Z-ADD     0             TOGCLI
028000020507     C                   Z-ADD     0             TOGPKG
028100950110     C*
028200950110     C                   ENDSR
028300931104      *--------------------------------------------------------------*
028400931109      *  DOCCON: Lettura distinte x numero documento                 *
028500931104      *--------------------------------------------------------------*
028600931109     C     DOCCON        BEGSR
028700931104     C*
028800940318     C*  Incremeto totali sommapesi giornata
028900940318     C                   ADD       FTTSNA        TOTSNA
029000940318     C                   ADD       FTTCLA        TOTCLA
029100940318     C                   ADD       FTTKFA        TOTKFA
029200940318     C                   ADD       FTTTVL        TOTTVL
029300941201     C                   MOVE      FTTNDC        KNDC
029400950113     C                   MOVE      FTTDDC        KDDCT
029500931104     C*  Posizionamento iniziale x loop letture dettaglio distinta
029600011114     C     KFTD          CHAIN     fiftd03L                           80
029700931104     C*
029800931104     C     *IN80         IFEQ      '0'
029900931104     C*  Scrivo dati testate distinta
030000931109     C                   EXSR      TESCON
030100931104     C     *IN80         DOWEQ     '0'
030200931104     C*  Scrittura dati di dettaglio distinta
030300931109     C                   EXSR      DETCON
030400011114     C     KFTD          READE     fiftd03L                               80
030500931104     C                   END
030600931104     C*
030700931111     C*  Scrivo dati totale distinta consegna
030800931111     C                   EXSR      TOTCON
030900931104     C                   END
031000931105     C*
031100931104     C                   ENDSR
031200931109      *--------------------------------------------------------------*
031300931109      *  DOCRIT: Lettura distinte ritiri                             *
031400931109      *--------------------------------------------------------------*
031500931109     C     DOCRIT        BEGSR
031600950109     C*
031700950109     C                   MOVE      FTTNDC        KNDC
031800950113     C                   MOVE      FTTDDC        KDDCT
031900011121     c                   move      fttdiv        coddiv
032000011121     C*
032100931109     C*  Posizionamento iniziale x loop letture dettaglio distinta
032200011114     C     KFTD          CHAIN     fiftd03L                           80
032300931109     C*
032400931109     C     *IN80         IFEQ      '0'
032500931109     C*  Scrivo dati testate distinta
032600931109     C                   EXSR      TESRIT
032700020315      * cambiato il ciclo per poter gestire i nuovi ritiri
032800931109     C     *IN80         DOWEQ     '0'
032900020315      *
033000020315      * salva per eseguire a rottura di stop
033100020315     c                   z-add     ftdnss        savnss
033200020315     c                   z-add     ftdstp        savstp
033300020315      *   salva il nome e il codice tariffa
033400020315     C                   movel     ftdRSC        savrsc
033500020315     C                   MOVE      FTDCTP        savCTP
033600020315     C                   MOVE      FTDfit        savFIT
033700020315     c                   MOVE      FTDfia        savFIA
033800020315     C*  da totalizzare
033900020315     C                   z-add     0             addSET
034000020315     C                   z-add     0             addNCL
034100020315     C                   z-add     0             addpkl
034200020315     C                   z-add     0             addvlu
034300020315     C                   z-add     0             additt
034400020315     C                   z-add     0             addita
034500020315      *
034600020315      *  x stop di ritiro
034700020315     C     *in80         doweq     *off
034800020315     c     ftdnss        andeq     savnss
034900020315     c     ftdstp        andeq     savstp
035000020315      *
035100020315     C*  cumula i campi per stop
035200020315     C                   EXSR      AddRIT
035300020315      *
035400011114     C     KFTD          READE     fiftd03L                               80
035500020315     c                   enddo
035600020315      *
035700020315     C*  Scrittura dati di dettaglio distinta
035800020315     C                   EXSR      DETRIT
035900020315      *
036000020315     C                   ENDdo
036100931109     C*
036200950109     C*  Scrivo dati totale numero ritiro
036300950109     C                   EXSR      TOTRIT
036400931109     C                   END
036500950109     C*
036600931109     C                   ENDSR
036700931105      *--------------------------------------------------------------*
036800020507      *  AZZFIN:Azzera dati di riepilogo distinte                    *
036900931105      *--------------------------------------------------------------*
037000020507     C     AZZFIN        BEGSR
037100931105     C*
037200020507     C*
037300020507     c                   z-add     0             spetot
037400020507     c                   z-add     0             spenoc
037500020507     c                   z-add     0             TOTP
037600020507     c                   z-add     0             TOTCOL
037700020507     c                   z-add     0             TOTPKG
037800020507     c                   z-add     0             TOTVOL
037900020507     c                   z-add     0             TOTVOL
038000020507     c                   z-add     0             NUMSTP
038100020507     c                   z-add     0             TOTCP
038200020507     c                   z-add     0             TOTNF
038300020507     c                   z-add     0             TOTGC
038400020507     c                   z-add     0             TOTAV
038500020507     c                   z-add     0             TOTNCP
038600020507     c                   z-add     0             TOTESG
038700020507     c                   z-add     0             inccnt
038800020507     c                   z-add     0             nsass
038900020507     c                   z-add     0             dirass
039000020507     c                   z-add     0             TOTEV
039100020507     c                   z-add     0             inccnV
039200020507     c                   z-add     0             nsassV
039300020507     c                   z-add     0             dirasV
039400020507     C                   Z-ADD     0             TOTSNA
039500020507     C                   Z-ADD     0             TOTCLA
039600020507     C                   Z-ADD     0             TOTKFA
039700020507     C                   Z-ADD     0             TOTTVL
039800020507     C*
039900020507     C                   ENDSR
040000020507      *--------------------------------------------------------------*
040100020507      *  TOTFIN:stampa dati di riepilogo distinte                    *
040200020507      *--------------------------------------------------------------*
040300020507     C     TOTFIN        BEGSR
040400020507     C*
040500020507     C*  Valorizzo i campi di riepilogo
040600020507     C     SPETOT        SUB       SPENOC        SPECON
040700020507     c                   if        spetot <> 0
040800020507     C                   Z-ADD     SPENOC        SPENCO
040900020507     C*  Totali in lire
041000020507     C                   Z-ADD     INCCNT        TOTG              9 0
041100020507     C                   ADD       NSASS         TOTG
041200020507     C                   ADD       DIRASS        TOTG
041300020507     C                   Z-ADD     TOTG          TOTESG
041400020507     C*  Totali in valuta
041500020507     C                   Z-ADD     INCCNV        TOTEV
041600020507     C                   ADD       NSASSV        TOTEV
041700020507     C                   ADD       DIRASV        TOTEV
041800020507     C*
041900020507     C*  Controlla se ci sono valori nei totali
042000020507     C                   SETOFF                                       1112
042100020507     C*
042200020507     C*  Imposta l'ovrflow da testare
042300020507     C                   Z-ADD     55            OVRLIN            3 0
042400020507     C     TOTCP         IFGT      0
042500020507     C                   SETON                                        11
042600020507     C                   SUB       1             OVRLIN
042700020507     C                   END
042800020507     C     TOTNCP        IFGT      0
042900020507     C                   SETON                                        12
043000020507     C                   SUB       1             OVRLIN
043100020507     C                   END
043200020507     C*
043300020507     C*  Scrittura dati di totale
043400020507     C     LIN           IFGE      OVRLIN
043500020507     C                   WRITE     TES00
043600020507     C                   END
043700020507     C*
043800020507     C                   WRITE     TOTAL
043900020507     C*
044000020507     C*  Controllo se devo scrivere il totale in lire
044100020507     C     INCCNT        IFNE      0
044200020507     C     NSASS         ORNE      0
044300020507     C     DIRASS        ORNE      0
044400020507     C     TOTESG        ORNE      0
044500020507     C                   WRITE     TOTESL
044600020507     C                   END
044700020507     C*  Controllo se devo scrivere il totale in valuta
044800020507     C     INCCNV        IFNE      0
044900020507     C     NSASSV        ORNE      0
045000020507     C     DIRASV        ORNE      0
045100020507     C     TOTEV         ORNE      0
045200020507     C                   WRITE     TOTESV
045300020507     C                   END
045400020507     C                   WRITE     TOTSMP
045500020507     c                   end
045600020507     C*
045700020507     C                   ENDSR
045800931104      *--------------------------------------------------------------*
045900931109      *  TESCON: Preparo la scrittura dei dati di testata x consegne *
046000931104      *--------------------------------------------------------------*
046100931109     C     TESCON        BEGSR
046200931104     C*
046300960214     C                   Z-ADD     0             STP
046400960214     C                   Z-ADD     0             F
046500931108     C                   MOVE      FTTCUC        CODUTE
046600931104     C                   MOVE      FTDNDC        NUMDOC
046700941201     C                   MOVE      FTTDDC        G02INV
046800941213     C                   MOVEL     '3'           G02ERR
046900941201     C                   CALL      'XSRDA8'
047000941201     C                   PARM                    WLBDA8
047100941201     C                   MOVE      G02DAT        STPDDC
047200011121     c                   move      fttdiv        coddiv
047300931108     C*
047400020515     C*  Scrittura testa  totale
047500940321     C     LIN           IFGE      61
047600940321     C                   WRITE     TES00
047700020515     C                   WRITE     TES01
047800940321     C                   SETOFF                                       05
047900940321     C                   ELSE
048000931108     C                   WRITE     SALTO
048100940321     C                   END
048200020515     C                   WRITE     TES03
048300931104     C*
048400931104     C                   ENDSR
048500931109      *--------------------------------------------------------------*
048600931109      *  TOTRIT:Stampa dati di riepilogo x ritiri                    *
048700931109      *--------------------------------------------------------------*
048800931109     C     TOTRIT        BEGSR
048900931111     C*
049000931111     C*  Calcolo totali ritiro
049100931111     C                   MOVE      FTTSET        TOPSPE
049200950110     C                   ADD       FTTSET        TOGSPE
049300931203     C                   MOVE      NUMSTP        TOPCLI
049400950110     C                   ADD       NUMSTP        TOGCLI
049500030107     C*
049600030107      * Valore Ritiri Annullati
049700030107     c                   setoff                                       77
049800030107     c                   if        ftttbs > 0
049900030107     c                   seton                                        77
050000030131     C                   z-add     ftttbs        topTBS            7 0
050100030107     c                   end
050200931111     C*
050300931111     C*  Preparo Stampa dati di tassazione
050400931111     C                   MOVE      FTTITT        TOPTAR
050500931111     C                   MOVE      FTTITA        TOPACC
050600011121     C                   move      fttdiv        TOPdiv
050700011114     C* >>>>>>>>> divisa
050800931111     C                   MOVE      *BLANKS       TOPTAF
050900931111     C                   MOVE      *BLANKS       TOPACF
051000931111     C     FTTFIT        IFNE      *BLANK
051100931111     C                   MOVE      'MAN'         TOPTAF                         Inserimento man.
051200931111     C                   END
051300931111     C     FTTFIA        IFNE      *BLANK
051400931111     C                   MOVE      'MAN'         TOPACF                         Inserimento man.
051500931111     C                   END
051600931111     C*
051700931111     C*  Scrittura dati di totale padroncino
051800940321     C     LIN           IFGE      63
051900940321     C                   WRITE     TES00
052000940321     C                   SETOFF                                       05
052100940321     C                   END
052200931111     C                   WRITE     TOTPD
052300950110     C*  Incremento totali generali ritiri
052400950110     C                   ADD       FTTSNA        TOGSNA
052500950110     C                   ADD       FTTCLA        TOGCLA
052600950110     C                   ADD       FTTKFA        TOGKFA
052700950110     C                   ADD       FTTTVL        TOGTVL
052800931109     C*
052900931109     C                   ENDSR
053000020507      *--------------------------------------------------------------*
053100020507      *  WRTTOG:Stampa dati totali ritiri                            *
053200020507      *--------------------------------------------------------------*
053300020507     C     WRTTOG        BEGSR
053400020507     C*
053500020507     c                   if        togspe <> 0
053600020507     C*  Scrittura dati di totale
053700020507     C     LIN           IFGE      57
053800020507     C                   WRITE     TES00
053900020507     C                   END
054000020507     C                   WRITE     TOTSPG
054100020507     c                   end
054200020507     C*
054300020507     C                   ENDSR
054400931111      *--------------------------------------------------------------*
054500931111      *  TOTCON:Stampa dati di riepilogo x distinta consegna         *
054600931111      *--------------------------------------------------------------*
054700931111     C     TOTCON        BEGSR
054800931111     C*
054900931111     C*  Preparo Stampa dati di tassazione
055000931111     C                   MOVE      FTTITT        TASTAD
055100931111     C                   MOVE      FTTITA        TASACD
055200011121     C                   move      fttdiv        tasdiv
055300011114      * >>>>>>>> divisa
055400931111     C                   MOVE      *BLANKS       FLGTAD
055500931111     C                   MOVE      *BLANKS       FLGACD
055600931111     C     FTTFIT        IFNE      *BLANK
055700931111     C                   MOVE      'MAN'         FLGTAD                         Inserimento man.
055800931111     C                   END
055900931111     C     FTTFIA        IFNE      *BLANK
056000931111     C                   MOVE      'MAN'         FLGACD                         Inserimento man.
056100931111     C                   END
056200931111     C*
056300931111     C*
056400931111     C*  Scrittura dati di totale padroncino
056500940321     C     LIN           IFGE      64
056600940321     C                   WRITE     TES00
056700940321     C                   SETOFF                                       05
056800940321     C                   END
056900940321     C   05              SETOFF                                       05
057000931111     C                   WRITE     TOT01
057100931111     C*
057200931111     C*  Aggiorno dati di riepilogo
057300931111     C                   ADD       FTTSET        SPETOT
057400931111     C                   ADD       FTTSNT        SPENOC
057500931111     C*
057600931111     C                   ENDSR
057700931109      *--------------------------------------------------------------*
057800931109      *  TESRIT: Preparo la scrittura dei dati di testata x ritiri   *
057900931109      *--------------------------------------------------------------*
058000931109     C     TESRIT        BEGSR
058100931109     C*
058200960214     C                   Z-ADD     0             STP
058300960214     C                   Z-ADD     0             F
058400020506     C*  Scrittura testa totale
058500020613     C     LIN           IFGE      60
058600020506     c     ok            oreq      0
058700940321     C                   WRITE     TES00
058800940321     C                   WRITE     TES02
058900931109     C                   SETOFF                                       05
059000940321     C                   ELSE
059100931109     C                   WRITE     SALT2
059200940321     C                   END
059300931109     C*
059400931109     C*  Giro la data distinta
059500931203     C                   Z-ADD     0             STP
059600931203     C                   Z-ADD     0             NUMSTP
059700941213     C                   MOVE      KDDC          G02INV
059800941202     C                   MOVEL     '3'           G02ERR
059900941202     C                   CALL      'XSRDA8'
060000941202     C                   PARM                    WLBDA8
060100941202     C                   MOVE      G02DAT        DRT
060200950109     C                   MOVE      FTTNDC        NRT
060300950110     C                   Z-ADD     0             TOPSPE
060400950110     C                   Z-ADD     0             TOPNCL
060500950110     C                   Z-ADD     0             TOPPKG
060600950110     C                   Z-ADD     0             TOPVOL
060700950110     C                   Z-ADD     0             TOPCLI
060800950110     C                   Z-ADD     0             TOPTAR
060900950110     C                   Z-ADD     0             TOPACC
061000950110     C                   MOVEL     *BLANKS       TOPTAF
061100950110     C                   MOVEL     *BLANKS       TOPACF
061200011121     C                   movel     *BLANKS       TOPdiv
061300931109     C*
061400931109     C                   ENDSR
061500931104      *--------------------------------------------------------------*
061600931109      *  DETCON: Scrittura dati dettaglio distinta consegna          *
061700931104      *--------------------------------------------------------------*
061800931109     C     DETCON        BEGSR
061900160503     C* tolto dal PRTF Bocchi
062000160503     C*********          MOVE      FTDHMC        ORACON
062100941202     C                   MOVE      FTDDCM        G02INV
062200941202     C                   MOVEL     '3'           G02ERR
062300941202     C                   CALL      'XSRDA8'
062400941202     C                   PARM                    WLBDA8
062500011121     C                   MOVE      G02DAT        DATCON            8 0
062600011121     c                   movel     datcon        datco6
062700011121     c                   move      datcon        fld2              2
062800011121     c                   move      fld2          datco6                         ggmmaa
062900931108     C                   MOVE      FTDAAS        AAS
063000931104     C                   MOVE      FTDLNP        LNP
063100931105     C                   MOVE      FTDNRS        NRS
063200931104     C                   MOVE      FTDNSP        NSP
063300931104     C                   MOVE      FTDNCL        NCL
063400941213     C                   MOVE      FTDPKL        PKG
063500941213     C                   MOVE      FTDVLU        VOL
063600931104     C                   MOVE      FTDCRC        RISER
063700931104     C                   MOVE      FTDTMC        TMC
063800931104     C*----------------------------------------------------*
063900931104     C*  Controllo se consegna parziale
064000931105     C*----------------------------------------------------*
064100011121     C                   MOVEL     *BLANKS       DESCON           19
064200931104     C     FTDCMC        IFEQ      'P  '
064300931104     C     FTDCMC        OREQ      '   '
064400931105     C                   MOVEL     FTDCMC        CPZ               1
064500931104     C                   MOVE      *BLANKS       CMC
064600931105     C*  Incremento totale colli, peso, volume x consegnati
064700931105     C                   ADD       1             TOTF              3 0          Fatte
064800931105     C                   ADD       FTDNCL        TOTCOL
064900941213     C                   ADD       FTDPKL        TOTPKG
065000941213     C                   ADD       FTDVLU        TOTVOL
065100931105     C*----------------------------------------------------*
065200931104     C                   ELSE
065300931105     C*----------------------------------------------------*
065400931105     C                   Z-ADD     1             Q
065500931105     C     FTDCMC        LOOKUP    C2A(Q)                                 15
065600931105     C   15              MOVEL     D2A(Q)        DESCON
065700931105     C*  Controllo il tipo di mancata consegna
065800931105     C     FTDTMC        IFEQ      'N'
065900931105     C                   ADD       1             TOTNF             3 0           Non fatte
066000931105     C                   ELSE
066100931105     C     FTDTMC        IFEQ      'A'
066200931105     C                   ADD       1             TOTAV             3 0           Lasciato avv.
066300931105     C                   ELSE
066400931105     C     FTDTMC        IFEQ      'G'
066500931105     C                   ADD       1             TOTGC             3 0           Giacenti
066600931105     C                   END
066700931105     C                   END
066800931105     C                   END
066900931104     C                   MOVE      FTDCMC        CMC
067000931104     C                   MOVE      *BLANKS       CPZ
067100931105     C*----------------------------------------------------*
067200931104     C                   END
067300931105     C*----------------------------------------------------*
067400931105     C*  Controllo se consegna parziale
067500931105     C     CPZ           IFEQ      'P'
067600931105     C                   ADD       1             TOTP              3 0
067700931105     C                   END
067800931108     C*----------------------------------------------------*
067900931108     C     FTDCMC        IFEQ      'T  '
068000931108     C     FTDCMC        OREQ      'N  '
068100931108     C                   MOVE      *BLANKS       CMC
068200931108     C                   END
068300931104     C*----------------------------------------------------*
068400941202     C                   MOVE      FTDAAS        KAAS
068500941202     C                   MOVE      FTDLNP        KLNP
068600941202     C                   MOVE      FTDNRS        KNRS
068700941202     C                   MOVE      FTDNSP        KNSP
068800941213     C                   MOVEL     FTDRSC        RSD
068900941202     C*  Reperisco i dati del destinatario
069000020506     C*                  MOVEL     *BLANKS       ARBCAD
069100020506     C*                  MOVE      *ZEROS        ARBMGS
069200020506     C                   MOVEL     *BLANKS       tasCAD
069300020506     C                   MOVE      *ZEROS        tasMGS
069400020506     c                   if        simfel = 0
069500020506     C     KARB          CHAIN     titas30c                           72
069600020506     c                   else
069700941202     C     KARB          CHAIN     FNARB01L                           72
069800020506     C                   MOVE      ARBTC1        CONCP1
069900020506     c                   end
070000090916      ****  Identifica TTL con un Asterisco in Stampa prima dell'importo
070100090916     c                   clear                   cliTTL
070200090916     c     ARBksc        lookup    KCliPar                                33
070300090916     c   33              move      'T'           cliTTL
070400090916      ****
070500020506     C*                  MOVE      ARBTC2        CONCP2
070600020506     C*                  MOVEL     ARBCAD        CAP
070700020506     C*                  MOVEL     ARBRSD        RSD
070800020506     C*                  MOVE      ARBMGS        GG                2 0
070900020506     C*                  MOVEL     ARBMGS        MM                2 0
071000020506     C                   MOVE      tasTC2        CONCP2
071100020506     C                   MOVEL     tasCAD        CAP
071200020506     C                   MOVEL     tasRSD        RSD
071300020506     C                   MOVE      tasMGS        GG                2 0
071400020506     C                   MOVEL     tasMGS        MM                2 0
071500941213     C                   MOVEL     GG            STPGMS
071600941213     C                   MOVE      MM            STPGMS
071700931104     C*----------------------------------------------------*
071800931108     C*  Controllo se spedizione non consegnata o consegnata
071900931108     C     FTDCMC        IFEQ      'P  '
072000931108     C     FTDCMC        OREQ      '   '
072100931116     C* Calcolo il n mero di stop
072200931108     C     FTDSTP        IFEQ      *ZEROS
072300011121     C                   ADD       1             NUMSTP            5 0
072400931108     C                   ELSE
072500931116     C     FTDSTP        LOOKUP    STP                                    15
072600931108     C  N15              DO
072700931108     C                   ADD       1             NUMSTP
072800931108     C                   ADD       1             F                 3 0
072900931108     C                   MOVE      FTDSTP        STP(F)
073000931108     C                   END
073100931108     C                   END
073200931108     C                   END
073300931108     C*----------------------------------------------------*
073400931108     C*  Stampo i dati di tassazione dei padroncini
073500931109     C                   MOVE      FTDCTP        CODTAR
073600931108     C                   MOVE      FTDITT        TASTAR
073700931109     C                   MOVE      FTDITA        TASACC
073800011114      *>>>>>>>> divisa
073900931109     C                   MOVE      *BLANKS       FLGTAR
074000931109     C                   MOVE      *BLANKS       FLGACC
074100931109     C     FTDFIT        IFNE      *BLANK
074200931109     C                   MOVE      'MAN'         FLGTAR                         Inserimento man.
074300931109     C                   END
074400931109     C     FTDFIA        IFNE      *BLANK
074500931109     C                   MOVE      'MAN'         FLGACC                         Inserimento man.
074600000623     C                   END
074700000623      *----------------------------------------------------*
074800000623      *TESTO SE FILIALE POSTE
074900000623     C     LNP           LOOKUP    FIP                                    88
075000000623     C     *IN88         IFEQ      *ON
075100000623     C     CMC           IFEQ      *BLANK
075200000703     C     TMC           ANDEQ     *BLANK
075300000623      * CONSEGNATO
075400000623     C                   ADD       1             TOTCP             3 0           Fatte
075500000623     C                   ELSE
075600000623      * NON CONSEGNATO
075700000623     C                   ADD       1             TOTNCP            3 0           Non fatte
075800000623     C                   END
075900000623     C                   ENDIF
076000000623     C*----------------------------------------------------*
076100020515     c                   exsr      salpag
076200931108     C                   WRITE     DET01
076300950109     C*----------------------------------------------------*
076400950109     C*  Stampa dati consegna richiesta
076500950110     C                   MOVEL     'N'           STSAL             1
076600950109     C                   EXSR      STPRIC
076700931104     C*----------------------------------------------------*
076800931104     C*  Controllo se ci sono importi da stampare
076900931104     C                   EXSR      GESIMP
077000950110     C*----------------------------------------------------*
077100950110     C     STSAL         IFEQ      'S'
077200020515     c                   exsr      salpag
077300950110     C                   WRITE     SALRIG
077400950110     C                   END
077500931104     C*----------------------------------------------------*
077600931104     C                   ENDSR
077700950109      *--------------------------------------------------------------*
077800950109      *  STPRIC: Scrittura dati consegna richiesta                   *
077900950109      *--------------------------------------------------------------*
078000950109     C     STPRIC        BEGSR
078100950109     C                   MOVEL     *BLANKS       TIPRIC
078200950109     C                   Z-ADD     0             DATRIC
078300020506     C*                  Z-ADD     ARBHCR        ORARIC
078400950109     C*  Controllo se devo gestire tipo di consegna richiesta
078500020506     C*    ARBDCR        IFNE      *ZEROS
078600020506     C*    ARBHCR        ORNE      *ZEROS
078700020506     C*    ARBTCR        ORNE      *BLANKS
078800950109     C*  Giro la data consegna richiesta
078900020506     C*    ARBDCR        IFNE      0
079000020506     C*                  MOVE      ARBDCR        G02INV
079100020506     c                   Z-ADD     tasHCR        ORARIC
079200020506     C*  Controllo se devo gestire tipo di consegna richiesta
079300020506     C     tasDCR        IFNE      *ZEROS
079400020506     C     tasHCR        ORNE      *ZEROS
079500020506     C     tasTCR        ORNE      *BLANKS
079600020506     C*  Giro la data consegna richiesta
079700020506     C     tasDCR        IFNE      0
079800020506     C                   MOVE      tasDCR        G02INV
079900950109     C                   MOVE      '3'           G02ERR
080000950109     C                   CALL      'XSRDA8'
080100950109     C                   PARM                    WLBDA8
080200011122     C                   MOVE      G02DAT        fld80             8 0
080300011122     C                   movel     G02DAT        DATRIC
080400011122     C                   move      G02DAT        fld20             2 0
080500011122     C                   move      fld20         DATRIC
080600950109     C                   END
080700950109     C*  Consegna richiesta il  gg./alle ore
080800020506     C*    ARBTCR        IFEQ      ' '
080900020506     C*    ARBDCR        IFNE      *ZERO
081000020506     C     tasTCR        IFEQ      ' '
081100020506     C     tasDCR        IFNE      *ZERO
081200950109     C                   MOVEL     'IL'          TIPRIC
081300950109     C                   ELSE
081400950109     C                   MOVE      'ALLE ORE'    TIPRIC
081500950109     C                   END
081600950109     C*  Consegna richiesta prima del/prima delle
081700950109     C                   ELSE
081800020506     C*    ARBTCR        IFEQ      'P'
081900020506     C*    ARBDCR        IFNE      *ZERO
082000020506     C     tasTCR        IFEQ      'P'
082100020506     C     tasDCR        IFNE      *ZERO
082200950109     C                   MOVEL     'PRIMA DE'    TIPRIC
082300950109     C                   MOVE      'L  '         TIPRIC
082400950109     C                   ELSE
082500950109     C                   MOVE      'PRIMA DE'    TIPRIC
082600950109     C                   MOVE      'LLE'         TIPRIC
082700950109     C                   END
082800950109     C*  Consegna richiesta dopo le/dopo il
082900950109     C                   ELSE
083000020506     C*    ARBTCR        IFEQ      'D'
083100020506     C*    ARBDCR        IFNE      *ZERO
083200020506     C     tasTCR        IFEQ      'D'
083300020506     C     tasDCR        IFNE      *ZERO
083400950109     C                   MOVEL     'DOPO  IL'    TIPRIC
083500950109     C                   ELSE
083600950109     C                   MOVE      'DOPO  LE'    TIPRIC
083700950109     C                   END
083800950109     C                   END
083900950109     C*
084000950109     C                   END
084100950109     C                   END
084200950109     C*----------------------------------------------------*
084300020515     c                   exsr      salpag
084400950110     C                   MOVEL     'S'           STSAL
084500950109     C                   WRITE     DETRIC
084600950109     C*----------------------------------------------------*
084700950109     C                   END
084800950109     C*----------------------------------------------------*
084900950109     C                   ENDSR
085000020315      *--------------------------------------------------------------*
085100020315      *  AddRIT: Cumula per Stop i ritiri dello stesso cliente       *
085200020315      *--------------------------------------------------------------*
085300020315     C     AddRIT        BEGSR
085400020315     C*
085500020315     C                   add       FTDSET        addSET
085600020315     C                   add       FTDNCL        addNCL
085700020315     C                   add       FTDPKL        addpkl
085800020315     C                   add       FTDVLU        addvlu
085900020315     C                   add       FTDITT        additt
086000020315     C                   add       FTDITA        addita
086100020315     C*
086200020315     C                   MOVE      FTDAAS        KAAS
086300020315     C                   MOVE      FTDLNP        KLNP
086400020315     C                   MOVE      FTDNRS        KNRS
086500020315     C                   MOVE      FTDNSP        KNSP
086600020315     C*
086700020315     C                   ENDSR
086800931109      *--------------------------------------------------------------*
086900931109      *  DETRIT: Scrittura dati dettaglio distinta ritiri            *
087000931109      *--------------------------------------------------------------*
087100931109     C     DETRIT        BEGSR
087200931109     C*
087300020315     C                   MOVE      savSTP        STOP
087400020315     C                   MOVEL     savRSC        RSM
087500020315     C*
087600020315     C                   z-add     addSET        SPE
087700020315     C                   z-add     addNCL        NCL
087800020315     C                   z-add     addPKL        PKG
087900020315     C                   z-add     addVLU        VOL
088000931109     C*
088100931109     C*  Preparo Stampa dati di tassazione
088200020315     C                   MOVE      savCTP        CODTAR
088300020315     C                   z-add     additt        TASTAR
088400020315     C                   MOVE      addita        TASACC
088500020315      *
088600931109     C                   MOVE      *BLANKS       FLGTAR
088700931109     C                   MOVE      *BLANKS       FLGACC
088800020315     C     savFIT        IFNE      *BLANK
088900931109     C                   MOVE      'MAN'         FLGTAR                         Inserimento man.
089000931109     C                   END
089100020315     C     savFIA        IFNE      *BLANK
089200931109     C                   MOVE      'MAN'         FLGACC                         Inserimento man.
089300931109     C                   END
089400931203     C*----------------------------------------------------*
089500931203     C* Calcolo il n mero di stop
089600020315     C     savSTP        IFEQ      *ZEROS
089700011121     C                   ADD       1             NUMSTP
089800931203     C                   ELSE
089900020315     C     savSTP        LOOKUP    STP                                    15
090000931203     C  N15              DO
090100931203     C                   ADD       1             NUMSTP
090200931203     C                   ADD       1             F                 3 0
090300020315     C                   MOVE      savSTP        STP(F)
090400931203     C                   END
090500931203     C                   END
090600931109     C*----------------------------------------------------*
090700940321     C     LIN           IFGE      65
090800931109     C                   WRITE     TES00
090900931109     C                   WRITE     TES02
091000931109     C                   SETOFF                                       05
091100931109     C                   END
091200931109     C                   WRITE     DET02
091300931109     C                   MOVE      *ZERO         DRT
091400950109     C                   MOVE      *ZERO         NRT
091500931109     C*----------------------------------------------------*
091600931111     C*  Incremento totali padroncino
091700931111     C                   ADD       NCL           TOPNCL
091800950110     C                   ADD       NCL           TOGNCL
091900931111     C                   ADD       PKG           TOPPKG
092000950110     C                   ADD       PKG           TOGPKG
092100931111     C                   ADD       VOL           TOPVOL
092200950110     C                   ADD       VOL           TOGVOL
092300931109     C*
092400931109     C                   ENDSR
092500931104      *--------------------------------------------------------------*
092600931104      *  GESIMP: Gestione scrittura importi C/A e assegnato          *
092700931104      *--------------------------------------------------------------*
092800931104     C     GESIMP        BEGSR
092900941213     C*----------------------------------------------------*
093000941213     C     FTDCAS        IFNE      *ZERO                                        C/A
093100941213     C*  Se esiste contrassegno controllo se in lire o in valuta
093200941213     C                   MOVE      'I'           WITA              1
093300941213     C     FTDVCA        IFNE      *BLANKS
093400941213     C                   Z-ADD     1             X                 3 0
093500941213     C     FTDVCA        LOOKUP    CCV(X)                                 32
093600941213     C   32              MOVEL     ITA(X)        WITA
093700941213     C                   END
093800941213     C                   END
093900941213     C*----------------------------------------------------*
094000931104     C*  Controllo se esiste importo C/A o Ass. e se si e se i dati
094100931104     C*  di pagamento sono uguali stampo una sola riga di totale
094200941202     C     FTDCAS        IFNE      *ZERO                                        C/A + Assegnato
094300941213     C     WITA          ANDEQ     'I'                                          C/A Italia
094400931104     C     FTDIFP        ANDNE     *ZERO
094500941202     C*  Dati di pagamento uguali
094600931104     C     FTDTIC        ANDEQ     FTDMIF
094700931104     C     FTDAS1        ANDEQ     FTDAS2
094800931104     C     FTDAB1        ANDEQ     FTDAB2
094900931104     C     FTDCA1        ANDEQ     FTDCA2
095000941213     C                   MOVE      FTDVCA        VALUTA
095100941213     C                   MOVE      *BLANKS       DESTIT           20
095200931104     C                   MOVE      FTDAS1        NUMASS
095300931104     C                   MOVE      FTDAB1        ABI
095400931104     C                   MOVE      FTDCA1        CAB
095500931105     C                   MOVE      FTDTIC        TIT               2
095600950110     C     FTDCAS        ADD       FTDIFP        IMPOR            13 3
095700991119     C                   MOVEL     'ITL'         VCA               3
095800931104     C*  Controllo se assegno
095900931104     C     FTDAS1        COMP      *BLANKS                            0101
096000931104     C* Ricerco descrizione tabella codici rimbordo
096100931104     C                   EXSR      DECTIT
096200931104     C* Scrittura record totale C/A + Assegnato
096300020515     c                   exsr      salpag
096400950110     C                   MOVE      IMPOR         COM3              3 0
096500950110     C                   Z-ADD     IMPOR         IMPOR1
096600950110     C                   Z-ADD     IMPOR         IMPOR2
096700950110     C     COM3          COMP      0                                      06
096800950110     C                   MOVEL     'N'           STSAL
096900931108     C                   WRITE     DETIMP
097000931104     C                   GOTO      FINIMP
097100931104     C                   END
097200931104     C*----------------------------------------------------*
097300931104     C*  Controllo se esiste importo C/A
097400941213     C     FTDCAS        IFNE      *ZERO                                        C/A + Assegnato
097500941213     C                   MOVE      FTDVCA        VALUTA
097600931104     C                   MOVE      *BLANKS       DESTIT
097700931104     C                   MOVE      FTDAS1        NUMASS
097800931104     C                   MOVE      FTDAB1        ABI
097900931104     C                   MOVE      FTDCA1        CAB
098000931105     C                   MOVE      FTDTIC        TIT
098100941213     C                   Z-ADD     FTDCAS        IMPOR
098200991119     C                   MOVEL     FTDVCA        VCA               3
098300931104     C*  Controllo se assegno
098400931104     C     FTDAS1        COMP      *BLANKS                            0101
098500931104     C* Ricerco descrizione tabella codici rimbordo
098600931104     C                   EXSR      DECTIT
098700931104     C* Scrittura record totale C/A + Assegnato
098800020515     c                   exsr      salpag
098900950110     C                   MOVE      IMPOR         COM3              3 0
099000950110     C                   Z-ADD     IMPOR         IMPOR1
099100950110     C                   Z-ADD     IMPOR         IMPOR2
099200950110     C     COM3          COMP      0                                      06
099300950110     C                   MOVEL     'N'           STSAL
099400931108     C                   WRITE     DETIMP
099500931104     C                   END
099600931104     C*----------------------------------------------------*
099700931104     C*  Controllo se esiste importo Assegnato
099800931104     C     FTDIFP        IFNE      *ZERO                                        C/A + Assegnato
099900931104     C                   MOVE      *BLANKS       DESTIT
100000941213     C                   MOVE      *BLANKS       VALUTA
100100931104     C                   MOVE      FTDAS2        NUMASS
100200931104     C                   MOVE      FTDAB2        ABI
100300931104     C                   MOVE      FTDCA2        CAB
100400931104     C                   MOVE      FTDMIF        TIT
100500931104     C                   Z-ADD     FTDIFP        IMPOR
100600020725     C                   IF        FTDDIV <> ' '
100700020725     C                   MOVEL     ftddiv        VCA
100800020725     C                   ELSE
100900020725     C                   MOVEL     'EUR'         VCA
101000020725     C                   END
101100931104     C*  Controllo se assegno
101200931104     C     FTDAS2        COMP      *BLANKS                            0101
101300931104     C* Ricerco descrizione tabella codici rimbordo
101400931104     C                   EXSR      DECTIT
101500931104     C* Scrittura record totale C/A + Assegnato
101600020515     c                   exsr      salpag
101700950110     C                   MOVE      IMPOR         COM3              3 0
101800950110     C                   Z-ADD     IMPOR         IMPOR1
101900950110     C                   Z-ADD     IMPOR         IMPOR2
102000950110     C     COM3          COMP      0                                      06
102100950110     C                   MOVEL     'N'           STSAL
102200931108     C                   WRITE     DETIMP
102300931104     C                   END
102400931104     C*----------------------------------------------------*
102500931104     C     FINIMP        ENDSR
102600931104      *--------------------------------------------------------------*
102700931104      *  DECTIT: Decodifico il titolo                                *
102800931104      *--------------------------------------------------------------*
102900931104     C     DECTIT        BEGSR
103000931104     C*----------------------------------------------------*
103100931104     C                   Z-ADD     1             Q
103200931105     C                   MOVE      *BLANKS       DESTIT
103300931105     C     TIT           IFNE      *BLANKS
103400931116     C     TIT           ANDNE     'SC'
103500931104     C     TIT           LOOKUP    CDT(Q)                                 15
103600931104     C   15              MOVEL     DST(Q)        DESTIT
103700931105     C                   MOVE      TIT           TPMB              1
103800931105     C*  Controllo se tipo di assegno mittente o bartolini e
103900931105     C*  incremento i relativi totali
104000991119     C     TPMB          IFEQ      'M'                                          02
104100991119     C     VCA           IFEQ      'ITL'                                        03
104200991119     C     VCA           OREQ      *BLANKS                                      03
104300931105     C                   ADD       IMPOR         DIRASS
104400991119     C*... EURO
104500991119     C                   ELSE
104600991119     C     VCA           IFEQ      'EUR'
104700991119     C                   ADD       IMPOR         DIRASV
104800991119     C                   END
104900991119     C                   END                                                    03
105000931105     C                   ELSE
105100991119     C     VCA           IFEQ      'ITL'
105200991119     C     VCA           OREQ      *BLANKS                                      03
105300931105     C                   ADD       IMPOR         NSASS
105400991119     C*... EURO
105500991119     C                   ELSE
105600991119     C     VCA           IFEQ      'EUR'
105700991119     C                   ADD       IMPOR         NSASSV
105800991119     C                   END
105900991119     C                   END
106000931105     C                   END
106100931105     C                   ELSE
106200931105     C*  pagamento in contanti
106300991119     C     VCA           IFEQ      'ITL'
106400991119     C     VCA           OREQ      *BLANKS                                      03
106500931105     C                   ADD       IMPOR         INCCNT
106600991119     C*... EURO
106700991119     C                   ELSE
106800991119     C     VCA           IFEQ      'EUR'
106900991119     C                   ADD       IMPOR         INCCNV
107000931105     C                   END
107100991119     C                   END
107200991119     C                   END
107300931104     C*----------------------------------------------------*
107400931104     C                   ENDSR
107500931110      *--------------------------------------------------------------*
107600931110      *  WRTGEN: Stampa totali generali tassazione                   *
107700931110      *--------------------------------------------------------------*
107800931110     C     WRTGEN        BEGSR
107900931110      *--------------------------------------------------------------*
108000941213     C                   MOVE      ' '           KTSR
108100011114     C     KFTT          CHAIN     fiftt02L                           80
108200931110     C     *IN80         IFEQ      '0'
108300931110     C* Totali generali tassazione
108400030108     C*  x i Ritiri Annullati li espone a parte e li toglie
108500030108     C*   dalla base in cui sono accorpati.
108600140429      * il campo PDU è stato liberato dal valore che era stato precedentemente
108700140429      * inserito x i RITIRI ANNULLATI (poichè equivaleva a VRANx1000)
108800140429      ** a questo punto
108900140429     c******             move      fttPDU        genRAN
109000140429     c                   move      fttVRAN       genRAN
109100030108     c                   z-add     fttTBS        genTBS
109200030108     c                   setoff                                       77
109300140429     c******             if        fttpdu > 0
109400140429     c                   if        fttVRAN >0
109500030108     c                   seton                                        77
109600030108     c                   end
109700931110     C                   Z-ADD     FTTITT        GENTAR
109800030108     c   77              sub       genran        gentar
109900931110     C                   Z-ADD     FTTITA        GENACC
110000011114      * >>>>>>>>> divisa
110100931110     C                   Z-ADD     FTTTPE        GENPCK
110200931110     C                   Z-ADD     FTTTBN        GENBON
110300931110     C                   Z-ADD     FTTTIM        GENIAM
110400931111     C                   Z-ADD     0             GENMIN
110500931111     C                   ADD       FTTITT        GENTO1
110600931111     C                   ADD       FTTITA        GENTO1
110700011114      * >>>>>>>> divisa
110800931111     C                   ADD       FTTTPE        GENTO1
110900931111     C     FTTMNT        ADD       GENTO1        GENTO2
111000931207     C                   ADD       FTTTIM        GENTO2
111100931203     C                   ADD       FTTTBN        GENTO2
111200020318      *
111300020318      * a rettifica dell'importo addizionale, occorre riportare anche altre righe
111400030123      *  di rettifica impostate sul FIFRE02L e impostate come riporta il FTTTIM.
111500020318     C                   movel     'N'           rett              1
111600030123     C     KFre          Chain     fifre02L
111700030123     c                   if        %Found(fifre02l)
111800020318     C                   movel     'S'           rett
111900020318     c                   end
112000020318      *
112100940401     C     FTTTBN        COMP      0                                  0808
112200940401     C     FTTMNT        COMP      0                                  0707
112300940401     C     FTTTIM        COMP      0                                  0909
112400931203     C   07FTTMNT        ADD       GENTO1        GENMIN
112500940321     C     LIN           IFGE      61
112600931111     C                   WRITE     TES00
112700940321     C                   END
112800090713      *
112900090713      * stampa PICKING FORFETTARIO solo il campo è valorizzato
113000090713     C     GENpck        COMP      0                                  76
113100090713      *
113200931111     C                   WRITE     TOTGEN
113300020506     c                   exsr      azzgen
113400020318      *
113500020318      * solo se ci sono delle rettifiche
113600020318     C                   if        rett = 'S'
113700060720     c                   clear                   savcre
113800020318     c                   seton                                        51
113900020507     c                   z-add     0             okret             1 0
114000030123     C     KFre          Setll     fifre02L
114100030123     C     KFre          reade     fifre02l
114200030123     c                   dow       not %eof(fifre02l)
114300020613     C     LIN           IFGE      65
114400020506     c                   WRITE     TES00
114500020318     c                   seton                                        51
114600020318     C                   END
114700020409     c                   if        savcre <> frecre
114800020409     c                   move      FRECRE        savCRE
114900020409     c                   move      FRECRE        detCRE
115000020318     c                   movel     'CRE'         tbecod
115100020318     C                   movel(P)  frecre        tbeke1
115200020318     c                   clear                   detDES
115300020318     C     Ktbe          chain     tntbe01l
115400020318     C                   if        %Found(tntbe01l) and tbeatb = *blank
115500020318     c                   movel     tbeuni        detDES
115600020318     c                   end
115700020409     c                   else
115800020409     c                   clear                   detCRE
115900020409     c                   clear                   detDES
116000020409     c                   end
116100020318     c                   move      FRENOT        detNOT
116200020318     c                   z-add     FRETIM        detTIM
116300020318     C                   WRITE     DETRET
116400020507     c                   z-add     1             okret
116500020318     c                   setoff                                       51
116600030123     C     KFre          reade     fifre02l
116700020318     c                   enddo
116800020507     c                   if        okret = 1
116900020318     C                   WRITE     ENDRET
117000020318     C                   end
117100020507     C                   end
117200020613     C     LIN           IFGE      59
117300020515     C                   WRITE     TES00
117400020515     C                   END
117500020515     C                   WRITE     TOTGE1
117600020515     c                   exsr      azzge1
117700020515     C                   END
117800020318      *
117900951025     C*  Se ci sono errori stampo segnalazione
118000951025     C     DD0ERR        IFEQ      'S'
118100951025     C     LIN           IFGE      59
118200951025     C                   WRITE     TES00
118300951025     C                   END
118400951025     C                   WRITE     ERRORE
118500951025     C                   END
118600931110     C*
118700931110     C                   ENDSR
118800020506      *--------------------------------------------------------------*
118900020506      *  AZZgen: Azzero totali generali                              *
119000020506      *--------------------------------------------------------------*
119100020506     C     AZZgen        BEGSR
119200020506     C*
119300020506     C                   Z-ADD     0             gentar
119400020506     C                   Z-ADD     0             genacc
119500020506     C                   Z-ADD     0             genpck
119600020506     C                   Z-ADD     0             gento1
119700020506     C                   Z-ADD     0             genmin
119800020506     C                   Z-ADD     0             genbon
119900020506     C*
120000020506     C                   ENDSR
120100020515      *--------------------------------------------------------------*
120200020515      *  AZZgen: Azzero totali generali                              *
120300020515      *--------------------------------------------------------------*
120400020515     C     AZZge1        BEGSR
120500020515     C*
120600020515     C                   Z-ADD     0             geniam
120700020515     C                   Z-ADD     0             gento2
120800020613     C                   Z-ADD     0             PAGE
120900020515     c                   seton                                        05
121000020515     C*
121100020515     C                   ENDSR
121200931104      *--------------------------------------------------------------*
121300020515      *  salto pagina per consegne                                   *
121400931104      *--------------------------------------------------------------*
121500020515     C     salpag        BEGSR
121600020515     C     LIN           IFGE      65
121700020515     C                   WRITE     TES00
121800020515     C                   WRITE     TES01
121900020515     C                   WRITE     TES03
122000020515     C                   SETOFF                                       05
122100020515     C                   END
122200020515     C                   ENDSR
122300020515      *--------------------------------------------------------------*
122400020529      *  Operazioni iniziali                                 *
122500020515      *--------------------------------------------------------------*
122600020529     C     defcam        BEGSR
122700931104      *--------------------------------------------------------------*
122800020315     C                   MOVEL     KPJBU         ficnB6
122900020529     c* chiudo file
123000020529     c                   if        dd0pdr = 0
123100020529     c                   seton                                        lr
123200020529     c                   return
123300020529     c                   end
123400020529     c*
123500941201     C     DD0SIM        COMP      'S'                                    02
123600020529     C* Carico dati del padroncino
123700030306     C                   MOVEL     *BLANKS       APDRSF
123800021202     C     kapd          CHAIN     fiapd01L                           50
123900030131     C                   MOVEL     APDRSF        DESPAD
124000020529     C                   MOVE      DD0PDR        KPDR
124100020529     C                   MOVE      DD0DDT        KDDC
124200020529     C                   MOVE      DD0PDR        CODCPC
124300020529     C                   CLEAR                   WLBDA8
124400020529     C                   MOVE      KDDC          G02INV
124500020529     C                   MOVEL     '3'           G02ERR
124600020529     C                   CALL      'XSRDA8'
124700020529     C                   PARM                    WLBDA8
124800020529     C                   MOVE      G02DAT        STPDDC
124900020529     c                   z-add     0             ok                1 0
125000020529     C                   TIME                    TIMES             6 0
125100020529     C                   SETON                                        05
125200020529     C                   SETON                                        OA
125300020529     c                   z-add     61            lin
125400020529     C                   MOVE      ' '           KTSR
125500020529     C     KFTT          CHAIN     fiftt02L                           80
125600020529     c                   if        *in80
125700020529     c                   return
125800020529     c                   else
125900020529     c                   exsr      srfor
126000020529     c                   end
126100020529     C                   ENDSR
126200020529      *--------------------------------------------------------------*
126300020529      *  *INZSR: Operazioni iniziali                                 *
126400020529      *--------------------------------------------------------------*
126500020529     C     *INZSR        BEGSR
126600110511      *
126700020529     C     *ENTRY        PLIST
126800020529     C                   PARM                    KPJBA
126900931109     C* Richiamo XPARUT
127000931109     C                   Z-ADD     1             CODUT
127100931109     C                   CALL      'X§PARUT'
127200931109     C                   PARM                    UT§DSE
127300110511     C****               MOVEL     RAGUT         RSUT             20
127400931109     C                   MOVEL     REC80         CNCR80
127500110511     C                   MOVEL     REC42         CNCR42
127600110511     C                   MOVEL     simRGS        RSUT             20
127700110511      *
127800020506     c                   if        simfel = 0
127900020530     c                   seton                                        20
128000020506     c                   open      titas30c
128100101112     c**********         open      ansif01l
128200101112
128300020506     c                   else
128400020530     c                   setoff                                       20
128500020506     c                   open      fnarb01l
128600020506     c                   end
128700020506     C*                  Z-ADD     SIMFEL        FILIA             3 0
128800931104     C*  Definizione delle chiavi
128900020515     C     Ktlz          KLIST
129000020515     C                   KFLD                    tlztip
129100020515     C                   KFLD                    tlzpdr
129200020515     C                   KFLD                    socp
129300020515     C                   movel(p)  'P'           tlztip
129400020515     C*
129500020515     C     KFTT          KLIST
129600020515     C                   KFLD                    KPDR
129700020515     C                   KFLD                    KFVL
129800020515     C                   KFLD                    KDDC
129900020529     C                   KFLD                    KTSR
130000020318     C*
130100941201     C     KFTD          KLIST
130200941201     C                   KFLD                    KPDR
130300941201     C                   KFLD                    KTSR
130400950113     C                   KFLD                    KNDC
130500950113     C                   KFLD                    KDDCT
130600020318     C*
130700020318     C     KFRE          KLIST
130800030123     C                   KFLD                    KTip              1
130900020318     C                   KFLD                    KPDR
131000020318     C                   KFLD                    KDDC
131100030123     C                   move      'A'           KTip              1
131200941201     C*
131300941201     C     KARB          KLIST
131400941201     C                   KFLD                    KAAS
131500941201     C                   KFLD                    KLNP
131600941201     C                   KFLD                    KNRS
131700941201     C                   KFLD                    KNSP
131800931105     C*
131900941201     C     KTAB          KLIST
132000941201     C                   KFLD                    KKUT
132100941201     C                   KFLD                    KCOD
132200020318     C*
132300020318     C     KTbe          KLIST
132400020318     C                   KFLD                    tbecod
132500020318     C                   KFLD                    tbeke1
132600021202     C*
132700021202     C     Kapd          KLIST
132800021202     C                   KFLD                    apdtip
132900021202     C                   KFLD                    dd0pdr
133000021202     c                   movel     'A'           apdtip
133100020318     c*
133200931105     C* Definizione delle variabili
133300020506     C*    *LIKE         DEFINE    ARBAAS        KAAS
133400020506     C*    *LIKE         DEFINE    ARBLNP        KLNP
133500020506     C*    *LIKE         DEFINE    ARBNRS        KNRS
133600020506     C*    *LIKE         DEFINE    ARBNSP        KNSP
133700020506     C     *LIKE         DEFINE    tasAAS        KAAS
133800020506     C     *LIKE         DEFINE    tasLNP        KLNP
133900020506     C     *LIKE         DEFINE    tasNRS        KNRS
134000020506     C     *LIKE         DEFINE    tasNSP        KNSP
134100941201     C     *LIKE         DEFINE    FTTPDR        KPDR
134200941201     C     *LIKE         DEFINE    FTTFVL        KFVL
134300941201     C     *LIKE         DEFINE    FTTTSR        KTSR
134400941201     C     *LIKE         DEFINE    FTTDDC        KDDC
134500950113     C     *LIKE         DEFINE    FTTDDC        KDDCT
134600941201     C     *LIKE         DEFINE    FTTNDC        KNDC
134700941201     C     *LIKE         DEFINE    TBLKUT        KKUT
134800941201     C     *LIKE         DEFINE    TBLCOD        KCOD
134900941201     C     *LIKE         DEFINE    FTTSET        SPETOT
135000931108     C     *LIKE         DEFINE    FTTSNT        SPENOC
135100020315     C     *like         define    ftdnss        savnss
135200020315     C     *like         define    ftdstp        savstp
135300020315     C     *like         define    FTDRSC        savrsc
135400020315     C     *like         define    FTDSET        addSET
135500020315     C     *like         define    FTDNCL        addNCL
135600020315     C     *like         define    FTDPKL        addpkl
135700071121     C     *like         define    FTDVLU        addvlu           +2
135800020315     C     *like         define    FTDCTP        savCTP
135900020315     C     *like         define    FTDITT        additt
136000020315     C     *like         define    FTDITA        addita
136100020315     C     *like         define    FTDFIA        savFIA
136200020315     C     *like         define    FTDFIT        savFIT
136300020409     c     *like         define    FRECRE        savCRE
136400000623     C* Inizializza  schiera delle Filiali POSTE
136500000623     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
136600000623     C                   CLEAR                   FIP
136700000623     C     *LOVAL        SETLL     AZORG01L
136800000623     C                   READ      AZORG01L                               89
136900000623     C     *IN89         DOWEQ     *OFF
137000000623     C                   MOVEL     ORGDE3        OG143
137100020717     C**** §OGPT         IFEQ      'S'
137200020717     C     §OgNTW        IFEQ      'PPT'
137300000623     C                   Z-ADD     1             X
137400000623     C     *ZEROS        LOOKUP    FIP(X)                                 89
137500000623     C   89              Z-ADD     ORGFIL        FIP(X)
137600000623     C                   END
137700000623     C                   READ      AZORG01L                               89
137800000623     C                   END
137900000623     C* Inizializzazione  variabili
138000950623     C                   MOVE      SIMFEL        CODFIL            3 0
138100020529     C                   MOVE      'C'           KFVL
138200020529     C                   MOVE      1             KKUT
138300020529     C                   MOVEL     NOMPG         NOMPGM
138400020529     C                   MOVEL     *ALL'-'       CMP
138500020529     C                   MOVEL     *ALL'-'       CM1
138600020529     C                   MOVEL     *ALL'*'       CM3
138700931105     C* Caricamento dati tabelle nelle schiere
138800931105     C                   OPEN      TABEL00F
138900931105     C* Carico in schiera la Tab. 2A: descrizione tipo consegna
139000941201     C                   MOVEL     '2A'          KCOD
139100931104     C                   Z-ADD     0             Q                 3 0
139200941201     C     KTAB          CHAIN     TABEL00F                           89
139300931104     C     *IN89         DOWEQ     '0'
139400931105     C     TBLFLG        IFEQ      *BLANKS
139500931105     C     Q             IFLT      150
139600931104     C                   ADD       1             Q
139700931108     C                   MOVEL     TBLKEY        C2A(Q)
139800931108     C                   MOVEL     TBLUNI        D2A(Q)
139900931105     C                   ELSE
140000931105     C                   SETON                                        9089
140100931105     C   OA              EXCEPT    TESERR
140200931105     C                   EXCEPT    DETERR
140300931104     C                   END
140400931105     C                   END
140500941201     C     KTAB          READE     TABEL00F                               89
140600931104     C                   END
140700931105     C*
140800931105     C* Carico in schiera la Tab. 1A: descrizione titolo
140900941201     C                   MOVEL     '1A'          KCOD
141000931105     C                   Z-ADD     0             Q                 3 0
141100941213     C     KTAB          CHAIN     TABEL00F                           89
141200931105     C     *IN89         DOWEQ     '0'
141300931105     C     TBLFLG        IFEQ      *BLANKS
141400931105     C     Q             IFLT      20
141500931105     C                   ADD       1             Q
141600931105     C                   MOVEL     TBLUNI        DST(Q)
141700931105     C                   MOVEL     TBLKEY        CDT(Q)
141800931105     C                   ELSE
141900931105     C                   SETON                                        9289
142000931105     C   OA              EXCEPT    TESERR
142100931105     C                   EXCEPT    DETERR
142200931105     C                   END
142300931105     C                   END
142400941201     C     KTAB          READE     TABEL00F                               89
142500931105     C                   END
142600931105     C*
142700941201     C* Carico dati valute
142800941201     C                   CLEAR                   DSCV
142900941201     C                   Z-ADD     0             Q                 3 0
143000941201     C                   MOVEL     'CV'          KCOD
143100941201     C     KTAB          CHAIN     TABEL00F                           89
143200941201     C     *IN89         DOWEQ     '0'
143300941201     C     TBLFLG        IFEQ      *BLANKS
143400941201     C     Q             IFLT      100
143500941201     C                   ADD       1             Q
143600941201     C                   MOVEL     TBLUNI        DSCV
143700941201     C                   MOVEL     TBLKEY        CCV(Q)
143800941201     C                   MOVEL     §CVITA        ITA(Q)
143900941201     C                   ELSE
144000941201     C                   SETON                                        9389
144100941201     C   OA              EXCEPT    TESERR
144200941201     C                   EXCEPT    DETERR
144300941201     C                   END
144400931105     C                   END
144500941201     C     KTAB          READE     TABEL00F                               89
144600941201     C                   END
144700931105     C* Fine caricamento:
144800931105     C                   CLOSE     TABEL00F
144900090916      *
145000090916      *----
145100090916     C                   Z-ADD     0             CP                5 0
145200090916     C                   movel(P)  'CTP'         tbeCOD
145300090916     c                   clear                   KCliPar
145400090916     c                   clear                   KCliParTSR
145500090916     c                   clear                   KTarStdNew
145600090916     c                   clear                   KTarStd
145700090916     c                   clear                   KTarNew
145800090916     c                   clear                   KTarObb
145900090916     c                   clear                   KTarUgu
146000090916      *----
146100090916     C     tbeCOD        setll     tntbe01l
146200090916     C     tbeCOD        reade     tntbe01l
146300090916      *
146400090916     c                   dow       not %eof(tntbe01l)
146500090916     C                   add       1             CP
146600090916     C                   MOVEL     TBeKE1        KCliPar(CP)
146700090916     C                   MOVEL     TBeKE1        KCliParTSR(CP)
146800090916     C                   MOVEL     TBeKE2        primoByte         1
146900090916      * il tipo servizio alla destra del codice cliente
147000090916     C                   MOVE      primoByte     KCliParTSR(CP)
147100090916     C                   MOVEL     TBeKE2        KTarStd(CP)
147200090916     C                   movel     tbeuni        dCTP
147300090916     C                   MOVEL     TBeKE2        KTarStdNew(CP)
147400090916     C                   MOVE      §CTPCTD       KTarStdNew(CP)
147500090916     C                   MOVEL     §CTPCTD       KTarnew(CP)
147600090916     C                   MOVEL     §CTPOBB       KTarobb(CP)
147700090916     C                   MOVEL     §CTPUGU       KTarugu(CP)
147800090916     C     tbeCOD        reade     tntbe01l
147900090916     c                   enddo
148000090916      *----
148100090916      *
148200931108     C                   ENDSR
148300101112     C**************************************************************************
148400101112     C* decodifica la società
148500101112     C************************************************************
148600101112     C     Decod_societa begSR
148700110613     C*
148800110613     ** Inizializzo il programma.
148900110613     C                   CALL      'TIBSSOCR'
149000110613     C                   PARM      'INIT'        prmRqsOpCode
149100110613     C                   PARM                    prmRpyOpCode
149200110613     C                   PARM                    prmRpyIdMsg
149300101112      *
149400110613      *
149500101112     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
149600101112     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
149700101112     C                   RESET                   tibsSocI0
149800101112     C                   EVAL      tibsSocI0.idSocieta = fttsoc
149900110509      * x data
150000110509     c                   move      Fttddc        dataiso
150100110509     C                   EVAL      tibsSocI0.dtvalidita = dataiso
150200101112     C
150300101112     C                   CALL      'TIBSSOCR'
150400101112     C                   PARM      'GETANAGRAF'  prmRqsOpCode
150500101112     C                   PARM      *BLANK        prmRpyOpCode
150600101112     C                   PARM      *ZERO         prmRpyIdMsg
150700101112     C                   PARM      'TIBSSOCI0'   prmRqsFormato
150800101112     C                   PARM                    tibsSocI0
150900101112     C                   PARM                    prmRqsDataSize
151000101112     C                   PARM      'TIBSSOCO0'   prmRpyFormato
151100101112     C                   PARM                    tibsSocO0
151200101112     C                   PARM                    prmRpyDataSize
151300101112     c                   if         PRMRPYIDMSG >= 0
151400101112     c                              and TIBSSOCO0.IDSOCIETA <> *blank
151500101112     c                   eval       dessoc   = tibsSocO0.RAGSOCIALE
151600101112     c                   end
151700101112      *
151800101112     C                   ENDSR
151900931105      *--------------------------------------------------------------*
152000931105     O*  STAMPA ERRORI !!!!!!!!
152100931105      *--------------------------------------------------------------*
152200931105     OSYSPRT    E            TESERR           01
152300931105     O                       STT(1)              66
152400931105     O                       STT(2)             132
152500931105     O                       RSUT                20
152600931105     O                       CODFIL              32
152700931105     O                       UDATE              118 '  /  /  '
152800931105     O                       PAGE          Z    132
152900931105     O          E            TESERR           03
153000931105     O                       TIMES              118 '  .  .  '
153100931105     O          E            TESERR           06
153200931105     O                                           24 'SI RACCOMANDA DI CONSEGN'
153300931105     O                                           48 'ARE QUESTA STAMPA ALL''U'
153400931105     O                                           72 'FFICIO  C.E.D.!!!!!!    '
153500931105     O          E            TESERR      0  2
153600931105     O                                           24 'SI RACCOMANDA DI CONSEGN'
153700931105     O                                           48 'ARE QUESTA STAMPA ALL''U'
153800931105     O                                           72 'FFICIO  C.E.D.!!!!!!    '
153900931105     O          E            DETERR         1
154000931105     O                                           24 'NON CARICATI TUTTI GLI E'
154100931105     O                                           48 'LEMENTI DELLA TABELLA '
154200931105     O               90                             '2A'
154300931105     O               92                             '1A'
154400941201     O               93                             'CV'
154500931105      *--------------------------------------------------------------*
154600931108** STT
154700931029                                    ***  C H I U S U R A     D I S
154800931108 T I N T A    C O N S E G N A  ***  FLFRD0R              PAG.
