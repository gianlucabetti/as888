000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300941025      * TNSC95R                                                      *
000400910918      *            *-------------------------*                       *
000500910918      *             MANUTENZIONE CONTRASSEGNI                        *
000600910918      *            *-------------------------*                       *
000700911104      *                                                              *
000800910918      * 36- ERRORI NEI CAMPI DELLA 1a VIDEATA                        *
000900910918      * 51 -INDICATORE DI COMODO                                     *
001000971219      * 52 -SI PUO VARIARE SOLO RAGGR.TO CTB IL RESTO DA GEST. BOLLE * OBS
001100971219      * 52 -PROTEGGO ALCUNI CAMPI                                    *
001200940124      * 53 -CODICE ABI-CAB ERRATO                                    *
001300940124      * 54 -IMMETTERE NUMERO ASSEGNO                                 *
001400940124      * 55 -NUMERO ASSEGNO ERRATO                                    *
001500950711      * 40-45 ERRORI NEI CAMPI VARIATI                               *
001600910930      * 70 -INDICATORE DI COMODO                                     *
001700941025      * 76 -CONT ASSEGNO NON ANCORA PAGATO                           *
001800971219      * 77 -SPROTEGGE  IMPORTO-RAGGRUPPAMENTO                        *
001900910918      * 78 -SPROTEGGE STATO E CODICE INTESTAZIONE                    *
002000950711      * 28 -ERRORE GENERICO                                          *
002100910918      * KC -FINE LAVORO                                              *
002200910918      * KL -FINE VARIAZIONE                                          *
002300900606?     *--------------------------------------------------------------*
002400950711     FTNSC43D   CF   E             WORKSTN
002500950711     FTNSC95D   CF   E             WORKSTN
002600120224     FTNCSB03L  UF   E           K DISK
002700120224     FTNCSB09L  IF   E           K DISK    PREFIX(£)
002800120224     f                                     rename(tncsb000:tncsb009)
002900941025     FTNCSV00F  O    E             DISK
003000971008     FTNCSA01L  UF   E           K DISK
003100910909     FCNABI00F  IF   E           K DISK
003200941025     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003300910906     FTABEL00F  IF   E           K DISK
003400011019     FTnTbe01L  IF   E           K DISK
003500011112     FTiTas30C  IF   E           K DISK
003600100219     Ffiar531c  IF   E           K DISK
003700941025?     *--------------------------------------------------------------*
003800120224     D ERR             S             70    DIM(28) CTDATA PERRCD(1)
003900900515      *
004000120210     D tnsc30ds      E DS
004100120209     D CNCR80        E DS
004200900514     D KPJBA         E DS
004300910828     D  LIBSYS                92    100
004400910828     D  MSGERR               453    502
004500941025     D CSBDS         E DS                  EXTNAME(TNCSB00F)
004600941025     D               E DS                  EXTNAME(TNCSV00F)
004700941028     D  CSVDS                 16    229
004800900516      *
004900900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005000000000     D  TCU                  398    697
005100000000     D                                     DIM(50)
005200000000     D  KCU                  698    847P 0
005300000000     D                                     DIM(50)
005400000000     D                                     PACKEVEN
005500900606      *
005600900606     D TCUDS           DS
005700900606     D  F3                     3      3
005800900606     D  F4                     4      4
005900900606     D  F56                    5      6
006000900515      *
006100941025     D WLBDA8          DS
006200941025     D  G02DAT                 1      8  0
006300941025     D  G02INV                 9     16  0
006400941025     D  G02ERR                17     17
006500941025     D  G02TGI                18     22  0
006600971218     D*
006700971218     D TIBS69        E DS                  EXTNAME(TIBS69DS)
006800971218     D CNACO         E DS                  EXTNAME(CNACO00F)
006900971218     D CNIND         E DS                  EXTNAME(CNIND00F)
007000971218     D CNCLP         E DS                  EXTNAME(CNCLP00F)
007100971218     D FNCLS         E DS                  EXTNAME(FNCLS00F)
007200100219     D dar5gen       E DS
007300910521      *
007400910918     D PAS01           DS
007500910918     D  FLAG                   1      1
007600960923     D DS4S          E DS
007700960923     D DSCV          E DS
007800011019     D DGEI          E DS
007900011019     D                                     INZ
008000011019     D DGED          E DS
008100011019     D                                     INZ
008200011019     D YEurCoDS      E DS
008300011019     D                                     INZ
008400011112     D Trul21DS      E DS
008500011112     D                                     INZ
008600950711     D                SDS
008700950711     D  NOMPGM                 1     10
008800941025     I*--------------------------------------------------------------*
008900941025     IAZORG01L  AA
009000941025     I                             P    4    5 0ORGFIL
009100941025     I                                 14   33  ORGDES
009200910830      *---------------------------------------------------------------*
009300910917     C*
009400910916     C                   EXSR      PULIZ
009500120224     c*
009600120224     C                   DO        *hival
009700971219     C                   SETON                                        777952
009800911018     C     FLAG          COMP      *BLANKS                                78
009900950711     C                   EXFMT     SC95D01
010000120224     c   kc              leave
010100940413     C                   MOVEL     *BLANKS       $MSG
010200941026     C*  CONTROLLO SE L'ANNO E'NEL FORMATO 2 CHR E SE SI LO CONVERTO
010300950711     C                   MOVEL     V1CAAS        WAA2              2 0
010400941026     C     WAA2          IFEQ      0
010500950711     C     V1CAAS        IFGT      60
010600950711     C                   ADD       1900          V1CAAS
010700941026     C                   ELSE
010800950711     C                   ADD       2000          V1CAAS
010900941026     C                   END
011000941026     C                   END
011100941026     C*
011200950727     C     KSPE          CHAIN     TNCSB000                           5060
011300120224     c                   if        *in50
011400120224     C                   MOVEL     ERR(1)        $MSG
011500120224     c                   iter
011600120224     c                   else
011700120224     C     *IN60         IFEQ      '1'
011800950727     C                   MOVEL     ERR(21)       $MSG
011900950727     C                   SETON                                        50
012000120224     c                   iter
012100971218     C                   END
012200120224     C                   END
012300120224     c*
012400120224     c                   EXSR      VISUAL
012500120207     c*
012600120224     C                   DO        *hival
012700950711     C                   EXFMT     SC43D01
012800120224     c*
012900120224     c                   if        *inkl
013000120224     C                   SETOFF                                       28
013100120224     c                   leave
013200120224     c                   end
013300120224     c* interroga multiassegni
013400120209     c   ki              exsr      srmulti
013500120224     c* multiassegni chiedo la forzatura
013600120224     c                   if        *inkf and *in09
013700120224     C                   SETON                                        28
013800120224     C                   MOVEL     err(28)       $MSG
013900120224     c                   iter
014000120224     c                   end
014100120224     c* controlo
014200120224     C                   EXSR      ctr02
014300120224     c   28              iter
014400120224     c* aggiorno
014500120224     c                   if        *inkf or *inkj
014600120224     C* MULTIASSEGNI
014700120224     c                   if        *INKJ
014800120224     C                   EXSR      AGGIO1
014900120224     c                   else
015000120224     C                   EXSR      AGGIO
015100120224     c                   end
015200120224     c                   leave
015300120224     c                   end
015400120224     c*
015500120224     C                   ENDdo
015600120224     c*
015700120224     C                   ENDdo
015800971218     C*
015900971218     C* RICHIAMO PGM X CHIUSURA FILES
016000971218     C                   CLEAR                   TIBS69
016100971218     C                   MOVEL     'C'           I69TLA
016200971218     C                   CALL      'TIBS69R'
016300971218     C                   PARM                    TIBS69
016400971218     C                   PARM                    CNACO
016500971218     C                   PARM                    CNIND
016600971218     C                   PARM                    CNCLP
016700971218     C                   PARM                    FNCLS
016800910521     C                   SETON                                        LR
016900910521     C*
017000900515      *-----------------------------------------------------***********
017100120209      * richiamo pgm multiassegni
017200900515      *-----------------------------------------------------***********
017300120209     C     srmulti       BEGSR
017400120209     c* interroga multiassegni
017500120210     c                   clear                   tnsc30ds
017600120210     c                   eval      sc30CMTI = 'N'
017700120210     c                   eval      sc30AMBI = 'S'
017800120209     c* solo se non pagato
017900120209     c                   if        v1cddp <> 0
018000120210     c                   eval      sc30TPCI = 'V'
018100120209     c                   else
018200120210     c                   eval      sc30TPCI = 'M'
018300120209     c                   end
018400120210     c                   eval      sc30AASI = v1caas
018500120210     c                   eval      sc30LNPI = v1clnp
018600120210     c                   eval      sc30NRSI = v1cnrs
018700120210     c                   eval      sc30NSPI = v1cnsp
018800120210     c                   movel(p)  tnsc30ds      kpjbu
018900120210     c                   call      'TNSC30R'
019000120209     c                   parm                    kpjba
019100120209     C*
019200120209     c                   endsr
019300120209      *-----------------------------------------------------***********
019400120209      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
019500120209      *-----------------------------------------------------***********
019600120209     C     PULIZ         BEGSR
019700950711     C                   Z-ADD     0             V1CLNP
019800950711     C                   Z-ADD     ANNO          V1CAAS
019900950711     C                   Z-ADD     0             V1CNRS
020000950711     C                   Z-ADD     0             V1CNSP
020100910918     C                   ENDSR
020200120209      *-----------------------------------------------------***********
020300120209      * dati a video
020400120209      *-----------------------------------------------------***********
020500910918     C*
020600910828     C     VISUAL        BEGSR
020700950711     C                   SETOFF                                       284041
020800950711     C                   SETOFF                                       424344
020900950711     C                   SETOFF                                       454647
021000980302     C                   SETOFF                                       4871
021100971219     C                   CLEAR                   V1DINT
021200971219     C                   CLEAR                   V1CVII
021300971219     C                   CLEAR                   V1CCIT
021400971219     C                   CLEAR                   V1CCAI
021500971219     C                   CLEAR                   V1CNZI
021600971219     C                   CLEAR                   V1CPRI
021700910917     C* STATO DEL C/A
021800950710     C                   Z-ADD     CSBSTA        V1CSTA
021900910917     C                   MOVEL     '4S'          CODTAB
022000941108     C                   MOVEL     *BLANK        KEYTAB
022100950710     C                   MOVE      V1CSTA        KEYTAB
022200960923     C                   MOVEL     *BLANKS       WMOD
022300910930     C     KTAB          CHAIN     TABEL                              70
022400950710     C  N70              MOVEL     TBLUNI        V1DSTA
022500960923     C  N70              MOVEL     TBLUNI        DS4S
022600960923     C  N70              MOVEL     §4SMOD        WMOD              1
022700950710     C   70              MOVEL     *BLANKS       V1DSTA
022800910917     C* LINEA DI PARTENZA + DECODIFICA
022900950710     C                   Z-ADD     CSBLNP        V1CLNP
023000950710     C     V1CLNP        CHAIN     AZORG01L                           70
023100950710     C  N70              MOVEL     ORGDES        V1DLNP
023200950710     C   70              MOVEL     *BLANKS       V1DLNP
023300910917     C* NR° SERIE + SPEDIZIONE
023400950710     C                   Z-ADD     CSBNRS        V1CNRS
023500950710     C                   Z-ADD     CSBNSP        V1CNSP
023600910917     C* CODICE CLIENTE
023700950710     C                   Z-ADD     CSBKSC        V1CKSC
023800910917     C* DATA SPEDIZIONE
023900941028     C                   CLEAR                   WLBDA8
024000941025     C                   MOVEL     CSBAAS        G02INV
024100941028     C                   MOVE      CSBMGS        G02INV
024200941025     C                   Z-ADD     0             G02DAT
024300941025     C                   MOVEL     '3'           G02ERR
024400941025     C                   CALL      'XSRDA8'
024500941025     C                   PARM                    WLBDA8
024600950710     C                   Z-ADD     G02DAT        V1CDSP
024700910917     C* DATA DISTINTA
024800941025     C                   Z-ADD     CSBDDC        G02INV
024900941025     C                   Z-ADD     0             G02DAT
025000941025     C                   MOVEL     '3'           G02ERR
025100941025     C                   CALL      'XSRDA8'
025200941025     C                   PARM                    WLBDA8
025300950710     C                   Z-ADD     G02DAT        V1CDDC
025400910917     C* LINEA ARRIVO + DECODIFICA
025500950710     C                   Z-ADD     CSBLNA        V1CLNA
025600941025     C                   Z-ADD     CSBLNA        ORGFIL
025700941025     C     ORGFIL        CHAIN     AZORG01L                           70
025800950710     C  N70              MOVEL     ORGDES        V1DLNA
025900950710     C   70              MOVEL     *BLANKS       V1DLNA
026000910917     C* NUMERO DISTINTA
026100950710     C                   Z-ADD     CSBNDT        V1CNDT
026200910917     C* RIFERIMENTO MITTENTE
026300950710     C                   Z-ADD     CSBRMN        V1CRMN
026400950710     C                   MOVEL     CSBRMA        V1CRMA
026500910917     C* DATA ARRIVO DISTINTA
026600941025     C                   Z-ADD     CSBDTA        G02INV
026700941025     C                   Z-ADD     0             G02DAT
026800941025     C                   MOVEL     '3'           G02ERR
026900941025     C                   CALL      'XSRDA8'
027000941025     C                   PARM                    WLBDA8
027100950710     C                   Z-ADD     G02DAT        V1CDTA
027200910917     C* NR° ASSEGNO = RIFERIM. INCASSO
027300950710     C                   MOVEL     CSBNRA        V1CNRA
027400910917     C* DATA ASSEGNO
027500941025     C                   Z-ADD     CSBDTE        G02INV
027600941025     C                   Z-ADD     0             G02DAT
027700941025     C                   MOVEL     '3'           G02ERR
027800941025     C                   CALL      'XSRDA8'
027900941025     C                   PARM                    WLBDA8
028000950710     C                   Z-ADD     G02DAT        V1CDTE
028100910917     C* TIPO INTESTAZIONE ASSEGNO
028200950710     C                   MOVEL     CSBTPI        V1CTPI
028300950710     C                   MOVEL     CSBTPA        V1CTPA
028400950711     C                   MOVEL     CSBFUS        V1CFUS
028500950711     C                   MOVEL     CSBMAS        V1CMAS
028600100219     C* verifico se è stato variato il tipo incasso bollettato
028700100224     c                   setoff                                       8889
028800100224     c                   clear                   v1ctici
028900100224     c                   clear                   v1cticmb
029000100224     c                   if        v1caas > 2009
029100100224     c                   seton                                        88
029200100219     c     kar5          chain     fiar531c
029300100219     c                   if        %found(fiar531c)
029400100219     c                   movel     ar5uni        dar5gen
029500100224     c                   movel     §AR5TICI      v1ctici
029600100219     c                   if        §AR5FLGMB = 'S'
029700100224     c                   seton                                        89
029800100224     c                   movel     §AR5TICMB     v1cticmb
029900100219     c                   end
030000100219     c                   end
030100100224     c                   end
030200910917     C* CODICE INTESTAZIONE ASS.
030300950710     C                   Z-ADD     CSBCDI        V1CCDI
030400910917     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
030500950710     C                   Z-ADD     CSBABI        V1CABI
030600950710     C                   Z-ADD     CSBCAB        V1CCAB
030700910917     C     KABI          CHAIN     CNABI000                           75
030800950710     C  N75              MOVEL     ABIIST        V1CDES
030900950710     C   75              MOVEL     *BLANKS       V1CDES
031000941025     C* DATA REGISTRAZIONE INCASSO
031100941025     C                   Z-ADD     CSBDRI        G02INV
031200941025     C                   Z-ADD     0             G02DAT
031300941026     C                   MOVEL     '3'           G02ERR
031400941025     C                   CALL      'XSRDA8'
031500941025     C                   PARM                    WLBDA8
031600950710     C                   Z-ADD     G02DAT        V1CDRI
031700941025     C* NUMERO REG.INCASSO/CAMBIO
031800950710     C                   Z-ADD     CSBNRI        V1CNRI
031900011019     C**                 Z-ADD     CSBCMB        V1CCMB
032000910917     C* INTESTAZIONE ASSEGNO / MITTENTE
032100971218     C                   CLEAR                   TNCSA000
032200971218     C* SE COD. INTESTAZ. ASSEGNO <> COD. CLIENTE DECODIFICO  CDI
032300971218  B1 C     V1CCDI        IFNE      V1CKSC
032400971218      *  SE CDI è = 8888 / 9999  PRENDO I DATI DA TNCSA
032500971219      *  (CASO POCO PROBABILE IN QUANTO IL CODICE INTESTAZ. NON DOVREBBE
032600971219      *   ESSERE=8888 O 9999 MENTRE IL KSC è UN CODIFICATO)
032700971218     C                   MOVE      V1CCDI        §8899             4 0
032800971218  B2 C     §8899         IFEQ      8888
032900971218     C     §8899         OREQ      9999
033000971218     C     KBLCAS        CHAIN     TNCSA000                           70
033100971218  B3 C     *IN70         IFEQ      *OFF
033200971219     C                   SETON                                        71
033300971218     C                   MOVEL     CSARSD        V1DINT
033400971218     C                   MOVEL     CSAVID        V1CVII
033500971218     C                   MOVEL     CSACAD        V1CCAI
033600971218     C                   MOVEL     CSACID        V1CCIT
033700971218     C                   MOVEL     CSANZM        V1CNZI
033800971218     C                   MOVEL     CSAPRM        V1CPRI
033900971218  E3 C                   ENDIF
034000971218  X2 C                   ELSE
034100971218      * ALTRIMENTI è UN CODIFICATO
034200971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE INTESTAZ. ASSEGNO
034300971218     C                   CLEAR                   TIBS69
034400971218     C                   CLEAR                   CNACO
034500971218     C                   CLEAR                   CNIND
034600971218     C                   CLEAR                   CNCLP
034700971218     C                   CLEAR                   FNCLS
034800971218     C                   MOVEL     ' '           I69TLA
034900971218     C                   MOVEL     KNSIF         I69SIF
035000971218     C                   Z-ADD     V1CCDI        I69KAC
035100971219     C                   Z-ADD     V1CCDI        I69KIN
035200971218     C                   CALL      'TIBS69R'
035300971218     C                   PARM                    TIBS69
035400971218     C                   PARM                    CNACO
035500971218     C                   PARM                    CNIND
035600971218     C                   PARM                    CNCLP
035700971218     C                   PARM                    FNCLS
035800971218  B3 C     O69ERR        IFEQ      *BLANK
035900971218     C                   MOVEL     ACORAG        V1DINT
036000971218     C                   MOVEL     INDVIA        V1CVII
036100971218     C                   MOVEL     *BLANKS       V1CCAI
036200971218  B4 C     INDCAP        IFNE      *ZEROS
036300971218     C                   MOVEL     INDCAP        V1CCAI
036400971218  E4 C                   END
036500971218     C                   MOVEL     INDSTA        V1CNZI
036600971218     C                   MOVEL     INDPRV        V1CPRI
036700971218  B4 C     INDCAE        IFNE      *ZEROS
036800971218     C     INDCAE        ANDNE     *BLANKS
036900971218     C                   MOVEL     INDCAE        V1CCAI
037000971218  E4 C                   END
037100971218     C                   MOVEL     INDCIT        V1CCIT
037200971218  E3 C                   ENDIF
037300971218  E2 C                   ENDIF
037400971218  E1 C                   ENDIF
037500971218     C* IN OGNI CASO DECODIFICO KSC
037600971218      *  SE KSC è = 8888 / 9999  PRENDO I DATI DA TNCSA
037700971218     C                   MOVE      V1CKSC        §8899
037800971218  B1 C     §8899         IFEQ      8888
037900971218     C     §8899         OREQ      9999
038000971126     C     KBLCAS        CHAIN     TNCSA000                           70
038100971218  B2 C     *IN70         IFEQ      *OFF
038200971219     C                   SETON                                        71
038300950710     C                   MOVEL     CSARSD        V1DMIO
038400950710     C                   MOVEL     CSAVID        V1CVIM
038500950710     C                   MOVEL     CSACAD        V1CCAM
038600950710     C                   MOVEL     CSACID        V1CLOM
038700950710     C                   MOVEL     CSANZM        V1CNZM
038800950710     C                   MOVEL     CSAPRM        V1CPRM
038900971218  E2 C                   ENDIF
039000971218  X1 C                   ELSE
039100971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE CODIFICATO
039200971218     C                   CLEAR                   TIBS69
039300971218     C                   CLEAR                   CNACO
039400971218     C                   CLEAR                   CNIND
039500971218     C                   CLEAR                   CNCLP
039600971218     C                   CLEAR                   FNCLS
039700971218     C                   MOVEL     ' '           I69TLA
039800971218     C                   MOVEL     KNSIF         I69SIF
039900971218     C                   Z-ADD     V1CKSC        I69KAC
040000971219     C                   Z-ADD     V1CKSC        I69KIN
040100971218     C                   CALL      'TIBS69R'
040200971218     C                   PARM                    TIBS69
040300971218     C                   PARM                    CNACO
040400971218     C                   PARM                    CNIND
040500971218     C                   PARM                    CNCLP
040600971218     C                   PARM                    FNCLS
040700971218  B2 C     O69ERR        IFEQ      *BLANK
040800950710     C                   MOVEL     ACORAG        V1DMIO
040900950710     C                   MOVEL     INDVIA        V1CVIM
041000950710     C                   MOVEL     *BLANKS       V1CCAM
041100971218  B3 C     INDCAP        IFNE      *ZEROS
041200950710     C                   MOVEL     INDCAP        V1CCAM
041300971218  E3 C                   END
041400950710     C                   MOVEL     INDSTA        V1CNZM
041500950710     C                   MOVEL     INDPRV        V1CPRM
041600971218  B3 C     INDCAE        IFNE      *ZEROS
041700941025     C     INDCAE        ANDNE     *BLANKS
041800950710     C                   MOVEL     INDCAE        V1CCAM
041900971218  E3 C                   END
042000950710     C                   MOVEL     INDCIT        V1CLOM
042100971218  E2 C                   END
042200971218  E1 C                   ENDIF
042300910917     C* DESTINATARIO
042400950710     C                   MOVEL     CSBRSD        V1CRSD
042500950710     C                   MOVEL     CSBPAR        V1CPAR
042600950710     C                   MOVEL     CSBPRO        V1CPRO
042700910917     C* NR° DOCUMENTO PAGAMENTO
042800950710     C                   Z-ADD     CSBNDP        V1CNDP
042900910917     C* BANCA PAGAMENTO - DECODIFICA SOLO SE COD. <> 0
043000950710     C                   MOVEL     *BLANKS       V1DBNA
043100950710     C                   Z-ADD     CSBBNA        V1CBNA
043200980127     C     CSBBNA        IFNE      0
043300980127     C                   CLEAR                   TIBS69
043400980127     C                   CLEAR                   CNACO
043500980127     C                   CLEAR                   CNIND
043600980127     C                   CLEAR                   CNCLP
043700980127     C                   CLEAR                   FNCLS
043800980127     C                   MOVEL     ' '           I69TLA
043900980127     C                   MOVEL     KNSIF         I69SIF
044000980127     C                   Z-ADD     KBNA          I69KCC
044100980127     C                   Z-ADD     CSBBNA        I69KAC
044200980127     C                   MOVEL     *BLANKS       ACORAG
044300980127     C                   CALL      'TIBS69R'
044400980127     C                   PARM                    TIBS69
044500980127     C                   PARM                    CNACO
044600980127     C                   PARM                    CNIND
044700980127     C                   PARM                    CNCLP
044800980127     C                   PARM                    FNCLS
044900980127     C     O69ERR        IFEQ      *BLANK
045000980127     C                   MOVEL     ACORAG        V1DBNA
045100910917     C                   END
045200980127     C                   END
045300910917     C* DATA PAGAMENTO
045400941025     C                   Z-ADD     CSBDDP        G02INV
045500941025     C                   Z-ADD     0             G02DAT
045600941026     C                   MOVEL     '3'           G02ERR
045700941025     C                   CALL      'XSRDA8'
045800941025     C                   PARM                    WLBDA8
045900950710     C                   Z-ADD     G02DAT        V1CDDP
046000941025     C* DATA REGISTRAZIONE PAGAMENTO
046100941025     C                   Z-ADD     CSBDRG        G02INV
046200941025     C                   Z-ADD     0             G02DAT
046300941026     C                   MOVEL     '3'           G02ERR
046400941025     C                   CALL      'XSRDA8'
046500941025     C                   PARM                    WLBDA8
046600950710     C                   Z-ADD     G02DAT        V1CDRG
046700950710     C                   Z-ADD     CSBNRG        V1CNRG
046800910917     C* TIPO PAGAMENTO EFFETTUATO
046900950710     C                   MOVEL     CSBFPC        V1CFPC
047000910917     C* DATI EVENTUALE GIACENZA
047100950710     C                   Z-ADD     CSBNGC        V1CNGC
047200941025     C                   Z-ADD     CSBDGC        G02INV
047300941025     C                   Z-ADD     0             G02DAT
047400941026     C                   MOVEL     '3'           G02ERR
047500941025     C                   CALL      'XSRDA8'
047600941025     C                   PARM                    WLBDA8
047700950710     C                   Z-ADD     G02DAT        V1CDGC
047800910917     C* RAGGRUPPAMENTO CONTABILE
047900950710     C                   Z-ADD     CSBRGF        V1CRGF
048000910917     C* TIPO BOLLA
048100950710     C                   MOVEL     CSBTBL        V1CTBL
048200910917     C* IMPORTO C/A
048300950710     C                   Z-ADD     CSBCAS        V1CCAS
048400950710     C                   MOVEL     CSBVCA        V1CVCA
048500941028     C* FLAG PARTICOLARITA'
048600950710     C                   MOVEL     CSBGCA        V1CGCA
048700120209     C* SE C/A GIA PAGATO  PROTEGGO TUTTI I CAMP oppure se multiassegno
048800120209     c*
048900950710     C     V1CDDP        IFNE      0
049000120209     C                   SETOFF                                       777876
049100120209     c                   else
049200120224     C                   SETOn                                        76
049300910920     C                   END
049400120209     c* 09 comando per interrogare multiassegno
049500120220     c                   if        %subst(csbnra: 10: 1)=' ' or csbtpi <> 'M'
049600120209     c                   setoff                                       09
049700120209     c                   else
049800120224     c                   seton                                        09
049900120224     c                   end
050000941028     C*
050100950710     C     V1CPRM        COMP      *BLANKS                            10
050200950710     C     V1CNZM        COMP      *BLANKS                            11
050300950710     C     V1CPRI        COMP      *BLANKS                            14
050400950710     C     V1CNZI        COMP      *BLANKS                            13
050500910920     C*
050600910902     C                   ENDSR
050700120224     C*-------------------------------------------------------------------*
050800120224     C     ctr02         BEGSR
050900120224     C*-------------------------------------------------------------------*
051000950711     C                   SETOFF                                       284041
051100950711     C                   SETOFF                                       424344
051200950711     C                   SETOFF                                       454647
051300950711     C                   SETOFF                                       4849
051400940413     C                   MOVEL     *BLANKS       $MSG
051500960326     C                   Z-ADD     0             DATASS            8 0
051600910918     C* CONTROLLO SE ESISTE LO STATO
051700910918     C                   MOVEL     '4S'          CODTAB
051800941108     C                   MOVEL     *BLANK        KEYTAB
051900950710     C                   MOVE      V1CSTA        KEYTAB
052000940413     C                   MOVEL     *BLANKS       TBLUNI
052100950711     C     KTAB          CHAIN     TABEL                              28
052200950711     C     *IN28         IFEQ      '1'
052300950711     C                   SETON                                        40
052400960923     C                   MOVEL     ERR(2)        $MSG
052500940413     C                   GOTO      FINAGG
052600940413     C                   END
052700960923     C                   MOVEL     TBLUNI        DS4S
052800960923     C     CSBSTA        IFNE      V1CSTA
052900960923     C     WMOD          IFNE      'S'
053000960923     C                   SETON                                        4028
053100960923     C                   MOVEL     CSBSTA        V1CSTA
053200960923     C                   MOVEL     ERR(23)       $MSG
053300960923     C                   GOTO      FINAGG
053400960923     C                   END
053500960923     C     §4SMOD        IFNE      'S'
053600960923     C                   SETON                                        4028
053700960923     C                   MOVEL     ERR(22)       $MSG
053800960923     C                   GOTO      FINAGG
053900960923     C                   END
054000960923     C                   END
054100960923     C                   MOVEL     §4SDES        V1DSTA
054200910920     C* SE SI VUOLE ANNULLARE IL C/A NON DEVE ESSERE INCASSATO
054300120217     C                   IF        (V1CSTA = 9 AND V1CDTA <> 0) OR
054400120217     C                             (V1CDDC <> 0 AND (V1CSTA=3 OR V1CSTA=8))
054500950711     C                   SETON                                        4028
054600940413     C                   MOVEL     ERR(3)        $MSG
054700940413     C                   GOTO      FINAGG
054800910920     C                   END
054900910918     C* CONTROLLA LA LINEA DI ARRIVO
055000950710     C                   Z-ADD     V1CLNA        ORGFIL
055100950711     C     ORGFIL        CHAIN     AZORG01L                           41
055200950711     C     *IN41         IFEQ      '1'
055300950711     C                   SETON                                        28
055400940413     C                   MOVEL     ERR(5)        $MSG
055500940413     C                   GOTO      FINAGG
055600940413     C                   END
055700950710     C                   MOVEL     ORGDES        V1DLNA
055800910918     C* CONTROLLA CODICE INTESTAZIONE CLIENTE
055900950710     C                   MOVE      V1CCDI        APP               4
056000971219      * NON SI PUò DIGITARE COD.INTESTAZ. = 8888/9999
056100910918     C     APP           IFEQ      '8888'
056200910918     C     APP           OREQ      '9999'
056300950710     C     CSBCDI        IFNE      V1CCDI
056400950711     C                   SETON                                        42
056500911001     C                   END
056600910918     C                   ELSE
056700971218      * CHIAMO TIBS69 PER RICERCA CODICE CLIENTE
056800971218     C                   CLEAR                   TIBS69
056900971218     C                   CLEAR                   CNACO
057000971218     C                   CLEAR                   CNIND
057100971218     C                   CLEAR                   CNCLP
057200971218     C                   CLEAR                   FNCLS
057300971218     C                   MOVEL     ' '           I69TLA
057400971218     C                   MOVEL     KNSIF         I69SIF
057500971218     C                   Z-ADD     V1CCDI        I69KAC
057600971219     C                   Z-ADD     V1CCDI        I69KIN
057700971218     C                   CALL      'TIBS69R'
057800971218     C                   PARM                    TIBS69
057900971218     C                   PARM                    CNACO
058000971218     C                   PARM                    CNIND
058100971218     C                   PARM                    CNCLP
058200971218     C                   PARM                    FNCLS
058300971218     C     O69ERR        IFNE      *BLANK
058400971218     C                   SETON                                        42
058500971218     C                   ENDIF
058600910918     C                   END
058700950711     C  N42V1CCDI        COMP      0                                      42
058800950711     C     *IN42         IFEQ      '1'
058900950711     C                   SETON                                        28
059000940413     C                   MOVEL     ERR(6)        $MSG
059100940413     C                   GOTO      FINAGG
059200940413     C                   END
059300971218      * CONTROLLO CONGRUENZA ramo aziendale delle PRIME TRE CIFRE DEL COD.CLI
059400971218      *           DI INTESTAZ. PAGAMENTO CON LNP SE FRANCO O LNA SE ASSEGN.
059500971218     C                   MOVEL     V1CCDI        §LNCLI            3 0
059600971219      * SE CAMBIATO IL COD. INTESTAZ. LO DECODIFICO
059700971219     C     V1CCDI        IFNE      CSBCDI
059800971219     C                   MOVEL     ACORAG        V1DINT
059900971219     C                   MOVEL     INDVIA        V1CVII
060000971219     C                   MOVEL     *BLANKS       V1CCAI
060100971219     C     INDCAP        IFNE      *ZEROS
060200971219     C                   MOVEL     INDCAP        V1CCAI
060300971219     C                   END
060400971219     C                   MOVEL     INDSTA        V1CNZI
060500971219     C                   MOVEL     INDPRV        V1CPRI
060600971219     C     INDCAE        IFNE      *ZEROS
060700971219     C     INDCAE        ANDNE     *BLANKS
060800971219     C                   MOVEL     INDCAE        V1CCAI
060900971219     C                   END
061000971219     C                   MOVEL     INDCIT        V1CCIT
061100971219     C                   ENDIF
061200971219      * SE è STATO REIMPOSTATO = A KSC PULISCO I DATI COD. INTEST.
061300971219     C     V1CCDI        IFEQ      CSBKSC
061400971219     C                   CLEAR                   V1DINT
061500971219     C                   CLEAR                   V1CVII
061600971219     C                   CLEAR                   V1CCIT
061700971219     C                   CLEAR                   V1CCAI
061800971219     C                   CLEAR                   V1CNZI
061900971219     C                   CLEAR                   V1CPRI
062000971219     C                   ENDIF
062100910918     C* CONTROLLA RAGGRUPPAMENTO CONTABILITA'
062200950710     C                   Z-ADD     V1CRGF        ORGFIL
062300950711     C     ORGFIL        CHAIN     AZORG01L                           43
062400950711     C     *IN43         IFEQ      '1'
062500950711     C                   SETON                                        28
062600940413     C                   MOVEL     ERR(7)        $MSG
062700940413     C                   GOTO      FINAGG
062800940413     C                   END
062900910918     C* CONTROLLA IMPORTO ASSEGNO CHE NON SIA 0
063000950711     C     V1CCAS        IFEQ      0
063100950711     C                   SETON                                        2844
063200940413     C                   MOVEL     ERR(8)        $MSG
063300940413     C                   GOTO      FINAGG
063400940413     C                   END
063500941025     C* CONTROLLO LA DIVISA
063600941025     C                   CLEAR                   DSCV
063700011129     C                   MOVE      V1CCAS        DEC               2
063800950710     C     V1CVCA        IFNE      *BLANKS
063900960604     C*  Controllo interrogazione codicd valuta
064000960604     C     '?'           SCAN      V1CVCA                                 95
064100960604     C     *IN95         IFEQ      '1'
064200960604     C                   Z-ADD     1             §KUT              1 0
064300960604     C                   MOVEL     'CV'          §COD              2
064400960604     C                   MOVE      *BLANKS       V1CVCA
064500960604     C                   CALL      'X§TABER'
064600960604     C                   PARM                    §KUT
064700960604     C                   PARM                    §COD
064800960604     C                   PARM                    §KEY              8
064900960604     C                   PARM                    §DES             30
065000960604     C                   MOVEL     §KEY          V1CVCA
065100960604     C                   END
065200960604     C*
065300941025     C                   MOVEL     'CV'          CODTAB
065400941108     C                   MOVEL     *BLANK        KEYTAB
065500950710     C                   MOVEL     V1CVCA        KEYTAB
065600950711     C     KTAB          CHAIN     TABEL                              49
065700950711     C     *IN49         IFEQ      '1'
065800941025     C     TBLFLG        ORNE      ' '
065900950711     C                   SETON                                        2849
066000941025     C                   MOVEL     ERR(17)       $MSG
066100941025     C                   GOTO      FINAGG
066200941025     C                   END
066300951023     C                   MOVEL     TBLUNI        DSCV
066400941025     C*
066500941025     C     DEC           IFNE      *ZEROS
066600941025     C     §CVFDC        ANDNE     'S'
066700950711     C                   SETON                                        494428
066800941025     C                   MOVEL     ERR(18)       $MSG
066900941025     C                   END
067000120209     ** Controllo importo minimo e massimo contrassegno.
067100011113     C                   EXSR      ChkCasMax
067200011113     C                   IF        O21Fx2 <> *BLANK
067300011114     C                             OR
067400011114     C                             O21FMn <> *BLANK
067500011113     C                   SETON                                        494428
067600011114     C                   EVAL      $Msg =
067700011114     C                             'Immettere un importo compreso fra '
067800011114     C                             + %TRIM(%EDITC(O21LMn:'2')) + ' ' + §GEDMc
067900011114     C                             + ' e '
068000011114     C                             + %TRIM(%EDITC(O21Lx2:'2')) + ' ' + §GEDMc
068100011114     C                             + '.'
068200011113     C                   GOTO      FinAgg
068300011113     C                   ENDIF
068400941025     C*
068500941025     C                   ELSE
068600941025     C*
068700950711     C                   SETON                                        494428
068800941025     C                   MOVEL     ERR(19)       $MSG
068900971008     C                   GOTO      FINAGG
069000941025     C                   END
069100940413     C* CONTROLLO DATI ASSEGNO
069200950710     C                   MOVEL     *BLANKS       V1CDES
069300950710     C     V1CTPA        IFEQ      ' '
069400950710     C     V1CNRA        IFNE      *ZEROS
069500950710     C     V1CNRA        ANDNE     *BLANKS
069600950711     C                   SETON                                        454428
069700940413     C                   MOVEL     ERR(9)        $MSG
069800940413     C                   GOTO      FINAGG
069900940413     C                   END
070000950710     C     V1CABI        IFNE      *ZEROS
070100950710     C     V1CCAB        ORNE      *ZEROS
070200950711     C                   SETON                                        474628
070300940413     C                   MOVEL     ERR(10)       $MSG
070400940413     C                   GOTO      FINAGG
070500940413     C                   END
070600950710     C     V1CDTE        IFNE      *ZEROS
070700950711     C                   SETON                                        484628
070800940413     C                   MOVEL     ERR(11)       $MSG
070900940413     C                   GOTO      FINAGG
071000940413     C                   END
071100940413     C*
071200940413     C                   ELSE
071300950710     C     V1CNRA        IFEQ      *ZEROS
071400950710     C     V1CNRA        OREQ      *BLANKS
071500950711     C                   SETON                                        454628
071600940413     C                   MOVEL     ERR(12)       $MSG
071700940413     C                   GOTO      FINAGG
071800940413     C                   END
071900120209     c* solo se non è multiassegno
072000120209     c                   if        not *in09
072100950710     C     V1CABI        IFEQ      *ZEROS
072200950710     C     V1CCAB        ANDEQ     *ZEROS
072300950711     C                   SETON                                        474628
072400940413     C                   MOVEL     ERR(13)       $MSG
072500940413     C                   GOTO      FINAGG
072600940413     C                   END
072700940413     C* ABI / CAB / EVENTUALE DECODIFICA BANCA
072800950710     C                   Z-ADD     V1CABI        CSBABI
072900950710     C                   Z-ADD     V1CCAB        CSBCAB
073000950711     C     KABI          CHAIN     CNABI000                           28
073100950711     C     *IN28         IFEQ      '1'
073200950711     C                   SETON                                        4728
073300940413     C                   MOVEL     ERR(14)       $MSG
073400940413     C                   GOTO      FINAGG
073500940413     C                   END
073600950710     C                   MOVEL     ABIIST        V1CDES
073700950710     C     V1CDTE        IFEQ      *ZEROS
073800950711     C                   SETON                                        484628
073900940413     C                   MOVEL     ERR(15)       $MSG
074000940413     C                   GOTO      FINAGG
074100940413     C                   END
074200940412     C*  Controllo la validità della data
074300950710     C                   Z-ADD     V1CDTE        G02DAT
074400940412     C                   MOVEL     *BLANKS       G02ERR
074500941025     C                   CALL      'XSRDA8'
074600941025     C                   PARM                    WLBDA8
074700940412     C     G02ERR        IFEQ      '1'
074800950711     C                   SETON                                        2848
074900940413     C                   MOVEL     ERR(16)       $MSG
075000940413     C                   GOTO      FINAGG
075100940412     C                   ELSE
075200940412     C                   Z-ADD     G02INV        DATASS
075300950710     C                   Z-ADD     G02DAT        V1CDTE
075400940412     C                   END
075500120209     C                   END
075600940413     C*
075700940124     C                   END
075800971219      * SE TPI = M , DEVONO ESSERCI I RIFERIM. DELL'ASSEGNO
075900971219     C     V1CTPI        IFEQ      'M'
076000971219     C     V1CNRA        IFEQ      *BLANKS
076100971219     C                   SETON                                        2845
076200971219     C                   MOVEL     ERR(12)       $MSG
076300971219     C                   ENDIF
076400120209     c* solo per no multiassegno
076500971219     C     V1CDTE        IFEQ      *ZEROS
076600120209     c     *in09         andeq     *off
076700971219     C                   SETON                                        2848
076800971219     C                   MOVEL     ERR(15)       $MSG
076900971219     C                   ENDIF
077000971219     C     V1CTPA        IFEQ      *BLANK
077100971219     C                   SETON                                        2846
077200971219     C                   MOVEL     ERR(26)       $MSG
077300971219     C                   ENDIF
077400971219     C   28              GOTO      FINAGG
077500971219     C                   ENDIF
077600971219     C     V1CNDT        IFEQ      *ZEROS
077700971219     C     V1CNRA        IFNE      *BLANKS
077800971219     C     V1CDTE        ORNE      *ZEROS
077900971219     C                   SETON                                        2845
078000971219     C                   MOVEL     ERR(27)       $MSG
078100971219     C                   ENDIF
078200971219     C   28              GOTO      FINAGG
078300971219     C                   ENDIF
078400120224     C*
078500120224     C     finagg        ENDsr
078600120224     c**********************************************************************
078700120224     c     aggio1        begsr
078800120224     c**********************************************************************
078900120224     C     kcsb          SETLL     tncsb09l
079000120224?    C                   do        *hival
079100120224     C     kcsb          READE     tncsb09l
079200120224     C                   if        %eof(tncsb09l)
079300120224     c                   leave
079400120224     c                   end
079500120224     c* solo il contrassegno con legame
079600120224     c                   if        £csbnra <> CSBNRA
079700120224     c                   iter
079800120224     c                   end
079900120224     c* aggancio i record per aggiornarli
080000120224     C     KSPED         CHAIN     TNCSB03L
080100120224     C                   IF        %FOUND(TNCSB03L)
080200120224     c*
080300120224     c                   setoff                                       71
080400120224     C                   MOVE      £csbCDI       §8899             4 0
080500120224  B2 C     §8899         IFEQ      8888
080600120224     C     §8899         OREQ      9999
080700120224     C     ksped         CHAIN     TNCSA000                           70
080800120224  B3 C     *IN70         IFEQ      *OFF
080900120224     C                   SETON                                        71
081000120224     c                   end
081100120224     c                   end
081200120224     c*
081300120224?    C                   EXSR      aggio
081400120224     c                   end
081500120224     C                   enddo
081600120224     c                   endsr
081700120224     C*-------------------------------------------------------------------*
081800120224     C     AGGIO         BEGSR
081900120224     C*-------------------------------------------------------------------*
082000910919     C* SE O.K. SCRIVE SITUAZIONE PRECEDENTE NELLO STORICO E AGGIORNA
082100971008     C* MUOVO I CAMPI DI TNCSB  IN BLCSV
082200971008     C                   MOVEL     CSBDS         CSVDS
082300971008      * Imposto nuovi valori
082400120224     c                   if        not *in09
082500951023     C                   Z-ADD     V1CABI        CSBABI
082600950710     C                   Z-ADD     V1CCAB        CSBCAB
082700950710     C                   Z-ADD     V1CLNA        CSBLNA
082800950710     C                   Z-ADD     V1CCAS        CSBCAS
082900950710     C                   MOVEL     V1CVCA        CSBVCA
083000120224     C                   Z-ADD     DATASS        CSBDTE
083100120224     C                   MOVEL     V1CTPA        CSBTPA
083200120224     C                   MOVEL     V1CNRA        CSBNRA
083400950710     C                   Z-ADD     V1CCDI        CSBCDI
083500950710     C                   Z-ADD     V1CRGF        CSBRGF
083501120301     c                   end
083600950710     C                   Z-ADD     V1CSTA        CSBSTA
083700971008     C     CSBDS         IFNE      CSVDS
083800971008     C                   EXSR      STORY
083900971008     C                   END
084000960923     C*  Se devo trasmettere la modifica dello stato in filiale
084100960923     C*  azzero dati trasmissione
084200960923     C     §4SFIL        IFEQ      'S'
084300960923     C                   Z-ADD     0             CSBDTR
084400960923     C                   MOVEL     *BLANKS       CSBFTR
084500960923     C                   END
084600910918     C                   EXCEPT    AGG
084700971008      * Scrivo storico se sono stati modificati i valori
084800971008     C* Aggiorno TNCSA
084900971009     C     *IN71         IFEQ      '1'
085000971008     C                   MOVEL     V1DMIO        CSARSD
085100971008     C                   MOVEL     V1CVIM        CSAVID
085200971008     C                   MOVEL     V1CCAM        CSACAD
085300971008     C                   MOVEL     V1CLOM        CSACID
085400971008     C                   MOVEL     V1CNZM        CSANZM
085500971008     C                   MOVEL     V1CPRM        CSAPRM
085600971008     C                   UPDATE    TNCSA000
085700971009     C                   END
085800971008     C*
085900120224     C                   ENDSR
086000120224     C**********************************************************************
086100910919     C     STORY         BEGSR
086200120224     C**********************************************************************
086300910919     C* IMPOSTO I CAMPI NON COMUNI
086400950710     C     V1CSTA        IFEQ      9
086500910919     C                   MOVE      'ANCA'        CSVCAV
086600910919     C                   ELSE
086700910919     C                   MOVE      'VACA'        CSVCAV
086800910919     C                   END
086900941025     C                   TIME                    WHHDAT           14 0
087000941103     C                   MOVE      WHHDAT        ANNO              4 0
087100941025     C                   MOVE      WHHDAT        G02DAT
087200941025     C                   Z-ADD     0             G02INV
087300941025     C                   CALL      'XSRDA8'
087400941025     C                   PARM                    WLBDA8
087500941025     C                   Z-ADD     G02INV        CSVDTV
087600910919     C                   TIME                    CSVORV
087700910919     C                   Z-ADD     046           CSVFEV
087800941025     C                   WRITE     TNCSV000
087900910919     C                   ENDSR
088000910930     C*
088100941025      *-----------------------------------------------------*
088200941025      * OPERAZIONI INIZIALI                                 *
088300941025      *-----------------------------------------------------*
088400940413     C     *INZSR        BEGSR
088500940413      * CHIAVI
088600940413     C     KTAB          KLIST
088700940413     C                   KFLD                    CODUT
088800940413     C                   KFLD                    CODTAB            2
088900940413     C                   KFLD                    KEYTAB            8
089000011019      *
089100011019     C     K03Tbe01      KLIST
089200011019     C                   KFLD                    TbeCod
089300011019     C                   KFLD                    TbeKe1
089400011019     C                   KFLD                    TbeKe2
089500120224      *
089600120224     c     kcsb          klist
089700120224     c                   kfld                    csbrgf
089800120224     c                   kfld                    csbndt
089900120224     c                   kfld                    csbddc
090000940413      *
090100940413     C     KSPE          KLIST
090200950711     C                   KFLD                    V1CAAS
090300950711     C                   KFLD                    V1CLNP
090400950711     C                   KFLD                    V1CNRS
090500950711     C                   KFLD                    V1CNSP
090600120224     C     KSPED         KLIST
090700120224     C                   KFLD                    £CSBAAS
090800120224     C                   KFLD                    £CSBLNP
090900120224     C                   KFLD                    £CSBNRS
091000120224     C                   KFLD                    £CSBNSP
091100940413     C*
091200940413     C     KBLCAS        KLIST
091300950710     C                   KFLD                    V1CAAS
091400950710     C                   KFLD                    V1CLNP
091500950710     C                   KFLD                    V1CNRS
091600950710     C                   KFLD                    V1CNSP
091700940413     C*
091800940413     C     KBAN1         KLIST
091900940413     C                   KFLD                    CODUT
092000940413     C                   KFLD                    KBNA
092100950710     C                   KFLD                    V1CBNA
092200940413     C*
092300940413     C     KABI          KLIST
092400940413     C                   KFLD                    CSBABI
092500940413     C                   KFLD                    CSBCAB
092600011112     C*
092700011112     C     K05Tas30      KLIST
092800011112     C                   KFLD                    TasAaS
092900011112     C                   KFLD                    TasLnP
093000011112     C                   KFLD                    TasNrS
093100011112     C                   KFLD                    TasNSp
093200011112     C                   KFLD                    TasTBl
093300100219     C     Kar5          KLIST
093400100219     C                   KFLD                    V1CAAS
093500100219     C                   KFLD                    V1CLNP
093600100219     C                   KFLD                    V1CNRS
093700100219     C                   KFLD                    V1CNSP
093800100219     C                   KFLD                    ar5trd
093900100219     c                   eval      ar5trd = 'GEN'
094000940413      *---------------------------------------------------------------*
094100940413     C     *ENTRY        PLIST
094200940413     C                   PARM                    KPJBA
094300940413     C                   MOVEL     KPJBU         PAS01
094400940413      *
094500940413     C                   Z-ADD     1             CODUT
094600940413     C                   CALL      'X§PARUT'
094700940413     C                   PARM                    UT§DSE
094800940413     C                   MOVEL     RAGUT         RSUT             20
094900940413     C                   MOVEL     REC80         CNCR80
095000940413     C                   MOVEL     KNMUS         §SEI              6
095100940413      *
095200940413      *  RICERCA CAPOCONTI
095300940413     C                   DO        50            X                 2 0
095400940413     C                   MOVE      TCU(X)        TCUDS
095500940413     C     F56           IFEQ      'CG'
095600940413     C     F4            IFEQ      '6'
095700940413     C                   Z-ADD     KCU(X)        KBNA              4 0
095800940413     C                   END
095900940413     C                   END
096000940413     C*
096100940413     C                   END
096200940413     C*
096300941103     C                   TIME                    WHHDAT           14 0
096400941103     C                   MOVE      WHHDAT        ANNO              4 0
096500011019     C*
096600011019     C                   EVAL      TbeCod = 'GED'
096700011019     C                   EVAL      TbeKe1 = '1'
096800011019     C                   CLEAR                   TbeKe2
096900011019     C     K03Tbe01      CHAIN     TnTbe01L
097000011019     C                   IF        %FOUND AND TbeAtb = *BLANK
097100011019     C                   EVAL      DGED = TbeUni
097200011019     C                   ENDIF
097300011019     C*
097400940413     C                   ENDSR
097500120209     c*---------------------------------------------------------------*
097600011112     ** Controllo importo massimo contrassegno.
097700120209     c*---------------------------------------------------------------*
097800011113     C     ChkCasMax     BEGSR
097900011112     C*
098000011112     C                   EVAL      TasAaS = CsbAaS
098100011112     C                   EVAL      TasLnP = CsbLnP
098200011112     C                   EVAL      TasNrS = CsbNrS
098300011112     C                   EVAL      TasNSp = CsbNSp
098400011112     C                   EVAL      TasTBl = CsbTBl
098500011112     C     K05Tas30      CHAIN     TiTas30C
098600011112     C*
098700011112     C                   CLEAR                   Trul21DS
098800011112     C                   EVAL      I21CBo = TasCBo
098900011112     C                   EVAL      I21TSp = TasTsp
099000011112     C                   EVAL      I21LnP = TasLnP
099100011112     C                   EVAL      I21NzM = TasNzM
099200011112     C                   EVAL      I21LnA = TasLnA
099300011112     C                   EVAL      I21NzD = TasNzD
099400130828     C****               EVAL      I21Ksc = TasKsc
099401130828     C                   EVAL      I21Ksc = Tasccm
099500011112     C                   MOVE      TasCTr        I21CTr
099600011113     C                   EVAL      I21TiC = V1CTpA + V1CTpI
099700011112     C                   EVAL      I21Imp = V1CCas
099800011112     C                   EVAL      I21Div = V1CVCa
099900011112     C                   EVAL      I21Ute = KNmUs
100000011112     C                   EVAL      I21Pgm = NomPgm
100100011112     C                   CALL      'TRUL21R'
100200011112     C                   PARM                    Trul21DS
100300011112     C*
100400011112     C                   ENDSR
100500900606      *----------------------------------------------------------------
100600941025     OTNCSB000  E            AGG
100700910918     O                       CSBLNA
100800941025     O                       CSBCAS
100900941025     O                       CSBVCA
101000910918     O                       CSBCDI
101100910918     O                       CSBRGF
101200910918     O                       CSBSTA
101300931203     O                       CSBABI
101400940121     O                       CSBCAB
101500940121     O                       CSBNRA
101600940412     O                       CSBDTE
101700940413     O                       CSBTPA
101800960923     O                       CSBDTR
101900960923     O                       CSBFTR
102000971219** ERR
102100940413Numero di spedizione errato                                           01
102200940413Stato Contrassegno non esistente in tabella !!                        02
102300010112Stato C/ass non utilizzabile : è già incassato  !!                    03
102400940413Spedizione non tassata modificabile solo per il raggruppamento        04
102500940413Linea di arrivo errata                                                05
102600940413Codice cliente non valido                                             06
102700961119Punto operativo raggruppamento inesistente                            07
102800940413L'importo del Contrassegno non può essere 0                           08
102900940413Impossibile immettere numero assegno                                  09
103000940413ABI-CAB non validi per questo tipo di incasso                         10
103100940413Data assegno non valida per questo tipo di incasso                    11
103200940413Immettere numero Assegno                                              12
103300940413ABI-CAB Obbligatori per questo tipo di incasso                        13
103400940413ABI-CAB Errati                                                        14
103500940413Data Assegno obbligatoria per questo tipo di incasso                  15
103600940413Data Assegno errata                                                   16
103700941025Codice divisa inesistente                                             17
103800941025Per la divisa indicata non sono ammessi importi con decimali          18
103900010615Divisa obbligatoria !!!                                               19
104000941025Se immesso cambio obbligatoria divisa estera                          20
104100950727C/Ass. non disponibile per effettuare variazione. Attendi 5' e ritenta21
104200960923Stato C/Assegno non utilizzabilie da manutenzione                     22
104300960923Stato C/Assegno iniziale non modificabile                             23
104400011019Importo superiore al limite di                                        24
104500971218Filiale cod. cl. non è congruente con ramo aziendale della LNP o LNA  25
104600971219Immettere il Tipo Assegno                                             26
104700971219C/Assegno non incassato , non immettere riferim. assegno              27
104800120224Sono presenti altre spedizioni/assegni. F10 per forzare               28
