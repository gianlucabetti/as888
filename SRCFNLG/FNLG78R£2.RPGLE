000100930203     H DECEDIT('0,') DATEDIT(*DMY.)
000200970701     H* FNLG78R *----------------------------------------------------*
000300930203     H*                                                              *
000400970701     H*      - RISTAMPA LETTERA CHIUSURA GIACENZA -    -FILTRO-      *
000500930203     H*                                                              *
000600970701     F*--------------------------------------------------------------*
000700970701     F*  D A T A       B A S E                                       *
000800970701     F*--------------------------------------------------------------*
000900970701     FFNLG78D   CF   E             WORKSTN
001000970715     F*--------
001100970715     FFNLBL01L  IF   E           K DISK
001200970701     F*--------
001300050309     Ftigcp02L  IF   E           K DISK
001400970701     F*--------
001500050309     Ftigcp01L  IF   E           K DISK
001600050309     F                                     RENAME(tigcp000:tigcp001)
001700970701     F*--------
001800970630     FFNBLP01L  IF   E           K DISK
001900970701     F*--------
002000970701     FTABEL00F  IF   E           K DISK
002100970701     F*--------
002200970701     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002300941018     D*--------------------------------------------------------------*
002400970701     D*  S C H I E R E                                               *
002500970701     D*--------------------------------------------------------------*
002600970109     D FL£7            S              3    DIM(29)
002700020503     D*FL£1            S              3    DIM(29)
002800020503     D £1              S              3  0 DIM(30)
002900970109     D £7              S              3  0 DIM(29)
003000970701     D ERR             S             78    DIM(24) CTDATA PERRCD(1)
003100941018     D*--------------------------------------------------------------*
003200970701     D*  DS                                                          *
003300970701     D*--------------------------------------------------------------*
003400970701     D PARA04          DS                  INZ
003500970701     D  P04AAS                 1      4  0
003600970701     D  P04LNP                 5      7  0
003700970701     D  P04NRS                 8      9  0
003800970701     D  P04NSP                10     16  0
003900970701     D  P04FLG                31     31
004000970701     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
004100970701     D  P04GIA                48     48
004200970701      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
004300970701     D  P04CLI               143    143
004400970701     D PARA05          DS                  INZ
004500970701     D  P05AAS                 1      4  0
004600970701     D  P05LNP                 5      7  0
004700970701     D  P05NRS                 8      9  0
004800970701     D  P05NSP                10     16  0
004900970701     D  P05F03                17     17
005000970701     D  P05FLG                18     18
005100970701     D  P05SIM                44     46  0
005200970701     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
005300970701     D  P05GIA                48     48
005400970701     D FNLG30        E DS                  EXTNAME(FNLG30DS)
005500970701     D FNLG78        E DS                  EXTNAME(FNLG78DS)
005600970701     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
005700930203     D TCUDS           DS
005800930203     D CNCR80        E DS
005900930203     D CNCR42        E DS
006000930203     D KPJBA         E DS
006100020503     d trul06ds      e ds
006200020503     d  lin                    1     90  0 dim(30)
006300970701     I*--------------------------------------------------------------*
006400970701     I*  File                                                        *
006500970701     I*--------------------------------------------------------------*
006600970701     IAZORG01L  AA
006700970701     I                             P    4    5 0ORGFIL
006800970701     I                                  7    7  ORGFAG
006900970701     I                                 12   12  ORGFL2
007000930203     C*---------------------------------------------------------------*
007100970701     C*    - - -   C i c l o        P r i n c i p a l e    - - -      *
007200970701     C*---------------------------------------------------------------*
007300970109     C*  Inizializzazione videata
007400970109     C                   EXSR      INZ01
007500970109     C*  Loop videate
007600970701     C     WFINE         DOWNE     'S'
007700970109     C                   EXSR      GESD01
007800970109     C                   END
007900970109     C*
008000920522     C                   SETON                                        LR
008100970701     C*------------------------------------------------------------------*
008200970701     C*  Inizializzazione Prima videata
008300970701     C*------------------------------------------------------------------*
008400970701     C     INZ01         BEGSR
008500970701     C*
008600970701     C*  Azzero campi e indicatori di errore
008700970701     C                   MOVEL     *BLANKS       $MSG
008800970701     C                   SETOFF                                       404142
008900970701     C                   SETOFF                                       434445
009000970701     C                   SETOFF                                       464748
009100970701     C                   SETOFF                                       492802
009200970701     C* Reperisco fil da elaborare
009300970701     C                   MOVEL     WFIL          V1CFIL
009400970701     C*  Caricamento tabella £7
009500970701     C                   EXSR      CAR£7
009600970701     C*  Inizializzo scelte
009700970701     C                   MOVEL     *BLANKS       V1CSC1
009800970701     C                   MOVEL     *BLANKS       V1CSC2
009900970701     C                   Z-ADD     0             V1CAGC
010000970701     C                   Z-ADD     0             V1CFGC
010100970701     C                   Z-ADD     0             V1CNGC
010200970701     C                   Z-ADD     0             V1CAAS
010300970701     C                   Z-ADD     0             V1CLNP
010400970701     C                   Z-ADD     0             V1CNRS
010500970701     C                   Z-ADD     0             V1CNSP
010600970701     C*
010700970701     C                   ENDSR
010800970109     C*------------------------------------------------------------------*
010900970109     C*  Gestione Videata
011000970109     C*------------------------------------------------------------------*
011100970109     C     GESD01        BEGSR
011200970109     C*
011300970701     C                   EXFMT     LG78D01
011400970109     C*  Azzero campi e indicatori di errore
011500970109     C                   MOVEL     *BLANKS       $MSG
011600970701     C                   SETOFF                                       404142
011700970701     C                   SETOFF                                       434445
011800970701     C                   SETOFF                                       464748
011900970701     C                   SETOFF                                       492802
012000970109     C*
012100970701     C* Fine
012200970701     C     *INKC         IFEQ      '1'
012300970701     C                   MOVEL     'S'           WFINE             1
012400970701     C                   GOTO      FINVD1
012500970701     C                   END
012600970701     C* Se richiesto cambio filiale vado a fine videata
012700970109     C     *INKS         IFEQ      '1'
012800970109     C                   SETON                                        1802
012900970109     C                   GOTO      FINVD1
013000970109     C                   END
013100970701     C* Interrogazione bolle
013200970701     C     *INKG         IFEQ      '1'
013300970701     C     V1CNSP        IFGT      0
013400970701     C                   CLEAR                   PARA05
013500970701     C                   Z-ADD     V1CAAS        P05AAS
013600970701     C                   Z-ADD     V1CLNP        P05LNP
013700970701     C                   Z-ADD     V1CNRS        P05NRS
013800970701     C                   Z-ADD     V1CNSP        P05NSP
013900970701     C                   MOVEL     '1'           P05GIA
014000970701     C                   MOVEL     PARA05        KPJBU
014100970701     C                   CALL      'FNLS05R'
014200970701     C                   PARM                    KPJBA
014300970701     C                   MOVEL     KPJBU         PARA05
014400970701     C*
014500970701     C                   ELSE
014600970701     C*
014700970701     C                   CLEAR                   PARA04
014800970701     C                   MOVEL     '1'           P04FLG
014900970701     C                   MOVEL     '1'           P04GIA
015000970701     C                   MOVEL     PARA04        KPJBU
015100970701     C                   CALL      'FNLS04R'
015200970701     C                   PARM                    KPJBA
015300970701     C                   MOVEL     KPJBU         PARA04
015400970701     C     P04NSP        IFGT      *ZERO
015500970701     C                   Z-ADD     P04AAS        V1CAAS
015600970701     C                   Z-ADD     P04LNP        V1CLNP
015700970701     C                   Z-ADD     P04NRS        V1CNRS
015800970701     C                   Z-ADD     P04NSP        V1CNSP
015900970701     C                   ENDIF
016000970701     C                   END
016100970701     C                   GOTO      FINVD1
016200970701     C                   ENDIF
016300970701     C*  Richiamo interr. giacen
016400970701     C     *INKN         IFEQ      '1'
016500970701     C                   CLEAR                   FNLG30
016600970701     C                   MOVEL     'P'           D30POA
016700970701     C                   Z-ADD     V1CAAS        D30AAS
016800970701     C                   Z-ADD     V1CLNP        D30LNP
016900970701     C                   Z-ADD     V1CNRS        D30NRS
017000970701     C                   Z-ADD     V1CNSP        D30NSP
017100970701     C                   Z-ADD     V1CAGC        D30AGC
017200970701     C                   Z-ADD     V1CFGC        D30FGC
017300970701     C                   Z-ADD     V1CNGC        D30NGC
017400970701     C                   MOVE      *ZEROS        D30FRG
017500970701     C                   MOVEL     FNLG30        KPJBU
017600970701     C                   CALL      'FNLG30C'
017700970701     C                   PARM                    KPJBA
017800970701     C                   GOTO      FINVD1
017900970701     C                   END
018000970109     C* Controlli
018100970109     C                   EXSR      CTR01
018200970701     C   28              GOTO      FINVD1
018300970701     C*  F6=Sottometto ristampa
018400970701     C     *INKF         IFEQ      '1'
018500970701     C                   EXSR      SBMP79
018600970630     C                   END
018700970109     C*
018800970109     C     FINVD1        ENDSR
018900970109     C*------------------------------------------------------------------*
019000970109     C*  Controlli
019100970109     C*------------------------------------------------------------------*
019200970109     C     CTR01         BEGSR
019300970701     C*------------------------------------------------------------------*
019400970701     C*   Se ho modificatao la filiale controllo nuovo valore
019500970701     C     WFIL          IFNE      V1CFIL
019600970701     C* Punto operativo inesistente
019700970701     C                   Z-ADD     V1CFIL        ORGFIL
019800970701     C     ORGFIL        CHAIN     AZORG01L                           32
019900970701     C     *IN32         IFEQ      '1'
020000970701     C                   SETON                                        2840
020100970701     C                   MOVEL     ERR(1)        $MSG
020200970701     C                   GOTO      FINCT1
020300970701     C                   END
020400970701     C* Punto operativo inesistente
020500970701     C     V1CFIL        LOOKUP    £1                                     32
020600970701     C     *IN32         IFEQ      '0'
020700970701     C                   SETON                                        2840
020800970701     C                   MOVEL     ERR(2)        $MSG
020900970701     C                   GOTO      FINCT1
021000970701     C                   ELSE
021100970701     C                   EXSR      CAR£7
021200970701     C                   END
021300970701     C                   END
021400970701     C*------------------------------------------------------------------*
021500970701     C* Scelta: nessuna
021600970701     C     V1CSC1        IFNE      '1'
021700970701     C     V1CSC2        ANDNE     '1'
021800970701     C                   SETON                                        284145
021900970701     C                   MOVEL     ERR(3)        $MSG
022000970701     C                   GOTO      FINCT1
022100970701     C                   END
022200970701     C* Scelta: entrambe
022300970701     C     V1CSC1        IFEQ      '1'
022400970701     C     V1CSC2        ANDEQ     '1'
022500970701     C                   SETON                                        284145
022600970701     C                   MOVEL     ERR(4)        $MSG
022700970701     C                   GOTO      FINCT1
022800970701     C                   END
022900970701     C*------------------------------------------------------------------*
023000970701     C* Ristampa per numero GIACENZA
023100970701     C     V1CSC1        IFEQ      '1'
023200970701     C* Controllo se immesso anno
023300970630     C     V1CAGC        IFEQ      0
023400970701     C                   SETON                                        414228
023500970701     C                   MOVEL     ERR(5)        $MSG
023600970701     C                   GOTO      FINCT1
023700970630     C                   END
023800970701     C* controllo se immessa filiale apertura
023900970701     C     V1CFGC        IFEQ      0
024000970701     C                   SETON                                        414328
024100970701     C                   MOVEL     ERR(7)        $MSG
024200970701     C                   GOTO      FINCT1
024300970701     C                   END
024400970701     C* controllo se esiste
024500970701     C                   MOVEL     V1CFGC        ORGFIL
024600970701     C     ORGFIL        CHAIN     AZORG01L                           31
024700970701     C     *IN31         IFEQ      '1'
024800970701     C                   SETON                                        4328
024900970701     C                   MOVEL     ERR(9)        $MSG
025000970701     C                   GOTO      FINCT1
025100970701     C                   END
025200970701     C* controllo se immesso numero apertura
025300970701     C     V1CNGC        IFEQ      0
025400970701     C                   SETON                                        414428
025500970701     C                   MOVEL     ERR(10)       $MSG
025600970701     C                   GOTO      FINCT1
025700970701     C                   END
025800970701     C* Controllo se immesso anno spedizione
025900970701     C     V1CAAS        IFNE      0
026000970701     C                   SETON                                        414628
026100970701     C                   MOVEL     ERR(12)       $MSG
026200970701     C                   GOTO      FINCT1
026300970701     C                   END
026400970701     C* controllo se immessa linea di partenza
026500970701     C     V1CLNP        IFNE      0
026600970701     C                   SETON                                        414728
026700970701     C                   MOVEL     ERR(14)       $MSG
026800970701     C                   GOTO      FINCT1
026900970701     C                   END
027000970701     C* controllo se immesso numero di serie
027100970701     C     V1CNRS        IFNE      0
027200970701     C                   SETON                                        414828
027300970701     C                   MOVEL     ERR(18)       $MSG
027400970701     C                   GOTO      FINCT1
027500970701     C                   END
027600970701     C* controllo se immesso numero spedizione
027700970701     C     V1CNSP        IFNE      0
027800970701     C                   SETON                                        414928
027900970701     C                   MOVEL     ERR(17)       $MSG
028000970701     C                   GOTO      FINCT1
028100970701     C                   END
028200970701     C* se tutto Ok Anno da 2 a 4 chr
028300970701     C                   MOVEL     V1CAGC        WSECOL            2
028400970701     C     WSECOL        IFEQ      *ZEROS
028500970701     C     V1CAGC        IFGT      40
028600970701     C                   ADD       1900          V1CAGC
028700970701     C                   ELSE
028800970701     C                   ADD       2000          V1CAGC
028900970701     C                   END
029000970701     C                   END
029100970701     C*
029200970701     C* Controllo se esiste giacenza
029300970701     C                   Z-ADD     V1CAGC        KAGC
029400970701     C                   Z-ADD     V1CFGC        KFGC
029500970701     C                   Z-ADD     V1CNGC        KNGC
029600970701     C                   Z-ADD     0             KFRG
029700050309     C     KGCP2         CHAIN     tigcp02L                           31
029800970701     C     *IN31         IFEQ      '1'
029900970701     C                   SETON                                        424344
030000970701     C                   SETON                                        28
030100970701     C                   MOVEL     ERR(19)       $MSG
030200970701     C                   GOTO      FINCT1
030300970701     C                   END
030400970701     C* Imposto chiave per numero spedzione
030500970701     C                   Z-ADD     GCPAAS        KAAS
030600970701     C                   Z-ADD     GCPLNP        KLNP
030700970701     C                   Z-ADD     GCPNRS        KNRS
030800970701     C                   Z-ADD     GCPNSP        KNSP
030900970701     C* Se filiale di partenza non in £7 do errore
031000970701     C     GCPLNP        LOOKUP    £7                                     31
031100970701     C     *IN31         IFEQ      '0'
031200970701     C                   SETON                                        424344
031300970701     C                   SETON                                        28
031400970701     C                   MOVEL     ERR(20)       $MSG
031500970701     C                   GOTO      FINCT1
031600970701     C                   END
031700970701     C* Controllo se già eseguita stampa
031800970701     C     GCPFAS        IFLE      50
031900970701     C     GCPDLM        OREQ      0
032000970703     C                   SETON                                        424443
032100970701     C                   SETON                                        28
032200970701     C                   MOVEL     ERR(21)       $MSG
032300970701     C                   GOTO      FINCT1
032400970701     C                   END
032500970701     C* No reso
032600970701     C     GCPDIS        IFNE      'R'
032700970701     C                   SETON                                        424344
032800970701     C                   SETON                                        28
032900970701     C                   MOVEL     ERR(24)       $MSG
033000970701     C                   GOTO      FINCT1
033100970701     C                   END
033200970701     C* Controllo se esiste bolla
033300970701     C                   Z-ADD     GCPAAS        KAAS
033400970701     C                   Z-ADD     GCPLNP        KLNP
033500970701     C                   Z-ADD     GCPNRS        KNRS
033600970701     C                   Z-ADD     GCPNSP        KNSP
033700970701     C     KBLP          CHAIN     FNBLP01L                           31
033800970701     C     *IN31         IFEQ      '1'
033900970701     C                   SETON                                        424344
034000970701     C                   SETON                                        28
034100970701     C                   MOVEL     ERR(22)       $MSG
034200970701     C                   GOTO      FINCT1
034300970701     C                   END
034400970701     C*
034500970701     C                   END
034600970701     C*------------------------------------------------------------------*
034700970701     C* Ristampa per numero SPEDIZIONE
034800970701     C     V1CSC2        IFEQ      '1'
034900970701     C* Controllo se immesso anno
035000970701     C     V1CAGC        IFNE      0
035100970701     C                   SETON                                        454228
035200970701     C                   MOVEL     ERR(6)        $MSG
035300970701     C                   GOTO      FINCT1
035400970701     C                   END
035500970701     C* controllo se immessa filiale apertura
035600970701     C     V1CFGC        IFNE      0
035700970701     C                   SETON                                        454328
035800970701     C                   MOVEL     ERR(8)        $MSG
035900970701     C                   GOTO      FINCT1
036000970701     C                   END
036100970701     C* controllo se immesso numero apertura
036200970701     C     V1CNGC        IFNE      0
036300970701     C                   SETON                                        454428
036400970701     C                   MOVEL     ERR(23)       $MSG
036500970701     C                   GOTO      FINCT1
036600970701     C                   END
036700970701     C* Controllo se immesso anno spedizione
036800970701     C     V1CAAS        IFEQ      0
036900970701     C                   SETON                                        454628
037000970701     C                   MOVEL     ERR(11)       $MSG
037100970701     C                   GOTO      FINCT1
037200970701     C                   END
037300970701     C* controllo se immessa linea di partenza
037400970701     C     V1CLNP        IFEQ      0
037500970701     C                   SETON                                        454728
037600970701     C                   MOVEL     ERR(13)       $MSG
037700970701     C                   GOTO      FINCT1
037800970701     C                   END
037900970701     C* controllo se esiste
038000970701     C                   MOVEL     V1CLNP        ORGFIL
038100970701     C     ORGFIL        CHAIN     AZORG01L                           31
038200970701     C     *IN31         IFEQ      '1'
038300970715     C                   SETON                                        4728
038400970701     C                   MOVEL     ERR(15)       $MSG
038500970701     C                   GOTO      FINCT1
038600970701     C                   END
038700970701     C* controllo se immesso numero spedizione
038800970701     C     V1CNSP        IFEQ      0
038900970701     C                   SETON                                        454928
039000970701     C                   MOVEL     ERR(16)       $MSG
039100970701     C                   GOTO      FINCT1
039200970701     C                   END
039300970701     C* se tutto Ok Anno da 2 a 4 chr
039400970701     C                   MOVEL     V1CAAS        WSECOL            2
039500970701     C     WSECOL        IFEQ      *ZEROS
039600970701     C     V1CAAS        IFGT      40
039700970701     C                   ADD       1900          V1CAAS
039800970701     C                   ELSE
039900970701     C                   ADD       2000          V1CAAS
040000970701     C                   END
040100970701     C                   END
040200970701     C*
040300970701     C* Controllo se esiste giacenza
040400970715     C                   Z-ADD     V1CAAS        KAAN
040500970715     C                   Z-ADD     V1CLNP        KLPN
040600970715     C                   Z-ADD     V1CNRS        KNRN
040700970715     C                   Z-ADD     V1CNSP        KNSN
040800970715     C     KLBL          CHAIN     FNLBL01L                           31
040900970715     C                   Z-ADD     LBLAAO        KAAS
041000970715     C                   Z-ADD     LBLLPO        KLNP
041100970715     C                   Z-ADD     LBLNRO        KNRS
041200970715     C                   Z-ADD     LBLNSO        KNSP
041300970715     C                   Z-ADD     0             KFRG
041400050309     C  N31KGCP1         CHAIN     tigcp01L                           31
041500970701     C     *IN31         IFEQ      '1'
041600970701     C                   SETON                                        464748
041700970701     C                   SETON                                        4928
041800970701     C                   MOVEL     ERR(19)       $MSG
041900970701     C                   GOTO      FINCT1
042000970701     C                   END
042100970701     C* Se filiale di partenza non in £7 do errore
042200970701     C     GCPLNP        LOOKUP    £7                                     31
042300970701     C     *IN31         IFEQ      '0'
042400970701     C                   SETON                                        464748
042500970701     C                   SETON                                        2849
042600970701     C                   MOVEL     ERR(20)       $MSG
042700970701     C                   GOTO      FINCT1
042800970701     C                   END
042900970701     C* Controllo se già eseguita stampa
043000970701     C     GCPFAS        IFLE      50
043100970701     C     GCPDLM        OREQ      0
043200970701     C                   SETON                                        464748
043300970701     C                   SETON                                        2849
043400970701     C                   MOVEL     ERR(21)       $MSG
043500970701     C                   GOTO      FINCT1
043600970701     C                   END
043700970701     C* No reso
043800970701     C     GCPDIS        IFNE      'R'
043900970701     C                   SETON                                        464748
044000970701     C                   SETON                                        2849
044100970701     C                   MOVEL     ERR(24)       $MSG
044200970701     C                   GOTO      FINCT1
044300970701     C                   END
044400970701     C* Controllo se esiste bolla
044500970701     C                   Z-ADD     GCPAAS        KAAS
044600970701     C                   Z-ADD     GCPLNP        KLNP
044700970701     C                   Z-ADD     GCPNRS        KNRS
044800970701     C                   Z-ADD     GCPNSP        KNSP
044900970701     C     KBLP          CHAIN     FNBLP01L                           31
045000970701     C     *IN31         IFEQ      '1'
045100970701     C                   SETON                                        464748
045200970701     C                   SETON                                        2849
045300970701     C                   MOVEL     ERR(22)       $MSG
045400970701     C                   GOTO      FINCT1
045500970701     C                   END
045600970701     C*
045700970701     C                   END
045800970701     C*------------------------------------------------------------------*
045900970701     C     FINCT1        ENDSR
046000970109     C*----------------------------------------------------------*
046100970109     C* Caricamento tabella £7
046200970109     C*----------------------------------------------------------*
046300970109     C     CAR£7         BEGSR
046400970109     C* Caricamento £7
046500970109     C                   MOVEA     *ZEROS        FL£7
046600970703     C                   MOVEL     *BLANKS       KKEY
046700970703     C                   MOVEL     V1CFIL        KKEY
046800970703     C                   MOVEL     '£7'          KCOD
046900970109     C     KTAB          CHAIN     TABEL                              31
047000970109     C     *IN31         IFEQ      '0'
047100970109     C     TBLFLG        ANDEQ     ' '
047200970109     C                   MOVEA     TBLUNI        FL£7
047300970109     C                   ELSE
047400970109     C                   MOVE      V1CFIL        FL£7(1)
047500970109     C                   END
047600970109     C*
047700970109     C                   Z-ADD     0             £7
047800970109     C                   Z-ADD     0             X£7               2 0
047900970109     C                   DO        29            X                 3 0
048000970109     C     FL£7(X)       IFNE      *BLANKS
048100970109     C                   ADD       1             X£7               2 0
048200970109     C                   MOVE      FL£7(X)       £7(X£7)
048300970109     C                   END
048400970109     C                   END
048500970703     C*
048600970703     C                   MOVEL     V1CFIL        WFIL
048700970109     C*
048800970109     C                   ENDSR
048900970701     C*------------------------------------------------------------------*
049000970701     C*  Sottometto elaborazione
049100970701     C*------------------------------------------------------------------*
049200970701     C     SBMP79        BEGSR
049300970701     C*
049400970701     C* Imposto DS
049500970701     C                   CLEAR                   FNLG78
049600970701     C     V1CSC1        IFEQ      '1'
049700970701     C                   MOVEL     '1'           D78SCE
049800970701     C                   ELSE
049900970701     C                   MOVEL     '2'           D78SCE
050000970701     C                   END
050100970701     C                   MOVEL     V1CFIL        D78FIL
050200970701     C                   Z-ADD     V1CAGC        D78AGC
050300970701     C                   Z-ADD     V1CFGC        D78FGC
050400970701     C                   Z-ADD     V1CNGC        D78NGC
050500970715     C                   Z-ADD     GCPAAS        D78AAS
050600970715     C                   Z-ADD     GCPLNP        D78LNP
050700970715     C                   Z-ADD     GCPNRS        D78NRS
050800970715     C                   Z-ADD     GCPNSP        D78NSP
050900970701     C                   MOVEL     FNLG78        KPJBU
051000970701     C                   MOVEL     'LG79'        KCOAZ
051100970701     C                   CALL      'BCH10'
051200970701     C                   PARM                    KPJBA
051300970701     C*
051400970701     C                   ENDSR
051500941018     C*------------------------------------------------------------------*
051600970701     C*  Operazioni iniziali
051700941018     C*------------------------------------------------------------------*
051800941018     C     *INZSR        BEGSR
051900970701     C*---------------------------------------------------------------*
052000970701     C* Richiamo XParut
052100941018     C     *ENTRY        PLIST
052200941018     C                   PARM                    KPJBA
052300941018     C                   Z-ADD     1             CODUT
052400941018     C                   CALL      'X§PARUT'
052500941018     C                   PARM                    UTEDSE
052600941018     C*          CODUT     CABEQ-1        FINE
052700941018     C                   MOVEL     RAGUT         RSUT             20
052800941018     C*---------------------------------------------------------------*
052900970701     C* Definisco chiavi di accesso
053000970109     C     KTAB          KLIST
053100970109     C                   KFLD                    CODUT
053200970109     C                   KFLD                    KCOD
053300970109     C                   KFLD                    KKEY
053400970715     C     KLBL          KLIST
053500970715     C                   KFLD                    KAAN
053600970715     C                   KFLD                    KLPN
053700970715     C                   KFLD                    KNRN
053800970715     C                   KFLD                    KNSN
053900970701     C     KGCP1         KLIST
054000970701     C                   KFLD                    KAAS
054100970701     C                   KFLD                    KLNP
054200970701     C                   KFLD                    KNRS
054300970701     C                   KFLD                    KNSP
054400970701     C                   KFLD                    KFRG
054500970701     C     KGCP2         KLIST
054600970630     C                   KFLD                    KAGC
054700970630     C                   KFLD                    KFGC
054800970630     C                   KFLD                    KNGC
054900970630     C                   KFLD                    KFRG
055000970630     C     KBLP          KLIST
055100970630     C                   KFLD                    KAAS
055200970630     C                   KFLD                    KLNP
055300970630     C                   KFLD                    KNRS
055400970630     C                   KFLD                    KNSP
055500970701     C*---------------------------------------------------------------*
055600970109     C*  Definisco variabili
055700970109     C     *LIKE         DEFINE    TBLCOD        KCOD
055800970109     C     *LIKE         DEFINE    TBLKEY        KKEY
055900970630     C     *LIKE         DEFINE    GCPAGC        KAGC
056000970630     C     *LIKE         DEFINE    GCPFGC        KFGC
056100970630     C     *LIKE         DEFINE    GCPNGC        KNGC
056200970630     C     *LIKE         DEFINE    GCPFRG        KFRG
056300970630     C     *LIKE         DEFINE    BLPAAS        KAAS
056400970630     C     *LIKE         DEFINE    BLPLNP        KLNP
056500970630     C     *LIKE         DEFINE    BLPNRS        KNRS
056600970630     C     *LIKE         DEFINE    BLPNSP        KNSP
056700970715     C     *LIKE         DEFINE    LBLAAN        KAAN
056800970715     C     *LIKE         DEFINE    LBLLPN        KLPN
056900970715     C     *LIKE         DEFINE    LBLNRN        KNRN
057000970715     C     *LIKE         DEFINE    LBLNSN        KNSN
057100970701     C*---------------------------------------------------------------*
057200970701     C*  Imposto variabili
057300970701     C                   MOVEL     REC80         CNCR80
057400970701     C                   MOVEL     REC42         CNCR42
057500970701     C                   MOVEL     *BLANKS       RAGSOC
057600970701     C                   MOVEL     SIMRGS        RAGSOC           30
057700970701     C                   MOVEL     RAGUT         PARDUT           30
057800970701     C*---------------------------------------------------------------*
057900970109     C*  Controllo qual'è la filiale con cui sto lavorando
058000020730     C     SimTpP        COMP      '2'                                0101
058100020730     C                   MOVE      SIMPOU        WFIL              3 0
058200970701     C*---------------------------------------------------------------*
058300970109     C*  Carico £1
058400020503     c                   clear                   trul06ds
058500020503     c                   eval      d06cod = '£1'
058600020503     c                   movel     wfil          d06key
058700020503     c                   movel     trul06ds      kpjbu
058800020503     c                   call      'TRUL06R'
058900020503     c                   parm                    kpjba
059000020503     c                   movel     kpjbu         trul06ds
059100020503     c                   movea     lin           £1
059200970109     C                   Z-ADD     0             Y                 2 0
059300020503     C*                  MOVEL     '£1'          KCOD
059400020503     C*                  MOVEL     WFIL          KKEY
059500020503     C*    KTAB          CHAIN     TABEL                              31
059600020503     C*    *IN31         IFEQ      '0'
059700020503     C*    TBLFLG        ANDEQ     ' '
059800020503     C*                  MOVEA     TBLUNI        FL£1
059900020503     C*                  DO        29            X
060000020503     C*    FL£1(X)       IFNE      *BLANKS
060100020503     C*                  ADD       1             Y
060200020503     C*                  MOVE      FL£1(X)       £1(Y)
060300020503     C*                  END
060400020503     C*                  END
060500020503     C*                  ELSE
060600020503     C*                  MOVE      WFIL          £1(1)
060700020503     C*                  END
060800970109     C*
060900970109     C                   ENDSR
061000970701**
061100970701Punto operativo inesistente                                                   01
061200970701Punto operativo non gestito su questo AS                                      02
061300970701Impostare una scelta                                                          03
061400970701Impossibile selezionare entrambe                                              04
061500970701Anno apertura giacenza obbligatorio                                           05
061600970701Impossibile impostare anno apertura giacenza per ristampa con nr.spedizione   06
061700970701Punto operativo apertura obbligatorio                                         07
061800970701Impossibile immettere P.O. apertura se selezionata ristampa per nr.spedizione 08
061900970701Punto operativo di apertura inesistente                                       09
062000970701Numero giacenza obbligatorio                                                  10
062100970701Anno di spedizione obbligatorio                                               11
062200970701Impossibile impostare anno spedizione se selezionata ristampa per nr.giacenza 12
062300970701Punto operativo partenza obbligatorio                                         13
062400970701Impossibile immettere P.O. partenza se selezionata ristampa per nr.giacenza   14
062500970701Punto operativo di partenza inesistente                                       15
062600970701Numero spedizione obbligatorio                                                16
062700970701Impossibile impostare numero spedizione se selez. ristampa per nr.giacenza    17
062800970701Impossibile impostare numero di serie se selezionata ristampa per nr.giacenza 18
062900970701Giacenza inesistente                                                          19
063000970701Giacenza non gestita in partenza dalla filiale selezionata                    20
063100970701Impossibile eseguire ristampa: giacenza ancora da stampare                    21
063200970701Impossibile eseguire ristampa: non esiste bolla in partenza                   22
063300970701Impossibile immettere numero giacenza se selezionata ristampa per nr.spediz.  23
063400970701Impossibile eseguire ristampa: non sono state immesse disposizioni di reso    24
