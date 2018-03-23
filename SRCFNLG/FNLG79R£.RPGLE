000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200970701     H* FNLG79R *----------------------------------------------------*
000300900418     H*                                                              *
000400970630     H*   - RISTAMPA SINGOLA COMUNICAZIONE DI FINE GIACENCA -        *
000500900418     H*--------------------------------------------------------------*
000600941018     FFNBLP01L  IF   E           K DISK
000700970701     F*--------
000800060213     FFiar401L  IF   E           K DISK
000900970701     F*--------
001000990929     FFIAR601L  IF   E           K DISK
001100970701     F*--------
001200970630     FTABEL00F  IF   E           K DISK
001300970701     F*--------
001400050309     Ftigcp02L  IF   E           K DISK
001500970701     F*--------
001600050309     FtigcP01L  IF   E           K DISK
001700050309     F                                     RENAME(tigcp000:tigcp001)
001800970701     F*--------
001900900503     FAZORG01L  IF   E           K DISK
002000970701     F*--------
002100900503     FCNACO00F  IF   E           K DISK
002200970701     F*--------
002300900503     FCNIND00F  IF   E           K DISK
002400970701     F*--------
002500050408     FTIGAP01L  IF   E           K DISK
002600970701     F*--------
002700970715     FLETGIC    O    F  132        PRINTER USROPN
002800941019     F*--------------------------------------------------------------*
002900911125     D LT              S             70    DIM(21) CTDATA PERRCD(1)
003000970715     D CMD             S             35    DIM(2) CTDATA PERRCD(1)
003100941019     D*--------------------------------------------------------------*
003200900608     D DS2C          E DS
003300020717     D OG143         E DS
003400970630     D TRUL43        E DS                  EXTNAME(TRUL43DS)
003500970701     D FNLG78        E DS                  EXTNAME(FNLG78DS)
003600960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
003700900504     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003800900418     D  TCU                  398    697
003900900418     D                                     DIM(50)
004000900418     D  KCU                  698    847P 0
004100900418     D                                     DIM(50)
004200900418     D                                     PACKEVEN
004300900418     D TCUDS           DS
004400900418     D  F3                     3      3
004500900418     D  F4                     4      4
004600900418     D  F56                    5      6
004700911112     D CNCR80        E DS
004800970630     D CNCR42        E DS
004900900418     D KPJBA         E DS
005000941018     D WLBDA8          DS
005100941018     D  G02DAT                 1      8  0
005200941018     D  G02INV                 9     16  0
005300941018     D  G02ERR                17     17
005400941018     D  G02TGI                18     22  0
005500961001     D*
005600961001     D                 DS
005700961001     D  WGG                    1      2
005800961001     D  WMM                    3      4
005900961001     D  WAA                    5      8
006000961001     D  WDATA                  1      8
006100941018     C*---------------------------------------------------------------*
006200970701     C* Chain su giacenza
006300970701     C                   Z-ADD     0             KFRG
006400970701     C     D78SCE        IFEQ      '1'
006500970701     C                   Z-ADD     D78AGC        KAGC
006600970701     C                   Z-ADD     D78FGC        KFGC
006700970701     C                   Z-ADD     D78NGC        KNGC
006800050309     C     KGCP2         CHAIN     tigcp02L                           1015
006900970701     C                   ELSE
007000970701     C                   Z-ADD     D78AAS        KAAS
007100970701     C                   Z-ADD     D78LNP        KLNP
007200970701     C                   Z-ADD     D78NRS        KNRS
007300970701     C                   Z-ADD     D78NSP        KNSP
007400050309     C     KGCP1         CHAIN     tigcp01L                           1015
007500970701     C                   END
007600970630     C** Aggancio la giacenza e controllo che esista, che sia già stato
007700970630     C** stampato il modulo  (fase > 50 e dlm <> 0)
007800941018     C     *IN15         IFEQ      '0'
007900941018     C     *IN10         ANDEQ     '0'
008000970630     C     GCPFAS        ANDGT     50
008100970630     C     GCPDLM        ANDNE     0
008200970630     C** Aggancio bolla ed eseguo ristampa solo se esiste
008300970630     C     KGAP          CHAIN     FNBLP01L                           25
008400970630     C     *IN25         IFEQ      '0'
008500970630     C                   EXSR      RISTAM
008600970630     C                   END
008700911112     C                   END
008800970715     C*  close
008900970715     C                   CLOSE     LETGIC
009000970715     C*  Eseguo dltovr
009100970715     C                   MOVEA     CMD(2)        CMDP             80
009200970715     C                   Z-ADD     35            LUNCMD           15 5
009300970715     C                   CALL      'QCMDEXC'
009400970715     C                   PARM                    CMDP
009500970715     C                   PARM                    LUNCMD
009600970715     C*
009700900503     C                   SETON                                        LR
009800970630     C*-----------------------------------------------------------*
009900970630     C*  Eseguo ristampa
010000970630     C*-----------------------------------------------------------*
010100970630     C     RISTAM        BEGSR
010200970630     C*
010300970630     C* Reperisco dati cliente mittente
010400970630     C                   EXSR      REPMIT
010500970630     C* Reperisco dati destinatario
010600970630     C                   EXSR      REPDES
010700970630     C* Reperisco dati bolla di addebito
010800970630     C                   EXSR      REPBOL
010900970630     C* Reperisco dati filiale di partenza
011000970630     C                   EXSR      REPLNP
011100970630     C* Ribaltamento date per stampa
011200970630     C                   EXSR      RIBDAT
011300970630     C* Decodifica codice fine giacenza
011400970630     C                   EXSR      CDCHIU
011500970630     C* Decodifica tipo disposizione
011600970630     C                   EXSR      TPDISP
011700970630     C* Calcolo spese
011800970630     C                   EXSR      CALSPE
011900970630     C* Esegue stampa
012000970630     C                   Z-ADD     1             STPPAG            4 0
012100990706     C     GCPDIV        IFEQ      *BLANKS
012200990716     C                   MOVEL     'ITL'         STPDIV            3
012300990706     C                   ELSE
012400990706     C                   MOVEL     GCPDIV        STPDIV
012500990706     C                   END
012600970630     C                   EXCEPT    STAMPA
012700970630     C*
012800970630     C*
012900970630     C                   ENDSR
013000970630     C*-----------------------------------------------------------*
013100970630     C*  Reperrisco dati cliente mittente
013200970630     C*-----------------------------------------------------------*
013300970630     C     REPMIT        BEGSR
013400970630     C*
013500970630     C* Azzero dati mittente
013600970630     C                   MOVEL     *BLANKS       RAG
013700970630     C                   MOVEL     *BLANKS       VIA
013800970630     C                   MOVEL     *BLANKS       CIT
013900970630     C                   MOVEL     *BLANKS       PRV
014000970630     C                   MOVEL     *BLANKS       CAP9
014100970630     C                   MOVEL     *BLANKS       STA
014200970630     C* Controllo se cliente codificato o vario
014300970630     C                   MOVE      GCPSCM        W0040             4 0
014400970630     C                   CLEAR                   TRUL43
014500970630     C*  Se non è un cliente vario controllo se c'è il luogo di
014600970630     C*  invio comunicazione giacenza
014700970630     C     W0040         IFNE      8888
014800970630     C     W0040         ANDNE     9999
014900970630     C                   Z-ADD     GCPSCM        D43KSC
015000970630     C                   CALL      'TRUL43R'
015100970630     C                   PARM                    TRUL43
015200970630     C     D43PER        COMP      *BLANKS                            0404
015300970819     C                   MOVEL     D43PER        ATTPER           27
015400970630     C                   END
015500970630     C*
015600970630     C* Se ho trovato il luogo di comunicazione giacenza invio tutto la
015700970630     C     D43TRV        IFEQ      'S'
015800970630     C                   MOVEL     D43RAG        RAG
015900970630     C                   MOVEL     D43IND        VIA
016000970630     C                   MOVEL     D43LOC        CIT
016100970630     C                   MOVEL     D43PRO        PRV
016200970630     C                   MOVEL     D43CAP        CAP9
016300970630     C                   MOVEL     D43NAZ        STA
016400970630     C** Altrimenti ....
016500970630     C                   ELSE
016600970630     C** Se è codificato prendo i dati anagrafici dal piano dei conti
016700970630     C     W0040         IFNE      8888
016800970630     C     W0040         ANDNE     9999
016900970630     C     KACO          CHAIN     CNACO000                           25
017000970630     C     KACO          CHAIN     CNIND000                           25
017100970630     C     *IN25         IFEQ      '0'
017200970630     C                   MOVEL     *BLANKS       CAP9              9
017300970630     C                   MOVEL     INDCAP        CAP9
017400970630     C     INDCAE        IFNE      *BLANKS
017500970630     C     INDCAE        ANDNE     *ZEROS
017600970630     C                   MOVEL     INDCAE        CAP9
017700970630     C                   END
017800970630     C                   MOVEL     ACORAG        RAG              38
017900970630     C                   MOVEL     INDVIA        VIA              38
018000970630     C                   MOVEL     INDCIT        CIT              21
018100970630     C                   MOVEL     INDPRV        PRV               2
018200970630     C                   MOVEL     INDSTA        STA               3
018300970630     C                   END
018400970630     C*
018500970630     C                   ELSE
018600970630     C** Se è un cliente vario (8888/9999) prendo dati anagrafici
018700970630     C** dall'anagrafica d'aapoggio o dalle bolle se questa non esiste
018800050408     C     KGAP          CHAIN     TIGAP000                           25
018900970630     C     *IN25         IFEQ      '0'
019000970630     C                   MOVEL     GAPRSM        RAG
019100970630     C                   MOVEL     GAPINM        VIA
019200970630     C                   MOVEL     GAPLOM        CIT
019300970630     C                   MOVEL     GAPPRM        PRV
019400970630     C                   MOVEL     GAPNZM        STA
019500970630     C                   MOVEL     GAPCAM        CAP9
019600970630     C                   ELSE
019700970630     C                   MOVEL     BLPRSM        RAG
019800970630     C                   MOVEL     BLPINM        VIA
019900970630     C                   MOVEL     BLPLOM        CIT
020000970630     C                   MOVEL     BLPPRM        PRV
020100970630     C                   MOVEL     BLPNZM        STA
020200970630     C                   MOVEL     BLPCAM        CAP9
020300970630     C                   END
020400970630     C                   END
020500970630     C                   END
020600970630     C*
020700970630     C                   ENDSR
020800970630     C*-----------------------------------------------------------*
020900970630     C*  Reperrisco dati destinatraio
021000970630     C*-----------------------------------------------------------*
021100970630     C     REPDES        BEGSR
021200970630     C*
021300970630     C* Reperisco destinatario originale se c'è
021400970630     C                   CLEAR                   FNLSN0
021500970630     C                   Z-ADD     BLPAAS        DN0AAS
021600970630     C                   Z-ADD     BLPLNP        DN0LNP
021700970630     C                   Z-ADD     BLPNRS        DN0NRS
021800970630     C                   Z-ADD     BLPNSP        DN0NSP
021900970630     C                   MOVEL     *BLANKS       DN0TRC
022000970630     C     GCPDUR        IFEQ      0
022100970630     C                   Z-ADD     GCPMGC        DN0DAG
022200970630     C                   MOVEL     GCPAGC        DN0DAG
022300970630     C                   ELSE
022400970630     C                   Z-ADD     GCPDUR        DN0DAG
022500970630     C                   END
022600970630     C                   MOVEL     KPJBU         WSVKPJ          256
022700970630     C                   MOVEL     FNLSN0        KPJBU
022800970630     C                   CALL      'FNLSN0R'
022900970630     C                   PARM                    KPJBA
023000970630     C                   MOVEL     KPJBU         FNLSN0
023100970630     C                   MOVEL     WSVKPJ        KPJBU
023200970630     C* Destinatario Originale
023300970630     C     DN0TRV        IFEQ      'S'
023400970630     C                   MOVEL     DN0RSD        STPRSD
023500970630     C                   MOVEL     DN0RS2        STPRS2
023600970630     C                   MOVEL     DN0IND        STPIND
023700970630     C                   MOVEL     DN0LOD        STPLOD
023800970630     C                   MOVEL     DN0PRD        STPPRD
023900970630     C                   MOVEL     DN0CAD        STPCAD
024000970630     C                   MOVEL     DN0NZD        STPNZD
024100970630     C                   ELSE
024200970630     C* Reperisco seconda ragione sociale destinatario
024300970630     C                   MOVEL     'D'           KTRC
024400060213     C                   MOVEL     *BLANKS       ar4NOT
024500060213     C     Kar4          CHAIN     Fiar401L                           31
024600970630     C* Ultimo destinatario
024700970630     C                   MOVEL     BLPRSD        STPRSD
024800060213     C                   MOVEL     ar4NOT        STPRS2
024900970630     C                   MOVEL     BLPIND        STPIND
025000970630     C                   MOVEL     BLPLOD        STPLOD
025100970630     C                   MOVEL     BLPPRD        STPPRD
025200970630     C                   MOVEL     BLPCAD        STPCAD
025300970630     C                   MOVEL     BLPNZD        STPNZD
025400970630     C                   END
025500970630     C*
025600970630     C                   ENDSR
025700970630     C*-----------------------------------------------------------*
025800970630     C*  Reperrisco dati bolla di addebito
025900970630     C*-----------------------------------------------------------*
026000970630     C     REPBOL        BEGSR
026100970630     C*
026200970630     C* Aggancio bolla di addebito x stampare l'event. nr. fattura
026300970630     C                   SETOFF                                       41
026400970630     C                   MOVE      *ZEROS        DATDFT
026500970630     C                   MOVE      *ZEROS        §NFT
026600970630     C     GCPNBW        IFGT      *ZEROS
026700990929     C                   MOVEL     '1'           KTRC
026800990929     C     KBAD          CHAIN     FIAR601L                           25
026900990929     C  N25AR6NFT        IFGT      *ZEROS
027000990929     C                   SETON                                        41
027100990929     C                   MOVE      AR6NFT        §NFT
027200970630     C* Data fattura
027300990929     C                   Z-ADD     AR6DFT        G02INV
027400970630     C                   Z-ADD     0             G02DAT
027500970630     C                   MOVEL     '3'           G02ERR
027600970630     C                   CALL      'XSRDA8'
027700970630     C                   PARM                    WLBDA8
027800970630     C                   Z-ADD     G02DAT        DATDFT            8 0
027900970630     C                   END
028000970630     C                   END
028100970630     C*
028200970630     C                   ENDSR
028300970630     C*-----------------------------------------------------------*
028400970630     C*  Reperisco dati filiale di partenza
028500970630     C*-----------------------------------------------------------*
028600970630     C     REPLNP        BEGSR
028700970630     C*
028800970630     C** Decodifico la Filiale di partenza
028900970630     C                   MOVEL     *BLANKS       DESLNP           20
029000970630     C                   MOVEL     *BLANKS       INDLNP           20
029100970630     C                   Z-ADD     0             CAPLNP            5 0
029200970630     C                   MOVEL     *BLANKS       LOCLNP           20
029300970630     C                   MOVEL     *BLANKS       PROLNP            2
029400970630     C                   MOVEL     *BLANKS       FAXLNP           13
029500970630     C                   MOVEL     *BLANKS       TELLNP           13
029600971113     C                   MOVEL     *BLANKS       ORGFL1            1
029700970630     C     GCPLNP        CHAIN     AZORG                              25
029800970630     C     *IN25         IFEQ      '0'
029900020717     C                   MOVEL     ORGDE3        OG143
030000970630     C                   MOVEL     ORGDES        DESLNP
030100970630     C                   MOVEL     ORGIND        INDLNP
030200970630     C                   Z-ADD     ORGCPF        CAPLNP
030300970630     C                   MOVEL     ORGLOC        LOCLNP
030400970630     C                   MOVEL     ORGPRO        PROLNP
030500970630     C                   MOVEL     ORGTEL        TELLNP
030600970630     C                   MOVEL     ORGFAX        FAXLNP
030700970630     C                   END
030800970630     C                   SETON                                        13
030900020717     C*****ORGFL1        IFEQ      'E'
031000020717     C  N25§OgNTW        ifeq      'FED'
031100020717     C     §OgNTW        oreq      'EEX'
031200020717     C     §OgNTW        oreq      'EUP'
031300970630     C                   SETOFF                                       13
031400970630     C                   END
031500970630     C** COSTRUISCO LOCALITA' + DATA
031600970630     C                   CLEAR                   LOCDAT
031700970630     C                   MOVEL     OGGI          WDATA
031800970630     C     ORGDES        CAT       ',':0         LOCDAT           32
031900970630     C     LOCDAT        CAT       WGG:1         LOCDAT
032000970630     C     LOCDAT        CAT       '/':0         LOCDAT
032100970630     C     LOCDAT        CAT       WMM:0         LOCDAT
032200970630     C     LOCDAT        CAT       '/':0         LOCDAT
032300970630     C     LOCDAT        CAT       WAA:0         LOCDAT
032400970630     C*
032500970630     C                   ENDSR
032600970630     C*------------------------------------------------------------*
032700970630     C* RIBALTAMENTO DATE
032800970630     C*------------------------------------------------------------*
032900970630     C     RIBDAT        BEGSR
033000970630     C*
033100970630     C** DATA APERTURA GIACENZA
033200970630     C                   MOVEL     GCPAGC        G02INV
033300970630     C                   MOVE      GCPMGC        G02INV
033400970630     C                   Z-ADD     0             G02DAT
033500970630     C                   MOVEL     '3'           G02ERR
033600970630     C                   CALL      'XSRDA8'
033700970630     C                   PARM                    WLBDA8
033800970630     C                   Z-ADD     G02DAT        DATGIA            8 0
033900970630     C** DATA SPEDIZIONE
034000970630     C                   MOVEL     BLPAAS        G02INV
034100970630     C                   MOVE      BLPMGS        G02INV
034200970630     C                   Z-ADD     0             G02DAT
034300970630     C                   MOVEL     '3'           G02ERR
034400970630     C                   CALL      'XSRDA8'
034500970630     C                   PARM                    WLBDA8
034600970630     C                   Z-ADD     G02DAT        DATSPE            8 0
034700970630     C** DATA CHIUSURA GIACENZA
034800970630     C                   Z-ADD     GCPDCG        G02INV
034900970630     C                   Z-ADD     0             G02DAT
035000970630     C                   MOVEL     '3'           G02ERR
035100970630     C                   CALL      'XSRDA8'
035200970630     C                   PARM                    WLBDA8
035300970630     C                   Z-ADD     G02DAT        DATDCG            8 0
035400970630     C** DATA SOSTA DAL
035500970630     C* SE C'E RIAPERTURA STAMPO DATA APERTURA GIACENZA
035600970630     C     GCPDUR        IFGT      0
035700970630     C                   MOVEL     GCPAGC        G02INV
035800970630     C                   MOVE      GCPMGC        G02INV
035900970630     C                   ELSE
036000970630     C                   Z-ADD     GCPDSD        G02INV
036100970630     C                   END
036200970630     C                   Z-ADD     0             G02DAT
036300970630     C                   MOVEL     '3'           G02ERR
036400970630     C                   CALL      'XSRDA8'
036500970630     C                   PARM                    WLBDA8
036600970630     C                   Z-ADD     G02DAT        DATDSD            8 0
036700970630     C** DATA SOSTA AL
036800970630     C                   Z-ADD     GCPDSA        G02INV
036900970630     C                   Z-ADD     0             G02DAT
037000970630     C                   MOVEL     '3'           G02ERR
037100970630     C                   CALL      'XSRDA8'
037200970630     C                   PARM                    WLBDA8
037300970630     C                   Z-ADD     G02DAT        DATDSA            8 0
037400970630     C** DATA CREAZ.BOLLA GIAC.
037500970630     C                   Z-ADD     GCPDBG        G02INV
037600970630     C                   Z-ADD     0             G02DAT
037700970630     C                   MOVEL     '3'           G02ERR
037800970630     C                   CALL      'XSRDA8'
037900970630     C                   PARM                    WLBDA8
038000970630     C                   Z-ADD     G02DAT        DATDBG            8 0
038100970630     C*
038200970630     C                   ENDSR
038300970630     C*-----------------------------------------------------------*
038400970630     C*  Decodifica codice fine giacenza
038500970630     C*-----------------------------------------------------------*
038600970630     C     CDCHIU        BEGSR
038700970630     C*
038800970630     C                   MOVEL     '2C'          COD
038900970630     C                   MOVEL     *BLANKS       KEY
039000970630     C                   MOVEL     GCPCFG        KEY
039100970630     C     KTAB          CHAIN     TABEL                              25
039200970630     C  N25              MOVEL     TBLUNI        DS2C
039300970630     C*
039400970630     C                   ENDSR
039500970630     C*---------------------------------------------------------------*
039600970630     C* Decodifica tipo disposizioni                                  *
039700970630     C*---------------------------------------------------------------*
039800970630     C     TPDISP        BEGSR
039900970630     C*
040000970630     C     GCPDIS        IFNE      *BLANKS
040100970630     C                   MOVEL     '2D'          COD
040200970630     C                   MOVEL     *BLANKS       KEY
040300970630     C                   MOVEL     GCPDIS        KEY
040400970630     C     KTAB          CHAIN     TABEL                              25
040500970630     C  N25              MOVEL     TBLUNI        RIENTR           20
040600970630     C   25              MOVEL     *BLANKS       RIENTR
040700970630     C                   ELSE
040800970630     C                   MOVEL     *BLANKS       RIENTR
040900970630     C                   END
041000970630     C* Per rientro stampo diversa lettera
041100970630     C                   SETOFF                                       42
041200970630     C     GCPDIS        IFEQ      'R'
041300970630     C                   SETON                                        42
041400970630     C                   END
041500970630     C*
041600970630     C                   ENDSR
041700970630     C*---------------------------------------------------------------*
041800970630     C* Calcolo totale generale spese                                 *
041900970630     C*---------------------------------------------------------------*
042000970630     C     CALSPE        BEGSR
042100970630     C*
042200970630     C     GCPTFT        IFNE      9
042300970630     C     GCPPSG        ANDNE     'D'
042400970630     C                   SETOFF                                       40
042500990628     C                   Z-ADD     0             TOTGEN           13 3
042600970630     C                   ADD       GCPSGS        TOTGEN
042700970630     C                   ADD       GCPSGR        TOTGEN
042800970630     C                   ADD       GCPSGD        TOTGEN
042900970630     C                   ADD       GCPSGA        TOTGEN
043000970630     C                   ELSE
043100970630     C                   SETON                                        40
043200970630     C                   END
043300970630     C*
043400970630     C                   ENDSR
043500900504     C*---------------------------------------------------------------*
043600941018     C*   OPERAZIONI INIZIALI                                         *
043700900504     C*---------------------------------------------------------------*
043800941018     C     *INZSR        BEGSR
043900970630     C*
044000900504     C     *ENTRY        PLIST
044100900504     C                   PARM                    KPJBA
044200900504     C                   Z-ADD     1             CODUT
044300900504     C                   CALL      'X§PARUT'
044400900504     C                   PARM                    UTEDSE
044500911112     C*          CODUT     CABEQ-1        FINE
044600900504     C                   MOVEL     RAGUT         RSUT             20
044700900504     C                   MOVEL     RAGUT         RSUT2            25
044800900504     C*--- RICERCA CAPOCONTI------------------------------------------*
044900900504     C                   DO        50            X                 2 0
045000900504     C                   MOVE      TCU(X)        TCUDS
045100941018     C     F56           IFEQ      'CG'
045200941018     C     F4            ANDEQ     '1'
045300941018     C     F3            ANDEQ     '0'
045400941018     C                   Z-ADD     KCU(X)        KCI               4 0
045500941018     C                   END
045600900504     C                   END
045700900504     C*-                                                   *
045800970630     C                   MOVEL     REC42         CNCR42
045900900504     C                   MOVEL     *BLANKS       DITRGS
046000900504     C                   MOVEL     SIMRGS        DITRGS           20
046100911112     C                   MOVEL     REC80         CNCR80
046200930215     C* GIRO DATA DEL GIORNO
046300941018     C                   CLEAR                   WLBDA8
046400941018     C                   TIME                    WHHDAT           14 0
046500941019     C                   MOVE      WHHDAT        OGGI              8 0
046600941018     C                   MOVE      WHHDAT        G02DAT
046700941018     C                   CALL      'XSRDA8'
046800941018     C                   PARM                    WLBDA8
046900941018     C                   Z-ADD     G02INV        DATEU             8 0
047000900504     C*
047100970701     C                   MOVEL     KPJBU         FNLG78
047200941018     C*---------------------------------------------------------------*
047300941018     C     KACO          KLIST
047400941018     C                   KFLD                    CODUT
047500941018     C                   KFLD                    KCI
047600941018     C                   KFLD                    GCPSCM
047700970630     C*
047800970701     C     KGCP2         KLIST
047900970630     C                   KFLD                    KAGC
048000970630     C                   KFLD                    KFGC
048100970630     C                   KFLD                    KNGC
048200970701     C                   KFLD                    KFRG
048300970701     C     KGCP1         KLIST
048400970701     C                   KFLD                    KAAS
048500970701     C                   KFLD                    KLNP
048600970701     C                   KFLD                    KNRS
048700970701     C                   KFLD                    KNSP
048800970630     C                   KFLD                    KFRG
048900941019     C*
049000970630     C     KBLP          KLIST
049100970630     C                   KFLD                    KAAS
049200970630     C                   KFLD                    KLNP
049300970630     C                   KFLD                    KNRS
049400970630     C                   KFLD                    KNSP
049500970630     C*
049600060213     C     Kar4          KLIST
049700941019     C                   KFLD                    KAAS
049800941019     C                   KFLD                    KLNP
049900941019     C                   KFLD                    KNRS
050000941019     C                   KFLD                    KNSP
050100941019     C                   KFLD                    KTRC
050200941019     C*
050300941018     C     KGAP          KLIST
050400941018     C                   KFLD                    GCPAAS
050500941018     C                   KFLD                    GCPLNP
050600941018     C                   KFLD                    GCPNRS
050700941018     C                   KFLD                    GCPNSP
050800941018     C     KBAD          KLIST
050900941018     C                   KFLD                    GCPASW
051000941018     C                   KFLD                    GCPLPW
051100941018     C                   KFLD                    GCPNSW
051200941018     C                   KFLD                    GCPNBW
051300990929     C                   KFLD                    KTRC
051400941018     C     KTAB          KLIST
051500941018     C                   KFLD                    CODUT
051600941018     C                   KFLD                    COD               2
051700941018     C                   KFLD                    KEY               8
051800941018     C*---------------------------------------------------------------*
051900941018     C*  LIKE DEFN
052000960123     C     *LIKE         DEFINE    BLPRSD        STPRSD
052100060213     C     *LIKE         DEFINE    ar4NOT        STPRS2
052200960123     C     *LIKE         DEFINE    BLPIND        STPIND
052300960123     C     *LIKE         DEFINE    BLPLOD        STPLOD
052400960123     C     *LIKE         DEFINE    BLPPRD        STPPRD
052500960123     C     *LIKE         DEFINE    BLPCAD        STPCAD
052600960123     C     *LIKE         DEFINE    BLPNZD        STPNZD
052700941019     C     *LIKE         DEFINE    BLPAAS        KAAS
052800941019     C     *LIKE         DEFINE    BLPLNP        KLNP
052900941019     C     *LIKE         DEFINE    BLPNRS        KNRS
053000941019     C     *LIKE         DEFINE    BLPNSP        KNSP
053100060213     C     *LIKE         DEFINE    ar4TRC        KTRC
053200970630     C     *LIKE         DEFINE    GCPFRG        KFRG
053300990929     C     *LIKE         DEFINE    AR6NFT        §NFT
053400970630     C     *LIKE         DEFINE    GCPAGC        KAGC
053500970630     C     *LIKE         DEFINE    GCPFGC        KFGC
053600970630     C     *LIKE         DEFINE    GCPNGC        KNGC
053700970630     C*----------_----------------------------------------------------*
053800941018     C                   Z-ADD     66            CONRIG            3 0
053900970715     C*
054000970715     C*  Eseguo ovrprtf + open letgic
054100970715     C                   MOVEA     CMD(1)        CMDP             80
054200970715     C                   Z-ADD     35            LUNCMD           15 5
054300970715     C                   CALL      'QCMDEXC'
054400970715     C                   PARM                    CMDP
054500970715     C                   PARM                    LUNCMD
054600970715     C                   OPEN      LETGIC
054700970630     C*
054800900504     C                   ENDSR
054900941019     C***------------------------------------------------***
055000970630     OLETGIC    E            STAMPA           10
055100961114     O                                         +  6 'Punto Operativo di'
055200961114     O                                           75 'PAG.:'
055300961114     O                       STPPAG        Z     80
055400970630     O          E            STAMPA           11
055500961114     O                       DESLNP            +  6
055600961114     O                                           53 'Spettabile'
055700970630     O                                            3 'R'
055800970630     O          E            STAMPA           12
055900961114     O               13      INDLNP            +  6
056000970630     O          E            STAMPA           13
056100961114     O               13      CAPLNP            +  6
056200961114     O               13      LOCLNP            +  1
056300961114     O               13      PROLNP            +  1
056400961114     O                       RAG                 81
056500970630     O          E            STAMPA           14
056600961114     O                                         +  6 'FAX'
056700961114     O                       FAXLNP            +  1
056800961114     O                       VIA                 81
056900961114     O          E            STAMPA           15
057000970630     O                                         +  6 'TEL'
057100970630     O                       TELLNP            +  1
057200960618     O                       CAP9                52
057300970701     O                       CIT                 74
057400960618     O                       PRV                 77
057500960618     O                       STA                 81
057600961001     O          E            STAMPA           16
057700961001     O                       LOCDAT              38
057800960618     O               04                          53 'Att.ne di '
057900970819     O               04      ATTPER              81
058000961001     O          E            STAMPA           20
058100900627     O                                           14 'OGGETTO:'
058200900627     O                                           26 'Giacenza n.'
058300950125     O                       GCPFGC        Z     30
058400950125     O                       GCPNGC        Z     38
058500950125     O                                           42 'del'
058600950125     O                       DATGIA              53 '  /  /    '
058700961001     O          E            STAMPA           22
058800900627     O                                           19 'Spedizione n.'
058900950125     O                       GCPLNP        Z     23
059000950125     O                       GCPNRS        Z     26
059100950125     O                       GCPNSP        Z     34
059200950125     O                                           38 'del'
059300950125     O                       DATSPE              49 '  /  /    '
059400950125     O                                           60 'Vs. rifer.'
059500950125     O                       BLPRMN        Z     69
059600961001     O          E            STAMPA           24
059700900627     O                                           18 'Destinatario'
059800960123     O                       STPRSD              54
059900961001     O          E            STAMPA           25
060000960123     O                       STPRS2              54
060100961001     O          E            STAMPA           26
060200960123     O                       STPIND              54
060300961001     O          E            STAMPA           27
060400960123     O                       STPCAD              28
060500960123     O                       STPLOD              64
060600960123     O                       STPPRD              67
060700960123     O                       STPNZD              71
060800961001     O          E            STAMPA           30
060900900627     O                       LT(1)             +  5
061000961001     O          E            STAMPA           31
061100900627     O                       LT(2)             +  5
061200960618     O                       DATDCG              24 '  /  /    '
061300960618     O                       RIENTR              49
061400961001     O          E            STAMPA           33
061500900627     O                       §2CDES              66
061600961001     O          E         N40STAMPA           35
061700900627     O                       LT(3)             +  5
061800961001     O          E         N40STAMPA           36
061900900627     O                       LT(4)             +  5
062000961001     O          E         N40STAMPA           38
062100900627     O                       LT(5)             +  5
062200941019     O                       DATDSD              27 '  /  /    '
062300941019     O                       DATDSA              41 '  /  /    '
062400941019     O                       GCPGGS        Z     49
062500990706     O                       STPDIV              57
062600941019     O                       GCPSGS        2     75
062700961001     O          E         N40STAMPA           40
062800900627     O                       LT(6)             +  5
062900990706     O                       STPDIV              57
063000900627     O                       GCPSGR        2     75
063100961001     O          E         N40STAMPA           42
063200900627     O                       LT(7)             +  5
063300990706     O                       STPDIV              57
063400900627     O                       GCPSGD        2     75
063500961001     O          E         N40STAMPA           46
063600911203     O                       LT(17)            +  5
063700990706     O                       STPDIV              57
063800911125     O                       GCPSGA        2     75
063900961001     O          E         N40STAMPA           47
064000911125     O                       LT(9)             +  5
064100961001     O          E         N40STAMPA           49
064200900627     O                       LT(10)            +  5
064300990706     O                       STPDIV              57
064400900627     O                       TOTGEN        2     75
064500961001     O          E         N40STAMPA           51
064600900627     O                       LT(11)            +  5
064700040123     O          E         N40STAMPA           53
064800900627     O                       LT(12)            +  5
064900040123     O          E    41   N40STAMPA           54
065000901220     O                       LT(16)            +  5
065100040123     O                       GCPlpW        Z     15
065200040123     O                       GCPNSW        Z     18
065300040123     O                       GCPNBW        Z     26
065400040123     O                       DATDBG              41 '  /  /    '
065500040123     O                       §NFT          Z     61
065600040123     O                       DATDFT              75 '  /  /    '
065700040123     O          E   n41   N40STAMPA           54
065800900627     O                       LT(13)            +  5
065900900515     O*MANCA L'ANNO
066000040123     O                       GCPlpW        Z     15
066100040123     O                       GCPNSW        Z     18
066200040123     O                       GCPNBW        Z     26
066300040123     O                       DATDBG              41 '  /  /    '
066400961001     O          E         N40STAMPA           56
066500900627     O                       LT(14)            +  5
066600961001     O          E       42 40STAMPA           37
066700920120     O                       LT(14)            +  5
066800961001     O          E      N42 40STAMPA           37
066900900627     O                       LT(15)            +  5
067000900524     O          E            STAMPA      2
067100961001     O                       DITRGS              63
067200900524     O          E            STAMPA      1
067300961001     O                                           61 'Punto Operativo di'
067400961001     O                       ORGDES              82
067500970715**   LT
067600960619      Vi comunichiamo che la giacenza di cui all'oggetto si e' risolta     1
067700960618 in data            per                                                    2
067800960618      Vi anticipiamo con la presente il dettaglio degli oneri relativi     3
067900960619 a quanto sopra, come da accordi gia' esistenti.                           4
068000990628 SOSTA: dal            al            GG                                    5
068100990628 SPESE DI RICONSEGNA E/O VARIE . . . . . . . .                             6
068200990628 SPESE DOSSIER (Diritto Fisso + Spese Postali)                             7
068300990628 ANTICIPATA. . . . . . . . . . . . . . . . . .                             8
068400920525                                                      ----------------     9
068500990628 PER UN TOTALE DI. . . . . . . . . . . . . . .                             10
068600920525                                                      ----------------     11
068700040123 che Vi addebiteremo, oltre alle eventuali addizionali concordate, con     12
068800040123 sped. xxx/xx/xxxxxxx del            sulla prossima fattura.               13
068900920525 Distinti saluti.                                                          14
069000920525 Nulla addebitandoVi, con l'occasione porgiamo distinti saluti.            15
069100040123 sped. xxx/xx/xxxxxxx del            fattura nr.         del  /  /         16
069200990628 SPESE AGGIUNTIVE  . . . . . . . . . . . . . .                             17
069300970715**   CMD
069400970715OVRPRTF  FILE(LETGIC) COPIES(2)
069500970715DLTOVR   FILE(LETGIC)
