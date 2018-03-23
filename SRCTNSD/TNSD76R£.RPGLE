000100991223     h*--------------------------------------------------------------------------------------------*
000200991223     h* GESTISCE TUTTE LE FUNZIONI SUL CALENDARIO                                                  *
000300991223     h*--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500991223     f*--------------------------------------------------------------------------------------------*
000600991223     f* Data base
000700991223     f*--------------------------------------------------------------------------------------------*
000800941110     FTNSD76D   CF   E             WORKSTN INFDS(DEVDS1)
000900941110     F                                     SFILE(SD76DMS:MSGULT)
001000941110     FAZCLN01L  UF A E           K DISK
001100941110     FAZCAS01L  IF   E           K DISK
001200930202     FTABEL00F  IF   E           K DISK
001300930202     FAZORG01L  IF   E           K DISK
001400991223     FANBCA01L  IF   E           K DISK
001500991223     f*--------------------------------------------------------------------------------------------*
001600991223     f* Data structure
001700991223     f*--------------------------------------------------------------------------------------------*
001800000000     D* MESSAGGI
001900930208     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000000000     D* COMANDI
002100000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002200000000     D* DESCRIZIONI TASTI FUNZIONALI
002300000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002500000000     D FUV             S             19    DIM(24)
002600930401     D* DS PER RIDEFINIRE SCHIERA GIORNI FESTIVI
002700930401     D GFEDS           DS
002800930401     D  GFE                    1     31
002900930401     D                                     DIM(31)
003000930401     D* DS PER RIDEFINIRE SCHIERA GIORNI SETTIMANA
003100930401     D GSEDS           DS
003200930401     D  GSE                    1     31
003300930401     D                                     DIM(31)
003400930401     D* DS PER RIDEFINIRE SCHIERA FLAG MATTINA
003500930401     D GMADS           DS
003600930401     D  GMA                    1     31
003700930401     D                                     DIM(31)
003800930401     D* DS PER RIDEFINIRE SCHIERA FLAG POMERIGGIO
003900930401     D GPODS           DS
004000930401     D  GPO                    1     31
004100930401     D                                     DIM(31)
004200000000     D* RIDEFINIZIONE SCHIERA -FUD-
004300000000     D FUDDS           DS
004400000000     D*                -DESCRIZIONE PARZIALE
004500000000     D  FUDDE1                 1      1
004600000000     D*                -FUNZIONE
004700000000     D  FUDFUN                 2      3
004800000000     D*                -DESCRIZIONE PARZIALE
004900000000     D  FUDDE2                 4     19
005000000000     D*                -FILLER
005100000000     D  FUDFI0                20     34
005200000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005300000000     D  FUDATT                35     35
005400000000     D*                -FILLER
005500000000     D  FUDFI1                36     80
005600000000     D*                -DESCRIZIONE COMPLETA
005700000000     D  FUDDES                 1     19
005800000000     D*                -RIDEFINIZIONE RIGA
005900000000     D  FUDRIG                 1     80
006000941110     D*----------------
006100941110     D* RIDEFINIZIONE DATA INVERTITA PER SCOMPOSIZIONE (8) -ROVESCIATA-
006200941110     D*----------------
006300930401     D                 DS
006400930401     D  WGG                    1      2  0
006500930401     D  WMM                    3      4  0
006600930401     D  WAA                    5      6  0
006700930401     D  WDATA                  1      6  0
006800941110     D*----------------
006900941110     D* RIDEFINIZIONE DATA PER SCOMPOSIZIONE (8) -ROVESCIATA-
007000941110     D*----------------
007100941110     D                 DS
007200941110     D  WAA8                   1      4  0
007300941110     D  WMM8                   5      6  0
007400941110     D  WGG8                   7      8  0
007500941110     D  WDAT8                  1      8  0
007600971113     D* DS ESTERNA TABELLA 1Y - SIMBOLI CALENDARIO
007700930330     D DS1Y          E DS
007800930201     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
007900930201     D DSMSG         E DS
008000930201     D* DS PER GESTIONE TASTI FUNZIONALI
008100930201     D DSKEY         E DS
008200930201     D DEVDS1          DS
008300930201     D  KEY                  369    369
008400930201     D* ARCHITETTURA
008500930201     D KPJBA         E DS
008600991223     d*---
008700991223     d* ds controlla data (8)
008800991223     d*---
008900991223     d wlbda8          DS                  INZ                                  *controlla data
009000991223     d  g08dat                        8  0
009100991223     d  g08inv                        8  0
009200991223     d  g08err                        1
009300991223     d  g08tgi                        5  0
009400991223     d*---
009500991223     d* variabili
009600991223     d*---
009700991223     d n14             S             14  0 INZ                                  *numerico 14
009800991223     d n8              S              8  0 INZ                                  *numerico 8
009900991223     d n4              S              4  0 INZ                                  *numerico 4
010000991223     d datcor          S              8  0 INZ                                  *data correrte a/m/g
010100991223     d*---
010200991223     d* Variabili riferite al data base
010300991223     d*---
010400991223     d KCACALEND       S                   LIKE(BCACALEND)                      *ANBCA01L
010500991223     d KCAANNO         S                   LIKE(BCAANNO)
010600991223     d KCAMESE         S                   LIKE(BCAMESE)
010700991223     f*--------------------------------------------------------------------------------------------*
010800991223     f* Main lines
010900991223     f*--------------------------------------------------------------------------------------------*
011000000000     C                   EXSR      RUTINZ
011100000000     C*
011200930225     C* SE CONTROLLI INIZIALI NON OK, RITORNO A PGM CHIAMANTE
011300930225     C     ERRL00        IFEQ      '1'                                          --- 1 -->
011400930225     C*
011500930225     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
011600930312     C                   MOVEL     '1'           DSCERR
011700930312     C                   MOVEL     DSMSMS        DSCMSG
011800930225     C*
011900930225     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
012000930225     C                   ELSE                                                   <-- 1 -->
012100000000     C*
012200000000     C* CICLO FINO A CHE:
012300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012500930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
012600000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
012700000000     C     F12L00        OREQ      '1'
012800930202     C     FLFINE        OREQ      '1'
012900000000     C*
013000000000     C* EMETTE FORMATO VIDEO
013100000000     C                   EXSR      BUTFMR
013200000000     C*
013300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
013400000000     C                   EXSR      FUNCON
013500000000     C*
013600930225     C                   END                                                    <-- 2 ---
013700000000     C*
013800930225     C                   END                                                    <-- 1 ---
013900000000     C*
014000000000     C                   SETON                                        LR
014100000000     C*FINE MAIN DEL PGM
014200000000     C*================================================================
014300000000     C* EMISSIONE FORMATO A VIDEO
014400000000     C*================================================================
014500000000     C     BUTFMR        BEGSR
014600930204     C*
014700930204     C* WRITE RECORD TESTATA
014800941110     C                   WRITE     SD76D01
014900930205     C*
015000930205     C* VISUALIZZAZIONE MESSAGGIO
015100930205     C                   EXSR      SNDMSG
015200930204     C*
015300930204     C* WRITE RECORD FUNZIONI
015400941110     C                   WRITE     SD76D02
015500000000     C*
015600930204     C* EXFMT RECORD DETTAGLIO
015700941110     C                   EXFMT     SD76D03
015800000000     C*
015900000000     C* AZZERO FLAG DI ERRORE LIVELLO
016000000000     C                   MOVEL     '0'           ERRL00
016100000000     C*
016200000000     C* AZZERO FLAG DI ERRORE
016300930330     C                   SETOFF                                       606162
016400930401     C                   SETOFF                                       636465
016500000000     C*
016600000000     C                   ENDSR
016700000000     C*================================================================
016800000000     C* SNDMSG - MANDA MESSAGGIO
016900000000     C*================================================================
017000000000     C     SNDMSG        BEGSR
017100000000     C*
017200000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
017300000000     C                   MOVEL     '1'           *IN40
017400941110     C                   WRITE     SD76DMC
017500000000     C                   MOVEL     '0'           *IN40
017600000000     C*
017700000000     C* VISUALIZZAZIONE MESSAGGIO
017800000000     C                   Z-ADD     0             MSGULT            4 0
017900930202     C                   CALL      'XMSG'        XMSGPA
018000930201     C     DSMSER        IFEQ      0
018100930201     C                   MOVEL     DSMSKE        MSGKEY
018200000000     C                   END
018300000000     C                   Z-ADD     1             MSGULT
018400941110     C                   WRITE     SD76DMS                                      SFL MESS
018500941110     C                   WRITE     SD76DMC                                      CONTR MESS
018600000000     C*
018700000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
018800930201     C                   MOVEL     *BLANKS       DSMSMS
018900000000     C*
019000000000     C                   ENDSR
019100000000     C*================================================================
019200000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
019300000000     C*================================================================
019400000000     C     ENTGES        BEGSR
019500930201     C*
019600930201     C* CONTROLLA I CAMPI IMMESSI
019700930201     C                   EXSR      DECODI
019800930202     C*
019900930202     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
020000930330     C     DSCOPZ        IFEQ      'O05'
020100930202     C                   MOVEL     '1'           FLFINE
020200930202     C                   END
020300000000     C*
020400000000     C                   ENDSR
020500000000     C*================================================================
020600930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
020700000000     C*================================================================
020800000000     C     F01GES        BEGSR
020900000000     C                   ENDSR
021000000000     C*================================================================
021100930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
021200000000     C*================================================================
021300000000     C     F03GES        BEGSR
021400000000     C*
021500930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
021600000000     C                   MOVEL     '1'           F03L00
021700930202     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
021800930312     C                   MOVEL     '1'           DSCF03
021900000000     C*
022000000000     C                   ENDSR
022100930201     C*================================================================
022200930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
022300930201     C*================================================================
022400930201     C     F06GES        BEGSR
022500930201     C*
022600930201     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
022700930330     C     DSCOPZ        IFEQ      'F06'
022800930201     C                   EXSR      INSERI
022900930201     C                   END
023000930330     C     DSCOPZ        IFEQ      'O02'
023100930202     C                   EXSR      MODIFI
023200930201     C                   END
023300930330     C     DSCOPZ        IFEQ      'O04'
023400930201     C                   EXSR      CANCEL
023500930201     C                   END
023600930330     C     DSCOPZ        IFEQ      'O07'
023700930201     C                   EXSR      RIPRIS
023800930201     C                   END
023900930201     C*
024000930201     C                   ENDSR
024100000000     C*================================================================
024200930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
024300000000     C*================================================================
024400000000     C     F07GES        BEGSR
024500000000     C                   ENDSR
024600000000     C*================================================================
024700930330     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
024800000000     C*================================================================
024900000000     C     F08GES        BEGSR
025000000000     C                   ENDSR
025100000000     C*================================================================
025200930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
025300000000     C*================================================================
025400000000     C     F12GES        BEGSR
025500000000     C*
025600000000     C* IMPOSTO FLAG DI LIVELLO
025700000000     C                   MOVEL     '1'           F12L00
025800000000     C*
025900000000     C                   ENDSR
026000930201     C*================================================================
026100930201     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA CAPPARIO
026200930201     C*================================================================
026300930201     C     F21GES        BEGSR
026400930201     C                   ENDSR
026500000000     C*================================================================
026600930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
026700000000     C*================================================================
026800000000     C     F22GES        BEGSR
026900000000     C*
027000000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
027100000000     C                   Z-ADD     80            LENGH            15 5
027200000000     C                   CALL      'QCAEXEC'
027300000000     C                   PARM                    CMD(1)
027400000000     C                   PARM                    LENGH
027500000000     C*
027600000000     C                   ENDSR
027700000000     C*================================================================
027800930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
027900000000     C*================================================================
028000000000     C     F24GES        BEGSR
028100000000     C*
028200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
028300000000     C                   EXSR      FUNGES
028400000000     C*
028500000000     C                   ENDSR
028600000000     C*================================================================
028700000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
028800000000     C*================================================================
028900000000     C     HELGES        BEGSR
029000000000     C                   ENDSR
029100930201     C*================================================================
029200930201     C* INSERI - FUNZIONE DI INSERIMENTO
029300930201     C*================================================================
029400930201     C     INSERI        BEGSR
029500930201     C*
029600930201     C* CONTROLLA I CAMPI IMMESSI
029700930201     C                   EXSR      DECODI
029800930201     C*
029900930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
030000930201     C*
030100930330     C* IMPOSTO IL RECORD DI E SCRIVO
030200930330     C                   EXSR      VIDREC
030300941110     C                   WRITE     AZCLN000
030400930405     C* AGGIORNO RECORD MESE
030500930405     C                   EXSR      INS000
030600930201     C*
030700930201     C                   END                                                    <-- 1 ---
030800930201     C*
030900930201     C* AZZERA TUTTI I CAMPI A VIDEO PER NUOVO INSERIMENTO
031000930201     C                   EXSR      AZZVID
031100930202     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
031200930202     C                   MOVEL     MSG(3)        DSMSMS
031300930201     C*
031400930201     C                   ENDSR
031500930201     C*================================================================
031600930201     C* MODIFI - FUNZIONE DI MODIFICA
031700930201     C*================================================================
031800930201     C     MODIFI        BEGSR
031900930201     C*
032000930201     C* CONTROLLA I CAMPI IMMESSI
032100930201     C                   EXSR      DECODI
032200930201     C*
032300930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
032400930201     C*
032500930330     C* IMPOSTO IL RECORD E RISCRIVO
032600930401     C                   EXSR      VIDREC
032700941110     C                   UPDATE    AZCLN000
032800930405     C* AGGIORNO RECORD MESE
032900930405     C                   EXSR      INS000
033000930201     C*
033100930201     C                   END                                                    <-- 1 ---
033200930201     C*
033300930201     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
033400930201     C                   MOVEL     '1'           FLFINE
033500930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI MODIF. EFFETTUATA
033600930312     C                   MOVEL     MSG(4)        DSCMSG
033700930201     C*
033800930201     C                   ENDSR
033900930201     C*================================================================
034000930201     C* CANCEL - FUNZIONE DI CANCELLAZIONE
034100930201     C*================================================================
034200930201     C     CANCEL        BEGSR
034300930201     C*
034400930330     C* ANNULLO RECORD E RISCRIVO
034500941110     C                   MOVEL     *BLANKS       CLNFTR
034600941110     C                   Z-ADD     *ZEROS        CLNDTR
034700941110     C                   MOVEL     'A'           CLNATB
034800941110     C                   UPDATE    AZCLN000
034900930405     C* AGGIORNO RECORD MESE
035000930405     C                   EXSR      INS000
035100930201     C*
035200930201     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
035300930201     C                   MOVEL     '1'           FLFINE
035400930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI CANC. EFFETTUATA
035500930312     C                   MOVEL     MSG(5)        DSCMSG
035600930201     C*
035700930201     C                   ENDSR
035800930201     C*================================================================
035900930201     C* RIPRIS - FUNZIONE DI RIPRISTINO
036000930201     C*================================================================
036100930201     C     RIPRIS        BEGSR
036200930202     C*
036300930330     C* RIPRISTINO IL RECORD
036400941110     C                   MOVEL     *BLANKS       CLNFTR
036500941110     C                   Z-ADD     *ZEROS        CLNDTR
036600941110     C                   MOVEL     ' '           CLNATB
036700941110     C                   UPDATE    AZCLN000
036800930405     C* AGGIORNO RECORD MESE
036900930405     C                   EXSR      INS000
037000930202     C*
037100930202     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
037200930202     C                   MOVEL     '1'           FLFINE
037300930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI RIPR. EFFETTUATO
037400930312     C                   MOVEL     MSG(6)        DSCMSG
037500930202     C*
037600930201     C                   ENDSR
037700930405     C*================================================================
037800930405     C* INS000 - GESTIONE RECORD MESE (GG.DELLA SETTIMANA E FESTIVI)
037900930405     C*================================================================
038000930405     C     INS000        BEGSR
038100930405     C*
038200930405     C* CONTROLLA SE IL RECORD ESISTE GIA' OPPURE NO
038300941110     C                   Z-ADD     *ZEROS        KCLTFP
038400941110     C                   Z-ADD     *ZEROS        KCLTFA
038500941110     C                   Z-ADD     VIDANN        KCLANN
038600941110     C                   Z-ADD     VIDMES        KCLMES
038700941110     C     KEYCLN        CHAIN     AZCLN01L                           99
038800941110     C                   Z-ADD     *ZEROS        CLNTFP
038900941110     C                   Z-ADD     *ZEROS        CLNTFA
039000941110     C                   Z-ADD     VIDANN        CLNANN
039100941110     C                   Z-ADD     VIDMES        CLNMES
039200941110     C                   MOVEL     WGSE00        CLNMAT
039300941110     C                   MOVEL     GFEDS         CLNPOM
039400941110     C                   MOVEL     *BLANKS       CLNFTR
039500941110     C                   Z-ADD     *ZEROS        CLNDTR
039600941110     C                   MOVEL     *BLANKS       CLNATB
039700930405     C     *IN99         IFEQ      '1'
039800941110     C                   WRITE     AZCLN000
039900930405     C                   ELSE
040000941110     C                   UPDATE    AZCLN000
040100930405     C                   END
040200930405     C*
040300930405     C                   ENDSR
040400000000     C*================================================================
040500930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
040600000000     C*================================================================
040700000000     C     DECODI        BEGSR
040800000000     C*
040900930330     C* CONTROLLO TERMINAL/FILIALE PARTENZA
041000991224if  1C     VIDPAR        IFNE      *ZEROS
041100930330     C                   MOVE      VIDPAR        WLIN              3
041200930201     C                   EXSR      CHKLNA
041300991224if  2C     ERRL00        IFEQ      '1'
041400930330     C                   SETON                                        60
041500991224x   2C                   ELSE
041600930330     C                   MOVEL     WLIN          VIDPAR
041700930330     C                   MOVEL     WDLIN         VIDDPA
041800991224e   2C                   ENDIF
041900991224e   1C                   ENDIF
042000930217     C*
042100930330     C* CONTROLLO TERMINAL/FILIALE ARRIVO
042200991224if  1C     ERRL00        IFEQ      '0'
042300991224if  2C     VIDARR        IFNE      *ZEROS
042400930330     C                   MOVE      VIDARR        WLIN
042500930217     C                   EXSR      CHKLNA
042600991224if  3C     ERRL00        IFEQ      '1'
042700930330     C                   SETON                                        61
042800991224x   3C                   ELSE
042900930330     C                   MOVEL     WLIN          VIDARR
043000930330     C                   MOVEL     WDLIN         VIDDAR
043100991224e   3C                   ENDIF
043200991224e   2C                   ENDIF
043300991224e   1C                   ENDIF
043400930401     C*
043500941110     C* LA COMBINAZIONE PARTENZA/ARRIVO DEVE ESISTERE NEL FILE TRAZIONI
043600991224if  1C     ERRL00        IFEQ      '0'
043700991224if  2C     VIDPAR        IFNE      *ZEROS
043800941110     C     VIDARR        ANDEQ     *ZEROS
043900941110     C                   MOVEL     'P'           KCSPTA                         P A R T E N Z A
044000941110     C                   MOVEL     VIDPAR        KCSLNP
044100941110     C                   MOVEL     VIDARR        KCSLNA
044200941110     C                   MOVEL     *BLANKS       KCSTSP
044300941110     C                   Z-ADD     *ZEROS        KCSDDE
044400991224e   2C                   ENDIF
044500991224if  2C     VIDARR        IFNE      *ZEROS                                       A R R I V O
044600941110     C     VIDPAR        ANDEQ     *ZEROS
044700941110     C                   MOVEL     'A'           KCSPTA
044800941110     C                   MOVEL     VIDARR        KCSLNP
044900941110     C                   MOVEL     VIDPAR        KCSLNA
045000941110     C                   MOVEL     *BLANKS       KCSTSP
045100941110     C                   Z-ADD     *ZEROS        KCSDDE
045200991224e   2C                   ENDIF
045300991224if  2C     VIDPAR        IFNE      *ZEROS
045400941110     C     VIDARR        ANDNE     *ZEROS
045500941110     C                   MOVEL     'T'           KCSPTA                         T R A Z I O N E
045600941110     C                   MOVEL     VIDPAR        KCSLNP
045700941110     C                   MOVEL     VIDARR        KCSLNA
045800941110     C                   MOVEL     *BLANKS       KCSTSP
045900941110     C                   Z-ADD     *ZEROS        KCSDDE
046000991224e   2C                   ENDIF
046100941110     C     KEYCAS        SETLL     AZCAS01L                           99        *FINE FILE
046200941110     C  N99              READ      AZCAS01L                               99
046300991224if  2C     *IN99         IFNE      *OFF
046400941110     C     CASLNP        ANDNE     KCSLNP
046500941110     C     CASLNA        ANDNE     KCSLNA
046600941110     C                   MOVEL     MSG(11)       DSMSMS
046700941110     C                   MOVEL     '1'           ERRL00
046800941110     C                   SETON                                        6061
046900991224e   2C                   ENDIF
047000991224e   1C                   ENDIF
047100991224     C*
047200991223     C* CONTROLLO ANNO
047300991224if  1C     ERRL00        IFEQ      '0'
047400991223     C                   TIME                    n14                            *Ora (6)+ Data (8)
047500991223     C                   MOVE      n14           n8                             *Data (8) in g/m/a
047600991223     C                   Z-ADD     n8            g08dat
047700991223     C                   Z-ADD     *zeros        g08inv
047800991223     C                   MOVEL     '0'           g08err
047900991223     C                   CALL      'XSRDA8'
048000991223     C                   PARM                    wlbda8
048100991223     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
048200991223     C                   MOVEL     datcor        n4                             *anno
048300991224if  2c                   IF        vidann > *zeros AND
048400991223     c                             vidann >= 1992  AND                          *compreso fra 1992 e
048500991223     c                             vidann <= (n4+1)                             *anno prossimo
048600991224x   2c                   ELSE
048700941110     C                   MOVEL     MSG(12)       DSMSMS
048800941110     C                   MOVEL     '1'           ERRL00
048900941110     C                   SETON                                        62
049000991224e   2c                   ENDIF
049100991224e   1C                   ENDIF
049200941110     C*
049300930330     C* CONTROLLO MESE
049400991224if  1C     ERRL00        IFEQ      '0'
049500991224if  2C     VIDMES        IFLT      1
049600930330     C     VIDMES        ORGT      12
049700930401     C                   MOVEL     MSG(13)       DSMSMS
049800930330     C                   MOVEL     '1'           ERRL00
049900930330     C                   SETON                                        63
050000991224e   2C                   ENDIF
050100991224e   1C                   ENDIF
050200930518     C*
050300930518     C* SE INSERIMENTO CONTROLLA ANCHE CHE IL RECORD NON ESISTA GIA'
050400991224if  1C     ERRL00        IFEQ      '0'
050500991224if  2C     DSCOPZ        IFEQ      'F06'
050600941110     C                   MOVE      VIDPAR        KCLTFP
050700941110     C                   MOVE      VIDARR        KCLTFA
050800941110     C                   Z-ADD     VIDANN        KCLANN
050900941110     C                   Z-ADD     VIDMES        KCLMES
051000971203     C     KEYCLN        CHAIN(N)  AZCLN01L                           99
051100991224if  3C     *IN99         IFEQ      '0'
051200930518     C                   MOVEL     '1'           ERRL00
051300930518     C                   MOVEL     MSG(2)        DSMSMS
051400930518     C                   SETON                                        606162
051500930518     C                   SETON                                        63
051600991224e   3C                   ENDIF
051700991224e   2C                   ENDIF
051800991224e   1C                   ENDIF
051900930401     C*
052000930401     C* IMPOSTO GIORNI SETTIMANA E FESTIVI
052100991224if  1C     ERRL00        IFEQ      '0'
052200930401     C                   EXSR      IMPGIO
052300930401     C*
052400930401     C* CONTROLLO FLAG MATTINA
052500930401     C                   MOVEL     VIDMAT        GMADS
052600991224do  2C                   DO        31            X                 6 0
052700991224if  3C     GFE(X)        IFEQ      '*'
052800930401     C                   MOVEL     '*'           GMA(X)
052900991224x   3C                   ELSE
053000991224if  4C     GMA(X)        IFEQ      *BLANKS
053100991224if  5C     GFE(X)        IFEQ      'F'
053200930401     C                   MOVEL     '1'           ERRL00
053300930401     C                   MOVEL     MSG(14)       DSMSMS
053400930401     C                   SETON                                        64
053500991224e   5C                   ENDIF
053600991224x   4C                   ELSE
053700930401     C                   MOVEL     GMA(X)        WSIM              1
053800930401     C                   EXSR      CHKSIM
053900991224if  5C     ERRL00        IFEQ      '1'
054000930401     C                   SETON                                        64
054100991224x   5C                   ELSE
054200930401     C                   MOVEL     WSIM          GMA(X)
054300991224e   5C                   ENDIF
054400991224e   4C                   ENDIF
054500991224e   3C                   ENDIF
054600991224e   2C                   ENDDO
054700930401     C                   MOVEL     GMADS         VIDMAT
054800930401     C*
054900930401     C* CONTROLLO FLAG POMERIGGIO
055000991224if  2C     ERRL00        IFEQ      '0'
055100930401     C                   MOVEL     VIDPOM        GPODS
055200991224do  3C                   DO        31            X                 6 0
055300991224if  4C     GFE(X)        IFEQ      '*'
055400930401     C                   MOVEL     '*'           GPO(X)
055500991224x   4C                   ELSE
055600991224if  5C     GPO(X)        IFEQ      *BLANKS
055700991224if  6C     GFE(X)        IFEQ      'F'
055800930401     C                   MOVEL     '1'           ERRL00
055900930401     C                   MOVEL     MSG(14)       DSMSMS
056000930401     C                   SETON                                        65
056100991224e   6C                   ENDIF
056200991224x   5C                   ELSE
056300930401     C                   MOVEL     GPO(X)        WSIM              1
056400930401     C                   EXSR      CHKSIM
056500991224if  6C     ERRL00        IFEQ      '1'
056600930401     C                   SETON                                        65
056700991224x   6C                   ELSE
056800930401     C                   MOVEL     WSIM          GPO(X)
056900991224e   6C                   ENDIF
057000991224e   5C                   ENDIF
057100991224e   4C                   ENDIF
057200991224e   3C                   ENDDO
057300930401     C                   MOVEL     GPODS         VIDPOM
057400991224e   2C                   ENDIF
057500991224e   1C                   ENDIF
057600000000     C*
057700000000     C                   ENDSR
057800930201     C*================================================================
057900930201     C*  CHKLNA - CONTROLLA LA LINEA
058000930201     C*================================================================
058100930201     C     CHKLNA        BEGSR
058200930201     C*
058300930201     C* CONTROLLO SE E' STATO INSERITO '?'
058400930217     C                   MOVE      WLIN          WLNA1             1
058500930217     C                   MOVEL     WLIN          WLNA2             1
058600930201     C*
058700930201     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
058800930202     C     WLNA1         IFEQ      '?'                                          --- 1 -->
058900930202     C     WLNA2         OREQ      '?'
059000930201     C                   MOVEL     *BLANKS       §COD1             3
059100930201     C                   MOVE      *BLANKS       §DES1
059200971203     C                   MOVE      *BLANKS       §PRA
059300971203     C                   CALL      'TNSD24R'
059400930201     C                   PARM                    §COD1
059500930201     C                   PARM                    §TIP              1
059600930201     C                   PARM                    §DES1            25
059700971203     C                   PARM                    §PRA              3
059800930217     C                   MOVEL     §COD1         WLIN
059900930217     C                   MOVEL     §DES1         WDLIN            20
060000930201     C*
060100930201     C                   ELSE                                                   <-- 1 -->
060200930201     C*
060300930201     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
060400930217     C                   MOVE      WLIN          KORFIL
060500930202     C     KEYORG        CHAIN     AZORG                              99
060600930201     C     *IN99         IFEQ      '0'                                          --- 2 -->
060700930217     C                   MOVEL     ORGDES        WDLIN
060800930201     C                   ELSE                                                   <-- 2 -->
060900930201     C                   MOVEL     '1'           ERRL00
061000930202     C                   MOVEL     MSG(7)        DSMSMS
061100930217     C                   MOVEL     *ALL'*'       WDLIN
061200930201     C                   END                                                    <-- 2 ---
061300930201     C*
061400930201     C                   END                                                    <-- 1 ---
061500930201     C*
061600930201     C                   ENDSR
061700930201     C*================================================================
061800930330     C*  CHKSIM - CONTROLLA SIMBOLI CALENDARIO
061900930201     C*================================================================
062000930330     C     CHKSIM        BEGSR
062100930201     C*
062200930201     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
062300930401     C     WSIM          IFEQ      '?'                                          --- 1 -->
062400930201     C*
062500930201     C                   MOVEL     CODUT         §KUT              1 0
062600930330     C                   MOVEL     '1Y'          §COD              2
062700930201     C                   MOVE      *BLANKS       §DES1
062800930201     C                   CALL      'X§TABER'
062900930201     C                   PARM                    §KUT
063000930201     C                   PARM                    §COD
063100930201     C                   PARM                    §KEY              8
063200930201     C                   PARM                    §DES             30
063300930330     C                   MOVEL     §KEY          WSIM
063400930330     C                   MOVEL     §DES          DS1Y                           *NELLA DS
063500930201     C*
063600930201     C                   ELSE                                                   <-- 1 -->
063700930201     C*
063800930201     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
063900930202     C                   MOVEL     CODUT         KTBUTE
064000930330     C                   MOVE      '1Y'          KTBCOD
064100930202     C                   MOVE      *BLANKS       KTBKEY
064200930330     C                   MOVEL     WSIM          KTBKEY
064300930202     C     KEYTAB        CHAIN     TABEL                              99
064400930201     C     *IN99         IFEQ      '1'                                          --- 2 -->
064500930201     C     TBLFLG        OREQ      '*'
064600930201     C                   MOVEL     '1'           ERRL00
064700930202     C                   MOVEL     MSG(9)        DSMSMS
064800930205     C                   ELSE
064900930330     C                   MOVEL     TBLUNI        DS1Y                           *NELLA DS
065000930201     C                   END                                                    <-- 2 ---
065100930201     C*
065200930201     C                   END                                                    <-- 1 ---
065300930201     C*
065400930201     C                   ENDSR
065500930201     C*================================================================
065600930330     C*  RECVID - PORTA I CAMPI DAL RECORD AL VIDEO
065700930201     C*================================================================
065800930330     C     RECVID        BEGSR
065900930201     C*
066000941110     C                   MOVE      CLNTFP        VIDPAR
066100941110     C                   MOVE      CLNTFA        VIDARR
066200941110     C                   Z-ADD     CLNANN        VIDANN
066300941110     C                   Z-ADD     CLNMES        VIDMES
066400930202     C*
066500941110     C                   MOVEL     CLNMAT        VIDMAT
066600941110     C                   MOVEL     CLNPOM        VIDPOM
066700930202     C*
066800930401     C* LANCIO ROUTINE CHE IMPOSTA I GIORNI DELLA SETTIMANA E FESTIVI
066900930401     C                   EXSR      IMPGIO
067000930202     C*
067100941110     C     CLNATB        IFEQ      'A'
067200930202     C                   MOVEL     'ANNULLAT'    VIDATB
067300930202     C                   MOVE      'O'           VIDATB
067400930202     C                   ELSE
067500930202     C                   MOVEL     *BLANKS       VIDATB
067600930202     C                   END
067700930201     C*
067800930201     C                   ENDSR
067900930201     C*================================================================
068000930201     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
068100930201     C*================================================================
068200930201     C     AZZVID        BEGSR
068300930201     C*
068400930401     C                   MOVE      *ZEROS        VIDPAR
068500930401     C                   MOVE      *ZEROS        VIDARR
068600930401     C                   Z-ADD     *ZEROS        VIDANN
068700930401     C                   Z-ADD     *ZEROS        VIDMES
068800930401     C*
068900930401     C                   MOVEL     *BLANKS       VIDGIO
069000930401     C                   MOVEL     *BLANKS       VIDFES
069100930401     C                   MOVEL     *BLANKS       VIDMAT
069200930401     C                   MOVEL     *BLANKS       VIDPOM
069300930201     C*
069400930201     C                   ENDSR
069500930201     C*================================================================
069600930401     C*  VIDREC - IMPOSTA CAMPI DA VIDEO A RECORD
069700930201     C*================================================================
069800930401     C     VIDREC        BEGSR
069900930201     C*
070000941110     C                   MOVE      VIDPAR        CLNTFP
070100941110     C                   MOVE      VIDARR        CLNTFA
070200941110     C                   Z-ADD     VIDANN        CLNANN
070300941110     C                   Z-ADD     VIDMES        CLNMES
070400930401     C*
070500941110     C                   MOVEL     VIDMAT        CLNMAT
070600941110     C                   MOVEL     VIDPOM        CLNPOM
070700930401     C*
070800941110     C                   MOVEL     *BLANKS       CLNFTR
070900941110     C                   Z-ADD     *ZEROS        CLNDTR
071000941110     C                   MOVEL     *BLANKS       CLNATB
071100930201     C*
071200930201     C                   ENDSR
071300930401     C*================================================================
071400930401     C*  IMPGIO - IMPOSTA GIORNI DELLA SETTIMANA E FESTIVI
071500930401     C*================================================================
071600930401     C     IMPGIO        BEGSR
071700930401     C*
071800930401     C* LANCIO ROUTINE PER IMPOSTARE GIORNI FESTIVI
071900930401     C                   EXSR      IMPGFE
072000930401     C*
072100930401     C* LANCIO ROUTINE PER IMPOSTARE GIORNI SETTIMANA
072200930401     C                   EXSR      IMPGSE
072300930401     C*
072400930401     C* IMPOSTO ASTERISCHI NEI GIORNI NON VALIDI
072500930401     C                   DO        31            X
072600930401     C     GFE(X)        IFEQ      '*'
072700930401     C                   MOVEL     '*'           GMA(X)
072800930401     C                   MOVEL     '*'           GPO(X)
072900930401     C                   ELSE
073000930401     C                   MOVEL     *BLANKS       GMA(X)
073100930401     C                   MOVEL     *BLANKS       GPO(X)
073200930401     C                   END
073300930401     C                   END
073400930401     C*
073500930401     C                   ENDSR
073600930201     C*================================================================
073700930401     C*  IMPGFE - IMPOSTA GIORNI FESTIVI
073800930201     C*================================================================
073900930401     C     IMPGFE        BEGSR
074000930201     C*
074100930401     C* LEGGE SUL FILE BNBCA00F IL RECORD DEL MESE
074200991223     C                   MOVEL     'CB'          KCACALEND
074300991223     C                   Z-ADD     VIDANN        KCAANNO
074400991223     C                   Z-ADD     VIDMES        KCAMESE
074500991223     C     KEYBCA        CHAIN     ANBCA01L                           99
074600930401     C     *IN99         IFEQ      '1'                                          --- 1 -->
074700930401     C                   MOVEL     '1'           ERRL00
074800930401     C                   MOVEL     MSG(8)        DSMSMS
074900930401     C                   ELSE                                                   <-- 1 -->
075000930401     C                   MOVEL     BCAGPF        GFEDS
075100930401     C                   DO        31            X                 6 0          --- 2 -->
075200930401     C     GFE(X)        IFEQ      *BLANKS                                      --- 3 -->
075300930401     C                   MOVEL     '*'           GFE(X)
075400930401     C                   ELSE                                                   <-- 3 -->
075500930401     C     GFE(X)        IFEQ      *ZEROS
075600930401     C                   MOVEL     ' '           GFE(X)
075700930401     C                   ELSE
075800930401     C                   MOVEL     'F'           GFE(X)
075900930401     C                   END
076000930401     C                   END                                                    <-- 3 ---
076100930401     C                   END                                                    <-- 2 ---
076200930401     C                   END                                                    <-- 1 ---
076300930401     C                   MOVEL     GFEDS         VIDFES
076400930201     C*
076500930201     C                   ENDSR
076600930401     C*================================================================
076700930401     C*  IMPGSE - IMPOSTA GIORNI DELLA SETTIMANA
076800930401     C*================================================================
076900930401     C     IMPGSE        BEGSR
077000930401     C*
077100930401     C* LANCIO COMANDO RTVDAT PER AVERE IL GIORNO DELLA SETTIMANA
077200941110     C                   MOVE      VIDANN        WAA                            *LUNGO 2
077300930401     C                   Z-ADD     VIDMES        WMM
077400930401     C                   DO        31            X                 6 0          --- 1 -->
077500930401     C     GFE(X)        IFEQ      '*'                                          --- 2 -->
077600930401     C                   MOVEL     '*'           GSE(X)
077700930401     C                   ELSE                                                   <-- 2 -->
077800930401     C                   Z-ADD     X             WGG
077900930401     C                   MOVE      WDATA         C98DAT
078000941110     C                   CALL      'XGIOSET'
078100930401     C                   PARM                    C98DAT            6
078200930401     C                   PARM                    C98GSE            1
078300930401     C                   MOVE      C98GSE        W98GSE            1 0
078400930401     C     W98GSE        IFEQ      1
078500930401     C                   Z-ADD     7             W98GSE
078600930401     C                   ELSE
078700930401     C                   SUB       1             W98GSE
078800930401     C                   END
078900930401     C                   MOVE      W98GSE        GSE(X)
079000930401     C                   END                                                    <-- 2 ---
079100930401     C                   END                                                    <-- 1 ---
079200930405     C                   MOVEL     GSEDS         WGSE00           31
079300930401     C     '1':'L'       XLATE     GSEDS         GSEDS
079400930401     C     '2':'M'       XLATE     GSEDS         GSEDS
079500930401     C     '3':'M'       XLATE     GSEDS         GSEDS
079600930401     C     '4':'G'       XLATE     GSEDS         GSEDS
079700930401     C     '5':'V'       XLATE     GSEDS         GSEDS
079800930401     C     '6':'S'       XLATE     GSEDS         GSEDS
079900930401     C     '7':'D'       XLATE     GSEDS         GSEDS
080000930401     C                   MOVEL     GSEDS         VIDGIO
080100930401     C*
080200930401     C                   ENDSR
080300000000     C*================================================================
080400000000     C*  RUTINZ - ROUTINE INIZIALE
080500000000     C*================================================================
080600000000     C     RUTINZ        BEGSR
080700930201     C*
080800930201     C* RICEVIMENTO PARAMETRI
080900930201     C     *ENTRY        PLIST
081000930401     C                   PARM                    DSCF03            1            PREMUTO F03
081100930401     C                   PARM                    DSCERR            1            '1'=ERRORE
081200930401     C                   PARM                    DSCMSG           80            MESSAGGIO
081300930401     C                   PARM                    DSCOPZ            3            OPZIONE LIV.PGM
081400930401     C                   PARM                    DSCPAR            3 0          PARTENZA
081500930401     C                   PARM                    DSCARR            3 0          ARRIVO
081600941110     C                   PARM                    DSCANN            4 0          ANNO
081700930401     C                   PARM                    DSCMES            2 0          MESE
081800930201     C*
081900930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
082000930201     C     XMSGPA        PLIST
082100930201     C                   PARM                    DSMSMS
082200930201     C                   PARM                    DSMSPG
082300930201     C                   PARM                    DSMSKE
082400930201     C                   PARM                    DSMSER
082500930401     C*
082600941110     C* CHIAVE LETTURA AZCLN01L - COMPLETA
082700941110     C     KEYCLN        KLIST
082800941110     C                   KFLD                    KCLTFP
082900941110     C                   KFLD                    KCLTFA
083000941110     C                   KFLD                    KCLANN
083100941110     C                   KFLD                    KCLMES
083200930201     C*
083300930201     C* CHIAVE LETTURA AZORG01L - COMPLETA
083400930201     C     KEYORG        KLIST
083500930201     C                   KFLD                    KORFIL
083600930201     C*
083700930201     C* CHIAVE LETTURA TABEL00F - COMPLETA
083800930201     C     KEYTAB        KLIST
083900930201     C                   KFLD                    KTBUTE
084000930201     C                   KFLD                    KTBCOD
084100930201     C                   KFLD                    KTBKEY
084200930401     C*
084300941110     C* CHIAVE LETTURA AZCAS01L - COMPLETA
084400941110     C     KEYCAS        KLIST
084500941110     C                   KFLD                    KCSPTA
084600941110     C                   KFLD                    KCSLNP
084700941110     C                   KFLD                    KCSLNA
084800941110     C                   KFLD                    KCSTSP
084900941110     C                   KFLD                    KCSDDE
085000930401     C*
085100991223     C* CHIAVE LETTURA ANBCA01L - COMPLETA
085200991223     C     KEYBCA        KLIST
085300991223     C                   KFLD                    KCACALEND
085400991223     C                   KFLD                    KCAANNO
085500991223     C                   KFLD                    KCAMESE
085600930201     C*
085700930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
085800930201     C                   CALL      'XKEY'
085900930201     C                   PARM                    DSKEY
086000930201     C*
086100930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
086200930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
086300930201     C     *LIKE         DEFINE    ORGFIL        KORFIL
086400941110     C     *LIKE         DEFINE    CLNTFP        KCLTFP
086500941110     C     *LIKE         DEFINE    CLNTFA        KCLTFA
086600941110     C     *LIKE         DEFINE    CLNANN        KCLANN
086700941110     C     *LIKE         DEFINE    CLNMES        KCLMES
086800930201     C     *LIKE         DEFINE    TBLKUT        KTBUTE
086900930201     C     *LIKE         DEFINE    TBLCOD        KTBCOD
087000930201     C     *LIKE         DEFINE    TBLKEY        KTBKEY
087100941110     C     *LIKE         DEFINE    CASPTA        KCSPTA
087200941110     C     *LIKE         DEFINE    CASLNP        KCSLNP
087300941110     C     *LIKE         DEFINE    CASLNA        KCSLNA
087400941110     C     *LIKE         DEFINE    CASTSP        KCSTSP
087500941110     C     *LIKE         DEFINE    CASDDE        KCSDDE
087600930201     C*
087700930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
087800930201     C*   -NOME PGM CHIAMANTE
087900930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
088000941110     C                   MOVEL     'TNSD76R'     DSMSPG
088100941110     C                   MOVEL     'TNSD76R'     MSGQUE
088200930201     C                   MOVEL     *BLANKS       MSGKEY
088300930201     C                   MOVEL     *BLANKS       DSMSMS
088400930202     C*
088500930202     C* SE VISUALIZZAZIONE TOLGO FUNZIONE AGGIORNAMENTO
088600930401     C     DSCOPZ        IFEQ      'O05'
088700930202     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
088800930202     C                   MOVEL     'N'           FUDATT
088900930202     C                   MOVEL     FUDRIG        FUD(6)
089000930202     C                   END
089100930201     C*
089200930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
089300930201     C                   Z-ADD     0             FUNGRU            5 0
089400930201     C*
089500930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
089600930201     C                   EXSR      FUNGES
089700930201     C*
089800930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
089900930201     C                   MOVEL     '0'           F12L00            1
090000930201     C                   MOVEL     '0'           F03L00            1
090100930201     C                   MOVEL     '0'           FINL00            1
090200930201     C                   MOVEL     '0'           ERRL00            1
090300930201     C                   MOVEL     '0'           FLFINE            1
090400020906     C*
090500020906     C                   Z-ADD     1             CODUT             1 0
090600930201     C*
090700930201     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
090800930201     C* 0=NON PROTETTO, 1=PROTETTO
090900930401     C*   90 - CHIAVE CALENDARIO
091000930401     C*   91 - PARAMETRI CALENDARIO
091100930201     C*
091200930401     C     DSCOPZ        IFEQ      'F06'                                        *INS.
091300930401     C                   SETOFF                                       9091
091400930201     C                   END
091500930201     C*
091600930401     C     DSCOPZ        IFEQ      'O02'                                        *MOD.
091700930201     C                   SETON                                        90
091800930401     C                   SETOFF                                       91
091900930401     C                   END
092000930201     C*
092100930401     C     DSCOPZ        IFEQ      'O05'                                        *VIS+CANC+RIPR
092200930401     C     DSCOPZ        OREQ      'O04'
092300930401     C     DSCOPZ        OREQ      'O07'
092400930401     C                   SETON                                        9091
092500930201     C                   END
092600930201     C*
092700930401     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO DATI RECORD
092800930401     C     DSCOPZ        IFEQ      'O02'                                        --- 1 -->
092900930401     C     DSCOPZ        OREQ      'O04'
093000930401     C     DSCOPZ        OREQ      'O05'
093100930401     C     DSCOPZ        OREQ      'O07'
093200930401     C* SE IL CALENDARIO NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
093300941110     C                   Z-ADD     DSCPAR        KCLTFP
093400941110     C                   Z-ADD     DSCARR        KCLTFA
093500941110     C                   Z-ADD     DSCANN        KCLANN
093600941110     C                   Z-ADD     DSCMES        KCLMES
093700941110     C     KEYCLN        CHAIN     AZCLN01L                           99
093800930201     C     *IN99         IFEQ      '1'
093900930201     C                   MOVEL     '1'           ERRL00
094000930225     C                   MOVEL     MSG(10)       DSMSMS
094100930202     C                   ELSE
094200930401     C                   EXSR      RECVID
094300930201     C                   END
094400930201     C*
094500930201     C     ERRL00        IFEQ      '0'                                          --- 2 -->
094600930208     C*
094700930208     C* SE TUTTO OK, DECODIFICO I CAMPI A VIDEO
094800930208     C                   EXSR      DECODI
094900930201     C*
095000930201     C                   END                                                    <-- 2 ---
095100930202     C                   END                                                    <-- 1 ---
095200930202     C*
095300930202     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
095400930401     C     DSCOPZ        IFEQ      'F06'
095500930202     C                   EXSR      AZZVID
095600930202     C                   END
095700930201     C*
095800930201     C* IMPOSTO DESCRIZIONE DELLA FUNZIONE IN CORSO A VIDEO
095900930401     C     DSCOPZ        IFEQ      'F06'
096000930201     C                   MOVEL     ' INSERIM'    VIDFUN
096100930201     C                   MOVE      'ENTO   '     VIDFUN
096200930201     C                   END
096300930401     C     DSCOPZ        IFEQ      'O02'
096400930201     C                   MOVEL     '   MODIF'    VIDFUN
096500930201     C                   MOVE      'ICA    '     VIDFUN
096600930201     C                   END
096700930401     C     DSCOPZ        IFEQ      'O04'
096800930201     C                   MOVEL     ' CANCELL'    VIDFUN
096900930201     C                   MOVE      'AZIONE '     VIDFUN
097000930201     C                   END
097100930401     C     DSCOPZ        IFEQ      'O05'
097200930201     C                   MOVEL     'VISUALIZ'    VIDFUN
097300930201     C                   MOVE      'ZAZIONE'     VIDFUN
097400930201     C                   END
097500930401     C     DSCOPZ        IFEQ      'O07'
097600930201     C                   MOVEL     ' RIPRIST'    VIDFUN
097700930201     C                   MOVE      'INO    '     VIDFUN
097800930201     C                   END
097900000000     C*
098000000000     C                   ENDSR
098100930401     C*================================================================
098200930401     C*
098300930401     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098400930401     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098500930401     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
098600930401     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
098700930401     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
098800930401     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098900930401     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099000930401     C*
099100000000     C*================================================================
099200000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
099300000000     C*================================================================
099400000000     C     FUNGES        BEGSR
099500000000     C*
099600000000     C* PULISCO CAMPI DI LAVORO
099700000000     C                   Z-ADD     0             W                 5 0
099800000000     C*
099900000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
100000000000     C* E CARICO TABELLA FUNZIONI ATTIVE
100100000000     C     1             DO        24            I
100200000000     C                   MOVEL     FUD(I)        FUDRIG
100300000000     C*
100400000000     C* PULIZIA FUNZIONI PRECEDENTI
100500000000     C                   MOVEL     *BLANKS       FUV(I)
100600000000     C*
100700000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
100800000000     C* (SALTA F24)
100900000000     C     FUDATT        IFEQ      'S'
101000000000     C     FUDFUN        ANDNE     '24'
101100000000     C                   ADD       1             W
101200000000     C                   MOVEL     FUDDES        FUV(W)
101300000000     C                   END
101400000000     C                   END                                                    DO WHILE
101500000000     C*
101600000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
101700000000     C* ALTRIMENTI DISATTIVO F24
101800000000     C     1             DO        24            I
101900000000     C                   MOVEL     FUD(I)        FUDRIG
102000000000     C*
102100000000     C     FUDFUN        IFEQ      '24'
102200000000     C     W             IFGT      6
102300000000     C                   MOVEL     'S'           FUDATT
102400000000     C                   ELSE
102500000000     C                   MOVEL     *BLANKS       FUDATT
102600000000     C                   END                                                    W GT 8
102700000000     C                   MOVEL     FUDRIG        FUD(I)
102800000000     C                   END
102900000000     C                   END                                                    DO WHILE
103000000000     C*
103100000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
103200000000     C     W             DIV       6             I
103300000000     C                   MVR                     Z
103400000000     C*
103500000000     C     Z             IFNE      0
103600000000     C     I             ADD       1             Z
103700000000     C                   ELSE
103800000000     C                   Z-ADD     I             Z                 5 0
103900000000     C                   END
104000000000     C*
104100000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
104200000000     C                   ADD       1             FUNGRU
104300000000     C     FUNGRU        IFGT      Z
104400000000     C     FUNGRU        ORLT      1
104500000000     C                   Z-ADD     1             FUNGRU
104600000000     C                   END
104700000000     C*
104800000000     C* PRIMO GRUPPO DI 6 FUNZIONI
104900000000     C     FUNGRU        IFEQ      1
105000000000     C                   MOVEL     FUV(1)        FUN001
105100000000     C                   MOVEL     FUV(2)        FUN002
105200000000     C                   MOVEL     FUV(3)        FUN003
105300000000     C                   MOVEL     FUV(4)        FUN004
105400000000     C                   MOVEL     FUV(5)        FUN005
105500000000     C                   MOVEL     FUV(6)        FUN006
105600000000     C                   END
105700000000     C*
105800000000     C* SECONDO GRUPPO DI 6 FUNZIONI
105900000000     C     FUNGRU        IFEQ      2
106000000000     C                   MOVEL     FUV(7)        FUN001
106100000000     C                   MOVEL     FUV(8)        FUN002
106200000000     C                   MOVEL     FUV(9)        FUN003
106300000000     C                   MOVEL     FUV(10)       FUN004
106400000000     C                   MOVEL     FUV(11)       FUN005
106500000000     C                   MOVEL     FUV(12)       FUN006
106600000000     C                   END
106700000000     C*
106800000000     C* TERZO GRUPPO DI 6 FUNZIONI
106900000000     C     FUNGRU        IFEQ      3
107000000000     C                   MOVEL     FUV(13)       FUN001
107100000000     C                   MOVEL     FUV(14)       FUN002
107200000000     C                   MOVEL     FUV(15)       FUN003
107300000000     C                   MOVEL     FUV(16)       FUN004
107400000000     C                   MOVEL     FUV(17)       FUN005
107500000000     C                   MOVEL     FUV(18)       FUN006
107600000000     C                   END
107700000000     C*
107800000000     C* QUARTO GRUPPO DI 6 FUNZIONI
107900000000     C     FUNGRU        IFEQ      4
108000000000     C                   MOVEL     FUV(19)       FUN001
108100000000     C                   MOVEL     FUV(20)       FUN002
108200000000     C                   MOVEL     FUV(21)       FUN003
108300000000     C                   MOVEL     FUV(22)       FUN004
108400000000     C                   MOVEL     *BLANKS       FUN005
108500000000     C                   MOVEL     *BLANKS       FUN006
108600000000     C                   END
108700000000     C*
108800000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
108900000000     C                   MOVEL     *BLANKS       FUN007
109000000000     C                   MOVEL     *BLANKS       FUN008
109100000000     C*
109200000000     C     1             DO        24            I
109300000000     C                   MOVEL     FUD(I)        FUDRIG
109400000000     C     FUDFUN        IFEQ      '24'
109500000000     C     FUDATT        ANDEQ     'S'
109600000000     C                   MOVEL     FUDDES        FUN008
109700000000     C                   END
109800000000     C                   END                                                    DO
109900000000     C                   ENDSR
110000000000     C*================================================================
110100000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
110200000000     C*================================================================
110300000000     C     FUNCON        BEGSR
110400000000     C*
110500000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
110600000000     C                   EXSR      FUNVAR
110700000000     C*
110800000000     C* PULISCO CAMPI DI LAVORO
110900000000     C                   Z-ADD     0             I                 5 0
111000000000     C                   Z-ADD     0             W                 5 0
111100000000     C*
111200000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
111300000000     C     FUNKEY        IFNE      *BLANKS
111400000000     C     1             DO        24            I
111500000000     C                   MOVEL     FUD(I)        FUDRIG
111600000000     C*
111700000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
111800000000     C     FUNKEY        IFEQ      FUDFUN
111900000000     C     FUDATT        ANDEQ     'S'
112000000000     C                   Z-ADD     I             W
112100000000     C                   END
112200000000     C                   END                                                    DO WHILE
112300000000     C*
112400000000     C* FUNZIONE NON ATTIVA (ERRORE)
112500000000     C     W             IFEQ      0
112600000000     C*
112700000000     C* EMISSIONE MESSAGGIO DI ERRORE
112800930201     C                   MOVEL     MSG(1)        DSMSMS
112900930201     C*
113000930201     C                   END                                                    W=0
113100000000     C                   END                                                    FUNKEY NE *BLANKS
113200000000     C*
113300000000     C* FUNZIONE ATTIVA (PROCEDI ...)
113400000000     C     W             IFNE      0
113500000000     C     FUNKEY        OREQ      *BLANKS
113600000000     C*
113700000000     C     FUNKEY        IFEQ      *BLANKS
113800000000     C                   EXSR      ENTGES
113900000000     C                   END
114000000000     C     FUNKEY        IFEQ      '01'
114100000000     C                   EXSR      F01GES
114200000000     C                   END
114300000000     C     FUNKEY        IFEQ      '03'
114400000000     C                   EXSR      F03GES
114500000000     C                   END
114600930202     C     FUNKEY        IFEQ      '06'
114700930202     C                   EXSR      F06GES
114800930202     C                   END
114900000000     C     FUNKEY        IFEQ      '07'
115000000000     C                   EXSR      F07GES
115100000000     C                   END
115200000000     C     FUNKEY        IFEQ      '08'
115300000000     C                   EXSR      F08GES
115400000000     C                   END
115500000000     C     FUNKEY        IFEQ      '12'
115600000000     C                   EXSR      F12GES
115700000000     C                   END
115800930201     C     FUNKEY        IFEQ      '21'
115900930201     C                   EXSR      F21GES
116000930201     C                   END
116100000000     C     FUNKEY        IFEQ      '22'
116200000000     C                   EXSR      F22GES
116300000000     C                   END
116400000000     C     FUNKEY        IFEQ      '24'
116500000000     C                   EXSR      F24GES
116600000000     C                   END
116700000000     C     FUNKEY        IFEQ      'HP'
116800000000     C                   EXSR      HELGES
116900000000     C                   END
117000000000     C*
117100000000     C                   END
117200000000     C*
117300000000     C                   ENDSR
117400000000     C*================================================================
117500000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
117600000000     C*================================================================
117700000000     C     FUNVAR        BEGSR
117800000000     C*
117900000000     C* INIZIALIZZA VARIABILE -FUNKEY-
118000000000     C*
118100000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
118200000000     C     KEY           IFEQ      ENTER
118300000000     C                   MOVEL     *BLANKS       FUNKEY
118400000000     C                   END
118500000000     C     KEY           IFEQ      F1
118600000000     C                   MOVEL     '01'          FUNKEY
118700000000     C                   END
118800000000     C     KEY           IFEQ      F2
118900000000     C                   MOVEL     '02'          FUNKEY
119000000000     C                   END
119100000000     C     KEY           IFEQ      F3
119200000000     C                   MOVEL     '03'          FUNKEY
119300000000     C                   END
119400000000     C     KEY           IFEQ      F4
119500000000     C                   MOVEL     '04'          FUNKEY
119600000000     C                   END
119700000000     C     KEY           IFEQ      F5
119800000000     C                   MOVEL     '05'          FUNKEY
119900000000     C                   END
120000000000     C     KEY           IFEQ      F6
120100000000     C                   MOVEL     '06'          FUNKEY
120200000000     C                   END
120300000000     C     KEY           IFEQ      F7
120400000000     C                   MOVEL     '07'          FUNKEY
120500000000     C                   END
120600000000     C     KEY           IFEQ      F8
120700000000     C                   MOVEL     '08'          FUNKEY
120800000000     C                   END
120900000000     C     KEY           IFEQ      F9
121000000000     C                   MOVEL     '09'          FUNKEY
121100000000     C                   END
121200000000     C     KEY           IFEQ      F10
121300000000     C                   MOVEL     '10'          FUNKEY
121400000000     C                   END
121500000000     C     KEY           IFEQ      F11
121600000000     C                   MOVEL     '11'          FUNKEY
121700000000     C                   END
121800000000     C     KEY           IFEQ      F12
121900000000     C                   MOVEL     '12'          FUNKEY
122000000000     C                   END
122100000000     C     KEY           IFEQ      F13
122200000000     C                   MOVEL     '13'          FUNKEY
122300000000     C                   END
122400000000     C     KEY           IFEQ      F14
122500000000     C                   MOVEL     '14'          FUNKEY
122600000000     C                   END
122700000000     C     KEY           IFEQ      F15
122800000000     C                   MOVEL     '15'          FUNKEY
122900000000     C                   END
123000000000     C     KEY           IFEQ      F16
123100000000     C                   MOVEL     '16'          FUNKEY
123200000000     C                   END
123300000000     C     KEY           IFEQ      F17
123400000000     C                   MOVEL     '17'          FUNKEY
123500000000     C                   END
123600000000     C     KEY           IFEQ      F18
123700000000     C                   MOVEL     '18'          FUNKEY
123800000000     C                   END
123900000000     C     KEY           IFEQ      F19
124000000000     C                   MOVEL     '19'          FUNKEY
124100000000     C                   END
124200000000     C     KEY           IFEQ      F20
124300000000     C                   MOVEL     '20'          FUNKEY
124400000000     C                   END
124500000000     C     KEY           IFEQ      F21
124600000000     C                   MOVEL     '21'          FUNKEY
124700000000     C                   END
124800000000     C     KEY           IFEQ      F22
124900000000     C                   MOVEL     '22'          FUNKEY
125000000000     C                   END
125100000000     C     KEY           IFEQ      F23
125200000000     C                   MOVEL     '23'          FUNKEY
125300000000     C                   END
125400000000     C     KEY           IFEQ      F24
125500000000     C                   MOVEL     '24'          FUNKEY
125600000000     C                   END
125700000000     C     KEY           IFEQ      ROLLUP
125800000000     C                   MOVEL     'RU'          FUNKEY
125900000000     C                   END
126000000000     C     KEY           IFEQ      HELP
126100000000     C                   MOVEL     'HP'          FUNKEY
126200000000     C                   END
126300000000     C                   ENDSR
126400000000     C*================================================================
126500000000     C* NOTE PER COMPILAZIONE SCHIERE
126600000000     C*================================================================
126700000000     C* -MSG-
126800000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
126900000000     C* (NON TOCCARE MSG,1)
127000000000     C*
127100000000     C* -CMD-
127200000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
127300000000     C*
127400000000     C* -FUD-
127500000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
127600000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
127700000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
127800000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
127900000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
128000000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
128100000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
128200000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
128300000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
128400000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
128500000000     C*
128600000000     C*================================================================
128700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
128800941110TNSD76R- Tasto funzionale non gestito                                          1
128900941110TNSD76R- Calendario già inserito per questa/e filiale/i.                       2
129000941110TNSD76R- Inserimento effettuato.                                               3
129100941110TNSD76R- Modifica effettuata.                                                  4
129200941110TNSD76R- Cancellazione effettuata.                                             5
129300941110TNSD76R- Ripristino effettuato.                                                6
129400941110TNSD76R- Linea di arrivo errata.                                               7
129500941110TNSD76R- Non trovato calendario banche. Avvertire CED.                         8
129600941110TNSD76R- Almeno uno dei simboli utilizzati non è accettabile.                  9
129700941110TNSD76R- Record non trovato.                                                  10
129800941110TNSD76R- Combinazione terminal partenza/arrivo non presente archivio trazioni.11
129900941110TNSD76R- Anno non accettabile.                                                12
130000941110TNSD76R- Mese non accettabile.                                                13
130100941110TNSD76R- Almeno uno dei giorni festivi non ha un flag nel calendario.         14
130200941110TNSD76R-                                                                      15
130300941110TNSD76R-                                                                      16
130400941110TNSD76R-                                                                      17
130500941110TNSD76R-                                                                      18
130600941110TNSD76R-                                                                      19
130700941110TNSD76R-                                                                      20
130800000000** ======== SCHIERA: CMD  (COMANDI)                        ================
130900000000WRKSPLF
131000000000
131100000000
131200000000
131300000000
131400000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
131500930201                   -        (S/N)=N                        funz.01¬
131600000000                   -        (S/N)=N                        funz.02¬
131700000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
131800000000                   -        (S/N)=N                        funz.04¬
131900000000                   -        (S/N)=N                        funz.05¬
132000930202F06=Aggiornamento  -        (S/N)=S                        funz.06¬
132100930201                   -        (S/N)=N                        funz.07¬
132200930401                   -        (S/N)=N                        funz.08¬
132300000000                   -        (S/N)=N                        funz.09¬
132400000000                   -        (S/N)=N                        funz.10¬
132500000000                   -        (S/N)=N                        funz.11¬
132600930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
132700000000                   -        (S/N)=N                        funz.13¬
132800000000                   -        (S/N)=N                        funz.14¬
132900000000                   -        (S/N)=N                        funz.15¬
133000000000                   -        (S/N)=N                        funz.16¬
133100000000                   -        (S/N)=N                        funz.17¬
133200000000                   -        (S/N)=N                        funz.18¬
133300000000                   -        (S/N)=N                        funz.19¬
133400000000                   -        (S/N)=N                        funz.20¬
133500930401                   -        (S/N)=N                        funz.21¬
133600930225F22=Gestione stampe-        (S/N)=N                        funz.22¬
133700000000                   -        (S/N)=N                        funz.23¬
133800000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
