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
008000930204     D* PARAMETRI CHIAMATA ROUTINE XFIL (RITORNO FILIALE IN CUI SONO)
008100930204     D DSFIL         E DS
008200930201     D* DS PER GESTIONE TASTI FUNZIONALI
008300930201     D DSKEY         E DS
008400930201     D DEVDS1          DS
008500930201     D  KEY                  369    369
008600930201     D* ARCHITETTURA
008700930201     D KPJBA         E DS
008800991223     d*---
008900991223     d* ds controlla data (8)
009000991223     d*---
009100991223     d wlbda8          DS                  INZ                                  *controlla data
009200991223     d  g08dat                        8  0
009300991223     d  g08inv                        8  0
009400991223     d  g08err                        1
009500991223     d  g08tgi                        5  0
009600991223     d*---
009700991223     d* variabili
009800991223     d*---
009900991223     d n14             S             14  0 INZ                                  *numerico 14
010000991223     d n8              S              8  0 INZ                                  *numerico 8
010100991223     d n4              S              4  0 INZ                                  *numerico 4
010200991223     d datcor          S              8  0 INZ                                  *data correrte a/m/g
010300991223     d*---
010400991223     d* Variabili riferite al data base
010500991223     d*---
010600991223     d KCACALEND       S                   LIKE(BCACALEND)                      *ANBCA01L
010700991223     d KCAANNO         S                   LIKE(BCAANNO)
010800991223     d KCAMESE         S                   LIKE(BCAMESE)
010900991223     f*--------------------------------------------------------------------------------------------*
011000991223     f* Main lines
011100991223     f*--------------------------------------------------------------------------------------------*
011200000000     C                   EXSR      RUTINZ
011300000000     C*
011400930225     C* SE CONTROLLI INIZIALI NON OK, RITORNO A PGM CHIAMANTE
011500930225     C     ERRL00        IFEQ      '1'                                          --- 1 -->
011600930225     C*
011700930225     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
011800930312     C                   MOVEL     '1'           DSCERR
011900930312     C                   MOVEL     DSMSMS        DSCMSG
012000930225     C*
012100930225     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
012200930225     C                   ELSE                                                   <-- 1 -->
012300000000     C*
012400000000     C* CICLO FINO A CHE:
012500000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012600000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012700930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
012800000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
012900000000     C     F12L00        OREQ      '1'
013000930202     C     FLFINE        OREQ      '1'
013100000000     C*
013200000000     C* EMETTE FORMATO VIDEO
013300000000     C                   EXSR      BUTFMR
013400000000     C*
013500000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
013600000000     C                   EXSR      FUNCON
013700000000     C*
013800930225     C                   END                                                    <-- 2 ---
013900000000     C*
014000930225     C                   END                                                    <-- 1 ---
014100000000     C*
014200000000     C                   SETON                                        LR
014300000000     C*FINE MAIN DEL PGM
014400000000     C*================================================================
014500000000     C* EMISSIONE FORMATO A VIDEO
014600000000     C*================================================================
014700000000     C     BUTFMR        BEGSR
014800930204     C*
014900930204     C* WRITE RECORD TESTATA
015000941110     C                   WRITE     SD76D01
015100930205     C*
015200930205     C* VISUALIZZAZIONE MESSAGGIO
015300930205     C                   EXSR      SNDMSG
015400930204     C*
015500930204     C* WRITE RECORD FUNZIONI
015600941110     C                   WRITE     SD76D02
015700000000     C*
015800930204     C* EXFMT RECORD DETTAGLIO
015900941110     C                   EXFMT     SD76D03
016000000000     C*
016100000000     C* AZZERO FLAG DI ERRORE LIVELLO
016200000000     C                   MOVEL     '0'           ERRL00
016300000000     C*
016400000000     C* AZZERO FLAG DI ERRORE
016500930330     C                   SETOFF                                       606162
016600930401     C                   SETOFF                                       636465
016700000000     C*
016800000000     C                   ENDSR
016900000000     C*================================================================
017000000000     C* SNDMSG - MANDA MESSAGGIO
017100000000     C*================================================================
017200000000     C     SNDMSG        BEGSR
017300000000     C*
017400000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
017500000000     C                   MOVEL     '1'           *IN40
017600941110     C                   WRITE     SD76DMC
017700000000     C                   MOVEL     '0'           *IN40
017800000000     C*
017900000000     C* VISUALIZZAZIONE MESSAGGIO
018000000000     C                   Z-ADD     0             MSGULT            4 0
018100930202     C                   CALL      'XMSG'        XMSGPA
018200930201     C     DSMSER        IFEQ      0
018300930201     C                   MOVEL     DSMSKE        MSGKEY
018400000000     C                   END
018500000000     C                   Z-ADD     1             MSGULT
018600941110     C                   WRITE     SD76DMS                                      SFL MESS
018700941110     C                   WRITE     SD76DMC                                      CONTR MESS
018800000000     C*
018900000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
019000930201     C                   MOVEL     *BLANKS       DSMSMS
019100000000     C*
019200000000     C                   ENDSR
019300000000     C*================================================================
019400000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
019500000000     C*================================================================
019600000000     C     ENTGES        BEGSR
019700930201     C*
019800930201     C* CONTROLLA I CAMPI IMMESSI
019900930201     C                   EXSR      DECODI
020000930202     C*
020100930202     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
020200930330     C     DSCOPZ        IFEQ      'O05'
020300930202     C                   MOVEL     '1'           FLFINE
020400930202     C                   END
020500000000     C*
020600000000     C                   ENDSR
020700000000     C*================================================================
020800930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
020900000000     C*================================================================
021000000000     C     F01GES        BEGSR
021100000000     C                   ENDSR
021200000000     C*================================================================
021300930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
021400000000     C*================================================================
021500000000     C     F03GES        BEGSR
021600000000     C*
021700930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
021800000000     C                   MOVEL     '1'           F03L00
021900930202     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
022000930312     C                   MOVEL     '1'           DSCF03
022100000000     C*
022200000000     C                   ENDSR
022300930201     C*================================================================
022400930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
022500930201     C*================================================================
022600930201     C     F06GES        BEGSR
022700930201     C*
022800930201     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
022900930330     C     DSCOPZ        IFEQ      'F06'
023000930201     C                   EXSR      INSERI
023100930201     C                   END
023200930330     C     DSCOPZ        IFEQ      'O02'
023300930202     C                   EXSR      MODIFI
023400930201     C                   END
023500930330     C     DSCOPZ        IFEQ      'O04'
023600930201     C                   EXSR      CANCEL
023700930201     C                   END
023800930330     C     DSCOPZ        IFEQ      'O07'
023900930201     C                   EXSR      RIPRIS
024000930201     C                   END
024100930201     C*
024200930201     C                   ENDSR
024300000000     C*================================================================
024400930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
024500000000     C*================================================================
024600000000     C     F07GES        BEGSR
024700000000     C                   ENDSR
024800000000     C*================================================================
024900930330     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
025000000000     C*================================================================
025100000000     C     F08GES        BEGSR
025200000000     C                   ENDSR
025300000000     C*================================================================
025400930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
025500000000     C*================================================================
025600000000     C     F12GES        BEGSR
025700000000     C*
025800000000     C* IMPOSTO FLAG DI LIVELLO
025900000000     C                   MOVEL     '1'           F12L00
026000000000     C*
026100000000     C                   ENDSR
026200930201     C*================================================================
026300930201     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA CAPPARIO
026400930201     C*================================================================
026500930201     C     F21GES        BEGSR
026600930201     C                   ENDSR
026700000000     C*================================================================
026800930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
026900000000     C*================================================================
027000000000     C     F22GES        BEGSR
027100000000     C*
027200000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
027300000000     C                   Z-ADD     80            LENGH            15 5
027400000000     C                   CALL      'QCAEXEC'
027500000000     C                   PARM                    CMD(1)
027600000000     C                   PARM                    LENGH
027700000000     C*
027800000000     C                   ENDSR
027900000000     C*================================================================
028000930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
028100000000     C*================================================================
028200000000     C     F24GES        BEGSR
028300000000     C*
028400000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
028500000000     C                   EXSR      FUNGES
028600000000     C*
028700000000     C                   ENDSR
028800000000     C*================================================================
028900000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
029000000000     C*================================================================
029100000000     C     HELGES        BEGSR
029200000000     C                   ENDSR
029300930201     C*================================================================
029400930201     C* INSERI - FUNZIONE DI INSERIMENTO
029500930201     C*================================================================
029600930201     C     INSERI        BEGSR
029700930201     C*
029800930201     C* CONTROLLA I CAMPI IMMESSI
029900930201     C                   EXSR      DECODI
030000930201     C*
030100930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
030200930201     C*
030300930330     C* IMPOSTO IL RECORD DI E SCRIVO
030400930330     C                   EXSR      VIDREC
030500941110     C                   WRITE     AZCLN000
030600930405     C* AGGIORNO RECORD MESE
030700930405     C                   EXSR      INS000
030800930201     C*
030900930201     C                   END                                                    <-- 1 ---
031000930201     C*
031100930201     C* AZZERA TUTTI I CAMPI A VIDEO PER NUOVO INSERIMENTO
031200930201     C                   EXSR      AZZVID
031300930202     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
031400930202     C                   MOVEL     MSG(3)        DSMSMS
031500930201     C*
031600930201     C                   ENDSR
031700930201     C*================================================================
031800930201     C* MODIFI - FUNZIONE DI MODIFICA
031900930201     C*================================================================
032000930201     C     MODIFI        BEGSR
032100930201     C*
032200930201     C* CONTROLLA I CAMPI IMMESSI
032300930201     C                   EXSR      DECODI
032400930201     C*
032500930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
032600930201     C*
032700930330     C* IMPOSTO IL RECORD E RISCRIVO
032800930401     C                   EXSR      VIDREC
032900941110     C                   UPDATE    AZCLN000
033000930405     C* AGGIORNO RECORD MESE
033100930405     C                   EXSR      INS000
033200930201     C*
033300930201     C                   END                                                    <-- 1 ---
033400930201     C*
033500930201     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
033600930201     C                   MOVEL     '1'           FLFINE
033700930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI MODIF. EFFETTUATA
033800930312     C                   MOVEL     MSG(4)        DSCMSG
033900930201     C*
034000930201     C                   ENDSR
034100930201     C*================================================================
034200930201     C* CANCEL - FUNZIONE DI CANCELLAZIONE
034300930201     C*================================================================
034400930201     C     CANCEL        BEGSR
034500930201     C*
034600930330     C* ANNULLO RECORD E RISCRIVO
034700941110     C                   MOVEL     *BLANKS       CLNFTR
034800941110     C                   Z-ADD     *ZEROS        CLNDTR
034900941110     C                   MOVEL     'A'           CLNATB
035000941110     C                   UPDATE    AZCLN000
035100930405     C* AGGIORNO RECORD MESE
035200930405     C                   EXSR      INS000
035300930201     C*
035400930201     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
035500930201     C                   MOVEL     '1'           FLFINE
035600930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI CANC. EFFETTUATA
035700930312     C                   MOVEL     MSG(5)        DSCMSG
035800930201     C*
035900930201     C                   ENDSR
036000930201     C*================================================================
036100930201     C* RIPRIS - FUNZIONE DI RIPRISTINO
036200930201     C*================================================================
036300930201     C     RIPRIS        BEGSR
036400930202     C*
036500930330     C* RIPRISTINO IL RECORD
036600941110     C                   MOVEL     *BLANKS       CLNFTR
036700941110     C                   Z-ADD     *ZEROS        CLNDTR
036800941110     C                   MOVEL     ' '           CLNATB
036900941110     C                   UPDATE    AZCLN000
037000930405     C* AGGIORNO RECORD MESE
037100930405     C                   EXSR      INS000
037200930202     C*
037300930202     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
037400930202     C                   MOVEL     '1'           FLFINE
037500930202     C* IMPOSTO MESSAGGIO PER PGM CHIAMANTE DI RIPR. EFFETTUATO
037600930312     C                   MOVEL     MSG(6)        DSCMSG
037700930202     C*
037800930201     C                   ENDSR
037900930405     C*================================================================
038000930405     C* INS000 - GESTIONE RECORD MESE (GG.DELLA SETTIMANA E FESTIVI)
038100930405     C*================================================================
038200930405     C     INS000        BEGSR
038300930405     C*
038400930405     C* CONTROLLA SE IL RECORD ESISTE GIA' OPPURE NO
038500941110     C                   Z-ADD     *ZEROS        KCLTFP
038600941110     C                   Z-ADD     *ZEROS        KCLTFA
038700941110     C                   Z-ADD     VIDANN        KCLANN
038800941110     C                   Z-ADD     VIDMES        KCLMES
038900941110     C     KEYCLN        CHAIN     AZCLN01L                           99
039000941110     C                   Z-ADD     *ZEROS        CLNTFP
039100941110     C                   Z-ADD     *ZEROS        CLNTFA
039200941110     C                   Z-ADD     VIDANN        CLNANN
039300941110     C                   Z-ADD     VIDMES        CLNMES
039400941110     C                   MOVEL     WGSE00        CLNMAT
039500941110     C                   MOVEL     GFEDS         CLNPOM
039600941110     C                   MOVEL     *BLANKS       CLNFTR
039700941110     C                   Z-ADD     *ZEROS        CLNDTR
039800941110     C                   MOVEL     *BLANKS       CLNATB
039900930405     C     *IN99         IFEQ      '1'
040000941110     C                   WRITE     AZCLN000
040100930405     C                   ELSE
040200941110     C                   UPDATE    AZCLN000
040300930405     C                   END
040400930405     C*
040500930405     C                   ENDSR
040600000000     C*================================================================
040700930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
040800000000     C*================================================================
040900000000     C     DECODI        BEGSR
041000000000     C*
041100930330     C* CONTROLLO TERMINAL/FILIALE PARTENZA
041200991224if  1C     VIDPAR        IFNE      *ZEROS
041300930330     C                   MOVE      VIDPAR        WLIN              3
041400930201     C                   EXSR      CHKLNA
041500991224if  2C     ERRL00        IFEQ      '1'
041600930330     C                   SETON                                        60
041700991224x   2C                   ELSE
041800930330     C                   MOVEL     WLIN          VIDPAR
041900930330     C                   MOVEL     WDLIN         VIDDPA
042000991224e   2C                   ENDIF
042100991224e   1C                   ENDIF
042200930217     C*
042300930330     C* CONTROLLO TERMINAL/FILIALE ARRIVO
042400991224if  1C     ERRL00        IFEQ      '0'
042500991224if  2C     VIDARR        IFNE      *ZEROS
042600930330     C                   MOVE      VIDARR        WLIN
042700930217     C                   EXSR      CHKLNA
042800991224if  3C     ERRL00        IFEQ      '1'
042900930330     C                   SETON                                        61
043000991224x   3C                   ELSE
043100930330     C                   MOVEL     WLIN          VIDARR
043200930330     C                   MOVEL     WDLIN         VIDDAR
043300991224e   3C                   ENDIF
043400991224e   2C                   ENDIF
043500991224e   1C                   ENDIF
043600930401     C*
043700941110     C* LA COMBINAZIONE PARTENZA/ARRIVO DEVE ESISTERE NEL FILE TRAZIONI
043800991224if  1C     ERRL00        IFEQ      '0'
043900991224if  2C     VIDPAR        IFNE      *ZEROS
044000941110     C     VIDARR        ANDEQ     *ZEROS
044100941110     C                   MOVEL     'P'           KCSPTA                         P A R T E N Z A
044200941110     C                   MOVEL     VIDPAR        KCSLNP
044300941110     C                   MOVEL     VIDARR        KCSLNA
044400941110     C                   MOVEL     *BLANKS       KCSTSP
044500941110     C                   Z-ADD     *ZEROS        KCSDDE
044600991224e   2C                   ENDIF
044700991224if  2C     VIDARR        IFNE      *ZEROS                                       A R R I V O
044800941110     C     VIDPAR        ANDEQ     *ZEROS
044900941110     C                   MOVEL     'A'           KCSPTA
045000941110     C                   MOVEL     VIDARR        KCSLNP
045100941110     C                   MOVEL     VIDPAR        KCSLNA
045200941110     C                   MOVEL     *BLANKS       KCSTSP
045300941110     C                   Z-ADD     *ZEROS        KCSDDE
045400991224e   2C                   ENDIF
045500991224if  2C     VIDPAR        IFNE      *ZEROS
045600941110     C     VIDARR        ANDNE     *ZEROS
045700941110     C                   MOVEL     'T'           KCSPTA                         T R A Z I O N E
045800941110     C                   MOVEL     VIDPAR        KCSLNP
045900941110     C                   MOVEL     VIDARR        KCSLNA
046000941110     C                   MOVEL     *BLANKS       KCSTSP
046100941110     C                   Z-ADD     *ZEROS        KCSDDE
046200991224e   2C                   ENDIF
046300941110     C     KEYCAS        SETLL     AZCAS01L                           99        *FINE FILE
046400941110     C  N99              READ      AZCAS01L                               99
046500991224if  2C     *IN99         IFNE      *OFF
046600941110     C     CASLNP        ANDNE     KCSLNP
046700941110     C     CASLNA        ANDNE     KCSLNA
046800941110     C                   MOVEL     MSG(11)       DSMSMS
046900941110     C                   MOVEL     '1'           ERRL00
047000941110     C                   SETON                                        6061
047100991224e   2C                   ENDIF
047200991224e   1C                   ENDIF
047300991224     C*
047400991223     C* CONTROLLO ANNO
047500991224if  1C     ERRL00        IFEQ      '0'
047600991223     C                   TIME                    n14                            *Ora (6)+ Data (8)
047700991223     C                   MOVE      n14           n8                             *Data (8) in g/m/a
047800991223     C                   Z-ADD     n8            g08dat
047900991223     C                   Z-ADD     *zeros        g08inv
048000991223     C                   MOVEL     '0'           g08err
048100991223     C                   CALL      'XSRDA8'
048200991223     C                   PARM                    wlbda8
048300991223     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
048400991223     C                   MOVEL     datcor        n4                             *anno
048500991224if  2c                   IF        vidann > *zeros AND
048600991223     c                             vidann >= 1992  AND                          *compreso fra 1992 e
048700991223     c                             vidann <= (n4+1)                             *anno prossimo
048800991224x   2c                   ELSE
048900941110     C                   MOVEL     MSG(12)       DSMSMS
049000941110     C                   MOVEL     '1'           ERRL00
049100941110     C                   SETON                                        62
049200991224e   2c                   ENDIF
049300991224e   1C                   ENDIF
049400941110     C*
049500930330     C* CONTROLLO MESE
049600991224if  1C     ERRL00        IFEQ      '0'
049700991224if  2C     VIDMES        IFLT      1
049800930330     C     VIDMES        ORGT      12
049900930401     C                   MOVEL     MSG(13)       DSMSMS
050000930330     C                   MOVEL     '1'           ERRL00
050100930330     C                   SETON                                        63
050200991224e   2C                   ENDIF
050300991224e   1C                   ENDIF
050400930518     C*
050500930518     C* SE INSERIMENTO CONTROLLA ANCHE CHE IL RECORD NON ESISTA GIA'
050600991224if  1C     ERRL00        IFEQ      '0'
050700991224if  2C     DSCOPZ        IFEQ      'F06'
050800941110     C                   MOVE      VIDPAR        KCLTFP
050900941110     C                   MOVE      VIDARR        KCLTFA
051000941110     C                   Z-ADD     VIDANN        KCLANN
051100941110     C                   Z-ADD     VIDMES        KCLMES
051200971203     C     KEYCLN        CHAIN(N)  AZCLN01L                           99
051300991224if  3C     *IN99         IFEQ      '0'
051400930518     C                   MOVEL     '1'           ERRL00
051500930518     C                   MOVEL     MSG(2)        DSMSMS
051600930518     C                   SETON                                        606162
051700930518     C                   SETON                                        63
051800991224e   3C                   ENDIF
051900991224e   2C                   ENDIF
052000991224e   1C                   ENDIF
052100930401     C*
052200930401     C* IMPOSTO GIORNI SETTIMANA E FESTIVI
052300991224if  1C     ERRL00        IFEQ      '0'
052400930401     C                   EXSR      IMPGIO
052500930401     C*
052600930401     C* CONTROLLO FLAG MATTINA
052700930401     C                   MOVEL     VIDMAT        GMADS
052800991224do  2C                   DO        31            X                 6 0
052900991224if  3C     GFE(X)        IFEQ      '*'
053000930401     C                   MOVEL     '*'           GMA(X)
053100991224x   3C                   ELSE
053200991224if  4C     GMA(X)        IFEQ      *BLANKS
053300991224if  5C     GFE(X)        IFEQ      'F'
053400930401     C                   MOVEL     '1'           ERRL00
053500930401     C                   MOVEL     MSG(14)       DSMSMS
053600930401     C                   SETON                                        64
053700991224e   5C                   ENDIF
053800991224x   4C                   ELSE
053900930401     C                   MOVEL     GMA(X)        WSIM              1
054000930401     C                   EXSR      CHKSIM
054100991224if  5C     ERRL00        IFEQ      '1'
054200930401     C                   SETON                                        64
054300991224x   5C                   ELSE
054400930401     C                   MOVEL     WSIM          GMA(X)
054500991224e   5C                   ENDIF
054600991224e   4C                   ENDIF
054700991224e   3C                   ENDIF
054800991224e   2C                   ENDDO
054900930401     C                   MOVEL     GMADS         VIDMAT
055000930401     C*
055100930401     C* CONTROLLO FLAG POMERIGGIO
055200991224if  2C     ERRL00        IFEQ      '0'
055300930401     C                   MOVEL     VIDPOM        GPODS
055400991224do  3C                   DO        31            X                 6 0
055500991224if  4C     GFE(X)        IFEQ      '*'
055600930401     C                   MOVEL     '*'           GPO(X)
055700991224x   4C                   ELSE
055800991224if  5C     GPO(X)        IFEQ      *BLANKS
055900991224if  6C     GFE(X)        IFEQ      'F'
056000930401     C                   MOVEL     '1'           ERRL00
056100930401     C                   MOVEL     MSG(14)       DSMSMS
056200930401     C                   SETON                                        65
056300991224e   6C                   ENDIF
056400991224x   5C                   ELSE
056500930401     C                   MOVEL     GPO(X)        WSIM              1
056600930401     C                   EXSR      CHKSIM
056700991224if  6C     ERRL00        IFEQ      '1'
056800930401     C                   SETON                                        65
056900991224x   6C                   ELSE
057000930401     C                   MOVEL     WSIM          GPO(X)
057100991224e   6C                   ENDIF
057200991224e   5C                   ENDIF
057300991224e   4C                   ENDIF
057400991224e   3C                   ENDDO
057500930401     C                   MOVEL     GPODS         VIDPOM
057600991224e   2C                   ENDIF
057700991224e   1C                   ENDIF
057800000000     C*
057900000000     C                   ENDSR
058000930201     C*================================================================
058100930201     C*  CHKLNA - CONTROLLA LA LINEA
058200930201     C*================================================================
058300930201     C     CHKLNA        BEGSR
058400930201     C*
058500930201     C* CONTROLLO SE E' STATO INSERITO '?'
058600930217     C                   MOVE      WLIN          WLNA1             1
058700930217     C                   MOVEL     WLIN          WLNA2             1
058800930201     C*
058900930201     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
059000930202     C     WLNA1         IFEQ      '?'                                          --- 1 -->
059100930202     C     WLNA2         OREQ      '?'
059200930201     C                   MOVEL     *BLANKS       §COD1             3
059300930201     C                   MOVE      *BLANKS       §DES1
059400971203     C                   MOVE      *BLANKS       §PRA
059500971203     C                   CALL      'TNSD24R'
059600930201     C                   PARM                    §COD1
059700930201     C                   PARM                    §TIP              1
059800930201     C                   PARM                    §DES1            25
059900971203     C                   PARM                    §PRA              3
060000930217     C                   MOVEL     §COD1         WLIN
060100930217     C                   MOVEL     §DES1         WDLIN            20
060200930201     C*
060300930201     C                   ELSE                                                   <-- 1 -->
060400930201     C*
060500930201     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
060600930217     C                   MOVE      WLIN          KORFIL
060700930202     C     KEYORG        CHAIN     AZORG                              99
060800930201     C     *IN99         IFEQ      '0'                                          --- 2 -->
060900930217     C                   MOVEL     ORGDES        WDLIN
061000930201     C                   ELSE                                                   <-- 2 -->
061100930201     C                   MOVEL     '1'           ERRL00
061200930202     C                   MOVEL     MSG(7)        DSMSMS
061300930217     C                   MOVEL     *ALL'*'       WDLIN
061400930201     C                   END                                                    <-- 2 ---
061500930201     C*
061600930201     C                   END                                                    <-- 1 ---
061700930201     C*
061800930201     C                   ENDSR
061900930201     C*================================================================
062000930330     C*  CHKSIM - CONTROLLA SIMBOLI CALENDARIO
062100930201     C*================================================================
062200930330     C     CHKSIM        BEGSR
062300930201     C*
062400930201     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
062500930401     C     WSIM          IFEQ      '?'                                          --- 1 -->
062600930201     C*
062700930201     C                   MOVEL     CODUT         §KUT              1 0
062800930330     C                   MOVEL     '1Y'          §COD              2
062900930201     C                   MOVE      *BLANKS       §DES1
063000930201     C                   CALL      'X§TABER'
063100930201     C                   PARM                    §KUT
063200930201     C                   PARM                    §COD
063300930201     C                   PARM                    §KEY              8
063400930201     C                   PARM                    §DES             30
063500930330     C                   MOVEL     §KEY          WSIM
063600930330     C                   MOVEL     §DES          DS1Y                           *NELLA DS
063700930201     C*
063800930201     C                   ELSE                                                   <-- 1 -->
063900930201     C*
064000930201     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
064100930202     C                   MOVEL     CODUT         KTBUTE
064200930330     C                   MOVE      '1Y'          KTBCOD
064300930202     C                   MOVE      *BLANKS       KTBKEY
064400930330     C                   MOVEL     WSIM          KTBKEY
064500930202     C     KEYTAB        CHAIN     TABEL                              99
064600930201     C     *IN99         IFEQ      '1'                                          --- 2 -->
064700930201     C     TBLFLG        OREQ      '*'
064800930201     C                   MOVEL     '1'           ERRL00
064900930202     C                   MOVEL     MSG(9)        DSMSMS
065000930205     C                   ELSE
065100930330     C                   MOVEL     TBLUNI        DS1Y                           *NELLA DS
065200930201     C                   END                                                    <-- 2 ---
065300930201     C*
065400930201     C                   END                                                    <-- 1 ---
065500930201     C*
065600930201     C                   ENDSR
065700930201     C*================================================================
065800930330     C*  RECVID - PORTA I CAMPI DAL RECORD AL VIDEO
065900930201     C*================================================================
066000930330     C     RECVID        BEGSR
066100930201     C*
066200941110     C                   MOVE      CLNTFP        VIDPAR
066300941110     C                   MOVE      CLNTFA        VIDARR
066400941110     C                   Z-ADD     CLNANN        VIDANN
066500941110     C                   Z-ADD     CLNMES        VIDMES
066600930202     C*
066700941110     C                   MOVEL     CLNMAT        VIDMAT
066800941110     C                   MOVEL     CLNPOM        VIDPOM
066900930202     C*
067000930401     C* LANCIO ROUTINE CHE IMPOSTA I GIORNI DELLA SETTIMANA E FESTIVI
067100930401     C                   EXSR      IMPGIO
067200930202     C*
067300941110     C     CLNATB        IFEQ      'A'
067400930202     C                   MOVEL     'ANNULLAT'    VIDATB
067500930202     C                   MOVE      'O'           VIDATB
067600930202     C                   ELSE
067700930202     C                   MOVEL     *BLANKS       VIDATB
067800930202     C                   END
067900930201     C*
068000930201     C                   ENDSR
068100930201     C*================================================================
068200930201     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
068300930201     C*================================================================
068400930201     C     AZZVID        BEGSR
068500930201     C*
068600930401     C                   MOVE      *ZEROS        VIDPAR
068700930401     C                   MOVE      *ZEROS        VIDARR
068800930401     C                   Z-ADD     *ZEROS        VIDANN
068900930401     C                   Z-ADD     *ZEROS        VIDMES
069000930401     C*
069100930401     C                   MOVEL     *BLANKS       VIDGIO
069200930401     C                   MOVEL     *BLANKS       VIDFES
069300930401     C                   MOVEL     *BLANKS       VIDMAT
069400930401     C                   MOVEL     *BLANKS       VIDPOM
069500930201     C*
069600930201     C                   ENDSR
069700930201     C*================================================================
069800930401     C*  VIDREC - IMPOSTA CAMPI DA VIDEO A RECORD
069900930201     C*================================================================
070000930401     C     VIDREC        BEGSR
070100930201     C*
070200941110     C                   MOVE      VIDPAR        CLNTFP
070300941110     C                   MOVE      VIDARR        CLNTFA
070400941110     C                   Z-ADD     VIDANN        CLNANN
070500941110     C                   Z-ADD     VIDMES        CLNMES
070600930401     C*
070700941110     C                   MOVEL     VIDMAT        CLNMAT
070800941110     C                   MOVEL     VIDPOM        CLNPOM
070900930401     C*
071000941110     C                   MOVEL     *BLANKS       CLNFTR
071100941110     C                   Z-ADD     *ZEROS        CLNDTR
071200941110     C                   MOVEL     *BLANKS       CLNATB
071300930201     C*
071400930201     C                   ENDSR
071500930401     C*================================================================
071600930401     C*  IMPGIO - IMPOSTA GIORNI DELLA SETTIMANA E FESTIVI
071700930401     C*================================================================
071800930401     C     IMPGIO        BEGSR
071900930401     C*
072000930401     C* LANCIO ROUTINE PER IMPOSTARE GIORNI FESTIVI
072100930401     C                   EXSR      IMPGFE
072200930401     C*
072300930401     C* LANCIO ROUTINE PER IMPOSTARE GIORNI SETTIMANA
072400930401     C                   EXSR      IMPGSE
072500930401     C*
072600930401     C* IMPOSTO ASTERISCHI NEI GIORNI NON VALIDI
072700930401     C                   DO        31            X
072800930401     C     GFE(X)        IFEQ      '*'
072900930401     C                   MOVEL     '*'           GMA(X)
073000930401     C                   MOVEL     '*'           GPO(X)
073100930401     C                   ELSE
073200930401     C                   MOVEL     *BLANKS       GMA(X)
073300930401     C                   MOVEL     *BLANKS       GPO(X)
073400930401     C                   END
073500930401     C                   END
073600930401     C*
073700930401     C                   ENDSR
073800930201     C*================================================================
073900930401     C*  IMPGFE - IMPOSTA GIORNI FESTIVI
074000930201     C*================================================================
074100930401     C     IMPGFE        BEGSR
074200930201     C*
074300930401     C* LEGGE SUL FILE BNBCA00F IL RECORD DEL MESE
074400991223     C                   MOVEL     'CB'          KCACALEND
074500991223     C                   Z-ADD     VIDANN        KCAANNO
074600991223     C                   Z-ADD     VIDMES        KCAMESE
074700991223     C     KEYBCA        CHAIN     ANBCA01L                           99
074800930401     C     *IN99         IFEQ      '1'                                          --- 1 -->
074900930401     C                   MOVEL     '1'           ERRL00
075000930401     C                   MOVEL     MSG(8)        DSMSMS
075100930401     C                   ELSE                                                   <-- 1 -->
075200930401     C                   MOVEL     BCAGPF        GFEDS
075300930401     C                   DO        31            X                 6 0          --- 2 -->
075400930401     C     GFE(X)        IFEQ      *BLANKS                                      --- 3 -->
075500930401     C                   MOVEL     '*'           GFE(X)
075600930401     C                   ELSE                                                   <-- 3 -->
075700930401     C     GFE(X)        IFEQ      *ZEROS
075800930401     C                   MOVEL     ' '           GFE(X)
075900930401     C                   ELSE
076000930401     C                   MOVEL     'F'           GFE(X)
076100930401     C                   END
076200930401     C                   END                                                    <-- 3 ---
076300930401     C                   END                                                    <-- 2 ---
076400930401     C                   END                                                    <-- 1 ---
076500930401     C                   MOVEL     GFEDS         VIDFES
076600930201     C*
076700930201     C                   ENDSR
076800930401     C*================================================================
076900930401     C*  IMPGSE - IMPOSTA GIORNI DELLA SETTIMANA
077000930401     C*================================================================
077100930401     C     IMPGSE        BEGSR
077200930401     C*
077300930401     C* LANCIO COMANDO RTVDAT PER AVERE IL GIORNO DELLA SETTIMANA
077400941110     C                   MOVE      VIDANN        WAA                            *LUNGO 2
077500930401     C                   Z-ADD     VIDMES        WMM
077600930401     C                   DO        31            X                 6 0          --- 1 -->
077700930401     C     GFE(X)        IFEQ      '*'                                          --- 2 -->
077800930401     C                   MOVEL     '*'           GSE(X)
077900930401     C                   ELSE                                                   <-- 2 -->
078000930401     C                   Z-ADD     X             WGG
078100930401     C                   MOVE      WDATA         C98DAT
078200941110     C                   CALL      'XGIOSET'
078300930401     C                   PARM                    C98DAT            6
078400930401     C                   PARM                    C98GSE            1
078500930401     C                   MOVE      C98GSE        W98GSE            1 0
078600930401     C     W98GSE        IFEQ      1
078700930401     C                   Z-ADD     7             W98GSE
078800930401     C                   ELSE
078900930401     C                   SUB       1             W98GSE
079000930401     C                   END
079100930401     C                   MOVE      W98GSE        GSE(X)
079200930401     C                   END                                                    <-- 2 ---
079300930401     C                   END                                                    <-- 1 ---
079400930405     C                   MOVEL     GSEDS         WGSE00           31
079500930401     C     '1':'L'       XLATE     GSEDS         GSEDS
079600930401     C     '2':'M'       XLATE     GSEDS         GSEDS
079700930401     C     '3':'M'       XLATE     GSEDS         GSEDS
079800930401     C     '4':'G'       XLATE     GSEDS         GSEDS
079900930401     C     '5':'V'       XLATE     GSEDS         GSEDS
080000930401     C     '6':'S'       XLATE     GSEDS         GSEDS
080100930401     C     '7':'D'       XLATE     GSEDS         GSEDS
080200930401     C                   MOVEL     GSEDS         VIDGIO
080300930401     C*
080400930401     C                   ENDSR
080500000000     C*================================================================
080600000000     C*  RUTINZ - ROUTINE INIZIALE
080700000000     C*================================================================
080800000000     C     RUTINZ        BEGSR
080900930201     C*
081000930201     C* RICEVIMENTO PARAMETRI
081100930201     C     *ENTRY        PLIST
081200930401     C                   PARM                    DSCF03            1            PREMUTO F03
081300930401     C                   PARM                    DSCERR            1            '1'=ERRORE
081400930401     C                   PARM                    DSCMSG           80            MESSAGGIO
081500930401     C                   PARM                    DSCOPZ            3            OPZIONE LIV.PGM
081600930401     C                   PARM                    DSCPAR            3 0          PARTENZA
081700930401     C                   PARM                    DSCARR            3 0          ARRIVO
081800941110     C                   PARM                    DSCANN            4 0          ANNO
081900930401     C                   PARM                    DSCMES            2 0          MESE
082000930201     C*
082100930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
082200930201     C     XMSGPA        PLIST
082300930201     C                   PARM                    DSMSMS
082400930201     C                   PARM                    DSMSPG
082500930201     C                   PARM                    DSMSKE
082600930201     C                   PARM                    DSMSER
082700930401     C*
082800941110     C* CHIAVE LETTURA AZCLN01L - COMPLETA
082900941110     C     KEYCLN        KLIST
083000941110     C                   KFLD                    KCLTFP
083100941110     C                   KFLD                    KCLTFA
083200941110     C                   KFLD                    KCLANN
083300941110     C                   KFLD                    KCLMES
083400930201     C*
083500930201     C* CHIAVE LETTURA AZORG01L - COMPLETA
083600930201     C     KEYORG        KLIST
083700930201     C                   KFLD                    KORFIL
083800930201     C*
083900930201     C* CHIAVE LETTURA TABEL00F - COMPLETA
084000930201     C     KEYTAB        KLIST
084100930201     C                   KFLD                    KTBUTE
084200930201     C                   KFLD                    KTBCOD
084300930201     C                   KFLD                    KTBKEY
084400930401     C*
084500941110     C* CHIAVE LETTURA AZCAS01L - COMPLETA
084600941110     C     KEYCAS        KLIST
084700941110     C                   KFLD                    KCSPTA
084800941110     C                   KFLD                    KCSLNP
084900941110     C                   KFLD                    KCSLNA
085000941110     C                   KFLD                    KCSTSP
085100941110     C                   KFLD                    KCSDDE
085200930401     C*
085300991223     C* CHIAVE LETTURA ANBCA01L - COMPLETA
085400991223     C     KEYBCA        KLIST
085500991223     C                   KFLD                    KCACALEND
085600991223     C                   KFLD                    KCAANNO
085700991223     C                   KFLD                    KCAMESE
085800930201     C*
085900930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
086000930201     C                   CALL      'XKEY'
086100930201     C                   PARM                    DSKEY
086200930201     C*
086300930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
086400930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
086500930201     C     *LIKE         DEFINE    ORGFIL        KORFIL
086600941110     C     *LIKE         DEFINE    CLNTFP        KCLTFP
086700941110     C     *LIKE         DEFINE    CLNTFA        KCLTFA
086800941110     C     *LIKE         DEFINE    CLNANN        KCLANN
086900941110     C     *LIKE         DEFINE    CLNMES        KCLMES
087000930201     C     *LIKE         DEFINE    TBLKUT        KTBUTE
087100930201     C     *LIKE         DEFINE    TBLCOD        KTBCOD
087200930201     C     *LIKE         DEFINE    TBLKEY        KTBKEY
087300941110     C     *LIKE         DEFINE    CASPTA        KCSPTA
087400941110     C     *LIKE         DEFINE    CASLNP        KCSLNP
087500941110     C     *LIKE         DEFINE    CASLNA        KCSLNA
087600941110     C     *LIKE         DEFINE    CASTSP        KCSTSP
087700941110     C     *LIKE         DEFINE    CASDDE        KCSDDE
087800930201     C*
087900930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
088000930201     C*   -NOME PGM CHIAMANTE
088100930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
088200941110     C                   MOVEL     'TNSD76R'     DSMSPG
088300941110     C                   MOVEL     'TNSD76R'     MSGQUE
088400930201     C                   MOVEL     *BLANKS       MSGKEY
088500930201     C                   MOVEL     *BLANKS       DSMSMS
088600930202     C*
088700930202     C* SE VISUALIZZAZIONE TOLGO FUNZIONE AGGIORNAMENTO
088800930401     C     DSCOPZ        IFEQ      'O05'
088900930202     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
089000930202     C                   MOVEL     'N'           FUDATT
089100930202     C                   MOVEL     FUDRIG        FUD(6)
089200930202     C                   END
089300930201     C*
089400930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
089500930201     C                   Z-ADD     0             FUNGRU            5 0
089600930201     C*
089700930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
089800930201     C                   EXSR      FUNGES
089900930201     C*
090000930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
090100930201     C                   MOVEL     '0'           F12L00            1
090200930201     C                   MOVEL     '0'           F03L00            1
090300930201     C                   MOVEL     '0'           FINL00            1
090400930201     C                   MOVEL     '0'           ERRL00            1
090500930201     C                   MOVEL     '0'           FLFINE            1
090600930204     C*
090700930204     C* LANCIO ROUTINE PER VERIFICARE SE
090800930204     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
090900930204     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
091000930204     C                   CALL      'XFIL'
091100930204     C                   PARM                    DSFIL
091200930204     C                   Z-ADD     DSFICD        CODUT             1 0
091300930201     C*
091400930201     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
091500930201     C* 0=NON PROTETTO, 1=PROTETTO
091600930401     C*   90 - CHIAVE CALENDARIO
091700930401     C*   91 - PARAMETRI CALENDARIO
091800930201     C*
091900930401     C     DSCOPZ        IFEQ      'F06'                                        *INS.
092000930401     C                   SETOFF                                       9091
092100930201     C                   END
092200930201     C*
092300930401     C     DSCOPZ        IFEQ      'O02'                                        *MOD.
092400930201     C                   SETON                                        90
092500930401     C                   SETOFF                                       91
092600930401     C                   END
092700930201     C*
092800930401     C     DSCOPZ        IFEQ      'O05'                                        *VIS+CANC+RIPR
092900930401     C     DSCOPZ        OREQ      'O04'
093000930401     C     DSCOPZ        OREQ      'O07'
093100930401     C                   SETON                                        9091
093200930201     C                   END
093300930201     C*
093400930401     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO DATI RECORD
093500930401     C     DSCOPZ        IFEQ      'O02'                                        --- 1 -->
093600930401     C     DSCOPZ        OREQ      'O04'
093700930401     C     DSCOPZ        OREQ      'O05'
093800930401     C     DSCOPZ        OREQ      'O07'
093900930401     C* SE IL CALENDARIO NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
094000941110     C                   Z-ADD     DSCPAR        KCLTFP
094100941110     C                   Z-ADD     DSCARR        KCLTFA
094200941110     C                   Z-ADD     DSCANN        KCLANN
094300941110     C                   Z-ADD     DSCMES        KCLMES
094400941110     C     KEYCLN        CHAIN     AZCLN01L                           99
094500930201     C     *IN99         IFEQ      '1'
094600930201     C                   MOVEL     '1'           ERRL00
094700930225     C                   MOVEL     MSG(10)       DSMSMS
094800930202     C                   ELSE
094900930401     C                   EXSR      RECVID
095000930201     C                   END
095100930201     C*
095200930201     C     ERRL00        IFEQ      '0'                                          --- 2 -->
095300930208     C*
095400930208     C* SE TUTTO OK, DECODIFICO I CAMPI A VIDEO
095500930208     C                   EXSR      DECODI
095600930201     C*
095700930201     C                   END                                                    <-- 2 ---
095800930202     C                   END                                                    <-- 1 ---
095900930202     C*
096000930202     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
096100930401     C     DSCOPZ        IFEQ      'F06'
096200930202     C                   EXSR      AZZVID
096300930202     C                   END
096400930201     C*
096500930201     C* IMPOSTO DESCRIZIONE DELLA FUNZIONE IN CORSO A VIDEO
096600930401     C     DSCOPZ        IFEQ      'F06'
096700930201     C                   MOVEL     ' INSERIM'    VIDFUN
096800930201     C                   MOVE      'ENTO   '     VIDFUN
096900930201     C                   END
097000930401     C     DSCOPZ        IFEQ      'O02'
097100930201     C                   MOVEL     '   MODIF'    VIDFUN
097200930201     C                   MOVE      'ICA    '     VIDFUN
097300930201     C                   END
097400930401     C     DSCOPZ        IFEQ      'O04'
097500930201     C                   MOVEL     ' CANCELL'    VIDFUN
097600930201     C                   MOVE      'AZIONE '     VIDFUN
097700930201     C                   END
097800930401     C     DSCOPZ        IFEQ      'O05'
097900930201     C                   MOVEL     'VISUALIZ'    VIDFUN
098000930201     C                   MOVE      'ZAZIONE'     VIDFUN
098100930201     C                   END
098200930401     C     DSCOPZ        IFEQ      'O07'
098300930201     C                   MOVEL     ' RIPRIST'    VIDFUN
098400930201     C                   MOVE      'INO    '     VIDFUN
098500930201     C                   END
098600000000     C*
098700000000     C                   ENDSR
098800930401     C*================================================================
098900930401     C*
099000930401     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099100930401     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099200930401     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
099300930401     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
099400930401     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
099500930401     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099600930401     C*§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099700930401     C*
099800000000     C*================================================================
099900000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
100000000000     C*================================================================
100100000000     C     FUNGES        BEGSR
100200000000     C*
100300000000     C* PULISCO CAMPI DI LAVORO
100400000000     C                   Z-ADD     0             W                 5 0
100500000000     C*
100600000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
100700000000     C* E CARICO TABELLA FUNZIONI ATTIVE
100800000000     C     1             DO        24            I
100900000000     C                   MOVEL     FUD(I)        FUDRIG
101000000000     C*
101100000000     C* PULIZIA FUNZIONI PRECEDENTI
101200000000     C                   MOVEL     *BLANKS       FUV(I)
101300000000     C*
101400000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
101500000000     C* (SALTA F24)
101600000000     C     FUDATT        IFEQ      'S'
101700000000     C     FUDFUN        ANDNE     '24'
101800000000     C                   ADD       1             W
101900000000     C                   MOVEL     FUDDES        FUV(W)
102000000000     C                   END
102100000000     C                   END                                                    DO WHILE
102200000000     C*
102300000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
102400000000     C* ALTRIMENTI DISATTIVO F24
102500000000     C     1             DO        24            I
102600000000     C                   MOVEL     FUD(I)        FUDRIG
102700000000     C*
102800000000     C     FUDFUN        IFEQ      '24'
102900000000     C     W             IFGT      6
103000000000     C                   MOVEL     'S'           FUDATT
103100000000     C                   ELSE
103200000000     C                   MOVEL     *BLANKS       FUDATT
103300000000     C                   END                                                    W GT 8
103400000000     C                   MOVEL     FUDRIG        FUD(I)
103500000000     C                   END
103600000000     C                   END                                                    DO WHILE
103700000000     C*
103800000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
103900000000     C     W             DIV       6             I
104000000000     C                   MVR                     Z
104100000000     C*
104200000000     C     Z             IFNE      0
104300000000     C     I             ADD       1             Z
104400000000     C                   ELSE
104500000000     C                   Z-ADD     I             Z                 5 0
104600000000     C                   END
104700000000     C*
104800000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
104900000000     C                   ADD       1             FUNGRU
105000000000     C     FUNGRU        IFGT      Z
105100000000     C     FUNGRU        ORLT      1
105200000000     C                   Z-ADD     1             FUNGRU
105300000000     C                   END
105400000000     C*
105500000000     C* PRIMO GRUPPO DI 6 FUNZIONI
105600000000     C     FUNGRU        IFEQ      1
105700000000     C                   MOVEL     FUV(1)        FUN001
105800000000     C                   MOVEL     FUV(2)        FUN002
105900000000     C                   MOVEL     FUV(3)        FUN003
106000000000     C                   MOVEL     FUV(4)        FUN004
106100000000     C                   MOVEL     FUV(5)        FUN005
106200000000     C                   MOVEL     FUV(6)        FUN006
106300000000     C                   END
106400000000     C*
106500000000     C* SECONDO GRUPPO DI 6 FUNZIONI
106600000000     C     FUNGRU        IFEQ      2
106700000000     C                   MOVEL     FUV(7)        FUN001
106800000000     C                   MOVEL     FUV(8)        FUN002
106900000000     C                   MOVEL     FUV(9)        FUN003
107000000000     C                   MOVEL     FUV(10)       FUN004
107100000000     C                   MOVEL     FUV(11)       FUN005
107200000000     C                   MOVEL     FUV(12)       FUN006
107300000000     C                   END
107400000000     C*
107500000000     C* TERZO GRUPPO DI 6 FUNZIONI
107600000000     C     FUNGRU        IFEQ      3
107700000000     C                   MOVEL     FUV(13)       FUN001
107800000000     C                   MOVEL     FUV(14)       FUN002
107900000000     C                   MOVEL     FUV(15)       FUN003
108000000000     C                   MOVEL     FUV(16)       FUN004
108100000000     C                   MOVEL     FUV(17)       FUN005
108200000000     C                   MOVEL     FUV(18)       FUN006
108300000000     C                   END
108400000000     C*
108500000000     C* QUARTO GRUPPO DI 6 FUNZIONI
108600000000     C     FUNGRU        IFEQ      4
108700000000     C                   MOVEL     FUV(19)       FUN001
108800000000     C                   MOVEL     FUV(20)       FUN002
108900000000     C                   MOVEL     FUV(21)       FUN003
109000000000     C                   MOVEL     FUV(22)       FUN004
109100000000     C                   MOVEL     *BLANKS       FUN005
109200000000     C                   MOVEL     *BLANKS       FUN006
109300000000     C                   END
109400000000     C*
109500000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
109600000000     C                   MOVEL     *BLANKS       FUN007
109700000000     C                   MOVEL     *BLANKS       FUN008
109800000000     C*
109900000000     C     1             DO        24            I
110000000000     C                   MOVEL     FUD(I)        FUDRIG
110100000000     C     FUDFUN        IFEQ      '24'
110200000000     C     FUDATT        ANDEQ     'S'
110300000000     C                   MOVEL     FUDDES        FUN008
110400000000     C                   END
110500000000     C                   END                                                    DO
110600000000     C                   ENDSR
110700000000     C*================================================================
110800000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
110900000000     C*================================================================
111000000000     C     FUNCON        BEGSR
111100000000     C*
111200000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
111300000000     C                   EXSR      FUNVAR
111400000000     C*
111500000000     C* PULISCO CAMPI DI LAVORO
111600000000     C                   Z-ADD     0             I                 5 0
111700000000     C                   Z-ADD     0             W                 5 0
111800000000     C*
111900000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
112000000000     C     FUNKEY        IFNE      *BLANKS
112100000000     C     1             DO        24            I
112200000000     C                   MOVEL     FUD(I)        FUDRIG
112300000000     C*
112400000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
112500000000     C     FUNKEY        IFEQ      FUDFUN
112600000000     C     FUDATT        ANDEQ     'S'
112700000000     C                   Z-ADD     I             W
112800000000     C                   END
112900000000     C                   END                                                    DO WHILE
113000000000     C*
113100000000     C* FUNZIONE NON ATTIVA (ERRORE)
113200000000     C     W             IFEQ      0
113300000000     C*
113400000000     C* EMISSIONE MESSAGGIO DI ERRORE
113500930201     C                   MOVEL     MSG(1)        DSMSMS
113600930201     C*
113700930201     C                   END                                                    W=0
113800000000     C                   END                                                    FUNKEY NE *BLANKS
113900000000     C*
114000000000     C* FUNZIONE ATTIVA (PROCEDI ...)
114100000000     C     W             IFNE      0
114200000000     C     FUNKEY        OREQ      *BLANKS
114300000000     C*
114400000000     C     FUNKEY        IFEQ      *BLANKS
114500000000     C                   EXSR      ENTGES
114600000000     C                   END
114700000000     C     FUNKEY        IFEQ      '01'
114800000000     C                   EXSR      F01GES
114900000000     C                   END
115000000000     C     FUNKEY        IFEQ      '03'
115100000000     C                   EXSR      F03GES
115200000000     C                   END
115300930202     C     FUNKEY        IFEQ      '06'
115400930202     C                   EXSR      F06GES
115500930202     C                   END
115600000000     C     FUNKEY        IFEQ      '07'
115700000000     C                   EXSR      F07GES
115800000000     C                   END
115900000000     C     FUNKEY        IFEQ      '08'
116000000000     C                   EXSR      F08GES
116100000000     C                   END
116200000000     C     FUNKEY        IFEQ      '12'
116300000000     C                   EXSR      F12GES
116400000000     C                   END
116500930201     C     FUNKEY        IFEQ      '21'
116600930201     C                   EXSR      F21GES
116700930201     C                   END
116800000000     C     FUNKEY        IFEQ      '22'
116900000000     C                   EXSR      F22GES
117000000000     C                   END
117100000000     C     FUNKEY        IFEQ      '24'
117200000000     C                   EXSR      F24GES
117300000000     C                   END
117400000000     C     FUNKEY        IFEQ      'HP'
117500000000     C                   EXSR      HELGES
117600000000     C                   END
117700000000     C*
117800000000     C                   END
117900000000     C*
118000000000     C                   ENDSR
118100000000     C*================================================================
118200000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
118300000000     C*================================================================
118400000000     C     FUNVAR        BEGSR
118500000000     C*
118600000000     C* INIZIALIZZA VARIABILE -FUNKEY-
118700000000     C*
118800000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
118900000000     C     KEY           IFEQ      ENTER
119000000000     C                   MOVEL     *BLANKS       FUNKEY
119100000000     C                   END
119200000000     C     KEY           IFEQ      F1
119300000000     C                   MOVEL     '01'          FUNKEY
119400000000     C                   END
119500000000     C     KEY           IFEQ      F2
119600000000     C                   MOVEL     '02'          FUNKEY
119700000000     C                   END
119800000000     C     KEY           IFEQ      F3
119900000000     C                   MOVEL     '03'          FUNKEY
120000000000     C                   END
120100000000     C     KEY           IFEQ      F4
120200000000     C                   MOVEL     '04'          FUNKEY
120300000000     C                   END
120400000000     C     KEY           IFEQ      F5
120500000000     C                   MOVEL     '05'          FUNKEY
120600000000     C                   END
120700000000     C     KEY           IFEQ      F6
120800000000     C                   MOVEL     '06'          FUNKEY
120900000000     C                   END
121000000000     C     KEY           IFEQ      F7
121100000000     C                   MOVEL     '07'          FUNKEY
121200000000     C                   END
121300000000     C     KEY           IFEQ      F8
121400000000     C                   MOVEL     '08'          FUNKEY
121500000000     C                   END
121600000000     C     KEY           IFEQ      F9
121700000000     C                   MOVEL     '09'          FUNKEY
121800000000     C                   END
121900000000     C     KEY           IFEQ      F10
122000000000     C                   MOVEL     '10'          FUNKEY
122100000000     C                   END
122200000000     C     KEY           IFEQ      F11
122300000000     C                   MOVEL     '11'          FUNKEY
122400000000     C                   END
122500000000     C     KEY           IFEQ      F12
122600000000     C                   MOVEL     '12'          FUNKEY
122700000000     C                   END
122800000000     C     KEY           IFEQ      F13
122900000000     C                   MOVEL     '13'          FUNKEY
123000000000     C                   END
123100000000     C     KEY           IFEQ      F14
123200000000     C                   MOVEL     '14'          FUNKEY
123300000000     C                   END
123400000000     C     KEY           IFEQ      F15
123500000000     C                   MOVEL     '15'          FUNKEY
123600000000     C                   END
123700000000     C     KEY           IFEQ      F16
123800000000     C                   MOVEL     '16'          FUNKEY
123900000000     C                   END
124000000000     C     KEY           IFEQ      F17
124100000000     C                   MOVEL     '17'          FUNKEY
124200000000     C                   END
124300000000     C     KEY           IFEQ      F18
124400000000     C                   MOVEL     '18'          FUNKEY
124500000000     C                   END
124600000000     C     KEY           IFEQ      F19
124700000000     C                   MOVEL     '19'          FUNKEY
124800000000     C                   END
124900000000     C     KEY           IFEQ      F20
125000000000     C                   MOVEL     '20'          FUNKEY
125100000000     C                   END
125200000000     C     KEY           IFEQ      F21
125300000000     C                   MOVEL     '21'          FUNKEY
125400000000     C                   END
125500000000     C     KEY           IFEQ      F22
125600000000     C                   MOVEL     '22'          FUNKEY
125700000000     C                   END
125800000000     C     KEY           IFEQ      F23
125900000000     C                   MOVEL     '23'          FUNKEY
126000000000     C                   END
126100000000     C     KEY           IFEQ      F24
126200000000     C                   MOVEL     '24'          FUNKEY
126300000000     C                   END
126400000000     C     KEY           IFEQ      ROLLUP
126500000000     C                   MOVEL     'RU'          FUNKEY
126600000000     C                   END
126700000000     C     KEY           IFEQ      HELP
126800000000     C                   MOVEL     'HP'          FUNKEY
126900000000     C                   END
127000000000     C                   ENDSR
127100000000     C*================================================================
127200000000     C* NOTE PER COMPILAZIONE SCHIERE
127300000000     C*================================================================
127400000000     C* -MSG-
127500000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
127600000000     C* (NON TOCCARE MSG,1)
127700000000     C*
127800000000     C* -CMD-
127900000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
128000000000     C*
128100000000     C* -FUD-
128200000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
128300000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
128400000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
128500000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
128600000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
128700000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
128800000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
128900000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
129000000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
129100000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
129200000000     C*
129300000000     C*================================================================
129400000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
129500941110TNSD76R- Tasto funzionale non gestito                                          1
129600941110TNSD76R- Calendario già inserito per questa/e filiale/i.                       2
129700941110TNSD76R- Inserimento effettuato.                                               3
129800941110TNSD76R- Modifica effettuata.                                                  4
129900941110TNSD76R- Cancellazione effettuata.                                             5
130000941110TNSD76R- Ripristino effettuato.                                                6
130100941110TNSD76R- Linea di arrivo errata.                                               7
130200941110TNSD76R- Non trovato calendario banche. Avvertire CED.                         8
130300941110TNSD76R- Almeno uno dei simboli utilizzati non è accettabile.                  9
130400941110TNSD76R- Record non trovato.                                                  10
130500941110TNSD76R- Combinazione terminal partenza/arrivo non presente archivio trazioni.11
130600941110TNSD76R- Anno non accettabile.                                                12
130700941110TNSD76R- Mese non accettabile.                                                13
130800941110TNSD76R- Almeno uno dei giorni festivi non ha un flag nel calendario.         14
130900941110TNSD76R-                                                                      15
131000941110TNSD76R-                                                                      16
131100941110TNSD76R-                                                                      17
131200941110TNSD76R-                                                                      18
131300941110TNSD76R-                                                                      19
131400941110TNSD76R-                                                                      20
131500000000** ======== SCHIERA: CMD  (COMANDI)                        ================
131600000000WRKSPLF
131700000000
131800000000
131900000000
132000000000
132100000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
132200930201                   -        (S/N)=N                        funz.01¬
132300000000                   -        (S/N)=N                        funz.02¬
132400000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
132500000000                   -        (S/N)=N                        funz.04¬
132600000000                   -        (S/N)=N                        funz.05¬
132700930202F06=Aggiornamento  -        (S/N)=S                        funz.06¬
132800930201                   -        (S/N)=N                        funz.07¬
132900930401                   -        (S/N)=N                        funz.08¬
133000000000                   -        (S/N)=N                        funz.09¬
133100000000                   -        (S/N)=N                        funz.10¬
133200000000                   -        (S/N)=N                        funz.11¬
133300930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
133400000000                   -        (S/N)=N                        funz.13¬
133500000000                   -        (S/N)=N                        funz.14¬
133600000000                   -        (S/N)=N                        funz.15¬
133700000000                   -        (S/N)=N                        funz.16¬
133800000000                   -        (S/N)=N                        funz.17¬
133900000000                   -        (S/N)=N                        funz.18¬
134000000000                   -        (S/N)=N                        funz.19¬
134100000000                   -        (S/N)=N                        funz.20¬
134200930401                   -        (S/N)=N                        funz.21¬
134300930225F22=Gestione stampe-        (S/N)=N                        funz.22¬
134400000000                   -        (S/N)=N                        funz.23¬
134500000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
