000100000000     H*------------------------------------------------------------------------*
000200000000     H* AUTOMATISMI CALENDARIO
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNSD77D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(SD77DMS:MSGULT)
001000000000     FAZCLN01L  UF A E           K DISK
001100971202     FAZCAE01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300000000     FAZORG01L  IF   E           K DISK
001400991224     FANBCA01L  IF   E           K DISK
001500000000     D*------------------------------------------------------------------------*
001600991224     D* DATA STRUCTURE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*--------------
001900000000     D* MESSAGGI
002000000000     D*--------------
002100000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200000000     D*--------------
002300000000     D* COMANDI
002400000000     D*--------------
002500000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002600000000     D*--------------
002700000000     D* DESCRIZIONI TASTI FUNZIONALI
002800000000     D*--------------
002900000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003000000000     D*--------------
003100000000     D* TASTI FUNZIONALI
003200000000     D*--------------
003300000000     D FUV             S             19    DIM(24)
003400971202     D*-------------------
003500971202     D* FILIALI E TERMINAL
003600971202     D*-------------------
003700971202     D FIL             S              3  0 DIM(500)                             *FILIALE
003800971202     D TFP             S              3  0 DIM(500)                             *TERMINAL PARTENZA
003900971202     D TFA             S              3  0 DIM(500)                             *TERMINAL ARRIVO
004000971202     D*--------------
004100971202     D* FILIALI E TERMINAL DA ELABORARE
004200971202     D*--------------
004300971203     D TP              S              3  0 DIM(5000)                            *PARTENZA
004400971203     D TA              S              3  0 DIM(5000)                            *ARRIVO
004500971203     D*--------------
004600971203     D* COMBINAZIONE PARTENZA / ARRIVO
004700971203     D*--------------
004800971203     D CPA             S              6  0 DIM(5000)
004900000000     D*--------------
005000000000     D* GIORNI DELLA SETTIMANA DI OGNI MESE DELL'ANNO
005100000000     D*--------------
005200000000     D SE              S             31    DIM(12)
005300000000     D*-------------------
005400000000     D* RIDEFINIZIONE SCHIERA -FUD-
005500000000     D*-------------------
005600000000     D FUDDS           DS
005700000000     D  FUDDE1                 1      1
005800000000     D  FUDFUN                 2      3
005900000000     D  FUDDE2                 4     19
006000000000     D  FUDFI0                20     34
006100000000     D  FUDATT                35     35
006200000000     D  FUDFI1                36     80
006300000000     D  FUDDES                 1     19
006400000000     D  FUDRIG                 1     80
006500000000     D*-------------------
006600000000     D* DS "XMSG" - GESTIONE MESSAGGI PGM
006700000000     D*-------------------
006800000000     D DSMSG         E DS
006900000000     D*-------------------
007000000000     D* DS GESTIONE TASTI FUNZIONALI
007100000000     D*-------------------
007200000000     D DSKEY         E DS
007300000000     D DEVDS1          DS
007400000000     D  KEY                  369    369
007500000000     D*-------------------
007600000000     D* RIDEFINIZIONE DATA
007700000000     D*-------------------
007800000000     D                 DS
007900000000     D  WGG                    1      2  0
008000000000     D  WMM                    3      4  0
008100000000     D  WAA                    5      6  0
008200991224     D  WDATA                  1      6  0
008300000000     D*-------------------
008400000000     D* RIDEFINIZIONE DATA VIDEO
008500000000     D*-------------------
008600000000     D                 DS
008700000000     D  VIDGGD                 1      2  0
008800000000     D  VIDMMD                 3      4  0
008900000000     D  VIDAAD                 5      8  0
009000000000     D  VIDDAT                 1      8  0
009100000000     D*-------------------
009200000000     D* DS GIORNI FESTIVI
009300000000     D*-------------------
009400000000     D GFEDS           DS
009500000000     D  GFE                    1     31
009600000000     D                                     DIM(31)
009700000000     D*-------------------
009800000000     D* DS GIORNI SETTIMANA
009900000000     D*-------------------
010000000000     D GSEDS           DS
010100000000     D  GSE                    1     31
010200000000     D                                     DIM(31)
010300000000     D*-------------------
010400000000     D* DS FLAG MATTINA
010500000000     D*-------------------
010600000000     D GMADS           DS
010700000000     D  GMA                    1     31
010800000000     D                                     DIM(31)
010900000000     D*-------------------
011000000000     D* DS FLAG POMERIGGIO
011100000000     D*-------------------
011200000000     D GPODS           DS
011300000000     D  GPO                    1     31
011400000000     D                                     DIM(31)
011500000000     D*-------------------
011600000000     D* DS SIMBOLI CALENDARIO
011700000000     D*-------------------
011800000000     D DS1Y          E DS
011900000000     D*------------------
012000000000     D* DS "FNLV55R" - REPERIMENTO TERMINALs
012100000000     D*------------------
012200000000     D DSLV55        E DS                  EXTNAME(FNLV55DS)
012300000000     D*-------------------
012400000000     D* ARCHITETTURA
012500000000     D*-------------------
012600000000     D KPJBA         E DS
012700991224     d*---
012800991224     d* ds controlla data (8)
012900991224     d*---
013000991224     d wlbda8          DS                  INZ                                  *controlla data
013100991224     d  g08dat                        8  0
013200991224     d  g08inv                        8  0
013300991224     d  g08err                        1
013400991224     d  g08tgi                        5  0
013500991224     d*---
013600991224     d* variabili
013700991224     d*---
013800991224     d n14             S             14  0 INZ                                  *numerico 14
013900991224     d n8              S              8  0 INZ                                  *numerico 8
014000991224     d n4              S              4  0 INZ                                  *numerico 4
014100991224     d datcor          S              8  0 INZ                                  *data correrte a/m/g
014200991224     d*---
014300991224     d* Variabili riferite al data base
014400991224     d*---
014500991224     d KCACALEND       S                   LIKE(BCACALEND)                      *ANBCA01L
014600991224     d KCAANNO         S                   LIKE(BCAANNO)
014700991224     d KCAMESE         S                   LIKE(BCAMESE)
014800000000     C*------------------------------------------------------------------------*
014900991224     C* MAIN LINES
015000000000     C*------------------------------------------------------------------------*
015100000000     C*
015200000000     C* SE CONTROLLI INIZIALI NON OK, RITORNO A PGM CHIAMANTE
015300000000IF  1C     ERRL00        IFEQ      '1'
015400000000     C*
015500000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
015600000000     C                   MOVEL     '1'           DSCERR
015700000000     C                   MOVEL     DSMSMS        DSCMSG
015800000000     C*
015900000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
016000000000X   1C                   ELSE
016100000000DO  2C     F03L00        DOUEQ     '1'
016200000000     C     F12L00        OREQ      '1'
016300000000     C     FLFINE        OREQ      '1'
016400000000     C*
016500000000     C* EMETTE FORMATO VIDEO
016600000000     C                   EXSR      BUTFMR
016700000000     C*
016800000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
016900000000     C                   EXSR      FUNCON
017000000000E   2C                   ENDDO
017100000000E   1C                   ENDIF
017200000000     C* FNLV55R
017300000000     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
017400000000     C                   MOVEL     'C'           D55TLA
017500000000     C                   CALL      'FNLV55R'
017600000000     C                   PARM                    DSLV55
017700000000     C*
017800000000     C                   SETON                                        LR
017900000000     C*------------------------------------------------------------------------*
018000000000     C* BUTMFR - EMISSIONE FORMATO A VIDEO
018100000000     C*------------------------------------------------------------------------*
018200000000     C     BUTFMR        BEGSR
018300000000     C*
018400000000     C* WRITE RECORD TESTATA
018500000000     C                   WRITE     SD77D01
018600000000     C*
018700000000     C* VISUALIZZAZIONE MESSAGGIO
018800000000     C                   EXSR      SNDMSG
018900000000     C*
019000000000     C* WRITE RECORD FUNZIONI
019100000000     C                   WRITE     SD77D02
019200000000     C*
019300000000     C* EXFMT RECORD DETTAGLIO
019400000000     C                   EXFMT     SD77D03
019500000000     C*
019600000000     C* AZZERO FLAG DI ERRORE LIVELLO
019700000000     C                   MOVEL     '0'           ERRL00
019800000000     C*
019900000000     C* AZZERO FLAG DI ERRORE
020000000000     C                   SETOFF                                       606162
020100000000     C                   SETOFF                                       636465
020200000000     C                   SETOFF                                       666768
020300000000     C                   SETOFF                                       697071
020400000000     C*
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* SNDMSG - MANDA MESSAGGIO
020800000000     C*------------------------------------------------------------------------*
020900000000     C     SNDMSG        BEGSR
021000000000     C*
021100000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
021200000000     C                   MOVEL     '1'           *IN40
021300000000     C                   WRITE     SD77DMC
021400000000     C                   MOVEL     '0'           *IN40
021500000000     C*
021600000000     C* VISUALIZZAZIONE MESSAGGIO
021700000000     C                   Z-ADD     0             MSGULT            4 0
021800000000     C                   CALL      'XMSG'        XMSGPA
021900000000     C     DSMSER        IFEQ      0
022000000000     C                   MOVEL     DSMSKE        MSGKEY
022100000000     C                   END
022200000000     C                   Z-ADD     1             MSGULT
022300000000     C                   WRITE     SD77DMS                                      SFL MESS
022400000000     C                   WRITE     SD77DMC                                      CONTR MESS
022500000000     C*
022600000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
022700000000     C                   MOVEL     *BLANKS       DSMSMS
022800000000     C*
022900000000     C                   ENDSR
023000000000     C*------------------------------------------------------------------------*
023100000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
023200000000     C*------------------------------------------------------------------------*
023300000000     C     ENTGES        BEGSR
023400000000     C*
023500000000     C* CONTROLLA I CAMPI IMMESSI
023600000000     C                   EXSR      DECODI
023700000000     C*
023800000000     C                   ENDSR
023900000000     C*------------------------------------------------------------------------*
024000000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
024100000000     C*------------------------------------------------------------------------*
024200000000     C     F01GES        BEGSR
024300000000     C                   ENDSR
024400000000     C*------------------------------------------------------------------------*
024500000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
024600000000     C*------------------------------------------------------------------------*
024700000000     C     F03GES        BEGSR
024800000000     C*
024900000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
025000000000     C                   MOVEL     '1'           F03L00
025100971127     C*
025200000000     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
025300000000     C                   MOVEL     '1'           DSCF03
025400000000     C*
025500000000     C                   ENDSR
025600000000     C*------------------------------------------------------------------------*
025700000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
025800000000     C*------------------------------------------------------------------------*
025900000000     C     F06GES        BEGSR
026000000000     C*
026100000000     C* CONTROLLA I CAMPI IMMESSI
026200000000     C                   EXSR      DECODI
026300000000     C*
026400000000     C* CARICA LA SCHIERA DELLE FILIALI
026500971127IF  1C     ERRL00        IFEQ      '0'
026600000000     C                   EXSR      CARFIL
026700971127IF  2C     ERRL00        IFEQ      '0'
026800000000     C*
026900000000     C* IN BASE AL TIPO DI AUTOMATISMO RICHIAMA LA ROUITINE INTERESSATA
027000971127IF  3C     VIDTIP        IFEQ      'C'
027100000000     C                   EXSR      RSTAND
027200971127E   3C                   ENDIF
027300971127IF  3C     VIDTIP        IFEQ      'D'
027400000000     C                   EXSR      RDATA
027500971127E   3C                   ENDIF
027600971127IF  3C     VIDTIP        IFEQ      'G'
027700000000     C                   EXSR      RGIORN
027800971127E   3C                   ENDIF
027900000000     C*
028000000000     C* MANDO MESSAGGIO DI AUTOMATISMO APPLICATO
028100971127IF  3C     ERRL00        IFEQ      '0'
028200000000     C                   MOVEL     MSG(3)        DSMSMS
028300971127E   3C                   ENDIF
028400971127E   2C                   ENDIF
028500971127E   1C                   ENDIF
028600000000     C*
028700000000     C                   ENDSR
028800000000     C*------------------------------------------------------------------------*
028900000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
029000000000     C*------------------------------------------------------------------------*
029100000000     C     F07GES        BEGSR
029200000000     C                   ENDSR
029300000000     C*------------------------------------------------------------------------*
029400000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
029500000000     C*------------------------------------------------------------------------*
029600000000     C     F08GES        BEGSR
029700000000     C                   ENDSR
029800000000     C*------------------------------------------------------------------------*
029900000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
030000000000     C*------------------------------------------------------------------------*
030100000000     C     F12GES        BEGSR
030200000000     C*
030300000000     C* IMPOSTO FLAG DI LIVELLO
030400000000     C                   MOVEL     '1'           F12L00
030500000000     C*
030600000000     C                   ENDSR
030700000000     C*------------------------------------------------------------------------*
030800000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA CAPPARIO
030900000000     C*------------------------------------------------------------------------*
031000000000     C     F21GES        BEGSR
031100000000     C                   ENDSR
031200000000     C*------------------------------------------------------------------------*
031300000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
031400000000     C*------------------------------------------------------------------------*
031500000000     C     F22GES        BEGSR
031600000000     C*
031700000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
031800000000     C                   Z-ADD     80            LENGH            15 5
031900000000     C                   CALL      'QCAEXEC'
032000000000     C                   PARM                    CMD(1)
032100000000     C                   PARM                    LENGH
032200000000     C*
032300000000     C                   ENDSR
032400000000     C*------------------------------------------------------------------------*
032500000000     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
032600000000     C*------------------------------------------------------------------------*
032700000000     C     F24GES        BEGSR
032800000000     C*
032900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
033000000000     C                   EXSR      FUNGES
033100000000     C*
033200000000     C                   ENDSR
033300000000     C*------------------------------------------------------------------------*
033400000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
033500000000     C*------------------------------------------------------------------------*
033600000000     C     HELGES        BEGSR
033700000000     C                   ENDSR
033800000000     C*------------------------------------------------------------------------*
033900000000     C* RGIORN - IMPOSTA UN FLAG IN UN GIORNO DELLA SETTIMANA
034000000000     C*------------------------------------------------------------------------*
034100000000     C     RGIORN        BEGSR
034200000000     C*
034300000000     C* CARICO IN SCHIERA TUTTI I GIORNI DELLA SETTIMANA DELL'ANNO
034400971127DO  1C                   DO        12            M                 4 0
034500991224     C                   Z-ADD     VIDAAG        WANN              4 0
034600000000     C                   Z-ADD     M             WMES              2 0
034700000000     C                   EXSR      IMPGFE                                       *GIORNI FESTIVI
034800000000     C                   EXSR      IMPGSE                                       *GIORNI SETTIMANA
034900000000     C                   MOVEL     GSEDS         SE(M)
035000971127E   1C                   ENDDO
035100000000     C*
035200000000     C* LEGGO TUTTO IL CALENDARIO E SE IL RECORD RIENTRA NEI PARAMETRI DATI
035300000000     C* A VIDEO LO AGGIORNO
035400000000     C                   Z-ADD     *ZEROS        KCATFP
035500000000     C                   Z-ADD     *ZEROS        KCATFA
035600000000     C                   Z-ADD     9999          KCAANN
035700000000     C                   Z-ADD     99            KCAMES
035800000000     C     KEYCAL        SETLL     AZCLN01L                           99        *FINE-ARCHIV
035900000000     C  N99              READ      AZCLN01L                               99
036000971127DO  1C     *IN99         DOWEQ     '0'
036100971127IF  2C     CLNATB        IFNE      'A'
036200971127IF  3C     CLNANN        IFEQ      VIDAAG
036300000000     C                   MOVE      CLNTFP        WCRPAR            3
036400000000     C                   MOVE      CLNTFA        WCRARR            3
036500000000     C                   EXSR      CHKREC
036600000000     C* AGGIORNO RECORD
036700971127IF  4C     WDAAGG        IFEQ      'S'
036800000000     C                   MOVEL     CLNMAT        GMADS
036900000000     C                   MOVEL     CLNPOM        GPODS
037000000000     C                   Z-ADD     CLNMES        MM                2 0
037100000000     C                   MOVEL     SE(MM)        GSEDS
037200971127DO  5C                   DO        31            GG                2 0
037300971127IF  6C     GSE(GG)       IFEQ      VIDGIO
037400000000     C                   MOVEL     VIDFMG        GMA(GG)
037500000000     C                   MOVEL     VIDFPG        GPO(GG)
037600971127E   6C                   ENDIF
037700971127E   5C                   ENDDO
037800000000     C                   MOVEL     GMADS         CLNMAT
037900000000     C                   MOVEL     GPODS         CLNPOM
038000000000     C                   MOVEL     *BLANKS       CLNFTR
038100000000     C                   Z-ADD     *ZEROS        CLNDTR
038200000000     C                   MOVEL     *BLANKS       CLNATB
038300000000     C                   UPDATE    AZCLN000
038400971127E   4C                   ENDIF
038500971127E   3C                   ENDIF
038600971127E   2C                   ENDIF
038700000000     C                   READ      AZCLN01L                               99
038800971127E   1C                   ENDDO
038900000000     C*
039000000000     C                   ENDSR
039100000000     C*------------------------------------------------------------------------*
039200000000     C* RDATA  - MODIFICA I FLAG DI UNA PARTICOLARE DATA
039300000000     C*------------------------------------------------------------------------*
039400000000     C     RDATA         BEGSR
039500000000     C*
039600000000     C* LEGGO TUTTO IL CALENDARIO E SE IL RECORD RIENTRA NEI PARAMETRI DATI
039700000000     C* A VIDEO LO AGGIORNO
039800000000     C                   Z-ADD     *ZEROS        KCATFP
039900000000     C                   Z-ADD     *ZEROS        KCATFA
040000000000     C                   Z-ADD     9999          KCAANN
040100000000     C                   Z-ADD     99            KCAMES
040200000000     C     KEYCAL        SETLL     AZCLN01L                           99        *FINE-ARCHIV
040300000000     C  N99              READ      AZCLN01L                               99
040400971127DO  1C     *IN99         DOWEQ     '0'
040500971127IF  2C     CLNATB        IFNE      'A'
040600971127IF  2C     CLNANN        IFEQ      VIDAAD
040700000000     C     CLNMES        ANDEQ     VIDMMD
040800000000     C                   MOVE      CLNTFP        WCRPAR
040900000000     C                   MOVE      CLNTFA        WCRARR
041000000000     C                   EXSR      CHKREC
041100000000     C* AGGIORNO RECORD
041200971127IF  3C     WDAAGG        IFEQ      'S'
041300000000     C                   MOVEL     CLNMAT        GMADS
041400000000     C                   MOVEL     CLNPOM        GPODS
041500971202     C                   Z-ADD     VIDGGD        K                 6 0
041600000000     C                   MOVEL     VIDFMD        GMA(K)
041700000000     C                   MOVEL     VIDFPD        GPO(K)
041800000000     C                   MOVEL     GMADS         CLNMAT
041900000000     C                   MOVEL     GPODS         CLNPOM
042000000000     C                   MOVEL     *BLANKS       CLNFTR
042100000000     C                   Z-ADD     *ZEROS        CLNDTR
042200000000     C                   MOVEL     *BLANKS       CLNATB
042300000000     C                   UPDATE    AZCLN000
042400971127E   3C                   ENDIF
042500971127E   2C                   ENDIF
042600971127E   1C                   ENDIF
042700000000     C*
042800000000     C* LEGGE IL RECORD SUCCESSIVO
042900000000     C                   READ      AZCLN01L                               99
043000000000     C*
043100000000     C                   END                                                    <-- 1 ---
043200000000     C*
043300000000     C                   ENDSR
043400971202     C*------------------------------------------------------------------------*
043500971202     C* CHKREC - CONTROLLA SE IL RECORD SODDISFA I PARAMETRI DI LANCIO
043600971202     C*------------------------------------------------------------------------*
043700971202     C     CHKREC        BEGSR
043800971202     C*
043900971202     C                   MOVEL     'N'           WDAAGG            1
044000971202     C     VIDTTF        IFEQ      'X'                                          --- 1 -->
044100971202     C                   MOVEL     'S'           WDAAGG
044200971202     C                   ELSE                                                   <-- 1 -->
044300971202     C     VIDTTF        IFEQ      'A'
044400971202     C     WCRPAR        IFEQ      *ZEROS
044500971202     C     WCRARR        IFNE      *ZEROS
044600971202     C                   MOVEL     'S'           WDAAGG
044700971202     C                   END
044800971202     C                   END
044900971202     C                   END
045000971202     C     VIDTTF        IFEQ      'P'
045100971202     C     WCRARR        IFEQ      *ZEROS
045200971202     C     WCRPAR        IFNE      *ZEROS
045300971202     C                   MOVEL     'S'           WDAAGG
045400971202     C                   END
045500971202     C                   END
045600971202     C                   END
045700971202     C     VIDTTF        IFEQ      'T'
045800971202     C     WCRPAR        IFNE      *ZEROS
045900971202     C     WCRARR        IFNE      *ZEROS
046000971202     C                   MOVEL     'S'           WDAAGG
046100971202     C                   END
046200971202     C                   END
046300971202     C                   END
046400971202     C     WCRPAR        IFEQ      VIDPAR
046500971202     C     WCRARR        ANDEQ     VIDARR
046600971202     C                   MOVEL     'S'           WDAAGG
046700971202     C                   END
046800971202     C     WCRPAR        IFEQ      VIDPAR
046900971202     C     WCRARR        ANDNE     *ZEROS
047000971202     C     VIDARR        IFEQ      '999'
047100971202     C     VIDARR        OREQ      '888'
047200971202     C                   MOVEL     'S'           WDAAGG
047300971202     C                   END
047400971202     C                   END
047500971202     C     WCRARR        IFEQ      VIDARR
047600971202     C     WCRPAR        ANDNE     *ZEROS
047700971202     C     VIDPAR        IFEQ      '999'
047800971202     C     VIDPAR        OREQ      '888'
047900971202     C                   MOVEL     'S'           WDAAGG
048000971202     C                   END
048100971202     C                   END
048200971202     C*
048300971202     C                   END                                                    <-- 1 ---
048400971202     C*
048500971202     C                   ENDSR
048600000000     C*------------------------------------------------------------------------*
048700000000     C* RSTAND - CREAZIONE CALENDARIO STANDARD PER UN ANNO
048800000000     C*------------------------------------------------------------------------*
048900000000     C     RSTAND        BEGSR
049000000000     C*
049100000000     C* LOOP PER DODICI MESI
049200971203DO  1C                   DO        12            M                 4 0          --- 1 -->
049300991224     C                   Z-ADD     VIDAAC        WANN              4 0
049400000000     C                   Z-ADD     M             WMES              2 0
049500000000     C*
049600000000     C* LANCIO ROUTINE PER IMPOSTARE GIORNI FESTIVI DEL MESE
049700000000     C                   EXSR      IMPGFE
049800000000     C*
049900000000     C* LANCIO ROUTINE PER IMPOSTARE GIORNI SETTIMANA DEL MESE
050000000000     C                   EXSR      IMPGSE
050100971203     C*---
050200971203     C* SCRITTURA PER TIPO TEMPO
050300971203     C*---
050400971203IF  2C     VIDPAR        IFEQ      *ZEROS
050500000000     C     VIDARR        ANDEQ     *ZEROS
050600000000     C                   EXSR      INSER0
050700971203DO  3C     1             DO        WUTP          J
050800971203     C                   Z-ADD     TP(J)         WPAR              3 0
050900971203     C                   Z-ADD     TA(J)         WARR              3 0
051000971203     C                   EXSR      INSERI
051100971203E   3C                   ENDDO                                                  *FINE TERMINAL
051200971203     C*---
051300971203     C* SCRITTURA PER TIPO TERMINAL / TERMINAL
051400971203     C*---
051500971203X   2C                   ELSE
051600971203     C*
051700971203     C* CICLO FINO A FINE TERMINAL PARTENZA
051800971203DO  3C     1             DO        WUTP          J
051900971203     C*
052000971203     C* CICLO FINO A FINE TERMINAL ARRIVO
052100971203DO  4C     1             DO        WUTA          K                 6 0
052200000000     C*
052300971203     C* SCRITTURA CALENDARIO
052400971203     C                   Z-ADD     TP(J)         WPAR              3 0
052500971203     C                   Z-ADD     TA(K)         WARR              3 0
052600000000     C                   EXSR      INSERI
052700971203E   4C                   ENDDO                                                  *FINE ARRIVO
052800971203E   3C                   ENDDO                                                  *FINE PARTENZA
052900971203E   2C                   ENDIF
053000971203E   1C                   ENDDO                                                  *FINE MESI
053100000000     C*
053200000000     C                   ENDSR
053300000000     C*------------------------------------------------------------------------*
053400000000     C* INSERI - CREA UN RECORD NEL CALEDARIO
053500000000     C*------------------------------------------------------------------------*
053600000000     C     INSERI        BEGSR
053700000000     C*
053800000000     C                   MOVE      WPAR          KCATFP
053900000000     C                   MOVE      WARR          KCATFA
054000991224     C                   Z-ADD     WANN          KCAANN
054100000000     C                   Z-ADD     WMES          KCAMES
054200000000     C     KEYCAL        CHAIN     AZCLN01L                           99
054300000000     C                   MOVE      WPAR          CLNTFP
054400000000     C                   MOVE      WARR          CLNTFA
054500000000     C                   Z-ADD     KCAANN        CLNANN
054600000000     C                   Z-ADD     WMES          CLNMES
054700000000     C                   MOVEL     GFEDS         CLNMAT
054800000000     C                   MOVEL     GFEDS         CLNPOM
054900000000     C                   MOVEL     *BLANKS       CLNFTR
055000000000     C                   Z-ADD     *ZEROS        CLNDTR
055100000000     C                   MOVEL     *BLANKS       CLNATB
055200000000     C     *IN99         IFEQ      '1'
055300000000     C                   WRITE     AZCLN000
055400000000     C                   ELSE
055500000000     C                   UPDATE    AZCLN000
055600000000     C                   END
055700000000     C*
055800000000     C                   ENDSR
055900000000     C*------------------------------------------------------------------------*
056000000000     C* INSER0 - CREA UN RECORD DI TIPO MESE NEL CALEDARIO
056100000000     C*------------------------------------------------------------------------*
056200000000     C     INSER0        BEGSR
056300000000     C*
056400000000     C                   Z-ADD     *ZEROS        KCATFP
056500000000     C                   Z-ADD     *ZEROS        KCATFA
056600991224     C                   Z-ADD     WANN          KCAANN
056700000000     C                   Z-ADD     WMES          KCAMES
056800000000     C     KEYCAL        CHAIN     AZCLN01L                           99
056900000000     C                   Z-ADD     *ZEROS        CLNTFP
057000000000     C                   Z-ADD     *ZEROS        CLNTFA
057100000000     C                   Z-ADD     KCAANN        CLNANN
057200000000     C                   Z-ADD     WMES          CLNMES
057300000000     C                   MOVEL     GSEDS         CLNMAT
057400000000     C                   MOVEL     GFEDS         CLNPOM
057500000000     C                   MOVEL     *BLANKS       CLNFTR
057600000000     C                   Z-ADD     *ZEROS        CLNDTR
057700000000     C                   MOVEL     *BLANKS       CLNATB
057800000000     C     *IN99         IFEQ      '1'
057900000000     C                   WRITE     AZCLN000
058000000000     C                   ELSE
058100000000     C                   UPDATE    AZCLN000
058200000000     C                   END
058300000000     C*
058400000000     C                   ENDSR
058500000000     C*------------------------------------------------------------------------*
058600000000     C* IMPGFE - IMPOSTA GIORNI FESTIVI
058700000000     C*------------------------------------------------------------------------*
058800000000     C     IMPGFE        BEGSR
058900000000     C*
059000000000     C* LEGGE SUL FILE BNBCA00F IL RECORD DEL MESE
059100991224     C                   MOVEL     'CB'          KCACALEND
059200991224     C                   Z-ADD     WANN          KCAANNO
059300991224     C                   Z-ADD     WMES          KCAMESE
059400991224     C     KEYBCA        CHAIN     ANBCA01L                           99
059500000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
059600000000     C                   MOVEL     '1'           ERRL00
059700000000     C                   MOVEL     MSG(8)        DSMSMS
059800000000     C                   ELSE                                                   <-- 1 -->
059900000000     C                   MOVEL     BCAGPF        GFEDS
060000000000     C                   DO        31            X                 6 0          --- 2 -->
060100000000     C     GFE(X)        IFEQ      *BLANKS                                      --- 3 -->
060200000000     C                   MOVEL     '*'           GFE(X)
060300000000     C                   ELSE                                                   <-- 3 -->
060400000000     C     GFE(X)        IFEQ      *ZEROS
060500000000     C                   MOVEL     ' '           GFE(X)
060600000000     C                   ELSE
060700000000     C                   MOVEL     'F'           GFE(X)
060800000000     C                   END
060900000000     C                   END                                                    <-- 3 ---
061000000000     C                   END                                                    <-- 2 ---
061100000000     C                   END                                                    <-- 1 ---
061200000000     C*
061300000000     C                   ENDSR
061400000000     C*------------------------------------------------------------------------*
061500971203     C* IMPGSE - IMPOSTA GIORNI DELLA SETTIMANA
061600000000     C*------------------------------------------------------------------------*
061700000000     C     IMPGSE        BEGSR
061800000000     C*
061900000000     C* LANCIO COMANDO RTVDAT PER AVERE IL GIORNO DELLA SETTIMANA
062000991224     C                   MOVE      WANN          WAA                            *ANNO (2)
062100000000     C                   Z-ADD     WMES          WMM
062200000000     C                   DO        31            X                 6 0          --- 1 -->
062300000000     C     GFE(X)        IFEQ      '*'                                          --- 2 -->
062400000000     C                   MOVEL     '*'           GSE(X)
062500000000     C                   ELSE                                                   <-- 2 -->
062600000000     C                   Z-ADD     X             WGG
062700000000     C                   MOVE      WDATA         C98DAT
062800000000     C                   CALL      'XGIOSET'
062900000000     C                   PARM                    C98DAT            6
063000000000     C                   PARM                    C98GSE            1
063100000000     C                   MOVE      C98GSE        W98GSE            1 0
063200000000     C     W98GSE        IFEQ      1
063300000000     C                   Z-ADD     7             W98GSE
063400000000     C                   ELSE
063500000000     C                   SUB       1             W98GSE
063600000000     C                   END
063700000000     C                   MOVE      W98GSE        GSE(X)
063800000000     C                   END                                                    <-- 2 ---
063900000000     C                   END                                                    <-- 1 ---
064000000000     C*
064100000000     C                   ENDSR
064200000000     C*------------------------------------------------------------------------*
064300000000     C* CARFIL - CARICA LA SCHIERA DELLE FILIALI/FILIALI
064400000000     C*------------------------------------------------------------------------*
064500000000     C     CARFIL        BEGSR
064600000000     C*
064700971202     C* CARICA TUTTE LE FILIALI
064800000000     C                   EXSR      CARORG
064900971215     C*
065000971202     C* CARICA TUTTI I TERMINAL
065100971215     C                   EXSR      CARCAE
065200971127     C*---
065300971202     C* CREAZIONE PER COMBINAZIONE FILIALE O TERMINAL
065400971127     C*---
065500971215     C                   Z-ADD     *ZEROS        J                 6 0
065600971127IF  1C     VIDPAR        IFNE      *ZEROS
065700000000     C     VIDARR        ORNE      *ZEROS
065800971202     C* PARTENZA
065900971202IF  2C     VIDPAR        IFEQ      '888'
066000971202DO  3C                   DO        WUTFP         J
066100971202     C                   Z-ADD     TFP(J)        TP(J)                          *TERMINAL PARTENZA
066200971202E   3C                   ENDDO
066300971127X   2C                   ELSE
066400971202IF  3C     VIDPAR        IFEQ      '999'
066500971215DO  4C                   DO        WUFIL         J
066600971202     C                   Z-ADD     FIL(J)        TP(J)                          *FILIALE PARTENZA
066700971215E   4C                   ENDDO
066800971202X   3C                   ELSE
066900971202     C                   Z-ADD     1             J
067000971202     C                   MOVEL     VIDPAR        TP(J)
067100971202E   3C                   ENDIF
067200971202E   2C                   ENDIF
067300971202     C                   Z-ADD     J             WUTP
067400971202     C* ARRIVO
067500971202IF  2C     VIDARR        IFEQ      '888'
067600971202DO  3C                   DO        WUTFA         J
067700971202     C                   Z-ADD     TFA(J)        TA(J)                          *TERMINAL ARRIVO
067800971202E   3C                   ENDDO
067900971202X   2C                   ELSE
068000971202IF  3C     VIDARR        IFEQ      '999'
068100971215DO  4C                   DO        WUFIL         J
068200971202     C                   Z-ADD     FIL(J)        TA(J)                          *FILIALE ARRIVO
068300971215E   4C                   ENDDO
068400971202X   3C                   ELSE
068500971202     C                   Z-ADD     1             J
068600971202     C                   MOVEL     VIDARR        TA(J)
068700971202E   3C                   ENDIF
068800971202E   2C                   ENDIF
068900971202     C                   Z-ADD     J             WUTA
069000971127     C*---
069100971202     C* CREAZIONE PER TIPO TEMPO
069200971127     C*---
069300971202X   1C                   ELSE
069400000000     C*
069500971127     C* TERMINAL / TERMINAL
069600971215IF  2C     VIDTTF        IFEQ      'T'
069700971215     C     VIDTTF        OREQ      'X'
069800971215     C                   Z-ADD     *ZEROS        K                 6 0
069900971215DO  3C     1             DO        WUTFP         Y                 6 0
070000971215DO  4C     1             DO        WUTFA         X                 6 0
070100971215     C                   SETOFF                                           28
070200971215     C                   MOVEL     TFP(Y)        WCOM              6 0
070300971215     C                   MOVE      TFA(X)        WCOM
070400971215     C     WCOM          LOOKUP    CPA                                    28
070500971215IF  5C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATA
070600971215     C                   ADD       1             K
070700971215     C                   Z-ADD     WCOM          CPA(K)
070800971215     C                   ADD       1             J
070900971215     C                   Z-ADD     TFP(Y)        TP(J)
071000971215     C                   Z-ADD     TFA(X)        TA(J)
071100971215E   5C                   ENDIF
071200971215E   4C                   ENDDO                                                  *FINE TERM ARRIVO
071300971215E   3C                   ENDDO                                                  *FINE TERM PARTENZA
071400971215E   2C                   ENDIF
071500971127     C*
071600971127     C* FILIALI ARRIVO
071700971202IF  2C     VIDTTF        IFEQ      'A'
071800000000     C     VIDTTF        OREQ      'X'
071900971215DO  3C     1             DO        WUFIL         K
072000971203     C                   ADD       1             J
072100971203     C                   Z-ADD     *ZEROS        TP(J)                          *ZERO
072200971203     C                   Z-ADD     FIL(K)        TA(J)                          *TUTTE LE FILIALI
072300971202E   3C                   ENDDO
072400971202E   2C                   ENDIF
072500971127     C*
072600971127     C* FILIALI PARTENZA
072700971202IF  2C     VIDTTF        IFEQ      'P'
072800000000     C     VIDTTF        OREQ      'X'
072900971215DO  3C     1             DO        WUFIL         K
073000971203     C                   ADD       1             J
073100971203     C                   Z-ADD     FIL(K)        TP(J)                          *TUTTE LE FILIALI
073200971203     C                   Z-ADD     *ZEROS        TA(J)                          *ZERO
073300971202E   3C                   ENDDO
073400971202E   2C                   ENDIF
073500971203     C                   Z-ADD     J             WUTP
073600971203     C                   Z-ADD     J             WUTA
073700971203E   1C                   ENDIF
073800000000     C*
073900000000     C                   ENDSR
074000000000     C*------------------------------------------------------------------------*
074100971202     C* CARORG - CARICA LE FILIALI
074200000000     C*------------------------------------------------------------------------*
074300000000     C     CARORG        BEGSR
074400971202     C*
074500971202     C* AZZERA LE VARIABILI DI LAVORO
074600971202     C                   Z-ADD     *ZEROS        J                 6 0
074700971202     C*
074800971202     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
074900971202     C     *LOVAL        SETLL     AZORG01L
075000971202     C                   READ      AZORG01L                               99
075100971202DO  1C     *IN99         DOWEQ     *OFF
075200971202IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDO
075300971202     C     ORGFAG        ANDNE     'V'                                          *FILIALI/AGENZIE
075400971202     C                   ADD       1             J
075500971202     C                   Z-ADD     ORGFIL        FIL(J)
075600971202E   2C                   ENDIF
075700971202     C                   READ      AZORG01L                               99
075800971202E   1C                   ENDDO
075900971202     C                   Z-ADD     J             WUFIL                          *ULTIMA FIL
076000000000     C*
076100000000     C                   ENDSR
076200971202     C*------------------------------------------------------------------------*
076300971202     C* CARCAE - CARICA I TERMINAL PARTENZA E ARRIVO
076400971202     C*------------------------------------------------------------------------*
076500971202     C     CARCAE        BEGSR
076600971202     C* ARRIVO
076700971202     C                   Z-ADD     *ZEROS        J                 6 0
076800971202     C                   MOVEL     'A'           KCEEPA                         *ARRIVO
076900971202     C     KCEEPA        SETLL     AZCAE01L
077000971202     C     KCEEPA        READE     AZCAE01L                               99
077100971202DO  1C     *IN99         DOWEQ     *OFF
077200971202IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
077300971202IF  3C     DATCOR        IFGE      CAEDDE                                       *IN DECORRENZA
077400971202     C     DATCOR        ANDLE     CAEDSC
077500971202     C                   SETOFF                                           28
077600971202     C     CAETFE        LOOKUP    TFA                                    28
077700971202IF  4C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATO
077800971202     C                   ADD       1             J
077900971202     C                   Z-ADD     CAETFE        TFA(J)
078000971202E   4C                   ENDIF
078100971202E   3C                   ENDIF
078200971202E   2C                   ENDIF
078300971202     C     KCEEPA        READE     AZCAE01L                               99
078400971202E   1C                   ENDDO
078500971202     C                   Z-ADD     J             WUTFA                          *ULTIMO TFA
078600971202     C* PARTENZA
078700971202     C                   Z-ADD     *ZEROS        J                 6 0
078800971202     C                   MOVEL     'P'           KCEEPA                         *ARRIVO
078900971202     C     KCEEPA        SETLL     AZCAE01L
079000971202     C     KCEEPA        READE     AZCAE01L                               99
079100971202DO  1C     *IN99         DOWEQ     *OFF
079200971202IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
079300971202IF  3C     DATCOR        IFGE      CAEDDE                                       *IN DECORRENZA
079400971202     C     DATCOR        ANDLE     CAEDSC
079500971202     C                   SETOFF                                           28
079600971202     C     CAETFE        LOOKUP    TFP                                    28
079700971202IF  4C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATO
079800971202     C                   ADD       1             J
079900971202     C                   Z-ADD     CAETFE        TFP(J)
080000971202E   4C                   ENDIF
080100971202E   3C                   ENDIF
080200971202E   2C                   ENDIF
080300971202     C     KCEEPA        READE     AZCAE01L                               99
080400971202E   1C                   ENDDO
080500971202     C                   Z-ADD     J             WUTFP                          *ULTIMO TFP
080600971202     C*
080700971202     C                   ENDSR
080800000000     C*------------------------------------------------------------------------*
080900000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
081000000000     C*------------------------------------------------------------------------*
081100000000     C     DECODI        BEGSR
081200000000     C*
081300000000     C                   MOVEL     *BLANKS       VIDDPA
081400000000     C                   MOVEL     *BLANKS       VIDDAR
081500971202     C*
081600971202IF  1C     VIDPAR        IFEQ      *ZEROS
081700971202     C     VIDARR        ANDEQ     *ZEROS
081800971202     C*
081900000000     C* CONTROLLO TIPO FILIALE
082000971202IF  2C     VIDTTF        IFEQ      'A'
082100000000     C     VIDTTF        OREQ      'P'
082200000000     C     VIDTTF        OREQ      'T'
082300000000     C     VIDTTF        OREQ      'X'
082400971202X   2C                   ELSE
082500000000     C                   MOVEL     MSG(13)       DSMSMS
082600000000     C                   MOVEL     '1'           ERRL00
082700000000     C                   SETON                                        71
082800971202E   2C                   ENDIF
082900000000     C*
083000000000X   1C                   ELSE
083100000000     C*
083200971202IF  2C     VIDTTF        IFNE      *BLANKS
083300000000     C                   MOVEL     MSG(14)       DSMSMS
083400000000     C                   MOVEL     '1'           ERRL00
083500000000     C                   SETON                                        716061
083600971215E   2C                   ENDIF
083700000000     C*
083800000000     C* CONTROLLO TERMINAL/FILIALE PARTENZA
083900971215IF  2C     ERRL00        IFEQ      '0'
084000971215IF  3C     VIDPAR        IFEQ      *ZEROS
084100000000     C     VIDPAR        OREQ      '999'
084200000000     C     VIDPAR        OREQ      '888'
084300971215X   3C                   ELSE
084400000000     C                   MOVE      VIDPAR        WLIN              3
084500000000     C                   EXSR      CHKLNA
084600971215IF  4C     ERRL00        IFEQ      '1'
084700000000     C                   SETON                                        60
084800971215X   4C                   ELSE
084900000000     C                   MOVEL     WLIN          VIDPAR
085000000000     C                   MOVEL     WDLIN         VIDDPA
085100971215E   4C                   END
085200971215E   3C                   END
085300971215E   2C                   END
085400000000     C*
085500000000     C* CONTROLLO TERMINAL/FILIALE ARRIVO
085600971215IF  2C     ERRL00        IFEQ      '0'
085700971215IF  3C     VIDARR        IFEQ      *ZEROS
085800000000     C     VIDARR        OREQ      '999'
085900000000     C     VIDARR        OREQ      '888'
086000971215X   3C                   ELSE
086100000000     C                   MOVE      VIDARR        WLIN
086200000000     C                   EXSR      CHKLNA
086300971215IF  4C     ERRL00        IFEQ      '1'
086400000000     C                   SETON                                        61
086500971215X   4C                   ELSE
086600000000     C                   MOVEL     WLIN          VIDARR
086700000000     C                   MOVEL     WDLIN         VIDDAR
086800971215E   4C                   END
086900971215E   3C                   END
087000971215E   2C                   END
087100000000     C*
087200971215     C* CONTROLLA LE COMBINAZIONI FILIALE PARTENZA/ARRIVO
087300000000IF  2C     ERRL00        IFEQ      '0'
087400971215IF  3C     VIDPAR        IFEQ      '888'
087500000000     C     VIDARR        ANDEQ     '999'
087600000000     C                   MOVEL     MSG(15)       DSMSMS
087700000000     C                   MOVEL     '1'           ERRL00
087800000000     C                   SETON                                        6061
087900971215E   3C                   END
088000000000     C*
088100971215IF  3C     VIDPAR        IFEQ      '999'
088200000000     C     VIDARR        ANDEQ     '888'
088300000000     C                   MOVEL     MSG(15)       DSMSMS
088400000000     C                   MOVEL     '1'           ERRL00
088500971215     C                   SETON                                        6061
088600971215E   3C                   END
088700000000     C*
088800971215IF  3C     VIDPAR        IFEQ      '888'
088900000000     C     VIDARR        ANDEQ     *ZEROS
089000000000     C                   MOVEL     MSG(15)       DSMSMS
089100000000     C                   MOVEL     '1'           ERRL00
089200000000     C                   SETON                                        6061
089300971215E   3C                   END
089400000000     C*
089500971215IF  3C     VIDPAR        IFEQ      '999'
089600000000     C     VIDARR        ANDEQ     *ZEROS
089700000000     C                   MOVEL     MSG(15)       DSMSMS
089800000000     C                   MOVEL     '1'           ERRL00
089900000000     C                   SETON                                        6061
090000971215E   3C                   END
090100000000     C*
090200971215IF  3C     VIDPAR        IFEQ      *ZEROS
090300000000     C     VIDARR        ANDEQ     '888'
090400000000     C                   MOVEL     MSG(15)       DSMSMS
090500000000     C                   MOVEL     '1'           ERRL00
090600000000     C                   SETON                                        6061
090700971215E   3C                   END
090800000000     C*
090900971215IF  3C     VIDPAR        IFEQ      *ZEROS
091000000000     C     VIDARR        ANDEQ     '999'
091100000000     C                   MOVEL     MSG(15)       DSMSMS
091200000000     C                   MOVEL     '1'           ERRL00
091300000000     C                   SETON                                        6061
091400971215E   3C                   END
091500971215E   2C                   END
091600000000E   1C                   END
091700000000     C*
091800971215     C* CONTROLLA TIPO AUTOMATISMO
091900000000IF  1C     ERRL00        IFEQ      '0'
092000000000     C     VIDTIP        IFEQ      'C'
092100000000     C     VIDTIP        OREQ      'D'
092200000000     C     VIDTIP        OREQ      'G'
092300000000     C                   ELSE
092400000000     C                   MOVEL     MSG(9)        DSMSMS
092500000000     C                   MOVEL     '1'           ERRL00
092600000000     C                   SETON                                        69
092700000000     C                   END
092800000000E   1C                   END
092900000000     C*
093000000000     C* CONTROLLI PER AUTOMATISMO -C- *****************************************
093100000000IF  1C     VIDTIP        IFEQ      'C'                                          --- C -->
093200000000     C*
093300000000     C* CONTROLLO ANNO (RISPETTO ANNO ATTUALE) (MIN 1 IN MENO,MAX 1 IN PIU')
093400000000     C     ERRL00        IFEQ      '0'
093500991224     C                   MOVEL     DATCOR        WAAINI            4 0
093600991224     C                   MOVEL     DATCOR        WAAFIN            4 0
093700000000     C                   SUB       1             WAAINI
093800000000     C                   ADD       1             WAAFIN
093900000000     C     VIDAAC        IFLT      WAAINI
094000000000     C     VIDAAC        ORGT      WAAFIN
094100000000     C                   MOVEL     MSG(4)        DSMSMS
094200000000     C                   MOVEL     '1'           ERRL00
094300000000     C                   SETON                                        62
094400000000     C                   END
094500000000     C                   END
094600000000E   1C                   END                                                    <-- C ---
094700000000     C*
094800000000     C* CONTROLLI PER AUTOMATISMO -D- *****************************************
094900000000IF  1C     VIDTIP        IFEQ      'D'                                          --- D -->
095000000000     C*
095100000000     C* CONTROLLO DATA
095200000000     C     ERRL00        IFEQ      '0'
095300000000     C                   Z-ADD     VIDDAT        G08DAT
095400000000     C                   Z-ADD     *ZEROS        G08INV
095500000000     C                   MOVEL     '0'           G08ERR
095600000000     C                   CALL      'XSRDA8'
095700000000     C                   PARM                    WLBDA8
095800000000     C                   Z-ADD     G08DAT        VIDDAT
095900000000     C     G08ERR        IFNE      '0'
096000000000     C                   MOVEL     '1'           ERRL00
096100000000     C                   MOVEL     MSG(6)        DSMSMS
096200000000     C                   SETON                                        63
096300000000     C                   END
096400000000     C                   END
096500000000     C*
096600000000     C* CONTROLLO FLAG MATTINO
096700000000     C     ERRL00        IFEQ      '0'
096800000000     C     VIDFMD        IFNE      *BLANKS
096900000000     C                   MOVEL     VIDFMD        WSIM              1
097000000000     C                   EXSR      CHKSIM
097100000000     C     ERRL00        IFEQ      '1'
097200000000     C                   MOVEL     '1'           ERRL00
097300000000     C                   MOVEL     MSG(7)        DSMSMS
097400000000     C                   SETON                                        67
097500000000     C                   ELSE
097600000000     C                   MOVEL     WSIM          VIDFMD
097700000000     C                   END
097800000000     C                   END
097900000000     C                   END
098000000000     C*
098100000000     C* CONTROLLO FLAG POMERIGGIO
098200000000     C     ERRL00        IFEQ      '0'
098300000000     C     VIDFPD        IFNE      *BLANKS
098400000000     C                   MOVEL     VIDFPD        WSIM              1
098500000000     C                   EXSR      CHKSIM
098600000000     C     ERRL00        IFEQ      '1'
098700000000     C                   MOVEL     '1'           ERRL00
098800000000     C                   MOVEL     MSG(7)        DSMSMS
098900000000     C                   SETON                                        68
099000000000     C                   ELSE
099100000000     C                   MOVEL     WSIM          VIDFPD
099200000000     C                   END
099300000000     C                   END
099400000000     C                   END
099500000000     C*
099600000000     C* NON ACCETTO FLAG = BLANK SE GIORNO FESTIVO
099700000000     C     ERRL00        IFEQ      '0'
099800000000     C     VIDFMD        IFEQ      *BLANKS
099900000000     C     VIDFPD        OREQ      *BLANKS
100000000000     C                   Z-ADD     *ZEROS        KCATFP
100100000000     C                   Z-ADD     *ZEROS        KCATFA
100200000000     C                   Z-ADD     VIDAAD        KCAANN
100300000000     C                   Z-ADD     VIDMMD        KCAMES
100400000000     C     KEYCAL        CHAIN     AZCLN01L                           99
100500000000     C     *IN99         IFEQ      '0'
100600000000     C                   MOVEL     CLNPOM        GFEDS
100700000000     C                   Z-ADD     VIDGGD        GG                2 0
100800000000     C     GFE(GG)       IFEQ      'F'
100900000000     C                   MOVEL     '1'           ERRL00
101000000000     C                   MOVEL     MSG(12)       DSMSMS
101100000000     C                   SETON                                        6768
101200000000     C                   END
101300000000     C                   END
101400000000     C                   END
101500000000     C                   END
101600000000     C*
101700000000E   1C                   END                                                    <-- D ---
101800000000     C*
101900000000     C* CONTROLLI PER AUTOMATISMO -G- *****************************************
102000000000IF  1C     VIDTIP        IFEQ      'G'                                          --- G -->
102100971203     C*
102200971203     C* CONTROLLO ANNO
102300971203     C     ERRL00        IFEQ      '0'
102400991224     C                   MOVEL     DATCOR        WAAINI            4 0
102500991224     C                   MOVEL     DATCOR        WAAFIN            4 0
102600971203     C                   SUB       1             WAAINI
102700971203     C                   ADD       1             WAAFIN
102800971203     C     VIDAAG        IFLT      WAAINI
102900971203     C     VIDAAG        ORGT      WAAFIN
103000971203     C                   MOVEL     MSG(4)        DSMSMS
103100971203     C                   MOVEL     '1'           ERRL00
103200971203     C                   SETON                                        65
103300971203     C                   END
103400971203     C                   END
103500000000     C*
103600000000     C* CONTROLLO FLAG MATTINO
103700000000     C     ERRL00        IFEQ      '0'
103800000000     C     VIDFMG        IFNE      *BLANKS
103900000000     C                   MOVEL     VIDFMG        WSIM              1
104000000000     C                   EXSR      CHKSIM
104100000000     C     ERRL00        IFEQ      '1'
104200000000     C                   MOVEL     '1'           ERRL00
104300000000     C                   MOVEL     MSG(7)        DSMSMS
104400000000     C                   SETON                                        64
104500000000     C                   ELSE
104600000000     C                   MOVEL     WSIM          VIDFMG
104700000000     C                   END
104800000000     C                   END
104900000000     C                   END
105000000000     C*
105100000000     C* CONTROLLO FLAG POMERIGGIO
105200000000     C     ERRL00        IFEQ      '0'
105300000000     C     VIDFPG        IFNE      *BLANKS
105400000000     C                   MOVEL     VIDFPG        WSIM              1
105500000000     C                   EXSR      CHKSIM
105600000000     C     ERRL00        IFEQ      '1'
105700000000     C                   MOVEL     '1'           ERRL00
105800000000     C                   MOVEL     MSG(7)        DSMSMS
105900000000     C                   SETON                                        66
106000000000     C                   ELSE
106100000000     C                   MOVEL     WSIM          VIDFPG
106200000000     C                   END
106300000000     C                   END
106400000000     C                   END
106500000000     C*
106600000000     C* CONTROLLO GIORNO DELLA SETTIMANA
106700000000     C     ERRL00        IFEQ      '0'
106800000000     C     VIDGIO        IFEQ      '1'
106900000000     C     VIDGIO        OREQ      '2'
107000000000     C     VIDGIO        OREQ      '3'
107100000000     C     VIDGIO        OREQ      '4'
107200000000     C     VIDGIO        OREQ      '5'
107300000000     C     VIDGIO        OREQ      '6'
107400000000     C     VIDGIO        OREQ      '7'
107500000000     C                   ELSE
107600000000     C                   MOVEL     MSG(10)       DSMSMS
107700000000     C                   MOVEL     '1'           ERRL00
107800000000     C                   SETON                                        70
107900000000     C                   END
108000000000     C                   END
108100000000     C*
108200000000     C* SE FLAG = BLANKS CONTROLLO CHE NON VI SIA GIORNO FESTIVO
108300000000     C     ERRL00        IFEQ      '0'
108400000000     C     VIDFMG        IFEQ      *BLANKS
108500000000     C     VIDFPG        OREQ      *BLANKS
108600000000     C     VIDGIO        IFEQ      '6'
108700000000     C     VIDGIO        OREQ      '7'
108800000000     C                   MOVEL     MSG(12)       DSMSMS
108900000000     C                   MOVEL     '1'           ERRL00
109000000000     C                   SETON                                        6466
109100000000     C                   END
109200000000     C                   END
109300000000     C                   END
109400000000     C*
109500000000     C                   END                                                    <-- G ---
109600000000E   1C*
109700000000     C                   ENDSR
109800000000     C*------------------------------------------------------------------------*
109900000000     C* CHKLNA - CONTROLLA LA LINEA
110000000000     C*------------------------------------------------------------------------*
110100000000     C     CHKLNA        BEGSR
110200000000     C*
110300000000     C* CONTROLLO SE E' STATO INSERITO '?'
110400000000     C                   MOVE      WLIN          WLNA1             1
110500000000     C                   MOVEL     WLIN          WLNA2             1
110600000000     C*
110700000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
110800000000     C     WLNA1         IFEQ      '?'                                          --- 1 -->
110900000000     C     WLNA2         OREQ      '?'
111000000000     C                   MOVEL     *BLANKS       §COD1             3
111100000000     C                   MOVE      *BLANKS       §DES1
111200971203     C                   MOVE      *BLANKS       §PRA
111300971203     C                   CALL      'TNSD24R'
111400000000     C                   PARM                    §COD1
111500000000     C                   PARM                    §TIP              1
111600000000     C                   PARM                    §DES1            25
111700971203     C                   PARM                    §PRA              3
111800000000     C                   MOVEL     §COD1         WLIN
111900000000     C                   MOVEL     §DES1         WDLIN            20
112000000000     C*
112100000000     C                   ELSE                                                   <-- 1 -->
112200000000     C*
112300000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
112400000000     C                   MOVE      WLIN          KORFIL
112500000000     C     KEYORG        CHAIN     AZORG                              99
112600000000     C     *IN99         IFEQ      '0'                                          --- 2 -->
112700000000     C                   MOVEL     ORGDES        WDLIN
112800000000     C                   ELSE                                                   <-- 2 -->
112900000000     C                   MOVEL     '1'           ERRL00
113000000000     C                   MOVEL     MSG(5)        DSMSMS
113100000000     C                   MOVEL     *ALL'*'       WDLIN
113200000000     C                   END                                                    <-- 2 ---
113300000000     C*
113400000000     C                   END                                                    <-- 1 ---
113500000000     C*
113600000000     C                   ENDSR
113700000000     C*------------------------------------------------------------------------*
113800000000     C* CHKSIM - CONTROLLA SIMBOLI CALENDARIO
113900000000     C*------------------------------------------------------------------------*
114000000000     C     CHKSIM        BEGSR
114100000000     C*
114200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
114300000000     C     WSIM          IFEQ      '?'                                          --- 1 -->
114400000000     C*
114500000000     C                   MOVEL     CODUT         §KUT              1 0
114600000000     C                   MOVEL     '1Y'          §COD              2
114700000000     C                   MOVE      *BLANKS       §DES1
114800000000     C                   CALL      'X§TABER'
114900000000     C                   PARM                    §KUT
115000000000     C                   PARM                    §COD
115100000000     C                   PARM                    §KEY              8
115200000000     C                   PARM                    §DES             30
115300000000     C                   MOVEL     §KEY          WSIM
115400000000     C                   MOVEL     §DES          DS1Y                           *NELLA DS
115500000000     C*
115600000000     C                   ELSE                                                   <-- 1 -->
115700000000     C*
115800000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
115900000000     C                   MOVEL     CODUT         KTBUTE
116000000000     C                   MOVE      '1Y'          KTBCOD
116100000000     C                   MOVE      *BLANKS       KTBKEY
116200000000     C                   MOVEL     WSIM          KTBKEY
116300000000     C     KEYTAB        CHAIN     TABEL                              99
116400000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
116500000000     C     TBLFLG        OREQ      '*'
116600000000     C                   MOVEL     '1'           ERRL00
116700000000     C                   MOVEL     MSG(7)        DSMSMS
116800000000     C                   ELSE
116900000000     C                   MOVEL     TBLUNI        DS1Y                           *NELLA DS
117000000000     C                   END                                                    <-- 2 ---
117100000000     C*
117200000000     C                   END                                                    <-- 1 ---
117300000000     C*
117400000000     C                   ENDSR
117500000000     C*------------------------------------------------------------------------*
117600000000     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
117700000000     C*------------------------------------------------------------------------*
117800000000     C     AZZVID        BEGSR
117900000000     C*
118000000000     C                   MOVE      *ZEROS        VIDPAR
118100000000     C                   MOVE      *ZEROS        VIDARR
118200000000     C                   MOVEL     *BLANKS       VIDTIP
118300000000     C                   Z-ADD     *ZEROS        VIDAAC
118400000000     C                   Z-ADD     *ZEROS        VIDDAT
118500000000     C                   MOVEL     *BLANKS       VIDFMD
118600000000     C                   MOVEL     *BLANKS       VIDFPD
118700000000     C                   MOVEL     *BLANKS       VIDFMG
118800000000     C                   MOVEL     *BLANKS       VIDFPG
118900000000     C                   MOVEL     *BLANKS       VIDGIO
119000000000     C                   Z-ADD     *ZEROS        VIDAAG
119100000000     C*
119200000000     C                   ENDSR
119300000000     C*------------------------------------------------------------------------*
119400000000     C* *INZSR - OPERAZIONI INIZIALI
119500000000     C*------------------------------------------------------------------------*
119600000000     C     *INZSR        BEGSR
119700000000     C*
119800000000     C* RICEVIMENTO PARAMETRI
119900000000     C     *ENTRY        PLIST
120000000000     C                   PARM                    DSCF03            1            PREMUTO F03
120100000000     C                   PARM                    DSCERR            1            '1'=ERRORE
120200000000     C                   PARM                    DSCMSG           80            MESSAGGIO
120300000000     C*
120400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
120500000000     C     XMSGPA        PLIST
120600000000     C                   PARM                    DSMSMS
120700000000     C                   PARM                    DSMSPG
120800000000     C                   PARM                    DSMSKE
120900000000     C                   PARM                    DSMSER
121000971202     C*
121100971202     C* VARIABILI RIFERITE AL DATA BASE
121200971202     C     *LIKE         DEFINE    FUDFUN        FUNKEY
121300971202     C     *LIKE         DEFINE    ORGFIL        KORFIL
121400971202     C     *LIKE         DEFINE    CLNTFP        KCATFP
121500971202     C     *LIKE         DEFINE    CLNTFA        KCATFA
121600971202     C     *LIKE         DEFINE    CLNANN        KCAANN
121700971202     C     *LIKE         DEFINE    CLNMES        KCAMES
121800971202     C     *LIKE         DEFINE    TBLKUT        KTBUTE
121900971202     C     *LIKE         DEFINE    TBLCOD        KTBCOD
122000971202     C     *LIKE         DEFINE    TBLKEY        KTBKEY
122100971202     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         AZCAE00F
122200000000     C*
122300000000     C* CHIAVE LETTURA AZCLN01L - COMPLETA
122400000000     C     KEYCAL        KLIST
122500000000     C                   KFLD                    KCATFP
122600000000     C                   KFLD                    KCATFA
122700000000     C                   KFLD                    KCAANN
122800000000     C                   KFLD                    KCAMES
122900000000     C*
123000000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
123100000000     C     KEYORG        KLIST
123200000000     C                   KFLD                    KORFIL
123300000000     C*
123400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
123500000000     C     KEYTAB        KLIST
123600000000     C                   KFLD                    KTBUTE
123700000000     C                   KFLD                    KTBCOD
123800000000     C                   KFLD                    KTBKEY
123900000000     C*
124000991224     C* CHIAVE LETTURA ANBCA01L - COMPLETA
124100000000     C     KEYBCA        KLIST
124200991224     C                   KFLD                    KCACALEND
124300991224     C                   KFLD                    KCAANNO
124400991224     C                   KFLD                    KCAMESE
124500971202     C*
124600971202     C* VARIABILI DI CONTROLLO
124700971202     C                   Z-ADD     *ZEROS        WUTFP             6 0          *ULTIMO TERM PART
124800971202     C                   Z-ADD     *ZEROS        WUTFA             6 0          *ULTIMO TERM ARR
124900971202     C                   Z-ADD     *ZEROS        WUFIL             6 0          *ULTIMA FILAILE
125000971202     C                   Z-ADD     *ZEROS        WUTP              6 0          *ULTIMA PARTENZA
125100971202     C                   Z-ADD     *ZEROS        WUTA              6 0          *ULTIMO ARRIVO
125200000000     C*
125300000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
125400000000     C                   CALL      'XKEY'
125500000000     C                   PARM                    DSKEY
125600000000     C*
125700000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
125800000000     C*   -NOME PGM CHIAMANTE
125900000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
126000000000     C                   MOVEL     'TNSD77R'     DSMSPG
126100000000     C                   MOVEL     'TNSD77R'     MSGQUE
126200000000     C                   MOVEL     *BLANKS       MSGKEY
126300000000     C                   MOVEL     *BLANKS       DSMSMS
126400000000     C*
126500000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
126600000000     C                   Z-ADD     0             FUNGRU            5 0
126700000000     C*
126800000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
126900000000     C                   EXSR      FUNGES
127000000000     C*
127100000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
127200000000     C                   MOVEL     '0'           F12L00            1
127300000000     C                   MOVEL     '0'           F03L00            1
127400000000     C                   MOVEL     '0'           FINL00            1
127500000000     C                   MOVEL     '0'           ERRL00            1
127600000000     C                   MOVEL     '0'           FLFINE            1
127700020906     C*
127800020906     C                   Z-ADD     1             CODUT             1 0
127900000000     C*
128000000000     C* RECUPERO DATA ODIERNA
128100991224     C                   TIME                    n14                            *Ora (6)+ Data (8)
128200991224     C                   MOVE      n14           n8                             *Data (8) in g/m/a
128300991224     C                   Z-ADD     n8            g08dat
128400991224     C                   Z-ADD     *zeros        g08inv
128500991224     C                   MOVEL     '0'           g08err
128600991224     C                   CALL      'XSRDA8'
128700991224     C                   PARM                    wlbda8
128800991224     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
128900000000     C*
129000000000     C* REPERISCE LA DATA CORRENTE
129100000000     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
129200000000     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
129300000000     C                   Z-ADD     N8            G08DAT
129400000000     C                   Z-ADD     *ZEROS        G08INV
129500000000     C                   MOVEL     '0'           G08ERR
129600000000     C                   CALL      'XSRDA8'
129700000000     C                   PARM                    WLBDA8
129800000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
129900000000     C*
130000000000     C* AZZERO CAMPI A VIDEO
130100000000     C                   EXSR      AZZVID
130200000000     C*
130300000000     C                   ENDSR
130400000000     C*------------------------------------------------------------------------*
130500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
130600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
130700000000     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
130800000000     C*     STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
130900000000     C*     NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
131000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
131100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
131200000000     C*------------------------------------------------------------------------*
131300000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
131400000000     C*------------------------------------------------------------------------*
131500000000     C     FUNGES        BEGSR
131600000000     C*
131700000000     C* PULISCO CAMPI DI LAVORO
131800000000     C                   Z-ADD     0             W                 5 0
131900000000     C*
132000000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
132100000000     C* E CARICO TABELLA FUNZIONI ATTIVE
132200000000     C     1             DO        24            I
132300971202     C                   MOVEL     FUD(I)        FUDRIG
132400000000     C*
132500000000     C* PULIZIA FUNZIONI PRECEDENTI
132600000000     C                   MOVEL     *BLANKS       FUV(I)
132700000000     C*
132800000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
132900000000     C* (SALTA F24)
133000000000     C     FUDATT        IFEQ      'S'
133100000000     C     FUDFUN        ANDNE     '24'
133200000000     C                   ADD       1             W
133300000000     C                   MOVEL     FUDDES        FUV(W)
133400000000     C                   END
133500000000     C                   END                                                    DO WHILE
133600000000     C*
133700000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
133800000000     C* ALTRIMENTI DISATTIVO F24
133900000000     C     1             DO        24            I
134000000000     C                   MOVEL     FUD(I)        FUDRIG
134100000000     C*
134200000000     C     FUDFUN        IFEQ      '24'
134300000000     C     W             IFGT      6
134400000000     C                   MOVEL     'S'           FUDATT
134500000000     C                   ELSE
134600000000     C                   MOVEL     *BLANKS       FUDATT
134700000000     C                   END                                                    W GT 8
134800000000     C                   MOVEL     FUDRIG        FUD(I)
134900000000     C                   END
135000000000     C                   END                                                    DO WHILE
135100000000     C*
135200000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
135300000000     C     W             DIV       6             I
135400000000     C                   MVR                     Z
135500000000     C*
135600000000     C     Z             IFNE      0
135700000000     C     I             ADD       1             Z
135800000000     C                   ELSE
135900000000     C                   Z-ADD     I             Z                 5 0
136000000000     C                   END
136100000000     C*
136200000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
136300000000     C                   ADD       1             FUNGRU
136400000000     C     FUNGRU        IFGT      Z
136500000000     C     FUNGRU        ORLT      1
136600000000     C                   Z-ADD     1             FUNGRU
136700000000     C                   END
136800000000     C*
136900000000     C* PRIMO GRUPPO DI 6 FUNZIONI
137000000000     C     FUNGRU        IFEQ      1
137100000000     C                   MOVEL     FUV(1)        FUN001
137200000000     C                   MOVEL     FUV(2)        FUN002
137300000000     C                   MOVEL     FUV(3)        FUN003
137400000000     C                   MOVEL     FUV(4)        FUN004
137500000000     C                   MOVEL     FUV(5)        FUN005
137600000000     C                   MOVEL     FUV(6)        FUN006
137700000000     C                   END
137800000000     C*
137900000000     C* SECONDO GRUPPO DI 6 FUNZIONI
138000000000     C     FUNGRU        IFEQ      2
138100000000     C                   MOVEL     FUV(7)        FUN001
138200000000     C                   MOVEL     FUV(8)        FUN002
138300000000     C                   MOVEL     FUV(9)        FUN003
138400000000     C                   MOVEL     FUV(10)       FUN004
138500000000     C                   MOVEL     FUV(11)       FUN005
138600000000     C                   MOVEL     FUV(12)       FUN006
138700000000     C                   END
138800000000     C*
138900000000     C* TERZO GRUPPO DI 6 FUNZIONI
139000000000     C     FUNGRU        IFEQ      3
139100000000     C                   MOVEL     FUV(13)       FUN001
139200000000     C                   MOVEL     FUV(14)       FUN002
139300000000     C                   MOVEL     FUV(15)       FUN003
139400000000     C                   MOVEL     FUV(16)       FUN004
139500000000     C                   MOVEL     FUV(17)       FUN005
139600000000     C                   MOVEL     FUV(18)       FUN006
139700000000     C                   END
139800000000     C*
139900000000     C* QUARTO GRUPPO DI 6 FUNZIONI
140000000000     C     FUNGRU        IFEQ      4
140100000000     C                   MOVEL     FUV(19)       FUN001
140200000000     C                   MOVEL     FUV(20)       FUN002
140300000000     C                   MOVEL     FUV(21)       FUN003
140400000000     C                   MOVEL     FUV(22)       FUN004
140500000000     C                   MOVEL     *BLANKS       FUN005
140600000000     C                   MOVEL     *BLANKS       FUN006
140700000000     C                   END
140800000000     C*
140900000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
141000000000     C                   MOVEL     *BLANKS       FUN007
141100000000     C                   MOVEL     *BLANKS       FUN008
141200000000     C*
141300000000     C     1             DO        24            I
141400000000     C                   MOVEL     FUD(I)        FUDRIG
141500000000     C     FUDFUN        IFEQ      '24'
141600000000     C     FUDATT        ANDEQ     'S'
141700000000     C                   MOVEL     FUDDES        FUN008
141800000000     C                   END
141900000000     C                   END                                                    DO
142000000000     C                   ENDSR
142100000000     C*------------------------------------------------------------------------*
142200000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
142300000000     C*------------------------------------------------------------------------*
142400000000     C     FUNCON        BEGSR
142500000000     C*
142600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
142700000000     C                   EXSR      FUNVAR
142800000000     C*
142900000000     C* PULISCO CAMPI DI LAVORO
143000000000     C                   Z-ADD     0             I                 5 0
143100000000     C                   Z-ADD     0             W                 5 0
143200000000     C*
143300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
143400000000     C     FUNKEY        IFNE      *BLANKS
143500000000     C     1             DO        24            I
143600000000     C                   MOVEL     FUD(I)        FUDRIG
143700000000     C*
143800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
143900000000     C     FUNKEY        IFEQ      FUDFUN
144000000000     C     FUDATT        ANDEQ     'S'
144100000000     C                   Z-ADD     I             W
144200000000     C                   END
144300000000     C                   END                                                    DO WHILE
144400000000     C*
144500000000     C* FUNZIONE NON ATTIVA (ERRORE)
144600000000     C     W             IFEQ      0
144700000000     C*
144800000000     C* EMISSIONE MESSAGGIO DI ERRORE
144900000000     C                   MOVEL     MSG(1)        DSMSMS
145000000000     C*
145100000000     C                   END                                                    W=0
145200000000     C                   END                                                    FUNKEY NE *BLANKS
145300000000     C*
145400000000     C* FUNZIONE ATTIVA (PROCEDI ...)
145500000000     C     W             IFNE      0
145600000000     C     FUNKEY        OREQ      *BLANKS
145700000000     C*
145800000000     C     FUNKEY        IFEQ      *BLANKS
145900000000     C                   EXSR      ENTGES
146000000000     C                   END
146100000000     C     FUNKEY        IFEQ      '01'
146200000000     C                   EXSR      F01GES
146300000000     C                   END
146400000000     C     FUNKEY        IFEQ      '03'
146500000000     C                   EXSR      F03GES
146600000000     C                   END
146700000000     C     FUNKEY        IFEQ      '06'
146800000000     C                   EXSR      F06GES
146900000000     C                   END
147000000000     C     FUNKEY        IFEQ      '07'
147100000000     C                   EXSR      F07GES
147200000000     C                   END
147300000000     C     FUNKEY        IFEQ      '08'
147400000000     C                   EXSR      F08GES
147500000000     C                   END
147600000000     C     FUNKEY        IFEQ      '12'
147700000000     C                   EXSR      F12GES
147800000000     C                   END
147900000000     C     FUNKEY        IFEQ      '21'
148000000000     C                   EXSR      F21GES
148100000000     C                   END
148200000000     C     FUNKEY        IFEQ      '22'
148300000000     C                   EXSR      F22GES
148400000000     C                   END
148500000000     C     FUNKEY        IFEQ      '24'
148600000000     C                   EXSR      F24GES
148700000000     C                   END
148800000000     C     FUNKEY        IFEQ      'HP'
148900000000     C                   EXSR      HELGES
149000000000     C                   END
149100000000     C*
149200000000     C                   END
149300000000     C*
149400000000     C                   ENDSR
149500000000     C*------------------------------------------------------------------------*
149600000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
149700000000     C*------------------------------------------------------------------------*
149800000000     C     FUNVAR        BEGSR
149900000000     C*
150000000000     C* INIZIALIZZA VARIABILE -FUNKEY-
150100000000     C*
150200000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
150300000000     C     KEY           IFEQ      ENTER
150400000000     C                   MOVEL     *BLANKS       FUNKEY
150500000000     C                   END
150600000000     C     KEY           IFEQ      F1
150700000000     C                   MOVEL     '01'          FUNKEY
150800000000     C                   END
150900000000     C     KEY           IFEQ      F2
151000000000     C                   MOVEL     '02'          FUNKEY
151100000000     C                   END
151200000000     C     KEY           IFEQ      F3
151300000000     C                   MOVEL     '03'          FUNKEY
151400000000     C                   END
151500000000     C     KEY           IFEQ      F4
151600000000     C                   MOVEL     '04'          FUNKEY
151700000000     C                   END
151800000000     C     KEY           IFEQ      F5
151900000000     C                   MOVEL     '05'          FUNKEY
152000000000     C                   END
152100000000     C     KEY           IFEQ      F6
152200000000     C                   MOVEL     '06'          FUNKEY
152300000000     C                   END
152400000000     C     KEY           IFEQ      F7
152500000000     C                   MOVEL     '07'          FUNKEY
152600000000     C                   END
152700000000     C     KEY           IFEQ      F8
152800000000     C                   MOVEL     '08'          FUNKEY
152900000000     C                   END
153000000000     C     KEY           IFEQ      F9
153100000000     C                   MOVEL     '09'          FUNKEY
153200000000     C                   END
153300000000     C     KEY           IFEQ      F10
153400000000     C                   MOVEL     '10'          FUNKEY
153500000000     C                   END
153600000000     C     KEY           IFEQ      F11
153700000000     C                   MOVEL     '11'          FUNKEY
153800000000     C                   END
153900000000     C     KEY           IFEQ      F12
154000000000     C                   MOVEL     '12'          FUNKEY
154100000000     C                   END
154200000000     C     KEY           IFEQ      F13
154300000000     C                   MOVEL     '13'          FUNKEY
154400000000     C                   END
154500000000     C     KEY           IFEQ      F14
154600000000     C                   MOVEL     '14'          FUNKEY
154700000000     C                   END
154800000000     C     KEY           IFEQ      F15
154900000000     C                   MOVEL     '15'          FUNKEY
155000000000     C                   END
155100000000     C     KEY           IFEQ      F16
155200000000     C                   MOVEL     '16'          FUNKEY
155300000000     C                   END
155400000000     C     KEY           IFEQ      F17
155500000000     C                   MOVEL     '17'          FUNKEY
155600000000     C                   END
155700000000     C     KEY           IFEQ      F18
155800000000     C                   MOVEL     '18'          FUNKEY
155900000000     C                   END
156000000000     C     KEY           IFEQ      F19
156100000000     C                   MOVEL     '19'          FUNKEY
156200000000     C                   END
156300000000     C     KEY           IFEQ      F20
156400000000     C                   MOVEL     '20'          FUNKEY
156500000000     C                   END
156600000000     C     KEY           IFEQ      F21
156700000000     C                   MOVEL     '21'          FUNKEY
156800000000     C                   END
156900000000     C     KEY           IFEQ      F22
157000000000     C                   MOVEL     '22'          FUNKEY
157100000000     C                   END
157200000000     C     KEY           IFEQ      F23
157300000000     C                   MOVEL     '23'          FUNKEY
157400000000     C                   END
157500000000     C     KEY           IFEQ      F24
157600000000     C                   MOVEL     '24'          FUNKEY
157700000000     C                   END
157800000000     C     KEY           IFEQ      ROLLUP
157900000000     C                   MOVEL     'RU'          FUNKEY
158000000000     C                   END
158100000000     C     KEY           IFEQ      HELP
158200000000     C                   MOVEL     'HP'          FUNKEY
158300000000     C                   END
158400000000     C                   ENDSR
158500000000     C*------------------------------------------------------------------------*
158600000000     C* NOTE PER COMPILAZIONE SCHIERE
158700000000     C*------------------------------------------------------------------------*
158800000000     C* -MSG-
158900000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
159000000000     C* (NON TOCCARE MSG,1)
159100000000     C*
159200000000     C* -CMD-
159300000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
159400000000     C*
159500000000     C* -FUD-
159600000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
159700000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
159800000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
159900000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
160000000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
160100000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
160200000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
160300000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
160400000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
160500000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
160600000000     C*
160700000000     C*------------------------------------------------------------------------*
160800000000**   MSG  - MESSAGGI
160900000000TNSD77R- Tasto funzionale non gestito                                          1
161000000000TNSD77R- Calendario già creato.                                                2
161100000000TNSD77R- Automatismo applicato.                                                3
161200000000TNSD77R- Anno non accettabile.                                                 4
161300000000TNSD77R- Terminal/Linea non accettabile.                                       5
161400000000TNSD77R- Data non accettabile.                                                 6
161500000000TNSD77R- Flag non accettabile.                                                 7
161600000000TNSD77R- Non trovato calendario banche. Avvertire CED.                         8
161700971202TNSD77R- Tipo automatismo arrato                                               9
161800971202TNSD77R- Giorno della settimana errato                                        10
161900971202TNSD77R- Combinazione terminal par/arr non presente in archivio trazioni      11
162000971202TNSD77R- Flag uguale a spazio non accettabile in un giorno festivo            12
162100971202TNSD77R- Tipo terminal/filiale non accettabile                                13
162200971202TNSD77R- Tipo terminal/filiale alternativo a term/filiale partenza e arrivo   14
162300971202TNSD77R- Combinazione filiale part/arr non accettabile                        15
162400000000TNSD77R-                                                                      16
162500000000TNSD77R-                                                                      17
162600000000TNSD77R-                                                                      18
162700000000TNSD77R-                                                                      19
162800000000TNSD77R-                                                                      20
162900000000**   CMD - COMANDI
163000000000WRKSPLF
163100000000
163200000000
163300000000
163400000000
163500000000**   FUD - TASTI FUNZIONALI
163600000000                   -        (S/N)=N                        funz.01¬
163700000000                   -        (S/N)=N                        funz.02¬
163800000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
163900000000                   -        (S/N)=N                        funz.04¬
164000000000                   -        (S/N)=N                        funz.05¬
164100000000F06=Aggiornamento  -        (S/N)=S                        funz.06¬
164200000000                   -        (S/N)=N                        funz.07¬
164300000000                   -        (S/N)=N                        funz.08¬
164400000000                   -        (S/N)=N                        funz.09¬
164500000000                   -        (S/N)=N                        funz.10¬
164600000000                   -        (S/N)=N                        funz.11¬
164700000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
164800000000                   -        (S/N)=N                        funz.13¬
164900000000                   -        (S/N)=N                        funz.14¬
165000000000                   -        (S/N)=N                        funz.15¬
165100000000                   -        (S/N)=N                        funz.16¬
165200000000                   -        (S/N)=N                        funz.17¬
165300000000                   -        (S/N)=N                        funz.18¬
165400000000                   -        (S/N)=N                        funz.19¬
165500000000                   -        (S/N)=N                        funz.20¬
165600000000                   -        (S/N)=N                        funz.21¬
165700000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
165800000000                   -        (S/N)=N                        funz.23¬
165900000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
