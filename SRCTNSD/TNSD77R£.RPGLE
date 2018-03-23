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
001200060424     FAZcep02l  IF   E           K DISK
001300000000     FTABEL00F  IF   E           K DISK
001400000000     FAZORG01L  IF   E           K DISK
001500991224     FANBCA01L  IF   E           K DISK
001600000000     D*------------------------------------------------------------------------*
001700991224     D* DATA STRUCTURE
001800000000     D*------------------------------------------------------------------------*
001900000000     D*--------------
002000000000     D* MESSAGGI
002100000000     D*--------------
002200000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002300000000     D*--------------
002400000000     D* COMANDI
002500000000     D*--------------
002600000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002700000000     D*--------------
002800000000     D* DESCRIZIONI TASTI FUNZIONALI
002900000000     D*--------------
003000000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100000000     D*--------------
003200000000     D* TASTI FUNZIONALI
003300000000     D*--------------
003400000000     D FUV             S             19    DIM(24)
003500971202     D*-------------------
003600971202     D* FILIALI E TERMINAL
003700971202     D*-------------------
003800971202     D FIL             S              3  0 DIM(500)                             *FILIALE
003900971202     D TFP             S              3  0 DIM(500)                             *TERMINAL PARTENZA
004000971202     D TFA             S              3  0 DIM(500)                             *TERMINAL ARRIVO
004100971202     D*--------------
004200971202     D* FILIALI E TERMINAL DA ELABORARE
004300971202     D*--------------
004400971203     D TP              S              3  0 DIM(5000)                            *PARTENZA
004500971203     D TA              S              3  0 DIM(5000)                            *ARRIVO
004600971203     D*--------------
004700971203     D* COMBINAZIONE PARTENZA / ARRIVO
004800971203     D*--------------
004900971203     D CPA             S              6  0 DIM(5000)
005000000000     D*--------------
005100000000     D* GIORNI DELLA SETTIMANA DI OGNI MESE DELL'ANNO
005200000000     D*--------------
005300000000     D SE              S             31    DIM(12)
005400000000     D*-------------------
005500000000     D* RIDEFINIZIONE SCHIERA -FUD-
005600000000     D*-------------------
005700000000     D FUDDS           DS
005800000000     D  FUDDE1                 1      1
005900000000     D  FUDFUN                 2      3
006000000000     D  FUDDE2                 4     19
006100000000     D  FUDFI0                20     34
006200000000     D  FUDATT                35     35
006300000000     D  FUDFI1                36     80
006400000000     D  FUDDES                 1     19
006500000000     D  FUDRIG                 1     80
006600000000     D*-------------------
006700000000     D* DS "XMSG" - GESTIONE MESSAGGI PGM
006800000000     D*-------------------
006900000000     D DSMSG         E DS
007000000000     D*-------------------
007100000000     D* DS GESTIONE TASTI FUNZIONALI
007200000000     D*-------------------
007300000000     D DSKEY         E DS
007400000000     D DEVDS1          DS
007500000000     D  KEY                  369    369
007600000000     D*-------------------
007700000000     D* RIDEFINIZIONE DATA
007800000000     D*-------------------
007900000000     D                 DS
008000000000     D  WGG                    1      2  0
008100000000     D  WMM                    3      4  0
008200000000     D  WAA                    5      6  0
008300991224     D  WDATA                  1      6  0
008400000000     D*-------------------
008500000000     D* RIDEFINIZIONE DATA VIDEO
008600000000     D*-------------------
008700000000     D                 DS
008800000000     D  VIDGGD                 1      2  0
008900000000     D  VIDMMD                 3      4  0
009000000000     D  VIDAAD                 5      8  0
009100000000     D  VIDDAT                 1      8  0
009200000000     D*-------------------
009300000000     D* DS GIORNI FESTIVI
009400000000     D*-------------------
009500000000     D GFEDS           DS
009600000000     D  GFE                    1     31
009700000000     D                                     DIM(31)
009800000000     D*-------------------
009900000000     D* DS GIORNI SETTIMANA
010000000000     D*-------------------
010100000000     D GSEDS           DS
010200000000     D  GSE                    1     31
010300000000     D                                     DIM(31)
010400000000     D*-------------------
010500000000     D* DS FLAG MATTINA
010600000000     D*-------------------
010700000000     D GMADS           DS
010800000000     D  GMA                    1     31
010900000000     D                                     DIM(31)
011000000000     D*-------------------
011100000000     D* DS FLAG POMERIGGIO
011200000000     D*-------------------
011300000000     D GPODS           DS
011400000000     D  GPO                    1     31
011500000000     D                                     DIM(31)
011600000000     D*-------------------
011700000000     D* DS SIMBOLI CALENDARIO
011800000000     D*-------------------
011900000000     D DS1Y          E DS
012000000000     D*------------------
012100000000     D* DS "FNLV55R" - REPERIMENTO TERMINALs
012200000000     D*------------------
012300000000     D DSLV55        E DS                  EXTNAME(FNLV55DS)
012400000000     D*-------------------
012500000000     D* ARCHITETTURA
012600000000     D*-------------------
012700000000     D KPJBA         E DS
012800060424     d* Passaggio parametri a duplica calendario eventi particolari
012900060424     D param           DS
013000060424     D  vidcepn                1      4  0
013100060424     D  vidcepo                5      8  0
013200991224     d*---
013300991224     d* ds controlla data (8)
013400991224     d*---
013500991224     d wlbda8          DS                  INZ                                  *controlla data
013600991224     d  g08dat                        8  0
013700991224     d  g08inv                        8  0
013800991224     d  g08err                        1
013900991224     d  g08tgi                        5  0
014000991224     d*---
014100991224     d* variabili
014200991224     d*---
014300991224     d n14             S             14  0 INZ                                  *numerico 14
014400991224     d n8              S              8  0 INZ                                  *numerico 8
014500991224     d n4              S              4  0 INZ                                  *numerico 4
014600991224     d datcor          S              8  0 INZ                                  *data correrte a/m/g
014700991224     d*---
014800991224     d* Variabili riferite al data base
014900991224     d*---
015000991224     d KCACALEND       S                   LIKE(BCACALEND)                      *ANBCA01L
015100991224     d KCAANNO         S                   LIKE(BCAANNO)
015200991224     d KCAMESE         S                   LIKE(BCAMESE)
015300060424     d savcepn         S                   LIKE(vidcepn)
015400060424     d kdev            S                   LIKE(cepdev)
015500060424     d wforza          S              1
015600000000     C*------------------------------------------------------------------------*
015700991224     C* MAIN LINES
015800000000     C*------------------------------------------------------------------------*
015900000000     C*
016000000000     C* SE CONTROLLI INIZIALI NON OK, RITORNO A PGM CHIAMANTE
016100000000IF  1C     ERRL00        IFEQ      '1'
016200000000     C*
016300000000     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
016400000000     C                   MOVEL     '1'           DSCERR
016500000000     C                   MOVEL     DSMSMS        DSCMSG
016600000000     C*
016700000000     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
016800000000X   1C                   ELSE
016900000000DO  2C     F03L00        DOUEQ     '1'
017000000000     C     F12L00        OREQ      '1'
017100000000     C     FLFINE        OREQ      '1'
017200000000     C*
017300000000     C* EMETTE FORMATO VIDEO
017400000000     C                   EXSR      BUTFMR
017500000000     C*
017600000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
017700000000     C                   EXSR      FUNCON
017800000000E   2C                   ENDDO
017900000000E   1C                   ENDIF
018000000000     C* FNLV55R
018100000000     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
018200000000     C                   MOVEL     'C'           D55TLA
018300000000     C                   CALL      'FNLV55R'
018400000000     C                   PARM                    DSLV55
018500000000     C*
018600000000     C                   SETON                                        LR
018700000000     C*------------------------------------------------------------------------*
018800000000     C* BUTMFR - EMISSIONE FORMATO A VIDEO
018900000000     C*------------------------------------------------------------------------*
019000000000     C     BUTFMR        BEGSR
019100000000     C*
019200000000     C* WRITE RECORD TESTATA
019300000000     C                   WRITE     SD77D01
019400000000     C*
019500000000     C* VISUALIZZAZIONE MESSAGGIO
019600000000     C                   EXSR      SNDMSG
019700000000     C*
019800000000     C* WRITE RECORD FUNZIONI
019900000000     C                   WRITE     SD77D02
020000000000     C*
020100000000     C* EXFMT RECORD DETTAGLIO
020200000000     C                   EXFMT     SD77D03
020300000000     C*
020400000000     C* AZZERO FLAG DI ERRORE LIVELLO
020500000000     C                   MOVEL     '0'           ERRL00
020600000000     C*
020700000000     C* AZZERO FLAG DI ERRORE
020800000000     C                   SETOFF                                       606162
020900000000     C                   SETOFF                                       636465
021000000000     C                   SETOFF                                       666768
021100000000     C                   SETOFF                                       697071
021200060424     C                   SETOFF                                       7273
021300000000     C*
021400000000     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C* SNDMSG - MANDA MESSAGGIO
021700000000     C*------------------------------------------------------------------------*
021800000000     C     SNDMSG        BEGSR
021900000000     C*
022000000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
022100000000     C                   MOVEL     '1'           *IN40
022200000000     C                   WRITE     SD77DMC
022300000000     C                   MOVEL     '0'           *IN40
022400000000     C*
022500000000     C* VISUALIZZAZIONE MESSAGGIO
022600000000     C                   Z-ADD     0             MSGULT            4 0
022700000000     C                   CALL      'XMSG'        XMSGPA
022800000000     C     DSMSER        IFEQ      0
022900000000     C                   MOVEL     DSMSKE        MSGKEY
023000000000     C                   END
023100000000     C                   Z-ADD     1             MSGULT
023200000000     C                   WRITE     SD77DMS                                      SFL MESS
023300000000     C                   WRITE     SD77DMC                                      CONTR MESS
023400000000     C*
023500000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
023600000000     C                   MOVEL     *BLANKS       DSMSMS
023700000000     C*
023800000000     C                   ENDSR
023900000000     C*------------------------------------------------------------------------*
024000000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
024100000000     C*------------------------------------------------------------------------*
024200000000     C     ENTGES        BEGSR
024300000000     C*
024400000000     C* CONTROLLA I CAMPI IMMESSI
024500000000     C                   EXSR      DECODI
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
025000000000     C*------------------------------------------------------------------------*
025100000000     C     F01GES        BEGSR
025200000000     C                   ENDSR
025300000000     C*------------------------------------------------------------------------*
025400000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
025500000000     C*------------------------------------------------------------------------*
025600000000     C     F03GES        BEGSR
025700000000     C*
025800000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
025900000000     C                   MOVEL     '1'           F03L00
026000971127     C*
026100000000     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
026200000000     C                   MOVEL     '1'           DSCF03
026300000000     C*
026400000000     C                   ENDSR
026500000000     C*------------------------------------------------------------------------*
026600000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
026700000000     C*------------------------------------------------------------------------*
026800000000     C     F06GES        BEGSR
026900000000     C*
027000000000     C* CONTROLLA I CAMPI IMMESSI
027100000000     C                   EXSR      DECODI
027200000000     C*
027300000000     C* CARICA LA SCHIERA DELLE FILIALI
027400971127IF  1C     ERRL00        IFEQ      '0'
027500000000     C                   EXSR      CARFIL
027600971127IF  2C     ERRL00        IFEQ      '0'
027700000000     C*
027800000000     C* IN BASE AL TIPO DI AUTOMATISMO RICHIAMA LA ROUITINE INTERESSATA
027900971127IF  3C     VIDTIP        IFEQ      'C'
028000000000     C                   EXSR      RSTAND
028100971127E   3C                   ENDIF
028200971127IF  3C     VIDTIP        IFEQ      'D'
028300000000     C                   EXSR      RDATA
028400971127E   3C                   ENDIF
028500971127IF  3C     VIDTIP        IFEQ      'G'
028600000000     C                   EXSR      RGIORN
028700971127E   3C                   ENDIF
028800060424     c*  Duplica calendario eventi particolari anno nuovo
028900060424IF  3C     VIDTIP        IFEQ      'E'
029000060424     c                   movel     param         kpjbu
029100060424     c                   call      'TISIP4R'
029200060424     c                   parm                    kpjba
029300060424     c                   endif
029400060424
029500000000     C*
029600000000     C* MANDO MESSAGGIO DI AUTOMATISMO APPLICATO
029700971127IF  3C     ERRL00        IFEQ      '0'
029800000000     C                   MOVEL     MSG(3)        DSMSMS
029900971127E   3C                   ENDIF
030000971127E   2C                   ENDIF
030100971127E   1C                   ENDIF
030200000000     C*
030300000000     C                   ENDSR
030400000000     C*------------------------------------------------------------------------*
030500000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
030600000000     C*------------------------------------------------------------------------*
030700000000     C     F07GES        BEGSR
030800000000     C                   ENDSR
030900000000     C*------------------------------------------------------------------------*
031000000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
031100000000     C*------------------------------------------------------------------------*
031200000000     C     F08GES        BEGSR
031300000000     C                   ENDSR
031400000000     C*------------------------------------------------------------------------*
031500000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
031600000000     C*------------------------------------------------------------------------*
031700000000     C     F12GES        BEGSR
031800000000     C*
031900000000     C* IMPOSTO FLAG DI LIVELLO
032000000000     C                   MOVEL     '1'           F12L00
032100000000     C*
032200000000     C                   ENDSR
032300000000     C*------------------------------------------------------------------------*
032400000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA CAPPARIO
032500000000     C*------------------------------------------------------------------------*
032600000000     C     F21GES        BEGSR
032700000000     C                   ENDSR
032800000000     C*------------------------------------------------------------------------*
032900000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
033000000000     C*------------------------------------------------------------------------*
033100000000     C     F22GES        BEGSR
033200000000     C*
033300000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
033400000000     C                   Z-ADD     80            LENGH            15 5
033500000000     C                   CALL      'QCAEXEC'
033600000000     C                   PARM                    CMD(1)
033700000000     C                   PARM                    LENGH
033800000000     C*
033900000000     C                   ENDSR
034000000000     C*------------------------------------------------------------------------*
034100000000     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
034200000000     C*------------------------------------------------------------------------*
034300000000     C     F24GES        BEGSR
034400000000     C*
034500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
034600000000     C                   EXSR      FUNGES
034700000000     C*
034800000000     C                   ENDSR
034900000000     C*------------------------------------------------------------------------*
035000000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
035100000000     C*------------------------------------------------------------------------*
035200000000     C     HELGES        BEGSR
035300000000     C                   ENDSR
035400000000     C*------------------------------------------------------------------------*
035500000000     C* RGIORN - IMPOSTA UN FLAG IN UN GIORNO DELLA SETTIMANA
035600000000     C*------------------------------------------------------------------------*
035700000000     C     RGIORN        BEGSR
035800000000     C*
035900000000     C* CARICO IN SCHIERA TUTTI I GIORNI DELLA SETTIMANA DELL'ANNO
036000971127DO  1C                   DO        12            M                 4 0
036100991224     C                   Z-ADD     VIDAAG        WANN              4 0
036200000000     C                   Z-ADD     M             WMES              2 0
036300000000     C                   EXSR      IMPGFE                                       *GIORNI FESTIVI
036400000000     C                   EXSR      IMPGSE                                       *GIORNI SETTIMANA
036500000000     C                   MOVEL     GSEDS         SE(M)
036600971127E   1C                   ENDDO
036700000000     C*
036800000000     C* LEGGO TUTTO IL CALENDARIO E SE IL RECORD RIENTRA NEI PARAMETRI DATI
036900000000     C* A VIDEO LO AGGIORNO
037000000000     C                   Z-ADD     *ZEROS        KCATFP
037100000000     C                   Z-ADD     *ZEROS        KCATFA
037200000000     C                   Z-ADD     9999          KCAANN
037300000000     C                   Z-ADD     99            KCAMES
037400000000     C     KEYCAL        SETLL     AZCLN01L                           99        *FINE-ARCHIV
037500000000     C  N99              READ      AZCLN01L                               99
037600971127DO  1C     *IN99         DOWEQ     '0'
037700971127IF  2C     CLNATB        IFNE      'A'
037800971127IF  3C     CLNANN        IFEQ      VIDAAG
037900000000     C                   MOVE      CLNTFP        WCRPAR            3
038000000000     C                   MOVE      CLNTFA        WCRARR            3
038100000000     C                   EXSR      CHKREC
038200000000     C* AGGIORNO RECORD
038300971127IF  4C     WDAAGG        IFEQ      'S'
038400000000     C                   MOVEL     CLNMAT        GMADS
038500000000     C                   MOVEL     CLNPOM        GPODS
038600000000     C                   Z-ADD     CLNMES        MM                2 0
038700000000     C                   MOVEL     SE(MM)        GSEDS
038800971127DO  5C                   DO        31            GG                2 0
038900971127IF  6C     GSE(GG)       IFEQ      VIDGIO
039000000000     C                   MOVEL     VIDFMG        GMA(GG)
039100000000     C                   MOVEL     VIDFPG        GPO(GG)
039200971127E   6C                   ENDIF
039300971127E   5C                   ENDDO
039400000000     C                   MOVEL     GMADS         CLNMAT
039500000000     C                   MOVEL     GPODS         CLNPOM
039600000000     C                   MOVEL     *BLANKS       CLNFTR
039700000000     C                   Z-ADD     *ZEROS        CLNDTR
039800000000     C                   MOVEL     *BLANKS       CLNATB
039900000000     C                   UPDATE    AZCLN000
040000971127E   4C                   ENDIF
040100971127E   3C                   ENDIF
040200971127E   2C                   ENDIF
040300000000     C                   READ      AZCLN01L                               99
040400971127E   1C                   ENDDO
040500000000     C*
040600000000     C                   ENDSR
040700000000     C*------------------------------------------------------------------------*
040800000000     C* RDATA  - MODIFICA I FLAG DI UNA PARTICOLARE DATA
040900000000     C*------------------------------------------------------------------------*
041000000000     C     RDATA         BEGSR
041100000000     C*
041200000000     C* LEGGO TUTTO IL CALENDARIO E SE IL RECORD RIENTRA NEI PARAMETRI DATI
041300000000     C* A VIDEO LO AGGIORNO
041400000000     C                   Z-ADD     *ZEROS        KCATFP
041500000000     C                   Z-ADD     *ZEROS        KCATFA
041600000000     C                   Z-ADD     9999          KCAANN
041700000000     C                   Z-ADD     99            KCAMES
041800000000     C     KEYCAL        SETLL     AZCLN01L                           99        *FINE-ARCHIV
041900000000     C  N99              READ      AZCLN01L                               99
042000971127DO  1C     *IN99         DOWEQ     '0'
042100971127IF  2C     CLNATB        IFNE      'A'
042200971127IF  2C     CLNANN        IFEQ      VIDAAD
042300000000     C     CLNMES        ANDEQ     VIDMMD
042400000000     C                   MOVE      CLNTFP        WCRPAR
042500000000     C                   MOVE      CLNTFA        WCRARR
042600000000     C                   EXSR      CHKREC
042700000000     C* AGGIORNO RECORD
042800971127IF  3C     WDAAGG        IFEQ      'S'
042900000000     C                   MOVEL     CLNMAT        GMADS
043000000000     C                   MOVEL     CLNPOM        GPODS
043100971202     C                   Z-ADD     VIDGGD        K                 6 0
043200000000     C                   MOVEL     VIDFMD        GMA(K)
043300000000     C                   MOVEL     VIDFPD        GPO(K)
043400000000     C                   MOVEL     GMADS         CLNMAT
043500000000     C                   MOVEL     GPODS         CLNPOM
043600000000     C                   MOVEL     *BLANKS       CLNFTR
043700000000     C                   Z-ADD     *ZEROS        CLNDTR
043800000000     C                   MOVEL     *BLANKS       CLNATB
043900000000     C                   UPDATE    AZCLN000
044000971127E   3C                   ENDIF
044100971127E   2C                   ENDIF
044200971127E   1C                   ENDIF
044300000000     C*
044400000000     C* LEGGE IL RECORD SUCCESSIVO
044500000000     C                   READ      AZCLN01L                               99
044600000000     C*
044700000000     C                   END                                                    <-- 1 ---
044800000000     C*
044900000000     C                   ENDSR
045000971202     C*------------------------------------------------------------------------*
045100971202     C* CHKREC - CONTROLLA SE IL RECORD SODDISFA I PARAMETRI DI LANCIO
045200971202     C*------------------------------------------------------------------------*
045300971202     C     CHKREC        BEGSR
045400971202     C*
045500971202     C                   MOVEL     'N'           WDAAGG            1
045600971202     C     VIDTTF        IFEQ      'X'                                          --- 1 -->
045700971202     C                   MOVEL     'S'           WDAAGG
045800971202     C                   ELSE                                                   <-- 1 -->
045900971202     C     VIDTTF        IFEQ      'A'
046000971202     C     WCRPAR        IFEQ      *ZEROS
046100971202     C     WCRARR        IFNE      *ZEROS
046200971202     C                   MOVEL     'S'           WDAAGG
046300971202     C                   END
046400971202     C                   END
046500971202     C                   END
046600971202     C     VIDTTF        IFEQ      'P'
046700971202     C     WCRARR        IFEQ      *ZEROS
046800971202     C     WCRPAR        IFNE      *ZEROS
046900971202     C                   MOVEL     'S'           WDAAGG
047000971202     C                   END
047100971202     C                   END
047200971202     C                   END
047300971202     C     VIDTTF        IFEQ      'T'
047400971202     C     WCRPAR        IFNE      *ZEROS
047500971202     C     WCRARR        IFNE      *ZEROS
047600971202     C                   MOVEL     'S'           WDAAGG
047700971202     C                   END
047800971202     C                   END
047900971202     C                   END
048000971202     C     WCRPAR        IFEQ      VIDPAR
048100971202     C     WCRARR        ANDEQ     VIDARR
048200971202     C                   MOVEL     'S'           WDAAGG
048300971202     C                   END
048400971202     C     WCRPAR        IFEQ      VIDPAR
048500971202     C     WCRARR        ANDNE     *ZEROS
048600971202     C     VIDARR        IFEQ      '999'
048700971202     C     VIDARR        OREQ      '888'
048800971202     C                   MOVEL     'S'           WDAAGG
048900971202     C                   END
049000971202     C                   END
049100971202     C     WCRARR        IFEQ      VIDARR
049200971202     C     WCRPAR        ANDNE     *ZEROS
049300971202     C     VIDPAR        IFEQ      '999'
049400971202     C     VIDPAR        OREQ      '888'
049500971202     C                   MOVEL     'S'           WDAAGG
049600971202     C                   END
049700971202     C                   END
049800971202     C*
049900971202     C                   END                                                    <-- 1 ---
050000971202     C*
050100971202     C                   ENDSR
050200000000     C*------------------------------------------------------------------------*
050300000000     C* RSTAND - CREAZIONE CALENDARIO STANDARD PER UN ANNO
050400000000     C*------------------------------------------------------------------------*
050500000000     C     RSTAND        BEGSR
050600000000     C*
050700000000     C* LOOP PER DODICI MESI
050800971203DO  1C                   DO        12            M                 4 0          --- 1 -->
050900991224     C                   Z-ADD     VIDAAC        WANN              4 0
051000000000     C                   Z-ADD     M             WMES              2 0
051100000000     C*
051200000000     C* LANCIO ROUTINE PER IMPOSTARE GIORNI FESTIVI DEL MESE
051300000000     C                   EXSR      IMPGFE
051400000000     C*
051500000000     C* LANCIO ROUTINE PER IMPOSTARE GIORNI SETTIMANA DEL MESE
051600000000     C                   EXSR      IMPGSE
051700971203     C*---
051800971203     C* SCRITTURA PER TIPO TEMPO
051900971203     C*---
052000971203IF  2C     VIDPAR        IFEQ      *ZEROS
052100000000     C     VIDARR        ANDEQ     *ZEROS
052200000000     C                   EXSR      INSER0
052300971203DO  3C     1             DO        WUTP          J
052400971203     C                   Z-ADD     TP(J)         WPAR              3 0
052500971203     C                   Z-ADD     TA(J)         WARR              3 0
052600971203     C                   EXSR      INSERI
052700971203E   3C                   ENDDO                                                  *FINE TERMINAL
052800971203     C*---
052900971203     C* SCRITTURA PER TIPO TERMINAL / TERMINAL
053000971203     C*---
053100971203X   2C                   ELSE
053200971203     C*
053300971203     C* CICLO FINO A FINE TERMINAL PARTENZA
053400971203DO  3C     1             DO        WUTP          J
053500971203     C*
053600971203     C* CICLO FINO A FINE TERMINAL ARRIVO
053700971203DO  4C     1             DO        WUTA          K                 6 0
053800000000     C*
053900971203     C* SCRITTURA CALENDARIO
054000971203     C                   Z-ADD     TP(J)         WPAR              3 0
054100971203     C                   Z-ADD     TA(K)         WARR              3 0
054200000000     C                   EXSR      INSERI
054300971203E   4C                   ENDDO                                                  *FINE ARRIVO
054400971203E   3C                   ENDDO                                                  *FINE PARTENZA
054500971203E   2C                   ENDIF
054600971203E   1C                   ENDDO                                                  *FINE MESI
054700000000     C*
054800000000     C                   ENDSR
054900000000     C*------------------------------------------------------------------------*
055000000000     C* INSERI - CREA UN RECORD NEL CALEDARIO
055100000000     C*------------------------------------------------------------------------*
055200000000     C     INSERI        BEGSR
055300000000     C*
055400000000     C                   MOVE      WPAR          KCATFP
055500000000     C                   MOVE      WARR          KCATFA
055600991224     C                   Z-ADD     WANN          KCAANN
055700000000     C                   Z-ADD     WMES          KCAMES
055800000000     C     KEYCAL        CHAIN     AZCLN01L                           99
055900000000     C                   MOVE      WPAR          CLNTFP
056000000000     C                   MOVE      WARR          CLNTFA
056100000000     C                   Z-ADD     KCAANN        CLNANN
056200000000     C                   Z-ADD     WMES          CLNMES
056300000000     C                   MOVEL     GFEDS         CLNMAT
056400000000     C                   MOVEL     GFEDS         CLNPOM
056500000000     C                   MOVEL     *BLANKS       CLNFTR
056600000000     C                   Z-ADD     *ZEROS        CLNDTR
056700000000     C                   MOVEL     *BLANKS       CLNATB
056800000000     C     *IN99         IFEQ      '1'
056900000000     C                   WRITE     AZCLN000
057000000000     C                   ELSE
057100000000     C                   UPDATE    AZCLN000
057200000000     C                   END
057300000000     C*
057400000000     C                   ENDSR
057500000000     C*------------------------------------------------------------------------*
057600000000     C* INSER0 - CREA UN RECORD DI TIPO MESE NEL CALEDARIO
057700000000     C*------------------------------------------------------------------------*
057800000000     C     INSER0        BEGSR
057900000000     C*
058000000000     C                   Z-ADD     *ZEROS        KCATFP
058100000000     C                   Z-ADD     *ZEROS        KCATFA
058200991224     C                   Z-ADD     WANN          KCAANN
058300000000     C                   Z-ADD     WMES          KCAMES
058400000000     C     KEYCAL        CHAIN     AZCLN01L                           99
058500000000     C                   Z-ADD     *ZEROS        CLNTFP
058600000000     C                   Z-ADD     *ZEROS        CLNTFA
058700000000     C                   Z-ADD     KCAANN        CLNANN
058800000000     C                   Z-ADD     WMES          CLNMES
058900000000     C                   MOVEL     GSEDS         CLNMAT
059000000000     C                   MOVEL     GFEDS         CLNPOM
059100000000     C                   MOVEL     *BLANKS       CLNFTR
059200000000     C                   Z-ADD     *ZEROS        CLNDTR
059300000000     C                   MOVEL     *BLANKS       CLNATB
059400000000     C     *IN99         IFEQ      '1'
059500000000     C                   WRITE     AZCLN000
059600000000     C                   ELSE
059700000000     C                   UPDATE    AZCLN000
059800000000     C                   END
059900000000     C*
060000000000     C                   ENDSR
060100000000     C*------------------------------------------------------------------------*
060200000000     C* IMPGFE - IMPOSTA GIORNI FESTIVI
060300000000     C*------------------------------------------------------------------------*
060400000000     C     IMPGFE        BEGSR
060500000000     C*
060600000000     C* LEGGE SUL FILE BNBCA00F IL RECORD DEL MESE
060700991224     C                   MOVEL     'CB'          KCACALEND
060800991224     C                   Z-ADD     WANN          KCAANNO
060900991224     C                   Z-ADD     WMES          KCAMESE
061000991224     C     KEYBCA        CHAIN     ANBCA01L                           99
061100000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
061200000000     C                   MOVEL     '1'           ERRL00
061300000000     C                   MOVEL     MSG(8)        DSMSMS
061400000000     C                   ELSE                                                   <-- 1 -->
061500000000     C                   MOVEL     BCAGPF        GFEDS
061600000000     C                   DO        31            X                 6 0          --- 2 -->
061700000000     C     GFE(X)        IFEQ      *BLANKS                                      --- 3 -->
061800000000     C                   MOVEL     '*'           GFE(X)
061900000000     C                   ELSE                                                   <-- 3 -->
062000000000     C     GFE(X)        IFEQ      *ZEROS
062100000000     C                   MOVEL     ' '           GFE(X)
062200000000     C                   ELSE
062300000000     C                   MOVEL     'F'           GFE(X)
062400000000     C                   END
062500000000     C                   END                                                    <-- 3 ---
062600000000     C                   END                                                    <-- 2 ---
062700000000     C                   END                                                    <-- 1 ---
062800000000     C*
062900000000     C                   ENDSR
063000000000     C*------------------------------------------------------------------------*
063100971203     C* IMPGSE - IMPOSTA GIORNI DELLA SETTIMANA
063200000000     C*------------------------------------------------------------------------*
063300000000     C     IMPGSE        BEGSR
063400000000     C*
063500000000     C* LANCIO COMANDO RTVDAT PER AVERE IL GIORNO DELLA SETTIMANA
063600991224     C                   MOVE      WANN          WAA                            *ANNO (2)
063700000000     C                   Z-ADD     WMES          WMM
063800000000     C                   DO        31            X                 6 0          --- 1 -->
063900000000     C     GFE(X)        IFEQ      '*'                                          --- 2 -->
064000000000     C                   MOVEL     '*'           GSE(X)
064100000000     C                   ELSE                                                   <-- 2 -->
064200000000     C                   Z-ADD     X             WGG
064300000000     C                   MOVE      WDATA         C98DAT
064400000000     C                   CALL      'XGIOSET'
064500000000     C                   PARM                    C98DAT            6
064600000000     C                   PARM                    C98GSE            1
064700000000     C                   MOVE      C98GSE        W98GSE            1 0
064800000000     C     W98GSE        IFEQ      1
064900000000     C                   Z-ADD     7             W98GSE
065000000000     C                   ELSE
065100000000     C                   SUB       1             W98GSE
065200000000     C                   END
065300000000     C                   MOVE      W98GSE        GSE(X)
065400000000     C                   END                                                    <-- 2 ---
065500000000     C                   END                                                    <-- 1 ---
065600000000     C*
065700000000     C                   ENDSR
065800000000     C*------------------------------------------------------------------------*
065900000000     C* CARFIL - CARICA LA SCHIERA DELLE FILIALI/FILIALI
066000000000     C*------------------------------------------------------------------------*
066100000000     C     CARFIL        BEGSR
066200000000     C*
066300971202     C* CARICA TUTTE LE FILIALI
066400000000     C                   EXSR      CARORG
066500971215     C*
066600971202     C* CARICA TUTTI I TERMINAL
066700971215     C                   EXSR      CARCAE
066800971127     C*---
066900971202     C* CREAZIONE PER COMBINAZIONE FILIALE O TERMINAL
067000971127     C*---
067100971215     C                   Z-ADD     *ZEROS        J                 6 0
067200971127IF  1C     VIDPAR        IFNE      *ZEROS
067300000000     C     VIDARR        ORNE      *ZEROS
067400971202     C* PARTENZA
067500971202IF  2C     VIDPAR        IFEQ      '888'
067600971202DO  3C                   DO        WUTFP         J
067700971202     C                   Z-ADD     TFP(J)        TP(J)                          *TERMINAL PARTENZA
067800971202E   3C                   ENDDO
067900971127X   2C                   ELSE
068000971202IF  3C     VIDPAR        IFEQ      '999'
068100971215DO  4C                   DO        WUFIL         J
068200971202     C                   Z-ADD     FIL(J)        TP(J)                          *FILIALE PARTENZA
068300971215E   4C                   ENDDO
068400971202X   3C                   ELSE
068500971202     C                   Z-ADD     1             J
068600971202     C                   MOVEL     VIDPAR        TP(J)
068700971202E   3C                   ENDIF
068800971202E   2C                   ENDIF
068900971202     C                   Z-ADD     J             WUTP
069000971202     C* ARRIVO
069100971202IF  2C     VIDARR        IFEQ      '888'
069200971202DO  3C                   DO        WUTFA         J
069300971202     C                   Z-ADD     TFA(J)        TA(J)                          *TERMINAL ARRIVO
069400971202E   3C                   ENDDO
069500971202X   2C                   ELSE
069600971202IF  3C     VIDARR        IFEQ      '999'
069700971215DO  4C                   DO        WUFIL         J
069800971202     C                   Z-ADD     FIL(J)        TA(J)                          *FILIALE ARRIVO
069900971215E   4C                   ENDDO
070000971202X   3C                   ELSE
070100971202     C                   Z-ADD     1             J
070200971202     C                   MOVEL     VIDARR        TA(J)
070300971202E   3C                   ENDIF
070400971202E   2C                   ENDIF
070500971202     C                   Z-ADD     J             WUTA
070600971127     C*---
070700971202     C* CREAZIONE PER TIPO TEMPO
070800971127     C*---
070900971202X   1C                   ELSE
071000000000     C*
071100971127     C* TERMINAL / TERMINAL
071200971215IF  2C     VIDTTF        IFEQ      'T'
071300971215     C     VIDTTF        OREQ      'X'
071400971215     C                   Z-ADD     *ZEROS        K                 6 0
071500971215DO  3C     1             DO        WUTFP         Y                 6 0
071600971215DO  4C     1             DO        WUTFA         X                 6 0
071700971215     C                   SETOFF                                           28
071800971215     C                   MOVEL     TFP(Y)        WCOM              6 0
071900971215     C                   MOVE      TFA(X)        WCOM
072000971215     C     WCOM          LOOKUP    CPA                                    28
072100971215IF  5C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATA
072200971215     C                   ADD       1             K
072300971215     C                   Z-ADD     WCOM          CPA(K)
072400971215     C                   ADD       1             J
072500971215     C                   Z-ADD     TFP(Y)        TP(J)
072600971215     C                   Z-ADD     TFA(X)        TA(J)
072700971215E   5C                   ENDIF
072800971215E   4C                   ENDDO                                                  *FINE TERM ARRIVO
072900971215E   3C                   ENDDO                                                  *FINE TERM PARTENZA
073000971215E   2C                   ENDIF
073100971127     C*
073200971127     C* FILIALI ARRIVO
073300971202IF  2C     VIDTTF        IFEQ      'A'
073400000000     C     VIDTTF        OREQ      'X'
073500971215DO  3C     1             DO        WUFIL         K
073600971203     C                   ADD       1             J
073700971203     C                   Z-ADD     *ZEROS        TP(J)                          *ZERO
073800971203     C                   Z-ADD     FIL(K)        TA(J)                          *TUTTE LE FILIALI
073900971202E   3C                   ENDDO
074000971202E   2C                   ENDIF
074100971127     C*
074200971127     C* FILIALI PARTENZA
074300971202IF  2C     VIDTTF        IFEQ      'P'
074400000000     C     VIDTTF        OREQ      'X'
074500971215DO  3C     1             DO        WUFIL         K
074600971203     C                   ADD       1             J
074700971203     C                   Z-ADD     FIL(K)        TP(J)                          *TUTTE LE FILIALI
074800971203     C                   Z-ADD     *ZEROS        TA(J)                          *ZERO
074900971202E   3C                   ENDDO
075000971202E   2C                   ENDIF
075100971203     C                   Z-ADD     J             WUTP
075200971203     C                   Z-ADD     J             WUTA
075300971203E   1C                   ENDIF
075400000000     C*
075500000000     C                   ENDSR
075600000000     C*------------------------------------------------------------------------*
075700971202     C* CARORG - CARICA LE FILIALI
075800000000     C*------------------------------------------------------------------------*
075900000000     C     CARORG        BEGSR
076000971202     C*
076100971202     C* AZZERA LE VARIABILI DI LAVORO
076200971202     C                   Z-ADD     *ZEROS        J                 6 0
076300971202     C*
076400971202     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
076500971202     C     *LOVAL        SETLL     AZORG01L
076600971202     C                   READ      AZORG01L                               99
076700971202DO  1C     *IN99         DOWEQ     *OFF
076800971202IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDO
076900971202     C     ORGFAG        ANDNE     'V'                                          *FILIALI/AGENZIE
077000971202     C                   ADD       1             J
077100971202     C                   Z-ADD     ORGFIL        FIL(J)
077200971202E   2C                   ENDIF
077300971202     C                   READ      AZORG01L                               99
077400971202E   1C                   ENDDO
077500971202     C                   Z-ADD     J             WUFIL                          *ULTIMA FIL
077600000000     C*
077700000000     C                   ENDSR
077800971202     C*------------------------------------------------------------------------*
077900971202     C* CARCAE - CARICA I TERMINAL PARTENZA E ARRIVO
078000971202     C*------------------------------------------------------------------------*
078100971202     C     CARCAE        BEGSR
078200971202     C* ARRIVO
078300971202     C                   Z-ADD     *ZEROS        J                 6 0
078400971202     C                   MOVEL     'A'           KCEEPA                         *ARRIVO
078500971202     C     KCEEPA        SETLL     AZCAE01L
078600971202     C     KCEEPA        READE     AZCAE01L                               99
078700971202DO  1C     *IN99         DOWEQ     *OFF
078800971202IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
078900971202IF  3C     DATCOR        IFGE      CAEDDE                                       *IN DECORRENZA
079000971202     C     DATCOR        ANDLE     CAEDSC
079100971202     C                   SETOFF                                           28
079200971202     C     CAETFE        LOOKUP    TFA                                    28
079300971202IF  4C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATO
079400971202     C                   ADD       1             J
079500971202     C                   Z-ADD     CAETFE        TFA(J)
079600971202E   4C                   ENDIF
079700971202E   3C                   ENDIF
079800971202E   2C                   ENDIF
079900971202     C     KCEEPA        READE     AZCAE01L                               99
080000971202E   1C                   ENDDO
080100971202     C                   Z-ADD     J             WUTFA                          *ULTIMO TFA
080200971202     C* PARTENZA
080300971202     C                   Z-ADD     *ZEROS        J                 6 0
080400971202     C                   MOVEL     'P'           KCEEPA                         *ARRIVO
080500971202     C     KCEEPA        SETLL     AZCAE01L
080600971202     C     KCEEPA        READE     AZCAE01L                               99
080700971202DO  1C     *IN99         DOWEQ     *OFF
080800971202IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDO
080900971202IF  3C     DATCOR        IFGE      CAEDDE                                       *IN DECORRENZA
081000971202     C     DATCOR        ANDLE     CAEDSC
081100971202     C                   SETOFF                                           28
081200971202     C     CAETFE        LOOKUP    TFP                                    28
081300971202IF  4C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATO
081400971202     C                   ADD       1             J
081500971202     C                   Z-ADD     CAETFE        TFP(J)
081600971202E   4C                   ENDIF
081700971202E   3C                   ENDIF
081800971202E   2C                   ENDIF
081900971202     C     KCEEPA        READE     AZCAE01L                               99
082000971202E   1C                   ENDDO
082100971202     C                   Z-ADD     J             WUTFP                          *ULTIMO TFP
082200971202     C*
082300971202     C                   ENDSR
082400000000     C*------------------------------------------------------------------------*
082500000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
082600000000     C*------------------------------------------------------------------------*
082700000000     C     DECODI        BEGSR
082800060424     C*
082900060424     C* CONTROLLA TIPO AUTOMATISMO
083000060424     C     VIDTIP        IFEQ      'C'
083100060424     C     VIDTIP        OREQ      'D'
083200060424     C     VIDTIP        OREQ      'G'
083300060424     C     VIDTIP        OREQ      'E'
083400060424     C                   ELSE
083500060424     C                   MOVEL     MSG(9)        DSMSMS
083600060424     C                   MOVEL     '1'           ERRL00
083700060424     C                   SETON                                        69
083800060424     C                   END
083900000000     C*
084000000000     C                   MOVEL     *BLANKS       VIDDPA
084100000000     C                   MOVEL     *BLANKS       VIDDAR
084200971202     C*
084300060424     c* Per automatismo 'E' non ammessi terminal part/arr  e tipo terminal
084400060424    1c                   if        vidtip='E' and errl00='0'
084500060424    2c                   if        vidpar<>*zeros or vidarr<>*zeros or
084600060424     c                             vidttf<>' '
084700060424     C                   MOVEL     MSG(19)       DSMSMS
084800060424     C                   MOVEL     '1'           ERRL00
084900060424     C                   SETON                                        60
085000060424    2c                   endif
085100060424    1c                   endif
085200060424     c
085300060424    0c                   if        errl00='0'
085400971202IF  1C     VIDPAR        IFEQ      *ZEROS
085500971202     C     VIDARR        ANDEQ     *ZEROS
085600971202     C*
085700000000     C* CONTROLLO TIPO FILIALE
085800060424     c*  non obbligatorio se richiesto calendario eventi particolari
085900060424   1ac                   if        vidttf=' ' and vidtip='E'
086000060424   1ac                   else
086100971202IF  2C     VIDTTF        IFEQ      'A'
086200000000     C     VIDTTF        OREQ      'P'
086300000000     C     VIDTTF        OREQ      'T'
086400000000     C     VIDTTF        OREQ      'X'
086500971202X   2C                   ELSE
086600000000     C                   MOVEL     MSG(13)       DSMSMS
086700000000     C                   MOVEL     '1'           ERRL00
086800000000     C                   SETON                                        71
086900971202E   2C                   ENDIF
087000060424E  1aC                   ENDIF
087100000000     C*
087200000000X   1C                   ELSE
087300000000     C*
087400971202IF  2C     VIDTTF        IFNE      *BLANKS
087500000000     C                   MOVEL     MSG(14)       DSMSMS
087600000000     C                   MOVEL     '1'           ERRL00
087700000000     C                   SETON                                        716061
087800971215E   2C                   ENDIF
087900000000     C*
088000000000     C* CONTROLLO TERMINAL/FILIALE PARTENZA
088100971215IF  2C     ERRL00        IFEQ      '0'
088200971215IF  3C     VIDPAR        IFEQ      *ZEROS
088300000000     C     VIDPAR        OREQ      '999'
088400000000     C     VIDPAR        OREQ      '888'
088500971215X   3C                   ELSE
088600000000     C                   MOVE      VIDPAR        WLIN              3
088700000000     C                   EXSR      CHKLNA
088800971215IF  4C     ERRL00        IFEQ      '1'
088900000000     C                   SETON                                        60
089000971215X   4C                   ELSE
089100000000     C                   MOVEL     WLIN          VIDPAR
089200000000     C                   MOVEL     WDLIN         VIDDPA
089300971215E   4C                   END
089400971215E   3C                   END
089500971215E   2C                   END
089600000000     C*
089700000000     C* CONTROLLO TERMINAL/FILIALE ARRIVO
089800971215IF  2C     ERRL00        IFEQ      '0'
089900971215IF  3C     VIDARR        IFEQ      *ZEROS
090000000000     C     VIDARR        OREQ      '999'
090100000000     C     VIDARR        OREQ      '888'
090200971215X   3C                   ELSE
090300000000     C                   MOVE      VIDARR        WLIN
090400000000     C                   EXSR      CHKLNA
090500971215IF  4C     ERRL00        IFEQ      '1'
090600000000     C                   SETON                                        61
090700971215X   4C                   ELSE
090800000000     C                   MOVEL     WLIN          VIDARR
090900000000     C                   MOVEL     WDLIN         VIDDAR
091000971215E   4C                   END
091100971215E   3C                   END
091200971215E   2C                   END
091300000000     C*
091400971215     C* CONTROLLA LE COMBINAZIONI FILIALE PARTENZA/ARRIVO
091500000000IF  2C     ERRL00        IFEQ      '0'
091600971215IF  3C     VIDPAR        IFEQ      '888'
091700000000     C     VIDARR        ANDEQ     '999'
091800000000     C                   MOVEL     MSG(15)       DSMSMS
091900000000     C                   MOVEL     '1'           ERRL00
092000000000     C                   SETON                                        6061
092100971215E   3C                   END
092200000000     C*
092300971215IF  3C     VIDPAR        IFEQ      '999'
092400000000     C     VIDARR        ANDEQ     '888'
092500000000     C                   MOVEL     MSG(15)       DSMSMS
092600000000     C                   MOVEL     '1'           ERRL00
092700971215     C                   SETON                                        6061
092800971215E   3C                   END
092900000000     C*
093000971215IF  3C     VIDPAR        IFEQ      '888'
093100000000     C     VIDARR        ANDEQ     *ZEROS
093200000000     C                   MOVEL     MSG(15)       DSMSMS
093300000000     C                   MOVEL     '1'           ERRL00
093400000000     C                   SETON                                        6061
093500971215E   3C                   END
093600000000     C*
093700971215IF  3C     VIDPAR        IFEQ      '999'
093800000000     C     VIDARR        ANDEQ     *ZEROS
093900000000     C                   MOVEL     MSG(15)       DSMSMS
094000000000     C                   MOVEL     '1'           ERRL00
094100000000     C                   SETON                                        6061
094200971215E   3C                   END
094300000000     C*
094400971215IF  3C     VIDPAR        IFEQ      *ZEROS
094500000000     C     VIDARR        ANDEQ     '888'
094600000000     C                   MOVEL     MSG(15)       DSMSMS
094700000000     C                   MOVEL     '1'           ERRL00
094800000000     C                   SETON                                        6061
094900971215E   3C                   END
095000000000     C*
095100971215IF  3C     VIDPAR        IFEQ      *ZEROS
095200000000     C     VIDARR        ANDEQ     '999'
095300000000     C                   MOVEL     MSG(15)       DSMSMS
095400000000     C                   MOVEL     '1'           ERRL00
095500000000     C                   SETON                                        6061
095600971215E   3C                   END
095700971215E   2C                   END
095800060424E   1C                   END
095900060424E   0C                   END
096000000000     C*
096100000000     C* CONTROLLI PER AUTOMATISMO -C- *****************************************
096200000000IF  1C     VIDTIP        IFEQ      'C'                                          --- C -->
096300000000     C*
096400000000     C* CONTROLLO ANNO (RISPETTO ANNO ATTUALE) (MIN 1 IN MENO,MAX 1 IN PIU')
096500000000     C     ERRL00        IFEQ      '0'
096600991224     C                   MOVEL     DATCOR        WAAINI            4 0
096700991224     C                   MOVEL     DATCOR        WAAFIN            4 0
096800000000     C                   SUB       1             WAAINI
096900000000     C                   ADD       1             WAAFIN
097000000000     C     VIDAAC        IFLT      WAAINI
097100000000     C     VIDAAC        ORGT      WAAFIN
097200000000     C                   MOVEL     MSG(4)        DSMSMS
097300000000     C                   MOVEL     '1'           ERRL00
097400000000     C                   SETON                                        62
097500000000     C                   END
097600000000     C                   END
097700000000E   1C                   END                                                    <-- C ---
097800000000     C*
097900000000     C* CONTROLLI PER AUTOMATISMO -D- *****************************************
098000000000IF  1C     VIDTIP        IFEQ      'D'                                          --- D -->
098100000000     C*
098200000000     C* CONTROLLO DATA
098300000000     C     ERRL00        IFEQ      '0'
098400000000     C                   Z-ADD     VIDDAT        G08DAT
098500000000     C                   Z-ADD     *ZEROS        G08INV
098600000000     C                   MOVEL     '0'           G08ERR
098700000000     C                   CALL      'XSRDA8'
098800000000     C                   PARM                    WLBDA8
098900000000     C                   Z-ADD     G08DAT        VIDDAT
099000000000     C     G08ERR        IFNE      '0'
099100000000     C                   MOVEL     '1'           ERRL00
099200000000     C                   MOVEL     MSG(6)        DSMSMS
099300000000     C                   SETON                                        63
099400000000     C                   END
099500000000     C                   END
099600000000     C*
099700000000     C* CONTROLLO FLAG MATTINO
099800000000     C     ERRL00        IFEQ      '0'
099900000000     C     VIDFMD        IFNE      *BLANKS
100000000000     C                   MOVEL     VIDFMD        WSIM              1
100100000000     C                   EXSR      CHKSIM
100200000000     C     ERRL00        IFEQ      '1'
100300000000     C                   MOVEL     '1'           ERRL00
100400000000     C                   MOVEL     MSG(7)        DSMSMS
100500000000     C                   SETON                                        67
100600000000     C                   ELSE
100700000000     C                   MOVEL     WSIM          VIDFMD
100800000000     C                   END
100900000000     C                   END
101000000000     C                   END
101100000000     C*
101200000000     C* CONTROLLO FLAG POMERIGGIO
101300000000     C     ERRL00        IFEQ      '0'
101400000000     C     VIDFPD        IFNE      *BLANKS
101500000000     C                   MOVEL     VIDFPD        WSIM              1
101600000000     C                   EXSR      CHKSIM
101700000000     C     ERRL00        IFEQ      '1'
101800000000     C                   MOVEL     '1'           ERRL00
101900000000     C                   MOVEL     MSG(7)        DSMSMS
102000000000     C                   SETON                                        68
102100000000     C                   ELSE
102200000000     C                   MOVEL     WSIM          VIDFPD
102300000000     C                   END
102400000000     C                   END
102500000000     C                   END
102600000000     C*
102700000000     C* NON ACCETTO FLAG = BLANK SE GIORNO FESTIVO
102800000000     C     ERRL00        IFEQ      '0'
102900000000     C     VIDFMD        IFEQ      *BLANKS
103000000000     C     VIDFPD        OREQ      *BLANKS
103100000000     C                   Z-ADD     *ZEROS        KCATFP
103200000000     C                   Z-ADD     *ZEROS        KCATFA
103300000000     C                   Z-ADD     VIDAAD        KCAANN
103400000000     C                   Z-ADD     VIDMMD        KCAMES
103500000000     C     KEYCAL        CHAIN     AZCLN01L                           99
103600000000     C     *IN99         IFEQ      '0'
103700000000     C                   MOVEL     CLNPOM        GFEDS
103800000000     C                   Z-ADD     VIDGGD        GG                2 0
103900000000     C     GFE(GG)       IFEQ      'F'
104000000000     C                   MOVEL     '1'           ERRL00
104100000000     C                   MOVEL     MSG(12)       DSMSMS
104200000000     C                   SETON                                        6768
104300000000     C                   END
104400000000     C                   END
104500000000     C                   END
104600000000     C                   END
104700000000     C*
104800000000E   1C                   END                                                    <-- D ---
104900000000     C*
105000000000     C* CONTROLLI PER AUTOMATISMO -G- *****************************************
105100000000IF  1C     VIDTIP        IFEQ      'G'                                          --- G -->
105200971203     C*
105300971203     C* CONTROLLO ANNO
105400971203     C     ERRL00        IFEQ      '0'
105500991224     C                   MOVEL     DATCOR        WAAINI            4 0
105600991224     C                   MOVEL     DATCOR        WAAFIN            4 0
105700971203     C                   SUB       1             WAAINI
105800971203     C                   ADD       1             WAAFIN
105900971203     C     VIDAAG        IFLT      WAAINI
106000971203     C     VIDAAG        ORGT      WAAFIN
106100971203     C                   MOVEL     MSG(4)        DSMSMS
106200971203     C                   MOVEL     '1'           ERRL00
106300971203     C                   SETON                                        65
106400971203     C                   END
106500971203     C                   END
106600000000     C*
106700000000     C* CONTROLLO FLAG MATTINO
106800061117    2C     ERRL00        IFEQ      '0'
106900061117    3C     VIDFMG        IFNE      *BLANKS
107000000000     C                   MOVEL     VIDFMG        WSIM              1
107100000000     C                   EXSR      CHKSIM
107200061117    4C     ERRL00        IFEQ      '1'
107300000000     C                   MOVEL     '1'           ERRL00
107400000000     C                   MOVEL     MSG(7)        DSMSMS
107500000000     C                   SETON                                        64
107600000000     C                   ELSE
107700000000     C                   MOVEL     WSIM          VIDFMG
107800061117    4C                   END
107900061117    3C                   END
108000061117    2C                   END
108100000000     C*
108200000000     C* CONTROLLO FLAG POMERIGGIO
108300061117    2C     ERRL00        IFEQ      '0'
108400061117    3C     VIDFPG        IFNE      *BLANKS
108500000000     C                   MOVEL     VIDFPG        WSIM              1
108600000000     C                   EXSR      CHKSIM
108700061117    4C     ERRL00        IFEQ      '1'
108800000000     C                   MOVEL     '1'           ERRL00
108900000000     C                   MOVEL     MSG(7)        DSMSMS
109000000000     C                   SETON                                        66
109100000000     C                   ELSE
109200000000     C                   MOVEL     WSIM          VIDFPG
109300061117    4C                   END
109400061117    3C                   END
109500061117    2C                   END
109600000000     C*
109700000000     C* CONTROLLO GIORNO DELLA SETTIMANA
109800061117    2C     ERRL00        IFEQ      '0'
109900061117    3C     VIDGIO        IFEQ      '1'
110000000000     C     VIDGIO        OREQ      '2'
110100000000     C     VIDGIO        OREQ      '3'
110200000000     C     VIDGIO        OREQ      '4'
110300000000     C     VIDGIO        OREQ      '5'
110400000000     C     VIDGIO        OREQ      '6'
110500000000     C     VIDGIO        OREQ      '7'
110600000000     C                   ELSE
110700000000     C                   MOVEL     MSG(10)       DSMSMS
110800000000     C                   MOVEL     '1'           ERRL00
110900000000     C                   SETON                                        70
111000061117    3C                   END
111100061117    2C                   END
111200000000     C*
111300000000     C* SE FLAG = BLANKS CONTROLLO CHE NON VI SIA GIORNO FESTIVO
111400061117    2C     ERRL00        IFEQ      '0'
111500061117    3C     VIDFMG        IFEQ      *BLANKS
111600000000     C     VIDFPG        OREQ      *BLANKS
111700061117    4C     VIDGIO        IFEQ      '6'
111800000000     C     VIDGIO        OREQ      '7'
111900000000     C                   MOVEL     MSG(12)       DSMSMS
112000000000     C                   MOVEL     '1'           ERRL00
112100000000     C                   SETON                                        6466
112200061117    4C                   END
112300061117    3C                   END
112400061117    2C                   END
112500000000     C*
112600061117    1C                   END                                                    <-- G ---
112700060424     c
112800061117     C*
112900061117     C* CONTROLLI PER AUTOMATISMO -E- *****************************************
113000061117IF  1C     VIDTIP        IFEQ      'E'                                          --- G -->
113100060424     c* Controllo automatismo per calendario eventi particolari
113200060424     C* CONTROLLO ANNO (RISPETTO ANNO ATTUALE) (MIN 1 IN MENO,MAX 1 IN PIU')
113300061117    2C     ERRL00        IFEQ      '0'
113400060424     C                   MOVEL     DATCOR        WAAINI            4 0
113500060424     C                   MOVEL     DATCOR        WAAFIN            4 0
113600060424     C                   SUB       1             WAAINI
113700060424     C                   ADD       1             WAAFIN
113800061117    3C     VIDcepn       IFLT      WAAINI
113900060424     C     VIDcepn       ORGT      WAAFIN
114000060424     C                   MOVEL     MSG(4)        DSMSMS
114100060424     C                   MOVEL     '1'           ERRL00
114200060424     C                   SETON                                        72
114300061117    3C                   END
114400061117    2C                   END
114500060424     c
114600060424    2c                   if        vidcepn<>savcepn  and errl00='0'
114700060424     c                   clear                   wforza
114800060424     c                   eval      savcepn=vidcepn
114900060424    2c                   endif
115000060424     c
115100060424     c* se già presenti eventi: msg forzabile
115200061117   1aC     ERRL00        IFEQ      '0'
115300060424     c     wforza        andeq     ' '
115400060424     c
115500060424     c                   clear                   wtrova
115600060424     c                   clear                   w0040
115700060424     c                   z-add     20010101      kdev
115800060424     c                   movel     vidcepn       kdev
115900060424     c     kdev          setll     azcep02l
116000060424     c                   read      azcep02l
116100060424     c                   clear                   wtrova            1
116200060424    2c                   dow       not %eof(azcep02l)  and wtrova=' '
116300060424     c                             and w0040<=vidcepn
116400060424     c                   movel     cepdev        w0040             4 0
116500060424    3c                   if        w0040=vidcepn and cepdup='S'
116600060424     c                   eval      wtrova='S'
116700060424     c                   else
116800060424     c                   read      azcep02l
116900060424    3c                   endif
117000060424    2c                   enddo
117100061205    2c                   if        wtrova='S'
117200060424     C                   MOVEL     MSG(18)       DSMSMS
117300060424     C                   MOVEL     '1'           ERRL00
117400060424     C                   SETON                                        72
117500060424     c                   eval      wforza='S'
117600060424    2C                   END
117700061117   1aC                   END
117800060424     c
117900060424     c
118000060424ù    C* CONTROLLO ANNO da cui copiare:
118100060424     c* deve essere <> dall'anno da creare e deve esistere nel
118200060424     c*  calendario
118300060424     C     ERRL00        IFEQ      '0'
118400060424     c                   if        vidcepn=vidcepo or vidcepo=0
118500060424     C                   MOVEL     MSG(16)       DSMSMS
118600060424     C                   MOVEL     '1'           ERRL00
118700060424     C                   SETON                                        73
118800060424     C                   END
118900060424     C                   END
119000060424     C     ERRL00        IFEQ      '0'
119100060424     c                   z-add     20010101      kdev
119200060424     c                   movel     vidcepo       kdev
119300060424     c     kdev          setll     azcep02l
119400060424     c                   read      azcep02l
119500060424     c                   clear                   wtrova            1
119600060424     c                   clear                   w0040
119700060424     c                   dow       not %eof(azcep02l)  and wtrova=' '
119800060424     c                             and w0040<=vidcepo
119900060424     c                   movel     cepdev        w0040             4 0
120000060424     c                   if        w0040=vidcepo and cepdup='S'
120100060424     c                   eval      wtrova='S'
120200060424     c                   else
120300060424     c                   read      azcep02l
120400060424     c                   endif
120500060424     c                   enddo
120600060424     c                   if        wtrova=' '
120700060424     C                   MOVEL     MSG(17)       DSMSMS
120800060424     C                   MOVEL     '1'           ERRL00
120900060424     C                   SETON                                        73
121000060424     C                   END
121100060424     C                   END
121200061117     c
121300061117     C                   END
121400000000E   1C*
121500000000     C                   ENDSR
121600000000     C*------------------------------------------------------------------------*
121700000000     C* CHKLNA - CONTROLLA LA LINEA
121800000000     C*------------------------------------------------------------------------*
121900000000     C     CHKLNA        BEGSR
122000000000     C*
122100000000     C* CONTROLLO SE E' STATO INSERITO '?'
122200000000     C                   MOVE      WLIN          WLNA1             1
122300000000     C                   MOVEL     WLIN          WLNA2             1
122400000000     C*
122500000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
122600000000     C     WLNA1         IFEQ      '?'                                          --- 1 -->
122700000000     C     WLNA2         OREQ      '?'
122800000000     C                   MOVEL     *BLANKS       §COD1             3
122900000000     C                   MOVE      *BLANKS       §DES1
123000971203     C                   MOVE      *BLANKS       §PRA
123100971203     C                   CALL      'TNSD24R'
123200000000     C                   PARM                    §COD1
123300000000     C                   PARM                    §TIP              1
123400000000     C                   PARM                    §DES1            25
123500971203     C                   PARM                    §PRA              3
123600000000     C                   MOVEL     §COD1         WLIN
123700000000     C                   MOVEL     §DES1         WDLIN            20
123800000000     C*
123900000000     C                   ELSE                                                   <-- 1 -->
124000000000     C*
124100000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
124200000000     C                   MOVE      WLIN          KORFIL
124300000000     C     KEYORG        CHAIN     AZORG                              99
124400000000     C     *IN99         IFEQ      '0'                                          --- 2 -->
124500000000     C                   MOVEL     ORGDES        WDLIN
124600000000     C                   ELSE                                                   <-- 2 -->
124700000000     C                   MOVEL     '1'           ERRL00
124800000000     C                   MOVEL     MSG(5)        DSMSMS
124900000000     C                   MOVEL     *ALL'*'       WDLIN
125000000000     C                   END                                                    <-- 2 ---
125100000000     C*
125200000000     C                   END                                                    <-- 1 ---
125300000000     C*
125400000000     C                   ENDSR
125500000000     C*------------------------------------------------------------------------*
125600000000     C* CHKSIM - CONTROLLA SIMBOLI CALENDARIO
125700000000     C*------------------------------------------------------------------------*
125800000000     C     CHKSIM        BEGSR
125900000000     C*
126000000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
126100000000     C     WSIM          IFEQ      '?'                                          --- 1 -->
126200000000     C*
126300000000     C                   MOVEL     CODUT         §KUT              1 0
126400000000     C                   MOVEL     '1Y'          §COD              2
126500000000     C                   MOVE      *BLANKS       §DES1
126600000000     C                   CALL      'X§TABER'
126700000000     C                   PARM                    §KUT
126800000000     C                   PARM                    §COD
126900000000     C                   PARM                    §KEY              8
127000000000     C                   PARM                    §DES             30
127100000000     C                   MOVEL     §KEY          WSIM
127200000000     C                   MOVEL     §DES          DS1Y                           *NELLA DS
127300000000     C*
127400000000     C                   ELSE                                                   <-- 1 -->
127500000000     C*
127600000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
127700000000     C                   MOVEL     CODUT         KTBUTE
127800000000     C                   MOVE      '1Y'          KTBCOD
127900000000     C                   MOVE      *BLANKS       KTBKEY
128000000000     C                   MOVEL     WSIM          KTBKEY
128100000000     C     KEYTAB        CHAIN     TABEL                              99
128200000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
128300000000     C     TBLFLG        OREQ      '*'
128400000000     C                   MOVEL     '1'           ERRL00
128500000000     C                   MOVEL     MSG(7)        DSMSMS
128600000000     C                   ELSE
128700000000     C                   MOVEL     TBLUNI        DS1Y                           *NELLA DS
128800000000     C                   END                                                    <-- 2 ---
128900000000     C*
129000000000     C                   END                                                    <-- 1 ---
129100000000     C*
129200000000     C                   ENDSR
129300000000     C*------------------------------------------------------------------------*
129400000000     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
129500000000     C*------------------------------------------------------------------------*
129600000000     C     AZZVID        BEGSR
129700000000     C*
129800000000     C                   MOVE      *ZEROS        VIDPAR
129900000000     C                   MOVE      *ZEROS        VIDARR
130000000000     C                   MOVEL     *BLANKS       VIDTIP
130100000000     C                   Z-ADD     *ZEROS        VIDAAC
130200000000     C                   Z-ADD     *ZEROS        VIDDAT
130300000000     C                   MOVEL     *BLANKS       VIDFMD
130400000000     C                   MOVEL     *BLANKS       VIDFPD
130500000000     C                   MOVEL     *BLANKS       VIDFMG
130600000000     C                   MOVEL     *BLANKS       VIDFPG
130700000000     C                   MOVEL     *BLANKS       VIDGIO
130800000000     C                   Z-ADD     *ZEROS        VIDAAG
130900060424     c                   clear                   vidcepn
131000060424     c                   clear                   vidcepo
131100060424     c                   clear                   savcepn
131200060424     c                   clear                   w0040
131300060424     c                   clear                   wtrova
131400000000     C*
131500000000     C                   ENDSR
131600000000     C*------------------------------------------------------------------------*
131700000000     C* *INZSR - OPERAZIONI INIZIALI
131800000000     C*------------------------------------------------------------------------*
131900000000     C     *INZSR        BEGSR
132000000000     C*
132100000000     C* RICEVIMENTO PARAMETRI
132200000000     C     *ENTRY        PLIST
132300000000     C                   PARM                    DSCF03            1            PREMUTO F03
132400000000     C                   PARM                    DSCERR            1            '1'=ERRORE
132500000000     C                   PARM                    DSCMSG           80            MESSAGGIO
132600000000     C*
132700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
132800000000     C     XMSGPA        PLIST
132900000000     C                   PARM                    DSMSMS
133000000000     C                   PARM                    DSMSPG
133100000000     C                   PARM                    DSMSKE
133200000000     C                   PARM                    DSMSER
133300971202     C*
133400971202     C* VARIABILI RIFERITE AL DATA BASE
133500971202     C     *LIKE         DEFINE    FUDFUN        FUNKEY
133600971202     C     *LIKE         DEFINE    ORGFIL        KORFIL
133700971202     C     *LIKE         DEFINE    CLNTFP        KCATFP
133800971202     C     *LIKE         DEFINE    CLNTFA        KCATFA
133900971202     C     *LIKE         DEFINE    CLNANN        KCAANN
134000971202     C     *LIKE         DEFINE    CLNMES        KCAMES
134100971202     C     *LIKE         DEFINE    TBLKUT        KTBUTE
134200971202     C     *LIKE         DEFINE    TBLCOD        KTBCOD
134300971202     C     *LIKE         DEFINE    TBLKEY        KTBKEY
134400971202     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         AZCAE00F
134500000000     C*
134600000000     C* CHIAVE LETTURA AZCLN01L - COMPLETA
134700000000     C     KEYCAL        KLIST
134800000000     C                   KFLD                    KCATFP
134900000000     C                   KFLD                    KCATFA
135000000000     C                   KFLD                    KCAANN
135100000000     C                   KFLD                    KCAMES
135200000000     C*
135300000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
135400000000     C     KEYORG        KLIST
135500000000     C                   KFLD                    KORFIL
135600000000     C*
135700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
135800000000     C     KEYTAB        KLIST
135900000000     C                   KFLD                    KTBUTE
136000000000     C                   KFLD                    KTBCOD
136100000000     C                   KFLD                    KTBKEY
136200000000     C*
136300991224     C* CHIAVE LETTURA ANBCA01L - COMPLETA
136400000000     C     KEYBCA        KLIST
136500991224     C                   KFLD                    KCACALEND
136600991224     C                   KFLD                    KCAANNO
136700991224     C                   KFLD                    KCAMESE
136800971202     C*
136900971202     C* VARIABILI DI CONTROLLO
137000971202     C                   Z-ADD     *ZEROS        WUTFP             6 0          *ULTIMO TERM PART
137100971202     C                   Z-ADD     *ZEROS        WUTFA             6 0          *ULTIMO TERM ARR
137200971202     C                   Z-ADD     *ZEROS        WUFIL             6 0          *ULTIMA FILAILE
137300971202     C                   Z-ADD     *ZEROS        WUTP              6 0          *ULTIMA PARTENZA
137400971202     C                   Z-ADD     *ZEROS        WUTA              6 0          *ULTIMO ARRIVO
137500000000     C*
137600000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
137700000000     C                   CALL      'XKEY'
137800000000     C                   PARM                    DSKEY
137900000000     C*
138000000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
138100000000     C*   -NOME PGM CHIAMANTE
138200000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
138300000000     C                   MOVEL     'TNSD77R'     DSMSPG
138400000000     C                   MOVEL     'TNSD77R'     MSGQUE
138500000000     C                   MOVEL     *BLANKS       MSGKEY
138600000000     C                   MOVEL     *BLANKS       DSMSMS
138700000000     C*
138800000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
138900000000     C                   Z-ADD     0             FUNGRU            5 0
139000000000     C*
139100000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
139200000000     C                   EXSR      FUNGES
139300000000     C*
139400000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
139500000000     C                   MOVEL     '0'           F12L00            1
139600000000     C                   MOVEL     '0'           F03L00            1
139700000000     C                   MOVEL     '0'           FINL00            1
139800000000     C                   MOVEL     '0'           ERRL00            1
139900000000     C                   MOVEL     '0'           FLFINE            1
140000020906     C*
140100020906     C                   Z-ADD     1             CODUT             1 0
140200000000     C*
140300000000     C* RECUPERO DATA ODIERNA
140400991224     C                   TIME                    n14                            *Ora (6)+ Data (8)
140500991224     C                   MOVE      n14           n8                             *Data (8) in g/m/a
140600991224     C                   Z-ADD     n8            g08dat
140700991224     C                   Z-ADD     *zeros        g08inv
140800991224     C                   MOVEL     '0'           g08err
140900991224     C                   CALL      'XSRDA8'
141000991224     C                   PARM                    wlbda8
141100991224     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
141200000000     C*
141300000000     C* REPERISCE LA DATA CORRENTE
141400000000     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
141500000000     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
141600000000     C                   Z-ADD     N8            G08DAT
141700000000     C                   Z-ADD     *ZEROS        G08INV
141800000000     C                   MOVEL     '0'           G08ERR
141900000000     C                   CALL      'XSRDA8'
142000000000     C                   PARM                    WLBDA8
142100000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
142200000000     C*
142300000000     C* AZZERO CAMPI A VIDEO
142400000000     C                   EXSR      AZZVID
142500000000     C*
142600000000     C                   ENDSR
142700000000     C*------------------------------------------------------------------------*
142800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
142900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
143000000000     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
143100000000     C*     STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
143200000000     C*     NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
143300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
143400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
143500000000     C*------------------------------------------------------------------------*
143600000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
143700000000     C*------------------------------------------------------------------------*
143800000000     C     FUNGES        BEGSR
143900000000     C*
144000000000     C* PULISCO CAMPI DI LAVORO
144100000000     C                   Z-ADD     0             W                 5 0
144200000000     C*
144300000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
144400000000     C* E CARICO TABELLA FUNZIONI ATTIVE
144500000000     C     1             DO        24            I
144600971202     C                   MOVEL     FUD(I)        FUDRIG
144700000000     C*
144800000000     C* PULIZIA FUNZIONI PRECEDENTI
144900000000     C                   MOVEL     *BLANKS       FUV(I)
145000000000     C*
145100000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
145200000000     C* (SALTA F24)
145300000000     C     FUDATT        IFEQ      'S'
145400000000     C     FUDFUN        ANDNE     '24'
145500000000     C                   ADD       1             W
145600000000     C                   MOVEL     FUDDES        FUV(W)
145700000000     C                   END
145800000000     C                   END                                                    DO WHILE
145900000000     C*
146000000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
146100000000     C* ALTRIMENTI DISATTIVO F24
146200000000     C     1             DO        24            I
146300000000     C                   MOVEL     FUD(I)        FUDRIG
146400000000     C*
146500000000     C     FUDFUN        IFEQ      '24'
146600000000     C     W             IFGT      6
146700000000     C                   MOVEL     'S'           FUDATT
146800000000     C                   ELSE
146900000000     C                   MOVEL     *BLANKS       FUDATT
147000000000     C                   END                                                    W GT 8
147100000000     C                   MOVEL     FUDRIG        FUD(I)
147200000000     C                   END
147300000000     C                   END                                                    DO WHILE
147400000000     C*
147500000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
147600000000     C     W             DIV       6             I
147700000000     C                   MVR                     Z
147800000000     C*
147900000000     C     Z             IFNE      0
148000000000     C     I             ADD       1             Z
148100000000     C                   ELSE
148200000000     C                   Z-ADD     I             Z                 5 0
148300000000     C                   END
148400000000     C*
148500000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
148600000000     C                   ADD       1             FUNGRU
148700000000     C     FUNGRU        IFGT      Z
148800000000     C     FUNGRU        ORLT      1
148900000000     C                   Z-ADD     1             FUNGRU
149000000000     C                   END
149100000000     C*
149200000000     C* PRIMO GRUPPO DI 6 FUNZIONI
149300000000     C     FUNGRU        IFEQ      1
149400000000     C                   MOVEL     FUV(1)        FUN001
149500000000     C                   MOVEL     FUV(2)        FUN002
149600000000     C                   MOVEL     FUV(3)        FUN003
149700000000     C                   MOVEL     FUV(4)        FUN004
149800000000     C                   MOVEL     FUV(5)        FUN005
149900000000     C                   MOVEL     FUV(6)        FUN006
150000000000     C                   END
150100000000     C*
150200000000     C* SECONDO GRUPPO DI 6 FUNZIONI
150300000000     C     FUNGRU        IFEQ      2
150400000000     C                   MOVEL     FUV(7)        FUN001
150500000000     C                   MOVEL     FUV(8)        FUN002
150600000000     C                   MOVEL     FUV(9)        FUN003
150700000000     C                   MOVEL     FUV(10)       FUN004
150800000000     C                   MOVEL     FUV(11)       FUN005
150900000000     C                   MOVEL     FUV(12)       FUN006
151000000000     C                   END
151100000000     C*
151200000000     C* TERZO GRUPPO DI 6 FUNZIONI
151300000000     C     FUNGRU        IFEQ      3
151400000000     C                   MOVEL     FUV(13)       FUN001
151500000000     C                   MOVEL     FUV(14)       FUN002
151600000000     C                   MOVEL     FUV(15)       FUN003
151700000000     C                   MOVEL     FUV(16)       FUN004
151800000000     C                   MOVEL     FUV(17)       FUN005
151900000000     C                   MOVEL     FUV(18)       FUN006
152000000000     C                   END
152100000000     C*
152200000000     C* QUARTO GRUPPO DI 6 FUNZIONI
152300000000     C     FUNGRU        IFEQ      4
152400000000     C                   MOVEL     FUV(19)       FUN001
152500000000     C                   MOVEL     FUV(20)       FUN002
152600000000     C                   MOVEL     FUV(21)       FUN003
152700000000     C                   MOVEL     FUV(22)       FUN004
152800000000     C                   MOVEL     *BLANKS       FUN005
152900000000     C                   MOVEL     *BLANKS       FUN006
153000000000     C                   END
153100000000     C*
153200000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
153300000000     C                   MOVEL     *BLANKS       FUN007
153400000000     C                   MOVEL     *BLANKS       FUN008
153500000000     C*
153600000000     C     1             DO        24            I
153700000000     C                   MOVEL     FUD(I)        FUDRIG
153800000000     C     FUDFUN        IFEQ      '24'
153900000000     C     FUDATT        ANDEQ     'S'
154000000000     C                   MOVEL     FUDDES        FUN008
154100000000     C                   END
154200000000     C                   END                                                    DO
154300000000     C                   ENDSR
154400000000     C*------------------------------------------------------------------------*
154500000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
154600000000     C*------------------------------------------------------------------------*
154700000000     C     FUNCON        BEGSR
154800000000     C*
154900000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
155000000000     C                   EXSR      FUNVAR
155100000000     C*
155200000000     C* PULISCO CAMPI DI LAVORO
155300000000     C                   Z-ADD     0             I                 5 0
155400000000     C                   Z-ADD     0             W                 5 0
155500000000     C*
155600000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
155700000000     C     FUNKEY        IFNE      *BLANKS
155800000000     C     1             DO        24            I
155900000000     C                   MOVEL     FUD(I)        FUDRIG
156000000000     C*
156100000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
156200000000     C     FUNKEY        IFEQ      FUDFUN
156300000000     C     FUDATT        ANDEQ     'S'
156400000000     C                   Z-ADD     I             W
156500000000     C                   END
156600000000     C                   END                                                    DO WHILE
156700000000     C*
156800000000     C* FUNZIONE NON ATTIVA (ERRORE)
156900000000     C     W             IFEQ      0
157000000000     C*
157100000000     C* EMISSIONE MESSAGGIO DI ERRORE
157200000000     C                   MOVEL     MSG(1)        DSMSMS
157300000000     C*
157400000000     C                   END                                                    W=0
157500000000     C                   END                                                    FUNKEY NE *BLANKS
157600000000     C*
157700000000     C* FUNZIONE ATTIVA (PROCEDI ...)
157800000000     C     W             IFNE      0
157900000000     C     FUNKEY        OREQ      *BLANKS
158000000000     C*
158100000000     C     FUNKEY        IFEQ      *BLANKS
158200000000     C                   EXSR      ENTGES
158300000000     C                   END
158400000000     C     FUNKEY        IFEQ      '01'
158500000000     C                   EXSR      F01GES
158600000000     C                   END
158700000000     C     FUNKEY        IFEQ      '03'
158800000000     C                   EXSR      F03GES
158900000000     C                   END
159000000000     C     FUNKEY        IFEQ      '06'
159100000000     C                   EXSR      F06GES
159200000000     C                   END
159300000000     C     FUNKEY        IFEQ      '07'
159400000000     C                   EXSR      F07GES
159500000000     C                   END
159600000000     C     FUNKEY        IFEQ      '08'
159700000000     C                   EXSR      F08GES
159800000000     C                   END
159900000000     C     FUNKEY        IFEQ      '12'
160000000000     C                   EXSR      F12GES
160100000000     C                   END
160200000000     C     FUNKEY        IFEQ      '21'
160300000000     C                   EXSR      F21GES
160400000000     C                   END
160500000000     C     FUNKEY        IFEQ      '22'
160600000000     C                   EXSR      F22GES
160700000000     C                   END
160800000000     C     FUNKEY        IFEQ      '24'
160900000000     C                   EXSR      F24GES
161000000000     C                   END
161100000000     C     FUNKEY        IFEQ      'HP'
161200000000     C                   EXSR      HELGES
161300000000     C                   END
161400000000     C*
161500000000     C                   END
161600000000     C*
161700000000     C                   ENDSR
161800000000     C*------------------------------------------------------------------------*
161900000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
162000000000     C*------------------------------------------------------------------------*
162100000000     C     FUNVAR        BEGSR
162200000000     C*
162300000000     C* INIZIALIZZA VARIABILE -FUNKEY-
162400000000     C*
162500000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
162600000000     C     KEY           IFEQ      ENTER
162700000000     C                   MOVEL     *BLANKS       FUNKEY
162800000000     C                   END
162900000000     C     KEY           IFEQ      F1
163000000000     C                   MOVEL     '01'          FUNKEY
163100000000     C                   END
163200000000     C     KEY           IFEQ      F2
163300000000     C                   MOVEL     '02'          FUNKEY
163400000000     C                   END
163500000000     C     KEY           IFEQ      F3
163600000000     C                   MOVEL     '03'          FUNKEY
163700000000     C                   END
163800000000     C     KEY           IFEQ      F4
163900000000     C                   MOVEL     '04'          FUNKEY
164000000000     C                   END
164100000000     C     KEY           IFEQ      F5
164200000000     C                   MOVEL     '05'          FUNKEY
164300000000     C                   END
164400000000     C     KEY           IFEQ      F6
164500000000     C                   MOVEL     '06'          FUNKEY
164600000000     C                   END
164700000000     C     KEY           IFEQ      F7
164800000000     C                   MOVEL     '07'          FUNKEY
164900000000     C                   END
165000000000     C     KEY           IFEQ      F8
165100000000     C                   MOVEL     '08'          FUNKEY
165200000000     C                   END
165300000000     C     KEY           IFEQ      F9
165400000000     C                   MOVEL     '09'          FUNKEY
165500000000     C                   END
165600000000     C     KEY           IFEQ      F10
165700000000     C                   MOVEL     '10'          FUNKEY
165800000000     C                   END
165900000000     C     KEY           IFEQ      F11
166000000000     C                   MOVEL     '11'          FUNKEY
166100000000     C                   END
166200000000     C     KEY           IFEQ      F12
166300000000     C                   MOVEL     '12'          FUNKEY
166400000000     C                   END
166500000000     C     KEY           IFEQ      F13
166600000000     C                   MOVEL     '13'          FUNKEY
166700000000     C                   END
166800000000     C     KEY           IFEQ      F14
166900000000     C                   MOVEL     '14'          FUNKEY
167000000000     C                   END
167100000000     C     KEY           IFEQ      F15
167200000000     C                   MOVEL     '15'          FUNKEY
167300000000     C                   END
167400000000     C     KEY           IFEQ      F16
167500000000     C                   MOVEL     '16'          FUNKEY
167600000000     C                   END
167700000000     C     KEY           IFEQ      F17
167800000000     C                   MOVEL     '17'          FUNKEY
167900000000     C                   END
168000000000     C     KEY           IFEQ      F18
168100000000     C                   MOVEL     '18'          FUNKEY
168200000000     C                   END
168300000000     C     KEY           IFEQ      F19
168400000000     C                   MOVEL     '19'          FUNKEY
168500000000     C                   END
168600000000     C     KEY           IFEQ      F20
168700000000     C                   MOVEL     '20'          FUNKEY
168800000000     C                   END
168900000000     C     KEY           IFEQ      F21
169000000000     C                   MOVEL     '21'          FUNKEY
169100000000     C                   END
169200000000     C     KEY           IFEQ      F22
169300000000     C                   MOVEL     '22'          FUNKEY
169400000000     C                   END
169500000000     C     KEY           IFEQ      F23
169600000000     C                   MOVEL     '23'          FUNKEY
169700000000     C                   END
169800000000     C     KEY           IFEQ      F24
169900000000     C                   MOVEL     '24'          FUNKEY
170000000000     C                   END
170100000000     C     KEY           IFEQ      ROLLUP
170200000000     C                   MOVEL     'RU'          FUNKEY
170300000000     C                   END
170400000000     C     KEY           IFEQ      HELP
170500000000     C                   MOVEL     'HP'          FUNKEY
170600000000     C                   END
170700000000     C                   ENDSR
170800000000     C*------------------------------------------------------------------------*
170900000000     C* NOTE PER COMPILAZIONE SCHIERE
171000000000     C*------------------------------------------------------------------------*
171100000000     C* -MSG-
171200000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
171300000000     C* (NON TOCCARE MSG,1)
171400000000     C*
171500000000     C* -CMD-
171600000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
171700000000     C*
171800000000     C* -FUD-
171900000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
172000000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
172100000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
172200000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
172300000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
172400000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
172500000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
172600000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
172700000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
172800000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
172900000000     C*
173000000000     C*------------------------------------------------------------------------*
173100000000**   MSG  - MESSAGGI
173200000000TNSD77R- Tasto funzionale non gestito                                          1
173300000000TNSD77R- Calendario già creato.                                                2
173400000000TNSD77R- Automatismo applicato.                                                3
173500000000TNSD77R- Anno non accettabile.                                                 4
173600000000TNSD77R- Terminal/Linea non accettabile.                                       5
173700000000TNSD77R- Data non accettabile.                                                 6
173800000000TNSD77R- Flag non accettabile.                                                 7
173900000000TNSD77R- Non trovato calendario banche. Avvertire CED.                         8
174000971202TNSD77R- Tipo automatismo arrato                                               9
174100971202TNSD77R- Giorno della settimana errato                                        10
174200971202TNSD77R- Combinazione terminal par/arr non presente in archivio trazioni      11
174300971202TNSD77R- Flag uguale a spazio non accettabile in un giorno festivo            12
174400971202TNSD77R- Tipo terminal/filiale non accettabile                                13
174500971202TNSD77R- Tipo terminal/filiale alternativo a term/filiale partenza e arrivo   14
174600971202TNSD77R- Combinazione filiale part/arr non accettabile                        15
174700060424TNSD77R- Anno da cui copiare deve essere diverso dall'anno da creare e > 0000 16
174800060424TNSD77R- Non sono stati trovati eventi duplicabili per l'anno indicato        17
174900060424Già trovati eventi per l'anno indicato:saranno duplicati eventuali mancanti
175000060424Per automatismo su calendario eventi partic, non ammessi terminal e/o p.o.
175100000000TNSD77R-                                                                      20
175200000000**   CMD - COMANDI
175300000000WRKSPLF
175400000000
175500000000
175600000000
175700000000
175800000000**   FUD - TASTI FUNZIONALI
175900000000                   -        (S/N)=N                        funz.01¬
176000000000                   -        (S/N)=N                        funz.02¬
176100000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
176200000000                   -        (S/N)=N                        funz.04¬
176300000000                   -        (S/N)=N                        funz.05¬
176400000000F06=Aggiornamento  -        (S/N)=S                        funz.06¬
176500000000                   -        (S/N)=N                        funz.07¬
176600000000                   -        (S/N)=N                        funz.08¬
176700000000                   -        (S/N)=N                        funz.09¬
176800000000                   -        (S/N)=N                        funz.10¬
176900000000                   -        (S/N)=N                        funz.11¬
177000000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
177100000000                   -        (S/N)=N                        funz.13¬
177200000000                   -        (S/N)=N                        funz.14¬
177300000000                   -        (S/N)=N                        funz.15¬
177400000000                   -        (S/N)=N                        funz.16¬
177500000000                   -        (S/N)=N                        funz.17¬
177600000000                   -        (S/N)=N                        funz.18¬
177700000000                   -        (S/N)=N                        funz.19¬
177800000000                   -        (S/N)=N                        funz.20¬
177900000000                   -        (S/N)=N                        funz.21¬
178000000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
178100000000                   -        (S/N)=N                        funz.23¬
178200000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
