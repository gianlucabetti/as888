000100970526     H*------------------------------------------------------------------------*
000200061221     H* CAPPARIO - GESTIONE ECCEZIONI LINEE/ZONE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970527     FTISI12D   CF   E             WORKSTN INFDS(DEVDS1)
000900970527     F                                     SFILE(SI12DMS:MSGULT)
001000970527     FTABEL00F  IF   E           K DISK
001100970527     FAZORG01L  IF   E           K DISK
001200970527     F*------------
001300970527     F* ECCEZIONI LINEE/ZONE
001400970527     F*------------
001500970527     FAZCPE01L  UF A E           K DISK
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------
002000970526     D* DECODIFICHE
002100970526     D*------------
002200970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* MESSAGGI
002500970526     D*------------
002600970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* DESCRIZIONI TASTI FUNZIONALI
002900970526     D*------------
003000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* FUNZIONI DA CARICARE A VIDEO
003300970526     D*------------
003400970526     D FUV             S             19    DIM(24)
003500970526     D*------------------------------------------------------------------------*
003600970526     D* INPUT
003700970526     D*------------------------------------------------------------------------*
003800970526     D*------------
003900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004000970526     D*------------
004100970526     D FUDDS           DS
004200970526     D  FUDDE1                 1      1
004300970526     D  FUDFUN                 2      3
004400970526     D  FUDDE2                 4     19
004500970526     D  FUDFI0                20     34
004600970526     D  FUDATT                35     35
004700970526     D  FUDFI1                36     80
004800970526     D  FUDDES                 1     19
004900970526     D  FUDRIG                 1     80
005000970526     D*------------
005100970526     D* DS GESTIONE TASTI FUNZIONALI
005200970526     D*------------
005300970526     D DSKEY         E DS
005400970526     D DEVDS1          DS
005500970526     D  KEY                  369    369
005600970526     D*------------
005700970526     D* DS "XMSG" - GESTIONE MESSAGGI
005800970526     D*------------
005900970526     D DSMSG         E DS
006000970626     D*------------------
006100970626     D* DS "TISI95R" - CONTROLLO CAP
006200970626     D*------------------
006300970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
006400970527     D*------------------
006500970527     D* DS NAZIONI
006600970527     D*------------------
006700970527     D DS15          E DS
006800970527     D*------------------
006900970527     D* DS ZONE DI CONSEGNA
007000970527     D*------------------
007100970527     D DS03          E DS
007200000613     D*------------
007300000613     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
007400000613     D*------------
007500000613     D OG143         E DS
007600970527     D*------------
007700970527     D* DS "TISI81R" - AGGIORNA UN CAP DAI SUOI ARCHIVI
007800970527     D*------------
007900970527     D DSSI81        E DS                  EXTNAME(TISI81DS)
008000971128     D*------------
008100971128     D* DS REPEIMENTO TERMINALs
008200971128     D*------------
008300971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
008400970526     D*------------
008500970526     D* DS DI PROCEDURA
008600970526     D*------------
008700970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
008800130905     d
008900130905     d tisio0ds      E DS
009000971128     D*------------------
009100971128     D* DS "XSRDA8" - CONTROLLA DATA (8)
009200971128     D*------------------
009300971128     D WLBDA8          DS                  INZ
009400971128     D  G08DAT                 1      8  0
009500971128     D  G08INV                 9     16  0
009600971128     D  G08ERR                17     17
009700971128     D  G08TGI                18     22  0
009800970526     D*------------
009900970526     D* ARCHITETTURA
010000970526     D*------------
010100970526     D KPJBA         E DS
010200060206      * - Parametri x Controllo profilo utenti
010300060206     d TIBS34ds      e ds
010400060206      * - Ds di riferimento al file esterno AZUTE00F
010500060206     d AZUTEds       e ds                  extname(AZUTE00F)
010600060206     d dUTE01        e ds
010700060206      * - Ds per dati organigramma
010800060206     d DDatiUte      e ds
010900970526     C*------------------------------------------------------------------------*
011000970526     C* MAIN LINES
011100970526     C*------------------------------------------------------------------------*
011200970526     C*
011300970526     C* NON CI SONO ERRORI IN ENTRATA
011400970526IF  1C     ERRL00        IFEQ      '0'
011500970526     C*
011600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011700970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012000970526     C*
012100970526     C* EMETTE FORMATO VIDEO
012200970526     C                   EXSR      BUTFMR
012300970526     C*
012400970526     C* GESTIONE TASTI FUNZIONALI
012500970526     C                   EXSR      FUNCON
012600970526E   2C                   ENDDO
012700970526     C*
012800970526     C* CI SONO ERRORI IN ENTRATA
012900970527X   1C                   ELSE
013000970526     C                   MOVEL     '1'           D00ERR
013100970526     C                   MOVEL     DSMSMS        D00MSG
013200970526E   1C                   ENDIF
013300970626     C*
013400970626     C* OPERAZIONI FINALI
013500970626     C                   EXSR      FINSR
013600970526     C* FINE PGM
013700970526     C                   SETON                                        LR
013800970526     C*------------------------------------------------------------------------*
013900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014000970526     C*------------------------------------------------------------------------*
014100970526     C     BUTFMR        BEGSR
014200970526     C*
014300970526     C* EMETTE LA TESTATA
014400970527     C                   WRITE     SI12D01
014500970526     C*
014600970526     C* SPEDISCE EVENTUALE MESSAGGIO
014700970526     C                   EXSR      SNDMSG
014800970526     C*
014900970526     C* EMETTE LE FUNZIONI
015000970527     C                   WRITE     SI12D02
015100970526     C*
015200970526     C* EMETTE IL VIDEO
015300970527     C                   EXFMT     SI12D03
015400970526     C*
015500970526     C* REIMPOSTA FLAG DI ERRORE
015600970526     C                   MOVEL     '0'           ERRL00
015700970526     C*
015800970526     C* REIMPOSTA INDICATORI DI ERRORE
015900970526     C                   SETOFF                                         5859
016000970526     C                   SETOFF                                       606162
016100970526     C                   SETOFF                                       636465
016200970526     C                   SETOFF                                       666768
016300970526     C                   SETOFF                                       697071
016400970526     C                   SETOFF                                       727374
016500970526     C                   SETOFF                                       757677
016600970526     C                   SETOFF                                       787980
016700970526     C                   SETOFF                                       818283
016800970526     C                   SETOFF                                       848586
016900970526     C                   SETOFF                                       878889
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     SNDMSG        BEGSR
017600970526     C*
017700970526     C* PULISCE IL SFILE MESSAGGI
017800970526     C                   MOVEL     '1'           *IN40
017900970527     C                   WRITE     SI12DMC
018000970526     C                   MOVEL     '0'           *IN40
018100970526     C*
018200970526     C* VISUALIZZA IL MESSAGGIO
018300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018400970526     C                   CALL      'XMSG'        XMSGPA
018500970526IF  1C     DSMSER        IFEQ      *ZEROS
018600970526     C                   MOVEL     DSMSKE        MSGKEY
018700970526E   1C                   ENDIF
018800970526     C                   Z-ADD     1             MSGULT
018900970527     C                   WRITE     SI12DMS
019000970527     C                   WRITE     SI12DMC
019100970526     C*
019200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019300970526     C                   MOVEL     *BLANKS       DSMSMS
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* ENTGES - GESTIONE ENTER -> INVIO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     ENTGES        BEGSR
020000970526     C*
020100970526     C* CONTROLLA I CAMPI IMMESSI
020200970526     C                   EXSR      DECODI
020300970526     C*
020400970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
020500970526IF  1C     DEPOP1        IFEQ      'O05'
020600970526     C                   MOVEL     '1'           FINL00
020700970526E   1C                   ENDIF
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F03GES - GESTIONE F03 -> FINE
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F03GES        BEGSR
021400970526     C*
021500970526     C* IMPOSTA "PREMUTO F03"
021600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021700970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
022200970526     C*------------------------------------------------------------------------*
022300970526     C     F06GES        BEGSR
022400970526     C*
022500970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
022600970526IF  1C     DEPOP1        IFEQ      'F06'
022700970526     C                   EXSR      INSERI
022800970526E   1C                   ENDIF
022900970526IF  1C     DEPOP1        IFEQ      'O02'
023000970526     C                   EXSR      MODIFI
023100970526E   1C                   ENDIF
023200970526IF  1C     DEPOP1        IFEQ      'O04'
023300970526     C                   EXSR      ANNULL
023400970526E   1C                   ENDIF
023500970526IF  1C     DEPOP1        IFEQ      'O07'
023600970526     C                   EXSR      RIPRIS
023700970526E   1C                   ENDIF
023800970526     C*
023900970526     C                   ENDSR
024000130905     C*------------------------------------------------------------------------*
024100130905     C* F07GES - GESTIONE F07 -> Orari servizi
024200130905     C*------------------------------------------------------------------------*
024300130905     C     F07GES        BEGSR
024400130905     C*
024500130905     c                   exsr      F06ges
024600130905
024700130905     c                   if        errl00='0'
024800130905
024900130905     c                   clear                   tisio0ds
025000130905     C                   MOVEL     VIDNAR        do0NAR
025100130905     C                   MOVEL     VIDCAP        Do0CAP
025200130905     c                   if        %subst(depop0:2:2)='02'
025300130905     C                   MOVEL     'G'           do0tric
025400130905     c                   else
025500130905     C                   MOVEL     'I'           do0tric
025600130905     c                   endif
025700130905     c                   movel     d00dri        do0dri
025800130905     c                   movel     d00ver        do0ver
025900130905
026000130905     C                   call      'TISIO2R'
026100130905     C                   PARM                    KPJBA
026200130905     C                   PARM                    tisio0ds
026300130905     C*
026400130905     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
026500130905IF  1C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
026600130905     C                   MOVEL     '1'           ERRL00                         *ERRORE
026700130905     C                   MOVEL     '1'           F03L00                         *FINE PGM
026800130905X   1C                   ELSE                                                   *NON PREMUTO F03
026900130905     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
027000130905E   1C                   ENDIF
027100130905     C*
027200130905E   1c                   ENDIF
027300130905     C*
027400130905     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600970526     C* F12GES - GESTIONE F12 -> RITORNO
027700970526     C*------------------------------------------------------------------------*
027800970526     C     F12GES        BEGSR
027900970526     C*
028000970526     C* IMPOSTA "PREMUTO F12"
028100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
028200970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
028300970526     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
028700970526     C*------------------------------------------------------------------------*
028800970526     C     F24GES        BEGSR
028900970526     C*
029000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
029100970526     C*
029200970526     C                   ENDSR
029300970526     C*------------------------------------------------------------------------*
029400970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
029500970526     C*------------------------------------------------------------------------*
029600970526     C     INSERI        BEGSR
029700970526     C*
029800970526     C* CONTROLLA I CAMPI IMMESSI
029900970526     C                   EXSR      DECODI
030000970526     C*
030100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030300970527     C                   Z-ADD     D00VER        CPEVER                         *CHIAVE
030400970527     C                   MOVEL     VIDNAR        CPENAR
030500970527     C                   MOVEL     VIDCAP        CPECAP
030600970527     C                   MOVEL     VIDTFP        CPETFP
030700970527     C                   EXSR      IMPCPE                                       *ALTRI CAMPI
030800970527     C                   Z-ADD     *ZEROS        CPEDTR                         *DATA TRASMISSIONE
030900970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
031000970527     C                   MOVEL     *BLANKS       CPEATB                         *ANNULLAMENTO
031100970527     C                   WRITE     AZCPE000
031200970527     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
031300970527     C*
031400970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
031500970527     C                   Z-ADD     D00VER        D81VER
031600970527     C                   MOVEL     VIDNAR        D81NAR
031700970527     C                   MOVEL     VIDCAP        D81CAP
031800970527     C                   MOVEL     'E'           D81CHK
031900970527     C                   CALL      'TISI81R'
032000970527     C                   PARM                    DSSI81
032100970602     C*
032200970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
032300970602     C                   EXSR      AZZVID
032400970526E   1C                   ENDIF
032500970526     C*
032600970526     C                   ENDSR
032700970526     C*------------------------------------------------------------------------*
032800970526     C* MODIFI - OPERAZIONI DI MODIFICA
032900970526     C*------------------------------------------------------------------------*
033000970526     C     MODIFI        BEGSR
033100970526     C*
033200970526     C* CONTROLLA I CAMPI IMMESSI
033300970526     C                   EXSR      DECODI
033400970526     C*
033500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
033600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033700970527     C                   EXSR      LETCPE
033800970526     C*
033900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
034000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034100970527     C                   EXSR      IMPCPE                                       *IMPOSTA I CAMPI
034200970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
034300970527     C                   MOVEL     *BLANKS       CPEATB                         *ANNULLAMENTO
034400970527     C                   UPDATE    AZCPE000
034500970526     C                   MOVEL     MSG(3)        D00MSG
034600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
034700000601     C*
034800970526E   2C                   ENDIF
034900970526E   1C                   ENDIF
035000970526     C*
035100970526     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
035400970526     C*------------------------------------------------------------------------*
035500970526     C     ANNULL        BEGSR
035600970526     C*
035700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
035800000602     C                   MOVEL     '0'           ANNDLT            1
035900970527     C                   EXSR      LETCPE
036000970527     C*
036100970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
036200970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036300970527IF  2C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
036400970527IF  3C     CPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
036500970527     C                   MOVEL     'A'           CPEATB                         *FLAG ANNULLAMENTO
036600970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
036700970527     C                   UPDATE    AZCPE000                                     *ANNULLA
036800970526X   3C                   ELSE                                                   *MAI TRASMESSO
036900970527     C                   DELETE    AZCPE000                                     *CANCELLA
037000000602     C                   MOVEL     '1'           ANNDLT
037100970526E   3C                   ENDIF
037200970526E   2C                   ENDIF
037300970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
037400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
037500970527     C*
037600970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
037700970527     C                   Z-ADD     D00VER        D81VER
037800970527     C                   MOVEL     VIDNAR        D81NAR
037900970527     C                   MOVEL     VIDCAP        D81CAP
038000970527     C                   MOVEL     'E'           D81CHK
038100970527     C                   CALL      'TISI81R'
038200970527     C                   PARM                    DSSI81
038300970526E   1C                   ENDIF
038400970526     C*
038500970526     C                   ENDSR
038600970526     C*------------------------------------------------------------------------*
038700970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
038800970526     C*------------------------------------------------------------------------*
038900970526     C     RIPRIS        BEGSR
039000970526     C*
039100970526     C* CONTROLLA I CAMPI IMMESSI
039200970526     C                   EXSR      DECODI
039300970526     C*
039400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
039500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039600970527     C                   EXSR      LETCPE
039700970526     C*
039800970526     C* IMPOSTA IL RECORD DA SCRIVERE
039900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040000970527     C                   MOVEL     *BLANKS       CPEATB                         *FLAG ANNULLAMENTO
040100970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
040200970527     C                   UPDATE    AZCPE000                                     *ANNULLA
040300970526E   2C                   ENDIF
040400970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
040500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
040600970527     C*
040700970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
040800970527     C                   Z-ADD     D00VER        D81VER
040900970527     C                   MOVEL     VIDNAR        D81NAR
041000970527     C                   MOVEL     VIDCAP        D81CAP
041100970527     C                   MOVEL     'E'           D81CHK
041200970530     C                   CALL      'TISI81R'
041300970530     C                   PARM                    DSSI81
041400970526E   1C                   ENDIF
041500970526     C*
041600970526     C                   ENDSR
041700970526     C*------------------------------------------------------------------------*
041800970527     C* LETCPE - LEGGE IL RECORD PERL'AGGIORNAMENTO
041900970526     C*------------------------------------------------------------------------*
042000970527     C     LETCPE        BEGSR
042100970526     C*
042200000601     C     DEPOP1        IFNE      'F06'
042300970527     C                   Z-ADD     D00VER        KCEVER
042400980127     C                   MOVEL     D00NAR        KCENAR
042500980127     C                   MOVEL     D00CAP        KCECAP
042600980127     C                   MOVEL     D00TFP        KCETFP
042700970527     C     KEYCPE        CHAIN     AZCPE01L                           9998
042800970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
042900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
043000970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
043100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
043200970526E   1C                   ENDIF
043300970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
043400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
043500970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
043600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
043700970526E   1C                   ENDIF
043800000601     C                   ENDIF
043900970526     C*
044000970526     C                   ENDSR
044100970526     C*------------------------------------------------------------------------*
044200970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
044300970526     C*------------------------------------------------------------------------*
044400970526     C     DECODI        BEGSR
044500970527     C*--------------------
044600970527     C* NAZIONE
044700970527     C*--------------------
044800970527     C     *LIKE         DEFINE    §15CPR        DEPCPR
044900970527IF  1C     ERRL00        IFEQ      '0'
045000970527     C                   MOVEL     *BLANKS       VDDNAR
045100970527     C                   MOVEL     VIDNAR        WNAR
045200970527     C                   MOVEL     *BLANKS       WDNAR
045300970527     C                   MOVEL     *BLANKS       WCPR
045400970527     C                   EXSR      CHKNAR
045500970527     C                   MOVEL     WNAR          VIDNAR
045600970527     C                   MOVEL     WDNAR         VDDNAR
045700970527     C                   MOVEL     WCPR          DEPCPR
045800970527IF  2C     ERRL00        IFEQ      '1'
045900970527     C                   SETON                                        60
046000970527     C                   MOVEL     MSG(11)       DSMSMS
046100970527E   2C                   ENDIF
046200970527E   1C                   ENDIF
046300970527     C*--------------------
046400970527     C* CAP
046500970527     C*--------------------
046600970527IF  1C     ERRL00        IFEQ      '0'
046700970527     C                   MOVEL     VIDNAR        WNAR
046800970527     C                   MOVEL     VIDCAP        WCAP
046900970527     C                   EXSR      CHKCAP
047000970527IF  2C     ERRL00        IFEQ      '1'
047100970527     C                   SETON                                        61
047200970527     C                   MOVEL     MSG(12)       DSMSMS
047300970527E   2C                   ENDIF
047400970527E   1C                   ENDIF
047500970626     C* CARICA DATI CAP
047600970527IF  1C     ERRL00        IFEQ      '0'
047700970626     C                   Z-ADD     O95CLA        VIDCLN
047800970626     C                   Z-ADD     O95CZC        VIDCZN
047900970626     C                   Z-ADD     O95CLO        VIDCLO
048000970626     C                   Z-ADD     O95CZO        VIDCZO
048100970626     C                   Z-ADD     O95CLK        VIDCKG
048200970626     C                   Z-ADD     O95CLM        VIDCMC
048300970626     C                   Z-ADD     O95CLS        VIDCLS
048400970626     C                   Z-ADD     O95CZS        VIDCZS
048500970626     C                   Z-ADD     O95CKS        VIDCKS
048600970626     C                   Z-ADD     O95CMS        VIDCMS
048700971118     C                   Z-ADD     O95CL2        VIDCL2
048800971118     C                   Z-ADD     O95CZ2        VIDCZ2
048900971118     C                   Z-ADD     O95CK2        VIDCK2
049000971118     C                   Z-ADD     O95CM2        VIDCM2
049100970527E   1C                   ENDIF
049200970527     C*--------------------
049300970527     C* TERMINAL PARTENZA
049400970527     C*--------------------
049500970527IF  1C     ERRL00        IFEQ      '0'
049600970527     C                   MOVEL     VIDTFP        WTFP
049700970527     C                   MOVEL     *BLANKS       WDTFP
049800970528     C                   EXSR      CHKTFX
049900970527     C                   MOVEL     WTFP          VIDTFP
050000970527     C                   MOVEL     WDTFP         VDDTFP
050100970527IF  2C     ERRL00        IFEQ      '1'
050200970527     C                   SETON                                        62
050300970527     C                   MOVEL     MSG(14)       DSMSMS
050400970527E   2C                   ENDIF
050500970527E   1C                   ENDIF
050600970527     C*--------------------
050700970527     C* NAZIONE + CAP + TERMINAL PARTENZA
050800970527     C*--------------------
050900970527     C* INSERIMENTO -> RECORD INESISTENTE
051000970527IF  1C     ERRL00        IFEQ      '0'
051100970527IF  2C     DEPOP1        IFEQ      'F06'
051200970527     C                   Z-ADD     D00VER        KCEVER
051300970527     C                   MOVEL     VIDNAR        KCENAR
051400970527     C                   MOVEL     VIDCAP        KCECAP
051500970527     C                   MOVEL     VIDTFP        KCETFP
051600970527     C     KEYCPE        CHAIN(N)  AZCPE01L                           99
051700970527IF  3C     *IN99         IFEQ      *OFF
051800970527     C                   SETON                                        606162
051900970527     C                   MOVEL     '1'           ERRL00
052000970527     C                   MOVEL     MSG(8)        DSMSMS
052100970527E   3C                   ENDIF
052200970527E   2C                   ENDIF
052300970527E   1C                   ENDIF
052400970527     C*--------------------
052500970606     C* LINEA ARRIVO STANDARD
052600970527     C*--------------------
052700970527IF  1C     ERRL00        IFEQ      '0'
052800970527     C                   MOVEL     VIDLNA        WLIN
052900970527     C                   MOVEL     *BLANKS       WDLIN
053000970527     C                   EXSR      CHKLIN
053100970527     C                   MOVEL     WLIN          VIDLNA
053200970527     C                   MOVEL     WDLIN         VDDLNA
053300970527IF  2C     ERRL00        IFEQ      '1'
053400970527     C                   SETON                                        63
053500970527     C                   MOVEL     MSG(15)       DSMSMS
053600970527E   2C                   ENDIF
053700970527E   1C                   ENDIF
053800970527     C*--------------------
053900970606     C* ZONA CONSEGNA STANDARD
054000970527     C*--------------------
054100970527IF  1C     ERRL00        IFEQ      '0'
054200970527     C                   MOVEL     VIDLNA        WLIN
054300970527     C                   Z-ADD     VIDZNC        WZNC
054400970527     C                   EXSR      CHKZNC
054500970527IF  2C     ERRL00        IFEQ      '1'
054600970527     C                   SETON                                        64
054700970527     C                   MOVEL     MSG(16)       DSMSMS
054800970527E   2C                   ENDIF
054900060206if  2c                   if        §03nuc = 'S'
055000060206     c                   movel     Msg(17)       DSmsms
055100060206     c                   eval      *in64  = *on
055200060206     c                   eval      ErrL00 = *on
055300060206e   2c                   endif
055400970527E   1C                   ENDIF
055500970606     C*--------------------
055600970912     C* LINEA ARRIVO SOTTO
055700970606     C*--------------------
055800970606IF  1C     ERRL00        IFEQ      '0'
055900970606     C                   MOVEL     *BLANKS       VDDLOS
056000970606IF  2C     VIDLOS        IFNE      *ZEROS
056100970606     C     VIDLOS        ANDNE     *BLANKS
056200970606     C                   MOVEL     VIDLOS        WLIN
056300970606     C                   MOVEL     *BLANKS       WDLIN
056400970606     C                   EXSR      CHKLIN
056500970606     C                   MOVEL     WLIN          VIDLOS
056600970606     C                   MOVEL     WDLIN         VDDLOS
056700970606IF  3C     ERRL00        IFEQ      '1'
056800970606     C                   SETON                                        67
056900970606     C                   MOVEL     MSG(15)       DSMSMS
057000970606E   3C                   ENDIF
057100970606E   2C                   ENDIF
057200970606E   1C                   ENDIF
057300970606     C*--------------------
057400970912     C* ZONA DI CONSEGNA SOTTO
057500970606     C*--------------------
057600970606IF  1C     ERRL00        IFEQ      '0'
057700971126IF  2C     VIDLOS        IFGT      *ZEROS
057800970606     C                   MOVEL     VIDLOS        WLIN
057900970606     C                   Z-ADD     VIDZOS        WZNC
058000970606     C                   EXSR      CHKZNC
058100970606IF  3C     ERRL00        IFEQ      '1'
058200970606     C                   SETON                                        68
058300970606     C                   MOVEL     MSG(16)       DSMSMS
058400970606E   3C                   ENDIF
058500060206if  3c                   if        §03nuc = 'S'
058600060206     c                   movel     Msg(17)       DSmsms
058700060206     c                   eval      *in68  = *on
058800060206     c                   eval      ErrL00 = *on
058900060206e   3c                   endif
059000970606E   2C                   ENDIF
059100970606E   1C                   ENDIF
059200970527     C*--------------------
059300970912     C* LINEA ARRIVO OLTRE
059400970527     C*--------------------
059500970527IF  1C     ERRL00        IFEQ      '0'
059600971126IF  2C     VIDLOL        IFNE      *ZEROS
059700971126     C     VIDLOL        ANDNE     *BLANKS
059800970527     C                   MOVEL     VIDLOL        WLIN
059900970527     C                   MOVEL     *BLANKS       WDLIN
060000970527     C                   EXSR      CHKLIN
060100970527     C                   MOVEL     WLIN          VIDLOL
060200970527     C                   MOVEL     WDLIN         VDDLOL
060300971126IF  3C     ERRL00        IFEQ      '1'
060400970527     C                   SETON                                        65
060500970527     C                   MOVEL     MSG(15)       DSMSMS
060600971126E   3C                   ENDIF
060700971126E   2C                   ENDIF
060800970527E   1C                   ENDIF
060900970527     C*--------------------
061000970912     C* ZONA CONSEGNA OLTRE
061100970527     C*--------------------
061200970527IF  1C     ERRL00        IFEQ      '0'
061300971126IF  2C     VIDLOL        IFGT      *ZEROS
061400970527     C                   MOVEL     VIDLOL        WLIN
061500970527     C                   Z-ADD     VIDZOL        WZNC
061600970527     C                   EXSR      CHKZNC
061700971126IF  3C     ERRL00        IFEQ      '1'
061800970527     C                   SETON                                        66
061900970527     C                   MOVEL     MSG(16)       DSMSMS
062000971126E   3C                   ENDIF
062100060206if  3c                   if        §03nuc = 'S'
062200060206     c                   movel     Msg(17)       DSmsms
062300060206     c                   eval      *in66  = *on
062400060206     c                   eval      ErrL00 = *on
062500060206e   3c                   endif
062600971126E   2C                   ENDIF
062700970527E   1C                   ENDIF
062800971118     C*--------------------
062900971118     C* LINEA ARRIVO OLTRE 2
063000971118     C*--------------------
063100971118IF  1C     ERRL00        IFEQ      '0'
063200971126IF  2C     VIDLO2        IFNE      *ZEROS
063300971126     C     VIDLO2        ANDNE     *BLANKS
063400971118     C                   MOVEL     VIDLO2        WLIN
063500971118     C                   MOVEL     *BLANKS       WDLIN
063600971118     C                   EXSR      CHKLIN
063700971118     C                   MOVEL     WLIN          VIDLO2
063800971118     C                   MOVEL     WDLIN         VDDLO2
063900971126IF  3C     ERRL00        IFEQ      '1'
064000971118     C                   SETON                                        69
064100971118     C                   MOVEL     MSG(15)       DSMSMS
064200971126E   3C                   ENDIF
064300971126E   2C                   ENDIF
064400971118E   1C                   ENDIF
064500971118     C*--------------------
064600971118     C* ZONA CONSEGNA OLTRE
064700971118     C*--------------------
064800971118IF  1C     ERRL00        IFEQ      '0'
064900971126IF  2C     VIDLO2        IFGT      *ZEROS
065000971118     C                   MOVEL     VIDLO2        WLIN
065100971118     C                   Z-ADD     VIDZO2        WZNC
065200971118     C                   EXSR      CHKZNC
065300971126IF  3C     ERRL00        IFEQ      '1'
065400971118     C                   SETON                                        70
065500971118     C                   MOVEL     MSG(16)       DSMSMS
065600971126E   3C                   ENDIF
065700060206if  2c                   if        §03nuc = 'S'
065800060206     c                   movel     Msg(17)       DSmsms
065900060206     c                   eval      *in70  = *on
066000060206     c                   eval      ErrL00 = *on
066100060206e   2c                   endif
066200971126E   2C                   ENDIF
066300971118E   1C                   ENDIF
066400970527     C*--------------------
066500970527     C* DECODIFICHE DATI CAPPARIO
066600970527     C*--------------------
066700970527     C* LINEA ARRIVO
066800970527IF  1C     ERRL00        IFEQ      '0'
066900970606IF  2C     VIDCLN        IFGT      *ZEROS
067000970527     C                   MOVEL     VIDCLN        WLIN
067100970527     C                   MOVEL     *BLANKS       WDLIN
067200970527     C                   EXSR      CHKLIN
067300970527     C                   MOVEL     WDLIN         VDDCLN
067400970606E   2C                   ENDIF
067500970606E   1C                   ENDIF
067600970912     C* LINEA ARRIVO SOTTO
067700970606IF  1C     ERRL00        IFEQ      '0'
067800970606IF  2C     VIDCLS        IFGT      *ZEROS
067900970606     C                   MOVEL     VIDCLS        WLIN
068000970606     C                   MOVEL     *BLANKS       WDLIN
068100970606     C                   EXSR      CHKLIN
068200970606     C                   MOVEL     WDLIN         VDDCLS
068300970606E   2C                   ENDIF
068400970606E   1C                   ENDIF
068500970912     C* LINEA ARRIVO OLTRE
068600970606IF  1C     ERRL00        IFEQ      '0'
068700970606IF  2C     VIDCLO        IFGT      *ZEROS
068800970527     C                   MOVEL     VIDCLO        WLIN
068900970527     C                   MOVEL     *BLANKS       WDLIN
069000970527     C                   EXSR      CHKLIN
069100970527     C                   MOVEL     WDLIN         VDDCLO
069200970606E   2C                   ENDIF
069300970606E   1C                   ENDIF
069400971118     C* LINEA ARRIVO OLTRE 2
069500971118IF  1C     ERRL00        IFEQ      '0'
069600971118IF  2C     VIDCL2        IFGT      *ZEROS
069700971118     C                   MOVEL     VIDCL2        WLIN
069800971118     C                   MOVEL     *BLANKS       WDLIN
069900971118     C                   EXSR      CHKLIN
070000971118     C                   MOVEL     WDLIN         VDDCL2
070100971118E   2C                   ENDIF
070200971118E   1C                   ENDIF
070300970527     C*
070400970527     C                   ENDSR
070500970527     C*------------------------------------------------------------------------*
070600970527     C* CHKNAR - CONTROLLA LA NAZIONE
070700970527     C*------------------------------------------------------------------------*
070800970527     C     CHKNAR        BEGSR
070900970527     C*
071000970527     C     *LIKE         DEFINE    VIDNAR        WNAR
071100970527     C     *LIKE         DEFINE    VDDNAR        WDNAR
071200970527     C     *LIKE         DEFINE    §15CPR        WCPR
071300970527     C*
071400970527     C* RICHIESTO ELENCO
071500970527     C                   SETOFF                                           28
071600970527     C     '?'           SCAN      WNAR                                   28
071700970527IF  1C     *IN28         IFEQ      *ON
071800020906     C                   Z-ADD     1             §KUT              1 0
071900970527     C                   MOVEL     '15'          §COD              2
072000970527     C                   MOVEL     *BLANKS       §KEY              8
072100970527     C                   MOVEL     *BLANKS       §DES             30
072200970527     C                   CALL      'X§TABER'
072300970527     C                   PARM                    §KUT
072400970527     C                   PARM                    §COD
072500970527     C                   PARM                    §KEY
072600970527     C                   PARM                    §DES
072700970527     C                   MOVEL     §KEY          WNAR
072800970527     C                   MOVEL     §DES          WDNAR
072900970527E   1C                   ENDIF
073000970527     C*
073100970527     C* CONTROLLA ESISTENZA CODICE
073200020906     C                   MOVEL     1             KTBKUT
073300970527     C                   MOVEL     '15'          KTBCOD
073400970527     C                   MOVEL(P)  WNAR          KTBKEY
073500970527     C     KEYTAB        CHAIN     TABEL00F                           99
073600970527IF  1C     *IN99         IFEQ      *OFF
073700970527     C     TBLFLG        ANDEQ     *BLANKS
073800970527     C                   MOVEL     TBLUNI        DS15
073900970527     C                   MOVEL     §15DES        WDNAR
074000970527     C                   MOVEL     §15CPR        WCPR
074100970527     C*
074200970527     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
074300970527IF  2C     VIDNAR        IFEQ      'I  '
074400970527     C                   MOVEL     '1'           ERRL00
074500970527     C                   MOVEL     *BLANKS       WDNAR
074600970527E   2C                   ENDIF
074700970527X   1C                   ELSE
074800970527     C                   MOVEL     '1'           ERRL00
074900970527E   1C                   ENDIF
075000970527     C*
075100970527     C                   ENDSR
075200970527     C*------------------------------------------------------------------------*
075300970527     C* CHKCAP - CONTROLLA IL CAP
075400970527     C*------------------------------------------------------------------------*
075500970527     C     CHKCAP        BEGSR
075600970527     C*
075700970527     C     *LIKE         DEFINE    VIDCAP        WCAP
075800970626     C*
075900970626     C* CONTROLLA IL CAP
076000970626     C                   CLEAR                   DSSI95
076100970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
076200970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
076300970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
076400970626     C                   MOVEL     WCAP          I95CAP                         *CAP
076500970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
076600970626     C                   CALL      'TISI95R'
076700970626     C                   PARM                    DSSI95
076800970626IF  1C     O95ERR        IFNE      *BLANKS
076900970626     C                   MOVEL     '1'           ERRL00
077000970626E   1C                   ENDIF
077100970527     C*
077200970527     C                   ENDSR
077300970527     C*------------------------------------------------------------------------*
077400970528     C* CHKTFX - CONTROLLA IL TERMINAL
077500970527     C*------------------------------------------------------------------------*
077600970528     C     CHKTFX        BEGSR
077700970527     C*
077800970527     C     *LIKE         DEFINE    VIDTFP        WTFP
077900970527     C     *LIKE         DEFINE    VDDTFP        WDTFP
078000970527     C*
078100970527     C* RICHIESTO ELENCO
078200970527     C                   SETOFF                                           28
078300970527     C     '?'           SCAN      WTFP                                   28
078400970527IF  1C     *IN28         IFEQ      *ON
078500970527     C                   MOVEL     *BLANKS       §COD1             3
078600970527     C                   MOVEL     *BLANKS       §TIP              1
078700970527     C                   MOVEL     *BLANKS       §DES1            25
078800971203     C                   MOVEL     *BLANKS       §PRA              3
078900971203     C                   CALL      'TNSD24R'
079000970527     C                   PARM                    §COD1
079100970527     C                   PARM                    §TIP
079200970527     C                   PARM                    §DES1
079300971203     C                   PARM                    §PRA
079400970527     C                   MOVEL     §COD1         WTFP
079500970527     C                   MOVEL     §DES1         WDTFP
079600970527E   1C                   ENDIF
079700970527     C*
079800970527     C* SOLO CARATTERI NUMERICI
079900970527     C                   SETOFF                                       28
080000970527     C                   TESTN                   WTFP                 28
080100970527IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
080200970527     C                   MOVEL     '1'           ERRL00
080300970527X   1C                   ELSE                                                   *E'NUMERO DA TESTN
080400970527     C                   MOVE      WTFP          W001              1
080500970527IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
080600970527     C                   MOVEL     '1'           ERRL00
080700970527X   2C                   ELSE                                                   *NUMERO
080800971128     C*
080900971128     C* REPERISCE IL TERMINAL
081000971128     C                   CLEAR                   DSLV55
081100971128     C                   MOVEL     'P'           D55TPT
081200971128     C                   MOVEL     WTFP          D55LIN
081300971128     C                   Z-ADD     DATCOR        D55DRF
081400971128     C                   CALL      'FNLV55R'
081500971128     C                   PARM                    DSLV55
081600971128IF  3C     D55ERR        IFNE      *BLANKS
081700971128     C                   MOVEL     WTFP          D55TFP
081800971128E   3C                   ENDIF
081900970527     C*
082000970527     C* CONTROLLA ESISTENZA CODICE
082100000613     C                   CLEAR                   OG143
082200970527     C                   MOVEL     WTFP          KORFIL
082300970527     C     KEYORG        CHAIN     AZORG01L                           99
082400000613IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
082500000613     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
082600000613     C     ORGFAG        ANDNE     'V'                                          *NON VARIA
082700000613     C                   MOVEL     ORGDE3        OG143
082800000613IF  4C     ORGFIL        IFEQ      D55TFP                                       *TERMINAL PARTENZA
082900020627     C     §OGNTW        OREQ      'PPT'                                        *FILIALE POSTE
083000000613     C                   MOVEL     ORGDES        WDTFP                          *DESCRIZIONE
083100000613X   4C                   ELSE
083200000613     C                   MOVEL     '1'           ERRL00                         *ERRORE
083300000613E   3C                   ENDIF
083400970527X   3C                   ELSE
083500000613     C                   MOVEL     '1'           ERRL00                         *ERRORE
083600970527E   3C                   ENDIF
083700970527E   2C                   ENDIF
083800970527E   1C                   ENDIF
083900970527     C*
084000970527     C                   ENDSR
084100970527     C*------------------------------------------------------------------------*
084200970527     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
084300970527     C*------------------------------------------------------------------------*
084400970527     C     CHKLIN        BEGSR
084500970527     C*
084600970527     C     *LIKE         DEFINE    VIDLNA        WLIN
084700970527     C     *LIKE         DEFINE    VDDLNA        WDLIN
084800970527     C*
084900970527     C* RICHIESTO ELENCO
085000970527     C                   SETOFF                                           28
085100970527     C     '?'           SCAN      WLIN                                   28
085200970527IF  1C     *IN28         IFEQ      *ON
085300970527     C                   MOVEL     *BLANKS       §COD1             3
085400970527     C                   MOVEL     *BLANKS       §TIP              1
085500970527     C                   MOVEL     *BLANKS       §DES1            25
085600971203     C                   MOVEL     *BLANKS       §PRA              3
085700980409     C                   CALL      'TNSD24R'
085800970527     C                   PARM                    §COD1
085900970527     C                   PARM                    §TIP
086000970527     C                   PARM                    §DES1
086100971203     C                   PARM                    §PRA
086200970527     C                   MOVEL     §COD1         WLIN
086300970527     C                   MOVEL     §DES1         WDLIN
086400970527E   1C                   ENDIF
086500970527     C*
086600970527     C* SOLO CARATTERI NUMERICI
086700970527     C                   SETOFF                                       28
086800970527     C                   TESTN                   WLIN                 28
086900970527IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
087000970527     C                   MOVEL     '1'           ERRL00
087100970527X   1C                   ELSE                                                   *E'NUMERO DA TESTN
087200970527     C                   MOVE      WLIN          W001              1
087300970527IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
087400970527     C                   MOVEL     '1'           ERRL00
087500970527X   2C                   ELSE                                                   *NUMERO
087600970527     C*
087700970527     C* CONTROLLA ESISTENZA CODICE
087800970527     C                   MOVEL     WLIN          KORFIL
087900970527     C     KEYORG        CHAIN     AZORG01L                           99
088000970527IF  3C     *IN99         IFEQ      *ON                                          *NON TROVATA
088100970527     C     ORGFVA        ORNE      *BLANKS                                      *ANNULLATA
088200970527     C     ORGFAG        OREQ      'V'                                          *VARIA
088300970527     C                   MOVEL     '1'           ERRL00
088400970527X   3C                   ELSE
088500970527     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
088600970527E   3C                   ENDIF
088700970527E   2C                   ENDIF
088800970527E   1C                   ENDIF
088900970527     C*
089000970527     C                   ENDSR
089100970527     C*------------------------------------------------------------------------*
089200970527     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
089300970527     C*------------------------------------------------------------------------*
089400970527     C     CHKZNC        BEGSR
089500970527     C*
089600970527     C     *LIKE         DEFINE    VIDZNC        WZNC
089700970527     C*
089800970527     C* CONTROLLA ESISTENZA CODICE
089900020906     C                   Z-ADD     1             KTBKUT
090000970527     C                   MOVEL     '03'          KTBCOD
090100970527     C                   MOVEL     WLIN          WA5               5
090200970527     C                   MOVE      WZNC          WA5
090300970527     C                   MOVEL(P)  WA5           KTBKEY
090400970527     C     KEYTAB        CHAIN     TABEL00F                           99
090500970527IF  1C     *IN99         IFEQ      *OFF
090600970527     C     TBLFLG        ANDEQ     *BLANKS
090700970527     C                   MOVEL     TBLUNI        DS03
090800970527X   1C                   ELSE
090900970527     C                   MOVEL     '1'           ERRL00
091000970527E   1C                   ENDIF
091100970527     C*
091200970527     C                   ENDSR
091300970526     C*------------------------------------------------------------------------*
091400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
091500970526     C*------------------------------------------------------------------------*
091600970526     C     IREVID        BEGSR
091700970526     C* CAMPI
091800970527     C                   MOVEL     CPENAR        VIDNAR
091900970527     C                   MOVEL     CPECAP        VIDCAP
092000970527     C                   MOVEL     CPETFP        VIDTFP
092100970527     C                   MOVEL     CPELNA        VIDLNA
092200970527     C                   Z-ADD     CPEZNC        VIDZNC
092300970527     C                   MOVEL     CPELOL        VIDLOL
092400970527     C                   Z-ADD     CPEZOL        VIDZOL
092500970606     C                   MOVEL     CPELOS        VIDLOS
092600970606     C                   Z-ADD     CPEZOS        VIDZOS
092700971118     C                   MOVEL     CPELO2        VIDLO2
092800971118     C                   Z-ADD     CPEZO2        VIDZO2
092900970527     C*
093000970526     C                   ENDSR
093100970526     C*------------------------------------------------------------------------*
093200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
093300970526     C*------------------------------------------------------------------------*
093400970526     C     AZZVID        BEGSR
093500970526     C* CAMPI
093600970603     C                   MOVEL     D00NAR        VIDNAR
093700970603     C                   MOVEL     D00CAP        VIDCAP
093800970527     C                   MOVEL     *ZEROS        VIDTFP
093900970527     C                   MOVEL     *ZEROS        VIDLNA
094000970527     C                   Z-ADD     *ZEROS        VIDZNC
094100970527     C                   MOVEL     *ZEROS        VIDLOL
094200970527     C                   Z-ADD     *ZEROS        VIDZOL
094300970606     C                   MOVEL     *ZEROS        VIDLOS
094400970606     C                   Z-ADD     *ZEROS        VIDZOS
094500971118     C                   MOVEL     *ZEROS        VIDLO2
094600971118     C                   Z-ADD     *ZEROS        VIDZO2
094700970526     C* DECODIFICHE
094800970527     C                   MOVEL     *BLANKS       VDDNAR
094900970527     C                   MOVEL     *BLANKS       VDDTFP
095000970527     C                   MOVEL     *BLANKS       VDDLNA
095100970527     C                   MOVEL     *BLANKS       VDDLOL
095200970606     C                   MOVEL     *BLANKS       VDDLOS
095300971118     C                   MOVEL     *BLANKS       VDDLO2
095400970527     C* CAMPI CAPPARIO
095500970527     C                   Z-ADD     *ZEROS        VIDCLN
095600970527     C                   Z-ADD     *ZEROS        VIDCZN
095700970606     C                   MOVEL     *BLANKS       VDDCLN
095800971118     C                   Z-ADD     *ZEROS        VIDCLO
095900970527     C                   Z-ADD     *ZEROS        VIDCZO
096000971118     C                   MOVEL     *BLANKS       VDDCLO
096100970606     C                   Z-ADD     *ZEROS        VIDCLS
096200970606     C                   Z-ADD     *ZEROS        VIDCZS
096300971118     C                   MOVEL     *BLANKS       VDDCLS
096400971118     C                   Z-ADD     *ZEROS        VIDCL2
096500971118     C                   Z-ADD     *ZEROS        VIDCZ2
096600971118     C                   MOVEL     *BLANKS       VDDCL2
096700970526     C*
096800970526     C                   ENDSR
096900970526     C*------------------------------------------------------------------------*
097000970527     C* IMPCPE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
097100970526     C*------------------------------------------------------------------------*
097200970527     C     IMPCPE        BEGSR
097300970526     C*
097400970527     C                   MOVEL     VIDLNA        CPELNA
097500970527     C                   Z-ADD     VIDZNC        CPEZNC
097600970527     C                   MOVEL     VIDLOL        CPELOL
097700970527     C                   Z-ADD     VIDZOL        CPEZOL
097800970606     C                   MOVEL     VIDLOS        CPELOS
097900970606     C                   Z-ADD     VIDZOS        CPEZOS
098000971118     C                   MOVEL     VIDLO2        CPELO2
098100971118     C                   Z-ADD     VIDZO2        CPEZO2
098200970526     C*
098300970526     C                   ENDSR
098400970526     C*------------------------------------------------------------------------*
098500970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
098600970526     C*------------------------------------------------------------------------*
098700970526     C     IMPFUN        BEGSR
098800970526     C*
098900970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
099000970526IF  1C     DEPOP1        IFEQ      'O05'
099100970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
099200970526     C                   MOVEL     'N'           FUDATT
099300970526     C                   MOVEL     FUDRIG        FUD(6)
099400970526E   1C                   ENDIF
099500970526     C*
099600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
099700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
099800970526     C                   EXSR      FUNGES
099900970526     C*
100000970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
100100970526     C*---
100200970526     C* 90 -> PROTEZIONE CHIAVE
100300970526     C* 91 -> PROTEZIONE TOTALE
100400970526     C*---
100500970527IF  1C     DEPOP0        IFEQ      'E02'                                        G E S T I O N E
100600970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
100700970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
100800970526     C                   SETOFF                                       91
100900970526E   2C                   ENDIF
101000970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
101100970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
101200970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
101300970526E   2C                   ENDIF
101400970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
101500970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
101600970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
101700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
101800970526     C                   SETON                                        91
101900970526E   2C                   ENDIF
102000970526X   1C                   ELSE                                                   *I N T E R R O G A
102100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
102200970526     C                   SETON                                        91
102300970526E   1C                   ENDIF
102400970526     C*
102500970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
102600970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
102700970527     C                   Z-ADD     D00VER        KCEVER
102800970527     C                   MOVEL     D00NAR        KCENAR
102900970527     C                   MOVEL     D00CAP        KCECAP
103000970527     C                   Z-ADD     D00TFP        KCETFP
103100970527     C     KEYCPE        CHAIN(N)  AZCPE01L                           99
103200970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
103300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
103400970526     C                   MOVEL     MSG(6)        DSMSMS
103500970526X   2C                   ELSE
103600970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
103700980128IF  3C     DEPOP1        IFEQ      'O05'
103800980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
103900980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
104000980128E   3C                   ENDIF
104100970526E   2C                   ENDIF
104200970526X   1C                   ELSE                                                   *INSERIMENTO
104300970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
104400970526E   1C                   ENDIF
104500970526     C*
104600970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
104700970526IF  1C     DEPOP1        IFEQ      'O02'
104800970526     C                   MOVEL     DEC(1)        VIDFUN
104900970526E   1C                   ENDIF
105000970526IF  1C     DEPOP1        IFEQ      'O04'
105100970526     C                   MOVEL     DEC(2)        VIDFUN
105200970526E   1C                   ENDIF
105300970526IF  1C     DEPOP1        IFEQ      'O05'
105400970526     C                   MOVEL     DEC(3)        VIDFUN
105500970526E   1C                   ENDIF
105600970526IF  1C     DEPOP1        IFEQ      'O07'
105700970526     C                   MOVEL     DEC(4)        VIDFUN
105800970526E   1C                   ENDIF
105900970526IF  1C     DEPOP1        IFEQ      'F06'
106000970526     C                   MOVEL     DEC(5)        VIDFUN
106100970526E   1C                   ENDIF
106200970526     C*
106300970526     C                   ENDSR
106400970626     C*------------------------------------------------------------------------*
106500970626     C* FINSR - OPERAZIONI FINALI
106600970626     C*------------------------------------------------------------------------*
106700970626     C     FINSR         BEGSR
106800971128     C*
106900970704     C                   CLEAR                   DSSI95
107000970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
107100970626     C                   CALL      'TISI95R'
107200970626     C                   PARM                    DSSI95
107300971128     C*
107400971128     C                   CLEAR                   DSLV55
107500971128     C                   MOVEL     'C'           D55TLA
107600971128     C                   CALL      'FNLV55R'
107700971128     C                   PARM                    DSLV55
107800970626     C*
107900970626     C                   ENDSR
108000970526     C*------------------------------------------------------------------------*
108100970526     C* *INZSR - OPERAZIONI INIZIALI
108200970526     C*------------------------------------------------------------------------*
108300970526     C     *INZSR        BEGSR
108400970526     C*---
108500970526     C* RICEVIMENTO PARAMETRI
108600970526     C*---
108700970526     C     *ENTRY        PLIST
108800970612     C                   PARM                    KPJBA
108900970612     C                   PARM                    DSSI00
109000970526     C*
109100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
109200970526     C     XMSGPA        PLIST
109300970526     C                   PARM                    DSMSMS
109400970526     C                   PARM                    DSMSPG
109500970526     C                   PARM                    DSMSKE
109600970526     C                   PARM                    DSMSER
109700970526     C*
109800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
109900970526     C                   CALL      'XKEY'
110000970526     C                   PARM                    DSKEY
110100970526     C*---
110200970526     C* VARIABILI NON RIFERITE AL DATA BASE
110300970526     C*---
110400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
110500970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
110600970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
110700970526     C     *LIKE         DEFINE    D00OP0        WOP0
110800970526     C     *LIKE         DEFINE    D00OP1        WOP1
110900970526     C*---
111000970526     C* VARIABILI DI CONTROLLO
111100970526     C*---
111200970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
111300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
111400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
111500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
111600970526     C*
111700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
111800970527     C                   MOVEL     'TISI12R'     DSMSPG
111900970527     C                   MOVEL     'TISI12R'     MSGQUE
112000970526     C                   MOVEL     *BLANKS       MSGKEY
112100970526     C                   MOVEL     *BLANKS       DSMSMS
112200970526     C*---
112300970526     C* VARIABILI RIFERITE AL DATA BASE
112400970526     C*---
112500970527     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
112600970527     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
112700970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
112800970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
112900970527     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
113000970527     C     *LIKE         DEFINE    CPENAR        KCENAR
113100970527     C     *LIKE         DEFINE    CPECAP        KCECAP
113200970527     C     *LIKE         DEFINE    CPETFP        KCETFP
113300970526     C*---
113400970526     C* CHIAVI DI LETTURA
113500970526     C*---
113600970527     C* LETTURA AZORG01L - COMPLETA
113700970527     C     KEYORG        KLIST
113800970528     C                   KFLD                    KORFIL                         *FILIALE
113900970526     C* LETTURA TABEL00F - COMPLETA
114000970526     C     KEYTAB        KLIST
114100970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
114200970526     C                   KFLD                    KTBCOD                         *TABELLA
114300970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
114400970527     C* LETTURA AZCPE01L - COMPLETA
114500970527     C     KEYCPE        KLIST
114600970527     C                   KFLD                    KCEVER                         *VERSIONE
114700970527     C                   KFLD                    KCENAR                         *NAZIONE
114800970527     C                   KFLD                    KCECAP                         *CAP
114900970527     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
115000970526     C*
115100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
115200970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
115300970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
115400971128     C*
115500971128     C* CALCOLA LA DATA CORRENTE
115600971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
115700971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
115800971128     C                   Z-ADD     WN8           G08DAT
115900971128     C                   Z-ADD     *ZEROS        G08INV
116000971128     C                   MOVEL     '0'           G08ERR
116100971128     C                   CALL      'XSRDA8'
116200971128     C                   PARM                    WLBDA8
116300971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
116400060206      *
116500060206      * REPERISCE I DATI DELL'UTENTE
116600060206     c                   exsr      RepDatiUte
116700060206     c                   movel     RSUT          DSfirs
116800970526     C*
116900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
117000970526     C                   EXSR      IMPFUN
117100970526     C*
117200970526     C                   ENDSR
117300060206      *---------------------------------------------------------------*
117400060206      * REPERISCE DATI UTENTE                                         *
117500060206      *---------------------------------------------------------------*
117600060206     c     RepDatiUte    BEGSR
117700060206      *
117800060206     c     *dtaara       define    §azute        AZUTEds
117900060206     c     *dtaara       define    §datiute      DDatiUte
118000060206      *
118100060206     c                   in(E)     *dtaara
118200060206     c                   IF        %ERROR or RSUT = *blanks
118300060206     c                   clear                   Tibs34Ds
118400060206     c                   call      'TIBS34R'
118500060206     c                   parm                    Tibs34Ds
118600060206     c                   in        *dtaara
118700060206     c                   ENDIF
118800060206      *
118900060206     c                   ENDSR
119000970526     C*------------------------------------------------------------------------*
119100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
119200970526     C*------------------------------------------------------------------------*
119300970526     C     FUNGES        BEGSR
119400970526     C*
119500970526     C* PULISCO CAMPI DI LAVORO
119600970526     C                   Z-ADD     0             W                 5 0
119700970526     C*
119800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
119900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
120000970526     C     1             DO        24            I
120100970526     C                   MOVEL     FUD(I)        FUDRIG
120200970526     C*
120300970526     C* PULIZIA FUNZIONI PRECEDENTI
120400970526     C                   MOVEL     *BLANKS       FUV(I)
120500970526     C*
120600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
120700970526     C* (SALTA F24)
120800970526     C     FUDATT        IFEQ      'S'
120900970526     C     FUDFUN        ANDNE     '24'
121000970526     C                   ADD       1             W
121100970526     C                   MOVEL     FUDDES        FUV(W)
121200970526     C                   END
121300970526     C                   END                                                    DO WHILE
121400970526     C*
121500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
121600970526     C* ALTRIMENTI DISATTIVO F24
121700970526     C     1             DO        24            I
121800970526     C                   MOVEL     FUD(I)        FUDRIG
121900970526     C*
122000970526     C     FUDFUN        IFEQ      '24'
122100970526     C     W             IFGT      6
122200970526     C                   MOVEL     'S'           FUDATT
122300970526     C                   ELSE
122400970526     C                   MOVEL     *BLANKS       FUDATT
122500970526     C                   END                                                    W GT 8
122600970526     C                   MOVEL     FUDRIG        FUD(I)
122700970526     C                   END
122800970526     C                   END                                                    DO WHILE
122900970526     C*
123000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
123100970526     C     W             DIV       6             I
123200970526     C                   MVR                     Z
123300970526     C*
123400970526     C     Z             IFNE      0
123500970526     C     I             ADD       1             Z
123600970526     C                   ELSE
123700970526     C                   Z-ADD     I             Z                 5 0
123800970526     C                   END
123900970526     C*
124000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
124100970526     C                   ADD       1             FUNGRU
124200970526     C     FUNGRU        IFGT      Z
124300970526     C     FUNGRU        ORLT      1
124400970526     C                   Z-ADD     1             FUNGRU
124500970526     C                   END
124600970526     C*
124700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
124800970526     C     FUNGRU        IFEQ      1
124900970526     C                   MOVEL     FUV(1)        FUN001
125000970526     C                   MOVEL     FUV(2)        FUN002
125100970526     C                   MOVEL     FUV(3)        FUN003
125200970526     C                   MOVEL     FUV(4)        FUN004
125300970526     C                   MOVEL     FUV(5)        FUN005
125400970526     C                   MOVEL     FUV(6)        FUN006
125500970526     C                   END
125600970526     C*
125700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
125800970526     C     FUNGRU        IFEQ      2
125900970526     C                   MOVEL     FUV(7)        FUN001
126000970526     C                   MOVEL     FUV(8)        FUN002
126100970526     C                   MOVEL     FUV(9)        FUN003
126200970526     C                   MOVEL     FUV(10)       FUN004
126300970526     C                   MOVEL     FUV(11)       FUN005
126400970526     C                   MOVEL     FUV(12)       FUN006
126500970526     C                   END
126600970526     C*
126700970526     C* TERZO GRUPPO DI 6 FUNZIONI
126800970526     C     FUNGRU        IFEQ      3
126900970526     C                   MOVEL     FUV(13)       FUN001
127000970526     C                   MOVEL     FUV(14)       FUN002
127100970526     C                   MOVEL     FUV(15)       FUN003
127200970526     C                   MOVEL     FUV(16)       FUN004
127300970526     C                   MOVEL     FUV(17)       FUN005
127400970526     C                   MOVEL     FUV(18)       FUN006
127500970526     C                   END
127600970526     C*
127700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
127800970526     C     FUNGRU        IFEQ      4
127900970526     C                   MOVEL     FUV(19)       FUN001
128000970526     C                   MOVEL     FUV(20)       FUN002
128100970526     C                   MOVEL     FUV(21)       FUN003
128200970526     C                   MOVEL     FUV(22)       FUN004
128300970526     C                   MOVEL     *BLANKS       FUN005
128400970526     C                   MOVEL     *BLANKS       FUN006
128500970526     C                   END
128600970526     C*
128700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
128800970526     C                   MOVEL     *BLANKS       FUN007
128900970526     C                   MOVEL     *BLANKS       FUN008
129000970526     C*
129100970526     C     1             DO        24            I
129200970526     C                   MOVEL     FUD(I)        FUDRIG
129300970526     C     FUDFUN        IFEQ      '24'
129400970526     C     FUDATT        ANDEQ     'S'
129500970526     C                   MOVEL     FUDDES        FUN008
129600970526     C                   END
129700970526     C                   END                                                    DO
129800970526     C*
129900970526     C                   ENDSR
130000970526     C*------------------------------------------------------------------------*
130100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
130200970526     C*------------------------------------------------------------------------*
130300970526     C     FUNCON        BEGSR
130400970526     C*
130500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
130600970526     C                   EXSR      FUNVAR
130700970526     C*
130800970526     C* PULISCO CAMPI DI LAVORO
130900970526     C                   Z-ADD     0             I                 5 0
131000970526     C                   Z-ADD     0             W                 5 0
131100970526     C*
131200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
131300970526     C     FUNKEY        IFNE      *BLANKS
131400970526     C     1             DO        24            I
131500970526     C                   MOVEL     FUD(I)        FUDRIG
131600970526     C*
131700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
131800970526     C     FUNKEY        IFEQ      FUDFUN
131900970526     C     FUDATT        ANDEQ     'S'
132000970526     C                   Z-ADD     I             W
132100970526     C                   END
132200970526     C                   END                                                    DO WHILE
132300970526     C*
132400970526     C* FUNZIONE NON ATTIVA (ERRORE)
132500970526     C     W             IFEQ      0
132600970526     C*
132700970526     C* EMISSIONE MESSAGGIO DI ERRORE
132800970526     C                   MOVEL     MSG(1)        DSMSMS
132900970526     C*
133000970526     C                   END                                                    W=0
133100970526     C                   END                                                    FUNKEY NE *BLANKS
133200970526     C*
133300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
133400970526     C     W             IFNE      0
133500970526     C     FUNKEY        OREQ      *BLANKS
133600970526     C     FUNKEY        IFEQ      *BLANKS
133700970526     C                   EXSR      ENTGES
133800970526     C                   END
133900970526     C     FUNKEY        IFEQ      '03'
134000970526     C                   EXSR      F03GES
134100970526     C                   END
134200970526     C     FUNKEY        IFEQ      '06'
134300970526     C                   EXSR      F06GES
134400970526     C                   END
134500130905     C     FUNKEY        IFEQ      '07'
134600130905     C                   EXSR      F07GES
134700130905     C                   END
134800970526     C     FUNKEY        IFEQ      '12'
134900970526     C                   EXSR      F12GES
135000970526     C                   END
135100970526     C     FUNKEY        IFEQ      '24'
135200970526     C                   EXSR      F24GES
135300970526     C                   END
135400970526     C                   END
135500970526     C*
135600970526     C                   ENDSR
135700970526     C*------------------------------------------------------------------------*
135800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
135900970526     C*------------------------------------------------------------------------*
136000970526     C     FUNVAR        BEGSR
136100970526     C*
136200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
136300970526     C*
136400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
136500970526     C     KEY           IFEQ      ENTER
136600970526     C                   MOVEL     *BLANKS       FUNKEY
136700970526     C                   END
136800970526     C     KEY           IFEQ      F1
136900970526     C                   MOVEL     '01'          FUNKEY
137000970526     C                   END
137100970526     C     KEY           IFEQ      F2
137200970526     C                   MOVEL     '02'          FUNKEY
137300970526     C                   END
137400970526     C     KEY           IFEQ      F3
137500970526     C                   MOVEL     '03'          FUNKEY
137600970526     C                   END
137700970526     C     KEY           IFEQ      F4
137800970526     C                   MOVEL     '04'          FUNKEY
137900970526     C                   END
138000970526     C     KEY           IFEQ      F5
138100970526     C                   MOVEL     '05'          FUNKEY
138200970526     C                   END
138300970526     C     KEY           IFEQ      F6
138400970526     C                   MOVEL     '06'          FUNKEY
138500970526     C                   END
138600970526     C     KEY           IFEQ      F7
138700970526     C                   MOVEL     '07'          FUNKEY
138800970526     C                   END
138900970526     C     KEY           IFEQ      F8
139000970526     C                   MOVEL     '08'          FUNKEY
139100970526     C                   END
139200970526     C     KEY           IFEQ      F9
139300970526     C                   MOVEL     '09'          FUNKEY
139400970526     C                   END
139500970526     C     KEY           IFEQ      F10
139600970526     C                   MOVEL     '10'          FUNKEY
139700970526     C                   END
139800970526     C     KEY           IFEQ      F11
139900970526     C                   MOVEL     '11'          FUNKEY
140000970526     C                   END
140100970526     C     KEY           IFEQ      F12
140200970526     C                   MOVEL     '12'          FUNKEY
140300970526     C                   END
140400970526     C     KEY           IFEQ      F13
140500970526     C                   MOVEL     '13'          FUNKEY
140600970526     C                   END
140700970526     C     KEY           IFEQ      F14
140800970526     C                   MOVEL     '14'          FUNKEY
140900970526     C                   END
141000970526     C     KEY           IFEQ      F15
141100970526     C                   MOVEL     '15'          FUNKEY
141200970526     C                   END
141300970526     C     KEY           IFEQ      F16
141400970526     C                   MOVEL     '16'          FUNKEY
141500970526     C                   END
141600970526     C     KEY           IFEQ      F17
141700970526     C                   MOVEL     '17'          FUNKEY
141800970526     C                   END
141900970526     C     KEY           IFEQ      F18
142000970526     C                   MOVEL     '18'          FUNKEY
142100970526     C                   END
142200970526     C     KEY           IFEQ      F19
142300970526     C                   MOVEL     '19'          FUNKEY
142400970526     C                   END
142500970526     C     KEY           IFEQ      F20
142600970526     C                   MOVEL     '20'          FUNKEY
142700970526     C                   END
142800970526     C     KEY           IFEQ      F21
142900970526     C                   MOVEL     '21'          FUNKEY
143000970526     C                   END
143100970526     C     KEY           IFEQ      F22
143200970526     C                   MOVEL     '22'          FUNKEY
143300970526     C                   END
143400970526     C     KEY           IFEQ      F23
143500970526     C                   MOVEL     '23'          FUNKEY
143600970526     C                   END
143700970526     C     KEY           IFEQ      F24
143800970526     C                   MOVEL     '24'          FUNKEY
143900970526     C                   END
144000970526     C     KEY           IFEQ      ROLLUP
144100970526     C                   MOVEL     'RU'          FUNKEY
144200970526     C                   END
144300970526     C     KEY           IFEQ      HELP
144400970526     C                   MOVEL     'HP'          FUNKEY
144500970526     C                   END
144600970526     C                   ENDSR
144700970526     C*------------------------------------------------------------------------*
144800970526     C* NOTE COSTRUZIONE SCHIERE
144900970526     C*------------------------------------------------------------------------*
145000970526     C*
145100970526     C* -MSG-MESSAGGI
145200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
145300970526     C* (NON TOCCARE MSG,1 E MSG,2)
145400970526     C*
145500970526     C* -CMD-COMANDI
145600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
145700970526     C*
145800970526     C* -OPD-OPZIONI
145900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
146000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
146100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
146200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
146300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
146400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
146500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
146600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
146700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
146800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
146900970526     C*
147000970526     C* -FUD-FUNZIONI
147100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
147200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
147300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
147400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
147500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
147600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
147700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
147800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
147900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
148000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
148100970526     O*------------------------------------------------------------------------*
148200970526** DEC - DECODIFICHE
148300970526    MODIFICA                                                      1
148400970526  ANNULLAMENTO                                                    2
148500970526VISUALIZZAZIONE                                                   3
148600970526   RIPRISTINO                                                     4
148700970526  INSERIMENTO                                                     5
148800970526** MSG - MESSAGGI
148900970527TISI12R- Tasto funzionale non gestito                                          1
149000970527TISI12R- Inserimento effettuato                                                2
149100970527TISI12R- Modifica effettuata                                                   3
149200970527TISI12R- Annullamento effettuato                                               4
149300970527TISI12R- Ripristino effettuato                                                 5
149400970527TISI12R- Richiesta funzione per record inesistente                             6
149500970527TISI12R- Record momentaneamente allocato: riprovare più tardi                  7
149600970527TISI12R- Richiesta funzione per record esistente                               8
149700000601TISI12R- 9
149800000601TISI12R- 10                                                                   10
149900970527TISI12R- Nazione errata                                                       11
150000970626TISI12R- CAP errato o inesistente                                             12
150100970626TISI12R- 13
150200970527TISI12R- Terminal partenza errato                                             14
150300970527TISI12R- Linea di arrivo errata                                               15
150400970527TISI12R- Zona di consegna errata                                              16
150500060206TISI12R- Zona di consegna NON abilitata                                       17
150600970527TISI12R- 18
150700970527TISI12R- 19
150800970527TISI12R- 20
150900970526** FUD - FUNZIONI
151000970526                   -        (S/N)=N                        funz.01¬
151100970526                   -        (S/N)=N                        funz.02¬
151200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
151300970526                   -        (S/N)=N                        funz.04¬
151400970526                   -        (S/N)=N                        funz.05¬
151500970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
151600130905F07=Orari Servizi  -        (S/N)=S                        funz.07¬
151700970527                   -        (S/N)=N                        funz.08¬
151800970527                   -        (S/N)=N                        funz.09¬
151900970527                   -        (S/N)=N                        funz.10¬
152000970526                   -        (S/N)=N                        funz.11¬
152100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
152200970526                   -        (S/N)=N                        funz.13¬
152300970526                   -        (S/N)=N                        funz.14¬
152400970526                   -        (S/N)=N                        funz.15¬
152500970526                   -        (S/N)=N                        funz.16¬
152600970526                   -        (S/N)=N                        funz.17¬
152700970526                   -        (S/N)=N                        funz.18¬
152800970526                   -        (S/N)=N                        funz.19¬
152900970526                   -        (S/N)=N                        funz.20¬
153000970526                   -        (S/N)=N                        funz.21¬
153100970526                   -        (S/N)=N                        funz.22¬
153200970526                   -        (S/N)=N                        funz.23¬
153300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
