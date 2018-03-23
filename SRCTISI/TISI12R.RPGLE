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
003500131007     d* Filiali abilitate  agli orari servizi
003600131007     D FIL_AB          S              3    DIM(85)
003700970526     D*------------------------------------------------------------------------*
003800970526     D* INPUT
003900970526     D*------------------------------------------------------------------------*
004000970526     D*------------
004100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004200970526     D*------------
004300970526     D FUDDS           DS
004400970526     D  FUDDE1                 1      1
004500970526     D  FUDFUN                 2      3
004600970526     D  FUDDE2                 4     19
004700970526     D  FUDFI0                20     34
004800970526     D  FUDATT                35     35
004900970526     D  FUDFI1                36     80
005000970526     D  FUDDES                 1     19
005100970526     D  FUDRIG                 1     80
005200970526     D*------------
005300970526     D* DS GESTIONE TASTI FUNZIONALI
005400970526     D*------------
005500970526     D DSKEY         E DS
005600970526     D DEVDS1          DS
005700970526     D  KEY                  369    369
005800970526     D*------------
005900970526     D* DS "XMSG" - GESTIONE MESSAGGI
006000970526     D*------------
006100970526     D DSMSG         E DS
006200970626     D*------------------
006300970626     D* DS "TISI95R" - CONTROLLO CAP
006400970626     D*------------------
006500970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
006600970527     D*------------------
006700970527     D* DS NAZIONI
006800970527     D*------------------
006900970527     D DS15          E DS
007000970527     D*------------------
007100970527     D* DS ZONE DI CONSEGNA
007200970527     D*------------------
007300970527     D DS03          E DS
007400000613     D*------------
007500000613     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
007600000613     D*------------
007700000613     D OG143         E DS
007800970527     D*------------
007900970527     D* DS "TISI81R" - AGGIORNA UN CAP DAI SUOI ARCHIVI
008000970527     D*------------
008100970527     D DSSI81        E DS                  EXTNAME(TISI81DS)
008200971128     D*------------
008300971128     D* DS REPEIMENTO TERMINALs
008400971128     D*------------
008500971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
008600970526     D*------------
008700970526     D* DS DI PROCEDURA
008800970526     D*------------
008900970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
009000130905     d
009100130905     d tisio0ds      E DS
009200130917
009300130917     d Tisio7ds      e ds
009400971128     D*------------------
009500971128     D* DS "XSRDA8" - CONTROLLA DATA (8)
009600971128     D*------------------
009700971128     D WLBDA8          DS                  INZ
009800971128     D  G08DAT                 1      8  0
009900971128     D  G08INV                 9     16  0
010000971128     D  G08ERR                17     17
010100971128     D  G08TGI                18     22  0
010200970526     D*------------
010300970526     D* ARCHITETTURA
010400970526     D*------------
010500970526     D KPJBA         E DS
010600060206      * - Parametri x Controllo profilo utenti
010700060206     d TIBS34ds      e ds
010800060206      * - Ds di riferimento al file esterno AZUTE00F
010900060206     d AZUTEds       e ds                  extname(AZUTE00F)
011000060206     d dUTE01        e ds
011100131007     d Tibs02Ds      e ds
011200060206      * - Ds per dati organigramma
011300060206     d DDatiUte      e ds
011400130917     d s_cpelna        s                   like(cpelna)  inz
011500130917     d s_cpelos        s                   like(cpelos)  inz
011600130917     d s_cpelol        s                   like(cpelol)  inz
011700130917     d f07lna          s              1                  inz
011800131007     d w003a           s              3
011900131007     d XX              s              3  0
012000131007     d YY              s              3  0
012100131007
012200131007
012300131007      /copy gaitrasrc/srcprotopr,tibs02r
012400131007
012500970526     C*------------------------------------------------------------------------*
012600970526     C* MAIN LINES
012700970526     C*------------------------------------------------------------------------*
012800970526     C*
012900970526     C* NON CI SONO ERRORI IN ENTRATA
013000970526IF  1C     ERRL00        IFEQ      '0'
013100970526     C*
013200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013600970526     C*
013700970526     C* EMETTE FORMATO VIDEO
013800970526     C                   EXSR      BUTFMR
013900970526     C*
014000970526     C* GESTIONE TASTI FUNZIONALI
014100970526     C                   EXSR      FUNCON
014200970526E   2C                   ENDDO
014300970526     C*
014400970526     C* CI SONO ERRORI IN ENTRATA
014500970527X   1C                   ELSE
014600970526     C                   MOVEL     '1'           D00ERR
014700970526     C                   MOVEL     DSMSMS        D00MSG
014800970526E   1C                   ENDIF
014900970626     C*
015000970626     C* OPERAZIONI FINALI
015100970626     C                   EXSR      FINSR
015200970526     C* FINE PGM
015300970526     C                   SETON                                        LR
015400970526     C*------------------------------------------------------------------------*
015500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
015600970526     C*------------------------------------------------------------------------*
015700970526     C     BUTFMR        BEGSR
015800970526     C*
015900970526     C* EMETTE LA TESTATA
016000970527     C                   WRITE     SI12D01
016100970526     C*
016200970526     C* SPEDISCE EVENTUALE MESSAGGIO
016300970526     C                   EXSR      SNDMSG
016400970526     C*
016500970526     C* EMETTE LE FUNZIONI
016600970527     C                   WRITE     SI12D02
016700970526     C*
016800970526     C* EMETTE IL VIDEO
016900970527     C                   EXFMT     SI12D03
017000970526     C*
017100970526     C* REIMPOSTA FLAG DI ERRORE
017200970526     C                   MOVEL     '0'           ERRL00
017300970526     C*
017400970526     C* REIMPOSTA INDICATORI DI ERRORE
017500970526     C                   SETOFF                                         5859
017600970526     C                   SETOFF                                       606162
017700970526     C                   SETOFF                                       636465
017800970526     C                   SETOFF                                       666768
017900970526     C                   SETOFF                                       697071
018000970526     C                   SETOFF                                       727374
018100970526     C                   SETOFF                                       757677
018200970526     C                   SETOFF                                       787980
018300970526     C                   SETOFF                                       818283
018400970526     C                   SETOFF                                       848586
018500970526     C                   SETOFF                                       878889
018600970526     C*
018700970526     C                   ENDSR
018800970526     C*------------------------------------------------------------------------*
018900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
019000970526     C*------------------------------------------------------------------------*
019100970526     C     SNDMSG        BEGSR
019200970526     C*
019300970526     C* PULISCE IL SFILE MESSAGGI
019400970526     C                   MOVEL     '1'           *IN40
019500970527     C                   WRITE     SI12DMC
019600970526     C                   MOVEL     '0'           *IN40
019700970526     C*
019800970526     C* VISUALIZZA IL MESSAGGIO
019900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
020000970526     C                   CALL      'XMSG'        XMSGPA
020100970526IF  1C     DSMSER        IFEQ      *ZEROS
020200970526     C                   MOVEL     DSMSKE        MSGKEY
020300970526E   1C                   ENDIF
020400970526     C                   Z-ADD     1             MSGULT
020500970527     C                   WRITE     SI12DMS
020600970527     C                   WRITE     SI12DMC
020700970526     C*
020800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
020900970526     C                   MOVEL     *BLANKS       DSMSMS
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* ENTGES - GESTIONE ENTER -> INVIO
021400970526     C*------------------------------------------------------------------------*
021500970526     C     ENTGES        BEGSR
021600970526     C*
021700970526     C* CONTROLLA I CAMPI IMMESSI
021800970526     C                   EXSR      DECODI
021900970526     C*
022000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
022100970526IF  1C     DEPOP1        IFEQ      'O05'
022200970526     C                   MOVEL     '1'           FINL00
022300970526E   1C                   ENDIF
022400970526     C*
022500970526     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* F03GES - GESTIONE F03 -> FINE
022800970526     C*------------------------------------------------------------------------*
022900970526     C     F03GES        BEGSR
023000970526     C*
023100970526     C* IMPOSTA "PREMUTO F03"
023200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
023300970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
023400970526     C*
023500970526     C                   ENDSR
023600970526     C*------------------------------------------------------------------------*
023700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
023800970526     C*------------------------------------------------------------------------*
023900970526     C     F06GES        BEGSR
024000970526     C*
024100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
024200970526IF  1C     DEPOP1        IFEQ      'F06'
024300970526     C                   EXSR      INSERI
024400970526E   1C                   ENDIF
024500970526IF  1C     DEPOP1        IFEQ      'O02'
024600970526     C                   EXSR      MODIFI
024700970526E   1C                   ENDIF
024800970526IF  1C     DEPOP1        IFEQ      'O04'
024900970526     C                   EXSR      ANNULL
025000970526E   1C                   ENDIF
025100970526IF  1C     DEPOP1        IFEQ      'O07'
025200970526     C                   EXSR      RIPRIS
025300970526E   1C                   ENDIF
025400970526     C*
025500970526     C                   ENDSR
025600130905     C*------------------------------------------------------------------------*
025700130905     C* F07GES - GESTIONE F07 -> Orari servizi
025800130905     C*------------------------------------------------------------------------*
025900130905     C     F07GES        BEGSR
026000130905     C*
026100130905     c                   exsr      F06ges
026200130905
026300130905     c                   if        errl00='0'
026400130905
026500130905     c                   clear                   tisio0ds
026600130905     C                   MOVEL     VIDNAR        do0NAR
026700130905     C                   MOVEL     VIDCAP        Do0CAP
026800130905     c                   if        %subst(depop0:2:2)='02'
026900130905     C                   MOVEL     'G'           do0tric
027000130905     c                   else
027100130905     C                   MOVEL     'I'           do0tric
027200130905     c                   endif
027300130905     c                   movel     d00dri        do0dri
027400130905     c                   movel     d00ver        do0ver
027500130905
027600130905     C                   call      'TISIO2R'
027700130905     C                   PARM                    KPJBA
027800130905     C                   PARM                    tisio0ds
027900130905     C*
028000130905     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
028100130905IF  1C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
028200130905     C                   MOVEL     '1'           ERRL00                         *ERRORE
028300130905     C                   MOVEL     '1'           F03L00                         *FINE PGM
028400130905X   1C                   ELSE                                                   *NON PREMUTO F03
028500130905     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
028600130905E   1C                   ENDIF
028700130905     C*
028800130905E   1c                   ENDIF
028900130905     C*
029000130905     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200970526     C* F12GES - GESTIONE F12 -> RITORNO
029300970526     C*------------------------------------------------------------------------*
029400970526     C     F12GES        BEGSR
029500970526     C*
029600970526     C* IMPOSTA "PREMUTO F12"
029700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
029800970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
029900970526     C*
030000970526     C                   ENDSR
030100970526     C*------------------------------------------------------------------------*
030200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
030300970526     C*------------------------------------------------------------------------*
030400970526     C     F24GES        BEGSR
030500970526     C*
030600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
030700970526     C*
030800970526     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
031100970526     C*------------------------------------------------------------------------*
031200970526     C     INSERI        BEGSR
031300970526     C*
031400970526     C* CONTROLLA I CAMPI IMMESSI
031500970526     C                   EXSR      DECODI
031600970526     C*
031700970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
031800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031900970527     C                   Z-ADD     D00VER        CPEVER                         *CHIAVE
032000970527     C                   MOVEL     VIDNAR        CPENAR
032100970527     C                   MOVEL     VIDCAP        CPECAP
032200970527     C                   MOVEL     VIDTFP        CPETFP
032300970527     C                   EXSR      IMPCPE                                       *ALTRI CAMPI
032400970527     C                   Z-ADD     *ZEROS        CPEDTR                         *DATA TRASMISSIONE
032500970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
032600970527     C                   MOVEL     *BLANKS       CPEATB                         *ANNULLAMENTO
032700970527     C                   WRITE     AZCPE000
032800970527     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
032900970527     C*
033000970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
033100970527     C                   Z-ADD     D00VER        D81VER
033200970527     C                   MOVEL     VIDNAR        D81NAR
033300970527     C                   MOVEL     VIDCAP        D81CAP
033400970527     C                   MOVEL     'E'           D81CHK
033500970527     C                   CALL      'TISI81R'
033600970527     C                   PARM                    DSSI81
033700970602     C*
033800970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
033900970602     C                   EXSR      AZZVID
034000970526E   1C                   ENDIF
034100970526     C*
034200970526     C                   ENDSR
034300970526     C*------------------------------------------------------------------------*
034400970526     C* MODIFI - OPERAZIONI DI MODIFICA
034500970526     C*------------------------------------------------------------------------*
034600970526     C     MODIFI        BEGSR
034700970526     C*
034800970526     C* CONTROLLA I CAMPI IMMESSI
034900970526     C                   EXSR      DECODI
035000970526     C*
035100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
035200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035300970527     C                   EXSR      LETCPE
035400970526     C*
035500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
035600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035700130917
035800130917     c* richiamo pgm per cancellare OrariServizi a fronte delle lna cambiate
035900130917     c                   exsr      sr_oraris
036000130917
036100970527     C                   EXSR      IMPCPE                                       *IMPOSTA I CAMPI
036200970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
036300970527     C                   MOVEL     *BLANKS       CPEATB                         *ANNULLAMENTO
036400970527     C                   UPDATE    AZCPE000
036500970526     C                   MOVEL     MSG(3)        D00MSG
036600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036700000601     C*
036800970526E   2C                   ENDIF
036900970526E   1C                   ENDIF
037000970526     C*
037100970526     C                   ENDSR
037200970526     C*------------------------------------------------------------------------*
037300970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
037400970526     C*------------------------------------------------------------------------*
037500970526     C     ANNULL        BEGSR
037600970526     C*
037700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
037800000602     C                   MOVEL     '0'           ANNDLT            1
037900970527     C                   EXSR      LETCPE
038000970527     C*
038100970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
038200970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038300130917
038400130917     c* richiamo pgm per annullamento eventuali orariServizi
038500130917     c                   exsr      sr_oraris
038600130917
038700970527IF  2C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
038800970527IF  3C     CPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
038900970527     C                   MOVEL     'A'           CPEATB                         *FLAG ANNULLAMENTO
039000970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
039100970527     C                   UPDATE    AZCPE000                                     *ANNULLA
039200970526X   3C                   ELSE                                                   *MAI TRASMESSO
039300970527     C                   DELETE    AZCPE000                                     *CANCELLA
039400000602     C                   MOVEL     '1'           ANNDLT
039500970526E   3C                   ENDIF
039600970526E   2C                   ENDIF
039700970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
039800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
039900970527     C*
040000970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
040100970527     C                   Z-ADD     D00VER        D81VER
040200970527     C                   MOVEL     VIDNAR        D81NAR
040300970527     C                   MOVEL     VIDCAP        D81CAP
040400970527     C                   MOVEL     'E'           D81CHK
040500970527     C                   CALL      'TISI81R'
040600970527     C                   PARM                    DSSI81
040700970526E   1C                   ENDIF
040800970526     C*
040900970526     C                   ENDSR
041000970526     C*------------------------------------------------------------------------*
041100970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
041200970526     C*------------------------------------------------------------------------*
041300970526     C     RIPRIS        BEGSR
041400970526     C*
041500970526     C* CONTROLLA I CAMPI IMMESSI
041600970526     C                   EXSR      DECODI
041700970526     C*
041800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
041900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042000970527     C                   EXSR      LETCPE
042100970526     C*
042200970526     C* IMPOSTA IL RECORD DA SCRIVERE
042300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042400970527     C                   MOVEL     *BLANKS       CPEATB                         *FLAG ANNULLAMENTO
042500970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
042600970527     C                   UPDATE    AZCPE000                                     *ANNULLA
042700970526E   2C                   ENDIF
042800970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
042900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
043000970527     C*
043100970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
043200970527     C                   Z-ADD     D00VER        D81VER
043300970527     C                   MOVEL     VIDNAR        D81NAR
043400970527     C                   MOVEL     VIDCAP        D81CAP
043500970527     C                   MOVEL     'E'           D81CHK
043600970530     C                   CALL      'TISI81R'
043700970530     C                   PARM                    DSSI81
043800970526E   1C                   ENDIF
043900970526     C*
044000970526     C                   ENDSR
044100970526     C*------------------------------------------------------------------------*
044200970527     C* LETCPE - LEGGE IL RECORD PERL'AGGIORNAMENTO
044300970526     C*------------------------------------------------------------------------*
044400970527     C     LETCPE        BEGSR
044500970526     C*
044600000601     C     DEPOP1        IFNE      'F06'
044700970527     C                   Z-ADD     D00VER        KCEVER
044800980127     C                   MOVEL     D00NAR        KCENAR
044900980127     C                   MOVEL     D00CAP        KCECAP
045000980127     C                   MOVEL     D00TFP        KCETFP
045100970527     C     KEYCPE        CHAIN     AZCPE01L                           9998
045200970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
045300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
045400970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
045500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
045600970526E   1C                   ENDIF
045700970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
045800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
045900970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
046000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
046100970526E   1C                   ENDIF
046200000601     C                   ENDIF
046300970526     C*
046400970526     C                   ENDSR
046500970526     C*------------------------------------------------------------------------*
046600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
046700970526     C*------------------------------------------------------------------------*
046800970526     C     DECODI        BEGSR
046900970527     C*--------------------
047000970527     C* NAZIONE
047100970527     C*--------------------
047200970527     C     *LIKE         DEFINE    §15CPR        DEPCPR
047300970527IF  1C     ERRL00        IFEQ      '0'
047400970527     C                   MOVEL     *BLANKS       VDDNAR
047500970527     C                   MOVEL     VIDNAR        WNAR
047600970527     C                   MOVEL     *BLANKS       WDNAR
047700970527     C                   MOVEL     *BLANKS       WCPR
047800970527     C                   EXSR      CHKNAR
047900970527     C                   MOVEL     WNAR          VIDNAR
048000970527     C                   MOVEL     WDNAR         VDDNAR
048100970527     C                   MOVEL     WCPR          DEPCPR
048200970527IF  2C     ERRL00        IFEQ      '1'
048300970527     C                   SETON                                        60
048400970527     C                   MOVEL     MSG(11)       DSMSMS
048500970527E   2C                   ENDIF
048600970527E   1C                   ENDIF
048700970527     C*--------------------
048800970527     C* CAP
048900970527     C*--------------------
049000970527IF  1C     ERRL00        IFEQ      '0'
049100970527     C                   MOVEL     VIDNAR        WNAR
049200970527     C                   MOVEL     VIDCAP        WCAP
049300970527     C                   EXSR      CHKCAP
049400970527IF  2C     ERRL00        IFEQ      '1'
049500970527     C                   SETON                                        61
049600970527     C                   MOVEL     MSG(12)       DSMSMS
049700970527E   2C                   ENDIF
049800970527E   1C                   ENDIF
049900970626     C* CARICA DATI CAP
050000970527IF  1C     ERRL00        IFEQ      '0'
050100970626     C                   Z-ADD     O95CLA        VIDCLN
050200970626     C                   Z-ADD     O95CZC        VIDCZN
050300970626     C                   Z-ADD     O95CLO        VIDCLO
050400970626     C                   Z-ADD     O95CZO        VIDCZO
050500970626     C                   Z-ADD     O95CLK        VIDCKG
050600970626     C                   Z-ADD     O95CLM        VIDCMC
050700970626     C                   Z-ADD     O95CLS        VIDCLS
050800970626     C                   Z-ADD     O95CZS        VIDCZS
050900970626     C                   Z-ADD     O95CKS        VIDCKS
051000970626     C                   Z-ADD     O95CMS        VIDCMS
051100971118     C                   Z-ADD     O95CL2        VIDCL2
051200971118     C                   Z-ADD     O95CZ2        VIDCZ2
051300971118     C                   Z-ADD     O95CK2        VIDCK2
051400971118     C                   Z-ADD     O95CM2        VIDCM2
051500970527E   1C                   ENDIF
051600970527     C*--------------------
051700970527     C* TERMINAL PARTENZA
051800970527     C*--------------------
051900970527IF  1C     ERRL00        IFEQ      '0'
052000970527     C                   MOVEL     VIDTFP        WTFP
052100970527     C                   MOVEL     *BLANKS       WDTFP
052200970528     C                   EXSR      CHKTFX
052300970527     C                   MOVEL     WTFP          VIDTFP
052400970527     C                   MOVEL     WDTFP         VDDTFP
052500970527IF  2C     ERRL00        IFEQ      '1'
052600970527     C                   SETON                                        62
052700970527     C                   MOVEL     MSG(14)       DSMSMS
052800970527E   2C                   ENDIF
052900970527E   1C                   ENDIF
053000970527     C*--------------------
053100970527     C* NAZIONE + CAP + TERMINAL PARTENZA
053200970527     C*--------------------
053300970527     C* INSERIMENTO -> RECORD INESISTENTE
053400970527IF  1C     ERRL00        IFEQ      '0'
053500970527IF  2C     DEPOP1        IFEQ      'F06'
053600970527     C                   Z-ADD     D00VER        KCEVER
053700970527     C                   MOVEL     VIDNAR        KCENAR
053800970527     C                   MOVEL     VIDCAP        KCECAP
053900970527     C                   MOVEL     VIDTFP        KCETFP
054000970527     C     KEYCPE        CHAIN(N)  AZCPE01L                           99
054100970527IF  3C     *IN99         IFEQ      *OFF
054200970527     C                   SETON                                        606162
054300970527     C                   MOVEL     '1'           ERRL00
054400970527     C                   MOVEL     MSG(8)        DSMSMS
054500970527E   3C                   ENDIF
054600970527E   2C                   ENDIF
054700970527E   1C                   ENDIF
054800970527     C*--------------------
054900970606     C* LINEA ARRIVO STANDARD
055000970527     C*--------------------
055100970527IF  1C     ERRL00        IFEQ      '0'
055200970527     C                   MOVEL     VIDLNA        WLIN
055300970527     C                   MOVEL     *BLANKS       WDLIN
055400970527     C                   EXSR      CHKLIN
055500970527     C                   MOVEL     WLIN          VIDLNA
055600970527     C                   MOVEL     WDLIN         VDDLNA
055700970527IF  2C     ERRL00        IFEQ      '1'
055800970527     C                   SETON                                        63
055900970527     C                   MOVEL     MSG(15)       DSMSMS
056000970527E   2C                   ENDIF
056100970527E   1C                   ENDIF
056200130917IF  1C     ERRL00        IFEQ      '0'
056300130917     c     depop1        andne     'F06'
056400130917     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
056500130917     c                   movel     vidlna        w0030             3 0
056600130917     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
056700130917     c* e non ho già avvisato avvisato della cancellazione orariservizi
056800130917    2c                   if        w0030<>cpelna and cpelna<>s_cpelna
056900130917     c                                           and cpelna<>s_cpelos
057000130917     c                                           and cpelna<>s_cpelol
057100130917     c                                           and f07lna=*blanks
057200130917     c                   clear                   tisio7ds
057300130917     c                   eval      io7mod='C'
057400130917     c                   eval      io7liv='E'
057500130917     c                   eval      io7ver=d00ver
057600130917     c                   eval      io7nar=vidnar
057700130917     c                   eval      io7cap=vidcap
057800130917     c                   eval      io7lna=cpelna
057900130917     c                   eval      io7tfp=cpetfp
058000130917     c                   eval      io7lns=%dec(vidlna:3:0)
058100130917     c                   eval      io7los=%dec(vidlos:3:0)
058200130917     c                   eval      io7lol=%dec(vidlol:3:0)
058300131016     c*****              movel     tisio7ds      kpjbu
058400130917     c                   call      'TISIO7R'
058500130917     c                   parm                    kpjba
058600131016     c                   parm                    tisio7ds
058700130917    3c                   if        oo7err='1'
058800130917     c                   movel     oo7msg        DSmsms
058900130917     c                   eval      *in63  = *on
059000130917     c                   eval      ErrL00 = *on
059100130917     c                   eval      f07lna='S'
059200130917e   3c                   endif
059300130917
059400130917     c                   eval      s_cpelna=cpelna
059500130917e   2c                   endif
059600130917E   1C                   ENDIF
059700970527     C*--------------------
059800970606     C* ZONA CONSEGNA STANDARD
059900970527     C*--------------------
060000970527IF  1C     ERRL00        IFEQ      '0'
060100970527     C                   MOVEL     VIDLNA        WLIN
060200970527     C                   Z-ADD     VIDZNC        WZNC
060300970527     C                   EXSR      CHKZNC
060400970527IF  2C     ERRL00        IFEQ      '1'
060500970527     C                   SETON                                        64
060600970527     C                   MOVEL     MSG(16)       DSMSMS
060700970527E   2C                   ENDIF
060800060206if  2c                   if        §03nuc = 'S'
060900060206     c                   movel     Msg(17)       DSmsms
061000060206     c                   eval      *in64  = *on
061100060206     c                   eval      ErrL00 = *on
061200060206e   2c                   endif
061300970527E   1C                   ENDIF
061400970606     C*--------------------
061500970912     C* LINEA ARRIVO SOTTO
061600970606     C*--------------------
061700970606IF  1C     ERRL00        IFEQ      '0'
061800970606     C                   MOVEL     *BLANKS       VDDLOS
061900970606IF  2C     VIDLOS        IFNE      *ZEROS
062000970606     C     VIDLOS        ANDNE     *BLANKS
062100970606     C                   MOVEL     VIDLOS        WLIN
062200970606     C                   MOVEL     *BLANKS       WDLIN
062300970606     C                   EXSR      CHKLIN
062400970606     C                   MOVEL     WLIN          VIDLOS
062500970606     C                   MOVEL     WDLIN         VDDLOS
062600970606IF  3C     ERRL00        IFEQ      '1'
062700970606     C                   SETON                                        67
062800970606     C                   MOVEL     MSG(15)       DSMSMS
062900970606E   3C                   ENDIF
063000970606E   2C                   ENDIF
063100970606E   1C                   ENDIF
063200130917IF  1C     ERRL00        IFEQ      '0'
063300130917     c     depop1        andne     'F06'
063400130917     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
063500130917     c                   movel     vidlos        w0030             3 0
063600130917     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
063700130917     c* e non ho già avvisato avvisato della cancellazione orariservizi
063800130917    2c                   if        w0030<>cpelos and cpelos<>s_cpelna
063900130917     c                                           and cpelos<>s_cpelos
064000130917     c                                           and cpelos<>s_cpelol
064100130917     c                                           and f07lna=*blanks
064200130917     c                   clear                   tisio7ds
064300130917     c                   eval      io7mod='C'
064400130917     c                   eval      io7liv='E'
064500130917     c                   eval      io7ver=d00ver
064600130917     c                   eval      io7nar=vidnar
064700130917     c                   eval      io7cap=vidcap
064800130917     c                   eval      io7lna=cpelos
064900130917     c                   eval      io7tfp=cpetfp
065000130917     c                   eval      io7lns=%dec(vidlna:3:0)
065100130917     c                   eval      io7los=%dec(vidlos:3:0)
065200130917     c                   eval      io7lol=%dec(vidlol:3:0)
065300131016     c*****              movel     tisio7ds      kpjbu
065400130917     c                   call      'TISIO7R'
065500130917     c                   parm                    kpjba
065600131016     c                   parm                    tisio7ds
065700130917    3c                   if        oo7err='1'
065800130917     c                   movel     oo7msg        DSmsms
065900130917     c                   eval      *in67  = *on
066000130917     c                   eval      ErrL00 = *on
066100130917     c                   eval      f07lna='S'
066200130917e   3c                   endif
066300130917
066400130917     c                   eval      s_cpelos=cpelos
066500130917e   2c                   endif
066600130917E   1C                   ENDIF
066700970606     C*--------------------
066800970912     C* ZONA DI CONSEGNA SOTTO
066900970606     C*--------------------
067000970606IF  1C     ERRL00        IFEQ      '0'
067100971126IF  2C     VIDLOS        IFGT      *ZEROS
067200970606     C                   MOVEL     VIDLOS        WLIN
067300970606     C                   Z-ADD     VIDZOS        WZNC
067400970606     C                   EXSR      CHKZNC
067500970606IF  3C     ERRL00        IFEQ      '1'
067600970606     C                   SETON                                        68
067700970606     C                   MOVEL     MSG(16)       DSMSMS
067800970606E   3C                   ENDIF
067900060206if  3c                   if        §03nuc = 'S'
068000060206     c                   movel     Msg(17)       DSmsms
068100060206     c                   eval      *in68  = *on
068200060206     c                   eval      ErrL00 = *on
068300060206e   3c                   endif
068400970606E   2C                   ENDIF
068500970606E   1C                   ENDIF
068600970527     C*--------------------
068700970912     C* LINEA ARRIVO OLTRE
068800970527     C*--------------------
068900970527IF  1C     ERRL00        IFEQ      '0'
069000971126IF  2C     VIDLOL        IFNE      *ZEROS
069100971126     C     VIDLOL        ANDNE     *BLANKS
069200970527     C                   MOVEL     VIDLOL        WLIN
069300970527     C                   MOVEL     *BLANKS       WDLIN
069400970527     C                   EXSR      CHKLIN
069500970527     C                   MOVEL     WLIN          VIDLOL
069600970527     C                   MOVEL     WDLIN         VDDLOL
069700971126IF  3C     ERRL00        IFEQ      '1'
069800970527     C                   SETON                                        65
069900970527     C                   MOVEL     MSG(15)       DSMSMS
070000971126E   3C                   ENDIF
070100971126E   2C                   ENDIF
070200970527E   1C                   ENDIF
070300130917IF  1C     ERRL00        IFEQ      '0'
070400130917     c     depop1        andne     'F06'
070500130917     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
070600130917     c                   movel     vidlol        w0030             3 0
070700130917     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
070800130917     c* e non ho già avvisato avvisato della cancellazione orariservizi
070900130917    2c                   if        w0030<>cpelol and cpelol<>s_cpelna
071000130917     c                                           and cpelol<>s_cpelos
071100130917     c                                           and cpelol<>s_cpelol
071200130917     c                                           and f07lna=*blanks
071300130917     c                   clear                   tisio7ds
071400130917     c                   eval      io7mod='C'
071500130917     c                   eval      io7liv='E'
071600130917     c                   eval      io7ver=d00ver
071700130917     c                   eval      io7nar=vidnar
071800130917     c                   eval      io7cap=vidcap
071900130917     c                   eval      io7lna=cpelol
072000130917     c                   eval      io7tfp=cpetfp
072100130917     c                   eval      io7lns=%dec(vidlna:3:0)
072200130917     c                   eval      io7los=%dec(vidlos:3:0)
072300130917     c                   eval      io7lol=%dec(vidlol:3:0)
072400131016     c****               movel     tisio7ds      kpjbu
072500130917     c                   call      'TISIO7R'
072600130917     c                   parm                    kpjba
072700131016     c                   parm                    tisio7ds
072800130917    3c                   if        oo7err='1'
072900130917     c                   movel     oo7msg        DSmsms
073000130917     c                   eval      *in65  = *on
073100130917     c                   eval      ErrL00 = *on
073200130917     c                   eval      f07lna='S'
073300130917e   3c                   endif
073400130917
073500130917     c                   eval      s_cpelol=cpelol
073600130917e   2c                   endif
073700130917E   1C                   ENDIF
073800970527     C*--------------------
073900970912     C* ZONA CONSEGNA OLTRE
074000970527     C*--------------------
074100970527IF  1C     ERRL00        IFEQ      '0'
074200971126IF  2C     VIDLOL        IFGT      *ZEROS
074300970527     C                   MOVEL     VIDLOL        WLIN
074400970527     C                   Z-ADD     VIDZOL        WZNC
074500970527     C                   EXSR      CHKZNC
074600971126IF  3C     ERRL00        IFEQ      '1'
074700970527     C                   SETON                                        66
074800970527     C                   MOVEL     MSG(16)       DSMSMS
074900971126E   3C                   ENDIF
075000060206if  3c                   if        §03nuc = 'S'
075100060206     c                   movel     Msg(17)       DSmsms
075200060206     c                   eval      *in66  = *on
075300060206     c                   eval      ErrL00 = *on
075400060206e   3c                   endif
075500971126E   2C                   ENDIF
075600970527E   1C                   ENDIF
075700971118     C*--------------------
075800971118     C* LINEA ARRIVO OLTRE 2
075900971118     C*--------------------
076000971118IF  1C     ERRL00        IFEQ      '0'
076100971126IF  2C     VIDLO2        IFNE      *ZEROS
076200971126     C     VIDLO2        ANDNE     *BLANKS
076300971118     C                   MOVEL     VIDLO2        WLIN
076400971118     C                   MOVEL     *BLANKS       WDLIN
076500971118     C                   EXSR      CHKLIN
076600971118     C                   MOVEL     WLIN          VIDLO2
076700971118     C                   MOVEL     WDLIN         VDDLO2
076800971126IF  3C     ERRL00        IFEQ      '1'
076900971118     C                   SETON                                        69
077000971118     C                   MOVEL     MSG(15)       DSMSMS
077100971126E   3C                   ENDIF
077200971126E   2C                   ENDIF
077300971118E   1C                   ENDIF
077400971118     C*--------------------
077500971118     C* ZONA CONSEGNA OLTRE
077600971118     C*--------------------
077700971118IF  1C     ERRL00        IFEQ      '0'
077800971126IF  2C     VIDLO2        IFGT      *ZEROS
077900971118     C                   MOVEL     VIDLO2        WLIN
078000971118     C                   Z-ADD     VIDZO2        WZNC
078100971118     C                   EXSR      CHKZNC
078200971126IF  3C     ERRL00        IFEQ      '1'
078300971118     C                   SETON                                        70
078400971118     C                   MOVEL     MSG(16)       DSMSMS
078500971126E   3C                   ENDIF
078600060206if  2c                   if        §03nuc = 'S'
078700060206     c                   movel     Msg(17)       DSmsms
078800060206     c                   eval      *in70  = *on
078900060206     c                   eval      ErrL00 = *on
079000060206e   2c                   endif
079100971126E   2C                   ENDIF
079200971118E   1C                   ENDIF
079300970527     C*--------------------
079400970527     C* DECODIFICHE DATI CAPPARIO
079500970527     C*--------------------
079600970527     C* LINEA ARRIVO
079700970527IF  1C     ERRL00        IFEQ      '0'
079800970606IF  2C     VIDCLN        IFGT      *ZEROS
079900970527     C                   MOVEL     VIDCLN        WLIN
080000970527     C                   MOVEL     *BLANKS       WDLIN
080100970527     C                   EXSR      CHKLIN
080200970527     C                   MOVEL     WDLIN         VDDCLN
080300970606E   2C                   ENDIF
080400970606E   1C                   ENDIF
080500970912     C* LINEA ARRIVO SOTTO
080600970606IF  1C     ERRL00        IFEQ      '0'
080700970606IF  2C     VIDCLS        IFGT      *ZEROS
080800970606     C                   MOVEL     VIDCLS        WLIN
080900970606     C                   MOVEL     *BLANKS       WDLIN
081000970606     C                   EXSR      CHKLIN
081100970606     C                   MOVEL     WDLIN         VDDCLS
081200970606E   2C                   ENDIF
081300970606E   1C                   ENDIF
081400970912     C* LINEA ARRIVO OLTRE
081500970606IF  1C     ERRL00        IFEQ      '0'
081600970606IF  2C     VIDCLO        IFGT      *ZEROS
081700970527     C                   MOVEL     VIDCLO        WLIN
081800970527     C                   MOVEL     *BLANKS       WDLIN
081900970527     C                   EXSR      CHKLIN
082000970527     C                   MOVEL     WDLIN         VDDCLO
082100970606E   2C                   ENDIF
082200970606E   1C                   ENDIF
082300971118     C* LINEA ARRIVO OLTRE 2
082400971118IF  1C     ERRL00        IFEQ      '0'
082500971118IF  2C     VIDCL2        IFGT      *ZEROS
082600971118     C                   MOVEL     VIDCL2        WLIN
082700971118     C                   MOVEL     *BLANKS       WDLIN
082800971118     C                   EXSR      CHKLIN
082900971118     C                   MOVEL     WDLIN         VDDCL2
083000971118E   2C                   ENDIF
083100971118E   1C                   ENDIF
083200970527     C*
083300970527     C                   ENDSR
083400970527     C*------------------------------------------------------------------------*
083500970527     C* CHKNAR - CONTROLLA LA NAZIONE
083600970527     C*------------------------------------------------------------------------*
083700970527     C     CHKNAR        BEGSR
083800970527     C*
083900970527     C     *LIKE         DEFINE    VIDNAR        WNAR
084000970527     C     *LIKE         DEFINE    VDDNAR        WDNAR
084100970527     C     *LIKE         DEFINE    §15CPR        WCPR
084200970527     C*
084300970527     C* RICHIESTO ELENCO
084400970527     C                   SETOFF                                           28
084500970527     C     '?'           SCAN      WNAR                                   28
084600970527IF  1C     *IN28         IFEQ      *ON
084700020906     C                   Z-ADD     1             §KUT              1 0
084800970527     C                   MOVEL     '15'          §COD              2
084900970527     C                   MOVEL     *BLANKS       §KEY              8
085000970527     C                   MOVEL     *BLANKS       §DES             30
085100970527     C                   CALL      'X§TABER'
085200970527     C                   PARM                    §KUT
085300970527     C                   PARM                    §COD
085400970527     C                   PARM                    §KEY
085500970527     C                   PARM                    §DES
085600970527     C                   MOVEL     §KEY          WNAR
085700970527     C                   MOVEL     §DES          WDNAR
085800970527E   1C                   ENDIF
085900970527     C*
086000970527     C* CONTROLLA ESISTENZA CODICE
086100020906     C                   MOVEL     1             KTBKUT
086200970527     C                   MOVEL     '15'          KTBCOD
086300970527     C                   MOVEL(P)  WNAR          KTBKEY
086400970527     C     KEYTAB        CHAIN     TABEL00F                           99
086500970527IF  1C     *IN99         IFEQ      *OFF
086600970527     C     TBLFLG        ANDEQ     *BLANKS
086700970527     C                   MOVEL     TBLUNI        DS15
086800970527     C                   MOVEL     §15DES        WDNAR
086900970527     C                   MOVEL     §15CPR        WCPR
087000970527     C*
087100970527     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
087200970527IF  2C     VIDNAR        IFEQ      'I  '
087300970527     C                   MOVEL     '1'           ERRL00
087400970527     C                   MOVEL     *BLANKS       WDNAR
087500970527E   2C                   ENDIF
087600970527X   1C                   ELSE
087700970527     C                   MOVEL     '1'           ERRL00
087800970527E   1C                   ENDIF
087900970527     C*
088000970527     C                   ENDSR
088100970527     C*------------------------------------------------------------------------*
088200970527     C* CHKCAP - CONTROLLA IL CAP
088300970527     C*------------------------------------------------------------------------*
088400970527     C     CHKCAP        BEGSR
088500970527     C*
088600970527     C     *LIKE         DEFINE    VIDCAP        WCAP
088700970626     C*
088800970626     C* CONTROLLA IL CAP
088900970626     C                   CLEAR                   DSSI95
089000970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
089100970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
089200970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
089300970626     C                   MOVEL     WCAP          I95CAP                         *CAP
089400970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
089500970626     C                   CALL      'TISI95R'
089600970626     C                   PARM                    DSSI95
089700970626IF  1C     O95ERR        IFNE      *BLANKS
089800970626     C                   MOVEL     '1'           ERRL00
089900970626E   1C                   ENDIF
090000970527     C*
090100970527     C                   ENDSR
090200970527     C*------------------------------------------------------------------------*
090300970528     C* CHKTFX - CONTROLLA IL TERMINAL
090400970527     C*------------------------------------------------------------------------*
090500970528     C     CHKTFX        BEGSR
090600970527     C*
090700970527     C     *LIKE         DEFINE    VIDTFP        WTFP
090800970527     C     *LIKE         DEFINE    VDDTFP        WDTFP
090900970527     C*
091000970527     C* RICHIESTO ELENCO
091100970527     C                   SETOFF                                           28
091200970527     C     '?'           SCAN      WTFP                                   28
091300970527IF  1C     *IN28         IFEQ      *ON
091400970527     C                   MOVEL     *BLANKS       §COD1             3
091500970527     C                   MOVEL     *BLANKS       §TIP              1
091600970527     C                   MOVEL     *BLANKS       §DES1            25
091700971203     C                   MOVEL     *BLANKS       §PRA              3
091800971203     C                   CALL      'TNSD24R'
091900970527     C                   PARM                    §COD1
092000970527     C                   PARM                    §TIP
092100970527     C                   PARM                    §DES1
092200971203     C                   PARM                    §PRA
092300970527     C                   MOVEL     §COD1         WTFP
092400970527     C                   MOVEL     §DES1         WDTFP
092500970527E   1C                   ENDIF
092600970527     C*
092700970527     C* SOLO CARATTERI NUMERICI
092800970527     C                   SETOFF                                       28
092900970527     C                   TESTN                   WTFP                 28
093000970527IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
093100970527     C                   MOVEL     '1'           ERRL00
093200970527X   1C                   ELSE                                                   *E'NUMERO DA TESTN
093300970527     C                   MOVE      WTFP          W001              1
093400970527IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
093500970527     C                   MOVEL     '1'           ERRL00
093600970527X   2C                   ELSE                                                   *NUMERO
093700971128     C*
093800971128     C* REPERISCE IL TERMINAL
093900971128     C                   CLEAR                   DSLV55
094000971128     C                   MOVEL     'P'           D55TPT
094100971128     C                   MOVEL     WTFP          D55LIN
094200971128     C                   Z-ADD     DATCOR        D55DRF
094300971128     C                   CALL      'FNLV55R'
094400971128     C                   PARM                    DSLV55
094500971128IF  3C     D55ERR        IFNE      *BLANKS
094600971128     C                   MOVEL     WTFP          D55TFP
094700971128E   3C                   ENDIF
094800970527     C*
094900970527     C* CONTROLLA ESISTENZA CODICE
095000000613     C                   CLEAR                   OG143
095100970527     C                   MOVEL     WTFP          KORFIL
095200970527     C     KEYORG        CHAIN     AZORG01L                           99
095300000613IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
095400000613     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
095500000613     C     ORGFAG        ANDNE     'V'                                          *NON VARIA
095600000613     C                   MOVEL     ORGDE3        OG143
095700000613IF  4C     ORGFIL        IFEQ      D55TFP                                       *TERMINAL PARTENZA
095800020627     C     §OGNTW        OREQ      'PPT'                                        *FILIALE POSTE
095900000613     C                   MOVEL     ORGDES        WDTFP                          *DESCRIZIONE
096000000613X   4C                   ELSE
096100000613     C                   MOVEL     '1'           ERRL00                         *ERRORE
096200000613E   3C                   ENDIF
096300970527X   3C                   ELSE
096400000613     C                   MOVEL     '1'           ERRL00                         *ERRORE
096500970527E   3C                   ENDIF
096600970527E   2C                   ENDIF
096700970527E   1C                   ENDIF
096800970527     C*
096900970527     C                   ENDSR
097000970527     C*------------------------------------------------------------------------*
097100970527     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
097200970527     C*------------------------------------------------------------------------*
097300970527     C     CHKLIN        BEGSR
097400970527     C*
097500970527     C     *LIKE         DEFINE    VIDLNA        WLIN
097600970527     C     *LIKE         DEFINE    VDDLNA        WDLIN
097700970527     C*
097800970527     C* RICHIESTO ELENCO
097900970527     C                   SETOFF                                           28
098000970527     C     '?'           SCAN      WLIN                                   28
098100970527IF  1C     *IN28         IFEQ      *ON
098200970527     C                   MOVEL     *BLANKS       §COD1             3
098300970527     C                   MOVEL     *BLANKS       §TIP              1
098400970527     C                   MOVEL     *BLANKS       §DES1            25
098500971203     C                   MOVEL     *BLANKS       §PRA              3
098600980409     C                   CALL      'TNSD24R'
098700970527     C                   PARM                    §COD1
098800970527     C                   PARM                    §TIP
098900970527     C                   PARM                    §DES1
099000971203     C                   PARM                    §PRA
099100970527     C                   MOVEL     §COD1         WLIN
099200970527     C                   MOVEL     §DES1         WDLIN
099300970527E   1C                   ENDIF
099400970527     C*
099500970527     C* SOLO CARATTERI NUMERICI
099600970527     C                   SETOFF                                       28
099700970527     C                   TESTN                   WLIN                 28
099800970527IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
099900970527     C                   MOVEL     '1'           ERRL00
100000970527X   1C                   ELSE                                                   *E'NUMERO DA TESTN
100100970527     C                   MOVE      WLIN          W001              1
100200970527IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
100300970527     C                   MOVEL     '1'           ERRL00
100400970527X   2C                   ELSE                                                   *NUMERO
100500970527     C*
100600970527     C* CONTROLLA ESISTENZA CODICE
100700970527     C                   MOVEL     WLIN          KORFIL
100800970527     C     KEYORG        CHAIN     AZORG01L                           99
100900970527IF  3C     *IN99         IFEQ      *ON                                          *NON TROVATA
101000970527     C     ORGFVA        ORNE      *BLANKS                                      *ANNULLATA
101100970527     C     ORGFAG        OREQ      'V'                                          *VARIA
101200970527     C                   MOVEL     '1'           ERRL00
101300970527X   3C                   ELSE
101400970527     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
101500970527E   3C                   ENDIF
101600970527E   2C                   ENDIF
101700970527E   1C                   ENDIF
101800970527     C*
101900970527     C                   ENDSR
102000970527     C*------------------------------------------------------------------------*
102100970527     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
102200970527     C*------------------------------------------------------------------------*
102300970527     C     CHKZNC        BEGSR
102400970527     C*
102500970527     C     *LIKE         DEFINE    VIDZNC        WZNC
102600970527     C*
102700970527     C* CONTROLLA ESISTENZA CODICE
102800020906     C                   Z-ADD     1             KTBKUT
102900970527     C                   MOVEL     '03'          KTBCOD
103000970527     C                   MOVEL     WLIN          WA5               5
103100970527     C                   MOVE      WZNC          WA5
103200970527     C                   MOVEL(P)  WA5           KTBKEY
103300970527     C     KEYTAB        CHAIN     TABEL00F                           99
103400970527IF  1C     *IN99         IFEQ      *OFF
103500970527     C     TBLFLG        ANDEQ     *BLANKS
103600970527     C                   MOVEL     TBLUNI        DS03
103700970527X   1C                   ELSE
103800970527     C                   MOVEL     '1'           ERRL00
103900970527E   1C                   ENDIF
104000970527     C*
104100970527     C                   ENDSR
104200970526     C*------------------------------------------------------------------------*
104300970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
104400970526     C*------------------------------------------------------------------------*
104500970526     C     IREVID        BEGSR
104600970526     C* CAMPI
104700970527     C                   MOVEL     CPENAR        VIDNAR
104800970527     C                   MOVEL     CPECAP        VIDCAP
104900970527     C                   MOVEL     CPETFP        VIDTFP
105000970527     C                   MOVEL     CPELNA        VIDLNA
105100970527     C                   Z-ADD     CPEZNC        VIDZNC
105200970527     C                   MOVEL     CPELOL        VIDLOL
105300970527     C                   Z-ADD     CPEZOL        VIDZOL
105400970606     C                   MOVEL     CPELOS        VIDLOS
105500970606     C                   Z-ADD     CPEZOS        VIDZOS
105600971118     C                   MOVEL     CPELO2        VIDLO2
105700971118     C                   Z-ADD     CPEZO2        VIDZO2
105800970527     C*
105900970526     C                   ENDSR
106000970526     C*------------------------------------------------------------------------*
106100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
106200970526     C*------------------------------------------------------------------------*
106300970526     C     AZZVID        BEGSR
106400970526     C* CAMPI
106500970603     C                   MOVEL     D00NAR        VIDNAR
106600970603     C                   MOVEL     D00CAP        VIDCAP
106700970527     C                   MOVEL     *ZEROS        VIDTFP
106800970527     C                   MOVEL     *ZEROS        VIDLNA
106900970527     C                   Z-ADD     *ZEROS        VIDZNC
107000970527     C                   MOVEL     *ZEROS        VIDLOL
107100970527     C                   Z-ADD     *ZEROS        VIDZOL
107200970606     C                   MOVEL     *ZEROS        VIDLOS
107300970606     C                   Z-ADD     *ZEROS        VIDZOS
107400971118     C                   MOVEL     *ZEROS        VIDLO2
107500971118     C                   Z-ADD     *ZEROS        VIDZO2
107600970526     C* DECODIFICHE
107700970527     C                   MOVEL     *BLANKS       VDDNAR
107800970527     C                   MOVEL     *BLANKS       VDDTFP
107900970527     C                   MOVEL     *BLANKS       VDDLNA
108000970527     C                   MOVEL     *BLANKS       VDDLOL
108100970606     C                   MOVEL     *BLANKS       VDDLOS
108200971118     C                   MOVEL     *BLANKS       VDDLO2
108300970527     C* CAMPI CAPPARIO
108400970527     C                   Z-ADD     *ZEROS        VIDCLN
108500970527     C                   Z-ADD     *ZEROS        VIDCZN
108600970606     C                   MOVEL     *BLANKS       VDDCLN
108700971118     C                   Z-ADD     *ZEROS        VIDCLO
108800970527     C                   Z-ADD     *ZEROS        VIDCZO
108900971118     C                   MOVEL     *BLANKS       VDDCLO
109000970606     C                   Z-ADD     *ZEROS        VIDCLS
109100970606     C                   Z-ADD     *ZEROS        VIDCZS
109200971118     C                   MOVEL     *BLANKS       VDDCLS
109300971118     C                   Z-ADD     *ZEROS        VIDCL2
109400971118     C                   Z-ADD     *ZEROS        VIDCZ2
109500971118     C                   MOVEL     *BLANKS       VDDCL2
109600970526     C*
109700970526     C                   ENDSR
109800970526     C*------------------------------------------------------------------------*
109900970527     C* IMPCPE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
110000970526     C*------------------------------------------------------------------------*
110100970527     C     IMPCPE        BEGSR
110200970526     C*
110300970527     C                   MOVEL     VIDLNA        CPELNA
110400970527     C                   Z-ADD     VIDZNC        CPEZNC
110500970527     C                   MOVEL     VIDLOL        CPELOL
110600970527     C                   Z-ADD     VIDZOL        CPEZOL
110700970606     C                   MOVEL     VIDLOS        CPELOS
110800970606     C                   Z-ADD     VIDZOS        CPEZOS
110900971118     C                   MOVEL     VIDLO2        CPELO2
111000971118     C                   Z-ADD     VIDZO2        CPEZO2
111100970526     C*
111200970526     C                   ENDSR
111300970526     C*------------------------------------------------------------------------*
111400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
111500970526     C*------------------------------------------------------------------------*
111600970526     C     IMPFUN        BEGSR
111700970526     C*
111800970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
111900970526IF  1C     DEPOP1        IFEQ      'O05'
112000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
112100970526     C                   MOVEL     'N'           FUDATT
112200970526     C                   MOVEL     FUDRIG        FUD(6)
112300131007
112400970526E   1C                   ENDIF
112500131007     c                   if        *in10 and  %subst(depop0:2:2) = '05'
112600131007     C                   MOVEL     FUD(7)        FUDRIG                         *LOCALITA'
112700131007     C                   MOVEL     'N'           FUDATT
112800131007     C                   MOVEL     FUDRIG        FUD(7)
112900131007     c                   endif
113000970526     C*
113100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
113200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
113300970526     C                   EXSR      FUNGES
113400970526     C*
113500970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
113600970526     C*---
113700970526     C* 90 -> PROTEZIONE CHIAVE
113800970526     C* 91 -> PROTEZIONE TOTALE
113900970526     C*---
114000970527IF  1C     DEPOP0        IFEQ      'E02'                                        G E S T I O N E
114100970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
114200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
114300970526     C                   SETOFF                                       91
114400970526E   2C                   ENDIF
114500970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
114600970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
114700970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
114800970526E   2C                   ENDIF
114900970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
115000970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
115100970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
115200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
115300970526     C                   SETON                                        91
115400970526E   2C                   ENDIF
115500970526X   1C                   ELSE                                                   *I N T E R R O G A
115600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
115700970526     C                   SETON                                        91
115800970526E   1C                   ENDIF
115900970526     C*
116000970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
116100970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
116200970527     C                   Z-ADD     D00VER        KCEVER
116300970527     C                   MOVEL     D00NAR        KCENAR
116400970527     C                   MOVEL     D00CAP        KCECAP
116500970527     C                   Z-ADD     D00TFP        KCETFP
116600970527     C     KEYCPE        CHAIN(N)  AZCPE01L                           99
116700970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
116800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
116900970526     C                   MOVEL     MSG(6)        DSMSMS
117000970526X   2C                   ELSE
117100970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
117200980128IF  3C     DEPOP1        IFEQ      'O05'
117300980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
117400980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
117500980128E   3C                   ENDIF
117600970526E   2C                   ENDIF
117700970526X   1C                   ELSE                                                   *INSERIMENTO
117800970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
117900970526E   1C                   ENDIF
118000970526     C*
118100970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
118200970526IF  1C     DEPOP1        IFEQ      'O02'
118300970526     C                   MOVEL     DEC(1)        VIDFUN
118400970526E   1C                   ENDIF
118500970526IF  1C     DEPOP1        IFEQ      'O04'
118600970526     C                   MOVEL     DEC(2)        VIDFUN
118700970526E   1C                   ENDIF
118800970526IF  1C     DEPOP1        IFEQ      'O05'
118900970526     C                   MOVEL     DEC(3)        VIDFUN
119000970526E   1C                   ENDIF
119100970526IF  1C     DEPOP1        IFEQ      'O07'
119200970526     C                   MOVEL     DEC(4)        VIDFUN
119300970526E   1C                   ENDIF
119400970526IF  1C     DEPOP1        IFEQ      'F06'
119500970526     C                   MOVEL     DEC(5)        VIDFUN
119600970526E   1C                   ENDIF
119700970526     C*
119800970526     C                   ENDSR
119900970626     C*------------------------------------------------------------------------*
120000970626     C* FINSR - OPERAZIONI FINALI
120100970626     C*------------------------------------------------------------------------*
120200970626     C     FINSR         BEGSR
120300971128     C*
120400970704     C                   CLEAR                   DSSI95
120500970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
120600970626     C                   CALL      'TISI95R'
120700970626     C                   PARM                    DSSI95
120800971128     C*
120900971128     C                   CLEAR                   DSLV55
121000971128     C                   MOVEL     'C'           D55TLA
121100971128     C                   CALL      'FNLV55R'
121200971128     C                   PARM                    DSLV55
121300970626     C*
121400130917     c                   clear                   tisio7ds
121500130917     C                   MOVEL     'C'           Io7TLA
121600131016     c****               movel     tisio7ds      kpjbu
121700130917     C                   CALL      'TISIO7R'
121800130917     C                   PARM                    kpjba
121900131016     C                   PARM                    tisio7ds
122000130917     C*
122100970626     C                   ENDSR
122200130917     C*------------------------------------------------------------------------*
122300130917     C* Richiamo pgm annullamento orari servizi a fronte di modifica lna
122400130917     C*------------------------------------------------------------------------*
122500130917     c     sr_oraris     begsr
122600130917
122700130917     C* LINEA STANDARD
122800130917     c                   movel     vidlna        w0030             3 0
122900130917     c* Se cambiata la linea oppure sempre se opzione 4=annullamento
123000130917     c                   if        w0030<>cpelna or depop1='O04'
123100130917     c                   clear                   tisio7ds
123200130917     c                   eval      io7mod='D'
123300130917     c                   eval      io7liv='E'
123400130917     c                   eval      io7ver=d00ver
123500130917     c                   eval      io7nar=vidnar
123600130917     c                   eval      io7cap=vidcap
123700130917     c                   eval      io7lna=cpelna
123800130917     c                   eval      io7tfp=cpetfp
123900130917     c                   if        depop1<>'O04'
124000130917     c                   eval      io7lns=%dec(vidlna:3:0)
124100130917     c                   eval      io7los=%dec(vidlos:3:0)
124200130917     c                   eval      io7lol=%dec(vidlol:3:0)
124300130917     c                   endif
124400131016     c*****+             movel     tisio7ds      kpjbu
124500130917     c                   call      'TISIO7R'
124600130917     c                   parm                    kpjba
124700131016     c                   parm                    tisio7ds
124800130917    3c                   endif
124900130917     C* LINEA SOTTO
125000130917     c                   movel     vidlos        w0030             3 0
125100130917     c* Se cambiata la linea e non ancora richiamato pgm per la linea stessa
125200130917     c                   if        (w0030<>cpelos or depop1='O04')
125300130917     c                             and cpelos<>cpelna
125400130917     c                   clear                   tisio7ds
125500130917     c                   eval      io7mod='D'
125600130917     c                   eval      io7liv='E'
125700130917     c                   eval      io7ver=d00ver
125800130917     c                   eval      io7nar=vidnar
125900130917     c                   eval      io7cap=vidcap
126000130917     c                   eval      io7lna=cpelos
126100130917     c                   eval      io7tfp=cpetfp
126200130917     c                   if        depop1<>'O04'
126300130917     c                   eval      io7lns=%dec(vidlna:3:0)
126400130917     c                   eval      io7los=%dec(vidlos:3:0)
126500130917     c                   eval      io7lol=%dec(vidlol:3:0)
126600130917     c                   endif
126700131016     c*******            movel     tisio7ds      kpjbu
126800130917     c                   call      'TISIO7R'
126900130917     c                   parm                    kpjba
127000131016     c                   parm                    tisio7ds
127100130917    3c                   endif
127200130917     C* LINEA OLTRE
127300130917     c                   movel     vidlos        w0030             3 0
127400130917     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
127500130917     c                   if        (w0030<>cpelol or depop1='O04')
127600130917     c                              and cpelol<>cpelna and cpelol<>cpelos
127700130917     c                   clear                   tisio7ds
127800130917     c                   eval      io7mod='D'
127900130917     c                   eval      io7liv='E'
128000130917     c                   eval      io7ver=d00ver
128100130917     c                   eval      io7nar=vidnar
128200130917     c                   eval      io7cap=vidcap
128300130917     c                   eval      io7lna=cpelol
128400130917     c                   eval      io7tfp=cpetfp
128500130917     c                   if        depop1<>'O04'
128600130917     c                   eval      io7lns=%dec(vidlna:3:0)
128700130917     c                   eval      io7los=%dec(vidlos:3:0)
128800130917     c                   eval      io7lol=%dec(vidlol:3:0)
128900130917     c                   endif
129000131016     c*******            movel     tisio7ds      kpjbu
129100130917     c                   call      'TISIO7R'
129200130917     c                   parm                    kpjba
129300131016     c                   parm                    tisio7ds
129400130917    3c                   endif
129500130917     c                   endsr
129600970526     C*------------------------------------------------------------------------*
129700970526     C* *INZSR - OPERAZIONI INIZIALI
129800970526     C*------------------------------------------------------------------------*
129900970526     C     *INZSR        BEGSR
130000970526     C*---
130100970526     C* RICEVIMENTO PARAMETRI
130200970526     C*---
130300970526     C     *ENTRY        PLIST
130400970612     C                   PARM                    KPJBA
130500970612     C                   PARM                    DSSI00
130600970526     C*
130700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
130800970526     C     XMSGPA        PLIST
130900970526     C                   PARM                    DSMSMS
131000970526     C                   PARM                    DSMSPG
131100970526     C                   PARM                    DSMSKE
131200970526     C                   PARM                    DSMSER
131300970526     C*
131400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
131500970526     C                   CALL      'XKEY'
131600970526     C                   PARM                    DSKEY
131700970526     C*---
131800970526     C* VARIABILI NON RIFERITE AL DATA BASE
131900970526     C*---
132000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
132100970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
132200970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
132300970526     C     *LIKE         DEFINE    D00OP0        WOP0
132400970526     C     *LIKE         DEFINE    D00OP1        WOP1
132500970526     C*---
132600970526     C* VARIABILI DI CONTROLLO
132700970526     C*---
132800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
132900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
133000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
133100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
133200970526     C*
133300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
133400970527     C                   MOVEL     'TISI12R'     DSMSPG
133500970527     C                   MOVEL     'TISI12R'     MSGQUE
133600970526     C                   MOVEL     *BLANKS       MSGKEY
133700970526     C                   MOVEL     *BLANKS       DSMSMS
133800970526     C*---
133900970526     C* VARIABILI RIFERITE AL DATA BASE
134000970526     C*---
134100970527     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
134200970527     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
134300970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
134400970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
134500970527     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
134600970527     C     *LIKE         DEFINE    CPENAR        KCENAR
134700970527     C     *LIKE         DEFINE    CPECAP        KCECAP
134800970527     C     *LIKE         DEFINE    CPETFP        KCETFP
134900970526     C*---
135000970526     C* CHIAVI DI LETTURA
135100970526     C*---
135200970527     C* LETTURA AZORG01L - COMPLETA
135300970527     C     KEYORG        KLIST
135400970528     C                   KFLD                    KORFIL                         *FILIALE
135500970526     C* LETTURA TABEL00F - COMPLETA
135600970526     C     KEYTAB        KLIST
135700970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
135800970526     C                   KFLD                    KTBCOD                         *TABELLA
135900970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
136000970527     C* LETTURA AZCPE01L - COMPLETA
136100970527     C     KEYCPE        KLIST
136200970527     C                   KFLD                    KCEVER                         *VERSIONE
136300970527     C                   KFLD                    KCENAR                         *NAZIONE
136400970527     C                   KFLD                    KCECAP                         *CAP
136500970527     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
136600970526     C*
136700970526     C* DEPOSITA LE OPZIONI DI ENTRATA
136800970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
136900970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
137000971128     C*
137100971128     C* CALCOLA LA DATA CORRENTE
137200971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
137300971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
137400971128     C                   Z-ADD     WN8           G08DAT
137500971128     C                   Z-ADD     *ZEROS        G08INV
137600971128     C                   MOVEL     '0'           G08ERR
137700971128     C                   CALL      'XSRDA8'
137800971128     C                   PARM                    WLBDA8
137900971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
138000060206      *
138100060206      * REPERISCE I DATI DELL'UTENTE
138200060206     c                   exsr      RepDatiUte
138300060206     c                   movel     RSUT          DSfirs
138400131007      /free
138500131007        // Carico filiali di test abilitate
138600131007         if dutpou<>46  ;
138700131007
138800131007         clear TIBS02ds;
138900131007         T02Mod = 'C';
139000131007         T02sif = knsif;
139100131007         T02cod = 'VPO';
139200131007         t02tla = 'L'   ;
139300131007         T02ke1 ='DECOFI816' ;
139400131007         TNTBE_RicercaControllo  (kpjba : tibs02ds);
139500131007
139600131007         if  T02err  = *blank;
139700131007         xx= 1 ;
139800131007         yy= 1 ;
139900131007
140000131007         dow xx< 256   ;
140100131007           fil_ab(yy)= %subst(t02uni:xx:3)  ;
140200131007           yy=yy+ 1   ;
140300131007           xx=xx+ 3   ;
140400131007         enddo  ;
140500131007
140600131007         endif;
140700131007
140800131007         // se c'e' 999 tutte abilitate
140900131007         w003a='999'  ;
141000131007         if   %lookup(w003a:fil_ab) = 0  ;
141100131007         w003a=%editc(dutpou:'X')  ;
141200131007         if   %lookup(w003a:fil_ab) = 0  ;
141300131007         *in10 =*on  ;
141400131007         endif  ;
141500131007         endif  ;
141600131007
141700131007         endif  ;
141800131007
141900131007      /end-free
142000131007
142100970526     C*
142200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
142300970526     C                   EXSR      IMPFUN
142400970526     C*
142500970526     C                   ENDSR
142600060206      *---------------------------------------------------------------*
142700060206      * REPERISCE DATI UTENTE                                         *
142800060206      *---------------------------------------------------------------*
142900060206     c     RepDatiUte    BEGSR
143000060206      *
143100060206     c     *dtaara       define    §azute        AZUTEds
143200060206     c     *dtaara       define    §datiute      DDatiUte
143300060206      *
143400060206     c                   in(E)     *dtaara
143500060206     c                   IF        %ERROR or RSUT = *blanks
143600060206     c                   clear                   Tibs34Ds
143700060206     c                   call      'TIBS34R'
143800060206     c                   parm                    Tibs34Ds
143900060206     c                   in        *dtaara
144000060206     c                   ENDIF
144100060206      *
144200060206     c                   ENDSR
144300970526     C*------------------------------------------------------------------------*
144400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
144500970526     C*------------------------------------------------------------------------*
144600970526     C     FUNGES        BEGSR
144700970526     C*
144800970526     C* PULISCO CAMPI DI LAVORO
144900970526     C                   Z-ADD     0             W                 5 0
145000970526     C*
145100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
145200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
145300970526     C     1             DO        24            I
145400970526     C                   MOVEL     FUD(I)        FUDRIG
145500970526     C*
145600970526     C* PULIZIA FUNZIONI PRECEDENTI
145700970526     C                   MOVEL     *BLANKS       FUV(I)
145800970526     C*
145900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
146000970526     C* (SALTA F24)
146100970526     C     FUDATT        IFEQ      'S'
146200970526     C     FUDFUN        ANDNE     '24'
146300970526     C                   ADD       1             W
146400970526     C                   MOVEL     FUDDES        FUV(W)
146500970526     C                   END
146600970526     C                   END                                                    DO WHILE
146700970526     C*
146800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
146900970526     C* ALTRIMENTI DISATTIVO F24
147000970526     C     1             DO        24            I
147100970526     C                   MOVEL     FUD(I)        FUDRIG
147200970526     C*
147300970526     C     FUDFUN        IFEQ      '24'
147400970526     C     W             IFGT      6
147500970526     C                   MOVEL     'S'           FUDATT
147600970526     C                   ELSE
147700970526     C                   MOVEL     *BLANKS       FUDATT
147800970526     C                   END                                                    W GT 8
147900970526     C                   MOVEL     FUDRIG        FUD(I)
148000970526     C                   END
148100970526     C                   END                                                    DO WHILE
148200970526     C*
148300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
148400970526     C     W             DIV       6             I
148500970526     C                   MVR                     Z
148600970526     C*
148700970526     C     Z             IFNE      0
148800970526     C     I             ADD       1             Z
148900970526     C                   ELSE
149000970526     C                   Z-ADD     I             Z                 5 0
149100970526     C                   END
149200970526     C*
149300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
149400970526     C                   ADD       1             FUNGRU
149500970526     C     FUNGRU        IFGT      Z
149600970526     C     FUNGRU        ORLT      1
149700970526     C                   Z-ADD     1             FUNGRU
149800970526     C                   END
149900970526     C*
150000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
150100970526     C     FUNGRU        IFEQ      1
150200970526     C                   MOVEL     FUV(1)        FUN001
150300970526     C                   MOVEL     FUV(2)        FUN002
150400970526     C                   MOVEL     FUV(3)        FUN003
150500970526     C                   MOVEL     FUV(4)        FUN004
150600970526     C                   MOVEL     FUV(5)        FUN005
150700970526     C                   MOVEL     FUV(6)        FUN006
150800970526     C                   END
150900970526     C*
151000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
151100970526     C     FUNGRU        IFEQ      2
151200970526     C                   MOVEL     FUV(7)        FUN001
151300970526     C                   MOVEL     FUV(8)        FUN002
151400970526     C                   MOVEL     FUV(9)        FUN003
151500970526     C                   MOVEL     FUV(10)       FUN004
151600970526     C                   MOVEL     FUV(11)       FUN005
151700970526     C                   MOVEL     FUV(12)       FUN006
151800970526     C                   END
151900970526     C*
152000970526     C* TERZO GRUPPO DI 6 FUNZIONI
152100970526     C     FUNGRU        IFEQ      3
152200970526     C                   MOVEL     FUV(13)       FUN001
152300970526     C                   MOVEL     FUV(14)       FUN002
152400970526     C                   MOVEL     FUV(15)       FUN003
152500970526     C                   MOVEL     FUV(16)       FUN004
152600970526     C                   MOVEL     FUV(17)       FUN005
152700970526     C                   MOVEL     FUV(18)       FUN006
152800970526     C                   END
152900970526     C*
153000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
153100970526     C     FUNGRU        IFEQ      4
153200970526     C                   MOVEL     FUV(19)       FUN001
153300970526     C                   MOVEL     FUV(20)       FUN002
153400970526     C                   MOVEL     FUV(21)       FUN003
153500970526     C                   MOVEL     FUV(22)       FUN004
153600970526     C                   MOVEL     *BLANKS       FUN005
153700970526     C                   MOVEL     *BLANKS       FUN006
153800970526     C                   END
153900970526     C*
154000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
154100970526     C                   MOVEL     *BLANKS       FUN007
154200970526     C                   MOVEL     *BLANKS       FUN008
154300970526     C*
154400970526     C     1             DO        24            I
154500970526     C                   MOVEL     FUD(I)        FUDRIG
154600970526     C     FUDFUN        IFEQ      '24'
154700970526     C     FUDATT        ANDEQ     'S'
154800970526     C                   MOVEL     FUDDES        FUN008
154900970526     C                   END
155000970526     C                   END                                                    DO
155100970526     C*
155200970526     C                   ENDSR
155300970526     C*------------------------------------------------------------------------*
155400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
155500970526     C*------------------------------------------------------------------------*
155600970526     C     FUNCON        BEGSR
155700970526     C*
155800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
155900970526     C                   EXSR      FUNVAR
156000970526     C*
156100970526     C* PULISCO CAMPI DI LAVORO
156200970526     C                   Z-ADD     0             I                 5 0
156300970526     C                   Z-ADD     0             W                 5 0
156400970526     C*
156500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
156600970526     C     FUNKEY        IFNE      *BLANKS
156700970526     C     1             DO        24            I
156800970526     C                   MOVEL     FUD(I)        FUDRIG
156900970526     C*
157000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
157100970526     C     FUNKEY        IFEQ      FUDFUN
157200970526     C     FUDATT        ANDEQ     'S'
157300970526     C                   Z-ADD     I             W
157400970526     C                   END
157500970526     C                   END                                                    DO WHILE
157600970526     C*
157700970526     C* FUNZIONE NON ATTIVA (ERRORE)
157800970526     C     W             IFEQ      0
157900970526     C*
158000970526     C* EMISSIONE MESSAGGIO DI ERRORE
158100970526     C                   MOVEL     MSG(1)        DSMSMS
158200970526     C*
158300970526     C                   END                                                    W=0
158400970526     C                   END                                                    FUNKEY NE *BLANKS
158500970526     C*
158600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
158700970526     C     W             IFNE      0
158800970526     C     FUNKEY        OREQ      *BLANKS
158900970526     C     FUNKEY        IFEQ      *BLANKS
159000970526     C                   EXSR      ENTGES
159100970526     C                   END
159200970526     C     FUNKEY        IFEQ      '03'
159300970526     C                   EXSR      F03GES
159400970526     C                   END
159500970526     C     FUNKEY        IFEQ      '06'
159600970526     C                   EXSR      F06GES
159700970526     C                   END
159800130905     C     FUNKEY        IFEQ      '07'
159900130905     C                   EXSR      F07GES
160000130905     C                   END
160100970526     C     FUNKEY        IFEQ      '12'
160200970526     C                   EXSR      F12GES
160300970526     C                   END
160400970526     C     FUNKEY        IFEQ      '24'
160500970526     C                   EXSR      F24GES
160600970526     C                   END
160700970526     C                   END
160800970526     C*
160900970526     C                   ENDSR
161000970526     C*------------------------------------------------------------------------*
161100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
161200970526     C*------------------------------------------------------------------------*
161300970526     C     FUNVAR        BEGSR
161400970526     C*
161500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
161600970526     C*
161700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
161800970526     C     KEY           IFEQ      ENTER
161900970526     C                   MOVEL     *BLANKS       FUNKEY
162000970526     C                   END
162100970526     C     KEY           IFEQ      F1
162200970526     C                   MOVEL     '01'          FUNKEY
162300970526     C                   END
162400970526     C     KEY           IFEQ      F2
162500970526     C                   MOVEL     '02'          FUNKEY
162600970526     C                   END
162700970526     C     KEY           IFEQ      F3
162800970526     C                   MOVEL     '03'          FUNKEY
162900970526     C                   END
163000970526     C     KEY           IFEQ      F4
163100970526     C                   MOVEL     '04'          FUNKEY
163200970526     C                   END
163300970526     C     KEY           IFEQ      F5
163400970526     C                   MOVEL     '05'          FUNKEY
163500970526     C                   END
163600970526     C     KEY           IFEQ      F6
163700970526     C                   MOVEL     '06'          FUNKEY
163800970526     C                   END
163900970526     C     KEY           IFEQ      F7
164000970526     C                   MOVEL     '07'          FUNKEY
164100970526     C                   END
164200970526     C     KEY           IFEQ      F8
164300970526     C                   MOVEL     '08'          FUNKEY
164400970526     C                   END
164500970526     C     KEY           IFEQ      F9
164600970526     C                   MOVEL     '09'          FUNKEY
164700970526     C                   END
164800970526     C     KEY           IFEQ      F10
164900970526     C                   MOVEL     '10'          FUNKEY
165000970526     C                   END
165100970526     C     KEY           IFEQ      F11
165200970526     C                   MOVEL     '11'          FUNKEY
165300970526     C                   END
165400970526     C     KEY           IFEQ      F12
165500970526     C                   MOVEL     '12'          FUNKEY
165600970526     C                   END
165700970526     C     KEY           IFEQ      F13
165800970526     C                   MOVEL     '13'          FUNKEY
165900970526     C                   END
166000970526     C     KEY           IFEQ      F14
166100970526     C                   MOVEL     '14'          FUNKEY
166200970526     C                   END
166300970526     C     KEY           IFEQ      F15
166400970526     C                   MOVEL     '15'          FUNKEY
166500970526     C                   END
166600970526     C     KEY           IFEQ      F16
166700970526     C                   MOVEL     '16'          FUNKEY
166800970526     C                   END
166900970526     C     KEY           IFEQ      F17
167000970526     C                   MOVEL     '17'          FUNKEY
167100970526     C                   END
167200970526     C     KEY           IFEQ      F18
167300970526     C                   MOVEL     '18'          FUNKEY
167400970526     C                   END
167500970526     C     KEY           IFEQ      F19
167600970526     C                   MOVEL     '19'          FUNKEY
167700970526     C                   END
167800970526     C     KEY           IFEQ      F20
167900970526     C                   MOVEL     '20'          FUNKEY
168000970526     C                   END
168100970526     C     KEY           IFEQ      F21
168200970526     C                   MOVEL     '21'          FUNKEY
168300970526     C                   END
168400970526     C     KEY           IFEQ      F22
168500970526     C                   MOVEL     '22'          FUNKEY
168600970526     C                   END
168700970526     C     KEY           IFEQ      F23
168800970526     C                   MOVEL     '23'          FUNKEY
168900970526     C                   END
169000970526     C     KEY           IFEQ      F24
169100970526     C                   MOVEL     '24'          FUNKEY
169200970526     C                   END
169300970526     C     KEY           IFEQ      ROLLUP
169400970526     C                   MOVEL     'RU'          FUNKEY
169500970526     C                   END
169600970526     C     KEY           IFEQ      HELP
169700970526     C                   MOVEL     'HP'          FUNKEY
169800970526     C                   END
169900970526     C                   ENDSR
170000970526     C*------------------------------------------------------------------------*
170100970526     C* NOTE COSTRUZIONE SCHIERE
170200970526     C*------------------------------------------------------------------------*
170300970526     C*
170400970526     C* -MSG-MESSAGGI
170500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
170600970526     C* (NON TOCCARE MSG,1 E MSG,2)
170700970526     C*
170800970526     C* -CMD-COMANDI
170900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
171000970526     C*
171100970526     C* -OPD-OPZIONI
171200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
171300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
171400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
171500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
171600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
171700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
171800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
171900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
172000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
172100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
172200970526     C*
172300970526     C* -FUD-FUNZIONI
172400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
172500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
172600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
172700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
172800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
172900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
173000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
173100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
173200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
173300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
173400970526     O*------------------------------------------------------------------------*
173500970526** DEC - DECODIFICHE
173600970526    MODIFICA                                                      1
173700970526  ANNULLAMENTO                                                    2
173800970526VISUALIZZAZIONE                                                   3
173900970526   RIPRISTINO                                                     4
174000970526  INSERIMENTO                                                     5
174100970526** MSG - MESSAGGI
174200970527TISI12R- Tasto funzionale non gestito                                          1
174300970527TISI12R- Inserimento effettuato                                                2
174400970527TISI12R- Modifica effettuata                                                   3
174500970527TISI12R- Annullamento effettuato                                               4
174600970527TISI12R- Ripristino effettuato                                                 5
174700970527TISI12R- Richiesta funzione per record inesistente                             6
174800970527TISI12R- Record momentaneamente allocato: riprovare più tardi                  7
174900970527TISI12R- Richiesta funzione per record esistente                               8
175000000601TISI12R- 9
175100000601TISI12R- 10                                                                   10
175200970527TISI12R- Nazione errata                                                       11
175300970626TISI12R- CAP errato o inesistente                                             12
175400970626TISI12R- 13
175500970527TISI12R- Terminal partenza errato                                             14
175600970527TISI12R- Linea di arrivo errata                                               15
175700970527TISI12R- Zona di consegna errata                                              16
175800060206TISI12R- Zona di consegna NON abilitata                                       17
175900970527TISI12R- 18
176000970527TISI12R- 19
176100970527TISI12R- 20
176200970526** FUD - FUNZIONI
176300970526                   -        (S/N)=N                        funz.01¬
176400970526                   -        (S/N)=N                        funz.02¬
176500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
176600970526                   -        (S/N)=N                        funz.04¬
176700970526                   -        (S/N)=N                        funz.05¬
176800970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
176900130905F07=Orari Servizi  -        (S/N)=S                        funz.07¬
177000970527                   -        (S/N)=N                        funz.08¬
177100970527                   -        (S/N)=N                        funz.09¬
177200970527                   -        (S/N)=N                        funz.10¬
177300970526                   -        (S/N)=N                        funz.11¬
177400970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
177500970526                   -        (S/N)=N                        funz.13¬
177600970526                   -        (S/N)=N                        funz.14¬
177700970526                   -        (S/N)=N                        funz.15¬
177800970526                   -        (S/N)=N                        funz.16¬
177900970526                   -        (S/N)=N                        funz.17¬
178000970526                   -        (S/N)=N                        funz.18¬
178100970526                   -        (S/N)=N                        funz.19¬
178200970526                   -        (S/N)=N                        funz.20¬
178300970526                   -        (S/N)=N                        funz.21¬
178400970526                   -        (S/N)=N                        funz.22¬
178500970526                   -        (S/N)=N                        funz.23¬
178600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
