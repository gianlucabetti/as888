000100970526     H*------------------------------------------------------------------------*
000200061009     H* CAPPARIO - GESTIONE LOCALITA'  - ROUTINE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970529     FTISI31D   CF   E             WORKSTN INFDS(DEVDS1)
000900970529     F                                     SFILE(SI31DMS:MSGULT)
001000970527     FTABEL00F  IF   E           K DISK
001100970527     FAZORG01L  IF   E           K DISK
001200970527     F*------------
001300970529     F* LOCALITA'
001400970527     F*------------
001500970529     FAZCPL01L  UF A E           K DISK
001600970626     F*------------
001700970626     F* CAPPARIO
001800970626     F*------------
001900970626     FAZCPC01L  IF   E           K DISK
002000970529     F*------------
002100970529     F* TEMPI CONSEGNA
002200970529     F*------------
002300970529     FAZCPT01L  IF   E           K DISK
002400970925     F*------------
002500970925     F* SINONIMI
002600970925     F*------------
002700970925     FAZCPS01L  UF   E           K DISK
002800090903     f
002900090903     FAZCPR01L  UF   E           K DISK
003000970526     D*------------------------------------------------------------------------*
003100970526     D* SCHIERE
003200970526     D*------------------------------------------------------------------------*
003300970526     D*------------
003400970526     D* DECODIFICHE
003500970526     D*------------
003600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
003700970526     D*------------
003800970526     D* MESSAGGI
003900970526     D*------------
004000010322     D MSG             S             78    DIM(35) CTDATA PERRCD(1)
004100970526     D*------------
004200970526     D* DESCRIZIONI TASTI FUNZIONALI
004300970526     D*------------
004400970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004500970526     D*------------
004600970526     D* FUNZIONI DA CARICARE A VIDEO
004700970526     D*------------
004800970526     D FUV             S             19    DIM(24)
004900970526     D*------------------------------------------------------------------------*
005000970526     D* INPUT
005100970526     D*------------------------------------------------------------------------*
005200970526     D*------------
005300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005400970526     D*------------
005500970526     D FUDDS           DS
005600970526     D  FUDDE1                 1      1
005700970526     D  FUDFUN                 2      3
005800970526     D  FUDDE2                 4     19
005900970526     D  FUDFI0                20     34
006000970526     D  FUDATT                35     35
006100970526     D  FUDFI1                36     80
006200970526     D  FUDDES                 1     19
006300970526     D  FUDRIG                 1     80
006400970526     D*------------
006500970526     D* DS GESTIONE TASTI FUNZIONALI
006600970526     D*------------
006700970526     D DSKEY         E DS
006800970526     D DEVDS1          DS
006900970526     D  KEY                  369    369
007000970526     D*------------
007100970526     D* DS "XMSG" - GESTIONE MESSAGGI
007200970526     D*------------
007300970526     D DSMSG         E DS
007400970527     D*------------------
007500970527     D* DS NAZIONI
007600970527     D*------------------
007700970527     D DS15          E DS
007800970527     D*------------------
007900970527     D* DS ZONE DI CONSEGNA
008000970527     D*------------------
008100970527     D DS03          E DS
008200970529     D*------------------
008300970529     D* DS CODICI TASSAZIONE
008400970529     D*------------------
008500970529     D DSCT          E DS
008600970529     D*------------------
008700970529     D* DS TIPI INOLTRO
008800970529     D*------------------
008900970529     D DS9A          E DS
009000970626     D*------------------
009100970626     D* DS "TISI95R" - CONTROLLO CAP
009200970626     D*------------------
009300970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
009400970527     D*------------
009500970527     D* DS "TISI81R" - AGGIORNA UN CAP DAI SUOI ARCHIVI
009600970527     D*------------
009700970527     D DSSI81        E DS                  EXTNAME(TISI81DS)
009800970526     D*------------
009900970526     D* DS DI PROCEDURA
010000970526     D*------------
010100970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010200130821     d tisio0ds      E DS
010300970526     D*------------
010400970526     D* ARCHITETTURA
010500970526     D*------------
010600970526     D KPJBA         E DS
010700050413     D*------------
010800050413     D* DS REPERIMENTO DATI UTENTE
010900050413     D*------------
011000050413     D TIBS34DS      E DS                                                       *Profili utente
011100050413     D DDATIUTE      E DS                                                       *Dati utente
011200050413     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
011300050413     D DUTE01        E DS                                                       *Abilitazioni
011400090605
011500090605     d ds5e          e ds
011600090903     d
011700090903     d dvpodeco      e ds
011800090903     d Tibs02Ds      e ds
011900000426     D*------------------
012000000426     D* PASSAGGIO PARAMETRI
012100000426     D*------------------
012200000426     D PARAM           DS
012300000426     D  POP1                   1      3
012400000426     D  PUPI                   4      8  0
012500000426     D  PMSG                   9     88
012600000426     D  POPZ                  89     91
012700090903
012800090903     d datasys         s               d   inz(*sys)
012900090903     d dataiso         s               d   datfmt(*iso)
013000970526     C*------------------------------------------------------------------------*
013100970526     C* MAIN LINES
013200970526     C*------------------------------------------------------------------------*
013300970526     C*
013400970526     C* NON CI SONO ERRORI IN ENTRATA
013500970526IF  1C     ERRL00        IFEQ      '0'
013600970526     C*
013700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014100970526     C*
014200970526     C* EMETTE FORMATO VIDEO
014300970526     C                   EXSR      BUTFMR
014400970526     C*
014500970526     C* GESTIONE TASTI FUNZIONALI
014600970526     C                   EXSR      FUNCON
014700970526E   2C                   ENDDO
014800970526     C*
014900970526     C* CI SONO ERRORI IN ENTRATA
015000970527X   1C                   ELSE
015100970526     C                   MOVEL     '1'           D00ERR
015200970526     C                   MOVEL     DSMSMS        D00MSG
015300970526E   1C                   ENDIF
015400970626     C*
015500970626     C* OPERAZIONI FINALI
015600970626     C                   EXSR      FINSR
015700970526     C* FINE PGM
015800970526     C                   SETON                                        LR
015900970526     C*------------------------------------------------------------------------*
016000970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016100970526     C*------------------------------------------------------------------------*
016200970526     C     BUTFMR        BEGSR
016300970526     C*
016400970526     C* EMETTE LA TESTATA
016500970529     C                   WRITE     SI31D01
016600970526     C*
016700970526     C* SPEDISCE EVENTUALE MESSAGGIO
016800970526     C                   EXSR      SNDMSG
016900970526     C*
017000970526     C* EMETTE LE FUNZIONI
017100970529     C                   WRITE     SI31D02
017200970526     C*
017300970526     C* EMETTE IL VIDEO
017400970529     C                   EXFMT     SI31D03
017500970526     C*
017600970526     C* REIMPOSTA FLAG DI ERRORE
017700970526     C                   MOVEL     '0'           ERRL00
017800970526     C*
017900970526     C* REIMPOSTA INDICATORI DI ERRORE
018000970526     C                   SETOFF                                         5859
018100970526     C                   SETOFF                                       606162
018200970526     C                   SETOFF                                       636465
018300970526     C                   SETOFF                                       666768
018400970526     C                   SETOFF                                       697071
018500970526     C                   SETOFF                                       727374
018600970526     C                   SETOFF                                       757677
018700970526     C                   SETOFF                                       787980
018800970526     C                   SETOFF                                       818283
018900970526     C                   SETOFF                                       848586
019000970526     C                   SETOFF                                       878889
019100000426     C                   SETOFF                                       525354
019200010322     C                   SETOFF                                       5544
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
019700970526     C*------------------------------------------------------------------------*
019800970526     C     SNDMSG        BEGSR
019900970526     C*
020000970526     C* PULISCE IL SFILE MESSAGGI
020100970526     C                   MOVEL     '1'           *IN40
020200970529     C                   WRITE     SI31DMC
020300970526     C                   MOVEL     '0'           *IN40
020400970526     C*
020500970526     C* VISUALIZZA IL MESSAGGIO
020600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
020700970526     C                   CALL      'XMSG'        XMSGPA
020800970526IF  1C     DSMSER        IFEQ      *ZEROS
020900970526     C                   MOVEL     DSMSKE        MSGKEY
021000970526E   1C                   ENDIF
021100970526     C                   Z-ADD     1             MSGULT
021200970529     C                   WRITE     SI31DMS
021300970529     C                   WRITE     SI31DMC
021400970526     C*
021500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
021600970526     C                   MOVEL     *BLANKS       DSMSMS
021700970526     C*
021800970526     C                   ENDSR
021900970526     C*------------------------------------------------------------------------*
022000970526     C* ENTGES - GESTIONE ENTER -> INVIO
022100970526     C*------------------------------------------------------------------------*
022200970526     C     ENTGES        BEGSR
022300970526     C*
022400970526     C* CONTROLLA I CAMPI IMMESSI
022500970526     C                   EXSR      DECODI
022600970526     C*
022700970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
022800970526IF  1C     DEPOP1        IFEQ      'O05'
022900970526     C                   MOVEL     '1'           FINL00
023000970526E   1C                   ENDIF
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400970526     C* F03GES - GESTIONE F03 -> FINE
023500970526     C*------------------------------------------------------------------------*
023600970526     C     F03GES        BEGSR
023700970526     C*
023800970526     C* IMPOSTA "PREMUTO F03"
023900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
024000970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
024100970526     C*
024200970526     C                   ENDSR
024300010322     C*------------------------------------------------------------------------*
024400010322     C* F04GES - GESTIONE F04 -> VISUALIZZAZIONE TEMPI CONSEGNA/RITIRO
024500010322     C*------------------------------------------------------------------------*
024600010322     C     F04GES        BEGSR
024700090605      * imposto le descrizioni fisse per i 3 tipi servizio utilizzati
024800090605      * 'C' - 'D' - 'E'
024900090605     c                   eval      ktbcod = '5E'
025000090605     c     ke2tab        setll     tabel00f
025100090605     c                   do        *hival
025200090605     c     ke2tab        reade     tabel00f
025300090605     c                   if        %eof(tabel00f)
025400090605     c                   leave
025500090605     c                   endif
025600090605     c                   eval      ds5e = tbluni
025700090605     c                   if        tblkey = 'C'
025800090605     c                   eval      viddtspc = §5ed03
025900090605     c                   endif
026000090605     c                   if        tblkey = 'D'
026100090605     c                   eval      viddtspd = §5ed03
026200090605     c                   endif
026300090605     c                   if        tblkey = 'E'
026400090605     c                   eval      viddtspe = §5ed03
026500090605     c                   endif
026600090903     c                   if        tblkey = 'H' and (vidh0c>0 or
026700090903     c                             Datasys>=dataiso)
026800090903     c                   eval      viddtsph = §5ed03
026900090903     c                   endif
027000090605     c                   enddo
027100010322     C*
027200010322     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
027300010322     C     RNMFL         IFEQ      'VIDTTC'
027400010322     C                   EXFMT     SI31WWC
027500010322     C                   ELSE
027600010322     C     RNMFL         IFEQ      'VIDTTR'
027700010322     C                   EXFMT     SI31WWR
027800010322     C                   ELSE
027900010322     C                   MOVEL     MSG(31)       DSMSMS
028000010322     C                   ENDIF
028100010322     C                   ENDIF
028200010322     C*
028300010322     C                   ENDSR
028400130830     C*------------------------------------------------------------------------*
028500130830     C* F17GES - GESTIONE F17 -> Orari servizi  per linea di arrivo
028600130830     C*------------------------------------------------------------------------*
028700130830     C     F17GES        BEGSR
028800130830     c                   clear                   dsmsms
028900130830     c                   exsr      decodi
029000130830     c
029100130830    1c                   if        dsmsms=*blanks
029200130830     C*
029300130830     c                   clear                   tisio0ds
029400130830     C                   MOVEL     VIDNAR        do0NAR
029500130830     c                   if        %subst(depop0:2:2)='02'
029600130830     C                   MOVEL     'G'           do0tric
029700130830     c                   else
029800130830     C                   MOVEL     'I'           do0tric
029900130830     c                   endif
030000130830     c                   movel     d00dri        do0dri
030100130830     c                   movel     d00ver        do0ver
030200130830
030300130830     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
030400130830    2c                   select
030500130830     C     RNMFL         wheneq    'VIDLNA'
030600130830     c     vidlna        andgt     *zeros
030700130830     C                   movel     vidlna        do0lna
030800130830     c
030900130830     C     RNMFL         wheneq    'VIDLOL'
031000130830     c     vidlol        andgt     *zeros
031100130830     C                   movel     vidlol        do0lna
031200130830     C     RNMFL         wheneq    'VIDLOS'
031300130830     c     vidlos        andgt     *zeros
031400130830     C                   movel     vidlos        do0lna
031500130830     C                   other
031600130905     C                   MOVEL     MSG(35)       DSMSMS
031700130830    2C                   ENDsl
031800130830
031900130830    2c                   if        dsmsms=*blanks
032000130830     C                   call      'TISIO2R'
032100130830     C                   PARM                    KPJBA
032200130830     C                   PARM                    tisio0ds
032300130830     C*
032400130830     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
032500130830IF  3C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
032600130830     C                   MOVEL     '1'           ERRL00                         *ERRORE
032700130830     C                   MOVEL     '1'           F03L00                         *FINE PGM
032800130830X   1C                   ELSE                                                   *NON PREMUTO F03
032900130830     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
033000130830E   3C                   ENDIF
033100130830     C*
033200130830E   2C                   ENDIF
033300130830E   1C                   ENDIF
033400130830     C*
033500130830     C                   ENDSR
033600970526     C*------------------------------------------------------------------------*
033700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
033800970526     C*------------------------------------------------------------------------*
033900970526     C     F06GES        BEGSR
034000970526     C*
034100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
034200970526IF  1C     DEPOP1        IFEQ      'F06'
034300970526     C                   EXSR      INSERI
034400970526E   1C                   ENDIF
034500970526IF  1C     DEPOP1        IFEQ      'O02'
034600970526     C                   EXSR      MODIFI
034700970526E   1C                   ENDIF
034800970526IF  1C     DEPOP1        IFEQ      'O04'
034900970526     C                   EXSR      ANNULL
035000970526E   1C                   ENDIF
035100970526IF  1C     DEPOP1        IFEQ      'O07'
035200970526     C                   EXSR      RIPRIS
035300970526E   1C                   ENDIF
035400970526     C*
035500970526     C                   ENDSR
035600970526     C*------------------------------------------------------------------------*
035700970526     C* F12GES - GESTIONE F12 -> RITORNO
035800970526     C*------------------------------------------------------------------------*
035900970526     C     F12GES        BEGSR
036000970526     C*
036100970526     C* IMPOSTA "PREMUTO F12"
036200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
036300970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
036400970526     C*
036500970526     C                   ENDSR
036600970602     C*------------------------------------------------------------------------*
036700970602     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
036800970602     C*------------------------------------------------------------------------*
036900970602     C     F13GES        BEGSR
037000970602     C*
037100970602     C                   MOVEL     VIDNAR        DNAR
037200970602     C                   MOVEL     VIDCAP        DCAP
037300970602     C                   MOVEL     VIDLOC        DLOC
037400970602     C                   MOVEL     DEPOP0        WOP0
037500970602     C                   MOVEL     'S'           WOP0
037600970602     C                   MOVEL     'F13'         WOP1
037700970602     C                   MOVEL     'TISI35R'     WPGM              8
037800970602     C                   EXSR      LANPGM
037900970602     C*
038000970602     C                   ENDSR
038100970603     C*------------------------------------------------------------------------*
038200970603     C* F14GES - GESTIONE F14 -> ZONE DI CONSEGNA
038300970603     C*------------------------------------------------------------------------*
038400970603     C     F14GES        BEGSR
038500970603     C*
038600970603     C                   CALL      'TRTB23R'
038700970603     C                   PARM                    KPJBA
038800970603     C*
038900970603     C                   ENDSR
039000130821     C*------------------------------------------------------------------------*
039100130827     C* F07GES - GESTIONE F07 -> Orari servizi
039200130821     C*------------------------------------------------------------------------*
039300130827     C     F07GES        BEGSR
039400130821     C*
039500130903     c                   exsr      F06ges
039600130903
039700130903     c                   if        errl00='0'
039800130903
039900130821     c                   clear                   tisio0ds
040000130821     C                   MOVEL     VIDNAR        do0NAR
040100130821     C                   MOVEL     VIDCAP        Do0CAP
040200130821     C                   MOVEL     VIDLOC        Do0LOC
040300130830     c                   if        %subst(depop0:2:2)='02'
040400130830     C                   MOVEL     'G'           do0tric
040500130830     c                   else
040600130830     C                   MOVEL     'I'           do0tric
040700130830     c                   endif
040800130821     c                   movel     d00dri        do0dri
040900130821     c                   movel     d00ver        do0ver
041000130821
041100130821     C                   call      'TISIO2R'
041200130821     C                   PARM                    KPJBA
041300130821     C                   PARM                    tisio0ds
041400130821     C*
041500130821     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
041600130821IF  1C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
041700130821     C                   MOVEL     '1'           ERRL00                         *ERRORE
041800130821     C                   MOVEL     '1'           F03L00                         *FINE PGM
041900130821X   1C                   ELSE                                                   *NON PREMUTO F03
042000130821     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
042100130821E   1C                   ENDIF
042200130821     C*
042300130903E   1c                   ENDIF
042400130821     C*
042500130821     C                   ENDSR
042600130821     C*------------------------------------------------------------------------*
042700970526     C*------------------------------------------------------------------------*
042800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
042900970526     C*------------------------------------------------------------------------*
043000970526     C     F24GES        BEGSR
043100970526     C*
043200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
043300970526     C*
043400970526     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
043700970526     C*------------------------------------------------------------------------*
043800970526     C     INSERI        BEGSR
043900970526     C*
044000970526     C* CONTROLLA I CAMPI IMMESSI
044100970526     C                   EXSR      DECODI
044200970618     C*
044300970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
044400970618IF  1C     ERRL00        IFEQ      '0'
044500970618     C                   EXSR      DECF06
044600970917IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
044700970530     C                   Z-ADD     D00VER        CPLVER                         *CHIAVE
044800970530     C                   MOVEL     VIDNAR        CPLNAR
044900970530     C                   MOVEL     VIDCAP        CPLCAP
045000970530     C                   MOVEL     VIDLOC        CPLLOC
045100970530     C                   EXSR      IMPCPL                                       *ALTRI CAMPI
045200970530     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
045300970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
045400970530     C                   MOVEL     *BLANKS       CPLATB                         *ANNULLAMENTO
045500980616     C                   MOVEL     'N'           CPLESS                         *ESISTENZA SINONIMI
045600980616     C                   MOVEL     'N'           CPLESV                         *ECCEIZONI X VIE
045700970530     C                   WRITE     AZCPL000
045800970527     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
045900970527     C*
046000970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
046100970527     C                   Z-ADD     D00VER        D81VER
046200970527     C                   MOVEL     VIDNAR        D81NAR
046300970527     C                   MOVEL     VIDCAP        D81CAP
046400970530     C                   MOVEL     'L'           D81CHK
046500970527     C                   CALL      'TISI81R'
046600970527     C                   PARM                    DSSI81
046700970602     C*
046800970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
046900970602     C                   EXSR      AZZVID
047000970618E   2C                   ENDIF
047100970618E   1C                   ENDIF
047200970526     C*
047300970526     C                   ENDSR
047400970526     C*------------------------------------------------------------------------*
047500970526     C* MODIFI - OPERAZIONI DI MODIFICA
047600970526     C*------------------------------------------------------------------------*
047700970526     C     MODIFI        BEGSR
047800970613     C*
047900970613     C* CONTROLLA I CAMPI IMMESSI
048000970613     C                   EXSR      DECODI
048100970618     C*
048200970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
048300970618IF  1C     ERRL00        IFEQ      '0'
048400970618     C                   EXSR      DECF06
048500970613     C*
048600970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
048700970613IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
048800970530     C                   EXSR      LETCPL
048900970526     C*
049000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
049100970613IF  3C     ERRL00        IFEQ      '0'                                          *NO ERRORI
049200970530     C                   EXSR      IMPCPL                                       *IMPOSTA I CAMPI
049300970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
049400970530     C                   MOVEL     *BLANKS       CPLATB                         *ANNULLAMENTO
049500970530     C                   UPDATE    AZCPL000
049600970526     C                   MOVEL     MSG(3)        D00MSG
049700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
049800970613E   3C                   ENDIF
049900970530     C*
050000970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
050100970530     C                   Z-ADD     D00VER        D81VER
050200970530     C                   MOVEL     VIDNAR        D81NAR
050300970530     C                   MOVEL     VIDCAP        D81CAP
050400970613     C                   MOVEL     'L'           D81CHK
050500970530     C                   CALL      'TISI81R'
050600970530     C                   PARM                    DSSI81
050700970613E   2C                   ENDIF
050800970618E   1C                   ENDIF
050900970526     C*
051000970526     C                   ENDSR
051100970526     C*------------------------------------------------------------------------*
051200970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
051300970526     C*------------------------------------------------------------------------*
051400970526     C     ANNULL        BEGSR
051500970526     C*
051600970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
051700000602     C                   MOVEL     '0'           ANNDLT            1
051800970530     C                   EXSR      LETCPL
051900970527     C*
052000000426     C* ANNULLA LE LOCALITA'
052100970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052200970530IF  2C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
052300970530IF  3C     CPLDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
052400970530     C                   MOVEL     'A'           CPLATB                         *FLAG ANNULLAMENTO
052500970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
052600970530     C                   UPDATE    AZCPL000                                     *ANNULLA
052700970526X   3C                   ELSE                                                   *MAI TRASMESSO
052800970530     C                   DELETE    AZCPL000                                     *CANCELLA
052900000602     C                   MOVEL     '1'           ANNDLT
053000970526E   3C                   ENDIF
053100970526E   2C                   ENDIF
053200970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
053300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
053400970925     C*
053500970925     C* ANNULLA I SINONIMI
053600970925     C                   Z-ADD     D00VER        KCSVER
053700970925     C                   MOVEL     CPLNAR        KCSNAR
053800970925     C                   MOVEL     CPLCAP        KCSCAP
053900970925     C                   MOVEL     CPLLOC        KCSLOC
054000970925     C     KEYCPS        SETLL     AZCPS01L                           99
054100970925     C     KEYCPS        READE     AZCPS01L                               99
054200970925DO  2C     *IN99         DOWEQ     *OFF
054300970925IF  3C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
054400970925IF  4C     CPSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
054500970925     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
054600970925     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
054700970925     C                   UPDATE    AZCPS000                                     *ANNULLA
054800970925X   4C                   ELSE                                                   *MAI TRASMESSO
054900970925     C                   DELETE    AZCPS000                                     *CANCELLA
055000970925E   4C                   ENDIF
055100970925E   3C                   ENDIF
055200970925     C     KEYCPS        READE     AZCPS01L                               99
055300970925E   2C                   ENDDO                                                  *FINE SINONIMI
055400970530     C*
055500970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
055600970530     C                   Z-ADD     D00VER        D81VER
055700970530     C                   MOVEL     VIDNAR        D81NAR
055800970530     C                   MOVEL     VIDCAP        D81CAP
055900970530     C                   MOVEL     'L'           D81CHK
056000970530     C                   CALL      'TISI81R'
056100970530     C                   PARM                    DSSI81
056200970526E   1C                   ENDIF
056300970526     C*
056400970526     C                   ENDSR
056500970526     C*------------------------------------------------------------------------*
056600970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
056700970526     C*------------------------------------------------------------------------*
056800970526     C     RIPRIS        BEGSR
056900970526     C*
057000970526     C* CONTROLLA I CAMPI IMMESSI
057100970526     C                   EXSR      DECODI
057200970618     C*
057300970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
057400970618IF  1C     ERRL00        IFEQ      '0'
057500970618     C                   EXSR      DECF06
057600970526     C*
057700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
057800970618IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
057900970530     C                   EXSR      LETCPL
058000970526     C*
058100970526     C* IMPOSTA IL RECORD DA SCRIVERE
058200970618IF  3C     ERRL00        IFEQ      '0'                                          *NO ERRORI
058300970530     C                   MOVEL     *BLANKS       CPLATB                         *FLAG ANNULLAMENTO
058400970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
058500970530     C                   UPDATE    AZCPL000                                     *ANNULLA
058600970618E   3C                   ENDIF
058700970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
058800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
058900970530     C*
059000970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
059100970530     C                   Z-ADD     D00VER        D81VER
059200970530     C                   MOVEL     VIDNAR        D81NAR
059300970530     C                   MOVEL     VIDCAP        D81CAP
059400970530     C                   MOVEL     'L'           D81CHK
059500970530     C                   CALL      'TISI81R'
059600970530     C                   PARM                    DSSI81
059700970618E   2C                   ENDIF
059800970618E   1C                   ENDIF
059900970526     C*
060000970526     C                   ENDSR
060100970526     C*------------------------------------------------------------------------*
060200970530     C* LETCPL - LEGGE IL RECORD PERL'AGGIORNAMENTO
060300970526     C*------------------------------------------------------------------------*
060400970530     C     LETCPL        BEGSR
060500970526     C*
060600000426     C     DEPOP1        IFNE      'F06'
060700970530     C                   Z-ADD     D00VER        KCLVER
060800980127     C                   MOVEL     D00NAR        KCLNAR
060900980127     C                   MOVEL     D00CAP        KCLCAP
061000980127     C                   MOVEL     D00LOC        KCLLOC
061100970530     C     KEYCPL        CHAIN     AZCPL01L                           9998
061200970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
061300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
061400970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
061500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
061600970526E   1C                   ENDIF
061700970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
061800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
061900970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
062000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
062100970526E   1C                   ENDIF
062200000426     C                   ENDIF
062300970526     C*
062400970526     C                   ENDSR
062500970526     C*------------------------------------------------------------------------*
062600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
062700970526     C*------------------------------------------------------------------------*
062800970526     C     DECODI        BEGSR
062900050428     C*
063000050428     C* NO VALORI NEGATIVI
063100050428     C                   IF        VIDLKS < *zeros
063200050428     C                   EVAL      VIDLKS = VIDLKS * -1
063300050428     C                   ENDIF
063400050428     C                   IF        VIDLKG < *zeros
063500050428     C                   EVAL      VIDLKG = VIDLKG * -1
063600050428     C                   ENDIF
063700050428     C                   IF        VIDLK2 < *zeros
063800050428     C                   EVAL      VIDLK2 = VIDLK2 * -1
063900050428     C                   ENDIF
064000050428     C                   IF        VIDLMS < *zeros
064100050428     C                   EVAL      VIDLMS = VIDLMS * -1
064200050428     C                   ENDIF
064300050428     C                   IF        VIDLMC < *zeros
064400050428     C                   EVAL      VIDLMC = VIDLMC * -1
064500050428     C                   ENDIF
064600050428     C                   IF        VIDLM2 < *zeros
064700050428     C                   EVAL      VIDLM2 = VIDLM2 * -1
064800050428     C                   ENDIF
064900050428     C*
065000970530     C*--------------------
065100970530     C* NAZIONE
065200970530     C*--------------------
065300970530     C     *LIKE         DEFINE    §15CPR        DEPCPR
065400970530IF  1C     ERRL00        IFEQ      '0'
065500970530     C                   MOVEL     *BLANKS       VDDNAR
065600970530     C                   MOVEL     VIDNAR        WNAR
065700970530     C                   MOVEL     *BLANKS       WDNAR
065800970530     C                   EXSR      CHKNAR
065900970530     C                   MOVEL     WNAR          VIDNAR
066000970530     C                   MOVEL     WDNAR         VDDNAR
066100970530IF  2C     ERRL00        IFEQ      '1'
066200970530     C                   SETON                                        60
066300970530     C                   MOVEL     MSG(13)       DSMSMS
066400970530E   2C                   ENDIF
066500970530E   1C                   ENDIF
066600970530     C*--------------------
066700970530     C* CAP
066800970530     C*--------------------
066900970530IF  1C     ERRL00        IFEQ      '0'
067000970530     C                   MOVEL     VIDNAR        WNAR
067100970530     C                   MOVEL     VIDCAP        WCAP
067200970917     C                   MOVEL     *BLANKS       WFI1
067300970530     C                   EXSR      CHKCAP
067400970530IF  2C     ERRL00        IFEQ      '1'
067500970530     C                   SETON                                        61
067600970602     C                   MOVEL     MSG(21)       DSMSMS
067700970530E   2C                   ENDIF
067800970530E   1C                   ENDIF
067900970530     C*--------------------
068000970530     C* LOCALITA'
068100970530     C*--------------------
068200970530IF  1C     ERRL00        IFEQ      '0'
068300970530IF  2C     VIDLOC        IFEQ      *BLANKS
068400970530     C                   SETON                                        70
068500970530     C                   MOVEL     '1'           ERRL00
068600970530     C                   MOVEL     MSG(22)       DSMSMS
068700970530E   2C                   ENDIF
068800970530E   1C                   ENDIF
068900970530     C*--------------------
069000970530     C* NAZIONE + CAP + LOCALITA'
069100970530     C*--------------------
069200970917IF  1C     DEPOP1        IFEQ      'F06'
069300970917     C*
069400970917     C* INSERIMENTO -> IL CAP NON PUO' ESSERE OBSOLETO
069500970917IF  2C     ERRL00        IFEQ      '0'
069600970917IF  3C     WFI1          IFEQ      'O '
069700970917     C                   SETON                                        6061
069800970917     C                   MOVEL     MSG(20)       DSMSMS
069900970917     C                   MOVEL     '1'           ERRL00
070000970917E   3C                   ENDIF
070100970917E   2C                   ENDIF
070200970917     C*
070300970530     C* INSERIMENTO -> RECORD INESISTENTE
070400970917IF  2C     ERRL00        IFEQ      '0'
070500970530     C                   Z-ADD     D00VER        KCLVER
070600970530     C                   MOVEL     VIDNAR        KCLNAR
070700970530     C                   MOVEL     VIDCAP        KCLCAP
070800970530     C                   MOVEL     VIDLOC        KCLLOC
070900970530     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
071000970917IF  3C     *IN99         IFEQ      *OFF
071100970530     C                   SETON                                        606170
071200970530     C                   MOVEL     '1'           ERRL00
071300970530     C                   MOVEL     MSG(8)        DSMSMS
071400970917E   3C                   ENDIF
071500970917E   2C                   ENDIF
071600970917E   1C                   ENDIF
071700970530     C*--------------------
071800970530     C* STAMPARE IN DELIVERY
071900970530     C*--------------------
072000970530IF  1C     ERRL00        IFEQ      '0'
072100970530IF  2C     VIDDLV        IFEQ      'S'
072200970530     C     VIDDLV        OREQ      'N'
072300970530X   2C                   ELSE
072400970530     C                   MOVEL     '1'           ERRL00
072500970530     C                   SETON                                        73
072600970530     C                   MOVEL     MSG(27)       DSMSMS
072700970530E   2C                   ENDIF
072800970530E   1C                   ENDIF
072900970917     C*--------------------
073000970917     C* OBSOLETO
073100970917     C*--------------------
073200970917IF  1C     ERRL00        IFEQ      '0'
073300970917IF  2C     VIDFI1        IFEQ      'O'
073400970917     C     VIDFI1        OREQ      *BLANKS
073500970917X   2C                   ELSE
073600970917     C                   MOVEL     '1'           ERRL00
073700970917     C                   SETON                                        85
073800970917     C                   MOVEL     MSG(25)       DSMSMS
073900970917E   2C                   ENDIF
074000970917E   1C                   ENDIF
074100970917     C*
074200970917     C* VARIAZIONE -> SE CAP OBSOLETO ANCHE LA LOCALITA' DEVE ESSERE OBSOLETA
074300970917IF  1C     ERRL00        IFEQ      '0'
074400970917IF  2C     DEPOP1        IFEQ      'O02'
074500970917IF  3C     WFI1          IFEQ      'O '
074600970917     C     VIDFI1        ANDNE     'O'
074700970917     C                   SETON                                        85
074800970917     C                   MOVEL     MSG(12)       DSMSMS
074900970917     C                   MOVEL     '1'           ERRL00
075000970917E   3C                   ENDIF
075100970917E   2C                   ENDIF
075200970917E   1C                   ENDIF
075300970530     C*--------------------
075400970530     C* LIVELLO DI GESTIONE
075500970530     C*--------------------
075600970530IF  1C     ERRL00        IFEQ      '0'
075700970530IF  2C     VIDLIV        IFEQ      'C'
075800970530     C     VIDLIV        OREQ      'L'
075900970530X   2C                   ELSE
076000970530     C                   SETON                                        74
076100970530     C                   MOVEL     '1'           ERRL00
076200970530     C                   MOVEL     MSG(23)       DSMSMS
076300970530E   2C                   ENDIF
076400970530E   1C                   ENDIF
076500970530     C*
076600970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> IMPORTA I DATI DEL CAPPARIO
076700970530IF  1C     ERRL00        IFEQ      '0'
076800970530IF  2C     VIDLIV        IFEQ      'C'                                          *LIVELLO CAP
076900970530     C                   SETON                                        92        *PROTEZIONE CAMPI
077000970530     C                   EXSR      IMPCPC
077100970530X   2C                   ELSE                                                   *LIVELLO LOCALITA
077200970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
077300970530E   2C                   ENDIF
077400970530E   1C                   ENDIF
077500970530     C*--------------------
077600970604     C* LINEA ARRIVO STANDARD
077700970530     C*--------------------
077800970530IF  1C     ERRL00        IFEQ      '0'
077900970530     C                   MOVEL     *BLANKS       VDDLNA
078000970530     C                   MOVEL     VIDLNA        WLIN
078100970530     C                   MOVEL     *BLANKS       WDLIN
078200970530     C                   EXSR      CHKLIN
078300970530     C                   MOVEL     WLIN          VIDLNA
078400970530     C                   MOVEL     WDLIN         VDDLNA
078500970530IF  2C     ERRL00        IFEQ      '1'
078600970530     C                   SETON                                        62
078700970530     C                   MOVEL     MSG(10)       DSMSMS
078800970530E   2C                   ENDIF
078900970530E   1C                   ENDIF
079000970604     C*--------------------
079100970604     C* ZONA DI CONSEGNA STANDARD
079200970604     C*--------------------
079300970604IF  1C     ERRL00        IFEQ      '0'
079400970604     C                   MOVEL     VIDLNA        WLIN
079500970604     C                   Z-ADD     VIDZNC        WZNC
079600970604     C                   EXSR      CHKZNC
079700970604IF  2C     ERRL00        IFEQ      '1'
079800970604     C                   SETON                                        63
079900970604     C                   MOVEL     MSG(11)       DSMSMS
080000970604E   2C                   ENDIF
080100060206if  2c                   if        §03nuc = 'S'
080200060206     c                   movel     Msg(17)       DSmsms
080300060206     c                   eval      *in63  = *on
080400060206     c                   eval      ErrL00 = *on
080500060206e   2c                   endif
080600970604E   1C                   ENDIF
080700970604     C*--------------------
080800970912     C* LINEA ARRIVO SOTTO
080900970604     C*--------------------
081000970604IF  1C     ERRL00        IFEQ      '0'
081100970604     C                   MOVEL     *BLANKS       VDDLOS
081200970604IF  2C     VIDLOS        IFNE      *ZEROS
081300970604     C     VIDLOS        ANDNE     *BLANKS
081400970604     C                   MOVEL     VIDLOS        WLIN
081500970604     C                   MOVEL     *BLANKS       WDLIN
081600970604     C                   EXSR      CHKLIN
081700970604     C                   MOVEL     WLIN          VIDLOS
081800970604     C                   MOVEL     WDLIN         VDDLOS
081900970604IF  3C     ERRL00        IFEQ      '1'
082000970604     C                   SETON                                        81
082100970604     C                   MOVEL     MSG(10)       DSMSMS
082200970604E   3C                   ENDIF
082300970604E   2C                   ENDIF
082400970604E   1C                   ENDIF
082500970604     C*--------------------
082600970912     C* ZONA DI CONSEGNA SOTTO
082700970604     C*--------------------
082800970604IF  1C     ERRL00        IFEQ      '0'
082900971126IF  2C     VIDLOS        IFNE      *ZEROS
083000970604     C                   MOVEL     VIDLOS        WLIN
083100970604     C                   Z-ADD     VIDZOS        WZNC
083200970604     C                   EXSR      CHKZNC
083300970604IF  3C     ERRL00        IFEQ      '1'
083400970604     C                   SETON                                        82
083500970604     C                   MOVEL     MSG(11)       DSMSMS
083600970604E   3C                   ENDIF
083700060206if  3c                   if        §03nuc = 'S'
083800060206     c                   movel     Msg(17)       DSmsms
083900060206     c                   eval      *in82  = *on
084000060206     c                   eval      ErrL00 = *on
084100060206e   3c                   endif
084200970604E   2C                   ENDIF
084300970604E   1C                   ENDIF
084400970604     C*--------------------
084500970912     C* PESO/VOLUME SOTTO
084600970604     C*--------------------
084700970604IF  1C     ERRL00        IFEQ      '0'
084800970604IF  2C     VIDLKS        IFGT      *ZEROS
084900970604     C     VIDLMS        ORGT      *ZEROS
085000970604IF  3C     VIDLOS        IFEQ      *BLANKS
085100970604     C     VIDLOS        OREQ      *ZEROS
085200970604     C                   SETON                                        81
085300970604     C                   MOVEL     MSG(18)       DSMSMS
085400970604     C                   MOVEL     '1'           ERRL00
085500970604E   3C                   ENDIF
085600070103E   2C                   ENDIF
085700070103IF  3C     VIDLOS        IFNE      *BLANKS
085800070103     C     VIDLOS        ANDNE     *ZEROS
085900070103IF  4C                   IF        VIDLKS = *ZEROS OR
086000070103     C                             VIDLMS = *ZEROS
086100970604     C                   SETON                                        8384
086200970604     C                   MOVEL     MSG(19)       DSMSMS
086300970604     C                   MOVEL     '1'           ERRL00
086400070103E   4C                   ENDIF
086500070103E   3C                   ENDIF
086600970604E   1C                   ENDIF
086700970530     C*--------------------
086800970912     C* LINEA ARRIVO OLTRE
086900970530     C*--------------------
087000970530IF  1C     ERRL00        IFEQ      '0'
087100970530     C                   MOVEL     *BLANKS       VDDLOL
087200970530IF  2C     VIDLOL        IFNE      *ZEROS
087300970530     C     VIDLOL        ANDNE     *BLANKS
087400970530     C                   MOVEL     VIDLOL        WLIN
087500970530     C                   MOVEL     *BLANKS       WDLIN
087600970530     C                   EXSR      CHKLIN
087700970530     C                   MOVEL     WLIN          VIDLOL
087800970530     C                   MOVEL     WDLIN         VDDLOL
087900970530IF  3C     ERRL00        IFEQ      '1'
088000970530     C                   SETON                                        67
088100970530     C                   MOVEL     MSG(10)       DSMSMS
088200970530E   3C                   ENDIF
088300970530E   2C                   ENDIF
088400970530E   1C                   ENDIF
088500970604     C*--------------------
088600970912     C* ZONA DI CONSEGNA OLTRE
088700970604     C*--------------------
088800970604IF  1C     ERRL00        IFEQ      '0'
088900971126IF  2C     VIDLOL        IFNE      *ZEROS
089000970604     C                   MOVEL     VIDLOL        WLIN
089100970604     C                   Z-ADD     VIDZOL        WZNC
089200970604     C                   EXSR      CHKZNC
089300970604IF  3C     ERRL00        IFEQ      '1'
089400970604     C                   SETON                                        68
089500970604     C                   MOVEL     MSG(11)       DSMSMS
089600970604E   3C                   ENDIF
089700060206if  3c                   if        §03nuc = 'S'
089800060206     c                   movel     Msg(17)       DSmsms
089900060206     c                   eval      *in68  = *on
090000060206     c                   eval      ErrL00 = *on
090100060206e   3c                   endif
090200970604E   2C                   ENDIF
090300970604E   1C                   ENDIF
090400970604     C*--------------------
090500970604     C* LIMITE PESO/VOLUME
090600970604     C*--------------------
090700970604IF  1C     ERRL00        IFEQ      '0'
090800970604IF  2C     VIDLKG        IFGT      *ZEROS
090900970604     C     VIDLMC        ORGT      *ZEROS
091000970604IF  3C     VIDLOL        IFEQ      *BLANKS
091100970604     C     VIDLOL        OREQ      *ZEROS
091200970604     C                   SETON                                        67
091300970604     C                   MOVEL     MSG(18)       DSMSMS
091400970604     C                   MOVEL     '1'           ERRL00
091500970604E   3C                   ENDIF
091600070103E   2C                   ENDIF
091700070103IF  3C     VIDLOL        IFNE      *BLANKS
091800070103     C     VIDLOL        ANDNE     *ZEROS
091900070103IF  4C                   IF        VIDLKG = *ZEROS OR
092000070103     C                             VIDLMC = *ZEROS
092100970604     C                   SETON                                        7172
092200970604     C                   MOVEL     MSG(19)       DSMSMS
092300970604     C                   MOVEL     '1'           ERRL00
092400070103E   4C                   ENDIF
092500070103E   3C                   ENDIF
092600970604E   1C                   ENDIF
092700041008     C*
092800041008IF  1C     ERRL00        IFEQ      '0'
092900041008IF  2C                   IF        (VIDLKG <> *zeros AND VIDLMC  = *zeros) OR
093000041008     C                             (VIDLKG  = *zeros AND VIDLMC <> *zeros)
093100041008     C                   SETON                                        7172
093200041008     C                   MOVEL     MSG(33)       DSMSMS
093300041008     C                   MOVEL     '1'           ERRL00
093400041008E   2C                   ENDIF
093500041008E   1C                   ENDIF
093600970604     C*--------------------
093700970912     C* PESO OLTRE + PESO SOTTO
093800970604     C*--------------------
093900970912     C* OLTRE > SOTTO
094000970604IF  1C     ERRL00        IFEQ      '0'
094100970604IF  2C     VIDLKG        IFGT      *ZEROS
094200970604     C     VIDLKS        ANDGT     *ZEROS
094300970604IF  3C     VIDLKG        IFLE      VIDLKS
094400970604     C                   SETON                                        7183
094500970604     C                   MOVEL     MSG(24)       DSMSMS
094600970604     C                   MOVEL     '1'           ERRL00
094700970604E   3C                   ENDIF
094800970604E   2C                   ENDIF
094900970604E   1C                   ENDIF
095000970604     C*--------------------
095100970912     C* VOLUME OLTRE + VOLUME SOTTO
095200970604     C*--------------------
095300970912     C* OLTRE > SOTTO
095400970604IF  1C     ERRL00        IFEQ      '0'
095500970604IF  2C     VIDLMC        IFGT      *ZEROS
095600970604     C     VIDLMS        ANDGT     *ZEROS
095700970604IF  3C     VIDLMC        IFLE      VIDLMS
095800970604     C                   SETON                                        7284
095900970604     C                   MOVEL     MSG(24)       DSMSMS
096000970604     C                   MOVEL     '1'           ERRL00
096100970604E   3C                   ENDIF
096200970604E   2C                   ENDIF
096300970604E   1C                   ENDIF
096400971118     C*--------------------
096500971118     C* LINEA ARRIVO OLTRE 2
096600971118     C*--------------------
096700971118IF  1C     ERRL00        IFEQ      '0'
096800971118     C                   MOVEL     *BLANKS       VDDLO2
096900971118IF  2C     VIDLO2        IFNE      *ZEROS
097000971118     C     VIDLO2        ANDNE     *BLANKS
097100971118     C                   MOVEL     VIDLO2        WLIN
097200971118     C                   MOVEL     *BLANKS       WDLIN
097300971118     C                   EXSR      CHKLIN
097400971118     C                   MOVEL     WLIN          VIDLO2
097500971118     C                   MOVEL     WDLIN         VDDLO2
097600971118IF  3C     ERRL00        IFEQ      '1'
097700971118     C                   SETON                                        86
097800971118     C                   MOVEL     MSG(10)       DSMSMS
097900971118E   3C                   ENDIF
098000971118E   2C                   ENDIF
098100971118E   1C                   ENDIF
098200971118     C*--------------------
098300971118     C* ZONA DI CONSEGNA OLTRE 2
098400971118     C*--------------------
098500971118IF  1C     ERRL00        IFEQ      '0'
098600971126IF  2C     VIDLO2        IFNE      *ZEROS
098700971118     C                   MOVEL     VIDLO2        WLIN
098800971118     C                   Z-ADD     VIDZO2        WZNC
098900971118     C                   EXSR      CHKZNC
099000971118IF  3C     ERRL00        IFEQ      '1'
099100971118     C                   SETON                                        87
099200971118     C                   MOVEL     MSG(11)       DSMSMS
099300971118E   3C                   ENDIF
099400060206if  3c                   if        §03nuc = 'S'
099500060206     c                   movel     Msg(17)       DSmsms
099600060206     c                   eval      *in87  = *on
099700060206     c                   eval      ErrL00 = *on
099800060206e   3c                   endif
099900971118E   2C                   ENDIF
100000971118E   1C                   ENDIF
100100971118     C*--------------------
100200971126     C* LIMITE PESO/VOLUME OLTRE 2
100300971118     C*--------------------
100400971126IF  1C*---       ERRL00    IFEQ '0'
100500971126IF  2C*---       VIDLK2    IFGT *ZEROS
100600971126     C*---       VIDLM2    ORGT *ZEROS
100700971126IF  3C*---       VIDLO2    IFEQ *BLANKS
100800971126     C*---       VIDLO2    OREQ *ZEROS
100900971126     C*---                 SETON                     86
101000971126     C*---                 MOVELMSG,18    DSMSMS
101100971126     C*---                 MOVEL'1'       ERRL00
101200971126E   3C*---                 ENDIF
101300971126X   2C*---                 ELSE
101400971126IF  3C*---       VIDLO2    IFNE *BLANKS
101500971126     C*---       VIDLO2    ANDNE*ZEROS
101600971126     C*---                 SETON                     8889
101700971126     C*---                 MOVELMSG,19    DSMSMS
101800971126     C*---                 MOVEL'1'       ERRL00
101900971126E   3C*---                 ENDIF
102000971126E   2C*---                 ENDIF
102100971126E   1C*---                 ENDIF
102200970530     C*--------------------
102300970530     C* LINEA ARRIVO FERMO DEPOSITO -OUTPUT-
102400970530     C*--------------------
102500970530IF  1C     ERRL00        IFEQ      '0'
102600971219     C                   MOVEL     VIDLNA        VIDLFD
102700971219     C                   MOVEL     *BLANKS       VDDLFD
102800970530IF  2C     VIDLFD        IFGT      *ZEROS
102900970530     C                   MOVEL     VIDLFD        WLIN
103000970530     C                   MOVEL     *BLANKS       WDLIN
103100970530     C                   EXSR      CHKLIN
103200970530     C                   MOVEL     WLIN          VIDLFD
103300970530     C                   MOVEL     WDLIN         VDDLFD
103400970530E   2C                   ENDIF
103500970530E   1C                   ENDIF
103600970530     C*--------------------
103700970530     C* ZONA DI CONSEGNA FERMO DEPOSITO
103800970530     C*--------------------
103900970530IF  1C     ERRL00        IFEQ      '0'
104000971126IF  2C     VIDLFD        IFNE      *ZEROS
104100970530     C                   MOVEL     VIDLFD        WLIN
104200970530     C                   Z-ADD     VIDZFD        WZNC
104300970530     C                   EXSR      CHKZNC
104400970530IF  3C     ERRL00        IFEQ      '1'
104500970530     C                   SETON                                        66
104600970530     C                   MOVEL     MSG(11)       DSMSMS
104700970530E   3C                   ENDIF
104800060206if  3c                   if        §03nuc = 'S'
104900060206     c                   movel     Msg(17)       DSmsms
105000060206     c                   eval      *in66  = *on
105100060206     c                   eval      ErrL00 = *on
105200060206e   3c                   endif
105300970530E   2C                   ENDIF
105400970530E   1C                   ENDIF
105500970530     C*--------------------
105600970530     C* CODICE TASSAZIONE
105700970530     C*--------------------
105800970530IF  1C     ERRL00        IFEQ      '0'
105900970530     C                   MOVEL     *BLANKS       VDDCTS
106000970530     C                   MOVEL     VIDCTS        WCTS
106100970530     C                   MOVEL     *BLANKS       WDCTS
106200970530     C                   EXSR      CHKCTS
106300970530     C                   MOVEL     WCTS          VIDCTS
106400970530     C                   MOVEL     WDCTS         VDDCTS
106500970530IF  2C     ERRL00        IFEQ      '1'
106600970530     C                   SETON                                        58
106700970530     C                   MOVEL     MSG(14)       DSMSMS
106800970530E   2C                   ENDIF
106900970530E   1C                   ENDIF
107000970530     C*--------------------
107100970530     C* CODICE TASSAZIONE ITALIA PER ESTERO
107200970530     C*--------------------
107300970530IF  1C     ERRL00        IFEQ      '0'
107400970530     C                   MOVEL     *BLANKS       VDDCTE
107500970530IF  2C     VIDNAR        IFEQ      *BLANKS                                      *NAZIONE ITALIA
107600970530     C                   MOVEL     VIDCTE        WA1D              1
107700970530     C                   MOVE      VIDCTE        WA1S              1
107800970530IF  3C     WA1D          IFNE      'I'
107900970530     C     WA1S          OREQ      *BLANKS
108000970530     C                   MOVEL     '1'           ERRL00
108100970530     C                   SETON                                        59
108200970530     C                   MOVEL     MSG(15)       DSMSMS
108300970530E   3C                   ENDIF
108400970530X   2C                   ELSE                                                   *NAZIONE ESTERO
108500970530IF  3C     VIDCTE        IFNE      *BLANKS
108600970530     C                   MOVEL     '1'           ERRL00
108700970530     C                   SETON                                        59
108800970530     C                   MOVEL     MSG(16)       DSMSMS
108900970530E   3C                   ENDIF
109000970530E   2C                   ENDIF
109100970530E   1C                   ENDIF
109200970530     C*--------------------
109300970530     C* INOLTRO
109400970530     C*--------------------
109500970530IF  1C     ERRL00        IFEQ      '0'
109600970530     C                   MOVEL     *BLANKS       VDDISO
109700970530     C                   MOVEL     VIDISO        WINL
109800970530     C                   MOVEL     *BLANKS       WDINL
109900970530     C                   EXSR      CHKINL
110000970530     C                   MOVEL     WINL          VIDISO
110100970530     C                   MOVEL     WDINL         VDDISO
110200970530IF  2C     ERRL00        IFEQ      '1'
110300970530     C                   SETON                                        75
110400970530     C                   MOVEL     MSG(28)       DSMSMS
110500970530E   2C                   ENDIF
110600970530E   1C                   ENDIF
110700970530     C*--------------------
110800970530     C* POSSIBILE INOLTRO ISOLA
110900970530     C*--------------------
111000970530IF  1C     ERRL00        IFEQ      '0'
111100970530IF  2C     VIDPII        IFEQ      'S'
111200970530     C     VIDPII        OREQ      'N'
111300970530X   2C                   ELSE
111400970530     C                   MOVEL     '1'           ERRL00
111500970530     C                   SETON                                        76
111600970530     C                   MOVEL     MSG(27)       DSMSMS
111700970530E   2C                   ENDIF
111800970530E   1C                   ENDIF
111900970530     C*--------------------
112000970530     C* POSSIBILE INOLTRO LOCALITA' DISAGIATA
112100970530     C*--------------------
112200970530IF  1C     ERRL00        IFEQ      '0'
112300970530IF  2C     VIDPID        IFEQ      'S'
112400970530     C     VIDPID        OREQ      'N'
112500970530X   2C                   ELSE
112600970530     C                   MOVEL     '1'           ERRL00
112700970530     C                   SETON                                        77
112800970530     C                   MOVEL     MSG(27)       DSMSMS
112900970530E   2C                   ENDIF
113000970530E   1C                   ENDIF
113100970530     C*--------------------
113200970530     C* POSSIBILE INOLTRO CENTRO STORICO
113300970530     C*--------------------
113400970530IF  1C     ERRL00        IFEQ      '0'
113500970530IF  2C     VIDPIS        IFEQ      'S'
113600970530     C     VIDPIS        OREQ      'N'
113700970530X   2C                   ELSE
113800970530     C                   MOVEL     '1'           ERRL00
113900970530     C                   SETON                                        78
114000970530     C                   MOVEL     MSG(27)       DSMSMS
114100970530E   2C                   ENDIF
114200970530E   1C                   ENDIF
114300970530     C*--------------------
114400970530     C* GESTIONE DDT
114500970530     C*--------------------
114600970530IF  1C     ERRL00        IFEQ      '0'
114700970530IF  2C     VIDGDD        IFEQ      'S'
114800970530     C     VIDGDD        OREQ      'N'
114900970530X   2C                   ELSE
115000970530     C                   MOVEL     '1'           ERRL00
115100970530     C                   SETON                                        79
115200970530     C                   MOVEL     MSG(27)       DSMSMS
115300970530E   2C                   ENDIF
115400970530E   1C                   ENDIF
115500970530     C*--------------------
115600970530     C* TIPO TEMPO CONSEGNA
115700970530     C*--------------------
115800970530IF  1C     ERRL00        IFEQ      '0'
115900010322     C                   MOVEL     VIDTTC        WTTC                           *CODICE TEMPO
116000970530     C                   Z-ADD     *ZEROS        WD0I                           *TEMPI DISTRIBUZIONE
116100970530     C                   Z-ADD     *ZEROS        WD1I
116200970530     C                   Z-ADD     *ZEROS        WD0C
116300970530     C                   Z-ADD     *ZEROS        WD1C
116400970530     C                   Z-ADD     *ZEROS        WDIS
116500970530     C                   Z-ADD     *ZEROS        WDCS
116600970530     C                   Z-ADD     *ZEROS        WDLD
116700090903
116800970530     C                   Z-ADD     *ZEROS        WC0I                           *TEMPI CORRIERE
116900970530     C                   Z-ADD     *ZEROS        WC1I
117000970530     C                   Z-ADD     *ZEROS        WC0C
117100970530     C                   Z-ADD     *ZEROS        WC1C
117200970530     C                   Z-ADD     *ZEROS        WCIS
117300970530     C                   Z-ADD     *ZEROS        WCCS
117400970530     C                   Z-ADD     *ZEROS        WCLD
117500090903
117600970530     C                   Z-ADD     *ZEROS        WE0I                           *TEMPI ESPRESSO
117700970530     C                   Z-ADD     *ZEROS        WE1I
117800970530     C                   Z-ADD     *ZEROS        WE0C
117900970530     C                   Z-ADD     *ZEROS        WE1C
118000970530     C                   Z-ADD     *ZEROS        WEIS
118100970530     C                   Z-ADD     *ZEROS        WECS
118200970530     C                   Z-ADD     *ZEROS        WELD
118300090903
118400090903     C                   Z-ADD     *ZEROS        Wh0I                           *TEMPI H1030
118500090903     C                   Z-ADD     *ZEROS        Wh1I
118600090903     C                   Z-ADD     *ZEROS        Wh0C
118700090903     C                   Z-ADD     *ZEROS        Wh1C
118800090903     C                   Z-ADD     *ZEROS        WhIS
118900090903     C                   Z-ADD     *ZEROS        WhCS
119000090903     C                   Z-ADD     *ZEROS        WhLD
119100090903
119200970530     C                   EXSR      CHKTTC
119300970530     C                   MOVEL     WTTC          VIDTTC                         *CODICE TEMPO
119400970530     C                   Z-ADD     WD0I          VIDD0I                         *TEMPI DISTRIBUZIONE
119500970530     C                   Z-ADD     WD1I          VIDD1I
119600970530     C                   Z-ADD     WD0C          VIDD0C
119700970530     C                   Z-ADD     WD1C          VIDD1C
119800970530     C                   Z-ADD     WDIS          VIDDIS
119900970530     C                   Z-ADD     WDCS          VIDDCS
120000970530     C                   Z-ADD     WDLD          VIDDLD
120100090903
120200970530     C                   Z-ADD     WC0I          VIDC0I                         *TEMPI CORRIERE
120300970530     C                   Z-ADD     WC1I          VIDC1I
120400970530     C                   Z-ADD     WC0C          VIDC0C
120500970530     C                   Z-ADD     WC1C          VIDC1C
120600970530     C                   Z-ADD     WCIS          VIDCIS
120700970530     C                   Z-ADD     WCCS          VIDCCS
120800970530     C                   Z-ADD     WCLD          VIDCLD
120900090903
121000970530     C                   Z-ADD     WE0I          VIDE0I                         *TEMPI ESPRESSO
121100970530     C                   Z-ADD     WE1I          VIDE1I
121200970530     C                   Z-ADD     WE0C          VIDE0C
121300970530     C                   Z-ADD     WE1C          VIDE1C
121400970530     C                   Z-ADD     WEIS          VIDEIS
121500970530     C                   Z-ADD     WECS          VIDECS
121600970530     C                   Z-ADD     WELD          VIDELD
121700090903
121800090903     C                   Z-ADD     Wh0I          VIDh0I                         *TEMPI H1030
121900090903     C                   Z-ADD     Wh1I          VIDh1I
122000090903     C                   Z-ADD     Wh0C          VIDh0C
122100090903     C                   Z-ADD     Wh1C          VIDh1C
122200090903     C                   Z-ADD     WhIS          VIDhIS
122300090903     C                   Z-ADD     WhCS          VIDhCS
122400090903     C                   Z-ADD     WhLD          VIDhLD
122500970530IF  2C     ERRL00        IFEQ      '1'
122600970530     C                   SETON                                        80
122700970530     C                   MOVEL     MSG(30)       DSMSMS
122800970530E   2C                   ENDIF
122900970530E   1C                   ENDIF
123000010322     C*--------------------
123100010322     C* TIPO TEMPO RITIRO
123200010322     C*--------------------
123300010322IF  1C     ERRL00        IFEQ      '0'
123400010322     C                   MOVEL     VIDTTR        WTTR                           *CODICE TEMPO
123500010322     C                   Z-ADD     *ZEROS        WD0IR                          *TEMPI DISTRIBUZIONE
123600010322     C                   Z-ADD     *ZEROS        WD1IR
123700010322     C                   Z-ADD     *ZEROS        WD0CR
123800010322     C                   Z-ADD     *ZEROS        WD1CR
123900010322     C                   Z-ADD     *ZEROS        WDISR
124000010322     C                   Z-ADD     *ZEROS        WDCSR
124100010322     C                   Z-ADD     *ZEROS        WDLDR
124200010322     C                   Z-ADD     *ZEROS        WC0IR                          *TEMPI CORRIERE
124300010322     C                   Z-ADD     *ZEROS        WC1IR
124400010322     C                   Z-ADD     *ZEROS        WC0CR
124500010322     C                   Z-ADD     *ZEROS        WC1CR
124600010322     C                   Z-ADD     *ZEROS        WCISR
124700010322     C                   Z-ADD     *ZEROS        WCCSR
124800010322     C                   Z-ADD     *ZEROS        WCLDR
124900010322     C                   Z-ADD     *ZEROS        WE0IR                          *TEMPI ESPRESSO
125000010322     C                   Z-ADD     *ZEROS        WE1IR
125100010322     C                   Z-ADD     *ZEROS        WE0CR
125200010322     C                   Z-ADD     *ZEROS        WE1CR
125300010322     C                   Z-ADD     *ZEROS        WEISR
125400010322     C                   Z-ADD     *ZEROS        WECSR
125500010322     C                   Z-ADD     *ZEROS        WELDR
125600010322     C                   EXSR      CHKTTR
125700010322     C                   MOVEL     WTTR          VIDTTR                         *CODICE TEMPO
125800010322     C                   Z-ADD     WD0IR         VIDD0IR                        *TEMPI DISTRIBUZIONE
125900010322     C                   Z-ADD     WD1IR         VIDD1IR
126000010322     C                   Z-ADD     WD0CR         VIDD0CR
126100010322     C                   Z-ADD     WD1CR         VIDD1CR
126200010322     C                   Z-ADD     WDISR         VIDDISR
126300010322     C                   Z-ADD     WDCSR         VIDDCSR
126400010322     C                   Z-ADD     WDLDR         VIDDLDR
126500010322     C                   Z-ADD     WC0IR         VIDC0IR                        *TEMPI CORRIERE
126600010322     C                   Z-ADD     WC1IR         VIDC1IR
126700010322     C                   Z-ADD     WC0CR         VIDC0CR
126800010322     C                   Z-ADD     WC1CR         VIDC1CR
126900010322     C                   Z-ADD     WCISR         VIDCISR
127000010322     C                   Z-ADD     WCCSR         VIDCCSR
127100010322     C                   Z-ADD     WCLDR         VIDCLDR
127200010322     C                   Z-ADD     WE0IR         VIDE0IR                        *TEMPI ESPRESSO
127300010322     C                   Z-ADD     WE1IR         VIDE1IR
127400010322     C                   Z-ADD     WE0CR         VIDE0CR
127500010322     C                   Z-ADD     WE1CR         VIDE1CR
127600010322     C                   Z-ADD     WEISR         VIDEISR
127700010322     C                   Z-ADD     WECSR         VIDECSR
127800010322     C                   Z-ADD     WELDR         VIDELDR
127900010322IF  2C     ERRL00        IFEQ      '1'
128000010322     C                   SETON                                        44
128100010322     C                   MOVEL     MSG(32)       DSMSMS
128200010322E   2C                   ENDIF
128300010322E   1C                   ENDIF
128400970530     C*
128500970530     C                   ENDSR
128600970618     C*------------------------------------------------------------------------*
128700970618     C* DECF06 - CONTROLLA CAMPI E DECODIFICA -SOLO ALLA CONFERMA-
128800970618     C*------------------------------------------------------------------------*
128900970618     C     DECF06        BEGSR
129000970618     C*--------------------
129100970618     C* LOCALITA' GESTITA A LIVELLO DI LOCALITA'
129200970618     C*--------------------
129300970618     C* DATI CAP + DATI LOCALITA'
129400091211IF  1C*    ERRL00        IFEQ      '0'
129500091211IF  2C*    VIDLIV        IFEQ      'L'                                          *LOCALITA' GESTITA
129600091211     C*                  MOVEL     VIDLNA        NLNA                           *CAMPI NUMERICI
129700091211     C*                  MOVEL     VIDLOS        NLOS
129800091211     C*                  MOVEL     VIDLOL        NLOL
129900091211     C*                  MOVEL     VIDLO2        NLO2
130000091211     C*                  MOVEL     VIDTTC        NTTC
130100091211     C*                  MOVE      VIDTTR        NTTR
130200091211IF  3C*    NLNA          IFEQ      CPCLNA
130300091211     C*    VIDZNC        ANDEQ     CPCZNC
130400091211     C*    VIDISO        ANDEQ     CPCISO
130500091211     C*    VIDZFD        ANDEQ     CPCZFD
130600091211     C*    VIDLKG        ANDEQ     CPCLKG
130700091211     C*    VIDLMC        ANDEQ     CPCLMC
130800091211     C*    NLOL          ANDEQ     CPCLOL
130900091211     C*    VIDZOL        ANDEQ     CPCZOL
131000091211     C*    VIDLKS        ANDEQ     CPCLKS
131100091211     C*    VIDLMS        ANDEQ     CPCLMS
131200091211     C*    NLO2          ANDEQ     CPCLO2
131300091211     C*    VIDZO2        ANDEQ     CPCZO2
131400091211     C*    VIDLK2        ANDEQ     CPCLK2
131500091211     C*    VIDLM2        ANDEQ     CPCLM2
131600091211     C*    NLOS          ANDEQ     CPCLOS
131700091211     C*    VIDZOS        ANDEQ     CPCZOS
131800091211     C*    VIDCTS        ANDEQ     CPCCTS
131900091211     C*    VIDCTE        ANDEQ     CPCCTE
132000091211     C*    NTTC          ANDEQ     CPCTTC
132100091211     C*    NTTR          ANDEQ     CPCFI2
132200091211     C*    VIDPII        ANDEQ     CPCPII
132300091211     C*    VIDPID        ANDEQ     CPCPID
132400091211     C*    VIDPIS        ANDEQ     CPCPIS
132500091211     C*    VIDGDD        ANDEQ     CPCGDD
132600091211     C*                  SETON                                        606170
132700091211     C*                  SETON                                        74
132800091211     C*                  MOVEL     MSG(29)       DSMSMS
132900091211     C*                  MOVEL     '1'           ERRL00
133000091211E   3C*                  ENDIF
133100091211E   2C*                  ENDIF
133200091211E   1C*                  ENDIF
133300970618     C*
133400970618     C* LIVELLO DI GESTIONE = CAP SE LA LOCALITA' E' UNICA
133500970618IF  1C     ERRL00        IFEQ      '0'
133600970618IF  2C     VIDLIV        IFEQ      'L'                                          *LOCALITA' GESTITA
133700970618IF  3C     DEPOP1        IFEQ      'F06'
133800970618     C                   Z-ADD     1             I
133900970618X   3C                   ELSE
134000970618     C                   Z-ADD     *ZEROS        I
134100970618E   3C                   ENDIF
134200970618     C                   Z-ADD     D00VER        KCLVER
134300970618     C                   MOVEL     VIDNAR        KCLNAR
134400970618     C                   MOVEL     VIDCAP        KCLCAP
134500970618     C     KE2CPL        SETLL     AZCPL01L
134600970618     C     KE2CPL        READE(N)  AZCPL01L                               99
134700970618DO  3C     *IN99         DOWEQ     *OFF
134800970618IF  4C     CPLATB        IFEQ      *BLANKS
134900970618     C                   ADD       1             I                              *NUMERO LOCALITA'
135000970618E   4C                   ENDIF
135100970618     C     KE2CPL        READE(N)  AZCPL01L                               99
135200970618E   3C                   ENDDO
135300970618IF  3C     I             IFEQ      1
135400970618     C                   SETON                                        606170
135500970618     C                   SETON                                        74
135600970618     C                   MOVEL     MSG(26)       DSMSMS
135700970618     C                   MOVEL     '1'           ERRL00
135800970618E   3C                   ENDIF
135900970618E   2C                   ENDIF
136000970618E   1C                   ENDIF
136100970618     C*
136200970618     C                   ENDSR
136300970530     C*------------------------------------------------------------------------*
136400970530     C* CHKNAR - CONTROLLA LA NAZIONE
136500970530     C*------------------------------------------------------------------------*
136600970530     C     CHKNAR        BEGSR
136700970530     C*
136800970530     C     *LIKE         DEFINE    VIDNAR        WNAR
136900970530     C     *LIKE         DEFINE    VDDNAR        WDNAR
137000970530     C*
137100970530     C* RICHIESTO ELENCO
137200970530     C                   SETOFF                                           28
137300970530     C     '?'           SCAN      WNAR                                   28
137400970530IF  1C     *IN28         IFEQ      *ON
137500020906     C                   Z-ADD     1             §KUT              1 0
137600970530     C                   MOVEL     '15'          §COD              2
137700970530     C                   MOVEL     *BLANKS       §KEY              8
137800970530     C                   MOVEL     *BLANKS       §DES             30
137900970530     C                   CALL      'X§TABER'
138000970530     C                   PARM                    §KUT
138100970530     C                   PARM                    §COD
138200970530     C                   PARM                    §KEY
138300970530     C                   PARM                    §DES
138400970530     C                   MOVEL     §KEY          WNAR
138500970530     C                   MOVEL     §DES          WDNAR
138600970530E   1C                   ENDIF
138700970530     C*
138800970530     C* CONTROLLA ESISTENZA CODICE
138900020906     C                   MOVEL     1             KTBKUT
139000970530     C                   MOVEL     '15'          KTBCOD
139100970530     C                   MOVEL(P)  WNAR          KTBKEY
139200970530     C     KEYTAB        CHAIN     TABEL00F                           99
139300970530IF  1C     *IN99         IFEQ      *OFF
139400970530     C     TBLFLG        ANDEQ     *BLANKS
139500970530     C                   MOVEL     TBLUNI        DS15
139600970530     C                   MOVEL     §15DES        WDNAR
139700970530     C*
139800970530     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
139900970530IF  2C     VIDNAR        IFEQ      'I  '
140000970530     C                   MOVEL     '1'           ERRL00
140100970530     C                   MOVEL     *BLANKS       WDNAR
140200970530E   2C                   ENDIF
140300970530X   1C                   ELSE
140400970530     C                   MOVEL     '1'           ERRL00
140500970530E   1C                   ENDIF
140600970530     C*
140700970530     C                   ENDSR
140800970530     C*------------------------------------------------------------------------*
140900970530     C* CHKCAP - CONTROLLA IL CAP
141000970530     C*------------------------------------------------------------------------*
141100970530     C     CHKCAP        BEGSR
141200970530     C*
141300970530     C     *LIKE         DEFINE    VIDCAP        WCAP
141400970917     C     *LIKE         DEFINE    O95CF1        WFI1
141500970626     C*
141600970626     C* CONTROLLA IL CAP
141700970626     C                   CLEAR                   DSSI95
141800970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
141900970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
142000970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
142100970626     C                   MOVEL     WCAP          I95CAP                         *CAP
142200970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
142300970626     C                   CALL      'TISI95R'
142400970626     C                   PARM                    DSSI95
142500970626IF  1C     O95ERR        IFNE      *BLANKS
142600970626     C                   MOVEL     '1'           ERRL00
142700970917X   1C                   ELSE
142800970917     C                   MOVEL     O95CF1        WFI1
142900970917E   1C                   ENDIF
143000970530     C*
143100970530     C                   ENDSR
143200970530     C*------------------------------------------------------------------------*
143300970530     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
143400970530     C*------------------------------------------------------------------------*
143500970530     C     CHKLIN        BEGSR
143600970530     C*
143700970530     C     *LIKE         DEFINE    VIDLNA        WLIN
143800970530     C     *LIKE         DEFINE    VDDLNA        WDLIN
143900970530     C*
144000970530     C* RICHIESTO ELENCO
144100970530     C                   SETOFF                                           28
144200970530     C     '?'           SCAN      WLIN                                   28
144300970530IF  1C     *IN28         IFEQ      *ON
144400970530     C                   MOVEL     *BLANKS       §COD1             3
144500970530     C                   MOVEL     *BLANKS       §TIP              1
144600970530     C                   MOVEL     *BLANKS       §DES1            25
144700971203     C                   MOVEL     *BLANKS       §PRA              3
144800971203     C                   CALL      'TNSD24R'
144900970530     C                   PARM                    §COD1
145000970530     C                   PARM                    §TIP
145100970530     C                   PARM                    §DES1
145200971203     C                   PARM                    §PRA
145300970530     C                   MOVEL     §COD1         WLIN
145400970530     C                   MOVEL     §DES1         WDLIN
145500970530E   1C                   ENDIF
145600970530     C*
145700970530     C* SOLO CARATTERI NUMERICI
145800970530     C                   SETOFF                                       28
145900970530     C                   TESTN                   WLIN                 28
146000970530IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
146100970530     C                   MOVEL     '1'           ERRL00
146200970530X   1C                   ELSE                                                   *E'NUMERO DA TESTN
146300970530     C                   MOVE      WLIN          W001              1
146400970530IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
146500970530     C                   MOVEL     '1'           ERRL00
146600970530X   2C                   ELSE                                                   *NUMERO
146700970530     C*
146800970530     C* CONTROLLA ESISTENZA CODICE
146900970530     C                   MOVEL     WLIN          KORFIL
147000970530     C     KEYORG        CHAIN     AZORG01L                           99
147100970530IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
147200970530     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
147300970530     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
147400970530     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
147500970530X   3C                   ELSE
147600970530     C                   MOVEL     '1'           ERRL00
147700970530E   3C                   ENDIF
147800970530E   2C                   ENDIF
147900970530E   1C                   ENDIF
148000970530     C*
148100970530     C                   ENDSR
148200970530     C*------------------------------------------------------------------------*
148300970530     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
148400970530     C*------------------------------------------------------------------------*
148500970530     C     CHKZNC        BEGSR
148600970530     C*
148700970530     C     *LIKE         DEFINE    VIDZNC        WZNC
148800970530     C*
148900970530     C* CONTROLLA ESISTENZA CODICE
149000020906     C                   Z-ADD     1             KTBKUT
149100970530     C                   MOVEL     '03'          KTBCOD
149200970530     C                   MOVEL     WLIN          WA5               5
149300970530     C                   MOVE      WZNC          WA5
149400970530     C                   MOVEL(P)  WA5           KTBKEY
149500970530     C     KEYTAB        CHAIN     TABEL00F                           99
149600970530IF  1C     *IN99         IFEQ      *OFF
149700970530     C     TBLFLG        ANDEQ     *BLANKS
149800970530     C                   MOVEL     TBLUNI        DS03
149900970530X   1C                   ELSE
150000970530     C                   MOVEL     '1'           ERRL00
150100970530E   1C                   ENDIF
150200970530     C*
150300970530     C                   ENDSR
150400970530     C*------------------------------------------------------------------------*
150500970530     C* CHKCTS - CONTROLLA IL CODICE TASSAZIONE
150600970530     C*------------------------------------------------------------------------*
150700970530     C     CHKCTS        BEGSR
150800970530     C*
150900970530     C     *LIKE         DEFINE    VIDCTS        WCTS
151000970530     C     *LIKE         DEFINE    VDDCTS        WDCTS
151100970530     C*
151200970530     C* RICHIESTO ELENCO
151300970530     C                   SETOFF                                           28
151400970530     C     '?'           SCAN      WCTS                                   28
151500970530IF  1C     *IN28         IFEQ      *ON
151600020906     C                   Z-ADD     1             §KUT              1 0
151700970530     C                   MOVEL     'CT'          §COD              2
151800970530     C                   MOVEL     *BLANKS       §KEY              8
151900970530     C                   MOVEL     *BLANKS       §DES             30
152000970530     C                   CALL      'X§TABER'
152100970530     C                   PARM                    §KUT
152200970530     C                   PARM                    §COD
152300970530     C                   PARM                    §KEY
152400970530     C                   PARM                    §DES
152500970530     C                   MOVEL     §KEY          WCTS
152600970530     C                   MOVEL     §DES          WDCTS
152700970530E   1C                   ENDIF
152800970530     C*
152900970530     C* CONTROLLA ESISTENZA CODICE
153000020906     C                   MOVEL     1             KTBKUT
153100970530     C                   MOVEL     'CT'          KTBCOD
153200970530     C                   MOVEL(P)  WCTS          KTBKEY
153300970530     C     KEYTAB        CHAIN     TABEL00F                           99
153400970530IF  1C     *IN99         IFEQ      *OFF
153500970530     C     TBLFLG        ANDEQ     *BLANKS
153600970530     C                   MOVEL     TBLUNI        DSCT
153700970530     C                   MOVEL     §CTDES        WDCTS
153800970530X   1C                   ELSE
153900970530     C                   MOVEL     '1'           ERRL00
154000970530E   1C                   ENDIF
154100970530     C*
154200970530     C                   ENDSR
154300970530     C*------------------------------------------------------------------------*
154400970530     C* CHKINL - CONTROLLA L'INOLTRO
154500970530     C*------------------------------------------------------------------------*
154600970530     C     CHKINL        BEGSR
154700970530     C*
154800970530     C     *LIKE         DEFINE    VIDISO        WINL
154900970530     C     *LIKE         DEFINE    VDDISO        WDINL
155000970530     C*
155100970530     C* RICHIESTO ELENCO
155200970530     C                   SETOFF                                           28
155300970530     C     '?'           SCAN      WINL                                   28
155400970530IF  1C     *IN28         IFEQ      *ON
155500020906     C                   Z-ADD     1             §KUT              1 0
155600970530     C                   MOVEL     '9A'          §COD              2
155700970530     C                   MOVEL     *BLANKS       §KEY              8
155800970530     C                   MOVEL     *BLANKS       §DES             30
155900970530     C                   CALL      'X§TABER'
156000970530     C                   PARM                    §KUT
156100970530     C                   PARM                    §COD
156200970530     C                   PARM                    §KEY
156300970530     C                   PARM                    §DES
156400970530     C                   MOVEL     §KEY          WINL
156500970530     C                   MOVEL     §DES          WDINL
156600970530E   1C                   ENDIF
156700970530     C*
156800970530     C* CONTROLLA ESISTENZA CODICE
156900020906     C                   MOVEL     1             KTBKUT
157000970530     C                   MOVEL     '9A'          KTBCOD
157100970530     C                   MOVEL(P)  WINL          KTBKEY
157200970530     C     KEYTAB        CHAIN     TABEL00F                           99
157300970530IF  1C     *IN99         IFEQ      *OFF
157400970530     C     TBLFLG        ANDEQ     *BLANKS
157500970530     C                   MOVEL     TBLUNI        DS9A
157600970530     C                   MOVEL     §9ADES        WDINL
157700970530X   1C                   ELSE
157800970530     C                   MOVEL     '1'           ERRL00
157900970530E   1C                   ENDIF
158000110623     c* Se non utilizzabile  nel cappario errore
158100110623     c                   if        §9auticpc='N'  and errl00='0'
158200110623     C                   MOVEL     '1'           ERRL00
158300110623E   1C                   ENDIF
158400970530     C*
158500970530     C                   ENDSR
158600970530     C*------------------------------------------------------------------------*
158700970530     C* CHKTTC - CONTROLLA IL TIPO TEMPO CONSEGNA
158800970530     C*------------------------------------------------------------------------*
158900970530     C     CHKTTC        BEGSR
159000970530     C*
159100970530     C     *LIKE         DEFINE    VIDTTC        WTTC                           *TIPO TEMPO
159200970530     C     *LIKE         DEFINE    CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
159300970530     C     *LIKE         DEFINE    CPT1IN        WD1I
159400970530     C     *LIKE         DEFINE    CPT0CN        WD0C
159500970530     C     *LIKE         DEFINE    CPT1CN        WD1C
159600970530     C     *LIKE         DEFINE    CPTTIS        WDIS
159700970530     C     *LIKE         DEFINE    CPTTCS        WDCS
159800970530     C     *LIKE         DEFINE    CPTTLD        WDLD
159900970530     C     *LIKE         DEFINE    CPT0IN        WC0I                           *TEMPI CORRIERE
160000970530     C     *LIKE         DEFINE    CPT1IN        WC1I
160100970530     C     *LIKE         DEFINE    CPT0CN        WC0C
160200970530     C     *LIKE         DEFINE    CPT1CN        WC1C
160300970530     C     *LIKE         DEFINE    CPTTIS        WCIS
160400970530     C     *LIKE         DEFINE    CPTTCS        WCCS
160500970530     C     *LIKE         DEFINE    CPTTLD        WCLD
160600090903
160700970530     C     *LIKE         DEFINE    CPT0IN        WE0I                           *TEMPI ESPRESSO
160800970530     C     *LIKE         DEFINE    CPT1IN        WE1I
160900970530     C     *LIKE         DEFINE    CPT0CN        WE0C
161000970530     C     *LIKE         DEFINE    CPT1CN        WE1C
161100970530     C     *LIKE         DEFINE    CPTTIS        WEIS
161200970530     C     *LIKE         DEFINE    CPTTCS        WECS
161300970530     C     *LIKE         DEFINE    CPTTLD        WELD
161400090903
161500090903     C     *LIKE         DEFINE    CPT0IN        Wh0I                           *TEMPI H1030
161600090903     C     *LIKE         DEFINE    CPT1IN        Wh1I
161700090903     C     *LIKE         DEFINE    CPT0CN        Wh0C
161800090903     C     *LIKE         DEFINE    CPT1CN        Wh1C
161900090903     C     *LIKE         DEFINE    CPTTIS        WhIS
162000090903     C     *LIKE         DEFINE    CPTTCS        WhCS
162100090903     C     *LIKE         DEFINE    CPTTLD        WhLD
162200970530     C*
162300970530     C* RICHIESTA ELENCO CODICI
162400970530     C                   SETOFF                                           28
162500970530     C     '?'           SCAN      WTTC                                   28
162600970530IF  1C     *IN28         IFEQ      *ON
162700970602     C                   MOVEL     *BLANKS       DNAR
162800970602     C                   MOVEL     *BLANKS       DCAP
162900970602     C                   MOVEL     *BLANKS       DLOC
163000970602     C                   MOVEL     'T01'         WOP0
163100970530     C                   MOVEL     *BLANKS       WOP1
163200970530     C                   MOVEL     'TISI41R'     WPGM              8
163300970530     C                   EXSR      LANPGM
163400970530     C                   MOVEL     D00TTC        WTTC
163500970530E   1C                   ENDIF
163600970530     C*
163700010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO CONSEGNA
163800010322     C                   MOVEL     VIDTTC        DEP3A             3
163900010322     C     DEP3A         IFNE      '999'
164000970530     C* CONTROLLA ESISTENZA CODICI
164100970530     C                   Z-ADD     D00VER        KCTVER
164200970530     C                   MOVEL     WTTC          KCTTTC
164300970530     C                   MOVEL     'D'           KCTTSP
164400970530     C     KEYCPT        CHAIN     AZCPT01L                           99
164500970530IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
164600970530     C                   Z-ADD     CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
164700970530     C                   Z-ADD     CPT1IN        WD1I
164800970530     C                   Z-ADD     CPT0CN        WD0C
164900970530     C                   Z-ADD     CPT1CN        WD1C
165000970530     C                   Z-ADD     CPTTIS        WDIS
165100970530     C                   Z-ADD     CPTTCS        WDCS
165200970530     C                   Z-ADD     CPTTLD        WDLD
165300970530     C                   MOVEL     'C'           KCTTSP
165400970530     C     KEYCPT        CHAIN     AZCPT01L                           99
165500970530IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
165600970530     C                   Z-ADD     CPT0IN        WC0I                           *TEMPI CORRIERE
165700970530     C                   Z-ADD     CPT1IN        WC1I
165800970530     C                   Z-ADD     CPT0CN        WC0C
165900970530     C                   Z-ADD     CPT1CN        WC1C
166000970530     C                   Z-ADD     CPTTIS        WCIS
166100970530     C                   Z-ADD     CPTTCS        WCCS
166200970530     C                   Z-ADD     CPTTLD        WCLD
166300970530     C                   MOVEL     'E'           KCTTSP
166400970530     C     KEYCPT        CHAIN     AZCPT01L                           99
166500970530IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
166600970530     C                   Z-ADD     CPT0IN        WE0I                           *TEMPI ESPRESSO
166700970530     C                   Z-ADD     CPT1IN        WE1I
166800970530     C                   Z-ADD     CPT0CN        WE0C
166900970530     C                   Z-ADD     CPT1CN        WE1C
167000970530     C                   Z-ADD     CPTTIS        WEIS
167100970530     C                   Z-ADD     CPTTCS        WECS
167200970530     C                   Z-ADD     CPTTLD        WELD
167300090903     C                   MOVEL     'H'           KCTTSP
167400090903     C     KEYCPT        CHAIN     AZCPT01L                           99
167500090903IF  4C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
167600090903     C                   Z-ADD     CPT0IN        Wh0I                           *TEMPI H1030
167700090903     C                   Z-ADD     CPT1IN        Wh1I
167800090903     C                   Z-ADD     CPT0CN        Wh0C
167900090903     C                   Z-ADD     CPT1CN        Wh1C
168000090903     C                   Z-ADD     CPTTIS        WhIS
168100090903     C                   Z-ADD     CPTTCS        WhCS
168200090903     C                   Z-ADD     CPTTLD        WhLD
168300090903X   4C                   ELSE
168400090903
168500090903     c* Se tipo servizio H non ancora decorrente (anche guardando alla
168600090903     c*    scadenza della versione in visualizzazione)non do errore e non
168700090903     c*    visualizzo nemmeno come descrizione
168800090903
168900090903     c                   if        Datasys>=dataiso
169000090903     c* Verifico anche la  scadenza ddella versione
169100090903     c     d00ver        chain     azcpr01l
169200090903     c                   if        not %found(azcpr01l) or cprdsc>=§vpotsph
169300090903     C                   MOVEL     '1'           ERRL00
169400090903     c                   endif
169500090903     c                   endif
169600090903     c
169700090903     c                   if        errl00='0'
169800090903     c                   clear                   viddtsph
169900090903E   4C                   ENDIF
170000090903
170100090903E   4C                   ENDIF
170200090903
170300970603X   3C                   ELSE
170400970603     C                   MOVEL     '1'           ERRL00
170500970603E   3C                   ENDIF
170600090903
170700970603X   2C                   ELSE
170800970603     C                   MOVEL     '1'           ERRL00
170900970603E   2C                   ENDIF
171000090903
171100970603X   1C                   ELSE
171200970603     C                   MOVEL     '1'           ERRL00
171300970603E   1C                   ENDIF
171400090903
171500010322     C                   ELSE
171600010322     C                   MOVEL     '1'           ERRL00
171700010322     C                   ENDIF
171800970530     C*
171900970530     C                   ENDSR
172000010322     C*------------------------------------------------------------------------*
172100010322     C* CHKTTR - CONTROLLA IL TIPO TEMPO RITIRO
172200010322     C*------------------------------------------------------------------------*
172300010322     C     CHKTTR        BEGSR
172400010322     C*
172500010322     C     *LIKE         DEFINE    VIDTTR        WTTR                           *TIPO TEMPO
172600010322     C     *LIKE         DEFINE    CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
172700010322     C     *LIKE         DEFINE    CPT1IN        WD1IR
172800010322     C     *LIKE         DEFINE    CPT0CN        WD0CR
172900010322     C     *LIKE         DEFINE    CPT1CN        WD1CR
173000010322     C     *LIKE         DEFINE    CPTTIS        WDISR
173100010322     C     *LIKE         DEFINE    CPTTCS        WDCSR
173200010322     C     *LIKE         DEFINE    CPTTLD        WDLDR
173300010322     C     *LIKE         DEFINE    CPT0IN        WC0IR                          *TEMPI CORRIERE
173400010322     C     *LIKE         DEFINE    CPT1IN        WC1IR
173500010322     C     *LIKE         DEFINE    CPT0CN        WC0CR
173600010322     C     *LIKE         DEFINE    CPT1CN        WC1CR
173700010322     C     *LIKE         DEFINE    CPTTIS        WCISR
173800010322     C     *LIKE         DEFINE    CPTTCS        WCCSR
173900010322     C     *LIKE         DEFINE    CPTTLD        WCLDR
174000010322     C     *LIKE         DEFINE    CPT0IN        WE0IR                          *TEMPI ESPRESSO
174100010322     C     *LIKE         DEFINE    CPT1IN        WE1IR
174200010322     C     *LIKE         DEFINE    CPT0CN        WE0CR
174300010322     C     *LIKE         DEFINE    CPT1CN        WE1CR
174400010322     C     *LIKE         DEFINE    CPTTIS        WEISR
174500010322     C     *LIKE         DEFINE    CPTTCS        WECSR
174600010322     C     *LIKE         DEFINE    CPTTLD        WELDR
174700010322     C*
174800010322     C* RICHIESTA ELENCO CODICI
174900010322     C                   SETOFF                                           28
175000010322     C     '?'           SCAN      WTTR                                   28
175100010322IF  1C     *IN28         IFEQ      *ON
175200010322     C                   MOVEL     *BLANK        DNAR
175300010322     C                   MOVEL     *BLANKS       DCAP
175400010322     C                   MOVEL     'T01'         WOP0
175500010322     C                   MOVEL     *BLANKS       WOP1
175600010323     C                   MOVEL     'TISI41R'     WPGM              8
175700010322     C                   EXSR      LANPGM
175800010322     C                   MOVEL     D00TTC        WTTR
175900010322E   1C                   ENDIF
176000010322     C*
176100010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO RITIRO
176200010322     C                   MOVEL     VIDTTR        DEP3A             3
176300010322     C     DEP3A         IFEQ      '999'
176400010322     C* CONTROLLA ESISTENZA CODICI
176500010322     C                   Z-ADD     D00VER        KCTVER
176600010322     C                   MOVEL     WTTR          KCTTTC
176700010322     C                   MOVEL     'D'           KCTTSP
176800010322     C     KEYCPT        CHAIN     AZCPT01L                           99
176900010322IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
177000010322     C                   Z-ADD     CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
177100010322     C                   Z-ADD     CPT1IN        WD1IR
177200010322     C                   Z-ADD     CPT0CN        WD0CR
177300010322     C                   Z-ADD     CPT1CN        WD1CR
177400010322     C                   Z-ADD     CPTTIS        WDISR
177500010322     C                   Z-ADD     CPTTCS        WDCSR
177600010322     C                   Z-ADD     CPTTLD        WDLDR
177700010322     C                   MOVEL     'C'           KCTTSP
177800010322     C     KEYCPT        CHAIN     AZCPT01L                           99
177900010322IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
178000010322     C                   Z-ADD     CPT0IN        WC0IR                          *TEMPI CORRIERE
178100010322     C                   Z-ADD     CPT1IN        WC1IR
178200010322     C                   Z-ADD     CPT0CN        WC0CR
178300010322     C                   Z-ADD     CPT1CN        WC1CR
178400010322     C                   Z-ADD     CPTTIS        WCISR
178500010322     C                   Z-ADD     CPTTCS        WCCSR
178600010322     C                   Z-ADD     CPTTLD        WCLDR
178700010322     C                   MOVEL     'E'           KCTTSP
178800010322     C     KEYCPT        CHAIN     AZCPT01L                           99
178900010322IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
179000010322     C                   Z-ADD     CPT0IN        WE0IR                          *TEMPI ESPRESSO
179100010322     C                   Z-ADD     CPT1IN        WE1IR
179200010322     C                   Z-ADD     CPT0CN        WE0CR
179300010322     C                   Z-ADD     CPT1CN        WE1CR
179400010322     C                   Z-ADD     CPTTIS        WEISR
179500010322     C                   Z-ADD     CPTTCS        WECSR
179600010322     C                   Z-ADD     CPTTLD        WELDR
179700010322X   3C                   ELSE
179800010322     C                   MOVEL     '1'           ERRL00
179900010322E   3C                   ENDIF
180000010322X   2C                   ELSE
180100010322     C                   MOVEL     '1'           ERRL00
180200010322E   2C                   ENDIF
180300010322X   1C                   ELSE
180400010322     C                   MOVEL     '1'           ERRL00
180500010322E   1C                   ENDIF
180600010322     C                   ELSE
180700010322     C                   MOVEL     '1'           ERRL00
180800010322     C                   ENDIF
180900010322     C*
181000010322     C                   ENDSR
181100970526     C*------------------------------------------------------------------------*
181200970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
181300970526     C*------------------------------------------------------------------------*
181400970526     C     IREVID        BEGSR
181500970526     C* CAMPI
181600970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
181700970530IF  1C     CPLESS        IFEQ      'S'
181800970530     C                   MOVEL     'SIN'         VDDESS
181900970530E   1C                   ENDIF
182000970530     C                   MOVEL     CPLNAR        VIDNAR
182100970530     C                   MOVEL     CPLCAP        VIDCAP
182200970530     C                   MOVEL     CPLLOC        VIDLOC
182300970530     C                   MOVEL     CPLLIV        VIDLIV
182400970530     C                   MOVEL     CPLDLV        VIDDLV
182500970917     C                   MOVEL     CPLFI1        VIDFI1
182600970530     C                   MOVEL     CPLLNA        VIDLNA
182700970530     C                   MOVEL     CPLLOL        VIDLOL
182800971118     C                   MOVEL     CPLLO2        VIDLO2
182900970604     C                   MOVEL     CPLLOS        VIDLOS
183000970530     C                   MOVEL     CPLZNC        VIDZNC
183100970530     C                   MOVEL     CPLZOL        VIDZOL
183200971118     C                   MOVEL     CPLZO2        VIDZO2
183300970604     C                   MOVEL     CPLZOS        VIDZOS
183400970530     C                   MOVEL     CPLLNA        VIDLFD
183500970530     C                   MOVEL     CPLZFD        VIDZFD
183600970530     C                   Z-ADD     CPLLKG        VIDLKG
183700970530     C                   Z-ADD     CPLLMC        VIDLMC
183800970604     C                   Z-ADD     CPLLKS        VIDLKS
183900970604     C                   Z-ADD     CPLLMS        VIDLMS
184000971118     C                   Z-ADD     CPLLK2        VIDLK2
184100971118     C                   Z-ADD     CPLLM2        VIDLM2
184200970530     C                   MOVEL     CPLCTS        VIDCTS
184300970530     C                   MOVEL     CPLCTE        VIDCTE
184400970530     C                   MOVEL     CPLISO        VIDISO
184500970530     C                   MOVEL     CPLPII        VIDPII
184600970530     C                   MOVEL     CPLPID        VIDPID
184700970530     C                   MOVEL     CPLPIS        VIDPIS
184800970530     C                   MOVEL     CPLGDD        VIDGDD
184900970530     C                   MOVEL     CPLTTC        VIDTTC
185000010322     C                   MOVEL     *all'9'       VIDTTR
185100010322     C                   MOVE      CPLFI2        VIDTTR
185200970530     C*
185300970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> IMPORTA I DATI DEL CAPPARIO
185400970530IF  2C     VIDLIV        IFEQ      'C'
185500970530     C                   SETON                                        92        *PROTEZIONE CAMPI
185600970530     C                   EXSR      IMPCPC
185700970530E   2C                   ENDIF
185800970530     C*
185900970526     C                   ENDSR
186000970526     C*------------------------------------------------------------------------*
186100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
186200970526     C*------------------------------------------------------------------------*
186300970526     C     AZZVID        BEGSR
186400970526     C* CAMPI
186500970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
186600970530     C                   MOVEL     *BLANKS       VDDESS
186700970603     C                   MOVEL     D00NAR        VIDNAR
186800970603     C                   MOVEL     D00CAP        VIDCAP
186900970530     C                   MOVEL     *BLANKS       VIDLOC
187000970530     C                   MOVEL     *BLANKS       VIDLIV
187100970530     C                   MOVEL     *BLANKS       VIDDLV
187200970917     C                   MOVEL     *BLANKS       VIDFI1
187300970530     C                   MOVEL     *ZEROS        VIDLNA
187400970530     C                   MOVEL     *ZEROS        VIDLOL
187500971118     C                   MOVEL     *ZEROS        VIDLO2
187600970604     C                   MOVEL     *ZEROS        VIDLOS
187700970530     C                   MOVEL     *ZEROS        VIDZNC
187800970530     C                   MOVEL     *ZEROS        VIDZOL
187900971118     C                   MOVEL     *ZEROS        VIDZO2
188000970604     C                   MOVEL     *ZEROS        VIDZOS
188100970530     C                   MOVEL     *ZEROS        VIDLFD
188200970530     C                   MOVEL     *ZEROS        VIDZFD
188300970530     C                   Z-ADD     *ZEROS        VIDLKG
188400970530     C                   Z-ADD     *ZEROS        VIDLMC
188500970604     C                   Z-ADD     *ZEROS        VIDLKS
188600970604     C                   Z-ADD     *ZEROS        VIDLMS
188700971118     C                   Z-ADD     *ZEROS        VIDLK2
188800971118     C                   Z-ADD     *ZEROS        VIDLM2
188900970530     C                   MOVEL     *BLANKS       VIDCTS
189000970530     C                   MOVEL     *BLANKS       VIDCTE
189100970530     C                   MOVEL     *BLANKS       VIDISO
189200970530     C                   MOVEL     *BLANKS       VIDPII
189300970530     C                   MOVEL     *BLANKS       VIDPID
189400970530     C                   MOVEL     *BLANKS       VIDPIS
189500970530     C                   MOVEL     *BLANKS       VIDGDD
189600970530     C                   MOVEL     *ZEROS        VIDTTC
189700970526     C* DECODIFICHE
189800970527     C                   MOVEL     *BLANKS       VDDNAR
189900970527     C                   MOVEL     *BLANKS       VDDLNA
190000970527     C                   MOVEL     *BLANKS       VDDLOL
190100971118     C                   MOVEL     *BLANKS       VDDLO2
190200970604     C                   MOVEL     *BLANKS       VDDLOS
190300970530     C                   MOVEL     *BLANKS       VDDLFD
190400970530     C                   MOVEL     *BLANKS       VDDCTS
190500970530     C                   MOVEL     *BLANKS       VDDCTE
190600970530     C                   MOVEL     *BLANKS       VDDISO
190700970526     C*
190800970526     C                   ENDSR
190900970530     C*------------------------------------------------------------------------*
191000000426     C* IMPCPC - IMPORTA I DATI DEL CAPPARIO
191100970530     C*------------------------------------------------------------------------*
191200970530     C     IMPCPC        BEGSR
191300970530     C*
191400970530     C                   MOVEL     D00VER        KCCVER
191500970530     C                   MOVEL     VIDNAR        KCCNAR
191600970530     C                   MOVEL     VIDCAP        KCCCAP
191700970530     C     KEYCPC        CHAIN     AZCPC01L                           99
191800970530IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
191900970530     C                   MOVEL     CPCLNA        VIDLNA
192000970530     C                   MOVEL     CPCLOL        VIDLOL
192100971118     C                   MOVEL     CPCLO2        VIDLO2
192200970604     C                   MOVEL     CPCLOS        VIDLOS
192300970530     C                   MOVEL     CPCZNC        VIDZNC
192400970530     C                   MOVEL     CPCZOL        VIDZOL
192500971118     C                   MOVEL     CPCZO2        VIDZO2
192600970604     C                   MOVEL     CPCZOS        VIDZOS
192700970530     C                   MOVEL     CPCLNA        VIDLFD
192800970530     C                   MOVEL     CPCZFD        VIDZFD
192900970530     C                   Z-ADD     CPCLKG        VIDLKG
193000970530     C                   Z-ADD     CPCLMC        VIDLMC
193100970604     C                   Z-ADD     CPCLKS        VIDLKS
193200970604     C                   Z-ADD     CPCLMS        VIDLMS
193300971118     C                   Z-ADD     CPCLK2        VIDLK2
193400971118     C                   Z-ADD     CPCLM2        VIDLM2
193500970530     C                   MOVEL     CPCCTS        VIDCTS
193600970530     C                   MOVEL     CPCCTE        VIDCTE
193700970530     C                   MOVEL     CPCISO        VIDISO
193800970530     C                   MOVEL     CPCPII        VIDPII
193900970530     C                   MOVEL     CPCPID        VIDPID
194000970530     C                   MOVEL     CPCPIS        VIDPIS
194100970530     C                   MOVEL     CPCGDD        VIDGDD
194200970530     C                   MOVEL     CPCTTC        VIDTTC
194300010705     C                   MOVEL     *all'9'       VIDTTR
194400010705     C                   MOVE      CPCFI2        VIDTTR
194500970530X   1C                   ELSE                                                   *NON TROVATO
194600970530     C                   MOVEL     *ZEROS        VIDLNA
194700970530     C                   MOVEL     *ZEROS        VIDLOL
194800971118     C                   MOVEL     *ZEROS        VIDLO2
194900970604     C                   MOVEL     *ZEROS        VIDLOS
195000970530     C                   MOVEL     *ZEROS        VIDZNC
195100970530     C                   MOVEL     *ZEROS        VIDZOL
195200971118     C                   MOVEL     *ZEROS        VIDZO2
195300970604     C                   MOVEL     *ZEROS        VIDZOS
195400970530     C                   MOVEL     *ZEROS        VIDLFD
195500970530     C                   MOVEL     *ZEROS        VIDZFD
195600970530     C                   Z-ADD     *ZEROS        VIDLKG
195700970530     C                   Z-ADD     *ZEROS        VIDLMC
195800970604     C                   Z-ADD     *ZEROS        VIDLKS
195900970604     C                   Z-ADD     *ZEROS        VIDLMS
196000971118     C                   Z-ADD     *ZEROS        VIDLK2
196100971118     C                   Z-ADD     *ZEROS        VIDLM2
196200970530     C                   MOVEL     *BLANKS       VIDCTS
196300970530     C                   MOVEL     *BLANKS       VIDCTE
196400970530     C                   MOVEL     *BLANKS       VIDISO
196500970530     C                   MOVEL     *BLANKS       VIDPII
196600970530     C                   MOVEL     *BLANKS       VIDPID
196700970530     C                   MOVEL     *BLANKS       VIDPIS
196800970530     C                   MOVEL     *BLANKS       VIDGDD
196900970530     C                   MOVEL     *ZEROS        VIDTTC
197000010705     C                   MOVEL     *BLANKS       VIDTTR
197100970530E   1C                   ENDIF
197200000426     C*
197300970530     C                   ENDSR
197400970526     C*------------------------------------------------------------------------*
197500970530     C* IMPCPL - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
197600970526     C*------------------------------------------------------------------------*
197700970530     C     IMPCPL        BEGSR
197800970526     C*
197900970530     C                   MOVEL     VIDNAR        CPLNAR
198000970530     C                   MOVEL     VIDCAP        CPLCAP
198100970530     C                   MOVEL     VIDLOC        CPLLOC
198200970530     C                   MOVEL     VIDLIV        CPLLIV
198300970530     C                   MOVEL     VIDDLV        CPLDLV
198400970917     C                   MOVEL     VIDFI1        CPLFI1
198500010322     C                   MOVE(p)   VIDTTR        CPLFI2
198600970530     C*
198700971118     C* LOCALITA' GESTITA A LIVELLO DI LOCALITA' -> SCRIVE I DATI VIDEO SUL FILE
198800970530IF  1C     VIDLIV        IFEQ      'L'
198900970530     C                   MOVEL     VIDLNA        CPLLNA
199000970530     C                   MOVEL     VIDLOL        CPLLOL
199100971118     C                   MOVEL     VIDLO2        CPLLO2
199200970604     C                   MOVEL     VIDLOS        CPLLOS
199300970530     C                   MOVEL     VIDZNC        CPLZNC
199400970530     C                   MOVEL     VIDZOL        CPLZOL
199500971118     C                   MOVEL     VIDZO2        CPLZO2
199600970604     C                   MOVEL     VIDZOS        CPLZOS
199700970530     C                   MOVEL     VIDZFD        CPLZFD
199800970530     C                   Z-ADD     VIDLKG        CPLLKG
199900970530     C                   Z-ADD     VIDLMC        CPLLMC
200000970604     C                   Z-ADD     VIDLKS        CPLLKS
200100970604     C                   Z-ADD     VIDLMS        CPLLMS
200200971118     C                   Z-ADD     VIDLK2        CPLLK2
200300971118     C                   Z-ADD     VIDLM2        CPLLM2
200400970530     C                   MOVEL     VIDCTS        CPLCTS
200500970530     C                   MOVEL     VIDCTE        CPLCTE
200600970530     C                   MOVEL     VIDISO        CPLISO
200700970603     C                   MOVEL     VIDPII        CPLPII
200800970603IF  3C     CPLISO        IFEQ      'I'
200900970603     C                   MOVEL     'S'           CPLPII
201000970603E   3C                   ENDIF
201100970603     C                   MOVEL     VIDPID        CPLPID
201200970603IF  3C     CPLISO        IFEQ      'D'
201300970603     C                   MOVEL     'S'           CPLPID
201400970603E   3C                   ENDIF
201500970603     C                   MOVEL     VIDPIS        CPLPIS
201600970603IF  3C     CPLISO        IFEQ      'S'
201700970603     C                   MOVEL     'S'           CPLPIS
201800970603E   3C                   ENDIF
201900970530     C                   MOVEL     VIDGDD        CPLGDD
202000970530     C                   MOVEL     VIDTTC        CPLTTC
202100970530     C*
202200970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> AZZERA I CAMPI RIFERITI AL CAP
202300970530X   1C                   ELSE
202400970530     C                   Z-ADD     *ZEROS        CPLLNA
202500970530     C                   Z-ADD     *ZEROS        CPLLOL
202600971118     C                   Z-ADD     *ZEROS        CPLLO2
202700970604     C                   Z-ADD     *ZEROS        CPLLOS
202800970530     C                   Z-ADD     *ZEROS        CPLZNC
202900970530     C                   Z-ADD     *ZEROS        CPLZOL
203000971118     C                   Z-ADD     *ZEROS        CPLZO2
203100970604     C                   Z-ADD     *ZEROS        CPLZOS
203200970530     C                   Z-ADD     *ZEROS        CPLZFD
203300970530     C                   Z-ADD     *ZEROS        CPLLKG
203400970530     C                   Z-ADD     *ZEROS        CPLLMC
203500970604     C                   Z-ADD     *ZEROS        CPLLKS
203600970604     C                   Z-ADD     *ZEROS        CPLLMS
203700971118     C                   Z-ADD     *ZEROS        CPLLK2
203800971118     C                   Z-ADD     *ZEROS        CPLLM2
203900970530     C                   MOVEL     *BLANKS       CPLCTS
204000970530     C                   MOVEL     *BLANKS       CPLCTE
204100970530     C                   MOVEL     *BLANKS       CPLISO
204200970530     C                   MOVEL     *BLANKS       CPLPII
204300970530     C                   MOVEL     *BLANKS       CPLPID
204400970530     C                   MOVEL     *BLANKS       CPLPIS
204500970530     C                   MOVEL     *BLANKS       CPLGDD
204600970530     C                   Z-ADD     *ZEROS        CPLTTC
204700010705     C                   MOVEL     *BLANKS       CPLFI2
204800970530E   1C                   ENDIF
204900970526     C*
205000970526     C                   ENDSR
205100970530     C*------------------------------------------------------------------------*
205200970530     C* LANPGM - LANCIA UN PROGRAMMA
205300970530     C*------------------------------------------------------------------------*
205400970530     C     LANPGM        BEGSR
205500970530     C*
205600970530     C* IMPOSTA LA DS DI PROCEDURA
205700970530     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
205800970530     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
205900970530     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
206000970530     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
206100970530     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
206200970530     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
206300970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
206400970602     C                   MOVEL     DCAP          D00CAP                         *CAP
206500970602     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
206600970603     C                   MOVEL     *BLANKS       D00SIN                         *ALTRI CAMPI
206700970603     C                   Z-ADD     *ZEROS        D00TTC
206800970530     C                   MOVEL     *BLANKS       D00TSP
206900970530     C                   Z-ADD     *ZEROS        D00TFP
207000970530     C                   MOVEL     *BLANKS       D00EPA
207100970530     C                   Z-ADD     *ZEROS        D00ETP
207200970530     C                   Z-ADD     *ZEROS        D00ETA
207300970530     C                   Z-ADD     *ZEROS        D00EDD
207400970530     C                   MOVEL     *BLANKS       D00SPT
207500970530     C                   Z-ADD     *ZEROS        D00SLP
207600970530     C                   Z-ADD     *ZEROS        D00SLA
207700970530     C                   MOVEL     *BLANKS       D00STS
207800970530     C                   Z-ADD     *ZEROS        D00SDD
207900970530     C*
208000970530     C                   CALL      WPGM
208100970612     C                   PARM                    KPJBA
208200970612     C                   PARM                    DSSI00
208300970530     C*
208400970530     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
208500970530IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
208600970530     C                   MOVEL     '1'           ERRL00                         *ERRORE
208700970530     C                   MOVEL     '1'           F03L00                         *FINE PGM
208800970530X   1C                   ELSE                                                   *NON PREMUTO F03
208900970530     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
209000970530E   1C                   ENDIF
209100970530     C*
209200970530     C                   ENDSR
209300970526     C*------------------------------------------------------------------------*
209400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
209500970526     C*------------------------------------------------------------------------*
209600970526     C     IMPFUN        BEGSR
209700970603     C*
209800970603     C* SE INTERROGAZIONE/VISUALIZZAZIONE TOGLE LA GESTIONE ALLE ZONE DI CONSEGNA
209900970603IF  1C     DEPOP0        IFEQ      'C05'
210000970603     C     DEPOP1        OREQ      'O05'
210100970603     C                   MOVEL     FUD(14)       FUDRIG                         *AGGIORNAMENTO
210200970603     C                   MOVEL     'N'           FUDATT
210300970603     C                   MOVEL     FUDRIG        FUD(14)
210400970603E   1C                   ENDIF
210500970526     C*
210600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
210700970526IF  1C     DEPOP1        IFEQ      'O05'
210800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
210900970526     C                   MOVEL     'N'           FUDATT
211000970526     C                   MOVEL     FUDRIG        FUD(6)
211100970526E   1C                   ENDIF
211200970526     C*
211300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
211400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
211500970526     C                   EXSR      FUNGES
211600000925     C*
211700000925     C* VERIFICO CHE SOLO UTENTI "EDP*" POSSANO MODIFICARE IL CAMPO "UFFICIO POSTALE"
211800000925     C                   MOVEL     KNMUS         UTE               3
211900000925     C     UTE           IFNE      'EDP'
212000000925     C                   SETON                                        95
212100000925     C                   ENDIF
212200970526     C*
212300970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
212400970526     C*---
212500970526     C* 90 -> PROTEZIONE CHIAVE
212600970526     C* 91 -> PROTEZIONE TOTALE
212700050414     C* 94 -> PROTEZIONE COD.TASSAZIONE e TIPO INOLTRO e altri
212800050413     C*---
212900050413     C*
213000050413     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
213100050413     C                   SETON                                        94
213200050413     C*
213300050413     C* VERIFICO X PRIMISSIMA COSA LE AUTORIZZAZIONI SPECIALI SULL'UTENTE CORRENTE
213400050413     C                   IF        §UTECAP = *blanks
213500050413     C* TUTTO RIMANE INVARIATO
213600050413     C                   ENDIF
213700050413     C*
213800050413     C                   IF        §UTECAP = 'I'
213900050413     C* FORZO SOLO INTERROGAZIONE
214000050413     C                   EVAL      DEPOP1 = 'O05'
214100050413     C                   ENDIF
214200050413     C*
214300050413     C                   IF        §UTECAP = 'C'
214400050413     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
214500050413     C                   SETOFF                                       94
214600050413     C                   ENDIF
214700050413     C*
214800970530IF  1C     DEPOP0        IFEQ      'L02'                                        G E S T I O N E
214900970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
215000970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
215100970526     C                   SETOFF                                       91
215200970526E   2C                   ENDIF
215300970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
215400970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
215500970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
215600970526E   2C                   ENDIF
215700970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
215800970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
215900970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
216000970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
216100970526     C                   SETON                                        91
216200970526E   2C                   ENDIF
216300970526X   1C                   ELSE                                                   *I N T E R R O G A
216400970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
216500970526     C                   SETON                                        91
216600970526E   1C                   ENDIF
216700970526     C*
216800970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
216900970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
217000970530     C                   Z-ADD     D00VER        KCLVER
217100970530     C                   MOVEL     D00NAR        KCLNAR
217200970530     C                   MOVEL     D00CAP        KCLCAP
217300970530     C                   MOVEL     D00LOC        KCLLOC
217400970530     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
217500970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
217600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
217700970526     C                   MOVEL     MSG(6)        DSMSMS
217800970526X   2C                   ELSE
217900970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
218000980128IF  3C     DEPOP1        IFEQ      'O05'
218100980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
218200980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
218300980128E   3C                   ENDIF
218400970526E   2C                   ENDIF
218500970526X   1C                   ELSE                                                   *INSERIMENTO
218600970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
218700970526E   1C                   ENDIF
218800970526     C*
218900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
219000970526IF  1C     DEPOP1        IFEQ      'O02'
219100970526     C                   MOVEL     DEC(1)        VIDFUN
219200970526E   1C                   ENDIF
219300970526IF  1C     DEPOP1        IFEQ      'O04'
219400970526     C                   MOVEL     DEC(2)        VIDFUN
219500970526E   1C                   ENDIF
219600970526IF  1C     DEPOP1        IFEQ      'O05'
219700970526     C                   MOVEL     DEC(3)        VIDFUN
219800970526E   1C                   ENDIF
219900970526IF  1C     DEPOP1        IFEQ      'O07'
220000970526     C                   MOVEL     DEC(4)        VIDFUN
220100970526E   1C                   ENDIF
220200970526IF  1C     DEPOP1        IFEQ      'F06'
220300970526     C                   MOVEL     DEC(5)        VIDFUN
220400970526E   1C                   ENDIF
220500970526     C*
220600970526     C                   ENDSR
220700970626     C*------------------------------------------------------------------------*
220800970626     C* FINSR - OPERAZIONI FINALI
220900970626     C*------------------------------------------------------------------------*
221000970626     C     FINSR         BEGSR
221100970626     C*
221200970626     C* CHIUDE I FILE DEL "TISI95R"
221300970704     C                   CLEAR                   DSSI95
221400970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
221500970626     C                   CALL      'TISI95R'
221600970626     C                   PARM                    DSSI95
221700970626     C*
221800970626     C                   ENDSR
221900050413     C*--------------------------------------------------------------------------------------------*
222000050413     C* REPERISCE DATI UTENTE
222100050413     C*--------------------------------------------------------------------------------------------*
222200050413     C     REPDATIUTE    BEGSR
222300050413     C*
222400050413     C* REimposta le variabili di lavoro
222500050413     C                   CLEAR                   TIBS34DS
222600050413     C                   CLEAR                   AZUTEDS
222700050413     C                   CLEAR                   DDATIUTE
222800050413     C*
222900050413     C* Reperisce i dati del profilo in ingresso
223000050413     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
223100050413     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
223200050413     C                   IN(E)     *DTAARA
223300050413     C                   IF        %Error                                       *NO errori
223400050413     C                   EVAL      I34TLA = 'L'
223500050413     C                   CALL      'TIBS34R'
223600050413     C                   PARM                    TIBS34DS
223700050413     C                   IN        *DTAARA
223800050413     C                   ENDIF
223900050413     C*
224000050413     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
224100050413     C                   EVAL      DUTE01 = UTEFAF
224200050413     C*
224300050413     C                   ENDSR
224400970526     C*------------------------------------------------------------------------*
224500970526     C* *INZSR - OPERAZIONI INIZIALI
224600970526     C*------------------------------------------------------------------------*
224700970526     C     *INZSR        BEGSR
224800970526     C*---
224900970526     C* RICEVIMENTO PARAMETRI
225000970526     C*---
225100970526     C     *ENTRY        PLIST
225200970612     C                   PARM                    KPJBA
225300970612     C                   PARM                    DSSI00
225400050413     C*
225500050413     C* REPERISCE I DATI DELL'UTENTE
225600050413     C                   EXSR      REPDATIUTE
225700970526     C*
225800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
225900970526     C     XMSGPA        PLIST
226000970526     C                   PARM                    DSMSMS
226100970526     C                   PARM                    DSMSPG
226200970526     C                   PARM                    DSMSKE
226300970526     C                   PARM                    DSMSER
226400970526     C*
226500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
226600970526     C                   CALL      'XKEY'
226700970526     C                   PARM                    DSKEY
226800090903     c*
226900090903     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
227000090903     c                   clear                   tibs02ds
227100090903     c                   clear                   dvpodeco
227200090903     c                   eval      t02mod = 'C'
227300090903     c                   eval      t02sif = knsif
227400090903     c                   eval      t02cod = 'VPO'
227500090903     c                   eval      t02ke1 = 'DECO'
227600090903     c                   call      'TIBS02R'
227700090903     c                   parm                    kpjba
227800090903     c                   parm                    tibs02ds
227900090903     c                   if        t02err = *blanks
228000090903     c                   eval      dvpodeco = t02uni
228100090903     c                   else
228200090903     c                   eval      §vpotsph=20091001
228300090903     c                   endif
228400090903
228500090903     c     *iso          movel     §vpotsph      dataiso
228600090903     c
228700970526     C*---
228800970526     C* VARIABILI NON RIFERITE AL DATA BASE
228900970526     C*---
229000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
229100970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
229200970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
229300970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
229400970526     C     *LIKE         DEFINE    D00OP1        WOP1
229500970602     C     *LIKE         DEFINE    D00NAR        DNAR
229600970602     C     *LIKE         DEFINE    D00CAP        DCAP
229700970602     C     *LIKE         DEFINE    D00LOC        DLOC
229800970526     C*---
229900970526     C* VARIABILI DI CONTROLLO
230000970526     C*---
230100970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
230200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
230300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
230400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
230500970526     C*
230600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
230700970529     C                   MOVEL     'TISI31R'     DSMSPG
230800970529     C                   MOVEL     'TISI31R'     MSGQUE
230900970526     C                   MOVEL     *BLANKS       MSGKEY
231000970526     C                   MOVEL     *BLANKS       DSMSMS
231100970526     C*---
231200970526     C* VARIABILI RIFERITE AL DATA BASE
231300970526     C*---
231400970530     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
231500970530     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
231600970530     C     *LIKE         DEFINE    TBLCOD        KTBCOD
231700970530     C     *LIKE         DEFINE    TBLKEY        KTBKEY
231800970530     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
231900970530     C     *LIKE         DEFINE    CPLNAR        KCLNAR
232000970530     C     *LIKE         DEFINE    CPLCAP        KCLCAP
232100970530     C     *LIKE         DEFINE    CPLLOC        KCLLOC
232200970530     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
232300970530     C     *LIKE         DEFINE    CPCNAR        KCCNAR
232400970530     C     *LIKE         DEFINE    CPCCAP        KCCCAP
232500970530     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
232600970530     C     *LIKE         DEFINE    CPTTTC        KCTTTC
232700970530     C     *LIKE         DEFINE    CPTTSP        KCTTSP
232800970925     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
232900970925     C     *LIKE         DEFINE    CPSNAR        KCSNAR
233000970925     C     *LIKE         DEFINE    CPSCAP        KCSCAP
233100970925     C     *LIKE         DEFINE    CPSLOC        KCSLOC
233200970613     C     *LIKE         DEFINE    CPCLNA        NLNA                           *DEPOSITI NUMERICI
233300970613     C     *LIKE         DEFINE    CPCLOS        NLOS
233400970613     C     *LIKE         DEFINE    CPCLOL        NLOL
233500971118     C     *LIKE         DEFINE    CPCLO2        NLO2
233600970613     C     *LIKE         DEFINE    CPCTTC        NTTC
233700010911     C     *LIKE         DEFINE    CPCFI2        NTTR
233800970526     C*---
233900970526     C* CHIAVI DI LETTURA
234000970526     C*---
234100970530     C* LETTURA AZORG01L - COMPLETA
234200970530     C     KEYORG        KLIST
234300970530     C                   KFLD                    KORFIL                         *FILIALE
234400970530     C* LETTURA TABEL00F - COMPLETA
234500970530     C     KEYTAB        KLIST
234600970530     C                   KFLD                    KTBKUT                         *CODICE UTENTE
234700970530     C                   KFLD                    KTBCOD                         *TABELLA
234800970530     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
234900970530     C* LETTURA TABEL00F - PARZIALE
235000970530     C     KE2TAB        KLIST
235100970530     C                   KFLD                    KTBKUT                         *CODICE UTENTE
235200970530     C                   KFLD                    KTBCOD                         *TABELLA
235300970530     C* LETTURA AZCPL01L - COMPLETA
235400970530     C     KEYCPL        KLIST
235500970530     C                   KFLD                    KCLVER                         *VERSIONE
235600970530     C                   KFLD                    KCLNAR                         *NAZIONE
235700970530     C                   KFLD                    KCLCAP                         *CAP
235800970530     C                   KFLD                    KCLLOC                         *LOCALITA'
235900970613     C* LETTURA AZCPL01L - PARZIALE
236000970613     C     KE2CPL        KLIST
236100970613     C                   KFLD                    KCLVER                         *VERSIONE
236200970613     C                   KFLD                    KCLNAR                         *NAZIONE
236300970613     C                   KFLD                    KCLCAP                         *CAP
236400970527     C* LETTURA AZCPC01L - COMPLETA
236500970527     C     KEYCPC        KLIST
236600970527     C                   KFLD                    KCCVER                         *VERSIONE
236700970527     C                   KFLD                    KCCNAR                         *NAZIONE
236800970527     C                   KFLD                    KCCCAP                         *CAP
236900970530     C* LETTURA AZCPT01L - COMPLETA
237000970530     C     KEYCPT        KLIST
237100970530     C                   KFLD                    KCTVER                         *VERSIONE
237200970530     C                   KFLD                    KCTTTC                         *TEMPO CONSEGNA
237300970530     C                   KFLD                    KCTTSP                         *TIPO SERVIZIO
237400970925     C* LETTURA AZCPS01L - PARZIALE
237500970925     C     KEYCPS        KLIST
237600970925     C                   KFLD                    KCSVER                         *VERSIONE
237700970925     C                   KFLD                    KCSNAR                         *NAZIONE
237800970925     C                   KFLD                    KCSCAP                         *CAP
237900970925     C                   KFLD                    KCSLOC                         *LOCALITA
238000970526     C*
238100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
238200970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
238300970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
238400970526     C*
238500970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
238600970526     C                   EXSR      IMPFUN
238700970526     C*
238800970526     C                   ENDSR
238900970526     C*------------------------------------------------------------------------*
239000970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
239100970526     C*------------------------------------------------------------------------*
239200970526     C     FUNGES        BEGSR
239300970526     C*
239400970526     C* PULISCO CAMPI DI LAVORO
239500970526     C                   Z-ADD     0             W                 5 0
239600970526     C*
239700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
239800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
239900970526     C     1             DO        24            I
240000970526     C                   MOVEL     FUD(I)        FUDRIG
240100970526     C*
240200970526     C* PULIZIA FUNZIONI PRECEDENTI
240300970526     C                   MOVEL     *BLANKS       FUV(I)
240400970526     C*
240500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
240600970526     C* (SALTA F24)
240700970526     C     FUDATT        IFEQ      'S'
240800970526     C     FUDFUN        ANDNE     '24'
240900970526     C                   ADD       1             W
241000970526     C                   MOVEL     FUDDES        FUV(W)
241100970526     C                   END
241200970526     C                   END                                                    DO WHILE
241300970526     C*
241400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
241500970526     C* ALTRIMENTI DISATTIVO F24
241600970526     C     1             DO        24            I
241700970526     C                   MOVEL     FUD(I)        FUDRIG
241800970526     C*
241900970526     C     FUDFUN        IFEQ      '24'
242000970526     C     W             IFGT      6
242100970526     C                   MOVEL     'S'           FUDATT
242200970526     C                   ELSE
242300970526     C                   MOVEL     *BLANKS       FUDATT
242400970526     C                   END                                                    W GT 8
242500970526     C                   MOVEL     FUDRIG        FUD(I)
242600970526     C                   END
242700970526     C                   END                                                    DO WHILE
242800970526     C*
242900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
243000970526     C     W             DIV       6             I
243100970526     C                   MVR                     Z
243200970526     C*
243300970526     C     Z             IFNE      0
243400970526     C     I             ADD       1             Z
243500970526     C                   ELSE
243600970526     C                   Z-ADD     I             Z                 5 0
243700970526     C                   END
243800970526     C*
243900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
244000970526     C                   ADD       1             FUNGRU
244100970526     C     FUNGRU        IFGT      Z
244200970526     C     FUNGRU        ORLT      1
244300970526     C                   Z-ADD     1             FUNGRU
244400970526     C                   END
244500970526     C*
244600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
244700970526     C     FUNGRU        IFEQ      1
244800970526     C                   MOVEL     FUV(1)        FUN001
244900970526     C                   MOVEL     FUV(2)        FUN002
245000970526     C                   MOVEL     FUV(3)        FUN003
245100970526     C                   MOVEL     FUV(4)        FUN004
245200970526     C                   MOVEL     FUV(5)        FUN005
245300970526     C                   MOVEL     FUV(6)        FUN006
245400970526     C                   END
245500970526     C*
245600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
245700970526     C     FUNGRU        IFEQ      2
245800970526     C                   MOVEL     FUV(7)        FUN001
245900970526     C                   MOVEL     FUV(8)        FUN002
246000970526     C                   MOVEL     FUV(9)        FUN003
246100970526     C                   MOVEL     FUV(10)       FUN004
246200970526     C                   MOVEL     FUV(11)       FUN005
246300970526     C                   MOVEL     FUV(12)       FUN006
246400970526     C                   END
246500970526     C*
246600970526     C* TERZO GRUPPO DI 6 FUNZIONI
246700970526     C     FUNGRU        IFEQ      3
246800970526     C                   MOVEL     FUV(13)       FUN001
246900970526     C                   MOVEL     FUV(14)       FUN002
247000970526     C                   MOVEL     FUV(15)       FUN003
247100970526     C                   MOVEL     FUV(16)       FUN004
247200970526     C                   MOVEL     FUV(17)       FUN005
247300970526     C                   MOVEL     FUV(18)       FUN006
247400970526     C                   END
247500970526     C*
247600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
247700970526     C     FUNGRU        IFEQ      4
247800970526     C                   MOVEL     FUV(19)       FUN001
247900970526     C                   MOVEL     FUV(20)       FUN002
248000970526     C                   MOVEL     FUV(21)       FUN003
248100970526     C                   MOVEL     FUV(22)       FUN004
248200970526     C                   MOVEL     *BLANKS       FUN005
248300970526     C                   MOVEL     *BLANKS       FUN006
248400970526     C                   END
248500970526     C*
248600970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
248700970526     C                   MOVEL     *BLANKS       FUN007
248800970526     C                   MOVEL     *BLANKS       FUN008
248900970526     C*
249000970526     C     1             DO        24            I
249100970526     C                   MOVEL     FUD(I)        FUDRIG
249200970526     C     FUDFUN        IFEQ      '24'
249300970526     C     FUDATT        ANDEQ     'S'
249400970526     C                   MOVEL     FUDDES        FUN008
249500970526     C                   END
249600970526     C                   END                                                    DO
249700970526     C*
249800970526     C                   ENDSR
249900970526     C*------------------------------------------------------------------------*
250000970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
250100970526     C*------------------------------------------------------------------------*
250200970526     C     FUNCON        BEGSR
250300970526     C*
250400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
250500970526     C                   EXSR      FUNVAR
250600970526     C*
250700970526     C* PULISCO CAMPI DI LAVORO
250800970526     C                   Z-ADD     0             I                 5 0
250900970526     C                   Z-ADD     0             W                 5 0
251000970526     C*
251100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
251200970526     C     FUNKEY        IFNE      *BLANKS
251300970526     C     1             DO        24            I
251400970526     C                   MOVEL     FUD(I)        FUDRIG
251500970526     C*
251600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
251700970526     C     FUNKEY        IFEQ      FUDFUN
251800970526     C     FUDATT        ANDEQ     'S'
251900970526     C                   Z-ADD     I             W
252000970526     C                   END
252100970526     C                   END                                                    DO WHILE
252200970526     C*
252300970526     C* FUNZIONE NON ATTIVA (ERRORE)
252400970526     C     W             IFEQ      0
252500970526     C*
252600970526     C* EMISSIONE MESSAGGIO DI ERRORE
252700970526     C                   MOVEL     MSG(1)        DSMSMS
252800970526     C*
252900970526     C                   END                                                    W=0
253000970526     C                   END                                                    FUNKEY NE *BLANKS
253100970526     C*
253200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
253300970526     C     W             IFNE      0
253400970526     C     FUNKEY        OREQ      *BLANKS
253500970526     C     FUNKEY        IFEQ      *BLANKS
253600970526     C                   EXSR      ENTGES
253700970526     C                   END
253800970526     C     FUNKEY        IFEQ      '03'
253900970526     C                   EXSR      F03GES
254000970526     C                   END
254100010322     C     FUNKEY        IFEQ      '04'
254200010322     C                   EXSR      F04GES
254300010322     C                   END
254400970526     C     FUNKEY        IFEQ      '06'
254500970526     C                   EXSR      F06GES
254600970526     C                   END
254700130827     C     FUNKEY        IFEQ      '07'
254800130827     C                   EXSR      F07GES
254900130827     C                   END
255000970526     C     FUNKEY        IFEQ      '12'
255100970526     C                   EXSR      F12GES
255200970526     C                   END
255300970603     C     FUNKEY        IFEQ      '14'
255400970603     C                   EXSR      F14GES
255500970603     C                   END
255600970602     C     FUNKEY        IFEQ      '13'
255700970602     C                   EXSR      F13GES
255800970602     C                   END
255900130830     C     FUNKEY        IFEQ      '17'
256000130830     C                   EXSR      F17GES
256100130830     C                   END
256200970526     C     FUNKEY        IFEQ      '24'
256300970526     C                   EXSR      F24GES
256400970526     C                   END
256500970526     C                   END
256600970526     C*
256700970526     C                   ENDSR
256800970526     C*------------------------------------------------------------------------*
256900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
257000970526     C*------------------------------------------------------------------------*
257100970526     C     FUNVAR        BEGSR
257200970526     C*
257300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
257400970526     C*
257500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
257600970526     C     KEY           IFEQ      ENTER
257700970526     C                   MOVEL     *BLANKS       FUNKEY
257800970526     C                   END
257900970526     C     KEY           IFEQ      F1
258000970526     C                   MOVEL     '01'          FUNKEY
258100970526     C                   END
258200970526     C     KEY           IFEQ      F2
258300970526     C                   MOVEL     '02'          FUNKEY
258400970526     C                   END
258500970526     C     KEY           IFEQ      F3
258600970526     C                   MOVEL     '03'          FUNKEY
258700970526     C                   END
258800970526     C     KEY           IFEQ      F4
258900970526     C                   MOVEL     '04'          FUNKEY
259000970526     C                   END
259100970526     C     KEY           IFEQ      F5
259200970526     C                   MOVEL     '05'          FUNKEY
259300970526     C                   END
259400970526     C     KEY           IFEQ      F6
259500970526     C                   MOVEL     '06'          FUNKEY
259600970526     C                   END
259700970526     C     KEY           IFEQ      F7
259800970526     C                   MOVEL     '07'          FUNKEY
259900970526     C                   END
260000970526     C     KEY           IFEQ      F8
260100970526     C                   MOVEL     '08'          FUNKEY
260200970526     C                   END
260300970526     C     KEY           IFEQ      F9
260400970526     C                   MOVEL     '09'          FUNKEY
260500970526     C                   END
260600970526     C     KEY           IFEQ      F10
260700970526     C                   MOVEL     '10'          FUNKEY
260800970526     C                   END
260900970526     C     KEY           IFEQ      F11
261000970526     C                   MOVEL     '11'          FUNKEY
261100970526     C                   END
261200970526     C     KEY           IFEQ      F12
261300970526     C                   MOVEL     '12'          FUNKEY
261400970526     C                   END
261500970526     C     KEY           IFEQ      F13
261600970526     C                   MOVEL     '13'          FUNKEY
261700970526     C                   END
261800970526     C     KEY           IFEQ      F14
261900970526     C                   MOVEL     '14'          FUNKEY
262000970526     C                   END
262100970526     C     KEY           IFEQ      F15
262200970526     C                   MOVEL     '15'          FUNKEY
262300970526     C                   END
262400970526     C     KEY           IFEQ      F16
262500970526     C                   MOVEL     '16'          FUNKEY
262600970526     C                   END
262700970526     C     KEY           IFEQ      F17
262800970526     C                   MOVEL     '17'          FUNKEY
262900970526     C                   END
263000970526     C     KEY           IFEQ      F18
263100970526     C                   MOVEL     '18'          FUNKEY
263200970526     C                   END
263300970526     C     KEY           IFEQ      F19
263400970526     C                   MOVEL     '19'          FUNKEY
263500970526     C                   END
263600970526     C     KEY           IFEQ      F20
263700970526     C                   MOVEL     '20'          FUNKEY
263800970526     C                   END
263900970526     C     KEY           IFEQ      F21
264000970526     C                   MOVEL     '21'          FUNKEY
264100970526     C                   END
264200970526     C     KEY           IFEQ      F22
264300970526     C                   MOVEL     '22'          FUNKEY
264400970526     C                   END
264500970526     C     KEY           IFEQ      F23
264600970526     C                   MOVEL     '23'          FUNKEY
264700970526     C                   END
264800970526     C     KEY           IFEQ      F24
264900970526     C                   MOVEL     '24'          FUNKEY
265000970526     C                   END
265100970526     C     KEY           IFEQ      ROLLUP
265200970526     C                   MOVEL     'RU'          FUNKEY
265300970526     C                   END
265400970526     C     KEY           IFEQ      HELP
265500970526     C                   MOVEL     'HP'          FUNKEY
265600970526     C                   END
265700970526     C                   ENDSR
265800970526     C*------------------------------------------------------------------------*
265900970526     C* NOTE COSTRUZIONE SCHIERE
266000970526     C*------------------------------------------------------------------------*
266100970526     C*
266200970526     C* -MSG-MESSAGGI
266300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
266400970526     C* (NON TOCCARE MSG,1 E MSG,2)
266500970526     C*
266600970526     C* -CMD-COMANDI
266700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
266800970526     C*
266900970526     C* -OPD-OPZIONI
267000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
267100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
267200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
267300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
267400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
267500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
267600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
267700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
267800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
267900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
268000970526     C*
268100970526     C* -FUD-FUNZIONI
268200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
268300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
268400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
268500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
268600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
268700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
268800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
268900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
269000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
269100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
269200970526     O*------------------------------------------------------------------------*
269300970526** DEC - DECODIFICHE
269400970526    MODIFICA                                                      1
269500970526  ANNULLAMENTO                                                    2
269600970526VISUALIZZAZIONE                                                   3
269700970526   RIPRISTINO                                                     4
269800970526  INSERIMENTO                                                     5
269900970526** MSG - MESSAGGI
270000970530TISI31R- Tasto funzionale non gestito                                          1
270100970530TISI31R- Inserimento effettuato                                                2
270200970530TISI31R- Modifica effettuata                                                   3
270300970530TISI31R- Annullamento effettuato                                               4
270400970530TISI31R- Ripristino effettuato                                                 5
270500970530TISI31R- Richiesta funzione per record inesistente                             6
270600970530TISI31R- Record momentaneamente allocato: riprovare più tardi                  7
270700970530TISI31R- Richiesta funzione per record esistente                               8
270800000504TISI31R- Ufficio Postale errato, annullato o inesistente                       9
270900970530TISI31R- Linea di arrivo errata                                               10
271000970530TISI31R- Zona di consegna errata                                              11
271100970917TISI31R- CAP obsoleto: anche la località deve esserlo                         12
271200970530TISI31R- Nazione errata                                                       13
271300970530TISI31R- Codice tassazione errato                                             14
271400970530TISI31R- Codice tassazione errato. Accettati solo I1, I2, I3, ecc ...         15
271500970530TISI31R- Per CAP estero lasciare il codice tassazione vuoto                   16
271600060206TISI31R- Zona di consegna NON abilitata                                       17
271700970604TISI31R- Linea limite obbligatoria se immessi limiti di peso o volume         18
271800970604TISI31R- Limiti obbligatori se immesse linea o zona limite                    19
271900970917TISI31R- Impossibile inserire località di CAP obsoleti                        20
272000970917TISI31R- CAP errato o inesistente                                             21
272100970530TISI31R- Località errata                                                      22
272200970530TISI31R- Immettere L x gestire i dati della località, C x usare quelli del cap23
272300970912TISI31R- Il limite oltre deve essere superiore del limite sotto               24
272400970917TISI31R- Immettere 'O' per rendere obsoleta la località, o lasciare vuoto     25
272500970618TISI31R- Unica località per il cap. impossibile gestirla a livello località   26
272600970530TISI31R- Immettere 'S' o 'N'                                                  27
272700110623TISI31R- Tipo inoltro errato o non utilizzabile nel cappario                  28
272800970618TISI31R- La località è uguale al cap. Impossibile gestirla a livello località 29
272900970603TISI31R- Tipo tempo di consegna errato o incompleto                           30
273000010322TISI31R- Tasto funzione non consentito sul campo corrente.                    31
273100010322TISI31R- Tipo tempo ritiro errato o incompleto                                32
273200041008TISI31R- Valorizzare entrambi i campi o non valorizzarne nessuno.             33
273300130905Esistono OrariServizi personalizz.x la linea modificata saranno elimiati      34
273400130905TISI31R- F17 consentito soltando sui campi LINEA di ARRIVO valorizzati        35
273500970526** FUD - FUNZIONI
273600970526                   -        (S/N)=N                        funz.01¬
273700970526                   -        (S/N)=N                        funz.02¬
273800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
273900010322F04=Visual. tempi  -        (S/N)=S  <== S-FISSO           funz.04¬
274000130827                   -        (S/N)=N                        funz.05¬
274100970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
274200130827F07=Orari Servizi  -        (S/N)=S                        funz.07¬
274300130821                   -        (S/N)=N                        funz.08¬
274400130821                   -        (S/N)=N                        funz.09¬
274500970527                   -        (S/N)=N                        funz.10¬
274600970526                   -        (S/N)=N                        funz.11¬
274700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
274800970602F13=Sinonimi       -        (S/N)=S                        funz.13¬
274900970603F14=Zone consegna  -        (S/N)=S                        funz.14¬
275000970526                   -        (S/N)=N                        funz.15¬
275100970526                   -        (S/N)=N                        funz.16¬
275200130905F17=OrariServLinea -        (S/N)=N                        funz.17¬
275300970526                   -        (S/N)=N                        funz.18¬
275400970526                   -        (S/N)=N                        funz.19¬
275500970526                   -        (S/N)=N                        funz.20¬
275600970526                   -        (S/N)=N                        funz.21¬
275700970526                   -        (S/N)=N                        funz.22¬
275800970526                   -        (S/N)=N                        funz.23¬
275900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
