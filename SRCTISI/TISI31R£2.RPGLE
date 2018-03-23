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
003000130917     F*------------
003100130917     F* ORARI SERVIZI
003200130917     F*------------
003300130917     FAZCOS01L  UF   E           K DISK
003400130917     F*------------
003500130917     F* eccezioni
003600130917     F*------------
003700130917     FAZCPE01L  IF   E           K DISK
003800970526     D*------------------------------------------------------------------------*
003900970526     D* SCHIERE
004000970526     D*------------------------------------------------------------------------*
004100970526     D*------------
004200970526     D* DECODIFICHE
004300970526     D*------------
004400970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
004500970526     D*------------
004600970526     D* MESSAGGI
004700970526     D*------------
004800010322     D MSG             S             78    DIM(35) CTDATA PERRCD(1)
004900970526     D*------------
005000970526     D* DESCRIZIONI TASTI FUNZIONALI
005100970526     D*------------
005200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
005300970526     D*------------
005400970526     D* FUNZIONI DA CARICARE A VIDEO
005500970526     D*------------
005600970526     D FUV             S             19    DIM(24)
005700131007     d* Filiali abilitate  agli orari servizi
005800131007     D FIL_AB          S              3    DIM(85)
005900970526     D*------------------------------------------------------------------------*
006000970526     D* INPUT
006100970526     D*------------------------------------------------------------------------*
006200970526     D*------------
006300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006400970526     D*------------
006500970526     D FUDDS           DS
006600970526     D  FUDDE1                 1      1
006700970526     D  FUDFUN                 2      3
006800970526     D  FUDDE2                 4     19
006900970526     D  FUDFI0                20     34
007000970526     D  FUDATT                35     35
007100970526     D  FUDFI1                36     80
007200970526     D  FUDDES                 1     19
007300970526     D  FUDRIG                 1     80
007400970526     D*------------
007500970526     D* DS GESTIONE TASTI FUNZIONALI
007600970526     D*------------
007700970526     D DSKEY         E DS
007800970526     D DEVDS1          DS
007900970526     D  KEY                  369    369
008000970526     D*------------
008100970526     D* DS "XMSG" - GESTIONE MESSAGGI
008200970526     D*------------
008300970526     D DSMSG         E DS
008400970527     D*------------------
008500970527     D* DS NAZIONI
008600970527     D*------------------
008700970527     D DS15          E DS
008800970527     D*------------------
008900970527     D* DS ZONE DI CONSEGNA
009000970527     D*------------------
009100970527     D DS03          E DS
009200970529     D*------------------
009300970529     D* DS CODICI TASSAZIONE
009400970529     D*------------------
009500970529     D DSCT          E DS
009600970529     D*------------------
009700970529     D* DS TIPI INOLTRO
009800970529     D*------------------
009900970529     D DS9A          E DS
010000970626     D*------------------
010100970626     D* DS "TISI95R" - CONTROLLO CAP
010200970626     D*------------------
010300970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
010400970527     D*------------
010500970527     D* DS "TISI81R" - AGGIORNA UN CAP DAI SUOI ARCHIVI
010600970527     D*------------
010700970527     D DSSI81        E DS                  EXTNAME(TISI81DS)
010800970526     D*------------
010900970526     D* DS DI PROCEDURA
011000970526     D*------------
011100970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
011200130821     d tisio0ds      E DS
011300130916
011400130916     d Tisio7ds      e ds
011500970526     D*------------
011600970526     D* ARCHITETTURA
011700970526     D*------------
011800970526     D KPJBA         E DS
011900050413     D*------------
012000050413     D* DS REPERIMENTO DATI UTENTE
012100050413     D*------------
012200050413     D TIBS34DS      E DS                                                       *Profili utente
012300050413     D DDATIUTE      E DS                                                       *Dati utente
012400050413     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012500050413     D DUTE01        E DS                                                       *Abilitazioni
012600090605
012700090605     d ds5e          e ds
012800090903     d
012900090903     d dvpodeco      e ds
013000090903     d Tibs02Ds      e ds
013100000426     D*------------------
013200000426     D* PASSAGGIO PARAMETRI
013300000426     D*------------------
013400000426     D PARAM           DS
013500000426     D  POP1                   1      3
013600000426     D  PUPI                   4      8  0
013700000426     D  PMSG                   9     88
013800000426     D  POPZ                  89     91
013900090903
014000090903     d datasys         s               d   inz(*sys)
014100090903     d dataiso         s               d   datfmt(*iso)
014200130917     d f_lna           s                   like(cpllna)
014300130917     d f_los           s                   like(cpllos)
014400130917     d f_lol           s                   like(cpllol)
014500130917     d s_vidlna        s                   like(cpllna)  inz
014600130917     d s_vidlos        s                   like(cpllos)  inz
014700130917     d s_vidlol        s                   like(cpllol)  inz
014800130917     d f07lna          s              1                  inz
014900131007     d w003a           s              3
015000131007     d XX              s              3  0
015100131007     d YY              s              3  0
015200131007
015300131007
015400131007      /copy gaitrasrc/srcprotopr,tibs02r
015500131007
015600970526     C*------------------------------------------------------------------------*
015700970526     C* MAIN LINES
015800970526     C*------------------------------------------------------------------------*
015900970526     C*
016000970526     C* NON CI SONO ERRORI IN ENTRATA
016100970526IF  1C     ERRL00        IFEQ      '0'
016200970526     C*
016300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016700970526     C*
016800970526     C* EMETTE FORMATO VIDEO
016900970526     C                   EXSR      BUTFMR
017000970526     C*
017100970526     C* GESTIONE TASTI FUNZIONALI
017200970526     C                   EXSR      FUNCON
017300970526E   2C                   ENDDO
017400970526     C*
017500970526     C* CI SONO ERRORI IN ENTRATA
017600970527X   1C                   ELSE
017700970526     C                   MOVEL     '1'           D00ERR
017800970526     C                   MOVEL     DSMSMS        D00MSG
017900970526E   1C                   ENDIF
018000970626     C*
018100970626     C* OPERAZIONI FINALI
018200970626     C                   EXSR      FINSR
018300970526     C* FINE PGM
018400970526     C                   SETON                                        LR
018500970526     C*------------------------------------------------------------------------*
018600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
018700970526     C*------------------------------------------------------------------------*
018800970526     C     BUTFMR        BEGSR
018900970526     C*
019000970526     C* EMETTE LA TESTATA
019100970529     C                   WRITE     SI31D01
019200970526     C*
019300970526     C* SPEDISCE EVENTUALE MESSAGGIO
019400970526     C                   EXSR      SNDMSG
019500970526     C*
019600970526     C* EMETTE LE FUNZIONI
019700970529     C                   WRITE     SI31D02
019800970526     C*
019900970526     C* EMETTE IL VIDEO
020000970529     C                   EXFMT     SI31D03
020100970526     C*
020200970526     C* REIMPOSTA FLAG DI ERRORE
020300970526     C                   MOVEL     '0'           ERRL00
020400970526     C*
020500970526     C* REIMPOSTA INDICATORI DI ERRORE
020600970526     C                   SETOFF                                         5859
020700970526     C                   SETOFF                                       606162
020800970526     C                   SETOFF                                       636465
020900970526     C                   SETOFF                                       666768
021000970526     C                   SETOFF                                       697071
021100970526     C                   SETOFF                                       727374
021200970526     C                   SETOFF                                       757677
021300970526     C                   SETOFF                                       787980
021400970526     C                   SETOFF                                       818283
021500970526     C                   SETOFF                                       848586
021600970526     C                   SETOFF                                       878889
021700000426     C                   SETOFF                                       525354
021800010322     C                   SETOFF                                       5544
021900970526     C*
022000970526     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
022300970526     C*------------------------------------------------------------------------*
022400970526     C     SNDMSG        BEGSR
022500970526     C*
022600970526     C* PULISCE IL SFILE MESSAGGI
022700970526     C                   MOVEL     '1'           *IN40
022800970529     C                   WRITE     SI31DMC
022900970526     C                   MOVEL     '0'           *IN40
023000970526     C*
023100970526     C* VISUALIZZA IL MESSAGGIO
023200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
023300970526     C                   CALL      'XMSG'        XMSGPA
023400970526IF  1C     DSMSER        IFEQ      *ZEROS
023500970526     C                   MOVEL     DSMSKE        MSGKEY
023600970526E   1C                   ENDIF
023700970526     C                   Z-ADD     1             MSGULT
023800970529     C                   WRITE     SI31DMS
023900970529     C                   WRITE     SI31DMC
024000970526     C*
024100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
024200970526     C                   MOVEL     *BLANKS       DSMSMS
024300970526     C*
024400970526     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* ENTGES - GESTIONE ENTER -> INVIO
024700970526     C*------------------------------------------------------------------------*
024800970526     C     ENTGES        BEGSR
024900970526     C*
025000970526     C* CONTROLLA I CAMPI IMMESSI
025100970526     C                   EXSR      DECODI
025200970526     C*
025300970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
025400970526IF  1C     DEPOP1        IFEQ      'O05'
025500970526     C                   MOVEL     '1'           FINL00
025600970526E   1C                   ENDIF
025700970526     C*
025800970526     C                   ENDSR
025900970526     C*------------------------------------------------------------------------*
026000970526     C* F03GES - GESTIONE F03 -> FINE
026100970526     C*------------------------------------------------------------------------*
026200970526     C     F03GES        BEGSR
026300970526     C*
026400970526     C* IMPOSTA "PREMUTO F03"
026500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
026600970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
026700970526     C*
026800970526     C                   ENDSR
026900010322     C*------------------------------------------------------------------------*
027000010322     C* F04GES - GESTIONE F04 -> VISUALIZZAZIONE TEMPI CONSEGNA/RITIRO
027100010322     C*------------------------------------------------------------------------*
027200010322     C     F04GES        BEGSR
027300090605      * imposto le descrizioni fisse per i 3 tipi servizio utilizzati
027400090605      * 'C' - 'D' - 'E'
027500090605     c                   eval      ktbcod = '5E'
027600090605     c     ke2tab        setll     tabel00f
027700090605     c                   do        *hival
027800090605     c     ke2tab        reade     tabel00f
027900090605     c                   if        %eof(tabel00f)
028000090605     c                   leave
028100090605     c                   endif
028200090605     c                   eval      ds5e = tbluni
028300090605     c                   if        tblkey = 'C'
028400090605     c                   eval      viddtspc = §5ed03
028500090605     c                   endif
028600090605     c                   if        tblkey = 'D'
028700090605     c                   eval      viddtspd = §5ed03
028800090605     c                   endif
028900090605     c                   if        tblkey = 'E'
029000090605     c                   eval      viddtspe = §5ed03
029100090605     c                   endif
029200090903     c                   if        tblkey = 'H' and (vidh0c>0 or
029300090903     c                             Datasys>=dataiso)
029400090903     c                   eval      viddtsph = §5ed03
029500090903     c                   endif
029600090605     c                   enddo
029700010322     C*
029800010322     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
029900010322     C     RNMFL         IFEQ      'VIDTTC'
030000010322     C                   EXFMT     SI31WWC
030100010322     C                   ELSE
030200010322     C     RNMFL         IFEQ      'VIDTTR'
030300010322     C                   EXFMT     SI31WWR
030400010322     C                   ELSE
030500010322     C                   MOVEL     MSG(31)       DSMSMS
030600010322     C                   ENDIF
030700010322     C                   ENDIF
030800010322     C*
030900010322     C                   ENDSR
031000130830     C*------------------------------------------------------------------------*
031100130830     C* F17GES - GESTIONE F17 -> Orari servizi  per linea di arrivo
031200130830     C*------------------------------------------------------------------------*
031300130830     C     F17GES        BEGSR
031400130830     c                   clear                   dsmsms
031500130830     c                   exsr      decodi
031600130830     c
031700130830    1c                   if        dsmsms=*blanks
031800130830     C*
031900130830     c                   clear                   tisio0ds
032000130830     C                   MOVEL     VIDNAR        do0NAR
032100130830     c                   if        %subst(depop0:2:2)='02'
032200130830     C                   MOVEL     'G'           do0tric
032300130830     c                   else
032400130830     C                   MOVEL     'I'           do0tric
032500130830     c                   endif
032600130830     c                   movel     d00dri        do0dri
032700130830     c                   movel     d00ver        do0ver
032800130830
032900130830     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
033000130830    2c                   select
033100130830     C     RNMFL         wheneq    'VIDLNA'
033200130830     c     vidlna        andgt     *zeros
033300130830     C                   movel     vidlna        do0lna
033400130830     c
033500130830     C     RNMFL         wheneq    'VIDLOL'
033600130830     c     vidlol        andgt     *zeros
033700130830     C                   movel     vidlol        do0lna
033800130830     C     RNMFL         wheneq    'VIDLOS'
033900130830     c     vidlos        andgt     *zeros
034000130830     C                   movel     vidlos        do0lna
034100130830     C                   other
034200130905     C                   MOVEL     MSG(35)       DSMSMS
034300130830    2C                   ENDsl
034400130830
034500130830    2c                   if        dsmsms=*blanks
034600130830     C                   call      'TISIO2R'
034700130830     C                   PARM                    KPJBA
034800130830     C                   PARM                    tisio0ds
034900130830     C*
035000130830     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
035100130830IF  3C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
035200130830     C                   MOVEL     '1'           ERRL00                         *ERRORE
035300130830     C                   MOVEL     '1'           F03L00                         *FINE PGM
035400130830X   1C                   ELSE                                                   *NON PREMUTO F03
035500130830     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
035600130830E   3C                   ENDIF
035700130830     C*
035800130830E   2C                   ENDIF
035900130830E   1C                   ENDIF
036000130830     C*
036100130830     C                   ENDSR
036200970526     C*------------------------------------------------------------------------*
036300970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
036400970526     C*------------------------------------------------------------------------*
036500970526     C     F06GES        BEGSR
036600970526     C*
036700970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
036800970526IF  1C     DEPOP1        IFEQ      'F06'
036900970526     C                   EXSR      INSERI
037000970526E   1C                   ENDIF
037100970526IF  1C     DEPOP1        IFEQ      'O02'
037200970526     C                   EXSR      MODIFI
037300970526E   1C                   ENDIF
037400970526IF  1C     DEPOP1        IFEQ      'O04'
037500970526     C                   EXSR      ANNULL
037600970526E   1C                   ENDIF
037700970526IF  1C     DEPOP1        IFEQ      'O07'
037800970526     C                   EXSR      RIPRIS
037900970526E   1C                   ENDIF
038000970526     C*
038100970526     C                   ENDSR
038200970526     C*------------------------------------------------------------------------*
038300970526     C* F12GES - GESTIONE F12 -> RITORNO
038400970526     C*------------------------------------------------------------------------*
038500970526     C     F12GES        BEGSR
038600970526     C*
038700970526     C* IMPOSTA "PREMUTO F12"
038800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
038900970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
039000970526     C*
039100970526     C                   ENDSR
039200970602     C*------------------------------------------------------------------------*
039300970602     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
039400970602     C*------------------------------------------------------------------------*
039500970602     C     F13GES        BEGSR
039600970602     C*
039700970602     C                   MOVEL     VIDNAR        DNAR
039800970602     C                   MOVEL     VIDCAP        DCAP
039900970602     C                   MOVEL     VIDLOC        DLOC
040000970602     C                   MOVEL     DEPOP0        WOP0
040100970602     C                   MOVEL     'S'           WOP0
040200970602     C                   MOVEL     'F13'         WOP1
040300970602     C                   MOVEL     'TISI35R'     WPGM              8
040400970602     C                   EXSR      LANPGM
040500970602     C*
040600970602     C                   ENDSR
040700970603     C*------------------------------------------------------------------------*
040800970603     C* F14GES - GESTIONE F14 -> ZONE DI CONSEGNA
040900970603     C*------------------------------------------------------------------------*
041000970603     C     F14GES        BEGSR
041100970603     C*
041200970603     C                   CALL      'TRTB23R'
041300970603     C                   PARM                    KPJBA
041400970603     C*
041500970603     C                   ENDSR
041600130821     C*------------------------------------------------------------------------*
041700130827     C* F07GES - GESTIONE F07 -> Orari servizi
041800130821     C*------------------------------------------------------------------------*
041900130827     C     F07GES        BEGSR
042000130821     C*
042100130903     c                   exsr      F06ges
042200130903
042300130903     c                   if        errl00='0'
042400130903
042500130821     c                   clear                   tisio0ds
042600130821     C                   MOVEL     VIDNAR        do0NAR
042700130821     C                   MOVEL     VIDCAP        Do0CAP
042800130821     C                   MOVEL     VIDLOC        Do0LOC
042900130830     c                   if        %subst(depop0:2:2)='02'
043000130830     C                   MOVEL     'G'           do0tric
043100130830     c                   else
043200130830     C                   MOVEL     'I'           do0tric
043300130830     c                   endif
043400130821     c                   movel     d00dri        do0dri
043500130821     c                   movel     d00ver        do0ver
043600130821
043700130821     C                   call      'TISIO2R'
043800130821     C                   PARM                    KPJBA
043900130821     C                   PARM                    tisio0ds
044000130821     C*
044100130821     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
044200130821IF  1C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
044300130821     C                   MOVEL     '1'           ERRL00                         *ERRORE
044400130821     C                   MOVEL     '1'           F03L00                         *FINE PGM
044500130821X   1C                   ELSE                                                   *NON PREMUTO F03
044600130821     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
044700130821E   1C                   ENDIF
044800130821     C*
044900130903E   1c                   ENDIF
045000130821     C*
045100130821     C                   ENDSR
045200130821     C*------------------------------------------------------------------------*
045300970526     C*------------------------------------------------------------------------*
045400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
045500970526     C*------------------------------------------------------------------------*
045600970526     C     F24GES        BEGSR
045700970526     C*
045800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
045900970526     C*
046000970526     C                   ENDSR
046100970526     C*------------------------------------------------------------------------*
046200970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
046300970526     C*------------------------------------------------------------------------*
046400970526     C     INSERI        BEGSR
046500970526     C*
046600970526     C* CONTROLLA I CAMPI IMMESSI
046700970526     C                   EXSR      DECODI
046800970618     C*
046900970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
047000970618IF  1C     ERRL00        IFEQ      '0'
047100970618     C                   EXSR      DECF06
047200970917IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047300970530     C                   Z-ADD     D00VER        CPLVER                         *CHIAVE
047400970530     C                   MOVEL     VIDNAR        CPLNAR
047500970530     C                   MOVEL     VIDCAP        CPLCAP
047600970530     C                   MOVEL     VIDLOC        CPLLOC
047700970530     C                   EXSR      IMPCPL                                       *ALTRI CAMPI
047800970530     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
047900970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
048000970530     C                   MOVEL     *BLANKS       CPLATB                         *ANNULLAMENTO
048100980616     C                   MOVEL     'N'           CPLESS                         *ESISTENZA SINONIMI
048200980616     C                   MOVEL     'N'           CPLESV                         *ECCEIZONI X VIE
048300970530     C                   WRITE     AZCPL000
048400970527     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
048500970527     C*
048600970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
048700970527     C                   Z-ADD     D00VER        D81VER
048800970527     C                   MOVEL     VIDNAR        D81NAR
048900970527     C                   MOVEL     VIDCAP        D81CAP
049000970530     C                   MOVEL     'L'           D81CHK
049100970527     C                   CALL      'TISI81R'
049200970527     C                   PARM                    DSSI81
049300970602     C*
049400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
049500970602     C                   EXSR      AZZVID
049600970618E   2C                   ENDIF
049700970618E   1C                   ENDIF
049800970526     C*
049900970526     C                   ENDSR
050000970526     C*------------------------------------------------------------------------*
050100970526     C* MODIFI - OPERAZIONI DI MODIFICA
050200130916     c*------------------------------------------------------------------------*
050300970526     C     MODIFI        BEGSR
050400970613     C*
050500970613     C* CONTROLLA I CAMPI IMMESSI
050600970613     C                   EXSR      DECODI
050700970618     C*
050800970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
050900970618IF  1C     ERRL00        IFEQ      '0'
051000970618     C                   EXSR      DECF06
051100970613     C*
051200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
051300970613IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051400970530     C                   EXSR      LETCPL
051500970526     C*
051600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
051700970613IF  3C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051800130916     c* richiamo pgm per cancellare OrariServizi a fronte delle lna cambiate
051900130916     c                   exsr      sr_oraris
052000130916
052100970530     C                   EXSR      IMPCPL                                       *IMPOSTA I CAMPI
052200970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
052300970530     C                   MOVEL     *BLANKS       CPLATB                         *ANNULLAMENTO
052400970530     C                   UPDATE    AZCPL000
052500970526     C                   MOVEL     MSG(3)        D00MSG
052600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
052700970613E   3C                   ENDIF
052800970530     C*
052900970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
053000970530     C                   Z-ADD     D00VER        D81VER
053100970530     C                   MOVEL     VIDNAR        D81NAR
053200970530     C                   MOVEL     VIDCAP        D81CAP
053300970613     C                   MOVEL     'L'           D81CHK
053400970530     C                   CALL      'TISI81R'
053500970530     C                   PARM                    DSSI81
053600970613E   2C                   ENDIF
053700970618E   1C                   ENDIF
053800970526     C*
053900970526     C                   ENDSR
054000970526     C*------------------------------------------------------------------------*
054100970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
054200970526     C*------------------------------------------------------------------------*
054300970526     C     ANNULL        BEGSR
054400970526     C*
054500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
054600000602     C                   MOVEL     '0'           ANNDLT            1
054700970530     C                   EXSR      LETCPL
054800970527     C*
054900000426     C* ANNULLA LE LOCALITA'
055000970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
055100970530IF  2C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
055200970530IF  3C     CPLDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
055300970530     C                   MOVEL     'A'           CPLATB                         *FLAG ANNULLAMENTO
055400970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
055500970530     C                   UPDATE    AZCPL000                                     *ANNULLA
055600970526X   3C                   ELSE                                                   *MAI TRASMESSO
055700970530     C                   DELETE    AZCPL000                                     *CANCELLA
055800000602     C                   MOVEL     '1'           ANNDLT
055900970526E   3C                   ENDIF
056000970526E   2C                   ENDIF
056100970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
056200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
056300970925     C*
056400970925     C* ANNULLA I SINONIMI
056500970925     C                   Z-ADD     D00VER        KCSVER
056600970925     C                   MOVEL     CPLNAR        KCSNAR
056700970925     C                   MOVEL     CPLCAP        KCSCAP
056800970925     C                   MOVEL     CPLLOC        KCSLOC
056900970925     C     KEYCPS        SETLL     AZCPS01L                           99
057000970925     C     KEYCPS        READE     AZCPS01L                               99
057100970925DO  2C     *IN99         DOWEQ     *OFF
057200970925IF  3C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
057300970925IF  4C     CPSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
057400970925     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
057500970925     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
057600970925     C                   UPDATE    AZCPS000                                     *ANNULLA
057700970925X   4C                   ELSE                                                   *MAI TRASMESSO
057800970925     C                   DELETE    AZCPS000                                     *CANCELLA
057900970925E   4C                   ENDIF
058000970925E   3C                   ENDIF
058100970925     C     KEYCPS        READE     AZCPS01L                               99
058200970925E   2C                   ENDDO                                                  *FINE SINONIMI
058300130917
058400130917     c* ANNULLA GLI ORARI SERVIZI
058500130917     C     KEYCPS        SETLL     AZcos01l
058600130917     C     KEYCPS        READE     AZCos01L
058700130917DO  2C                   DOW       not %eof(azcos01l)
058800130917     C                   DELETE    AZCOS000                                     *CANCELLA
058900130917     C     KEYCPS        READE     AZCos01L
059000130917E   2C                   ENDDO
059100970530     C*
059200970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
059300970530     C                   Z-ADD     D00VER        D81VER
059400970530     C                   MOVEL     VIDNAR        D81NAR
059500970530     C                   MOVEL     VIDCAP        D81CAP
059600970530     C                   MOVEL     'L'           D81CHK
059700970530     C                   CALL      'TISI81R'
059800970530     C                   PARM                    DSSI81
059900970526E   1C                   ENDIF
060000970526     C*
060100970526     C                   ENDSR
060200970526     C*------------------------------------------------------------------------*
060300970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
060400970526     C*------------------------------------------------------------------------*
060500970526     C     RIPRIS        BEGSR
060600970526     C*
060700970526     C* CONTROLLA I CAMPI IMMESSI
060800970526     C                   EXSR      DECODI
060900970618     C*
061000970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
061100970618IF  1C     ERRL00        IFEQ      '0'
061200970618     C                   EXSR      DECF06
061300970526     C*
061400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
061500970618IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
061600970530     C                   EXSR      LETCPL
061700970526     C*
061800970526     C* IMPOSTA IL RECORD DA SCRIVERE
061900970618IF  3C     ERRL00        IFEQ      '0'                                          *NO ERRORI
062000970530     C                   MOVEL     *BLANKS       CPLATB                         *FLAG ANNULLAMENTO
062100970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
062200970530     C                   UPDATE    AZCPL000                                     *ANNULLA
062300970618E   3C                   ENDIF
062400970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
062500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
062600970530     C*
062700970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
062800970530     C                   Z-ADD     D00VER        D81VER
062900970530     C                   MOVEL     VIDNAR        D81NAR
063000970530     C                   MOVEL     VIDCAP        D81CAP
063100970530     C                   MOVEL     'L'           D81CHK
063200970530     C                   CALL      'TISI81R'
063300970530     C                   PARM                    DSSI81
063400970618E   2C                   ENDIF
063500970618E   1C                   ENDIF
063600970526     C*
063700970526     C                   ENDSR
063800970526     C*------------------------------------------------------------------------*
063900970530     C* LETCPL - LEGGE IL RECORD PERL'AGGIORNAMENTO
064000970526     C*------------------------------------------------------------------------*
064100970530     C     LETCPL        BEGSR
064200970526     C*
064300000426     C     DEPOP1        IFNE      'F06'
064400970530     C                   Z-ADD     D00VER        KCLVER
064500980127     C                   MOVEL     D00NAR        KCLNAR
064600980127     C                   MOVEL     D00CAP        KCLCAP
064700980127     C                   MOVEL     D00LOC        KCLLOC
064800970530     C     KEYCPL        CHAIN     AZCPL01L                           9998
064900970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
065000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
065100970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
065200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
065300970526E   1C                   ENDIF
065400970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
065500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
065600970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
065700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
065800970526E   1C                   ENDIF
065900000426     C                   ENDIF
066000970526     C*
066100970526     C                   ENDSR
066200970526     C*------------------------------------------------------------------------*
066300970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
066400970526     C*------------------------------------------------------------------------*
066500970526     C     DECODI        BEGSR
066600050428     C*
066700050428     C* NO VALORI NEGATIVI
066800050428     C                   IF        VIDLKS < *zeros
066900050428     C                   EVAL      VIDLKS = VIDLKS * -1
067000050428     C                   ENDIF
067100050428     C                   IF        VIDLKG < *zeros
067200050428     C                   EVAL      VIDLKG = VIDLKG * -1
067300050428     C                   ENDIF
067400050428     C                   IF        VIDLK2 < *zeros
067500050428     C                   EVAL      VIDLK2 = VIDLK2 * -1
067600050428     C                   ENDIF
067700050428     C                   IF        VIDLMS < *zeros
067800050428     C                   EVAL      VIDLMS = VIDLMS * -1
067900050428     C                   ENDIF
068000050428     C                   IF        VIDLMC < *zeros
068100050428     C                   EVAL      VIDLMC = VIDLMC * -1
068200050428     C                   ENDIF
068300050428     C                   IF        VIDLM2 < *zeros
068400050428     C                   EVAL      VIDLM2 = VIDLM2 * -1
068500050428     C                   ENDIF
068600050428     C*
068700970530     C*--------------------
068800970530     C* NAZIONE
068900970530     C*--------------------
069000970530     C     *LIKE         DEFINE    §15CPR        DEPCPR
069100970530IF  1C     ERRL00        IFEQ      '0'
069200970530     C                   MOVEL     *BLANKS       VDDNAR
069300970530     C                   MOVEL     VIDNAR        WNAR
069400970530     C                   MOVEL     *BLANKS       WDNAR
069500970530     C                   EXSR      CHKNAR
069600970530     C                   MOVEL     WNAR          VIDNAR
069700970530     C                   MOVEL     WDNAR         VDDNAR
069800970530IF  2C     ERRL00        IFEQ      '1'
069900970530     C                   SETON                                        60
070000970530     C                   MOVEL     MSG(13)       DSMSMS
070100970530E   2C                   ENDIF
070200970530E   1C                   ENDIF
070300970530     C*--------------------
070400970530     C* CAP
070500970530     C*--------------------
070600970530IF  1C     ERRL00        IFEQ      '0'
070700970530     C                   MOVEL     VIDNAR        WNAR
070800970530     C                   MOVEL     VIDCAP        WCAP
070900970917     C                   MOVEL     *BLANKS       WFI1
071000970530     C                   EXSR      CHKCAP
071100970530IF  2C     ERRL00        IFEQ      '1'
071200970530     C                   SETON                                        61
071300970602     C                   MOVEL     MSG(21)       DSMSMS
071400970530E   2C                   ENDIF
071500970530E   1C                   ENDIF
071600970530     C*--------------------
071700970530     C* LOCALITA'
071800970530     C*--------------------
071900970530IF  1C     ERRL00        IFEQ      '0'
072000970530IF  2C     VIDLOC        IFEQ      *BLANKS
072100970530     C                   SETON                                        70
072200970530     C                   MOVEL     '1'           ERRL00
072300970530     C                   MOVEL     MSG(22)       DSMSMS
072400970530E   2C                   ENDIF
072500970530E   1C                   ENDIF
072600970530     C*--------------------
072700970530     C* NAZIONE + CAP + LOCALITA'
072800970530     C*--------------------
072900970917IF  1C     DEPOP1        IFEQ      'F06'
073000970917     C*
073100970917     C* INSERIMENTO -> IL CAP NON PUO' ESSERE OBSOLETO
073200970917IF  2C     ERRL00        IFEQ      '0'
073300970917IF  3C     WFI1          IFEQ      'O '
073400970917     C                   SETON                                        6061
073500970917     C                   MOVEL     MSG(20)       DSMSMS
073600970917     C                   MOVEL     '1'           ERRL00
073700970917E   3C                   ENDIF
073800970917E   2C                   ENDIF
073900970917     C*
074000970530     C* INSERIMENTO -> RECORD INESISTENTE
074100970917IF  2C     ERRL00        IFEQ      '0'
074200970530     C                   Z-ADD     D00VER        KCLVER
074300970530     C                   MOVEL     VIDNAR        KCLNAR
074400970530     C                   MOVEL     VIDCAP        KCLCAP
074500970530     C                   MOVEL     VIDLOC        KCLLOC
074600970530     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
074700970917IF  3C     *IN99         IFEQ      *OFF
074800970530     C                   SETON                                        606170
074900970530     C                   MOVEL     '1'           ERRL00
075000970530     C                   MOVEL     MSG(8)        DSMSMS
075100970917E   3C                   ENDIF
075200970917E   2C                   ENDIF
075300970917E   1C                   ENDIF
075400970530     C*--------------------
075500970530     C* STAMPARE IN DELIVERY
075600970530     C*--------------------
075700970530IF  1C     ERRL00        IFEQ      '0'
075800970530IF  2C     VIDDLV        IFEQ      'S'
075900970530     C     VIDDLV        OREQ      'N'
076000970530X   2C                   ELSE
076100970530     C                   MOVEL     '1'           ERRL00
076200970530     C                   SETON                                        73
076300970530     C                   MOVEL     MSG(27)       DSMSMS
076400970530E   2C                   ENDIF
076500970530E   1C                   ENDIF
076600970917     C*--------------------
076700970917     C* OBSOLETO
076800970917     C*--------------------
076900970917IF  1C     ERRL00        IFEQ      '0'
077000970917IF  2C     VIDFI1        IFEQ      'O'
077100970917     C     VIDFI1        OREQ      *BLANKS
077200970917X   2C                   ELSE
077300970917     C                   MOVEL     '1'           ERRL00
077400970917     C                   SETON                                        85
077500970917     C                   MOVEL     MSG(25)       DSMSMS
077600970917E   2C                   ENDIF
077700970917E   1C                   ENDIF
077800970917     C*
077900970917     C* VARIAZIONE -> SE CAP OBSOLETO ANCHE LA LOCALITA' DEVE ESSERE OBSOLETA
078000970917IF  1C     ERRL00        IFEQ      '0'
078100970917IF  2C     DEPOP1        IFEQ      'O02'
078200970917IF  3C     WFI1          IFEQ      'O '
078300970917     C     VIDFI1        ANDNE     'O'
078400970917     C                   SETON                                        85
078500970917     C                   MOVEL     MSG(12)       DSMSMS
078600970917     C                   MOVEL     '1'           ERRL00
078700970917E   3C                   ENDIF
078800970917E   2C                   ENDIF
078900970917E   1C                   ENDIF
079000970530     C*--------------------
079100970530     C* LIVELLO DI GESTIONE
079200970530     C*--------------------
079300970530IF  1C     ERRL00        IFEQ      '0'
079400970530IF  2C     VIDLIV        IFEQ      'C'
079500970530     C     VIDLIV        OREQ      'L'
079600970530X   2C                   ELSE
079700970530     C                   SETON                                        74
079800970530     C                   MOVEL     '1'           ERRL00
079900970530     C                   MOVEL     MSG(23)       DSMSMS
080000970530E   2C                   ENDIF
080100970530E   1C                   ENDIF
080200970530     C*
080300970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> IMPORTA I DATI DEL CAPPARIO
080400970530IF  1C     ERRL00        IFEQ      '0'
080500970530IF  2C     VIDLIV        IFEQ      'C'                                          *LIVELLO CAP
080600970530     C                   SETON                                        92        *PROTEZIONE CAMPI
080700970530     C                   EXSR      IMPCPC
080800970530X   2C                   ELSE                                                   *LIVELLO LOCALITA
080900970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
081000970530E   2C                   ENDIF
081100970530E   1C                   ENDIF
081200970530     C*--------------------
081300970604     C* LINEA ARRIVO STANDARD
081400970530     C*--------------------
081500970530IF  1C     ERRL00        IFEQ      '0'
081600970530     C                   MOVEL     *BLANKS       VDDLNA
081700970530     C                   MOVEL     VIDLNA        WLIN
081800970530     C                   MOVEL     *BLANKS       WDLIN
081900970530     C                   EXSR      CHKLIN
082000970530     C                   MOVEL     WLIN          VIDLNA
082100970530     C                   MOVEL     WDLIN         VDDLNA
082200970530IF  2C     ERRL00        IFEQ      '1'
082300970530     C                   SETON                                        62
082400970530     C                   MOVEL     MSG(10)       DSMSMS
082500970530E   2C                   ENDIF
082600970530E   1C                   ENDIF
082700130916
082800130916IF  1C     ERRL00        IFEQ      '0'
082900130917     c     depop1        andne     'F06'
083000130916     c* se cambiata la linea di arrivo verifico se ci sono OrariServizi da eliminare
083100130916     c                   movel     vidlna        w0030             3 0
083200130916     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
083300130916     c* e non ho già avvisato della cancellazione orariservizi
083400130916     c                   if        w0030<>f_lna and F_lna<>s_vidlna
083500130916     c                                          and f_lna<>s_vidlos
083600130916     c                                          and f_lna<>s_vidlol
083700130916     c                                          and f07lna=*blanks
083800130916     c                   clear                   tisio7ds
083900130916     c                   eval      io7mod='C'
084000130917     c* Il livello da passare è quello a video
084100130917     c                   eval      io7liv=vidliv
084200130916     c                   eval      io7ver=d00ver
084300130916     c                   eval      io7nar=vidnar
084400130916     c                   eval      io7cap=vidcap
084500130917     c                   eval      io7loc=vidloc
084600130916     c                   eval      io7lna=f_lna
084700130916     c                   eval      io7lns=%dec(vidlna:3:0)
084800130916     c                   eval      io7los=%dec(vidlos:3:0)
084900130916     c                   eval      io7lol=%dec(vidlol:3:0)
085000131016     c***                movel     tisio7ds      kpjbu
085100130916     c                   call      'TISIO7R'
085200130916     c                   parm                    kpjba
085300131016     c                   parm                    tisio7ds
085400130916     c                   if        oo7err='1'
085500130916     c                   movel     oo7msg        DSmsms
085600130916     c                   eval      *in62  = *on
085700130916     c                   eval      ErrL00 = *on
085800130916     c                   eval      f07lna='S'
085900130916e   4c                   endif
086000130916     c                   eval      s_vidlna=F_lna
086100130916    3c                   endif
086200130916     c                   endif
086300970604     C*--------------------
086400970604     C* ZONA DI CONSEGNA STANDARD
086500970604     C*--------------------
086600970604IF  1C     ERRL00        IFEQ      '0'
086700970604     C                   MOVEL     VIDLNA        WLIN
086800970604     C                   Z-ADD     VIDZNC        WZNC
086900970604     C                   EXSR      CHKZNC
087000970604IF  2C     ERRL00        IFEQ      '1'
087100970604     C                   SETON                                        63
087200970604     C                   MOVEL     MSG(11)       DSMSMS
087300970604E   2C                   ENDIF
087400060206if  2c                   if        §03nuc = 'S'
087500060206     c                   movel     Msg(17)       DSmsms
087600060206     c                   eval      *in63  = *on
087700060206     c                   eval      ErrL00 = *on
087800060206e   2c                   endif
087900970604E   1C                   ENDIF
088000970604     C*--------------------
088100970912     C* LINEA ARRIVO SOTTO
088200970604     C*--------------------
088300970604IF  1C     ERRL00        IFEQ      '0'
088400970604     C                   MOVEL     *BLANKS       VDDLOS
088500970604IF  2C     VIDLOS        IFNE      *ZEROS
088600970604     C     VIDLOS        ANDNE     *BLANKS
088700970604     C                   MOVEL     VIDLOS        WLIN
088800970604     C                   MOVEL     *BLANKS       WDLIN
088900970604     C                   EXSR      CHKLIN
089000970604     C                   MOVEL     WLIN          VIDLOS
089100970604     C                   MOVEL     WDLIN         VDDLOS
089200970604IF  3C     ERRL00        IFEQ      '1'
089300970604     C                   SETON                                        81
089400970604     C                   MOVEL     MSG(10)       DSMSMS
089500970604E   3C                   ENDIF
089600970604E   2C                   ENDIF
089700970604E   1C                   ENDIF
089800130916IF  1C     ERRL00        IFEQ      '0'
089900130917     c     depop1        andne     'F06'
090000130916     c* se cambiata la linea di arrivo verifico se ci sono OrariServizi da eliminare
090100130916     c                   movel     vidlos        w0030             3 0
090200130916     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
090300130916     c* e non ho già avvisato della cancellazione orariservizi
090400130916     c                   if        w0030<>f_los and F_los<>s_vidlna
090500130916     c                                          and f_los<>s_vidlos
090600130916     c                                          and f_los<>s_vidlol
090700130917     c                                          and f07lna=*blanks
090800130916     c                   clear                   tisio7ds
090900130916     c                   eval      io7mod='C'
091000130917     c* Il livello da passare è quello a video
091100130917     c                   eval      io7liv=vidliv
091200130916     c                   eval      io7ver=d00ver
091300130916     c                   eval      io7nar=vidnar
091400130916     c                   eval      io7cap=vidcap
091500130917     c                   eval      io7loc=vidloc
091600130916     c                   eval      io7lna=f_los
091700130916     c                   eval      io7lns=%dec(vidlna:3:0)
091800130916     c                   eval      io7los=%dec(vidlos:3:0)
091900130916     c                   eval      io7lol=%dec(vidlol:3:0)
092000131016     c****               movel     tisio7ds      kpjbu
092100130916     c                   call      'TISIO7R'
092200130916     c                   parm                    kpjba
092300131016     c                   parm                    tisio7ds
092400130916     c                   if        oo7err='1'
092500130916     c                   movel     oo7msg        DSmsms
092600130916     c                   eval      *in81  = *on
092700130916     c                   eval      ErrL00 = *on
092800130916     c                   eval      f07lna='S'
092900130916e   4c                   endif
093000130916     c                   eval      s_vidlos=F_los
093100130916    3c                   endif
093200130916     c                   endif
093300970604     C*--------------------
093400970912     C* ZONA DI CONSEGNA SOTTO
093500970604     C*--------------------
093600970604IF  1C     ERRL00        IFEQ      '0'
093700971126IF  2C     VIDLOS        IFNE      *ZEROS
093800970604     C                   MOVEL     VIDLOS        WLIN
093900970604     C                   Z-ADD     VIDZOS        WZNC
094000970604     C                   EXSR      CHKZNC
094100970604IF  3C     ERRL00        IFEQ      '1'
094200970604     C                   SETON                                        82
094300970604     C                   MOVEL     MSG(11)       DSMSMS
094400970604E   3C                   ENDIF
094500060206if  3c                   if        §03nuc = 'S'
094600060206     c                   movel     Msg(17)       DSmsms
094700060206     c                   eval      *in82  = *on
094800060206     c                   eval      ErrL00 = *on
094900060206e   3c                   endif
095000970604E   2C                   ENDIF
095100970604E   1C                   ENDIF
095200970604     C*--------------------
095300970912     C* PESO/VOLUME SOTTO
095400970604     C*--------------------
095500970604IF  1C     ERRL00        IFEQ      '0'
095600970604IF  2C     VIDLKS        IFGT      *ZEROS
095700970604     C     VIDLMS        ORGT      *ZEROS
095800970604IF  3C     VIDLOS        IFEQ      *BLANKS
095900970604     C     VIDLOS        OREQ      *ZEROS
096000970604     C                   SETON                                        81
096100970604     C                   MOVEL     MSG(18)       DSMSMS
096200970604     C                   MOVEL     '1'           ERRL00
096300970604E   3C                   ENDIF
096400070103E   2C                   ENDIF
096500070103IF  3C     VIDLOS        IFNE      *BLANKS
096600070103     C     VIDLOS        ANDNE     *ZEROS
096700070103IF  4C                   IF        VIDLKS = *ZEROS OR
096800070103     C                             VIDLMS = *ZEROS
096900970604     C                   SETON                                        8384
097000970604     C                   MOVEL     MSG(19)       DSMSMS
097100970604     C                   MOVEL     '1'           ERRL00
097200070103E   4C                   ENDIF
097300070103E   3C                   ENDIF
097400970604E   1C                   ENDIF
097500970530     C*--------------------
097600970912     C* LINEA ARRIVO OLTRE
097700970530     C*--------------------
097800970530IF  1C     ERRL00        IFEQ      '0'
097900970530     C                   MOVEL     *BLANKS       VDDLOL
098000970530IF  2C     VIDLOL        IFNE      *ZEROS
098100970530     C     VIDLOL        ANDNE     *BLANKS
098200970530     C                   MOVEL     VIDLOL        WLIN
098300970530     C                   MOVEL     *BLANKS       WDLIN
098400970530     C                   EXSR      CHKLIN
098500970530     C                   MOVEL     WLIN          VIDLOL
098600970530     C                   MOVEL     WDLIN         VDDLOL
098700970530IF  3C     ERRL00        IFEQ      '1'
098800970530     C                   SETON                                        67
098900970530     C                   MOVEL     MSG(10)       DSMSMS
099000970530E   3C                   ENDIF
099100970530E   2C                   ENDIF
099200970530E   1C                   ENDIF
099300130916IF  1C     ERRL00        IFEQ      '0'
099400130917     c     depop1        andne     'F06'
099500130916     c* se cambiata la linea di arrivo verifico se ci sono OrariServizi da eliminare
099600130916     c                   movel     vidlol        w0030             3 0
099700130916     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
099800130916     c* e non ho già avvisato della cancellazione orariservizi
099900130916     c                   if        w0030<>f_lol and F_lol<>s_vidlna
100000130916     c                                          and f_lol<>s_vidlos
100100130916     c                                          and f_lol<>s_vidlol
100200130917     c                                          and f07lna=*blanks
100300130916     c                   clear                   tisio7ds
100400130916     c                   eval      io7mod='C'
100500130917     c* Il livello da passare è quello a video
100600130917     c                   eval      io7liv=vidliv
100700130916     c                   eval      io7ver=d00ver
100800130916     c                   eval      io7nar=vidnar
100900130916     c                   eval      io7cap=vidcap
101000130917     c                   eval      io7loc=vidloc
101100130916     c                   eval      io7lna=f_lol
101200130916     c                   eval      io7lns=%dec(vidlna:3:0)
101300130916     c                   eval      io7los=%dec(vidlos:3:0)
101400130916     c                   eval      io7lol=%dec(vidlol:3:0)
101500131016     c*****              movel     tisio7ds      kpjbu
101600130916     c                   call      'TISIO7R'
101700130916     c                   parm                    kpjba
101800131016     c                   parm                    tisio7ds
101900130916     c                   if        oo7err='1'
102000130916     c                   movel     oo7msg        DSmsms
102100130916     c                   eval      *in67  = *on
102200130916     c                   eval      ErrL00 = *on
102300130917     c                   eval      f07lna='S'
102400130916e   4c                   endif
102500130916     c                   eval      s_vidlol=F_lol
102600130916    3c                   endif
102700130916     c                   endif
102800970604     C*--------------------
102900970912     C* ZONA DI CONSEGNA OLTRE
103000970604     C*--------------------
103100970604IF  1C     ERRL00        IFEQ      '0'
103200971126IF  2C     VIDLOL        IFNE      *ZEROS
103300970604     C                   MOVEL     VIDLOL        WLIN
103400970604     C                   Z-ADD     VIDZOL        WZNC
103500970604     C                   EXSR      CHKZNC
103600970604IF  3C     ERRL00        IFEQ      '1'
103700970604     C                   SETON                                        68
103800970604     C                   MOVEL     MSG(11)       DSMSMS
103900970604E   3C                   ENDIF
104000060206if  3c                   if        §03nuc = 'S'
104100060206     c                   movel     Msg(17)       DSmsms
104200060206     c                   eval      *in68  = *on
104300060206     c                   eval      ErrL00 = *on
104400060206e   3c                   endif
104500970604E   2C                   ENDIF
104600970604E   1C                   ENDIF
104700970604     C*--------------------
104800970604     C* LIMITE PESO/VOLUME
104900970604     C*--------------------
105000970604IF  1C     ERRL00        IFEQ      '0'
105100970604IF  2C     VIDLKG        IFGT      *ZEROS
105200970604     C     VIDLMC        ORGT      *ZEROS
105300970604IF  3C     VIDLOL        IFEQ      *BLANKS
105400970604     C     VIDLOL        OREQ      *ZEROS
105500970604     C                   SETON                                        67
105600970604     C                   MOVEL     MSG(18)       DSMSMS
105700970604     C                   MOVEL     '1'           ERRL00
105800970604E   3C                   ENDIF
105900070103E   2C                   ENDIF
106000070103IF  3C     VIDLOL        IFNE      *BLANKS
106100070103     C     VIDLOL        ANDNE     *ZEROS
106200070103IF  4C                   IF        VIDLKG = *ZEROS OR
106300070103     C                             VIDLMC = *ZEROS
106400970604     C                   SETON                                        7172
106500970604     C                   MOVEL     MSG(19)       DSMSMS
106600970604     C                   MOVEL     '1'           ERRL00
106700070103E   4C                   ENDIF
106800070103E   3C                   ENDIF
106900970604E   1C                   ENDIF
107000041008     C*
107100041008IF  1C     ERRL00        IFEQ      '0'
107200041008IF  2C                   IF        (VIDLKG <> *zeros AND VIDLMC  = *zeros) OR
107300041008     C                             (VIDLKG  = *zeros AND VIDLMC <> *zeros)
107400041008     C                   SETON                                        7172
107500041008     C                   MOVEL     MSG(33)       DSMSMS
107600041008     C                   MOVEL     '1'           ERRL00
107700041008E   2C                   ENDIF
107800041008E   1C                   ENDIF
107900970604     C*--------------------
108000970912     C* PESO OLTRE + PESO SOTTO
108100970604     C*--------------------
108200970912     C* OLTRE > SOTTO
108300970604IF  1C     ERRL00        IFEQ      '0'
108400970604IF  2C     VIDLKG        IFGT      *ZEROS
108500970604     C     VIDLKS        ANDGT     *ZEROS
108600970604IF  3C     VIDLKG        IFLE      VIDLKS
108700970604     C                   SETON                                        7183
108800970604     C                   MOVEL     MSG(24)       DSMSMS
108900970604     C                   MOVEL     '1'           ERRL00
109000970604E   3C                   ENDIF
109100970604E   2C                   ENDIF
109200970604E   1C                   ENDIF
109300970604     C*--------------------
109400970912     C* VOLUME OLTRE + VOLUME SOTTO
109500970604     C*--------------------
109600970912     C* OLTRE > SOTTO
109700970604IF  1C     ERRL00        IFEQ      '0'
109800970604IF  2C     VIDLMC        IFGT      *ZEROS
109900970604     C     VIDLMS        ANDGT     *ZEROS
110000970604IF  3C     VIDLMC        IFLE      VIDLMS
110100970604     C                   SETON                                        7284
110200970604     C                   MOVEL     MSG(24)       DSMSMS
110300970604     C                   MOVEL     '1'           ERRL00
110400970604E   3C                   ENDIF
110500970604E   2C                   ENDIF
110600970604E   1C                   ENDIF
110700971118     C*--------------------
110800971118     C* LINEA ARRIVO OLTRE 2
110900971118     C*--------------------
111000971118IF  1C     ERRL00        IFEQ      '0'
111100971118     C                   MOVEL     *BLANKS       VDDLO2
111200971118IF  2C     VIDLO2        IFNE      *ZEROS
111300971118     C     VIDLO2        ANDNE     *BLANKS
111400971118     C                   MOVEL     VIDLO2        WLIN
111500971118     C                   MOVEL     *BLANKS       WDLIN
111600971118     C                   EXSR      CHKLIN
111700971118     C                   MOVEL     WLIN          VIDLO2
111800971118     C                   MOVEL     WDLIN         VDDLO2
111900971118IF  3C     ERRL00        IFEQ      '1'
112000971118     C                   SETON                                        86
112100971118     C                   MOVEL     MSG(10)       DSMSMS
112200971118E   3C                   ENDIF
112300971118E   2C                   ENDIF
112400971118E   1C                   ENDIF
112500971118     C*--------------------
112600971118     C* ZONA DI CONSEGNA OLTRE 2
112700971118     C*--------------------
112800971118IF  1C     ERRL00        IFEQ      '0'
112900971126IF  2C     VIDLO2        IFNE      *ZEROS
113000971118     C                   MOVEL     VIDLO2        WLIN
113100971118     C                   Z-ADD     VIDZO2        WZNC
113200971118     C                   EXSR      CHKZNC
113300971118IF  3C     ERRL00        IFEQ      '1'
113400971118     C                   SETON                                        87
113500971118     C                   MOVEL     MSG(11)       DSMSMS
113600971118E   3C                   ENDIF
113700060206if  3c                   if        §03nuc = 'S'
113800060206     c                   movel     Msg(17)       DSmsms
113900060206     c                   eval      *in87  = *on
114000060206     c                   eval      ErrL00 = *on
114100060206e   3c                   endif
114200971118E   2C                   ENDIF
114300971118E   1C                   ENDIF
114400971118     C*--------------------
114500971126     C* LIMITE PESO/VOLUME OLTRE 2
114600971118     C*--------------------
114700971126IF  1C*---       ERRL00    IFEQ '0'
114800971126IF  2C*---       VIDLK2    IFGT *ZEROS
114900971126     C*---       VIDLM2    ORGT *ZEROS
115000971126IF  3C*---       VIDLO2    IFEQ *BLANKS
115100971126     C*---       VIDLO2    OREQ *ZEROS
115200971126     C*---                 SETON                     86
115300971126     C*---                 MOVELMSG,18    DSMSMS
115400971126     C*---                 MOVEL'1'       ERRL00
115500971126E   3C*---                 ENDIF
115600971126X   2C*---                 ELSE
115700971126IF  3C*---       VIDLO2    IFNE *BLANKS
115800971126     C*---       VIDLO2    ANDNE*ZEROS
115900971126     C*---                 SETON                     8889
116000971126     C*---                 MOVELMSG,19    DSMSMS
116100971126     C*---                 MOVEL'1'       ERRL00
116200971126E   3C*---                 ENDIF
116300971126E   2C*---                 ENDIF
116400971126E   1C*---                 ENDIF
116500970530     C*--------------------
116600970530     C* LINEA ARRIVO FERMO DEPOSITO -OUTPUT-
116700970530     C*--------------------
116800970530IF  1C     ERRL00        IFEQ      '0'
116900971219     C                   MOVEL     VIDLNA        VIDLFD
117000971219     C                   MOVEL     *BLANKS       VDDLFD
117100970530IF  2C     VIDLFD        IFGT      *ZEROS
117200970530     C                   MOVEL     VIDLFD        WLIN
117300970530     C                   MOVEL     *BLANKS       WDLIN
117400970530     C                   EXSR      CHKLIN
117500970530     C                   MOVEL     WLIN          VIDLFD
117600970530     C                   MOVEL     WDLIN         VDDLFD
117700970530E   2C                   ENDIF
117800970530E   1C                   ENDIF
117900970530     C*--------------------
118000970530     C* ZONA DI CONSEGNA FERMO DEPOSITO
118100970530     C*--------------------
118200970530IF  1C     ERRL00        IFEQ      '0'
118300971126IF  2C     VIDLFD        IFNE      *ZEROS
118400970530     C                   MOVEL     VIDLFD        WLIN
118500970530     C                   Z-ADD     VIDZFD        WZNC
118600970530     C                   EXSR      CHKZNC
118700970530IF  3C     ERRL00        IFEQ      '1'
118800970530     C                   SETON                                        66
118900970530     C                   MOVEL     MSG(11)       DSMSMS
119000970530E   3C                   ENDIF
119100060206if  3c                   if        §03nuc = 'S'
119200060206     c                   movel     Msg(17)       DSmsms
119300060206     c                   eval      *in66  = *on
119400060206     c                   eval      ErrL00 = *on
119500060206e   3c                   endif
119600970530E   2C                   ENDIF
119700970530E   1C                   ENDIF
119800970530     C*--------------------
119900970530     C* CODICE TASSAZIONE
120000970530     C*--------------------
120100970530IF  1C     ERRL00        IFEQ      '0'
120200970530     C                   MOVEL     *BLANKS       VDDCTS
120300970530     C                   MOVEL     VIDCTS        WCTS
120400970530     C                   MOVEL     *BLANKS       WDCTS
120500970530     C                   EXSR      CHKCTS
120600970530     C                   MOVEL     WCTS          VIDCTS
120700970530     C                   MOVEL     WDCTS         VDDCTS
120800970530IF  2C     ERRL00        IFEQ      '1'
120900970530     C                   SETON                                        58
121000970530     C                   MOVEL     MSG(14)       DSMSMS
121100970530E   2C                   ENDIF
121200970530E   1C                   ENDIF
121300970530     C*--------------------
121400970530     C* CODICE TASSAZIONE ITALIA PER ESTERO
121500970530     C*--------------------
121600970530IF  1C     ERRL00        IFEQ      '0'
121700970530     C                   MOVEL     *BLANKS       VDDCTE
121800970530IF  2C     VIDNAR        IFEQ      *BLANKS                                      *NAZIONE ITALIA
121900970530     C                   MOVEL     VIDCTE        WA1D              1
122000970530     C                   MOVE      VIDCTE        WA1S              1
122100970530IF  3C     WA1D          IFNE      'I'
122200970530     C     WA1S          OREQ      *BLANKS
122300970530     C                   MOVEL     '1'           ERRL00
122400970530     C                   SETON                                        59
122500970530     C                   MOVEL     MSG(15)       DSMSMS
122600970530E   3C                   ENDIF
122700970530X   2C                   ELSE                                                   *NAZIONE ESTERO
122800970530IF  3C     VIDCTE        IFNE      *BLANKS
122900970530     C                   MOVEL     '1'           ERRL00
123000970530     C                   SETON                                        59
123100970530     C                   MOVEL     MSG(16)       DSMSMS
123200970530E   3C                   ENDIF
123300970530E   2C                   ENDIF
123400970530E   1C                   ENDIF
123500970530     C*--------------------
123600970530     C* INOLTRO
123700970530     C*--------------------
123800970530IF  1C     ERRL00        IFEQ      '0'
123900970530     C                   MOVEL     *BLANKS       VDDISO
124000970530     C                   MOVEL     VIDISO        WINL
124100970530     C                   MOVEL     *BLANKS       WDINL
124200970530     C                   EXSR      CHKINL
124300970530     C                   MOVEL     WINL          VIDISO
124400970530     C                   MOVEL     WDINL         VDDISO
124500970530IF  2C     ERRL00        IFEQ      '1'
124600970530     C                   SETON                                        75
124700970530     C                   MOVEL     MSG(28)       DSMSMS
124800970530E   2C                   ENDIF
124900970530E   1C                   ENDIF
125000970530     C*--------------------
125100970530     C* POSSIBILE INOLTRO ISOLA
125200970530     C*--------------------
125300970530IF  1C     ERRL00        IFEQ      '0'
125400970530IF  2C     VIDPII        IFEQ      'S'
125500970530     C     VIDPII        OREQ      'N'
125600970530X   2C                   ELSE
125700970530     C                   MOVEL     '1'           ERRL00
125800970530     C                   SETON                                        76
125900970530     C                   MOVEL     MSG(27)       DSMSMS
126000970530E   2C                   ENDIF
126100970530E   1C                   ENDIF
126200970530     C*--------------------
126300970530     C* POSSIBILE INOLTRO LOCALITA' DISAGIATA
126400970530     C*--------------------
126500970530IF  1C     ERRL00        IFEQ      '0'
126600970530IF  2C     VIDPID        IFEQ      'S'
126700970530     C     VIDPID        OREQ      'N'
126800970530X   2C                   ELSE
126900970530     C                   MOVEL     '1'           ERRL00
127000970530     C                   SETON                                        77
127100970530     C                   MOVEL     MSG(27)       DSMSMS
127200970530E   2C                   ENDIF
127300970530E   1C                   ENDIF
127400970530     C*--------------------
127500970530     C* POSSIBILE INOLTRO CENTRO STORICO
127600970530     C*--------------------
127700970530IF  1C     ERRL00        IFEQ      '0'
127800970530IF  2C     VIDPIS        IFEQ      'S'
127900970530     C     VIDPIS        OREQ      'N'
128000970530X   2C                   ELSE
128100970530     C                   MOVEL     '1'           ERRL00
128200970530     C                   SETON                                        78
128300970530     C                   MOVEL     MSG(27)       DSMSMS
128400970530E   2C                   ENDIF
128500970530E   1C                   ENDIF
128600970530     C*--------------------
128700970530     C* GESTIONE DDT
128800970530     C*--------------------
128900970530IF  1C     ERRL00        IFEQ      '0'
129000970530IF  2C     VIDGDD        IFEQ      'S'
129100970530     C     VIDGDD        OREQ      'N'
129200970530X   2C                   ELSE
129300970530     C                   MOVEL     '1'           ERRL00
129400970530     C                   SETON                                        79
129500970530     C                   MOVEL     MSG(27)       DSMSMS
129600970530E   2C                   ENDIF
129700970530E   1C                   ENDIF
129800970530     C*--------------------
129900970530     C* TIPO TEMPO CONSEGNA
130000970530     C*--------------------
130100970530IF  1C     ERRL00        IFEQ      '0'
130200010322     C                   MOVEL     VIDTTC        WTTC                           *CODICE TEMPO
130300970530     C                   Z-ADD     *ZEROS        WD0I                           *TEMPI DISTRIBUZIONE
130400970530     C                   Z-ADD     *ZEROS        WD1I
130500970530     C                   Z-ADD     *ZEROS        WD0C
130600970530     C                   Z-ADD     *ZEROS        WD1C
130700970530     C                   Z-ADD     *ZEROS        WDIS
130800970530     C                   Z-ADD     *ZEROS        WDCS
130900970530     C                   Z-ADD     *ZEROS        WDLD
131000090903
131100970530     C                   Z-ADD     *ZEROS        WC0I                           *TEMPI CORRIERE
131200970530     C                   Z-ADD     *ZEROS        WC1I
131300970530     C                   Z-ADD     *ZEROS        WC0C
131400970530     C                   Z-ADD     *ZEROS        WC1C
131500970530     C                   Z-ADD     *ZEROS        WCIS
131600970530     C                   Z-ADD     *ZEROS        WCCS
131700970530     C                   Z-ADD     *ZEROS        WCLD
131800090903
131900970530     C                   Z-ADD     *ZEROS        WE0I                           *TEMPI ESPRESSO
132000970530     C                   Z-ADD     *ZEROS        WE1I
132100970530     C                   Z-ADD     *ZEROS        WE0C
132200970530     C                   Z-ADD     *ZEROS        WE1C
132300970530     C                   Z-ADD     *ZEROS        WEIS
132400970530     C                   Z-ADD     *ZEROS        WECS
132500970530     C                   Z-ADD     *ZEROS        WELD
132600090903
132700090903     C                   Z-ADD     *ZEROS        Wh0I                           *TEMPI H1030
132800090903     C                   Z-ADD     *ZEROS        Wh1I
132900090903     C                   Z-ADD     *ZEROS        Wh0C
133000090903     C                   Z-ADD     *ZEROS        Wh1C
133100090903     C                   Z-ADD     *ZEROS        WhIS
133200090903     C                   Z-ADD     *ZEROS        WhCS
133300090903     C                   Z-ADD     *ZEROS        WhLD
133400090903
133500970530     C                   EXSR      CHKTTC
133600970530     C                   MOVEL     WTTC          VIDTTC                         *CODICE TEMPO
133700970530     C                   Z-ADD     WD0I          VIDD0I                         *TEMPI DISTRIBUZIONE
133800970530     C                   Z-ADD     WD1I          VIDD1I
133900970530     C                   Z-ADD     WD0C          VIDD0C
134000970530     C                   Z-ADD     WD1C          VIDD1C
134100970530     C                   Z-ADD     WDIS          VIDDIS
134200970530     C                   Z-ADD     WDCS          VIDDCS
134300970530     C                   Z-ADD     WDLD          VIDDLD
134400090903
134500970530     C                   Z-ADD     WC0I          VIDC0I                         *TEMPI CORRIERE
134600970530     C                   Z-ADD     WC1I          VIDC1I
134700970530     C                   Z-ADD     WC0C          VIDC0C
134800970530     C                   Z-ADD     WC1C          VIDC1C
134900970530     C                   Z-ADD     WCIS          VIDCIS
135000970530     C                   Z-ADD     WCCS          VIDCCS
135100970530     C                   Z-ADD     WCLD          VIDCLD
135200090903
135300970530     C                   Z-ADD     WE0I          VIDE0I                         *TEMPI ESPRESSO
135400970530     C                   Z-ADD     WE1I          VIDE1I
135500970530     C                   Z-ADD     WE0C          VIDE0C
135600970530     C                   Z-ADD     WE1C          VIDE1C
135700970530     C                   Z-ADD     WEIS          VIDEIS
135800970530     C                   Z-ADD     WECS          VIDECS
135900970530     C                   Z-ADD     WELD          VIDELD
136000090903
136100090903     C                   Z-ADD     Wh0I          VIDh0I                         *TEMPI H1030
136200090903     C                   Z-ADD     Wh1I          VIDh1I
136300090903     C                   Z-ADD     Wh0C          VIDh0C
136400090903     C                   Z-ADD     Wh1C          VIDh1C
136500090903     C                   Z-ADD     WhIS          VIDhIS
136600090903     C                   Z-ADD     WhCS          VIDhCS
136700090903     C                   Z-ADD     WhLD          VIDhLD
136800970530IF  2C     ERRL00        IFEQ      '1'
136900970530     C                   SETON                                        80
137000970530     C                   MOVEL     MSG(30)       DSMSMS
137100970530E   2C                   ENDIF
137200970530E   1C                   ENDIF
137300010322     C*--------------------
137400010322     C* TIPO TEMPO RITIRO
137500010322     C*--------------------
137600010322IF  1C     ERRL00        IFEQ      '0'
137700010322     C                   MOVEL     VIDTTR        WTTR                           *CODICE TEMPO
137800010322     C                   Z-ADD     *ZEROS        WD0IR                          *TEMPI DISTRIBUZIONE
137900010322     C                   Z-ADD     *ZEROS        WD1IR
138000010322     C                   Z-ADD     *ZEROS        WD0CR
138100010322     C                   Z-ADD     *ZEROS        WD1CR
138200010322     C                   Z-ADD     *ZEROS        WDISR
138300010322     C                   Z-ADD     *ZEROS        WDCSR
138400010322     C                   Z-ADD     *ZEROS        WDLDR
138500010322     C                   Z-ADD     *ZEROS        WC0IR                          *TEMPI CORRIERE
138600010322     C                   Z-ADD     *ZEROS        WC1IR
138700010322     C                   Z-ADD     *ZEROS        WC0CR
138800010322     C                   Z-ADD     *ZEROS        WC1CR
138900010322     C                   Z-ADD     *ZEROS        WCISR
139000010322     C                   Z-ADD     *ZEROS        WCCSR
139100010322     C                   Z-ADD     *ZEROS        WCLDR
139200010322     C                   Z-ADD     *ZEROS        WE0IR                          *TEMPI ESPRESSO
139300010322     C                   Z-ADD     *ZEROS        WE1IR
139400010322     C                   Z-ADD     *ZEROS        WE0CR
139500010322     C                   Z-ADD     *ZEROS        WE1CR
139600010322     C                   Z-ADD     *ZEROS        WEISR
139700010322     C                   Z-ADD     *ZEROS        WECSR
139800010322     C                   Z-ADD     *ZEROS        WELDR
139900010322     C                   EXSR      CHKTTR
140000010322     C                   MOVEL     WTTR          VIDTTR                         *CODICE TEMPO
140100010322     C                   Z-ADD     WD0IR         VIDD0IR                        *TEMPI DISTRIBUZIONE
140200010322     C                   Z-ADD     WD1IR         VIDD1IR
140300010322     C                   Z-ADD     WD0CR         VIDD0CR
140400010322     C                   Z-ADD     WD1CR         VIDD1CR
140500010322     C                   Z-ADD     WDISR         VIDDISR
140600010322     C                   Z-ADD     WDCSR         VIDDCSR
140700010322     C                   Z-ADD     WDLDR         VIDDLDR
140800010322     C                   Z-ADD     WC0IR         VIDC0IR                        *TEMPI CORRIERE
140900010322     C                   Z-ADD     WC1IR         VIDC1IR
141000010322     C                   Z-ADD     WC0CR         VIDC0CR
141100010322     C                   Z-ADD     WC1CR         VIDC1CR
141200010322     C                   Z-ADD     WCISR         VIDCISR
141300010322     C                   Z-ADD     WCCSR         VIDCCSR
141400010322     C                   Z-ADD     WCLDR         VIDCLDR
141500010322     C                   Z-ADD     WE0IR         VIDE0IR                        *TEMPI ESPRESSO
141600010322     C                   Z-ADD     WE1IR         VIDE1IR
141700010322     C                   Z-ADD     WE0CR         VIDE0CR
141800010322     C                   Z-ADD     WE1CR         VIDE1CR
141900010322     C                   Z-ADD     WEISR         VIDEISR
142000010322     C                   Z-ADD     WECSR         VIDECSR
142100010322     C                   Z-ADD     WELDR         VIDELDR
142200010322IF  2C     ERRL00        IFEQ      '1'
142300010322     C                   SETON                                        44
142400010322     C                   MOVEL     MSG(32)       DSMSMS
142500010322E   2C                   ENDIF
142600010322E   1C                   ENDIF
142700130917     c*
142800130917     c                   clear                   wdel              1
142900130917     c                   exsr      sr_cpe
143000970530     C*
143100970530     C                   ENDSR
143200970618     C*------------------------------------------------------------------------*
143300970618     C* DECF06 - CONTROLLA CAMPI E DECODIFICA -SOLO ALLA CONFERMA-
143400970618     C*------------------------------------------------------------------------*
143500970618     C     DECF06        BEGSR
143600970618     C*--------------------
143700970618     C* LOCALITA' GESTITA A LIVELLO DI LOCALITA'
143800970618     C*--------------------
143900970618     C* DATI CAP + DATI LOCALITA'
144000091211IF  1C*    ERRL00        IFEQ      '0'
144100091211IF  2C*    VIDLIV        IFEQ      'L'                                          *LOCALITA' GESTITA
144200091211     C*                  MOVEL     VIDLNA        NLNA                           *CAMPI NUMERICI
144300091211     C*                  MOVEL     VIDLOS        NLOS
144400091211     C*                  MOVEL     VIDLOL        NLOL
144500091211     C*                  MOVEL     VIDLO2        NLO2
144600091211     C*                  MOVEL     VIDTTC        NTTC
144700091211     C*                  MOVE      VIDTTR        NTTR
144800091211IF  3C*    NLNA          IFEQ      CPCLNA
144900091211     C*    VIDZNC        ANDEQ     CPCZNC
145000091211     C*    VIDISO        ANDEQ     CPCISO
145100091211     C*    VIDZFD        ANDEQ     CPCZFD
145200091211     C*    VIDLKG        ANDEQ     CPCLKG
145300091211     C*    VIDLMC        ANDEQ     CPCLMC
145400091211     C*    NLOL          ANDEQ     CPCLOL
145500091211     C*    VIDZOL        ANDEQ     CPCZOL
145600091211     C*    VIDLKS        ANDEQ     CPCLKS
145700091211     C*    VIDLMS        ANDEQ     CPCLMS
145800091211     C*    NLO2          ANDEQ     CPCLO2
145900091211     C*    VIDZO2        ANDEQ     CPCZO2
146000091211     C*    VIDLK2        ANDEQ     CPCLK2
146100091211     C*    VIDLM2        ANDEQ     CPCLM2
146200091211     C*    NLOS          ANDEQ     CPCLOS
146300091211     C*    VIDZOS        ANDEQ     CPCZOS
146400091211     C*    VIDCTS        ANDEQ     CPCCTS
146500091211     C*    VIDCTE        ANDEQ     CPCCTE
146600091211     C*    NTTC          ANDEQ     CPCTTC
146700091211     C*    NTTR          ANDEQ     CPCFI2
146800091211     C*    VIDPII        ANDEQ     CPCPII
146900091211     C*    VIDPID        ANDEQ     CPCPID
147000091211     C*    VIDPIS        ANDEQ     CPCPIS
147100091211     C*    VIDGDD        ANDEQ     CPCGDD
147200091211     C*                  SETON                                        606170
147300091211     C*                  SETON                                        74
147400091211     C*                  MOVEL     MSG(29)       DSMSMS
147500091211     C*                  MOVEL     '1'           ERRL00
147600091211E   3C*                  ENDIF
147700091211E   2C*                  ENDIF
147800091211E   1C*                  ENDIF
147900970618     C*
148000970618     C* LIVELLO DI GESTIONE = CAP SE LA LOCALITA' E' UNICA
148100970618IF  1C     ERRL00        IFEQ      '0'
148200970618IF  2C     VIDLIV        IFEQ      'L'                                          *LOCALITA' GESTITA
148300970618IF  3C     DEPOP1        IFEQ      'F06'
148400970618     C                   Z-ADD     1             I
148500970618X   3C                   ELSE
148600970618     C                   Z-ADD     *ZEROS        I
148700970618E   3C                   ENDIF
148800970618     C                   Z-ADD     D00VER        KCLVER
148900970618     C                   MOVEL     VIDNAR        KCLNAR
149000970618     C                   MOVEL     VIDCAP        KCLCAP
149100970618     C     KE2CPL        SETLL     AZCPL01L
149200970618     C     KE2CPL        READE(N)  AZCPL01L                               99
149300970618DO  3C     *IN99         DOWEQ     *OFF
149400970618IF  4C     CPLATB        IFEQ      *BLANKS
149500970618     C                   ADD       1             I                              *NUMERO LOCALITA'
149600970618E   4C                   ENDIF
149700970618     C     KE2CPL        READE(N)  AZCPL01L                               99
149800970618E   3C                   ENDDO
149900970618IF  3C     I             IFEQ      1
150000970618     C                   SETON                                        606170
150100970618     C                   SETON                                        74
150200970618     C                   MOVEL     MSG(26)       DSMSMS
150300970618     C                   MOVEL     '1'           ERRL00
150400970618E   3C                   ENDIF
150500970618E   2C                   ENDIF
150600970618E   1C                   ENDIF
150700970618     C*
150800970618     C                   ENDSR
150900970530     C*------------------------------------------------------------------------*
151000970530     C* CHKNAR - CONTROLLA LA NAZIONE
151100970530     C*------------------------------------------------------------------------*
151200970530     C     CHKNAR        BEGSR
151300970530     C*
151400970530     C     *LIKE         DEFINE    VIDNAR        WNAR
151500970530     C     *LIKE         DEFINE    VDDNAR        WDNAR
151600970530     C*
151700970530     C* RICHIESTO ELENCO
151800970530     C                   SETOFF                                           28
151900970530     C     '?'           SCAN      WNAR                                   28
152000970530IF  1C     *IN28         IFEQ      *ON
152100020906     C                   Z-ADD     1             §KUT              1 0
152200970530     C                   MOVEL     '15'          §COD              2
152300970530     C                   MOVEL     *BLANKS       §KEY              8
152400970530     C                   MOVEL     *BLANKS       §DES             30
152500970530     C                   CALL      'X§TABER'
152600970530     C                   PARM                    §KUT
152700970530     C                   PARM                    §COD
152800970530     C                   PARM                    §KEY
152900970530     C                   PARM                    §DES
153000970530     C                   MOVEL     §KEY          WNAR
153100970530     C                   MOVEL     §DES          WDNAR
153200970530E   1C                   ENDIF
153300970530     C*
153400970530     C* CONTROLLA ESISTENZA CODICE
153500020906     C                   MOVEL     1             KTBKUT
153600970530     C                   MOVEL     '15'          KTBCOD
153700970530     C                   MOVEL(P)  WNAR          KTBKEY
153800970530     C     KEYTAB        CHAIN     TABEL00F                           99
153900970530IF  1C     *IN99         IFEQ      *OFF
154000970530     C     TBLFLG        ANDEQ     *BLANKS
154100970530     C                   MOVEL     TBLUNI        DS15
154200970530     C                   MOVEL     §15DES        WDNAR
154300970530     C*
154400970530     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
154500970530IF  2C     VIDNAR        IFEQ      'I  '
154600970530     C                   MOVEL     '1'           ERRL00
154700970530     C                   MOVEL     *BLANKS       WDNAR
154800970530E   2C                   ENDIF
154900970530X   1C                   ELSE
155000970530     C                   MOVEL     '1'           ERRL00
155100970530E   1C                   ENDIF
155200970530     C*
155300970530     C                   ENDSR
155400970530     C*------------------------------------------------------------------------*
155500970530     C* CHKCAP - CONTROLLA IL CAP
155600970530     C*------------------------------------------------------------------------*
155700970530     C     CHKCAP        BEGSR
155800970530     C*
155900970530     C     *LIKE         DEFINE    VIDCAP        WCAP
156000970917     C     *LIKE         DEFINE    O95CF1        WFI1
156100970626     C*
156200970626     C* CONTROLLA IL CAP
156300970626     C                   CLEAR                   DSSI95
156400970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
156500970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
156600970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
156700970626     C                   MOVEL     WCAP          I95CAP                         *CAP
156800970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
156900970626     C                   CALL      'TISI95R'
157000970626     C                   PARM                    DSSI95
157100970626IF  1C     O95ERR        IFNE      *BLANKS
157200970626     C                   MOVEL     '1'           ERRL00
157300970917X   1C                   ELSE
157400970917     C                   MOVEL     O95CF1        WFI1
157500970917E   1C                   ENDIF
157600970530     C*
157700970530     C                   ENDSR
157800970530     C*------------------------------------------------------------------------*
157900970530     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
158000970530     C*------------------------------------------------------------------------*
158100970530     C     CHKLIN        BEGSR
158200970530     C*
158300970530     C     *LIKE         DEFINE    VIDLNA        WLIN
158400970530     C     *LIKE         DEFINE    VDDLNA        WDLIN
158500970530     C*
158600970530     C* RICHIESTO ELENCO
158700970530     C                   SETOFF                                           28
158800970530     C     '?'           SCAN      WLIN                                   28
158900970530IF  1C     *IN28         IFEQ      *ON
159000970530     C                   MOVEL     *BLANKS       §COD1             3
159100970530     C                   MOVEL     *BLANKS       §TIP              1
159200970530     C                   MOVEL     *BLANKS       §DES1            25
159300971203     C                   MOVEL     *BLANKS       §PRA              3
159400971203     C                   CALL      'TNSD24R'
159500970530     C                   PARM                    §COD1
159600970530     C                   PARM                    §TIP
159700970530     C                   PARM                    §DES1
159800971203     C                   PARM                    §PRA
159900970530     C                   MOVEL     §COD1         WLIN
160000970530     C                   MOVEL     §DES1         WDLIN
160100970530E   1C                   ENDIF
160200970530     C*
160300970530     C* SOLO CARATTERI NUMERICI
160400970530     C                   SETOFF                                       28
160500970530     C                   TESTN                   WLIN                 28
160600970530IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
160700970530     C                   MOVEL     '1'           ERRL00
160800970530X   1C                   ELSE                                                   *E'NUMERO DA TESTN
160900970530     C                   MOVE      WLIN          W001              1
161000970530IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
161100970530     C                   MOVEL     '1'           ERRL00
161200970530X   2C                   ELSE                                                   *NUMERO
161300970530     C*
161400970530     C* CONTROLLA ESISTENZA CODICE
161500970530     C                   MOVEL     WLIN          KORFIL
161600970530     C     KEYORG        CHAIN     AZORG01L                           99
161700970530IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
161800970530     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
161900970530     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
162000970530     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
162100970530X   3C                   ELSE
162200970530     C                   MOVEL     '1'           ERRL00
162300970530E   3C                   ENDIF
162400970530E   2C                   ENDIF
162500970530E   1C                   ENDIF
162600970530     C*
162700970530     C                   ENDSR
162800970530     C*------------------------------------------------------------------------*
162900970530     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
163000970530     C*------------------------------------------------------------------------*
163100970530     C     CHKZNC        BEGSR
163200970530     C*
163300970530     C     *LIKE         DEFINE    VIDZNC        WZNC
163400970530     C*
163500970530     C* CONTROLLA ESISTENZA CODICE
163600020906     C                   Z-ADD     1             KTBKUT
163700970530     C                   MOVEL     '03'          KTBCOD
163800970530     C                   MOVEL     WLIN          WA5               5
163900970530     C                   MOVE      WZNC          WA5
164000970530     C                   MOVEL(P)  WA5           KTBKEY
164100970530     C     KEYTAB        CHAIN     TABEL00F                           99
164200970530IF  1C     *IN99         IFEQ      *OFF
164300970530     C     TBLFLG        ANDEQ     *BLANKS
164400970530     C                   MOVEL     TBLUNI        DS03
164500970530X   1C                   ELSE
164600970530     C                   MOVEL     '1'           ERRL00
164700970530E   1C                   ENDIF
164800970530     C*
164900970530     C                   ENDSR
165000970530     C*------------------------------------------------------------------------*
165100970530     C* CHKCTS - CONTROLLA IL CODICE TASSAZIONE
165200970530     C*------------------------------------------------------------------------*
165300970530     C     CHKCTS        BEGSR
165400970530     C*
165500970530     C     *LIKE         DEFINE    VIDCTS        WCTS
165600970530     C     *LIKE         DEFINE    VDDCTS        WDCTS
165700970530     C*
165800970530     C* RICHIESTO ELENCO
165900970530     C                   SETOFF                                           28
166000970530     C     '?'           SCAN      WCTS                                   28
166100970530IF  1C     *IN28         IFEQ      *ON
166200020906     C                   Z-ADD     1             §KUT              1 0
166300970530     C                   MOVEL     'CT'          §COD              2
166400970530     C                   MOVEL     *BLANKS       §KEY              8
166500970530     C                   MOVEL     *BLANKS       §DES             30
166600970530     C                   CALL      'X§TABER'
166700970530     C                   PARM                    §KUT
166800970530     C                   PARM                    §COD
166900970530     C                   PARM                    §KEY
167000970530     C                   PARM                    §DES
167100970530     C                   MOVEL     §KEY          WCTS
167200970530     C                   MOVEL     §DES          WDCTS
167300970530E   1C                   ENDIF
167400970530     C*
167500970530     C* CONTROLLA ESISTENZA CODICE
167600020906     C                   MOVEL     1             KTBKUT
167700970530     C                   MOVEL     'CT'          KTBCOD
167800970530     C                   MOVEL(P)  WCTS          KTBKEY
167900970530     C     KEYTAB        CHAIN     TABEL00F                           99
168000970530IF  1C     *IN99         IFEQ      *OFF
168100970530     C     TBLFLG        ANDEQ     *BLANKS
168200970530     C                   MOVEL     TBLUNI        DSCT
168300970530     C                   MOVEL     §CTDES        WDCTS
168400970530X   1C                   ELSE
168500970530     C                   MOVEL     '1'           ERRL00
168600970530E   1C                   ENDIF
168700970530     C*
168800970530     C                   ENDSR
168900970530     C*------------------------------------------------------------------------*
169000970530     C* CHKINL - CONTROLLA L'INOLTRO
169100970530     C*------------------------------------------------------------------------*
169200970530     C     CHKINL        BEGSR
169300970530     C*
169400970530     C     *LIKE         DEFINE    VIDISO        WINL
169500970530     C     *LIKE         DEFINE    VDDISO        WDINL
169600970530     C*
169700970530     C* RICHIESTO ELENCO
169800970530     C                   SETOFF                                           28
169900970530     C     '?'           SCAN      WINL                                   28
170000970530IF  1C     *IN28         IFEQ      *ON
170100020906     C                   Z-ADD     1             §KUT              1 0
170200970530     C                   MOVEL     '9A'          §COD              2
170300970530     C                   MOVEL     *BLANKS       §KEY              8
170400970530     C                   MOVEL     *BLANKS       §DES             30
170500970530     C                   CALL      'X§TABER'
170600970530     C                   PARM                    §KUT
170700970530     C                   PARM                    §COD
170800970530     C                   PARM                    §KEY
170900970530     C                   PARM                    §DES
171000970530     C                   MOVEL     §KEY          WINL
171100970530     C                   MOVEL     §DES          WDINL
171200970530E   1C                   ENDIF
171300970530     C*
171400970530     C* CONTROLLA ESISTENZA CODICE
171500020906     C                   MOVEL     1             KTBKUT
171600970530     C                   MOVEL     '9A'          KTBCOD
171700970530     C                   MOVEL(P)  WINL          KTBKEY
171800970530     C     KEYTAB        CHAIN     TABEL00F                           99
171900970530IF  1C     *IN99         IFEQ      *OFF
172000970530     C     TBLFLG        ANDEQ     *BLANKS
172100970530     C                   MOVEL     TBLUNI        DS9A
172200970530     C                   MOVEL     §9ADES        WDINL
172300970530X   1C                   ELSE
172400970530     C                   MOVEL     '1'           ERRL00
172500970530E   1C                   ENDIF
172600110623     c* Se non utilizzabile  nel cappario errore
172700110623     c                   if        §9auticpc='N'  and errl00='0'
172800110623     C                   MOVEL     '1'           ERRL00
172900110623E   1C                   ENDIF
173000970530     C*
173100970530     C                   ENDSR
173200970530     C*------------------------------------------------------------------------*
173300970530     C* CHKTTC - CONTROLLA IL TIPO TEMPO CONSEGNA
173400970530     C*------------------------------------------------------------------------*
173500970530     C     CHKTTC        BEGSR
173600970530     C*
173700970530     C     *LIKE         DEFINE    VIDTTC        WTTC                           *TIPO TEMPO
173800970530     C     *LIKE         DEFINE    CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
173900970530     C     *LIKE         DEFINE    CPT1IN        WD1I
174000970530     C     *LIKE         DEFINE    CPT0CN        WD0C
174100970530     C     *LIKE         DEFINE    CPT1CN        WD1C
174200970530     C     *LIKE         DEFINE    CPTTIS        WDIS
174300970530     C     *LIKE         DEFINE    CPTTCS        WDCS
174400970530     C     *LIKE         DEFINE    CPTTLD        WDLD
174500970530     C     *LIKE         DEFINE    CPT0IN        WC0I                           *TEMPI CORRIERE
174600970530     C     *LIKE         DEFINE    CPT1IN        WC1I
174700970530     C     *LIKE         DEFINE    CPT0CN        WC0C
174800970530     C     *LIKE         DEFINE    CPT1CN        WC1C
174900970530     C     *LIKE         DEFINE    CPTTIS        WCIS
175000970530     C     *LIKE         DEFINE    CPTTCS        WCCS
175100970530     C     *LIKE         DEFINE    CPTTLD        WCLD
175200090903
175300970530     C     *LIKE         DEFINE    CPT0IN        WE0I                           *TEMPI ESPRESSO
175400970530     C     *LIKE         DEFINE    CPT1IN        WE1I
175500970530     C     *LIKE         DEFINE    CPT0CN        WE0C
175600970530     C     *LIKE         DEFINE    CPT1CN        WE1C
175700970530     C     *LIKE         DEFINE    CPTTIS        WEIS
175800970530     C     *LIKE         DEFINE    CPTTCS        WECS
175900970530     C     *LIKE         DEFINE    CPTTLD        WELD
176000090903
176100090903     C     *LIKE         DEFINE    CPT0IN        Wh0I                           *TEMPI H1030
176200090903     C     *LIKE         DEFINE    CPT1IN        Wh1I
176300090903     C     *LIKE         DEFINE    CPT0CN        Wh0C
176400090903     C     *LIKE         DEFINE    CPT1CN        Wh1C
176500090903     C     *LIKE         DEFINE    CPTTIS        WhIS
176600090903     C     *LIKE         DEFINE    CPTTCS        WhCS
176700090903     C     *LIKE         DEFINE    CPTTLD        WhLD
176800970530     C*
176900970530     C* RICHIESTA ELENCO CODICI
177000970530     C                   SETOFF                                           28
177100970530     C     '?'           SCAN      WTTC                                   28
177200970530IF  1C     *IN28         IFEQ      *ON
177300970602     C                   MOVEL     *BLANKS       DNAR
177400970602     C                   MOVEL     *BLANKS       DCAP
177500970602     C                   MOVEL     *BLANKS       DLOC
177600970602     C                   MOVEL     'T01'         WOP0
177700970530     C                   MOVEL     *BLANKS       WOP1
177800970530     C                   MOVEL     'TISI41R'     WPGM              8
177900970530     C                   EXSR      LANPGM
178000970530     C                   MOVEL     D00TTC        WTTC
178100970530E   1C                   ENDIF
178200970530     C*
178300010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO CONSEGNA
178400010322     C                   MOVEL     VIDTTC        DEP3A             3
178500010322     C     DEP3A         IFNE      '999'
178600970530     C* CONTROLLA ESISTENZA CODICI
178700970530     C                   Z-ADD     D00VER        KCTVER
178800970530     C                   MOVEL     WTTC          KCTTTC
178900970530     C                   MOVEL     'D'           KCTTSP
179000970530     C     KEYCPT        CHAIN     AZCPT01L                           99
179100970530IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
179200970530     C                   Z-ADD     CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
179300970530     C                   Z-ADD     CPT1IN        WD1I
179400970530     C                   Z-ADD     CPT0CN        WD0C
179500970530     C                   Z-ADD     CPT1CN        WD1C
179600970530     C                   Z-ADD     CPTTIS        WDIS
179700970530     C                   Z-ADD     CPTTCS        WDCS
179800970530     C                   Z-ADD     CPTTLD        WDLD
179900970530     C                   MOVEL     'C'           KCTTSP
180000970530     C     KEYCPT        CHAIN     AZCPT01L                           99
180100970530IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
180200970530     C                   Z-ADD     CPT0IN        WC0I                           *TEMPI CORRIERE
180300970530     C                   Z-ADD     CPT1IN        WC1I
180400970530     C                   Z-ADD     CPT0CN        WC0C
180500970530     C                   Z-ADD     CPT1CN        WC1C
180600970530     C                   Z-ADD     CPTTIS        WCIS
180700970530     C                   Z-ADD     CPTTCS        WCCS
180800970530     C                   Z-ADD     CPTTLD        WCLD
180900970530     C                   MOVEL     'E'           KCTTSP
181000970530     C     KEYCPT        CHAIN     AZCPT01L                           99
181100970530IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
181200970530     C                   Z-ADD     CPT0IN        WE0I                           *TEMPI ESPRESSO
181300970530     C                   Z-ADD     CPT1IN        WE1I
181400970530     C                   Z-ADD     CPT0CN        WE0C
181500970530     C                   Z-ADD     CPT1CN        WE1C
181600970530     C                   Z-ADD     CPTTIS        WEIS
181700970530     C                   Z-ADD     CPTTCS        WECS
181800970530     C                   Z-ADD     CPTTLD        WELD
181900090903     C                   MOVEL     'H'           KCTTSP
182000090903     C     KEYCPT        CHAIN     AZCPT01L                           99
182100090903IF  4C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
182200090903     C                   Z-ADD     CPT0IN        Wh0I                           *TEMPI H1030
182300090903     C                   Z-ADD     CPT1IN        Wh1I
182400090903     C                   Z-ADD     CPT0CN        Wh0C
182500090903     C                   Z-ADD     CPT1CN        Wh1C
182600090903     C                   Z-ADD     CPTTIS        WhIS
182700090903     C                   Z-ADD     CPTTCS        WhCS
182800090903     C                   Z-ADD     CPTTLD        WhLD
182900090903X   4C                   ELSE
183000090903
183100090903     c* Se tipo servizio H non ancora decorrente (anche guardando alla
183200090903     c*    scadenza della versione in visualizzazione)non do errore e non
183300090903     c*    visualizzo nemmeno come descrizione
183400090903
183500090903     c                   if        Datasys>=dataiso
183600090903     c* Verifico anche la  scadenza ddella versione
183700090903     c     d00ver        chain     azcpr01l
183800090903     c                   if        not %found(azcpr01l) or cprdsc>=§vpotsph
183900090903     C                   MOVEL     '1'           ERRL00
184000090903     c                   endif
184100090903     c                   endif
184200090903     c
184300090903     c                   if        errl00='0'
184400090903     c                   clear                   viddtsph
184500090903E   4C                   ENDIF
184600090903
184700090903E   4C                   ENDIF
184800090903
184900970603X   3C                   ELSE
185000970603     C                   MOVEL     '1'           ERRL00
185100970603E   3C                   ENDIF
185200090903
185300970603X   2C                   ELSE
185400970603     C                   MOVEL     '1'           ERRL00
185500970603E   2C                   ENDIF
185600090903
185700970603X   1C                   ELSE
185800970603     C                   MOVEL     '1'           ERRL00
185900970603E   1C                   ENDIF
186000090903
186100010322     C                   ELSE
186200010322     C                   MOVEL     '1'           ERRL00
186300010322     C                   ENDIF
186400970530     C*
186500970530     C                   ENDSR
186600010322     C*------------------------------------------------------------------------*
186700010322     C* CHKTTR - CONTROLLA IL TIPO TEMPO RITIRO
186800010322     C*------------------------------------------------------------------------*
186900010322     C     CHKTTR        BEGSR
187000010322     C*
187100010322     C     *LIKE         DEFINE    VIDTTR        WTTR                           *TIPO TEMPO
187200010322     C     *LIKE         DEFINE    CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
187300010322     C     *LIKE         DEFINE    CPT1IN        WD1IR
187400010322     C     *LIKE         DEFINE    CPT0CN        WD0CR
187500010322     C     *LIKE         DEFINE    CPT1CN        WD1CR
187600010322     C     *LIKE         DEFINE    CPTTIS        WDISR
187700010322     C     *LIKE         DEFINE    CPTTCS        WDCSR
187800010322     C     *LIKE         DEFINE    CPTTLD        WDLDR
187900010322     C     *LIKE         DEFINE    CPT0IN        WC0IR                          *TEMPI CORRIERE
188000010322     C     *LIKE         DEFINE    CPT1IN        WC1IR
188100010322     C     *LIKE         DEFINE    CPT0CN        WC0CR
188200010322     C     *LIKE         DEFINE    CPT1CN        WC1CR
188300010322     C     *LIKE         DEFINE    CPTTIS        WCISR
188400010322     C     *LIKE         DEFINE    CPTTCS        WCCSR
188500010322     C     *LIKE         DEFINE    CPTTLD        WCLDR
188600010322     C     *LIKE         DEFINE    CPT0IN        WE0IR                          *TEMPI ESPRESSO
188700010322     C     *LIKE         DEFINE    CPT1IN        WE1IR
188800010322     C     *LIKE         DEFINE    CPT0CN        WE0CR
188900010322     C     *LIKE         DEFINE    CPT1CN        WE1CR
189000010322     C     *LIKE         DEFINE    CPTTIS        WEISR
189100010322     C     *LIKE         DEFINE    CPTTCS        WECSR
189200010322     C     *LIKE         DEFINE    CPTTLD        WELDR
189300010322     C*
189400010322     C* RICHIESTA ELENCO CODICI
189500010322     C                   SETOFF                                           28
189600010322     C     '?'           SCAN      WTTR                                   28
189700010322IF  1C     *IN28         IFEQ      *ON
189800010322     C                   MOVEL     *BLANK        DNAR
189900010322     C                   MOVEL     *BLANKS       DCAP
190000010322     C                   MOVEL     'T01'         WOP0
190100010322     C                   MOVEL     *BLANKS       WOP1
190200010323     C                   MOVEL     'TISI41R'     WPGM              8
190300010322     C                   EXSR      LANPGM
190400010322     C                   MOVEL     D00TTC        WTTR
190500010322E   1C                   ENDIF
190600010322     C*
190700010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO RITIRO
190800010322     C                   MOVEL     VIDTTR        DEP3A             3
190900010322     C     DEP3A         IFEQ      '999'
191000010322     C* CONTROLLA ESISTENZA CODICI
191100010322     C                   Z-ADD     D00VER        KCTVER
191200010322     C                   MOVEL     WTTR          KCTTTC
191300010322     C                   MOVEL     'D'           KCTTSP
191400010322     C     KEYCPT        CHAIN     AZCPT01L                           99
191500010322IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
191600010322     C                   Z-ADD     CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
191700010322     C                   Z-ADD     CPT1IN        WD1IR
191800010322     C                   Z-ADD     CPT0CN        WD0CR
191900010322     C                   Z-ADD     CPT1CN        WD1CR
192000010322     C                   Z-ADD     CPTTIS        WDISR
192100010322     C                   Z-ADD     CPTTCS        WDCSR
192200010322     C                   Z-ADD     CPTTLD        WDLDR
192300010322     C                   MOVEL     'C'           KCTTSP
192400010322     C     KEYCPT        CHAIN     AZCPT01L                           99
192500010322IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
192600010322     C                   Z-ADD     CPT0IN        WC0IR                          *TEMPI CORRIERE
192700010322     C                   Z-ADD     CPT1IN        WC1IR
192800010322     C                   Z-ADD     CPT0CN        WC0CR
192900010322     C                   Z-ADD     CPT1CN        WC1CR
193000010322     C                   Z-ADD     CPTTIS        WCISR
193100010322     C                   Z-ADD     CPTTCS        WCCSR
193200010322     C                   Z-ADD     CPTTLD        WCLDR
193300010322     C                   MOVEL     'E'           KCTTSP
193400010322     C     KEYCPT        CHAIN     AZCPT01L                           99
193500010322IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
193600010322     C                   Z-ADD     CPT0IN        WE0IR                          *TEMPI ESPRESSO
193700010322     C                   Z-ADD     CPT1IN        WE1IR
193800010322     C                   Z-ADD     CPT0CN        WE0CR
193900010322     C                   Z-ADD     CPT1CN        WE1CR
194000010322     C                   Z-ADD     CPTTIS        WEISR
194100010322     C                   Z-ADD     CPTTCS        WECSR
194200010322     C                   Z-ADD     CPTTLD        WELDR
194300010322X   3C                   ELSE
194400010322     C                   MOVEL     '1'           ERRL00
194500010322E   3C                   ENDIF
194600010322X   2C                   ELSE
194700010322     C                   MOVEL     '1'           ERRL00
194800010322E   2C                   ENDIF
194900010322X   1C                   ELSE
195000010322     C                   MOVEL     '1'           ERRL00
195100010322E   1C                   ENDIF
195200010322     C                   ELSE
195300010322     C                   MOVEL     '1'           ERRL00
195400010322     C                   ENDIF
195500010322     C*
195600010322     C                   ENDSR
195700970526     C*------------------------------------------------------------------------*
195800970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
195900970526     C*------------------------------------------------------------------------*
196000970526     C     IREVID        BEGSR
196100970526     C* CAMPI
196200970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
196300970530IF  1C     CPLESS        IFEQ      'S'
196400970530     C                   MOVEL     'SIN'         VDDESS
196500970530E   1C                   ENDIF
196600970530     C                   MOVEL     CPLNAR        VIDNAR
196700970530     C                   MOVEL     CPLCAP        VIDCAP
196800970530     C                   MOVEL     CPLLOC        VIDLOC
196900970530     C                   MOVEL     CPLLIV        VIDLIV
197000970530     C                   MOVEL     CPLDLV        VIDDLV
197100970917     C                   MOVEL     CPLFI1        VIDFI1
197200970530     C                   MOVEL     CPLLNA        VIDLNA
197300970530     C                   MOVEL     CPLLOL        VIDLOL
197400971118     C                   MOVEL     CPLLO2        VIDLO2
197500970604     C                   MOVEL     CPLLOS        VIDLOS
197600970530     C                   MOVEL     CPLZNC        VIDZNC
197700970530     C                   MOVEL     CPLZOL        VIDZOL
197800971118     C                   MOVEL     CPLZO2        VIDZO2
197900970604     C                   MOVEL     CPLZOS        VIDZOS
198000970530     C                   MOVEL     CPLLNA        VIDLFD
198100970530     C                   MOVEL     CPLZFD        VIDZFD
198200970530     C                   Z-ADD     CPLLKG        VIDLKG
198300970530     C                   Z-ADD     CPLLMC        VIDLMC
198400970604     C                   Z-ADD     CPLLKS        VIDLKS
198500970604     C                   Z-ADD     CPLLMS        VIDLMS
198600971118     C                   Z-ADD     CPLLK2        VIDLK2
198700971118     C                   Z-ADD     CPLLM2        VIDLM2
198800970530     C                   MOVEL     CPLCTS        VIDCTS
198900970530     C                   MOVEL     CPLCTE        VIDCTE
199000970530     C                   MOVEL     CPLISO        VIDISO
199100970530     C                   MOVEL     CPLPII        VIDPII
199200970530     C                   MOVEL     CPLPID        VIDPID
199300970530     C                   MOVEL     CPLPIS        VIDPIS
199400970530     C                   MOVEL     CPLGDD        VIDGDD
199500970530     C                   MOVEL     CPLTTC        VIDTTC
199600010322     C                   MOVEL     *all'9'       VIDTTR
199700010322     C                   MOVE      CPLFI2        VIDTTR
199800970530     C*
199900970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> IMPORTA I DATI DEL CAPPARIO
200000970530IF  2C     VIDLIV        IFEQ      'C'
200100970530     C                   SETON                                        92        *PROTEZIONE CAMPI
200200970530     C                   EXSR      IMPCPC
200300970530E   2C                   ENDIF
200400130916
200500130916     C                   MOVEL     VIDLNA        F_LNA
200600130916     C                   MOVEL     VIDLOL        F_LOL
200700130916     C                   MOVEL     VIDLOS        F_LOS
200800970530     C*
200900970526     C                   ENDSR
201000970526     C*------------------------------------------------------------------------*
201100130916     C* AZZVID - AZZERA I CAMPI DEL VIDEO
201200970526     C*------------------------------------------------------------------------*
201300970526     C     AZZVID        BEGSR
201400970526     C* CAMPI
201500970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
201600970530     C                   MOVEL     *BLANKS       VDDESS
201700970603     C                   MOVEL     D00NAR        VIDNAR
201800970603     C                   MOVEL     D00CAP        VIDCAP
201900970530     C                   MOVEL     *BLANKS       VIDLOC
202000970530     C                   MOVEL     *BLANKS       VIDLIV
202100970530     C                   MOVEL     *BLANKS       VIDDLV
202200970917     C                   MOVEL     *BLANKS       VIDFI1
202300970530     C                   MOVEL     *ZEROS        VIDLNA
202400970530     C                   MOVEL     *ZEROS        VIDLOL
202500971118     C                   MOVEL     *ZEROS        VIDLO2
202600970604     C                   MOVEL     *ZEROS        VIDLOS
202700970530     C                   MOVEL     *ZEROS        VIDZNC
202800970530     C                   MOVEL     *ZEROS        VIDZOL
202900971118     C                   MOVEL     *ZEROS        VIDZO2
203000970604     C                   MOVEL     *ZEROS        VIDZOS
203100970530     C                   MOVEL     *ZEROS        VIDLFD
203200970530     C                   MOVEL     *ZEROS        VIDZFD
203300970530     C                   Z-ADD     *ZEROS        VIDLKG
203400970530     C                   Z-ADD     *ZEROS        VIDLMC
203500970604     C                   Z-ADD     *ZEROS        VIDLKS
203600970604     C                   Z-ADD     *ZEROS        VIDLMS
203700971118     C                   Z-ADD     *ZEROS        VIDLK2
203800971118     C                   Z-ADD     *ZEROS        VIDLM2
203900970530     C                   MOVEL     *BLANKS       VIDCTS
204000970530     C                   MOVEL     *BLANKS       VIDCTE
204100970530     C                   MOVEL     *BLANKS       VIDISO
204200970530     C                   MOVEL     *BLANKS       VIDPII
204300970530     C                   MOVEL     *BLANKS       VIDPID
204400970530     C                   MOVEL     *BLANKS       VIDPIS
204500970530     C                   MOVEL     *BLANKS       VIDGDD
204600970530     C                   MOVEL     *ZEROS        VIDTTC
204700970526     C* DECODIFICHE
204800970527     C                   MOVEL     *BLANKS       VDDNAR
204900970527     C                   MOVEL     *BLANKS       VDDLNA
205000970527     C                   MOVEL     *BLANKS       VDDLOL
205100971118     C                   MOVEL     *BLANKS       VDDLO2
205200970604     C                   MOVEL     *BLANKS       VDDLOS
205300970530     C                   MOVEL     *BLANKS       VDDLFD
205400970530     C                   MOVEL     *BLANKS       VDDCTS
205500970530     C                   MOVEL     *BLANKS       VDDCTE
205600130916     C                   MOVEL     *BLANKS       VDDISO
205700130916     c* campi  di comodo
205800130916     C                   clear                   F_LNA
205900130916     C                   clear                   F_LOL
206000130916     C                   clear                   F_LOS
206100970526     C*
206200970526     C                   ENDSR
206300970530     C*------------------------------------------------------------------------*
206400000426     C* IMPCPC - IMPORTA I DATI DEL CAPPARIO
206500970530     C*------------------------------------------------------------------------*
206600970530     C     IMPCPC        BEGSR
206700970530     C*
206800970530     C                   MOVEL     D00VER        KCCVER
206900970530     C                   MOVEL     VIDNAR        KCCNAR
207000970530     C                   MOVEL     VIDCAP        KCCCAP
207100970530     C     KEYCPC        CHAIN     AZCPC01L                           99
207200970530IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
207300970530     C                   MOVEL     CPCLNA        VIDLNA
207400970530     C                   MOVEL     CPCLOL        VIDLOL
207500971118     C                   MOVEL     CPCLO2        VIDLO2
207600970604     C                   MOVEL     CPCLOS        VIDLOS
207700970530     C                   MOVEL     CPCZNC        VIDZNC
207800970530     C                   MOVEL     CPCZOL        VIDZOL
207900971118     C                   MOVEL     CPCZO2        VIDZO2
208000970604     C                   MOVEL     CPCZOS        VIDZOS
208100970530     C                   MOVEL     CPCLNA        VIDLFD
208200970530     C                   MOVEL     CPCZFD        VIDZFD
208300970530     C                   Z-ADD     CPCLKG        VIDLKG
208400970530     C                   Z-ADD     CPCLMC        VIDLMC
208500970604     C                   Z-ADD     CPCLKS        VIDLKS
208600970604     C                   Z-ADD     CPCLMS        VIDLMS
208700971118     C                   Z-ADD     CPCLK2        VIDLK2
208800971118     C                   Z-ADD     CPCLM2        VIDLM2
208900970530     C                   MOVEL     CPCCTS        VIDCTS
209000970530     C                   MOVEL     CPCCTE        VIDCTE
209100970530     C                   MOVEL     CPCISO        VIDISO
209200970530     C                   MOVEL     CPCPII        VIDPII
209300970530     C                   MOVEL     CPCPID        VIDPID
209400970530     C                   MOVEL     CPCPIS        VIDPIS
209500970530     C                   MOVEL     CPCGDD        VIDGDD
209600970530     C                   MOVEL     CPCTTC        VIDTTC
209700010705     C                   MOVEL     *all'9'       VIDTTR
209800010705     C                   MOVE      CPCFI2        VIDTTR
209900970530X   1C                   ELSE                                                   *NON TROVATO
210000970530     C                   MOVEL     *ZEROS        VIDLNA
210100970530     C                   MOVEL     *ZEROS        VIDLOL
210200971118     C                   MOVEL     *ZEROS        VIDLO2
210300970604     C                   MOVEL     *ZEROS        VIDLOS
210400970530     C                   MOVEL     *ZEROS        VIDZNC
210500970530     C                   MOVEL     *ZEROS        VIDZOL
210600971118     C                   MOVEL     *ZEROS        VIDZO2
210700970604     C                   MOVEL     *ZEROS        VIDZOS
210800970530     C                   MOVEL     *ZEROS        VIDLFD
210900970530     C                   MOVEL     *ZEROS        VIDZFD
211000970530     C                   Z-ADD     *ZEROS        VIDLKG
211100970530     C                   Z-ADD     *ZEROS        VIDLMC
211200970604     C                   Z-ADD     *ZEROS        VIDLKS
211300970604     C                   Z-ADD     *ZEROS        VIDLMS
211400971118     C                   Z-ADD     *ZEROS        VIDLK2
211500971118     C                   Z-ADD     *ZEROS        VIDLM2
211600970530     C                   MOVEL     *BLANKS       VIDCTS
211700970530     C                   MOVEL     *BLANKS       VIDCTE
211800970530     C                   MOVEL     *BLANKS       VIDISO
211900970530     C                   MOVEL     *BLANKS       VIDPII
212000970530     C                   MOVEL     *BLANKS       VIDPID
212100970530     C                   MOVEL     *BLANKS       VIDPIS
212200970530     C                   MOVEL     *BLANKS       VIDGDD
212300970530     C                   MOVEL     *ZEROS        VIDTTC
212400010705     C                   MOVEL     *BLANKS       VIDTTR
212500970530E   1C                   ENDIF
212600000426     C*
212700970530     C                   ENDSR
212800970526     C*------------------------------------------------------------------------*
212900970530     C* IMPCPL - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
213000970526     C*------------------------------------------------------------------------*
213100970530     C     IMPCPL        BEGSR
213200970526     C*
213300970530     C                   MOVEL     VIDNAR        CPLNAR
213400970530     C                   MOVEL     VIDCAP        CPLCAP
213500970530     C                   MOVEL     VIDLOC        CPLLOC
213600970530     C                   MOVEL     VIDLIV        CPLLIV
213700970530     C                   MOVEL     VIDDLV        CPLDLV
213800970917     C                   MOVEL     VIDFI1        CPLFI1
213900010322     C                   MOVE(p)   VIDTTR        CPLFI2
214000970530     C*
214100971118     C* LOCALITA' GESTITA A LIVELLO DI LOCALITA' -> SCRIVE I DATI VIDEO SUL FILE
214200970530IF  1C     VIDLIV        IFEQ      'L'
214300970530     C                   MOVEL     VIDLNA        CPLLNA
214400970530     C                   MOVEL     VIDLOL        CPLLOL
214500971118     C                   MOVEL     VIDLO2        CPLLO2
214600970604     C                   MOVEL     VIDLOS        CPLLOS
214700970530     C                   MOVEL     VIDZNC        CPLZNC
214800970530     C                   MOVEL     VIDZOL        CPLZOL
214900971118     C                   MOVEL     VIDZO2        CPLZO2
215000970604     C                   MOVEL     VIDZOS        CPLZOS
215100970530     C                   MOVEL     VIDZFD        CPLZFD
215200970530     C                   Z-ADD     VIDLKG        CPLLKG
215300970530     C                   Z-ADD     VIDLMC        CPLLMC
215400970604     C                   Z-ADD     VIDLKS        CPLLKS
215500970604     C                   Z-ADD     VIDLMS        CPLLMS
215600971118     C                   Z-ADD     VIDLK2        CPLLK2
215700971118     C                   Z-ADD     VIDLM2        CPLLM2
215800970530     C                   MOVEL     VIDCTS        CPLCTS
215900970530     C                   MOVEL     VIDCTE        CPLCTE
216000970530     C                   MOVEL     VIDISO        CPLISO
216100970603     C                   MOVEL     VIDPII        CPLPII
216200970603IF  3C     CPLISO        IFEQ      'I'
216300970603     C                   MOVEL     'S'           CPLPII
216400970603E   3C                   ENDIF
216500970603     C                   MOVEL     VIDPID        CPLPID
216600970603IF  3C     CPLISO        IFEQ      'D'
216700970603     C                   MOVEL     'S'           CPLPID
216800970603E   3C                   ENDIF
216900970603     C                   MOVEL     VIDPIS        CPLPIS
217000970603IF  3C     CPLISO        IFEQ      'S'
217100970603     C                   MOVEL     'S'           CPLPIS
217200970603E   3C                   ENDIF
217300970530     C                   MOVEL     VIDGDD        CPLGDD
217400970530     C                   MOVEL     VIDTTC        CPLTTC
217500970530     C*
217600970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> AZZERA I CAMPI RIFERITI AL CAP
217700970530X   1C                   ELSE
217800970530     C                   Z-ADD     *ZEROS        CPLLNA
217900970530     C                   Z-ADD     *ZEROS        CPLLOL
218000971118     C                   Z-ADD     *ZEROS        CPLLO2
218100970604     C                   Z-ADD     *ZEROS        CPLLOS
218200970530     C                   Z-ADD     *ZEROS        CPLZNC
218300970530     C                   Z-ADD     *ZEROS        CPLZOL
218400971118     C                   Z-ADD     *ZEROS        CPLZO2
218500970604     C                   Z-ADD     *ZEROS        CPLZOS
218600970530     C                   Z-ADD     *ZEROS        CPLZFD
218700970530     C                   Z-ADD     *ZEROS        CPLLKG
218800970530     C                   Z-ADD     *ZEROS        CPLLMC
218900970604     C                   Z-ADD     *ZEROS        CPLLKS
219000970604     C                   Z-ADD     *ZEROS        CPLLMS
219100971118     C                   Z-ADD     *ZEROS        CPLLK2
219200971118     C                   Z-ADD     *ZEROS        CPLLM2
219300970530     C                   MOVEL     *BLANKS       CPLCTS
219400970530     C                   MOVEL     *BLANKS       CPLCTE
219500970530     C                   MOVEL     *BLANKS       CPLISO
219600970530     C                   MOVEL     *BLANKS       CPLPII
219700970530     C                   MOVEL     *BLANKS       CPLPID
219800970530     C                   MOVEL     *BLANKS       CPLPIS
219900970530     C                   MOVEL     *BLANKS       CPLGDD
220000970530     C                   Z-ADD     *ZEROS        CPLTTC
220100010705     C                   MOVEL     *BLANKS       CPLFI2
220200970530E   1C                   ENDIF
220300970526     C*
220400970526     C                   ENDSR
220500970530     C*------------------------------------------------------------------------*
220600970530     C* LANPGM - LANCIA UN PROGRAMMA
220700970530     C*------------------------------------------------------------------------*
220800970530     C     LANPGM        BEGSR
220900970530     C*
221000970530     C* IMPOSTA LA DS DI PROCEDURA
221100970530     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
221200970530     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
221300970530     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
221400970530     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
221500970530     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
221600970530     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
221700970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
221800970602     C                   MOVEL     DCAP          D00CAP                         *CAP
221900970602     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
222000970603     C                   MOVEL     *BLANKS       D00SIN                         *ALTRI CAMPI
222100970603     C                   Z-ADD     *ZEROS        D00TTC
222200970530     C                   MOVEL     *BLANKS       D00TSP
222300970530     C                   Z-ADD     *ZEROS        D00TFP
222400970530     C                   MOVEL     *BLANKS       D00EPA
222500970530     C                   Z-ADD     *ZEROS        D00ETP
222600970530     C                   Z-ADD     *ZEROS        D00ETA
222700970530     C                   Z-ADD     *ZEROS        D00EDD
222800970530     C                   MOVEL     *BLANKS       D00SPT
222900970530     C                   Z-ADD     *ZEROS        D00SLP
223000970530     C                   Z-ADD     *ZEROS        D00SLA
223100970530     C                   MOVEL     *BLANKS       D00STS
223200970530     C                   Z-ADD     *ZEROS        D00SDD
223300970530     C*
223400970530     C                   CALL      WPGM
223500970612     C                   PARM                    KPJBA
223600970612     C                   PARM                    DSSI00
223700970530     C*
223800970530     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
223900970530IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
224000970530     C                   MOVEL     '1'           ERRL00                         *ERRORE
224100970530     C                   MOVEL     '1'           F03L00                         *FINE PGM
224200970530X   1C                   ELSE                                                   *NON PREMUTO F03
224300970530     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
224400970530E   1C                   ENDIF
224500970530     C*
224600970530     C                   ENDSR
224700970526     C*------------------------------------------------------------------------*
224800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
224900970526     C*------------------------------------------------------------------------*
225000970526     C     IMPFUN        BEGSR
225100970603     C*
225200970603     C* SE INTERROGAZIONE/VISUALIZZAZIONE TOGLE LA GESTIONE ALLE ZONE DI CONSEGNA
225300970603IF  1C     DEPOP0        IFEQ      'C05'
225400131007IF  1C     DEPOP0        oreq      'L05'
225500970603     C     DEPOP1        OREQ      'O05'
225600970603     C                   MOVEL     FUD(14)       FUDRIG                         *AGGIORNAMENTO
225700970603     C                   MOVEL     'N'           FUDATT
225800970603     C                   MOVEL     FUDRIG        FUD(14)
225900970603E   1C                   ENDIF
226000970526     C*
226100970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
226200970526IF  1C     DEPOP1        IFEQ      'O05'
226300970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
226400970526     C                   MOVEL     'N'           FUDATT
226500970526     C                   MOVEL     FUDRIG        FUD(6)
226600970526E   1C                   ENDIF
226700131007     c                   if        *in10 and  %subst(depop0:2:2) = '05'
226800131007     C                   MOVEL     FUD(7)        FUDRIG                         *LOCALITA'
226900131007     C                   MOVEL     'N'           FUDATT
227000131007     C                   MOVEL     FUDRIG        FUD(7)
227100131007     C                   MOVEL     FUD(17)       FUDRIG                         *LOCALITA'
227200131007     C                   MOVEL     'N'           FUDATT
227300131007     C                   MOVEL     FUDRIG        FUD(17)
227400131007     c                   endif
227500970526     C*
227600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
227700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
227800970526     C                   EXSR      FUNGES
227900000925     C*
228000000925     C* VERIFICO CHE SOLO UTENTI "EDP*" POSSANO MODIFICARE IL CAMPO "UFFICIO POSTALE"
228100000925     C                   MOVEL     KNMUS         UTE               3
228200000925     C     UTE           IFNE      'EDP'
228300000925     C                   SETON                                        95
228400000925     C                   ENDIF
228500970526     C*
228600970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
228700970526     C*---
228800970526     C* 90 -> PROTEZIONE CHIAVE
228900970526     C* 91 -> PROTEZIONE TOTALE
229000050414     C* 94 -> PROTEZIONE COD.TASSAZIONE e TIPO INOLTRO e altri
229100050413     C*---
229200050413     C*
229300050413     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
229400050413     C                   SETON                                        94
229500050413     C*
229600050413     C* VERIFICO X PRIMISSIMA COSA LE AUTORIZZAZIONI SPECIALI SULL'UTENTE CORRENTE
229700050413     C                   IF        §UTECAP = *blanks
229800050413     C* TUTTO RIMANE INVARIATO
229900050413     C                   ENDIF
230000050413     C*
230100050413     C                   IF        §UTECAP = 'I'
230200050413     C* FORZO SOLO INTERROGAZIONE
230300050413     C                   EVAL      DEPOP1 = 'O05'
230400050413     C                   ENDIF
230500050413     C*
230600050413     C                   IF        §UTECAP = 'C'
230700050413     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
230800050413     C                   SETOFF                                       94
230900050413     C                   ENDIF
231000050413     C*
231100970530IF  1C     DEPOP0        IFEQ      'L02'                                        G E S T I O N E
231200970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
231300970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
231400970526     C                   SETOFF                                       91
231500970526E   2C                   ENDIF
231600970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
231700970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
231800970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
231900970526E   2C                   ENDIF
232000970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
232100970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
232200970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
232300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
232400970526     C                   SETON                                        91
232500970526E   2C                   ENDIF
232600970526X   1C                   ELSE                                                   *I N T E R R O G A
232700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
232800970526     C                   SETON                                        91
232900970526E   1C                   ENDIF
233000970526     C*
233100970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
233200970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
233300970530     C                   Z-ADD     D00VER        KCLVER
233400970530     C                   MOVEL     D00NAR        KCLNAR
233500970530     C                   MOVEL     D00CAP        KCLCAP
233600970530     C                   MOVEL     D00LOC        KCLLOC
233700970530     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
233800970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
233900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
234000970526     C                   MOVEL     MSG(6)        DSMSMS
234100970526X   2C                   ELSE
234200970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
234300980128IF  3C     DEPOP1        IFEQ      'O05'
234400980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
234500980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
234600980128E   3C                   ENDIF
234700970526E   2C                   ENDIF
234800970526X   1C                   ELSE                                                   *INSERIMENTO
234900970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
235000970526E   1C                   ENDIF
235100970526     C*
235200970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
235300970526IF  1C     DEPOP1        IFEQ      'O02'
235400970526     C                   MOVEL     DEC(1)        VIDFUN
235500970526E   1C                   ENDIF
235600970526IF  1C     DEPOP1        IFEQ      'O04'
235700970526     C                   MOVEL     DEC(2)        VIDFUN
235800970526E   1C                   ENDIF
235900970526IF  1C     DEPOP1        IFEQ      'O05'
236000970526     C                   MOVEL     DEC(3)        VIDFUN
236100970526E   1C                   ENDIF
236200970526IF  1C     DEPOP1        IFEQ      'O07'
236300970526     C                   MOVEL     DEC(4)        VIDFUN
236400970526E   1C                   ENDIF
236500970526IF  1C     DEPOP1        IFEQ      'F06'
236600970526     C                   MOVEL     DEC(5)        VIDFUN
236700970526E   1C                   ENDIF
236800970526     C*
236900970526     C                   ENDSR
237000970626     C*------------------------------------------------------------------------*
237100970626     C* FINSR - OPERAZIONI FINALI
237200970626     C*------------------------------------------------------------------------*
237300970626     C     FINSR         BEGSR
237400970626     C*
237500970626     C* CHIUDE I FILE DEL "TISI95R"
237600970704     C                   CLEAR                   DSSI95
237700970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
237800970626     C                   CALL      'TISI95R'
237900970626     C                   PARM                    DSSI95
238000130917     c*
238100130917     c                   clear                   tisio7ds
238200130917     C                   MOVEL     'C'           Io7TLA
238300131016     c*****              movel     tisio7ds      kpjbu
238400130917     C                   CALL      'TISIO7R'
238500130917     C                   PARM                    kpjba
238600131016     C                   PARM                    tisio7ds
238700970626     C*
238800970626     C                   ENDSR
238900050413     C*--------------------------------------------------------------------------------------------*
239000050413     C* REPERISCE DATI UTENTE
239100050413     C*--------------------------------------------------------------------------------------------*
239200050413     C     REPDATIUTE    BEGSR
239300050413     C*
239400050413     C* REimposta le variabili di lavoro
239500050413     C                   CLEAR                   TIBS34DS
239600050413     C                   CLEAR                   AZUTEDS
239700050413     C                   CLEAR                   DDATIUTE
239800050413     C*
239900050413     C* Reperisce i dati del profilo in ingresso
240000050413     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
240100050413     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
240200050413     C                   IN(E)     *DTAARA
240300050413     C                   IF        %Error                                       *NO errori
240400050413     C                   EVAL      I34TLA = 'L'
240500050413     C                   CALL      'TIBS34R'
240600050413     C                   PARM                    TIBS34DS
240700050413     C                   IN        *DTAARA
240800050413     C                   ENDIF
240900050413     C*
241000050413     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
241100050413     C                   EVAL      DUTE01 = UTEFAF
241200050413     C*
241300050413     C                   ENDSR
241400130916     C*------------------------------------------------------------------------*
241500130916     C* Richiamo pgm annullamento orari servizi a fronte di modifica lna
241600130916     C*------------------------------------------------------------------------*
241700130916     c     sr_oraris     begsr
241800130916     C* LINEA STANDARD
241900130916     c                   movel     vidlna        w0030             3 0
242000130916     c* Se cambiata la linea
242100130916     c                   if        w0030<>f_lna
242200130916     c                   clear                   tisio7ds
242300130916     c                   eval      io7mod='D'
242400130917     c* Il livello da passare è quello a video
242500130917     c                   eval      io7liv=vidliv
242600130916     c                   eval      io7ver=d00ver
242700130916     c                   eval      io7nar=vidnar
242800130916     c                   eval      io7cap=vidcap
242900130917     c                   eval      io7loc=vidloc
243000130916     c                   eval      io7lna=f_lna
243100130916     c                   eval      io7lns=%dec(vidlna:3:0)
243200130916     c                   eval      io7los=%dec(vidlos:3:0)
243300130916     c                   eval      io7lol=%dec(vidlol:3:0)
243400131016     c******             movel     tisio7ds      kpjbu
243500130916     c                   call      'TISIO7R'
243600130916     c                   parm                    kpjba
243700131016     c                   parm                    tisio7ds
243800130916     c                   endif
243900130916     C* LINEA sotto
244000130916     c                   movel     vidlos        w0030             3 0
244100130916     c* Se cambiata la linea e non ancora richiamato pgm per la linea stessa
244200130916     c                   if        w0030<>f_los  and f_los<> f_lna
244300130916     c                   clear                   tisio7ds
244400130916     c                   eval      io7mod='D'
244500130917     c* Il livello da passare è quello a video
244600130917     c                   eval      io7liv=vidliv
244700130916     c                   eval      io7ver=d00ver
244800130916     c                   eval      io7nar=vidnar
244900130916     c                   eval      io7cap=vidcap
245000130917     c                   eval      io7loc=vidloc
245100130916     c                   eval      io7lna=f_los
245200130916     c                   eval      io7lns=%dec(vidlna:3:0)
245300130916     c                   eval      io7los=%dec(vidlos:3:0)
245400130916     c                   eval      io7lol=%dec(vidlol:3:0)
245500131016     c******             movel     tisio7ds      kpjbu
245600130916     c                   call      'TISIO7R'
245700130916     c                   parm                    kpjba
245800131016     c                   parm                    tisio7ds
245900130916     c                   endif
246000130916     C* LINEA OLTRE
246100130916     c                   movel     vidlol        w0030             3 0
246200130916     c* Se cambiata la linea e non ancora richiamato pgm per la linea stessa
246300130916     c                   if        w0030<>f_lol  and f_lol<> f_lna
246400130916     c                                           and f_lol<> f_los
246500130916     c                   clear                   tisio7ds
246600130916     c                   eval      io7mod='D'
246700130917     c* Il livello da passare è quello a video
246800130917     c                   eval      io7liv=vidliv
246900130916     c                   eval      io7ver=d00ver
247000130916     c                   eval      io7nar=vidnar
247100130916     c                   eval      io7cap=vidcap
247200130917     c                   eval      io7loc=vidloc
247300130916     c                   eval      io7lna=f_lol
247400130916     c                   eval      io7lns=%dec(vidlna:3:0)
247500130916     c                   eval      io7los=%dec(vidlos:3:0)
247600130916     c                   eval      io7lol=%dec(vidlol:3:0)
247700131016     c*****              movel     tisio7ds      kpjbu
247800130916     c                   call      'TISIO7R'
247900130916     c                   parm                    kpjba
248000131016     c                   parm                    tisio7ds
248100130916     c                   endif
248200130917     c*
248300130917     c                   eval      wdel='S'
248400130917     c                   exsr      sr_cpe
248500130917     c                   endsr
248600130917     C*------------------------------------------------------------------------*
248700130917     C* Richiamo pgm orari servizi partendo dalle eccezioni
248800130917     C*------------------------------------------------------------------------*
248900130917     c     sr_cpe        begsr
249000130917     c* Se passaggio di livello da cap a località  leggo le eccezioni
249100130917     c* e se presenti richiamo tisio7r passando le linee delle eccezioni
249200130917     c                   if        vidliv='L' and cplliv<>vidliv
249300130917     c     kcpe          setll     azcpe01l
249400130917     c     kcpe          reade     azcpe01l
249500130917     c                   dow       not %eof(azcpe01l)
249600130917     c* lna standard
249700130917     c                   move      cpelna        w003A             3
249800130917     c                   if        w003a <>vidlna and w003a <>vidlos
249900130917     c                             and w003a <>vidlol
250000130917     c                             and (f07lna=*blanks or wdel='S')
250100130917     c                   clear                   tisio7ds
250200130917     c                   if        wdel='S'
250300130917     c                   eval      io7mod='D'
250400130917     c                   else
250500130917     c                   eval      io7mod='C'
250600130917     c                   endif
250700130917     c* Il livello da passare è quello a video
250800130917     c                   eval      io7liv=vidliv
250900130917     c                   eval      io7ver=d00ver
251000130917     c                   eval      io7nar=vidnar
251100130917     c                   eval      io7cap=vidcap
251200130917     c                   eval      io7loc=vidloc
251300130917     c                   eval      io7lna=cpelna
251400130917     c                   eval      io7lns=%dec(vidlna:3:0)
251500130917     c                   eval      io7los=%dec(vidlos:3:0)
251600130917     c                   eval      io7lol=%dec(vidlol:3:0)
251700131016     c******             movel     tisio7ds      kpjbu
251800130917     c                   call      'TISIO7R'
251900130917     c                   parm                    kpjba
252000131016     c                   parm                    tisio7ds
252100130917     c                   if        oo7err='1'
252200130917     c                   movel     oo7msg        DSmsms
252300130917     c                   eval      ErrL00 = *on
252400130917     c                   eval      f07lna='S'
252500130917e   4c                   endif
252600130917     c                   endif
252700130917     c* lna sotto
252800130917     c                   move      cpelos        w003A             3
252900130917     c                   if        w003a<>vidlna and w003a <>vidlos
253000130917     c                             and w003a<>vidlol
253100130917     c                             and (f07lna=*blanks or wdel='S')
253200130917     c                   clear                   tisio7ds
253300130917     c                   if        wdel='S'
253400130917     c                   eval      io7mod='D'
253500130917     c                   else
253600130917     c                   eval      io7mod='C'
253700130917     c                   endif
253800130917     c* Il livello da passare è quello a video
253900130917     c                   eval      io7liv=vidliv
254000130917     c                   eval      io7ver=d00ver
254100130917     c                   eval      io7nar=vidnar
254200130917     c                   eval      io7cap=vidcap
254300130917     c                   eval      io7loc=vidloc
254400130917     c                   eval      io7lna=cpelos
254500130917     c                   eval      io7lns=%dec(vidlna:3:0)
254600130917     c                   eval      io7los=%dec(vidlos:3:0)
254700130917     c                   eval      io7lol=%dec(vidlol:3:0)
254800131016     c******             movel     tisio7ds      kpjbu
254900130917     c                   call      'TISIO7R'
255000130917     c                   parm                    kpjba
255100131016     c                   parm                    tisio7ds
255200130917     c                   if        oo7err='1'
255300130917     c                   movel     oo7msg        DSmsms
255400130917     c                   eval      ErrL00 = *on
255500130917     c                   eval      f07lna='S'
255600130917e   4c                   endif
255700130917e   4c                   endif
255800130917     c* lna oltre
255900130917     c                   move      cpelol        w003A             3
256000130917     c                   if        w003a <>vidlna and w003a <>vidlos
256100130917     c                             and w003a <>vidlol
256200130917     c                             and (f07lna=*blanks or wdel='S')
256300130917     c                   clear                   tisio7ds
256400130917     c                   if        wdel='S'
256500130917     c                   eval      io7mod='D'
256600130917     c                   else
256700130917     c                   eval      io7mod='C'
256800130917     c                   endif
256900130917     c* Il livello da passare è quello a video
257000130917     c                   eval      io7liv=vidliv
257100130917     c                   eval      io7ver=d00ver
257200130917     c                   eval      io7nar=vidnar
257300130917     c                   eval      io7cap=vidcap
257400130917     c                   eval      io7loc=vidloc
257500130917     c                   eval      io7lna=cpelol
257600130917     c                   eval      io7lns=%dec(vidlna:3:0)
257700130917     c                   eval      io7los=%dec(vidlos:3:0)
257800130917     c                   eval      io7lol=%dec(vidlol:3:0)
257900131016     c******             movel     tisio7ds      kpjbu
258000130917     c                   call      'TISIO7R'
258100130917     c                   parm                    kpjba
258200131016     c                   parm                    tisio7ds
258300130917     c                   if        oo7err='1'
258400130917     c                   movel     oo7msg        DSmsms
258500130917     c                   eval      ErrL00 = *on
258600130917     c                   eval      f07lna='S'
258700130917e   4c                   endif
258800130917e   4c                   endif
258900130917     c     kcpe          reade     azcpe01l
259000130917     c                   enddo
259100130917     c                   endif
259200130917     c                   endsr
259300970526     C*------------------------------------------------------------------------*
259400970526     C* *INZSR - OPERAZIONI INIZIALI
259500970526     C*------------------------------------------------------------------------*
259600970526     C     *INZSR        BEGSR
259700970526     C*---
259800970526     C* RICEVIMENTO PARAMETRI
259900970526     C*---
260000970526     C     *ENTRY        PLIST
260100970612     C                   PARM                    KPJBA
260200970612     C                   PARM                    DSSI00
260300050413     C*
260400050413     C* REPERISCE I DATI DELL'UTENTE
260500050413     C                   EXSR      REPDATIUTE
260600970526     C*
260700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
260800970526     C     XMSGPA        PLIST
260900970526     C                   PARM                    DSMSMS
261000970526     C                   PARM                    DSMSPG
261100970526     C                   PARM                    DSMSKE
261200970526     C                   PARM                    DSMSER
261300970526     C*
261400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
261500970526     C                   CALL      'XKEY'
261600970526     C                   PARM                    DSKEY
261700131007      /free
261800131007        // Carico filiali di test abilitate
261900131007         if dutpou<>46  ;
262000131007
262100131007         clear TIBS02ds;
262200131007         T02Mod = 'C';
262300131007         T02sif = knsif;
262400131007         T02cod = 'VPO';
262500131007         t02tla = 'L'   ;
262600131007         T02ke1 ='DECOFI816' ;
262700131007         TNTBE_RicercaControllo  (kpjba : tibs02ds);
262800131007
262900131007         if  T02err  = *blank;
263000131007         xx= 1 ;
263100131007         yy= 1 ;
263200131007
263300131007         dow xx< 256   ;
263400131007           fil_ab(yy)= %subst(t02uni:xx:3)  ;
263500131007           yy=yy+ 1   ;
263600131007           xx=xx+ 3   ;
263700131007         enddo  ;
263800131007
263900131007         endif;
264000131007
264100131007         // se c'e' 999 tutte abilitate
264200131007         w003a='999'  ;
264300131007         if   %lookup(w003a:fil_ab) = 0  ;
264400131007         w003a=%editc(dutpou:'X')  ;
264500131007         if   %lookup(w003a:fil_ab) = 0  ;
264600131007         *in10 =*on  ;
264700131007         endif  ;
264800131007         endif  ;
264900131007
265000131007         endif  ;
265100131007
265200131007      /end-free
265300131007
265400090903     c*
265500090903     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
265600090903     c                   clear                   tibs02ds
265700090903     c                   clear                   dvpodeco
265800090903     c                   eval      t02mod = 'C'
265900090903     c                   eval      t02sif = knsif
266000090903     c                   eval      t02cod = 'VPO'
266100090903     c                   eval      t02ke1 = 'DECO'
266200090903     c                   call      'TIBS02R'
266300090903     c                   parm                    kpjba
266400090903     c                   parm                    tibs02ds
266500090903     c                   if        t02err = *blanks
266600090903     c                   eval      dvpodeco = t02uni
266700090903     c                   else
266800090903     c                   eval      §vpotsph=20091001
266900090903     c                   endif
267000090903
267100090903     c     *iso          movel     §vpotsph      dataiso
267200090903     c
267300970526     C*---
267400970526     C* VARIABILI NON RIFERITE AL DATA BASE
267500970526     C*---
267600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
267700970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
267800970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
267900970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
268000970526     C     *LIKE         DEFINE    D00OP1        WOP1
268100970602     C     *LIKE         DEFINE    D00NAR        DNAR
268200970602     C     *LIKE         DEFINE    D00CAP        DCAP
268300970602     C     *LIKE         DEFINE    D00LOC        DLOC
268400970526     C*---
268500970526     C* VARIABILI DI CONTROLLO
268600970526     C*---
268700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
268800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
268900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
269000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
269100970526     C*
269200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
269300970529     C                   MOVEL     'TISI31R'     DSMSPG
269400970529     C                   MOVEL     'TISI31R'     MSGQUE
269500970526     C                   MOVEL     *BLANKS       MSGKEY
269600970526     C                   MOVEL     *BLANKS       DSMSMS
269700970526     C*---
269800970526     C* VARIABILI RIFERITE AL DATA BASE
269900970526     C*---
270000970530     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
270100970530     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
270200970530     C     *LIKE         DEFINE    TBLCOD        KTBCOD
270300970530     C     *LIKE         DEFINE    TBLKEY        KTBKEY
270400970530     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
270500970530     C     *LIKE         DEFINE    CPLNAR        KCLNAR
270600970530     C     *LIKE         DEFINE    CPLCAP        KCLCAP
270700970530     C     *LIKE         DEFINE    CPLLOC        KCLLOC
270800970530     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
270900970530     C     *LIKE         DEFINE    CPCNAR        KCCNAR
271000970530     C     *LIKE         DEFINE    CPCCAP        KCCCAP
271100970530     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
271200970530     C     *LIKE         DEFINE    CPTTTC        KCTTTC
271300970530     C     *LIKE         DEFINE    CPTTSP        KCTTSP
271400970925     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
271500970925     C     *LIKE         DEFINE    CPSNAR        KCSNAR
271600970925     C     *LIKE         DEFINE    CPSCAP        KCSCAP
271700970925     C     *LIKE         DEFINE    CPSLOC        KCSLOC
271800970613     C     *LIKE         DEFINE    CPCLNA        NLNA                           *DEPOSITI NUMERICI
271900970613     C     *LIKE         DEFINE    CPCLOS        NLOS
272000970613     C     *LIKE         DEFINE    CPCLOL        NLOL
272100971118     C     *LIKE         DEFINE    CPCLO2        NLO2
272200970613     C     *LIKE         DEFINE    CPCTTC        NTTC
272300010911     C     *LIKE         DEFINE    CPCFI2        NTTR
272400970526     C*---
272500970526     C* CHIAVI DI LETTURA
272600970526     C*---
272700970530     C* LETTURA AZORG01L - COMPLETA
272800970530     C     KEYORG        KLIST
272900970530     C                   KFLD                    KORFIL                         *FILIALE
273000970530     C* LETTURA TABEL00F - COMPLETA
273100970530     C     KEYTAB        KLIST
273200970530     C                   KFLD                    KTBKUT                         *CODICE UTENTE
273300970530     C                   KFLD                    KTBCOD                         *TABELLA
273400970530     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
273500970530     C* LETTURA TABEL00F - PARZIALE
273600970530     C     KE2TAB        KLIST
273700970530     C                   KFLD                    KTBKUT                         *CODICE UTENTE
273800970530     C                   KFLD                    KTBCOD                         *TABELLA
273900970530     C* LETTURA AZCPL01L - COMPLETA
274000970530     C     KEYCPL        KLIST
274100970530     C                   KFLD                    KCLVER                         *VERSIONE
274200970530     C                   KFLD                    KCLNAR                         *NAZIONE
274300970530     C                   KFLD                    KCLCAP                         *CAP
274400970530     C                   KFLD                    KCLLOC                         *LOCALITA'
274500970613     C* LETTURA AZCPL01L - PARZIALE
274600970613     C     KE2CPL        KLIST
274700970613     C                   KFLD                    KCLVER                         *VERSIONE
274800970613     C                   KFLD                    KCLNAR                         *NAZIONE
274900970613     C                   KFLD                    KCLCAP                         *CAP
275000970527     C* LETTURA AZCPC01L - COMPLETA
275100970527     C     KEYCPC        KLIST
275200970527     C                   KFLD                    KCCVER                         *VERSIONE
275300970527     C                   KFLD                    KCCNAR                         *NAZIONE
275400970527     C                   KFLD                    KCCCAP                         *CAP
275500970530     C* LETTURA AZCPT01L - COMPLETA
275600970530     C     KEYCPT        KLIST
275700970530     C                   KFLD                    KCTVER                         *VERSIONE
275800970530     C                   KFLD                    KCTTTC                         *TEMPO CONSEGNA
275900970530     C                   KFLD                    KCTTSP                         *TIPO SERVIZIO
276000970925     C* LETTURA AZCPS01L - PARZIALE
276100970925     C     KEYCPS        KLIST
276200970925     C                   KFLD                    KCSVER                         *VERSIONE
276300970925     C                   KFLD                    KCSNAR                         *NAZIONE
276400970925     C                   KFLD                    KCSCAP                         *CAP
276500970925     C                   KFLD                    KCSLOC                         *LOCALITA
276600130917     C* LETTURA AZCpe01L -
276700130917     C     Kcpe          KLIST
276800130917     C                   KFLD                    d00ver                         *VERSIONE
276900130917     C                   KFLD                    vidnar                         *NAZIONE
277000130917     C                   KFLD                    vidcap                         *CAP
277100970526     C*
277200970526     C* DEPOSITA LE OPZIONI DI ENTRATA
277300970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
277400970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
277500970526     C*
277600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
277700970526     C                   EXSR      IMPFUN
277800970526     C*
277900970526     C                   ENDSR
278000970526     C*------------------------------------------------------------------------*
278100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
278200970526     C*------------------------------------------------------------------------*
278300970526     C     FUNGES        BEGSR
278400970526     C*
278500970526     C* PULISCO CAMPI DI LAVORO
278600970526     C                   Z-ADD     0             W                 5 0
278700970526     C*
278800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
278900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
279000970526     C     1             DO        24            I
279100970526     C                   MOVEL     FUD(I)        FUDRIG
279200970526     C*
279300970526     C* PULIZIA FUNZIONI PRECEDENTI
279400970526     C                   MOVEL     *BLANKS       FUV(I)
279500970526     C*
279600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
279700970526     C* (SALTA F24)
279800970526     C     FUDATT        IFEQ      'S'
279900970526     C     FUDFUN        ANDNE     '24'
280000970526     C                   ADD       1             W
280100970526     C                   MOVEL     FUDDES        FUV(W)
280200970526     C                   END
280300970526     C                   END                                                    DO WHILE
280400970526     C*
280500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
280600970526     C* ALTRIMENTI DISATTIVO F24
280700970526     C     1             DO        24            I
280800970526     C                   MOVEL     FUD(I)        FUDRIG
280900970526     C*
281000970526     C     FUDFUN        IFEQ      '24'
281100970526     C     W             IFGT      6
281200970526     C                   MOVEL     'S'           FUDATT
281300970526     C                   ELSE
281400970526     C                   MOVEL     *BLANKS       FUDATT
281500970526     C                   END                                                    W GT 8
281600970526     C                   MOVEL     FUDRIG        FUD(I)
281700970526     C                   END
281800970526     C                   END                                                    DO WHILE
281900970526     C*
282000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
282100970526     C     W             DIV       6             I
282200970526     C                   MVR                     Z
282300970526     C*
282400970526     C     Z             IFNE      0
282500970526     C     I             ADD       1             Z
282600970526     C                   ELSE
282700970526     C                   Z-ADD     I             Z                 5 0
282800970526     C                   END
282900970526     C*
283000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
283100970526     C                   ADD       1             FUNGRU
283200970526     C     FUNGRU        IFGT      Z
283300970526     C     FUNGRU        ORLT      1
283400970526     C                   Z-ADD     1             FUNGRU
283500970526     C                   END
283600970526     C*
283700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
283800970526     C     FUNGRU        IFEQ      1
283900970526     C                   MOVEL     FUV(1)        FUN001
284000970526     C                   MOVEL     FUV(2)        FUN002
284100970526     C                   MOVEL     FUV(3)        FUN003
284200970526     C                   MOVEL     FUV(4)        FUN004
284300970526     C                   MOVEL     FUV(5)        FUN005
284400970526     C                   MOVEL     FUV(6)        FUN006
284500970526     C                   END
284600970526     C*
284700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
284800970526     C     FUNGRU        IFEQ      2
284900970526     C                   MOVEL     FUV(7)        FUN001
285000970526     C                   MOVEL     FUV(8)        FUN002
285100970526     C                   MOVEL     FUV(9)        FUN003
285200970526     C                   MOVEL     FUV(10)       FUN004
285300970526     C                   MOVEL     FUV(11)       FUN005
285400970526     C                   MOVEL     FUV(12)       FUN006
285500970526     C                   END
285600970526     C*
285700970526     C* TERZO GRUPPO DI 6 FUNZIONI
285800970526     C     FUNGRU        IFEQ      3
285900970526     C                   MOVEL     FUV(13)       FUN001
286000970526     C                   MOVEL     FUV(14)       FUN002
286100970526     C                   MOVEL     FUV(15)       FUN003
286200970526     C                   MOVEL     FUV(16)       FUN004
286300970526     C                   MOVEL     FUV(17)       FUN005
286400970526     C                   MOVEL     FUV(18)       FUN006
286500970526     C                   END
286600970526     C*
286700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
286800970526     C     FUNGRU        IFEQ      4
286900970526     C                   MOVEL     FUV(19)       FUN001
287000970526     C                   MOVEL     FUV(20)       FUN002
287100970526     C                   MOVEL     FUV(21)       FUN003
287200970526     C                   MOVEL     FUV(22)       FUN004
287300970526     C                   MOVEL     *BLANKS       FUN005
287400970526     C                   MOVEL     *BLANKS       FUN006
287500970526     C                   END
287600970526     C*
287700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
287800970526     C                   MOVEL     *BLANKS       FUN007
287900970526     C                   MOVEL     *BLANKS       FUN008
288000970526     C*
288100970526     C     1             DO        24            I
288200970526     C                   MOVEL     FUD(I)        FUDRIG
288300970526     C     FUDFUN        IFEQ      '24'
288400970526     C     FUDATT        ANDEQ     'S'
288500970526     C                   MOVEL     FUDDES        FUN008
288600970526     C                   END
288700970526     C                   END                                                    DO
288800970526     C*
288900970526     C                   ENDSR
289000970526     C*------------------------------------------------------------------------*
289100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
289200970526     C*------------------------------------------------------------------------*
289300970526     C     FUNCON        BEGSR
289400970526     C*
289500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
289600970526     C                   EXSR      FUNVAR
289700970526     C*
289800970526     C* PULISCO CAMPI DI LAVORO
289900970526     C                   Z-ADD     0             I                 5 0
290000970526     C                   Z-ADD     0             W                 5 0
290100970526     C*
290200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
290300970526     C     FUNKEY        IFNE      *BLANKS
290400970526     C     1             DO        24            I
290500970526     C                   MOVEL     FUD(I)        FUDRIG
290600970526     C*
290700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
290800970526     C     FUNKEY        IFEQ      FUDFUN
290900970526     C     FUDATT        ANDEQ     'S'
291000970526     C                   Z-ADD     I             W
291100970526     C                   END
291200970526     C                   END                                                    DO WHILE
291300970526     C*
291400970526     C* FUNZIONE NON ATTIVA (ERRORE)
291500970526     C     W             IFEQ      0
291600970526     C*
291700970526     C* EMISSIONE MESSAGGIO DI ERRORE
291800970526     C                   MOVEL     MSG(1)        DSMSMS
291900970526     C*
292000970526     C                   END                                                    W=0
292100970526     C                   END                                                    FUNKEY NE *BLANKS
292200970526     C*
292300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
292400970526     C     W             IFNE      0
292500970526     C     FUNKEY        OREQ      *BLANKS
292600970526     C     FUNKEY        IFEQ      *BLANKS
292700970526     C                   EXSR      ENTGES
292800970526     C                   END
292900970526     C     FUNKEY        IFEQ      '03'
293000970526     C                   EXSR      F03GES
293100970526     C                   END
293200010322     C     FUNKEY        IFEQ      '04'
293300010322     C                   EXSR      F04GES
293400010322     C                   END
293500970526     C     FUNKEY        IFEQ      '06'
293600970526     C                   EXSR      F06GES
293700970526     C                   END
293800130827     C     FUNKEY        IFEQ      '07'
293900130827     C                   EXSR      F07GES
294000130827     C                   END
294100970526     C     FUNKEY        IFEQ      '12'
294200970526     C                   EXSR      F12GES
294300970526     C                   END
294400970603     C     FUNKEY        IFEQ      '14'
294500970603     C                   EXSR      F14GES
294600970603     C                   END
294700970602     C     FUNKEY        IFEQ      '13'
294800970602     C                   EXSR      F13GES
294900970602     C                   END
295000130830     C     FUNKEY        IFEQ      '17'
295100130830     C                   EXSR      F17GES
295200130830     C                   END
295300970526     C     FUNKEY        IFEQ      '24'
295400970526     C                   EXSR      F24GES
295500970526     C                   END
295600970526     C                   END
295700970526     C*
295800970526     C                   ENDSR
295900970526     C*------------------------------------------------------------------------*
296000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
296100970526     C*------------------------------------------------------------------------*
296200970526     C     FUNVAR        BEGSR
296300970526     C*
296400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
296500970526     C*
296600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
296700970526     C     KEY           IFEQ      ENTER
296800970526     C                   MOVEL     *BLANKS       FUNKEY
296900970526     C                   END
297000970526     C     KEY           IFEQ      F1
297100970526     C                   MOVEL     '01'          FUNKEY
297200970526     C                   END
297300970526     C     KEY           IFEQ      F2
297400970526     C                   MOVEL     '02'          FUNKEY
297500970526     C                   END
297600970526     C     KEY           IFEQ      F3
297700970526     C                   MOVEL     '03'          FUNKEY
297800970526     C                   END
297900970526     C     KEY           IFEQ      F4
298000970526     C                   MOVEL     '04'          FUNKEY
298100970526     C                   END
298200970526     C     KEY           IFEQ      F5
298300970526     C                   MOVEL     '05'          FUNKEY
298400970526     C                   END
298500970526     C     KEY           IFEQ      F6
298600970526     C                   MOVEL     '06'          FUNKEY
298700970526     C                   END
298800970526     C     KEY           IFEQ      F7
298900970526     C                   MOVEL     '07'          FUNKEY
299000970526     C                   END
299100970526     C     KEY           IFEQ      F8
299200970526     C                   MOVEL     '08'          FUNKEY
299300970526     C                   END
299400970526     C     KEY           IFEQ      F9
299500970526     C                   MOVEL     '09'          FUNKEY
299600970526     C                   END
299700970526     C     KEY           IFEQ      F10
299800970526     C                   MOVEL     '10'          FUNKEY
299900970526     C                   END
300000970526     C     KEY           IFEQ      F11
300100970526     C                   MOVEL     '11'          FUNKEY
300200970526     C                   END
300300970526     C     KEY           IFEQ      F12
300400970526     C                   MOVEL     '12'          FUNKEY
300500970526     C                   END
300600970526     C     KEY           IFEQ      F13
300700970526     C                   MOVEL     '13'          FUNKEY
300800970526     C                   END
300900970526     C     KEY           IFEQ      F14
301000970526     C                   MOVEL     '14'          FUNKEY
301100970526     C                   END
301200970526     C     KEY           IFEQ      F15
301300970526     C                   MOVEL     '15'          FUNKEY
301400970526     C                   END
301500970526     C     KEY           IFEQ      F16
301600970526     C                   MOVEL     '16'          FUNKEY
301700970526     C                   END
301800970526     C     KEY           IFEQ      F17
301900970526     C                   MOVEL     '17'          FUNKEY
302000970526     C                   END
302100970526     C     KEY           IFEQ      F18
302200970526     C                   MOVEL     '18'          FUNKEY
302300970526     C                   END
302400970526     C     KEY           IFEQ      F19
302500970526     C                   MOVEL     '19'          FUNKEY
302600970526     C                   END
302700970526     C     KEY           IFEQ      F20
302800970526     C                   MOVEL     '20'          FUNKEY
302900970526     C                   END
303000970526     C     KEY           IFEQ      F21
303100970526     C                   MOVEL     '21'          FUNKEY
303200970526     C                   END
303300970526     C     KEY           IFEQ      F22
303400970526     C                   MOVEL     '22'          FUNKEY
303500970526     C                   END
303600970526     C     KEY           IFEQ      F23
303700970526     C                   MOVEL     '23'          FUNKEY
303800970526     C                   END
303900970526     C     KEY           IFEQ      F24
304000970526     C                   MOVEL     '24'          FUNKEY
304100970526     C                   END
304200970526     C     KEY           IFEQ      ROLLUP
304300970526     C                   MOVEL     'RU'          FUNKEY
304400970526     C                   END
304500970526     C     KEY           IFEQ      HELP
304600970526     C                   MOVEL     'HP'          FUNKEY
304700970526     C                   END
304800970526     C                   ENDSR
304900970526     C*------------------------------------------------------------------------*
305000970526     C* NOTE COSTRUZIONE SCHIERE
305100970526     C*------------------------------------------------------------------------*
305200970526     C*
305300970526     C* -MSG-MESSAGGI
305400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
305500970526     C* (NON TOCCARE MSG,1 E MSG,2)
305600970526     C*
305700970526     C* -CMD-COMANDI
305800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
305900970526     C*
306000970526     C* -OPD-OPZIONI
306100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
306200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
306300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
306400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
306500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
306600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
306700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
306800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
306900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
307000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
307100970526     C*
307200970526     C* -FUD-FUNZIONI
307300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
307400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
307500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
307600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
307700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
307800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
307900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
308000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
308100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
308200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
308300970526     O*------------------------------------------------------------------------*
308400970526** DEC - DECODIFICHE
308500970526    MODIFICA                                                      1
308600970526  ANNULLAMENTO                                                    2
308700970526VISUALIZZAZIONE                                                   3
308800970526   RIPRISTINO                                                     4
308900970526  INSERIMENTO                                                     5
309000970526** MSG - MESSAGGI
309100970530TISI31R- Tasto funzionale non gestito                                          1
309200970530TISI31R- Inserimento effettuato                                                2
309300970530TISI31R- Modifica effettuata                                                   3
309400970530TISI31R- Annullamento effettuato                                               4
309500970530TISI31R- Ripristino effettuato                                                 5
309600970530TISI31R- Richiesta funzione per record inesistente                             6
309700970530TISI31R- Record momentaneamente allocato: riprovare più tardi                  7
309800970530TISI31R- Richiesta funzione per record esistente                               8
309900000504TISI31R- Ufficio Postale errato, annullato o inesistente                       9
310000970530TISI31R- Linea di arrivo errata                                               10
310100970530TISI31R- Zona di consegna errata                                              11
310200970917TISI31R- CAP obsoleto: anche la località deve esserlo                         12
310300970530TISI31R- Nazione errata                                                       13
310400970530TISI31R- Codice tassazione errato                                             14
310500970530TISI31R- Codice tassazione errato. Accettati solo I1, I2, I3, ecc ...         15
310600970530TISI31R- Per CAP estero lasciare il codice tassazione vuoto                   16
310700060206TISI31R- Zona di consegna NON abilitata                                       17
310800970604TISI31R- Linea limite obbligatoria se immessi limiti di peso o volume         18
310900970604TISI31R- Limiti obbligatori se immesse linea o zona limite                    19
311000970917TISI31R- Impossibile inserire località di CAP obsoleti                        20
311100970917TISI31R- CAP errato o inesistente                                             21
311200970530TISI31R- Località errata                                                      22
311300970530TISI31R- Immettere L x gestire i dati della località, C x usare quelli del cap23
311400970912TISI31R- Il limite oltre deve essere superiore del limite sotto               24
311500970917TISI31R- Immettere 'O' per rendere obsoleta la località, o lasciare vuoto     25
311600970618TISI31R- Unica località per il cap. impossibile gestirla a livello località   26
311700970530TISI31R- Immettere 'S' o 'N'                                                  27
311800110623TISI31R- Tipo inoltro errato o non utilizzabile nel cappario                  28
311900970618TISI31R- La località è uguale al cap. Impossibile gestirla a livello località 29
312000970603TISI31R- Tipo tempo di consegna errato o incompleto                           30
312100010322TISI31R- Tasto funzione non consentito sul campo corrente.                    31
312200010322TISI31R- Tipo tempo ritiro errato o incompleto                                32
312300041008TISI31R- Valorizzare entrambi i campi o non valorizzarne nessuno.             33
312400130905Esistono OrariServizi personalizz.x la linea modificata saranno elimiati      34
312500130905TISI31R- F17 consentito soltando sui campi LINEA di ARRIVO valorizzati        35
312600970526** FUD - FUNZIONI
312700970526                   -        (S/N)=N                        funz.01¬
312800970526                   -        (S/N)=N                        funz.02¬
312900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
313000010322F04=Visual. tempi  -        (S/N)=S  <== S-FISSO           funz.04¬
313100130827                   -        (S/N)=N                        funz.05¬
313200970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
313300130827F07=Orari Servizi  -        (S/N)=S                        funz.07¬
313400130821                   -        (S/N)=N                        funz.08¬
313500130821                   -        (S/N)=N                        funz.09¬
313600970527                   -        (S/N)=N                        funz.10¬
313700970526                   -        (S/N)=N                        funz.11¬
313800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
313900970602F13=Sinonimi       -        (S/N)=S                        funz.13¬
314000970603F14=Zone consegna  -        (S/N)=S                        funz.14¬
314100970526                   -        (S/N)=N                        funz.15¬
314200970526                   -        (S/N)=N                        funz.16¬
314300130905F17=OrariServLinea -        (S/N)=N                        funz.17¬
314400970526                   -        (S/N)=N                        funz.18¬
314500970526                   -        (S/N)=N                        funz.19¬
314600970526                   -        (S/N)=N                        funz.20¬
314700970526                   -        (S/N)=N                        funz.21¬
314800970526                   -        (S/N)=N                        funz.22¬
314900970526                   -        (S/N)=N                        funz.23¬
315000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
