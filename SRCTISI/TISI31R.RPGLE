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
012900170116     d*** dvpodeco      e ds
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
014900170116     d wabiute         s              1
015000170116     d w003a           s              3
015100131007     d XX              s              3  0
015200131007     d YY              s              3  0
015300131007
015400131007
015500131007      /copy gaitrasrc/srcprotopr,tibs02r
015600131007
015700970526     C*------------------------------------------------------------------------*
015800970526     C* MAIN LINES
015900970526     C*------------------------------------------------------------------------*
016000970526     C*
016100970526     C* NON CI SONO ERRORI IN ENTRATA
016200970526IF  1C     ERRL00        IFEQ      '0'
016300970526     C*
016400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016800970526     C*
016900970526     C* EMETTE FORMATO VIDEO
017000970526     C                   EXSR      BUTFMR
017100970526     C*
017200970526     C* GESTIONE TASTI FUNZIONALI
017300970526     C                   EXSR      FUNCON
017400970526E   2C                   ENDDO
017500970526     C*
017600970526     C* CI SONO ERRORI IN ENTRATA
017700970527X   1C                   ELSE
017800970526     C                   MOVEL     '1'           D00ERR
017900970526     C                   MOVEL     DSMSMS        D00MSG
018000970526E   1C                   ENDIF
018100970626     C*
018200970626     C* OPERAZIONI FINALI
018300970626     C                   EXSR      FINSR
018400970526     C* FINE PGM
018500970526     C                   SETON                                        LR
018600970526     C*------------------------------------------------------------------------*
018700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
018800970526     C*------------------------------------------------------------------------*
018900970526     C     BUTFMR        BEGSR
019000970526     C*
019100970526     C* EMETTE LA TESTATA
019200970529     C                   WRITE     SI31D01
019300970526     C*
019400970526     C* SPEDISCE EVENTUALE MESSAGGIO
019500970526     C                   EXSR      SNDMSG
019600970526     C*
019700970526     C* EMETTE LE FUNZIONI
019800970529     C                   WRITE     SI31D02
019900970526     C*
020000970526     C* EMETTE IL VIDEO
020100970529     C                   EXFMT     SI31D03
020200970526     C*
020300970526     C* REIMPOSTA FLAG DI ERRORE
020400970526     C                   MOVEL     '0'           ERRL00
020500970526     C*
020600970526     C* REIMPOSTA INDICATORI DI ERRORE
020700970526     C                   SETOFF                                         5859
020800970526     C                   SETOFF                                       606162
020900970526     C                   SETOFF                                       636465
021000970526     C                   SETOFF                                       666768
021100970526     C                   SETOFF                                       697071
021200970526     C                   SETOFF                                       727374
021300970526     C                   SETOFF                                       757677
021400970526     C                   SETOFF                                       787980
021500970526     C                   SETOFF                                       818283
021600970526     C                   SETOFF                                       848586
021700970526     C                   SETOFF                                       878889
021800000426     C                   SETOFF                                       525354
021900010322     C                   SETOFF                                       5544
022000970526     C*
022100970526     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
022400970526     C*------------------------------------------------------------------------*
022500970526     C     SNDMSG        BEGSR
022600970526     C*
022700970526     C* PULISCE IL SFILE MESSAGGI
022800970526     C                   MOVEL     '1'           *IN40
022900970529     C                   WRITE     SI31DMC
023000970526     C                   MOVEL     '0'           *IN40
023100970526     C*
023200970526     C* VISUALIZZA IL MESSAGGIO
023300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
023400970526     C                   CALL      'XMSG'        XMSGPA
023500970526IF  1C     DSMSER        IFEQ      *ZEROS
023600970526     C                   MOVEL     DSMSKE        MSGKEY
023700970526E   1C                   ENDIF
023800970526     C                   Z-ADD     1             MSGULT
023900970529     C                   WRITE     SI31DMS
024000970529     C                   WRITE     SI31DMC
024100970526     C*
024200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
024300970526     C                   MOVEL     *BLANKS       DSMSMS
024400970526     C*
024500970526     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* ENTGES - GESTIONE ENTER -> INVIO
024800970526     C*------------------------------------------------------------------------*
024900970526     C     ENTGES        BEGSR
025000970526     C*
025100970526     C* CONTROLLA I CAMPI IMMESSI
025200970526     C                   EXSR      DECODI
025300970526     C*
025400970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
025500970526IF  1C     DEPOP1        IFEQ      'O05'
025600970526     C                   MOVEL     '1'           FINL00
025700970526E   1C                   ENDIF
025800970526     C*
025900970526     C                   ENDSR
026000970526     C*------------------------------------------------------------------------*
026100970526     C* F03GES - GESTIONE F03 -> FINE
026200970526     C*------------------------------------------------------------------------*
026300970526     C     F03GES        BEGSR
026400970526     C*
026500970526     C* IMPOSTA "PREMUTO F03"
026600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
026700970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
026800970526     C*
026900970526     C                   ENDSR
027000010322     C*------------------------------------------------------------------------*
027100010322     C* F04GES - GESTIONE F04 -> VISUALIZZAZIONE TEMPI CONSEGNA/RITIRO
027200010322     C*------------------------------------------------------------------------*
027300010322     C     F04GES        BEGSR
027400090605      * imposto le descrizioni fisse per i 3 tipi servizio utilizzati
027500090605      * 'C' - 'D' - 'E'
027600090605     c                   eval      ktbcod = '5E'
027700090605     c     ke2tab        setll     tabel00f
027800090605     c                   do        *hival
027900090605     c     ke2tab        reade     tabel00f
028000090605     c                   if        %eof(tabel00f)
028100090605     c                   leave
028200090605     c                   endif
028300090605     c                   eval      ds5e = tbluni
028400090605     c                   if        tblkey = 'C'
028500090605     c                   eval      viddtspc = §5ed03
028600090605     c                   endif
028700090605     c                   if        tblkey = 'D'
028800090605     c                   eval      viddtspd = §5ed03
028900090605     c                   endif
029000090605     c                   if        tblkey = 'E'
029100090605     c                   eval      viddtspe = §5ed03
029200090605     c                   endif
029300170116     c**                 if        tblkey = 'H' and (vidh0c>0 or
029400170116     c**                           Datasys>=dataiso)
029500170116     c
029600170116     c                   if        tblkey = 'H' and  vidh0c>0
029700090903     c                   eval      viddtsph = §5ed03
029800090903     c                   endif
029900090605     c                   enddo
030000010322     C*
030100010322     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
030200010322     C     RNMFL         IFEQ      'VIDTTC'
030300010322     C                   EXFMT     SI31WWC
030400010322     C                   ELSE
030500010322     C     RNMFL         IFEQ      'VIDTTR'
030600010322     C                   EXFMT     SI31WWR
030700010322     C                   ELSE
030800010322     C                   MOVEL     MSG(31)       DSMSMS
030900010322     C                   ENDIF
031000010322     C                   ENDIF
031100010322     C*
031200010322     C                   ENDSR
031300130830     C*------------------------------------------------------------------------*
031400130830     C* F17GES - GESTIONE F17 -> Orari servizi  per linea di arrivo
031500130830     C*------------------------------------------------------------------------*
031600130830     C     F17GES        BEGSR
031700130830     c                   clear                   dsmsms
031800130830     c                   exsr      decodi
031900130830     c
032000130830    1c                   if        dsmsms=*blanks
032100130830     C*
032200130830     c                   clear                   tisio0ds
032300130830     C                   MOVEL     VIDNAR        do0NAR
032400130830     c                   if        %subst(depop0:2:2)='02'
032500130830     C                   MOVEL     'G'           do0tric
032600130830     c                   else
032700130830     C                   MOVEL     'I'           do0tric
032800130830     c                   endif
032900130830     c                   movel     d00dri        do0dri
033000130830     c                   movel     d00ver        do0ver
033100130830
033200130830     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
033300130830    2c                   select
033400130830     C     RNMFL         wheneq    'VIDLNA'
033500130830     c     vidlna        andgt     *zeros
033600130830     C                   movel     vidlna        do0lna
033700130830     c
033800130830     C     RNMFL         wheneq    'VIDLOL'
033900130830     c     vidlol        andgt     *zeros
034000130830     C                   movel     vidlol        do0lna
034100130830     C     RNMFL         wheneq    'VIDLOS'
034200130830     c     vidlos        andgt     *zeros
034300130830     C                   movel     vidlos        do0lna
034400130830     C                   other
034500130905     C                   MOVEL     MSG(35)       DSMSMS
034600130830    2C                   ENDsl
034700130830
034800130830    2c                   if        dsmsms=*blanks
034900130830     C                   call      'TISIO2R'
035000130830     C                   PARM                    KPJBA
035100130830     C                   PARM                    tisio0ds
035200130830     C*
035300130830     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
035400130830IF  3C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
035500130830     C                   MOVEL     '1'           ERRL00                         *ERRORE
035600130830     C                   MOVEL     '1'           F03L00                         *FINE PGM
035700130830X   1C                   ELSE                                                   *NON PREMUTO F03
035800130830     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
035900130830E   3C                   ENDIF
036000130830     C*
036100130830E   2C                   ENDIF
036200130830E   1C                   ENDIF
036300130830     C*
036400130830     C                   ENDSR
036500970526     C*------------------------------------------------------------------------*
036600970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
036700970526     C*------------------------------------------------------------------------*
036800970526     C     F06GES        BEGSR
036900970526     C*
037000970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
037100970526IF  1C     DEPOP1        IFEQ      'F06'
037200970526     C                   EXSR      INSERI
037300970526E   1C                   ENDIF
037400970526IF  1C     DEPOP1        IFEQ      'O02'
037500970526     C                   EXSR      MODIFI
037600970526E   1C                   ENDIF
037700970526IF  1C     DEPOP1        IFEQ      'O04'
037800970526     C                   EXSR      ANNULL
037900970526E   1C                   ENDIF
038000970526IF  1C     DEPOP1        IFEQ      'O07'
038100970526     C                   EXSR      RIPRIS
038200970526E   1C                   ENDIF
038300970526     C*
038400970526     C                   ENDSR
038500970526     C*------------------------------------------------------------------------*
038600970526     C* F12GES - GESTIONE F12 -> RITORNO
038700970526     C*------------------------------------------------------------------------*
038800970526     C     F12GES        BEGSR
038900970526     C*
039000970526     C* IMPOSTA "PREMUTO F12"
039100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
039200970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
039300970526     C*
039400970526     C                   ENDSR
039500970602     C*------------------------------------------------------------------------*
039600970602     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
039700970602     C*------------------------------------------------------------------------*
039800970602     C     F13GES        BEGSR
039900970602     C*
040000970602     C                   MOVEL     VIDNAR        DNAR
040100970602     C                   MOVEL     VIDCAP        DCAP
040200970602     C                   MOVEL     VIDLOC        DLOC
040300970602     C                   MOVEL     DEPOP0        WOP0
040400970602     C                   MOVEL     'S'           WOP0
040500970602     C                   MOVEL     'F13'         WOP1
040600970602     C                   MOVEL     'TISI35R'     WPGM              8
040700970602     C                   EXSR      LANPGM
040800970602     C*
040900970602     C                   ENDSR
041000970603     C*------------------------------------------------------------------------*
041100970603     C* F14GES - GESTIONE F14 -> ZONE DI CONSEGNA
041200970603     C*------------------------------------------------------------------------*
041300970603     C     F14GES        BEGSR
041400970603     C*
041500970603     C                   CALL      'TRTB23R'
041600970603     C                   PARM                    KPJBA
041700970603     C*
041800970603     C                   ENDSR
041900130821     C*------------------------------------------------------------------------*
042000130827     C* F07GES - GESTIONE F07 -> Orari servizi
042100130821     C*------------------------------------------------------------------------*
042200130827     C     F07GES        BEGSR
042300130821     C*
042400130903     c                   exsr      F06ges
042500130903
042600130903     c                   if        errl00='0'
042700130903
042800130821     c                   clear                   tisio0ds
042900130821     C                   MOVEL     VIDNAR        do0NAR
043000130821     C                   MOVEL     VIDCAP        Do0CAP
043100130821     C                   MOVEL     VIDLOC        Do0LOC
043200130830     c                   if        %subst(depop0:2:2)='02'
043300130830     C                   MOVEL     'G'           do0tric
043400130830     c                   else
043500130830     C                   MOVEL     'I'           do0tric
043600130830     c                   endif
043700130821     c                   movel     d00dri        do0dri
043800130821     c                   movel     d00ver        do0ver
043900130821
044000130821     C                   call      'TISIO2R'
044100130821     C                   PARM                    KPJBA
044200130821     C                   PARM                    tisio0ds
044300130821     C*
044400130821     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
044500130821IF  1C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
044600130821     C                   MOVEL     '1'           ERRL00                         *ERRORE
044700130821     C                   MOVEL     '1'           F03L00                         *FINE PGM
044800130821X   1C                   ELSE                                                   *NON PREMUTO F03
044900130821     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
045000130821E   1C                   ENDIF
045100130821     C*
045200130903E   1c                   ENDIF
045300130821     C*
045400130821     C                   ENDSR
045500130821     C*------------------------------------------------------------------------*
045600970526     C*------------------------------------------------------------------------*
045700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
045800970526     C*------------------------------------------------------------------------*
045900970526     C     F24GES        BEGSR
046000970526     C*
046100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
046200970526     C*
046300970526     C                   ENDSR
046400970526     C*------------------------------------------------------------------------*
046500970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
046600970526     C*------------------------------------------------------------------------*
046700970526     C     INSERI        BEGSR
046800970526     C*
046900970526     C* CONTROLLA I CAMPI IMMESSI
047000970526     C                   EXSR      DECODI
047100970618     C*
047200970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
047300970618IF  1C     ERRL00        IFEQ      '0'
047400970618     C                   EXSR      DECF06
047500970917IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047600970530     C                   Z-ADD     D00VER        CPLVER                         *CHIAVE
047700970530     C                   MOVEL     VIDNAR        CPLNAR
047800970530     C                   MOVEL     VIDCAP        CPLCAP
047900970530     C                   MOVEL     VIDLOC        CPLLOC
048000970530     C                   EXSR      IMPCPL                                       *ALTRI CAMPI
048100970530     C                   Z-ADD     *ZEROS        CPLDTR                         *DATA TRASMISSIONE
048200970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
048300970530     C                   MOVEL     *BLANKS       CPLATB                         *ANNULLAMENTO
048400980616     C                   MOVEL     'N'           CPLESS                         *ESISTENZA SINONIMI
048500980616     C                   MOVEL     'N'           CPLESV                         *ECCEIZONI X VIE
048600970530     C                   WRITE     AZCPL000
048700970527     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
048800970527     C*
048900970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
049000970527     C                   Z-ADD     D00VER        D81VER
049100970527     C                   MOVEL     VIDNAR        D81NAR
049200970527     C                   MOVEL     VIDCAP        D81CAP
049300970530     C                   MOVEL     'L'           D81CHK
049400970527     C                   CALL      'TISI81R'
049500970527     C                   PARM                    DSSI81
049600970602     C*
049700970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
049800970602     C                   EXSR      AZZVID
049900970618E   2C                   ENDIF
050000970618E   1C                   ENDIF
050100970526     C*
050200970526     C                   ENDSR
050300970526     C*------------------------------------------------------------------------*
050400970526     C* MODIFI - OPERAZIONI DI MODIFICA
050500130916     c*------------------------------------------------------------------------*
050600970526     C     MODIFI        BEGSR
050700970613     C*
050800970613     C* CONTROLLA I CAMPI IMMESSI
050900970613     C                   EXSR      DECODI
051000970618     C*
051100970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
051200970618IF  1C     ERRL00        IFEQ      '0'
051300970618     C                   EXSR      DECF06
051400970613     C*
051500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
051600970613IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051700970530     C                   EXSR      LETCPL
051800970526     C*
051900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
052000970613IF  3C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052100130916     c* richiamo pgm per cancellare OrariServizi a fronte delle lna cambiate
052200130916     c                   exsr      sr_oraris
052300130916
052400970530     C                   EXSR      IMPCPL                                       *IMPOSTA I CAMPI
052500970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
052600970530     C                   MOVEL     *BLANKS       CPLATB                         *ANNULLAMENTO
052700970530     C                   UPDATE    AZCPL000
052800970526     C                   MOVEL     MSG(3)        D00MSG
052900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
053000970613E   3C                   ENDIF
053100970530     C*
053200970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
053300970530     C                   Z-ADD     D00VER        D81VER
053400970530     C                   MOVEL     VIDNAR        D81NAR
053500970530     C                   MOVEL     VIDCAP        D81CAP
053600970613     C                   MOVEL     'L'           D81CHK
053700970530     C                   CALL      'TISI81R'
053800970530     C                   PARM                    DSSI81
053900970613E   2C                   ENDIF
054000970618E   1C                   ENDIF
054100970526     C*
054200970526     C                   ENDSR
054300970526     C*------------------------------------------------------------------------*
054400970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
054500970526     C*------------------------------------------------------------------------*
054600970526     C     ANNULL        BEGSR
054700970526     C*
054800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
054900000602     C                   MOVEL     '0'           ANNDLT            1
055000970530     C                   EXSR      LETCPL
055100970527     C*
055200000426     C* ANNULLA LE LOCALITA'
055300970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
055400970530IF  2C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
055500970530IF  3C     CPLDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
055600970530     C                   MOVEL     'A'           CPLATB                         *FLAG ANNULLAMENTO
055700970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
055800970530     C                   UPDATE    AZCPL000                                     *ANNULLA
055900970526X   3C                   ELSE                                                   *MAI TRASMESSO
056000970530     C                   DELETE    AZCPL000                                     *CANCELLA
056100000602     C                   MOVEL     '1'           ANNDLT
056200970526E   3C                   ENDIF
056300970526E   2C                   ENDIF
056400970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
056500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
056600970925     C*
056700970925     C* ANNULLA I SINONIMI
056800970925     C                   Z-ADD     D00VER        KCSVER
056900970925     C                   MOVEL     CPLNAR        KCSNAR
057000970925     C                   MOVEL     CPLCAP        KCSCAP
057100970925     C                   MOVEL     CPLLOC        KCSLOC
057200970925     C     KEYCPS        SETLL     AZCPS01L                           99
057300970925     C     KEYCPS        READE     AZCPS01L                               99
057400970925DO  2C     *IN99         DOWEQ     *OFF
057500970925IF  3C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
057600970925IF  4C     CPSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
057700970925     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
057800970925     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
057900970925     C                   UPDATE    AZCPS000                                     *ANNULLA
058000970925X   4C                   ELSE                                                   *MAI TRASMESSO
058100970925     C                   DELETE    AZCPS000                                     *CANCELLA
058200970925E   4C                   ENDIF
058300970925E   3C                   ENDIF
058400970925     C     KEYCPS        READE     AZCPS01L                               99
058500970925E   2C                   ENDDO                                                  *FINE SINONIMI
058600130917
058700130917     c* ANNULLA GLI ORARI SERVIZI
058800130917     C     KEYCPS        SETLL     AZcos01l
058900130917     C     KEYCPS        READE     AZCos01L
059000130917DO  2C                   DOW       not %eof(azcos01l)
059100130917     C                   DELETE    AZCOS000                                     *CANCELLA
059200130917     C     KEYCPS        READE     AZCos01L
059300130917E   2C                   ENDDO
059400970530     C*
059500970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
059600970530     C                   Z-ADD     D00VER        D81VER
059700970530     C                   MOVEL     VIDNAR        D81NAR
059800970530     C                   MOVEL     VIDCAP        D81CAP
059900970530     C                   MOVEL     'L'           D81CHK
060000970530     C                   CALL      'TISI81R'
060100970530     C                   PARM                    DSSI81
060200970526E   1C                   ENDIF
060300970526     C*
060400970526     C                   ENDSR
060500970526     C*------------------------------------------------------------------------*
060600970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
060700970526     C*------------------------------------------------------------------------*
060800970526     C     RIPRIS        BEGSR
060900970526     C*
061000970526     C* CONTROLLA I CAMPI IMMESSI
061100970526     C                   EXSR      DECODI
061200970618     C*
061300970618     C* CONTROLLA I CAMPI IMMESSI -CONTROLLI SOLO ALLA CONFERMA-
061400970618IF  1C     ERRL00        IFEQ      '0'
061500970618     C                   EXSR      DECF06
061600970526     C*
061700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
061800970618IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
061900970530     C                   EXSR      LETCPL
062000970526     C*
062100970526     C* IMPOSTA IL RECORD DA SCRIVERE
062200970618IF  3C     ERRL00        IFEQ      '0'                                          *NO ERRORI
062300970530     C                   MOVEL     *BLANKS       CPLATB                         *FLAG ANNULLAMENTO
062400970530     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
062500970530     C                   UPDATE    AZCPL000                                     *ANNULLA
062600970618E   3C                   ENDIF
062700970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
062800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
062900970530     C*
063000970530     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
063100970530     C                   Z-ADD     D00VER        D81VER
063200970530     C                   MOVEL     VIDNAR        D81NAR
063300970530     C                   MOVEL     VIDCAP        D81CAP
063400970530     C                   MOVEL     'L'           D81CHK
063500970530     C                   CALL      'TISI81R'
063600970530     C                   PARM                    DSSI81
063700970618E   2C                   ENDIF
063800970618E   1C                   ENDIF
063900970526     C*
064000970526     C                   ENDSR
064100970526     C*------------------------------------------------------------------------*
064200970530     C* LETCPL - LEGGE IL RECORD PERL'AGGIORNAMENTO
064300970526     C*------------------------------------------------------------------------*
064400970530     C     LETCPL        BEGSR
064500970526     C*
064600000426     C     DEPOP1        IFNE      'F06'
064700970530     C                   Z-ADD     D00VER        KCLVER
064800160728     C                   MOVEL     vidNAR        KCLNAR
064900160728     C                   MOVEL     vidCAP        KCLCAP
065000160728     C                   MOVEL     vidLOC        KCLLOC
065100970530     C     KEYCPL        CHAIN     AZCPL01L                           9998
065200970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
065300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
065400970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
065500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
065600970526E   1C                   ENDIF
065700970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
065800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
065900970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
066000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
066100970526E   1C                   ENDIF
066200000426     C                   ENDIF
066300970526     C*
066400970526     C                   ENDSR
066500970526     C*------------------------------------------------------------------------*
066600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
066700970526     C*------------------------------------------------------------------------*
066800970526     C     DECODI        BEGSR
066900050428     C*
067000050428     C* NO VALORI NEGATIVI
067100050428     C                   IF        VIDLKS < *zeros
067200050428     C                   EVAL      VIDLKS = VIDLKS * -1
067300050428     C                   ENDIF
067400050428     C                   IF        VIDLKG < *zeros
067500050428     C                   EVAL      VIDLKG = VIDLKG * -1
067600050428     C                   ENDIF
067700050428     C                   IF        VIDLK2 < *zeros
067800050428     C                   EVAL      VIDLK2 = VIDLK2 * -1
067900050428     C                   ENDIF
068000050428     C                   IF        VIDLMS < *zeros
068100050428     C                   EVAL      VIDLMS = VIDLMS * -1
068200050428     C                   ENDIF
068300050428     C                   IF        VIDLMC < *zeros
068400050428     C                   EVAL      VIDLMC = VIDLMC * -1
068500050428     C                   ENDIF
068600050428     C                   IF        VIDLM2 < *zeros
068700050428     C                   EVAL      VIDLM2 = VIDLM2 * -1
068800050428     C                   ENDIF
068900050428     C*
069000970530     C*--------------------
069100970530     C* NAZIONE
069200970530     C*--------------------
069300970530     C     *LIKE         DEFINE    §15CPR        DEPCPR
069400970530IF  1C     ERRL00        IFEQ      '0'
069500970530     C                   MOVEL     *BLANKS       VDDNAR
069600970530     C                   MOVEL     VIDNAR        WNAR
069700970530     C                   MOVEL     *BLANKS       WDNAR
069800970530     C                   EXSR      CHKNAR
069900970530     C                   MOVEL     WNAR          VIDNAR
070000970530     C                   MOVEL     WDNAR         VDDNAR
070100970530IF  2C     ERRL00        IFEQ      '1'
070200970530     C                   SETON                                        60
070300970530     C                   MOVEL     MSG(13)       DSMSMS
070400970530E   2C                   ENDIF
070500970530E   1C                   ENDIF
070600970530     C*--------------------
070700970530     C* CAP
070800970530     C*--------------------
070900970530IF  1C     ERRL00        IFEQ      '0'
071000970530     C                   MOVEL     VIDNAR        WNAR
071100970530     C                   MOVEL     VIDCAP        WCAP
071200970917     C                   MOVEL     *BLANKS       WFI1
071300970530     C                   EXSR      CHKCAP
071400970530IF  2C     ERRL00        IFEQ      '1'
071500970530     C                   SETON                                        61
071600970602     C                   MOVEL     MSG(21)       DSMSMS
071700970530E   2C                   ENDIF
071800970530E   1C                   ENDIF
071900970530     C*--------------------
072000970530     C* LOCALITA'
072100970530     C*--------------------
072200970530IF  1C     ERRL00        IFEQ      '0'
072300970530IF  2C     VIDLOC        IFEQ      *BLANKS
072400970530     C                   SETON                                        70
072500970530     C                   MOVEL     '1'           ERRL00
072600970530     C                   MOVEL     MSG(22)       DSMSMS
072700970530E   2C                   ENDIF
072800970530E   1C                   ENDIF
072900970530     C*--------------------
073000970530     C* NAZIONE + CAP + LOCALITA'
073100970530     C*--------------------
073200970917IF  1C     DEPOP1        IFEQ      'F06'
073300970917     C*
073400970917     C* INSERIMENTO -> IL CAP NON PUO' ESSERE OBSOLETO
073500970917IF  2C     ERRL00        IFEQ      '0'
073600970917IF  3C     WFI1          IFEQ      'O '
073700970917     C                   SETON                                        6061
073800970917     C                   MOVEL     MSG(20)       DSMSMS
073900970917     C                   MOVEL     '1'           ERRL00
074000970917E   3C                   ENDIF
074100970917E   2C                   ENDIF
074200970917     C*
074300970530     C* INSERIMENTO -> RECORD INESISTENTE
074400970917IF  2C     ERRL00        IFEQ      '0'
074500970530     C                   Z-ADD     D00VER        KCLVER
074600970530     C                   MOVEL     VIDNAR        KCLNAR
074700970530     C                   MOVEL     VIDCAP        KCLCAP
074800970530     C                   MOVEL     VIDLOC        KCLLOC
074900970530     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
075000970917IF  3C     *IN99         IFEQ      *OFF
075100970530     C                   SETON                                        606170
075200970530     C                   MOVEL     '1'           ERRL00
075300970530     C                   MOVEL     MSG(8)        DSMSMS
075400970917E   3C                   ENDIF
075500970917E   2C                   ENDIF
075600970917E   1C                   ENDIF
075700970530     C*--------------------
075800970530     C* STAMPARE IN DELIVERY
075900970530     C*--------------------
076000970530IF  1C     ERRL00        IFEQ      '0'
076100970530IF  2C     VIDDLV        IFEQ      'S'
076200970530     C     VIDDLV        OREQ      'N'
076300970530X   2C                   ELSE
076400970530     C                   MOVEL     '1'           ERRL00
076500970530     C                   SETON                                        73
076600970530     C                   MOVEL     MSG(27)       DSMSMS
076700970530E   2C                   ENDIF
076800970530E   1C                   ENDIF
076900970917     C*--------------------
077000970917     C* OBSOLETO
077100970917     C*--------------------
077200970917IF  1C     ERRL00        IFEQ      '0'
077300970917IF  2C     VIDFI1        IFEQ      'O'
077400970917     C     VIDFI1        OREQ      *BLANKS
077500970917X   2C                   ELSE
077600970917     C                   MOVEL     '1'           ERRL00
077700970917     C                   SETON                                        85
077800970917     C                   MOVEL     MSG(25)       DSMSMS
077900970917E   2C                   ENDIF
078000970917E   1C                   ENDIF
078100970917     C*
078200970917     C* VARIAZIONE -> SE CAP OBSOLETO ANCHE LA LOCALITA' DEVE ESSERE OBSOLETA
078300970917IF  1C     ERRL00        IFEQ      '0'
078400970917IF  2C     DEPOP1        IFEQ      'O02'
078500970917IF  3C     WFI1          IFEQ      'O '
078600970917     C     VIDFI1        ANDNE     'O'
078700970917     C                   SETON                                        85
078800970917     C                   MOVEL     MSG(12)       DSMSMS
078900970917     C                   MOVEL     '1'           ERRL00
079000970917E   3C                   ENDIF
079100970917E   2C                   ENDIF
079200970917E   1C                   ENDIF
079300970530     C*--------------------
079400970530     C* LIVELLO DI GESTIONE
079500970530     C*--------------------
079600970530IF  1C     ERRL00        IFEQ      '0'
079700970530IF  2C     VIDLIV        IFEQ      'C'
079800970530     C     VIDLIV        OREQ      'L'
079900970530X   2C                   ELSE
080000970530     C                   SETON                                        74
080100970530     C                   MOVEL     '1'           ERRL00
080200970530     C                   MOVEL     MSG(23)       DSMSMS
080300970530E   2C                   ENDIF
080400970530E   1C                   ENDIF
080500970530     C*
080600970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> IMPORTA I DATI DEL CAPPARIO
080700970530IF  1C     ERRL00        IFEQ      '0'
080800970530IF  2C     VIDLIV        IFEQ      'C'                                          *LIVELLO CAP
080900970530     C                   SETON                                        92        *PROTEZIONE CAMPI
081000970530     C                   EXSR      IMPCPC
081100970530X   2C                   ELSE                                                   *LIVELLO LOCALITA
081200970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
081300970530E   2C                   ENDIF
081400970530E   1C                   ENDIF
081500970530     C*--------------------
081600970604     C* LINEA ARRIVO STANDARD
081700970530     C*--------------------
081800970530IF  1C     ERRL00        IFEQ      '0'
081900970530     C                   MOVEL     *BLANKS       VDDLNA
082000970530     C                   MOVEL     VIDLNA        WLIN
082100970530     C                   MOVEL     *BLANKS       WDLIN
082200970530     C                   EXSR      CHKLIN
082300970530     C                   MOVEL     WLIN          VIDLNA
082400970530     C                   MOVEL     WDLIN         VDDLNA
082500970530IF  2C     ERRL00        IFEQ      '1'
082600970530     C                   SETON                                        62
082700970530     C                   MOVEL     MSG(10)       DSMSMS
082800970530E   2C                   ENDIF
082900970530E   1C                   ENDIF
083000130916
083100130916IF  1C     ERRL00        IFEQ      '0'
083200130917     c     depop1        andne     'F06'
083300130916     c* se cambiata la linea di arrivo verifico se ci sono OrariServizi da eliminare
083400130916     c                   movel     vidlna        w0030             3 0
083500130916     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
083600130916     c* e non ho già avvisato della cancellazione orariservizi
083700130916     c                   if        w0030<>f_lna and F_lna<>s_vidlna
083800130916     c                                          and f_lna<>s_vidlos
083900130916     c                                          and f_lna<>s_vidlol
084000130916     c                                          and f07lna=*blanks
084100130916     c                   clear                   tisio7ds
084200130916     c                   eval      io7mod='C'
084300130917     c* Il livello da passare è quello a video
084400130917     c                   eval      io7liv=vidliv
084500130916     c                   eval      io7ver=d00ver
084600130916     c                   eval      io7nar=vidnar
084700130916     c                   eval      io7cap=vidcap
084800130917     c                   eval      io7loc=vidloc
084900130916     c                   eval      io7lna=f_lna
085000130916     c                   eval      io7lns=%dec(vidlna:3:0)
085100130916     c                   eval      io7los=%dec(vidlos:3:0)
085200130916     c                   eval      io7lol=%dec(vidlol:3:0)
085300131016     c***                movel     tisio7ds      kpjbu
085400130916     c                   call      'TISIO7R'
085500130916     c                   parm                    kpjba
085600131016     c                   parm                    tisio7ds
085700130916     c                   if        oo7err='1'
085800130916     c                   movel     oo7msg        DSmsms
085900130916     c                   eval      *in62  = *on
086000130916     c                   eval      ErrL00 = *on
086100130916     c                   eval      f07lna='S'
086200130916e   4c                   endif
086300130916     c                   eval      s_vidlna=F_lna
086400130916    3c                   endif
086500130916     c                   endif
086600970604     C*--------------------
086700970604     C* ZONA DI CONSEGNA STANDARD
086800970604     C*--------------------
086900970604IF  1C     ERRL00        IFEQ      '0'
087000970604     C                   MOVEL     VIDLNA        WLIN
087100970604     C                   Z-ADD     VIDZNC        WZNC
087200970604     C                   EXSR      CHKZNC
087300970604IF  2C     ERRL00        IFEQ      '1'
087400970604     C                   SETON                                        63
087500970604     C                   MOVEL     MSG(11)       DSMSMS
087600970604E   2C                   ENDIF
087700060206if  2c                   if        §03nuc = 'S'
087800060206     c                   movel     Msg(17)       DSmsms
087900060206     c                   eval      *in63  = *on
088000060206     c                   eval      ErrL00 = *on
088100060206e   2c                   endif
088200970604E   1C                   ENDIF
088300970604     C*--------------------
088400970912     C* LINEA ARRIVO SOTTO
088500970604     C*--------------------
088600970604IF  1C     ERRL00        IFEQ      '0'
088700970604     C                   MOVEL     *BLANKS       VDDLOS
088800970604IF  2C     VIDLOS        IFNE      *ZEROS
088900970604     C     VIDLOS        ANDNE     *BLANKS
089000970604     C                   MOVEL     VIDLOS        WLIN
089100970604     C                   MOVEL     *BLANKS       WDLIN
089200970604     C                   EXSR      CHKLIN
089300970604     C                   MOVEL     WLIN          VIDLOS
089400970604     C                   MOVEL     WDLIN         VDDLOS
089500970604IF  3C     ERRL00        IFEQ      '1'
089600970604     C                   SETON                                        81
089700970604     C                   MOVEL     MSG(10)       DSMSMS
089800970604E   3C                   ENDIF
089900970604E   2C                   ENDIF
090000970604E   1C                   ENDIF
090100130916IF  1C     ERRL00        IFEQ      '0'
090200130917     c     depop1        andne     'F06'
090300130916     c* se cambiata la linea di arrivo verifico se ci sono OrariServizi da eliminare
090400130916     c                   movel     vidlos        w0030             3 0
090500130916     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
090600130916     c* e non ho già avvisato della cancellazione orariservizi
090700130916     c                   if        w0030<>f_los and F_los<>s_vidlna
090800130916     c                                          and f_los<>s_vidlos
090900130916     c                                          and f_los<>s_vidlol
091000130917     c                                          and f07lna=*blanks
091100130916     c                   clear                   tisio7ds
091200130916     c                   eval      io7mod='C'
091300130917     c* Il livello da passare è quello a video
091400130917     c                   eval      io7liv=vidliv
091500130916     c                   eval      io7ver=d00ver
091600130916     c                   eval      io7nar=vidnar
091700130916     c                   eval      io7cap=vidcap
091800130917     c                   eval      io7loc=vidloc
091900130916     c                   eval      io7lna=f_los
092000130916     c                   eval      io7lns=%dec(vidlna:3:0)
092100130916     c                   eval      io7los=%dec(vidlos:3:0)
092200130916     c                   eval      io7lol=%dec(vidlol:3:0)
092300131016     c****               movel     tisio7ds      kpjbu
092400130916     c                   call      'TISIO7R'
092500130916     c                   parm                    kpjba
092600131016     c                   parm                    tisio7ds
092700130916     c                   if        oo7err='1'
092800130916     c                   movel     oo7msg        DSmsms
092900130916     c                   eval      *in81  = *on
093000130916     c                   eval      ErrL00 = *on
093100130916     c                   eval      f07lna='S'
093200130916e   4c                   endif
093300130916     c                   eval      s_vidlos=F_los
093400130916    3c                   endif
093500130916     c                   endif
093600970604     C*--------------------
093700970912     C* ZONA DI CONSEGNA SOTTO
093800970604     C*--------------------
093900970604IF  1C     ERRL00        IFEQ      '0'
094000971126IF  2C     VIDLOS        IFNE      *ZEROS
094100970604     C                   MOVEL     VIDLOS        WLIN
094200970604     C                   Z-ADD     VIDZOS        WZNC
094300970604     C                   EXSR      CHKZNC
094400970604IF  3C     ERRL00        IFEQ      '1'
094500970604     C                   SETON                                        82
094600970604     C                   MOVEL     MSG(11)       DSMSMS
094700970604E   3C                   ENDIF
094800060206if  3c                   if        §03nuc = 'S'
094900060206     c                   movel     Msg(17)       DSmsms
095000060206     c                   eval      *in82  = *on
095100060206     c                   eval      ErrL00 = *on
095200060206e   3c                   endif
095300970604E   2C                   ENDIF
095400970604E   1C                   ENDIF
095500970604     C*--------------------
095600970912     C* PESO/VOLUME SOTTO
095700970604     C*--------------------
095800970604IF  1C     ERRL00        IFEQ      '0'
095900970604IF  2C     VIDLKS        IFGT      *ZEROS
096000970604     C     VIDLMS        ORGT      *ZEROS
096100970604IF  3C     VIDLOS        IFEQ      *BLANKS
096200970604     C     VIDLOS        OREQ      *ZEROS
096300970604     C                   SETON                                        81
096400970604     C                   MOVEL     MSG(18)       DSMSMS
096500970604     C                   MOVEL     '1'           ERRL00
096600970604E   3C                   ENDIF
096700070103E   2C                   ENDIF
096800070103IF  3C     VIDLOS        IFNE      *BLANKS
096900070103     C     VIDLOS        ANDNE     *ZEROS
097000070103IF  4C                   IF        VIDLKS = *ZEROS OR
097100070103     C                             VIDLMS = *ZEROS
097200970604     C                   SETON                                        8384
097300970604     C                   MOVEL     MSG(19)       DSMSMS
097400970604     C                   MOVEL     '1'           ERRL00
097500070103E   4C                   ENDIF
097600070103E   3C                   ENDIF
097700970604E   1C                   ENDIF
097800970530     C*--------------------
097900970912     C* LINEA ARRIVO OLTRE
098000970530     C*--------------------
098100970530IF  1C     ERRL00        IFEQ      '0'
098200970530     C                   MOVEL     *BLANKS       VDDLOL
098300970530IF  2C     VIDLOL        IFNE      *ZEROS
098400970530     C     VIDLOL        ANDNE     *BLANKS
098500970530     C                   MOVEL     VIDLOL        WLIN
098600970530     C                   MOVEL     *BLANKS       WDLIN
098700970530     C                   EXSR      CHKLIN
098800970530     C                   MOVEL     WLIN          VIDLOL
098900970530     C                   MOVEL     WDLIN         VDDLOL
099000970530IF  3C     ERRL00        IFEQ      '1'
099100970530     C                   SETON                                        67
099200970530     C                   MOVEL     MSG(10)       DSMSMS
099300970530E   3C                   ENDIF
099400970530E   2C                   ENDIF
099500970530E   1C                   ENDIF
099600130916IF  1C     ERRL00        IFEQ      '0'
099700130917     c     depop1        andne     'F06'
099800130916     c* se cambiata la linea di arrivo verifico se ci sono OrariServizi da eliminare
099900130916     c                   movel     vidlol        w0030             3 0
100000130916     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
100100130916     c* e non ho già avvisato della cancellazione orariservizi
100200130916     c                   if        w0030<>f_lol and F_lol<>s_vidlna
100300130916     c                                          and f_lol<>s_vidlos
100400130916     c                                          and f_lol<>s_vidlol
100500130917     c                                          and f07lna=*blanks
100600130916     c                   clear                   tisio7ds
100700130916     c                   eval      io7mod='C'
100800130917     c* Il livello da passare è quello a video
100900130917     c                   eval      io7liv=vidliv
101000130916     c                   eval      io7ver=d00ver
101100130916     c                   eval      io7nar=vidnar
101200130916     c                   eval      io7cap=vidcap
101300130917     c                   eval      io7loc=vidloc
101400130916     c                   eval      io7lna=f_lol
101500130916     c                   eval      io7lns=%dec(vidlna:3:0)
101600130916     c                   eval      io7los=%dec(vidlos:3:0)
101700130916     c                   eval      io7lol=%dec(vidlol:3:0)
101800131016     c*****              movel     tisio7ds      kpjbu
101900130916     c                   call      'TISIO7R'
102000130916     c                   parm                    kpjba
102100131016     c                   parm                    tisio7ds
102200130916     c                   if        oo7err='1'
102300130916     c                   movel     oo7msg        DSmsms
102400130916     c                   eval      *in67  = *on
102500130916     c                   eval      ErrL00 = *on
102600130917     c                   eval      f07lna='S'
102700130916e   4c                   endif
102800130916     c                   eval      s_vidlol=F_lol
102900130916    3c                   endif
103000130916     c                   endif
103100970604     C*--------------------
103200970912     C* ZONA DI CONSEGNA OLTRE
103300970604     C*--------------------
103400970604IF  1C     ERRL00        IFEQ      '0'
103500971126IF  2C     VIDLOL        IFNE      *ZEROS
103600970604     C                   MOVEL     VIDLOL        WLIN
103700970604     C                   Z-ADD     VIDZOL        WZNC
103800970604     C                   EXSR      CHKZNC
103900970604IF  3C     ERRL00        IFEQ      '1'
104000970604     C                   SETON                                        68
104100970604     C                   MOVEL     MSG(11)       DSMSMS
104200970604E   3C                   ENDIF
104300060206if  3c                   if        §03nuc = 'S'
104400060206     c                   movel     Msg(17)       DSmsms
104500060206     c                   eval      *in68  = *on
104600060206     c                   eval      ErrL00 = *on
104700060206e   3c                   endif
104800970604E   2C                   ENDIF
104900970604E   1C                   ENDIF
105000970604     C*--------------------
105100970604     C* LIMITE PESO/VOLUME
105200970604     C*--------------------
105300970604IF  1C     ERRL00        IFEQ      '0'
105400970604IF  2C     VIDLKG        IFGT      *ZEROS
105500970604     C     VIDLMC        ORGT      *ZEROS
105600970604IF  3C     VIDLOL        IFEQ      *BLANKS
105700970604     C     VIDLOL        OREQ      *ZEROS
105800970604     C                   SETON                                        67
105900970604     C                   MOVEL     MSG(18)       DSMSMS
106000970604     C                   MOVEL     '1'           ERRL00
106100970604E   3C                   ENDIF
106200070103E   2C                   ENDIF
106300070103IF  3C     VIDLOL        IFNE      *BLANKS
106400070103     C     VIDLOL        ANDNE     *ZEROS
106500070103IF  4C                   IF        VIDLKG = *ZEROS OR
106600070103     C                             VIDLMC = *ZEROS
106700970604     C                   SETON                                        7172
106800970604     C                   MOVEL     MSG(19)       DSMSMS
106900970604     C                   MOVEL     '1'           ERRL00
107000070103E   4C                   ENDIF
107100070103E   3C                   ENDIF
107200970604E   1C                   ENDIF
107300041008     C*
107400041008IF  1C     ERRL00        IFEQ      '0'
107500041008IF  2C                   IF        (VIDLKG <> *zeros AND VIDLMC  = *zeros) OR
107600041008     C                             (VIDLKG  = *zeros AND VIDLMC <> *zeros)
107700041008     C                   SETON                                        7172
107800041008     C                   MOVEL     MSG(33)       DSMSMS
107900041008     C                   MOVEL     '1'           ERRL00
108000041008E   2C                   ENDIF
108100041008E   1C                   ENDIF
108200970604     C*--------------------
108300970912     C* PESO OLTRE + PESO SOTTO
108400970604     C*--------------------
108500970912     C* OLTRE > SOTTO
108600970604IF  1C     ERRL00        IFEQ      '0'
108700970604IF  2C     VIDLKG        IFGT      *ZEROS
108800970604     C     VIDLKS        ANDGT     *ZEROS
108900970604IF  3C     VIDLKG        IFLE      VIDLKS
109000970604     C                   SETON                                        7183
109100970604     C                   MOVEL     MSG(24)       DSMSMS
109200970604     C                   MOVEL     '1'           ERRL00
109300970604E   3C                   ENDIF
109400970604E   2C                   ENDIF
109500970604E   1C                   ENDIF
109600970604     C*--------------------
109700970912     C* VOLUME OLTRE + VOLUME SOTTO
109800970604     C*--------------------
109900970912     C* OLTRE > SOTTO
110000970604IF  1C     ERRL00        IFEQ      '0'
110100970604IF  2C     VIDLMC        IFGT      *ZEROS
110200970604     C     VIDLMS        ANDGT     *ZEROS
110300970604IF  3C     VIDLMC        IFLE      VIDLMS
110400970604     C                   SETON                                        7284
110500970604     C                   MOVEL     MSG(24)       DSMSMS
110600970604     C                   MOVEL     '1'           ERRL00
110700970604E   3C                   ENDIF
110800970604E   2C                   ENDIF
110900970604E   1C                   ENDIF
111000971118     C*--------------------
111100971118     C* LINEA ARRIVO OLTRE 2
111200971118     C*--------------------
111300971118IF  1C     ERRL00        IFEQ      '0'
111400971118     C                   MOVEL     *BLANKS       VDDLO2
111500971118IF  2C     VIDLO2        IFNE      *ZEROS
111600971118     C     VIDLO2        ANDNE     *BLANKS
111700971118     C                   MOVEL     VIDLO2        WLIN
111800971118     C                   MOVEL     *BLANKS       WDLIN
111900971118     C                   EXSR      CHKLIN
112000971118     C                   MOVEL     WLIN          VIDLO2
112100971118     C                   MOVEL     WDLIN         VDDLO2
112200971118IF  3C     ERRL00        IFEQ      '1'
112300971118     C                   SETON                                        86
112400971118     C                   MOVEL     MSG(10)       DSMSMS
112500971118E   3C                   ENDIF
112600971118E   2C                   ENDIF
112700971118E   1C                   ENDIF
112800971118     C*--------------------
112900971118     C* ZONA DI CONSEGNA OLTRE 2
113000971118     C*--------------------
113100971118IF  1C     ERRL00        IFEQ      '0'
113200971126IF  2C     VIDLO2        IFNE      *ZEROS
113300971118     C                   MOVEL     VIDLO2        WLIN
113400971118     C                   Z-ADD     VIDZO2        WZNC
113500971118     C                   EXSR      CHKZNC
113600971118IF  3C     ERRL00        IFEQ      '1'
113700971118     C                   SETON                                        87
113800971118     C                   MOVEL     MSG(11)       DSMSMS
113900971118E   3C                   ENDIF
114000060206if  3c                   if        §03nuc = 'S'
114100060206     c                   movel     Msg(17)       DSmsms
114200060206     c                   eval      *in87  = *on
114300060206     c                   eval      ErrL00 = *on
114400060206e   3c                   endif
114500971118E   2C                   ENDIF
114600971118E   1C                   ENDIF
114700971118     C*--------------------
114800971126     C* LIMITE PESO/VOLUME OLTRE 2
114900971118     C*--------------------
115000971126IF  1C*---       ERRL00    IFEQ '0'
115100971126IF  2C*---       VIDLK2    IFGT *ZEROS
115200971126     C*---       VIDLM2    ORGT *ZEROS
115300971126IF  3C*---       VIDLO2    IFEQ *BLANKS
115400971126     C*---       VIDLO2    OREQ *ZEROS
115500971126     C*---                 SETON                     86
115600971126     C*---                 MOVELMSG,18    DSMSMS
115700971126     C*---                 MOVEL'1'       ERRL00
115800971126E   3C*---                 ENDIF
115900971126X   2C*---                 ELSE
116000971126IF  3C*---       VIDLO2    IFNE *BLANKS
116100971126     C*---       VIDLO2    ANDNE*ZEROS
116200971126     C*---                 SETON                     8889
116300971126     C*---                 MOVELMSG,19    DSMSMS
116400971126     C*---                 MOVEL'1'       ERRL00
116500971126E   3C*---                 ENDIF
116600971126E   2C*---                 ENDIF
116700971126E   1C*---                 ENDIF
116800970530     C*--------------------
116900970530     C* LINEA ARRIVO FERMO DEPOSITO -OUTPUT-
117000970530     C*--------------------
117100970530IF  1C     ERRL00        IFEQ      '0'
117200971219     C                   MOVEL     VIDLNA        VIDLFD
117300971219     C                   MOVEL     *BLANKS       VDDLFD
117400970530IF  2C     VIDLFD        IFGT      *ZEROS
117500970530     C                   MOVEL     VIDLFD        WLIN
117600970530     C                   MOVEL     *BLANKS       WDLIN
117700970530     C                   EXSR      CHKLIN
117800970530     C                   MOVEL     WLIN          VIDLFD
117900970530     C                   MOVEL     WDLIN         VDDLFD
118000970530E   2C                   ENDIF
118100970530E   1C                   ENDIF
118200970530     C*--------------------
118300970530     C* ZONA DI CONSEGNA FERMO DEPOSITO
118400970530     C*--------------------
118500970530IF  1C     ERRL00        IFEQ      '0'
118600971126IF  2C     VIDLFD        IFNE      *ZEROS
118700970530     C                   MOVEL     VIDLFD        WLIN
118800970530     C                   Z-ADD     VIDZFD        WZNC
118900970530     C                   EXSR      CHKZNC
119000970530IF  3C     ERRL00        IFEQ      '1'
119100970530     C                   SETON                                        66
119200970530     C                   MOVEL     MSG(11)       DSMSMS
119300970530E   3C                   ENDIF
119400060206if  3c                   if        §03nuc = 'S'
119500060206     c                   movel     Msg(17)       DSmsms
119600060206     c                   eval      *in66  = *on
119700060206     c                   eval      ErrL00 = *on
119800060206e   3c                   endif
119900970530E   2C                   ENDIF
120000970530E   1C                   ENDIF
120100170511     C*--------------------
120200170511     C* ZONE DI CONSEGNA VS LINEE DI CONSEGNA
120300170511     C*--------------------
120400170511IF  1C     ERRL00        IFEQ      '0'
120500170511     C                   IF        VIDLNA = *zeros AND
120600170511     C                             VIDZNC > *zeros
120700170511     C                   Z-ADD     *zeros        VIDZOS
120800170511     C                   ENDIF
120900170511     C                   IF        VIDLOL = *zeros AND
121000170511     C                             VIDZOL > *zeros
121100170511     C                   Z-ADD     *zeros        VIDZOL
121200170511     C                   ENDIF
121300170511     C                   IF        VIDLOS = *zeros AND
121400170511     C                             VIDZOS > *zeros
121500170511     C                   Z-ADD     *zeros        VIDZOS
121600170511     C                   ENDIF
121700170511     C                   IF        VIDLO2 = *zeros AND
121800170511     C                             VIDZO2 > *zeros
121900170511     C                   Z-ADD     *zeros        VIDZO2
122000170511     C                   ENDIF
122100170511     C                   ENDIF
122200970530     C*--------------------
122300970530     C* CODICE TASSAZIONE
122400970530     C*--------------------
122500970530IF  1C     ERRL00        IFEQ      '0'
122600970530     C                   MOVEL     *BLANKS       VDDCTS
122700970530     C                   MOVEL     VIDCTS        WCTS
122800970530     C                   MOVEL     *BLANKS       WDCTS
122900970530     C                   EXSR      CHKCTS
123000970530     C                   MOVEL     WCTS          VIDCTS
123100970530     C                   MOVEL     WDCTS         VDDCTS
123200970530IF  2C     ERRL00        IFEQ      '1'
123300970530     C                   SETON                                        58
123400970530     C                   MOVEL     MSG(14)       DSMSMS
123500970530E   2C                   ENDIF
123600970530E   1C                   ENDIF
123700970530     C*--------------------
123800970530     C* CODICE TASSAZIONE ITALIA PER ESTERO
123900970530     C*--------------------
124000970530IF  1C     ERRL00        IFEQ      '0'
124100970530     C                   MOVEL     *BLANKS       VDDCTE
124200970530IF  2C     VIDNAR        IFEQ      *BLANKS                                      *NAZIONE ITALIA
124300970530     C                   MOVEL     VIDCTE        WA1D              1
124400970530     C                   MOVE      VIDCTE        WA1S              1
124500970530IF  3C     WA1D          IFNE      'I'
124600970530     C     WA1S          OREQ      *BLANKS
124700970530     C                   MOVEL     '1'           ERRL00
124800970530     C                   SETON                                        59
124900970530     C                   MOVEL     MSG(15)       DSMSMS
125000970530E   3C                   ENDIF
125100970530X   2C                   ELSE                                                   *NAZIONE ESTERO
125200970530IF  3C     VIDCTE        IFNE      *BLANKS
125300970530     C                   MOVEL     '1'           ERRL00
125400970530     C                   SETON                                        59
125500970530     C                   MOVEL     MSG(16)       DSMSMS
125600970530E   3C                   ENDIF
125700970530E   2C                   ENDIF
125800970530E   1C                   ENDIF
125900970530     C*--------------------
126000970530     C* INOLTRO
126100970530     C*--------------------
126200970530IF  1C     ERRL00        IFEQ      '0'
126300970530     C                   MOVEL     *BLANKS       VDDISO
126400970530     C                   MOVEL     VIDISO        WINL
126500970530     C                   MOVEL     *BLANKS       WDINL
126600970530     C                   EXSR      CHKINL
126700970530     C                   MOVEL     WINL          VIDISO
126800970530     C                   MOVEL     WDINL         VDDISO
126900970530IF  2C     ERRL00        IFEQ      '1'
127000970530     C                   SETON                                        75
127100970530     C                   MOVEL     MSG(28)       DSMSMS
127200970530E   2C                   ENDIF
127300970530E   1C                   ENDIF
127400970530     C*--------------------
127500970530     C* POSSIBILE INOLTRO ISOLA
127600970530     C*--------------------
127700970530IF  1C     ERRL00        IFEQ      '0'
127800970530IF  2C     VIDPII        IFEQ      'S'
127900970530     C     VIDPII        OREQ      'N'
128000970530X   2C                   ELSE
128100970530     C                   MOVEL     '1'           ERRL00
128200970530     C                   SETON                                        76
128300970530     C                   MOVEL     MSG(27)       DSMSMS
128400970530E   2C                   ENDIF
128500970530E   1C                   ENDIF
128600970530     C*--------------------
128700970530     C* POSSIBILE INOLTRO LOCALITA' DISAGIATA
128800970530     C*--------------------
128900970530IF  1C     ERRL00        IFEQ      '0'
129000970530IF  2C     VIDPID        IFEQ      'S'
129100970530     C     VIDPID        OREQ      'N'
129200970530X   2C                   ELSE
129300970530     C                   MOVEL     '1'           ERRL00
129400970530     C                   SETON                                        77
129500970530     C                   MOVEL     MSG(27)       DSMSMS
129600970530E   2C                   ENDIF
129700970530E   1C                   ENDIF
129800970530     C*--------------------
129900970530     C* POSSIBILE INOLTRO CENTRO STORICO
130000970530     C*--------------------
130100970530IF  1C     ERRL00        IFEQ      '0'
130200970530IF  2C     VIDPIS        IFEQ      'S'
130300970530     C     VIDPIS        OREQ      'N'
130400970530X   2C                   ELSE
130500970530     C                   MOVEL     '1'           ERRL00
130600970530     C                   SETON                                        78
130700970530     C                   MOVEL     MSG(27)       DSMSMS
130800970530E   2C                   ENDIF
130900970530E   1C                   ENDIF
131000970530     C*--------------------
131100970530     C* GESTIONE DDT
131200970530     C*--------------------
131300970530IF  1C     ERRL00        IFEQ      '0'
131400970530IF  2C     VIDGDD        IFEQ      'S'
131500970530     C     VIDGDD        OREQ      'N'
131600970530X   2C                   ELSE
131700970530     C                   MOVEL     '1'           ERRL00
131800970530     C                   SETON                                        79
131900970530     C                   MOVEL     MSG(27)       DSMSMS
132000970530E   2C                   ENDIF
132100970530E   1C                   ENDIF
132200970530     C*--------------------
132300970530     C* TIPO TEMPO CONSEGNA
132400970530     C*--------------------
132500970530IF  1C     ERRL00        IFEQ      '0'
132600010322     C                   MOVEL     VIDTTC        WTTC                           *CODICE TEMPO
132700970530     C                   Z-ADD     *ZEROS        WD0I                           *TEMPI DISTRIBUZIONE
132800970530     C                   Z-ADD     *ZEROS        WD1I
132900970530     C                   Z-ADD     *ZEROS        WD0C
133000970530     C                   Z-ADD     *ZEROS        WD1C
133100970530     C                   Z-ADD     *ZEROS        WDIS
133200970530     C                   Z-ADD     *ZEROS        WDCS
133300970530     C                   Z-ADD     *ZEROS        WDLD
133400090903
133500970530     C                   Z-ADD     *ZEROS        WC0I                           *TEMPI CORRIERE
133600970530     C                   Z-ADD     *ZEROS        WC1I
133700970530     C                   Z-ADD     *ZEROS        WC0C
133800970530     C                   Z-ADD     *ZEROS        WC1C
133900970530     C                   Z-ADD     *ZEROS        WCIS
134000970530     C                   Z-ADD     *ZEROS        WCCS
134100970530     C                   Z-ADD     *ZEROS        WCLD
134200090903
134300970530     C                   Z-ADD     *ZEROS        WE0I                           *TEMPI ESPRESSO
134400970530     C                   Z-ADD     *ZEROS        WE1I
134500970530     C                   Z-ADD     *ZEROS        WE0C
134600970530     C                   Z-ADD     *ZEROS        WE1C
134700970530     C                   Z-ADD     *ZEROS        WEIS
134800970530     C                   Z-ADD     *ZEROS        WECS
134900970530     C                   Z-ADD     *ZEROS        WELD
135000090903
135100090903     C                   Z-ADD     *ZEROS        Wh0I                           *TEMPI H1030
135200090903     C                   Z-ADD     *ZEROS        Wh1I
135300090903     C                   Z-ADD     *ZEROS        Wh0C
135400090903     C                   Z-ADD     *ZEROS        Wh1C
135500090903     C                   Z-ADD     *ZEROS        WhIS
135600090903     C                   Z-ADD     *ZEROS        WhCS
135700090903     C                   Z-ADD     *ZEROS        WhLD
135800090903
135900970530     C                   EXSR      CHKTTC
136000970530     C                   MOVEL     WTTC          VIDTTC                         *CODICE TEMPO
136100970530     C                   Z-ADD     WD0I          VIDD0I                         *TEMPI DISTRIBUZIONE
136200970530     C                   Z-ADD     WD1I          VIDD1I
136300970530     C                   Z-ADD     WD0C          VIDD0C
136400970530     C                   Z-ADD     WD1C          VIDD1C
136500970530     C                   Z-ADD     WDIS          VIDDIS
136600970530     C                   Z-ADD     WDCS          VIDDCS
136700970530     C                   Z-ADD     WDLD          VIDDLD
136800090903
136900970530     C                   Z-ADD     WC0I          VIDC0I                         *TEMPI CORRIERE
137000970530     C                   Z-ADD     WC1I          VIDC1I
137100970530     C                   Z-ADD     WC0C          VIDC0C
137200970530     C                   Z-ADD     WC1C          VIDC1C
137300970530     C                   Z-ADD     WCIS          VIDCIS
137400970530     C                   Z-ADD     WCCS          VIDCCS
137500970530     C                   Z-ADD     WCLD          VIDCLD
137600090903
137700970530     C                   Z-ADD     WE0I          VIDE0I                         *TEMPI ESPRESSO
137800970530     C                   Z-ADD     WE1I          VIDE1I
137900970530     C                   Z-ADD     WE0C          VIDE0C
138000970530     C                   Z-ADD     WE1C          VIDE1C
138100970530     C                   Z-ADD     WEIS          VIDEIS
138200970530     C                   Z-ADD     WECS          VIDECS
138300970530     C                   Z-ADD     WELD          VIDELD
138400090903
138500090903     C                   Z-ADD     Wh0I          VIDh0I                         *TEMPI H1030
138600090903     C                   Z-ADD     Wh1I          VIDh1I
138700090903     C                   Z-ADD     Wh0C          VIDh0C
138800090903     C                   Z-ADD     Wh1C          VIDh1C
138900090903     C                   Z-ADD     WhIS          VIDhIS
139000090903     C                   Z-ADD     WhCS          VIDhCS
139100090903     C                   Z-ADD     WhLD          VIDhLD
139200970530IF  2C     ERRL00        IFEQ      '1'
139300970530     C                   SETON                                        80
139400970530     C                   MOVEL     MSG(30)       DSMSMS
139500970530E   2C                   ENDIF
139600970530E   1C                   ENDIF
139700010322     C*--------------------
139800010322     C* TIPO TEMPO RITIRO
139900010322     C*--------------------
140000010322IF  1C     ERRL00        IFEQ      '0'
140100010322     C                   MOVEL     VIDTTR        WTTR                           *CODICE TEMPO
140200010322     C                   Z-ADD     *ZEROS        WD0IR                          *TEMPI DISTRIBUZIONE
140300010322     C                   Z-ADD     *ZEROS        WD1IR
140400010322     C                   Z-ADD     *ZEROS        WD0CR
140500010322     C                   Z-ADD     *ZEROS        WD1CR
140600010322     C                   Z-ADD     *ZEROS        WDISR
140700010322     C                   Z-ADD     *ZEROS        WDCSR
140800010322     C                   Z-ADD     *ZEROS        WDLDR
140900010322     C                   Z-ADD     *ZEROS        WC0IR                          *TEMPI CORRIERE
141000010322     C                   Z-ADD     *ZEROS        WC1IR
141100010322     C                   Z-ADD     *ZEROS        WC0CR
141200010322     C                   Z-ADD     *ZEROS        WC1CR
141300010322     C                   Z-ADD     *ZEROS        WCISR
141400010322     C                   Z-ADD     *ZEROS        WCCSR
141500010322     C                   Z-ADD     *ZEROS        WCLDR
141600010322     C                   Z-ADD     *ZEROS        WE0IR                          *TEMPI ESPRESSO
141700010322     C                   Z-ADD     *ZEROS        WE1IR
141800010322     C                   Z-ADD     *ZEROS        WE0CR
141900010322     C                   Z-ADD     *ZEROS        WE1CR
142000010322     C                   Z-ADD     *ZEROS        WEISR
142100010322     C                   Z-ADD     *ZEROS        WECSR
142200010322     C                   Z-ADD     *ZEROS        WELDR
142300010322     C                   EXSR      CHKTTR
142400010322     C                   MOVEL     WTTR          VIDTTR                         *CODICE TEMPO
142500010322     C                   Z-ADD     WD0IR         VIDD0IR                        *TEMPI DISTRIBUZIONE
142600010322     C                   Z-ADD     WD1IR         VIDD1IR
142700010322     C                   Z-ADD     WD0CR         VIDD0CR
142800010322     C                   Z-ADD     WD1CR         VIDD1CR
142900010322     C                   Z-ADD     WDISR         VIDDISR
143000010322     C                   Z-ADD     WDCSR         VIDDCSR
143100010322     C                   Z-ADD     WDLDR         VIDDLDR
143200010322     C                   Z-ADD     WC0IR         VIDC0IR                        *TEMPI CORRIERE
143300010322     C                   Z-ADD     WC1IR         VIDC1IR
143400010322     C                   Z-ADD     WC0CR         VIDC0CR
143500010322     C                   Z-ADD     WC1CR         VIDC1CR
143600010322     C                   Z-ADD     WCISR         VIDCISR
143700010322     C                   Z-ADD     WCCSR         VIDCCSR
143800010322     C                   Z-ADD     WCLDR         VIDCLDR
143900010322     C                   Z-ADD     WE0IR         VIDE0IR                        *TEMPI ESPRESSO
144000010322     C                   Z-ADD     WE1IR         VIDE1IR
144100010322     C                   Z-ADD     WE0CR         VIDE0CR
144200010322     C                   Z-ADD     WE1CR         VIDE1CR
144300010322     C                   Z-ADD     WEISR         VIDEISR
144400010322     C                   Z-ADD     WECSR         VIDECSR
144500010322     C                   Z-ADD     WELDR         VIDELDR
144600010322IF  2C     ERRL00        IFEQ      '1'
144700010322     C                   SETON                                        44
144800010322     C                   MOVEL     MSG(32)       DSMSMS
144900010322E   2C                   ENDIF
145000010322E   1C                   ENDIF
145100130917     c*
145200130917     c                   clear                   wdel              1
145300130917     c                   exsr      sr_cpe
145400970530     C*
145500970530     C                   ENDSR
145600970618     C*------------------------------------------------------------------------*
145700970618     C* DECF06 - CONTROLLA CAMPI E DECODIFICA -SOLO ALLA CONFERMA-
145800970618     C*------------------------------------------------------------------------*
145900970618     C     DECF06        BEGSR
146000970618     C*--------------------
146100970618     C* LOCALITA' GESTITA A LIVELLO DI LOCALITA'
146200970618     C*--------------------
146300970618     C* DATI CAP + DATI LOCALITA'
146400091211IF  1C*    ERRL00        IFEQ      '0'
146500091211IF  2C*    VIDLIV        IFEQ      'L'                                          *LOCALITA' GESTITA
146600091211     C*                  MOVEL     VIDLNA        NLNA                           *CAMPI NUMERICI
146700091211     C*                  MOVEL     VIDLOS        NLOS
146800091211     C*                  MOVEL     VIDLOL        NLOL
146900091211     C*                  MOVEL     VIDLO2        NLO2
147000091211     C*                  MOVEL     VIDTTC        NTTC
147100091211     C*                  MOVE      VIDTTR        NTTR
147200091211IF  3C*    NLNA          IFEQ      CPCLNA
147300091211     C*    VIDZNC        ANDEQ     CPCZNC
147400091211     C*    VIDISO        ANDEQ     CPCISO
147500091211     C*    VIDZFD        ANDEQ     CPCZFD
147600091211     C*    VIDLKG        ANDEQ     CPCLKG
147700091211     C*    VIDLMC        ANDEQ     CPCLMC
147800091211     C*    NLOL          ANDEQ     CPCLOL
147900091211     C*    VIDZOL        ANDEQ     CPCZOL
148000091211     C*    VIDLKS        ANDEQ     CPCLKS
148100091211     C*    VIDLMS        ANDEQ     CPCLMS
148200091211     C*    NLO2          ANDEQ     CPCLO2
148300091211     C*    VIDZO2        ANDEQ     CPCZO2
148400091211     C*    VIDLK2        ANDEQ     CPCLK2
148500091211     C*    VIDLM2        ANDEQ     CPCLM2
148600091211     C*    NLOS          ANDEQ     CPCLOS
148700091211     C*    VIDZOS        ANDEQ     CPCZOS
148800091211     C*    VIDCTS        ANDEQ     CPCCTS
148900091211     C*    VIDCTE        ANDEQ     CPCCTE
149000091211     C*    NTTC          ANDEQ     CPCTTC
149100091211     C*    NTTR          ANDEQ     CPCFI2
149200091211     C*    VIDPII        ANDEQ     CPCPII
149300091211     C*    VIDPID        ANDEQ     CPCPID
149400091211     C*    VIDPIS        ANDEQ     CPCPIS
149500091211     C*    VIDGDD        ANDEQ     CPCGDD
149600091211     C*                  SETON                                        606170
149700091211     C*                  SETON                                        74
149800091211     C*                  MOVEL     MSG(29)       DSMSMS
149900091211     C*                  MOVEL     '1'           ERRL00
150000091211E   3C*                  ENDIF
150100091211E   2C*                  ENDIF
150200091211E   1C*                  ENDIF
150300970618     C*
150400970618     C* LIVELLO DI GESTIONE = CAP SE LA LOCALITA' E' UNICA
150500970618IF  1C     ERRL00        IFEQ      '0'
150600970618IF  2C     VIDLIV        IFEQ      'L'                                          *LOCALITA' GESTITA
150700970618IF  3C     DEPOP1        IFEQ      'F06'
150800970618     C                   Z-ADD     1             I
150900970618X   3C                   ELSE
151000970618     C                   Z-ADD     *ZEROS        I
151100970618E   3C                   ENDIF
151200970618     C                   Z-ADD     D00VER        KCLVER
151300970618     C                   MOVEL     VIDNAR        KCLNAR
151400970618     C                   MOVEL     VIDCAP        KCLCAP
151500970618     C     KE2CPL        SETLL     AZCPL01L
151600970618     C     KE2CPL        READE(N)  AZCPL01L                               99
151700970618DO  3C     *IN99         DOWEQ     *OFF
151800970618IF  4C     CPLATB        IFEQ      *BLANKS
151900970618     C                   ADD       1             I                              *NUMERO LOCALITA'
152000970618E   4C                   ENDIF
152100970618     C     KE2CPL        READE(N)  AZCPL01L                               99
152200970618E   3C                   ENDDO
152300970618IF  3C     I             IFEQ      1
152400970618     C                   SETON                                        606170
152500970618     C                   SETON                                        74
152600970618     C                   MOVEL     MSG(26)       DSMSMS
152700970618     C                   MOVEL     '1'           ERRL00
152800970618E   3C                   ENDIF
152900970618E   2C                   ENDIF
153000970618E   1C                   ENDIF
153100970618     C*
153200970618     C                   ENDSR
153300970530     C*------------------------------------------------------------------------*
153400970530     C* CHKNAR - CONTROLLA LA NAZIONE
153500970530     C*------------------------------------------------------------------------*
153600970530     C     CHKNAR        BEGSR
153700970530     C*
153800970530     C     *LIKE         DEFINE    VIDNAR        WNAR
153900970530     C     *LIKE         DEFINE    VDDNAR        WDNAR
154000970530     C*
154100970530     C* RICHIESTO ELENCO
154200970530     C                   SETOFF                                           28
154300970530     C     '?'           SCAN      WNAR                                   28
154400970530IF  1C     *IN28         IFEQ      *ON
154500020906     C                   Z-ADD     1             §KUT              1 0
154600970530     C                   MOVEL     '15'          §COD              2
154700970530     C                   MOVEL     *BLANKS       §KEY              8
154800970530     C                   MOVEL     *BLANKS       §DES             30
154900970530     C                   CALL      'X§TABER'
155000970530     C                   PARM                    §KUT
155100970530     C                   PARM                    §COD
155200970530     C                   PARM                    §KEY
155300970530     C                   PARM                    §DES
155400970530     C                   MOVEL     §KEY          WNAR
155500970530     C                   MOVEL     §DES          WDNAR
155600970530E   1C                   ENDIF
155700970530     C*
155800970530     C* CONTROLLA ESISTENZA CODICE
155900020906     C                   MOVEL     1             KTBKUT
156000970530     C                   MOVEL     '15'          KTBCOD
156100970530     C                   MOVEL(P)  WNAR          KTBKEY
156200970530     C     KEYTAB        CHAIN     TABEL00F                           99
156300970530IF  1C     *IN99         IFEQ      *OFF
156400970530     C     TBLFLG        ANDEQ     *BLANKS
156500970530     C                   MOVEL     TBLUNI        DS15
156600970530     C                   MOVEL     §15DES        WDNAR
156700970530     C*
156800970530     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
156900970530IF  2C     VIDNAR        IFEQ      'I  '
157000970530     C                   MOVEL     '1'           ERRL00
157100970530     C                   MOVEL     *BLANKS       WDNAR
157200970530E   2C                   ENDIF
157300970530X   1C                   ELSE
157400970530     C                   MOVEL     '1'           ERRL00
157500970530E   1C                   ENDIF
157600970530     C*
157700970530     C                   ENDSR
157800970530     C*------------------------------------------------------------------------*
157900970530     C* CHKCAP - CONTROLLA IL CAP
158000970530     C*------------------------------------------------------------------------*
158100970530     C     CHKCAP        BEGSR
158200970530     C*
158300970530     C     *LIKE         DEFINE    VIDCAP        WCAP
158400970917     C     *LIKE         DEFINE    O95CF1        WFI1
158500970626     C*
158600970626     C* CONTROLLA IL CAP
158700970626     C                   CLEAR                   DSSI95
158800970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
158900970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
159000970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
159100970626     C                   MOVEL     WCAP          I95CAP                         *CAP
159200970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
159300970626     C                   CALL      'TISI95R'
159400970626     C                   PARM                    DSSI95
159500970626IF  1C     O95ERR        IFNE      *BLANKS
159600970626     C                   MOVEL     '1'           ERRL00
159700970917X   1C                   ELSE
159800970917     C                   MOVEL     O95CF1        WFI1
159900970917E   1C                   ENDIF
160000970530     C*
160100970530     C                   ENDSR
160200970530     C*------------------------------------------------------------------------*
160300970530     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
160400970530     C*------------------------------------------------------------------------*
160500970530     C     CHKLIN        BEGSR
160600970530     C*
160700970530     C     *LIKE         DEFINE    VIDLNA        WLIN
160800970530     C     *LIKE         DEFINE    VDDLNA        WDLIN
160900970530     C*
161000970530     C* RICHIESTO ELENCO
161100970530     C                   SETOFF                                           28
161200970530     C     '?'           SCAN      WLIN                                   28
161300970530IF  1C     *IN28         IFEQ      *ON
161400970530     C                   MOVEL     *BLANKS       §COD1             3
161500970530     C                   MOVEL     *BLANKS       §TIP              1
161600970530     C                   MOVEL     *BLANKS       §DES1            25
161700971203     C                   MOVEL     *BLANKS       §PRA              3
161800971203     C                   CALL      'TNSD24R'
161900970530     C                   PARM                    §COD1
162000970530     C                   PARM                    §TIP
162100970530     C                   PARM                    §DES1
162200971203     C                   PARM                    §PRA
162300970530     C                   MOVEL     §COD1         WLIN
162400970530     C                   MOVEL     §DES1         WDLIN
162500970530E   1C                   ENDIF
162600970530     C*
162700970530     C* SOLO CARATTERI NUMERICI
162800970530     C                   SETOFF                                       28
162900970530     C                   TESTN                   WLIN                 28
163000970530IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
163100970530     C                   MOVEL     '1'           ERRL00
163200970530X   1C                   ELSE                                                   *E'NUMERO DA TESTN
163300970530     C                   MOVE      WLIN          W001              1
163400970530IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
163500970530     C                   MOVEL     '1'           ERRL00
163600970530X   2C                   ELSE                                                   *NUMERO
163700970530     C*
163800970530     C* CONTROLLA ESISTENZA CODICE
163900970530     C                   MOVEL     WLIN          KORFIL
164000970530     C     KEYORG        CHAIN     AZORG01L                           99
164100970530IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
164200970530     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
164300970530     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
164400970530     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
164500970530X   3C                   ELSE
164600970530     C                   MOVEL     '1'           ERRL00
164700970530E   3C                   ENDIF
164800970530E   2C                   ENDIF
164900970530E   1C                   ENDIF
165000970530     C*
165100970530     C                   ENDSR
165200970530     C*------------------------------------------------------------------------*
165300970530     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
165400970530     C*------------------------------------------------------------------------*
165500970530     C     CHKZNC        BEGSR
165600970530     C*
165700970530     C     *LIKE         DEFINE    VIDZNC        WZNC
165800970530     C*
165900970530     C* CONTROLLA ESISTENZA CODICE
166000020906     C                   Z-ADD     1             KTBKUT
166100970530     C                   MOVEL     '03'          KTBCOD
166200970530     C                   MOVEL     WLIN          WA5               5
166300970530     C                   MOVE      WZNC          WA5
166400970530     C                   MOVEL(P)  WA5           KTBKEY
166500970530     C     KEYTAB        CHAIN     TABEL00F                           99
166600970530IF  1C     *IN99         IFEQ      *OFF
166700970530     C     TBLFLG        ANDEQ     *BLANKS
166800970530     C                   MOVEL     TBLUNI        DS03
166900970530X   1C                   ELSE
167000970530     C                   MOVEL     '1'           ERRL00
167100970530E   1C                   ENDIF
167200970530     C*
167300970530     C                   ENDSR
167400970530     C*------------------------------------------------------------------------*
167500970530     C* CHKCTS - CONTROLLA IL CODICE TASSAZIONE
167600970530     C*------------------------------------------------------------------------*
167700970530     C     CHKCTS        BEGSR
167800970530     C*
167900970530     C     *LIKE         DEFINE    VIDCTS        WCTS
168000970530     C     *LIKE         DEFINE    VDDCTS        WDCTS
168100970530     C*
168200970530     C* RICHIESTO ELENCO
168300970530     C                   SETOFF                                           28
168400970530     C     '?'           SCAN      WCTS                                   28
168500970530IF  1C     *IN28         IFEQ      *ON
168600020906     C                   Z-ADD     1             §KUT              1 0
168700970530     C                   MOVEL     'CT'          §COD              2
168800970530     C                   MOVEL     *BLANKS       §KEY              8
168900970530     C                   MOVEL     *BLANKS       §DES             30
169000970530     C                   CALL      'X§TABER'
169100970530     C                   PARM                    §KUT
169200970530     C                   PARM                    §COD
169300970530     C                   PARM                    §KEY
169400970530     C                   PARM                    §DES
169500970530     C                   MOVEL     §KEY          WCTS
169600970530     C                   MOVEL     §DES          WDCTS
169700970530E   1C                   ENDIF
169800970530     C*
169900970530     C* CONTROLLA ESISTENZA CODICE
170000020906     C                   MOVEL     1             KTBKUT
170100970530     C                   MOVEL     'CT'          KTBCOD
170200970530     C                   MOVEL(P)  WCTS          KTBKEY
170300970530     C     KEYTAB        CHAIN     TABEL00F                           99
170400970530IF  1C     *IN99         IFEQ      *OFF
170500970530     C     TBLFLG        ANDEQ     *BLANKS
170600970530     C                   MOVEL     TBLUNI        DSCT
170700970530     C                   MOVEL     §CTDES        WDCTS
170800970530X   1C                   ELSE
170900970530     C                   MOVEL     '1'           ERRL00
171000970530E   1C                   ENDIF
171100970530     C*
171200970530     C                   ENDSR
171300970530     C*------------------------------------------------------------------------*
171400970530     C* CHKINL - CONTROLLA L'INOLTRO
171500970530     C*------------------------------------------------------------------------*
171600970530     C     CHKINL        BEGSR
171700970530     C*
171800970530     C     *LIKE         DEFINE    VIDISO        WINL
171900970530     C     *LIKE         DEFINE    VDDISO        WDINL
172000970530     C*
172100970530     C* RICHIESTO ELENCO
172200970530     C                   SETOFF                                           28
172300970530     C     '?'           SCAN      WINL                                   28
172400970530IF  1C     *IN28         IFEQ      *ON
172500020906     C                   Z-ADD     1             §KUT              1 0
172600970530     C                   MOVEL     '9A'          §COD              2
172700970530     C                   MOVEL     *BLANKS       §KEY              8
172800970530     C                   MOVEL     *BLANKS       §DES             30
172900970530     C                   CALL      'X§TABER'
173000970530     C                   PARM                    §KUT
173100970530     C                   PARM                    §COD
173200970530     C                   PARM                    §KEY
173300970530     C                   PARM                    §DES
173400970530     C                   MOVEL     §KEY          WINL
173500970530     C                   MOVEL     §DES          WDINL
173600970530E   1C                   ENDIF
173700970530     C*
173800970530     C* CONTROLLA ESISTENZA CODICE
173900020906     C                   MOVEL     1             KTBKUT
174000970530     C                   MOVEL     '9A'          KTBCOD
174100970530     C                   MOVEL(P)  WINL          KTBKEY
174200970530     C     KEYTAB        CHAIN     TABEL00F                           99
174300970530IF  1C     *IN99         IFEQ      *OFF
174400970530     C     TBLFLG        ANDEQ     *BLANKS
174500970530     C                   MOVEL     TBLUNI        DS9A
174600970530     C                   MOVEL     §9ADES        WDINL
174700970530X   1C                   ELSE
174800970530     C                   MOVEL     '1'           ERRL00
174900970530E   1C                   ENDIF
175000110623     c* Se non utilizzabile  nel cappario errore
175100110623     c                   if        §9auticpc='N'  and errl00='0'
175200110623     C                   MOVEL     '1'           ERRL00
175300110623E   1C                   ENDIF
175400970530     C*
175500970530     C                   ENDSR
175600970530     C*------------------------------------------------------------------------*
175700970530     C* CHKTTC - CONTROLLA IL TIPO TEMPO CONSEGNA
175800970530     C*------------------------------------------------------------------------*
175900970530     C     CHKTTC        BEGSR
176000970530     C*
176100970530     C     *LIKE         DEFINE    VIDTTC        WTTC                           *TIPO TEMPO
176200970530     C     *LIKE         DEFINE    CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
176300970530     C     *LIKE         DEFINE    CPT1IN        WD1I
176400970530     C     *LIKE         DEFINE    CPT0CN        WD0C
176500970530     C     *LIKE         DEFINE    CPT1CN        WD1C
176600970530     C     *LIKE         DEFINE    CPTTIS        WDIS
176700970530     C     *LIKE         DEFINE    CPTTCS        WDCS
176800970530     C     *LIKE         DEFINE    CPTTLD        WDLD
176900970530     C     *LIKE         DEFINE    CPT0IN        WC0I                           *TEMPI CORRIERE
177000970530     C     *LIKE         DEFINE    CPT1IN        WC1I
177100970530     C     *LIKE         DEFINE    CPT0CN        WC0C
177200970530     C     *LIKE         DEFINE    CPT1CN        WC1C
177300970530     C     *LIKE         DEFINE    CPTTIS        WCIS
177400970530     C     *LIKE         DEFINE    CPTTCS        WCCS
177500970530     C     *LIKE         DEFINE    CPTTLD        WCLD
177600090903
177700970530     C     *LIKE         DEFINE    CPT0IN        WE0I                           *TEMPI ESPRESSO
177800970530     C     *LIKE         DEFINE    CPT1IN        WE1I
177900970530     C     *LIKE         DEFINE    CPT0CN        WE0C
178000970530     C     *LIKE         DEFINE    CPT1CN        WE1C
178100970530     C     *LIKE         DEFINE    CPTTIS        WEIS
178200970530     C     *LIKE         DEFINE    CPTTCS        WECS
178300970530     C     *LIKE         DEFINE    CPTTLD        WELD
178400090903
178500090903     C     *LIKE         DEFINE    CPT0IN        Wh0I                           *TEMPI H1030
178600090903     C     *LIKE         DEFINE    CPT1IN        Wh1I
178700090903     C     *LIKE         DEFINE    CPT0CN        Wh0C
178800090903     C     *LIKE         DEFINE    CPT1CN        Wh1C
178900090903     C     *LIKE         DEFINE    CPTTIS        WhIS
179000090903     C     *LIKE         DEFINE    CPTTCS        WhCS
179100090903     C     *LIKE         DEFINE    CPTTLD        WhLD
179200970530     C*
179300970530     C* RICHIESTA ELENCO CODICI
179400970530     C                   SETOFF                                           28
179500970530     C     '?'           SCAN      WTTC                                   28
179600970530IF  1C     *IN28         IFEQ      *ON
179700160728     C*                  MOVEL     *BLANKS       DNAR
179800160728     C*                  MOVEL     *BLANKS       DCAP
179900160728     C*                  MOVEL     *BLANKS       DLOC
180000970602     C                   MOVEL     'T01'         WOP0
180100970530     C                   MOVEL     *BLANKS       WOP1
180200970530     C                   MOVEL     'TISI41R'     WPGM              8
180300970530     C                   EXSR      LANPGM
180400970530     C                   MOVEL     D00TTC        WTTC
180500970530E   1C                   ENDIF
180600970530     C*
180700010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO CONSEGNA
180800160727     C                   MOVEL     WTTC          DEP3A             3
180900010322     C     DEP3A         IFNE      '999'
181000970530     C* CONTROLLA ESISTENZA CODICI
181100970530     C                   Z-ADD     D00VER        KCTVER
181200970530     C                   MOVEL     WTTC          KCTTTC
181300970530     C                   MOVEL     'D'           KCTTSP
181400970530     C     KEYCPT        CHAIN     AZCPT01L                           99
181500970530IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
181600970530     C                   Z-ADD     CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
181700970530     C                   Z-ADD     CPT1IN        WD1I
181800970530     C                   Z-ADD     CPT0CN        WD0C
181900970530     C                   Z-ADD     CPT1CN        WD1C
182000970530     C                   Z-ADD     CPTTIS        WDIS
182100970530     C                   Z-ADD     CPTTCS        WDCS
182200970530     C                   Z-ADD     CPTTLD        WDLD
182300970530     C                   MOVEL     'C'           KCTTSP
182400970530     C     KEYCPT        CHAIN     AZCPT01L                           99
182500970530IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
182600970530     C                   Z-ADD     CPT0IN        WC0I                           *TEMPI CORRIERE
182700970530     C                   Z-ADD     CPT1IN        WC1I
182800970530     C                   Z-ADD     CPT0CN        WC0C
182900970530     C                   Z-ADD     CPT1CN        WC1C
183000970530     C                   Z-ADD     CPTTIS        WCIS
183100970530     C                   Z-ADD     CPTTCS        WCCS
183200970530     C                   Z-ADD     CPTTLD        WCLD
183300970530     C                   MOVEL     'E'           KCTTSP
183400970530     C     KEYCPT        CHAIN     AZCPT01L                           99
183500970530IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
183600970530     C                   Z-ADD     CPT0IN        WE0I                           *TEMPI ESPRESSO
183700970530     C                   Z-ADD     CPT1IN        WE1I
183800970530     C                   Z-ADD     CPT0CN        WE0C
183900970530     C                   Z-ADD     CPT1CN        WE1C
184000970530     C                   Z-ADD     CPTTIS        WEIS
184100970530     C                   Z-ADD     CPTTCS        WECS
184200970530     C                   Z-ADD     CPTTLD        WELD
184300090903     C                   MOVEL     'H'           KCTTSP
184400090903     C     KEYCPT        CHAIN     AZCPT01L                           99
184500090903IF  4C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
184600090903     C                   Z-ADD     CPT0IN        Wh0I                           *TEMPI H1030
184700090903     C                   Z-ADD     CPT1IN        Wh1I
184800090903     C                   Z-ADD     CPT0CN        Wh0C
184900090903     C                   Z-ADD     CPT1CN        Wh1C
185000090903     C                   Z-ADD     CPTTIS        WhIS
185100090903     C                   Z-ADD     CPTTCS        WhCS
185200090903     C                   Z-ADD     CPTTLD        WhLD
185300090903X   4C                   ELSE
185400090903
185500090903     c* Se tipo servizio H non ancora decorrente (anche guardando alla
185600090903     c*    scadenza della versione in visualizzazione)non do errore e non
185700090903     c*    visualizzo nemmeno come descrizione
185800090903
185900170116     c**                 if        Datasys>=dataiso
186000090903     c* Verifico anche la  scadenza ddella versione
186100170116     c**   d00ver        chain     azcpr01l
186200170116     c**                 if        not %found(azcpr01l) or cprdsc>=§vpotsph
186300170116
186400090903     C                   MOVEL     '1'           ERRL00
186500170116
186600170116     c**                 endif
186700170116     c**                 endif
186800090903     c
186900170116     c**                 if        errl00='0'
187000170116     c**                 clear                   viddtsph
187100170116E   4C**                 ENDIF
187200090903
187300090903E   4C                   ENDIF
187400090903
187500970603X   3C                   ELSE
187600970603     C                   MOVEL     '1'           ERRL00
187700970603E   3C                   ENDIF
187800090903
187900970603X   2C                   ELSE
188000970603     C                   MOVEL     '1'           ERRL00
188100970603E   2C                   ENDIF
188200090903
188300970603X   1C                   ELSE
188400970603     C                   MOVEL     '1'           ERRL00
188500970603E   1C                   ENDIF
188600090903
188700010322     C                   ELSE
188800010322     C                   MOVEL     '1'           ERRL00
188900010322     C                   ENDIF
189000970530     C*
189100970530     C                   ENDSR
189200010322     C*------------------------------------------------------------------------*
189300010322     C* CHKTTR - CONTROLLA IL TIPO TEMPO RITIRO
189400010322     C*------------------------------------------------------------------------*
189500010322     C     CHKTTR        BEGSR
189600010322     C*
189700010322     C     *LIKE         DEFINE    VIDTTR        WTTR                           *TIPO TEMPO
189800010322     C     *LIKE         DEFINE    CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
189900010322     C     *LIKE         DEFINE    CPT1IN        WD1IR
190000010322     C     *LIKE         DEFINE    CPT0CN        WD0CR
190100010322     C     *LIKE         DEFINE    CPT1CN        WD1CR
190200010322     C     *LIKE         DEFINE    CPTTIS        WDISR
190300010322     C     *LIKE         DEFINE    CPTTCS        WDCSR
190400010322     C     *LIKE         DEFINE    CPTTLD        WDLDR
190500010322     C     *LIKE         DEFINE    CPT0IN        WC0IR                          *TEMPI CORRIERE
190600010322     C     *LIKE         DEFINE    CPT1IN        WC1IR
190700010322     C     *LIKE         DEFINE    CPT0CN        WC0CR
190800010322     C     *LIKE         DEFINE    CPT1CN        WC1CR
190900010322     C     *LIKE         DEFINE    CPTTIS        WCISR
191000010322     C     *LIKE         DEFINE    CPTTCS        WCCSR
191100010322     C     *LIKE         DEFINE    CPTTLD        WCLDR
191200010322     C     *LIKE         DEFINE    CPT0IN        WE0IR                          *TEMPI ESPRESSO
191300010322     C     *LIKE         DEFINE    CPT1IN        WE1IR
191400010322     C     *LIKE         DEFINE    CPT0CN        WE0CR
191500010322     C     *LIKE         DEFINE    CPT1CN        WE1CR
191600010322     C     *LIKE         DEFINE    CPTTIS        WEISR
191700010322     C     *LIKE         DEFINE    CPTTCS        WECSR
191800010322     C     *LIKE         DEFINE    CPTTLD        WELDR
191900010322     C*
192000010322     C* RICHIESTA ELENCO CODICI
192100010322     C                   SETOFF                                           28
192200010322     C     '?'           SCAN      WTTR                                   28
192300010322IF  1C     *IN28         IFEQ      *ON
192400160728     C*                  MOVEL     *BLANK        DNAR
192500160728     C*                  MOVEL     *BLANKS       DCAP
192600010322     C                   MOVEL     'T01'         WOP0
192700010322     C                   MOVEL     *BLANKS       WOP1
192800010323     C                   MOVEL     'TISI41R'     WPGM              8
192900010322     C                   EXSR      LANPGM
193000010322     C                   MOVEL     D00TTC        WTTR
193100010322E   1C                   ENDIF
193200010322     C*
193300010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO RITIRO
193400160728     C                   MOVEL     wttr          DEP3A             3
193500010322     C     DEP3A         IFEQ      '999'
193600010322     C* CONTROLLA ESISTENZA CODICI
193700010322     C                   Z-ADD     D00VER        KCTVER
193800010322     C                   MOVEL     WTTR          KCTTTC
193900010322     C                   MOVEL     'D'           KCTTSP
194000010322     C     KEYCPT        CHAIN     AZCPT01L                           99
194100010322IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
194200010322     C                   Z-ADD     CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
194300010322     C                   Z-ADD     CPT1IN        WD1IR
194400010322     C                   Z-ADD     CPT0CN        WD0CR
194500010322     C                   Z-ADD     CPT1CN        WD1CR
194600010322     C                   Z-ADD     CPTTIS        WDISR
194700010322     C                   Z-ADD     CPTTCS        WDCSR
194800010322     C                   Z-ADD     CPTTLD        WDLDR
194900010322     C                   MOVEL     'C'           KCTTSP
195000010322     C     KEYCPT        CHAIN     AZCPT01L                           99
195100010322IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
195200010322     C                   Z-ADD     CPT0IN        WC0IR                          *TEMPI CORRIERE
195300010322     C                   Z-ADD     CPT1IN        WC1IR
195400010322     C                   Z-ADD     CPT0CN        WC0CR
195500010322     C                   Z-ADD     CPT1CN        WC1CR
195600010322     C                   Z-ADD     CPTTIS        WCISR
195700010322     C                   Z-ADD     CPTTCS        WCCSR
195800010322     C                   Z-ADD     CPTTLD        WCLDR
195900010322     C                   MOVEL     'E'           KCTTSP
196000010322     C     KEYCPT        CHAIN     AZCPT01L                           99
196100010322IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
196200010322     C                   Z-ADD     CPT0IN        WE0IR                          *TEMPI ESPRESSO
196300010322     C                   Z-ADD     CPT1IN        WE1IR
196400010322     C                   Z-ADD     CPT0CN        WE0CR
196500010322     C                   Z-ADD     CPT1CN        WE1CR
196600010322     C                   Z-ADD     CPTTIS        WEISR
196700010322     C                   Z-ADD     CPTTCS        WECSR
196800010322     C                   Z-ADD     CPTTLD        WELDR
196900010322X   3C                   ELSE
197000010322     C                   MOVEL     '1'           ERRL00
197100010322E   3C                   ENDIF
197200010322X   2C                   ELSE
197300010322     C                   MOVEL     '1'           ERRL00
197400010322E   2C                   ENDIF
197500010322X   1C                   ELSE
197600010322     C                   MOVEL     '1'           ERRL00
197700010322E   1C                   ENDIF
197800010322     C                   ELSE
197900010322     C                   MOVEL     '1'           ERRL00
198000010322     C                   ENDIF
198100010322     C*
198200010322     C                   ENDSR
198300970526     C*------------------------------------------------------------------------*
198400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
198500970526     C*------------------------------------------------------------------------*
198600970526     C     IREVID        BEGSR
198700970526     C* CAMPI
198800970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
198900970530IF  1C     CPLESS        IFEQ      'S'
199000970530     C                   MOVEL     'SIN'         VDDESS
199100970530E   1C                   ENDIF
199200970530     C                   MOVEL     CPLNAR        VIDNAR
199300970530     C                   MOVEL     CPLCAP        VIDCAP
199400970530     C                   MOVEL     CPLLOC        VIDLOC
199500970530     C                   MOVEL     CPLLIV        VIDLIV
199600970530     C                   MOVEL     CPLDLV        VIDDLV
199700970917     C                   MOVEL     CPLFI1        VIDFI1
199800970530     C                   MOVEL     CPLLNA        VIDLNA
199900970530     C                   MOVEL     CPLLOL        VIDLOL
200000971118     C                   MOVEL     CPLLO2        VIDLO2
200100970604     C                   MOVEL     CPLLOS        VIDLOS
200200970530     C                   MOVEL     CPLZNC        VIDZNC
200300970530     C                   MOVEL     CPLZOL        VIDZOL
200400971118     C                   MOVEL     CPLZO2        VIDZO2
200500970604     C                   MOVEL     CPLZOS        VIDZOS
200600970530     C                   MOVEL     CPLLNA        VIDLFD
200700970530     C                   MOVEL     CPLZFD        VIDZFD
200800970530     C                   Z-ADD     CPLLKG        VIDLKG
200900970530     C                   Z-ADD     CPLLMC        VIDLMC
201000970604     C                   Z-ADD     CPLLKS        VIDLKS
201100970604     C                   Z-ADD     CPLLMS        VIDLMS
201200971118     C                   Z-ADD     CPLLK2        VIDLK2
201300971118     C                   Z-ADD     CPLLM2        VIDLM2
201400970530     C                   MOVEL     CPLCTS        VIDCTS
201500970530     C                   MOVEL     CPLCTE        VIDCTE
201600970530     C                   MOVEL     CPLISO        VIDISO
201700970530     C                   MOVEL     CPLPII        VIDPII
201800970530     C                   MOVEL     CPLPID        VIDPID
201900970530     C                   MOVEL     CPLPIS        VIDPIS
202000970530     C                   MOVEL     CPLGDD        VIDGDD
202100970530     C                   MOVEL     CPLTTC        VIDTTC
202200010322     C                   MOVEL     *all'9'       VIDTTR
202300010322     C                   MOVE      CPLFI2        VIDTTR
202400970530     C*
202500970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> IMPORTA I DATI DEL CAPPARIO
202600970530IF  2C     VIDLIV        IFEQ      'C'
202700970530     C                   SETON                                        92        *PROTEZIONE CAMPI
202800970530     C                   EXSR      IMPCPC
202900970530E   2C                   ENDIF
203000130916
203100130916     C                   MOVEL     VIDLNA        F_LNA
203200130916     C                   MOVEL     VIDLOL        F_LOL
203300130916     C                   MOVEL     VIDLOS        F_LOS
203400970530     C*
203500970526     C                   ENDSR
203600970526     C*------------------------------------------------------------------------*
203700130916     C* AZZVID - AZZERA I CAMPI DEL VIDEO
203800970526     C*------------------------------------------------------------------------*
203900970526     C     AZZVID        BEGSR
204000970526     C* CAMPI
204100970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
204200970530     C                   MOVEL     *BLANKS       VDDESS
204300970603     C                   MOVEL     D00NAR        VIDNAR
204400970603     C                   MOVEL     D00CAP        VIDCAP
204500970530     C                   MOVEL     *BLANKS       VIDLOC
204600970530     C                   MOVEL     *BLANKS       VIDLIV
204700970530     C                   MOVEL     *BLANKS       VIDDLV
204800970917     C                   MOVEL     *BLANKS       VIDFI1
204900970530     C                   MOVEL     *ZEROS        VIDLNA
205000970530     C                   MOVEL     *ZEROS        VIDLOL
205100971118     C                   MOVEL     *ZEROS        VIDLO2
205200970604     C                   MOVEL     *ZEROS        VIDLOS
205300970530     C                   MOVEL     *ZEROS        VIDZNC
205400970530     C                   MOVEL     *ZEROS        VIDZOL
205500971118     C                   MOVEL     *ZEROS        VIDZO2
205600970604     C                   MOVEL     *ZEROS        VIDZOS
205700970530     C                   MOVEL     *ZEROS        VIDLFD
205800970530     C                   MOVEL     *ZEROS        VIDZFD
205900970530     C                   Z-ADD     *ZEROS        VIDLKG
206000970530     C                   Z-ADD     *ZEROS        VIDLMC
206100970604     C                   Z-ADD     *ZEROS        VIDLKS
206200970604     C                   Z-ADD     *ZEROS        VIDLMS
206300971118     C                   Z-ADD     *ZEROS        VIDLK2
206400971118     C                   Z-ADD     *ZEROS        VIDLM2
206500970530     C                   MOVEL     *BLANKS       VIDCTS
206600970530     C                   MOVEL     *BLANKS       VIDCTE
206700970530     C                   MOVEL     *BLANKS       VIDISO
206800970530     C                   MOVEL     *BLANKS       VIDPII
206900970530     C                   MOVEL     *BLANKS       VIDPID
207000970530     C                   MOVEL     *BLANKS       VIDPIS
207100970530     C                   MOVEL     *BLANKS       VIDGDD
207200970530     C                   MOVEL     *ZEROS        VIDTTC
207300970526     C* DECODIFICHE
207400970527     C                   MOVEL     *BLANKS       VDDNAR
207500970527     C                   MOVEL     *BLANKS       VDDLNA
207600970527     C                   MOVEL     *BLANKS       VDDLOL
207700971118     C                   MOVEL     *BLANKS       VDDLO2
207800970604     C                   MOVEL     *BLANKS       VDDLOS
207900970530     C                   MOVEL     *BLANKS       VDDLFD
208000970530     C                   MOVEL     *BLANKS       VDDCTS
208100970530     C                   MOVEL     *BLANKS       VDDCTE
208200130916     C                   MOVEL     *BLANKS       VDDISO
208300130916     c* campi  di comodo
208400130916     C                   clear                   F_LNA
208500130916     C                   clear                   F_LOL
208600130916     C                   clear                   F_LOS
208700970526     C*
208800970526     C                   ENDSR
208900970530     C*------------------------------------------------------------------------*
209000000426     C* IMPCPC - IMPORTA I DATI DEL CAPPARIO
209100970530     C*------------------------------------------------------------------------*
209200970530     C     IMPCPC        BEGSR
209300970530     C*
209400970530     C                   MOVEL     D00VER        KCCVER
209500970530     C                   MOVEL     VIDNAR        KCCNAR
209600970530     C                   MOVEL     VIDCAP        KCCCAP
209700970530     C     KEYCPC        CHAIN     AZCPC01L                           99
209800970530IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
209900970530     C                   MOVEL     CPCLNA        VIDLNA
210000970530     C                   MOVEL     CPCLOL        VIDLOL
210100971118     C                   MOVEL     CPCLO2        VIDLO2
210200970604     C                   MOVEL     CPCLOS        VIDLOS
210300970530     C                   MOVEL     CPCZNC        VIDZNC
210400970530     C                   MOVEL     CPCZOL        VIDZOL
210500971118     C                   MOVEL     CPCZO2        VIDZO2
210600970604     C                   MOVEL     CPCZOS        VIDZOS
210700970530     C                   MOVEL     CPCLNA        VIDLFD
210800970530     C                   MOVEL     CPCZFD        VIDZFD
210900970530     C                   Z-ADD     CPCLKG        VIDLKG
211000970530     C                   Z-ADD     CPCLMC        VIDLMC
211100970604     C                   Z-ADD     CPCLKS        VIDLKS
211200970604     C                   Z-ADD     CPCLMS        VIDLMS
211300971118     C                   Z-ADD     CPCLK2        VIDLK2
211400971118     C                   Z-ADD     CPCLM2        VIDLM2
211500970530     C                   MOVEL     CPCCTS        VIDCTS
211600970530     C                   MOVEL     CPCCTE        VIDCTE
211700970530     C                   MOVEL     CPCISO        VIDISO
211800970530     C                   MOVEL     CPCPII        VIDPII
211900970530     C                   MOVEL     CPCPID        VIDPID
212000970530     C                   MOVEL     CPCPIS        VIDPIS
212100970530     C                   MOVEL     CPCGDD        VIDGDD
212200970530     C                   MOVEL     CPCTTC        VIDTTC
212300010705     C                   MOVEL     *all'9'       VIDTTR
212400010705     C                   MOVE      CPCFI2        VIDTTR
212500970530X   1C                   ELSE                                                   *NON TROVATO
212600970530     C                   MOVEL     *ZEROS        VIDLNA
212700970530     C                   MOVEL     *ZEROS        VIDLOL
212800971118     C                   MOVEL     *ZEROS        VIDLO2
212900970604     C                   MOVEL     *ZEROS        VIDLOS
213000970530     C                   MOVEL     *ZEROS        VIDZNC
213100970530     C                   MOVEL     *ZEROS        VIDZOL
213200971118     C                   MOVEL     *ZEROS        VIDZO2
213300970604     C                   MOVEL     *ZEROS        VIDZOS
213400970530     C                   MOVEL     *ZEROS        VIDLFD
213500970530     C                   MOVEL     *ZEROS        VIDZFD
213600970530     C                   Z-ADD     *ZEROS        VIDLKG
213700970530     C                   Z-ADD     *ZEROS        VIDLMC
213800970604     C                   Z-ADD     *ZEROS        VIDLKS
213900970604     C                   Z-ADD     *ZEROS        VIDLMS
214000971118     C                   Z-ADD     *ZEROS        VIDLK2
214100971118     C                   Z-ADD     *ZEROS        VIDLM2
214200970530     C                   MOVEL     *BLANKS       VIDCTS
214300970530     C                   MOVEL     *BLANKS       VIDCTE
214400970530     C                   MOVEL     *BLANKS       VIDISO
214500970530     C                   MOVEL     *BLANKS       VIDPII
214600970530     C                   MOVEL     *BLANKS       VIDPID
214700970530     C                   MOVEL     *BLANKS       VIDPIS
214800970530     C                   MOVEL     *BLANKS       VIDGDD
214900970530     C                   MOVEL     *ZEROS        VIDTTC
215000010705     C                   MOVEL     *BLANKS       VIDTTR
215100970530E   1C                   ENDIF
215200000426     C*
215300970530     C                   ENDSR
215400970526     C*------------------------------------------------------------------------*
215500970530     C* IMPCPL - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
215600970526     C*------------------------------------------------------------------------*
215700970530     C     IMPCPL        BEGSR
215800970526     C*
215900970530     C                   MOVEL     VIDNAR        CPLNAR
216000970530     C                   MOVEL     VIDCAP        CPLCAP
216100970530     C                   MOVEL     VIDLOC        CPLLOC
216200970530     C                   MOVEL     VIDLIV        CPLLIV
216300970530     C                   MOVEL     VIDDLV        CPLDLV
216400970917     C                   MOVEL     VIDFI1        CPLFI1
216500010322     C                   MOVE(p)   VIDTTR        CPLFI2
216600970530     C*
216700971118     C* LOCALITA' GESTITA A LIVELLO DI LOCALITA' -> SCRIVE I DATI VIDEO SUL FILE
216800970530IF  1C     VIDLIV        IFEQ      'L'
216900970530     C                   MOVEL     VIDLNA        CPLLNA
217000970530     C                   MOVEL     VIDLOL        CPLLOL
217100971118     C                   MOVEL     VIDLO2        CPLLO2
217200970604     C                   MOVEL     VIDLOS        CPLLOS
217300970530     C                   MOVEL     VIDZNC        CPLZNC
217400970530     C                   MOVEL     VIDZOL        CPLZOL
217500971118     C                   MOVEL     VIDZO2        CPLZO2
217600970604     C                   MOVEL     VIDZOS        CPLZOS
217700970530     C                   MOVEL     VIDZFD        CPLZFD
217800970530     C                   Z-ADD     VIDLKG        CPLLKG
217900970530     C                   Z-ADD     VIDLMC        CPLLMC
218000970604     C                   Z-ADD     VIDLKS        CPLLKS
218100970604     C                   Z-ADD     VIDLMS        CPLLMS
218200971118     C                   Z-ADD     VIDLK2        CPLLK2
218300971118     C                   Z-ADD     VIDLM2        CPLLM2
218400970530     C                   MOVEL     VIDCTS        CPLCTS
218500970530     C                   MOVEL     VIDCTE        CPLCTE
218600970530     C                   MOVEL     VIDISO        CPLISO
218700970603     C                   MOVEL     VIDPII        CPLPII
218800970603IF  3C     CPLISO        IFEQ      'I'
218900970603     C                   MOVEL     'S'           CPLPII
219000970603E   3C                   ENDIF
219100970603     C                   MOVEL     VIDPID        CPLPID
219200970603IF  3C     CPLISO        IFEQ      'D'
219300970603     C                   MOVEL     'S'           CPLPID
219400970603E   3C                   ENDIF
219500970603     C                   MOVEL     VIDPIS        CPLPIS
219600970603IF  3C     CPLISO        IFEQ      'S'
219700970603     C                   MOVEL     'S'           CPLPIS
219800970603E   3C                   ENDIF
219900970530     C                   MOVEL     VIDGDD        CPLGDD
220000970530     C                   MOVEL     VIDTTC        CPLTTC
220100970530     C*
220200970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> AZZERA I CAMPI RIFERITI AL CAP
220300970530X   1C                   ELSE
220400970530     C                   Z-ADD     *ZEROS        CPLLNA
220500970530     C                   Z-ADD     *ZEROS        CPLLOL
220600971118     C                   Z-ADD     *ZEROS        CPLLO2
220700970604     C                   Z-ADD     *ZEROS        CPLLOS
220800970530     C                   Z-ADD     *ZEROS        CPLZNC
220900970530     C                   Z-ADD     *ZEROS        CPLZOL
221000971118     C                   Z-ADD     *ZEROS        CPLZO2
221100970604     C                   Z-ADD     *ZEROS        CPLZOS
221200970530     C                   Z-ADD     *ZEROS        CPLZFD
221300970530     C                   Z-ADD     *ZEROS        CPLLKG
221400970530     C                   Z-ADD     *ZEROS        CPLLMC
221500970604     C                   Z-ADD     *ZEROS        CPLLKS
221600970604     C                   Z-ADD     *ZEROS        CPLLMS
221700971118     C                   Z-ADD     *ZEROS        CPLLK2
221800971118     C                   Z-ADD     *ZEROS        CPLLM2
221900970530     C                   MOVEL     *BLANKS       CPLCTS
222000970530     C                   MOVEL     *BLANKS       CPLCTE
222100970530     C                   MOVEL     *BLANKS       CPLISO
222200970530     C                   MOVEL     *BLANKS       CPLPII
222300970530     C                   MOVEL     *BLANKS       CPLPID
222400970530     C                   MOVEL     *BLANKS       CPLPIS
222500970530     C                   MOVEL     *BLANKS       CPLGDD
222600970530     C                   Z-ADD     *ZEROS        CPLTTC
222700010705     C                   MOVEL     *BLANKS       CPLFI2
222800970530E   1C                   ENDIF
222900970526     C*
223000970526     C                   ENDSR
223100970530     C*------------------------------------------------------------------------*
223200970530     C* LANPGM - LANCIA UN PROGRAMMA
223300970530     C*------------------------------------------------------------------------*
223400970530     C     LANPGM        BEGSR
223500970530     C*
223600970530     C* IMPOSTA LA DS DI PROCEDURA
223700970530     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
223800970530     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
223900970530     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
224000970530     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
224100970530     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
224200970530     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
224300970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
224400970602     C                   MOVEL     DCAP          D00CAP                         *CAP
224500970602     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
224600970603     C                   MOVEL     *BLANKS       D00SIN                         *ALTRI CAMPI
224700970603     C                   Z-ADD     *ZEROS        D00TTC
224800970530     C                   MOVEL     *BLANKS       D00TSP
224900970530     C                   Z-ADD     *ZEROS        D00TFP
225000970530     C                   MOVEL     *BLANKS       D00EPA
225100970530     C                   Z-ADD     *ZEROS        D00ETP
225200970530     C                   Z-ADD     *ZEROS        D00ETA
225300970530     C                   Z-ADD     *ZEROS        D00EDD
225400970530     C                   MOVEL     *BLANKS       D00SPT
225500970530     C                   Z-ADD     *ZEROS        D00SLP
225600970530     C                   Z-ADD     *ZEROS        D00SLA
225700970530     C                   MOVEL     *BLANKS       D00STS
225800970530     C                   Z-ADD     *ZEROS        D00SDD
225900970530     C*
226000970530     C                   CALL      WPGM
226100970612     C                   PARM                    KPJBA
226200970612     C                   PARM                    DSSI00
226300970530     C*
226400970530     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
226500970530IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
226600970530     C                   MOVEL     '1'           ERRL00                         *ERRORE
226700970530     C                   MOVEL     '1'           F03L00                         *FINE PGM
226800970530X   1C                   ELSE                                                   *NON PREMUTO F03
226900970530     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
227000970530E   1C                   ENDIF
227100970530     C*
227200970530     C                   ENDSR
227300970526     C*------------------------------------------------------------------------*
227400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
227500970526     C*------------------------------------------------------------------------*
227600970526     C     IMPFUN        BEGSR
227700970603     C*
227800970603     C* SE INTERROGAZIONE/VISUALIZZAZIONE TOGLE LA GESTIONE ALLE ZONE DI CONSEGNA
227900970603IF  1C     DEPOP0        IFEQ      'C05'
228000131007IF  1C     DEPOP0        oreq      'L05'
228100970603     C     DEPOP1        OREQ      'O05'
228200970603     C                   MOVEL     FUD(14)       FUDRIG                         *AGGIORNAMENTO
228300970603     C                   MOVEL     'N'           FUDATT
228400970603     C                   MOVEL     FUDRIG        FUD(14)
228500970603E   1C                   ENDIF
228600970526     C*
228700970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
228800970526IF  1C     DEPOP1        IFEQ      'O05'
228900970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
229000970526     C                   MOVEL     'N'           FUDATT
229100970526     C                   MOVEL     FUDRIG        FUD(6)
229200970526E   1C                   ENDIF
229300131007     c                   if        *in10 and  %subst(depop0:2:2) = '05'
229400131007     C                   MOVEL     FUD(7)        FUDRIG                         *LOCALITA'
229500131007     C                   MOVEL     'N'           FUDATT
229600131007     C                   MOVEL     FUDRIG        FUD(7)
229700131007     C                   MOVEL     FUD(17)       FUDRIG                         *LOCALITA'
229800131007     C                   MOVEL     'N'           FUDATT
229900131007     C                   MOVEL     FUDRIG        FUD(17)
230000131007     c                   endif
230100970526     C*
230200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
230300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
230400970526     C                   EXSR      FUNGES
230500000925     C*
230600000925     C* VERIFICO CHE SOLO UTENTI "EDP*" POSSANO MODIFICARE IL CAMPO "UFFICIO POSTALE"
230700000925     C                   MOVEL     KNMUS         UTE               3
230800000925     C     UTE           IFNE      'EDP'
230900000925     C                   SETON                                        95
231000000925     C                   ENDIF
231100970526     C*
231200970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
231300970526     C*---
231400970526     C* 90 -> PROTEZIONE CHIAVE
231500970526     C* 91 -> PROTEZIONE TOTALE
231600050414     C* 94 -> PROTEZIONE COD.TASSAZIONE e TIPO INOLTRO e altri
231700050413     C*---
231800050413     C*
231900050413     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
232000050413     C                   SETON                                        94
232100170116     c                   if        wabiute='V'
232200170116     c* se manut completa solo se  czppario ancora da decorrere, verifico con
232300170116     c*  la versione
232400170116     c     d00ver        chain     azcpr01l
232500170116     c                   if        %found(azcpr01l)
232600170116     c     *iso          movel     cprdde        dataiso
232700170116     c                   if        dataiso>datasys
232800170116     c                   setoff                                       94
232900170116     c                   else
233000170116     c                   eval      wabiute='I'
233100170116     c                   endif
233200170116     c
233300170116     c                   else
233400170116     c                   eval      wabiute='I'
233500170116     c                   endif
233600170116     c                   endif
233700050413     C*
233800050413     C* VERIFICO X PRIMISSIMA COSA LE AUTORIZZAZIONI SPECIALI SULL'UTENTE CORRENTE
233900170116     C                   IF        wabiute = *blanks  or wabiute='M'
234000050413     C* TUTTO RIMANE INVARIATO
234100050413     C                   ENDIF
234200050413     C*
234300170116     C                   IF        wabiute = 'I'  or wabiute='O'
234400140206     C* FORZO SOLO INTERROGAZIONE ma permetto di modificare gli orari se c'e' "O"
234500050413     C                   EVAL      DEPOP1 = 'O05'
234600050413     C                   ENDIF
234700050413     C*
234800170116     C                   IF        wabiute = 'C'
234900050413     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
235000050413     C                   SETOFF                                       94
235100050413     C                   ENDIF
235200050413     C*
235300970530IF  1C     DEPOP0        IFEQ      'L02'                                        G E S T I O N E
235400970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
235500970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
235600970526     C                   SETOFF                                       91
235700970526E   2C                   ENDIF
235800970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
235900970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
236000970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
236100970526E   2C                   ENDIF
236200970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
236300970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
236400970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
236500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
236600970526     C                   SETON                                        91
236700970526E   2C                   ENDIF
236800970526X   1C                   ELSE                                                   *I N T E R R O G A
236900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
237000970526     C                   SETON                                        91
237100970526E   1C                   ENDIF
237200970526     C*
237300970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
237400970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
237500970530     C                   Z-ADD     D00VER        KCLVER
237600970530     C                   MOVEL     D00NAR        KCLNAR
237700970530     C                   MOVEL     D00CAP        KCLCAP
237800970530     C                   MOVEL     D00LOC        KCLLOC
237900970530     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
238000970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
238100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
238200970526     C                   MOVEL     MSG(6)        DSMSMS
238300970526X   2C                   ELSE
238400970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
238500980128IF  3C     DEPOP1        IFEQ      'O05'
238600980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
238700980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
238800980128E   3C                   ENDIF
238900970526E   2C                   ENDIF
239000970526X   1C                   ELSE                                                   *INSERIMENTO
239100970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
239200970526E   1C                   ENDIF
239300970526     C*
239400970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
239500970526IF  1C     DEPOP1        IFEQ      'O02'
239600970526     C                   MOVEL     DEC(1)        VIDFUN
239700970526E   1C                   ENDIF
239800970526IF  1C     DEPOP1        IFEQ      'O04'
239900970526     C                   MOVEL     DEC(2)        VIDFUN
240000970526E   1C                   ENDIF
240100970526IF  1C     DEPOP1        IFEQ      'O05'
240200970526     C                   MOVEL     DEC(3)        VIDFUN
240300970526E   1C                   ENDIF
240400970526IF  1C     DEPOP1        IFEQ      'O07'
240500970526     C                   MOVEL     DEC(4)        VIDFUN
240600970526E   1C                   ENDIF
240700970526IF  1C     DEPOP1        IFEQ      'F06'
240800970526     C                   MOVEL     DEC(5)        VIDFUN
240900970526E   1C                   ENDIF
241000970526     C*
241100970526     C                   ENDSR
241200970626     C*------------------------------------------------------------------------*
241300970626     C* FINSR - OPERAZIONI FINALI
241400970626     C*------------------------------------------------------------------------*
241500970626     C     FINSR         BEGSR
241600970626     C*
241700970626     C* CHIUDE I FILE DEL "TISI95R"
241800970704     C                   CLEAR                   DSSI95
241900970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
242000970626     C                   CALL      'TISI95R'
242100970626     C                   PARM                    DSSI95
242200130917     c*
242300130917     c                   clear                   tisio7ds
242400130917     C                   MOVEL     'C'           Io7TLA
242500131016     c*****              movel     tisio7ds      kpjbu
242600130917     C                   CALL      'TISIO7R'
242700130917     C                   PARM                    kpjba
242800131016     C                   PARM                    tisio7ds
242900970626     C*
243000970626     C                   ENDSR
243100050413     C*--------------------------------------------------------------------------------------------*
243200050413     C* REPERISCE DATI UTENTE
243300050413     C*--------------------------------------------------------------------------------------------*
243400050413     C     REPDATIUTE    BEGSR
243500050413     C*
243600050413     C* REimposta le variabili di lavoro
243700050413     C                   CLEAR                   TIBS34DS
243800050413     C                   CLEAR                   AZUTEDS
243900050413     C                   CLEAR                   DDATIUTE
244000050413     C*
244100050413     C* Reperisce i dati del profilo in ingresso
244200050413     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
244300050413     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
244400050413     C                   IN(E)     *DTAARA
244500050413     C                   IF        %Error                                       *NO errori
244600050413     C                   EVAL      I34TLA = 'L'
244700050413     C                   CALL      'TIBS34R'
244800050413     C                   PARM                    TIBS34DS
244900050413     C                   IN        *DTAARA
245000050413     C                   ENDIF
245100050413     C*
245200050413     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
245300050413     C                   EVAL      DUTE01 = UTEFAF
245400050413     C*
245500050413     C                   ENDSR
245600130916     C*------------------------------------------------------------------------*
245700130916     C* Richiamo pgm annullamento orari servizi a fronte di modifica lna
245800130916     C*------------------------------------------------------------------------*
245900130916     c     sr_oraris     begsr
246000130916     C* LINEA STANDARD
246100130916     c                   movel     vidlna        w0030             3 0
246200130916     c* Se cambiata la linea
246300130916     c                   if        w0030<>f_lna
246400130916     c                   clear                   tisio7ds
246500130916     c                   eval      io7mod='D'
246600130917     c* Il livello da passare è quello a video
246700130917     c                   eval      io7liv=vidliv
246800130916     c                   eval      io7ver=d00ver
246900130916     c                   eval      io7nar=vidnar
247000130916     c                   eval      io7cap=vidcap
247100130917     c                   eval      io7loc=vidloc
247200130916     c                   eval      io7lna=f_lna
247300130916     c                   eval      io7lns=%dec(vidlna:3:0)
247400130916     c                   eval      io7los=%dec(vidlos:3:0)
247500130916     c                   eval      io7lol=%dec(vidlol:3:0)
247600131016     c******             movel     tisio7ds      kpjbu
247700130916     c                   call      'TISIO7R'
247800130916     c                   parm                    kpjba
247900131016     c                   parm                    tisio7ds
248000130916     c                   endif
248100130916     C* LINEA sotto
248200130916     c                   movel     vidlos        w0030             3 0
248300130916     c* Se cambiata la linea e non ancora richiamato pgm per la linea stessa
248400130916     c                   if        w0030<>f_los  and f_los<> f_lna
248500130916     c                   clear                   tisio7ds
248600130916     c                   eval      io7mod='D'
248700130917     c* Il livello da passare è quello a video
248800130917     c                   eval      io7liv=vidliv
248900130916     c                   eval      io7ver=d00ver
249000130916     c                   eval      io7nar=vidnar
249100130916     c                   eval      io7cap=vidcap
249200130917     c                   eval      io7loc=vidloc
249300130916     c                   eval      io7lna=f_los
249400130916     c                   eval      io7lns=%dec(vidlna:3:0)
249500130916     c                   eval      io7los=%dec(vidlos:3:0)
249600130916     c                   eval      io7lol=%dec(vidlol:3:0)
249700131016     c******             movel     tisio7ds      kpjbu
249800130916     c                   call      'TISIO7R'
249900130916     c                   parm                    kpjba
250000131016     c                   parm                    tisio7ds
250100130916     c                   endif
250200130916     C* LINEA OLTRE
250300130916     c                   movel     vidlol        w0030             3 0
250400130916     c* Se cambiata la linea e non ancora richiamato pgm per la linea stessa
250500130916     c                   if        w0030<>f_lol  and f_lol<> f_lna
250600130916     c                                           and f_lol<> f_los
250700130916     c                   clear                   tisio7ds
250800130916     c                   eval      io7mod='D'
250900130917     c* Il livello da passare è quello a video
251000130917     c                   eval      io7liv=vidliv
251100130916     c                   eval      io7ver=d00ver
251200130916     c                   eval      io7nar=vidnar
251300130916     c                   eval      io7cap=vidcap
251400130917     c                   eval      io7loc=vidloc
251500130916     c                   eval      io7lna=f_lol
251600130916     c                   eval      io7lns=%dec(vidlna:3:0)
251700130916     c                   eval      io7los=%dec(vidlos:3:0)
251800130916     c                   eval      io7lol=%dec(vidlol:3:0)
251900131016     c*****              movel     tisio7ds      kpjbu
252000130916     c                   call      'TISIO7R'
252100130916     c                   parm                    kpjba
252200131016     c                   parm                    tisio7ds
252300130916     c                   endif
252400130917     c*
252500130917     c                   eval      wdel='S'
252600130917     c                   exsr      sr_cpe
252700130917     c                   endsr
252800130917     C*------------------------------------------------------------------------*
252900130917     C* Richiamo pgm orari servizi partendo dalle eccezioni
253000130917     C*------------------------------------------------------------------------*
253100130917     c     sr_cpe        begsr
253200130917     c* Se passaggio di livello da cap a località  leggo le eccezioni
253300130917     c* e se presenti richiamo tisio7r passando le linee delle eccezioni
253400130917     c                   if        vidliv='L' and cplliv<>vidliv
253500130917     c     kcpe          setll     azcpe01l
253600130917     c     kcpe          reade     azcpe01l
253700130917     c                   dow       not %eof(azcpe01l)
253800130917     c* lna standard
253900130917     c                   move      cpelna        w003A             3
254000130917     c                   if        w003a <>vidlna and w003a <>vidlos
254100130917     c                             and w003a <>vidlol
254200130917     c                             and (f07lna=*blanks or wdel='S')
254300130917     c                   clear                   tisio7ds
254400130917     c                   if        wdel='S'
254500130917     c                   eval      io7mod='D'
254600130917     c                   else
254700130917     c                   eval      io7mod='C'
254800130917     c                   endif
254900130917     c* Il livello da passare è quello a video
255000130917     c                   eval      io7liv=vidliv
255100130917     c                   eval      io7ver=d00ver
255200130917     c                   eval      io7nar=vidnar
255300130917     c                   eval      io7cap=vidcap
255400130917     c                   eval      io7loc=vidloc
255500130917     c                   eval      io7lna=cpelna
255600130917     c                   eval      io7lns=%dec(vidlna:3:0)
255700130917     c                   eval      io7los=%dec(vidlos:3:0)
255800130917     c                   eval      io7lol=%dec(vidlol:3:0)
255900131016     c******             movel     tisio7ds      kpjbu
256000130917     c                   call      'TISIO7R'
256100130917     c                   parm                    kpjba
256200131016     c                   parm                    tisio7ds
256300130917     c                   if        oo7err='1'
256400130917     c                   movel     oo7msg        DSmsms
256500130917     c                   eval      ErrL00 = *on
256600130917     c                   eval      f07lna='S'
256700130917e   4c                   endif
256800130917     c                   endif
256900130917     c* lna sotto
257000130917     c                   move      cpelos        w003A             3
257100130917     c                   if        w003a<>vidlna and w003a <>vidlos
257200130917     c                             and w003a<>vidlol
257300130917     c                             and (f07lna=*blanks or wdel='S')
257400130917     c                   clear                   tisio7ds
257500130917     c                   if        wdel='S'
257600130917     c                   eval      io7mod='D'
257700130917     c                   else
257800130917     c                   eval      io7mod='C'
257900130917     c                   endif
258000130917     c* Il livello da passare è quello a video
258100130917     c                   eval      io7liv=vidliv
258200130917     c                   eval      io7ver=d00ver
258300130917     c                   eval      io7nar=vidnar
258400130917     c                   eval      io7cap=vidcap
258500130917     c                   eval      io7loc=vidloc
258600130917     c                   eval      io7lna=cpelos
258700130917     c                   eval      io7lns=%dec(vidlna:3:0)
258800130917     c                   eval      io7los=%dec(vidlos:3:0)
258900130917     c                   eval      io7lol=%dec(vidlol:3:0)
259000131016     c******             movel     tisio7ds      kpjbu
259100130917     c                   call      'TISIO7R'
259200130917     c                   parm                    kpjba
259300131016     c                   parm                    tisio7ds
259400130917     c                   if        oo7err='1'
259500130917     c                   movel     oo7msg        DSmsms
259600130917     c                   eval      ErrL00 = *on
259700130917     c                   eval      f07lna='S'
259800130917e   4c                   endif
259900130917e   4c                   endif
260000130917     c* lna oltre
260100130917     c                   move      cpelol        w003A             3
260200130917     c                   if        w003a <>vidlna and w003a <>vidlos
260300130917     c                             and w003a <>vidlol
260400130917     c                             and (f07lna=*blanks or wdel='S')
260500130917     c                   clear                   tisio7ds
260600130917     c                   if        wdel='S'
260700130917     c                   eval      io7mod='D'
260800130917     c                   else
260900130917     c                   eval      io7mod='C'
261000130917     c                   endif
261100130917     c* Il livello da passare è quello a video
261200130917     c                   eval      io7liv=vidliv
261300130917     c                   eval      io7ver=d00ver
261400130917     c                   eval      io7nar=vidnar
261500130917     c                   eval      io7cap=vidcap
261600130917     c                   eval      io7loc=vidloc
261700130917     c                   eval      io7lna=cpelol
261800130917     c                   eval      io7lns=%dec(vidlna:3:0)
261900130917     c                   eval      io7los=%dec(vidlos:3:0)
262000130917     c                   eval      io7lol=%dec(vidlol:3:0)
262100131016     c******             movel     tisio7ds      kpjbu
262200130917     c                   call      'TISIO7R'
262300130917     c                   parm                    kpjba
262400131016     c                   parm                    tisio7ds
262500130917     c                   if        oo7err='1'
262600130917     c                   movel     oo7msg        DSmsms
262700130917     c                   eval      ErrL00 = *on
262800130917     c                   eval      f07lna='S'
262900130917e   4c                   endif
263000130917e   4c                   endif
263100130917     c     kcpe          reade     azcpe01l
263200130917     c                   enddo
263300130917     c                   endif
263400130917     c                   endsr
263500970526     C*------------------------------------------------------------------------*
263600970526     C* *INZSR - OPERAZIONI INIZIALI
263700970526     C*------------------------------------------------------------------------*
263800970526     C     *INZSR        BEGSR
263900970526     C*---
264000970526     C* RICEVIMENTO PARAMETRI
264100970526     C*---
264200970526     C     *ENTRY        PLIST
264300970612     C                   PARM                    KPJBA
264400970612     C                   PARM                    DSSI00
264500050413     C*
264600050413     C* REPERISCE I DATI DELL'UTENTE
264700050413     C                   EXSR      REPDATIUTE
264800170116     c                   eval      wabiute=§utecap
264900970526     C*
265000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
265100970526     C     XMSGPA        PLIST
265200970526     C                   PARM                    DSMSMS
265300970526     C                   PARM                    DSMSPG
265400970526     C                   PARM                    DSMSKE
265500970526     C                   PARM                    DSMSER
265600970526     C*
265700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
265800970526     C                   CALL      'XKEY'
265900970526     C                   PARM                    DSKEY
266000131007      /free
266100131007        // Carico filiali di test abilitate
266200131007         if dutpou<>46  ;
266300131007
266400131007         clear TIBS02ds;
266500131007         T02Mod = 'C';
266600131007         T02sif = knsif;
266700131007         T02cod = 'VPO';
266800131007         t02tla = 'L'   ;
266900131007         T02ke1 ='DECOFI816' ;
267000131007         TNTBE_RicercaControllo  (kpjba : tibs02ds);
267100131007
267200131007         if  T02err  = *blank;
267300131007         xx= 1 ;
267400131007         yy= 1 ;
267500131007
267600131007         dow xx< 256   ;
267700131007           fil_ab(yy)= %subst(t02uni:xx:3)  ;
267800131007           yy=yy+ 1   ;
267900131007           xx=xx+ 3   ;
268000131007         enddo  ;
268100131007
268200131007         endif;
268300131007
268400131007         // se c'e' 999 tutte abilitate
268500131007         w003a='999'  ;
268600131007         if   %lookup(w003a:fil_ab) = 0  ;
268700131007         w003a=%editc(dutpou:'X')  ;
268800131007         if   %lookup(w003a:fil_ab) = 0  ;
268900131007         *in10 =*on  ;
269000131007         endif  ;
269100131007         endif  ;
269200131007
269300131007         endif  ;
269400131007
269500131007      /end-free
269600131007
269700090903     c*
269800090903     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
269900170116     c**                 clear                   tibs02ds
270000170116     c**                 clear                   dvpodeco
270100170116     c**                 eval      t02mod = 'C'
270200170116     c**                 eval      t02sif = knsif
270300170116     c**                 eval      t02cod = 'VPO'
270400170116     c**                 eval      t02ke1 = 'DECO'
270500170116     c**                 call      'TIBS02R'
270600170116     c**                 parm                    kpjba
270700170116     c**                 parm                    tibs02ds
270800170116     c**                 if        t02err = *blanks
270900170116     c**                 eval      dvpodeco = t02uni
271000170116     c**                 else
271100170116     c**                 eval      §vpotsph=20091001
271200170116     c**                 endif
271300090903
271400170116     c**   *iso          movel     §vpotsph      dataiso
271500090903     c
271600970526     C*---
271700970526     C* VARIABILI NON RIFERITE AL DATA BASE
271800970526     C*---
271900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
272000970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
272100970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
272200970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
272300970526     C     *LIKE         DEFINE    D00OP1        WOP1
272400970602     C     *LIKE         DEFINE    D00NAR        DNAR
272500970602     C     *LIKE         DEFINE    D00CAP        DCAP
272600970602     C     *LIKE         DEFINE    D00LOC        DLOC
272700970526     C*---
272800970526     C* VARIABILI DI CONTROLLO
272900970526     C*---
273000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
273100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
273200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
273300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
273400970526     C*
273500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
273600970529     C                   MOVEL     'TISI31R'     DSMSPG
273700970529     C                   MOVEL     'TISI31R'     MSGQUE
273800970526     C                   MOVEL     *BLANKS       MSGKEY
273900970526     C                   MOVEL     *BLANKS       DSMSMS
274000970526     C*---
274100970526     C* VARIABILI RIFERITE AL DATA BASE
274200970526     C*---
274300970530     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
274400970530     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
274500970530     C     *LIKE         DEFINE    TBLCOD        KTBCOD
274600970530     C     *LIKE         DEFINE    TBLKEY        KTBKEY
274700970530     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
274800970530     C     *LIKE         DEFINE    CPLNAR        KCLNAR
274900970530     C     *LIKE         DEFINE    CPLCAP        KCLCAP
275000970530     C     *LIKE         DEFINE    CPLLOC        KCLLOC
275100970530     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
275200970530     C     *LIKE         DEFINE    CPCNAR        KCCNAR
275300970530     C     *LIKE         DEFINE    CPCCAP        KCCCAP
275400970530     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
275500970530     C     *LIKE         DEFINE    CPTTTC        KCTTTC
275600970530     C     *LIKE         DEFINE    CPTTSP        KCTTSP
275700970925     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
275800970925     C     *LIKE         DEFINE    CPSNAR        KCSNAR
275900970925     C     *LIKE         DEFINE    CPSCAP        KCSCAP
276000970925     C     *LIKE         DEFINE    CPSLOC        KCSLOC
276100970613     C     *LIKE         DEFINE    CPCLNA        NLNA                           *DEPOSITI NUMERICI
276200970613     C     *LIKE         DEFINE    CPCLOS        NLOS
276300970613     C     *LIKE         DEFINE    CPCLOL        NLOL
276400971118     C     *LIKE         DEFINE    CPCLO2        NLO2
276500970613     C     *LIKE         DEFINE    CPCTTC        NTTC
276600010911     C     *LIKE         DEFINE    CPCFI2        NTTR
276700970526     C*---
276800970526     C* CHIAVI DI LETTURA
276900970526     C*---
277000970530     C* LETTURA AZORG01L - COMPLETA
277100970530     C     KEYORG        KLIST
277200970530     C                   KFLD                    KORFIL                         *FILIALE
277300970530     C* LETTURA TABEL00F - COMPLETA
277400970530     C     KEYTAB        KLIST
277500970530     C                   KFLD                    KTBKUT                         *CODICE UTENTE
277600970530     C                   KFLD                    KTBCOD                         *TABELLA
277700970530     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
277800970530     C* LETTURA TABEL00F - PARZIALE
277900970530     C     KE2TAB        KLIST
278000970530     C                   KFLD                    KTBKUT                         *CODICE UTENTE
278100970530     C                   KFLD                    KTBCOD                         *TABELLA
278200970530     C* LETTURA AZCPL01L - COMPLETA
278300970530     C     KEYCPL        KLIST
278400970530     C                   KFLD                    KCLVER                         *VERSIONE
278500970530     C                   KFLD                    KCLNAR                         *NAZIONE
278600970530     C                   KFLD                    KCLCAP                         *CAP
278700970530     C                   KFLD                    KCLLOC                         *LOCALITA'
278800970613     C* LETTURA AZCPL01L - PARZIALE
278900970613     C     KE2CPL        KLIST
279000970613     C                   KFLD                    KCLVER                         *VERSIONE
279100970613     C                   KFLD                    KCLNAR                         *NAZIONE
279200970613     C                   KFLD                    KCLCAP                         *CAP
279300970527     C* LETTURA AZCPC01L - COMPLETA
279400970527     C     KEYCPC        KLIST
279500970527     C                   KFLD                    KCCVER                         *VERSIONE
279600970527     C                   KFLD                    KCCNAR                         *NAZIONE
279700970527     C                   KFLD                    KCCCAP                         *CAP
279800970530     C* LETTURA AZCPT01L - COMPLETA
279900970530     C     KEYCPT        KLIST
280000970530     C                   KFLD                    KCTVER                         *VERSIONE
280100970530     C                   KFLD                    KCTTTC                         *TEMPO CONSEGNA
280200970530     C                   KFLD                    KCTTSP                         *TIPO SERVIZIO
280300970925     C* LETTURA AZCPS01L - PARZIALE
280400970925     C     KEYCPS        KLIST
280500970925     C                   KFLD                    KCSVER                         *VERSIONE
280600970925     C                   KFLD                    KCSNAR                         *NAZIONE
280700970925     C                   KFLD                    KCSCAP                         *CAP
280800970925     C                   KFLD                    KCSLOC                         *LOCALITA
280900130917     C* LETTURA AZCpe01L -
281000130917     C     Kcpe          KLIST
281100130917     C                   KFLD                    d00ver                         *VERSIONE
281200130917     C                   KFLD                    vidnar                         *NAZIONE
281300130917     C                   KFLD                    vidcap                         *CAP
281400970526     C*
281500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
281600970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
281700970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
281800970526     C*
281900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
282000970526     C                   EXSR      IMPFUN
282100970526     C*
282200970526     C                   ENDSR
282300970526     C*------------------------------------------------------------------------*
282400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
282500970526     C*------------------------------------------------------------------------*
282600970526     C     FUNGES        BEGSR
282700970526     C*
282800970526     C* PULISCO CAMPI DI LAVORO
282900970526     C                   Z-ADD     0             W                 5 0
283000970526     C*
283100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
283200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
283300970526     C     1             DO        24            I
283400970526     C                   MOVEL     FUD(I)        FUDRIG
283500970526     C*
283600970526     C* PULIZIA FUNZIONI PRECEDENTI
283700970526     C                   MOVEL     *BLANKS       FUV(I)
283800970526     C*
283900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
284000970526     C* (SALTA F24)
284100970526     C     FUDATT        IFEQ      'S'
284200970526     C     FUDFUN        ANDNE     '24'
284300970526     C                   ADD       1             W
284400970526     C                   MOVEL     FUDDES        FUV(W)
284500970526     C                   END
284600970526     C                   END                                                    DO WHILE
284700970526     C*
284800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
284900970526     C* ALTRIMENTI DISATTIVO F24
285000970526     C     1             DO        24            I
285100970526     C                   MOVEL     FUD(I)        FUDRIG
285200970526     C*
285300970526     C     FUDFUN        IFEQ      '24'
285400970526     C     W             IFGT      6
285500970526     C                   MOVEL     'S'           FUDATT
285600970526     C                   ELSE
285700970526     C                   MOVEL     *BLANKS       FUDATT
285800970526     C                   END                                                    W GT 8
285900970526     C                   MOVEL     FUDRIG        FUD(I)
286000970526     C                   END
286100970526     C                   END                                                    DO WHILE
286200970526     C*
286300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
286400970526     C     W             DIV       6             I
286500970526     C                   MVR                     Z
286600970526     C*
286700970526     C     Z             IFNE      0
286800970526     C     I             ADD       1             Z
286900970526     C                   ELSE
287000970526     C                   Z-ADD     I             Z                 5 0
287100970526     C                   END
287200970526     C*
287300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
287400970526     C                   ADD       1             FUNGRU
287500970526     C     FUNGRU        IFGT      Z
287600970526     C     FUNGRU        ORLT      1
287700970526     C                   Z-ADD     1             FUNGRU
287800970526     C                   END
287900970526     C*
288000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
288100970526     C     FUNGRU        IFEQ      1
288200970526     C                   MOVEL     FUV(1)        FUN001
288300970526     C                   MOVEL     FUV(2)        FUN002
288400970526     C                   MOVEL     FUV(3)        FUN003
288500970526     C                   MOVEL     FUV(4)        FUN004
288600970526     C                   MOVEL     FUV(5)        FUN005
288700970526     C                   MOVEL     FUV(6)        FUN006
288800970526     C                   END
288900970526     C*
289000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
289100970526     C     FUNGRU        IFEQ      2
289200970526     C                   MOVEL     FUV(7)        FUN001
289300970526     C                   MOVEL     FUV(8)        FUN002
289400970526     C                   MOVEL     FUV(9)        FUN003
289500970526     C                   MOVEL     FUV(10)       FUN004
289600970526     C                   MOVEL     FUV(11)       FUN005
289700970526     C                   MOVEL     FUV(12)       FUN006
289800970526     C                   END
289900970526     C*
290000970526     C* TERZO GRUPPO DI 6 FUNZIONI
290100970526     C     FUNGRU        IFEQ      3
290200970526     C                   MOVEL     FUV(13)       FUN001
290300970526     C                   MOVEL     FUV(14)       FUN002
290400970526     C                   MOVEL     FUV(15)       FUN003
290500970526     C                   MOVEL     FUV(16)       FUN004
290600970526     C                   MOVEL     FUV(17)       FUN005
290700970526     C                   MOVEL     FUV(18)       FUN006
290800970526     C                   END
290900970526     C*
291000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
291100970526     C     FUNGRU        IFEQ      4
291200970526     C                   MOVEL     FUV(19)       FUN001
291300970526     C                   MOVEL     FUV(20)       FUN002
291400970526     C                   MOVEL     FUV(21)       FUN003
291500970526     C                   MOVEL     FUV(22)       FUN004
291600970526     C                   MOVEL     *BLANKS       FUN005
291700970526     C                   MOVEL     *BLANKS       FUN006
291800970526     C                   END
291900970526     C*
292000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
292100970526     C                   MOVEL     *BLANKS       FUN007
292200970526     C                   MOVEL     *BLANKS       FUN008
292300970526     C*
292400970526     C     1             DO        24            I
292500970526     C                   MOVEL     FUD(I)        FUDRIG
292600970526     C     FUDFUN        IFEQ      '24'
292700970526     C     FUDATT        ANDEQ     'S'
292800970526     C                   MOVEL     FUDDES        FUN008
292900970526     C                   END
293000970526     C                   END                                                    DO
293100970526     C*
293200970526     C                   ENDSR
293300970526     C*------------------------------------------------------------------------*
293400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
293500970526     C*------------------------------------------------------------------------*
293600970526     C     FUNCON        BEGSR
293700970526     C*
293800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
293900970526     C                   EXSR      FUNVAR
294000970526     C*
294100970526     C* PULISCO CAMPI DI LAVORO
294200970526     C                   Z-ADD     0             I                 5 0
294300970526     C                   Z-ADD     0             W                 5 0
294400970526     C*
294500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
294600970526     C     FUNKEY        IFNE      *BLANKS
294700970526     C     1             DO        24            I
294800970526     C                   MOVEL     FUD(I)        FUDRIG
294900970526     C*
295000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
295100970526     C     FUNKEY        IFEQ      FUDFUN
295200970526     C     FUDATT        ANDEQ     'S'
295300970526     C                   Z-ADD     I             W
295400970526     C                   END
295500970526     C                   END                                                    DO WHILE
295600970526     C*
295700970526     C* FUNZIONE NON ATTIVA (ERRORE)
295800970526     C     W             IFEQ      0
295900970526     C*
296000970526     C* EMISSIONE MESSAGGIO DI ERRORE
296100970526     C                   MOVEL     MSG(1)        DSMSMS
296200970526     C*
296300970526     C                   END                                                    W=0
296400970526     C                   END                                                    FUNKEY NE *BLANKS
296500970526     C*
296600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
296700970526     C     W             IFNE      0
296800970526     C     FUNKEY        OREQ      *BLANKS
296900970526     C     FUNKEY        IFEQ      *BLANKS
297000970526     C                   EXSR      ENTGES
297100970526     C                   END
297200970526     C     FUNKEY        IFEQ      '03'
297300970526     C                   EXSR      F03GES
297400970526     C                   END
297500010322     C     FUNKEY        IFEQ      '04'
297600010322     C                   EXSR      F04GES
297700010322     C                   END
297800970526     C     FUNKEY        IFEQ      '06'
297900970526     C                   EXSR      F06GES
298000970526     C                   END
298100130827     C     FUNKEY        IFEQ      '07'
298200130827     C                   EXSR      F07GES
298300130827     C                   END
298400970526     C     FUNKEY        IFEQ      '12'
298500970526     C                   EXSR      F12GES
298600970526     C                   END
298700970603     C     FUNKEY        IFEQ      '14'
298800970603     C                   EXSR      F14GES
298900970603     C                   END
299000970602     C     FUNKEY        IFEQ      '13'
299100970602     C                   EXSR      F13GES
299200970602     C                   END
299300130830     C     FUNKEY        IFEQ      '17'
299400130830     C                   EXSR      F17GES
299500130830     C                   END
299600970526     C     FUNKEY        IFEQ      '24'
299700970526     C                   EXSR      F24GES
299800970526     C                   END
299900970526     C                   END
300000970526     C*
300100970526     C                   ENDSR
300200970526     C*------------------------------------------------------------------------*
300300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
300400970526     C*------------------------------------------------------------------------*
300500970526     C     FUNVAR        BEGSR
300600970526     C*
300700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
300800970526     C*
300900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
301000970526     C     KEY           IFEQ      ENTER
301100970526     C                   MOVEL     *BLANKS       FUNKEY
301200970526     C                   END
301300970526     C     KEY           IFEQ      F1
301400970526     C                   MOVEL     '01'          FUNKEY
301500970526     C                   END
301600970526     C     KEY           IFEQ      F2
301700970526     C                   MOVEL     '02'          FUNKEY
301800970526     C                   END
301900970526     C     KEY           IFEQ      F3
302000970526     C                   MOVEL     '03'          FUNKEY
302100970526     C                   END
302200970526     C     KEY           IFEQ      F4
302300970526     C                   MOVEL     '04'          FUNKEY
302400970526     C                   END
302500970526     C     KEY           IFEQ      F5
302600970526     C                   MOVEL     '05'          FUNKEY
302700970526     C                   END
302800970526     C     KEY           IFEQ      F6
302900970526     C                   MOVEL     '06'          FUNKEY
303000970526     C                   END
303100970526     C     KEY           IFEQ      F7
303200970526     C                   MOVEL     '07'          FUNKEY
303300970526     C                   END
303400970526     C     KEY           IFEQ      F8
303500970526     C                   MOVEL     '08'          FUNKEY
303600970526     C                   END
303700970526     C     KEY           IFEQ      F9
303800970526     C                   MOVEL     '09'          FUNKEY
303900970526     C                   END
304000970526     C     KEY           IFEQ      F10
304100970526     C                   MOVEL     '10'          FUNKEY
304200970526     C                   END
304300970526     C     KEY           IFEQ      F11
304400970526     C                   MOVEL     '11'          FUNKEY
304500970526     C                   END
304600970526     C     KEY           IFEQ      F12
304700970526     C                   MOVEL     '12'          FUNKEY
304800970526     C                   END
304900970526     C     KEY           IFEQ      F13
305000970526     C                   MOVEL     '13'          FUNKEY
305100970526     C                   END
305200970526     C     KEY           IFEQ      F14
305300970526     C                   MOVEL     '14'          FUNKEY
305400970526     C                   END
305500970526     C     KEY           IFEQ      F15
305600970526     C                   MOVEL     '15'          FUNKEY
305700970526     C                   END
305800970526     C     KEY           IFEQ      F16
305900970526     C                   MOVEL     '16'          FUNKEY
306000970526     C                   END
306100970526     C     KEY           IFEQ      F17
306200970526     C                   MOVEL     '17'          FUNKEY
306300970526     C                   END
306400970526     C     KEY           IFEQ      F18
306500970526     C                   MOVEL     '18'          FUNKEY
306600970526     C                   END
306700970526     C     KEY           IFEQ      F19
306800970526     C                   MOVEL     '19'          FUNKEY
306900970526     C                   END
307000970526     C     KEY           IFEQ      F20
307100970526     C                   MOVEL     '20'          FUNKEY
307200970526     C                   END
307300970526     C     KEY           IFEQ      F21
307400970526     C                   MOVEL     '21'          FUNKEY
307500970526     C                   END
307600970526     C     KEY           IFEQ      F22
307700970526     C                   MOVEL     '22'          FUNKEY
307800970526     C                   END
307900970526     C     KEY           IFEQ      F23
308000970526     C                   MOVEL     '23'          FUNKEY
308100970526     C                   END
308200970526     C     KEY           IFEQ      F24
308300970526     C                   MOVEL     '24'          FUNKEY
308400970526     C                   END
308500970526     C     KEY           IFEQ      ROLLUP
308600970526     C                   MOVEL     'RU'          FUNKEY
308700970526     C                   END
308800970526     C     KEY           IFEQ      HELP
308900970526     C                   MOVEL     'HP'          FUNKEY
309000970526     C                   END
309100970526     C                   ENDSR
309200970526     C*------------------------------------------------------------------------*
309300970526     C* NOTE COSTRUZIONE SCHIERE
309400970526     C*------------------------------------------------------------------------*
309500970526     C*
309600970526     C* -MSG-MESSAGGI
309700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
309800970526     C* (NON TOCCARE MSG,1 E MSG,2)
309900970526     C*
310000970526     C* -CMD-COMANDI
310100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
310200970526     C*
310300970526     C* -OPD-OPZIONI
310400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
310500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
310600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
310700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
310800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
310900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
311000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
311100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
311200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
311300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
311400970526     C*
311500970526     C* -FUD-FUNZIONI
311600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
311700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
311800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
311900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
312000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
312100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
312200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
312300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
312400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
312500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
312600970526     O*------------------------------------------------------------------------*
312700970526** DEC - DECODIFICHE
312800970526    MODIFICA                                                      1
312900970526  ANNULLAMENTO                                                    2
313000970526VISUALIZZAZIONE                                                   3
313100970526   RIPRISTINO                                                     4
313200970526  INSERIMENTO                                                     5
313300970526** MSG - MESSAGGI
313400970530TISI31R- Tasto funzionale non gestito                                          1
313500970530TISI31R- Inserimento effettuato                                                2
313600970530TISI31R- Modifica effettuata                                                   3
313700970530TISI31R- Annullamento effettuato                                               4
313800970530TISI31R- Ripristino effettuato                                                 5
313900970530TISI31R- Richiesta funzione per record inesistente                             6
314000970530TISI31R- Record momentaneamente allocato: riprovare più tardi                  7
314100970530TISI31R- Richiesta funzione per record esistente                               8
314200000504TISI31R- Ufficio Postale errato, annullato o inesistente                       9
314300970530TISI31R- Linea di arrivo errata                                               10
314400970530TISI31R- Zona di consegna errata                                              11
314500970917TISI31R- CAP obsoleto: anche la località deve esserlo                         12
314600970530TISI31R- Nazione errata                                                       13
314700970530TISI31R- Codice tassazione errato                                             14
314800970530TISI31R- Codice tassazione errato. Accettati solo I1, I2, I3, ecc ...         15
314900970530TISI31R- Per CAP estero lasciare il codice tassazione vuoto                   16
315000060206TISI31R- Zona di consegna NON abilitata                                       17
315100970604TISI31R- Linea limite obbligatoria se immessi limiti di peso o volume         18
315200970604TISI31R- Limiti obbligatori se immesse linea o zona limite                    19
315300970917TISI31R- Impossibile inserire località di CAP obsoleti                        20
315400970917TISI31R- CAP errato o inesistente                                             21
315500970530TISI31R- Località errata                                                      22
315600970530TISI31R- Immettere L x gestire i dati della località, C x usare quelli del cap23
315700970912TISI31R- Il limite oltre deve essere superiore del limite sotto               24
315800970917TISI31R- Immettere 'O' per rendere obsoleta la località, o lasciare vuoto     25
315900970618TISI31R- Unica località per il cap. impossibile gestirla a livello località   26
316000970530TISI31R- Immettere 'S' o 'N'                                                  27
316100110623TISI31R- Tipo inoltro errato o non utilizzabile nel cappario                  28
316200970618TISI31R- La località è uguale al cap. Impossibile gestirla a livello località 29
316300970603TISI31R- Tipo tempo di consegna errato o incompleto                           30
316400010322TISI31R- Tasto funzione non consentito sul campo corrente.                    31
316500010322TISI31R- Tipo tempo ritiro errato o incompleto                                32
316600041008TISI31R- Valorizzare entrambi i campi o non valorizzarne nessuno.             33
316700130905Esistono OrariServizi personalizz.x la linea modificata saranno elimiati      34
316800130905TISI31R- F17 consentito soltando sui campi LINEA di ARRIVO valorizzati        35
316900970526** FUD - FUNZIONI
317000970526                   -        (S/N)=N                        funz.01¬
317100970526                   -        (S/N)=N                        funz.02¬
317200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
317300010322F04=Visual. tempi  -        (S/N)=S  <== S-FISSO           funz.04¬
317400130827                   -        (S/N)=N                        funz.05¬
317500970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
317600130827F07=Orari Servizi  -        (S/N)=S                        funz.07¬
317700130821                   -        (S/N)=N                        funz.08¬
317800130821                   -        (S/N)=N                        funz.09¬
317900970527                   -        (S/N)=N                        funz.10¬
318000970526                   -        (S/N)=N                        funz.11¬
318100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
318200970602F13=Sinonimi       -        (S/N)=S                        funz.13¬
318300970603F14=Zone consegna  -        (S/N)=S                        funz.14¬
318400970526                   -        (S/N)=N                        funz.15¬
318500970526                   -        (S/N)=N                        funz.16¬
318600140210F17=OrariServLinea -        (S/N)=S                        funz.17¬
318700970526                   -        (S/N)=N                        funz.18¬
318800970526                   -        (S/N)=N                        funz.19¬
318900970526                   -        (S/N)=N                        funz.20¬
319000970526                   -        (S/N)=N                        funz.21¬
319100970526                   -        (S/N)=N                        funz.22¬
319200970526                   -        (S/N)=N                        funz.23¬
319300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
