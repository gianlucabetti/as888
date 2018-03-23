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
120100970530     C*--------------------
120200970530     C* CODICE TASSAZIONE
120300970530     C*--------------------
120400970530IF  1C     ERRL00        IFEQ      '0'
120500970530     C                   MOVEL     *BLANKS       VDDCTS
120600970530     C                   MOVEL     VIDCTS        WCTS
120700970530     C                   MOVEL     *BLANKS       WDCTS
120800970530     C                   EXSR      CHKCTS
120900970530     C                   MOVEL     WCTS          VIDCTS
121000970530     C                   MOVEL     WDCTS         VDDCTS
121100970530IF  2C     ERRL00        IFEQ      '1'
121200970530     C                   SETON                                        58
121300970530     C                   MOVEL     MSG(14)       DSMSMS
121400970530E   2C                   ENDIF
121500970530E   1C                   ENDIF
121600970530     C*--------------------
121700970530     C* CODICE TASSAZIONE ITALIA PER ESTERO
121800970530     C*--------------------
121900970530IF  1C     ERRL00        IFEQ      '0'
122000970530     C                   MOVEL     *BLANKS       VDDCTE
122100970530IF  2C     VIDNAR        IFEQ      *BLANKS                                      *NAZIONE ITALIA
122200970530     C                   MOVEL     VIDCTE        WA1D              1
122300970530     C                   MOVE      VIDCTE        WA1S              1
122400970530IF  3C     WA1D          IFNE      'I'
122500970530     C     WA1S          OREQ      *BLANKS
122600970530     C                   MOVEL     '1'           ERRL00
122700970530     C                   SETON                                        59
122800970530     C                   MOVEL     MSG(15)       DSMSMS
122900970530E   3C                   ENDIF
123000970530X   2C                   ELSE                                                   *NAZIONE ESTERO
123100970530IF  3C     VIDCTE        IFNE      *BLANKS
123200970530     C                   MOVEL     '1'           ERRL00
123300970530     C                   SETON                                        59
123400970530     C                   MOVEL     MSG(16)       DSMSMS
123500970530E   3C                   ENDIF
123600970530E   2C                   ENDIF
123700970530E   1C                   ENDIF
123800970530     C*--------------------
123900970530     C* INOLTRO
124000970530     C*--------------------
124100970530IF  1C     ERRL00        IFEQ      '0'
124200970530     C                   MOVEL     *BLANKS       VDDISO
124300970530     C                   MOVEL     VIDISO        WINL
124400970530     C                   MOVEL     *BLANKS       WDINL
124500970530     C                   EXSR      CHKINL
124600970530     C                   MOVEL     WINL          VIDISO
124700970530     C                   MOVEL     WDINL         VDDISO
124800970530IF  2C     ERRL00        IFEQ      '1'
124900970530     C                   SETON                                        75
125000970530     C                   MOVEL     MSG(28)       DSMSMS
125100970530E   2C                   ENDIF
125200970530E   1C                   ENDIF
125300970530     C*--------------------
125400970530     C* POSSIBILE INOLTRO ISOLA
125500970530     C*--------------------
125600970530IF  1C     ERRL00        IFEQ      '0'
125700970530IF  2C     VIDPII        IFEQ      'S'
125800970530     C     VIDPII        OREQ      'N'
125900970530X   2C                   ELSE
126000970530     C                   MOVEL     '1'           ERRL00
126100970530     C                   SETON                                        76
126200970530     C                   MOVEL     MSG(27)       DSMSMS
126300970530E   2C                   ENDIF
126400970530E   1C                   ENDIF
126500970530     C*--------------------
126600970530     C* POSSIBILE INOLTRO LOCALITA' DISAGIATA
126700970530     C*--------------------
126800970530IF  1C     ERRL00        IFEQ      '0'
126900970530IF  2C     VIDPID        IFEQ      'S'
127000970530     C     VIDPID        OREQ      'N'
127100970530X   2C                   ELSE
127200970530     C                   MOVEL     '1'           ERRL00
127300970530     C                   SETON                                        77
127400970530     C                   MOVEL     MSG(27)       DSMSMS
127500970530E   2C                   ENDIF
127600970530E   1C                   ENDIF
127700970530     C*--------------------
127800970530     C* POSSIBILE INOLTRO CENTRO STORICO
127900970530     C*--------------------
128000970530IF  1C     ERRL00        IFEQ      '0'
128100970530IF  2C     VIDPIS        IFEQ      'S'
128200970530     C     VIDPIS        OREQ      'N'
128300970530X   2C                   ELSE
128400970530     C                   MOVEL     '1'           ERRL00
128500970530     C                   SETON                                        78
128600970530     C                   MOVEL     MSG(27)       DSMSMS
128700970530E   2C                   ENDIF
128800970530E   1C                   ENDIF
128900970530     C*--------------------
129000970530     C* GESTIONE DDT
129100970530     C*--------------------
129200970530IF  1C     ERRL00        IFEQ      '0'
129300970530IF  2C     VIDGDD        IFEQ      'S'
129400970530     C     VIDGDD        OREQ      'N'
129500970530X   2C                   ELSE
129600970530     C                   MOVEL     '1'           ERRL00
129700970530     C                   SETON                                        79
129800970530     C                   MOVEL     MSG(27)       DSMSMS
129900970530E   2C                   ENDIF
130000970530E   1C                   ENDIF
130100970530     C*--------------------
130200970530     C* TIPO TEMPO CONSEGNA
130300970530     C*--------------------
130400970530IF  1C     ERRL00        IFEQ      '0'
130500010322     C                   MOVEL     VIDTTC        WTTC                           *CODICE TEMPO
130600970530     C                   Z-ADD     *ZEROS        WD0I                           *TEMPI DISTRIBUZIONE
130700970530     C                   Z-ADD     *ZEROS        WD1I
130800970530     C                   Z-ADD     *ZEROS        WD0C
130900970530     C                   Z-ADD     *ZEROS        WD1C
131000970530     C                   Z-ADD     *ZEROS        WDIS
131100970530     C                   Z-ADD     *ZEROS        WDCS
131200970530     C                   Z-ADD     *ZEROS        WDLD
131300090903
131400970530     C                   Z-ADD     *ZEROS        WC0I                           *TEMPI CORRIERE
131500970530     C                   Z-ADD     *ZEROS        WC1I
131600970530     C                   Z-ADD     *ZEROS        WC0C
131700970530     C                   Z-ADD     *ZEROS        WC1C
131800970530     C                   Z-ADD     *ZEROS        WCIS
131900970530     C                   Z-ADD     *ZEROS        WCCS
132000970530     C                   Z-ADD     *ZEROS        WCLD
132100090903
132200970530     C                   Z-ADD     *ZEROS        WE0I                           *TEMPI ESPRESSO
132300970530     C                   Z-ADD     *ZEROS        WE1I
132400970530     C                   Z-ADD     *ZEROS        WE0C
132500970530     C                   Z-ADD     *ZEROS        WE1C
132600970530     C                   Z-ADD     *ZEROS        WEIS
132700970530     C                   Z-ADD     *ZEROS        WECS
132800970530     C                   Z-ADD     *ZEROS        WELD
132900090903
133000090903     C                   Z-ADD     *ZEROS        Wh0I                           *TEMPI H1030
133100090903     C                   Z-ADD     *ZEROS        Wh1I
133200090903     C                   Z-ADD     *ZEROS        Wh0C
133300090903     C                   Z-ADD     *ZEROS        Wh1C
133400090903     C                   Z-ADD     *ZEROS        WhIS
133500090903     C                   Z-ADD     *ZEROS        WhCS
133600090903     C                   Z-ADD     *ZEROS        WhLD
133700090903
133800970530     C                   EXSR      CHKTTC
133900970530     C                   MOVEL     WTTC          VIDTTC                         *CODICE TEMPO
134000970530     C                   Z-ADD     WD0I          VIDD0I                         *TEMPI DISTRIBUZIONE
134100970530     C                   Z-ADD     WD1I          VIDD1I
134200970530     C                   Z-ADD     WD0C          VIDD0C
134300970530     C                   Z-ADD     WD1C          VIDD1C
134400970530     C                   Z-ADD     WDIS          VIDDIS
134500970530     C                   Z-ADD     WDCS          VIDDCS
134600970530     C                   Z-ADD     WDLD          VIDDLD
134700090903
134800970530     C                   Z-ADD     WC0I          VIDC0I                         *TEMPI CORRIERE
134900970530     C                   Z-ADD     WC1I          VIDC1I
135000970530     C                   Z-ADD     WC0C          VIDC0C
135100970530     C                   Z-ADD     WC1C          VIDC1C
135200970530     C                   Z-ADD     WCIS          VIDCIS
135300970530     C                   Z-ADD     WCCS          VIDCCS
135400970530     C                   Z-ADD     WCLD          VIDCLD
135500090903
135600970530     C                   Z-ADD     WE0I          VIDE0I                         *TEMPI ESPRESSO
135700970530     C                   Z-ADD     WE1I          VIDE1I
135800970530     C                   Z-ADD     WE0C          VIDE0C
135900970530     C                   Z-ADD     WE1C          VIDE1C
136000970530     C                   Z-ADD     WEIS          VIDEIS
136100970530     C                   Z-ADD     WECS          VIDECS
136200970530     C                   Z-ADD     WELD          VIDELD
136300090903
136400090903     C                   Z-ADD     Wh0I          VIDh0I                         *TEMPI H1030
136500090903     C                   Z-ADD     Wh1I          VIDh1I
136600090903     C                   Z-ADD     Wh0C          VIDh0C
136700090903     C                   Z-ADD     Wh1C          VIDh1C
136800090903     C                   Z-ADD     WhIS          VIDhIS
136900090903     C                   Z-ADD     WhCS          VIDhCS
137000090903     C                   Z-ADD     WhLD          VIDhLD
137100970530IF  2C     ERRL00        IFEQ      '1'
137200970530     C                   SETON                                        80
137300970530     C                   MOVEL     MSG(30)       DSMSMS
137400970530E   2C                   ENDIF
137500970530E   1C                   ENDIF
137600010322     C*--------------------
137700010322     C* TIPO TEMPO RITIRO
137800010322     C*--------------------
137900010322IF  1C     ERRL00        IFEQ      '0'
138000010322     C                   MOVEL     VIDTTR        WTTR                           *CODICE TEMPO
138100010322     C                   Z-ADD     *ZEROS        WD0IR                          *TEMPI DISTRIBUZIONE
138200010322     C                   Z-ADD     *ZEROS        WD1IR
138300010322     C                   Z-ADD     *ZEROS        WD0CR
138400010322     C                   Z-ADD     *ZEROS        WD1CR
138500010322     C                   Z-ADD     *ZEROS        WDISR
138600010322     C                   Z-ADD     *ZEROS        WDCSR
138700010322     C                   Z-ADD     *ZEROS        WDLDR
138800010322     C                   Z-ADD     *ZEROS        WC0IR                          *TEMPI CORRIERE
138900010322     C                   Z-ADD     *ZEROS        WC1IR
139000010322     C                   Z-ADD     *ZEROS        WC0CR
139100010322     C                   Z-ADD     *ZEROS        WC1CR
139200010322     C                   Z-ADD     *ZEROS        WCISR
139300010322     C                   Z-ADD     *ZEROS        WCCSR
139400010322     C                   Z-ADD     *ZEROS        WCLDR
139500010322     C                   Z-ADD     *ZEROS        WE0IR                          *TEMPI ESPRESSO
139600010322     C                   Z-ADD     *ZEROS        WE1IR
139700010322     C                   Z-ADD     *ZEROS        WE0CR
139800010322     C                   Z-ADD     *ZEROS        WE1CR
139900010322     C                   Z-ADD     *ZEROS        WEISR
140000010322     C                   Z-ADD     *ZEROS        WECSR
140100010322     C                   Z-ADD     *ZEROS        WELDR
140200010322     C                   EXSR      CHKTTR
140300010322     C                   MOVEL     WTTR          VIDTTR                         *CODICE TEMPO
140400010322     C                   Z-ADD     WD0IR         VIDD0IR                        *TEMPI DISTRIBUZIONE
140500010322     C                   Z-ADD     WD1IR         VIDD1IR
140600010322     C                   Z-ADD     WD0CR         VIDD0CR
140700010322     C                   Z-ADD     WD1CR         VIDD1CR
140800010322     C                   Z-ADD     WDISR         VIDDISR
140900010322     C                   Z-ADD     WDCSR         VIDDCSR
141000010322     C                   Z-ADD     WDLDR         VIDDLDR
141100010322     C                   Z-ADD     WC0IR         VIDC0IR                        *TEMPI CORRIERE
141200010322     C                   Z-ADD     WC1IR         VIDC1IR
141300010322     C                   Z-ADD     WC0CR         VIDC0CR
141400010322     C                   Z-ADD     WC1CR         VIDC1CR
141500010322     C                   Z-ADD     WCISR         VIDCISR
141600010322     C                   Z-ADD     WCCSR         VIDCCSR
141700010322     C                   Z-ADD     WCLDR         VIDCLDR
141800010322     C                   Z-ADD     WE0IR         VIDE0IR                        *TEMPI ESPRESSO
141900010322     C                   Z-ADD     WE1IR         VIDE1IR
142000010322     C                   Z-ADD     WE0CR         VIDE0CR
142100010322     C                   Z-ADD     WE1CR         VIDE1CR
142200010322     C                   Z-ADD     WEISR         VIDEISR
142300010322     C                   Z-ADD     WECSR         VIDECSR
142400010322     C                   Z-ADD     WELDR         VIDELDR
142500010322IF  2C     ERRL00        IFEQ      '1'
142600010322     C                   SETON                                        44
142700010322     C                   MOVEL     MSG(32)       DSMSMS
142800010322E   2C                   ENDIF
142900010322E   1C                   ENDIF
143000130917     c*
143100130917     c                   clear                   wdel              1
143200130917     c                   exsr      sr_cpe
143300970530     C*
143400970530     C                   ENDSR
143500970618     C*------------------------------------------------------------------------*
143600970618     C* DECF06 - CONTROLLA CAMPI E DECODIFICA -SOLO ALLA CONFERMA-
143700970618     C*------------------------------------------------------------------------*
143800970618     C     DECF06        BEGSR
143900970618     C*--------------------
144000970618     C* LOCALITA' GESTITA A LIVELLO DI LOCALITA'
144100970618     C*--------------------
144200970618     C* DATI CAP + DATI LOCALITA'
144300091211IF  1C*    ERRL00        IFEQ      '0'
144400091211IF  2C*    VIDLIV        IFEQ      'L'                                          *LOCALITA' GESTITA
144500091211     C*                  MOVEL     VIDLNA        NLNA                           *CAMPI NUMERICI
144600091211     C*                  MOVEL     VIDLOS        NLOS
144700091211     C*                  MOVEL     VIDLOL        NLOL
144800091211     C*                  MOVEL     VIDLO2        NLO2
144900091211     C*                  MOVEL     VIDTTC        NTTC
145000091211     C*                  MOVE      VIDTTR        NTTR
145100091211IF  3C*    NLNA          IFEQ      CPCLNA
145200091211     C*    VIDZNC        ANDEQ     CPCZNC
145300091211     C*    VIDISO        ANDEQ     CPCISO
145400091211     C*    VIDZFD        ANDEQ     CPCZFD
145500091211     C*    VIDLKG        ANDEQ     CPCLKG
145600091211     C*    VIDLMC        ANDEQ     CPCLMC
145700091211     C*    NLOL          ANDEQ     CPCLOL
145800091211     C*    VIDZOL        ANDEQ     CPCZOL
145900091211     C*    VIDLKS        ANDEQ     CPCLKS
146000091211     C*    VIDLMS        ANDEQ     CPCLMS
146100091211     C*    NLO2          ANDEQ     CPCLO2
146200091211     C*    VIDZO2        ANDEQ     CPCZO2
146300091211     C*    VIDLK2        ANDEQ     CPCLK2
146400091211     C*    VIDLM2        ANDEQ     CPCLM2
146500091211     C*    NLOS          ANDEQ     CPCLOS
146600091211     C*    VIDZOS        ANDEQ     CPCZOS
146700091211     C*    VIDCTS        ANDEQ     CPCCTS
146800091211     C*    VIDCTE        ANDEQ     CPCCTE
146900091211     C*    NTTC          ANDEQ     CPCTTC
147000091211     C*    NTTR          ANDEQ     CPCFI2
147100091211     C*    VIDPII        ANDEQ     CPCPII
147200091211     C*    VIDPID        ANDEQ     CPCPID
147300091211     C*    VIDPIS        ANDEQ     CPCPIS
147400091211     C*    VIDGDD        ANDEQ     CPCGDD
147500091211     C*                  SETON                                        606170
147600091211     C*                  SETON                                        74
147700091211     C*                  MOVEL     MSG(29)       DSMSMS
147800091211     C*                  MOVEL     '1'           ERRL00
147900091211E   3C*                  ENDIF
148000091211E   2C*                  ENDIF
148100091211E   1C*                  ENDIF
148200970618     C*
148300970618     C* LIVELLO DI GESTIONE = CAP SE LA LOCALITA' E' UNICA
148400970618IF  1C     ERRL00        IFEQ      '0'
148500970618IF  2C     VIDLIV        IFEQ      'L'                                          *LOCALITA' GESTITA
148600970618IF  3C     DEPOP1        IFEQ      'F06'
148700970618     C                   Z-ADD     1             I
148800970618X   3C                   ELSE
148900970618     C                   Z-ADD     *ZEROS        I
149000970618E   3C                   ENDIF
149100970618     C                   Z-ADD     D00VER        KCLVER
149200970618     C                   MOVEL     VIDNAR        KCLNAR
149300970618     C                   MOVEL     VIDCAP        KCLCAP
149400970618     C     KE2CPL        SETLL     AZCPL01L
149500970618     C     KE2CPL        READE(N)  AZCPL01L                               99
149600970618DO  3C     *IN99         DOWEQ     *OFF
149700970618IF  4C     CPLATB        IFEQ      *BLANKS
149800970618     C                   ADD       1             I                              *NUMERO LOCALITA'
149900970618E   4C                   ENDIF
150000970618     C     KE2CPL        READE(N)  AZCPL01L                               99
150100970618E   3C                   ENDDO
150200970618IF  3C     I             IFEQ      1
150300970618     C                   SETON                                        606170
150400970618     C                   SETON                                        74
150500970618     C                   MOVEL     MSG(26)       DSMSMS
150600970618     C                   MOVEL     '1'           ERRL00
150700970618E   3C                   ENDIF
150800970618E   2C                   ENDIF
150900970618E   1C                   ENDIF
151000970618     C*
151100970618     C                   ENDSR
151200970530     C*------------------------------------------------------------------------*
151300970530     C* CHKNAR - CONTROLLA LA NAZIONE
151400970530     C*------------------------------------------------------------------------*
151500970530     C     CHKNAR        BEGSR
151600970530     C*
151700970530     C     *LIKE         DEFINE    VIDNAR        WNAR
151800970530     C     *LIKE         DEFINE    VDDNAR        WDNAR
151900970530     C*
152000970530     C* RICHIESTO ELENCO
152100970530     C                   SETOFF                                           28
152200970530     C     '?'           SCAN      WNAR                                   28
152300970530IF  1C     *IN28         IFEQ      *ON
152400020906     C                   Z-ADD     1             §KUT              1 0
152500970530     C                   MOVEL     '15'          §COD              2
152600970530     C                   MOVEL     *BLANKS       §KEY              8
152700970530     C                   MOVEL     *BLANKS       §DES             30
152800970530     C                   CALL      'X§TABER'
152900970530     C                   PARM                    §KUT
153000970530     C                   PARM                    §COD
153100970530     C                   PARM                    §KEY
153200970530     C                   PARM                    §DES
153300970530     C                   MOVEL     §KEY          WNAR
153400970530     C                   MOVEL     §DES          WDNAR
153500970530E   1C                   ENDIF
153600970530     C*
153700970530     C* CONTROLLA ESISTENZA CODICE
153800020906     C                   MOVEL     1             KTBKUT
153900970530     C                   MOVEL     '15'          KTBCOD
154000970530     C                   MOVEL(P)  WNAR          KTBKEY
154100970530     C     KEYTAB        CHAIN     TABEL00F                           99
154200970530IF  1C     *IN99         IFEQ      *OFF
154300970530     C     TBLFLG        ANDEQ     *BLANKS
154400970530     C                   MOVEL     TBLUNI        DS15
154500970530     C                   MOVEL     §15DES        WDNAR
154600970530     C*
154700970530     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
154800970530IF  2C     VIDNAR        IFEQ      'I  '
154900970530     C                   MOVEL     '1'           ERRL00
155000970530     C                   MOVEL     *BLANKS       WDNAR
155100970530E   2C                   ENDIF
155200970530X   1C                   ELSE
155300970530     C                   MOVEL     '1'           ERRL00
155400970530E   1C                   ENDIF
155500970530     C*
155600970530     C                   ENDSR
155700970530     C*------------------------------------------------------------------------*
155800970530     C* CHKCAP - CONTROLLA IL CAP
155900970530     C*------------------------------------------------------------------------*
156000970530     C     CHKCAP        BEGSR
156100970530     C*
156200970530     C     *LIKE         DEFINE    VIDCAP        WCAP
156300970917     C     *LIKE         DEFINE    O95CF1        WFI1
156400970626     C*
156500970626     C* CONTROLLA IL CAP
156600970626     C                   CLEAR                   DSSI95
156700970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
156800970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
156900970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
157000970626     C                   MOVEL     WCAP          I95CAP                         *CAP
157100970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
157200970626     C                   CALL      'TISI95R'
157300970626     C                   PARM                    DSSI95
157400970626IF  1C     O95ERR        IFNE      *BLANKS
157500970626     C                   MOVEL     '1'           ERRL00
157600970917X   1C                   ELSE
157700970917     C                   MOVEL     O95CF1        WFI1
157800970917E   1C                   ENDIF
157900970530     C*
158000970530     C                   ENDSR
158100970530     C*------------------------------------------------------------------------*
158200970530     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
158300970530     C*------------------------------------------------------------------------*
158400970530     C     CHKLIN        BEGSR
158500970530     C*
158600970530     C     *LIKE         DEFINE    VIDLNA        WLIN
158700970530     C     *LIKE         DEFINE    VDDLNA        WDLIN
158800970530     C*
158900970530     C* RICHIESTO ELENCO
159000970530     C                   SETOFF                                           28
159100970530     C     '?'           SCAN      WLIN                                   28
159200970530IF  1C     *IN28         IFEQ      *ON
159300970530     C                   MOVEL     *BLANKS       §COD1             3
159400970530     C                   MOVEL     *BLANKS       §TIP              1
159500970530     C                   MOVEL     *BLANKS       §DES1            25
159600971203     C                   MOVEL     *BLANKS       §PRA              3
159700971203     C                   CALL      'TNSD24R'
159800970530     C                   PARM                    §COD1
159900970530     C                   PARM                    §TIP
160000970530     C                   PARM                    §DES1
160100971203     C                   PARM                    §PRA
160200970530     C                   MOVEL     §COD1         WLIN
160300970530     C                   MOVEL     §DES1         WDLIN
160400970530E   1C                   ENDIF
160500970530     C*
160600970530     C* SOLO CARATTERI NUMERICI
160700970530     C                   SETOFF                                       28
160800970530     C                   TESTN                   WLIN                 28
160900970530IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
161000970530     C                   MOVEL     '1'           ERRL00
161100970530X   1C                   ELSE                                                   *E'NUMERO DA TESTN
161200970530     C                   MOVE      WLIN          W001              1
161300970530IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
161400970530     C                   MOVEL     '1'           ERRL00
161500970530X   2C                   ELSE                                                   *NUMERO
161600970530     C*
161700970530     C* CONTROLLA ESISTENZA CODICE
161800970530     C                   MOVEL     WLIN          KORFIL
161900970530     C     KEYORG        CHAIN     AZORG01L                           99
162000970530IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
162100970530     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
162200970530     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
162300970530     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
162400970530X   3C                   ELSE
162500970530     C                   MOVEL     '1'           ERRL00
162600970530E   3C                   ENDIF
162700970530E   2C                   ENDIF
162800970530E   1C                   ENDIF
162900970530     C*
163000970530     C                   ENDSR
163100970530     C*------------------------------------------------------------------------*
163200970530     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
163300970530     C*------------------------------------------------------------------------*
163400970530     C     CHKZNC        BEGSR
163500970530     C*
163600970530     C     *LIKE         DEFINE    VIDZNC        WZNC
163700970530     C*
163800970530     C* CONTROLLA ESISTENZA CODICE
163900020906     C                   Z-ADD     1             KTBKUT
164000970530     C                   MOVEL     '03'          KTBCOD
164100970530     C                   MOVEL     WLIN          WA5               5
164200970530     C                   MOVE      WZNC          WA5
164300970530     C                   MOVEL(P)  WA5           KTBKEY
164400970530     C     KEYTAB        CHAIN     TABEL00F                           99
164500970530IF  1C     *IN99         IFEQ      *OFF
164600970530     C     TBLFLG        ANDEQ     *BLANKS
164700970530     C                   MOVEL     TBLUNI        DS03
164800970530X   1C                   ELSE
164900970530     C                   MOVEL     '1'           ERRL00
165000970530E   1C                   ENDIF
165100970530     C*
165200970530     C                   ENDSR
165300970530     C*------------------------------------------------------------------------*
165400970530     C* CHKCTS - CONTROLLA IL CODICE TASSAZIONE
165500970530     C*------------------------------------------------------------------------*
165600970530     C     CHKCTS        BEGSR
165700970530     C*
165800970530     C     *LIKE         DEFINE    VIDCTS        WCTS
165900970530     C     *LIKE         DEFINE    VDDCTS        WDCTS
166000970530     C*
166100970530     C* RICHIESTO ELENCO
166200970530     C                   SETOFF                                           28
166300970530     C     '?'           SCAN      WCTS                                   28
166400970530IF  1C     *IN28         IFEQ      *ON
166500020906     C                   Z-ADD     1             §KUT              1 0
166600970530     C                   MOVEL     'CT'          §COD              2
166700970530     C                   MOVEL     *BLANKS       §KEY              8
166800970530     C                   MOVEL     *BLANKS       §DES             30
166900970530     C                   CALL      'X§TABER'
167000970530     C                   PARM                    §KUT
167100970530     C                   PARM                    §COD
167200970530     C                   PARM                    §KEY
167300970530     C                   PARM                    §DES
167400970530     C                   MOVEL     §KEY          WCTS
167500970530     C                   MOVEL     §DES          WDCTS
167600970530E   1C                   ENDIF
167700970530     C*
167800970530     C* CONTROLLA ESISTENZA CODICE
167900020906     C                   MOVEL     1             KTBKUT
168000970530     C                   MOVEL     'CT'          KTBCOD
168100970530     C                   MOVEL(P)  WCTS          KTBKEY
168200970530     C     KEYTAB        CHAIN     TABEL00F                           99
168300970530IF  1C     *IN99         IFEQ      *OFF
168400970530     C     TBLFLG        ANDEQ     *BLANKS
168500970530     C                   MOVEL     TBLUNI        DSCT
168600970530     C                   MOVEL     §CTDES        WDCTS
168700970530X   1C                   ELSE
168800970530     C                   MOVEL     '1'           ERRL00
168900970530E   1C                   ENDIF
169000970530     C*
169100970530     C                   ENDSR
169200970530     C*------------------------------------------------------------------------*
169300970530     C* CHKINL - CONTROLLA L'INOLTRO
169400970530     C*------------------------------------------------------------------------*
169500970530     C     CHKINL        BEGSR
169600970530     C*
169700970530     C     *LIKE         DEFINE    VIDISO        WINL
169800970530     C     *LIKE         DEFINE    VDDISO        WDINL
169900970530     C*
170000970530     C* RICHIESTO ELENCO
170100970530     C                   SETOFF                                           28
170200970530     C     '?'           SCAN      WINL                                   28
170300970530IF  1C     *IN28         IFEQ      *ON
170400020906     C                   Z-ADD     1             §KUT              1 0
170500970530     C                   MOVEL     '9A'          §COD              2
170600970530     C                   MOVEL     *BLANKS       §KEY              8
170700970530     C                   MOVEL     *BLANKS       §DES             30
170800970530     C                   CALL      'X§TABER'
170900970530     C                   PARM                    §KUT
171000970530     C                   PARM                    §COD
171100970530     C                   PARM                    §KEY
171200970530     C                   PARM                    §DES
171300970530     C                   MOVEL     §KEY          WINL
171400970530     C                   MOVEL     §DES          WDINL
171500970530E   1C                   ENDIF
171600970530     C*
171700970530     C* CONTROLLA ESISTENZA CODICE
171800020906     C                   MOVEL     1             KTBKUT
171900970530     C                   MOVEL     '9A'          KTBCOD
172000970530     C                   MOVEL(P)  WINL          KTBKEY
172100970530     C     KEYTAB        CHAIN     TABEL00F                           99
172200970530IF  1C     *IN99         IFEQ      *OFF
172300970530     C     TBLFLG        ANDEQ     *BLANKS
172400970530     C                   MOVEL     TBLUNI        DS9A
172500970530     C                   MOVEL     §9ADES        WDINL
172600970530X   1C                   ELSE
172700970530     C                   MOVEL     '1'           ERRL00
172800970530E   1C                   ENDIF
172900110623     c* Se non utilizzabile  nel cappario errore
173000110623     c                   if        §9auticpc='N'  and errl00='0'
173100110623     C                   MOVEL     '1'           ERRL00
173200110623E   1C                   ENDIF
173300970530     C*
173400970530     C                   ENDSR
173500970530     C*------------------------------------------------------------------------*
173600970530     C* CHKTTC - CONTROLLA IL TIPO TEMPO CONSEGNA
173700970530     C*------------------------------------------------------------------------*
173800970530     C     CHKTTC        BEGSR
173900970530     C*
174000970530     C     *LIKE         DEFINE    VIDTTC        WTTC                           *TIPO TEMPO
174100970530     C     *LIKE         DEFINE    CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
174200970530     C     *LIKE         DEFINE    CPT1IN        WD1I
174300970530     C     *LIKE         DEFINE    CPT0CN        WD0C
174400970530     C     *LIKE         DEFINE    CPT1CN        WD1C
174500970530     C     *LIKE         DEFINE    CPTTIS        WDIS
174600970530     C     *LIKE         DEFINE    CPTTCS        WDCS
174700970530     C     *LIKE         DEFINE    CPTTLD        WDLD
174800970530     C     *LIKE         DEFINE    CPT0IN        WC0I                           *TEMPI CORRIERE
174900970530     C     *LIKE         DEFINE    CPT1IN        WC1I
175000970530     C     *LIKE         DEFINE    CPT0CN        WC0C
175100970530     C     *LIKE         DEFINE    CPT1CN        WC1C
175200970530     C     *LIKE         DEFINE    CPTTIS        WCIS
175300970530     C     *LIKE         DEFINE    CPTTCS        WCCS
175400970530     C     *LIKE         DEFINE    CPTTLD        WCLD
175500090903
175600970530     C     *LIKE         DEFINE    CPT0IN        WE0I                           *TEMPI ESPRESSO
175700970530     C     *LIKE         DEFINE    CPT1IN        WE1I
175800970530     C     *LIKE         DEFINE    CPT0CN        WE0C
175900970530     C     *LIKE         DEFINE    CPT1CN        WE1C
176000970530     C     *LIKE         DEFINE    CPTTIS        WEIS
176100970530     C     *LIKE         DEFINE    CPTTCS        WECS
176200970530     C     *LIKE         DEFINE    CPTTLD        WELD
176300090903
176400090903     C     *LIKE         DEFINE    CPT0IN        Wh0I                           *TEMPI H1030
176500090903     C     *LIKE         DEFINE    CPT1IN        Wh1I
176600090903     C     *LIKE         DEFINE    CPT0CN        Wh0C
176700090903     C     *LIKE         DEFINE    CPT1CN        Wh1C
176800090903     C     *LIKE         DEFINE    CPTTIS        WhIS
176900090903     C     *LIKE         DEFINE    CPTTCS        WhCS
177000090903     C     *LIKE         DEFINE    CPTTLD        WhLD
177100970530     C*
177200970530     C* RICHIESTA ELENCO CODICI
177300970530     C                   SETOFF                                           28
177400970530     C     '?'           SCAN      WTTC                                   28
177500970530IF  1C     *IN28         IFEQ      *ON
177600160728     C*                  MOVEL     *BLANKS       DNAR
177700160728     C*                  MOVEL     *BLANKS       DCAP
177800160728     C*                  MOVEL     *BLANKS       DLOC
177900970602     C                   MOVEL     'T01'         WOP0
178000970530     C                   MOVEL     *BLANKS       WOP1
178100970530     C                   MOVEL     'TISI41R'     WPGM              8
178200970530     C                   EXSR      LANPGM
178300970530     C                   MOVEL     D00TTC        WTTC
178400970530E   1C                   ENDIF
178500970530     C*
178600010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO CONSEGNA
178700160727     C                   MOVEL     WTTC          DEP3A             3
178800010322     C     DEP3A         IFNE      '999'
178900970530     C* CONTROLLA ESISTENZA CODICI
179000970530     C                   Z-ADD     D00VER        KCTVER
179100970530     C                   MOVEL     WTTC          KCTTTC
179200970530     C                   MOVEL     'D'           KCTTSP
179300970530     C     KEYCPT        CHAIN     AZCPT01L                           99
179400970530IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
179500970530     C                   Z-ADD     CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
179600970530     C                   Z-ADD     CPT1IN        WD1I
179700970530     C                   Z-ADD     CPT0CN        WD0C
179800970530     C                   Z-ADD     CPT1CN        WD1C
179900970530     C                   Z-ADD     CPTTIS        WDIS
180000970530     C                   Z-ADD     CPTTCS        WDCS
180100970530     C                   Z-ADD     CPTTLD        WDLD
180200970530     C                   MOVEL     'C'           KCTTSP
180300970530     C     KEYCPT        CHAIN     AZCPT01L                           99
180400970530IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
180500970530     C                   Z-ADD     CPT0IN        WC0I                           *TEMPI CORRIERE
180600970530     C                   Z-ADD     CPT1IN        WC1I
180700970530     C                   Z-ADD     CPT0CN        WC0C
180800970530     C                   Z-ADD     CPT1CN        WC1C
180900970530     C                   Z-ADD     CPTTIS        WCIS
181000970530     C                   Z-ADD     CPTTCS        WCCS
181100970530     C                   Z-ADD     CPTTLD        WCLD
181200970530     C                   MOVEL     'E'           KCTTSP
181300970530     C     KEYCPT        CHAIN     AZCPT01L                           99
181400970530IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
181500970530     C                   Z-ADD     CPT0IN        WE0I                           *TEMPI ESPRESSO
181600970530     C                   Z-ADD     CPT1IN        WE1I
181700970530     C                   Z-ADD     CPT0CN        WE0C
181800970530     C                   Z-ADD     CPT1CN        WE1C
181900970530     C                   Z-ADD     CPTTIS        WEIS
182000970530     C                   Z-ADD     CPTTCS        WECS
182100970530     C                   Z-ADD     CPTTLD        WELD
182200090903     C                   MOVEL     'H'           KCTTSP
182300090903     C     KEYCPT        CHAIN     AZCPT01L                           99
182400090903IF  4C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
182500090903     C                   Z-ADD     CPT0IN        Wh0I                           *TEMPI H1030
182600090903     C                   Z-ADD     CPT1IN        Wh1I
182700090903     C                   Z-ADD     CPT0CN        Wh0C
182800090903     C                   Z-ADD     CPT1CN        Wh1C
182900090903     C                   Z-ADD     CPTTIS        WhIS
183000090903     C                   Z-ADD     CPTTCS        WhCS
183100090903     C                   Z-ADD     CPTTLD        WhLD
183200090903X   4C                   ELSE
183300090903
183400090903     c* Se tipo servizio H non ancora decorrente (anche guardando alla
183500090903     c*    scadenza della versione in visualizzazione)non do errore e non
183600090903     c*    visualizzo nemmeno come descrizione
183700090903
183800170116     c**                 if        Datasys>=dataiso
183900090903     c* Verifico anche la  scadenza ddella versione
184000170116     c**   d00ver        chain     azcpr01l
184100170116     c**                 if        not %found(azcpr01l) or cprdsc>=§vpotsph
184200170116
184300090903     C                   MOVEL     '1'           ERRL00
184400170116
184500170116     c**                 endif
184600170116     c**                 endif
184700090903     c
184800170116     c**                 if        errl00='0'
184900170116     c**                 clear                   viddtsph
185000170116E   4C**                 ENDIF
185100090903
185200090903E   4C                   ENDIF
185300090903
185400970603X   3C                   ELSE
185500970603     C                   MOVEL     '1'           ERRL00
185600970603E   3C                   ENDIF
185700090903
185800970603X   2C                   ELSE
185900970603     C                   MOVEL     '1'           ERRL00
186000970603E   2C                   ENDIF
186100090903
186200970603X   1C                   ELSE
186300970603     C                   MOVEL     '1'           ERRL00
186400970603E   1C                   ENDIF
186500090903
186600010322     C                   ELSE
186700010322     C                   MOVEL     '1'           ERRL00
186800010322     C                   ENDIF
186900970530     C*
187000970530     C                   ENDSR
187100010322     C*------------------------------------------------------------------------*
187200010322     C* CHKTTR - CONTROLLA IL TIPO TEMPO RITIRO
187300010322     C*------------------------------------------------------------------------*
187400010322     C     CHKTTR        BEGSR
187500010322     C*
187600010322     C     *LIKE         DEFINE    VIDTTR        WTTR                           *TIPO TEMPO
187700010322     C     *LIKE         DEFINE    CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
187800010322     C     *LIKE         DEFINE    CPT1IN        WD1IR
187900010322     C     *LIKE         DEFINE    CPT0CN        WD0CR
188000010322     C     *LIKE         DEFINE    CPT1CN        WD1CR
188100010322     C     *LIKE         DEFINE    CPTTIS        WDISR
188200010322     C     *LIKE         DEFINE    CPTTCS        WDCSR
188300010322     C     *LIKE         DEFINE    CPTTLD        WDLDR
188400010322     C     *LIKE         DEFINE    CPT0IN        WC0IR                          *TEMPI CORRIERE
188500010322     C     *LIKE         DEFINE    CPT1IN        WC1IR
188600010322     C     *LIKE         DEFINE    CPT0CN        WC0CR
188700010322     C     *LIKE         DEFINE    CPT1CN        WC1CR
188800010322     C     *LIKE         DEFINE    CPTTIS        WCISR
188900010322     C     *LIKE         DEFINE    CPTTCS        WCCSR
189000010322     C     *LIKE         DEFINE    CPTTLD        WCLDR
189100010322     C     *LIKE         DEFINE    CPT0IN        WE0IR                          *TEMPI ESPRESSO
189200010322     C     *LIKE         DEFINE    CPT1IN        WE1IR
189300010322     C     *LIKE         DEFINE    CPT0CN        WE0CR
189400010322     C     *LIKE         DEFINE    CPT1CN        WE1CR
189500010322     C     *LIKE         DEFINE    CPTTIS        WEISR
189600010322     C     *LIKE         DEFINE    CPTTCS        WECSR
189700010322     C     *LIKE         DEFINE    CPTTLD        WELDR
189800010322     C*
189900010322     C* RICHIESTA ELENCO CODICI
190000010322     C                   SETOFF                                           28
190100010322     C     '?'           SCAN      WTTR                                   28
190200010322IF  1C     *IN28         IFEQ      *ON
190300160728     C*                  MOVEL     *BLANK        DNAR
190400160728     C*                  MOVEL     *BLANKS       DCAP
190500010322     C                   MOVEL     'T01'         WOP0
190600010322     C                   MOVEL     *BLANKS       WOP1
190700010323     C                   MOVEL     'TISI41R'     WPGM              8
190800010322     C                   EXSR      LANPGM
190900010322     C                   MOVEL     D00TTC        WTTR
191000010322E   1C                   ENDIF
191100010322     C*
191200010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO RITIRO
191300160728     C                   MOVEL     wttr          DEP3A             3
191400010322     C     DEP3A         IFEQ      '999'
191500010322     C* CONTROLLA ESISTENZA CODICI
191600010322     C                   Z-ADD     D00VER        KCTVER
191700010322     C                   MOVEL     WTTR          KCTTTC
191800010322     C                   MOVEL     'D'           KCTTSP
191900010322     C     KEYCPT        CHAIN     AZCPT01L                           99
192000010322IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
192100010322     C                   Z-ADD     CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
192200010322     C                   Z-ADD     CPT1IN        WD1IR
192300010322     C                   Z-ADD     CPT0CN        WD0CR
192400010322     C                   Z-ADD     CPT1CN        WD1CR
192500010322     C                   Z-ADD     CPTTIS        WDISR
192600010322     C                   Z-ADD     CPTTCS        WDCSR
192700010322     C                   Z-ADD     CPTTLD        WDLDR
192800010322     C                   MOVEL     'C'           KCTTSP
192900010322     C     KEYCPT        CHAIN     AZCPT01L                           99
193000010322IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
193100010322     C                   Z-ADD     CPT0IN        WC0IR                          *TEMPI CORRIERE
193200010322     C                   Z-ADD     CPT1IN        WC1IR
193300010322     C                   Z-ADD     CPT0CN        WC0CR
193400010322     C                   Z-ADD     CPT1CN        WC1CR
193500010322     C                   Z-ADD     CPTTIS        WCISR
193600010322     C                   Z-ADD     CPTTCS        WCCSR
193700010322     C                   Z-ADD     CPTTLD        WCLDR
193800010322     C                   MOVEL     'E'           KCTTSP
193900010322     C     KEYCPT        CHAIN     AZCPT01L                           99
194000010322IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
194100010322     C                   Z-ADD     CPT0IN        WE0IR                          *TEMPI ESPRESSO
194200010322     C                   Z-ADD     CPT1IN        WE1IR
194300010322     C                   Z-ADD     CPT0CN        WE0CR
194400010322     C                   Z-ADD     CPT1CN        WE1CR
194500010322     C                   Z-ADD     CPTTIS        WEISR
194600010322     C                   Z-ADD     CPTTCS        WECSR
194700010322     C                   Z-ADD     CPTTLD        WELDR
194800010322X   3C                   ELSE
194900010322     C                   MOVEL     '1'           ERRL00
195000010322E   3C                   ENDIF
195100010322X   2C                   ELSE
195200010322     C                   MOVEL     '1'           ERRL00
195300010322E   2C                   ENDIF
195400010322X   1C                   ELSE
195500010322     C                   MOVEL     '1'           ERRL00
195600010322E   1C                   ENDIF
195700010322     C                   ELSE
195800010322     C                   MOVEL     '1'           ERRL00
195900010322     C                   ENDIF
196000010322     C*
196100010322     C                   ENDSR
196200970526     C*------------------------------------------------------------------------*
196300970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
196400970526     C*------------------------------------------------------------------------*
196500970526     C     IREVID        BEGSR
196600970526     C* CAMPI
196700970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
196800970530IF  1C     CPLESS        IFEQ      'S'
196900970530     C                   MOVEL     'SIN'         VDDESS
197000970530E   1C                   ENDIF
197100970530     C                   MOVEL     CPLNAR        VIDNAR
197200970530     C                   MOVEL     CPLCAP        VIDCAP
197300970530     C                   MOVEL     CPLLOC        VIDLOC
197400970530     C                   MOVEL     CPLLIV        VIDLIV
197500970530     C                   MOVEL     CPLDLV        VIDDLV
197600970917     C                   MOVEL     CPLFI1        VIDFI1
197700970530     C                   MOVEL     CPLLNA        VIDLNA
197800970530     C                   MOVEL     CPLLOL        VIDLOL
197900971118     C                   MOVEL     CPLLO2        VIDLO2
198000970604     C                   MOVEL     CPLLOS        VIDLOS
198100970530     C                   MOVEL     CPLZNC        VIDZNC
198200970530     C                   MOVEL     CPLZOL        VIDZOL
198300971118     C                   MOVEL     CPLZO2        VIDZO2
198400970604     C                   MOVEL     CPLZOS        VIDZOS
198500970530     C                   MOVEL     CPLLNA        VIDLFD
198600970530     C                   MOVEL     CPLZFD        VIDZFD
198700970530     C                   Z-ADD     CPLLKG        VIDLKG
198800970530     C                   Z-ADD     CPLLMC        VIDLMC
198900970604     C                   Z-ADD     CPLLKS        VIDLKS
199000970604     C                   Z-ADD     CPLLMS        VIDLMS
199100971118     C                   Z-ADD     CPLLK2        VIDLK2
199200971118     C                   Z-ADD     CPLLM2        VIDLM2
199300970530     C                   MOVEL     CPLCTS        VIDCTS
199400970530     C                   MOVEL     CPLCTE        VIDCTE
199500970530     C                   MOVEL     CPLISO        VIDISO
199600970530     C                   MOVEL     CPLPII        VIDPII
199700970530     C                   MOVEL     CPLPID        VIDPID
199800970530     C                   MOVEL     CPLPIS        VIDPIS
199900970530     C                   MOVEL     CPLGDD        VIDGDD
200000970530     C                   MOVEL     CPLTTC        VIDTTC
200100010322     C                   MOVEL     *all'9'       VIDTTR
200200010322     C                   MOVE      CPLFI2        VIDTTR
200300970530     C*
200400970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> IMPORTA I DATI DEL CAPPARIO
200500970530IF  2C     VIDLIV        IFEQ      'C'
200600970530     C                   SETON                                        92        *PROTEZIONE CAMPI
200700970530     C                   EXSR      IMPCPC
200800970530E   2C                   ENDIF
200900130916
201000130916     C                   MOVEL     VIDLNA        F_LNA
201100130916     C                   MOVEL     VIDLOL        F_LOL
201200130916     C                   MOVEL     VIDLOS        F_LOS
201300970530     C*
201400970526     C                   ENDSR
201500970526     C*------------------------------------------------------------------------*
201600130916     C* AZZVID - AZZERA I CAMPI DEL VIDEO
201700970526     C*------------------------------------------------------------------------*
201800970526     C     AZZVID        BEGSR
201900970526     C* CAMPI
202000970530     C                   SETOFF                                       92        *NO PROTEZIONE CAMPI
202100970530     C                   MOVEL     *BLANKS       VDDESS
202200970603     C                   MOVEL     D00NAR        VIDNAR
202300970603     C                   MOVEL     D00CAP        VIDCAP
202400970530     C                   MOVEL     *BLANKS       VIDLOC
202500970530     C                   MOVEL     *BLANKS       VIDLIV
202600970530     C                   MOVEL     *BLANKS       VIDDLV
202700970917     C                   MOVEL     *BLANKS       VIDFI1
202800970530     C                   MOVEL     *ZEROS        VIDLNA
202900970530     C                   MOVEL     *ZEROS        VIDLOL
203000971118     C                   MOVEL     *ZEROS        VIDLO2
203100970604     C                   MOVEL     *ZEROS        VIDLOS
203200970530     C                   MOVEL     *ZEROS        VIDZNC
203300970530     C                   MOVEL     *ZEROS        VIDZOL
203400971118     C                   MOVEL     *ZEROS        VIDZO2
203500970604     C                   MOVEL     *ZEROS        VIDZOS
203600970530     C                   MOVEL     *ZEROS        VIDLFD
203700970530     C                   MOVEL     *ZEROS        VIDZFD
203800970530     C                   Z-ADD     *ZEROS        VIDLKG
203900970530     C                   Z-ADD     *ZEROS        VIDLMC
204000970604     C                   Z-ADD     *ZEROS        VIDLKS
204100970604     C                   Z-ADD     *ZEROS        VIDLMS
204200971118     C                   Z-ADD     *ZEROS        VIDLK2
204300971118     C                   Z-ADD     *ZEROS        VIDLM2
204400970530     C                   MOVEL     *BLANKS       VIDCTS
204500970530     C                   MOVEL     *BLANKS       VIDCTE
204600970530     C                   MOVEL     *BLANKS       VIDISO
204700970530     C                   MOVEL     *BLANKS       VIDPII
204800970530     C                   MOVEL     *BLANKS       VIDPID
204900970530     C                   MOVEL     *BLANKS       VIDPIS
205000970530     C                   MOVEL     *BLANKS       VIDGDD
205100970530     C                   MOVEL     *ZEROS        VIDTTC
205200970526     C* DECODIFICHE
205300970527     C                   MOVEL     *BLANKS       VDDNAR
205400970527     C                   MOVEL     *BLANKS       VDDLNA
205500970527     C                   MOVEL     *BLANKS       VDDLOL
205600971118     C                   MOVEL     *BLANKS       VDDLO2
205700970604     C                   MOVEL     *BLANKS       VDDLOS
205800970530     C                   MOVEL     *BLANKS       VDDLFD
205900970530     C                   MOVEL     *BLANKS       VDDCTS
206000970530     C                   MOVEL     *BLANKS       VDDCTE
206100130916     C                   MOVEL     *BLANKS       VDDISO
206200130916     c* campi  di comodo
206300130916     C                   clear                   F_LNA
206400130916     C                   clear                   F_LOL
206500130916     C                   clear                   F_LOS
206600970526     C*
206700970526     C                   ENDSR
206800970530     C*------------------------------------------------------------------------*
206900000426     C* IMPCPC - IMPORTA I DATI DEL CAPPARIO
207000970530     C*------------------------------------------------------------------------*
207100970530     C     IMPCPC        BEGSR
207200970530     C*
207300970530     C                   MOVEL     D00VER        KCCVER
207400970530     C                   MOVEL     VIDNAR        KCCNAR
207500970530     C                   MOVEL     VIDCAP        KCCCAP
207600970530     C     KEYCPC        CHAIN     AZCPC01L                           99
207700970530IF  1C     *IN99         IFEQ      *OFF                                         *TROVATO
207800970530     C                   MOVEL     CPCLNA        VIDLNA
207900970530     C                   MOVEL     CPCLOL        VIDLOL
208000971118     C                   MOVEL     CPCLO2        VIDLO2
208100970604     C                   MOVEL     CPCLOS        VIDLOS
208200970530     C                   MOVEL     CPCZNC        VIDZNC
208300970530     C                   MOVEL     CPCZOL        VIDZOL
208400971118     C                   MOVEL     CPCZO2        VIDZO2
208500970604     C                   MOVEL     CPCZOS        VIDZOS
208600970530     C                   MOVEL     CPCLNA        VIDLFD
208700970530     C                   MOVEL     CPCZFD        VIDZFD
208800970530     C                   Z-ADD     CPCLKG        VIDLKG
208900970530     C                   Z-ADD     CPCLMC        VIDLMC
209000970604     C                   Z-ADD     CPCLKS        VIDLKS
209100970604     C                   Z-ADD     CPCLMS        VIDLMS
209200971118     C                   Z-ADD     CPCLK2        VIDLK2
209300971118     C                   Z-ADD     CPCLM2        VIDLM2
209400970530     C                   MOVEL     CPCCTS        VIDCTS
209500970530     C                   MOVEL     CPCCTE        VIDCTE
209600970530     C                   MOVEL     CPCISO        VIDISO
209700970530     C                   MOVEL     CPCPII        VIDPII
209800970530     C                   MOVEL     CPCPID        VIDPID
209900970530     C                   MOVEL     CPCPIS        VIDPIS
210000970530     C                   MOVEL     CPCGDD        VIDGDD
210100970530     C                   MOVEL     CPCTTC        VIDTTC
210200010705     C                   MOVEL     *all'9'       VIDTTR
210300010705     C                   MOVE      CPCFI2        VIDTTR
210400970530X   1C                   ELSE                                                   *NON TROVATO
210500970530     C                   MOVEL     *ZEROS        VIDLNA
210600970530     C                   MOVEL     *ZEROS        VIDLOL
210700971118     C                   MOVEL     *ZEROS        VIDLO2
210800970604     C                   MOVEL     *ZEROS        VIDLOS
210900970530     C                   MOVEL     *ZEROS        VIDZNC
211000970530     C                   MOVEL     *ZEROS        VIDZOL
211100971118     C                   MOVEL     *ZEROS        VIDZO2
211200970604     C                   MOVEL     *ZEROS        VIDZOS
211300970530     C                   MOVEL     *ZEROS        VIDLFD
211400970530     C                   MOVEL     *ZEROS        VIDZFD
211500970530     C                   Z-ADD     *ZEROS        VIDLKG
211600970530     C                   Z-ADD     *ZEROS        VIDLMC
211700970604     C                   Z-ADD     *ZEROS        VIDLKS
211800970604     C                   Z-ADD     *ZEROS        VIDLMS
211900971118     C                   Z-ADD     *ZEROS        VIDLK2
212000971118     C                   Z-ADD     *ZEROS        VIDLM2
212100970530     C                   MOVEL     *BLANKS       VIDCTS
212200970530     C                   MOVEL     *BLANKS       VIDCTE
212300970530     C                   MOVEL     *BLANKS       VIDISO
212400970530     C                   MOVEL     *BLANKS       VIDPII
212500970530     C                   MOVEL     *BLANKS       VIDPID
212600970530     C                   MOVEL     *BLANKS       VIDPIS
212700970530     C                   MOVEL     *BLANKS       VIDGDD
212800970530     C                   MOVEL     *ZEROS        VIDTTC
212900010705     C                   MOVEL     *BLANKS       VIDTTR
213000970530E   1C                   ENDIF
213100000426     C*
213200970530     C                   ENDSR
213300970526     C*------------------------------------------------------------------------*
213400970530     C* IMPCPL - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
213500970526     C*------------------------------------------------------------------------*
213600970530     C     IMPCPL        BEGSR
213700970526     C*
213800970530     C                   MOVEL     VIDNAR        CPLNAR
213900970530     C                   MOVEL     VIDCAP        CPLCAP
214000970530     C                   MOVEL     VIDLOC        CPLLOC
214100970530     C                   MOVEL     VIDLIV        CPLLIV
214200970530     C                   MOVEL     VIDDLV        CPLDLV
214300970917     C                   MOVEL     VIDFI1        CPLFI1
214400010322     C                   MOVE(p)   VIDTTR        CPLFI2
214500970530     C*
214600971118     C* LOCALITA' GESTITA A LIVELLO DI LOCALITA' -> SCRIVE I DATI VIDEO SUL FILE
214700970530IF  1C     VIDLIV        IFEQ      'L'
214800970530     C                   MOVEL     VIDLNA        CPLLNA
214900970530     C                   MOVEL     VIDLOL        CPLLOL
215000971118     C                   MOVEL     VIDLO2        CPLLO2
215100970604     C                   MOVEL     VIDLOS        CPLLOS
215200970530     C                   MOVEL     VIDZNC        CPLZNC
215300970530     C                   MOVEL     VIDZOL        CPLZOL
215400971118     C                   MOVEL     VIDZO2        CPLZO2
215500970604     C                   MOVEL     VIDZOS        CPLZOS
215600970530     C                   MOVEL     VIDZFD        CPLZFD
215700970530     C                   Z-ADD     VIDLKG        CPLLKG
215800970530     C                   Z-ADD     VIDLMC        CPLLMC
215900970604     C                   Z-ADD     VIDLKS        CPLLKS
216000970604     C                   Z-ADD     VIDLMS        CPLLMS
216100971118     C                   Z-ADD     VIDLK2        CPLLK2
216200971118     C                   Z-ADD     VIDLM2        CPLLM2
216300970530     C                   MOVEL     VIDCTS        CPLCTS
216400970530     C                   MOVEL     VIDCTE        CPLCTE
216500970530     C                   MOVEL     VIDISO        CPLISO
216600970603     C                   MOVEL     VIDPII        CPLPII
216700970603IF  3C     CPLISO        IFEQ      'I'
216800970603     C                   MOVEL     'S'           CPLPII
216900970603E   3C                   ENDIF
217000970603     C                   MOVEL     VIDPID        CPLPID
217100970603IF  3C     CPLISO        IFEQ      'D'
217200970603     C                   MOVEL     'S'           CPLPID
217300970603E   3C                   ENDIF
217400970603     C                   MOVEL     VIDPIS        CPLPIS
217500970603IF  3C     CPLISO        IFEQ      'S'
217600970603     C                   MOVEL     'S'           CPLPIS
217700970603E   3C                   ENDIF
217800970530     C                   MOVEL     VIDGDD        CPLGDD
217900970530     C                   MOVEL     VIDTTC        CPLTTC
218000970530     C*
218100970530     C* LOCALITA' GESTITA A LIVELLO DI CAP -> AZZERA I CAMPI RIFERITI AL CAP
218200970530X   1C                   ELSE
218300970530     C                   Z-ADD     *ZEROS        CPLLNA
218400970530     C                   Z-ADD     *ZEROS        CPLLOL
218500971118     C                   Z-ADD     *ZEROS        CPLLO2
218600970604     C                   Z-ADD     *ZEROS        CPLLOS
218700970530     C                   Z-ADD     *ZEROS        CPLZNC
218800970530     C                   Z-ADD     *ZEROS        CPLZOL
218900971118     C                   Z-ADD     *ZEROS        CPLZO2
219000970604     C                   Z-ADD     *ZEROS        CPLZOS
219100970530     C                   Z-ADD     *ZEROS        CPLZFD
219200970530     C                   Z-ADD     *ZEROS        CPLLKG
219300970530     C                   Z-ADD     *ZEROS        CPLLMC
219400970604     C                   Z-ADD     *ZEROS        CPLLKS
219500970604     C                   Z-ADD     *ZEROS        CPLLMS
219600971118     C                   Z-ADD     *ZEROS        CPLLK2
219700971118     C                   Z-ADD     *ZEROS        CPLLM2
219800970530     C                   MOVEL     *BLANKS       CPLCTS
219900970530     C                   MOVEL     *BLANKS       CPLCTE
220000970530     C                   MOVEL     *BLANKS       CPLISO
220100970530     C                   MOVEL     *BLANKS       CPLPII
220200970530     C                   MOVEL     *BLANKS       CPLPID
220300970530     C                   MOVEL     *BLANKS       CPLPIS
220400970530     C                   MOVEL     *BLANKS       CPLGDD
220500970530     C                   Z-ADD     *ZEROS        CPLTTC
220600010705     C                   MOVEL     *BLANKS       CPLFI2
220700970530E   1C                   ENDIF
220800970526     C*
220900970526     C                   ENDSR
221000970530     C*------------------------------------------------------------------------*
221100970530     C* LANPGM - LANCIA UN PROGRAMMA
221200970530     C*------------------------------------------------------------------------*
221300970530     C     LANPGM        BEGSR
221400970530     C*
221500970530     C* IMPOSTA LA DS DI PROCEDURA
221600970530     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
221700970530     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
221800970530     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
221900970530     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
222000970530     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
222100970530     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
222200970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
222300970602     C                   MOVEL     DCAP          D00CAP                         *CAP
222400970602     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
222500970603     C                   MOVEL     *BLANKS       D00SIN                         *ALTRI CAMPI
222600970603     C                   Z-ADD     *ZEROS        D00TTC
222700970530     C                   MOVEL     *BLANKS       D00TSP
222800970530     C                   Z-ADD     *ZEROS        D00TFP
222900970530     C                   MOVEL     *BLANKS       D00EPA
223000970530     C                   Z-ADD     *ZEROS        D00ETP
223100970530     C                   Z-ADD     *ZEROS        D00ETA
223200970530     C                   Z-ADD     *ZEROS        D00EDD
223300970530     C                   MOVEL     *BLANKS       D00SPT
223400970530     C                   Z-ADD     *ZEROS        D00SLP
223500970530     C                   Z-ADD     *ZEROS        D00SLA
223600970530     C                   MOVEL     *BLANKS       D00STS
223700970530     C                   Z-ADD     *ZEROS        D00SDD
223800970530     C*
223900970530     C                   CALL      WPGM
224000970612     C                   PARM                    KPJBA
224100970612     C                   PARM                    DSSI00
224200970530     C*
224300970530     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
224400970530IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
224500970530     C                   MOVEL     '1'           ERRL00                         *ERRORE
224600970530     C                   MOVEL     '1'           F03L00                         *FINE PGM
224700970530X   1C                   ELSE                                                   *NON PREMUTO F03
224800970530     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
224900970530E   1C                   ENDIF
225000970530     C*
225100970530     C                   ENDSR
225200970526     C*------------------------------------------------------------------------*
225300970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
225400970526     C*------------------------------------------------------------------------*
225500970526     C     IMPFUN        BEGSR
225600970603     C*
225700970603     C* SE INTERROGAZIONE/VISUALIZZAZIONE TOGLE LA GESTIONE ALLE ZONE DI CONSEGNA
225800970603IF  1C     DEPOP0        IFEQ      'C05'
225900131007IF  1C     DEPOP0        oreq      'L05'
226000970603     C     DEPOP1        OREQ      'O05'
226100970603     C                   MOVEL     FUD(14)       FUDRIG                         *AGGIORNAMENTO
226200970603     C                   MOVEL     'N'           FUDATT
226300970603     C                   MOVEL     FUDRIG        FUD(14)
226400970603E   1C                   ENDIF
226500970526     C*
226600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
226700970526IF  1C     DEPOP1        IFEQ      'O05'
226800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
226900970526     C                   MOVEL     'N'           FUDATT
227000970526     C                   MOVEL     FUDRIG        FUD(6)
227100970526E   1C                   ENDIF
227200131007     c                   if        *in10 and  %subst(depop0:2:2) = '05'
227300131007     C                   MOVEL     FUD(7)        FUDRIG                         *LOCALITA'
227400131007     C                   MOVEL     'N'           FUDATT
227500131007     C                   MOVEL     FUDRIG        FUD(7)
227600131007     C                   MOVEL     FUD(17)       FUDRIG                         *LOCALITA'
227700131007     C                   MOVEL     'N'           FUDATT
227800131007     C                   MOVEL     FUDRIG        FUD(17)
227900131007     c                   endif
228000970526     C*
228100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
228200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
228300970526     C                   EXSR      FUNGES
228400000925     C*
228500000925     C* VERIFICO CHE SOLO UTENTI "EDP*" POSSANO MODIFICARE IL CAMPO "UFFICIO POSTALE"
228600000925     C                   MOVEL     KNMUS         UTE               3
228700000925     C     UTE           IFNE      'EDP'
228800000925     C                   SETON                                        95
228900000925     C                   ENDIF
229000970526     C*
229100970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
229200970526     C*---
229300970526     C* 90 -> PROTEZIONE CHIAVE
229400970526     C* 91 -> PROTEZIONE TOTALE
229500050414     C* 94 -> PROTEZIONE COD.TASSAZIONE e TIPO INOLTRO e altri
229600050413     C*---
229700050413     C*
229800050413     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
229900050413     C                   SETON                                        94
230000170116     c                   if        wabiute='V'
230100170116     c* se manut completa solo se  czppario ancora da decorrere, verifico con
230200170116     c*  la versione
230300170116     c     d00ver        chain     azcpr01l
230400170116     c                   if        %found(azcpr01l)
230500170116     c     *iso          movel     cprdde        dataiso
230600170116     c                   if        dataiso>datasys
230700170116     c                   setoff                                       94
230800170116     c                   else
230900170116     c                   eval      wabiute='I'
231000170116     c                   endif
231100170116     c
231200170116     c                   else
231300170116     c                   eval      wabiute='I'
231400170116     c                   endif
231500170116     c                   endif
231600050413     C*
231700050413     C* VERIFICO X PRIMISSIMA COSA LE AUTORIZZAZIONI SPECIALI SULL'UTENTE CORRENTE
231800170116     C                   IF        wabiute = *blanks  or wabiute='M'
231900050413     C* TUTTO RIMANE INVARIATO
232000050413     C                   ENDIF
232100050413     C*
232200170116     C                   IF        wabiute = 'I'  or wabiute='O'
232300140206     C* FORZO SOLO INTERROGAZIONE ma permetto di modificare gli orari se c'e' "O"
232400050413     C                   EVAL      DEPOP1 = 'O05'
232500050413     C                   ENDIF
232600050413     C*
232700170116     C                   IF        wabiute = 'C'
232800050413     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
232900050413     C                   SETOFF                                       94
233000050413     C                   ENDIF
233100050413     C*
233200970530IF  1C     DEPOP0        IFEQ      'L02'                                        G E S T I O N E
233300970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
233400970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
233500970526     C                   SETOFF                                       91
233600970526E   2C                   ENDIF
233700970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
233800970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
233900970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
234000970526E   2C                   ENDIF
234100970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
234200970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
234300970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
234400970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
234500970526     C                   SETON                                        91
234600970526E   2C                   ENDIF
234700970526X   1C                   ELSE                                                   *I N T E R R O G A
234800970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
234900970526     C                   SETON                                        91
235000970526E   1C                   ENDIF
235100970526     C*
235200970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
235300970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
235400970530     C                   Z-ADD     D00VER        KCLVER
235500970530     C                   MOVEL     D00NAR        KCLNAR
235600970530     C                   MOVEL     D00CAP        KCLCAP
235700970530     C                   MOVEL     D00LOC        KCLLOC
235800970530     C     KEYCPL        CHAIN(N)  AZCPL01L                           99
235900970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
236000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
236100970526     C                   MOVEL     MSG(6)        DSMSMS
236200970526X   2C                   ELSE
236300970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
236400980128IF  3C     DEPOP1        IFEQ      'O05'
236500980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
236600980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
236700980128E   3C                   ENDIF
236800970526E   2C                   ENDIF
236900970526X   1C                   ELSE                                                   *INSERIMENTO
237000970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
237100970526E   1C                   ENDIF
237200970526     C*
237300970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
237400970526IF  1C     DEPOP1        IFEQ      'O02'
237500970526     C                   MOVEL     DEC(1)        VIDFUN
237600970526E   1C                   ENDIF
237700970526IF  1C     DEPOP1        IFEQ      'O04'
237800970526     C                   MOVEL     DEC(2)        VIDFUN
237900970526E   1C                   ENDIF
238000970526IF  1C     DEPOP1        IFEQ      'O05'
238100970526     C                   MOVEL     DEC(3)        VIDFUN
238200970526E   1C                   ENDIF
238300970526IF  1C     DEPOP1        IFEQ      'O07'
238400970526     C                   MOVEL     DEC(4)        VIDFUN
238500970526E   1C                   ENDIF
238600970526IF  1C     DEPOP1        IFEQ      'F06'
238700970526     C                   MOVEL     DEC(5)        VIDFUN
238800970526E   1C                   ENDIF
238900970526     C*
239000970526     C                   ENDSR
239100970626     C*------------------------------------------------------------------------*
239200970626     C* FINSR - OPERAZIONI FINALI
239300970626     C*------------------------------------------------------------------------*
239400970626     C     FINSR         BEGSR
239500970626     C*
239600970626     C* CHIUDE I FILE DEL "TISI95R"
239700970704     C                   CLEAR                   DSSI95
239800970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
239900970626     C                   CALL      'TISI95R'
240000970626     C                   PARM                    DSSI95
240100130917     c*
240200130917     c                   clear                   tisio7ds
240300130917     C                   MOVEL     'C'           Io7TLA
240400131016     c*****              movel     tisio7ds      kpjbu
240500130917     C                   CALL      'TISIO7R'
240600130917     C                   PARM                    kpjba
240700131016     C                   PARM                    tisio7ds
240800970626     C*
240900970626     C                   ENDSR
241000050413     C*--------------------------------------------------------------------------------------------*
241100050413     C* REPERISCE DATI UTENTE
241200050413     C*--------------------------------------------------------------------------------------------*
241300050413     C     REPDATIUTE    BEGSR
241400050413     C*
241500050413     C* REimposta le variabili di lavoro
241600050413     C                   CLEAR                   TIBS34DS
241700050413     C                   CLEAR                   AZUTEDS
241800050413     C                   CLEAR                   DDATIUTE
241900050413     C*
242000050413     C* Reperisce i dati del profilo in ingresso
242100050413     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
242200050413     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
242300050413     C                   IN(E)     *DTAARA
242400050413     C                   IF        %Error                                       *NO errori
242500050413     C                   EVAL      I34TLA = 'L'
242600050413     C                   CALL      'TIBS34R'
242700050413     C                   PARM                    TIBS34DS
242800050413     C                   IN        *DTAARA
242900050413     C                   ENDIF
243000050413     C*
243100050413     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
243200050413     C                   EVAL      DUTE01 = UTEFAF
243300050413     C*
243400050413     C                   ENDSR
243500130916     C*------------------------------------------------------------------------*
243600130916     C* Richiamo pgm annullamento orari servizi a fronte di modifica lna
243700130916     C*------------------------------------------------------------------------*
243800130916     c     sr_oraris     begsr
243900130916     C* LINEA STANDARD
244000130916     c                   movel     vidlna        w0030             3 0
244100130916     c* Se cambiata la linea
244200130916     c                   if        w0030<>f_lna
244300130916     c                   clear                   tisio7ds
244400130916     c                   eval      io7mod='D'
244500130917     c* Il livello da passare è quello a video
244600130917     c                   eval      io7liv=vidliv
244700130916     c                   eval      io7ver=d00ver
244800130916     c                   eval      io7nar=vidnar
244900130916     c                   eval      io7cap=vidcap
245000130917     c                   eval      io7loc=vidloc
245100130916     c                   eval      io7lna=f_lna
245200130916     c                   eval      io7lns=%dec(vidlna:3:0)
245300130916     c                   eval      io7los=%dec(vidlos:3:0)
245400130916     c                   eval      io7lol=%dec(vidlol:3:0)
245500131016     c******             movel     tisio7ds      kpjbu
245600130916     c                   call      'TISIO7R'
245700130916     c                   parm                    kpjba
245800131016     c                   parm                    tisio7ds
245900130916     c                   endif
246000130916     C* LINEA sotto
246100130916     c                   movel     vidlos        w0030             3 0
246200130916     c* Se cambiata la linea e non ancora richiamato pgm per la linea stessa
246300130916     c                   if        w0030<>f_los  and f_los<> f_lna
246400130916     c                   clear                   tisio7ds
246500130916     c                   eval      io7mod='D'
246600130917     c* Il livello da passare è quello a video
246700130917     c                   eval      io7liv=vidliv
246800130916     c                   eval      io7ver=d00ver
246900130916     c                   eval      io7nar=vidnar
247000130916     c                   eval      io7cap=vidcap
247100130917     c                   eval      io7loc=vidloc
247200130916     c                   eval      io7lna=f_los
247300130916     c                   eval      io7lns=%dec(vidlna:3:0)
247400130916     c                   eval      io7los=%dec(vidlos:3:0)
247500130916     c                   eval      io7lol=%dec(vidlol:3:0)
247600131016     c******             movel     tisio7ds      kpjbu
247700130916     c                   call      'TISIO7R'
247800130916     c                   parm                    kpjba
247900131016     c                   parm                    tisio7ds
248000130916     c                   endif
248100130916     C* LINEA OLTRE
248200130916     c                   movel     vidlol        w0030             3 0
248300130916     c* Se cambiata la linea e non ancora richiamato pgm per la linea stessa
248400130916     c                   if        w0030<>f_lol  and f_lol<> f_lna
248500130916     c                                           and f_lol<> f_los
248600130916     c                   clear                   tisio7ds
248700130916     c                   eval      io7mod='D'
248800130917     c* Il livello da passare è quello a video
248900130917     c                   eval      io7liv=vidliv
249000130916     c                   eval      io7ver=d00ver
249100130916     c                   eval      io7nar=vidnar
249200130916     c                   eval      io7cap=vidcap
249300130917     c                   eval      io7loc=vidloc
249400130916     c                   eval      io7lna=f_lol
249500130916     c                   eval      io7lns=%dec(vidlna:3:0)
249600130916     c                   eval      io7los=%dec(vidlos:3:0)
249700130916     c                   eval      io7lol=%dec(vidlol:3:0)
249800131016     c*****              movel     tisio7ds      kpjbu
249900130916     c                   call      'TISIO7R'
250000130916     c                   parm                    kpjba
250100131016     c                   parm                    tisio7ds
250200130916     c                   endif
250300130917     c*
250400130917     c                   eval      wdel='S'
250500130917     c                   exsr      sr_cpe
250600130917     c                   endsr
250700130917     C*------------------------------------------------------------------------*
250800130917     C* Richiamo pgm orari servizi partendo dalle eccezioni
250900130917     C*------------------------------------------------------------------------*
251000130917     c     sr_cpe        begsr
251100130917     c* Se passaggio di livello da cap a località  leggo le eccezioni
251200130917     c* e se presenti richiamo tisio7r passando le linee delle eccezioni
251300130917     c                   if        vidliv='L' and cplliv<>vidliv
251400130917     c     kcpe          setll     azcpe01l
251500130917     c     kcpe          reade     azcpe01l
251600130917     c                   dow       not %eof(azcpe01l)
251700130917     c* lna standard
251800130917     c                   move      cpelna        w003A             3
251900130917     c                   if        w003a <>vidlna and w003a <>vidlos
252000130917     c                             and w003a <>vidlol
252100130917     c                             and (f07lna=*blanks or wdel='S')
252200130917     c                   clear                   tisio7ds
252300130917     c                   if        wdel='S'
252400130917     c                   eval      io7mod='D'
252500130917     c                   else
252600130917     c                   eval      io7mod='C'
252700130917     c                   endif
252800130917     c* Il livello da passare è quello a video
252900130917     c                   eval      io7liv=vidliv
253000130917     c                   eval      io7ver=d00ver
253100130917     c                   eval      io7nar=vidnar
253200130917     c                   eval      io7cap=vidcap
253300130917     c                   eval      io7loc=vidloc
253400130917     c                   eval      io7lna=cpelna
253500130917     c                   eval      io7lns=%dec(vidlna:3:0)
253600130917     c                   eval      io7los=%dec(vidlos:3:0)
253700130917     c                   eval      io7lol=%dec(vidlol:3:0)
253800131016     c******             movel     tisio7ds      kpjbu
253900130917     c                   call      'TISIO7R'
254000130917     c                   parm                    kpjba
254100131016     c                   parm                    tisio7ds
254200130917     c                   if        oo7err='1'
254300130917     c                   movel     oo7msg        DSmsms
254400130917     c                   eval      ErrL00 = *on
254500130917     c                   eval      f07lna='S'
254600130917e   4c                   endif
254700130917     c                   endif
254800130917     c* lna sotto
254900130917     c                   move      cpelos        w003A             3
255000130917     c                   if        w003a<>vidlna and w003a <>vidlos
255100130917     c                             and w003a<>vidlol
255200130917     c                             and (f07lna=*blanks or wdel='S')
255300130917     c                   clear                   tisio7ds
255400130917     c                   if        wdel='S'
255500130917     c                   eval      io7mod='D'
255600130917     c                   else
255700130917     c                   eval      io7mod='C'
255800130917     c                   endif
255900130917     c* Il livello da passare è quello a video
256000130917     c                   eval      io7liv=vidliv
256100130917     c                   eval      io7ver=d00ver
256200130917     c                   eval      io7nar=vidnar
256300130917     c                   eval      io7cap=vidcap
256400130917     c                   eval      io7loc=vidloc
256500130917     c                   eval      io7lna=cpelos
256600130917     c                   eval      io7lns=%dec(vidlna:3:0)
256700130917     c                   eval      io7los=%dec(vidlos:3:0)
256800130917     c                   eval      io7lol=%dec(vidlol:3:0)
256900131016     c******             movel     tisio7ds      kpjbu
257000130917     c                   call      'TISIO7R'
257100130917     c                   parm                    kpjba
257200131016     c                   parm                    tisio7ds
257300130917     c                   if        oo7err='1'
257400130917     c                   movel     oo7msg        DSmsms
257500130917     c                   eval      ErrL00 = *on
257600130917     c                   eval      f07lna='S'
257700130917e   4c                   endif
257800130917e   4c                   endif
257900130917     c* lna oltre
258000130917     c                   move      cpelol        w003A             3
258100130917     c                   if        w003a <>vidlna and w003a <>vidlos
258200130917     c                             and w003a <>vidlol
258300130917     c                             and (f07lna=*blanks or wdel='S')
258400130917     c                   clear                   tisio7ds
258500130917     c                   if        wdel='S'
258600130917     c                   eval      io7mod='D'
258700130917     c                   else
258800130917     c                   eval      io7mod='C'
258900130917     c                   endif
259000130917     c* Il livello da passare è quello a video
259100130917     c                   eval      io7liv=vidliv
259200130917     c                   eval      io7ver=d00ver
259300130917     c                   eval      io7nar=vidnar
259400130917     c                   eval      io7cap=vidcap
259500130917     c                   eval      io7loc=vidloc
259600130917     c                   eval      io7lna=cpelol
259700130917     c                   eval      io7lns=%dec(vidlna:3:0)
259800130917     c                   eval      io7los=%dec(vidlos:3:0)
259900130917     c                   eval      io7lol=%dec(vidlol:3:0)
260000131016     c******             movel     tisio7ds      kpjbu
260100130917     c                   call      'TISIO7R'
260200130917     c                   parm                    kpjba
260300131016     c                   parm                    tisio7ds
260400130917     c                   if        oo7err='1'
260500130917     c                   movel     oo7msg        DSmsms
260600130917     c                   eval      ErrL00 = *on
260700130917     c                   eval      f07lna='S'
260800130917e   4c                   endif
260900130917e   4c                   endif
261000130917     c     kcpe          reade     azcpe01l
261100130917     c                   enddo
261200130917     c                   endif
261300130917     c                   endsr
261400970526     C*------------------------------------------------------------------------*
261500970526     C* *INZSR - OPERAZIONI INIZIALI
261600970526     C*------------------------------------------------------------------------*
261700970526     C     *INZSR        BEGSR
261800970526     C*---
261900970526     C* RICEVIMENTO PARAMETRI
262000970526     C*---
262100970526     C     *ENTRY        PLIST
262200970612     C                   PARM                    KPJBA
262300970612     C                   PARM                    DSSI00
262400050413     C*
262500050413     C* REPERISCE I DATI DELL'UTENTE
262600050413     C                   EXSR      REPDATIUTE
262700170116     c                   eval      wabiute=§utecap
262800970526     C*
262900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
263000970526     C     XMSGPA        PLIST
263100970526     C                   PARM                    DSMSMS
263200970526     C                   PARM                    DSMSPG
263300970526     C                   PARM                    DSMSKE
263400970526     C                   PARM                    DSMSER
263500970526     C*
263600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
263700970526     C                   CALL      'XKEY'
263800970526     C                   PARM                    DSKEY
263900131007      /free
264000131007        // Carico filiali di test abilitate
264100131007         if dutpou<>46  ;
264200131007
264300131007         clear TIBS02ds;
264400131007         T02Mod = 'C';
264500131007         T02sif = knsif;
264600131007         T02cod = 'VPO';
264700131007         t02tla = 'L'   ;
264800131007         T02ke1 ='DECOFI816' ;
264900131007         TNTBE_RicercaControllo  (kpjba : tibs02ds);
265000131007
265100131007         if  T02err  = *blank;
265200131007         xx= 1 ;
265300131007         yy= 1 ;
265400131007
265500131007         dow xx< 256   ;
265600131007           fil_ab(yy)= %subst(t02uni:xx:3)  ;
265700131007           yy=yy+ 1   ;
265800131007           xx=xx+ 3   ;
265900131007         enddo  ;
266000131007
266100131007         endif;
266200131007
266300131007         // se c'e' 999 tutte abilitate
266400131007         w003a='999'  ;
266500131007         if   %lookup(w003a:fil_ab) = 0  ;
266600131007         w003a=%editc(dutpou:'X')  ;
266700131007         if   %lookup(w003a:fil_ab) = 0  ;
266800131007         *in10 =*on  ;
266900131007         endif  ;
267000131007         endif  ;
267100131007
267200131007         endif  ;
267300131007
267400131007      /end-free
267500131007
267600090903     c*
267700090903     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
267800170116     c**                 clear                   tibs02ds
267900170116     c**                 clear                   dvpodeco
268000170116     c**                 eval      t02mod = 'C'
268100170116     c**                 eval      t02sif = knsif
268200170116     c**                 eval      t02cod = 'VPO'
268300170116     c**                 eval      t02ke1 = 'DECO'
268400170116     c**                 call      'TIBS02R'
268500170116     c**                 parm                    kpjba
268600170116     c**                 parm                    tibs02ds
268700170116     c**                 if        t02err = *blanks
268800170116     c**                 eval      dvpodeco = t02uni
268900170116     c**                 else
269000170116     c**                 eval      §vpotsph=20091001
269100170116     c**                 endif
269200090903
269300170116     c**   *iso          movel     §vpotsph      dataiso
269400090903     c
269500970526     C*---
269600970526     C* VARIABILI NON RIFERITE AL DATA BASE
269700970526     C*---
269800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
269900970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
270000970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
270100970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
270200970526     C     *LIKE         DEFINE    D00OP1        WOP1
270300970602     C     *LIKE         DEFINE    D00NAR        DNAR
270400970602     C     *LIKE         DEFINE    D00CAP        DCAP
270500970602     C     *LIKE         DEFINE    D00LOC        DLOC
270600970526     C*---
270700970526     C* VARIABILI DI CONTROLLO
270800970526     C*---
270900970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
271000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
271100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
271200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
271300970526     C*
271400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
271500970529     C                   MOVEL     'TISI31R'     DSMSPG
271600970529     C                   MOVEL     'TISI31R'     MSGQUE
271700970526     C                   MOVEL     *BLANKS       MSGKEY
271800970526     C                   MOVEL     *BLANKS       DSMSMS
271900970526     C*---
272000970526     C* VARIABILI RIFERITE AL DATA BASE
272100970526     C*---
272200970530     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
272300970530     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
272400970530     C     *LIKE         DEFINE    TBLCOD        KTBCOD
272500970530     C     *LIKE         DEFINE    TBLKEY        KTBKEY
272600970530     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
272700970530     C     *LIKE         DEFINE    CPLNAR        KCLNAR
272800970530     C     *LIKE         DEFINE    CPLCAP        KCLCAP
272900970530     C     *LIKE         DEFINE    CPLLOC        KCLLOC
273000970530     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
273100970530     C     *LIKE         DEFINE    CPCNAR        KCCNAR
273200970530     C     *LIKE         DEFINE    CPCCAP        KCCCAP
273300970530     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
273400970530     C     *LIKE         DEFINE    CPTTTC        KCTTTC
273500970530     C     *LIKE         DEFINE    CPTTSP        KCTTSP
273600970925     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
273700970925     C     *LIKE         DEFINE    CPSNAR        KCSNAR
273800970925     C     *LIKE         DEFINE    CPSCAP        KCSCAP
273900970925     C     *LIKE         DEFINE    CPSLOC        KCSLOC
274000970613     C     *LIKE         DEFINE    CPCLNA        NLNA                           *DEPOSITI NUMERICI
274100970613     C     *LIKE         DEFINE    CPCLOS        NLOS
274200970613     C     *LIKE         DEFINE    CPCLOL        NLOL
274300971118     C     *LIKE         DEFINE    CPCLO2        NLO2
274400970613     C     *LIKE         DEFINE    CPCTTC        NTTC
274500010911     C     *LIKE         DEFINE    CPCFI2        NTTR
274600970526     C*---
274700970526     C* CHIAVI DI LETTURA
274800970526     C*---
274900970530     C* LETTURA AZORG01L - COMPLETA
275000970530     C     KEYORG        KLIST
275100970530     C                   KFLD                    KORFIL                         *FILIALE
275200970530     C* LETTURA TABEL00F - COMPLETA
275300970530     C     KEYTAB        KLIST
275400970530     C                   KFLD                    KTBKUT                         *CODICE UTENTE
275500970530     C                   KFLD                    KTBCOD                         *TABELLA
275600970530     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
275700970530     C* LETTURA TABEL00F - PARZIALE
275800970530     C     KE2TAB        KLIST
275900970530     C                   KFLD                    KTBKUT                         *CODICE UTENTE
276000970530     C                   KFLD                    KTBCOD                         *TABELLA
276100970530     C* LETTURA AZCPL01L - COMPLETA
276200970530     C     KEYCPL        KLIST
276300970530     C                   KFLD                    KCLVER                         *VERSIONE
276400970530     C                   KFLD                    KCLNAR                         *NAZIONE
276500970530     C                   KFLD                    KCLCAP                         *CAP
276600970530     C                   KFLD                    KCLLOC                         *LOCALITA'
276700970613     C* LETTURA AZCPL01L - PARZIALE
276800970613     C     KE2CPL        KLIST
276900970613     C                   KFLD                    KCLVER                         *VERSIONE
277000970613     C                   KFLD                    KCLNAR                         *NAZIONE
277100970613     C                   KFLD                    KCLCAP                         *CAP
277200970527     C* LETTURA AZCPC01L - COMPLETA
277300970527     C     KEYCPC        KLIST
277400970527     C                   KFLD                    KCCVER                         *VERSIONE
277500970527     C                   KFLD                    KCCNAR                         *NAZIONE
277600970527     C                   KFLD                    KCCCAP                         *CAP
277700970530     C* LETTURA AZCPT01L - COMPLETA
277800970530     C     KEYCPT        KLIST
277900970530     C                   KFLD                    KCTVER                         *VERSIONE
278000970530     C                   KFLD                    KCTTTC                         *TEMPO CONSEGNA
278100970530     C                   KFLD                    KCTTSP                         *TIPO SERVIZIO
278200970925     C* LETTURA AZCPS01L - PARZIALE
278300970925     C     KEYCPS        KLIST
278400970925     C                   KFLD                    KCSVER                         *VERSIONE
278500970925     C                   KFLD                    KCSNAR                         *NAZIONE
278600970925     C                   KFLD                    KCSCAP                         *CAP
278700970925     C                   KFLD                    KCSLOC                         *LOCALITA
278800130917     C* LETTURA AZCpe01L -
278900130917     C     Kcpe          KLIST
279000130917     C                   KFLD                    d00ver                         *VERSIONE
279100130917     C                   KFLD                    vidnar                         *NAZIONE
279200130917     C                   KFLD                    vidcap                         *CAP
279300970526     C*
279400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
279500970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
279600970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
279700970526     C*
279800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
279900970526     C                   EXSR      IMPFUN
280000970526     C*
280100970526     C                   ENDSR
280200970526     C*------------------------------------------------------------------------*
280300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
280400970526     C*------------------------------------------------------------------------*
280500970526     C     FUNGES        BEGSR
280600970526     C*
280700970526     C* PULISCO CAMPI DI LAVORO
280800970526     C                   Z-ADD     0             W                 5 0
280900970526     C*
281000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
281100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
281200970526     C     1             DO        24            I
281300970526     C                   MOVEL     FUD(I)        FUDRIG
281400970526     C*
281500970526     C* PULIZIA FUNZIONI PRECEDENTI
281600970526     C                   MOVEL     *BLANKS       FUV(I)
281700970526     C*
281800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
281900970526     C* (SALTA F24)
282000970526     C     FUDATT        IFEQ      'S'
282100970526     C     FUDFUN        ANDNE     '24'
282200970526     C                   ADD       1             W
282300970526     C                   MOVEL     FUDDES        FUV(W)
282400970526     C                   END
282500970526     C                   END                                                    DO WHILE
282600970526     C*
282700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
282800970526     C* ALTRIMENTI DISATTIVO F24
282900970526     C     1             DO        24            I
283000970526     C                   MOVEL     FUD(I)        FUDRIG
283100970526     C*
283200970526     C     FUDFUN        IFEQ      '24'
283300970526     C     W             IFGT      6
283400970526     C                   MOVEL     'S'           FUDATT
283500970526     C                   ELSE
283600970526     C                   MOVEL     *BLANKS       FUDATT
283700970526     C                   END                                                    W GT 8
283800970526     C                   MOVEL     FUDRIG        FUD(I)
283900970526     C                   END
284000970526     C                   END                                                    DO WHILE
284100970526     C*
284200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
284300970526     C     W             DIV       6             I
284400970526     C                   MVR                     Z
284500970526     C*
284600970526     C     Z             IFNE      0
284700970526     C     I             ADD       1             Z
284800970526     C                   ELSE
284900970526     C                   Z-ADD     I             Z                 5 0
285000970526     C                   END
285100970526     C*
285200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
285300970526     C                   ADD       1             FUNGRU
285400970526     C     FUNGRU        IFGT      Z
285500970526     C     FUNGRU        ORLT      1
285600970526     C                   Z-ADD     1             FUNGRU
285700970526     C                   END
285800970526     C*
285900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
286000970526     C     FUNGRU        IFEQ      1
286100970526     C                   MOVEL     FUV(1)        FUN001
286200970526     C                   MOVEL     FUV(2)        FUN002
286300970526     C                   MOVEL     FUV(3)        FUN003
286400970526     C                   MOVEL     FUV(4)        FUN004
286500970526     C                   MOVEL     FUV(5)        FUN005
286600970526     C                   MOVEL     FUV(6)        FUN006
286700970526     C                   END
286800970526     C*
286900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
287000970526     C     FUNGRU        IFEQ      2
287100970526     C                   MOVEL     FUV(7)        FUN001
287200970526     C                   MOVEL     FUV(8)        FUN002
287300970526     C                   MOVEL     FUV(9)        FUN003
287400970526     C                   MOVEL     FUV(10)       FUN004
287500970526     C                   MOVEL     FUV(11)       FUN005
287600970526     C                   MOVEL     FUV(12)       FUN006
287700970526     C                   END
287800970526     C*
287900970526     C* TERZO GRUPPO DI 6 FUNZIONI
288000970526     C     FUNGRU        IFEQ      3
288100970526     C                   MOVEL     FUV(13)       FUN001
288200970526     C                   MOVEL     FUV(14)       FUN002
288300970526     C                   MOVEL     FUV(15)       FUN003
288400970526     C                   MOVEL     FUV(16)       FUN004
288500970526     C                   MOVEL     FUV(17)       FUN005
288600970526     C                   MOVEL     FUV(18)       FUN006
288700970526     C                   END
288800970526     C*
288900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
289000970526     C     FUNGRU        IFEQ      4
289100970526     C                   MOVEL     FUV(19)       FUN001
289200970526     C                   MOVEL     FUV(20)       FUN002
289300970526     C                   MOVEL     FUV(21)       FUN003
289400970526     C                   MOVEL     FUV(22)       FUN004
289500970526     C                   MOVEL     *BLANKS       FUN005
289600970526     C                   MOVEL     *BLANKS       FUN006
289700970526     C                   END
289800970526     C*
289900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
290000970526     C                   MOVEL     *BLANKS       FUN007
290100970526     C                   MOVEL     *BLANKS       FUN008
290200970526     C*
290300970526     C     1             DO        24            I
290400970526     C                   MOVEL     FUD(I)        FUDRIG
290500970526     C     FUDFUN        IFEQ      '24'
290600970526     C     FUDATT        ANDEQ     'S'
290700970526     C                   MOVEL     FUDDES        FUN008
290800970526     C                   END
290900970526     C                   END                                                    DO
291000970526     C*
291100970526     C                   ENDSR
291200970526     C*------------------------------------------------------------------------*
291300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
291400970526     C*------------------------------------------------------------------------*
291500970526     C     FUNCON        BEGSR
291600970526     C*
291700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
291800970526     C                   EXSR      FUNVAR
291900970526     C*
292000970526     C* PULISCO CAMPI DI LAVORO
292100970526     C                   Z-ADD     0             I                 5 0
292200970526     C                   Z-ADD     0             W                 5 0
292300970526     C*
292400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
292500970526     C     FUNKEY        IFNE      *BLANKS
292600970526     C     1             DO        24            I
292700970526     C                   MOVEL     FUD(I)        FUDRIG
292800970526     C*
292900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
293000970526     C     FUNKEY        IFEQ      FUDFUN
293100970526     C     FUDATT        ANDEQ     'S'
293200970526     C                   Z-ADD     I             W
293300970526     C                   END
293400970526     C                   END                                                    DO WHILE
293500970526     C*
293600970526     C* FUNZIONE NON ATTIVA (ERRORE)
293700970526     C     W             IFEQ      0
293800970526     C*
293900970526     C* EMISSIONE MESSAGGIO DI ERRORE
294000970526     C                   MOVEL     MSG(1)        DSMSMS
294100970526     C*
294200970526     C                   END                                                    W=0
294300970526     C                   END                                                    FUNKEY NE *BLANKS
294400970526     C*
294500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
294600970526     C     W             IFNE      0
294700970526     C     FUNKEY        OREQ      *BLANKS
294800970526     C     FUNKEY        IFEQ      *BLANKS
294900970526     C                   EXSR      ENTGES
295000970526     C                   END
295100970526     C     FUNKEY        IFEQ      '03'
295200970526     C                   EXSR      F03GES
295300970526     C                   END
295400010322     C     FUNKEY        IFEQ      '04'
295500010322     C                   EXSR      F04GES
295600010322     C                   END
295700970526     C     FUNKEY        IFEQ      '06'
295800970526     C                   EXSR      F06GES
295900970526     C                   END
296000130827     C     FUNKEY        IFEQ      '07'
296100130827     C                   EXSR      F07GES
296200130827     C                   END
296300970526     C     FUNKEY        IFEQ      '12'
296400970526     C                   EXSR      F12GES
296500970526     C                   END
296600970603     C     FUNKEY        IFEQ      '14'
296700970603     C                   EXSR      F14GES
296800970603     C                   END
296900970602     C     FUNKEY        IFEQ      '13'
297000970602     C                   EXSR      F13GES
297100970602     C                   END
297200130830     C     FUNKEY        IFEQ      '17'
297300130830     C                   EXSR      F17GES
297400130830     C                   END
297500970526     C     FUNKEY        IFEQ      '24'
297600970526     C                   EXSR      F24GES
297700970526     C                   END
297800970526     C                   END
297900970526     C*
298000970526     C                   ENDSR
298100970526     C*------------------------------------------------------------------------*
298200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
298300970526     C*------------------------------------------------------------------------*
298400970526     C     FUNVAR        BEGSR
298500970526     C*
298600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
298700970526     C*
298800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
298900970526     C     KEY           IFEQ      ENTER
299000970526     C                   MOVEL     *BLANKS       FUNKEY
299100970526     C                   END
299200970526     C     KEY           IFEQ      F1
299300970526     C                   MOVEL     '01'          FUNKEY
299400970526     C                   END
299500970526     C     KEY           IFEQ      F2
299600970526     C                   MOVEL     '02'          FUNKEY
299700970526     C                   END
299800970526     C     KEY           IFEQ      F3
299900970526     C                   MOVEL     '03'          FUNKEY
300000970526     C                   END
300100970526     C     KEY           IFEQ      F4
300200970526     C                   MOVEL     '04'          FUNKEY
300300970526     C                   END
300400970526     C     KEY           IFEQ      F5
300500970526     C                   MOVEL     '05'          FUNKEY
300600970526     C                   END
300700970526     C     KEY           IFEQ      F6
300800970526     C                   MOVEL     '06'          FUNKEY
300900970526     C                   END
301000970526     C     KEY           IFEQ      F7
301100970526     C                   MOVEL     '07'          FUNKEY
301200970526     C                   END
301300970526     C     KEY           IFEQ      F8
301400970526     C                   MOVEL     '08'          FUNKEY
301500970526     C                   END
301600970526     C     KEY           IFEQ      F9
301700970526     C                   MOVEL     '09'          FUNKEY
301800970526     C                   END
301900970526     C     KEY           IFEQ      F10
302000970526     C                   MOVEL     '10'          FUNKEY
302100970526     C                   END
302200970526     C     KEY           IFEQ      F11
302300970526     C                   MOVEL     '11'          FUNKEY
302400970526     C                   END
302500970526     C     KEY           IFEQ      F12
302600970526     C                   MOVEL     '12'          FUNKEY
302700970526     C                   END
302800970526     C     KEY           IFEQ      F13
302900970526     C                   MOVEL     '13'          FUNKEY
303000970526     C                   END
303100970526     C     KEY           IFEQ      F14
303200970526     C                   MOVEL     '14'          FUNKEY
303300970526     C                   END
303400970526     C     KEY           IFEQ      F15
303500970526     C                   MOVEL     '15'          FUNKEY
303600970526     C                   END
303700970526     C     KEY           IFEQ      F16
303800970526     C                   MOVEL     '16'          FUNKEY
303900970526     C                   END
304000970526     C     KEY           IFEQ      F17
304100970526     C                   MOVEL     '17'          FUNKEY
304200970526     C                   END
304300970526     C     KEY           IFEQ      F18
304400970526     C                   MOVEL     '18'          FUNKEY
304500970526     C                   END
304600970526     C     KEY           IFEQ      F19
304700970526     C                   MOVEL     '19'          FUNKEY
304800970526     C                   END
304900970526     C     KEY           IFEQ      F20
305000970526     C                   MOVEL     '20'          FUNKEY
305100970526     C                   END
305200970526     C     KEY           IFEQ      F21
305300970526     C                   MOVEL     '21'          FUNKEY
305400970526     C                   END
305500970526     C     KEY           IFEQ      F22
305600970526     C                   MOVEL     '22'          FUNKEY
305700970526     C                   END
305800970526     C     KEY           IFEQ      F23
305900970526     C                   MOVEL     '23'          FUNKEY
306000970526     C                   END
306100970526     C     KEY           IFEQ      F24
306200970526     C                   MOVEL     '24'          FUNKEY
306300970526     C                   END
306400970526     C     KEY           IFEQ      ROLLUP
306500970526     C                   MOVEL     'RU'          FUNKEY
306600970526     C                   END
306700970526     C     KEY           IFEQ      HELP
306800970526     C                   MOVEL     'HP'          FUNKEY
306900970526     C                   END
307000970526     C                   ENDSR
307100970526     C*------------------------------------------------------------------------*
307200970526     C* NOTE COSTRUZIONE SCHIERE
307300970526     C*------------------------------------------------------------------------*
307400970526     C*
307500970526     C* -MSG-MESSAGGI
307600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
307700970526     C* (NON TOCCARE MSG,1 E MSG,2)
307800970526     C*
307900970526     C* -CMD-COMANDI
308000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
308100970526     C*
308200970526     C* -OPD-OPZIONI
308300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
308400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
308500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
308600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
308700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
308800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
308900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
309000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
309100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
309200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
309300970526     C*
309400970526     C* -FUD-FUNZIONI
309500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
309600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
309700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
309800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
309900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
310000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
310100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
310200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
310300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
310400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
310500970526     O*------------------------------------------------------------------------*
310600970526** DEC - DECODIFICHE
310700970526    MODIFICA                                                      1
310800970526  ANNULLAMENTO                                                    2
310900970526VISUALIZZAZIONE                                                   3
311000970526   RIPRISTINO                                                     4
311100970526  INSERIMENTO                                                     5
311200970526** MSG - MESSAGGI
311300970530TISI31R- Tasto funzionale non gestito                                          1
311400970530TISI31R- Inserimento effettuato                                                2
311500970530TISI31R- Modifica effettuata                                                   3
311600970530TISI31R- Annullamento effettuato                                               4
311700970530TISI31R- Ripristino effettuato                                                 5
311800970530TISI31R- Richiesta funzione per record inesistente                             6
311900970530TISI31R- Record momentaneamente allocato: riprovare più tardi                  7
312000970530TISI31R- Richiesta funzione per record esistente                               8
312100000504TISI31R- Ufficio Postale errato, annullato o inesistente                       9
312200970530TISI31R- Linea di arrivo errata                                               10
312300970530TISI31R- Zona di consegna errata                                              11
312400970917TISI31R- CAP obsoleto: anche la località deve esserlo                         12
312500970530TISI31R- Nazione errata                                                       13
312600970530TISI31R- Codice tassazione errato                                             14
312700970530TISI31R- Codice tassazione errato. Accettati solo I1, I2, I3, ecc ...         15
312800970530TISI31R- Per CAP estero lasciare il codice tassazione vuoto                   16
312900060206TISI31R- Zona di consegna NON abilitata                                       17
313000970604TISI31R- Linea limite obbligatoria se immessi limiti di peso o volume         18
313100970604TISI31R- Limiti obbligatori se immesse linea o zona limite                    19
313200970917TISI31R- Impossibile inserire località di CAP obsoleti                        20
313300970917TISI31R- CAP errato o inesistente                                             21
313400970530TISI31R- Località errata                                                      22
313500970530TISI31R- Immettere L x gestire i dati della località, C x usare quelli del cap23
313600970912TISI31R- Il limite oltre deve essere superiore del limite sotto               24
313700970917TISI31R- Immettere 'O' per rendere obsoleta la località, o lasciare vuoto     25
313800970618TISI31R- Unica località per il cap. impossibile gestirla a livello località   26
313900970530TISI31R- Immettere 'S' o 'N'                                                  27
314000110623TISI31R- Tipo inoltro errato o non utilizzabile nel cappario                  28
314100970618TISI31R- La località è uguale al cap. Impossibile gestirla a livello località 29
314200970603TISI31R- Tipo tempo di consegna errato o incompleto                           30
314300010322TISI31R- Tasto funzione non consentito sul campo corrente.                    31
314400010322TISI31R- Tipo tempo ritiro errato o incompleto                                32
314500041008TISI31R- Valorizzare entrambi i campi o non valorizzarne nessuno.             33
314600130905Esistono OrariServizi personalizz.x la linea modificata saranno elimiati      34
314700130905TISI31R- F17 consentito soltando sui campi LINEA di ARRIVO valorizzati        35
314800970526** FUD - FUNZIONI
314900970526                   -        (S/N)=N                        funz.01¬
315000970526                   -        (S/N)=N                        funz.02¬
315100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
315200010322F04=Visual. tempi  -        (S/N)=S  <== S-FISSO           funz.04¬
315300130827                   -        (S/N)=N                        funz.05¬
315400970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
315500130827F07=Orari Servizi  -        (S/N)=S                        funz.07¬
315600130821                   -        (S/N)=N                        funz.08¬
315700130821                   -        (S/N)=N                        funz.09¬
315800970527                   -        (S/N)=N                        funz.10¬
315900970526                   -        (S/N)=N                        funz.11¬
316000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
316100970602F13=Sinonimi       -        (S/N)=S                        funz.13¬
316200970603F14=Zone consegna  -        (S/N)=S                        funz.14¬
316300970526                   -        (S/N)=N                        funz.15¬
316400970526                   -        (S/N)=N                        funz.16¬
316500140210F17=OrariServLinea -        (S/N)=S                        funz.17¬
316600970526                   -        (S/N)=N                        funz.18¬
316700970526                   -        (S/N)=N                        funz.19¬
316800970526                   -        (S/N)=N                        funz.20¬
316900970526                   -        (S/N)=N                        funz.21¬
317000970526                   -        (S/N)=N                        funz.22¬
317100970526                   -        (S/N)=N                        funz.23¬
317200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
