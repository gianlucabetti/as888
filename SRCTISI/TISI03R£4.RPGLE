000100970526     H*------------------------------------------------------------------------*
000200130903     H* CAPPARIO - GESTIONE CAP - ROUTINE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970526     FTISI03D   CF   E             WORKSTN INFDS(DEVDS1)
000900970526     F                                     SFILE(SI03DMS:MSGULT)
001000970526     FAZCPC01L  UF A E           K DISK
001100970526     FAZCPE01L  UF   E           K DISK
001200970526     FAZCPL01L  UF   E           K DISK
001300970526     FAZCPS01L  UF   E           K DISK
001400970526     FAZCPT01L  IF   E           K DISK
001500970526     FTABEL00F  IF   E           K DISK
001600970526     FAZORG01L  IF   E           K DISK
001700090903     FAZcpr01L  IF   E           K DISK
001800130913     FAZcos01L  UF   E           K DISK
001900970526     D*------------------------------------------------------------------------*
002000970526     D* SCHIERE
002100970526     D*------------------------------------------------------------------------*
002200970526     D*------------
002300970526     D* DECODIFICHE
002400970526     D*------------
002500970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* MESSAGGI
002800970526     D*------------
002900130903     D MSG             S             78    DIM(31) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI TASTI FUNZIONALI
003200970526     D*------------
003300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* FUNZIONI DA CARICARE A VIDEO
003600970526     D*------------
003700970526     D FUV             S             19    DIM(24)
003800131007     d
003900131007     d* Filiali abilitate  agli orari servizi
004000131007     D FIL_AB          S              3    DIM(85)
004100970526     D*------------------------------------------------------------------------*
004200970526     D* INPUT
004300970526     D*------------------------------------------------------------------------*
004400970526     D*------------
004500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004600970526     D*------------
004700970526     D FUDDS           DS
004800970526     D  FUDDE1                 1      1
004900970526     D  FUDFUN                 2      3
005000970526     D  FUDDE2                 4     19
005100970526     D  FUDFI0                20     34
005200970526     D  FUDATT                35     35
005300970526     D  FUDFI1                36     80
005400970526     D  FUDDES                 1     19
005500970526     D  FUDRIG                 1     80
005600970526     D*------------
005700970526     D* DS GESTIONE TASTI FUNZIONALI
005800970526     D*------------
005900970526     D DSKEY         E DS
006000970526     D DEVDS1          DS
006100970526     D  KEY                  369    369
006200970526     D*------------
006300970526     D* DS "XMSG" - GESTIONE MESSAGGI
006400970526     D*------------
006500970526     D DSMSG         E DS
006600970526     D*------------------
006700970526     D* DS NAZIONI
006800970526     D*------------------
006900970526     D DS15          E DS
007000970526     D*------------------
007100970526     D* DS PROVINCIE
007200970526     D*------------------
007300970526     D DSPR          E DS
007400970526     D*------------------
007500970526     D* DS ZONE DI CONSEGNA
007600970526     D*------------------
007700970526     D DS03          E DS
007800970526     D*------------------
007900970526     D* DS CODICI TASSAZIONE
008000970526     D*------------------
008100970526     D DSCT          E DS
008200970526     D*------------------
008300970526     D* DS TIPI INOLTRO
008400970526     D*------------------
008500970526     D DS9A          E DS
008600970526     D*------------------
008700970626     D* DS "TISI95R" - CONTROLLO CAP
008800970526     D*------------------
008900970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
009000970526     D*------------
009100970526     D* DS DI PROCEDURA
009200970526     D*------------
009300970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
009400130821     d tisio0ds      E DS
009500970526     D*------------
009600970526     D* ARCHITETTURA
009700970526     D*------------
009800970526     D KPJBA         E DS
009900050413     D*------------
010000050413     D* DS REPERIMENTO DATI UTENTE
010100050413     D*------------
010200050413     D TIBS34DS      E DS                                                       *Profili utente
010300050413     D DDATIUTE      E DS                                                       *Dati utente
010400050413     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010500050413     D DUTE01        E DS                                                       *Abilitazioni
010600090605
010700090605     d ds5e          e ds
010800090903     d
010900090903     d dvpodeco      e ds
011000090903     d Tibs02Ds      e ds
011100120712
011200120712     d TRTB09DS      e ds
011300130913     d Tisio7ds      e ds
011400120712
011500000420     D*------------------
011600000420     D* PASSAGGIO PARAMETRI
011700000420     D*------------------
011800000420     D PARAM           DS
011900000420     D  POP1                   1      3
012000000420     D  PUPI                   4      8  0
012100000420     D  PMSG                   9     88
012200000420     D  POPZ                  89     91
012300090903
012400090903     d datasys         s               d   inz(*sys)
012500090903     d dataiso         s               d   datfmt(*iso)
012600130913     d*caplna          s                   like(vidlna)
012700130913     d*loclna          s                   like(vidlna)
012800130902     d caplol          s                   like(vidlna)
012900130902     d loclol          s                   like(vidlna)
013000130902     d caplos          s                   like(vidlna)
013100130902     d loclos          s                   like(vidlna)
013200130913     d s_cpclna        s                   like(cpclna)
013300130913     d s_cpclos        s                   like(cpclos)
013400130913     d s_cpclol        s                   like(cpclol)
013500131007     d w003a           s              3
013600131007     d XX              s              3  0
013700131007     d YY              s              3  0
013800131007
013900131007
014000131007      /copy gaitrasrc/srcprotopr,tibs02r
014100131007
014200131007     C*------------------------------------------------------------------------*
014300970526     C* MAIN LINES
014400970526     C*------------------------------------------------------------------------*
014500970526     C*
014600970526     C* NON CI SONO ERRORI IN ENTRATA
014700970526IF  1C     ERRL00        IFEQ      '0'
014800970526     C*
014900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015300970526     C*
015400970526     C* EMETTE FORMATO VIDEO
015500970526     C                   EXSR      BUTFMR
015600970526     C*
015700970526     C* GESTIONE TASTI FUNZIONALI
015800970526     C                   EXSR      FUNCON
015900970526E   2C                   ENDDO
016000970526     C*
016100970526     C* CI SONO ERRORI IN ENTRATA
016200970527X   1C                   ELSE
016300970526     C                   MOVEL     '1'           D00ERR
016400970526     C                   MOVEL     DSMSMS        D00MSG
016500970526E   1C                   ENDIF
016600970703     C*
016700970703     C* OPERAZIONI FINALI
016800970703     C                   EXSR      FINSR
016900970526     C* FINE PGM
017000970526     C                   SETON                                        LR
017100970526     C*------------------------------------------------------------------------*
017200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
017300970526     C*------------------------------------------------------------------------*
017400970526     C     BUTFMR        BEGSR
017500970526     C*
017600970526     C* EMETTE LA TESTATA
017700970526     C                   WRITE     SI03D01
017800970526     C*
017900970526     C* SPEDISCE EVENTUALE MESSAGGIO
018000970526     C                   EXSR      SNDMSG
018100970526     C*
018200970526     C* EMETTE LE FUNZIONI
018300970526     C                   WRITE     SI03D02
018400970526     C*
018500970526     C* EMETTE IL VIDEO
018600970526     C                   EXFMT     SI03D03
018700970526     C*
018800970526     C* REIMPOSTA FLAG DI ERRORE
018900970526     C                   MOVEL     '0'           ERRL00
019000970526     C*
019100970526     C* REIMPOSTA INDICATORI DI ERRORE
019200970526     C                   SETOFF                                         5859
019300970526     C                   SETOFF                                       606162
019400970526     C                   SETOFF                                       636465
019500970526     C                   SETOFF                                       666768
019600970526     C                   SETOFF                                       697071
019700970526     C                   SETOFF                                       727374
019800970526     C                   SETOFF                                       757677
019900970526     C                   SETOFF                                       787980
020000970526     C                   SETOFF                                       818283
020100970526     C                   SETOFF                                       848586
020200970526     C                   SETOFF                                       878889
020300000420     C                   SETOFF                                       525354
020400010322     C                   SETOFF                                       5544
020500970526     C*
020600970526     C                   ENDSR
020700970526     C*------------------------------------------------------------------------*
020800970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
020900970526     C*------------------------------------------------------------------------*
021000970526     C     SNDMSG        BEGSR
021100970526     C*
021200970526     C* PULISCE IL SFILE MESSAGGI
021300970526     C                   MOVEL     '1'           *IN40
021400970526     C                   WRITE     SI03DMC
021500970526     C                   MOVEL     '0'           *IN40
021600970526     C*
021700970526     C* VISUALIZZA IL MESSAGGIO
021800970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
021900970526     C                   CALL      'XMSG'        XMSGPA
022000970526IF  1C     DSMSER        IFEQ      *ZEROS
022100970526     C                   MOVEL     DSMSKE        MSGKEY
022200970526E   1C                   ENDIF
022300970526     C                   Z-ADD     1             MSGULT
022400970526     C                   WRITE     SI03DMS
022500970526     C                   WRITE     SI03DMC
022600970526     C*
022700970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
022800970526     C                   MOVEL     *BLANKS       DSMSMS
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* ENTGES - GESTIONE ENTER -> INVIO
023300970526     C*------------------------------------------------------------------------*
023400970526     C     ENTGES        BEGSR
023500970526     C*
023600970526     C* CONTROLLA I CAMPI IMMESSI
023700970526     C                   EXSR      DECODI
023800970526     C*
023900970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
024000980128IF  1C     DEPOP1        IFEQ      'O05'
024100980128     C                   MOVEL     '1'           FINL00
024200980128E   1C                   ENDIF
024300970526     C*
024400970526     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600970526     C* F03GES - GESTIONE F03 -> FINE
024700970526     C*------------------------------------------------------------------------*
024800970526     C     F03GES        BEGSR
024900970526     C*
025000970526     C* IMPOSTA "PREMUTO F03"
025100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
025200970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
025300970526     C*
025400970526     C                   ENDSR
025500010320     C*------------------------------------------------------------------------*
025600010320     C* F04GES - GESTIONE F04 -> VISUALIZZAZIONE TEMPI CONSEGNA/RITIRO
025700010320     C*------------------------------------------------------------------------*
025800010320     C     F04GES        BEGSR
025900090605      * imposto le descrizioni fisse per i 3 tipi servizio utilizzati
026000090903      * 'C' - 'D' - 'E' -'H'
026100090605     c                   eval      ktbcod = '5E'
026200090605     c     ke2tab        setll     tabel00f
026300090605     c                   do        *hival
026400090605     c     ke2tab        reade     tabel00f
026500090605     c                   if        %eof(tabel00f)
026600090605     c                   leave
026700090605     c                   endif
026800090605     c                   eval      ds5e = tbluni
026900090605     c                   if        tblkey = 'C'
027000090605     c                   eval      viddtspc = §5ed03
027100090605     c                   endif
027200090605     c                   if        tblkey = 'D'
027300090605     c                   eval      viddtspd = §5ed03
027400090605     c                   endif
027500090605     c                   if        tblkey = 'E'
027600090605     c                   eval      viddtspe = §5ed03
027700090605     c                   endif
027800090903     c                   if        tblkey = 'H' and (vidh0c>0 or
027900090903     c                             Datasys>=dataiso)
028000090903     c                   eval      viddtsph = §5ed03
028100090903     c                   endif
028200090605     c                   enddo
028300010320     C*
028400010320     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
028500010320     C     RNMFL         IFEQ      'VIDTTC'
028600010320     C                   EXFMT     SI03WWC
028700010320     C                   ELSE
028800010320     C     RNMFL         IFEQ      'VIDTTR'
028900010320     C                   EXFMT     SI03WWR
029000010320     C                   ELSE
029100010322     C                   MOVEL     MSG(22)       DSMSMS
029200010320     C                   ENDIF
029300010320     C                   ENDIF
029400010320     C*
029500010320     C                   ENDSR
029600130821     C*------------------------------------------------------------------------*
029700130827     C* F07GES - GESTIONE F07 -> Orari servizi
029800130821     C*------------------------------------------------------------------------*
029900130827     C     F07GES        BEGSR
030000130902     c* Gestisco prima l'aggiornamento e poi gli orari
030100130902     c                   exsr      F06ges
030200130903     c                   if        errl00='0'
030300130821     C*
030400130821     c                   clear                   tisio0ds
030500130821     C                   MOVEL     VIDNAR        do0NAR
030600130821     C                   MOVEL     VIDCAP        Do0CAP
030700130830     c                   if        %subst(depop0:2:2)='02'
030800130830     C                   MOVEL     'G'           do0tric
030900130830     c                   else
031000130830     C                   MOVEL     'I'           do0tric
031100130830     c                   endif
031200130821     c                   movel     d00dri        do0dri
031300130821     c                   movel     d00ver        do0ver
031400130821
031500130821     C                   call      'TISIO2R'
031600130821     C                   PARM                    KPJBA
031700130821     C                   PARM                    tisio0ds
031800130821     C*
031900130821     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
032000130903     c                   clear                   f12l00
032100130821IF  1C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
032200130821     C                   MOVEL     '1'           ERRL00                         *ERRORE
032300130821     C                   MOVEL     '1'           F03L00                         *FINE PGM
032400130821X   1C                   ELSE                                                   *NON PREMUTO F03
032500130821     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
032600130821E   1C                   ENDIF
032700130821     C*
032800130903E   1C                   ENDIF
032900130903     C                   MOVEL     ' '           FINL00                         *FINE PGM
033000130821     C*
033100130821     C                   ENDSR
033200130828     C*------------------------------------------------------------------------*
033300130828     C* F17GES - GESTIONE F17 -> Orari servizi  per linea di arrivo
033400130828     C*------------------------------------------------------------------------*
033500130828     C     F17GES        BEGSR
033600130828     c                   clear                   dsmsms
033700130828     c                   exsr      decodi
033800130828     c
033900130828    1c                   if        dsmsms=*blanks
034000130828     C*
034100130828     c                   clear                   tisio0ds
034200130828     C                   MOVEL     VIDNAR        do0NAR
034300130830     c                   if        %subst(depop0:2:2)='02'
034400130830     C                   MOVEL     'G'           do0tric
034500130830     c                   else
034600130830     C                   MOVEL     'I'           do0tric
034700130830     c                   endif
034800130828     c                   movel     d00dri        do0dri
034900130828     c                   movel     d00ver        do0ver
035000130828
035100130828     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
035200130828    2c                   select
035300130828     C     RNMFL         wheneq    'VIDLNA'
035400130828     c     vidlna        andgt     *zeros
035500130828     C                   movel     vidlna        do0lna
035600130828     c
035700130828     C     RNMFL         wheneq    'VIDLOL'
035800130828     c     vidlol        andgt     *zeros
035900130828     C                   movel     vidlol        do0lna
036000130828     C     RNMFL         wheneq    'VIDLOS'
036100130828     c     vidlos        andgt     *zeros
036200130828     C                   movel     vidlos        do0lna
036300130828     C                   other
036400130828     C                   MOVEL     MSG(23)       DSMSMS
036500130828    2C                   ENDsl
036600130828
036700130828    2c                   if        dsmsms=*blanks
036800130828     C                   call      'TISIO2R'
036900130828     C                   PARM                    KPJBA
037000130828     C                   PARM                    tisio0ds
037100130828     C*
037200130828     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
037300130828IF  3C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
037400130828     C                   MOVEL     '1'           ERRL00                         *ERRORE
037500130828     C                   MOVEL     '1'           F03L00                         *FINE PGM
037600130828X   1C                   ELSE                                                   *NON PREMUTO F03
037700130828     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
037800130828E   3C                   ENDIF
037900130828     C*
038000130828E   2C                   ENDIF
038100130828E   1C                   ENDIF
038200130828     C*
038300130828     C                   ENDSR
038400970526     C*------------------------------------------------------------------------*
038500970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
038600970526     C*------------------------------------------------------------------------*
038700970526     C     F06GES        BEGSR
038800970526     C*
038900970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
039000970526IF  1C     DEPOP1        IFEQ      'F06'
039100970526     C                   EXSR      INSERI
039200970526E   1C                   ENDIF
039300970526IF  1C     DEPOP1        IFEQ      'O02'
039400970526     C                   EXSR      MODIFI
039500970526E   1C                   ENDIF
039600970526IF  1C     DEPOP1        IFEQ      'O04'
039700970526     C                   EXSR      ANNULL
039800970526E   1C                   ENDIF
039900970526IF  1C     DEPOP1        IFEQ      'O07'
040000970526     C                   EXSR      RIPRIS
040100970526E   1C                   ENDIF
040200130902     c
040300970526     C                   ENDSR
040400970526     C*------------------------------------------------------------------------*
040500970526     C* F08GES - GESTIONE F08 -> GESTIONE LOCALITA'
040600970526     C*------------------------------------------------------------------------*
040700970526     C     F08GES        BEGSR
040800970528     C*
040900970602     C                   MOVEL     VIDNAR        DNAR
041000970602     C                   MOVEL     VIDCAP        DCAP
041100970528     C                   MOVEL     DEPOP0        WOP0
041200970528     C                   MOVEL     'L'           WOP0
041300970528     C                   MOVEL     'F08'         WOP1
041400970526     C                   MOVEL     'TISI30R'     WPGM              8
041500970526     C                   EXSR      LANPGM
041600970526     C*
041700970526     C                   ENDSR
041800970526     C*------------------------------------------------------------------------*
041900970526     C* F09GES - GESTIONE F09 -> ECCEZIONI LINEE/ZONE
042000970526     C*------------------------------------------------------------------------*
042100970526     C     F09GES        BEGSR
042200970528     C*
042300970602     C                   MOVEL     VIDNAR        DNAR
042400970602     C                   MOVEL     VIDCAP        DCAP
042500970528     C                   MOVEL     DEPOP0        WOP0
042600970528     C                   MOVEL     'E'           WOP0
042700970526     C                   MOVEL     'F09'         WOP1
042800970526     C                   MOVEL     'TISI11R'     WPGM              8
042900970526     C                   EXSR      LANPGM
043000970526     C*
043100970526     C                   ENDSR
043200970526     C*------------------------------------------------------------------------*
043300970526     C* F10GES - GESTIONE F10 -> TEMPI DI CONSEGNA
043400970526     C*------------------------------------------------------------------------*
043500970526     C     F10GES        BEGSR
043600970528     C*
043700970602     C                   MOVEL     VIDNAR        DNAR
043800970602     C                   MOVEL     VIDCAP        DCAP
043900970528     C                   MOVEL     DEPOP0        WOP0
044000970528     C                   MOVEL     'T'           WOP0
044100970526     C                   MOVEL     'F10'         WOP1
044200970526     C                   MOVEL     'TISI41R'     WPGM              8
044300970526     C                   EXSR      LANPGM
044400970526     C*
044500970526     C                   ENDSR
044600970526     C*------------------------------------------------------------------------*
044700970526     C* F12GES - GESTIONE F12 -> RITORNO
044800970526     C*------------------------------------------------------------------------*
044900970526     C     F12GES        BEGSR
045000970526     C*
045100970526     C* IMPOSTA "PREMUTO F12"
045200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
045300970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
045400970526     C*
045500970526     C                   ENDSR
045600970603     C*------------------------------------------------------------------------*
045700970603     C* F14GES - GESTIONE F14 -> ZONE DI CONSEGNA
045800970603     C*------------------------------------------------------------------------*
045900970603     C     F14GES        BEGSR
046000970603     C*
046100970603     C                   CALL      'TRTB23R'
046200970603     C                   PARM                    KPJBA
046300970612     C*
046400970603     C                   ENDSR
046500970526     C*------------------------------------------------------------------------*
046600970526     C* F21GES - GESTIONE F21 -> STAMPA CAPPARIO
046700970526     C*------------------------------------------------------------------------*
046800970526     C     F21GES        BEGSR
046900970526     C*
047000970602     C                   MOVEL     VIDNAR        DNAR
047100970602     C                   MOVEL     VIDCAP        DCAP
047200970526     C                   MOVEL     'F21'         WOP1
047300970526     C                   MOVEL     'TISI04R'     WPGM              8
047400970526     C                   EXSR      LANPGM
047500970526     C*
047600970526     C                   ENDSR
047700970526     C*------------------------------------------------------------------------*
047800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
047900970526     C*------------------------------------------------------------------------*
048000970526     C     F24GES        BEGSR
048100970526     C*
048200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
048300970526     C*
048400970526     C                   ENDSR
048500970526     C*------------------------------------------------------------------------*
048600000420     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
048700970526     C*------------------------------------------------------------------------*
048800970526     C     INSERI        BEGSR
048900970526     C*
049000970526     C* CONTROLLA I CAMPI IMMESSI
049100970526     C                   EXSR      DECODI
049200970526     C*
049300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
049400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
049500970526     C                   Z-ADD     D00VER        CPCVER                         *CHIAVE
049600970526     C                   MOVEL     VIDNAR        CPCNAR
049700970526     C                   MOVEL     VIDCAP        CPCCAP
049800000420     C                   EXSR      IMPCPC                                       *ALTRI CAMPI
049900970526     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
050000970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
050100970526     C                   MOVEL     *BLANKS       CPCATB                         *ANNULLAMENTO
050200980616     C                   MOVEL     'N'           CPCESE                         *ECCEZIONI X LINEA
050300980616     C                   MOVEL     'N'           CPCESL                         *ECCEZIONI X LOC
050400970526     C                   WRITE     AZCPC000
050500000420     C*
050600000420     C                   EXSR      LETCPC
050700000420     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
050800970602     C*
050900970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
051000970602     C                   EXSR      AZZVID
051100970526E   1C                   ENDIF
051200970526     C*
051300970526     C                   ENDSR
051400970526     C*------------------------------------------------------------------------*
051500000420     C* MODIFI - OPERAZIONI DI MODIFICA
051600970526     C*------------------------------------------------------------------------*
051700970526     C     MODIFI        BEGSR
051800970526     C*
051900970526     C* CONTROLLA I CAMPI IMMESSI
052000970526     C                   EXSR      DECODI
052100970526     C*
052200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
052300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052400000420     C                   EXSR      LETCPC
052500970526     C*
052600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
052700970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052800130913     c* richiamo pgm per cancellare OrariServizi a fronte delle lna cambiate
052900130913     c                   exsr      sr_oraris
053000130913
053100970526     C                   EXSR      IMPCPC                                       *IMPOSTA I CAMPI
053200970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
053300970526     C                   MOVEL     *BLANKS       CPCATB                         *ANNULLAMENTO
053400970526     C                   UPDATE    AZCPC000
053500000420     C*
053600000420     C                   MOVEL     MSG(3)        D00MSG
053700000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
053800970526E   2C                   ENDIF
053900970526E   1C                   ENDIF
054000970526     C*
054100970526     C                   ENDSR
054200970526     C*------------------------------------------------------------------------*
054300970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
054400970526     C*------------------------------------------------------------------------*
054500970526     C     ANNULL        BEGSR
054600130902     C*
054700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
054800000420     C                   EXSR      LETCPC
054900970526     C*
055000970526     C* ANNULLA LE LOCALITA'
055100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
055200970526     C                   Z-ADD     CPCVER        KCLVER
055300970526     C                   MOVEL     CPCNAR        KCLNAR
055400970526     C                   MOVEL     CPCCAP        KCLCAP
055500970917     C     KEYCPL        SETLL     AZCPL01L                           99
055600970526     C     KEYCPL        READE     AZCPL01L                               99
055700970526DO  2C     *IN99         DOWEQ     *OFF
055800970526IF  3C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
055900970526IF  4C     CPLDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
056000970526     C                   MOVEL     'A'           CPLATB                         *FLAG ANNULLAMENTO
056100970526     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
056200970526     C                   UPDATE    AZCPL000                                     *ANNULLA
056300970526X   4C                   ELSE                                                   *MAI TRASMESSO
056400000426     C                   DELETE    AZCPL000                                     *CANCELLA
056500970526E   4C                   ENDIF
056600970526E   3C                   ENDIF
056700970526     C     KEYCPL        READE     AZCPL01L                               99
056800970526E   2C                   ENDDO                                                  *FINE LOCALITA'
056900000420     C*
057000970526     C* ANNULLA I SINONIMI
057100970526     C                   Z-ADD     D00VER        KCSVER
057200970526     C                   MOVEL     CPCNAR        KCSNAR
057300970526     C                   MOVEL     CPCCAP        KCSCAP
057400970526     C     KEYCPS        SETLL     AZCPS01L                           99
057500970526     C     KEYCPS        READE     AZCPS01L                               99
057600970526DO  2C     *IN99         DOWEQ     *OFF
057700970526IF  3C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
057800970526IF  4C     CPSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
057900970526     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
058000970526     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
058100970526     C                   UPDATE    AZCPS000                                     *ANNULLA
058200970526X   4C                   ELSE                                                   *MAI TRASMESSO
058300970526     C                   DELETE    AZCPS000                                     *CANCELLA
058400970526E   4C                   ENDIF
058500970526E   3C                   ENDIF
058600970526     C     KEYCPS        READE     AZCPS01L                               99
058700970526E   2C                   ENDDO                                                  *FINE SINONIMI
058800970526     C*
058900000602     C* ANNULLA LE ECCEZIONI
059000970526     C                   Z-ADD     CPCVER        KCEVER
059100970526     C                   MOVEL     CPCNAR        KCENAR
059200970526     C                   MOVEL     CPCCAP        KCECAP
059300970526     C     KEYCPE        SETLL     AZCPE01L                           99
059400970526     C     KEYCPE        READE     AZCPE01L                               99
059500970526DO  2C     *IN99         DOWEQ     *OFF
059600970526IF  3C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
059700970526IF  4C     CPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
059800970526     C                   MOVEL     'A'           CPEATB                         *FLAG ANNULLAMENTO
059900970526     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
060000970526     C                   UPDATE    AZCPE000                                     *ANNULLA
060100970526X   4C                   ELSE                                                   *MAI TRASMESSO
060200970526     C                   DELETE    AZCPE000                                     *CANCELLA
060300970526E   4C                   ENDIF
060400970526E   3C                   ENDIF
060500970526     C     KEYCPE        READE     AZCPE01L                               99
060600970526E   2C                   ENDDO                                                  *FINE SINONIMI
060700130913     c*
060800130913     c* ANNULLA GLI ORARI
060900130913     C                   Z-ADD     CPCVER        KCEVER
061000130913     C                   MOVEL     CPCNAR        KCENAR
061100130913     C                   MOVEL     CPCCAP        KCECAP
061200130913     C     KEYCPE        SETLL     AZCos01L                           99
061300130913     C     KEYCPE        READE     AZCos01L                               99
061400130913DO  2C     *IN99         DOWEQ     *OFF
061500130913     C                   DELETE    AZCos000                                     *CANCELLA
061600130913     C     KEYCPE        READE     AZCos01L                               99
061700130913E   2C                   ENDDO                                                  *FINE SINONIMI
061800970526     C*
061900970526     C* ANNULLA IL CAP
062000000602     C                   MOVEL     '0'           ANNDLT            1
062100970526IF  2C     CPCATB        IFEQ      *BLANKS                                      *NO ANNULLATI
062200970526IF  3C     CPCDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
062300970526     C                   MOVEL     'A'           CPCATB                         *FLAG ANNULLAMENTO
062400970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
062500970526     C                   UPDATE    AZCPC000                                     *ANNULLA
062600970526X   3C                   ELSE                                                   *MAI TRASMESSO
062700970526     C                   DELETE    AZCPC000                                     *CANCELLA
062800000602     C                   MOVEL     '1'           ANNDLT
062900970526E   3C                   ENDIF
063000970526E   2C                   ENDIF
063100000420     C*
063200000420     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
063300000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
063400970526E   1C                   ENDIF
063500970526     C*
063600970526     C                   ENDSR
063700970526     C*------------------------------------------------------------------------*
063800000420     C* RIPRIS - OPERAZIONI DI RIPRISTINO
063900970526     C*------------------------------------------------------------------------*
064000970526     C     RIPRIS        BEGSR
064100970526     C*
064200970526     C* CONTROLLA I CAMPI IMMESSI
064300970526     C                   EXSR      DECODI
064400970526     C*
064500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
064600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
064700000420     C                   EXSR      LETCPC
064800970526     C*
064900970526     C* IMPOSTA IL RECORD DA SCRIVERE
065000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
065100970526     C                   MOVEL     *BLANKS       CPCATB                         *FLAG ANNULLAMENTO
065200970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
065300000420     C                   UPDATE    AZCPC000                                     *RIPRISTINA
065400000420     C*
065500970526E   2C                   ENDIF
065600970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
065700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
065800970526E   1C                   ENDIF
065900970526     C*
066000970526     C                   ENDSR
066100970526     C*------------------------------------------------------------------------*
066200970526     C* LETCPC - LEGGE IL RECORD PERL'AGGIORNAMENTO
066300970526     C*------------------------------------------------------------------------*
066400970526     C     LETCPC        BEGSR
066500970526     C*
066600000420     C     DEPOP1        IFNE      'F06'
066700970526     C                   Z-ADD     D00VER        KCCVER
066800980127     C                   MOVEL     D00NAR        KCCNAR
066900980127     C                   MOVEL     D00CAP        KCCCAP
067000970526     C     KEYCPC        CHAIN     AZCPC01L                           9998
067100970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
067200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
067300970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
067400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
067500970526E   1C                   ENDIF
067600970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
067700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
067800970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
067900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
068000970526E   1C                   ENDIF
068100000420     C                   ENDIF
068200000420     C*
068300000420     C                   ENDSR
068400970526     C*------------------------------------------------------------------------*
068500970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
068600970526     C*------------------------------------------------------------------------*
068700970526     C     DECODI        BEGSR
068800050428     C*
068900050428     C* NO VALORI NEGATIVI
069000050428     C                   IF        VIDLKS < *zeros
069100050428     C                   EVAL      VIDLKS = VIDLKS * -1
069200050428     C                   ENDIF
069300050428     C                   IF        VIDLKG < *zeros
069400050428     C                   EVAL      VIDLKG = VIDLKG * -1
069500050428     C                   ENDIF
069600050428     C                   IF        VIDLK2 < *zeros
069700050428     C                   EVAL      VIDLK2 = VIDLK2 * -1
069800050428     C                   ENDIF
069900050428     C                   IF        VIDLMS < *zeros
070000050428     C                   EVAL      VIDLMS = VIDLMS * -1
070100050428     C                   ENDIF
070200050428     C                   IF        VIDLMC < *zeros
070300050428     C                   EVAL      VIDLMC = VIDLMC * -1
070400050428     C                   ENDIF
070500050428     C                   IF        VIDLM2 < *zeros
070600050428     C                   EVAL      VIDLM2 = VIDLM2 * -1
070700050428     C                   ENDIF
070800050428     C*
070900970527     C*--------------------
071000970526     C* NAZIONE
071100970527     C*--------------------
071200970526     C     *LIKE         DEFINE    §15CPR        DEPCPR
071300970527IF  1C     ERRL00        IFEQ      '0'
071400970526     C                   MOVEL     *BLANKS       VDDNAR
071500970526     C                   MOVEL     VIDNAR        WNAR
071600970526     C                   MOVEL     *BLANKS       WDNAR
071700970526     C                   MOVEL     *BLANKS       WCPR
071800970526     C                   EXSR      CHKNAR
071900970526     C                   MOVEL     WNAR          VIDNAR
072000970526     C                   MOVEL     WDNAR         VDDNAR
072100970526     C                   MOVEL     WCPR          DEPCPR
072200970526IF  2C     ERRL00        IFEQ      '1'
072300970526     C                   SETON                                        60
072400970526     C                   MOVEL     MSG(13)       DSMSMS
072500970526E   2C                   ENDIF
072600970526E   1C                   ENDIF
072700970527     C*--------------------
072800970526     C* CAP
072900970527     C*--------------------
073000970526IF  1C     ERRL00        IFEQ      '0'
073100970526     C                   MOVEL     VIDNAR        WNAR
073200970602     C                   MOVEL     VIDCAP        WCAP
073300970526     C                   EXSR      CHKCAP
073400970526IF  2C     ERRL00        IFEQ      '1'
073500970526     C                   SETON                                        61
073600970526E   2C                   ENDIF
073700970526E   1C                   ENDIF
073800970526     C*--------------------
073900970526     C* NAZIONE + CAP
074000970526     C*--------------------
074100970527     C* INSERIMENTO -> RECORD INESISTENTE
074200970526IF  1C     ERRL00        IFEQ      '0'
074300970527IF  2C     DEPOP1        IFEQ      'F06'
074400970526     C                   Z-ADD     D00VER        KCCVER
074500970526     C                   MOVEL     VIDNAR        KCCNAR
074600970526     C                   MOVEL     VIDCAP        KCCCAP
074700970526     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
074800970526IF  3C     *IN99         IFEQ      *OFF
074900970527     C                   SETON                                        6061
075000970526     C                   MOVEL     '1'           ERRL00
075100970527     C                   MOVEL     MSG(8)        DSMSMS
075200970526E   3C                   ENDIF
075300970526E   2C                   ENDIF
075400970526E   1C                   ENDIF
075500970526     C*--------------------
075600970604     C* LINEA ARRIVO STANDARD
075700970526     C*--------------------
075800970526IF  1C     ERRL00        IFEQ      '0'
075900970526     C                   MOVEL     *BLANKS       VDDLNA
076000970526     C                   MOVEL     VIDLNA        WLIN
076100970526     C                   MOVEL     *BLANKS       WDLIN
076200970526     C                   EXSR      CHKLIN
076300970526     C                   MOVEL     WLIN          VIDLNA
076400970526     C                   MOVEL     WDLIN         VDDLNA
076500970526IF  2C     ERRL00        IFEQ      '1'
076600970526     C                   SETON                                        62
076700970526     C                   MOVEL     MSG(10)       DSMSMS
076800130902     c*
076900130902   x2c                   else
077000130917     c                   if        depop1<>'F06'
077100130902     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
077200130902     c                   movel     vidlna        w0030             3 0
077300130913    3c***                if        w0030 =cpclna
077400130913     c***                clear                   caplna
077500130911     c*****              clear                   loclna
077600130913     c***                clear                   f07lna
077700130913    3c***                endif
077800130902     c
077900130913    3c*                  if        w0030 <>cpclna and vidlna<>caplna
078000130913     c*
078100130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
078200130913     C*                  MOVEL     VIDNAR        cosNAR
078300130913     C*                  MOVEL     VIDCAP        cosCAP
078400130913     C*                  clear                   cosloc
078500130913     C*                  MOVEL     cpclna        coslna
078600130913     c*    kcos          chain     azcos01l
078700130913    4c*                  if        %found(azcos01l)
078800130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
078900130913     c* e non ho già avvisato avvisato della cancellazione orariservizi
079000130913     c                   if        w0030<>cpclna and cpclna<>s_cpclna
079100130913     c                                           and cpclna<>s_cpclos
079200130913     c                                           and cpclna<>s_cpclol
079300130913     c                                           and f07lna=*blanks
079400130913     c                   clear                   tisio7ds
079500130913     c                   eval      io7mod='C'
079600130913     c                   eval      io7liv='C'
079700130913     c                   eval      io7ver=d00ver
079800130913     c                   eval      io7nar=vidnar
079900130913     c                   eval      io7cap=vidcap
080000130913     c                   eval      io7lna=cpclna
080100130913     c                   eval      io7lns=%dec(vidlna:3:0)
080200130913     c                   eval      io7los=%dec(vidlos:3:0)
080300130913     c                   eval      io7lol=%dec(vidlol:3:0)
080400131016     c*****              movel     tisio7ds      kpjbu
080500130913     c                   call      'TISIO7R'
080600130913     c                   parm                    kpjba
080700131016     c                   parm                    tisio7ds
080800131016     c*****              movel     kpjbu         tisio7ds
080900130913     c                   if        oo7err='1'
081000130913     c                   movel     oo7msg        DSmsms
081100130902     c                   eval      *in62  = *on
081200130902     c                   eval      ErrL00 = *on
081300130913     c                   eval      f07lna='S'
081400130902e   4c                   endif
081500130902c
081600130913     c                   eval      s_cpclna=cpclna
081700130913     c***                eval      caplna=vidlna
081800130902    3c                   endif
081900130917    2c                   endif
082000130902     c*
082100130902     c* Verifico se ci sono inserimenti a livello di località per quel CAP
082200130913IF  3C*    ERRL00        IFEQ      '0'
082300130913    4c*                  if        w0030 <>cpclna and vidlna<>caplna
082400130913     c*
082500130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
082600130913     C*                  MOVEL     VIDNAR        cosNAR
082700130913     C*                  MOVEL     VIDCAP        cosCAP
082800130913     C*                  movel     'A'           cosloc
082900130913     C*                  MOVEL     cpclna        coslna
083000130913     c*    kcos2         setll     azcos02l
083100130913     c*    kcos22        reade     azcos02l
083200130913    5c*                  dow       not %eof(azcos02l) and errl00='0'
083300130913     c*                  movel     cosloc        kclloc
083400130913     c*                  movel     coscap        kclcap
083500130913     c*                  movel     cosver        kclver
083600130913     c*                  movel     cosnar        kclnar
083700130903     c* se la trovo, verifico che la località non abbia una eccezione
083800130913     c*    KEYCPL2       chain     azcpl01l
083900130913    6c*                  if        %found(azcpl01l) and
084000130913     c*                            cpllna<>cpclna and cpllol<>cpclna and
084100130913     c*                            cpllos<>cpclna
084200130913     c*
084300130913     c*                  movel     Msg(20)       DSmsms
084400130913     c*                  eval      *in62  = *on
084500130913     c*                  eval      f07lna='S'
084600130913     c*                  eval      ErrL00 = *on
084700130913e   6c*                  endif
084800130913     c*    kcos22        reade     azcos02l
084900130913    5c*                  enddo
085000130903c
085100130913     c*                  eval      caplna=vidlna
085200130903     c*
085300130913E   4C*                  ENDIF
085400130913E   3C*                  ENDIF
085500130903E   2C                   ENDIF
085600130903E   1C                   ENDIF
085700970604     C*--------------------
085800970604     C* ZONA DI CONSEGNA STANDARD
085900970604     C*--------------------
086000970604IF  1C     ERRL00        IFEQ      '0'
086100970604     C                   MOVEL     VIDLNA        WLIN
086200970604     C                   Z-ADD     VIDZNC        WZNC
086300970604     C                   EXSR      CHKZNC
086400970604IF  2C     ERRL00        IFEQ      '1'
086500970604     C                   SETON                                        63
086600970604     C                   MOVEL     MSG(11)       DSMSMS
086700970604E   2C                   ENDIF
086800060206if  2c                   if        §03nuc = 'S'
086900060206     c                   movel     Msg(17)       DSmsms
087000060206     c                   eval      *in63  = *on
087100060206     c                   eval      ErrL00 = *on
087200060206e   2c                   endif
087300970604E   1C                   ENDIF
087400970604     C*--------------------
087500970912     C* LINEA ARRIVO SOTTO
087600970604     C*--------------------
087700970604IF  1C     ERRL00        IFEQ      '0'
087800970604     C                   MOVEL     *BLANKS       VDDLOS
087900970604IF  2C     VIDLOS        IFNE      *ZEROS
088000970604     C     VIDLOS        ANDNE     *BLANKS
088100970604     C                   MOVEL     VIDLOS        WLIN
088200970604     C                   MOVEL     *BLANKS       WDLIN
088300970604     C                   EXSR      CHKLIN
088400970604     C                   MOVEL     WLIN          VIDLOS
088500970604     C                   MOVEL     WDLIN         VDDLOS
088600970604IF  3C     ERRL00        IFEQ      '1'
088700970604     C                   SETON                                        81
088800970604     C                   MOVEL     MSG(10)       DSMSMS
088900130903    3c                   endif
089000130903    2c                   endif
089100130903    1c                   endif
089200130902     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
089300130903IF  1C     ERRL00        IFEQ      '0'
089400130917     c     depop1        andne     'F06'
089500130903    2c                   if        vidlos>*zeros
089600130902     c                   movel     vidlos        w0030             3 0
089700130903     c                   else
089800130903     c                   clear                   w0030
089900130903    2c                   endif
090000130903     c
090100130913    2c***                if        w0030 =cpclos
090200130913     c***                clear                   caplos
090300130911     c****               clear                   loclos
090400130913     c***                clear                   f07los
090500130913    2c***                endif
090600130902     c
090700130913    2c*                  if        w0030 <>cpclos and vidlos<>caplos
090800130902     c
090900130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
091000130913     C*                  MOVEL     VIDNAR        cosNAR
091100130913     C*                  MOVEL     VIDCAP        cosCAP
091200130913     C*                  clear                   cosloc
091300130913     C*                  MOVEL     cpclos        coslna
091400130913     c*    kcos          chain     azcos01l
091500130913    3c*                  if        %found(azcos01l)
091600130913     c*                  movel     Msg(20)       DSmsms
091700130913     c*                  eval      *in81  = *on
091800130913     c*                  eval      ErrL00 = *on
091900130913     c*                  eval      f07los='S'
092000130913e   3c*                  endif
092100130902c
092200130913     c*                  eval      caplos=vidlos
092300130913E   2C*                  ENDIF
092400130913E   1C                   ENDIF
092500130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
092600130913     c* e non ho già avvisato avvisato della cancellazione orariservizi
092700130913    2c                   if        w0030<>cpclos and cpclos<>s_cpclna
092800130913     c                                           and cpclos<>s_cpclos
092900130913     c                                           and cpclos<>s_cpclol
093000130913     c                                           and f07lna=*blanks
093100130913     c                   clear                   tisio7ds
093200130913     c                   eval      io7mod='C'
093300130913     c                   eval      io7liv='C'
093400130913     c                   eval      io7ver=d00ver
093500130913     c                   eval      io7nar=vidnar
093600130913     c                   eval      io7cap=vidcap
093700130913     c                   eval      io7lna=cpclos
093800130913     c                   eval      io7lns=%dec(vidlna:3:0)
093900130913     c                   eval      io7los=%dec(vidlos:3:0)
094000130913     c                   eval      io7lol=%dec(vidlol:3:0)
094100131016     c*****              movel     tisio7ds      kpjbu
094200130913     c                   call      'TISIO7R'
094300130913     c                   parm                    kpjba
094400131016     c                   parm                    tisio7ds
094500131016     c*****              movel     kpjbu         tisio7ds
094600130913    3c                   if        oo7err='1'
094700130913     c                   movel     oo7msg        DSmsms
094800130913     c                   eval      *in81  = *on
094900130913     c                   eval      ErrL00 = *on
095000130913     c                   eval      f07lna='S'
095100130913e   3c                   endif
095200130913c
095300130913     c                   eval      s_cpclos=cpclos
095400130913    2c                   endif
095500130911     c* Verifico se ci sono inserimenti a livello di località per quel CAP
095600130913IF  3C*    ERRL00        IFEQ      '0'
095700130913    4c*                  if        w0030 <>cpclos and vidlos<>caplos
095800130913     c*
095900130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
096000130913     C*                  MOVEL     VIDNAR        cosNAR
096100130913     C*                  MOVEL     VIDlos        cosCAP
096200130913     C*                  movel     'A'           cosloc
096300130913     C*                  MOVEL     cpclos        coslna
096400130913     c*    kcos2         setll     azcos02l
096500130913     c*    kcos22        reade     azcos02l
096600130913    5c*                  dow       not %eof(azcos02l) and errl00='0'
096700130913     c*                  movel     cosloc        kclloc
096800130913     c*                  movel     coscap        kclcap
096900130913     c*                  movel     cosver        kclver
097000130913     c*                  movel     cosnar        kclnar
097100130911     c* se la trovo, verifico che la località non abbia una eccezione
097200130913     c*    KEYCPL2       chain     azcpl01l
097300130913    6c*                  if        %found(azcpl01l) and
097400130913     c*                            cpllna<>cpclos and cpllol<>cpclos and
097500130913     c*                            cpllos<>cpclos
097600130913     c*
097700130913     c*                  movel     Msg(20)       DSmsms
097800130913     c*                  eval      *in81  = *on
097900130913     c*                  eval      f07los='S'
098000130913     c*                  eval      ErrL00 = *on
098100130913e   6c*                  endif
098200130913     c*    kcos22        reade     azcos02l
098300130913    5c*                  enddo
098400130911c
098500130913     c*                  eval      caplos=vidlos
098600130911     c*
098700130913E   4C*                  ENDIF
098800130913E   3C*                  ENDIF
098900970604     C*--------------------
099000970912     C* ZONA DI CONSEGNA SOTTO
099100970604     C*--------------------
099200970604IF  1C     ERRL00        IFEQ      '0'
099300971126IF  2C     VIDLOS        IFNE      *ZEROS
099400970604     C                   MOVEL     VIDLOS        WLIN
099500970604     C                   Z-ADD     VIDZOS        WZNC
099600970604     C                   EXSR      CHKZNC
099700970604IF  3C     ERRL00        IFEQ      '1'
099800970604     C                   SETON                                        82
099900970604     C                   MOVEL     MSG(11)       DSMSMS
100000970604E   3C                   ENDIF
100100060206if  3c                   if        §03nuc = 'S'
100200060206     c                   movel     Msg(17)       DSmsms
100300060206     c                   eval      *in82  = *on
100400060206     c                   eval      ErrL00 = *on
100500060206e   3c                   endif
100600970604E   2C                   ENDIF
100700970604E   1C                   ENDIF
100800970604     C*--------------------
100900970912     C* PESO/VOLUME SOTTO
101000970604     C*--------------------
101100970604IF  1C     ERRL00        IFEQ      '0'
101200970604IF  2C     VIDLKS        IFGT      *ZEROS
101300970604     C     VIDLMS        ORGT      *ZEROS
101400970604IF  3C     VIDLOS        IFEQ      *BLANKS
101500970604     C     VIDLOS        OREQ      *ZEROS
101600970604     C                   SETON                                        81
101700970604     C                   MOVEL     MSG(18)       DSMSMS
101800970604     C                   MOVEL     '1'           ERRL00
101900970604E   3C                   ENDIF
102000070103E   2C                   ENDIF
102100970604IF  3C     VIDLOS        IFNE      *BLANKS
102200970604     C     VIDLOS        ANDNE     *ZEROS
102300070103IF  4C                   IF        VIDLKS = *ZEROS OR
102400070103     C                             VIDLMS = *ZEROS
102500970604     C                   SETON                                        8384
102600970604     C                   MOVEL     MSG(19)       DSMSMS
102700970604     C                   MOVEL     '1'           ERRL00
102800070103E   4C                   ENDIF
102900070103E   3C                   ENDIF
103000970604E   1C                   ENDIF
103100970604     C*--------------------
103200970912     C* LINEA ARRIVO OLTRE
103300970604     C*--------------------
103400970604IF  1C     ERRL00        IFEQ      '0'
103500970604     C                   MOVEL     *BLANKS       VDDLOL
103600970604IF  2C     VIDLOL        IFNE      *ZEROS
103700970604     C     VIDLOL        ANDNE     *BLANKS
103800970604     C                   MOVEL     VIDLOL        WLIN
103900970604     C                   MOVEL     *BLANKS       WDLIN
104000970604     C                   EXSR      CHKLIN
104100970604     C                   MOVEL     WLIN          VIDLOL
104200970604     C                   MOVEL     WDLIN         VDDLOL
104300970604IF  3C     ERRL00        IFEQ      '1'
104400970604     C                   SETON                                        67
104500970604     C                   MOVEL     MSG(10)       DSMSMS
104600970604E   3C                   ENDIF
104700970604E   2C                   ENDIF
104800970604E   1C                   ENDIF
104900130903     c
105000130903     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
105100130903IF  1C     ERRL00        IFEQ      '0'
105200130917     c     depop1        andne     'F06'
105300130903    2c                   if        vidlol>*zeros
105400130903     c                   movel     vidlol        w0030             3 0
105500130903     c                   else
105600130903     c                   clear                   w0030
105700130903    2c                   endif
105800130903     c
105900130913    2c***                if        w0030 =cpclol
106000130913     c***                clear                   caplol
106100130911     c***                clear                   loclol
106200130913     c***                clear                   f07lol
106300130913    2c***                endif
106400130903     c
106500130913    2c*                  if        w0030 <>cpclol and vidlol<>caplol
106600130913     c*
106700130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
106800130913     C*                  MOVEL     VIDNAR        cosNAR
106900130913     C*                  MOVEL     VIDCAP        cosCAP
107000130913     C*                  clear                   cosloc
107100130913     C*                  MOVEL     cpclol        coslna
107200130913     c*    kcos          chain     azcos01l
107300130913    3c*                  if        %found(azcos01l)
107400130913     c*                  movel     Msg(20)       DSmsms
107500130913     c*                  eval      *in67  = *on
107600130913     c*                  eval      ErrL00 = *on
107700130913     c*                  eval      f07lol='S'
107800130913e   3c*                  endif
107900130903c
108000130913     c*                  eval      caplol=vidlol
108100130913E   2C*                  ENDIF
108200130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
108300130913     c* e non ho già avvisato avvisato della cancellazione orariservizi
108400130913    2c                   if        w0030<>cpclol and cpclol<>s_cpclna
108500130913     c                                           and cpclol<>s_cpclos
108600130913     c                                           and cpclol<>s_cpclol
108700130913     c                                           and f07lna=*blanks
108800130913     c                   clear                   tisio7ds
108900130913     c                   eval      io7mod='C'
109000130913     c                   eval      io7liv='C'
109100130913     c                   eval      io7ver=d00ver
109200130913     c                   eval      io7nar=vidnar
109300130913     c                   eval      io7cap=vidcap
109400130913     c                   eval      io7lna=cpclol
109500130913     c                   eval      io7lns=%dec(vidlna:3:0)
109600130913     c                   eval      io7los=%dec(vidlos:3:0)
109700130913     c                   eval      io7lol=%dec(vidlol:3:0)
109800131016     c*****              movel     tisio7ds      kpjbu
109900130913     c                   call      'TISIO7R'
110000130913     c                   parm                    kpjba
110100131016     c                   parm                    tisio7ds
110200131016     c*****              movel     kpjbu         tisio7ds
110300130913    3c                   if        oo7err='1'
110400130913     c                   movel     oo7msg        DSmsms
110500130913     c                   eval      *in67  = *on
110600130913     c                   eval      ErrL00 = *on
110700130913     c                   eval      f07lna='S'
110800130913e   3c                   endif
110900130913c
111000130913     c                   eval      s_cpclol=cpclol
111100130913    2c                   endif
111200130903E   1C                   ENDIF
111300970604     C*--------------------
111400970912     C* ZONA DI CONSEGNA OLTRE
111500970604     C*--------------------
111600970604IF  1C     ERRL00        IFEQ      '0'
111700971126IF  2C     VIDLOL        IFNE      *ZEROS
111800970604     C                   MOVEL     VIDLOL        WLIN
111900970604     C                   Z-ADD     VIDZOL        WZNC
112000970604     C                   EXSR      CHKZNC
112100970604IF  3C     ERRL00        IFEQ      '1'
112200970604     C                   SETON                                        68
112300970604     C                   MOVEL     MSG(11)       DSMSMS
112400970604E   3C                   ENDIF
112500060206if  3c                   if        §03nuc = 'S'
112600060206     c                   movel     Msg(17)       DSmsms
112700060206     c                   eval      *in68  = *on
112800060206     c                   eval      ErrL00 = *on
112900060206e   3c                   endif
113000970604E   2C                   ENDIF
113100970604E   1C                   ENDIF
113200970604     C*--------------------
113300970912     C* PESO/VOLUME OLTRE
113400970604     C*--------------------
113500970604IF  1C     ERRL00        IFEQ      '0'
113600970604IF  2C     VIDLKG        IFGT      *ZEROS
113700970604     C     VIDLMC        ORGT      *ZEROS
113800970604IF  3C     VIDLOL        IFEQ      *BLANKS
113900970604     C     VIDLOL        OREQ      *ZEROS
114000970604     C                   SETON                                        67
114100970604     C                   MOVEL     MSG(18)       DSMSMS
114200970604     C                   MOVEL     '1'           ERRL00
114300970604E   3C                   ENDIF
114400070103E   2C                   ENDIF
114500070103IF  3C     VIDLOL        IFNE      *BLANKS
114600070103     C     VIDLOL        ANDNE     *ZEROS
114700070103IF  4C                   IF        VIDLKG = *ZEROS OR
114800070103     C                             VIDLMC = *ZEROS
114900970604     C                   SETON                                        7172
115000970604     C                   MOVEL     MSG(19)       DSMSMS
115100970604     C                   MOVEL     '1'           ERRL00
115200070103E   4C                   ENDIF
115300070103E   3C                   ENDIF
115400970604E   1C                   ENDIF
115500970604     C*--------------------
115600970912     C* PESO OLTRE + PESO SOTTO
115700970604     C*--------------------
115800970912     C* OLTRE > SOTTO
115900970604IF  1C     ERRL00        IFEQ      '0'
116000970604IF  2C     VIDLKG        IFGT      *ZEROS
116100970604     C     VIDLKS        ANDGT     *ZEROS
116200970604IF  3C     VIDLKG        IFLE      VIDLKS
116300970604     C                   SETON                                        7183
116400970604     C                   MOVEL     MSG(21)       DSMSMS
116500970604     C                   MOVEL     '1'           ERRL00
116600970604E   3C                   ENDIF
116700970604E   2C                   ENDIF
116800970604E   1C                   ENDIF
116900970604     C*--------------------
117000970912     C* VOLUME OLTRE + VOLUME SOTTO
117100970604     C*--------------------
117200970912     C* OLTRE > SOTTO
117300970604IF  1C     ERRL00        IFEQ      '0'
117400970604IF  2C     VIDLMC        IFGT      *ZEROS
117500970604     C     VIDLMS        ANDGT     *ZEROS
117600970604IF  3C     VIDLMC        IFLE      VIDLMS
117700970604     C                   SETON                                        7284
117800970604     C                   MOVEL     MSG(21)       DSMSMS
117900970604     C                   MOVEL     '1'           ERRL00
118000970604E   3C                   ENDIF
118100970604E   2C                   ENDIF
118200970604E   1C                   ENDIF
118300971118     C*--------------------
118400971118     C* LINEA ARRIVO OLTRE 2
118500971118     C*--------------------
118600971118IF  1C     ERRL00        IFEQ      '0'
118700971118     C                   MOVEL     *BLANKS       VDDLO2
118800971118IF  2C     VIDLO2        IFNE      *ZEROS
118900971118     C     VIDLO2        ANDNE     *BLANKS
119000971118     C                   MOVEL     VIDLO2        WLIN
119100971118     C                   MOVEL     *BLANKS       WDLIN
119200971118     C                   EXSR      CHKLIN
119300971118     C                   MOVEL     WLIN          VIDLO2
119400971118     C                   MOVEL     WDLIN         VDDLO2
119500971118IF  3C     ERRL00        IFEQ      '1'
119600971118     C                   SETON                                        86
119700971118     C                   MOVEL     MSG(10)       DSMSMS
119800971118E   3C                   ENDIF
119900971118E   2C                   ENDIF
120000971118E   1C                   ENDIF
120100971118     C*--------------------
120200971118     C* ZONA DI CONSEGNA OLTRE 2
120300971118     C*--------------------
120400971118IF  1C     ERRL00        IFEQ      '0'
120500971126IF  2C     VIDLO2        IFNE      *ZEROS
120600971118     C                   MOVEL     VIDLO2        WLIN
120700971118     C                   Z-ADD     VIDZO2        WZNC
120800971118     C                   EXSR      CHKZNC
120900971118IF  3C     ERRL00        IFEQ      '1'
121000971118     C                   SETON                                        87
121100971118     C                   MOVEL     MSG(11)       DSMSMS
121200971118E   3C                   ENDIF
121300060206if  3c                   if        §03nuc = 'S'
121400060206     c                   movel     Msg(17)       DSmsms
121500060206     c                   eval      *in87  = *on
121600060206     c                   eval      ErrL00 = *on
121700060206e   3c                   endif
121800971118E   2C                   ENDIF
121900971118E   1C                   ENDIF
122000970526     C*--------------------
122100970526     C* LINEA ARRIVO FERMO DEPOSITO -OUTPUT-
122200970526     C*--------------------
122300970526IF  1C     ERRL00        IFEQ      '0'
122400971219     C                   MOVEL     VIDLNA        VIDLFD
122500970526     C                   MOVEL     *BLANKS       VDDLFD
122600970526IF  2C     VIDLFD        IFGT      *ZEROS
122700970526     C                   MOVEL     VIDLFD        WLIN
122800970526     C                   MOVEL     *BLANKS       WDLIN
122900970526     C                   EXSR      CHKLIN
123000970526     C                   MOVEL     WLIN          VIDLFD
123100970526     C                   MOVEL     WDLIN         VDDLFD
123200970526E   2C                   ENDIF
123300970526E   1C                   ENDIF
123400970526     C*--------------------
123500970526     C* ZONA DI CONSEGNA FERMO DEPOSITO
123600970526     C*--------------------
123700970526IF  1C     ERRL00        IFEQ      '0'
123800971126IF  2C     VIDLFD        IFNE      *ZEROS
123900970526     C                   MOVEL     VIDLFD        WLIN
124000970526     C                   Z-ADD     VIDZFD        WZNC
124100970526     C                   EXSR      CHKZNC
124200970526IF  3C     ERRL00        IFEQ      '1'
124300970526     C                   SETON                                        66
124400970526     C                   MOVEL     MSG(11)       DSMSMS
124500970526E   3C                   ENDIF
124600060206if  3c                   if        §03nuc = 'S'
124700060206     c                   movel     Msg(17)       DSmsms
124800060206     c                   eval      *in66  = *on
124900060206     c                   eval      ErrL00 = *on
125000060206e   3c                   endif
125100970526E   2C                   ENDIF
125200970526E   1C                   ENDIF
125300970526     C*--------------------
125400970526     C* PROVINCIA
125500970526     C*--------------------
125600970526IF  1C     ERRL00        IFEQ      '0'
125700970526     C                   MOVEL     *BLANKS       VDDPRV
125800970526IF  2C     VIDPRV        IFEQ      *BLANKS                                      *PRV NON IMMESSA
125900970526IF  3C     DEPCPR        IFEQ      'S'                                          *PRV DA CONTROLLARE
126000970526     C                   MOVEL     '1'           ERRL00
126100970526     C                   SETON                                        64
126200970526     C                   MOVEL     MSG(9)        DSMSMS
126300970526E   3C                   ENDIF
126400970526X   2C                   ELSE                                                   *PRV IMMESSA
126500970526     C                   MOVEL     VIDPRV        WPRV
126600970526     C                   MOVEL     *BLANKS       WDPRV
126700970526     C                   EXSR      CHKPRV
126800970526     C                   MOVEL     WPRV          VIDPRV
126900970526     C                   MOVEL     WDPRV         VDDPRV
127000970526IF  3C     ERRL00        IFEQ      '1'
127100970526     C                   SETON                                        64
127200970526     C                   MOVEL     MSG(9)        DSMSMS
127300970526E   3C                   ENDIF
127400970526E   2C                   ENDIF
127500970526E   1C                   ENDIF
127600970526     C*--------------------
127700970526     C* PROVINCIA ALTERNATIVA
127800970526     C*--------------------
127900970526IF  1C     ERRL00        IFEQ      '0'
128000970526     C                   MOVEL     *BLANKS       VDDPRA
128100970526IF  2C     VIDPRA        IFNE      *BLANKS
128200970526     C                   MOVEL     VIDPRA        WPRV
128300970526     C                   MOVEL     *BLANKS       WDPRV
128400970526     C                   EXSR      CHKPRV
128500970526     C                   MOVEL     WPRV          VIDPRA
128600970526     C                   MOVEL     WDPRV         VDDPRA
128700970526IF  3C     ERRL00        IFEQ      '1'
128800970526     C                   SETON                                        65
128900970526     C                   MOVEL     MSG(9)        DSMSMS
129000970526E   3C                   ENDIF
129100970526E   2C                   ENDIF
129200970526E   1C                   ENDIF
129300970526     C*--------------------
129400970526     C* CODICE TASSAZIONE
129500970526     C*--------------------
129600970526IF  1C     ERRL00        IFEQ      '0'
129700970526     C                   MOVEL     *BLANKS       VDDCTS
129800970526     C                   MOVEL     VIDCTS        WCTS
129900970526     C                   MOVEL     *BLANKS       WDCTS
130000970526     C                   EXSR      CHKCTS
130100970526     C                   MOVEL     WCTS          VIDCTS
130200970526     C                   MOVEL     WDCTS         VDDCTS
130300970526IF  2C     ERRL00        IFEQ      '1'
130400970526     C                   SETON                                        58
130500970526     C                   MOVEL     MSG(14)       DSMSMS
130600970526E   2C                   ENDIF
130700970526E   1C                   ENDIF
130800970526     C*--------------------
130900970526     C* CODICE TASSAZIONE ITALIA PER ESTERO
131000970526     C*--------------------
131100970526IF  1C     ERRL00        IFEQ      '0'
131200970526     C                   MOVEL     *BLANKS       VDDCTE
131300970526IF  2C     VIDNAR        IFEQ      *BLANKS                                      *NAZIONE ITALIA
131400970526     C                   MOVEL     VIDCTE        WA1D              1
131500970526     C                   MOVE      VIDCTE        WA1S              1
131600970526IF  3C     WA1D          IFNE      'I'
131700970526     C     WA1S          OREQ      *BLANKS
131800970526     C                   MOVEL     '1'           ERRL00
131900970526     C                   SETON                                        59
132000970526     C                   MOVEL     MSG(15)       DSMSMS
132100970526E   3C                   ENDIF
132200970526X   2C                   ELSE                                                   *NAZIONE ESTERO
132300970526IF  3C     VIDCTE        IFNE      *BLANKS
132400970526     C                   MOVEL     '1'           ERRL00
132500970526     C                   SETON                                        59
132600970526     C                   MOVEL     MSG(16)       DSMSMS
132700970526E   3C                   ENDIF
132800970526E   2C                   ENDIF
132900970526E   1C                   ENDIF
133000970526     C*--------------------
133100970526     C* CAP FITTIZIO
133200970526     C*--------------------
133300970526IF  1C     ERRL00        IFEQ      '0'
133400970526IF  2C     VIDFIT        IFEQ      'S'
133500970526     C     VIDFIT        OREQ      'N'
133600970526X   2C                   ELSE
133700970526     C                   MOVEL     '1'           ERRL00
133800970526     C                   SETON                                        73
133900970526     C                   MOVEL     MSG(27)       DSMSMS
134000970526E   2C                   ENDIF
134100970526E   1C                   ENDIF
134200970526     C*--------------------
134300970526     C* CITTA CON VIARIO
134400970526     C*--------------------
134500970526IF  1C     ERRL00        IFEQ      '0'
134600970526IF  2C     VIDFLG        IFEQ      'S'
134700970526     C     VIDFLG        OREQ      'N'
134800970526X   2C                   ELSE
134900970526     C                   MOVEL     '1'           ERRL00
135000970526     C                   SETON                                        74
135100970526     C                   MOVEL     MSG(27)       DSMSMS
135200970526E   2C                   ENDIF
135300970526E   1C                   ENDIF
135400970526     C*--------------------
135500970526     C* INOLTRO
135600970526     C*--------------------
135700970526IF  1C     ERRL00        IFEQ      '0'
135800970526     C                   MOVEL     *BLANKS       VDDISO
135900970526     C                   MOVEL     VIDISO        WINL
136000970526     C                   MOVEL     *BLANKS       WDINL
136100970526     C                   EXSR      CHKINL
136200970526     C                   MOVEL     WINL          VIDISO
136300970526     C                   MOVEL     WDINL         VDDISO
136400970526IF  2C     ERRL00        IFEQ      '1'
136500970526     C                   SETON                                        75
136600970526     C                   MOVEL     MSG(28)       DSMSMS
136700970603X   2C                   ELSE
136800970526E   2C                   ENDIF
136900970526E   1C                   ENDIF
137000970526     C*--------------------
137100970526     C* POSSIBILE INOLTRO ISOLA
137200970526     C*--------------------
137300970526IF  1C     ERRL00        IFEQ      '0'
137400970526IF  2C     VIDPII        IFEQ      'S'
137500970526     C     VIDPII        OREQ      'N'
137600970526X   2C                   ELSE
137700970526     C                   MOVEL     '1'           ERRL00
137800970526     C                   SETON                                        76
137900970526     C                   MOVEL     MSG(27)       DSMSMS
138000970526E   2C                   ENDIF
138100970526E   1C                   ENDIF
138200970526     C*--------------------
138300970526     C* POSSIBILE INOLTRO LOCALITA' DISAGIATA
138400970526     C*--------------------
138500970526IF  1C     ERRL00        IFEQ      '0'
138600970526IF  2C     VIDPID        IFEQ      'S'
138700970526     C     VIDPID        OREQ      'N'
138800970526X   2C                   ELSE
138900970526     C                   MOVEL     '1'           ERRL00
139000970526     C                   SETON                                        77
139100970526     C                   MOVEL     MSG(27)       DSMSMS
139200970526E   2C                   ENDIF
139300970526E   1C                   ENDIF
139400970526     C*--------------------
139500970526     C* POSSIBILE INOLTRO CENTRO STORICO
139600970526     C*--------------------
139700970526IF  1C     ERRL00        IFEQ      '0'
139800970526IF  2C     VIDPIS        IFEQ      'S'
139900970526     C     VIDPIS        OREQ      'N'
140000970526X   2C                   ELSE
140100970526     C                   MOVEL     '1'           ERRL00
140200970526     C                   SETON                                        78
140300970526     C                   MOVEL     MSG(27)       DSMSMS
140400970526E   2C                   ENDIF
140500970526E   1C                   ENDIF
140600970526     C*--------------------
140700970526     C* GESTIONE DDT
140800970526     C*--------------------
140900970526IF  1C     ERRL00        IFEQ      '0'
141000970526IF  2C     VIDGDD        IFEQ      'S'
141100970526     C     VIDGDD        OREQ      'N'
141200970526X   2C                   ELSE
141300970526     C                   MOVEL     '1'           ERRL00
141400970526     C                   SETON                                        79
141500970526     C                   MOVEL     MSG(27)       DSMSMS
141600970526E   2C                   ENDIF
141700970526E   1C                   ENDIF
141800970526     C*--------------------
141900970526     C* TIPO TEMPO CONSEGNA
142000010322     C*--------------------
142100970526IF  1C     ERRL00        IFEQ      '0'
142200970526     C                   MOVEL     VIDTTC        WTTC                           *CODICE TEMPO
142300970526     C                   Z-ADD     *ZEROS        WD0I                           *TEMPI DISTRIBUZIONE
142400970526     C                   Z-ADD     *ZEROS        WD1I
142500970526     C                   Z-ADD     *ZEROS        WD0C
142600970526     C                   Z-ADD     *ZEROS        WD1C
142700970526     C                   Z-ADD     *ZEROS        WDIS
142800970526     C                   Z-ADD     *ZEROS        WDCS
142900970526     C                   Z-ADD     *ZEROS        WDLD
143000090903
143100970526     C                   Z-ADD     *ZEROS        WC0I                           *TEMPI CORRIERE
143200970526     C                   Z-ADD     *ZEROS        WC1I
143300970526     C                   Z-ADD     *ZEROS        WC0C
143400970526     C                   Z-ADD     *ZEROS        WC1C
143500970526     C                   Z-ADD     *ZEROS        WCIS
143600970526     C                   Z-ADD     *ZEROS        WCCS
143700970526     C                   Z-ADD     *ZEROS        WCLD
143800090903
143900970526     C                   Z-ADD     *ZEROS        WE0I                           *TEMPI ESPRESSO
144000970526     C                   Z-ADD     *ZEROS        WE1I
144100970526     C                   Z-ADD     *ZEROS        WE0C
144200970526     C                   Z-ADD     *ZEROS        WE1C
144300970526     C                   Z-ADD     *ZEROS        WEIS
144400970526     C                   Z-ADD     *ZEROS        WECS
144500970526     C                   Z-ADD     *ZEROS        WELD
144600090903
144700090903     C                   Z-ADD     *ZEROS        Wh0I                           *TEMPI H 1030
144800090903     C                   Z-ADD     *ZEROS        Wh1I
144900090903     C                   Z-ADD     *ZEROS        Wh0C
145000090903     C                   Z-ADD     *ZEROS        Wh1C
145100090903     C                   Z-ADD     *ZEROS        WhIS
145200090903     C                   Z-ADD     *ZEROS        WhCS
145300090903     C                   Z-ADD     *ZEROS        WhLD
145400970526     C                   EXSR      CHKTTC
145500970530     C                   MOVEL     WTTC          VIDTTC                         *CODICE TEMPO
145600970526     C                   Z-ADD     WD0I          VIDD0I                         *TEMPI DISTRIBUZIONE
145700970526     C                   Z-ADD     WD1I          VIDD1I
145800970526     C                   Z-ADD     WD0C          VIDD0C
145900970526     C                   Z-ADD     WD1C          VIDD1C
146000970526     C                   Z-ADD     WDIS          VIDDIS
146100970526     C                   Z-ADD     WDCS          VIDDCS
146200970526     C                   Z-ADD     WDLD          VIDDLD
146300090903
146400970526     C                   Z-ADD     WC0I          VIDC0I                         *TEMPI CORRIERE
146500970526     C                   Z-ADD     WC1I          VIDC1I
146600970526     C                   Z-ADD     WC0C          VIDC0C
146700970526     C                   Z-ADD     WC1C          VIDC1C
146800970526     C                   Z-ADD     WCIS          VIDCIS
146900970526     C                   Z-ADD     WCCS          VIDCCS
147000970526     C                   Z-ADD     WCLD          VIDCLD
147100090903
147200970526     C                   Z-ADD     WE0I          VIDE0I                         *TEMPI ESPRESSO
147300970526     C                   Z-ADD     WE1I          VIDE1I
147400970526     C                   Z-ADD     WE0C          VIDE0C
147500970526     C                   Z-ADD     WE1C          VIDE1C
147600970526     C                   Z-ADD     WEIS          VIDEIS
147700970526     C                   Z-ADD     WECS          VIDECS
147800970526     C                   Z-ADD     WELD          VIDELD
147900090903
148000090903     C                   Z-ADD     Wh0I          VIDh0I                         *TEMPI H1030
148100090903     C                   Z-ADD     Wh1I          VIDh1I
148200090903     C                   Z-ADD     Wh0C          VIDh0C
148300090903     C                   Z-ADD     Wh1C          VIDh1C
148400090903     C                   Z-ADD     WhIS          VIDhIS
148500090903     C                   Z-ADD     WhCS          VIDhCS
148600090903     C                   Z-ADD     WhLD          VIDhLD
148700970526IF  2C     ERRL00        IFEQ      '1'
148800970526     C                   SETON                                        80
148900970526     C                   MOVEL     MSG(30)       DSMSMS
149000970526E   2C                   ENDIF
149100970526E   1C                   ENDIF
149200010320     C*--------------------
149300010320     C* TIPO TEMPO RITIRO
149400010320     C*--------------------
149500010320IF  1C     ERRL00        IFEQ      '0'
149600010320     C                   MOVEL     VIDTTR        WTTR                           *CODICE TEMPO
149700010320     C                   Z-ADD     *ZEROS        WD0IR                          *TEMPI DISTRIBUZIONE
149800010320     C                   Z-ADD     *ZEROS        WD1IR
149900010320     C                   Z-ADD     *ZEROS        WD0CR
150000010320     C                   Z-ADD     *ZEROS        WD1CR
150100010320     C                   Z-ADD     *ZEROS        WDISR
150200010320     C                   Z-ADD     *ZEROS        WDCSR
150300010320     C                   Z-ADD     *ZEROS        WDLDR
150400010320     C                   Z-ADD     *ZEROS        WC0IR                          *TEMPI CORRIERE
150500010320     C                   Z-ADD     *ZEROS        WC1IR
150600010320     C                   Z-ADD     *ZEROS        WC0CR
150700010320     C                   Z-ADD     *ZEROS        WC1CR
150800010320     C                   Z-ADD     *ZEROS        WCISR
150900010320     C                   Z-ADD     *ZEROS        WCCSR
151000010320     C                   Z-ADD     *ZEROS        WCLDR
151100010320     C                   Z-ADD     *ZEROS        WE0IR                          *TEMPI ESPRESSO
151200010320     C                   Z-ADD     *ZEROS        WE1IR
151300010320     C                   Z-ADD     *ZEROS        WE0CR
151400010320     C                   Z-ADD     *ZEROS        WE1CR
151500010320     C                   Z-ADD     *ZEROS        WEISR
151600010320     C                   Z-ADD     *ZEROS        WECSR
151700010320     C                   Z-ADD     *ZEROS        WELDR
151800010320     C                   EXSR      CHKTTR
151900010320     C                   MOVEL     WTTR          VIDTTR                         *CODICE TEMPO
152000010320     C                   Z-ADD     WD0IR         VIDD0IR                        *TEMPI DISTRIBUZIONE
152100010320     C                   Z-ADD     WD1IR         VIDD1IR
152200010320     C                   Z-ADD     WD0CR         VIDD0CR
152300010320     C                   Z-ADD     WD1CR         VIDD1CR
152400010320     C                   Z-ADD     WDISR         VIDDISR
152500010320     C                   Z-ADD     WDCSR         VIDDCSR
152600010320     C                   Z-ADD     WDLDR         VIDDLDR
152700010320     C                   Z-ADD     WC0IR         VIDC0IR                        *TEMPI CORRIERE
152800010320     C                   Z-ADD     WC1IR         VIDC1IR
152900010320     C                   Z-ADD     WC0CR         VIDC0CR
153000010320     C                   Z-ADD     WC1CR         VIDC1CR
153100010320     C                   Z-ADD     WCISR         VIDCISR
153200010320     C                   Z-ADD     WCCSR         VIDCCSR
153300010320     C                   Z-ADD     WCLDR         VIDCLDR
153400010320     C                   Z-ADD     WE0IR         VIDE0IR                        *TEMPI ESPRESSO
153500010320     C                   Z-ADD     WE1IR         VIDE1IR
153600010320     C                   Z-ADD     WE0CR         VIDE0CR
153700010320     C                   Z-ADD     WE1CR         VIDE1CR
153800010320     C                   Z-ADD     WEISR         VIDEISR
153900010320     C                   Z-ADD     WECSR         VIDECSR
154000010320     C                   Z-ADD     WELDR         VIDELDR
154100010320IF  2C     ERRL00        IFEQ      '1'
154200010320     C                   SETON                                        44
154300010320     C                   MOVEL     MSG(24)       DSMSMS
154400010320E   2C                   ENDIF
154500010320E   1C                   ENDIF
154600970917     C*--------------------
154700970917     C* OBSOLETO
154800970917     C*--------------------
154900970917IF  1C     ERRL00        IFEQ      '0'
155000970917IF  2C     VIDFI1        IFEQ      'O'
155100970917     C     VIDFI1        OREQ      *BLANKS
155200970917X   2C                   ELSE
155300970917     C                   MOVEL     '1'           ERRL00
155400970917     C                   SETON                                        85
155500970917     C                   MOVEL     MSG(29)       DSMSMS
155600970917E   2C                   ENDIF
155700970917E   1C                   ENDIF
155800970917     C*
155900970917     C* INSERIMENTO -> NON POSSO INSERIRE CAP OBSOLETI
156000970917IF  1C     ERRL00        IFEQ      '0'
156100970917IF  2C     DEPOP1        IFEQ      'F06'
156200970917IF  3C     VIDFI1        IFEQ      'O'
156300970917     C                   SETON                                        85
156400970917     C                   MOVEL     MSG(26)       DSMSMS
156500970917     C                   MOVEL     '1'           ERRL00
156600970917E   3C                   ENDIF
156700970917E   2C                   ENDIF
156800970917E   1C                   ENDIF
156900970917     C*
157000970917     C* MODIFICA -> SE CAP OBSOLETO LO DEVONO ESSERE ANCHE TUTTE LE SUE LOCALITA'
157100970917IF  1C     ERRL00        IFEQ      '0'
157200970917IF  2C     DEPOP1        IFEQ      'O02'
157300970917IF  3C     VIDFI1        IFEQ      'O'
157400970917     C                   MOVEL     'S'           $LOBS
157500970917     C                   Z-ADD     D00VER        KCLVER
157600970917     C                   MOVEL     VIDNAR        KCLNAR
157700970917     C                   MOVEL     VIDCAP        KCLCAP
157800970917     C     KEYCPL        SETLL     AZCPL01L
157900970917     C     KEYCPL        READE(N)  AZCPL01L                               99
158000970917DO  4C     *IN99         DOWEQ     *OFF
158100970917IF  5C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
158200970917IF  6C     CPLFI1        IFNE      'O '                                         *NO OBSOLETO
158300970917     C                   MOVEL     'N'           $LOBS
158400970917     C                   SETON                                        85
158500970917     C                   MOVEL     MSG(25)       DSMSMS
158600970917     C                   MOVEL     '1'           ERRL00
158700970917     C                   LEAVE
158800970917E   6C                   ENDIF
158900970917E   5C                   ENDIF
159000970917     C     KEYCPL        READE(N)  AZCPL01L                               99
159100970917E   4C                   ENDDO                                                  *FINE LOCALITA'
159200970917E   3C                   ENDIF
159300970917E   2C                   ENDIF
159400970917E   1C                   ENDIF
159500060927     C***
159600970526     C                   ENDSR
159700970526     C*------------------------------------------------------------------------*
159800970526     C* CHKNAR - CONTROLLA LA NAZIONE
159900970526     C*------------------------------------------------------------------------*
160000970526     C     CHKNAR        BEGSR
160100970526     C*
160200970526     C     *LIKE         DEFINE    VIDNAR        WNAR
160300970526     C     *LIKE         DEFINE    VDDNAR        WDNAR
160400970526     C     *LIKE         DEFINE    §15CPR        WCPR
160500970526     C*
160600970526     C* RICHIESTO ELENCO
160700970526     C                   SETOFF                                           28
160800970526     C     '?'           SCAN      WNAR                                   28
160900970526IF  1C     *IN28         IFEQ      *ON
161000020906     C                   Z-ADD     1             §KUT              1 0
161100970526     C                   MOVEL     '15'          §COD              2
161200970526     C                   MOVEL     *BLANKS       §KEY              8
161300970526     C                   MOVEL     *BLANKS       §DES             30
161400970526     C                   CALL      'X§TABER'
161500970526     C                   PARM                    §KUT
161600970526     C                   PARM                    §COD
161700970526     C                   PARM                    §KEY
161800970526     C                   PARM                    §DES
161900970526     C                   MOVEL     §KEY          WNAR
162000970526     C                   MOVEL     §DES          WDNAR
162100970526E   1C                   ENDIF
162200970526     C*
162300970526     C* CONTROLLA ESISTENZA CODICE
162400020906     C                   MOVEL     1             KTBKUT
162500970526     C                   MOVEL     '15'          KTBCOD
162600970526     C                   MOVEL(P)  WNAR          KTBKEY
162700970526     C     KEYTAB        CHAIN     TABEL00F                           99
162800970526IF  1C     *IN99         IFEQ      *OFF
162900970526     C     TBLFLG        ANDEQ     *BLANKS
163000970526     C                   MOVEL     TBLUNI        DS15
163100970526     C                   MOVEL     §15DES        WDNAR
163200970526     C                   MOVEL     §15CPR        WCPR
163300970526     C*
163400970526     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
163500970526IF  2C     VIDNAR        IFEQ      'I  '
163600970526     C                   MOVEL     '1'           ERRL00
163700970526     C                   MOVEL     *BLANKS       WDNAR
163800970526E   2C                   ENDIF
163900970526X   1C                   ELSE
164000970526     C                   MOVEL     '1'           ERRL00
164100970526E   1C                   ENDIF
164200970526     C*
164300970526     C                   ENDSR
164400970526     C*------------------------------------------------------------------------*
164500970526     C* CHKCAP - CONTROLLA IL CAP
164600970526     C*------------------------------------------------------------------------*
164700970526     C     CHKCAP        BEGSR
164800970526     C*
164900970526     C     *LIKE         DEFINE    VIDCAP        WCAP
165000970526     C*
165100970626     C* CONTROLLA IL CAP
165200970626     C                   CLEAR                   DSSI95
165300970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
165400970626     C                   MOVEL     '1'           I95TCN                         *CONTROLLO FORMALE
165500970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
165600970626     C                   MOVEL     WCAP          I95CAP                         *CAP
165700970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
165800970626     C                   CALL      'TISI95R'
165900970626     C                   PARM                    DSSI95
166000970626IF  1C     O95ERR        IFNE      *BLANKS
166100970703     C     VIDFIT        ANDNE     'S'
166200970526     C                   MOVEL     '1'           ERRL00
166300970626     C                   MOVEL     O95MSG        DSMSMS
166400970526E   1C                   ENDIF
166500970526     C*
166600970526     C                   ENDSR
166700970526     C*------------------------------------------------------------------------*
166800970526     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
166900970526     C*------------------------------------------------------------------------*
167000970526     C     CHKLIN        BEGSR
167100970526     C*
167200970526     C     *LIKE         DEFINE    VIDLNA        WLIN
167300970526     C     *LIKE         DEFINE    VDDLNA        WDLIN
167400970526     C*
167500970526     C* RICHIESTO ELENCO
167600970526     C                   SETOFF                                           28
167700970526     C     '?'           SCAN      WLIN                                   28
167800970526IF  1C     *IN28         IFEQ      *ON
167900970526     C                   MOVEL     *BLANKS       §COD1             3
168000970526     C                   MOVEL     *BLANKS       §TIP              1
168100970526     C                   MOVEL     *BLANKS       §DES1            25
168200971203     C                   MOVEL     *BLANKS       §PRA              3
168300971203     C                   CALL      'TNSD24R'
168400970526     C                   PARM                    §COD1
168500970526     C                   PARM                    §TIP
168600970526     C                   PARM                    §DES1
168700971203     C                   PARM                    §PRA
168800970526     C                   MOVEL     §COD1         WLIN
168900970526     C                   MOVEL     §DES1         WDLIN
169000970526E   1C                   ENDIF
169100970526     C*
169200970526     C* SOLO CARATTERI NUMERICI
169300970526     C                   SETOFF                                       28
169400970526     C                   TESTN                   WLIN                 28
169500970526IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
169600970526     C                   MOVEL     '1'           ERRL00
169700970526X   1C                   ELSE                                                   *E'NUMERO DA TESTN
169800970526     C                   MOVE      WLIN          W001              1
169900970526IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
170000970526     C                   MOVEL     '1'           ERRL00
170100970526X   2C                   ELSE                                                   *NUMERO
170200970526     C*
170300970526     C* CONTROLLA ESISTENZA CODICE
170400970526     C                   MOVEL     WLIN          KORFIL
170500970526     C     KEYORG        CHAIN     AZORG01L                           99
170600970526IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
170700970526     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
170800970527     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
170900970526     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
171000970526X   3C                   ELSE
171100970526     C                   MOVEL     '1'           ERRL00
171200970526E   3C                   ENDIF
171300970526E   2C                   ENDIF
171400970526E   1C                   ENDIF
171500970526     C*
171600970526     C                   ENDSR
171700970526     C*------------------------------------------------------------------------*
171800970526     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
171900970526     C*------------------------------------------------------------------------*
172000970526     C     CHKZNC        BEGSR
172100970526     C*
172200970526     C     *LIKE         DEFINE    VIDZNC        WZNC
172300970526     C*
172400970526     C* CONTROLLA ESISTENZA CODICE
172500020906     C                   Z-ADD     1             KTBKUT
172600970526     C                   MOVEL     '03'          KTBCOD
172700970526     C                   MOVEL     WLIN          WA5               5
172800970526     C                   MOVE      WZNC          WA5
172900970526     C                   MOVEL(P)  WA5           KTBKEY
173000970526     C     KEYTAB        CHAIN     TABEL00F                           99
173100970526IF  1C     *IN99         IFEQ      *OFF
173200970526     C     TBLFLG        ANDEQ     *BLANKS
173300970526     C                   MOVEL     TBLUNI        DS03
173400970526X   1C                   ELSE
173500970526     C                   MOVEL     '1'           ERRL00
173600970526E   1C                   ENDIF
173700970526     C*
173800970526     C                   ENDSR
173900970526     C*------------------------------------------------------------------------*
174000970526     C* CHKPRV - CONTROLLA LA PROVINCIA
174100970526     C*------------------------------------------------------------------------*
174200970526     C     CHKPRV        BEGSR
174300970526     C*
174400970526     C     *LIKE         DEFINE    VIDPRV        WPRV
174500970526     C     *LIKE         DEFINE    VDDPRV        WDPRV
174600970526     C*
174700970526     C* RICHIESTO ELENCO
174800970526     C                   SETOFF                                           28
174900970526     C     '?'           SCAN      WPRV                                   28
175000970526IF  1C     *IN28         IFEQ      *ON
175100020906     C                   Z-ADD     1             §KUT              1 0
175200970526     C                   MOVEL     'PR'          §COD              2
175300970526     C                   MOVEL     *BLANKS       §KEY              8
175400970526     C                   MOVEL     *BLANKS       §DES             30
175500970526     C                   CALL      'X§TABER'
175600970526     C                   PARM                    §KUT
175700970526     C                   PARM                    §COD
175800970526     C                   PARM                    §KEY
175900970526     C                   PARM                    §DES
176000970526     C                   MOVEL     §KEY          WPRV
176100970526     C                   MOVEL     §DES          WDPRV
176200970526E   1C                   ENDIF
176300970526     C*
176400970526     C* CONTROLLA ESISTENZA CODICE
176500020906     C                   MOVEL     1             KTBKUT
176600970526     C                   MOVEL     'PR'          KTBCOD
176700970526     C                   MOVEL(P)  WPRV          KTBKEY
176800970526     C     KEYTAB        CHAIN     TABEL00F                           99
176900970526IF  1C     *IN99         IFEQ      *OFF
177000970526     C     TBLFLG        ANDEQ     *BLANKS
177100970526     C                   MOVEL     TBLUNI        DSPR
177200970526     C                   MOVEL     §PRDES        WDPRV
177300970526X   1C                   ELSE
177400970526     C                   MOVEL     '1'           ERRL00
177500970526E   1C                   ENDIF
177600970526     C*
177700970526     C                   ENDSR
177800970526     C*------------------------------------------------------------------------*
177900970526     C* CHKCTS - CONTROLLA IL CODICE TASSAZIONE
178000970526     C*------------------------------------------------------------------------*
178100970526     C     CHKCTS        BEGSR
178200970526     C*
178300970526     C     *LIKE         DEFINE    VIDCTS        WCTS
178400970526     C     *LIKE         DEFINE    VDDCTS        WDCTS
178500970526     C*
178600970526     C* RICHIESTO ELENCO
178700970526     C                   SETOFF                                           28
178800970526     C     '?'           SCAN      WCTS                                   28
178900970526IF  1C     *IN28         IFEQ      *ON
179000120712     c                   clear                   TRTB09DS
179100120712     c                   eval      TB09fun = '1'
179200120712     c                   eval      kpjbu = TRTB09DS
179300120712     c                   call      'TRTB09R'
179400120712     c                   parm                    kpjba
179500120712     c                   eval      TRTB09DS = kpjbu
179600120712     c                   eval      wcts  = TB09cod
179700120712     c                   eval      wdcts = TB09des
179800970526E   1C                   ENDIF
179900970526     C*
180000970526     C* CONTROLLA ESISTENZA CODICE
180100020906     C                   MOVEL     1             KTBKUT
180200970526     C                   MOVEL     'CT'          KTBCOD
180300970526     C                   MOVEL(P)  WCTS          KTBKEY
180400970526     C     KEYTAB        CHAIN     TABEL00F                           99
180500970526IF  1C     *IN99         IFEQ      *OFF
180600970526     C     TBLFLG        ANDEQ     *BLANKS
180700970526     C                   MOVEL     TBLUNI        DSCT
180800970526     C                   MOVEL     §CTDES        WDCTS
180900970526X   1C                   ELSE
181000970526     C                   MOVEL     '1'           ERRL00
181100970526E   1C                   ENDIF
181200970526     C*
181300970526     C                   ENDSR
181400970526     C*------------------------------------------------------------------------*
181500970526     C* CHKINL - CONTROLLA L'INOLTRO
181600970526     C*------------------------------------------------------------------------*
181700970526     C     CHKINL        BEGSR
181800970526     C*
181900970526     C     *LIKE         DEFINE    VIDISO        WINL
182000970526     C     *LIKE         DEFINE    VDDISO        WDINL
182100970526     C*
182200970526     C* RICHIESTO ELENCO
182300970526     C                   SETOFF                                           28
182400970526     C     '?'           SCAN      WINL                                   28
182500970526IF  1C     *IN28         IFEQ      *ON
182600020906     C                   Z-ADD     1             §KUT              1 0
182700970526     C                   MOVEL     '9A'          §COD              2
182800970526     C                   MOVEL     *BLANKS       §KEY              8
182900970526     C                   MOVEL     *BLANKS       §DES             30
183000970526     C                   CALL      'X§TABER'
183100970526     C                   PARM                    §KUT
183200970526     C                   PARM                    §COD
183300970526     C                   PARM                    §KEY
183400970526     C                   PARM                    §DES
183500970526     C                   MOVEL     §KEY          WINL
183600970526     C                   MOVEL     §DES          WDINL
183700970526E   1C                   ENDIF
183800970526     C*
183900970526     C* CONTROLLA ESISTENZA CODICE
184000020906     C                   MOVEL     1             KTBKUT
184100970526     C                   MOVEL     '9A'          KTBCOD
184200970526     C                   MOVEL(P)  WINL          KTBKEY
184300970526     C     KEYTAB        CHAIN     TABEL00F                           99
184400970526IF  1C     *IN99         IFEQ      *OFF
184500970526     C     TBLFLG        ANDEQ     *BLANKS
184600970526     C                   MOVEL     TBLUNI        DS9A
184700970526     C                   MOVEL     §9ADES        WDINL
184800970526X   1C                   ELSE
184900970526     C                   MOVEL     '1'           ERRL00
185000970526E   1C                   ENDIF
185100110623     c
185200110623     c* Se non utilizzabile  nel cappario errore
185300110623     c                   if        §9auticpc='N'  and errl00='0'
185400110623     C                   MOVEL     '1'           ERRL00
185500110623E   1C                   ENDIF
185600970526     C*
185700970526     C                   ENDSR
185800970526     C*------------------------------------------------------------------------*
185900970526     C* CHKTTC - CONTROLLA IL TIPO TEMPO CONSEGNA
186000970526     C*------------------------------------------------------------------------*
186100970526     C     CHKTTC        BEGSR
186200970526     C*
186300970526     C     *LIKE         DEFINE    VIDTTC        WTTC                           *TIPO TEMPO
186400970526     C     *LIKE         DEFINE    CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
186500970526     C     *LIKE         DEFINE    CPT1IN        WD1I
186600970526     C     *LIKE         DEFINE    CPT0CN        WD0C
186700970526     C     *LIKE         DEFINE    CPT1CN        WD1C
186800970526     C     *LIKE         DEFINE    CPTTIS        WDIS
186900970526     C     *LIKE         DEFINE    CPTTCS        WDCS
187000970526     C     *LIKE         DEFINE    CPTTLD        WDLD
187100970526     C     *LIKE         DEFINE    CPT0IN        WC0I                           *TEMPI CORRIERE
187200970526     C     *LIKE         DEFINE    CPT1IN        WC1I
187300970526     C     *LIKE         DEFINE    CPT0CN        WC0C
187400970526     C     *LIKE         DEFINE    CPT1CN        WC1C
187500970526     C     *LIKE         DEFINE    CPTTIS        WCIS
187600970526     C     *LIKE         DEFINE    CPTTCS        WCCS
187700970526     C     *LIKE         DEFINE    CPTTLD        WCLD
187800090903
187900970526     C     *LIKE         DEFINE    CPT0IN        WE0I                           *TEMPI ESPRESSO
188000970526     C     *LIKE         DEFINE    CPT1IN        WE1I
188100970526     C     *LIKE         DEFINE    CPT0CN        WE0C
188200970526     C     *LIKE         DEFINE    CPT1CN        WE1C
188300970526     C     *LIKE         DEFINE    CPTTIS        WEIS
188400970526     C     *LIKE         DEFINE    CPTTCS        WECS
188500970526     C     *LIKE         DEFINE    CPTTLD        WELD
188600090903
188700090903     C     *LIKE         DEFINE    CPT0IN        Wh0I                           *TEMPI H1030
188800090903     C     *LIKE         DEFINE    CPT1IN        Wh1I
188900090903     C     *LIKE         DEFINE    CPT0CN        Wh0C
189000090903     C     *LIKE         DEFINE    CPT1CN        Wh1C
189100090903     C     *LIKE         DEFINE    CPTTIS        WhIS
189200090903     C     *LIKE         DEFINE    CPTTCS        WhCS
189300090903     C     *LIKE         DEFINE    CPTTLD        WhLD
189400970526     C*
189500970526     C* RICHIESTA ELENCO CODICI
189600970526     C                   SETOFF                                           28
189700970526     C     '?'           SCAN      WTTC                                   28
189800970526IF  1C     *IN28         IFEQ      *ON
189900970602     C                   MOVEL     *BLANK        DNAR
190000970602     C                   MOVEL     *BLANKS       DCAP
190100970530     C                   MOVEL     'T01'         WOP0
190200970530     C                   MOVEL     *BLANKS       WOP1
190300970530     C                   MOVEL     'TISI41R'     WPGM              8
190400970530     C                   EXSR      LANPGM
190500970530     C                   MOVEL     D00TTC        WTTC
190600010322E   1C                   ENDIF
190700010322     C*
190800050413     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO CONSEGNA
190900010322     C                   MOVEL     VIDTTC        DEP3A             3
191000010322     C     DEP3A         IFNE      '999'
191100970526     C*
191200970526     C* CONTROLLA ESISTENZA CODICI
191300970526     C                   Z-ADD     D00VER        KCTVER
191400970530     C                   MOVEL     WTTC          KCTTTC
191500970526     C                   MOVEL     'D'           KCTTSP
191600970526     C     KEYCPT        CHAIN     AZCPT01L                           99
191700970526IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
191800970526     C                   Z-ADD     CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
191900970526     C                   Z-ADD     CPT1IN        WD1I
192000970526     C                   Z-ADD     CPT0CN        WD0C
192100970526     C                   Z-ADD     CPT1CN        WD1C
192200970526     C                   Z-ADD     CPTTIS        WDIS
192300970526     C                   Z-ADD     CPTTCS        WDCS
192400970526     C                   Z-ADD     CPTTLD        WDLD
192500970526     C                   MOVEL     'C'           KCTTSP
192600970526     C     KEYCPT        CHAIN     AZCPT01L                           99
192700970526IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
192800970526     C                   Z-ADD     CPT0IN        WC0I                           *TEMPI CORRIERE
192900970526     C                   Z-ADD     CPT1IN        WC1I
193000970526     C                   Z-ADD     CPT0CN        WC0C
193100970526     C                   Z-ADD     CPT1CN        WC1C
193200970526     C                   Z-ADD     CPTTIS        WCIS
193300970526     C                   Z-ADD     CPTTCS        WCCS
193400970526     C                   Z-ADD     CPTTLD        WCLD
193500970526     C                   MOVEL     'E'           KCTTSP
193600970526     C     KEYCPT        CHAIN     AZCPT01L                           99
193700970526IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
193800970526     C                   Z-ADD     CPT0IN        WE0I                           *TEMPI ESPRESSO
193900970526     C                   Z-ADD     CPT1IN        WE1I
194000970526     C                   Z-ADD     CPT0CN        WE0C
194100970526     C                   Z-ADD     CPT1CN        WE1C
194200970526     C                   Z-ADD     CPTTIS        WEIS
194300970526     C                   Z-ADD     CPTTCS        WECS
194400970526     C                   Z-ADD     CPTTLD        WELD
194500090903     C                   MOVEL     'H'           KCTTSP
194600090903     C     KEYCPT        CHAIN     AZCPT01L                           99
194700090903IF  4C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
194800090903     C                   Z-ADD     CPT0IN        Wh0I                           *TEMPI H1030
194900090903     C                   Z-ADD     CPT1IN        Wh1I
195000090903     C                   Z-ADD     CPT0CN        Wh0C
195100090903     C                   Z-ADD     CPT1CN        Wh1C
195200090903     C                   Z-ADD     CPTTIS        WhIS
195300090903     C                   Z-ADD     CPTTCS        WhCS
195400090903     C                   Z-ADD     CPTTLD        WhLD
195500090903X   4C                   ELSE
195600090903     c* Se tipo servizio H non ancora decorrente (anche guardando alla
195700090903     c*    scadenza della versione in visualizzazione)non do errore e non
195800090903     c*    visualizzo nemmeno come descrizione
195900090903
196000090903     c                   if        Datasys>=dataiso
196100090903     c* Verifico anche la  scadenza ddella versione
196200090903     c     d00ver        chain     azcpr01l
196300090903     c                   if        not %found(azcpr01l) or cprdsc>=§vpotsph
196400970603     C                   MOVEL     '1'           ERRL00
196500090903     c                   endif
196600090903     c                   endif
196700090903     c
196800090903     c                   if        errl00='0'
196900090903     c                   clear                   viddtsph
197000090903E   4C                   ENDIF
197100090903
197200090903E   4C                   ENDIF
197300090903
197400090903X   3C                   ELSE
197500090903     C                   MOVEL     '1'           ERRL00
197600090903E   3C                   ENDIF
197700090903
197800970603X   2C                   ELSE
197900970603     C                   MOVEL     '1'           ERRL00
198000970526E   2C                   ENDIF
198100090903
198200970603X   1C                   ELSE
198300970603     C                   MOVEL     '1'           ERRL00
198400970526E   1C                   ENDIF
198500090903
198600010322     C                   ELSE
198700010322     C                   MOVEL     '1'           ERRL00
198800010322     C                   ENDIF
198900970526     C*
199000970526     C                   ENDSR
199100010320     C*------------------------------------------------------------------------*
199200010320     C* CHKTTR - CONTROLLA IL TIPO TEMPO RITIRO
199300010320     C*------------------------------------------------------------------------*
199400010320     C     CHKTTR        BEGSR
199500010320     C*
199600010320     C     *LIKE         DEFINE    VIDTTR        WTTR                           *TIPO TEMPO
199700010320     C     *LIKE         DEFINE    CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
199800010320     C     *LIKE         DEFINE    CPT1IN        WD1IR
199900010320     C     *LIKE         DEFINE    CPT0CN        WD0CR
200000010320     C     *LIKE         DEFINE    CPT1CN        WD1CR
200100010320     C     *LIKE         DEFINE    CPTTIS        WDISR
200200010320     C     *LIKE         DEFINE    CPTTCS        WDCSR
200300010320     C     *LIKE         DEFINE    CPTTLD        WDLDR
200400010320     C     *LIKE         DEFINE    CPT0IN        WC0IR                          *TEMPI CORRIERE
200500010320     C     *LIKE         DEFINE    CPT1IN        WC1IR
200600010320     C     *LIKE         DEFINE    CPT0CN        WC0CR
200700010320     C     *LIKE         DEFINE    CPT1CN        WC1CR
200800010320     C     *LIKE         DEFINE    CPTTIS        WCISR
200900010320     C     *LIKE         DEFINE    CPTTCS        WCCSR
201000010320     C     *LIKE         DEFINE    CPTTLD        WCLDR
201100010320     C     *LIKE         DEFINE    CPT0IN        WE0IR                          *TEMPI ESPRESSO
201200010320     C     *LIKE         DEFINE    CPT1IN        WE1IR
201300010320     C     *LIKE         DEFINE    CPT0CN        WE0CR
201400010320     C     *LIKE         DEFINE    CPT1CN        WE1CR
201500010320     C     *LIKE         DEFINE    CPTTIS        WEISR
201600010320     C     *LIKE         DEFINE    CPTTCS        WECSR
201700010320     C     *LIKE         DEFINE    CPTTLD        WELDR
201800010320     C*
201900010320     C* RICHIESTA ELENCO CODICI
202000010320     C                   SETOFF                                           28
202100010320     C     '?'           SCAN      WTTR                                   28
202200010320IF  1C     *IN28         IFEQ      *ON
202300010320     C                   MOVEL     *BLANK        DNAR
202400010320     C                   MOVEL     *BLANKS       DCAP
202500010320     C                   MOVEL     'T01'         WOP0
202600010320     C                   MOVEL     *BLANKS       WOP1
202700010323     C                   MOVEL     'TISI41R'     WPGM              8
202800010320     C                   EXSR      LANPGM
202900010320     C                   MOVEL     D00TTC        WTTR
203000010320E   1C                   ENDIF
203100010320     C*
203200010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO RITIRO
203300010322     C                   MOVEL     VIDTTR        DEP3A             3
203400010322     C     DEP3A         IFEQ      '999'
203500010320     C* CONTROLLA ESISTENZA CODICI
203600010320     C                   Z-ADD     D00VER        KCTVER
203700010320     C                   MOVEL     WTTR          KCTTTC
203800010320     C                   MOVEL     'D'           KCTTSP
203900010320     C     KEYCPT        CHAIN     AZCPT01L                           99
204000010320IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
204100010320     C                   Z-ADD     CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
204200010320     C                   Z-ADD     CPT1IN        WD1IR
204300010320     C                   Z-ADD     CPT0CN        WD0CR
204400010320     C                   Z-ADD     CPT1CN        WD1CR
204500010320     C                   Z-ADD     CPTTIS        WDISR
204600010320     C                   Z-ADD     CPTTCS        WDCSR
204700010320     C                   Z-ADD     CPTTLD        WDLDR
204800010320     C                   MOVEL     'C'           KCTTSP
204900010320     C     KEYCPT        CHAIN     AZCPT01L                           99
205000010320IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
205100010320     C                   Z-ADD     CPT0IN        WC0IR                          *TEMPI CORRIERE
205200010320     C                   Z-ADD     CPT1IN        WC1IR
205300010320     C                   Z-ADD     CPT0CN        WC0CR
205400010320     C                   Z-ADD     CPT1CN        WC1CR
205500010320     C                   Z-ADD     CPTTIS        WCISR
205600010320     C                   Z-ADD     CPTTCS        WCCSR
205700010320     C                   Z-ADD     CPTTLD        WCLDR
205800010320     C                   MOVEL     'E'           KCTTSP
205900010320     C     KEYCPT        CHAIN     AZCPT01L                           99
206000010320IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
206100010320     C                   Z-ADD     CPT0IN        WE0IR                          *TEMPI ESPRESSO
206200010320     C                   Z-ADD     CPT1IN        WE1IR
206300010320     C                   Z-ADD     CPT0CN        WE0CR
206400010320     C                   Z-ADD     CPT1CN        WE1CR
206500010320     C                   Z-ADD     CPTTIS        WEISR
206600010320     C                   Z-ADD     CPTTCS        WECSR
206700010320     C                   Z-ADD     CPTTLD        WELDR
206800010320X   3C                   ELSE
206900010320     C                   MOVEL     '1'           ERRL00
207000010320E   3C                   ENDIF
207100010320X   2C                   ELSE
207200010320     C                   MOVEL     '1'           ERRL00
207300010320E   2C                   ENDIF
207400010320X   1C                   ELSE
207500010320     C                   MOVEL     '1'           ERRL00
207600010320E   1C                   ENDIF
207700010322     C                   ELSE
207800010322     C                   MOVEL     '1'           ERRL00
207900010322     C                   ENDIF
208000010320     C*
208100010320     C                   ENDSR
208200970526     C*------------------------------------------------------------------------*
208300970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
208400970526     C*------------------------------------------------------------------------*
208500970526     C     IREVID        BEGSR
208600970526     C* CAMPI
208700970526IF  1C     CPCESL        IFEQ      'S'
208800970526     C                   MOVEL     'LOC'         VDDESL
208900970526E   1C                   ENDIF
209000970526IF  1C     CPCESE        IFEQ      'S'
209100970526     C                   MOVEL     'ECC'         VDDESE
209200970526E   1C                   ENDIF
209300970526     C                   MOVEL     CPCNAR        VIDNAR
209400970526     C                   MOVEL     CPCCAP        VIDCAP
209500970526     C                   MOVEL     CPCLNA        VIDLNA
209600970526     C                   MOVEL     CPCLOL        VIDLOL
209700971118     C                   MOVEL     CPCLO2        VIDLO2
209800970604     C                   MOVEL     CPCLOS        VIDLOS
209900970526     C                   MOVEL     CPCLNA        VIDLFD
210000970526     C                   Z-ADD     CPCZNC        VIDZNC
210100970526     C                   Z-ADD     CPCZOL        VIDZOL
210200971118     C                   Z-ADD     CPCZO2        VIDZO2
210300970604     C                   Z-ADD     CPCZOS        VIDZOS
210400970526     C                   Z-ADD     CPCZFD        VIDZFD
210500970526     C                   Z-ADD     CPCLKG        VIDLKG
210600971118     C                   Z-ADD     CPCLK2        VIDLK2
210700970526     C                   Z-ADD     CPCLMC        VIDLMC
210800971118     C                   Z-ADD     CPCLM2        VIDLM2
210900970604     C                   Z-ADD     CPCLKS        VIDLKS
211000970604     C                   Z-ADD     CPCLMS        VIDLMS
211100970526     C                   MOVEL     CPCPRV        VIDPRV
211200970526     C                   MOVEL     CPCPRA        VIDPRA
211300970526     C                   MOVEL     CPCCTS        VIDCTS
211400970526     C                   MOVEL     CPCCTE        VIDCTE
211500970526     C                   MOVEL     CPCFIT        VIDFIT
211600970526     C                   MOVEL     CPCFLG        VIDFLG
211700970526     C                   MOVEL     CPCISO        VIDISO
211800970526     C                   MOVEL     CPCPII        VIDPII
211900970526     C                   MOVEL     CPCPID        VIDPID
212000970526     C                   MOVEL     CPCPIS        VIDPIS
212100970526     C                   MOVEL     CPCGDD        VIDGDD
212200970526     C                   MOVEL     CPCTTC        VIDTTC
212300970917     C                   MOVEL     CPCFI1        VIDFI1
212400010320     C                   MOVEL     *all'9'       VIDTTR
212500010320     C                   MOVE      CPCFI2        VIDTTR
212600000420     C*
212700060927     c*** 27/09/06 - EDPES: campi poste non più usati
212800970526     C*
212900970526     C                   ENDSR
213000970526     C*------------------------------------------------------------------------*
213100970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
213200970526     C*------------------------------------------------------------------------*
213300970526     C     AZZVID        BEGSR
213400970526     C* CAMPI
213500970526     C                   MOVEL     *BLANKS       VDDESL
213600970526     C                   MOVEL     *BLANKS       VDDESE
213700970526     C                   MOVEL     *BLANKS       VIDNAR
213800970526     C                   MOVEL     *BLANKS       VIDCAP
213900970526     C                   MOVEL     *ZEROS        VIDLNA
214000970526     C                   MOVEL     *ZEROS        VIDLOL
214100971118     C                   MOVEL     *ZEROS        VIDLO2
214200970604     C                   MOVEL     *ZEROS        VIDLOS
214300970526     C                   MOVEL     *ZEROS        VIDLFD
214400970526     C                   Z-ADD     *ZEROS        VIDZNC
214500970526     C                   Z-ADD     *ZEROS        VIDZOL
214600971118     C                   Z-ADD     *ZEROS        VIDZO2
214700970604     C                   Z-ADD     *ZEROS        VIDZOS
214800970526     C                   Z-ADD     *ZEROS        VIDZFD
214900970526     C                   Z-ADD     *ZEROS        VIDLKG
215000971118     C                   Z-ADD     *ZEROS        VIDLK2
215100970526     C                   Z-ADD     *ZEROS        VIDLMC
215200971118     C                   Z-ADD     *ZEROS        VIDLM2
215300970604     C                   Z-ADD     *ZEROS        VIDLKS
215400970604     C                   Z-ADD     *ZEROS        VIDLMS
215500970526     C                   MOVEL     *BLANKS       VIDPRV
215600970526     C                   MOVEL     *BLANKS       VIDPRA
215700970526     C                   MOVEL     *BLANKS       VIDCTS
215800970526     C                   MOVEL     *BLANKS       VIDCTE
215900970526     C                   MOVEL     'N'           VIDFIT
216000970526     C                   MOVEL     'N'           VIDFLG
216100970526     C                   MOVEL     *BLANKS       VIDISO
216200970526     C                   MOVEL     'N'           VIDPII
216300970526     C                   MOVEL     'N'           VIDPID
216400970526     C                   MOVEL     'N'           VIDPIS
216500970526     C                   MOVEL     'N'           VIDGDD
216600970526     C                   MOVEL     *BLANKS       VIDTTC
216700970917     C                   MOVEL     *BLANKS       VIDFI1
216800970526     C* DECODIFICHE
216900970526     C                   MOVEL     *BLANKS       VDDNAR
217000970526     C                   MOVEL     *BLANKS       VDDLNA
217100970526     C                   MOVEL     *BLANKS       VDDLOL
217200971118     C                   MOVEL     *BLANKS       VDDLO2
217300970604     C                   MOVEL     *BLANKS       VDDLOS
217400970526     C                   MOVEL     *BLANKS       VDDLFD
217500970526     C                   MOVEL     *BLANKS       VDDPRV
217600970526     C                   MOVEL     *BLANKS       VDDPRA
217700970526     C                   MOVEL     *BLANKS       VDDCTS
217800970526     C                   MOVEL     *BLANKS       VDDCTE
217900970526     C                   MOVEL     *BLANKS       VDDISO
218000000426     C                   MOVEL     *BLANKS       VDPLNA
218100130902     c
218200130913     c*****              clear                   caplna
218300130913     c*****              clear                   loclna
218400130903     c                   clear                   caplos
218500130903     c                   clear                   loclos
218600130903     c                   clear                   caplol
218700130903     c                   clear                   loclol
218800130903     c                   clear                   F07los            1
218900130903     c                   clear                   F07lna            1
219000130903     c                   clear                   F07lol            1
219100130913     c                   clear                   s_cpclna
219200130913     c                   clear                   s_cpclos
219300130913     c                   clear                   s_cpclol
219400970526     C*
219500970526     C                   ENDSR
219600970526     C*------------------------------------------------------------------------*
219700970526     C* IMPCPC - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
219800970526     C*------------------------------------------------------------------------*
219900970526     C     IMPCPC        BEGSR
220000970526     C*
220100970526     C                   MOVEL     VIDLNA        CPCLNA
220200970526     C                   MOVEL     VIDLOL        CPCLOL
220300971118     C                   MOVEL     VIDLO2        CPCLO2
220400970604     C                   MOVEL     VIDLOS        CPCLOS
220500970526     C                   Z-ADD     VIDZNC        CPCZNC
220600970526     C                   Z-ADD     VIDZOL        CPCZOL
220700971118     C                   Z-ADD     VIDZO2        CPCZO2
220800970604     C                   Z-ADD     VIDZOS        CPCZOS
220900970526     C                   Z-ADD     VIDZFD        CPCZFD
221000970526     C                   Z-ADD     VIDLKG        CPCLKG
221100971118     C                   Z-ADD     VIDLK2        CPCLK2
221200970526     C                   Z-ADD     VIDLMC        CPCLMC
221300971118     C                   Z-ADD     VIDLM2        CPCLM2
221400970604     C                   Z-ADD     VIDLKS        CPCLKS
221500970604     C                   Z-ADD     VIDLMS        CPCLMS
221600970526     C                   MOVEL     VIDPRV        CPCPRV
221700970526     C                   MOVEL     VIDPRA        CPCPRA
221800970526     C                   MOVEL     VIDCTS        CPCCTS
221900970526     C                   MOVEL     VIDCTE        CPCCTE
222000970526     C                   MOVEL     VIDFIT        CPCFIT
222100970526     C                   MOVEL     VIDFLG        CPCFLG
222200970526     C                   MOVEL     VIDISO        CPCISO
222300970526     C                   MOVEL     VIDPII        CPCPII
222400970603IF  3C     CPCISO        IFEQ      'I'
222500970603     C                   MOVEL     'S'           CPCPII
222600970603E   3C                   ENDIF
222700970603     C                   MOVEL     VIDPID        CPCPID
222800970603IF  3C     CPCISO        IFEQ      'D'
222900970603     C                   MOVEL     'S'           CPCPID
223000970603E   3C                   ENDIF
223100970603     C                   MOVEL     VIDPIS        CPCPIS
223200970603IF  3C     CPCISO        IFEQ      'S'
223300970603     C                   MOVEL     'S'           CPCPIS
223400970603E   3C                   ENDIF
223500970526     C                   MOVEL     VIDGDD        CPCGDD
223600970526     C                   MOVEL     VIDTTC        CPCTTC
223700970917     C                   MOVEL     VIDFI1        CPCFI1
223800010320     C                   MOVE(p)   VIDTTR        CPCFI2
223900970526     C*
224000970526     C                   ENDSR
224100970530     C*------------------------------------------------------------------------*
224200970530     C* LANPGM - LANCIA UN PROGRAMMA
224300970530     C*------------------------------------------------------------------------*
224400970530     C     LANPGM        BEGSR
224500970530     C*
224600970530     C* IMPOSTA LA DS DI PROCEDURA
224700970530     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
224800970530     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
224900970530     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
225000970530     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
225100970530     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
225200970530     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
225300970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
225400970602     C                   MOVEL     DCAP          D00CAP                         *CAP
225500970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
225600970530     C                   MOVEL     *BLANKS       D00TSP
225700970530     C                   Z-ADD     *ZEROS        D00TFP
225800970530     C                   MOVEL     *BLANKS       D00LOC
225900970603     C                   MOVEL     *BLANKS       D00SIN
226000970530     C                   MOVEL     *BLANKS       D00EPA
226100970530     C                   Z-ADD     *ZEROS        D00ETP
226200970530     C                   Z-ADD     *ZEROS        D00ETA
226300970530     C                   Z-ADD     *ZEROS        D00EDD
226400970530     C                   MOVEL     *BLANKS       D00SPT
226500970530     C                   Z-ADD     *ZEROS        D00SLP
226600970530     C                   Z-ADD     *ZEROS        D00SLA
226700970530     C                   MOVEL     *BLANKS       D00STS
226800970530     C                   Z-ADD     *ZEROS        D00SDD
226900970530     C*
227000970530     C                   CALL      WPGM
227100970612     C                   PARM                    KPJBA
227200970612     C                   PARM                    DSSI00
227300970530     C*
227400970530     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
227500970530IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
227600970530     C                   MOVEL     '1'           ERRL00                         *ERRORE
227700970530     C                   MOVEL     '1'           F03L00                         *FINE PGM
227800970530X   1C                   ELSE                                                   *NON PREMUTO F03
227900970530     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
228000970530E   1C                   ENDIF
228100970530     C*
228200970530     C                   ENDSR
228300970526     C*------------------------------------------------------------------------*
228400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
228500970526     C*------------------------------------------------------------------------*
228600970526     C     IMPFUN        BEGSR
228700970603     C*
228800970603     C* SE INTERROGAZIONE/VISUALIZZAZIONE TOGLE LA GESTIONE ALLE ZONE DI CONSEGNA
228900970603IF  1C     DEPOP0        IFEQ      'C05'
229000970603     C     DEPOP1        OREQ      'O05'
229100970603     C                   MOVEL     FUD(14)       FUDRIG                         *AGGIORNAMENTO
229200970603     C                   MOVEL     'N'           FUDATT
229300970603     C                   MOVEL     FUDRIG        FUD(14)
229400970603E   1C                   ENDIF
229500970526     C*
229600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
229700970526IF  1C     DEPOP1        IFEQ      'O05'
229800150423     c     §utecap       oreq      'I'
229900150423     c     §utecap       oreq      'O'
230000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
230100970526     C                   MOVEL     'N'           FUDATT
230200970526     C                   MOVEL     FUDRIG        FUD(6)
230300970526E   1C                   ENDIF
230400970526     C*
230500970526     C* SE INSERIMENTO TOGLIE LE FUNZIONI DI ACCESSO AGLI ALTRI ARCHIVI
230600970526IF  1C     DEPOP1        IFEQ      'F06'
230700970526     C                   MOVEL     FUD(8)        FUDRIG                         *LOCALITA'
230800970526     C                   MOVEL     'N'           FUDATT
230900970526     C                   MOVEL     FUDRIG        FUD(8)
231000970527     C                   MOVEL     FUD(9)        FUDRIG                         *ECCEZIONI
231100970526     C                   MOVEL     'N'           FUDATT
231200970526     C                   MOVEL     FUDRIG        FUD(9)
231300970527     C                   MOVEL     FUD(10)       FUDRIG                         *TEMPI
231400970527     C                   MOVEL     'N'           FUDATT
231500970527     C                   MOVEL     FUDRIG        FUD(10)
231600970526E   1C                   ENDIF
231700131007     c
231800131007     c* Se utente non abilitato, non attivo F7 e F17
231900131007     c* lascio in manutenzione
232000131007     c                   if        *in10 and  depop0 = 'C05'
232100131007     C                   MOVEL     FUD(7)        FUDRIG                         *LOCALITA'
232200131007     C                   MOVEL     'N'           FUDATT
232300131007     C                   MOVEL     FUDRIG        FUD(7)
232400131007     C                   MOVEL     FUD(17)       FUDRIG                         *LOCALITA'
232500131007     C                   MOVEL     'N'           FUDATT
232600131007     C                   MOVEL     FUDRIG        FUD(17)
232700131007     c                   endif
232800970526     C*
232900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
233000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
233100970526     C                   EXSR      FUNGES
233200000925     C*
233300000925     C* VERIFICO CHE SOLO UTENTI "EDP*" POSSANO MODIFICARE IL CAMPO "UFFICIO POSTALE"
233400000925     C                   MOVEL     KNMUS         UTE               3
233500000925     C     UTE           IFNE      'EDP'
233600000925     C                   SETON                                        95
233700000925     C                   ENDIF
233800000925     C*
233900970526     C*
234000970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
234100970526     C*---
234200970526     C* 90 -> PROTEZIONE CHIAVE
234300970526     C* 91 -> PROTEZIONE TOTALE
234400050414     C* 93 -> PROTEZIONE COD.TASSAZIONE e TIPO INOLTRO e altri
234500970526     C*---
234600050413     C*
234700050413     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
234800050413     C                   SETON                                        93
234900050413     C*
235000050413     C* VERIFICO X PRIMISSIMA COSA LE AUTORIZZAZIONI SPECIALI SULL'UTENTE CORRENTE
235100140206     C                   IF        §UTECAP = *blanks  or §utecap='M'
235200050413     C* TUTTO RIMANE INVARIATO
235300050413     C                   ENDIF
235400050413     C*
235500140206     C                   IF        §UTECAP = 'I'  or §utecap='O'
235600140206     C* FORZO SOLO INTERROGAZIONE ma permetto di modificare gli orari se c'e' "O"
235700050413     C                   EVAL      DEPOP1 = 'O05'
235800050413     C                   ENDIF
235900050413     C*
236000050413     C                   IF        §UTECAP = 'C'
236100050413     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
236200050413     C                   SETOFF                                       93
236300050413     C                   ENDIF
236400050413     C*
236500970526IF  1C     DEPOP0        IFEQ      'C02'                                        G E S T I O N E
236600970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
236700970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
236800970526     C                   SETOFF                                       91
236900970526E   2C                   ENDIF
237000970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
237100970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
237200970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
237300970526E   2C                   ENDIF
237400970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
237500970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
237600970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
237700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
237800970526     C                   SETON                                        91
237900970526E   2C                   ENDIF
238000970526X   1C                   ELSE                                                   *I N T E R R O G A
238100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
238200970526     C                   SETON                                        91
238300970526E   1C                   ENDIF
238400970526     C*
238500970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
238600970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
238700970526     C                   Z-ADD     D00VER        KCCVER
238800970526     C                   MOVEL     D00NAR        KCCNAR
238900970526     C                   MOVEL     D00CAP        KCCCAP
239000970526     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
239100970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
239200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
239300970526     C                   MOVEL     MSG(6)        DSMSMS
239400970526X   2C                   ELSE
239500970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
239600980128IF  3C     DEPOP1        IFEQ      'O05'
239700980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
239800980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
239900980128E   3C                   ENDIF
240000980128E   2C                   ENDIF
240100970526X   1C                   ELSE                                                   *INSERIMENTO
240200970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
240300970526E   1C                   ENDIF
240400970526     C*
240500970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
240600970526IF  1C     DEPOP1        IFEQ      'O02'
240700970526     C                   MOVEL     DEC(1)        VIDFUN
240800970526E   1C                   ENDIF
240900970526IF  1C     DEPOP1        IFEQ      'O04'
241000970526     C                   MOVEL     DEC(2)        VIDFUN
241100970526E   1C                   ENDIF
241200970526IF  1C     DEPOP1        IFEQ      'O05'
241300970526     C                   MOVEL     DEC(3)        VIDFUN
241400970526E   1C                   ENDIF
241500970526IF  1C     DEPOP1        IFEQ      'O07'
241600970526     C                   MOVEL     DEC(4)        VIDFUN
241700970526E   1C                   ENDIF
241800970526IF  1C     DEPOP1        IFEQ      'F06'
241900970526     C                   MOVEL     DEC(5)        VIDFUN
242000970526E   1C                   ENDIF
242100970526     C*
242200970526     C                   ENDSR
242300130913     C*------------------------------------------------------------------------*
242400130913     C* Richiamo pgm annullamento orari servizi a fronte di modifica lna
242500130913     C*------------------------------------------------------------------------*
242600130913     c     sr_oraris     begsr
242700130913
242800130913     C* LINEA STANDARD
242900130913     c                   movel     vidlna        w0030             3 0
243000130913     c* Se cambiata la linea
243100130913     c                   if        w0030<>cpclna
243200130913     c                   clear                   tisio7ds
243300130913     c                   eval      io7mod='D'
243400130913     c                   eval      io7liv='C'
243500130913     c                   eval      io7ver=d00ver
243600130913     c                   eval      io7nar=vidnar
243700130913     c                   eval      io7cap=vidcap
243800130913     c                   eval      io7lna=cpclna
243900130913     c                   eval      io7lns=%dec(vidlna:3:0)
244000130913     c                   eval      io7los=%dec(vidlos:3:0)
244100130913     c                   eval      io7lol=%dec(vidlol:3:0)
244200131016     c******             movel     tisio7ds      kpjbu
244300130913     c                   call      'TISIO7R'
244400130913     c                   parm                    kpjba
244500131016     c                   parm                    tisio7ds
244600130913    3c                   endif
244700130913     C* LINEA SOTTO
244800130913     c                   movel     vidlos        w0030             3 0
244900130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
245000130913     c                   if        w0030<>cpclos and cpclos<>cpclna
245100130913     c                   clear                   tisio7ds
245200130913     c                   eval      io7mod='D'
245300130913     c                   eval      io7liv='C'
245400130913     c                   eval      io7ver=d00ver
245500130913     c                   eval      io7nar=vidnar
245600130913     c                   eval      io7cap=vidcap
245700130913     c                   eval      io7lna=cpclos
245800130913     c                   eval      io7lns=%dec(vidlna:3:0)
245900130913     c                   eval      io7los=%dec(vidlos:3:0)
246000130913     c                   eval      io7lol=%dec(vidlol:3:0)
246100131016     c******             movel     tisio7ds      kpjbu
246200130913     c                   call      'TISIO7R'
246300130913     c                   parm                    kpjba
246400131016     c                   parm                    tisio7ds
246500130913    3c                   endif
246600130913     C* LINEA OLTRE
246700130913     c                   movel     vidlol        w0030             3 0
246800130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
246900130913     c                   if        w0030<>cpclol and cpclol<>cpclna
247000130913     c                                           and cpclol<>cpclos
247100130913     c                   clear                   tisio7ds
247200130913     c                   eval      io7mod='D'
247300130913     c                   eval      io7liv='C'
247400130913     c                   eval      io7ver=d00ver
247500130913     c                   eval      io7nar=vidnar
247600130913     c                   eval      io7cap=vidcap
247700130913     c                   eval      io7lna=cpclol
247800130913     c                   eval      io7lns=%dec(vidlna:3:0)
247900130913     c                   eval      io7los=%dec(vidlos:3:0)
248000130913     c                   eval      io7lol=%dec(vidlol:3:0)
248100131016     c*****              movel     tisio7ds      kpjbu
248200130913     c                   call      'TISIO7R'
248300130913     c                   parm                    kpjba
248400131016     c                   parm                    tisio7ds
248500130913    3c                   endif
248600130913     c                   endsr
248700970703     C*------------------------------------------------------------------------*
248800970703     C* FINSR - OPERAZIONI FINALI
248900970703     C*------------------------------------------------------------------------*
249000970703     C     FINSR         BEGSR
249100970703     C*
249200970704     C                   CLEAR                   DSSI95
249300970703     C                   MOVEL     'C'           I95TLA
249400970703     C                   CALL      'TISI95R'
249500970703     C                   PARM                    DSSI95
249600130913     c*
249700130913     c                   clear                   tisio7ds
249800130913     C                   MOVEL     'C'           Io7TLA
249900131016     c******             movel     tisio7ds      kpjbu
250000130913     C                   CALL      'TISIO7R'
250100130913     C                   PARM                    kpjba
250200131016     c                   parm                    tisio7ds
250300970703     C*
250400970703     C                   ENDSR
250500050413     C*--------------------------------------------------------------------------------------------*
250600050413     C* REPERISCE DATI UTENTE
250700050413     C*--------------------------------------------------------------------------------------------*
250800050413     C     REPDATIUTE    BEGSR
250900050413     C*
251000050413     C* REimposta le variabili di lavoro
251100050413     C                   CLEAR                   TIBS34DS
251200050413     C                   CLEAR                   AZUTEDS
251300050413     C                   CLEAR                   DDATIUTE
251400050413     C*
251500050413     C* Reperisce i dati del profilo in ingresso
251600050413     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
251700050413     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
251800050413     C                   IN(E)     *DTAARA
251900050413     C                   IF        %Error                                       *NO errori
252000050413     C                   EVAL      I34TLA = 'L'
252100050413     C                   CALL      'TIBS34R'
252200050413     C                   PARM                    TIBS34DS
252300050413     C                   IN        *DTAARA
252400050413     C                   ENDIF
252500050413     C*
252600050413     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
252700050413     C                   EVAL      DUTE01 = UTEFAF
252800050413     C*
252900050413     C                   ENDSR
253000970526     C*------------------------------------------------------------------------*
253100970526     C* *INZSR - OPERAZIONI INIZIALI
253200970526     C*------------------------------------------------------------------------*
253300970526     C     *INZSR        BEGSR
253400970526     C*---
253500970526     C* RICEVIMENTO PARAMETRI
253600970526     C*---
253700970526     C     *ENTRY        PLIST
253800970612     C                   PARM                    KPJBA
253900970612     C                   PARM                    DSSI00
254000050413     C*
254100050413     C* REPERISCE I DATI DELL'UTENTE
254200050413     C                   EXSR      REPDATIUTE
254300060206     c                   movel     RSUT          DSfirs
254400970526     C*
254500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
254600970526     C     XMSGPA        PLIST
254700970526     C                   PARM                    DSMSMS
254800970526     C                   PARM                    DSMSPG
254900970526     C                   PARM                    DSMSKE
255000970526     C                   PARM                    DSMSER
255100131007      /free
255200131007        // Carico filiali di test abilitate
255300131007         if dutpou<>46  ;
255400131007
255500131007         clear TIBS02ds;
255600131007         T02Mod = 'C';
255700131007         T02sif = knsif;
255800131007         T02cod = 'VPO';
255900131007         t02tla = 'L'   ;
256000131007         T02ke1 ='DECOFI816' ;
256100131007         TNTBE_RicercaControllo  (kpjba : tibs02ds);
256200131007
256300131007         if  T02err  = *blank;
256400131007         xx= 1 ;
256500131007         yy= 1 ;
256600131007
256700131007         dow xx< 256   ;
256800131007           fil_ab(yy)= %subst(t02uni:xx:3)  ;
256900131007           yy=yy+ 1   ;
257000131007           xx=xx+ 3   ;
257100131007         enddo  ;
257200131007
257300131007         endif;
257400131007
257500131007         // se c'e' 999 tutte abilitate
257600131007         w003a='999'  ;
257700131007         if   %lookup(w003a:fil_ab) = 0  ;
257800131007         w003a=%editc(dutpou:'X')   ;
257900131007         if   %lookup(w003a:fil_ab) = 0  ;
258000131007         *in10 =*on  ;
258100131007         endif  ;
258200131007         endif  ;
258300131007
258400131007         endif  ;
258500131007
258600131007      /end-free
258700131007
258800090903     c*
258900090903     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
259000090903     c                   clear                   tibs02ds
259100090903     c                   clear                   dvpodeco
259200090903     c                   eval      t02mod = 'C'
259300090903     c                   eval      t02sif = knsif
259400090903     c                   eval      t02cod = 'VPO'
259500090903     c                   eval      t02ke1 = 'DECO'
259600090903     c                   call      'TIBS02R'
259700090903     c                   parm                    kpjba
259800090903     c                   parm                    tibs02ds
259900090903     c                   if        t02err = *blanks
260000090903     c                   eval      dvpodeco = t02uni
260100090903     c                   else
260200090903     c                   eval      §vpotsph=20091001
260300090903     c                   endif
260400090903
260500090903     c     *iso          movel     §vpotsph      dataiso
260600090903     c
260700090903     c* Calcolo data oddierna
260800090903     C*
260900000426     C*
261000000426     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
261100000426     C     D00NAR        IFNE      *BLANKS
261200000426     C                   SETON                                        92
261300000426     C                   ELSE
261400000426     C                   SETOFF                                       92
261500000426     C                   ENDIF
261600970526     C*
261700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
261800970526     C                   CALL      'XKEY'
261900970526     C                   PARM                    DSKEY
262000970526     C*---
262100970526     C* VARIABILI NON RIFERITE AL DATA BASE
262200970526     C*---
262300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
262400970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
262500970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
262600970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
262700970526     C     *LIKE         DEFINE    D00OP1        WOP1
262800970602     C     *LIKE         DEFINE    D00NAR        DNAR
262900970602     C     *LIKE         DEFINE    D00CAP        DCAP
263000970526     C*---
263100970526     C* VARIABILI DI CONTROLLO
263200970526     C*---
263300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
263400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
263500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
263600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
263700970917     C                   MOVEL     'S'           $LOBS             1            *LOCALITA' OBSOLETE
263800970526     C*
263900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
264000970526     C                   MOVEL     'TISI03R'     DSMSPG
264100970526     C                   MOVEL     'TISI03R'     MSGQUE
264200970526     C                   MOVEL     *BLANKS       MSGKEY
264300970526     C                   MOVEL     *BLANKS       DSMSMS
264400970526     C*---
264500970526     C* VARIABILI RIFERITE AL DATA BASE
264600970526     C*---
264700970526     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
264800970526     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
264900970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
265000970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
265100970526     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
265200970526     C     *LIKE         DEFINE    CPCNAR        KCCNAR
265300970526     C     *LIKE         DEFINE    CPCCAP        KCCCAP
265400970526     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
265500970526     C     *LIKE         DEFINE    CPENAR        KCENAR
265600970526     C     *LIKE         DEFINE    CPECAP        KCECAP
265700970526     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
265800970526     C     *LIKE         DEFINE    CPLNAR        KCLNAR
265900970526     C     *LIKE         DEFINE    CPLCAP        KCLCAP
266000130903     C     *LIKE         DEFINE    CPLloc        KCLloc
266100970526     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
266200970526     C     *LIKE         DEFINE    CPSNAR        KCSNAR
266300970526     C     *LIKE         DEFINE    CPSCAP        KCSCAP
266400970526     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
266500970526     C     *LIKE         DEFINE    CPTTTC        KCTTTC
266600970526     C     *LIKE         DEFINE    CPTTSP        KCTTSP
266700970526     C*---
266800970526     C* CHIAVI DI LETTURA
266900970526     C*---
267000970526     C* LETTURA AZORG01L - COMPLETA
267100970526     C     KEYORG        KLIST
267200970526     C                   KFLD                    KORFIL                         *FILIALE
267300970526     C* LETTURA TABEL00F - COMPLETA
267400970526     C     KEYTAB        KLIST
267500970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
267600970526     C                   KFLD                    KTBCOD                         *TABELLA
267700970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
267800970526     C* LETTURA TABEL00F - PARZIALE
267900970526     C     KE2TAB        KLIST
268000970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
268100970526     C                   KFLD                    KTBCOD                         *TABELLA
268200970526     C* LETTURA AZCPC01L - COMPLETA
268300970526     C     KEYCPC        KLIST
268400970526     C                   KFLD                    KCCVER                         *VERSIONE
268500970526     C                   KFLD                    KCCNAR                         *NAZIONE
268600970526     C                   KFLD                    KCCCAP                         *CAP
268700970527     C* LETTURA AZCPT01L - COMPLETA
268800970526     C     KEYCPT        KLIST
268900970526     C                   KFLD                    KCTVER                         *VERSIONE
269000970526     C                   KFLD                    KCTTTC                         *TEMPO CONSEGNA
269100970526     C                   KFLD                    KCTTSP                         *TIPO SERVIZIO
269200970527     C* LETTURA AZCPE01L - PARZIALE
269300970527     C     KEYCPE        KLIST
269400970527     C                   KFLD                    KCEVER                         *VERSIONE
269500970527     C                   KFLD                    KCENAR                         *NAZIONE
269600970527     C                   KFLD                    KCECAP                         *CAP
269700970526     C* LETTURA AZCPL01L - PARZIALE
269800970526     C     KEYCPL        KLIST
269900970526     C                   KFLD                    KCLVER                         *VERSIONE
270000970526     C                   KFLD                    KCLNAR                         *NAZIONE
270100970526     C                   KFLD                    KCLCAP                         *CAP
270200130903     C* LETTURA AZCPL01L - PARZIALE
270300130903     C     KEYCPL2       KLIST
270400130903     C                   KFLD                    KCLVER                         *VERSIONE
270500130903     C                   KFLD                    KCLNAR                         *NAZIONE
270600130903     C                   KFLD                    KCLCAP                         *CAP
270700130903     C                   KFLD                    KCLloc                         *CAP
270800970526     C* LETTURA AZCPS01L - PARZIALE
270900970526     C     KEYCPS        KLIST
271000970526     C                   KFLD                    KCSVER                         *VERSIONE
271100970526     C                   KFLD                    KCSNAR                         *NAZIONE
271200970526     C                   KFLD                    KCSCAP                         *CAP
271300130902     C*
271400130902     C     Kcos          KLIST
271500130903     C                   KFLD                    cosVER
271600130903     C                   KFLD                    cosnar
271700130903     C                   KFLD                    coscap
271800130903     C                   KFLD                    cosloc
271900130903     C                   KFLD                    coslna
272000130903     C     Kcos2         KLIST
272100130903     C                   KFLD                    cosVER
272200130903     C                   KFLD                    coslna
272300130903     C                   KFLD                    cosnar
272400130903     C                   KFLD                    coscap
272500130903     C                   KFLD                    cosloc
272600130903     C     Kcos22        KLIST
272700130903     C                   KFLD                    cosVER
272800130903     C                   KFLD                    coslna
272900130903     C                   KFLD                    cosnar
273000130903     C                   KFLD                    coscap
273100970526     C*
273200970526     C* DEPOSITA LE OPZIONI DI ENTRATA
273300970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
273400970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
273500970526     C*
273600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
273700970526     C                   EXSR      IMPFUN
273800970526     C*
273900970526     C                   ENDSR
274000970526     C*------------------------------------------------------------------------*
274100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
274200970526     C*------------------------------------------------------------------------*
274300970526     C     FUNGES        BEGSR
274400970526     C*
274500970526     C* PULISCO CAMPI DI LAVORO
274600970526     C                   Z-ADD     0             W                 5 0
274700970526     C*
274800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
274900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
275000970526     C     1             DO        24            I
275100970526     C                   MOVEL     FUD(I)        FUDRIG
275200970526     C*
275300970526     C* PULIZIA FUNZIONI PRECEDENTI
275400970526     C                   MOVEL     *BLANKS       FUV(I)
275500970526     C*
275600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
275700970526     C* (SALTA F24)
275800970526     C     FUDATT        IFEQ      'S'
275900970526     C     FUDFUN        ANDNE     '24'
276000970526     C                   ADD       1             W
276100970526     C                   MOVEL     FUDDES        FUV(W)
276200970526     C                   END
276300970526     C                   END                                                    DO WHILE
276400970526     C*
276500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
276600970526     C* ALTRIMENTI DISATTIVO F24
276700970526     C     1             DO        24            I
276800970526     C                   MOVEL     FUD(I)        FUDRIG
276900970526     C*
277000970526     C     FUDFUN        IFEQ      '24'
277100970526     C     W             IFGT      6
277200970526     C                   MOVEL     'S'           FUDATT
277300970526     C                   ELSE
277400970526     C                   MOVEL     *BLANKS       FUDATT
277500970526     C                   END                                                    W GT 8
277600970526     C                   MOVEL     FUDRIG        FUD(I)
277700970526     C                   END
277800970526     C                   END                                                    DO WHILE
277900970526     C*
278000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
278100970526     C     W             DIV       6             I
278200970526     C                   MVR                     Z
278300970526     C*
278400970526     C     Z             IFNE      0
278500970526     C     I             ADD       1             Z
278600970526     C                   ELSE
278700970526     C                   Z-ADD     I             Z                 5 0
278800970526     C                   END
278900970526     C*
279000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
279100970526     C                   ADD       1             FUNGRU
279200970526     C     FUNGRU        IFGT      Z
279300970526     C     FUNGRU        ORLT      1
279400970526     C                   Z-ADD     1             FUNGRU
279500970526     C                   END
279600970526     C*
279700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
279800970526     C     FUNGRU        IFEQ      1
279900970526     C                   MOVEL     FUV(1)        FUN001
280000970526     C                   MOVEL     FUV(2)        FUN002
280100970526     C                   MOVEL     FUV(3)        FUN003
280200970526     C                   MOVEL     FUV(4)        FUN004
280300970526     C                   MOVEL     FUV(5)        FUN005
280400970526     C                   MOVEL     FUV(6)        FUN006
280500970526     C                   END
280600970526     C*
280700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
280800970526     C     FUNGRU        IFEQ      2
280900970526     C                   MOVEL     FUV(7)        FUN001
281000970526     C                   MOVEL     FUV(8)        FUN002
281100970526     C                   MOVEL     FUV(9)        FUN003
281200970526     C                   MOVEL     FUV(10)       FUN004
281300970526     C                   MOVEL     FUV(11)       FUN005
281400970526     C                   MOVEL     FUV(12)       FUN006
281500970526     C                   END
281600970526     C*
281700970526     C* TERZO GRUPPO DI 6 FUNZIONI
281800970526     C     FUNGRU        IFEQ      3
281900970526     C                   MOVEL     FUV(13)       FUN001
282000970526     C                   MOVEL     FUV(14)       FUN002
282100970526     C                   MOVEL     FUV(15)       FUN003
282200970526     C                   MOVEL     FUV(16)       FUN004
282300970526     C                   MOVEL     FUV(17)       FUN005
282400970526     C                   MOVEL     FUV(18)       FUN006
282500970526     C                   END
282600970526     C*
282700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
282800970526     C     FUNGRU        IFEQ      4
282900970526     C                   MOVEL     FUV(19)       FUN001
283000970526     C                   MOVEL     FUV(20)       FUN002
283100970526     C                   MOVEL     FUV(21)       FUN003
283200970526     C                   MOVEL     FUV(22)       FUN004
283300970526     C                   MOVEL     *BLANKS       FUN005
283400970526     C                   MOVEL     *BLANKS       FUN006
283500970526     C                   END
283600970526     C*
283700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
283800970526     C                   MOVEL     *BLANKS       FUN007
283900970526     C                   MOVEL     *BLANKS       FUN008
284000970526     C*
284100970526     C     1             DO        24            I
284200970526     C                   MOVEL     FUD(I)        FUDRIG
284300970526     C     FUDFUN        IFEQ      '24'
284400970526     C     FUDATT        ANDEQ     'S'
284500970526     C                   MOVEL     FUDDES        FUN008
284600970526     C                   END
284700970526     C                   END                                                    DO
284800970526     C*
284900970526     C                   ENDSR
285000970526     C*------------------------------------------------------------------------*
285100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
285200970526     C*------------------------------------------------------------------------*
285300970526     C     FUNCON        BEGSR
285400970526     C*
285500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
285600970526     C                   EXSR      FUNVAR
285700970526     C*
285800970526     C* PULISCO CAMPI DI LAVORO
285900970526     C                   Z-ADD     0             I                 5 0
286000970526     C                   Z-ADD     0             W                 5 0
286100970526     C*
286200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
286300970526     C     FUNKEY        IFNE      *BLANKS
286400970526     C     1             DO        24            I
286500970526     C                   MOVEL     FUD(I)        FUDRIG
286600970526     C*
286700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
286800970526     C     FUNKEY        IFEQ      FUDFUN
286900970526     C     FUDATT        ANDEQ     'S'
287000970526     C                   Z-ADD     I             W
287100970526     C                   END
287200970526     C                   END                                                    DO WHILE
287300970526     C*
287400970526     C* FUNZIONE NON ATTIVA (ERRORE)
287500970526     C     W             IFEQ      0
287600970526     C*
287700970526     C* EMISSIONE MESSAGGIO DI ERRORE
287800970526     C                   MOVEL     MSG(1)        DSMSMS
287900970526     C*
288000970526     C                   END                                                    W=0
288100970526     C                   END                                                    FUNKEY NE *BLANKS
288200970526     C*
288300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
288400970526     C     W             IFNE      0
288500970526     C     FUNKEY        OREQ      *BLANKS
288600970526     C     FUNKEY        IFEQ      *BLANKS
288700970526     C                   EXSR      ENTGES
288800970526     C                   END
288900970526     C     FUNKEY        IFEQ      '03'
289000970526     C                   EXSR      F03GES
289100970526     C                   END
289200010320     C     FUNKEY        IFEQ      '04'
289300010320     C                   EXSR      F04GES
289400010320     C                   END
289500130821     C     FUNKEY        IFEQ      '06'
289600130905     c***                if        f07lna='S' or f07los='S'
289700130905     c***                          or f07lol='S'
289800130905     C***                EXSR      F07GES
289900130905     c***                if        errl00='0'
290000130905     c***                clear                   f07lna
290100130905     c***                clear                   f07lol
290200130905     c***                clear                   f07los
290300130905     c***                endif
290400130903     c
290500130905     c***                else
290600130821     C                   EXSR      F06GES
290700130905     c***                endif
290800130902     c
290900130821     C                   END
291000130827     C     FUNKEY        IFEQ      '07'
291100130827     C                   EXSR      F07GES
291200130827     C                   END
291300970526     C     FUNKEY        IFEQ      '08'
291400970526     C                   EXSR      F08GES
291500970526     C                   END
291600970526     C     FUNKEY        IFEQ      '09'
291700970526     C                   EXSR      F09GES
291800970526     C                   END
291900970526     C     FUNKEY        IFEQ      '10'
292000970526     C                   EXSR      F10GES
292100970526     C                   END
292200970526     C     FUNKEY        IFEQ      '12'
292300970526     C                   EXSR      F12GES
292400970526     C                   END
292500970603     C     FUNKEY        IFEQ      '14'
292600970603     C                   EXSR      F14GES
292700970603     C                   END
292800130828     C     FUNKEY        IFEQ      '17'
292900130828     C                   EXSR      F17GES
293000130828     C                   END
293100970526     C     FUNKEY        IFEQ      '24'
293200970526     C                   EXSR      F24GES
293300970526     C                   END
293400970526     C                   END
293500970526     C*
293600970526     C                   ENDSR
293700970526     C*------------------------------------------------------------------------*
293800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
293900970526     C*------------------------------------------------------------------------*
294000970526     C     FUNVAR        BEGSR
294100970526     C*
294200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
294300970526     C*
294400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
294500970526     C     KEY           IFEQ      ENTER
294600970526     C                   MOVEL     *BLANKS       FUNKEY
294700970526     C                   END
294800970526     C     KEY           IFEQ      F1
294900970526     C                   MOVEL     '01'          FUNKEY
295000970526     C                   END
295100970526     C     KEY           IFEQ      F2
295200970526     C                   MOVEL     '02'          FUNKEY
295300970526     C                   END
295400970526     C     KEY           IFEQ      F3
295500970526     C                   MOVEL     '03'          FUNKEY
295600970526     C                   END
295700970526     C     KEY           IFEQ      F4
295800970526     C                   MOVEL     '04'          FUNKEY
295900970526     C                   END
296000970526     C     KEY           IFEQ      F5
296100970526     C                   MOVEL     '05'          FUNKEY
296200970526     C                   END
296300970526     C     KEY           IFEQ      F6
296400970526     C                   MOVEL     '06'          FUNKEY
296500970526     C                   END
296600970526     C     KEY           IFEQ      F7
296700970526     C                   MOVEL     '07'          FUNKEY
296800970526     C                   END
296900970526     C     KEY           IFEQ      F8
297000970526     C                   MOVEL     '08'          FUNKEY
297100970526     C                   END
297200970526     C     KEY           IFEQ      F9
297300970526     C                   MOVEL     '09'          FUNKEY
297400970526     C                   END
297500970526     C     KEY           IFEQ      F10
297600970526     C                   MOVEL     '10'          FUNKEY
297700970526     C                   END
297800970526     C     KEY           IFEQ      F11
297900970526     C                   MOVEL     '11'          FUNKEY
298000970526     C                   END
298100970526     C     KEY           IFEQ      F12
298200970526     C                   MOVEL     '12'          FUNKEY
298300970526     C                   END
298400970526     C     KEY           IFEQ      F13
298500970526     C                   MOVEL     '13'          FUNKEY
298600970526     C                   END
298700970526     C     KEY           IFEQ      F14
298800970526     C                   MOVEL     '14'          FUNKEY
298900970526     C                   END
299000970526     C     KEY           IFEQ      F15
299100970526     C                   MOVEL     '15'          FUNKEY
299200970526     C                   END
299300970526     C     KEY           IFEQ      F16
299400970526     C                   MOVEL     '16'          FUNKEY
299500970526     C                   END
299600970526     C     KEY           IFEQ      F17
299700970526     C                   MOVEL     '17'          FUNKEY
299800970526     C                   END
299900970526     C     KEY           IFEQ      F18
300000970526     C                   MOVEL     '18'          FUNKEY
300100970526     C                   END
300200970526     C     KEY           IFEQ      F19
300300970526     C                   MOVEL     '19'          FUNKEY
300400970526     C                   END
300500970526     C     KEY           IFEQ      F20
300600970526     C                   MOVEL     '20'          FUNKEY
300700970526     C                   END
300800970526     C     KEY           IFEQ      F21
300900970526     C                   MOVEL     '21'          FUNKEY
301000970526     C                   END
301100970526     C     KEY           IFEQ      F22
301200970526     C                   MOVEL     '22'          FUNKEY
301300970526     C                   END
301400970526     C     KEY           IFEQ      F23
301500970526     C                   MOVEL     '23'          FUNKEY
301600970526     C                   END
301700970526     C     KEY           IFEQ      F24
301800970526     C                   MOVEL     '24'          FUNKEY
301900970526     C                   END
302000970526     C     KEY           IFEQ      ROLLUP
302100970526     C                   MOVEL     'RU'          FUNKEY
302200970526     C                   END
302300970526     C     KEY           IFEQ      HELP
302400970526     C                   MOVEL     'HP'          FUNKEY
302500970526     C                   END
302600970526     C                   ENDSR
302700970526     C*------------------------------------------------------------------------*
302800970526     C* NOTE COSTRUZIONE SCHIERE
302900970526     C*------------------------------------------------------------------------*
303000970526     C*
303100970526     C* -MSG-MESSAGGI
303200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
303300970526     C* (NON TOCCARE MSG,1 E MSG,2)
303400970526     C*
303500970526     C* -CMD-COMANDI
303600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
303700970526     C*
303800970526     C* -OPD-OPZIONI
303900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
304000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
304100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
304200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
304300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
304400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
304500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
304600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
304700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
304800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
304900970526     C*
305000970526     C* -FUD-FUNZIONI
305100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
305200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
305300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
305400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
305500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
305600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
305700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
305800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
305900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
306000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
306100970526     O*------------------------------------------------------------------------*
306200970526** DEC - DECODIFICHE
306300970526    MODIFICA                                                      1
306400970526  ANNULLAMENTO                                                    2
306500970526VISUALIZZAZIONE                                                   3
306600970526   RIPRISTINO                                                     4
306700970526  INSERIMENTO                                                     5
306800970526** MSG - MESSAGGI
306900970526TISI03R- Tasto funzionale non gestito                                          1
307000970526TISI03R- Inserimento effettuato                                                2
307100970526TISI03R- Modifica effettuata                                                   3
307200970526TISI03R- Annullamento effettuato                                               4
307300970526TISI03R- Ripristino effettuato                                                 5
307400970526TISI03R- Richiesta funzione per record inesistente                             6
307500970526TISI03R- Record momentaneamente allocato: riprovare più tardi                  7
307600970527TISI03R- Richiesta funzione per record esistente                               8
307700970526TISI03R- Provincia errata                                                      9
307800970526TISI03R- Linea di arrivo errata                                               10
307900970526TISI03R- Zona di consegna errata                                              11
308000060927§§§§§§§§ Ufficio Postale inesistente o annullato                              12
308100970526TISI03R- Nazione errata                                                       13
308200970526TISI03R- Codice tassazione errato                                             14
308300970526TISI03R- Codice tassazione errato. Accettati solo I1, I2, I3, ecc ...         15
308400970526TISI03R- Per CAP estero lasciare il codice tassazione vuoto                   16
308500060927§§§§§§§§ Zona di consegna NON abilitata                                       17
308600970604TISI03R- Linea limite obbligatoria se immessi limiti di peso o volume         18
308700970604TISI03R- Limiti obbligatori se immesse linea o zona limite                    19
308800130905Esistono OrariServizi personalizz.x la linea variata. Saranno eliminati       20
308900970912TISI03R- Il limite oltre deve essere superiore del limite sotto               21
309000010320TISI03R- Tasto funzione non consentito sul campo corrente.                    22
309100130905TISI03R- F17 consentito soltando sui campi LINEA di ARRIVO valorizzati        23
309200010320TISI03R- Tipo tempo ritiro errato o incompleto                                24
309300970917TISI03R- Il cap può essere obsoleto se lo sono anche tutte le sue località    25
309400970917TISI03R- Impossibile inserire cap obsoleti                                    26
309500970526TISI03R- Immettere 'S' o 'N'                                                  27
309600110623TISI03R- Tipo inoltro errato o non utilizzabile nel cappario                  28
309700970917TISI03R- Immettere 'O' per rendere obsoleto il cap, altrimenti lasciare vuoto 29
309800970603TISI03R- Tipo tempo di consegna errato o incompleto                           30
309900970526** FUD - FUNZIONI
310000970526                   -        (S/N)=N                        funz.01¬
310100970530                   -        (S/N)=N                        funz.02¬
310200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
310300010320F04=Visual. tempi  -        (S/N)=S  <== S-FISSO           funz.04¬
310400130827                   -        (S/N)=N                        funz.05¬
310500970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
310600130827F07=Orari servizi  -        (S/N)=S                        funz.07¬
310700970526F08=Località       -        (S/N)=S                        funz.08¬
310800970526F09=Eccezioni      -        (S/N)=S                        funz.09¬
310900970526F10=Tempi consegna -        (S/N)=S                        funz.10¬
311000970526                   -        (S/N)=N                        funz.11¬
311100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
311200970526                   -        (S/N)=N                        funz.13¬
311300970603F14=Zone consegna  -        (S/N)=S                        funz.14¬
311400970526                   -        (S/N)=N                        funz.15¬
311500970526                   -        (S/N)=N                        funz.16¬
311600130905F17=OrariServLinea -        (S/N)=S                        funz.17¬
311700970526                   -        (S/N)=N                        funz.18¬
311800970526                   -        (S/N)=N                        funz.19¬
311900970526                   -        (S/N)=N                        funz.20¬
312000970526                   -        (S/N)=N                        funz.21¬
312100970526                   -        (S/N)=N                        funz.22¬
312200970526                   -        (S/N)=N                        funz.23¬
312300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
