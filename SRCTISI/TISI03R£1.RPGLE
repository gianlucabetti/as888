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
001800130902     FAZcos01L  IF   E           K DISK
001900130903     FAZcos02L  IF   E           K DISK    rename(azcos000:azcos002)
002000970526     D*------------------------------------------------------------------------*
002100970526     D* SCHIERE
002200970526     D*------------------------------------------------------------------------*
002300970526     D*------------
002400970526     D* DECODIFICHE
002500970526     D*------------
002600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* MESSAGGI
002900970526     D*------------
003000130903     D MSG             S             78    DIM(31) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* DESCRIZIONI TASTI FUNZIONALI
003300970526     D*------------
003400970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* FUNZIONI DA CARICARE A VIDEO
003700970526     D*------------
003800970526     D FUV             S             19    DIM(24)
003900970526     D*------------------------------------------------------------------------*
004000970526     D* INPUT
004100970526     D*------------------------------------------------------------------------*
004200970526     D*------------
004300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004400970526     D*------------
004500970526     D FUDDS           DS
004600970526     D  FUDDE1                 1      1
004700970526     D  FUDFUN                 2      3
004800970526     D  FUDDE2                 4     19
004900970526     D  FUDFI0                20     34
005000970526     D  FUDATT                35     35
005100970526     D  FUDFI1                36     80
005200970526     D  FUDDES                 1     19
005300970526     D  FUDRIG                 1     80
005400970526     D*------------
005500970526     D* DS GESTIONE TASTI FUNZIONALI
005600970526     D*------------
005700970526     D DSKEY         E DS
005800970526     D DEVDS1          DS
005900970526     D  KEY                  369    369
006000970526     D*------------
006100970526     D* DS "XMSG" - GESTIONE MESSAGGI
006200970526     D*------------
006300970526     D DSMSG         E DS
006400970526     D*------------------
006500970526     D* DS NAZIONI
006600970526     D*------------------
006700970526     D DS15          E DS
006800970526     D*------------------
006900970526     D* DS PROVINCIE
007000970526     D*------------------
007100970526     D DSPR          E DS
007200970526     D*------------------
007300970526     D* DS ZONE DI CONSEGNA
007400970526     D*------------------
007500970526     D DS03          E DS
007600970526     D*------------------
007700970526     D* DS CODICI TASSAZIONE
007800970526     D*------------------
007900970526     D DSCT          E DS
008000970526     D*------------------
008100970526     D* DS TIPI INOLTRO
008200970526     D*------------------
008300970526     D DS9A          E DS
008400970526     D*------------------
008500970626     D* DS "TISI95R" - CONTROLLO CAP
008600970526     D*------------------
008700970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
008800970526     D*------------
008900970526     D* DS DI PROCEDURA
009000970526     D*------------
009100970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
009200130821     d tisio0ds      E DS
009300970526     D*------------
009400970526     D* ARCHITETTURA
009500970526     D*------------
009600970526     D KPJBA         E DS
009700050413     D*------------
009800050413     D* DS REPERIMENTO DATI UTENTE
009900050413     D*------------
010000050413     D TIBS34DS      E DS                                                       *Profili utente
010100050413     D DDATIUTE      E DS                                                       *Dati utente
010200050413     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010300050413     D DUTE01        E DS                                                       *Abilitazioni
010400090605
010500090605     d ds5e          e ds
010600090903     d
010700090903     d dvpodeco      e ds
010800090903     d Tibs02Ds      e ds
010900120712
011000120712     d TRTB09DS      e ds
011100120712
011200000420     D*------------------
011300000420     D* PASSAGGIO PARAMETRI
011400000420     D*------------------
011500000420     D PARAM           DS
011600000420     D  POP1                   1      3
011700000420     D  PUPI                   4      8  0
011800000420     D  PMSG                   9     88
011900000420     D  POPZ                  89     91
012000090903
012100090903     d datasys         s               d   inz(*sys)
012200090903     d dataiso         s               d   datfmt(*iso)
012300130902     d caplna          s                   like(vidlna)
012400130902     d loclna          s                   like(vidlna)
012500130902     d caplol          s                   like(vidlna)
012600130902     d loclol          s                   like(vidlna)
012700130902     d caplos          s                   like(vidlna)
012800130902     d loclos          s                   like(vidlna)
012900970526     C*------------------------------------------------------------------------*
013000970526     C* MAIN LINES
013100970526     C*------------------------------------------------------------------------*
013200970526     C*
013300970526     C* NON CI SONO ERRORI IN ENTRATA
013400970526IF  1C     ERRL00        IFEQ      '0'
013500970526     C*
013600970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013700970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014000970526     C*
014100970526     C* EMETTE FORMATO VIDEO
014200970526     C                   EXSR      BUTFMR
014300970526     C*
014400970526     C* GESTIONE TASTI FUNZIONALI
014500970526     C                   EXSR      FUNCON
014600970526E   2C                   ENDDO
014700970526     C*
014800970526     C* CI SONO ERRORI IN ENTRATA
014900970527X   1C                   ELSE
015000970526     C                   MOVEL     '1'           D00ERR
015100970526     C                   MOVEL     DSMSMS        D00MSG
015200970526E   1C                   ENDIF
015300970703     C*
015400970703     C* OPERAZIONI FINALI
015500970703     C                   EXSR      FINSR
015600970526     C* FINE PGM
015700970526     C                   SETON                                        LR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
016000970526     C*------------------------------------------------------------------------*
016100970526     C     BUTFMR        BEGSR
016200970526     C*
016300970526     C* EMETTE LA TESTATA
016400970526     C                   WRITE     SI03D01
016500970526     C*
016600970526     C* SPEDISCE EVENTUALE MESSAGGIO
016700970526     C                   EXSR      SNDMSG
016800970526     C*
016900970526     C* EMETTE LE FUNZIONI
017000970526     C                   WRITE     SI03D02
017100970526     C*
017200970526     C* EMETTE IL VIDEO
017300970526     C                   EXFMT     SI03D03
017400970526     C*
017500970526     C* REIMPOSTA FLAG DI ERRORE
017600970526     C                   MOVEL     '0'           ERRL00
017700970526     C*
017800970526     C* REIMPOSTA INDICATORI DI ERRORE
017900970526     C                   SETOFF                                         5859
018000970526     C                   SETOFF                                       606162
018100970526     C                   SETOFF                                       636465
018200970526     C                   SETOFF                                       666768
018300970526     C                   SETOFF                                       697071
018400970526     C                   SETOFF                                       727374
018500970526     C                   SETOFF                                       757677
018600970526     C                   SETOFF                                       787980
018700970526     C                   SETOFF                                       818283
018800970526     C                   SETOFF                                       848586
018900970526     C                   SETOFF                                       878889
019000000420     C                   SETOFF                                       525354
019100010322     C                   SETOFF                                       5544
019200970526     C*
019300970526     C                   ENDSR
019400970526     C*------------------------------------------------------------------------*
019500970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
019600970526     C*------------------------------------------------------------------------*
019700970526     C     SNDMSG        BEGSR
019800970526     C*
019900970526     C* PULISCE IL SFILE MESSAGGI
020000970526     C                   MOVEL     '1'           *IN40
020100970526     C                   WRITE     SI03DMC
020200970526     C                   MOVEL     '0'           *IN40
020300970526     C*
020400970526     C* VISUALIZZA IL MESSAGGIO
020500970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
020600970526     C                   CALL      'XMSG'        XMSGPA
020700970526IF  1C     DSMSER        IFEQ      *ZEROS
020800970526     C                   MOVEL     DSMSKE        MSGKEY
020900970526E   1C                   ENDIF
021000970526     C                   Z-ADD     1             MSGULT
021100970526     C                   WRITE     SI03DMS
021200970526     C                   WRITE     SI03DMC
021300970526     C*
021400970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
021500970526     C                   MOVEL     *BLANKS       DSMSMS
021600970526     C*
021700970526     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900970526     C* ENTGES - GESTIONE ENTER -> INVIO
022000970526     C*------------------------------------------------------------------------*
022100970526     C     ENTGES        BEGSR
022200970526     C*
022300970526     C* CONTROLLA I CAMPI IMMESSI
022400970526     C                   EXSR      DECODI
022500970526     C*
022600970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
022700980128IF  1C     DEPOP1        IFEQ      'O05'
022800980128     C                   MOVEL     '1'           FINL00
022900980128E   1C                   ENDIF
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* F03GES - GESTIONE F03 -> FINE
023400970526     C*------------------------------------------------------------------------*
023500970526     C     F03GES        BEGSR
023600970526     C*
023700970526     C* IMPOSTA "PREMUTO F03"
023800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
023900970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
024000970526     C*
024100970526     C                   ENDSR
024200010320     C*------------------------------------------------------------------------*
024300010320     C* F04GES - GESTIONE F04 -> VISUALIZZAZIONE TEMPI CONSEGNA/RITIRO
024400010320     C*------------------------------------------------------------------------*
024500010320     C     F04GES        BEGSR
024600090605      * imposto le descrizioni fisse per i 3 tipi servizio utilizzati
024700090903      * 'C' - 'D' - 'E' -'H'
024800090605     c                   eval      ktbcod = '5E'
024900090605     c     ke2tab        setll     tabel00f
025000090605     c                   do        *hival
025100090605     c     ke2tab        reade     tabel00f
025200090605     c                   if        %eof(tabel00f)
025300090605     c                   leave
025400090605     c                   endif
025500090605     c                   eval      ds5e = tbluni
025600090605     c                   if        tblkey = 'C'
025700090605     c                   eval      viddtspc = §5ed03
025800090605     c                   endif
025900090605     c                   if        tblkey = 'D'
026000090605     c                   eval      viddtspd = §5ed03
026100090605     c                   endif
026200090605     c                   if        tblkey = 'E'
026300090605     c                   eval      viddtspe = §5ed03
026400090605     c                   endif
026500090903     c                   if        tblkey = 'H' and (vidh0c>0 or
026600090903     c                             Datasys>=dataiso)
026700090903     c                   eval      viddtsph = §5ed03
026800090903     c                   endif
026900090605     c                   enddo
027000010320     C*
027100010320     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
027200010320     C     RNMFL         IFEQ      'VIDTTC'
027300010320     C                   EXFMT     SI03WWC
027400010320     C                   ELSE
027500010320     C     RNMFL         IFEQ      'VIDTTR'
027600010320     C                   EXFMT     SI03WWR
027700010320     C                   ELSE
027800010322     C                   MOVEL     MSG(22)       DSMSMS
027900010320     C                   ENDIF
028000010320     C                   ENDIF
028100010320     C*
028200010320     C                   ENDSR
028300130821     C*------------------------------------------------------------------------*
028400130827     C* F07GES - GESTIONE F07 -> Orari servizi
028500130821     C*------------------------------------------------------------------------*
028600130827     C     F07GES        BEGSR
028700130902     c* Gestisco prima l'aggiornamento e poi gli orari
028800130902     c                   exsr      F06ges
028900130903     c                   if        errl00='0'
029000130821     C*
029100130821     c                   clear                   tisio0ds
029200130821     C                   MOVEL     VIDNAR        do0NAR
029300130821     C                   MOVEL     VIDCAP        Do0CAP
029400130830     c                   if        %subst(depop0:2:2)='02'
029500130830     C                   MOVEL     'G'           do0tric
029600130830     c                   else
029700130830     C                   MOVEL     'I'           do0tric
029800130830     c                   endif
029900130821     c                   movel     d00dri        do0dri
030000130821     c                   movel     d00ver        do0ver
030100130821
030200130821     C                   call      'TISIO2R'
030300130821     C                   PARM                    KPJBA
030400130821     C                   PARM                    tisio0ds
030500130821     C*
030600130821     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
030700130903     c                   clear                   f12l00
030800130821IF  1C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
030900130821     C                   MOVEL     '1'           ERRL00                         *ERRORE
031000130821     C                   MOVEL     '1'           F03L00                         *FINE PGM
031100130821X   1C                   ELSE                                                   *NON PREMUTO F03
031200130821     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
031300130821E   1C                   ENDIF
031400130821     C*
031500130903E   1C                   ENDIF
031600130903     C                   MOVEL     ' '           FINL00                         *FINE PGM
031700130821     C*
031800130821     C                   ENDSR
031900130828     C*------------------------------------------------------------------------*
032000130828     C* F17GES - GESTIONE F17 -> Orari servizi  per linea di arrivo
032100130828     C*------------------------------------------------------------------------*
032200130828     C     F17GES        BEGSR
032300130828     c                   clear                   dsmsms
032400130828     c                   exsr      decodi
032500130828     c
032600130828    1c                   if        dsmsms=*blanks
032700130828     C*
032800130828     c                   clear                   tisio0ds
032900130828     C                   MOVEL     VIDNAR        do0NAR
033000130830     c                   if        %subst(depop0:2:2)='02'
033100130830     C                   MOVEL     'G'           do0tric
033200130830     c                   else
033300130830     C                   MOVEL     'I'           do0tric
033400130830     c                   endif
033500130828     c                   movel     d00dri        do0dri
033600130828     c                   movel     d00ver        do0ver
033700130828
033800130828     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
033900130828    2c                   select
034000130828     C     RNMFL         wheneq    'VIDLNA'
034100130828     c     vidlna        andgt     *zeros
034200130828     C                   movel     vidlna        do0lna
034300130828     c
034400130828     C     RNMFL         wheneq    'VIDLOL'
034500130828     c     vidlol        andgt     *zeros
034600130828     C                   movel     vidlol        do0lna
034700130828     C     RNMFL         wheneq    'VIDLOS'
034800130828     c     vidlos        andgt     *zeros
034900130828     C                   movel     vidlos        do0lna
035000130828     C                   other
035100130828     C                   MOVEL     MSG(23)       DSMSMS
035200130828    2C                   ENDsl
035300130828
035400130828    2c                   if        dsmsms=*blanks
035500130828     C                   call      'TISIO2R'
035600130828     C                   PARM                    KPJBA
035700130828     C                   PARM                    tisio0ds
035800130828     C*
035900130828     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
036000130828IF  3C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
036100130828     C                   MOVEL     '1'           ERRL00                         *ERRORE
036200130828     C                   MOVEL     '1'           F03L00                         *FINE PGM
036300130828X   1C                   ELSE                                                   *NON PREMUTO F03
036400130828     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
036500130828E   3C                   ENDIF
036600130828     C*
036700130828E   2C                   ENDIF
036800130828E   1C                   ENDIF
036900130828     C*
037000130828     C                   ENDSR
037100970526     C*------------------------------------------------------------------------*
037200970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
037300970526     C*------------------------------------------------------------------------*
037400970526     C     F06GES        BEGSR
037500970526     C*
037600970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
037700970526IF  1C     DEPOP1        IFEQ      'F06'
037800970526     C                   EXSR      INSERI
037900970526E   1C                   ENDIF
038000970526IF  1C     DEPOP1        IFEQ      'O02'
038100970526     C                   EXSR      MODIFI
038200970526E   1C                   ENDIF
038300970526IF  1C     DEPOP1        IFEQ      'O04'
038400970526     C                   EXSR      ANNULL
038500970526E   1C                   ENDIF
038600970526IF  1C     DEPOP1        IFEQ      'O07'
038700970526     C                   EXSR      RIPRIS
038800970526E   1C                   ENDIF
038900130902     c
039000970526     C                   ENDSR
039100970526     C*------------------------------------------------------------------------*
039200970526     C* F08GES - GESTIONE F08 -> GESTIONE LOCALITA'
039300970526     C*------------------------------------------------------------------------*
039400970526     C     F08GES        BEGSR
039500970528     C*
039600970602     C                   MOVEL     VIDNAR        DNAR
039700970602     C                   MOVEL     VIDCAP        DCAP
039800970528     C                   MOVEL     DEPOP0        WOP0
039900970528     C                   MOVEL     'L'           WOP0
040000970528     C                   MOVEL     'F08'         WOP1
040100970526     C                   MOVEL     'TISI30R'     WPGM              8
040200970526     C                   EXSR      LANPGM
040300970526     C*
040400970526     C                   ENDSR
040500970526     C*------------------------------------------------------------------------*
040600970526     C* F09GES - GESTIONE F09 -> ECCEZIONI LINEE/ZONE
040700970526     C*------------------------------------------------------------------------*
040800970526     C     F09GES        BEGSR
040900970528     C*
041000970602     C                   MOVEL     VIDNAR        DNAR
041100970602     C                   MOVEL     VIDCAP        DCAP
041200970528     C                   MOVEL     DEPOP0        WOP0
041300970528     C                   MOVEL     'E'           WOP0
041400970526     C                   MOVEL     'F09'         WOP1
041500970526     C                   MOVEL     'TISI11R'     WPGM              8
041600970526     C                   EXSR      LANPGM
041700970526     C*
041800970526     C                   ENDSR
041900970526     C*------------------------------------------------------------------------*
042000970526     C* F10GES - GESTIONE F10 -> TEMPI DI CONSEGNA
042100970526     C*------------------------------------------------------------------------*
042200970526     C     F10GES        BEGSR
042300970528     C*
042400970602     C                   MOVEL     VIDNAR        DNAR
042500970602     C                   MOVEL     VIDCAP        DCAP
042600970528     C                   MOVEL     DEPOP0        WOP0
042700970528     C                   MOVEL     'T'           WOP0
042800970526     C                   MOVEL     'F10'         WOP1
042900970526     C                   MOVEL     'TISI41R'     WPGM              8
043000970526     C                   EXSR      LANPGM
043100970526     C*
043200970526     C                   ENDSR
043300970526     C*------------------------------------------------------------------------*
043400970526     C* F12GES - GESTIONE F12 -> RITORNO
043500970526     C*------------------------------------------------------------------------*
043600970526     C     F12GES        BEGSR
043700970526     C*
043800970526     C* IMPOSTA "PREMUTO F12"
043900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
044000970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
044100970526     C*
044200970526     C                   ENDSR
044300970603     C*------------------------------------------------------------------------*
044400970603     C* F14GES - GESTIONE F14 -> ZONE DI CONSEGNA
044500970603     C*------------------------------------------------------------------------*
044600970603     C     F14GES        BEGSR
044700970603     C*
044800970603     C                   CALL      'TRTB23R'
044900970603     C                   PARM                    KPJBA
045000970612     C*
045100970603     C                   ENDSR
045200970526     C*------------------------------------------------------------------------*
045300970526     C* F21GES - GESTIONE F21 -> STAMPA CAPPARIO
045400970526     C*------------------------------------------------------------------------*
045500970526     C     F21GES        BEGSR
045600970526     C*
045700970602     C                   MOVEL     VIDNAR        DNAR
045800970602     C                   MOVEL     VIDCAP        DCAP
045900970526     C                   MOVEL     'F21'         WOP1
046000970526     C                   MOVEL     'TISI04R'     WPGM              8
046100970526     C                   EXSR      LANPGM
046200970526     C*
046300970526     C                   ENDSR
046400970526     C*------------------------------------------------------------------------*
046500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
046600970526     C*------------------------------------------------------------------------*
046700970526     C     F24GES        BEGSR
046800970526     C*
046900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
047000970526     C*
047100970526     C                   ENDSR
047200970526     C*------------------------------------------------------------------------*
047300000420     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
047400970526     C*------------------------------------------------------------------------*
047500970526     C     INSERI        BEGSR
047600970526     C*
047700970526     C* CONTROLLA I CAMPI IMMESSI
047800970526     C                   EXSR      DECODI
047900970526     C*
048000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
048100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
048200970526     C                   Z-ADD     D00VER        CPCVER                         *CHIAVE
048300970526     C                   MOVEL     VIDNAR        CPCNAR
048400970526     C                   MOVEL     VIDCAP        CPCCAP
048500000420     C                   EXSR      IMPCPC                                       *ALTRI CAMPI
048600970526     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
048700970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
048800970526     C                   MOVEL     *BLANKS       CPCATB                         *ANNULLAMENTO
048900980616     C                   MOVEL     'N'           CPCESE                         *ECCEZIONI X LINEA
049000980616     C                   MOVEL     'N'           CPCESL                         *ECCEZIONI X LOC
049100970526     C                   WRITE     AZCPC000
049200000420     C*
049300000420     C                   EXSR      LETCPC
049400000420     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
049500970602     C*
049600970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
049700970602     C                   EXSR      AZZVID
049800970526E   1C                   ENDIF
049900970526     C*
050000970526     C                   ENDSR
050100970526     C*------------------------------------------------------------------------*
050200000420     C* MODIFI - OPERAZIONI DI MODIFICA
050300970526     C*------------------------------------------------------------------------*
050400970526     C     MODIFI        BEGSR
050500970526     C*
050600970526     C* CONTROLLA I CAMPI IMMESSI
050700970526     C                   EXSR      DECODI
050800970526     C*
050900970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
051000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051100000420     C                   EXSR      LETCPC
051200970526     C*
051300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
051400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051500970526     C                   EXSR      IMPCPC                                       *IMPOSTA I CAMPI
051600970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
051700970526     C                   MOVEL     *BLANKS       CPCATB                         *ANNULLAMENTO
051800970526     C                   UPDATE    AZCPC000
051900000420     C*
052000000420     C                   MOVEL     MSG(3)        D00MSG
052100000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
052200970526E   2C                   ENDIF
052300970526E   1C                   ENDIF
052400970526     C*
052500970526     C                   ENDSR
052600970526     C*------------------------------------------------------------------------*
052700970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
052800970526     C*------------------------------------------------------------------------*
052900970526     C     ANNULL        BEGSR
053000130902     C*
053100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
053200000420     C                   EXSR      LETCPC
053300970526     C*
053400970526     C* ANNULLA LE LOCALITA'
053500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
053600970526     C                   Z-ADD     CPCVER        KCLVER
053700970526     C                   MOVEL     CPCNAR        KCLNAR
053800970526     C                   MOVEL     CPCCAP        KCLCAP
053900970917     C     KEYCPL        SETLL     AZCPL01L                           99
054000970526     C     KEYCPL        READE     AZCPL01L                               99
054100970526DO  2C     *IN99         DOWEQ     *OFF
054200970526IF  3C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
054300970526IF  4C     CPLDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
054400970526     C                   MOVEL     'A'           CPLATB                         *FLAG ANNULLAMENTO
054500970526     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
054600970526     C                   UPDATE    AZCPL000                                     *ANNULLA
054700970526X   4C                   ELSE                                                   *MAI TRASMESSO
054800000426     C                   DELETE    AZCPL000                                     *CANCELLA
054900970526E   4C                   ENDIF
055000970526E   3C                   ENDIF
055100970526     C     KEYCPL        READE     AZCPL01L                               99
055200970526E   2C                   ENDDO                                                  *FINE LOCALITA'
055300000420     C*
055400970526     C* ANNULLA I SINONIMI
055500970526     C                   Z-ADD     D00VER        KCSVER
055600970526     C                   MOVEL     CPCNAR        KCSNAR
055700970526     C                   MOVEL     CPCCAP        KCSCAP
055800970526     C     KEYCPS        SETLL     AZCPS01L                           99
055900970526     C     KEYCPS        READE     AZCPS01L                               99
056000970526DO  2C     *IN99         DOWEQ     *OFF
056100970526IF  3C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
056200970526IF  4C     CPSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
056300970526     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
056400970526     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
056500970526     C                   UPDATE    AZCPS000                                     *ANNULLA
056600970526X   4C                   ELSE                                                   *MAI TRASMESSO
056700970526     C                   DELETE    AZCPS000                                     *CANCELLA
056800970526E   4C                   ENDIF
056900970526E   3C                   ENDIF
057000970526     C     KEYCPS        READE     AZCPS01L                               99
057100970526E   2C                   ENDDO                                                  *FINE SINONIMI
057200970526     C*
057300000602     C* ANNULLA LE ECCEZIONI
057400970526     C                   Z-ADD     CPCVER        KCEVER
057500970526     C                   MOVEL     CPCNAR        KCENAR
057600970526     C                   MOVEL     CPCCAP        KCECAP
057700970526     C     KEYCPE        SETLL     AZCPE01L                           99
057800970526     C     KEYCPE        READE     AZCPE01L                               99
057900970526DO  2C     *IN99         DOWEQ     *OFF
058000970526IF  3C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
058100970526IF  4C     CPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
058200970526     C                   MOVEL     'A'           CPEATB                         *FLAG ANNULLAMENTO
058300970526     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
058400970526     C                   UPDATE    AZCPE000                                     *ANNULLA
058500970526X   4C                   ELSE                                                   *MAI TRASMESSO
058600970526     C                   DELETE    AZCPE000                                     *CANCELLA
058700970526E   4C                   ENDIF
058800970526E   3C                   ENDIF
058900970526     C     KEYCPE        READE     AZCPE01L                               99
059000970526E   2C                   ENDDO                                                  *FINE SINONIMI
059100970526     C*
059200970526     C* ANNULLA IL CAP
059300000602     C                   MOVEL     '0'           ANNDLT            1
059400970526IF  2C     CPCATB        IFEQ      *BLANKS                                      *NO ANNULLATI
059500970526IF  3C     CPCDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
059600970526     C                   MOVEL     'A'           CPCATB                         *FLAG ANNULLAMENTO
059700970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
059800970526     C                   UPDATE    AZCPC000                                     *ANNULLA
059900970526X   3C                   ELSE                                                   *MAI TRASMESSO
060000970526     C                   DELETE    AZCPC000                                     *CANCELLA
060100000602     C                   MOVEL     '1'           ANNDLT
060200970526E   3C                   ENDIF
060300970526E   2C                   ENDIF
060400000420     C*
060500000420     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
060600000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
060700970526E   1C                   ENDIF
060800970526     C*
060900970526     C                   ENDSR
061000970526     C*------------------------------------------------------------------------*
061100000420     C* RIPRIS - OPERAZIONI DI RIPRISTINO
061200970526     C*------------------------------------------------------------------------*
061300970526     C     RIPRIS        BEGSR
061400970526     C*
061500970526     C* CONTROLLA I CAMPI IMMESSI
061600970526     C                   EXSR      DECODI
061700970526     C*
061800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
061900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
062000000420     C                   EXSR      LETCPC
062100970526     C*
062200970526     C* IMPOSTA IL RECORD DA SCRIVERE
062300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
062400970526     C                   MOVEL     *BLANKS       CPCATB                         *FLAG ANNULLAMENTO
062500970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
062600000420     C                   UPDATE    AZCPC000                                     *RIPRISTINA
062700000420     C*
062800970526E   2C                   ENDIF
062900970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
063000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
063100970526E   1C                   ENDIF
063200970526     C*
063300970526     C                   ENDSR
063400970526     C*------------------------------------------------------------------------*
063500970526     C* LETCPC - LEGGE IL RECORD PERL'AGGIORNAMENTO
063600970526     C*------------------------------------------------------------------------*
063700970526     C     LETCPC        BEGSR
063800970526     C*
063900000420     C     DEPOP1        IFNE      'F06'
064000970526     C                   Z-ADD     D00VER        KCCVER
064100980127     C                   MOVEL     D00NAR        KCCNAR
064200980127     C                   MOVEL     D00CAP        KCCCAP
064300970526     C     KEYCPC        CHAIN     AZCPC01L                           9998
064400970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
064500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
064600970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
064700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
064800970526E   1C                   ENDIF
064900970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
065000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
065100970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
065200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
065300970526E   1C                   ENDIF
065400000420     C                   ENDIF
065500000420     C*
065600000420     C                   ENDSR
065700970526     C*------------------------------------------------------------------------*
065800970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
065900970526     C*------------------------------------------------------------------------*
066000970526     C     DECODI        BEGSR
066100050428     C*
066200050428     C* NO VALORI NEGATIVI
066300050428     C                   IF        VIDLKS < *zeros
066400050428     C                   EVAL      VIDLKS = VIDLKS * -1
066500050428     C                   ENDIF
066600050428     C                   IF        VIDLKG < *zeros
066700050428     C                   EVAL      VIDLKG = VIDLKG * -1
066800050428     C                   ENDIF
066900050428     C                   IF        VIDLK2 < *zeros
067000050428     C                   EVAL      VIDLK2 = VIDLK2 * -1
067100050428     C                   ENDIF
067200050428     C                   IF        VIDLMS < *zeros
067300050428     C                   EVAL      VIDLMS = VIDLMS * -1
067400050428     C                   ENDIF
067500050428     C                   IF        VIDLMC < *zeros
067600050428     C                   EVAL      VIDLMC = VIDLMC * -1
067700050428     C                   ENDIF
067800050428     C                   IF        VIDLM2 < *zeros
067900050428     C                   EVAL      VIDLM2 = VIDLM2 * -1
068000050428     C                   ENDIF
068100050428     C*
068200970527     C*--------------------
068300970526     C* NAZIONE
068400970527     C*--------------------
068500970526     C     *LIKE         DEFINE    §15CPR        DEPCPR
068600970527IF  1C     ERRL00        IFEQ      '0'
068700970526     C                   MOVEL     *BLANKS       VDDNAR
068800970526     C                   MOVEL     VIDNAR        WNAR
068900970526     C                   MOVEL     *BLANKS       WDNAR
069000970526     C                   MOVEL     *BLANKS       WCPR
069100970526     C                   EXSR      CHKNAR
069200970526     C                   MOVEL     WNAR          VIDNAR
069300970526     C                   MOVEL     WDNAR         VDDNAR
069400970526     C                   MOVEL     WCPR          DEPCPR
069500970526IF  2C     ERRL00        IFEQ      '1'
069600970526     C                   SETON                                        60
069700970526     C                   MOVEL     MSG(13)       DSMSMS
069800970526E   2C                   ENDIF
069900970526E   1C                   ENDIF
070000970527     C*--------------------
070100970526     C* CAP
070200970527     C*--------------------
070300970526IF  1C     ERRL00        IFEQ      '0'
070400970526     C                   MOVEL     VIDNAR        WNAR
070500970602     C                   MOVEL     VIDCAP        WCAP
070600970526     C                   EXSR      CHKCAP
070700970526IF  2C     ERRL00        IFEQ      '1'
070800970526     C                   SETON                                        61
070900970526E   2C                   ENDIF
071000970526E   1C                   ENDIF
071100970526     C*--------------------
071200970526     C* NAZIONE + CAP
071300970526     C*--------------------
071400970527     C* INSERIMENTO -> RECORD INESISTENTE
071500970526IF  1C     ERRL00        IFEQ      '0'
071600970527IF  2C     DEPOP1        IFEQ      'F06'
071700970526     C                   Z-ADD     D00VER        KCCVER
071800970526     C                   MOVEL     VIDNAR        KCCNAR
071900970526     C                   MOVEL     VIDCAP        KCCCAP
072000970526     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
072100970526IF  3C     *IN99         IFEQ      *OFF
072200970527     C                   SETON                                        6061
072300970526     C                   MOVEL     '1'           ERRL00
072400970527     C                   MOVEL     MSG(8)        DSMSMS
072500970526E   3C                   ENDIF
072600970526E   2C                   ENDIF
072700970526E   1C                   ENDIF
072800970526     C*--------------------
072900970604     C* LINEA ARRIVO STANDARD
073000970526     C*--------------------
073100970526IF  1C     ERRL00        IFEQ      '0'
073200970526     C                   MOVEL     *BLANKS       VDDLNA
073300970526     C                   MOVEL     VIDLNA        WLIN
073400970526     C                   MOVEL     *BLANKS       WDLIN
073500970526     C                   EXSR      CHKLIN
073600970526     C                   MOVEL     WLIN          VIDLNA
073700970526     C                   MOVEL     WDLIN         VDDLNA
073800970526IF  2C     ERRL00        IFEQ      '1'
073900970526     C                   SETON                                        62
074000970526     C                   MOVEL     MSG(10)       DSMSMS
074100130902     c*
074200130902   x2c                   else
074300130902     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
074400130902     c                   movel     vidlna        w0030             3 0
074500130902    3c                   if        w0030 =cpclna
074600130902     c                   clear                   caplna
074700130902     c                   clear                   loclna
074800130903     c                   clear                   f07lna
074900130902    3c                   endif
075000130902     c
075100130902    3c                   if        w0030 <>cpclna and vidlna<>caplna
075200130902     c
075300130902     C                   Z-ADD     D00VER        cosVER                         *CHIAVE
075400130902     C                   MOVEL     VIDNAR        cosNAR
075500130902     C                   MOVEL     VIDCAP        cosCAP
075600130902     C                   clear                   cosloc
075700130902     C                   MOVEL     cpclna        coslna
075800130902     c     kcos          chain     azcos01l
075900130902    4c                   if        %found(azcos01l)
076000130902     c                   movel     Msg(20)       DSmsms
076100130902     c                   eval      *in62  = *on
076200130902     c                   eval      ErrL00 = *on
076300130903     c                   eval      f07lna='S'
076400130902e   4c                   endif
076500130902c
076600130902     c                   eval      caplna=vidlna
076700130902    3c                   endif
076800130902     c*
076900130902     c* Verifico se ci sono inserimenti a livello di località per quel CAP
077000130903IF  3C     ERRL00        IFEQ      '0'
077100130903    4c                   if        w0030 <>cpclna and vidlna<>loclna
077200130903     c
077300130903     C                   Z-ADD     D00VER        cosVER                         *CHIAVE
077400130903     C                   MOVEL     VIDNAR        cosNAR
077500130903     C                   MOVEL     VIDCAP        cosCAP
077600130903     C                   movel     'A'           cosloc
077700130903     C                   MOVEL     cpclna        coslna
077800130903     c     kcos2         setll     azcos02l
077900130903     c     kcos22        reade     azcos02l
078000130903    5c                   dow       not %eof(azcos02l) and errl00='0'
078100130903     c                   movel     cosloc        kclloc
078200130903     c                   movel     coscap        kclcap
078300130903     c                   movel     cosver        kclver
078400130903     c                   movel     cosnar        kclnar
078500130903     c* se la trovo, verifico che la località non abbia una eccezione
078600130903     c     KEYCPL2       chain     azcpl01l
078700130903    6c                   if        %found(azcpl01l) and
078800130903     c                             cpllna<>cpclna and cpllol<>cpclna and
078900130903     c                             cpllos<>cpclna
079000130903     c
079100130905     c                   movel     Msg(20)       DSmsms
079200130903     c                   eval      *in62  = *on
079300130903     c                   eval      ErrL00 = *on
079400130903e   6c                   endif
079500130903     c     kcos22        reade     azcos02l
079600130903    5c                   enddo
079700130903c
079800130903     c                   eval      loclna=vidlna
079900130903     c*
080000130903E   4C                   ENDIF
080100130903E   3C                   ENDIF
080200130903E   2C                   ENDIF
080300130903E   1C                   ENDIF
080400970604     C*--------------------
080500970604     C* ZONA DI CONSEGNA STANDARD
080600970604     C*--------------------
080700970604IF  1C     ERRL00        IFEQ      '0'
080800970604     C                   MOVEL     VIDLNA        WLIN
080900970604     C                   Z-ADD     VIDZNC        WZNC
081000970604     C                   EXSR      CHKZNC
081100970604IF  2C     ERRL00        IFEQ      '1'
081200970604     C                   SETON                                        63
081300970604     C                   MOVEL     MSG(11)       DSMSMS
081400970604E   2C                   ENDIF
081500060206if  2c                   if        §03nuc = 'S'
081600060206     c                   movel     Msg(17)       DSmsms
081700060206     c                   eval      *in63  = *on
081800060206     c                   eval      ErrL00 = *on
081900060206e   2c                   endif
082000970604E   1C                   ENDIF
082100970604     C*--------------------
082200970912     C* LINEA ARRIVO SOTTO
082300970604     C*--------------------
082400970604IF  1C     ERRL00        IFEQ      '0'
082500970604     C                   MOVEL     *BLANKS       VDDLOS
082600970604IF  2C     VIDLOS        IFNE      *ZEROS
082700970604     C     VIDLOS        ANDNE     *BLANKS
082800970604     C                   MOVEL     VIDLOS        WLIN
082900970604     C                   MOVEL     *BLANKS       WDLIN
083000970604     C                   EXSR      CHKLIN
083100970604     C                   MOVEL     WLIN          VIDLOS
083200970604     C                   MOVEL     WDLIN         VDDLOS
083300970604IF  3C     ERRL00        IFEQ      '1'
083400970604     C                   SETON                                        81
083500970604     C                   MOVEL     MSG(10)       DSMSMS
083600130903    3c                   endif
083700130903    2c                   endif
083800130903    1c                   endif
083900130902     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
084000130903IF  1C     ERRL00        IFEQ      '0'
084100130903    2c                   if        vidlos>*zeros
084200130902     c                   movel     vidlos        w0030             3 0
084300130903     c                   else
084400130903     c                   clear                   w0030
084500130903    2c                   endif
084600130903     c
084700130903    2c                   if        w0030 =cpclos
084800130902     c                   clear                   caplos
084900130902     c                   clear                   loclos
085000130903     c                   clear                   f07los
085100130903    2c                   endif
085200130902     c
085300130903    2c                   if        w0030 <>cpclos and vidlos<>caplos
085400130902     c
085500130902     C                   Z-ADD     D00VER        cosVER                         *CHIAVE
085600130902     C                   MOVEL     VIDNAR        cosNAR
085700130902     C                   MOVEL     VIDCAP        cosCAP
085800130902     C                   clear                   cosloc
085900130902     C                   MOVEL     cpclos        coslna
086000130902     c     kcos          chain     azcos01l
086100130903    3c                   if        %found(azcos01l)
086200130902     c                   movel     Msg(20)       DSmsms
086300130902     c                   eval      *in81  = *on
086400130902     c                   eval      ErrL00 = *on
086500130903     c                   eval      f07los='S'
086600130903e   3c                   endif
086700130902c
086800130902     c                   eval      caplos=vidlos
086900970604E   2C                   ENDIF
087000970604E   1C                   ENDIF
087100970604     C*--------------------
087200970912     C* ZONA DI CONSEGNA SOTTO
087300970604     C*--------------------
087400970604IF  1C     ERRL00        IFEQ      '0'
087500971126IF  2C     VIDLOS        IFNE      *ZEROS
087600970604     C                   MOVEL     VIDLOS        WLIN
087700970604     C                   Z-ADD     VIDZOS        WZNC
087800970604     C                   EXSR      CHKZNC
087900970604IF  3C     ERRL00        IFEQ      '1'
088000970604     C                   SETON                                        82
088100970604     C                   MOVEL     MSG(11)       DSMSMS
088200970604E   3C                   ENDIF
088300060206if  3c                   if        §03nuc = 'S'
088400060206     c                   movel     Msg(17)       DSmsms
088500060206     c                   eval      *in82  = *on
088600060206     c                   eval      ErrL00 = *on
088700060206e   3c                   endif
088800970604E   2C                   ENDIF
088900970604E   1C                   ENDIF
089000970604     C*--------------------
089100970912     C* PESO/VOLUME SOTTO
089200970604     C*--------------------
089300970604IF  1C     ERRL00        IFEQ      '0'
089400970604IF  2C     VIDLKS        IFGT      *ZEROS
089500970604     C     VIDLMS        ORGT      *ZEROS
089600970604IF  3C     VIDLOS        IFEQ      *BLANKS
089700970604     C     VIDLOS        OREQ      *ZEROS
089800970604     C                   SETON                                        81
089900970604     C                   MOVEL     MSG(18)       DSMSMS
090000970604     C                   MOVEL     '1'           ERRL00
090100970604E   3C                   ENDIF
090200070103E   2C                   ENDIF
090300970604IF  3C     VIDLOS        IFNE      *BLANKS
090400970604     C     VIDLOS        ANDNE     *ZEROS
090500070103IF  4C                   IF        VIDLKS = *ZEROS OR
090600070103     C                             VIDLMS = *ZEROS
090700970604     C                   SETON                                        8384
090800970604     C                   MOVEL     MSG(19)       DSMSMS
090900970604     C                   MOVEL     '1'           ERRL00
091000070103E   4C                   ENDIF
091100070103E   3C                   ENDIF
091200970604E   1C                   ENDIF
091300970604     C*--------------------
091400970912     C* LINEA ARRIVO OLTRE
091500970604     C*--------------------
091600970604IF  1C     ERRL00        IFEQ      '0'
091700970604     C                   MOVEL     *BLANKS       VDDLOL
091800970604IF  2C     VIDLOL        IFNE      *ZEROS
091900970604     C     VIDLOL        ANDNE     *BLANKS
092000970604     C                   MOVEL     VIDLOL        WLIN
092100970604     C                   MOVEL     *BLANKS       WDLIN
092200970604     C                   EXSR      CHKLIN
092300970604     C                   MOVEL     WLIN          VIDLOL
092400970604     C                   MOVEL     WDLIN         VDDLOL
092500970604IF  3C     ERRL00        IFEQ      '1'
092600970604     C                   SETON                                        67
092700970604     C                   MOVEL     MSG(10)       DSMSMS
092800970604E   3C                   ENDIF
092900970604E   2C                   ENDIF
093000970604E   1C                   ENDIF
093100130903     c
093200130903     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
093300130903IF  1C     ERRL00        IFEQ      '0'
093400130903    2c                   if        vidlol>*zeros
093500130903     c                   movel     vidlol        w0030             3 0
093600130903     c                   else
093700130903     c                   clear                   w0030
093800130903    2c                   endif
093900130903     c
094000130903    2c                   if        w0030 =cpclol
094100130903     c                   clear                   caplol
094200130903     c                   clear                   loclol
094300130903     c                   clear                   f07lol
094400130903    2c                   endif
094500130903     c
094600130903    2c                   if        w0030 <>cpclol and vidlol<>caplol
094700130903     c
094800130903     C                   Z-ADD     D00VER        cosVER                         *CHIAVE
094900130903     C                   MOVEL     VIDNAR        cosNAR
095000130903     C                   MOVEL     VIDCAP        cosCAP
095100130903     C                   clear                   cosloc
095200130903     C                   MOVEL     cpclol        coslna
095300130903     c     kcos          chain     azcos01l
095400130903    3c                   if        %found(azcos01l)
095500130903     c                   movel     Msg(20)       DSmsms
095600130903     c                   eval      *in67  = *on
095700130903     c                   eval      ErrL00 = *on
095800130903     c                   eval      f07lol='S'
095900130903e   3c                   endif
096000130903c
096100130903     c                   eval      caplol=vidlol
096200130903E   2C                   ENDIF
096300130903E   1C                   ENDIF
096400970604     C*--------------------
096500970912     C* ZONA DI CONSEGNA OLTRE
096600970604     C*--------------------
096700970604IF  1C     ERRL00        IFEQ      '0'
096800971126IF  2C     VIDLOL        IFNE      *ZEROS
096900970604     C                   MOVEL     VIDLOL        WLIN
097000970604     C                   Z-ADD     VIDZOL        WZNC
097100970604     C                   EXSR      CHKZNC
097200970604IF  3C     ERRL00        IFEQ      '1'
097300970604     C                   SETON                                        68
097400970604     C                   MOVEL     MSG(11)       DSMSMS
097500970604E   3C                   ENDIF
097600060206if  3c                   if        §03nuc = 'S'
097700060206     c                   movel     Msg(17)       DSmsms
097800060206     c                   eval      *in68  = *on
097900060206     c                   eval      ErrL00 = *on
098000060206e   3c                   endif
098100970604E   2C                   ENDIF
098200970604E   1C                   ENDIF
098300970604     C*--------------------
098400970912     C* PESO/VOLUME OLTRE
098500970604     C*--------------------
098600970604IF  1C     ERRL00        IFEQ      '0'
098700970604IF  2C     VIDLKG        IFGT      *ZEROS
098800970604     C     VIDLMC        ORGT      *ZEROS
098900970604IF  3C     VIDLOL        IFEQ      *BLANKS
099000970604     C     VIDLOL        OREQ      *ZEROS
099100970604     C                   SETON                                        67
099200970604     C                   MOVEL     MSG(18)       DSMSMS
099300970604     C                   MOVEL     '1'           ERRL00
099400970604E   3C                   ENDIF
099500070103E   2C                   ENDIF
099600070103IF  3C     VIDLOL        IFNE      *BLANKS
099700070103     C     VIDLOL        ANDNE     *ZEROS
099800070103IF  4C                   IF        VIDLKG = *ZEROS OR
099900070103     C                             VIDLMC = *ZEROS
100000970604     C                   SETON                                        7172
100100970604     C                   MOVEL     MSG(19)       DSMSMS
100200970604     C                   MOVEL     '1'           ERRL00
100300070103E   4C                   ENDIF
100400070103E   3C                   ENDIF
100500970604E   1C                   ENDIF
100600970604     C*--------------------
100700970912     C* PESO OLTRE + PESO SOTTO
100800970604     C*--------------------
100900970912     C* OLTRE > SOTTO
101000970604IF  1C     ERRL00        IFEQ      '0'
101100970604IF  2C     VIDLKG        IFGT      *ZEROS
101200970604     C     VIDLKS        ANDGT     *ZEROS
101300970604IF  3C     VIDLKG        IFLE      VIDLKS
101400970604     C                   SETON                                        7183
101500970604     C                   MOVEL     MSG(21)       DSMSMS
101600970604     C                   MOVEL     '1'           ERRL00
101700970604E   3C                   ENDIF
101800970604E   2C                   ENDIF
101900970604E   1C                   ENDIF
102000970604     C*--------------------
102100970912     C* VOLUME OLTRE + VOLUME SOTTO
102200970604     C*--------------------
102300970912     C* OLTRE > SOTTO
102400970604IF  1C     ERRL00        IFEQ      '0'
102500970604IF  2C     VIDLMC        IFGT      *ZEROS
102600970604     C     VIDLMS        ANDGT     *ZEROS
102700970604IF  3C     VIDLMC        IFLE      VIDLMS
102800970604     C                   SETON                                        7284
102900970604     C                   MOVEL     MSG(21)       DSMSMS
103000970604     C                   MOVEL     '1'           ERRL00
103100970604E   3C                   ENDIF
103200970604E   2C                   ENDIF
103300970604E   1C                   ENDIF
103400971118     C*--------------------
103500971118     C* LINEA ARRIVO OLTRE 2
103600971118     C*--------------------
103700971118IF  1C     ERRL00        IFEQ      '0'
103800971118     C                   MOVEL     *BLANKS       VDDLO2
103900971118IF  2C     VIDLO2        IFNE      *ZEROS
104000971118     C     VIDLO2        ANDNE     *BLANKS
104100971118     C                   MOVEL     VIDLO2        WLIN
104200971118     C                   MOVEL     *BLANKS       WDLIN
104300971118     C                   EXSR      CHKLIN
104400971118     C                   MOVEL     WLIN          VIDLO2
104500971118     C                   MOVEL     WDLIN         VDDLO2
104600971118IF  3C     ERRL00        IFEQ      '1'
104700971118     C                   SETON                                        86
104800971118     C                   MOVEL     MSG(10)       DSMSMS
104900971118E   3C                   ENDIF
105000971118E   2C                   ENDIF
105100971118E   1C                   ENDIF
105200971118     C*--------------------
105300971118     C* ZONA DI CONSEGNA OLTRE 2
105400971118     C*--------------------
105500971118IF  1C     ERRL00        IFEQ      '0'
105600971126IF  2C     VIDLO2        IFNE      *ZEROS
105700971118     C                   MOVEL     VIDLO2        WLIN
105800971118     C                   Z-ADD     VIDZO2        WZNC
105900971118     C                   EXSR      CHKZNC
106000971118IF  3C     ERRL00        IFEQ      '1'
106100971118     C                   SETON                                        87
106200971118     C                   MOVEL     MSG(11)       DSMSMS
106300971118E   3C                   ENDIF
106400060206if  3c                   if        §03nuc = 'S'
106500060206     c                   movel     Msg(17)       DSmsms
106600060206     c                   eval      *in87  = *on
106700060206     c                   eval      ErrL00 = *on
106800060206e   3c                   endif
106900971118E   2C                   ENDIF
107000971118E   1C                   ENDIF
107100970526     C*--------------------
107200970526     C* LINEA ARRIVO FERMO DEPOSITO -OUTPUT-
107300970526     C*--------------------
107400970526IF  1C     ERRL00        IFEQ      '0'
107500971219     C                   MOVEL     VIDLNA        VIDLFD
107600970526     C                   MOVEL     *BLANKS       VDDLFD
107700970526IF  2C     VIDLFD        IFGT      *ZEROS
107800970526     C                   MOVEL     VIDLFD        WLIN
107900970526     C                   MOVEL     *BLANKS       WDLIN
108000970526     C                   EXSR      CHKLIN
108100970526     C                   MOVEL     WLIN          VIDLFD
108200970526     C                   MOVEL     WDLIN         VDDLFD
108300970526E   2C                   ENDIF
108400970526E   1C                   ENDIF
108500970526     C*--------------------
108600970526     C* ZONA DI CONSEGNA FERMO DEPOSITO
108700970526     C*--------------------
108800970526IF  1C     ERRL00        IFEQ      '0'
108900971126IF  2C     VIDLFD        IFNE      *ZEROS
109000970526     C                   MOVEL     VIDLFD        WLIN
109100970526     C                   Z-ADD     VIDZFD        WZNC
109200970526     C                   EXSR      CHKZNC
109300970526IF  3C     ERRL00        IFEQ      '1'
109400970526     C                   SETON                                        66
109500970526     C                   MOVEL     MSG(11)       DSMSMS
109600970526E   3C                   ENDIF
109700060206if  3c                   if        §03nuc = 'S'
109800060206     c                   movel     Msg(17)       DSmsms
109900060206     c                   eval      *in66  = *on
110000060206     c                   eval      ErrL00 = *on
110100060206e   3c                   endif
110200970526E   2C                   ENDIF
110300970526E   1C                   ENDIF
110400970526     C*--------------------
110500970526     C* PROVINCIA
110600970526     C*--------------------
110700970526IF  1C     ERRL00        IFEQ      '0'
110800970526     C                   MOVEL     *BLANKS       VDDPRV
110900970526IF  2C     VIDPRV        IFEQ      *BLANKS                                      *PRV NON IMMESSA
111000970526IF  3C     DEPCPR        IFEQ      'S'                                          *PRV DA CONTROLLARE
111100970526     C                   MOVEL     '1'           ERRL00
111200970526     C                   SETON                                        64
111300970526     C                   MOVEL     MSG(9)        DSMSMS
111400970526E   3C                   ENDIF
111500970526X   2C                   ELSE                                                   *PRV IMMESSA
111600970526     C                   MOVEL     VIDPRV        WPRV
111700970526     C                   MOVEL     *BLANKS       WDPRV
111800970526     C                   EXSR      CHKPRV
111900970526     C                   MOVEL     WPRV          VIDPRV
112000970526     C                   MOVEL     WDPRV         VDDPRV
112100970526IF  3C     ERRL00        IFEQ      '1'
112200970526     C                   SETON                                        64
112300970526     C                   MOVEL     MSG(9)        DSMSMS
112400970526E   3C                   ENDIF
112500970526E   2C                   ENDIF
112600970526E   1C                   ENDIF
112700970526     C*--------------------
112800970526     C* PROVINCIA ALTERNATIVA
112900970526     C*--------------------
113000970526IF  1C     ERRL00        IFEQ      '0'
113100970526     C                   MOVEL     *BLANKS       VDDPRA
113200970526IF  2C     VIDPRA        IFNE      *BLANKS
113300970526     C                   MOVEL     VIDPRA        WPRV
113400970526     C                   MOVEL     *BLANKS       WDPRV
113500970526     C                   EXSR      CHKPRV
113600970526     C                   MOVEL     WPRV          VIDPRA
113700970526     C                   MOVEL     WDPRV         VDDPRA
113800970526IF  3C     ERRL00        IFEQ      '1'
113900970526     C                   SETON                                        65
114000970526     C                   MOVEL     MSG(9)        DSMSMS
114100970526E   3C                   ENDIF
114200970526E   2C                   ENDIF
114300970526E   1C                   ENDIF
114400970526     C*--------------------
114500970526     C* CODICE TASSAZIONE
114600970526     C*--------------------
114700970526IF  1C     ERRL00        IFEQ      '0'
114800970526     C                   MOVEL     *BLANKS       VDDCTS
114900970526     C                   MOVEL     VIDCTS        WCTS
115000970526     C                   MOVEL     *BLANKS       WDCTS
115100970526     C                   EXSR      CHKCTS
115200970526     C                   MOVEL     WCTS          VIDCTS
115300970526     C                   MOVEL     WDCTS         VDDCTS
115400970526IF  2C     ERRL00        IFEQ      '1'
115500970526     C                   SETON                                        58
115600970526     C                   MOVEL     MSG(14)       DSMSMS
115700970526E   2C                   ENDIF
115800970526E   1C                   ENDIF
115900970526     C*--------------------
116000970526     C* CODICE TASSAZIONE ITALIA PER ESTERO
116100970526     C*--------------------
116200970526IF  1C     ERRL00        IFEQ      '0'
116300970526     C                   MOVEL     *BLANKS       VDDCTE
116400970526IF  2C     VIDNAR        IFEQ      *BLANKS                                      *NAZIONE ITALIA
116500970526     C                   MOVEL     VIDCTE        WA1D              1
116600970526     C                   MOVE      VIDCTE        WA1S              1
116700970526IF  3C     WA1D          IFNE      'I'
116800970526     C     WA1S          OREQ      *BLANKS
116900970526     C                   MOVEL     '1'           ERRL00
117000970526     C                   SETON                                        59
117100970526     C                   MOVEL     MSG(15)       DSMSMS
117200970526E   3C                   ENDIF
117300970526X   2C                   ELSE                                                   *NAZIONE ESTERO
117400970526IF  3C     VIDCTE        IFNE      *BLANKS
117500970526     C                   MOVEL     '1'           ERRL00
117600970526     C                   SETON                                        59
117700970526     C                   MOVEL     MSG(16)       DSMSMS
117800970526E   3C                   ENDIF
117900970526E   2C                   ENDIF
118000970526E   1C                   ENDIF
118100970526     C*--------------------
118200970526     C* CAP FITTIZIO
118300970526     C*--------------------
118400970526IF  1C     ERRL00        IFEQ      '0'
118500970526IF  2C     VIDFIT        IFEQ      'S'
118600970526     C     VIDFIT        OREQ      'N'
118700970526X   2C                   ELSE
118800970526     C                   MOVEL     '1'           ERRL00
118900970526     C                   SETON                                        73
119000970526     C                   MOVEL     MSG(27)       DSMSMS
119100970526E   2C                   ENDIF
119200970526E   1C                   ENDIF
119300970526     C*--------------------
119400970526     C* CITTA CON VIARIO
119500970526     C*--------------------
119600970526IF  1C     ERRL00        IFEQ      '0'
119700970526IF  2C     VIDFLG        IFEQ      'S'
119800970526     C     VIDFLG        OREQ      'N'
119900970526X   2C                   ELSE
120000970526     C                   MOVEL     '1'           ERRL00
120100970526     C                   SETON                                        74
120200970526     C                   MOVEL     MSG(27)       DSMSMS
120300970526E   2C                   ENDIF
120400970526E   1C                   ENDIF
120500970526     C*--------------------
120600970526     C* INOLTRO
120700970526     C*--------------------
120800970526IF  1C     ERRL00        IFEQ      '0'
120900970526     C                   MOVEL     *BLANKS       VDDISO
121000970526     C                   MOVEL     VIDISO        WINL
121100970526     C                   MOVEL     *BLANKS       WDINL
121200970526     C                   EXSR      CHKINL
121300970526     C                   MOVEL     WINL          VIDISO
121400970526     C                   MOVEL     WDINL         VDDISO
121500970526IF  2C     ERRL00        IFEQ      '1'
121600970526     C                   SETON                                        75
121700970526     C                   MOVEL     MSG(28)       DSMSMS
121800970603X   2C                   ELSE
121900970526E   2C                   ENDIF
122000970526E   1C                   ENDIF
122100970526     C*--------------------
122200970526     C* POSSIBILE INOLTRO ISOLA
122300970526     C*--------------------
122400970526IF  1C     ERRL00        IFEQ      '0'
122500970526IF  2C     VIDPII        IFEQ      'S'
122600970526     C     VIDPII        OREQ      'N'
122700970526X   2C                   ELSE
122800970526     C                   MOVEL     '1'           ERRL00
122900970526     C                   SETON                                        76
123000970526     C                   MOVEL     MSG(27)       DSMSMS
123100970526E   2C                   ENDIF
123200970526E   1C                   ENDIF
123300970526     C*--------------------
123400970526     C* POSSIBILE INOLTRO LOCALITA' DISAGIATA
123500970526     C*--------------------
123600970526IF  1C     ERRL00        IFEQ      '0'
123700970526IF  2C     VIDPID        IFEQ      'S'
123800970526     C     VIDPID        OREQ      'N'
123900970526X   2C                   ELSE
124000970526     C                   MOVEL     '1'           ERRL00
124100970526     C                   SETON                                        77
124200970526     C                   MOVEL     MSG(27)       DSMSMS
124300970526E   2C                   ENDIF
124400970526E   1C                   ENDIF
124500970526     C*--------------------
124600970526     C* POSSIBILE INOLTRO CENTRO STORICO
124700970526     C*--------------------
124800970526IF  1C     ERRL00        IFEQ      '0'
124900970526IF  2C     VIDPIS        IFEQ      'S'
125000970526     C     VIDPIS        OREQ      'N'
125100970526X   2C                   ELSE
125200970526     C                   MOVEL     '1'           ERRL00
125300970526     C                   SETON                                        78
125400970526     C                   MOVEL     MSG(27)       DSMSMS
125500970526E   2C                   ENDIF
125600970526E   1C                   ENDIF
125700970526     C*--------------------
125800970526     C* GESTIONE DDT
125900970526     C*--------------------
126000970526IF  1C     ERRL00        IFEQ      '0'
126100970526IF  2C     VIDGDD        IFEQ      'S'
126200970526     C     VIDGDD        OREQ      'N'
126300970526X   2C                   ELSE
126400970526     C                   MOVEL     '1'           ERRL00
126500970526     C                   SETON                                        79
126600970526     C                   MOVEL     MSG(27)       DSMSMS
126700970526E   2C                   ENDIF
126800970526E   1C                   ENDIF
126900970526     C*--------------------
127000970526     C* TIPO TEMPO CONSEGNA
127100010322     C*--------------------
127200970526IF  1C     ERRL00        IFEQ      '0'
127300970526     C                   MOVEL     VIDTTC        WTTC                           *CODICE TEMPO
127400970526     C                   Z-ADD     *ZEROS        WD0I                           *TEMPI DISTRIBUZIONE
127500970526     C                   Z-ADD     *ZEROS        WD1I
127600970526     C                   Z-ADD     *ZEROS        WD0C
127700970526     C                   Z-ADD     *ZEROS        WD1C
127800970526     C                   Z-ADD     *ZEROS        WDIS
127900970526     C                   Z-ADD     *ZEROS        WDCS
128000970526     C                   Z-ADD     *ZEROS        WDLD
128100090903
128200970526     C                   Z-ADD     *ZEROS        WC0I                           *TEMPI CORRIERE
128300970526     C                   Z-ADD     *ZEROS        WC1I
128400970526     C                   Z-ADD     *ZEROS        WC0C
128500970526     C                   Z-ADD     *ZEROS        WC1C
128600970526     C                   Z-ADD     *ZEROS        WCIS
128700970526     C                   Z-ADD     *ZEROS        WCCS
128800970526     C                   Z-ADD     *ZEROS        WCLD
128900090903
129000970526     C                   Z-ADD     *ZEROS        WE0I                           *TEMPI ESPRESSO
129100970526     C                   Z-ADD     *ZEROS        WE1I
129200970526     C                   Z-ADD     *ZEROS        WE0C
129300970526     C                   Z-ADD     *ZEROS        WE1C
129400970526     C                   Z-ADD     *ZEROS        WEIS
129500970526     C                   Z-ADD     *ZEROS        WECS
129600970526     C                   Z-ADD     *ZEROS        WELD
129700090903
129800090903     C                   Z-ADD     *ZEROS        Wh0I                           *TEMPI H 1030
129900090903     C                   Z-ADD     *ZEROS        Wh1I
130000090903     C                   Z-ADD     *ZEROS        Wh0C
130100090903     C                   Z-ADD     *ZEROS        Wh1C
130200090903     C                   Z-ADD     *ZEROS        WhIS
130300090903     C                   Z-ADD     *ZEROS        WhCS
130400090903     C                   Z-ADD     *ZEROS        WhLD
130500970526     C                   EXSR      CHKTTC
130600970530     C                   MOVEL     WTTC          VIDTTC                         *CODICE TEMPO
130700970526     C                   Z-ADD     WD0I          VIDD0I                         *TEMPI DISTRIBUZIONE
130800970526     C                   Z-ADD     WD1I          VIDD1I
130900970526     C                   Z-ADD     WD0C          VIDD0C
131000970526     C                   Z-ADD     WD1C          VIDD1C
131100970526     C                   Z-ADD     WDIS          VIDDIS
131200970526     C                   Z-ADD     WDCS          VIDDCS
131300970526     C                   Z-ADD     WDLD          VIDDLD
131400090903
131500970526     C                   Z-ADD     WC0I          VIDC0I                         *TEMPI CORRIERE
131600970526     C                   Z-ADD     WC1I          VIDC1I
131700970526     C                   Z-ADD     WC0C          VIDC0C
131800970526     C                   Z-ADD     WC1C          VIDC1C
131900970526     C                   Z-ADD     WCIS          VIDCIS
132000970526     C                   Z-ADD     WCCS          VIDCCS
132100970526     C                   Z-ADD     WCLD          VIDCLD
132200090903
132300970526     C                   Z-ADD     WE0I          VIDE0I                         *TEMPI ESPRESSO
132400970526     C                   Z-ADD     WE1I          VIDE1I
132500970526     C                   Z-ADD     WE0C          VIDE0C
132600970526     C                   Z-ADD     WE1C          VIDE1C
132700970526     C                   Z-ADD     WEIS          VIDEIS
132800970526     C                   Z-ADD     WECS          VIDECS
132900970526     C                   Z-ADD     WELD          VIDELD
133000090903
133100090903     C                   Z-ADD     Wh0I          VIDh0I                         *TEMPI H1030
133200090903     C                   Z-ADD     Wh1I          VIDh1I
133300090903     C                   Z-ADD     Wh0C          VIDh0C
133400090903     C                   Z-ADD     Wh1C          VIDh1C
133500090903     C                   Z-ADD     WhIS          VIDhIS
133600090903     C                   Z-ADD     WhCS          VIDhCS
133700090903     C                   Z-ADD     WhLD          VIDhLD
133800970526IF  2C     ERRL00        IFEQ      '1'
133900970526     C                   SETON                                        80
134000970526     C                   MOVEL     MSG(30)       DSMSMS
134100970526E   2C                   ENDIF
134200970526E   1C                   ENDIF
134300010320     C*--------------------
134400010320     C* TIPO TEMPO RITIRO
134500010320     C*--------------------
134600010320IF  1C     ERRL00        IFEQ      '0'
134700010320     C                   MOVEL     VIDTTR        WTTR                           *CODICE TEMPO
134800010320     C                   Z-ADD     *ZEROS        WD0IR                          *TEMPI DISTRIBUZIONE
134900010320     C                   Z-ADD     *ZEROS        WD1IR
135000010320     C                   Z-ADD     *ZEROS        WD0CR
135100010320     C                   Z-ADD     *ZEROS        WD1CR
135200010320     C                   Z-ADD     *ZEROS        WDISR
135300010320     C                   Z-ADD     *ZEROS        WDCSR
135400010320     C                   Z-ADD     *ZEROS        WDLDR
135500010320     C                   Z-ADD     *ZEROS        WC0IR                          *TEMPI CORRIERE
135600010320     C                   Z-ADD     *ZEROS        WC1IR
135700010320     C                   Z-ADD     *ZEROS        WC0CR
135800010320     C                   Z-ADD     *ZEROS        WC1CR
135900010320     C                   Z-ADD     *ZEROS        WCISR
136000010320     C                   Z-ADD     *ZEROS        WCCSR
136100010320     C                   Z-ADD     *ZEROS        WCLDR
136200010320     C                   Z-ADD     *ZEROS        WE0IR                          *TEMPI ESPRESSO
136300010320     C                   Z-ADD     *ZEROS        WE1IR
136400010320     C                   Z-ADD     *ZEROS        WE0CR
136500010320     C                   Z-ADD     *ZEROS        WE1CR
136600010320     C                   Z-ADD     *ZEROS        WEISR
136700010320     C                   Z-ADD     *ZEROS        WECSR
136800010320     C                   Z-ADD     *ZEROS        WELDR
136900010320     C                   EXSR      CHKTTR
137000010320     C                   MOVEL     WTTR          VIDTTR                         *CODICE TEMPO
137100010320     C                   Z-ADD     WD0IR         VIDD0IR                        *TEMPI DISTRIBUZIONE
137200010320     C                   Z-ADD     WD1IR         VIDD1IR
137300010320     C                   Z-ADD     WD0CR         VIDD0CR
137400010320     C                   Z-ADD     WD1CR         VIDD1CR
137500010320     C                   Z-ADD     WDISR         VIDDISR
137600010320     C                   Z-ADD     WDCSR         VIDDCSR
137700010320     C                   Z-ADD     WDLDR         VIDDLDR
137800010320     C                   Z-ADD     WC0IR         VIDC0IR                        *TEMPI CORRIERE
137900010320     C                   Z-ADD     WC1IR         VIDC1IR
138000010320     C                   Z-ADD     WC0CR         VIDC0CR
138100010320     C                   Z-ADD     WC1CR         VIDC1CR
138200010320     C                   Z-ADD     WCISR         VIDCISR
138300010320     C                   Z-ADD     WCCSR         VIDCCSR
138400010320     C                   Z-ADD     WCLDR         VIDCLDR
138500010320     C                   Z-ADD     WE0IR         VIDE0IR                        *TEMPI ESPRESSO
138600010320     C                   Z-ADD     WE1IR         VIDE1IR
138700010320     C                   Z-ADD     WE0CR         VIDE0CR
138800010320     C                   Z-ADD     WE1CR         VIDE1CR
138900010320     C                   Z-ADD     WEISR         VIDEISR
139000010320     C                   Z-ADD     WECSR         VIDECSR
139100010320     C                   Z-ADD     WELDR         VIDELDR
139200010320IF  2C     ERRL00        IFEQ      '1'
139300010320     C                   SETON                                        44
139400010320     C                   MOVEL     MSG(24)       DSMSMS
139500010320E   2C                   ENDIF
139600010320E   1C                   ENDIF
139700970917     C*--------------------
139800970917     C* OBSOLETO
139900970917     C*--------------------
140000970917IF  1C     ERRL00        IFEQ      '0'
140100970917IF  2C     VIDFI1        IFEQ      'O'
140200970917     C     VIDFI1        OREQ      *BLANKS
140300970917X   2C                   ELSE
140400970917     C                   MOVEL     '1'           ERRL00
140500970917     C                   SETON                                        85
140600970917     C                   MOVEL     MSG(29)       DSMSMS
140700970917E   2C                   ENDIF
140800970917E   1C                   ENDIF
140900970917     C*
141000970917     C* INSERIMENTO -> NON POSSO INSERIRE CAP OBSOLETI
141100970917IF  1C     ERRL00        IFEQ      '0'
141200970917IF  2C     DEPOP1        IFEQ      'F06'
141300970917IF  3C     VIDFI1        IFEQ      'O'
141400970917     C                   SETON                                        85
141500970917     C                   MOVEL     MSG(26)       DSMSMS
141600970917     C                   MOVEL     '1'           ERRL00
141700970917E   3C                   ENDIF
141800970917E   2C                   ENDIF
141900970917E   1C                   ENDIF
142000970917     C*
142100970917     C* MODIFICA -> SE CAP OBSOLETO LO DEVONO ESSERE ANCHE TUTTE LE SUE LOCALITA'
142200970917IF  1C     ERRL00        IFEQ      '0'
142300970917IF  2C     DEPOP1        IFEQ      'O02'
142400970917IF  3C     VIDFI1        IFEQ      'O'
142500970917     C                   MOVEL     'S'           $LOBS
142600970917     C                   Z-ADD     D00VER        KCLVER
142700970917     C                   MOVEL     VIDNAR        KCLNAR
142800970917     C                   MOVEL     VIDCAP        KCLCAP
142900970917     C     KEYCPL        SETLL     AZCPL01L
143000970917     C     KEYCPL        READE(N)  AZCPL01L                               99
143100970917DO  4C     *IN99         DOWEQ     *OFF
143200970917IF  5C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
143300970917IF  6C     CPLFI1        IFNE      'O '                                         *NO OBSOLETO
143400970917     C                   MOVEL     'N'           $LOBS
143500970917     C                   SETON                                        85
143600970917     C                   MOVEL     MSG(25)       DSMSMS
143700970917     C                   MOVEL     '1'           ERRL00
143800970917     C                   LEAVE
143900970917E   6C                   ENDIF
144000970917E   5C                   ENDIF
144100970917     C     KEYCPL        READE(N)  AZCPL01L                               99
144200970917E   4C                   ENDDO                                                  *FINE LOCALITA'
144300970917E   3C                   ENDIF
144400970917E   2C                   ENDIF
144500970917E   1C                   ENDIF
144600060927     C***
144700970526     C                   ENDSR
144800970526     C*------------------------------------------------------------------------*
144900970526     C* CHKNAR - CONTROLLA LA NAZIONE
145000970526     C*------------------------------------------------------------------------*
145100970526     C     CHKNAR        BEGSR
145200970526     C*
145300970526     C     *LIKE         DEFINE    VIDNAR        WNAR
145400970526     C     *LIKE         DEFINE    VDDNAR        WDNAR
145500970526     C     *LIKE         DEFINE    §15CPR        WCPR
145600970526     C*
145700970526     C* RICHIESTO ELENCO
145800970526     C                   SETOFF                                           28
145900970526     C     '?'           SCAN      WNAR                                   28
146000970526IF  1C     *IN28         IFEQ      *ON
146100020906     C                   Z-ADD     1             §KUT              1 0
146200970526     C                   MOVEL     '15'          §COD              2
146300970526     C                   MOVEL     *BLANKS       §KEY              8
146400970526     C                   MOVEL     *BLANKS       §DES             30
146500970526     C                   CALL      'X§TABER'
146600970526     C                   PARM                    §KUT
146700970526     C                   PARM                    §COD
146800970526     C                   PARM                    §KEY
146900970526     C                   PARM                    §DES
147000970526     C                   MOVEL     §KEY          WNAR
147100970526     C                   MOVEL     §DES          WDNAR
147200970526E   1C                   ENDIF
147300970526     C*
147400970526     C* CONTROLLA ESISTENZA CODICE
147500020906     C                   MOVEL     1             KTBKUT
147600970526     C                   MOVEL     '15'          KTBCOD
147700970526     C                   MOVEL(P)  WNAR          KTBKEY
147800970526     C     KEYTAB        CHAIN     TABEL00F                           99
147900970526IF  1C     *IN99         IFEQ      *OFF
148000970526     C     TBLFLG        ANDEQ     *BLANKS
148100970526     C                   MOVEL     TBLUNI        DS15
148200970526     C                   MOVEL     §15DES        WDNAR
148300970526     C                   MOVEL     §15CPR        WCPR
148400970526     C*
148500970526     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
148600970526IF  2C     VIDNAR        IFEQ      'I  '
148700970526     C                   MOVEL     '1'           ERRL00
148800970526     C                   MOVEL     *BLANKS       WDNAR
148900970526E   2C                   ENDIF
149000970526X   1C                   ELSE
149100970526     C                   MOVEL     '1'           ERRL00
149200970526E   1C                   ENDIF
149300970526     C*
149400970526     C                   ENDSR
149500970526     C*------------------------------------------------------------------------*
149600970526     C* CHKCAP - CONTROLLA IL CAP
149700970526     C*------------------------------------------------------------------------*
149800970526     C     CHKCAP        BEGSR
149900970526     C*
150000970526     C     *LIKE         DEFINE    VIDCAP        WCAP
150100970526     C*
150200970626     C* CONTROLLA IL CAP
150300970626     C                   CLEAR                   DSSI95
150400970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
150500970626     C                   MOVEL     '1'           I95TCN                         *CONTROLLO FORMALE
150600970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
150700970626     C                   MOVEL     WCAP          I95CAP                         *CAP
150800970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
150900970626     C                   CALL      'TISI95R'
151000970626     C                   PARM                    DSSI95
151100970626IF  1C     O95ERR        IFNE      *BLANKS
151200970703     C     VIDFIT        ANDNE     'S'
151300970526     C                   MOVEL     '1'           ERRL00
151400970626     C                   MOVEL     O95MSG        DSMSMS
151500970526E   1C                   ENDIF
151600970526     C*
151700970526     C                   ENDSR
151800970526     C*------------------------------------------------------------------------*
151900970526     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
152000970526     C*------------------------------------------------------------------------*
152100970526     C     CHKLIN        BEGSR
152200970526     C*
152300970526     C     *LIKE         DEFINE    VIDLNA        WLIN
152400970526     C     *LIKE         DEFINE    VDDLNA        WDLIN
152500970526     C*
152600970526     C* RICHIESTO ELENCO
152700970526     C                   SETOFF                                           28
152800970526     C     '?'           SCAN      WLIN                                   28
152900970526IF  1C     *IN28         IFEQ      *ON
153000970526     C                   MOVEL     *BLANKS       §COD1             3
153100970526     C                   MOVEL     *BLANKS       §TIP              1
153200970526     C                   MOVEL     *BLANKS       §DES1            25
153300971203     C                   MOVEL     *BLANKS       §PRA              3
153400971203     C                   CALL      'TNSD24R'
153500970526     C                   PARM                    §COD1
153600970526     C                   PARM                    §TIP
153700970526     C                   PARM                    §DES1
153800971203     C                   PARM                    §PRA
153900970526     C                   MOVEL     §COD1         WLIN
154000970526     C                   MOVEL     §DES1         WDLIN
154100970526E   1C                   ENDIF
154200970526     C*
154300970526     C* SOLO CARATTERI NUMERICI
154400970526     C                   SETOFF                                       28
154500970526     C                   TESTN                   WLIN                 28
154600970526IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
154700970526     C                   MOVEL     '1'           ERRL00
154800970526X   1C                   ELSE                                                   *E'NUMERO DA TESTN
154900970526     C                   MOVE      WLIN          W001              1
155000970526IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
155100970526     C                   MOVEL     '1'           ERRL00
155200970526X   2C                   ELSE                                                   *NUMERO
155300970526     C*
155400970526     C* CONTROLLA ESISTENZA CODICE
155500970526     C                   MOVEL     WLIN          KORFIL
155600970526     C     KEYORG        CHAIN     AZORG01L                           99
155700970526IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
155800970526     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
155900970527     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
156000970526     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
156100970526X   3C                   ELSE
156200970526     C                   MOVEL     '1'           ERRL00
156300970526E   3C                   ENDIF
156400970526E   2C                   ENDIF
156500970526E   1C                   ENDIF
156600970526     C*
156700970526     C                   ENDSR
156800970526     C*------------------------------------------------------------------------*
156900970526     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
157000970526     C*------------------------------------------------------------------------*
157100970526     C     CHKZNC        BEGSR
157200970526     C*
157300970526     C     *LIKE         DEFINE    VIDZNC        WZNC
157400970526     C*
157500970526     C* CONTROLLA ESISTENZA CODICE
157600020906     C                   Z-ADD     1             KTBKUT
157700970526     C                   MOVEL     '03'          KTBCOD
157800970526     C                   MOVEL     WLIN          WA5               5
157900970526     C                   MOVE      WZNC          WA5
158000970526     C                   MOVEL(P)  WA5           KTBKEY
158100970526     C     KEYTAB        CHAIN     TABEL00F                           99
158200970526IF  1C     *IN99         IFEQ      *OFF
158300970526     C     TBLFLG        ANDEQ     *BLANKS
158400970526     C                   MOVEL     TBLUNI        DS03
158500970526X   1C                   ELSE
158600970526     C                   MOVEL     '1'           ERRL00
158700970526E   1C                   ENDIF
158800970526     C*
158900970526     C                   ENDSR
159000970526     C*------------------------------------------------------------------------*
159100970526     C* CHKPRV - CONTROLLA LA PROVINCIA
159200970526     C*------------------------------------------------------------------------*
159300970526     C     CHKPRV        BEGSR
159400970526     C*
159500970526     C     *LIKE         DEFINE    VIDPRV        WPRV
159600970526     C     *LIKE         DEFINE    VDDPRV        WDPRV
159700970526     C*
159800970526     C* RICHIESTO ELENCO
159900970526     C                   SETOFF                                           28
160000970526     C     '?'           SCAN      WPRV                                   28
160100970526IF  1C     *IN28         IFEQ      *ON
160200020906     C                   Z-ADD     1             §KUT              1 0
160300970526     C                   MOVEL     'PR'          §COD              2
160400970526     C                   MOVEL     *BLANKS       §KEY              8
160500970526     C                   MOVEL     *BLANKS       §DES             30
160600970526     C                   CALL      'X§TABER'
160700970526     C                   PARM                    §KUT
160800970526     C                   PARM                    §COD
160900970526     C                   PARM                    §KEY
161000970526     C                   PARM                    §DES
161100970526     C                   MOVEL     §KEY          WPRV
161200970526     C                   MOVEL     §DES          WDPRV
161300970526E   1C                   ENDIF
161400970526     C*
161500970526     C* CONTROLLA ESISTENZA CODICE
161600020906     C                   MOVEL     1             KTBKUT
161700970526     C                   MOVEL     'PR'          KTBCOD
161800970526     C                   MOVEL(P)  WPRV          KTBKEY
161900970526     C     KEYTAB        CHAIN     TABEL00F                           99
162000970526IF  1C     *IN99         IFEQ      *OFF
162100970526     C     TBLFLG        ANDEQ     *BLANKS
162200970526     C                   MOVEL     TBLUNI        DSPR
162300970526     C                   MOVEL     §PRDES        WDPRV
162400970526X   1C                   ELSE
162500970526     C                   MOVEL     '1'           ERRL00
162600970526E   1C                   ENDIF
162700970526     C*
162800970526     C                   ENDSR
162900970526     C*------------------------------------------------------------------------*
163000970526     C* CHKCTS - CONTROLLA IL CODICE TASSAZIONE
163100970526     C*------------------------------------------------------------------------*
163200970526     C     CHKCTS        BEGSR
163300970526     C*
163400970526     C     *LIKE         DEFINE    VIDCTS        WCTS
163500970526     C     *LIKE         DEFINE    VDDCTS        WDCTS
163600970526     C*
163700970526     C* RICHIESTO ELENCO
163800970526     C                   SETOFF                                           28
163900970526     C     '?'           SCAN      WCTS                                   28
164000970526IF  1C     *IN28         IFEQ      *ON
164100120712     c                   clear                   TRTB09DS
164200120712     c                   eval      TB09fun = '1'
164300120712     c                   eval      kpjbu = TRTB09DS
164400120712     c                   call      'TRTB09R'
164500120712     c                   parm                    kpjba
164600120712     c                   eval      TRTB09DS = kpjbu
164700120712     c                   eval      wcts  = TB09cod
164800120712     c                   eval      wdcts = TB09des
164900970526E   1C                   ENDIF
165000970526     C*
165100970526     C* CONTROLLA ESISTENZA CODICE
165200020906     C                   MOVEL     1             KTBKUT
165300970526     C                   MOVEL     'CT'          KTBCOD
165400970526     C                   MOVEL(P)  WCTS          KTBKEY
165500970526     C     KEYTAB        CHAIN     TABEL00F                           99
165600970526IF  1C     *IN99         IFEQ      *OFF
165700970526     C     TBLFLG        ANDEQ     *BLANKS
165800970526     C                   MOVEL     TBLUNI        DSCT
165900970526     C                   MOVEL     §CTDES        WDCTS
166000970526X   1C                   ELSE
166100970526     C                   MOVEL     '1'           ERRL00
166200970526E   1C                   ENDIF
166300970526     C*
166400970526     C                   ENDSR
166500970526     C*------------------------------------------------------------------------*
166600970526     C* CHKINL - CONTROLLA L'INOLTRO
166700970526     C*------------------------------------------------------------------------*
166800970526     C     CHKINL        BEGSR
166900970526     C*
167000970526     C     *LIKE         DEFINE    VIDISO        WINL
167100970526     C     *LIKE         DEFINE    VDDISO        WDINL
167200970526     C*
167300970526     C* RICHIESTO ELENCO
167400970526     C                   SETOFF                                           28
167500970526     C     '?'           SCAN      WINL                                   28
167600970526IF  1C     *IN28         IFEQ      *ON
167700020906     C                   Z-ADD     1             §KUT              1 0
167800970526     C                   MOVEL     '9A'          §COD              2
167900970526     C                   MOVEL     *BLANKS       §KEY              8
168000970526     C                   MOVEL     *BLANKS       §DES             30
168100970526     C                   CALL      'X§TABER'
168200970526     C                   PARM                    §KUT
168300970526     C                   PARM                    §COD
168400970526     C                   PARM                    §KEY
168500970526     C                   PARM                    §DES
168600970526     C                   MOVEL     §KEY          WINL
168700970526     C                   MOVEL     §DES          WDINL
168800970526E   1C                   ENDIF
168900970526     C*
169000970526     C* CONTROLLA ESISTENZA CODICE
169100020906     C                   MOVEL     1             KTBKUT
169200970526     C                   MOVEL     '9A'          KTBCOD
169300970526     C                   MOVEL(P)  WINL          KTBKEY
169400970526     C     KEYTAB        CHAIN     TABEL00F                           99
169500970526IF  1C     *IN99         IFEQ      *OFF
169600970526     C     TBLFLG        ANDEQ     *BLANKS
169700970526     C                   MOVEL     TBLUNI        DS9A
169800970526     C                   MOVEL     §9ADES        WDINL
169900970526X   1C                   ELSE
170000970526     C                   MOVEL     '1'           ERRL00
170100970526E   1C                   ENDIF
170200110623     c
170300110623     c* Se non utilizzabile  nel cappario errore
170400110623     c                   if        §9auticpc='N'  and errl00='0'
170500110623     C                   MOVEL     '1'           ERRL00
170600110623E   1C                   ENDIF
170700970526     C*
170800970526     C                   ENDSR
170900970526     C*------------------------------------------------------------------------*
171000970526     C* CHKTTC - CONTROLLA IL TIPO TEMPO CONSEGNA
171100970526     C*------------------------------------------------------------------------*
171200970526     C     CHKTTC        BEGSR
171300970526     C*
171400970526     C     *LIKE         DEFINE    VIDTTC        WTTC                           *TIPO TEMPO
171500970526     C     *LIKE         DEFINE    CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
171600970526     C     *LIKE         DEFINE    CPT1IN        WD1I
171700970526     C     *LIKE         DEFINE    CPT0CN        WD0C
171800970526     C     *LIKE         DEFINE    CPT1CN        WD1C
171900970526     C     *LIKE         DEFINE    CPTTIS        WDIS
172000970526     C     *LIKE         DEFINE    CPTTCS        WDCS
172100970526     C     *LIKE         DEFINE    CPTTLD        WDLD
172200970526     C     *LIKE         DEFINE    CPT0IN        WC0I                           *TEMPI CORRIERE
172300970526     C     *LIKE         DEFINE    CPT1IN        WC1I
172400970526     C     *LIKE         DEFINE    CPT0CN        WC0C
172500970526     C     *LIKE         DEFINE    CPT1CN        WC1C
172600970526     C     *LIKE         DEFINE    CPTTIS        WCIS
172700970526     C     *LIKE         DEFINE    CPTTCS        WCCS
172800970526     C     *LIKE         DEFINE    CPTTLD        WCLD
172900090903
173000970526     C     *LIKE         DEFINE    CPT0IN        WE0I                           *TEMPI ESPRESSO
173100970526     C     *LIKE         DEFINE    CPT1IN        WE1I
173200970526     C     *LIKE         DEFINE    CPT0CN        WE0C
173300970526     C     *LIKE         DEFINE    CPT1CN        WE1C
173400970526     C     *LIKE         DEFINE    CPTTIS        WEIS
173500970526     C     *LIKE         DEFINE    CPTTCS        WECS
173600970526     C     *LIKE         DEFINE    CPTTLD        WELD
173700090903
173800090903     C     *LIKE         DEFINE    CPT0IN        Wh0I                           *TEMPI H1030
173900090903     C     *LIKE         DEFINE    CPT1IN        Wh1I
174000090903     C     *LIKE         DEFINE    CPT0CN        Wh0C
174100090903     C     *LIKE         DEFINE    CPT1CN        Wh1C
174200090903     C     *LIKE         DEFINE    CPTTIS        WhIS
174300090903     C     *LIKE         DEFINE    CPTTCS        WhCS
174400090903     C     *LIKE         DEFINE    CPTTLD        WhLD
174500970526     C*
174600970526     C* RICHIESTA ELENCO CODICI
174700970526     C                   SETOFF                                           28
174800970526     C     '?'           SCAN      WTTC                                   28
174900970526IF  1C     *IN28         IFEQ      *ON
175000970602     C                   MOVEL     *BLANK        DNAR
175100970602     C                   MOVEL     *BLANKS       DCAP
175200970530     C                   MOVEL     'T01'         WOP0
175300970530     C                   MOVEL     *BLANKS       WOP1
175400970530     C                   MOVEL     'TISI41R'     WPGM              8
175500970530     C                   EXSR      LANPGM
175600970530     C                   MOVEL     D00TTC        WTTC
175700010322E   1C                   ENDIF
175800010322     C*
175900050413     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO CONSEGNA
176000010322     C                   MOVEL     VIDTTC        DEP3A             3
176100010322     C     DEP3A         IFNE      '999'
176200970526     C*
176300970526     C* CONTROLLA ESISTENZA CODICI
176400970526     C                   Z-ADD     D00VER        KCTVER
176500970530     C                   MOVEL     WTTC          KCTTTC
176600970526     C                   MOVEL     'D'           KCTTSP
176700970526     C     KEYCPT        CHAIN     AZCPT01L                           99
176800970526IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
176900970526     C                   Z-ADD     CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
177000970526     C                   Z-ADD     CPT1IN        WD1I
177100970526     C                   Z-ADD     CPT0CN        WD0C
177200970526     C                   Z-ADD     CPT1CN        WD1C
177300970526     C                   Z-ADD     CPTTIS        WDIS
177400970526     C                   Z-ADD     CPTTCS        WDCS
177500970526     C                   Z-ADD     CPTTLD        WDLD
177600970526     C                   MOVEL     'C'           KCTTSP
177700970526     C     KEYCPT        CHAIN     AZCPT01L                           99
177800970526IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
177900970526     C                   Z-ADD     CPT0IN        WC0I                           *TEMPI CORRIERE
178000970526     C                   Z-ADD     CPT1IN        WC1I
178100970526     C                   Z-ADD     CPT0CN        WC0C
178200970526     C                   Z-ADD     CPT1CN        WC1C
178300970526     C                   Z-ADD     CPTTIS        WCIS
178400970526     C                   Z-ADD     CPTTCS        WCCS
178500970526     C                   Z-ADD     CPTTLD        WCLD
178600970526     C                   MOVEL     'E'           KCTTSP
178700970526     C     KEYCPT        CHAIN     AZCPT01L                           99
178800970526IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
178900970526     C                   Z-ADD     CPT0IN        WE0I                           *TEMPI ESPRESSO
179000970526     C                   Z-ADD     CPT1IN        WE1I
179100970526     C                   Z-ADD     CPT0CN        WE0C
179200970526     C                   Z-ADD     CPT1CN        WE1C
179300970526     C                   Z-ADD     CPTTIS        WEIS
179400970526     C                   Z-ADD     CPTTCS        WECS
179500970526     C                   Z-ADD     CPTTLD        WELD
179600090903     C                   MOVEL     'H'           KCTTSP
179700090903     C     KEYCPT        CHAIN     AZCPT01L                           99
179800090903IF  4C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
179900090903     C                   Z-ADD     CPT0IN        Wh0I                           *TEMPI H1030
180000090903     C                   Z-ADD     CPT1IN        Wh1I
180100090903     C                   Z-ADD     CPT0CN        Wh0C
180200090903     C                   Z-ADD     CPT1CN        Wh1C
180300090903     C                   Z-ADD     CPTTIS        WhIS
180400090903     C                   Z-ADD     CPTTCS        WhCS
180500090903     C                   Z-ADD     CPTTLD        WhLD
180600090903X   4C                   ELSE
180700090903     c* Se tipo servizio H non ancora decorrente (anche guardando alla
180800090903     c*    scadenza della versione in visualizzazione)non do errore e non
180900090903     c*    visualizzo nemmeno come descrizione
181000090903
181100090903     c                   if        Datasys>=dataiso
181200090903     c* Verifico anche la  scadenza ddella versione
181300090903     c     d00ver        chain     azcpr01l
181400090903     c                   if        not %found(azcpr01l) or cprdsc>=§vpotsph
181500970603     C                   MOVEL     '1'           ERRL00
181600090903     c                   endif
181700090903     c                   endif
181800090903     c
181900090903     c                   if        errl00='0'
182000090903     c                   clear                   viddtsph
182100090903E   4C                   ENDIF
182200090903
182300090903E   4C                   ENDIF
182400090903
182500090903X   3C                   ELSE
182600090903     C                   MOVEL     '1'           ERRL00
182700090903E   3C                   ENDIF
182800090903
182900970603X   2C                   ELSE
183000970603     C                   MOVEL     '1'           ERRL00
183100970526E   2C                   ENDIF
183200090903
183300970603X   1C                   ELSE
183400970603     C                   MOVEL     '1'           ERRL00
183500970526E   1C                   ENDIF
183600090903
183700010322     C                   ELSE
183800010322     C                   MOVEL     '1'           ERRL00
183900010322     C                   ENDIF
184000970526     C*
184100970526     C                   ENDSR
184200010320     C*------------------------------------------------------------------------*
184300010320     C* CHKTTR - CONTROLLA IL TIPO TEMPO RITIRO
184400010320     C*------------------------------------------------------------------------*
184500010320     C     CHKTTR        BEGSR
184600010320     C*
184700010320     C     *LIKE         DEFINE    VIDTTR        WTTR                           *TIPO TEMPO
184800010320     C     *LIKE         DEFINE    CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
184900010320     C     *LIKE         DEFINE    CPT1IN        WD1IR
185000010320     C     *LIKE         DEFINE    CPT0CN        WD0CR
185100010320     C     *LIKE         DEFINE    CPT1CN        WD1CR
185200010320     C     *LIKE         DEFINE    CPTTIS        WDISR
185300010320     C     *LIKE         DEFINE    CPTTCS        WDCSR
185400010320     C     *LIKE         DEFINE    CPTTLD        WDLDR
185500010320     C     *LIKE         DEFINE    CPT0IN        WC0IR                          *TEMPI CORRIERE
185600010320     C     *LIKE         DEFINE    CPT1IN        WC1IR
185700010320     C     *LIKE         DEFINE    CPT0CN        WC0CR
185800010320     C     *LIKE         DEFINE    CPT1CN        WC1CR
185900010320     C     *LIKE         DEFINE    CPTTIS        WCISR
186000010320     C     *LIKE         DEFINE    CPTTCS        WCCSR
186100010320     C     *LIKE         DEFINE    CPTTLD        WCLDR
186200010320     C     *LIKE         DEFINE    CPT0IN        WE0IR                          *TEMPI ESPRESSO
186300010320     C     *LIKE         DEFINE    CPT1IN        WE1IR
186400010320     C     *LIKE         DEFINE    CPT0CN        WE0CR
186500010320     C     *LIKE         DEFINE    CPT1CN        WE1CR
186600010320     C     *LIKE         DEFINE    CPTTIS        WEISR
186700010320     C     *LIKE         DEFINE    CPTTCS        WECSR
186800010320     C     *LIKE         DEFINE    CPTTLD        WELDR
186900010320     C*
187000010320     C* RICHIESTA ELENCO CODICI
187100010320     C                   SETOFF                                           28
187200010320     C     '?'           SCAN      WTTR                                   28
187300010320IF  1C     *IN28         IFEQ      *ON
187400010320     C                   MOVEL     *BLANK        DNAR
187500010320     C                   MOVEL     *BLANKS       DCAP
187600010320     C                   MOVEL     'T01'         WOP0
187700010320     C                   MOVEL     *BLANKS       WOP1
187800010323     C                   MOVEL     'TISI41R'     WPGM              8
187900010320     C                   EXSR      LANPGM
188000010320     C                   MOVEL     D00TTC        WTTR
188100010320E   1C                   ENDIF
188200010320     C*
188300010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO RITIRO
188400010322     C                   MOVEL     VIDTTR        DEP3A             3
188500010322     C     DEP3A         IFEQ      '999'
188600010320     C* CONTROLLA ESISTENZA CODICI
188700010320     C                   Z-ADD     D00VER        KCTVER
188800010320     C                   MOVEL     WTTR          KCTTTC
188900010320     C                   MOVEL     'D'           KCTTSP
189000010320     C     KEYCPT        CHAIN     AZCPT01L                           99
189100010320IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
189200010320     C                   Z-ADD     CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
189300010320     C                   Z-ADD     CPT1IN        WD1IR
189400010320     C                   Z-ADD     CPT0CN        WD0CR
189500010320     C                   Z-ADD     CPT1CN        WD1CR
189600010320     C                   Z-ADD     CPTTIS        WDISR
189700010320     C                   Z-ADD     CPTTCS        WDCSR
189800010320     C                   Z-ADD     CPTTLD        WDLDR
189900010320     C                   MOVEL     'C'           KCTTSP
190000010320     C     KEYCPT        CHAIN     AZCPT01L                           99
190100010320IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
190200010320     C                   Z-ADD     CPT0IN        WC0IR                          *TEMPI CORRIERE
190300010320     C                   Z-ADD     CPT1IN        WC1IR
190400010320     C                   Z-ADD     CPT0CN        WC0CR
190500010320     C                   Z-ADD     CPT1CN        WC1CR
190600010320     C                   Z-ADD     CPTTIS        WCISR
190700010320     C                   Z-ADD     CPTTCS        WCCSR
190800010320     C                   Z-ADD     CPTTLD        WCLDR
190900010320     C                   MOVEL     'E'           KCTTSP
191000010320     C     KEYCPT        CHAIN     AZCPT01L                           99
191100010320IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
191200010320     C                   Z-ADD     CPT0IN        WE0IR                          *TEMPI ESPRESSO
191300010320     C                   Z-ADD     CPT1IN        WE1IR
191400010320     C                   Z-ADD     CPT0CN        WE0CR
191500010320     C                   Z-ADD     CPT1CN        WE1CR
191600010320     C                   Z-ADD     CPTTIS        WEISR
191700010320     C                   Z-ADD     CPTTCS        WECSR
191800010320     C                   Z-ADD     CPTTLD        WELDR
191900010320X   3C                   ELSE
192000010320     C                   MOVEL     '1'           ERRL00
192100010320E   3C                   ENDIF
192200010320X   2C                   ELSE
192300010320     C                   MOVEL     '1'           ERRL00
192400010320E   2C                   ENDIF
192500010320X   1C                   ELSE
192600010320     C                   MOVEL     '1'           ERRL00
192700010320E   1C                   ENDIF
192800010322     C                   ELSE
192900010322     C                   MOVEL     '1'           ERRL00
193000010322     C                   ENDIF
193100010320     C*
193200010320     C                   ENDSR
193300970526     C*------------------------------------------------------------------------*
193400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
193500970526     C*------------------------------------------------------------------------*
193600970526     C     IREVID        BEGSR
193700970526     C* CAMPI
193800970526IF  1C     CPCESL        IFEQ      'S'
193900970526     C                   MOVEL     'LOC'         VDDESL
194000970526E   1C                   ENDIF
194100970526IF  1C     CPCESE        IFEQ      'S'
194200970526     C                   MOVEL     'ECC'         VDDESE
194300970526E   1C                   ENDIF
194400970526     C                   MOVEL     CPCNAR        VIDNAR
194500970526     C                   MOVEL     CPCCAP        VIDCAP
194600970526     C                   MOVEL     CPCLNA        VIDLNA
194700970526     C                   MOVEL     CPCLOL        VIDLOL
194800971118     C                   MOVEL     CPCLO2        VIDLO2
194900970604     C                   MOVEL     CPCLOS        VIDLOS
195000970526     C                   MOVEL     CPCLNA        VIDLFD
195100970526     C                   Z-ADD     CPCZNC        VIDZNC
195200970526     C                   Z-ADD     CPCZOL        VIDZOL
195300971118     C                   Z-ADD     CPCZO2        VIDZO2
195400970604     C                   Z-ADD     CPCZOS        VIDZOS
195500970526     C                   Z-ADD     CPCZFD        VIDZFD
195600970526     C                   Z-ADD     CPCLKG        VIDLKG
195700971118     C                   Z-ADD     CPCLK2        VIDLK2
195800970526     C                   Z-ADD     CPCLMC        VIDLMC
195900971118     C                   Z-ADD     CPCLM2        VIDLM2
196000970604     C                   Z-ADD     CPCLKS        VIDLKS
196100970604     C                   Z-ADD     CPCLMS        VIDLMS
196200970526     C                   MOVEL     CPCPRV        VIDPRV
196300970526     C                   MOVEL     CPCPRA        VIDPRA
196400970526     C                   MOVEL     CPCCTS        VIDCTS
196500970526     C                   MOVEL     CPCCTE        VIDCTE
196600970526     C                   MOVEL     CPCFIT        VIDFIT
196700970526     C                   MOVEL     CPCFLG        VIDFLG
196800970526     C                   MOVEL     CPCISO        VIDISO
196900970526     C                   MOVEL     CPCPII        VIDPII
197000970526     C                   MOVEL     CPCPID        VIDPID
197100970526     C                   MOVEL     CPCPIS        VIDPIS
197200970526     C                   MOVEL     CPCGDD        VIDGDD
197300970526     C                   MOVEL     CPCTTC        VIDTTC
197400970917     C                   MOVEL     CPCFI1        VIDFI1
197500010320     C                   MOVEL     *all'9'       VIDTTR
197600010320     C                   MOVE      CPCFI2        VIDTTR
197700000420     C*
197800060927     c*** 27/09/06 - EDPES: campi poste non più usati
197900970526     C*
198000970526     C                   ENDSR
198100970526     C*------------------------------------------------------------------------*
198200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
198300970526     C*------------------------------------------------------------------------*
198400970526     C     AZZVID        BEGSR
198500970526     C* CAMPI
198600970526     C                   MOVEL     *BLANKS       VDDESL
198700970526     C                   MOVEL     *BLANKS       VDDESE
198800970526     C                   MOVEL     *BLANKS       VIDNAR
198900970526     C                   MOVEL     *BLANKS       VIDCAP
199000970526     C                   MOVEL     *ZEROS        VIDLNA
199100970526     C                   MOVEL     *ZEROS        VIDLOL
199200971118     C                   MOVEL     *ZEROS        VIDLO2
199300970604     C                   MOVEL     *ZEROS        VIDLOS
199400970526     C                   MOVEL     *ZEROS        VIDLFD
199500970526     C                   Z-ADD     *ZEROS        VIDZNC
199600970526     C                   Z-ADD     *ZEROS        VIDZOL
199700971118     C                   Z-ADD     *ZEROS        VIDZO2
199800970604     C                   Z-ADD     *ZEROS        VIDZOS
199900970526     C                   Z-ADD     *ZEROS        VIDZFD
200000970526     C                   Z-ADD     *ZEROS        VIDLKG
200100971118     C                   Z-ADD     *ZEROS        VIDLK2
200200970526     C                   Z-ADD     *ZEROS        VIDLMC
200300971118     C                   Z-ADD     *ZEROS        VIDLM2
200400970604     C                   Z-ADD     *ZEROS        VIDLKS
200500970604     C                   Z-ADD     *ZEROS        VIDLMS
200600970526     C                   MOVEL     *BLANKS       VIDPRV
200700970526     C                   MOVEL     *BLANKS       VIDPRA
200800970526     C                   MOVEL     *BLANKS       VIDCTS
200900970526     C                   MOVEL     *BLANKS       VIDCTE
201000970526     C                   MOVEL     'N'           VIDFIT
201100970526     C                   MOVEL     'N'           VIDFLG
201200970526     C                   MOVEL     *BLANKS       VIDISO
201300970526     C                   MOVEL     'N'           VIDPII
201400970526     C                   MOVEL     'N'           VIDPID
201500970526     C                   MOVEL     'N'           VIDPIS
201600970526     C                   MOVEL     'N'           VIDGDD
201700970526     C                   MOVEL     *BLANKS       VIDTTC
201800970917     C                   MOVEL     *BLANKS       VIDFI1
201900970526     C* DECODIFICHE
202000970526     C                   MOVEL     *BLANKS       VDDNAR
202100970526     C                   MOVEL     *BLANKS       VDDLNA
202200970526     C                   MOVEL     *BLANKS       VDDLOL
202300971118     C                   MOVEL     *BLANKS       VDDLO2
202400970604     C                   MOVEL     *BLANKS       VDDLOS
202500970526     C                   MOVEL     *BLANKS       VDDLFD
202600970526     C                   MOVEL     *BLANKS       VDDPRV
202700970526     C                   MOVEL     *BLANKS       VDDPRA
202800970526     C                   MOVEL     *BLANKS       VDDCTS
202900970526     C                   MOVEL     *BLANKS       VDDCTE
203000970526     C                   MOVEL     *BLANKS       VDDISO
203100000426     C                   MOVEL     *BLANKS       VDPLNA
203200130902     c
203300130902     c                   clear                   caplna
203400130903     c                   clear                   loclna
203500130903     c                   clear                   caplos
203600130903     c                   clear                   loclos
203700130903     c                   clear                   caplol
203800130903     c                   clear                   loclol
203900130903     c                   clear                   F07los            1
204000130903     c                   clear                   F07lna            1
204100130903     c                   clear                   F07lol            1
204200970526     C*
204300970526     C                   ENDSR
204400970526     C*------------------------------------------------------------------------*
204500970526     C* IMPCPC - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
204600970526     C*------------------------------------------------------------------------*
204700970526     C     IMPCPC        BEGSR
204800970526     C*
204900970526     C                   MOVEL     VIDLNA        CPCLNA
205000970526     C                   MOVEL     VIDLOL        CPCLOL
205100971118     C                   MOVEL     VIDLO2        CPCLO2
205200970604     C                   MOVEL     VIDLOS        CPCLOS
205300970526     C                   Z-ADD     VIDZNC        CPCZNC
205400970526     C                   Z-ADD     VIDZOL        CPCZOL
205500971118     C                   Z-ADD     VIDZO2        CPCZO2
205600970604     C                   Z-ADD     VIDZOS        CPCZOS
205700970526     C                   Z-ADD     VIDZFD        CPCZFD
205800970526     C                   Z-ADD     VIDLKG        CPCLKG
205900971118     C                   Z-ADD     VIDLK2        CPCLK2
206000970526     C                   Z-ADD     VIDLMC        CPCLMC
206100971118     C                   Z-ADD     VIDLM2        CPCLM2
206200970604     C                   Z-ADD     VIDLKS        CPCLKS
206300970604     C                   Z-ADD     VIDLMS        CPCLMS
206400970526     C                   MOVEL     VIDPRV        CPCPRV
206500970526     C                   MOVEL     VIDPRA        CPCPRA
206600970526     C                   MOVEL     VIDCTS        CPCCTS
206700970526     C                   MOVEL     VIDCTE        CPCCTE
206800970526     C                   MOVEL     VIDFIT        CPCFIT
206900970526     C                   MOVEL     VIDFLG        CPCFLG
207000970526     C                   MOVEL     VIDISO        CPCISO
207100970526     C                   MOVEL     VIDPII        CPCPII
207200970603IF  3C     CPCISO        IFEQ      'I'
207300970603     C                   MOVEL     'S'           CPCPII
207400970603E   3C                   ENDIF
207500970603     C                   MOVEL     VIDPID        CPCPID
207600970603IF  3C     CPCISO        IFEQ      'D'
207700970603     C                   MOVEL     'S'           CPCPID
207800970603E   3C                   ENDIF
207900970603     C                   MOVEL     VIDPIS        CPCPIS
208000970603IF  3C     CPCISO        IFEQ      'S'
208100970603     C                   MOVEL     'S'           CPCPIS
208200970603E   3C                   ENDIF
208300970526     C                   MOVEL     VIDGDD        CPCGDD
208400970526     C                   MOVEL     VIDTTC        CPCTTC
208500970917     C                   MOVEL     VIDFI1        CPCFI1
208600010320     C                   MOVE(p)   VIDTTR        CPCFI2
208700970526     C*
208800970526     C                   ENDSR
208900970530     C*------------------------------------------------------------------------*
209000970530     C* LANPGM - LANCIA UN PROGRAMMA
209100970530     C*------------------------------------------------------------------------*
209200970530     C     LANPGM        BEGSR
209300970530     C*
209400970530     C* IMPOSTA LA DS DI PROCEDURA
209500970530     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
209600970530     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
209700970530     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
209800970530     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
209900970530     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
210000970530     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
210100970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
210200970602     C                   MOVEL     DCAP          D00CAP                         *CAP
210300970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
210400970530     C                   MOVEL     *BLANKS       D00TSP
210500970530     C                   Z-ADD     *ZEROS        D00TFP
210600970530     C                   MOVEL     *BLANKS       D00LOC
210700970603     C                   MOVEL     *BLANKS       D00SIN
210800970530     C                   MOVEL     *BLANKS       D00EPA
210900970530     C                   Z-ADD     *ZEROS        D00ETP
211000970530     C                   Z-ADD     *ZEROS        D00ETA
211100970530     C                   Z-ADD     *ZEROS        D00EDD
211200970530     C                   MOVEL     *BLANKS       D00SPT
211300970530     C                   Z-ADD     *ZEROS        D00SLP
211400970530     C                   Z-ADD     *ZEROS        D00SLA
211500970530     C                   MOVEL     *BLANKS       D00STS
211600970530     C                   Z-ADD     *ZEROS        D00SDD
211700970530     C*
211800970530     C                   CALL      WPGM
211900970612     C                   PARM                    KPJBA
212000970612     C                   PARM                    DSSI00
212100970530     C*
212200970530     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
212300970530IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
212400970530     C                   MOVEL     '1'           ERRL00                         *ERRORE
212500970530     C                   MOVEL     '1'           F03L00                         *FINE PGM
212600970530X   1C                   ELSE                                                   *NON PREMUTO F03
212700970530     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
212800970530E   1C                   ENDIF
212900970530     C*
213000970530     C                   ENDSR
213100970526     C*------------------------------------------------------------------------*
213200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
213300970526     C*------------------------------------------------------------------------*
213400970526     C     IMPFUN        BEGSR
213500970603     C*
213600970603     C* SE INTERROGAZIONE/VISUALIZZAZIONE TOGLE LA GESTIONE ALLE ZONE DI CONSEGNA
213700970603IF  1C     DEPOP0        IFEQ      'C05'
213800970603     C     DEPOP1        OREQ      'O05'
213900970603     C                   MOVEL     FUD(14)       FUDRIG                         *AGGIORNAMENTO
214000970603     C                   MOVEL     'N'           FUDATT
214100970603     C                   MOVEL     FUDRIG        FUD(14)
214200970603E   1C                   ENDIF
214300970526     C*
214400970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
214500970526IF  1C     DEPOP1        IFEQ      'O05'
214600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
214700970526     C                   MOVEL     'N'           FUDATT
214800970526     C                   MOVEL     FUDRIG        FUD(6)
214900970526E   1C                   ENDIF
215000970526     C*
215100970526     C* SE INSERIMENTO TOGLIE LE FUNZIONI DI ACCESSO AGLI ALTRI ARCHIVI
215200970526IF  1C     DEPOP1        IFEQ      'F06'
215300970526     C                   MOVEL     FUD(8)        FUDRIG                         *LOCALITA'
215400970526     C                   MOVEL     'N'           FUDATT
215500970526     C                   MOVEL     FUDRIG        FUD(8)
215600970527     C                   MOVEL     FUD(9)        FUDRIG                         *ECCEZIONI
215700970526     C                   MOVEL     'N'           FUDATT
215800970526     C                   MOVEL     FUDRIG        FUD(9)
215900970527     C                   MOVEL     FUD(10)       FUDRIG                         *TEMPI
216000970527     C                   MOVEL     'N'           FUDATT
216100970527     C                   MOVEL     FUDRIG        FUD(10)
216200970526E   1C                   ENDIF
216300970526     C*
216400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
216500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
216600970526     C                   EXSR      FUNGES
216700000925     C*
216800000925     C* VERIFICO CHE SOLO UTENTI "EDP*" POSSANO MODIFICARE IL CAMPO "UFFICIO POSTALE"
216900000925     C                   MOVEL     KNMUS         UTE               3
217000000925     C     UTE           IFNE      'EDP'
217100000925     C                   SETON                                        95
217200000925     C                   ENDIF
217300000925     C*
217400970526     C*
217500970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
217600970526     C*---
217700970526     C* 90 -> PROTEZIONE CHIAVE
217800970526     C* 91 -> PROTEZIONE TOTALE
217900050414     C* 93 -> PROTEZIONE COD.TASSAZIONE e TIPO INOLTRO e altri
218000970526     C*---
218100050413     C*
218200050413     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
218300050413     C                   SETON                                        93
218400050413     C*
218500050413     C* VERIFICO X PRIMISSIMA COSA LE AUTORIZZAZIONI SPECIALI SULL'UTENTE CORRENTE
218600050413     C                   IF        §UTECAP = *blanks
218700050413     C* TUTTO RIMANE INVARIATO
218800050413     C                   ENDIF
218900050413     C*
219000050413     C                   IF        §UTECAP = 'I'
219100050413     C* FORZO SOLO INTERROGAZIONE
219200050413     C                   EVAL      DEPOP1 = 'O05'
219300050413     C                   ENDIF
219400050413     C*
219500050413     C                   IF        §UTECAP = 'C'
219600050413     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
219700050413     C                   SETOFF                                       93
219800050413     C                   ENDIF
219900050413     C*
220000970526IF  1C     DEPOP0        IFEQ      'C02'                                        G E S T I O N E
220100970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
220200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
220300970526     C                   SETOFF                                       91
220400970526E   2C                   ENDIF
220500970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
220600970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
220700970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
220800970526E   2C                   ENDIF
220900970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
221000970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
221100970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
221200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
221300970526     C                   SETON                                        91
221400970526E   2C                   ENDIF
221500970526X   1C                   ELSE                                                   *I N T E R R O G A
221600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
221700970526     C                   SETON                                        91
221800970526E   1C                   ENDIF
221900970526     C*
222000970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
222100970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
222200970526     C                   Z-ADD     D00VER        KCCVER
222300970526     C                   MOVEL     D00NAR        KCCNAR
222400970526     C                   MOVEL     D00CAP        KCCCAP
222500970526     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
222600970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
222700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
222800970526     C                   MOVEL     MSG(6)        DSMSMS
222900970526X   2C                   ELSE
223000970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
223100980128IF  3C     DEPOP1        IFEQ      'O05'
223200980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
223300980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
223400980128E   3C                   ENDIF
223500980128E   2C                   ENDIF
223600970526X   1C                   ELSE                                                   *INSERIMENTO
223700970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
223800970526E   1C                   ENDIF
223900970526     C*
224000970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
224100970526IF  1C     DEPOP1        IFEQ      'O02'
224200970526     C                   MOVEL     DEC(1)        VIDFUN
224300970526E   1C                   ENDIF
224400970526IF  1C     DEPOP1        IFEQ      'O04'
224500970526     C                   MOVEL     DEC(2)        VIDFUN
224600970526E   1C                   ENDIF
224700970526IF  1C     DEPOP1        IFEQ      'O05'
224800970526     C                   MOVEL     DEC(3)        VIDFUN
224900970526E   1C                   ENDIF
225000970526IF  1C     DEPOP1        IFEQ      'O07'
225100970526     C                   MOVEL     DEC(4)        VIDFUN
225200970526E   1C                   ENDIF
225300970526IF  1C     DEPOP1        IFEQ      'F06'
225400970526     C                   MOVEL     DEC(5)        VIDFUN
225500970526E   1C                   ENDIF
225600970526     C*
225700970526     C                   ENDSR
225800970703     C*------------------------------------------------------------------------*
225900970703     C* FINSR - OPERAZIONI FINALI
226000970703     C*------------------------------------------------------------------------*
226100970703     C     FINSR         BEGSR
226200970703     C*
226300970704     C                   CLEAR                   DSSI95
226400970703     C                   MOVEL     'C'           I95TLA
226500970703     C                   CALL      'TISI95R'
226600970703     C                   PARM                    DSSI95
226700970703     C*
226800970703     C                   ENDSR
226900050413     C*--------------------------------------------------------------------------------------------*
227000050413     C* REPERISCE DATI UTENTE
227100050413     C*--------------------------------------------------------------------------------------------*
227200050413     C     REPDATIUTE    BEGSR
227300050413     C*
227400050413     C* REimposta le variabili di lavoro
227500050413     C                   CLEAR                   TIBS34DS
227600050413     C                   CLEAR                   AZUTEDS
227700050413     C                   CLEAR                   DDATIUTE
227800050413     C*
227900050413     C* Reperisce i dati del profilo in ingresso
228000050413     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
228100050413     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
228200050413     C                   IN(E)     *DTAARA
228300050413     C                   IF        %Error                                       *NO errori
228400050413     C                   EVAL      I34TLA = 'L'
228500050413     C                   CALL      'TIBS34R'
228600050413     C                   PARM                    TIBS34DS
228700050413     C                   IN        *DTAARA
228800050413     C                   ENDIF
228900050413     C*
229000050413     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
229100050413     C                   EVAL      DUTE01 = UTEFAF
229200050413     C*
229300050413     C                   ENDSR
229400970526     C*------------------------------------------------------------------------*
229500970526     C* *INZSR - OPERAZIONI INIZIALI
229600970526     C*------------------------------------------------------------------------*
229700970526     C     *INZSR        BEGSR
229800970526     C*---
229900970526     C* RICEVIMENTO PARAMETRI
230000970526     C*---
230100970526     C     *ENTRY        PLIST
230200970612     C                   PARM                    KPJBA
230300970612     C                   PARM                    DSSI00
230400050413     C*
230500050413     C* REPERISCE I DATI DELL'UTENTE
230600050413     C                   EXSR      REPDATIUTE
230700060206     c                   movel     RSUT          DSfirs
230800970526     C*
230900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
231000970526     C     XMSGPA        PLIST
231100970526     C                   PARM                    DSMSMS
231200970526     C                   PARM                    DSMSPG
231300970526     C                   PARM                    DSMSKE
231400970526     C                   PARM                    DSMSER
231500090903     c*
231600090903     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
231700090903     c                   clear                   tibs02ds
231800090903     c                   clear                   dvpodeco
231900090903     c                   eval      t02mod = 'C'
232000090903     c                   eval      t02sif = knsif
232100090903     c                   eval      t02cod = 'VPO'
232200090903     c                   eval      t02ke1 = 'DECO'
232300090903     c                   call      'TIBS02R'
232400090903     c                   parm                    kpjba
232500090903     c                   parm                    tibs02ds
232600090903     c                   if        t02err = *blanks
232700090903     c                   eval      dvpodeco = t02uni
232800090903     c                   else
232900090903     c                   eval      §vpotsph=20091001
233000090903     c                   endif
233100090903
233200090903     c     *iso          movel     §vpotsph      dataiso
233300090903     c
233400090903     c* Calcolo data oddierna
233500090903     C*
233600000426     C*
233700000426     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
233800000426     C     D00NAR        IFNE      *BLANKS
233900000426     C                   SETON                                        92
234000000426     C                   ELSE
234100000426     C                   SETOFF                                       92
234200000426     C                   ENDIF
234300970526     C*
234400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
234500970526     C                   CALL      'XKEY'
234600970526     C                   PARM                    DSKEY
234700970526     C*---
234800970526     C* VARIABILI NON RIFERITE AL DATA BASE
234900970526     C*---
235000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
235100970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
235200970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
235300970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
235400970526     C     *LIKE         DEFINE    D00OP1        WOP1
235500970602     C     *LIKE         DEFINE    D00NAR        DNAR
235600970602     C     *LIKE         DEFINE    D00CAP        DCAP
235700970526     C*---
235800970526     C* VARIABILI DI CONTROLLO
235900970526     C*---
236000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
236100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
236200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
236300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
236400970917     C                   MOVEL     'S'           $LOBS             1            *LOCALITA' OBSOLETE
236500970526     C*
236600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
236700970526     C                   MOVEL     'TISI03R'     DSMSPG
236800970526     C                   MOVEL     'TISI03R'     MSGQUE
236900970526     C                   MOVEL     *BLANKS       MSGKEY
237000970526     C                   MOVEL     *BLANKS       DSMSMS
237100970526     C*---
237200970526     C* VARIABILI RIFERITE AL DATA BASE
237300970526     C*---
237400970526     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
237500970526     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
237600970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
237700970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
237800970526     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
237900970526     C     *LIKE         DEFINE    CPCNAR        KCCNAR
238000970526     C     *LIKE         DEFINE    CPCCAP        KCCCAP
238100970526     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
238200970526     C     *LIKE         DEFINE    CPENAR        KCENAR
238300970526     C     *LIKE         DEFINE    CPECAP        KCECAP
238400970526     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
238500970526     C     *LIKE         DEFINE    CPLNAR        KCLNAR
238600970526     C     *LIKE         DEFINE    CPLCAP        KCLCAP
238700130903     C     *LIKE         DEFINE    CPLloc        KCLloc
238800970526     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
238900970526     C     *LIKE         DEFINE    CPSNAR        KCSNAR
239000970526     C     *LIKE         DEFINE    CPSCAP        KCSCAP
239100970526     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
239200970526     C     *LIKE         DEFINE    CPTTTC        KCTTTC
239300970526     C     *LIKE         DEFINE    CPTTSP        KCTTSP
239400970526     C*---
239500970526     C* CHIAVI DI LETTURA
239600970526     C*---
239700970526     C* LETTURA AZORG01L - COMPLETA
239800970526     C     KEYORG        KLIST
239900970526     C                   KFLD                    KORFIL                         *FILIALE
240000970526     C* LETTURA TABEL00F - COMPLETA
240100970526     C     KEYTAB        KLIST
240200970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
240300970526     C                   KFLD                    KTBCOD                         *TABELLA
240400970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
240500970526     C* LETTURA TABEL00F - PARZIALE
240600970526     C     KE2TAB        KLIST
240700970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
240800970526     C                   KFLD                    KTBCOD                         *TABELLA
240900970526     C* LETTURA AZCPC01L - COMPLETA
241000970526     C     KEYCPC        KLIST
241100970526     C                   KFLD                    KCCVER                         *VERSIONE
241200970526     C                   KFLD                    KCCNAR                         *NAZIONE
241300970526     C                   KFLD                    KCCCAP                         *CAP
241400970527     C* LETTURA AZCPT01L - COMPLETA
241500970526     C     KEYCPT        KLIST
241600970526     C                   KFLD                    KCTVER                         *VERSIONE
241700970526     C                   KFLD                    KCTTTC                         *TEMPO CONSEGNA
241800970526     C                   KFLD                    KCTTSP                         *TIPO SERVIZIO
241900970527     C* LETTURA AZCPE01L - PARZIALE
242000970527     C     KEYCPE        KLIST
242100970527     C                   KFLD                    KCEVER                         *VERSIONE
242200970527     C                   KFLD                    KCENAR                         *NAZIONE
242300970527     C                   KFLD                    KCECAP                         *CAP
242400970526     C* LETTURA AZCPL01L - PARZIALE
242500970526     C     KEYCPL        KLIST
242600970526     C                   KFLD                    KCLVER                         *VERSIONE
242700970526     C                   KFLD                    KCLNAR                         *NAZIONE
242800970526     C                   KFLD                    KCLCAP                         *CAP
242900130903     C* LETTURA AZCPL01L - PARZIALE
243000130903     C     KEYCPL2       KLIST
243100130903     C                   KFLD                    KCLVER                         *VERSIONE
243200130903     C                   KFLD                    KCLNAR                         *NAZIONE
243300130903     C                   KFLD                    KCLCAP                         *CAP
243400130903     C                   KFLD                    KCLloc                         *CAP
243500970526     C* LETTURA AZCPS01L - PARZIALE
243600970526     C     KEYCPS        KLIST
243700970526     C                   KFLD                    KCSVER                         *VERSIONE
243800970526     C                   KFLD                    KCSNAR                         *NAZIONE
243900970526     C                   KFLD                    KCSCAP                         *CAP
244000130902     C*
244100130902     C     Kcos          KLIST
244200130903     C                   KFLD                    cosVER
244300130903     C                   KFLD                    cosnar
244400130903     C                   KFLD                    coscap
244500130903     C                   KFLD                    cosloc
244600130903     C                   KFLD                    coslna
244700130903     C     Kcos2         KLIST
244800130903     C                   KFLD                    cosVER
244900130903     C                   KFLD                    coslna
245000130903     C                   KFLD                    cosnar
245100130903     C                   KFLD                    coscap
245200130903     C                   KFLD                    cosloc
245300130903     C     Kcos22        KLIST
245400130903     C                   KFLD                    cosVER
245500130903     C                   KFLD                    coslna
245600130903     C                   KFLD                    cosnar
245700130903     C                   KFLD                    coscap
245800970526     C*
245900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
246000970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
246100970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
246200970526     C*
246300970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
246400970526     C                   EXSR      IMPFUN
246500970526     C*
246600970526     C                   ENDSR
246700970526     C*------------------------------------------------------------------------*
246800970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
246900970526     C*------------------------------------------------------------------------*
247000970526     C     FUNGES        BEGSR
247100970526     C*
247200970526     C* PULISCO CAMPI DI LAVORO
247300970526     C                   Z-ADD     0             W                 5 0
247400970526     C*
247500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
247600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
247700970526     C     1             DO        24            I
247800970526     C                   MOVEL     FUD(I)        FUDRIG
247900970526     C*
248000970526     C* PULIZIA FUNZIONI PRECEDENTI
248100970526     C                   MOVEL     *BLANKS       FUV(I)
248200970526     C*
248300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
248400970526     C* (SALTA F24)
248500970526     C     FUDATT        IFEQ      'S'
248600970526     C     FUDFUN        ANDNE     '24'
248700970526     C                   ADD       1             W
248800970526     C                   MOVEL     FUDDES        FUV(W)
248900970526     C                   END
249000970526     C                   END                                                    DO WHILE
249100970526     C*
249200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
249300970526     C* ALTRIMENTI DISATTIVO F24
249400970526     C     1             DO        24            I
249500970526     C                   MOVEL     FUD(I)        FUDRIG
249600970526     C*
249700970526     C     FUDFUN        IFEQ      '24'
249800970526     C     W             IFGT      6
249900970526     C                   MOVEL     'S'           FUDATT
250000970526     C                   ELSE
250100970526     C                   MOVEL     *BLANKS       FUDATT
250200970526     C                   END                                                    W GT 8
250300970526     C                   MOVEL     FUDRIG        FUD(I)
250400970526     C                   END
250500970526     C                   END                                                    DO WHILE
250600970526     C*
250700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
250800970526     C     W             DIV       6             I
250900970526     C                   MVR                     Z
251000970526     C*
251100970526     C     Z             IFNE      0
251200970526     C     I             ADD       1             Z
251300970526     C                   ELSE
251400970526     C                   Z-ADD     I             Z                 5 0
251500970526     C                   END
251600970526     C*
251700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
251800970526     C                   ADD       1             FUNGRU
251900970526     C     FUNGRU        IFGT      Z
252000970526     C     FUNGRU        ORLT      1
252100970526     C                   Z-ADD     1             FUNGRU
252200970526     C                   END
252300970526     C*
252400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
252500970526     C     FUNGRU        IFEQ      1
252600970526     C                   MOVEL     FUV(1)        FUN001
252700970526     C                   MOVEL     FUV(2)        FUN002
252800970526     C                   MOVEL     FUV(3)        FUN003
252900970526     C                   MOVEL     FUV(4)        FUN004
253000970526     C                   MOVEL     FUV(5)        FUN005
253100970526     C                   MOVEL     FUV(6)        FUN006
253200970526     C                   END
253300970526     C*
253400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
253500970526     C     FUNGRU        IFEQ      2
253600970526     C                   MOVEL     FUV(7)        FUN001
253700970526     C                   MOVEL     FUV(8)        FUN002
253800970526     C                   MOVEL     FUV(9)        FUN003
253900970526     C                   MOVEL     FUV(10)       FUN004
254000970526     C                   MOVEL     FUV(11)       FUN005
254100970526     C                   MOVEL     FUV(12)       FUN006
254200970526     C                   END
254300970526     C*
254400970526     C* TERZO GRUPPO DI 6 FUNZIONI
254500970526     C     FUNGRU        IFEQ      3
254600970526     C                   MOVEL     FUV(13)       FUN001
254700970526     C                   MOVEL     FUV(14)       FUN002
254800970526     C                   MOVEL     FUV(15)       FUN003
254900970526     C                   MOVEL     FUV(16)       FUN004
255000970526     C                   MOVEL     FUV(17)       FUN005
255100970526     C                   MOVEL     FUV(18)       FUN006
255200970526     C                   END
255300970526     C*
255400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
255500970526     C     FUNGRU        IFEQ      4
255600970526     C                   MOVEL     FUV(19)       FUN001
255700970526     C                   MOVEL     FUV(20)       FUN002
255800970526     C                   MOVEL     FUV(21)       FUN003
255900970526     C                   MOVEL     FUV(22)       FUN004
256000970526     C                   MOVEL     *BLANKS       FUN005
256100970526     C                   MOVEL     *BLANKS       FUN006
256200970526     C                   END
256300970526     C*
256400970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
256500970526     C                   MOVEL     *BLANKS       FUN007
256600970526     C                   MOVEL     *BLANKS       FUN008
256700970526     C*
256800970526     C     1             DO        24            I
256900970526     C                   MOVEL     FUD(I)        FUDRIG
257000970526     C     FUDFUN        IFEQ      '24'
257100970526     C     FUDATT        ANDEQ     'S'
257200970526     C                   MOVEL     FUDDES        FUN008
257300970526     C                   END
257400970526     C                   END                                                    DO
257500970526     C*
257600970526     C                   ENDSR
257700970526     C*------------------------------------------------------------------------*
257800970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
257900970526     C*------------------------------------------------------------------------*
258000970526     C     FUNCON        BEGSR
258100970526     C*
258200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
258300970526     C                   EXSR      FUNVAR
258400970526     C*
258500970526     C* PULISCO CAMPI DI LAVORO
258600970526     C                   Z-ADD     0             I                 5 0
258700970526     C                   Z-ADD     0             W                 5 0
258800970526     C*
258900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
259000970526     C     FUNKEY        IFNE      *BLANKS
259100970526     C     1             DO        24            I
259200970526     C                   MOVEL     FUD(I)        FUDRIG
259300970526     C*
259400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
259500970526     C     FUNKEY        IFEQ      FUDFUN
259600970526     C     FUDATT        ANDEQ     'S'
259700970526     C                   Z-ADD     I             W
259800970526     C                   END
259900970526     C                   END                                                    DO WHILE
260000970526     C*
260100970526     C* FUNZIONE NON ATTIVA (ERRORE)
260200970526     C     W             IFEQ      0
260300970526     C*
260400970526     C* EMISSIONE MESSAGGIO DI ERRORE
260500970526     C                   MOVEL     MSG(1)        DSMSMS
260600970526     C*
260700970526     C                   END                                                    W=0
260800970526     C                   END                                                    FUNKEY NE *BLANKS
260900970526     C*
261000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
261100970526     C     W             IFNE      0
261200970526     C     FUNKEY        OREQ      *BLANKS
261300970526     C     FUNKEY        IFEQ      *BLANKS
261400970526     C                   EXSR      ENTGES
261500970526     C                   END
261600970526     C     FUNKEY        IFEQ      '03'
261700970526     C                   EXSR      F03GES
261800970526     C                   END
261900010320     C     FUNKEY        IFEQ      '04'
262000010320     C                   EXSR      F04GES
262100010320     C                   END
262200130821     C     FUNKEY        IFEQ      '06'
262300130905     c***                if        f07lna='S' or f07los='S'
262400130905     c***                          or f07lol='S'
262500130905     C***                EXSR      F07GES
262600130905     c***                if        errl00='0'
262700130905     c***                clear                   f07lna
262800130905     c***                clear                   f07lol
262900130905     c***                clear                   f07los
263000130905     c***                endif
263100130903     c
263200130905     c***                else
263300130821     C                   EXSR      F06GES
263400130905     c***                endif
263500130902     c
263600130821     C                   END
263700130827     C     FUNKEY        IFEQ      '07'
263800130827     C                   EXSR      F07GES
263900130827     C                   END
264000970526     C     FUNKEY        IFEQ      '08'
264100970526     C                   EXSR      F08GES
264200970526     C                   END
264300970526     C     FUNKEY        IFEQ      '09'
264400970526     C                   EXSR      F09GES
264500970526     C                   END
264600970526     C     FUNKEY        IFEQ      '10'
264700970526     C                   EXSR      F10GES
264800970526     C                   END
264900970526     C     FUNKEY        IFEQ      '12'
265000970526     C                   EXSR      F12GES
265100970526     C                   END
265200970603     C     FUNKEY        IFEQ      '14'
265300970603     C                   EXSR      F14GES
265400970603     C                   END
265500130828     C     FUNKEY        IFEQ      '17'
265600130828     C                   EXSR      F17GES
265700130828     C                   END
265800970526     C     FUNKEY        IFEQ      '24'
265900970526     C                   EXSR      F24GES
266000970526     C                   END
266100970526     C                   END
266200970526     C*
266300970526     C                   ENDSR
266400970526     C*------------------------------------------------------------------------*
266500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
266600970526     C*------------------------------------------------------------------------*
266700970526     C     FUNVAR        BEGSR
266800970526     C*
266900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
267000970526     C*
267100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
267200970526     C     KEY           IFEQ      ENTER
267300970526     C                   MOVEL     *BLANKS       FUNKEY
267400970526     C                   END
267500970526     C     KEY           IFEQ      F1
267600970526     C                   MOVEL     '01'          FUNKEY
267700970526     C                   END
267800970526     C     KEY           IFEQ      F2
267900970526     C                   MOVEL     '02'          FUNKEY
268000970526     C                   END
268100970526     C     KEY           IFEQ      F3
268200970526     C                   MOVEL     '03'          FUNKEY
268300970526     C                   END
268400970526     C     KEY           IFEQ      F4
268500970526     C                   MOVEL     '04'          FUNKEY
268600970526     C                   END
268700970526     C     KEY           IFEQ      F5
268800970526     C                   MOVEL     '05'          FUNKEY
268900970526     C                   END
269000970526     C     KEY           IFEQ      F6
269100970526     C                   MOVEL     '06'          FUNKEY
269200970526     C                   END
269300970526     C     KEY           IFEQ      F7
269400970526     C                   MOVEL     '07'          FUNKEY
269500970526     C                   END
269600970526     C     KEY           IFEQ      F8
269700970526     C                   MOVEL     '08'          FUNKEY
269800970526     C                   END
269900970526     C     KEY           IFEQ      F9
270000970526     C                   MOVEL     '09'          FUNKEY
270100970526     C                   END
270200970526     C     KEY           IFEQ      F10
270300970526     C                   MOVEL     '10'          FUNKEY
270400970526     C                   END
270500970526     C     KEY           IFEQ      F11
270600970526     C                   MOVEL     '11'          FUNKEY
270700970526     C                   END
270800970526     C     KEY           IFEQ      F12
270900970526     C                   MOVEL     '12'          FUNKEY
271000970526     C                   END
271100970526     C     KEY           IFEQ      F13
271200970526     C                   MOVEL     '13'          FUNKEY
271300970526     C                   END
271400970526     C     KEY           IFEQ      F14
271500970526     C                   MOVEL     '14'          FUNKEY
271600970526     C                   END
271700970526     C     KEY           IFEQ      F15
271800970526     C                   MOVEL     '15'          FUNKEY
271900970526     C                   END
272000970526     C     KEY           IFEQ      F16
272100970526     C                   MOVEL     '16'          FUNKEY
272200970526     C                   END
272300970526     C     KEY           IFEQ      F17
272400970526     C                   MOVEL     '17'          FUNKEY
272500970526     C                   END
272600970526     C     KEY           IFEQ      F18
272700970526     C                   MOVEL     '18'          FUNKEY
272800970526     C                   END
272900970526     C     KEY           IFEQ      F19
273000970526     C                   MOVEL     '19'          FUNKEY
273100970526     C                   END
273200970526     C     KEY           IFEQ      F20
273300970526     C                   MOVEL     '20'          FUNKEY
273400970526     C                   END
273500970526     C     KEY           IFEQ      F21
273600970526     C                   MOVEL     '21'          FUNKEY
273700970526     C                   END
273800970526     C     KEY           IFEQ      F22
273900970526     C                   MOVEL     '22'          FUNKEY
274000970526     C                   END
274100970526     C     KEY           IFEQ      F23
274200970526     C                   MOVEL     '23'          FUNKEY
274300970526     C                   END
274400970526     C     KEY           IFEQ      F24
274500970526     C                   MOVEL     '24'          FUNKEY
274600970526     C                   END
274700970526     C     KEY           IFEQ      ROLLUP
274800970526     C                   MOVEL     'RU'          FUNKEY
274900970526     C                   END
275000970526     C     KEY           IFEQ      HELP
275100970526     C                   MOVEL     'HP'          FUNKEY
275200970526     C                   END
275300970526     C                   ENDSR
275400970526     C*------------------------------------------------------------------------*
275500970526     C* NOTE COSTRUZIONE SCHIERE
275600970526     C*------------------------------------------------------------------------*
275700970526     C*
275800970526     C* -MSG-MESSAGGI
275900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
276000970526     C* (NON TOCCARE MSG,1 E MSG,2)
276100970526     C*
276200970526     C* -CMD-COMANDI
276300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
276400970526     C*
276500970526     C* -OPD-OPZIONI
276600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
276700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
276800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
276900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
277000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
277100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
277200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
277300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
277400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
277500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
277600970526     C*
277700970526     C* -FUD-FUNZIONI
277800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
277900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
278000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
278100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
278200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
278300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
278400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
278500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
278600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
278700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
278800970526     O*------------------------------------------------------------------------*
278900970526** DEC - DECODIFICHE
279000970526    MODIFICA                                                      1
279100970526  ANNULLAMENTO                                                    2
279200970526VISUALIZZAZIONE                                                   3
279300970526   RIPRISTINO                                                     4
279400970526  INSERIMENTO                                                     5
279500970526** MSG - MESSAGGI
279600970526TISI03R- Tasto funzionale non gestito                                          1
279700970526TISI03R- Inserimento effettuato                                                2
279800970526TISI03R- Modifica effettuata                                                   3
279900970526TISI03R- Annullamento effettuato                                               4
280000970526TISI03R- Ripristino effettuato                                                 5
280100970526TISI03R- Richiesta funzione per record inesistente                             6
280200970526TISI03R- Record momentaneamente allocato: riprovare più tardi                  7
280300970527TISI03R- Richiesta funzione per record esistente                               8
280400970526TISI03R- Provincia errata                                                      9
280500970526TISI03R- Linea di arrivo errata                                               10
280600970526TISI03R- Zona di consegna errata                                              11
280700060927§§§§§§§§ Ufficio Postale inesistente o annullato                              12
280800970526TISI03R- Nazione errata                                                       13
280900970526TISI03R- Codice tassazione errato                                             14
281000970526TISI03R- Codice tassazione errato. Accettati solo I1, I2, I3, ecc ...         15
281100970526TISI03R- Per CAP estero lasciare il codice tassazione vuoto                   16
281200060927§§§§§§§§ Zona di consegna NON abilitata                                       17
281300970604TISI03R- Linea limite obbligatoria se immessi limiti di peso o volume         18
281400970604TISI03R- Limiti obbligatori se immesse linea o zona limite                    19
281500130905Esistono OrariServizi personalizz.x la linea variata. Saranno eliminati       20
281600970912TISI03R- Il limite oltre deve essere superiore del limite sotto               21
281700010320TISI03R- Tasto funzione non consentito sul campo corrente.                    22
281800130905TISI03R- F17 consentito soltando sui campi LINEA di ARRIVO valorizzati        23
281900010320TISI03R- Tipo tempo ritiro errato o incompleto                                24
282000970917TISI03R- Il cap può essere obsoleto se lo sono anche tutte le sue località    25
282100970917TISI03R- Impossibile inserire cap obsoleti                                    26
282200970526TISI03R- Immettere 'S' o 'N'                                                  27
282300110623TISI03R- Tipo inoltro errato o non utilizzabile nel cappario                  28
282400970917TISI03R- Immettere 'O' per rendere obsoleto il cap, altrimenti lasciare vuoto 29
282500970603TISI03R- Tipo tempo di consegna errato o incompleto                           30
282600970526** FUD - FUNZIONI
282700970526                   -        (S/N)=N                        funz.01¬
282800970530                   -        (S/N)=N                        funz.02¬
282900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
283000010320F04=Visual. tempi  -        (S/N)=S  <== S-FISSO           funz.04¬
283100130827                   -        (S/N)=N                        funz.05¬
283200970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
283300130827F07=Orari servizi  -        (S/N)=S                        funz.07¬
283400970526F08=Località       -        (S/N)=S                        funz.08¬
283500970526F09=Eccezioni      -        (S/N)=S                        funz.09¬
283600970526F10=Tempi consegna -        (S/N)=S                        funz.10¬
283700970526                   -        (S/N)=N                        funz.11¬
283800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
283900970526                   -        (S/N)=N                        funz.13¬
284000970603F14=Zone consegna  -        (S/N)=S                        funz.14¬
284100970526                   -        (S/N)=N                        funz.15¬
284200970526                   -        (S/N)=N                        funz.16¬
284300130905F17=OrariServLinea -        (S/N)=S                        funz.17¬
284400970526                   -        (S/N)=N                        funz.18¬
284500970526                   -        (S/N)=N                        funz.19¬
284600970526                   -        (S/N)=N                        funz.20¬
284700970526                   -        (S/N)=N                        funz.21¬
284800970526                   -        (S/N)=N                        funz.22¬
284900970526                   -        (S/N)=N                        funz.23¬
285000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
