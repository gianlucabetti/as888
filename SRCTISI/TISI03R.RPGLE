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
010900170116     d*** dvpodeco      e ds
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
013600170116     d wabiute         s              1
013700170116     d XX              s              3  0
013800131007     d YY              s              3  0
013900131007
014000131007
014100131007      /copy gaitrasrc/srcprotopr,tibs02r
014200131007
014300131007     C*------------------------------------------------------------------------*
014400970526     C* MAIN LINES
014500970526     C*------------------------------------------------------------------------*
014600970526     C*
014700970526     C* NON CI SONO ERRORI IN ENTRATA
014800970526IF  1C     ERRL00        IFEQ      '0'
014900970526     C*
015000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015400970526     C*
015500970526     C* EMETTE FORMATO VIDEO
015600970526     C                   EXSR      BUTFMR
015700970526     C*
015800970526     C* GESTIONE TASTI FUNZIONALI
015900970526     C                   EXSR      FUNCON
016000970526E   2C                   ENDDO
016100970526     C*
016200970526     C* CI SONO ERRORI IN ENTRATA
016300970527X   1C                   ELSE
016400970526     C                   MOVEL     '1'           D00ERR
016500970526     C                   MOVEL     DSMSMS        D00MSG
016600970526E   1C                   ENDIF
016700970703     C*
016800970703     C* OPERAZIONI FINALI
016900970703     C                   EXSR      FINSR
017000970526     C* FINE PGM
017100970526     C                   SETON                                        LR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     BUTFMR        BEGSR
017600970526     C*
017700970526     C* EMETTE LA TESTATA
017800970526     C                   WRITE     SI03D01
017900970526     C*
018000970526     C* SPEDISCE EVENTUALE MESSAGGIO
018100970526     C                   EXSR      SNDMSG
018200970526     C*
018300970526     C* EMETTE LE FUNZIONI
018400970526     C                   WRITE     SI03D02
018500970526     C*
018600970526     C* EMETTE IL VIDEO
018700970526     C                   EXFMT     SI03D03
018800970526     C*
018900970526     C* REIMPOSTA FLAG DI ERRORE
019000970526     C                   MOVEL     '0'           ERRL00
019100970526     C*
019200970526     C* REIMPOSTA INDICATORI DI ERRORE
019300970526     C                   SETOFF                                         5859
019400970526     C                   SETOFF                                       606162
019500970526     C                   SETOFF                                       636465
019600970526     C                   SETOFF                                       666768
019700970526     C                   SETOFF                                       697071
019800970526     C                   SETOFF                                       727374
019900970526     C                   SETOFF                                       757677
020000970526     C                   SETOFF                                       787980
020100970526     C                   SETOFF                                       818283
020200970526     C                   SETOFF                                       848586
020300970526     C                   SETOFF                                       878889
020400000420     C                   SETOFF                                       525354
020500010322     C                   SETOFF                                       5544
020600970526     C*
020700970526     C                   ENDSR
020800970526     C*------------------------------------------------------------------------*
020900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
021000970526     C*------------------------------------------------------------------------*
021100970526     C     SNDMSG        BEGSR
021200970526     C*
021300970526     C* PULISCE IL SFILE MESSAGGI
021400970526     C                   MOVEL     '1'           *IN40
021500970526     C                   WRITE     SI03DMC
021600970526     C                   MOVEL     '0'           *IN40
021700970526     C*
021800970526     C* VISUALIZZA IL MESSAGGIO
021900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
022000970526     C                   CALL      'XMSG'        XMSGPA
022100970526IF  1C     DSMSER        IFEQ      *ZEROS
022200970526     C                   MOVEL     DSMSKE        MSGKEY
022300970526E   1C                   ENDIF
022400970526     C                   Z-ADD     1             MSGULT
022500970526     C                   WRITE     SI03DMS
022600970526     C                   WRITE     SI03DMC
022700970526     C*
022800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
022900970526     C                   MOVEL     *BLANKS       DSMSMS
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* ENTGES - GESTIONE ENTER -> INVIO
023400970526     C*------------------------------------------------------------------------*
023500970526     C     ENTGES        BEGSR
023600970526     C*
023700970526     C* CONTROLLA I CAMPI IMMESSI
023800970526     C                   EXSR      DECODI
023900970526     C*
024000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
024100980128IF  1C     DEPOP1        IFEQ      'O05'
024200980128     C                   MOVEL     '1'           FINL00
024300980128E   1C                   ENDIF
024400970526     C*
024500970526     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* F03GES - GESTIONE F03 -> FINE
024800970526     C*------------------------------------------------------------------------*
024900970526     C     F03GES        BEGSR
025000970526     C*
025100970526     C* IMPOSTA "PREMUTO F03"
025200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
025300970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
025400970526     C*
025500970526     C                   ENDSR
025600010320     C*------------------------------------------------------------------------*
025700010320     C* F04GES - GESTIONE F04 -> VISUALIZZAZIONE TEMPI CONSEGNA/RITIRO
025800010320     C*------------------------------------------------------------------------*
025900010320     C     F04GES        BEGSR
026000090605      * imposto le descrizioni fisse per i 3 tipi servizio utilizzati
026100090903      * 'C' - 'D' - 'E' -'H'
026200090605     c                   eval      ktbcod = '5E'
026300090605     c     ke2tab        setll     tabel00f
026400090605     c                   do        *hival
026500090605     c     ke2tab        reade     tabel00f
026600090605     c                   if        %eof(tabel00f)
026700090605     c                   leave
026800090605     c                   endif
026900090605     c                   eval      ds5e = tbluni
027000090605     c                   if        tblkey = 'C'
027100090605     c                   eval      viddtspc = §5ed03
027200090605     c                   endif
027300090605     c                   if        tblkey = 'D'
027400090605     c                   eval      viddtspd = §5ed03
027500090605     c                   endif
027600090605     c                   if        tblkey = 'E'
027700090605     c                   eval      viddtspe = §5ed03
027800090605     c                   endif
027900170116     c**                 if        tblkey = 'H' and (vidh0c>0 or
028000170116     c**                           Datasys>=dataiso)
028100170116
028200170116     c                   if        tblkey = 'H' and (vidh0c>0)
028300090903     c                   eval      viddtsph = §5ed03
028400090903     c                   endif
028500090605     c                   enddo
028600010320     C*
028700010320     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
028800010320     C     RNMFL         IFEQ      'VIDTTC'
028900010320     C                   EXFMT     SI03WWC
029000010320     C                   ELSE
029100010320     C     RNMFL         IFEQ      'VIDTTR'
029200010320     C                   EXFMT     SI03WWR
029300010320     C                   ELSE
029400010322     C                   MOVEL     MSG(22)       DSMSMS
029500010320     C                   ENDIF
029600010320     C                   ENDIF
029700010320     C*
029800010320     C                   ENDSR
029900130821     C*------------------------------------------------------------------------*
030000130827     C* F07GES - GESTIONE F07 -> Orari servizi
030100130821     C*------------------------------------------------------------------------*
030200130827     C     F07GES        BEGSR
030300130902     c* Gestisco prima l'aggiornamento e poi gli orari
030400130902     c                   exsr      F06ges
030500130903     c                   if        errl00='0'
030600130821     C*
030700130821     c                   clear                   tisio0ds
030800130821     C                   MOVEL     VIDNAR        do0NAR
030900130821     C                   MOVEL     VIDCAP        Do0CAP
031000130830     c                   if        %subst(depop0:2:2)='02'
031100130830     C                   MOVEL     'G'           do0tric
031200130830     c                   else
031300130830     C                   MOVEL     'I'           do0tric
031400130830     c                   endif
031500130821     c                   movel     d00dri        do0dri
031600130821     c                   movel     d00ver        do0ver
031700130821
031800130821     C                   call      'TISIO2R'
031900130821     C                   PARM                    KPJBA
032000130821     C                   PARM                    tisio0ds
032100130821     C*
032200130821     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
032300130903     c                   clear                   f12l00
032400130821IF  1C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
032500130821     C                   MOVEL     '1'           ERRL00                         *ERRORE
032600130821     C                   MOVEL     '1'           F03L00                         *FINE PGM
032700130821X   1C                   ELSE                                                   *NON PREMUTO F03
032800130821     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
032900130821E   1C                   ENDIF
033000130821     C*
033100130903E   1C                   ENDIF
033200130903     C                   MOVEL     ' '           FINL00                         *FINE PGM
033300130821     C*
033400130821     C                   ENDSR
033500130828     C*------------------------------------------------------------------------*
033600130828     C* F17GES - GESTIONE F17 -> Orari servizi  per linea di arrivo
033700130828     C*------------------------------------------------------------------------*
033800130828     C     F17GES        BEGSR
033900130828     c                   clear                   dsmsms
034000130828     c                   exsr      decodi
034100130828     c
034200130828    1c                   if        dsmsms=*blanks
034300130828     C*
034400130828     c                   clear                   tisio0ds
034500130828     C                   MOVEL     VIDNAR        do0NAR
034600130830     c                   if        %subst(depop0:2:2)='02'
034700130830     C                   MOVEL     'G'           do0tric
034800130830     c                   else
034900130830     C                   MOVEL     'I'           do0tric
035000130830     c                   endif
035100130828     c                   movel     d00dri        do0dri
035200130828     c                   movel     d00ver        do0ver
035300130828
035400130828     C* TESTO IL CAMPO IN CUI E' POSIZIONATO IL CURSORE
035500130828    2c                   select
035600130828     C     RNMFL         wheneq    'VIDLNA'
035700130828     c     vidlna        andgt     *zeros
035800130828     C                   movel     vidlna        do0lna
035900130828     c
036000130828     C     RNMFL         wheneq    'VIDLOL'
036100130828     c     vidlol        andgt     *zeros
036200130828     C                   movel     vidlol        do0lna
036300130828     C     RNMFL         wheneq    'VIDLOS'
036400130828     c     vidlos        andgt     *zeros
036500130828     C                   movel     vidlos        do0lna
036600130828     C                   other
036700130828     C                   MOVEL     MSG(23)       DSMSMS
036800130828    2C                   ENDsl
036900130828
037000130828    2c                   if        dsmsms=*blanks
037100130828     C                   call      'TISIO2R'
037200130828     C                   PARM                    KPJBA
037300130828     C                   PARM                    tisio0ds
037400130828     C*
037500130828     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
037600130828IF  3C     Do0F03        IFEQ      '1'                                          *PREMUTO F03
037700130828     C                   MOVEL     '1'           ERRL00                         *ERRORE
037800130828     C                   MOVEL     '1'           F03L00                         *FINE PGM
037900130828X   1C                   ELSE                                                   *NON PREMUTO F03
038000130828     C                   MOVEL     Do0MSG        DSMSMS                         *MESSAGGIO DI RITORN
038100130828E   3C                   ENDIF
038200130828     C*
038300130828E   2C                   ENDIF
038400130828E   1C                   ENDIF
038500130828     C*
038600130828     C                   ENDSR
038700970526     C*------------------------------------------------------------------------*
038800970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
038900970526     C*------------------------------------------------------------------------*
039000970526     C     F06GES        BEGSR
039100970526     C*
039200970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
039300970526IF  1C     DEPOP1        IFEQ      'F06'
039400970526     C                   EXSR      INSERI
039500970526E   1C                   ENDIF
039600970526IF  1C     DEPOP1        IFEQ      'O02'
039700970526     C                   EXSR      MODIFI
039800970526E   1C                   ENDIF
039900970526IF  1C     DEPOP1        IFEQ      'O04'
040000970526     C                   EXSR      ANNULL
040100970526E   1C                   ENDIF
040200970526IF  1C     DEPOP1        IFEQ      'O07'
040300970526     C                   EXSR      RIPRIS
040400970526E   1C                   ENDIF
040500130902     c
040600970526     C                   ENDSR
040700970526     C*------------------------------------------------------------------------*
040800970526     C* F08GES - GESTIONE F08 -> GESTIONE LOCALITA'
040900970526     C*------------------------------------------------------------------------*
041000970526     C     F08GES        BEGSR
041100970528     C*
041200970602     C                   MOVEL     VIDNAR        DNAR
041300970602     C                   MOVEL     VIDCAP        DCAP
041400970528     C                   MOVEL     DEPOP0        WOP0
041500970528     C                   MOVEL     'L'           WOP0
041600970528     C                   MOVEL     'F08'         WOP1
041700970526     C                   MOVEL     'TISI30R'     WPGM              8
041800970526     C                   EXSR      LANPGM
041900970526     C*
042000970526     C                   ENDSR
042100970526     C*------------------------------------------------------------------------*
042200970526     C* F09GES - GESTIONE F09 -> ECCEZIONI LINEE/ZONE
042300970526     C*------------------------------------------------------------------------*
042400970526     C     F09GES        BEGSR
042500970528     C*
042600970602     C                   MOVEL     VIDNAR        DNAR
042700970602     C                   MOVEL     VIDCAP        DCAP
042800970528     C                   MOVEL     DEPOP0        WOP0
042900970528     C                   MOVEL     'E'           WOP0
043000970526     C                   MOVEL     'F09'         WOP1
043100970526     C                   MOVEL     'TISI11R'     WPGM              8
043200970526     C                   EXSR      LANPGM
043300970526     C*
043400970526     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600970526     C* F10GES - GESTIONE F10 -> TEMPI DI CONSEGNA
043700970526     C*------------------------------------------------------------------------*
043800970526     C     F10GES        BEGSR
043900970528     C*
044000970602     C                   MOVEL     VIDNAR        DNAR
044100970602     C                   MOVEL     VIDCAP        DCAP
044200970528     C                   MOVEL     DEPOP0        WOP0
044300970528     C                   MOVEL     'T'           WOP0
044400970526     C                   MOVEL     'F10'         WOP1
044500970526     C                   MOVEL     'TISI41R'     WPGM              8
044600970526     C                   EXSR      LANPGM
044700970526     C*
044800970526     C                   ENDSR
044900970526     C*------------------------------------------------------------------------*
045000970526     C* F12GES - GESTIONE F12 -> RITORNO
045100970526     C*------------------------------------------------------------------------*
045200970526     C     F12GES        BEGSR
045300970526     C*
045400970526     C* IMPOSTA "PREMUTO F12"
045500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
045600970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
045700970526     C*
045800970526     C                   ENDSR
045900970603     C*------------------------------------------------------------------------*
046000970603     C* F14GES - GESTIONE F14 -> ZONE DI CONSEGNA
046100970603     C*------------------------------------------------------------------------*
046200970603     C     F14GES        BEGSR
046300970603     C*
046400970603     C                   CALL      'TRTB23R'
046500970603     C                   PARM                    KPJBA
046600970612     C*
046700970603     C                   ENDSR
046800970526     C*------------------------------------------------------------------------*
046900970526     C* F21GES - GESTIONE F21 -> STAMPA CAPPARIO
047000970526     C*------------------------------------------------------------------------*
047100970526     C     F21GES        BEGSR
047200970526     C*
047300970602     C                   MOVEL     VIDNAR        DNAR
047400970602     C                   MOVEL     VIDCAP        DCAP
047500970526     C                   MOVEL     'F21'         WOP1
047600970526     C                   MOVEL     'TISI04R'     WPGM              8
047700970526     C                   EXSR      LANPGM
047800970526     C*
047900970526     C                   ENDSR
048000970526     C*------------------------------------------------------------------------*
048100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
048200970526     C*------------------------------------------------------------------------*
048300970526     C     F24GES        BEGSR
048400970526     C*
048500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
048600970526     C*
048700970526     C                   ENDSR
048800970526     C*------------------------------------------------------------------------*
048900000420     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
049000970526     C*------------------------------------------------------------------------*
049100970526     C     INSERI        BEGSR
049200970526     C*
049300970526     C* CONTROLLA I CAMPI IMMESSI
049400970526     C                   EXSR      DECODI
049500970526     C*
049600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
049700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
049800970526     C                   Z-ADD     D00VER        CPCVER                         *CHIAVE
049900970526     C                   MOVEL     VIDNAR        CPCNAR
050000970526     C                   MOVEL     VIDCAP        CPCCAP
050100000420     C                   EXSR      IMPCPC                                       *ALTRI CAMPI
050200970526     C                   Z-ADD     *ZEROS        CPCDTR                         *DATA TRASMISSIONE
050300970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
050400970526     C                   MOVEL     *BLANKS       CPCATB                         *ANNULLAMENTO
050500980616     C                   MOVEL     'N'           CPCESE                         *ECCEZIONI X LINEA
050600980616     C                   MOVEL     'N'           CPCESL                         *ECCEZIONI X LOC
050700970526     C                   WRITE     AZCPC000
050800000420     C*
050900000420     C                   EXSR      LETCPC
051000000420     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
051100970602     C*
051200970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
051300970602     C                   EXSR      AZZVID
051400970526E   1C                   ENDIF
051500970526     C*
051600970526     C                   ENDSR
051700970526     C*------------------------------------------------------------------------*
051800000420     C* MODIFI - OPERAZIONI DI MODIFICA
051900970526     C*------------------------------------------------------------------------*
052000970526     C     MODIFI        BEGSR
052100970526     C*
052200970526     C* CONTROLLA I CAMPI IMMESSI
052300970526     C                   EXSR      DECODI
052400970526     C*
052500970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
052600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052700000420     C                   EXSR      LETCPC
052800970526     C*
052900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
053000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
053100130913     c* richiamo pgm per cancellare OrariServizi a fronte delle lna cambiate
053200130913     c                   exsr      sr_oraris
053300130913
053400970526     C                   EXSR      IMPCPC                                       *IMPOSTA I CAMPI
053500970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
053600970526     C                   MOVEL     *BLANKS       CPCATB                         *ANNULLAMENTO
053700970526     C                   UPDATE    AZCPC000
053800000420     C*
053900000420     C                   MOVEL     MSG(3)        D00MSG
054000000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
054100970526E   2C                   ENDIF
054200970526E   1C                   ENDIF
054300970526     C*
054400970526     C                   ENDSR
054500970526     C*------------------------------------------------------------------------*
054600970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
054700970526     C*------------------------------------------------------------------------*
054800970526     C     ANNULL        BEGSR
054900130902     C*
055000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
055100000420     C                   EXSR      LETCPC
055200970526     C*
055300970526     C* ANNULLA LE LOCALITA'
055400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
055500970526     C                   Z-ADD     CPCVER        KCLVER
055600970526     C                   MOVEL     CPCNAR        KCLNAR
055700970526     C                   MOVEL     CPCCAP        KCLCAP
055800970917     C     KEYCPL        SETLL     AZCPL01L                           99
055900970526     C     KEYCPL        READE     AZCPL01L                               99
056000970526DO  2C     *IN99         DOWEQ     *OFF
056100970526IF  3C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
056200970526IF  4C     CPLDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
056300970526     C                   MOVEL     'A'           CPLATB                         *FLAG ANNULLAMENTO
056400970526     C                   MOVEL     *BLANKS       CPLFTR                         *FLAG TRASMISSIONE
056500970526     C                   UPDATE    AZCPL000                                     *ANNULLA
056600970526X   4C                   ELSE                                                   *MAI TRASMESSO
056700000426     C                   DELETE    AZCPL000                                     *CANCELLA
056800970526E   4C                   ENDIF
056900970526E   3C                   ENDIF
057000970526     C     KEYCPL        READE     AZCPL01L                               99
057100970526E   2C                   ENDDO                                                  *FINE LOCALITA'
057200000420     C*
057300970526     C* ANNULLA I SINONIMI
057400970526     C                   Z-ADD     D00VER        KCSVER
057500970526     C                   MOVEL     CPCNAR        KCSNAR
057600970526     C                   MOVEL     CPCCAP        KCSCAP
057700970526     C     KEYCPS        SETLL     AZCPS01L                           99
057800970526     C     KEYCPS        READE     AZCPS01L                               99
057900970526DO  2C     *IN99         DOWEQ     *OFF
058000970526IF  3C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
058100970526IF  4C     CPSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
058200970526     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
058300970526     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
058400970526     C                   UPDATE    AZCPS000                                     *ANNULLA
058500970526X   4C                   ELSE                                                   *MAI TRASMESSO
058600970526     C                   DELETE    AZCPS000                                     *CANCELLA
058700970526E   4C                   ENDIF
058800970526E   3C                   ENDIF
058900970526     C     KEYCPS        READE     AZCPS01L                               99
059000970526E   2C                   ENDDO                                                  *FINE SINONIMI
059100970526     C*
059200000602     C* ANNULLA LE ECCEZIONI
059300970526     C                   Z-ADD     CPCVER        KCEVER
059400970526     C                   MOVEL     CPCNAR        KCENAR
059500970526     C                   MOVEL     CPCCAP        KCECAP
059600970526     C     KEYCPE        SETLL     AZCPE01L                           99
059700970526     C     KEYCPE        READE     AZCPE01L                               99
059800970526DO  2C     *IN99         DOWEQ     *OFF
059900970526IF  3C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
060000970526IF  4C     CPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
060100970526     C                   MOVEL     'A'           CPEATB                         *FLAG ANNULLAMENTO
060200970526     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
060300970526     C                   UPDATE    AZCPE000                                     *ANNULLA
060400970526X   4C                   ELSE                                                   *MAI TRASMESSO
060500970526     C                   DELETE    AZCPE000                                     *CANCELLA
060600970526E   4C                   ENDIF
060700970526E   3C                   ENDIF
060800970526     C     KEYCPE        READE     AZCPE01L                               99
060900970526E   2C                   ENDDO                                                  *FINE SINONIMI
061000130913     c*
061100130913     c* ANNULLA GLI ORARI
061200130913     C                   Z-ADD     CPCVER        KCEVER
061300130913     C                   MOVEL     CPCNAR        KCENAR
061400130913     C                   MOVEL     CPCCAP        KCECAP
061500130913     C     KEYCPE        SETLL     AZCos01L                           99
061600130913     C     KEYCPE        READE     AZCos01L                               99
061700130913DO  2C     *IN99         DOWEQ     *OFF
061800130913     C                   DELETE    AZCos000                                     *CANCELLA
061900130913     C     KEYCPE        READE     AZCos01L                               99
062000130913E   2C                   ENDDO                                                  *FINE SINONIMI
062100970526     C*
062200970526     C* ANNULLA IL CAP
062300000602     C                   MOVEL     '0'           ANNDLT            1
062400970526IF  2C     CPCATB        IFEQ      *BLANKS                                      *NO ANNULLATI
062500970526IF  3C     CPCDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
062600970526     C                   MOVEL     'A'           CPCATB                         *FLAG ANNULLAMENTO
062700970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
062800970526     C                   UPDATE    AZCPC000                                     *ANNULLA
062900970526X   3C                   ELSE                                                   *MAI TRASMESSO
063000970526     C                   DELETE    AZCPC000                                     *CANCELLA
063100000602     C                   MOVEL     '1'           ANNDLT
063200970526E   3C                   ENDIF
063300970526E   2C                   ENDIF
063400000420     C*
063500000420     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
063600000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
063700970526E   1C                   ENDIF
063800970526     C*
063900970526     C                   ENDSR
064000970526     C*------------------------------------------------------------------------*
064100000420     C* RIPRIS - OPERAZIONI DI RIPRISTINO
064200970526     C*------------------------------------------------------------------------*
064300970526     C     RIPRIS        BEGSR
064400970526     C*
064500970526     C* CONTROLLA I CAMPI IMMESSI
064600970526     C                   EXSR      DECODI
064700970526     C*
064800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
064900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
065000000420     C                   EXSR      LETCPC
065100970526     C*
065200970526     C* IMPOSTA IL RECORD DA SCRIVERE
065300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
065400970526     C                   MOVEL     *BLANKS       CPCATB                         *FLAG ANNULLAMENTO
065500970526     C                   MOVEL     *BLANKS       CPCFTR                         *FLAG TRASMISSIONE
065600000420     C                   UPDATE    AZCPC000                                     *RIPRISTINA
065700000420     C*
065800970526E   2C                   ENDIF
065900970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
066000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
066100970526E   1C                   ENDIF
066200970526     C*
066300970526     C                   ENDSR
066400970526     C*------------------------------------------------------------------------*
066500970526     C* LETCPC - LEGGE IL RECORD PERL'AGGIORNAMENTO
066600970526     C*------------------------------------------------------------------------*
066700970526     C     LETCPC        BEGSR
066800970526     C*
066900000420     C     DEPOP1        IFNE      'F06'
067000970526     C                   Z-ADD     D00VER        KCCVER
067100160728     C                   MOVEL     vidnar        KCCNAR
067200160728     C                   MOVEL     vidcap        KCCCAP
067300970526     C     KEYCPC        CHAIN     AZCPC01L                           9998
067400970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
067500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
067600970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
067700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
067800970526E   1C                   ENDIF
067900970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
068000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
068100970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
068200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
068300970526E   1C                   ENDIF
068400000420     C                   ENDIF
068500000420     C*
068600000420     C                   ENDSR
068700970526     C*------------------------------------------------------------------------*
068800970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
068900970526     C*------------------------------------------------------------------------*
069000970526     C     DECODI        BEGSR
069100050428     C*
069200050428     C* NO VALORI NEGATIVI
069300050428     C                   IF        VIDLKS < *zeros
069400050428     C                   EVAL      VIDLKS = VIDLKS * -1
069500050428     C                   ENDIF
069600050428     C                   IF        VIDLKG < *zeros
069700050428     C                   EVAL      VIDLKG = VIDLKG * -1
069800050428     C                   ENDIF
069900050428     C                   IF        VIDLK2 < *zeros
070000050428     C                   EVAL      VIDLK2 = VIDLK2 * -1
070100050428     C                   ENDIF
070200050428     C                   IF        VIDLMS < *zeros
070300050428     C                   EVAL      VIDLMS = VIDLMS * -1
070400050428     C                   ENDIF
070500050428     C                   IF        VIDLMC < *zeros
070600050428     C                   EVAL      VIDLMC = VIDLMC * -1
070700050428     C                   ENDIF
070800050428     C                   IF        VIDLM2 < *zeros
070900050428     C                   EVAL      VIDLM2 = VIDLM2 * -1
071000050428     C                   ENDIF
071100050428     C*
071200970527     C*--------------------
071300970526     C* NAZIONE
071400970527     C*--------------------
071500970526     C     *LIKE         DEFINE    §15CPR        DEPCPR
071600970527IF  1C     ERRL00        IFEQ      '0'
071700970526     C                   MOVEL     *BLANKS       VDDNAR
071800970526     C                   MOVEL     VIDNAR        WNAR
071900970526     C                   MOVEL     *BLANKS       WDNAR
072000970526     C                   MOVEL     *BLANKS       WCPR
072100970526     C                   EXSR      CHKNAR
072200970526     C                   MOVEL     WNAR          VIDNAR
072300970526     C                   MOVEL     WDNAR         VDDNAR
072400970526     C                   MOVEL     WCPR          DEPCPR
072500970526IF  2C     ERRL00        IFEQ      '1'
072600970526     C                   SETON                                        60
072700970526     C                   MOVEL     MSG(13)       DSMSMS
072800970526E   2C                   ENDIF
072900970526E   1C                   ENDIF
073000970527     C*--------------------
073100970526     C* CAP
073200970527     C*--------------------
073300970526IF  1C     ERRL00        IFEQ      '0'
073400970526     C                   MOVEL     VIDNAR        WNAR
073500970602     C                   MOVEL     VIDCAP        WCAP
073600970526     C                   EXSR      CHKCAP
073700970526IF  2C     ERRL00        IFEQ      '1'
073800970526     C                   SETON                                        61
073900970526E   2C                   ENDIF
074000970526E   1C                   ENDIF
074100970526     C*--------------------
074200970526     C* NAZIONE + CAP
074300970526     C*--------------------
074400970527     C* INSERIMENTO -> RECORD INESISTENTE
074500970526IF  1C     ERRL00        IFEQ      '0'
074600970527IF  2C     DEPOP1        IFEQ      'F06'
074700970526     C                   Z-ADD     D00VER        KCCVER
074800970526     C                   MOVEL     VIDNAR        KCCNAR
074900970526     C                   MOVEL     VIDCAP        KCCCAP
075000970526     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
075100970526IF  3C     *IN99         IFEQ      *OFF
075200970527     C                   SETON                                        6061
075300970526     C                   MOVEL     '1'           ERRL00
075400970527     C                   MOVEL     MSG(8)        DSMSMS
075500970526E   3C                   ENDIF
075600970526E   2C                   ENDIF
075700970526E   1C                   ENDIF
075800970526     C*--------------------
075900970604     C* LINEA ARRIVO STANDARD
076000970526     C*--------------------
076100970526IF  1C     ERRL00        IFEQ      '0'
076200970526     C                   MOVEL     *BLANKS       VDDLNA
076300970526     C                   MOVEL     VIDLNA        WLIN
076400970526     C                   MOVEL     *BLANKS       WDLIN
076500970526     C                   EXSR      CHKLIN
076600970526     C                   MOVEL     WLIN          VIDLNA
076700970526     C                   MOVEL     WDLIN         VDDLNA
076800970526IF  2C     ERRL00        IFEQ      '1'
076900970526     C                   SETON                                        62
077000970526     C                   MOVEL     MSG(10)       DSMSMS
077100130902     c*
077200130902   x2c                   else
077300130917     c                   if        depop1<>'F06'
077400130902     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
077500130902     c                   movel     vidlna        w0030             3 0
077600130913    3c***                if        w0030 =cpclna
077700130913     c***                clear                   caplna
077800130911     c*****              clear                   loclna
077900130913     c***                clear                   f07lna
078000130913    3c***                endif
078100130902     c
078200130913    3c*                  if        w0030 <>cpclna and vidlna<>caplna
078300130913     c*
078400130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
078500130913     C*                  MOVEL     VIDNAR        cosNAR
078600130913     C*                  MOVEL     VIDCAP        cosCAP
078700130913     C*                  clear                   cosloc
078800130913     C*                  MOVEL     cpclna        coslna
078900130913     c*    kcos          chain     azcos01l
079000130913    4c*                  if        %found(azcos01l)
079100130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
079200130913     c* e non ho già avvisato avvisato della cancellazione orariservizi
079300130913     c                   if        w0030<>cpclna and cpclna<>s_cpclna
079400130913     c                                           and cpclna<>s_cpclos
079500130913     c                                           and cpclna<>s_cpclol
079600130913     c                                           and f07lna=*blanks
079700130913     c                   clear                   tisio7ds
079800130913     c                   eval      io7mod='C'
079900130913     c                   eval      io7liv='C'
080000130913     c                   eval      io7ver=d00ver
080100130913     c                   eval      io7nar=vidnar
080200130913     c                   eval      io7cap=vidcap
080300130913     c                   eval      io7lna=cpclna
080400130913     c                   eval      io7lns=%dec(vidlna:3:0)
080500130913     c                   eval      io7los=%dec(vidlos:3:0)
080600130913     c                   eval      io7lol=%dec(vidlol:3:0)
080700131016     c*****              movel     tisio7ds      kpjbu
080800130913     c                   call      'TISIO7R'
080900130913     c                   parm                    kpjba
081000131016     c                   parm                    tisio7ds
081100131016     c*****              movel     kpjbu         tisio7ds
081200130913     c                   if        oo7err='1'
081300130913     c                   movel     oo7msg        DSmsms
081400130902     c                   eval      *in62  = *on
081500130902     c                   eval      ErrL00 = *on
081600130913     c                   eval      f07lna='S'
081700130902e   4c                   endif
081800130902c
081900130913     c                   eval      s_cpclna=cpclna
082000130913     c***                eval      caplna=vidlna
082100130902    3c                   endif
082200130917    2c                   endif
082300130902     c*
082400130902     c* Verifico se ci sono inserimenti a livello di località per quel CAP
082500130913IF  3C*    ERRL00        IFEQ      '0'
082600130913    4c*                  if        w0030 <>cpclna and vidlna<>caplna
082700130913     c*
082800130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
082900130913     C*                  MOVEL     VIDNAR        cosNAR
083000130913     C*                  MOVEL     VIDCAP        cosCAP
083100130913     C*                  movel     'A'           cosloc
083200130913     C*                  MOVEL     cpclna        coslna
083300130913     c*    kcos2         setll     azcos02l
083400130913     c*    kcos22        reade     azcos02l
083500130913    5c*                  dow       not %eof(azcos02l) and errl00='0'
083600130913     c*                  movel     cosloc        kclloc
083700130913     c*                  movel     coscap        kclcap
083800130913     c*                  movel     cosver        kclver
083900130913     c*                  movel     cosnar        kclnar
084000130903     c* se la trovo, verifico che la località non abbia una eccezione
084100130913     c*    KEYCPL2       chain     azcpl01l
084200130913    6c*                  if        %found(azcpl01l) and
084300130913     c*                            cpllna<>cpclna and cpllol<>cpclna and
084400130913     c*                            cpllos<>cpclna
084500130913     c*
084600130913     c*                  movel     Msg(20)       DSmsms
084700130913     c*                  eval      *in62  = *on
084800130913     c*                  eval      f07lna='S'
084900130913     c*                  eval      ErrL00 = *on
085000130913e   6c*                  endif
085100130913     c*    kcos22        reade     azcos02l
085200130913    5c*                  enddo
085300130903c
085400130913     c*                  eval      caplna=vidlna
085500130903     c*
085600130913E   4C*                  ENDIF
085700130913E   3C*                  ENDIF
085800130903E   2C                   ENDIF
085900130903E   1C                   ENDIF
086000970604     C*--------------------
086100970604     C* ZONA DI CONSEGNA STANDARD
086200970604     C*--------------------
086300970604IF  1C     ERRL00        IFEQ      '0'
086400970604     C                   MOVEL     VIDLNA        WLIN
086500970604     C                   Z-ADD     VIDZNC        WZNC
086600970604     C                   EXSR      CHKZNC
086700970604IF  2C     ERRL00        IFEQ      '1'
086800970604     C                   SETON                                        63
086900970604     C                   MOVEL     MSG(11)       DSMSMS
087000970604E   2C                   ENDIF
087100060206if  2c                   if        §03nuc = 'S'
087200060206     c                   movel     Msg(17)       DSmsms
087300060206     c                   eval      *in63  = *on
087400060206     c                   eval      ErrL00 = *on
087500060206e   2c                   endif
087600970604E   1C                   ENDIF
087700970604     C*--------------------
087800970912     C* LINEA ARRIVO SOTTO
087900970604     C*--------------------
088000970604IF  1C     ERRL00        IFEQ      '0'
088100970604     C                   MOVEL     *BLANKS       VDDLOS
088200970604IF  2C     VIDLOS        IFNE      *ZEROS
088300970604     C     VIDLOS        ANDNE     *BLANKS
088400970604     C                   MOVEL     VIDLOS        WLIN
088500970604     C                   MOVEL     *BLANKS       WDLIN
088600970604     C                   EXSR      CHKLIN
088700970604     C                   MOVEL     WLIN          VIDLOS
088800970604     C                   MOVEL     WDLIN         VDDLOS
088900970604IF  3C     ERRL00        IFEQ      '1'
089000970604     C                   SETON                                        81
089100970604     C                   MOVEL     MSG(10)       DSMSMS
089200130903    3c                   endif
089300130903    2c                   endif
089400130903    1c                   endif
089500130902     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
089600130903IF  1C     ERRL00        IFEQ      '0'
089700130917     c     depop1        andne     'F06'
089800130903    2c                   if        vidlos>*zeros
089900130902     c                   movel     vidlos        w0030             3 0
090000130903     c                   else
090100130903     c                   clear                   w0030
090200130903    2c                   endif
090300130903     c
090400130913    2c***                if        w0030 =cpclos
090500130913     c***                clear                   caplos
090600130911     c****               clear                   loclos
090700130913     c***                clear                   f07los
090800130913    2c***                endif
090900130902     c
091000130913    2c*                  if        w0030 <>cpclos and vidlos<>caplos
091100130902     c
091200130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
091300130913     C*                  MOVEL     VIDNAR        cosNAR
091400130913     C*                  MOVEL     VIDCAP        cosCAP
091500130913     C*                  clear                   cosloc
091600130913     C*                  MOVEL     cpclos        coslna
091700130913     c*    kcos          chain     azcos01l
091800130913    3c*                  if        %found(azcos01l)
091900130913     c*                  movel     Msg(20)       DSmsms
092000130913     c*                  eval      *in81  = *on
092100130913     c*                  eval      ErrL00 = *on
092200130913     c*                  eval      f07los='S'
092300130913e   3c*                  endif
092400130902c
092500130913     c*                  eval      caplos=vidlos
092600130913E   2C*                  ENDIF
092700130913E   1C                   ENDIF
092800130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
092900130913     c* e non ho già avvisato avvisato della cancellazione orariservizi
093000130913    2c                   if        w0030<>cpclos and cpclos<>s_cpclna
093100130913     c                                           and cpclos<>s_cpclos
093200130913     c                                           and cpclos<>s_cpclol
093300130913     c                                           and f07lna=*blanks
093400130913     c                   clear                   tisio7ds
093500130913     c                   eval      io7mod='C'
093600130913     c                   eval      io7liv='C'
093700130913     c                   eval      io7ver=d00ver
093800130913     c                   eval      io7nar=vidnar
093900130913     c                   eval      io7cap=vidcap
094000130913     c                   eval      io7lna=cpclos
094100130913     c                   eval      io7lns=%dec(vidlna:3:0)
094200130913     c                   eval      io7los=%dec(vidlos:3:0)
094300130913     c                   eval      io7lol=%dec(vidlol:3:0)
094400131016     c*****              movel     tisio7ds      kpjbu
094500130913     c                   call      'TISIO7R'
094600130913     c                   parm                    kpjba
094700131016     c                   parm                    tisio7ds
094800131016     c*****              movel     kpjbu         tisio7ds
094900130913    3c                   if        oo7err='1'
095000130913     c                   movel     oo7msg        DSmsms
095100130913     c                   eval      *in81  = *on
095200130913     c                   eval      ErrL00 = *on
095300130913     c                   eval      f07lna='S'
095400130913e   3c                   endif
095500130913c
095600130913     c                   eval      s_cpclos=cpclos
095700130913    2c                   endif
095800130911     c* Verifico se ci sono inserimenti a livello di località per quel CAP
095900130913IF  3C*    ERRL00        IFEQ      '0'
096000130913    4c*                  if        w0030 <>cpclos and vidlos<>caplos
096100130913     c*
096200130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
096300130913     C*                  MOVEL     VIDNAR        cosNAR
096400130913     C*                  MOVEL     VIDlos        cosCAP
096500130913     C*                  movel     'A'           cosloc
096600130913     C*                  MOVEL     cpclos        coslna
096700130913     c*    kcos2         setll     azcos02l
096800130913     c*    kcos22        reade     azcos02l
096900130913    5c*                  dow       not %eof(azcos02l) and errl00='0'
097000130913     c*                  movel     cosloc        kclloc
097100130913     c*                  movel     coscap        kclcap
097200130913     c*                  movel     cosver        kclver
097300130913     c*                  movel     cosnar        kclnar
097400130911     c* se la trovo, verifico che la località non abbia una eccezione
097500130913     c*    KEYCPL2       chain     azcpl01l
097600130913    6c*                  if        %found(azcpl01l) and
097700130913     c*                            cpllna<>cpclos and cpllol<>cpclos and
097800130913     c*                            cpllos<>cpclos
097900130913     c*
098000130913     c*                  movel     Msg(20)       DSmsms
098100130913     c*                  eval      *in81  = *on
098200130913     c*                  eval      f07los='S'
098300130913     c*                  eval      ErrL00 = *on
098400130913e   6c*                  endif
098500130913     c*    kcos22        reade     azcos02l
098600130913    5c*                  enddo
098700130911c
098800130913     c*                  eval      caplos=vidlos
098900130911     c*
099000130913E   4C*                  ENDIF
099100130913E   3C*                  ENDIF
099200970604     C*--------------------
099300970912     C* ZONA DI CONSEGNA SOTTO
099400970604     C*--------------------
099500970604IF  1C     ERRL00        IFEQ      '0'
099600971126IF  2C     VIDLOS        IFNE      *ZEROS
099700970604     C                   MOVEL     VIDLOS        WLIN
099800970604     C                   Z-ADD     VIDZOS        WZNC
099900970604     C                   EXSR      CHKZNC
100000970604IF  3C     ERRL00        IFEQ      '1'
100100970604     C                   SETON                                        82
100200970604     C                   MOVEL     MSG(11)       DSMSMS
100300970604E   3C                   ENDIF
100400060206if  3c                   if        §03nuc = 'S'
100500060206     c                   movel     Msg(17)       DSmsms
100600060206     c                   eval      *in82  = *on
100700060206     c                   eval      ErrL00 = *on
100800060206e   3c                   endif
100900970604E   2C                   ENDIF
101000970604E   1C                   ENDIF
101100970604     C*--------------------
101200970912     C* PESO/VOLUME SOTTO
101300970604     C*--------------------
101400970604IF  1C     ERRL00        IFEQ      '0'
101500970604IF  2C     VIDLKS        IFGT      *ZEROS
101600970604     C     VIDLMS        ORGT      *ZEROS
101700970604IF  3C     VIDLOS        IFEQ      *BLANKS
101800970604     C     VIDLOS        OREQ      *ZEROS
101900970604     C                   SETON                                        81
102000970604     C                   MOVEL     MSG(18)       DSMSMS
102100970604     C                   MOVEL     '1'           ERRL00
102200970604E   3C                   ENDIF
102300070103E   2C                   ENDIF
102400970604IF  3C     VIDLOS        IFNE      *BLANKS
102500970604     C     VIDLOS        ANDNE     *ZEROS
102600070103IF  4C                   IF        VIDLKS = *ZEROS OR
102700070103     C                             VIDLMS = *ZEROS
102800970604     C                   SETON                                        8384
102900970604     C                   MOVEL     MSG(19)       DSMSMS
103000970604     C                   MOVEL     '1'           ERRL00
103100070103E   4C                   ENDIF
103200070103E   3C                   ENDIF
103300970604E   1C                   ENDIF
103400970604     C*--------------------
103500970912     C* LINEA ARRIVO OLTRE
103600970604     C*--------------------
103700970604IF  1C     ERRL00        IFEQ      '0'
103800970604     C                   MOVEL     *BLANKS       VDDLOL
103900970604IF  2C     VIDLOL        IFNE      *ZEROS
104000970604     C     VIDLOL        ANDNE     *BLANKS
104100970604     C                   MOVEL     VIDLOL        WLIN
104200970604     C                   MOVEL     *BLANKS       WDLIN
104300970604     C                   EXSR      CHKLIN
104400970604     C                   MOVEL     WLIN          VIDLOL
104500970604     C                   MOVEL     WDLIN         VDDLOL
104600970604IF  3C     ERRL00        IFEQ      '1'
104700970604     C                   SETON                                        67
104800970604     C                   MOVEL     MSG(10)       DSMSMS
104900970604E   3C                   ENDIF
105000970604E   2C                   ENDIF
105100970604E   1C                   ENDIF
105200130903     c
105300130903     c* Verifico se per la linea modificata ci sono gli orari servizi: andranno eliminati
105400130903IF  1C     ERRL00        IFEQ      '0'
105500130917     c     depop1        andne     'F06'
105600130903    2c                   if        vidlol>*zeros
105700130903     c                   movel     vidlol        w0030             3 0
105800130903     c                   else
105900130903     c                   clear                   w0030
106000130903    2c                   endif
106100130903     c
106200130913    2c***                if        w0030 =cpclol
106300130913     c***                clear                   caplol
106400130911     c***                clear                   loclol
106500130913     c***                clear                   f07lol
106600130913    2c***                endif
106700130903     c
106800130913    2c*                  if        w0030 <>cpclol and vidlol<>caplol
106900130913     c*
107000130913     C*                  Z-ADD     D00VER        cosVER                         *CHIAVE
107100130913     C*                  MOVEL     VIDNAR        cosNAR
107200130913     C*                  MOVEL     VIDCAP        cosCAP
107300130913     C*                  clear                   cosloc
107400130913     C*                  MOVEL     cpclol        coslna
107500130913     c*    kcos          chain     azcos01l
107600130913    3c*                  if        %found(azcos01l)
107700130913     c*                  movel     Msg(20)       DSmsms
107800130913     c*                  eval      *in67  = *on
107900130913     c*                  eval      ErrL00 = *on
108000130913     c*                  eval      f07lol='S'
108100130913e   3c*                  endif
108200130903c
108300130913     c*                  eval      caplol=vidlol
108400130913E   2C*                  ENDIF
108500130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
108600130913     c* e non ho già avvisato avvisato della cancellazione orariservizi
108700130913    2c                   if        w0030<>cpclol and cpclol<>s_cpclna
108800130913     c                                           and cpclol<>s_cpclos
108900130913     c                                           and cpclol<>s_cpclol
109000130913     c                                           and f07lna=*blanks
109100130913     c                   clear                   tisio7ds
109200130913     c                   eval      io7mod='C'
109300130913     c                   eval      io7liv='C'
109400130913     c                   eval      io7ver=d00ver
109500130913     c                   eval      io7nar=vidnar
109600130913     c                   eval      io7cap=vidcap
109700130913     c                   eval      io7lna=cpclol
109800130913     c                   eval      io7lns=%dec(vidlna:3:0)
109900130913     c                   eval      io7los=%dec(vidlos:3:0)
110000130913     c                   eval      io7lol=%dec(vidlol:3:0)
110100131016     c*****              movel     tisio7ds      kpjbu
110200130913     c                   call      'TISIO7R'
110300130913     c                   parm                    kpjba
110400131016     c                   parm                    tisio7ds
110500131016     c*****              movel     kpjbu         tisio7ds
110600130913    3c                   if        oo7err='1'
110700130913     c                   movel     oo7msg        DSmsms
110800130913     c                   eval      *in67  = *on
110900130913     c                   eval      ErrL00 = *on
111000130913     c                   eval      f07lna='S'
111100130913e   3c                   endif
111200130913c
111300130913     c                   eval      s_cpclol=cpclol
111400130913    2c                   endif
111500130903E   1C                   ENDIF
111600970604     C*--------------------
111700970912     C* ZONA DI CONSEGNA OLTRE
111800970604     C*--------------------
111900970604IF  1C     ERRL00        IFEQ      '0'
112000971126IF  2C     VIDLOL        IFNE      *ZEROS
112100970604     C                   MOVEL     VIDLOL        WLIN
112200970604     C                   Z-ADD     VIDZOL        WZNC
112300970604     C                   EXSR      CHKZNC
112400970604IF  3C     ERRL00        IFEQ      '1'
112500970604     C                   SETON                                        68
112600970604     C                   MOVEL     MSG(11)       DSMSMS
112700970604E   3C                   ENDIF
112800060206if  3c                   if        §03nuc = 'S'
112900060206     c                   movel     Msg(17)       DSmsms
113000060206     c                   eval      *in68  = *on
113100060206     c                   eval      ErrL00 = *on
113200060206e   3c                   endif
113300970604E   2C                   ENDIF
113400970604E   1C                   ENDIF
113500970604     C*--------------------
113600970912     C* PESO/VOLUME OLTRE
113700970604     C*--------------------
113800970604IF  1C     ERRL00        IFEQ      '0'
113900970604IF  2C     VIDLKG        IFGT      *ZEROS
114000970604     C     VIDLMC        ORGT      *ZEROS
114100970604IF  3C     VIDLOL        IFEQ      *BLANKS
114200970604     C     VIDLOL        OREQ      *ZEROS
114300970604     C                   SETON                                        67
114400970604     C                   MOVEL     MSG(18)       DSMSMS
114500970604     C                   MOVEL     '1'           ERRL00
114600970604E   3C                   ENDIF
114700070103E   2C                   ENDIF
114800070103IF  3C     VIDLOL        IFNE      *BLANKS
114900070103     C     VIDLOL        ANDNE     *ZEROS
115000070103IF  4C                   IF        VIDLKG = *ZEROS OR
115100070103     C                             VIDLMC = *ZEROS
115200970604     C                   SETON                                        7172
115300970604     C                   MOVEL     MSG(19)       DSMSMS
115400970604     C                   MOVEL     '1'           ERRL00
115500070103E   4C                   ENDIF
115600070103E   3C                   ENDIF
115700970604E   1C                   ENDIF
115800970604     C*--------------------
115900970912     C* PESO OLTRE + PESO SOTTO
116000970604     C*--------------------
116100970912     C* OLTRE > SOTTO
116200970604IF  1C     ERRL00        IFEQ      '0'
116300970604IF  2C     VIDLKG        IFGT      *ZEROS
116400970604     C     VIDLKS        ANDGT     *ZEROS
116500970604IF  3C     VIDLKG        IFLE      VIDLKS
116600970604     C                   SETON                                        7183
116700970604     C                   MOVEL     MSG(21)       DSMSMS
116800970604     C                   MOVEL     '1'           ERRL00
116900970604E   3C                   ENDIF
117000970604E   2C                   ENDIF
117100970604E   1C                   ENDIF
117200970604     C*--------------------
117300970912     C* VOLUME OLTRE + VOLUME SOTTO
117400970604     C*--------------------
117500970912     C* OLTRE > SOTTO
117600970604IF  1C     ERRL00        IFEQ      '0'
117700970604IF  2C     VIDLMC        IFGT      *ZEROS
117800970604     C     VIDLMS        ANDGT     *ZEROS
117900970604IF  3C     VIDLMC        IFLE      VIDLMS
118000970604     C                   SETON                                        7284
118100970604     C                   MOVEL     MSG(21)       DSMSMS
118200970604     C                   MOVEL     '1'           ERRL00
118300970604E   3C                   ENDIF
118400970604E   2C                   ENDIF
118500970604E   1C                   ENDIF
118600971118     C*--------------------
118700971118     C* LINEA ARRIVO OLTRE 2
118800971118     C*--------------------
118900971118IF  1C     ERRL00        IFEQ      '0'
119000971118     C                   MOVEL     *BLANKS       VDDLO2
119100971118IF  2C     VIDLO2        IFNE      *ZEROS
119200971118     C     VIDLO2        ANDNE     *BLANKS
119300971118     C                   MOVEL     VIDLO2        WLIN
119400971118     C                   MOVEL     *BLANKS       WDLIN
119500971118     C                   EXSR      CHKLIN
119600971118     C                   MOVEL     WLIN          VIDLO2
119700971118     C                   MOVEL     WDLIN         VDDLO2
119800971118IF  3C     ERRL00        IFEQ      '1'
119900971118     C                   SETON                                        86
120000971118     C                   MOVEL     MSG(10)       DSMSMS
120100971118E   3C                   ENDIF
120200971118E   2C                   ENDIF
120300971118E   1C                   ENDIF
120400971118     C*--------------------
120500971118     C* ZONA DI CONSEGNA OLTRE 2
120600971118     C*--------------------
120700971118IF  1C     ERRL00        IFEQ      '0'
120800971126IF  2C     VIDLO2        IFNE      *ZEROS
120900971118     C                   MOVEL     VIDLO2        WLIN
121000971118     C                   Z-ADD     VIDZO2        WZNC
121100971118     C                   EXSR      CHKZNC
121200971118IF  3C     ERRL00        IFEQ      '1'
121300971118     C                   SETON                                        87
121400971118     C                   MOVEL     MSG(11)       DSMSMS
121500971118E   3C                   ENDIF
121600060206if  3c                   if        §03nuc = 'S'
121700060206     c                   movel     Msg(17)       DSmsms
121800060206     c                   eval      *in87  = *on
121900060206     c                   eval      ErrL00 = *on
122000060206e   3c                   endif
122100971118E   2C                   ENDIF
122200971118E   1C                   ENDIF
122300970526     C*--------------------
122400970526     C* LINEA ARRIVO FERMO DEPOSITO -OUTPUT-
122500970526     C*--------------------
122600970526IF  1C     ERRL00        IFEQ      '0'
122700971219     C                   MOVEL     VIDLNA        VIDLFD
122800970526     C                   MOVEL     *BLANKS       VDDLFD
122900970526IF  2C     VIDLFD        IFGT      *ZEROS
123000970526     C                   MOVEL     VIDLFD        WLIN
123100970526     C                   MOVEL     *BLANKS       WDLIN
123200970526     C                   EXSR      CHKLIN
123300970526     C                   MOVEL     WLIN          VIDLFD
123400970526     C                   MOVEL     WDLIN         VDDLFD
123500970526E   2C                   ENDIF
123600970526E   1C                   ENDIF
123700970526     C*--------------------
123800970526     C* ZONA DI CONSEGNA FERMO DEPOSITO
123900970526     C*--------------------
124000970526IF  1C     ERRL00        IFEQ      '0'
124100971126IF  2C     VIDLFD        IFNE      *ZEROS
124200970526     C                   MOVEL     VIDLFD        WLIN
124300970526     C                   Z-ADD     VIDZFD        WZNC
124400970526     C                   EXSR      CHKZNC
124500970526IF  3C     ERRL00        IFEQ      '1'
124600970526     C                   SETON                                        66
124700970526     C                   MOVEL     MSG(11)       DSMSMS
124800970526E   3C                   ENDIF
124900060206if  3c                   if        §03nuc = 'S'
125000060206     c                   movel     Msg(17)       DSmsms
125100060206     c                   eval      *in66  = *on
125200060206     c                   eval      ErrL00 = *on
125300060206e   3c                   endif
125400970526E   2C                   ENDIF
125500970526E   1C                   ENDIF
125600970526     C*--------------------
125700970526     C* PROVINCIA
125800970526     C*--------------------
125900970526IF  1C     ERRL00        IFEQ      '0'
126000970526     C                   MOVEL     *BLANKS       VDDPRV
126100970526IF  2C     VIDPRV        IFEQ      *BLANKS                                      *PRV NON IMMESSA
126200970526IF  3C     DEPCPR        IFEQ      'S'                                          *PRV DA CONTROLLARE
126300970526     C                   MOVEL     '1'           ERRL00
126400970526     C                   SETON                                        64
126500970526     C                   MOVEL     MSG(9)        DSMSMS
126600970526E   3C                   ENDIF
126700970526X   2C                   ELSE                                                   *PRV IMMESSA
126800970526     C                   MOVEL     VIDPRV        WPRV
126900970526     C                   MOVEL     *BLANKS       WDPRV
127000970526     C                   EXSR      CHKPRV
127100970526     C                   MOVEL     WPRV          VIDPRV
127200970526     C                   MOVEL     WDPRV         VDDPRV
127300970526IF  3C     ERRL00        IFEQ      '1'
127400970526     C                   SETON                                        64
127500970526     C                   MOVEL     MSG(9)        DSMSMS
127600970526E   3C                   ENDIF
127700970526E   2C                   ENDIF
127800970526E   1C                   ENDIF
127900970526     C*--------------------
128000970526     C* PROVINCIA ALTERNATIVA
128100970526     C*--------------------
128200970526IF  1C     ERRL00        IFEQ      '0'
128300970526     C                   MOVEL     *BLANKS       VDDPRA
128400970526IF  2C     VIDPRA        IFNE      *BLANKS
128500970526     C                   MOVEL     VIDPRA        WPRV
128600970526     C                   MOVEL     *BLANKS       WDPRV
128700970526     C                   EXSR      CHKPRV
128800970526     C                   MOVEL     WPRV          VIDPRA
128900970526     C                   MOVEL     WDPRV         VDDPRA
129000970526IF  3C     ERRL00        IFEQ      '1'
129100970526     C                   SETON                                        65
129200970526     C                   MOVEL     MSG(9)        DSMSMS
129300970526E   3C                   ENDIF
129400970526E   2C                   ENDIF
129500970526E   1C                   ENDIF
129600170511     C*--------------------
129700170511     C* ZONE DI CONSEGNA VS LINEE DI CONSEGNA
129800170511     C*--------------------
129900170511IF  1C     ERRL00        IFEQ      '0'
130000170511     C                   IF        VIDLNA = *zeros AND
130100170511     C                             VIDZNC > *zeros
130200170511     C                   Z-ADD     *zeros        VIDZOS
130300170511     C                   ENDIF
130400170511     C                   IF        VIDLOL = *zeros AND
130500170511     C                             VIDZOL > *zeros
130600170511     C                   Z-ADD     *zeros        VIDZOL
130700170511     C                   ENDIF
130800170511     C                   IF        VIDLOS = *zeros AND
130900170511     C                             VIDZOS > *zeros
131000170511     C                   Z-ADD     *zeros        VIDZOS
131100170511     C                   ENDIF
131200170511     C                   IF        VIDLO2 = *zeros AND
131300170511     C                             VIDZO2 > *zeros
131400170511     C                   Z-ADD     *zeros        VIDZO2
131500170511     C                   ENDIF
131600170511     C                   ENDIF
131700970526     C*--------------------
131800970526     C* CODICE TASSAZIONE
131900970526     C*--------------------
132000970526IF  1C     ERRL00        IFEQ      '0'
132100970526     C                   MOVEL     *BLANKS       VDDCTS
132200970526     C                   MOVEL     VIDCTS        WCTS
132300970526     C                   MOVEL     *BLANKS       WDCTS
132400970526     C                   EXSR      CHKCTS
132500970526     C                   MOVEL     WCTS          VIDCTS
132600970526     C                   MOVEL     WDCTS         VDDCTS
132700970526IF  2C     ERRL00        IFEQ      '1'
132800970526     C                   SETON                                        58
132900970526     C                   MOVEL     MSG(14)       DSMSMS
133000970526E   2C                   ENDIF
133100970526E   1C                   ENDIF
133200970526     C*--------------------
133300970526     C* CODICE TASSAZIONE ITALIA PER ESTERO
133400970526     C*--------------------
133500970526IF  1C     ERRL00        IFEQ      '0'
133600970526     C                   MOVEL     *BLANKS       VDDCTE
133700970526IF  2C     VIDNAR        IFEQ      *BLANKS                                      *NAZIONE ITALIA
133800970526     C                   MOVEL     VIDCTE        WA1D              1
133900970526     C                   MOVE      VIDCTE        WA1S              1
134000970526IF  3C     WA1D          IFNE      'I'
134100970526     C     WA1S          OREQ      *BLANKS
134200970526     C                   MOVEL     '1'           ERRL00
134300970526     C                   SETON                                        59
134400970526     C                   MOVEL     MSG(15)       DSMSMS
134500970526E   3C                   ENDIF
134600970526X   2C                   ELSE                                                   *NAZIONE ESTERO
134700970526IF  3C     VIDCTE        IFNE      *BLANKS
134800970526     C                   MOVEL     '1'           ERRL00
134900970526     C                   SETON                                        59
135000970526     C                   MOVEL     MSG(16)       DSMSMS
135100970526E   3C                   ENDIF
135200970526E   2C                   ENDIF
135300970526E   1C                   ENDIF
135400970526     C*--------------------
135500970526     C* CAP FITTIZIO
135600970526     C*--------------------
135700970526IF  1C     ERRL00        IFEQ      '0'
135800970526IF  2C     VIDFIT        IFEQ      'S'
135900970526     C     VIDFIT        OREQ      'N'
136000970526X   2C                   ELSE
136100970526     C                   MOVEL     '1'           ERRL00
136200970526     C                   SETON                                        73
136300970526     C                   MOVEL     MSG(27)       DSMSMS
136400970526E   2C                   ENDIF
136500970526E   1C                   ENDIF
136600970526     C*--------------------
136700970526     C* CITTA CON VIARIO
136800970526     C*--------------------
136900970526IF  1C     ERRL00        IFEQ      '0'
137000970526IF  2C     VIDFLG        IFEQ      'S'
137100970526     C     VIDFLG        OREQ      'N'
137200970526X   2C                   ELSE
137300970526     C                   MOVEL     '1'           ERRL00
137400970526     C                   SETON                                        74
137500970526     C                   MOVEL     MSG(27)       DSMSMS
137600970526E   2C                   ENDIF
137700970526E   1C                   ENDIF
137800970526     C*--------------------
137900970526     C* INOLTRO
138000970526     C*--------------------
138100970526IF  1C     ERRL00        IFEQ      '0'
138200970526     C                   MOVEL     *BLANKS       VDDISO
138300970526     C                   MOVEL     VIDISO        WINL
138400970526     C                   MOVEL     *BLANKS       WDINL
138500970526     C                   EXSR      CHKINL
138600970526     C                   MOVEL     WINL          VIDISO
138700970526     C                   MOVEL     WDINL         VDDISO
138800970526IF  2C     ERRL00        IFEQ      '1'
138900970526     C                   SETON                                        75
139000970526     C                   MOVEL     MSG(28)       DSMSMS
139100970603X   2C                   ELSE
139200970526E   2C                   ENDIF
139300970526E   1C                   ENDIF
139400970526     C*--------------------
139500970526     C* POSSIBILE INOLTRO ISOLA
139600970526     C*--------------------
139700970526IF  1C     ERRL00        IFEQ      '0'
139800970526IF  2C     VIDPII        IFEQ      'S'
139900970526     C     VIDPII        OREQ      'N'
140000970526X   2C                   ELSE
140100970526     C                   MOVEL     '1'           ERRL00
140200970526     C                   SETON                                        76
140300970526     C                   MOVEL     MSG(27)       DSMSMS
140400970526E   2C                   ENDIF
140500970526E   1C                   ENDIF
140600970526     C*--------------------
140700970526     C* POSSIBILE INOLTRO LOCALITA' DISAGIATA
140800970526     C*--------------------
140900970526IF  1C     ERRL00        IFEQ      '0'
141000970526IF  2C     VIDPID        IFEQ      'S'
141100970526     C     VIDPID        OREQ      'N'
141200970526X   2C                   ELSE
141300970526     C                   MOVEL     '1'           ERRL00
141400970526     C                   SETON                                        77
141500970526     C                   MOVEL     MSG(27)       DSMSMS
141600970526E   2C                   ENDIF
141700970526E   1C                   ENDIF
141800970526     C*--------------------
141900970526     C* POSSIBILE INOLTRO CENTRO STORICO
142000970526     C*--------------------
142100970526IF  1C     ERRL00        IFEQ      '0'
142200970526IF  2C     VIDPIS        IFEQ      'S'
142300970526     C     VIDPIS        OREQ      'N'
142400970526X   2C                   ELSE
142500970526     C                   MOVEL     '1'           ERRL00
142600970526     C                   SETON                                        78
142700970526     C                   MOVEL     MSG(27)       DSMSMS
142800970526E   2C                   ENDIF
142900970526E   1C                   ENDIF
143000970526     C*--------------------
143100970526     C* GESTIONE DDT
143200970526     C*--------------------
143300970526IF  1C     ERRL00        IFEQ      '0'
143400970526IF  2C     VIDGDD        IFEQ      'S'
143500970526     C     VIDGDD        OREQ      'N'
143600970526X   2C                   ELSE
143700970526     C                   MOVEL     '1'           ERRL00
143800970526     C                   SETON                                        79
143900970526     C                   MOVEL     MSG(27)       DSMSMS
144000970526E   2C                   ENDIF
144100970526E   1C                   ENDIF
144200970526     C*--------------------
144300970526     C* TIPO TEMPO CONSEGNA
144400010322     C*--------------------
144500970526IF  1C     ERRL00        IFEQ      '0'
144600970526     C                   MOVEL     VIDTTC        WTTC                           *CODICE TEMPO
144700970526     C                   Z-ADD     *ZEROS        WD0I                           *TEMPI DISTRIBUZIONE
144800970526     C                   Z-ADD     *ZEROS        WD1I
144900970526     C                   Z-ADD     *ZEROS        WD0C
145000970526     C                   Z-ADD     *ZEROS        WD1C
145100970526     C                   Z-ADD     *ZEROS        WDIS
145200970526     C                   Z-ADD     *ZEROS        WDCS
145300970526     C                   Z-ADD     *ZEROS        WDLD
145400090903
145500970526     C                   Z-ADD     *ZEROS        WC0I                           *TEMPI CORRIERE
145600970526     C                   Z-ADD     *ZEROS        WC1I
145700970526     C                   Z-ADD     *ZEROS        WC0C
145800970526     C                   Z-ADD     *ZEROS        WC1C
145900970526     C                   Z-ADD     *ZEROS        WCIS
146000970526     C                   Z-ADD     *ZEROS        WCCS
146100970526     C                   Z-ADD     *ZEROS        WCLD
146200090903
146300970526     C                   Z-ADD     *ZEROS        WE0I                           *TEMPI ESPRESSO
146400970526     C                   Z-ADD     *ZEROS        WE1I
146500970526     C                   Z-ADD     *ZEROS        WE0C
146600970526     C                   Z-ADD     *ZEROS        WE1C
146700970526     C                   Z-ADD     *ZEROS        WEIS
146800970526     C                   Z-ADD     *ZEROS        WECS
146900970526     C                   Z-ADD     *ZEROS        WELD
147000090903
147100090903     C                   Z-ADD     *ZEROS        Wh0I                           *TEMPI H 1030
147200090903     C                   Z-ADD     *ZEROS        Wh1I
147300090903     C                   Z-ADD     *ZEROS        Wh0C
147400090903     C                   Z-ADD     *ZEROS        Wh1C
147500090903     C                   Z-ADD     *ZEROS        WhIS
147600090903     C                   Z-ADD     *ZEROS        WhCS
147700090903     C                   Z-ADD     *ZEROS        WhLD
147800970526     C                   EXSR      CHKTTC
147900970530     C                   MOVEL     WTTC          VIDTTC                         *CODICE TEMPO
148000970526     C                   Z-ADD     WD0I          VIDD0I                         *TEMPI DISTRIBUZIONE
148100970526     C                   Z-ADD     WD1I          VIDD1I
148200970526     C                   Z-ADD     WD0C          VIDD0C
148300970526     C                   Z-ADD     WD1C          VIDD1C
148400970526     C                   Z-ADD     WDIS          VIDDIS
148500970526     C                   Z-ADD     WDCS          VIDDCS
148600970526     C                   Z-ADD     WDLD          VIDDLD
148700090903
148800970526     C                   Z-ADD     WC0I          VIDC0I                         *TEMPI CORRIERE
148900970526     C                   Z-ADD     WC1I          VIDC1I
149000970526     C                   Z-ADD     WC0C          VIDC0C
149100970526     C                   Z-ADD     WC1C          VIDC1C
149200970526     C                   Z-ADD     WCIS          VIDCIS
149300970526     C                   Z-ADD     WCCS          VIDCCS
149400970526     C                   Z-ADD     WCLD          VIDCLD
149500090903
149600970526     C                   Z-ADD     WE0I          VIDE0I                         *TEMPI ESPRESSO
149700970526     C                   Z-ADD     WE1I          VIDE1I
149800970526     C                   Z-ADD     WE0C          VIDE0C
149900970526     C                   Z-ADD     WE1C          VIDE1C
150000970526     C                   Z-ADD     WEIS          VIDEIS
150100970526     C                   Z-ADD     WECS          VIDECS
150200970526     C                   Z-ADD     WELD          VIDELD
150300090903
150400090903     C                   Z-ADD     Wh0I          VIDh0I                         *TEMPI H1030
150500090903     C                   Z-ADD     Wh1I          VIDh1I
150600090903     C                   Z-ADD     Wh0C          VIDh0C
150700090903     C                   Z-ADD     Wh1C          VIDh1C
150800090903     C                   Z-ADD     WhIS          VIDhIS
150900090903     C                   Z-ADD     WhCS          VIDhCS
151000090903     C                   Z-ADD     WhLD          VIDhLD
151100970526IF  2C     ERRL00        IFEQ      '1'
151200970526     C                   SETON                                        80
151300970526     C                   MOVEL     MSG(30)       DSMSMS
151400970526E   2C                   ENDIF
151500970526E   1C                   ENDIF
151600010320     C*--------------------
151700010320     C* TIPO TEMPO RITIRO
151800010320     C*--------------------
151900010320IF  1C     ERRL00        IFEQ      '0'
152000010320     C                   MOVEL     VIDTTR        WTTR                           *CODICE TEMPO
152100010320     C                   Z-ADD     *ZEROS        WD0IR                          *TEMPI DISTRIBUZIONE
152200010320     C                   Z-ADD     *ZEROS        WD1IR
152300010320     C                   Z-ADD     *ZEROS        WD0CR
152400010320     C                   Z-ADD     *ZEROS        WD1CR
152500010320     C                   Z-ADD     *ZEROS        WDISR
152600010320     C                   Z-ADD     *ZEROS        WDCSR
152700010320     C                   Z-ADD     *ZEROS        WDLDR
152800010320     C                   Z-ADD     *ZEROS        WC0IR                          *TEMPI CORRIERE
152900010320     C                   Z-ADD     *ZEROS        WC1IR
153000010320     C                   Z-ADD     *ZEROS        WC0CR
153100010320     C                   Z-ADD     *ZEROS        WC1CR
153200010320     C                   Z-ADD     *ZEROS        WCISR
153300010320     C                   Z-ADD     *ZEROS        WCCSR
153400010320     C                   Z-ADD     *ZEROS        WCLDR
153500010320     C                   Z-ADD     *ZEROS        WE0IR                          *TEMPI ESPRESSO
153600010320     C                   Z-ADD     *ZEROS        WE1IR
153700010320     C                   Z-ADD     *ZEROS        WE0CR
153800010320     C                   Z-ADD     *ZEROS        WE1CR
153900010320     C                   Z-ADD     *ZEROS        WEISR
154000010320     C                   Z-ADD     *ZEROS        WECSR
154100010320     C                   Z-ADD     *ZEROS        WELDR
154200010320     C                   EXSR      CHKTTR
154300010320     C                   MOVEL     WTTR          VIDTTR                         *CODICE TEMPO
154400010320     C                   Z-ADD     WD0IR         VIDD0IR                        *TEMPI DISTRIBUZIONE
154500010320     C                   Z-ADD     WD1IR         VIDD1IR
154600010320     C                   Z-ADD     WD0CR         VIDD0CR
154700010320     C                   Z-ADD     WD1CR         VIDD1CR
154800010320     C                   Z-ADD     WDISR         VIDDISR
154900010320     C                   Z-ADD     WDCSR         VIDDCSR
155000010320     C                   Z-ADD     WDLDR         VIDDLDR
155100010320     C                   Z-ADD     WC0IR         VIDC0IR                        *TEMPI CORRIERE
155200010320     C                   Z-ADD     WC1IR         VIDC1IR
155300010320     C                   Z-ADD     WC0CR         VIDC0CR
155400010320     C                   Z-ADD     WC1CR         VIDC1CR
155500010320     C                   Z-ADD     WCISR         VIDCISR
155600010320     C                   Z-ADD     WCCSR         VIDCCSR
155700010320     C                   Z-ADD     WCLDR         VIDCLDR
155800010320     C                   Z-ADD     WE0IR         VIDE0IR                        *TEMPI ESPRESSO
155900010320     C                   Z-ADD     WE1IR         VIDE1IR
156000010320     C                   Z-ADD     WE0CR         VIDE0CR
156100010320     C                   Z-ADD     WE1CR         VIDE1CR
156200010320     C                   Z-ADD     WEISR         VIDEISR
156300010320     C                   Z-ADD     WECSR         VIDECSR
156400010320     C                   Z-ADD     WELDR         VIDELDR
156500010320IF  2C     ERRL00        IFEQ      '1'
156600010320     C                   SETON                                        44
156700010320     C                   MOVEL     MSG(24)       DSMSMS
156800010320E   2C                   ENDIF
156900010320E   1C                   ENDIF
157000970917     C*--------------------
157100970917     C* OBSOLETO
157200970917     C*--------------------
157300970917IF  1C     ERRL00        IFEQ      '0'
157400970917IF  2C     VIDFI1        IFEQ      'O'
157500970917     C     VIDFI1        OREQ      *BLANKS
157600970917X   2C                   ELSE
157700970917     C                   MOVEL     '1'           ERRL00
157800970917     C                   SETON                                        85
157900970917     C                   MOVEL     MSG(29)       DSMSMS
158000970917E   2C                   ENDIF
158100970917E   1C                   ENDIF
158200970917     C*
158300970917     C* INSERIMENTO -> NON POSSO INSERIRE CAP OBSOLETI
158400970917IF  1C     ERRL00        IFEQ      '0'
158500970917IF  2C     DEPOP1        IFEQ      'F06'
158600970917IF  3C     VIDFI1        IFEQ      'O'
158700970917     C                   SETON                                        85
158800970917     C                   MOVEL     MSG(26)       DSMSMS
158900970917     C                   MOVEL     '1'           ERRL00
159000970917E   3C                   ENDIF
159100970917E   2C                   ENDIF
159200970917E   1C                   ENDIF
159300970917     C*
159400970917     C* MODIFICA -> SE CAP OBSOLETO LO DEVONO ESSERE ANCHE TUTTE LE SUE LOCALITA'
159500970917IF  1C     ERRL00        IFEQ      '0'
159600970917IF  2C     DEPOP1        IFEQ      'O02'
159700970917IF  3C     VIDFI1        IFEQ      'O'
159800970917     C                   MOVEL     'S'           $LOBS
159900970917     C                   Z-ADD     D00VER        KCLVER
160000970917     C                   MOVEL     VIDNAR        KCLNAR
160100970917     C                   MOVEL     VIDCAP        KCLCAP
160200970917     C     KEYCPL        SETLL     AZCPL01L
160300970917     C     KEYCPL        READE(N)  AZCPL01L                               99
160400970917DO  4C     *IN99         DOWEQ     *OFF
160500970917IF  5C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
160600970917IF  6C     CPLFI1        IFNE      'O '                                         *NO OBSOLETO
160700970917     C                   MOVEL     'N'           $LOBS
160800970917     C                   SETON                                        85
160900970917     C                   MOVEL     MSG(25)       DSMSMS
161000970917     C                   MOVEL     '1'           ERRL00
161100970917     C                   LEAVE
161200970917E   6C                   ENDIF
161300970917E   5C                   ENDIF
161400970917     C     KEYCPL        READE(N)  AZCPL01L                               99
161500970917E   4C                   ENDDO                                                  *FINE LOCALITA'
161600970917E   3C                   ENDIF
161700970917E   2C                   ENDIF
161800970917E   1C                   ENDIF
161900060927     C***
162000970526     C                   ENDSR
162100970526     C*------------------------------------------------------------------------*
162200970526     C* CHKNAR - CONTROLLA LA NAZIONE
162300970526     C*------------------------------------------------------------------------*
162400970526     C     CHKNAR        BEGSR
162500970526     C*
162600970526     C     *LIKE         DEFINE    VIDNAR        WNAR
162700970526     C     *LIKE         DEFINE    VDDNAR        WDNAR
162800970526     C     *LIKE         DEFINE    §15CPR        WCPR
162900970526     C*
163000970526     C* RICHIESTO ELENCO
163100970526     C                   SETOFF                                           28
163200970526     C     '?'           SCAN      WNAR                                   28
163300970526IF  1C     *IN28         IFEQ      *ON
163400020906     C                   Z-ADD     1             §KUT              1 0
163500970526     C                   MOVEL     '15'          §COD              2
163600970526     C                   MOVEL     *BLANKS       §KEY              8
163700970526     C                   MOVEL     *BLANKS       §DES             30
163800970526     C                   CALL      'X§TABER'
163900970526     C                   PARM                    §KUT
164000970526     C                   PARM                    §COD
164100970526     C                   PARM                    §KEY
164200970526     C                   PARM                    §DES
164300970526     C                   MOVEL     §KEY          WNAR
164400970526     C                   MOVEL     §DES          WDNAR
164500970526E   1C                   ENDIF
164600970526     C*
164700970526     C* CONTROLLA ESISTENZA CODICE
164800020906     C                   MOVEL     1             KTBKUT
164900970526     C                   MOVEL     '15'          KTBCOD
165000970526     C                   MOVEL(P)  WNAR          KTBKEY
165100970526     C     KEYTAB        CHAIN     TABEL00F                           99
165200970526IF  1C     *IN99         IFEQ      *OFF
165300970526     C     TBLFLG        ANDEQ     *BLANKS
165400970526     C                   MOVEL     TBLUNI        DS15
165500970526     C                   MOVEL     §15DES        WDNAR
165600970526     C                   MOVEL     §15CPR        WCPR
165700970526     C*
165800970526     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
165900970526IF  2C     VIDNAR        IFEQ      'I  '
166000970526     C                   MOVEL     '1'           ERRL00
166100970526     C                   MOVEL     *BLANKS       WDNAR
166200970526E   2C                   ENDIF
166300970526X   1C                   ELSE
166400970526     C                   MOVEL     '1'           ERRL00
166500970526E   1C                   ENDIF
166600970526     C*
166700970526     C                   ENDSR
166800970526     C*------------------------------------------------------------------------*
166900970526     C* CHKCAP - CONTROLLA IL CAP
167000970526     C*------------------------------------------------------------------------*
167100970526     C     CHKCAP        BEGSR
167200970526     C*
167300970526     C     *LIKE         DEFINE    VIDCAP        WCAP
167400970526     C*
167500970626     C* CONTROLLA IL CAP
167600970626     C                   CLEAR                   DSSI95
167700970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
167800970626     C                   MOVEL     '1'           I95TCN                         *CONTROLLO FORMALE
167900970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
168000970626     C                   MOVEL     WCAP          I95CAP                         *CAP
168100970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
168200970626     C                   CALL      'TISI95R'
168300970626     C                   PARM                    DSSI95
168400970626IF  1C     O95ERR        IFNE      *BLANKS
168500970703     C     VIDFIT        ANDNE     'S'
168600970526     C                   MOVEL     '1'           ERRL00
168700970626     C                   MOVEL     O95MSG        DSMSMS
168800970526E   1C                   ENDIF
168900970526     C*
169000970526     C                   ENDSR
169100970526     C*------------------------------------------------------------------------*
169200970526     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
169300970526     C*------------------------------------------------------------------------*
169400970526     C     CHKLIN        BEGSR
169500970526     C*
169600970526     C     *LIKE         DEFINE    VIDLNA        WLIN
169700970526     C     *LIKE         DEFINE    VDDLNA        WDLIN
169800970526     C*
169900970526     C* RICHIESTO ELENCO
170000970526     C                   SETOFF                                           28
170100970526     C     '?'           SCAN      WLIN                                   28
170200970526IF  1C     *IN28         IFEQ      *ON
170300970526     C                   MOVEL     *BLANKS       §COD1             3
170400970526     C                   MOVEL     *BLANKS       §TIP              1
170500970526     C                   MOVEL     *BLANKS       §DES1            25
170600971203     C                   MOVEL     *BLANKS       §PRA              3
170700971203     C                   CALL      'TNSD24R'
170800970526     C                   PARM                    §COD1
170900970526     C                   PARM                    §TIP
171000970526     C                   PARM                    §DES1
171100971203     C                   PARM                    §PRA
171200970526     C                   MOVEL     §COD1         WLIN
171300970526     C                   MOVEL     §DES1         WDLIN
171400970526E   1C                   ENDIF
171500970526     C*
171600970526     C* SOLO CARATTERI NUMERICI
171700970526     C                   SETOFF                                       28
171800970526     C                   TESTN                   WLIN                 28
171900970526IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
172000970526     C                   MOVEL     '1'           ERRL00
172100970526X   1C                   ELSE                                                   *E'NUMERO DA TESTN
172200970526     C                   MOVE      WLIN          W001              1
172300970526IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
172400970526     C                   MOVEL     '1'           ERRL00
172500970526X   2C                   ELSE                                                   *NUMERO
172600970526     C*
172700970526     C* CONTROLLA ESISTENZA CODICE
172800970526     C                   MOVEL     WLIN          KORFIL
172900970526     C     KEYORG        CHAIN     AZORG01L                           99
173000970526IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
173100970526     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
173200970527     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
173300970526     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
173400970526X   3C                   ELSE
173500970526     C                   MOVEL     '1'           ERRL00
173600970526E   3C                   ENDIF
173700970526E   2C                   ENDIF
173800970526E   1C                   ENDIF
173900970526     C*
174000970526     C                   ENDSR
174100970526     C*------------------------------------------------------------------------*
174200970526     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
174300970526     C*------------------------------------------------------------------------*
174400970526     C     CHKZNC        BEGSR
174500970526     C*
174600970526     C     *LIKE         DEFINE    VIDZNC        WZNC
174700970526     C*
174800970526     C* CONTROLLA ESISTENZA CODICE
174900020906     C                   Z-ADD     1             KTBKUT
175000970526     C                   MOVEL     '03'          KTBCOD
175100970526     C                   MOVEL     WLIN          WA5               5
175200970526     C                   MOVE      WZNC          WA5
175300970526     C                   MOVEL(P)  WA5           KTBKEY
175400970526     C     KEYTAB        CHAIN     TABEL00F                           99
175500970526IF  1C     *IN99         IFEQ      *OFF
175600970526     C     TBLFLG        ANDEQ     *BLANKS
175700970526     C                   MOVEL     TBLUNI        DS03
175800970526X   1C                   ELSE
175900970526     C                   MOVEL     '1'           ERRL00
176000970526E   1C                   ENDIF
176100970526     C*
176200970526     C                   ENDSR
176300970526     C*------------------------------------------------------------------------*
176400970526     C* CHKPRV - CONTROLLA LA PROVINCIA
176500970526     C*------------------------------------------------------------------------*
176600970526     C     CHKPRV        BEGSR
176700970526     C*
176800970526     C     *LIKE         DEFINE    VIDPRV        WPRV
176900970526     C     *LIKE         DEFINE    VDDPRV        WDPRV
177000970526     C*
177100970526     C* RICHIESTO ELENCO
177200970526     C                   SETOFF                                           28
177300970526     C     '?'           SCAN      WPRV                                   28
177400970526IF  1C     *IN28         IFEQ      *ON
177500020906     C                   Z-ADD     1             §KUT              1 0
177600970526     C                   MOVEL     'PR'          §COD              2
177700970526     C                   MOVEL     *BLANKS       §KEY              8
177800970526     C                   MOVEL     *BLANKS       §DES             30
177900970526     C                   CALL      'X§TABER'
178000970526     C                   PARM                    §KUT
178100970526     C                   PARM                    §COD
178200970526     C                   PARM                    §KEY
178300970526     C                   PARM                    §DES
178400970526     C                   MOVEL     §KEY          WPRV
178500970526     C                   MOVEL     §DES          WDPRV
178600970526E   1C                   ENDIF
178700970526     C*
178800970526     C* CONTROLLA ESISTENZA CODICE
178900020906     C                   MOVEL     1             KTBKUT
179000970526     C                   MOVEL     'PR'          KTBCOD
179100970526     C                   MOVEL(P)  WPRV          KTBKEY
179200970526     C     KEYTAB        CHAIN     TABEL00F                           99
179300970526IF  1C     *IN99         IFEQ      *OFF
179400970526     C     TBLFLG        ANDEQ     *BLANKS
179500970526     C                   MOVEL     TBLUNI        DSPR
179600970526     C                   MOVEL     §PRDES        WDPRV
179700970526X   1C                   ELSE
179800970526     C                   MOVEL     '1'           ERRL00
179900970526E   1C                   ENDIF
180000970526     C*
180100970526     C                   ENDSR
180200970526     C*------------------------------------------------------------------------*
180300970526     C* CHKCTS - CONTROLLA IL CODICE TASSAZIONE
180400970526     C*------------------------------------------------------------------------*
180500970526     C     CHKCTS        BEGSR
180600970526     C*
180700970526     C     *LIKE         DEFINE    VIDCTS        WCTS
180800970526     C     *LIKE         DEFINE    VDDCTS        WDCTS
180900970526     C*
181000970526     C* RICHIESTO ELENCO
181100970526     C                   SETOFF                                           28
181200970526     C     '?'           SCAN      WCTS                                   28
181300970526IF  1C     *IN28         IFEQ      *ON
181400120712     c                   clear                   TRTB09DS
181500120712     c                   eval      TB09fun = '1'
181600120712     c                   eval      kpjbu = TRTB09DS
181700120712     c                   call      'TRTB09R'
181800120712     c                   parm                    kpjba
181900120712     c                   eval      TRTB09DS = kpjbu
182000120712     c                   eval      wcts  = TB09cod
182100120712     c                   eval      wdcts = TB09des
182200970526E   1C                   ENDIF
182300970526     C*
182400970526     C* CONTROLLA ESISTENZA CODICE
182500020906     C                   MOVEL     1             KTBKUT
182600970526     C                   MOVEL     'CT'          KTBCOD
182700970526     C                   MOVEL(P)  WCTS          KTBKEY
182800970526     C     KEYTAB        CHAIN     TABEL00F                           99
182900970526IF  1C     *IN99         IFEQ      *OFF
183000970526     C     TBLFLG        ANDEQ     *BLANKS
183100970526     C                   MOVEL     TBLUNI        DSCT
183200970526     C                   MOVEL     §CTDES        WDCTS
183300970526X   1C                   ELSE
183400970526     C                   MOVEL     '1'           ERRL00
183500970526E   1C                   ENDIF
183600970526     C*
183700970526     C                   ENDSR
183800970526     C*------------------------------------------------------------------------*
183900970526     C* CHKINL - CONTROLLA L'INOLTRO
184000970526     C*------------------------------------------------------------------------*
184100970526     C     CHKINL        BEGSR
184200970526     C*
184300970526     C     *LIKE         DEFINE    VIDISO        WINL
184400970526     C     *LIKE         DEFINE    VDDISO        WDINL
184500970526     C*
184600970526     C* RICHIESTO ELENCO
184700970526     C                   SETOFF                                           28
184800970526     C     '?'           SCAN      WINL                                   28
184900970526IF  1C     *IN28         IFEQ      *ON
185000020906     C                   Z-ADD     1             §KUT              1 0
185100970526     C                   MOVEL     '9A'          §COD              2
185200970526     C                   MOVEL     *BLANKS       §KEY              8
185300970526     C                   MOVEL     *BLANKS       §DES             30
185400970526     C                   CALL      'X§TABER'
185500970526     C                   PARM                    §KUT
185600970526     C                   PARM                    §COD
185700970526     C                   PARM                    §KEY
185800970526     C                   PARM                    §DES
185900970526     C                   MOVEL     §KEY          WINL
186000970526     C                   MOVEL     §DES          WDINL
186100970526E   1C                   ENDIF
186200970526     C*
186300970526     C* CONTROLLA ESISTENZA CODICE
186400020906     C                   MOVEL     1             KTBKUT
186500970526     C                   MOVEL     '9A'          KTBCOD
186600970526     C                   MOVEL(P)  WINL          KTBKEY
186700970526     C     KEYTAB        CHAIN     TABEL00F                           99
186800970526IF  1C     *IN99         IFEQ      *OFF
186900970526     C     TBLFLG        ANDEQ     *BLANKS
187000970526     C                   MOVEL     TBLUNI        DS9A
187100970526     C                   MOVEL     §9ADES        WDINL
187200970526X   1C                   ELSE
187300970526     C                   MOVEL     '1'           ERRL00
187400970526E   1C                   ENDIF
187500110623     c
187600110623     c* Se non utilizzabile  nel cappario errore
187700110623     c                   if        §9auticpc='N'  and errl00='0'
187800110623     C                   MOVEL     '1'           ERRL00
187900110623E   1C                   ENDIF
188000970526     C*
188100970526     C                   ENDSR
188200970526     C*------------------------------------------------------------------------*
188300970526     C* CHKTTC - CONTROLLA IL TIPO TEMPO CONSEGNA
188400970526     C*------------------------------------------------------------------------*
188500970526     C     CHKTTC        BEGSR
188600970526     C*
188700970526     C     *LIKE         DEFINE    VIDTTC        WTTC                           *TIPO TEMPO
188800970526     C     *LIKE         DEFINE    CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
188900970526     C     *LIKE         DEFINE    CPT1IN        WD1I
189000970526     C     *LIKE         DEFINE    CPT0CN        WD0C
189100970526     C     *LIKE         DEFINE    CPT1CN        WD1C
189200970526     C     *LIKE         DEFINE    CPTTIS        WDIS
189300970526     C     *LIKE         DEFINE    CPTTCS        WDCS
189400970526     C     *LIKE         DEFINE    CPTTLD        WDLD
189500970526     C     *LIKE         DEFINE    CPT0IN        WC0I                           *TEMPI CORRIERE
189600970526     C     *LIKE         DEFINE    CPT1IN        WC1I
189700970526     C     *LIKE         DEFINE    CPT0CN        WC0C
189800970526     C     *LIKE         DEFINE    CPT1CN        WC1C
189900970526     C     *LIKE         DEFINE    CPTTIS        WCIS
190000970526     C     *LIKE         DEFINE    CPTTCS        WCCS
190100970526     C     *LIKE         DEFINE    CPTTLD        WCLD
190200090903
190300970526     C     *LIKE         DEFINE    CPT0IN        WE0I                           *TEMPI ESPRESSO
190400970526     C     *LIKE         DEFINE    CPT1IN        WE1I
190500970526     C     *LIKE         DEFINE    CPT0CN        WE0C
190600970526     C     *LIKE         DEFINE    CPT1CN        WE1C
190700970526     C     *LIKE         DEFINE    CPTTIS        WEIS
190800970526     C     *LIKE         DEFINE    CPTTCS        WECS
190900970526     C     *LIKE         DEFINE    CPTTLD        WELD
191000090903
191100090903     C     *LIKE         DEFINE    CPT0IN        Wh0I                           *TEMPI H1030
191200090903     C     *LIKE         DEFINE    CPT1IN        Wh1I
191300090903     C     *LIKE         DEFINE    CPT0CN        Wh0C
191400090903     C     *LIKE         DEFINE    CPT1CN        Wh1C
191500090903     C     *LIKE         DEFINE    CPTTIS        WhIS
191600090903     C     *LIKE         DEFINE    CPTTCS        WhCS
191700090903     C     *LIKE         DEFINE    CPTTLD        WhLD
191800970526     C*
191900970526     C* RICHIESTA ELENCO CODICI
192000970526     C                   SETOFF                                           28
192100970526     C     '?'           SCAN      WTTC                                   28
192200970526IF  1C     *IN28         IFEQ      *ON
192300970602     C                   MOVEL     *BLANK        DNAR
192400970602     C                   MOVEL     *BLANKS       DCAP
192500970530     C                   MOVEL     'T01'         WOP0
192600970530     C                   MOVEL     *BLANKS       WOP1
192700970530     C                   MOVEL     'TISI41R'     WPGM              8
192800970530     C                   EXSR      LANPGM
192900970530     C                   MOVEL     D00TTC        WTTC
193000010322E   1C                   ENDIF
193100010322     C*
193200050413     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO CONSEGNA
193300010322     C                   MOVEL     VIDTTC        DEP3A             3
193400010322     C     DEP3A         IFNE      '999'
193500970526     C*
193600970526     C* CONTROLLA ESISTENZA CODICI
193700970526     C                   Z-ADD     D00VER        KCTVER
193800970530     C                   MOVEL     WTTC          KCTTTC
193900970526     C                   MOVEL     'D'           KCTTSP
194000970526     C     KEYCPT        CHAIN     AZCPT01L                           99
194100970526IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
194200970526     C                   Z-ADD     CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
194300970526     C                   Z-ADD     CPT1IN        WD1I
194400970526     C                   Z-ADD     CPT0CN        WD0C
194500970526     C                   Z-ADD     CPT1CN        WD1C
194600970526     C                   Z-ADD     CPTTIS        WDIS
194700970526     C                   Z-ADD     CPTTCS        WDCS
194800970526     C                   Z-ADD     CPTTLD        WDLD
194900970526     C                   MOVEL     'C'           KCTTSP
195000970526     C     KEYCPT        CHAIN     AZCPT01L                           99
195100970526IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
195200970526     C                   Z-ADD     CPT0IN        WC0I                           *TEMPI CORRIERE
195300970526     C                   Z-ADD     CPT1IN        WC1I
195400970526     C                   Z-ADD     CPT0CN        WC0C
195500970526     C                   Z-ADD     CPT1CN        WC1C
195600970526     C                   Z-ADD     CPTTIS        WCIS
195700970526     C                   Z-ADD     CPTTCS        WCCS
195800970526     C                   Z-ADD     CPTTLD        WCLD
195900970526     C                   MOVEL     'E'           KCTTSP
196000970526     C     KEYCPT        CHAIN     AZCPT01L                           99
196100970526IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
196200970526     C                   Z-ADD     CPT0IN        WE0I                           *TEMPI ESPRESSO
196300970526     C                   Z-ADD     CPT1IN        WE1I
196400970526     C                   Z-ADD     CPT0CN        WE0C
196500970526     C                   Z-ADD     CPT1CN        WE1C
196600970526     C                   Z-ADD     CPTTIS        WEIS
196700970526     C                   Z-ADD     CPTTCS        WECS
196800970526     C                   Z-ADD     CPTTLD        WELD
196900090903     C                   MOVEL     'H'           KCTTSP
197000090903     C     KEYCPT        CHAIN     AZCPT01L                           99
197100090903IF  4C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
197200090903     C                   Z-ADD     CPT0IN        Wh0I                           *TEMPI H1030
197300090903     C                   Z-ADD     CPT1IN        Wh1I
197400090903     C                   Z-ADD     CPT0CN        Wh0C
197500090903     C                   Z-ADD     CPT1CN        Wh1C
197600090903     C                   Z-ADD     CPTTIS        WhIS
197700090903     C                   Z-ADD     CPTTCS        WhCS
197800090903     C                   Z-ADD     CPTTLD        WhLD
197900090903X   4C                   ELSE
198000090903     c* Se tipo servizio H non ancora decorrente (anche guardando alla
198100090903     c*    scadenza della versione in visualizzazione)non do errore e non
198200090903     c*    visualizzo nemmeno come descrizione
198300090903
198400170116     c**                 if        Datasys>=dataiso
198500090903     c* Verifico anche la  scadenza ddella versione
198600170116     c**   d00ver        chain     azcpr01l
198700170116     c**                 if        not %found(azcpr01l) or cprdsc>=§vpotsph
198800170116
198900170116     C                   MOVEL     '1'           ERRL00
199000170116
199100170116     c**                 endif
199200170116     c**                 endif
199300090903     c
199400170116     c**                 if        errl00='0'
199500170116     c**                 clear                   viddtsph
199600170116E   4C**                 ENDIF
199700090903
199800090903E   4C                   ENDIF
199900090903
200000090903X   3C                   ELSE
200100090903     C                   MOVEL     '1'           ERRL00
200200090903E   3C                   ENDIF
200300090903
200400970603X   2C                   ELSE
200500970603     C                   MOVEL     '1'           ERRL00
200600970526E   2C                   ENDIF
200700090903
200800970603X   1C                   ELSE
200900970603     C                   MOVEL     '1'           ERRL00
201000970526E   1C                   ENDIF
201100090903
201200010322     C                   ELSE
201300010322     C                   MOVEL     '1'           ERRL00
201400010322     C                   ENDIF
201500970526     C*
201600970526     C                   ENDSR
201700010320     C*------------------------------------------------------------------------*
201800010320     C* CHKTTR - CONTROLLA IL TIPO TEMPO RITIRO
201900010320     C*------------------------------------------------------------------------*
202000010320     C     CHKTTR        BEGSR
202100010320     C*
202200010320     C     *LIKE         DEFINE    VIDTTR        WTTR                           *TIPO TEMPO
202300010320     C     *LIKE         DEFINE    CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
202400010320     C     *LIKE         DEFINE    CPT1IN        WD1IR
202500010320     C     *LIKE         DEFINE    CPT0CN        WD0CR
202600010320     C     *LIKE         DEFINE    CPT1CN        WD1CR
202700010320     C     *LIKE         DEFINE    CPTTIS        WDISR
202800010320     C     *LIKE         DEFINE    CPTTCS        WDCSR
202900010320     C     *LIKE         DEFINE    CPTTLD        WDLDR
203000010320     C     *LIKE         DEFINE    CPT0IN        WC0IR                          *TEMPI CORRIERE
203100010320     C     *LIKE         DEFINE    CPT1IN        WC1IR
203200010320     C     *LIKE         DEFINE    CPT0CN        WC0CR
203300010320     C     *LIKE         DEFINE    CPT1CN        WC1CR
203400010320     C     *LIKE         DEFINE    CPTTIS        WCISR
203500010320     C     *LIKE         DEFINE    CPTTCS        WCCSR
203600010320     C     *LIKE         DEFINE    CPTTLD        WCLDR
203700010320     C     *LIKE         DEFINE    CPT0IN        WE0IR                          *TEMPI ESPRESSO
203800010320     C     *LIKE         DEFINE    CPT1IN        WE1IR
203900010320     C     *LIKE         DEFINE    CPT0CN        WE0CR
204000010320     C     *LIKE         DEFINE    CPT1CN        WE1CR
204100010320     C     *LIKE         DEFINE    CPTTIS        WEISR
204200010320     C     *LIKE         DEFINE    CPTTCS        WECSR
204300010320     C     *LIKE         DEFINE    CPTTLD        WELDR
204400010320     C*
204500010320     C* RICHIESTA ELENCO CODICI
204600010320     C                   SETOFF                                           28
204700010320     C     '?'           SCAN      WTTR                                   28
204800010320IF  1C     *IN28         IFEQ      *ON
204900010320     C                   MOVEL     *BLANK        DNAR
205000010320     C                   MOVEL     *BLANKS       DCAP
205100010320     C                   MOVEL     'T01'         WOP0
205200010320     C                   MOVEL     *BLANKS       WOP1
205300010323     C                   MOVEL     'TISI41R'     WPGM              8
205400010320     C                   EXSR      LANPGM
205500010320     C                   MOVEL     D00TTC        WTTR
205600010320E   1C                   ENDIF
205700010320     C*
205800010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO RITIRO
205900010322     C                   MOVEL     VIDTTR        DEP3A             3
206000010322     C     DEP3A         IFEQ      '999'
206100010320     C* CONTROLLA ESISTENZA CODICI
206200010320     C                   Z-ADD     D00VER        KCTVER
206300010320     C                   MOVEL     WTTR          KCTTTC
206400010320     C                   MOVEL     'D'           KCTTSP
206500010320     C     KEYCPT        CHAIN     AZCPT01L                           99
206600010320IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
206700010320     C                   Z-ADD     CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
206800010320     C                   Z-ADD     CPT1IN        WD1IR
206900010320     C                   Z-ADD     CPT0CN        WD0CR
207000010320     C                   Z-ADD     CPT1CN        WD1CR
207100010320     C                   Z-ADD     CPTTIS        WDISR
207200010320     C                   Z-ADD     CPTTCS        WDCSR
207300010320     C                   Z-ADD     CPTTLD        WDLDR
207400010320     C                   MOVEL     'C'           KCTTSP
207500010320     C     KEYCPT        CHAIN     AZCPT01L                           99
207600010320IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
207700010320     C                   Z-ADD     CPT0IN        WC0IR                          *TEMPI CORRIERE
207800010320     C                   Z-ADD     CPT1IN        WC1IR
207900010320     C                   Z-ADD     CPT0CN        WC0CR
208000010320     C                   Z-ADD     CPT1CN        WC1CR
208100010320     C                   Z-ADD     CPTTIS        WCISR
208200010320     C                   Z-ADD     CPTTCS        WCCSR
208300010320     C                   Z-ADD     CPTTLD        WCLDR
208400010320     C                   MOVEL     'E'           KCTTSP
208500010320     C     KEYCPT        CHAIN     AZCPT01L                           99
208600010320IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
208700010320     C                   Z-ADD     CPT0IN        WE0IR                          *TEMPI ESPRESSO
208800010320     C                   Z-ADD     CPT1IN        WE1IR
208900010320     C                   Z-ADD     CPT0CN        WE0CR
209000010320     C                   Z-ADD     CPT1CN        WE1CR
209100010320     C                   Z-ADD     CPTTIS        WEISR
209200010320     C                   Z-ADD     CPTTCS        WECSR
209300010320     C                   Z-ADD     CPTTLD        WELDR
209400010320X   3C                   ELSE
209500010320     C                   MOVEL     '1'           ERRL00
209600010320E   3C                   ENDIF
209700010320X   2C                   ELSE
209800010320     C                   MOVEL     '1'           ERRL00
209900010320E   2C                   ENDIF
210000010320X   1C                   ELSE
210100010320     C                   MOVEL     '1'           ERRL00
210200010320E   1C                   ENDIF
210300010322     C                   ELSE
210400010322     C                   MOVEL     '1'           ERRL00
210500010322     C                   ENDIF
210600010320     C*
210700010320     C                   ENDSR
210800970526     C*------------------------------------------------------------------------*
210900970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
211000970526     C*------------------------------------------------------------------------*
211100970526     C     IREVID        BEGSR
211200970526     C* CAMPI
211300970526IF  1C     CPCESL        IFEQ      'S'
211400970526     C                   MOVEL     'LOC'         VDDESL
211500970526E   1C                   ENDIF
211600970526IF  1C     CPCESE        IFEQ      'S'
211700970526     C                   MOVEL     'ECC'         VDDESE
211800970526E   1C                   ENDIF
211900970526     C                   MOVEL     CPCNAR        VIDNAR
212000970526     C                   MOVEL     CPCCAP        VIDCAP
212100970526     C                   MOVEL     CPCLNA        VIDLNA
212200970526     C                   MOVEL     CPCLOL        VIDLOL
212300971118     C                   MOVEL     CPCLO2        VIDLO2
212400970604     C                   MOVEL     CPCLOS        VIDLOS
212500970526     C                   MOVEL     CPCLNA        VIDLFD
212600970526     C                   Z-ADD     CPCZNC        VIDZNC
212700970526     C                   Z-ADD     CPCZOL        VIDZOL
212800971118     C                   Z-ADD     CPCZO2        VIDZO2
212900970604     C                   Z-ADD     CPCZOS        VIDZOS
213000970526     C                   Z-ADD     CPCZFD        VIDZFD
213100970526     C                   Z-ADD     CPCLKG        VIDLKG
213200971118     C                   Z-ADD     CPCLK2        VIDLK2
213300970526     C                   Z-ADD     CPCLMC        VIDLMC
213400971118     C                   Z-ADD     CPCLM2        VIDLM2
213500970604     C                   Z-ADD     CPCLKS        VIDLKS
213600970604     C                   Z-ADD     CPCLMS        VIDLMS
213700970526     C                   MOVEL     CPCPRV        VIDPRV
213800970526     C                   MOVEL     CPCPRA        VIDPRA
213900970526     C                   MOVEL     CPCCTS        VIDCTS
214000970526     C                   MOVEL     CPCCTE        VIDCTE
214100970526     C                   MOVEL     CPCFIT        VIDFIT
214200970526     C                   MOVEL     CPCFLG        VIDFLG
214300970526     C                   MOVEL     CPCISO        VIDISO
214400970526     C                   MOVEL     CPCPII        VIDPII
214500970526     C                   MOVEL     CPCPID        VIDPID
214600970526     C                   MOVEL     CPCPIS        VIDPIS
214700970526     C                   MOVEL     CPCGDD        VIDGDD
214800970526     C                   MOVEL     CPCTTC        VIDTTC
214900970917     C                   MOVEL     CPCFI1        VIDFI1
215000010320     C                   MOVEL     *all'9'       VIDTTR
215100010320     C                   MOVE      CPCFI2        VIDTTR
215200000420     C*
215300060927     c*** 27/09/06 - EDPES: campi poste non più usati
215400970526     C*
215500970526     C                   ENDSR
215600970526     C*------------------------------------------------------------------------*
215700970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
215800970526     C*------------------------------------------------------------------------*
215900970526     C     AZZVID        BEGSR
216000970526     C* CAMPI
216100970526     C                   MOVEL     *BLANKS       VDDESL
216200970526     C                   MOVEL     *BLANKS       VDDESE
216300970526     C                   MOVEL     *BLANKS       VIDNAR
216400970526     C                   MOVEL     *BLANKS       VIDCAP
216500970526     C                   MOVEL     *ZEROS        VIDLNA
216600970526     C                   MOVEL     *ZEROS        VIDLOL
216700971118     C                   MOVEL     *ZEROS        VIDLO2
216800970604     C                   MOVEL     *ZEROS        VIDLOS
216900970526     C                   MOVEL     *ZEROS        VIDLFD
217000970526     C                   Z-ADD     *ZEROS        VIDZNC
217100970526     C                   Z-ADD     *ZEROS        VIDZOL
217200971118     C                   Z-ADD     *ZEROS        VIDZO2
217300970604     C                   Z-ADD     *ZEROS        VIDZOS
217400970526     C                   Z-ADD     *ZEROS        VIDZFD
217500970526     C                   Z-ADD     *ZEROS        VIDLKG
217600971118     C                   Z-ADD     *ZEROS        VIDLK2
217700970526     C                   Z-ADD     *ZEROS        VIDLMC
217800971118     C                   Z-ADD     *ZEROS        VIDLM2
217900970604     C                   Z-ADD     *ZEROS        VIDLKS
218000970604     C                   Z-ADD     *ZEROS        VIDLMS
218100970526     C                   MOVEL     *BLANKS       VIDPRV
218200970526     C                   MOVEL     *BLANKS       VIDPRA
218300970526     C                   MOVEL     *BLANKS       VIDCTS
218400970526     C                   MOVEL     *BLANKS       VIDCTE
218500970526     C                   MOVEL     'N'           VIDFIT
218600970526     C                   MOVEL     'N'           VIDFLG
218700970526     C                   MOVEL     *BLANKS       VIDISO
218800970526     C                   MOVEL     'N'           VIDPII
218900970526     C                   MOVEL     'N'           VIDPID
219000970526     C                   MOVEL     'N'           VIDPIS
219100970526     C                   MOVEL     'N'           VIDGDD
219200970526     C                   MOVEL     *BLANKS       VIDTTC
219300970917     C                   MOVEL     *BLANKS       VIDFI1
219400970526     C* DECODIFICHE
219500970526     C                   MOVEL     *BLANKS       VDDNAR
219600970526     C                   MOVEL     *BLANKS       VDDLNA
219700970526     C                   MOVEL     *BLANKS       VDDLOL
219800971118     C                   MOVEL     *BLANKS       VDDLO2
219900970604     C                   MOVEL     *BLANKS       VDDLOS
220000970526     C                   MOVEL     *BLANKS       VDDLFD
220100970526     C                   MOVEL     *BLANKS       VDDPRV
220200970526     C                   MOVEL     *BLANKS       VDDPRA
220300970526     C                   MOVEL     *BLANKS       VDDCTS
220400970526     C                   MOVEL     *BLANKS       VDDCTE
220500970526     C                   MOVEL     *BLANKS       VDDISO
220600000426     C                   MOVEL     *BLANKS       VDPLNA
220700130902     c
220800130913     c*****              clear                   caplna
220900130913     c*****              clear                   loclna
221000130903     c                   clear                   caplos
221100130903     c                   clear                   loclos
221200130903     c                   clear                   caplol
221300130903     c                   clear                   loclol
221400130903     c                   clear                   F07los            1
221500130903     c                   clear                   F07lna            1
221600130903     c                   clear                   F07lol            1
221700130913     c                   clear                   s_cpclna
221800130913     c                   clear                   s_cpclos
221900130913     c                   clear                   s_cpclol
222000970526     C*
222100970526     C                   ENDSR
222200970526     C*------------------------------------------------------------------------*
222300970526     C* IMPCPC - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
222400970526     C*------------------------------------------------------------------------*
222500970526     C     IMPCPC        BEGSR
222600970526     C*
222700970526     C                   MOVEL     VIDLNA        CPCLNA
222800970526     C                   MOVEL     VIDLOL        CPCLOL
222900971118     C                   MOVEL     VIDLO2        CPCLO2
223000970604     C                   MOVEL     VIDLOS        CPCLOS
223100970526     C                   Z-ADD     VIDZNC        CPCZNC
223200970526     C                   Z-ADD     VIDZOL        CPCZOL
223300971118     C                   Z-ADD     VIDZO2        CPCZO2
223400970604     C                   Z-ADD     VIDZOS        CPCZOS
223500970526     C                   Z-ADD     VIDZFD        CPCZFD
223600970526     C                   Z-ADD     VIDLKG        CPCLKG
223700971118     C                   Z-ADD     VIDLK2        CPCLK2
223800970526     C                   Z-ADD     VIDLMC        CPCLMC
223900971118     C                   Z-ADD     VIDLM2        CPCLM2
224000970604     C                   Z-ADD     VIDLKS        CPCLKS
224100970604     C                   Z-ADD     VIDLMS        CPCLMS
224200970526     C                   MOVEL     VIDPRV        CPCPRV
224300970526     C                   MOVEL     VIDPRA        CPCPRA
224400970526     C                   MOVEL     VIDCTS        CPCCTS
224500970526     C                   MOVEL     VIDCTE        CPCCTE
224600970526     C                   MOVEL     VIDFIT        CPCFIT
224700970526     C                   MOVEL     VIDFLG        CPCFLG
224800970526     C                   MOVEL     VIDISO        CPCISO
224900970526     C                   MOVEL     VIDPII        CPCPII
225000970603IF  3C     CPCISO        IFEQ      'I'
225100970603     C                   MOVEL     'S'           CPCPII
225200970603E   3C                   ENDIF
225300970603     C                   MOVEL     VIDPID        CPCPID
225400970603IF  3C     CPCISO        IFEQ      'D'
225500970603     C                   MOVEL     'S'           CPCPID
225600970603E   3C                   ENDIF
225700970603     C                   MOVEL     VIDPIS        CPCPIS
225800970603IF  3C     CPCISO        IFEQ      'S'
225900970603     C                   MOVEL     'S'           CPCPIS
226000970603E   3C                   ENDIF
226100970526     C                   MOVEL     VIDGDD        CPCGDD
226200970526     C                   MOVEL     VIDTTC        CPCTTC
226300970917     C                   MOVEL     VIDFI1        CPCFI1
226400010320     C                   MOVE(p)   VIDTTR        CPCFI2
226500970526     C*
226600970526     C                   ENDSR
226700970530     C*------------------------------------------------------------------------*
226800970530     C* LANPGM - LANCIA UN PROGRAMMA
226900970530     C*------------------------------------------------------------------------*
227000970530     C     LANPGM        BEGSR
227100970530     C*
227200970530     C* IMPOSTA LA DS DI PROCEDURA
227300970530     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
227400970530     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
227500970530     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
227600970530     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
227700970530     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
227800970530     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
227900970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
228000970602     C                   MOVEL     DCAP          D00CAP                         *CAP
228100970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
228200970530     C                   MOVEL     *BLANKS       D00TSP
228300970530     C                   Z-ADD     *ZEROS        D00TFP
228400970530     C                   MOVEL     *BLANKS       D00LOC
228500970603     C                   MOVEL     *BLANKS       D00SIN
228600970530     C                   MOVEL     *BLANKS       D00EPA
228700970530     C                   Z-ADD     *ZEROS        D00ETP
228800970530     C                   Z-ADD     *ZEROS        D00ETA
228900970530     C                   Z-ADD     *ZEROS        D00EDD
229000970530     C                   MOVEL     *BLANKS       D00SPT
229100970530     C                   Z-ADD     *ZEROS        D00SLP
229200970530     C                   Z-ADD     *ZEROS        D00SLA
229300970530     C                   MOVEL     *BLANKS       D00STS
229400970530     C                   Z-ADD     *ZEROS        D00SDD
229500970530     C*
229600970530     C                   CALL      WPGM
229700970612     C                   PARM                    KPJBA
229800970612     C                   PARM                    DSSI00
229900970530     C*
230000970530     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
230100970530IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
230200970530     C                   MOVEL     '1'           ERRL00                         *ERRORE
230300970530     C                   MOVEL     '1'           F03L00                         *FINE PGM
230400970530X   1C                   ELSE                                                   *NON PREMUTO F03
230500970530     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
230600970530E   1C                   ENDIF
230700970530     C*
230800970530     C                   ENDSR
230900970526     C*------------------------------------------------------------------------*
231000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
231100970526     C*------------------------------------------------------------------------*
231200970526     C     IMPFUN        BEGSR
231300970603     C*
231400970603     C* SE INTERROGAZIONE/VISUALIZZAZIONE TOGLE LA GESTIONE ALLE ZONE DI CONSEGNA
231500970603IF  1C     DEPOP0        IFEQ      'C05'
231600970603     C     DEPOP1        OREQ      'O05'
231700970603     C                   MOVEL     FUD(14)       FUDRIG                         *AGGIORNAMENTO
231800970603     C                   MOVEL     'N'           FUDATT
231900970603     C                   MOVEL     FUDRIG        FUD(14)
232000970603E   1C                   ENDIF
232100970526     C*
232200970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
232300970526IF  1C     DEPOP1        IFEQ      'O05'
232400170116     c     wabiute       oreq      'I'
232500170116     c     wabiute       oreq      'O'
232600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
232700970526     C                   MOVEL     'N'           FUDATT
232800970526     C                   MOVEL     FUDRIG        FUD(6)
232900970526E   1C                   ENDIF
233000970526     C*
233100970526     C* SE INSERIMENTO TOGLIE LE FUNZIONI DI ACCESSO AGLI ALTRI ARCHIVI
233200970526IF  1C     DEPOP1        IFEQ      'F06'
233300970526     C                   MOVEL     FUD(8)        FUDRIG                         *LOCALITA'
233400970526     C                   MOVEL     'N'           FUDATT
233500970526     C                   MOVEL     FUDRIG        FUD(8)
233600970527     C                   MOVEL     FUD(9)        FUDRIG                         *ECCEZIONI
233700970526     C                   MOVEL     'N'           FUDATT
233800970526     C                   MOVEL     FUDRIG        FUD(9)
233900970527     C                   MOVEL     FUD(10)       FUDRIG                         *TEMPI
234000970527     C                   MOVEL     'N'           FUDATT
234100970527     C                   MOVEL     FUDRIG        FUD(10)
234200970526E   1C                   ENDIF
234300131007     c
234400131007     c* Se utente non abilitato, non attivo F7 e F17
234500131007     c* lascio in manutenzione
234600131007     c                   if        *in10 and  depop0 = 'C05'
234700131007     C                   MOVEL     FUD(7)        FUDRIG                         *LOCALITA'
234800131007     C                   MOVEL     'N'           FUDATT
234900131007     C                   MOVEL     FUDRIG        FUD(7)
235000131007     C                   MOVEL     FUD(17)       FUDRIG                         *LOCALITA'
235100131007     C                   MOVEL     'N'           FUDATT
235200131007     C                   MOVEL     FUDRIG        FUD(17)
235300131007     c                   endif
235400970526     C*
235500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
235600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
235700970526     C                   EXSR      FUNGES
235800000925     C*
235900000925     C* VERIFICO CHE SOLO UTENTI "EDP*" POSSANO MODIFICARE IL CAMPO "UFFICIO POSTALE"
236000000925     C                   MOVEL     KNMUS         UTE               3
236100000925     C     UTE           IFNE      'EDP'
236200000925     C                   SETON                                        95
236300000925     C                   ENDIF
236400000925     C*
236500970526     C*
236600970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
236700970526     C*---
236800970526     C* 90 -> PROTEZIONE CHIAVE
236900970526     C* 91 -> PROTEZIONE TOTALE
237000050414     C* 93 -> PROTEZIONE COD.TASSAZIONE e TIPO INOLTRO e altri
237100970526     C*---
237200050413     C*
237300050413     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
237400050413     C                   SETON                                        93
237500170116     c
237600170116     c
237700170116     c                   if        wabiute='V'
237800170116     c* se manut completa solo se  czppario ancora da decorrere, verifico con
237900170116     c*  la versione
238000170116     c     d00ver        chain     azcpr01l
238100170116     c                   if        %found(azcpr01l)
238200170116     c     *iso          movel     cprdde        dataiso
238300170116     c                   if        dataiso>datasys
238400170116     c                   setoff                                       93
238500170116     c                   else
238600170116     c                   eval      wabiute='I'
238700170116     c                   endif
238800170116     c
238900170116     c                   else
239000170116     c                   eval      wabiute='I'
239100170116     c                   endif
239200170116     c                   endif
239300050413     C*
239400050413     C* VERIFICO X PRIMISSIMA COSA LE AUTORIZZAZIONI SPECIALI SULL'UTENTE CORRENTE
239500170116     C                   IF        wabiute = *blanks  or wabiute='M'
239600050413     C* TUTTO RIMANE INVARIATO
239700050413     C                   ENDIF
239800050413     C*
239900170116     C                   IF        wabiute = 'I'  or wabiute='O'
240000140206     C* FORZO SOLO INTERROGAZIONE ma permetto di modificare gli orari se c'e' "O"
240100050413     C                   EVAL      DEPOP1 = 'O05'
240200050413     C                   ENDIF
240300050413     C*
240400170116     C                   IF        wabiute = 'C'
240500050413     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
240600050413     C                   SETOFF                                       93
240700050413     C                   ENDIF
240800050413     C*
240900970526IF  1C     DEPOP0        IFEQ      'C02'                                        G E S T I O N E
241000970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
241100970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
241200970526     C                   SETOFF                                       91
241300970526E   2C                   ENDIF
241400970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
241500970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
241600970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
241700970526E   2C                   ENDIF
241800970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
241900970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
242000970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
242100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
242200970526     C                   SETON                                        91
242300970526E   2C                   ENDIF
242400970526X   1C                   ELSE                                                   *I N T E R R O G A
242500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
242600970526     C                   SETON                                        91
242700970526E   1C                   ENDIF
242800970526     C*
242900970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
243000970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
243100970526     C                   Z-ADD     D00VER        KCCVER
243200970526     C                   MOVEL     D00NAR        KCCNAR
243300970526     C                   MOVEL     D00CAP        KCCCAP
243400970526     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
243500970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
243600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
243700970526     C                   MOVEL     MSG(6)        DSMSMS
243800970526X   2C                   ELSE
243900970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
244000980128IF  3C     DEPOP1        IFEQ      'O05'
244100980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
244200980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
244300980128E   3C                   ENDIF
244400980128E   2C                   ENDIF
244500970526X   1C                   ELSE                                                   *INSERIMENTO
244600970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
244700970526E   1C                   ENDIF
244800970526     C*
244900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
245000970526IF  1C     DEPOP1        IFEQ      'O02'
245100970526     C                   MOVEL     DEC(1)        VIDFUN
245200970526E   1C                   ENDIF
245300970526IF  1C     DEPOP1        IFEQ      'O04'
245400970526     C                   MOVEL     DEC(2)        VIDFUN
245500970526E   1C                   ENDIF
245600970526IF  1C     DEPOP1        IFEQ      'O05'
245700970526     C                   MOVEL     DEC(3)        VIDFUN
245800970526E   1C                   ENDIF
245900970526IF  1C     DEPOP1        IFEQ      'O07'
246000970526     C                   MOVEL     DEC(4)        VIDFUN
246100970526E   1C                   ENDIF
246200970526IF  1C     DEPOP1        IFEQ      'F06'
246300970526     C                   MOVEL     DEC(5)        VIDFUN
246400970526E   1C                   ENDIF
246500970526     C*
246600970526     C                   ENDSR
246700130913     C*------------------------------------------------------------------------*
246800130913     C* Richiamo pgm annullamento orari servizi a fronte di modifica lna
246900130913     C*------------------------------------------------------------------------*
247000130913     c     sr_oraris     begsr
247100130913
247200130913     C* LINEA STANDARD
247300130913     c                   movel     vidlna        w0030             3 0
247400130913     c* Se cambiata la linea
247500130913     c                   if        w0030<>cpclna
247600130913     c                   clear                   tisio7ds
247700130913     c                   eval      io7mod='D'
247800130913     c                   eval      io7liv='C'
247900130913     c                   eval      io7ver=d00ver
248000130913     c                   eval      io7nar=vidnar
248100130913     c                   eval      io7cap=vidcap
248200130913     c                   eval      io7lna=cpclna
248300130913     c                   eval      io7lns=%dec(vidlna:3:0)
248400130913     c                   eval      io7los=%dec(vidlos:3:0)
248500130913     c                   eval      io7lol=%dec(vidlol:3:0)
248600131016     c******             movel     tisio7ds      kpjbu
248700130913     c                   call      'TISIO7R'
248800130913     c                   parm                    kpjba
248900131016     c                   parm                    tisio7ds
249000130913    3c                   endif
249100130913     C* LINEA SOTTO
249200130913     c                   movel     vidlos        w0030             3 0
249300130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
249400130913     c                   if        w0030<>cpclos and cpclos<>cpclna
249500130913     c                   clear                   tisio7ds
249600130913     c                   eval      io7mod='D'
249700130913     c                   eval      io7liv='C'
249800130913     c                   eval      io7ver=d00ver
249900130913     c                   eval      io7nar=vidnar
250000130913     c                   eval      io7cap=vidcap
250100130913     c                   eval      io7lna=cpclos
250200130913     c                   eval      io7lns=%dec(vidlna:3:0)
250300130913     c                   eval      io7los=%dec(vidlos:3:0)
250400130913     c                   eval      io7lol=%dec(vidlol:3:0)
250500131016     c******             movel     tisio7ds      kpjbu
250600130913     c                   call      'TISIO7R'
250700130913     c                   parm                    kpjba
250800131016     c                   parm                    tisio7ds
250900130913    3c                   endif
251000130913     C* LINEA OLTRE
251100130913     c                   movel     vidlol        w0030             3 0
251200130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
251300130913     c                   if        w0030<>cpclol and cpclol<>cpclna
251400130913     c                                           and cpclol<>cpclos
251500130913     c                   clear                   tisio7ds
251600130913     c                   eval      io7mod='D'
251700130913     c                   eval      io7liv='C'
251800130913     c                   eval      io7ver=d00ver
251900130913     c                   eval      io7nar=vidnar
252000130913     c                   eval      io7cap=vidcap
252100130913     c                   eval      io7lna=cpclol
252200130913     c                   eval      io7lns=%dec(vidlna:3:0)
252300130913     c                   eval      io7los=%dec(vidlos:3:0)
252400130913     c                   eval      io7lol=%dec(vidlol:3:0)
252500131016     c*****              movel     tisio7ds      kpjbu
252600130913     c                   call      'TISIO7R'
252700130913     c                   parm                    kpjba
252800131016     c                   parm                    tisio7ds
252900130913    3c                   endif
253000130913     c                   endsr
253100970703     C*------------------------------------------------------------------------*
253200970703     C* FINSR - OPERAZIONI FINALI
253300970703     C*------------------------------------------------------------------------*
253400970703     C     FINSR         BEGSR
253500970703     C*
253600970704     C                   CLEAR                   DSSI95
253700970703     C                   MOVEL     'C'           I95TLA
253800970703     C                   CALL      'TISI95R'
253900970703     C                   PARM                    DSSI95
254000130913     c*
254100130913     c                   clear                   tisio7ds
254200130913     C                   MOVEL     'C'           Io7TLA
254300131016     c******             movel     tisio7ds      kpjbu
254400130913     C                   CALL      'TISIO7R'
254500130913     C                   PARM                    kpjba
254600131016     c                   parm                    tisio7ds
254700970703     C*
254800970703     C                   ENDSR
254900050413     C*--------------------------------------------------------------------------------------------*
255000050413     C* REPERISCE DATI UTENTE
255100050413     C*--------------------------------------------------------------------------------------------*
255200050413     C     REPDATIUTE    BEGSR
255300050413     C*
255400050413     C* REimposta le variabili di lavoro
255500050413     C                   CLEAR                   TIBS34DS
255600050413     C                   CLEAR                   AZUTEDS
255700050413     C                   CLEAR                   DDATIUTE
255800050413     C*
255900050413     C* Reperisce i dati del profilo in ingresso
256000050413     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
256100050413     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
256200050413     C                   IN(E)     *DTAARA
256300050413     C                   IF        %Error                                       *NO errori
256400050413     C                   EVAL      I34TLA = 'L'
256500050413     C                   CALL      'TIBS34R'
256600050413     C                   PARM                    TIBS34DS
256700050413     C                   IN        *DTAARA
256800050413     C                   ENDIF
256900050413     C*
257000050413     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
257100050413     C                   EVAL      DUTE01 = UTEFAF
257200050413     C*
257300050413     C                   ENDSR
257400970526     C*------------------------------------------------------------------------*
257500970526     C* *INZSR - OPERAZIONI INIZIALI
257600970526     C*------------------------------------------------------------------------*
257700970526     C     *INZSR        BEGSR
257800970526     C*---
257900970526     C* RICEVIMENTO PARAMETRI
258000970526     C*---
258100970526     C     *ENTRY        PLIST
258200970612     C                   PARM                    KPJBA
258300970612     C                   PARM                    DSSI00
258400170116
258500050413     C*
258600050413     C* REPERISCE I DATI DELL'UTENTE
258700050413     C                   EXSR      REPDATIUTE
258800060206     c                   movel     RSUT          DSfirs
258900170116
259000170116     c                   eval      wabiute=§utecap
259100970526     C*
259200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
259300970526     C     XMSGPA        PLIST
259400970526     C                   PARM                    DSMSMS
259500970526     C                   PARM                    DSMSPG
259600970526     C                   PARM                    DSMSKE
259700970526     C                   PARM                    DSMSER
259800131007      /free
259900131007        // Carico filiali di test abilitate
260000131007         if dutpou<>46  ;
260100131007
260200131007         clear TIBS02ds;
260300131007         T02Mod = 'C';
260400131007         T02sif = knsif;
260500131007         T02cod = 'VPO';
260600131007         t02tla = 'L'   ;
260700131007         T02ke1 ='DECOFI816' ;
260800131007         TNTBE_RicercaControllo  (kpjba : tibs02ds);
260900131007
261000131007         if  T02err  = *blank;
261100131007         xx= 1 ;
261200131007         yy= 1 ;
261300131007
261400131007         dow xx< 256   ;
261500131007           fil_ab(yy)= %subst(t02uni:xx:3)  ;
261600131007           yy=yy+ 1   ;
261700131007           xx=xx+ 3   ;
261800131007         enddo  ;
261900131007
262000131007         endif;
262100131007
262200131007         // se c'e' 999 tutte abilitate
262300131007         w003a='999'  ;
262400131007         if   %lookup(w003a:fil_ab) = 0  ;
262500131007         w003a=%editc(dutpou:'X')   ;
262600131007         if   %lookup(w003a:fil_ab) = 0  ;
262700131007         *in10 =*on  ;
262800131007         endif  ;
262900131007         endif  ;
263000131007
263100131007         endif  ;
263200131007
263300131007      /end-free
263400131007
263500090903     c*
263600090903     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
263700170116     c**                 clear                   tibs02ds
263800170116     c**                 clear                   dvpodeco
263900170116     c**                 eval      t02mod = 'C'
264000170116     c**                 eval      t02sif = knsif
264100170116     c**                 eval      t02cod = 'VPO'
264200170116     c**                 eval      t02ke1 = 'DECO'
264300170116     c**                 call      'TIBS02R'
264400170116     c**                 parm                    kpjba
264500170116     c**                 parm                    tibs02ds
264600170116     c**                 if        t02err = *blanks
264700170116     c**                 eval      dvpodeco = t02uni
264800170116     c**                 else
264900170116     c**                 eval      §vpotsph=20091001
265000170116     c**                 endif
265100090903
265200170116     c**   *iso          movel     §vpotsph      dataiso
265300090903     C*
265400000426     C*
265500000426     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
265600000426     C     D00NAR        IFNE      *BLANKS
265700000426     C                   SETON                                        92
265800000426     C                   ELSE
265900000426     C                   SETOFF                                       92
266000000426     C                   ENDIF
266100970526     C*
266200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
266300970526     C                   CALL      'XKEY'
266400970526     C                   PARM                    DSKEY
266500970526     C*---
266600970526     C* VARIABILI NON RIFERITE AL DATA BASE
266700970526     C*---
266800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
266900970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
267000970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
267100970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
267200970526     C     *LIKE         DEFINE    D00OP1        WOP1
267300970602     C     *LIKE         DEFINE    D00NAR        DNAR
267400970602     C     *LIKE         DEFINE    D00CAP        DCAP
267500970526     C*---
267600970526     C* VARIABILI DI CONTROLLO
267700970526     C*---
267800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
267900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
268000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
268100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
268200970917     C                   MOVEL     'S'           $LOBS             1            *LOCALITA' OBSOLETE
268300970526     C*
268400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
268500970526     C                   MOVEL     'TISI03R'     DSMSPG
268600970526     C                   MOVEL     'TISI03R'     MSGQUE
268700970526     C                   MOVEL     *BLANKS       MSGKEY
268800970526     C                   MOVEL     *BLANKS       DSMSMS
268900970526     C*---
269000970526     C* VARIABILI RIFERITE AL DATA BASE
269100970526     C*---
269200970526     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
269300970526     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
269400970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
269500970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
269600970526     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
269700970526     C     *LIKE         DEFINE    CPCNAR        KCCNAR
269800970526     C     *LIKE         DEFINE    CPCCAP        KCCCAP
269900970526     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
270000970526     C     *LIKE         DEFINE    CPENAR        KCENAR
270100970526     C     *LIKE         DEFINE    CPECAP        KCECAP
270200970526     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
270300970526     C     *LIKE         DEFINE    CPLNAR        KCLNAR
270400970526     C     *LIKE         DEFINE    CPLCAP        KCLCAP
270500130903     C     *LIKE         DEFINE    CPLloc        KCLloc
270600970526     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
270700970526     C     *LIKE         DEFINE    CPSNAR        KCSNAR
270800970526     C     *LIKE         DEFINE    CPSCAP        KCSCAP
270900970526     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
271000970526     C     *LIKE         DEFINE    CPTTTC        KCTTTC
271100970526     C     *LIKE         DEFINE    CPTTSP        KCTTSP
271200970526     C*---
271300970526     C* CHIAVI DI LETTURA
271400970526     C*---
271500970526     C* LETTURA AZORG01L - COMPLETA
271600970526     C     KEYORG        KLIST
271700970526     C                   KFLD                    KORFIL                         *FILIALE
271800970526     C* LETTURA TABEL00F - COMPLETA
271900970526     C     KEYTAB        KLIST
272000970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
272100970526     C                   KFLD                    KTBCOD                         *TABELLA
272200970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
272300970526     C* LETTURA TABEL00F - PARZIALE
272400970526     C     KE2TAB        KLIST
272500970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
272600970526     C                   KFLD                    KTBCOD                         *TABELLA
272700970526     C* LETTURA AZCPC01L - COMPLETA
272800970526     C     KEYCPC        KLIST
272900970526     C                   KFLD                    KCCVER                         *VERSIONE
273000970526     C                   KFLD                    KCCNAR                         *NAZIONE
273100970526     C                   KFLD                    KCCCAP                         *CAP
273200970527     C* LETTURA AZCPT01L - COMPLETA
273300970526     C     KEYCPT        KLIST
273400970526     C                   KFLD                    KCTVER                         *VERSIONE
273500970526     C                   KFLD                    KCTTTC                         *TEMPO CONSEGNA
273600970526     C                   KFLD                    KCTTSP                         *TIPO SERVIZIO
273700970527     C* LETTURA AZCPE01L - PARZIALE
273800970527     C     KEYCPE        KLIST
273900970527     C                   KFLD                    KCEVER                         *VERSIONE
274000970527     C                   KFLD                    KCENAR                         *NAZIONE
274100970527     C                   KFLD                    KCECAP                         *CAP
274200970526     C* LETTURA AZCPL01L - PARZIALE
274300970526     C     KEYCPL        KLIST
274400970526     C                   KFLD                    KCLVER                         *VERSIONE
274500970526     C                   KFLD                    KCLNAR                         *NAZIONE
274600970526     C                   KFLD                    KCLCAP                         *CAP
274700130903     C* LETTURA AZCPL01L - PARZIALE
274800130903     C     KEYCPL2       KLIST
274900130903     C                   KFLD                    KCLVER                         *VERSIONE
275000130903     C                   KFLD                    KCLNAR                         *NAZIONE
275100130903     C                   KFLD                    KCLCAP                         *CAP
275200130903     C                   KFLD                    KCLloc                         *CAP
275300970526     C* LETTURA AZCPS01L - PARZIALE
275400970526     C     KEYCPS        KLIST
275500970526     C                   KFLD                    KCSVER                         *VERSIONE
275600970526     C                   KFLD                    KCSNAR                         *NAZIONE
275700970526     C                   KFLD                    KCSCAP                         *CAP
275800130902     C*
275900130902     C     Kcos          KLIST
276000130903     C                   KFLD                    cosVER
276100130903     C                   KFLD                    cosnar
276200130903     C                   KFLD                    coscap
276300130903     C                   KFLD                    cosloc
276400130903     C                   KFLD                    coslna
276500130903     C     Kcos2         KLIST
276600130903     C                   KFLD                    cosVER
276700130903     C                   KFLD                    coslna
276800130903     C                   KFLD                    cosnar
276900130903     C                   KFLD                    coscap
277000130903     C                   KFLD                    cosloc
277100130903     C     Kcos22        KLIST
277200130903     C                   KFLD                    cosVER
277300130903     C                   KFLD                    coslna
277400130903     C                   KFLD                    cosnar
277500130903     C                   KFLD                    coscap
277600970526     C*
277700970526     C* DEPOSITA LE OPZIONI DI ENTRATA
277800970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
277900970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
278000970526     C*
278100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
278200970526     C                   EXSR      IMPFUN
278300970526     C*
278400970526     C                   ENDSR
278500970526     C*------------------------------------------------------------------------*
278600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
278700970526     C*------------------------------------------------------------------------*
278800970526     C     FUNGES        BEGSR
278900970526     C*
279000970526     C* PULISCO CAMPI DI LAVORO
279100970526     C                   Z-ADD     0             W                 5 0
279200970526     C*
279300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
279400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
279500970526     C     1             DO        24            I
279600970526     C                   MOVEL     FUD(I)        FUDRIG
279700970526     C*
279800970526     C* PULIZIA FUNZIONI PRECEDENTI
279900970526     C                   MOVEL     *BLANKS       FUV(I)
280000970526     C*
280100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
280200970526     C* (SALTA F24)
280300970526     C     FUDATT        IFEQ      'S'
280400970526     C     FUDFUN        ANDNE     '24'
280500970526     C                   ADD       1             W
280600970526     C                   MOVEL     FUDDES        FUV(W)
280700970526     C                   END
280800970526     C                   END                                                    DO WHILE
280900970526     C*
281000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
281100970526     C* ALTRIMENTI DISATTIVO F24
281200970526     C     1             DO        24            I
281300970526     C                   MOVEL     FUD(I)        FUDRIG
281400970526     C*
281500970526     C     FUDFUN        IFEQ      '24'
281600970526     C     W             IFGT      6
281700970526     C                   MOVEL     'S'           FUDATT
281800970526     C                   ELSE
281900970526     C                   MOVEL     *BLANKS       FUDATT
282000970526     C                   END                                                    W GT 8
282100970526     C                   MOVEL     FUDRIG        FUD(I)
282200970526     C                   END
282300970526     C                   END                                                    DO WHILE
282400970526     C*
282500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
282600970526     C     W             DIV       6             I
282700970526     C                   MVR                     Z
282800970526     C*
282900970526     C     Z             IFNE      0
283000970526     C     I             ADD       1             Z
283100970526     C                   ELSE
283200970526     C                   Z-ADD     I             Z                 5 0
283300970526     C                   END
283400970526     C*
283500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
283600970526     C                   ADD       1             FUNGRU
283700970526     C     FUNGRU        IFGT      Z
283800970526     C     FUNGRU        ORLT      1
283900970526     C                   Z-ADD     1             FUNGRU
284000970526     C                   END
284100970526     C*
284200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
284300970526     C     FUNGRU        IFEQ      1
284400970526     C                   MOVEL     FUV(1)        FUN001
284500970526     C                   MOVEL     FUV(2)        FUN002
284600970526     C                   MOVEL     FUV(3)        FUN003
284700970526     C                   MOVEL     FUV(4)        FUN004
284800970526     C                   MOVEL     FUV(5)        FUN005
284900970526     C                   MOVEL     FUV(6)        FUN006
285000970526     C                   END
285100970526     C*
285200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
285300970526     C     FUNGRU        IFEQ      2
285400970526     C                   MOVEL     FUV(7)        FUN001
285500970526     C                   MOVEL     FUV(8)        FUN002
285600970526     C                   MOVEL     FUV(9)        FUN003
285700970526     C                   MOVEL     FUV(10)       FUN004
285800970526     C                   MOVEL     FUV(11)       FUN005
285900970526     C                   MOVEL     FUV(12)       FUN006
286000970526     C                   END
286100970526     C*
286200970526     C* TERZO GRUPPO DI 6 FUNZIONI
286300970526     C     FUNGRU        IFEQ      3
286400970526     C                   MOVEL     FUV(13)       FUN001
286500970526     C                   MOVEL     FUV(14)       FUN002
286600970526     C                   MOVEL     FUV(15)       FUN003
286700970526     C                   MOVEL     FUV(16)       FUN004
286800970526     C                   MOVEL     FUV(17)       FUN005
286900970526     C                   MOVEL     FUV(18)       FUN006
287000970526     C                   END
287100970526     C*
287200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
287300970526     C     FUNGRU        IFEQ      4
287400970526     C                   MOVEL     FUV(19)       FUN001
287500970526     C                   MOVEL     FUV(20)       FUN002
287600970526     C                   MOVEL     FUV(21)       FUN003
287700970526     C                   MOVEL     FUV(22)       FUN004
287800970526     C                   MOVEL     *BLANKS       FUN005
287900970526     C                   MOVEL     *BLANKS       FUN006
288000970526     C                   END
288100970526     C*
288200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
288300970526     C                   MOVEL     *BLANKS       FUN007
288400970526     C                   MOVEL     *BLANKS       FUN008
288500970526     C*
288600970526     C     1             DO        24            I
288700970526     C                   MOVEL     FUD(I)        FUDRIG
288800970526     C     FUDFUN        IFEQ      '24'
288900970526     C     FUDATT        ANDEQ     'S'
289000970526     C                   MOVEL     FUDDES        FUN008
289100970526     C                   END
289200970526     C                   END                                                    DO
289300970526     C*
289400970526     C                   ENDSR
289500970526     C*------------------------------------------------------------------------*
289600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
289700970526     C*------------------------------------------------------------------------*
289800970526     C     FUNCON        BEGSR
289900970526     C*
290000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
290100970526     C                   EXSR      FUNVAR
290200970526     C*
290300970526     C* PULISCO CAMPI DI LAVORO
290400970526     C                   Z-ADD     0             I                 5 0
290500970526     C                   Z-ADD     0             W                 5 0
290600970526     C*
290700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
290800970526     C     FUNKEY        IFNE      *BLANKS
290900970526     C     1             DO        24            I
291000970526     C                   MOVEL     FUD(I)        FUDRIG
291100970526     C*
291200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
291300970526     C     FUNKEY        IFEQ      FUDFUN
291400970526     C     FUDATT        ANDEQ     'S'
291500970526     C                   Z-ADD     I             W
291600970526     C                   END
291700970526     C                   END                                                    DO WHILE
291800970526     C*
291900970526     C* FUNZIONE NON ATTIVA (ERRORE)
292000970526     C     W             IFEQ      0
292100970526     C*
292200970526     C* EMISSIONE MESSAGGIO DI ERRORE
292300970526     C                   MOVEL     MSG(1)        DSMSMS
292400970526     C*
292500970526     C                   END                                                    W=0
292600970526     C                   END                                                    FUNKEY NE *BLANKS
292700970526     C*
292800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
292900970526     C     W             IFNE      0
293000970526     C     FUNKEY        OREQ      *BLANKS
293100970526     C     FUNKEY        IFEQ      *BLANKS
293200970526     C                   EXSR      ENTGES
293300970526     C                   END
293400970526     C     FUNKEY        IFEQ      '03'
293500970526     C                   EXSR      F03GES
293600970526     C                   END
293700010320     C     FUNKEY        IFEQ      '04'
293800010320     C                   EXSR      F04GES
293900010320     C                   END
294000130821     C     FUNKEY        IFEQ      '06'
294100130905     c***                if        f07lna='S' or f07los='S'
294200130905     c***                          or f07lol='S'
294300130905     C***                EXSR      F07GES
294400130905     c***                if        errl00='0'
294500130905     c***                clear                   f07lna
294600130905     c***                clear                   f07lol
294700130905     c***                clear                   f07los
294800130905     c***                endif
294900130903     c
295000130905     c***                else
295100130821     C                   EXSR      F06GES
295200130905     c***                endif
295300130902     c
295400130821     C                   END
295500130827     C     FUNKEY        IFEQ      '07'
295600130827     C                   EXSR      F07GES
295700130827     C                   END
295800970526     C     FUNKEY        IFEQ      '08'
295900970526     C                   EXSR      F08GES
296000970526     C                   END
296100970526     C     FUNKEY        IFEQ      '09'
296200970526     C                   EXSR      F09GES
296300970526     C                   END
296400970526     C     FUNKEY        IFEQ      '10'
296500970526     C                   EXSR      F10GES
296600970526     C                   END
296700970526     C     FUNKEY        IFEQ      '12'
296800970526     C                   EXSR      F12GES
296900970526     C                   END
297000970603     C     FUNKEY        IFEQ      '14'
297100970603     C                   EXSR      F14GES
297200970603     C                   END
297300130828     C     FUNKEY        IFEQ      '17'
297400130828     C                   EXSR      F17GES
297500130828     C                   END
297600970526     C     FUNKEY        IFEQ      '24'
297700970526     C                   EXSR      F24GES
297800970526     C                   END
297900970526     C                   END
298000970526     C*
298100970526     C                   ENDSR
298200970526     C*------------------------------------------------------------------------*
298300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
298400970526     C*------------------------------------------------------------------------*
298500970526     C     FUNVAR        BEGSR
298600970526     C*
298700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
298800970526     C*
298900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
299000970526     C     KEY           IFEQ      ENTER
299100970526     C                   MOVEL     *BLANKS       FUNKEY
299200970526     C                   END
299300970526     C     KEY           IFEQ      F1
299400970526     C                   MOVEL     '01'          FUNKEY
299500970526     C                   END
299600970526     C     KEY           IFEQ      F2
299700970526     C                   MOVEL     '02'          FUNKEY
299800970526     C                   END
299900970526     C     KEY           IFEQ      F3
300000970526     C                   MOVEL     '03'          FUNKEY
300100970526     C                   END
300200970526     C     KEY           IFEQ      F4
300300970526     C                   MOVEL     '04'          FUNKEY
300400970526     C                   END
300500970526     C     KEY           IFEQ      F5
300600970526     C                   MOVEL     '05'          FUNKEY
300700970526     C                   END
300800970526     C     KEY           IFEQ      F6
300900970526     C                   MOVEL     '06'          FUNKEY
301000970526     C                   END
301100970526     C     KEY           IFEQ      F7
301200970526     C                   MOVEL     '07'          FUNKEY
301300970526     C                   END
301400970526     C     KEY           IFEQ      F8
301500970526     C                   MOVEL     '08'          FUNKEY
301600970526     C                   END
301700970526     C     KEY           IFEQ      F9
301800970526     C                   MOVEL     '09'          FUNKEY
301900970526     C                   END
302000970526     C     KEY           IFEQ      F10
302100970526     C                   MOVEL     '10'          FUNKEY
302200970526     C                   END
302300970526     C     KEY           IFEQ      F11
302400970526     C                   MOVEL     '11'          FUNKEY
302500970526     C                   END
302600970526     C     KEY           IFEQ      F12
302700970526     C                   MOVEL     '12'          FUNKEY
302800970526     C                   END
302900970526     C     KEY           IFEQ      F13
303000970526     C                   MOVEL     '13'          FUNKEY
303100970526     C                   END
303200970526     C     KEY           IFEQ      F14
303300970526     C                   MOVEL     '14'          FUNKEY
303400970526     C                   END
303500970526     C     KEY           IFEQ      F15
303600970526     C                   MOVEL     '15'          FUNKEY
303700970526     C                   END
303800970526     C     KEY           IFEQ      F16
303900970526     C                   MOVEL     '16'          FUNKEY
304000970526     C                   END
304100970526     C     KEY           IFEQ      F17
304200970526     C                   MOVEL     '17'          FUNKEY
304300970526     C                   END
304400970526     C     KEY           IFEQ      F18
304500970526     C                   MOVEL     '18'          FUNKEY
304600970526     C                   END
304700970526     C     KEY           IFEQ      F19
304800970526     C                   MOVEL     '19'          FUNKEY
304900970526     C                   END
305000970526     C     KEY           IFEQ      F20
305100970526     C                   MOVEL     '20'          FUNKEY
305200970526     C                   END
305300970526     C     KEY           IFEQ      F21
305400970526     C                   MOVEL     '21'          FUNKEY
305500970526     C                   END
305600970526     C     KEY           IFEQ      F22
305700970526     C                   MOVEL     '22'          FUNKEY
305800970526     C                   END
305900970526     C     KEY           IFEQ      F23
306000970526     C                   MOVEL     '23'          FUNKEY
306100970526     C                   END
306200970526     C     KEY           IFEQ      F24
306300970526     C                   MOVEL     '24'          FUNKEY
306400970526     C                   END
306500970526     C     KEY           IFEQ      ROLLUP
306600970526     C                   MOVEL     'RU'          FUNKEY
306700970526     C                   END
306800970526     C     KEY           IFEQ      HELP
306900970526     C                   MOVEL     'HP'          FUNKEY
307000970526     C                   END
307100970526     C                   ENDSR
307200970526     C*------------------------------------------------------------------------*
307300970526     C* NOTE COSTRUZIONE SCHIERE
307400970526     C*------------------------------------------------------------------------*
307500970526     C*
307600970526     C* -MSG-MESSAGGI
307700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
307800970526     C* (NON TOCCARE MSG,1 E MSG,2)
307900970526     C*
308000970526     C* -CMD-COMANDI
308100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
308200970526     C*
308300970526     C* -OPD-OPZIONI
308400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
308500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
308600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
308700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
308800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
308900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
309000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
309100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
309200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
309300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
309400970526     C*
309500970526     C* -FUD-FUNZIONI
309600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
309700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
309800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
309900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
310000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
310100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
310200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
310300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
310400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
310500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
310600970526     O*------------------------------------------------------------------------*
310700970526** DEC - DECODIFICHE
310800970526    MODIFICA                                                      1
310900970526  ANNULLAMENTO                                                    2
311000970526VISUALIZZAZIONE                                                   3
311100970526   RIPRISTINO                                                     4
311200970526  INSERIMENTO                                                     5
311300970526** MSG - MESSAGGI
311400970526TISI03R- Tasto funzionale non gestito                                          1
311500970526TISI03R- Inserimento effettuato                                                2
311600970526TISI03R- Modifica effettuata                                                   3
311700970526TISI03R- Annullamento effettuato                                               4
311800970526TISI03R- Ripristino effettuato                                                 5
311900970526TISI03R- Richiesta funzione per record inesistente                             6
312000970526TISI03R- Record momentaneamente allocato: riprovare più tardi                  7
312100970527TISI03R- Richiesta funzione per record esistente                               8
312200970526TISI03R- Provincia errata                                                      9
312300970526TISI03R- Linea di arrivo errata                                               10
312400970526TISI03R- Zona di consegna errata                                              11
312500060927§§§§§§§§ Ufficio Postale inesistente o annullato                              12
312600970526TISI03R- Nazione errata                                                       13
312700970526TISI03R- Codice tassazione errato                                             14
312800970526TISI03R- Codice tassazione errato. Accettati solo I1, I2, I3, ecc ...         15
312900970526TISI03R- Per CAP estero lasciare il codice tassazione vuoto                   16
313000060927§§§§§§§§ Zona di consegna NON abilitata                                       17
313100970604TISI03R- Linea limite obbligatoria se immessi limiti di peso o volume         18
313200970604TISI03R- Limiti obbligatori se immesse linea o zona limite                    19
313300130905Esistono OrariServizi personalizz.x la linea variata. Saranno eliminati       20
313400970912TISI03R- Il limite oltre deve essere superiore del limite sotto               21
313500010320TISI03R- Tasto funzione non consentito sul campo corrente.                    22
313600130905TISI03R- F17 consentito soltando sui campi LINEA di ARRIVO valorizzati        23
313700010320TISI03R- Tipo tempo ritiro errato o incompleto                                24
313800970917TISI03R- Il cap può essere obsoleto se lo sono anche tutte le sue località    25
313900970917TISI03R- Impossibile inserire cap obsoleti                                    26
314000970526TISI03R- Immettere 'S' o 'N'                                                  27
314100110623TISI03R- Tipo inoltro errato o non utilizzabile nel cappario                  28
314200970917TISI03R- Immettere 'O' per rendere obsoleto il cap, altrimenti lasciare vuoto 29
314300970603TISI03R- Tipo tempo di consegna errato o incompleto                           30
314400970526** FUD - FUNZIONI
314500970526                   -        (S/N)=N                        funz.01¬
314600970530                   -        (S/N)=N                        funz.02¬
314700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
314800010320F04=Visual. tempi  -        (S/N)=S  <== S-FISSO           funz.04¬
314900130827                   -        (S/N)=N                        funz.05¬
315000970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
315100130827F07=Orari servizi  -        (S/N)=S                        funz.07¬
315200970526F08=Località       -        (S/N)=S                        funz.08¬
315300970526F09=Eccezioni      -        (S/N)=S                        funz.09¬
315400970526F10=Tempi consegna -        (S/N)=S                        funz.10¬
315500970526                   -        (S/N)=N                        funz.11¬
315600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
315700970526                   -        (S/N)=N                        funz.13¬
315800970603F14=Zone consegna  -        (S/N)=S                        funz.14¬
315900970526                   -        (S/N)=N                        funz.15¬
316000970526                   -        (S/N)=N                        funz.16¬
316100130905F17=OrariServLinea -        (S/N)=S                        funz.17¬
316200970526                   -        (S/N)=N                        funz.18¬
316300970526                   -        (S/N)=N                        funz.19¬
316400970526                   -        (S/N)=N                        funz.20¬
316500970526                   -        (S/N)=N                        funz.21¬
316600970526                   -        (S/N)=N                        funz.22¬
316700970526                   -        (S/N)=N                        funz.23¬
316800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
