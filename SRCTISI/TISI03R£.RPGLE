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
129600970526     C*--------------------
129700970526     C* CODICE TASSAZIONE
129800970526     C*--------------------
129900970526IF  1C     ERRL00        IFEQ      '0'
130000970526     C                   MOVEL     *BLANKS       VDDCTS
130100970526     C                   MOVEL     VIDCTS        WCTS
130200970526     C                   MOVEL     *BLANKS       WDCTS
130300970526     C                   EXSR      CHKCTS
130400970526     C                   MOVEL     WCTS          VIDCTS
130500970526     C                   MOVEL     WDCTS         VDDCTS
130600970526IF  2C     ERRL00        IFEQ      '1'
130700970526     C                   SETON                                        58
130800970526     C                   MOVEL     MSG(14)       DSMSMS
130900970526E   2C                   ENDIF
131000970526E   1C                   ENDIF
131100970526     C*--------------------
131200970526     C* CODICE TASSAZIONE ITALIA PER ESTERO
131300970526     C*--------------------
131400970526IF  1C     ERRL00        IFEQ      '0'
131500970526     C                   MOVEL     *BLANKS       VDDCTE
131600970526IF  2C     VIDNAR        IFEQ      *BLANKS                                      *NAZIONE ITALIA
131700970526     C                   MOVEL     VIDCTE        WA1D              1
131800970526     C                   MOVE      VIDCTE        WA1S              1
131900970526IF  3C     WA1D          IFNE      'I'
132000970526     C     WA1S          OREQ      *BLANKS
132100970526     C                   MOVEL     '1'           ERRL00
132200970526     C                   SETON                                        59
132300970526     C                   MOVEL     MSG(15)       DSMSMS
132400970526E   3C                   ENDIF
132500970526X   2C                   ELSE                                                   *NAZIONE ESTERO
132600970526IF  3C     VIDCTE        IFNE      *BLANKS
132700970526     C                   MOVEL     '1'           ERRL00
132800970526     C                   SETON                                        59
132900970526     C                   MOVEL     MSG(16)       DSMSMS
133000970526E   3C                   ENDIF
133100970526E   2C                   ENDIF
133200970526E   1C                   ENDIF
133300970526     C*--------------------
133400970526     C* CAP FITTIZIO
133500970526     C*--------------------
133600970526IF  1C     ERRL00        IFEQ      '0'
133700970526IF  2C     VIDFIT        IFEQ      'S'
133800970526     C     VIDFIT        OREQ      'N'
133900970526X   2C                   ELSE
134000970526     C                   MOVEL     '1'           ERRL00
134100970526     C                   SETON                                        73
134200970526     C                   MOVEL     MSG(27)       DSMSMS
134300970526E   2C                   ENDIF
134400970526E   1C                   ENDIF
134500970526     C*--------------------
134600970526     C* CITTA CON VIARIO
134700970526     C*--------------------
134800970526IF  1C     ERRL00        IFEQ      '0'
134900970526IF  2C     VIDFLG        IFEQ      'S'
135000970526     C     VIDFLG        OREQ      'N'
135100970526X   2C                   ELSE
135200970526     C                   MOVEL     '1'           ERRL00
135300970526     C                   SETON                                        74
135400970526     C                   MOVEL     MSG(27)       DSMSMS
135500970526E   2C                   ENDIF
135600970526E   1C                   ENDIF
135700970526     C*--------------------
135800970526     C* INOLTRO
135900970526     C*--------------------
136000970526IF  1C     ERRL00        IFEQ      '0'
136100970526     C                   MOVEL     *BLANKS       VDDISO
136200970526     C                   MOVEL     VIDISO        WINL
136300970526     C                   MOVEL     *BLANKS       WDINL
136400970526     C                   EXSR      CHKINL
136500970526     C                   MOVEL     WINL          VIDISO
136600970526     C                   MOVEL     WDINL         VDDISO
136700970526IF  2C     ERRL00        IFEQ      '1'
136800970526     C                   SETON                                        75
136900970526     C                   MOVEL     MSG(28)       DSMSMS
137000970603X   2C                   ELSE
137100970526E   2C                   ENDIF
137200970526E   1C                   ENDIF
137300970526     C*--------------------
137400970526     C* POSSIBILE INOLTRO ISOLA
137500970526     C*--------------------
137600970526IF  1C     ERRL00        IFEQ      '0'
137700970526IF  2C     VIDPII        IFEQ      'S'
137800970526     C     VIDPII        OREQ      'N'
137900970526X   2C                   ELSE
138000970526     C                   MOVEL     '1'           ERRL00
138100970526     C                   SETON                                        76
138200970526     C                   MOVEL     MSG(27)       DSMSMS
138300970526E   2C                   ENDIF
138400970526E   1C                   ENDIF
138500970526     C*--------------------
138600970526     C* POSSIBILE INOLTRO LOCALITA' DISAGIATA
138700970526     C*--------------------
138800970526IF  1C     ERRL00        IFEQ      '0'
138900970526IF  2C     VIDPID        IFEQ      'S'
139000970526     C     VIDPID        OREQ      'N'
139100970526X   2C                   ELSE
139200970526     C                   MOVEL     '1'           ERRL00
139300970526     C                   SETON                                        77
139400970526     C                   MOVEL     MSG(27)       DSMSMS
139500970526E   2C                   ENDIF
139600970526E   1C                   ENDIF
139700970526     C*--------------------
139800970526     C* POSSIBILE INOLTRO CENTRO STORICO
139900970526     C*--------------------
140000970526IF  1C     ERRL00        IFEQ      '0'
140100970526IF  2C     VIDPIS        IFEQ      'S'
140200970526     C     VIDPIS        OREQ      'N'
140300970526X   2C                   ELSE
140400970526     C                   MOVEL     '1'           ERRL00
140500970526     C                   SETON                                        78
140600970526     C                   MOVEL     MSG(27)       DSMSMS
140700970526E   2C                   ENDIF
140800970526E   1C                   ENDIF
140900970526     C*--------------------
141000970526     C* GESTIONE DDT
141100970526     C*--------------------
141200970526IF  1C     ERRL00        IFEQ      '0'
141300970526IF  2C     VIDGDD        IFEQ      'S'
141400970526     C     VIDGDD        OREQ      'N'
141500970526X   2C                   ELSE
141600970526     C                   MOVEL     '1'           ERRL00
141700970526     C                   SETON                                        79
141800970526     C                   MOVEL     MSG(27)       DSMSMS
141900970526E   2C                   ENDIF
142000970526E   1C                   ENDIF
142100970526     C*--------------------
142200970526     C* TIPO TEMPO CONSEGNA
142300010322     C*--------------------
142400970526IF  1C     ERRL00        IFEQ      '0'
142500970526     C                   MOVEL     VIDTTC        WTTC                           *CODICE TEMPO
142600970526     C                   Z-ADD     *ZEROS        WD0I                           *TEMPI DISTRIBUZIONE
142700970526     C                   Z-ADD     *ZEROS        WD1I
142800970526     C                   Z-ADD     *ZEROS        WD0C
142900970526     C                   Z-ADD     *ZEROS        WD1C
143000970526     C                   Z-ADD     *ZEROS        WDIS
143100970526     C                   Z-ADD     *ZEROS        WDCS
143200970526     C                   Z-ADD     *ZEROS        WDLD
143300090903
143400970526     C                   Z-ADD     *ZEROS        WC0I                           *TEMPI CORRIERE
143500970526     C                   Z-ADD     *ZEROS        WC1I
143600970526     C                   Z-ADD     *ZEROS        WC0C
143700970526     C                   Z-ADD     *ZEROS        WC1C
143800970526     C                   Z-ADD     *ZEROS        WCIS
143900970526     C                   Z-ADD     *ZEROS        WCCS
144000970526     C                   Z-ADD     *ZEROS        WCLD
144100090903
144200970526     C                   Z-ADD     *ZEROS        WE0I                           *TEMPI ESPRESSO
144300970526     C                   Z-ADD     *ZEROS        WE1I
144400970526     C                   Z-ADD     *ZEROS        WE0C
144500970526     C                   Z-ADD     *ZEROS        WE1C
144600970526     C                   Z-ADD     *ZEROS        WEIS
144700970526     C                   Z-ADD     *ZEROS        WECS
144800970526     C                   Z-ADD     *ZEROS        WELD
144900090903
145000090903     C                   Z-ADD     *ZEROS        Wh0I                           *TEMPI H 1030
145100090903     C                   Z-ADD     *ZEROS        Wh1I
145200090903     C                   Z-ADD     *ZEROS        Wh0C
145300090903     C                   Z-ADD     *ZEROS        Wh1C
145400090903     C                   Z-ADD     *ZEROS        WhIS
145500090903     C                   Z-ADD     *ZEROS        WhCS
145600090903     C                   Z-ADD     *ZEROS        WhLD
145700970526     C                   EXSR      CHKTTC
145800970530     C                   MOVEL     WTTC          VIDTTC                         *CODICE TEMPO
145900970526     C                   Z-ADD     WD0I          VIDD0I                         *TEMPI DISTRIBUZIONE
146000970526     C                   Z-ADD     WD1I          VIDD1I
146100970526     C                   Z-ADD     WD0C          VIDD0C
146200970526     C                   Z-ADD     WD1C          VIDD1C
146300970526     C                   Z-ADD     WDIS          VIDDIS
146400970526     C                   Z-ADD     WDCS          VIDDCS
146500970526     C                   Z-ADD     WDLD          VIDDLD
146600090903
146700970526     C                   Z-ADD     WC0I          VIDC0I                         *TEMPI CORRIERE
146800970526     C                   Z-ADD     WC1I          VIDC1I
146900970526     C                   Z-ADD     WC0C          VIDC0C
147000970526     C                   Z-ADD     WC1C          VIDC1C
147100970526     C                   Z-ADD     WCIS          VIDCIS
147200970526     C                   Z-ADD     WCCS          VIDCCS
147300970526     C                   Z-ADD     WCLD          VIDCLD
147400090903
147500970526     C                   Z-ADD     WE0I          VIDE0I                         *TEMPI ESPRESSO
147600970526     C                   Z-ADD     WE1I          VIDE1I
147700970526     C                   Z-ADD     WE0C          VIDE0C
147800970526     C                   Z-ADD     WE1C          VIDE1C
147900970526     C                   Z-ADD     WEIS          VIDEIS
148000970526     C                   Z-ADD     WECS          VIDECS
148100970526     C                   Z-ADD     WELD          VIDELD
148200090903
148300090903     C                   Z-ADD     Wh0I          VIDh0I                         *TEMPI H1030
148400090903     C                   Z-ADD     Wh1I          VIDh1I
148500090903     C                   Z-ADD     Wh0C          VIDh0C
148600090903     C                   Z-ADD     Wh1C          VIDh1C
148700090903     C                   Z-ADD     WhIS          VIDhIS
148800090903     C                   Z-ADD     WhCS          VIDhCS
148900090903     C                   Z-ADD     WhLD          VIDhLD
149000970526IF  2C     ERRL00        IFEQ      '1'
149100970526     C                   SETON                                        80
149200970526     C                   MOVEL     MSG(30)       DSMSMS
149300970526E   2C                   ENDIF
149400970526E   1C                   ENDIF
149500010320     C*--------------------
149600010320     C* TIPO TEMPO RITIRO
149700010320     C*--------------------
149800010320IF  1C     ERRL00        IFEQ      '0'
149900010320     C                   MOVEL     VIDTTR        WTTR                           *CODICE TEMPO
150000010320     C                   Z-ADD     *ZEROS        WD0IR                          *TEMPI DISTRIBUZIONE
150100010320     C                   Z-ADD     *ZEROS        WD1IR
150200010320     C                   Z-ADD     *ZEROS        WD0CR
150300010320     C                   Z-ADD     *ZEROS        WD1CR
150400010320     C                   Z-ADD     *ZEROS        WDISR
150500010320     C                   Z-ADD     *ZEROS        WDCSR
150600010320     C                   Z-ADD     *ZEROS        WDLDR
150700010320     C                   Z-ADD     *ZEROS        WC0IR                          *TEMPI CORRIERE
150800010320     C                   Z-ADD     *ZEROS        WC1IR
150900010320     C                   Z-ADD     *ZEROS        WC0CR
151000010320     C                   Z-ADD     *ZEROS        WC1CR
151100010320     C                   Z-ADD     *ZEROS        WCISR
151200010320     C                   Z-ADD     *ZEROS        WCCSR
151300010320     C                   Z-ADD     *ZEROS        WCLDR
151400010320     C                   Z-ADD     *ZEROS        WE0IR                          *TEMPI ESPRESSO
151500010320     C                   Z-ADD     *ZEROS        WE1IR
151600010320     C                   Z-ADD     *ZEROS        WE0CR
151700010320     C                   Z-ADD     *ZEROS        WE1CR
151800010320     C                   Z-ADD     *ZEROS        WEISR
151900010320     C                   Z-ADD     *ZEROS        WECSR
152000010320     C                   Z-ADD     *ZEROS        WELDR
152100010320     C                   EXSR      CHKTTR
152200010320     C                   MOVEL     WTTR          VIDTTR                         *CODICE TEMPO
152300010320     C                   Z-ADD     WD0IR         VIDD0IR                        *TEMPI DISTRIBUZIONE
152400010320     C                   Z-ADD     WD1IR         VIDD1IR
152500010320     C                   Z-ADD     WD0CR         VIDD0CR
152600010320     C                   Z-ADD     WD1CR         VIDD1CR
152700010320     C                   Z-ADD     WDISR         VIDDISR
152800010320     C                   Z-ADD     WDCSR         VIDDCSR
152900010320     C                   Z-ADD     WDLDR         VIDDLDR
153000010320     C                   Z-ADD     WC0IR         VIDC0IR                        *TEMPI CORRIERE
153100010320     C                   Z-ADD     WC1IR         VIDC1IR
153200010320     C                   Z-ADD     WC0CR         VIDC0CR
153300010320     C                   Z-ADD     WC1CR         VIDC1CR
153400010320     C                   Z-ADD     WCISR         VIDCISR
153500010320     C                   Z-ADD     WCCSR         VIDCCSR
153600010320     C                   Z-ADD     WCLDR         VIDCLDR
153700010320     C                   Z-ADD     WE0IR         VIDE0IR                        *TEMPI ESPRESSO
153800010320     C                   Z-ADD     WE1IR         VIDE1IR
153900010320     C                   Z-ADD     WE0CR         VIDE0CR
154000010320     C                   Z-ADD     WE1CR         VIDE1CR
154100010320     C                   Z-ADD     WEISR         VIDEISR
154200010320     C                   Z-ADD     WECSR         VIDECSR
154300010320     C                   Z-ADD     WELDR         VIDELDR
154400010320IF  2C     ERRL00        IFEQ      '1'
154500010320     C                   SETON                                        44
154600010320     C                   MOVEL     MSG(24)       DSMSMS
154700010320E   2C                   ENDIF
154800010320E   1C                   ENDIF
154900970917     C*--------------------
155000970917     C* OBSOLETO
155100970917     C*--------------------
155200970917IF  1C     ERRL00        IFEQ      '0'
155300970917IF  2C     VIDFI1        IFEQ      'O'
155400970917     C     VIDFI1        OREQ      *BLANKS
155500970917X   2C                   ELSE
155600970917     C                   MOVEL     '1'           ERRL00
155700970917     C                   SETON                                        85
155800970917     C                   MOVEL     MSG(29)       DSMSMS
155900970917E   2C                   ENDIF
156000970917E   1C                   ENDIF
156100970917     C*
156200970917     C* INSERIMENTO -> NON POSSO INSERIRE CAP OBSOLETI
156300970917IF  1C     ERRL00        IFEQ      '0'
156400970917IF  2C     DEPOP1        IFEQ      'F06'
156500970917IF  3C     VIDFI1        IFEQ      'O'
156600970917     C                   SETON                                        85
156700970917     C                   MOVEL     MSG(26)       DSMSMS
156800970917     C                   MOVEL     '1'           ERRL00
156900970917E   3C                   ENDIF
157000970917E   2C                   ENDIF
157100970917E   1C                   ENDIF
157200970917     C*
157300970917     C* MODIFICA -> SE CAP OBSOLETO LO DEVONO ESSERE ANCHE TUTTE LE SUE LOCALITA'
157400970917IF  1C     ERRL00        IFEQ      '0'
157500970917IF  2C     DEPOP1        IFEQ      'O02'
157600970917IF  3C     VIDFI1        IFEQ      'O'
157700970917     C                   MOVEL     'S'           $LOBS
157800970917     C                   Z-ADD     D00VER        KCLVER
157900970917     C                   MOVEL     VIDNAR        KCLNAR
158000970917     C                   MOVEL     VIDCAP        KCLCAP
158100970917     C     KEYCPL        SETLL     AZCPL01L
158200970917     C     KEYCPL        READE(N)  AZCPL01L                               99
158300970917DO  4C     *IN99         DOWEQ     *OFF
158400970917IF  5C     CPLATB        IFEQ      *BLANKS                                      *NO ANNULLATI
158500970917IF  6C     CPLFI1        IFNE      'O '                                         *NO OBSOLETO
158600970917     C                   MOVEL     'N'           $LOBS
158700970917     C                   SETON                                        85
158800970917     C                   MOVEL     MSG(25)       DSMSMS
158900970917     C                   MOVEL     '1'           ERRL00
159000970917     C                   LEAVE
159100970917E   6C                   ENDIF
159200970917E   5C                   ENDIF
159300970917     C     KEYCPL        READE(N)  AZCPL01L                               99
159400970917E   4C                   ENDDO                                                  *FINE LOCALITA'
159500970917E   3C                   ENDIF
159600970917E   2C                   ENDIF
159700970917E   1C                   ENDIF
159800060927     C***
159900970526     C                   ENDSR
160000970526     C*------------------------------------------------------------------------*
160100970526     C* CHKNAR - CONTROLLA LA NAZIONE
160200970526     C*------------------------------------------------------------------------*
160300970526     C     CHKNAR        BEGSR
160400970526     C*
160500970526     C     *LIKE         DEFINE    VIDNAR        WNAR
160600970526     C     *LIKE         DEFINE    VDDNAR        WDNAR
160700970526     C     *LIKE         DEFINE    §15CPR        WCPR
160800970526     C*
160900970526     C* RICHIESTO ELENCO
161000970526     C                   SETOFF                                           28
161100970526     C     '?'           SCAN      WNAR                                   28
161200970526IF  1C     *IN28         IFEQ      *ON
161300020906     C                   Z-ADD     1             §KUT              1 0
161400970526     C                   MOVEL     '15'          §COD              2
161500970526     C                   MOVEL     *BLANKS       §KEY              8
161600970526     C                   MOVEL     *BLANKS       §DES             30
161700970526     C                   CALL      'X§TABER'
161800970526     C                   PARM                    §KUT
161900970526     C                   PARM                    §COD
162000970526     C                   PARM                    §KEY
162100970526     C                   PARM                    §DES
162200970526     C                   MOVEL     §KEY          WNAR
162300970526     C                   MOVEL     §DES          WDNAR
162400970526E   1C                   ENDIF
162500970526     C*
162600970526     C* CONTROLLA ESISTENZA CODICE
162700020906     C                   MOVEL     1             KTBKUT
162800970526     C                   MOVEL     '15'          KTBCOD
162900970526     C                   MOVEL(P)  WNAR          KTBKEY
163000970526     C     KEYTAB        CHAIN     TABEL00F                           99
163100970526IF  1C     *IN99         IFEQ      *OFF
163200970526     C     TBLFLG        ANDEQ     *BLANKS
163300970526     C                   MOVEL     TBLUNI        DS15
163400970526     C                   MOVEL     §15DES        WDNAR
163500970526     C                   MOVEL     §15CPR        WCPR
163600970526     C*
163700970526     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
163800970526IF  2C     VIDNAR        IFEQ      'I  '
163900970526     C                   MOVEL     '1'           ERRL00
164000970526     C                   MOVEL     *BLANKS       WDNAR
164100970526E   2C                   ENDIF
164200970526X   1C                   ELSE
164300970526     C                   MOVEL     '1'           ERRL00
164400970526E   1C                   ENDIF
164500970526     C*
164600970526     C                   ENDSR
164700970526     C*------------------------------------------------------------------------*
164800970526     C* CHKCAP - CONTROLLA IL CAP
164900970526     C*------------------------------------------------------------------------*
165000970526     C     CHKCAP        BEGSR
165100970526     C*
165200970526     C     *LIKE         DEFINE    VIDCAP        WCAP
165300970526     C*
165400970626     C* CONTROLLA IL CAP
165500970626     C                   CLEAR                   DSSI95
165600970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
165700970626     C                   MOVEL     '1'           I95TCN                         *CONTROLLO FORMALE
165800970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
165900970626     C                   MOVEL     WCAP          I95CAP                         *CAP
166000970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
166100970626     C                   CALL      'TISI95R'
166200970626     C                   PARM                    DSSI95
166300970626IF  1C     O95ERR        IFNE      *BLANKS
166400970703     C     VIDFIT        ANDNE     'S'
166500970526     C                   MOVEL     '1'           ERRL00
166600970626     C                   MOVEL     O95MSG        DSMSMS
166700970526E   1C                   ENDIF
166800970526     C*
166900970526     C                   ENDSR
167000970526     C*------------------------------------------------------------------------*
167100970526     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
167200970526     C*------------------------------------------------------------------------*
167300970526     C     CHKLIN        BEGSR
167400970526     C*
167500970526     C     *LIKE         DEFINE    VIDLNA        WLIN
167600970526     C     *LIKE         DEFINE    VDDLNA        WDLIN
167700970526     C*
167800970526     C* RICHIESTO ELENCO
167900970526     C                   SETOFF                                           28
168000970526     C     '?'           SCAN      WLIN                                   28
168100970526IF  1C     *IN28         IFEQ      *ON
168200970526     C                   MOVEL     *BLANKS       §COD1             3
168300970526     C                   MOVEL     *BLANKS       §TIP              1
168400970526     C                   MOVEL     *BLANKS       §DES1            25
168500971203     C                   MOVEL     *BLANKS       §PRA              3
168600971203     C                   CALL      'TNSD24R'
168700970526     C                   PARM                    §COD1
168800970526     C                   PARM                    §TIP
168900970526     C                   PARM                    §DES1
169000971203     C                   PARM                    §PRA
169100970526     C                   MOVEL     §COD1         WLIN
169200970526     C                   MOVEL     §DES1         WDLIN
169300970526E   1C                   ENDIF
169400970526     C*
169500970526     C* SOLO CARATTERI NUMERICI
169600970526     C                   SETOFF                                       28
169700970526     C                   TESTN                   WLIN                 28
169800970526IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
169900970526     C                   MOVEL     '1'           ERRL00
170000970526X   1C                   ELSE                                                   *E'NUMERO DA TESTN
170100970526     C                   MOVE      WLIN          W001              1
170200970526IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
170300970526     C                   MOVEL     '1'           ERRL00
170400970526X   2C                   ELSE                                                   *NUMERO
170500970526     C*
170600970526     C* CONTROLLA ESISTENZA CODICE
170700970526     C                   MOVEL     WLIN          KORFIL
170800970526     C     KEYORG        CHAIN     AZORG01L                           99
170900970526IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
171000970526     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
171100970527     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
171200970526     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
171300970526X   3C                   ELSE
171400970526     C                   MOVEL     '1'           ERRL00
171500970526E   3C                   ENDIF
171600970526E   2C                   ENDIF
171700970526E   1C                   ENDIF
171800970526     C*
171900970526     C                   ENDSR
172000970526     C*------------------------------------------------------------------------*
172100970526     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
172200970526     C*------------------------------------------------------------------------*
172300970526     C     CHKZNC        BEGSR
172400970526     C*
172500970526     C     *LIKE         DEFINE    VIDZNC        WZNC
172600970526     C*
172700970526     C* CONTROLLA ESISTENZA CODICE
172800020906     C                   Z-ADD     1             KTBKUT
172900970526     C                   MOVEL     '03'          KTBCOD
173000970526     C                   MOVEL     WLIN          WA5               5
173100970526     C                   MOVE      WZNC          WA5
173200970526     C                   MOVEL(P)  WA5           KTBKEY
173300970526     C     KEYTAB        CHAIN     TABEL00F                           99
173400970526IF  1C     *IN99         IFEQ      *OFF
173500970526     C     TBLFLG        ANDEQ     *BLANKS
173600970526     C                   MOVEL     TBLUNI        DS03
173700970526X   1C                   ELSE
173800970526     C                   MOVEL     '1'           ERRL00
173900970526E   1C                   ENDIF
174000970526     C*
174100970526     C                   ENDSR
174200970526     C*------------------------------------------------------------------------*
174300970526     C* CHKPRV - CONTROLLA LA PROVINCIA
174400970526     C*------------------------------------------------------------------------*
174500970526     C     CHKPRV        BEGSR
174600970526     C*
174700970526     C     *LIKE         DEFINE    VIDPRV        WPRV
174800970526     C     *LIKE         DEFINE    VDDPRV        WDPRV
174900970526     C*
175000970526     C* RICHIESTO ELENCO
175100970526     C                   SETOFF                                           28
175200970526     C     '?'           SCAN      WPRV                                   28
175300970526IF  1C     *IN28         IFEQ      *ON
175400020906     C                   Z-ADD     1             §KUT              1 0
175500970526     C                   MOVEL     'PR'          §COD              2
175600970526     C                   MOVEL     *BLANKS       §KEY              8
175700970526     C                   MOVEL     *BLANKS       §DES             30
175800970526     C                   CALL      'X§TABER'
175900970526     C                   PARM                    §KUT
176000970526     C                   PARM                    §COD
176100970526     C                   PARM                    §KEY
176200970526     C                   PARM                    §DES
176300970526     C                   MOVEL     §KEY          WPRV
176400970526     C                   MOVEL     §DES          WDPRV
176500970526E   1C                   ENDIF
176600970526     C*
176700970526     C* CONTROLLA ESISTENZA CODICE
176800020906     C                   MOVEL     1             KTBKUT
176900970526     C                   MOVEL     'PR'          KTBCOD
177000970526     C                   MOVEL(P)  WPRV          KTBKEY
177100970526     C     KEYTAB        CHAIN     TABEL00F                           99
177200970526IF  1C     *IN99         IFEQ      *OFF
177300970526     C     TBLFLG        ANDEQ     *BLANKS
177400970526     C                   MOVEL     TBLUNI        DSPR
177500970526     C                   MOVEL     §PRDES        WDPRV
177600970526X   1C                   ELSE
177700970526     C                   MOVEL     '1'           ERRL00
177800970526E   1C                   ENDIF
177900970526     C*
178000970526     C                   ENDSR
178100970526     C*------------------------------------------------------------------------*
178200970526     C* CHKCTS - CONTROLLA IL CODICE TASSAZIONE
178300970526     C*------------------------------------------------------------------------*
178400970526     C     CHKCTS        BEGSR
178500970526     C*
178600970526     C     *LIKE         DEFINE    VIDCTS        WCTS
178700970526     C     *LIKE         DEFINE    VDDCTS        WDCTS
178800970526     C*
178900970526     C* RICHIESTO ELENCO
179000970526     C                   SETOFF                                           28
179100970526     C     '?'           SCAN      WCTS                                   28
179200970526IF  1C     *IN28         IFEQ      *ON
179300120712     c                   clear                   TRTB09DS
179400120712     c                   eval      TB09fun = '1'
179500120712     c                   eval      kpjbu = TRTB09DS
179600120712     c                   call      'TRTB09R'
179700120712     c                   parm                    kpjba
179800120712     c                   eval      TRTB09DS = kpjbu
179900120712     c                   eval      wcts  = TB09cod
180000120712     c                   eval      wdcts = TB09des
180100970526E   1C                   ENDIF
180200970526     C*
180300970526     C* CONTROLLA ESISTENZA CODICE
180400020906     C                   MOVEL     1             KTBKUT
180500970526     C                   MOVEL     'CT'          KTBCOD
180600970526     C                   MOVEL(P)  WCTS          KTBKEY
180700970526     C     KEYTAB        CHAIN     TABEL00F                           99
180800970526IF  1C     *IN99         IFEQ      *OFF
180900970526     C     TBLFLG        ANDEQ     *BLANKS
181000970526     C                   MOVEL     TBLUNI        DSCT
181100970526     C                   MOVEL     §CTDES        WDCTS
181200970526X   1C                   ELSE
181300970526     C                   MOVEL     '1'           ERRL00
181400970526E   1C                   ENDIF
181500970526     C*
181600970526     C                   ENDSR
181700970526     C*------------------------------------------------------------------------*
181800970526     C* CHKINL - CONTROLLA L'INOLTRO
181900970526     C*------------------------------------------------------------------------*
182000970526     C     CHKINL        BEGSR
182100970526     C*
182200970526     C     *LIKE         DEFINE    VIDISO        WINL
182300970526     C     *LIKE         DEFINE    VDDISO        WDINL
182400970526     C*
182500970526     C* RICHIESTO ELENCO
182600970526     C                   SETOFF                                           28
182700970526     C     '?'           SCAN      WINL                                   28
182800970526IF  1C     *IN28         IFEQ      *ON
182900020906     C                   Z-ADD     1             §KUT              1 0
183000970526     C                   MOVEL     '9A'          §COD              2
183100970526     C                   MOVEL     *BLANKS       §KEY              8
183200970526     C                   MOVEL     *BLANKS       §DES             30
183300970526     C                   CALL      'X§TABER'
183400970526     C                   PARM                    §KUT
183500970526     C                   PARM                    §COD
183600970526     C                   PARM                    §KEY
183700970526     C                   PARM                    §DES
183800970526     C                   MOVEL     §KEY          WINL
183900970526     C                   MOVEL     §DES          WDINL
184000970526E   1C                   ENDIF
184100970526     C*
184200970526     C* CONTROLLA ESISTENZA CODICE
184300020906     C                   MOVEL     1             KTBKUT
184400970526     C                   MOVEL     '9A'          KTBCOD
184500970526     C                   MOVEL(P)  WINL          KTBKEY
184600970526     C     KEYTAB        CHAIN     TABEL00F                           99
184700970526IF  1C     *IN99         IFEQ      *OFF
184800970526     C     TBLFLG        ANDEQ     *BLANKS
184900970526     C                   MOVEL     TBLUNI        DS9A
185000970526     C                   MOVEL     §9ADES        WDINL
185100970526X   1C                   ELSE
185200970526     C                   MOVEL     '1'           ERRL00
185300970526E   1C                   ENDIF
185400110623     c
185500110623     c* Se non utilizzabile  nel cappario errore
185600110623     c                   if        §9auticpc='N'  and errl00='0'
185700110623     C                   MOVEL     '1'           ERRL00
185800110623E   1C                   ENDIF
185900970526     C*
186000970526     C                   ENDSR
186100970526     C*------------------------------------------------------------------------*
186200970526     C* CHKTTC - CONTROLLA IL TIPO TEMPO CONSEGNA
186300970526     C*------------------------------------------------------------------------*
186400970526     C     CHKTTC        BEGSR
186500970526     C*
186600970526     C     *LIKE         DEFINE    VIDTTC        WTTC                           *TIPO TEMPO
186700970526     C     *LIKE         DEFINE    CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
186800970526     C     *LIKE         DEFINE    CPT1IN        WD1I
186900970526     C     *LIKE         DEFINE    CPT0CN        WD0C
187000970526     C     *LIKE         DEFINE    CPT1CN        WD1C
187100970526     C     *LIKE         DEFINE    CPTTIS        WDIS
187200970526     C     *LIKE         DEFINE    CPTTCS        WDCS
187300970526     C     *LIKE         DEFINE    CPTTLD        WDLD
187400970526     C     *LIKE         DEFINE    CPT0IN        WC0I                           *TEMPI CORRIERE
187500970526     C     *LIKE         DEFINE    CPT1IN        WC1I
187600970526     C     *LIKE         DEFINE    CPT0CN        WC0C
187700970526     C     *LIKE         DEFINE    CPT1CN        WC1C
187800970526     C     *LIKE         DEFINE    CPTTIS        WCIS
187900970526     C     *LIKE         DEFINE    CPTTCS        WCCS
188000970526     C     *LIKE         DEFINE    CPTTLD        WCLD
188100090903
188200970526     C     *LIKE         DEFINE    CPT0IN        WE0I                           *TEMPI ESPRESSO
188300970526     C     *LIKE         DEFINE    CPT1IN        WE1I
188400970526     C     *LIKE         DEFINE    CPT0CN        WE0C
188500970526     C     *LIKE         DEFINE    CPT1CN        WE1C
188600970526     C     *LIKE         DEFINE    CPTTIS        WEIS
188700970526     C     *LIKE         DEFINE    CPTTCS        WECS
188800970526     C     *LIKE         DEFINE    CPTTLD        WELD
188900090903
189000090903     C     *LIKE         DEFINE    CPT0IN        Wh0I                           *TEMPI H1030
189100090903     C     *LIKE         DEFINE    CPT1IN        Wh1I
189200090903     C     *LIKE         DEFINE    CPT0CN        Wh0C
189300090903     C     *LIKE         DEFINE    CPT1CN        Wh1C
189400090903     C     *LIKE         DEFINE    CPTTIS        WhIS
189500090903     C     *LIKE         DEFINE    CPTTCS        WhCS
189600090903     C     *LIKE         DEFINE    CPTTLD        WhLD
189700970526     C*
189800970526     C* RICHIESTA ELENCO CODICI
189900970526     C                   SETOFF                                           28
190000970526     C     '?'           SCAN      WTTC                                   28
190100970526IF  1C     *IN28         IFEQ      *ON
190200970602     C                   MOVEL     *BLANK        DNAR
190300970602     C                   MOVEL     *BLANKS       DCAP
190400970530     C                   MOVEL     'T01'         WOP0
190500970530     C                   MOVEL     *BLANKS       WOP1
190600970530     C                   MOVEL     'TISI41R'     WPGM              8
190700970530     C                   EXSR      LANPGM
190800970530     C                   MOVEL     D00TTC        WTTC
190900010322E   1C                   ENDIF
191000010322     C*
191100050413     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO CONSEGNA
191200010322     C                   MOVEL     VIDTTC        DEP3A             3
191300010322     C     DEP3A         IFNE      '999'
191400970526     C*
191500970526     C* CONTROLLA ESISTENZA CODICI
191600970526     C                   Z-ADD     D00VER        KCTVER
191700970530     C                   MOVEL     WTTC          KCTTTC
191800970526     C                   MOVEL     'D'           KCTTSP
191900970526     C     KEYCPT        CHAIN     AZCPT01L                           99
192000970526IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
192100970526     C                   Z-ADD     CPT0IN        WD0I                           *TEMPI DISTRIBUZIONE
192200970526     C                   Z-ADD     CPT1IN        WD1I
192300970526     C                   Z-ADD     CPT0CN        WD0C
192400970526     C                   Z-ADD     CPT1CN        WD1C
192500970526     C                   Z-ADD     CPTTIS        WDIS
192600970526     C                   Z-ADD     CPTTCS        WDCS
192700970526     C                   Z-ADD     CPTTLD        WDLD
192800970526     C                   MOVEL     'C'           KCTTSP
192900970526     C     KEYCPT        CHAIN     AZCPT01L                           99
193000970526IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
193100970526     C                   Z-ADD     CPT0IN        WC0I                           *TEMPI CORRIERE
193200970526     C                   Z-ADD     CPT1IN        WC1I
193300970526     C                   Z-ADD     CPT0CN        WC0C
193400970526     C                   Z-ADD     CPT1CN        WC1C
193500970526     C                   Z-ADD     CPTTIS        WCIS
193600970526     C                   Z-ADD     CPTTCS        WCCS
193700970526     C                   Z-ADD     CPTTLD        WCLD
193800970526     C                   MOVEL     'E'           KCTTSP
193900970526     C     KEYCPT        CHAIN     AZCPT01L                           99
194000970526IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
194100970526     C                   Z-ADD     CPT0IN        WE0I                           *TEMPI ESPRESSO
194200970526     C                   Z-ADD     CPT1IN        WE1I
194300970526     C                   Z-ADD     CPT0CN        WE0C
194400970526     C                   Z-ADD     CPT1CN        WE1C
194500970526     C                   Z-ADD     CPTTIS        WEIS
194600970526     C                   Z-ADD     CPTTCS        WECS
194700970526     C                   Z-ADD     CPTTLD        WELD
194800090903     C                   MOVEL     'H'           KCTTSP
194900090903     C     KEYCPT        CHAIN     AZCPT01L                           99
195000090903IF  4C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
195100090903     C                   Z-ADD     CPT0IN        Wh0I                           *TEMPI H1030
195200090903     C                   Z-ADD     CPT1IN        Wh1I
195300090903     C                   Z-ADD     CPT0CN        Wh0C
195400090903     C                   Z-ADD     CPT1CN        Wh1C
195500090903     C                   Z-ADD     CPTTIS        WhIS
195600090903     C                   Z-ADD     CPTTCS        WhCS
195700090903     C                   Z-ADD     CPTTLD        WhLD
195800090903X   4C                   ELSE
195900090903     c* Se tipo servizio H non ancora decorrente (anche guardando alla
196000090903     c*    scadenza della versione in visualizzazione)non do errore e non
196100090903     c*    visualizzo nemmeno come descrizione
196200090903
196300170116     c**                 if        Datasys>=dataiso
196400090903     c* Verifico anche la  scadenza ddella versione
196500170116     c**   d00ver        chain     azcpr01l
196600170116     c**                 if        not %found(azcpr01l) or cprdsc>=§vpotsph
196700170116
196800170116     C                   MOVEL     '1'           ERRL00
196900170116
197000170116     c**                 endif
197100170116     c**                 endif
197200090903     c
197300170116     c**                 if        errl00='0'
197400170116     c**                 clear                   viddtsph
197500170116E   4C**                 ENDIF
197600090903
197700090903E   4C                   ENDIF
197800090903
197900090903X   3C                   ELSE
198000090903     C                   MOVEL     '1'           ERRL00
198100090903E   3C                   ENDIF
198200090903
198300970603X   2C                   ELSE
198400970603     C                   MOVEL     '1'           ERRL00
198500970526E   2C                   ENDIF
198600090903
198700970603X   1C                   ELSE
198800970603     C                   MOVEL     '1'           ERRL00
198900970526E   1C                   ENDIF
199000090903
199100010322     C                   ELSE
199200010322     C                   MOVEL     '1'           ERRL00
199300010322     C                   ENDIF
199400970526     C*
199500970526     C                   ENDSR
199600010320     C*------------------------------------------------------------------------*
199700010320     C* CHKTTR - CONTROLLA IL TIPO TEMPO RITIRO
199800010320     C*------------------------------------------------------------------------*
199900010320     C     CHKTTR        BEGSR
200000010320     C*
200100010320     C     *LIKE         DEFINE    VIDTTR        WTTR                           *TIPO TEMPO
200200010320     C     *LIKE         DEFINE    CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
200300010320     C     *LIKE         DEFINE    CPT1IN        WD1IR
200400010320     C     *LIKE         DEFINE    CPT0CN        WD0CR
200500010320     C     *LIKE         DEFINE    CPT1CN        WD1CR
200600010320     C     *LIKE         DEFINE    CPTTIS        WDISR
200700010320     C     *LIKE         DEFINE    CPTTCS        WDCSR
200800010320     C     *LIKE         DEFINE    CPTTLD        WDLDR
200900010320     C     *LIKE         DEFINE    CPT0IN        WC0IR                          *TEMPI CORRIERE
201000010320     C     *LIKE         DEFINE    CPT1IN        WC1IR
201100010320     C     *LIKE         DEFINE    CPT0CN        WC0CR
201200010320     C     *LIKE         DEFINE    CPT1CN        WC1CR
201300010320     C     *LIKE         DEFINE    CPTTIS        WCISR
201400010320     C     *LIKE         DEFINE    CPTTCS        WCCSR
201500010320     C     *LIKE         DEFINE    CPTTLD        WCLDR
201600010320     C     *LIKE         DEFINE    CPT0IN        WE0IR                          *TEMPI ESPRESSO
201700010320     C     *LIKE         DEFINE    CPT1IN        WE1IR
201800010320     C     *LIKE         DEFINE    CPT0CN        WE0CR
201900010320     C     *LIKE         DEFINE    CPT1CN        WE1CR
202000010320     C     *LIKE         DEFINE    CPTTIS        WEISR
202100010320     C     *LIKE         DEFINE    CPTTCS        WECSR
202200010320     C     *LIKE         DEFINE    CPTTLD        WELDR
202300010320     C*
202400010320     C* RICHIESTA ELENCO CODICI
202500010320     C                   SETOFF                                           28
202600010320     C     '?'           SCAN      WTTR                                   28
202700010320IF  1C     *IN28         IFEQ      *ON
202800010320     C                   MOVEL     *BLANK        DNAR
202900010320     C                   MOVEL     *BLANKS       DCAP
203000010320     C                   MOVEL     'T01'         WOP0
203100010320     C                   MOVEL     *BLANKS       WOP1
203200010323     C                   MOVEL     'TISI41R'     WPGM              8
203300010320     C                   EXSR      LANPGM
203400010320     C                   MOVEL     D00TTC        WTTR
203500010320E   1C                   ENDIF
203600010320     C*
203700010322     C* VERIFICO CHE TRATTASI DI CODICE FORMALMENTE VALIDO COME TEMPO RITIRO
203800010322     C                   MOVEL     VIDTTR        DEP3A             3
203900010322     C     DEP3A         IFEQ      '999'
204000010320     C* CONTROLLA ESISTENZA CODICI
204100010320     C                   Z-ADD     D00VER        KCTVER
204200010320     C                   MOVEL     WTTR          KCTTTC
204300010320     C                   MOVEL     'D'           KCTTSP
204400010320     C     KEYCPT        CHAIN     AZCPT01L                           99
204500010320IF  1C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
204600010320     C                   Z-ADD     CPT0IN        WD0IR                          *TEMPI DISTRIBUZIONE
204700010320     C                   Z-ADD     CPT1IN        WD1IR
204800010320     C                   Z-ADD     CPT0CN        WD0CR
204900010320     C                   Z-ADD     CPT1CN        WD1CR
205000010320     C                   Z-ADD     CPTTIS        WDISR
205100010320     C                   Z-ADD     CPTTCS        WDCSR
205200010320     C                   Z-ADD     CPTTLD        WDLDR
205300010320     C                   MOVEL     'C'           KCTTSP
205400010320     C     KEYCPT        CHAIN     AZCPT01L                           99
205500010320IF  2C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
205600010320     C                   Z-ADD     CPT0IN        WC0IR                          *TEMPI CORRIERE
205700010320     C                   Z-ADD     CPT1IN        WC1IR
205800010320     C                   Z-ADD     CPT0CN        WC0CR
205900010320     C                   Z-ADD     CPT1CN        WC1CR
206000010320     C                   Z-ADD     CPTTIS        WCISR
206100010320     C                   Z-ADD     CPTTCS        WCCSR
206200010320     C                   Z-ADD     CPTTLD        WCLDR
206300010320     C                   MOVEL     'E'           KCTTSP
206400010320     C     KEYCPT        CHAIN     AZCPT01L                           99
206500010320IF  3C     *IN99         IFEQ      *OFF                                         *TEMPI TROVATI
206600010320     C                   Z-ADD     CPT0IN        WE0IR                          *TEMPI ESPRESSO
206700010320     C                   Z-ADD     CPT1IN        WE1IR
206800010320     C                   Z-ADD     CPT0CN        WE0CR
206900010320     C                   Z-ADD     CPT1CN        WE1CR
207000010320     C                   Z-ADD     CPTTIS        WEISR
207100010320     C                   Z-ADD     CPTTCS        WECSR
207200010320     C                   Z-ADD     CPTTLD        WELDR
207300010320X   3C                   ELSE
207400010320     C                   MOVEL     '1'           ERRL00
207500010320E   3C                   ENDIF
207600010320X   2C                   ELSE
207700010320     C                   MOVEL     '1'           ERRL00
207800010320E   2C                   ENDIF
207900010320X   1C                   ELSE
208000010320     C                   MOVEL     '1'           ERRL00
208100010320E   1C                   ENDIF
208200010322     C                   ELSE
208300010322     C                   MOVEL     '1'           ERRL00
208400010322     C                   ENDIF
208500010320     C*
208600010320     C                   ENDSR
208700970526     C*------------------------------------------------------------------------*
208800970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
208900970526     C*------------------------------------------------------------------------*
209000970526     C     IREVID        BEGSR
209100970526     C* CAMPI
209200970526IF  1C     CPCESL        IFEQ      'S'
209300970526     C                   MOVEL     'LOC'         VDDESL
209400970526E   1C                   ENDIF
209500970526IF  1C     CPCESE        IFEQ      'S'
209600970526     C                   MOVEL     'ECC'         VDDESE
209700970526E   1C                   ENDIF
209800970526     C                   MOVEL     CPCNAR        VIDNAR
209900970526     C                   MOVEL     CPCCAP        VIDCAP
210000970526     C                   MOVEL     CPCLNA        VIDLNA
210100970526     C                   MOVEL     CPCLOL        VIDLOL
210200971118     C                   MOVEL     CPCLO2        VIDLO2
210300970604     C                   MOVEL     CPCLOS        VIDLOS
210400970526     C                   MOVEL     CPCLNA        VIDLFD
210500970526     C                   Z-ADD     CPCZNC        VIDZNC
210600970526     C                   Z-ADD     CPCZOL        VIDZOL
210700971118     C                   Z-ADD     CPCZO2        VIDZO2
210800970604     C                   Z-ADD     CPCZOS        VIDZOS
210900970526     C                   Z-ADD     CPCZFD        VIDZFD
211000970526     C                   Z-ADD     CPCLKG        VIDLKG
211100971118     C                   Z-ADD     CPCLK2        VIDLK2
211200970526     C                   Z-ADD     CPCLMC        VIDLMC
211300971118     C                   Z-ADD     CPCLM2        VIDLM2
211400970604     C                   Z-ADD     CPCLKS        VIDLKS
211500970604     C                   Z-ADD     CPCLMS        VIDLMS
211600970526     C                   MOVEL     CPCPRV        VIDPRV
211700970526     C                   MOVEL     CPCPRA        VIDPRA
211800970526     C                   MOVEL     CPCCTS        VIDCTS
211900970526     C                   MOVEL     CPCCTE        VIDCTE
212000970526     C                   MOVEL     CPCFIT        VIDFIT
212100970526     C                   MOVEL     CPCFLG        VIDFLG
212200970526     C                   MOVEL     CPCISO        VIDISO
212300970526     C                   MOVEL     CPCPII        VIDPII
212400970526     C                   MOVEL     CPCPID        VIDPID
212500970526     C                   MOVEL     CPCPIS        VIDPIS
212600970526     C                   MOVEL     CPCGDD        VIDGDD
212700970526     C                   MOVEL     CPCTTC        VIDTTC
212800970917     C                   MOVEL     CPCFI1        VIDFI1
212900010320     C                   MOVEL     *all'9'       VIDTTR
213000010320     C                   MOVE      CPCFI2        VIDTTR
213100000420     C*
213200060927     c*** 27/09/06 - EDPES: campi poste non più usati
213300970526     C*
213400970526     C                   ENDSR
213500970526     C*------------------------------------------------------------------------*
213600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
213700970526     C*------------------------------------------------------------------------*
213800970526     C     AZZVID        BEGSR
213900970526     C* CAMPI
214000970526     C                   MOVEL     *BLANKS       VDDESL
214100970526     C                   MOVEL     *BLANKS       VDDESE
214200970526     C                   MOVEL     *BLANKS       VIDNAR
214300970526     C                   MOVEL     *BLANKS       VIDCAP
214400970526     C                   MOVEL     *ZEROS        VIDLNA
214500970526     C                   MOVEL     *ZEROS        VIDLOL
214600971118     C                   MOVEL     *ZEROS        VIDLO2
214700970604     C                   MOVEL     *ZEROS        VIDLOS
214800970526     C                   MOVEL     *ZEROS        VIDLFD
214900970526     C                   Z-ADD     *ZEROS        VIDZNC
215000970526     C                   Z-ADD     *ZEROS        VIDZOL
215100971118     C                   Z-ADD     *ZEROS        VIDZO2
215200970604     C                   Z-ADD     *ZEROS        VIDZOS
215300970526     C                   Z-ADD     *ZEROS        VIDZFD
215400970526     C                   Z-ADD     *ZEROS        VIDLKG
215500971118     C                   Z-ADD     *ZEROS        VIDLK2
215600970526     C                   Z-ADD     *ZEROS        VIDLMC
215700971118     C                   Z-ADD     *ZEROS        VIDLM2
215800970604     C                   Z-ADD     *ZEROS        VIDLKS
215900970604     C                   Z-ADD     *ZEROS        VIDLMS
216000970526     C                   MOVEL     *BLANKS       VIDPRV
216100970526     C                   MOVEL     *BLANKS       VIDPRA
216200970526     C                   MOVEL     *BLANKS       VIDCTS
216300970526     C                   MOVEL     *BLANKS       VIDCTE
216400970526     C                   MOVEL     'N'           VIDFIT
216500970526     C                   MOVEL     'N'           VIDFLG
216600970526     C                   MOVEL     *BLANKS       VIDISO
216700970526     C                   MOVEL     'N'           VIDPII
216800970526     C                   MOVEL     'N'           VIDPID
216900970526     C                   MOVEL     'N'           VIDPIS
217000970526     C                   MOVEL     'N'           VIDGDD
217100970526     C                   MOVEL     *BLANKS       VIDTTC
217200970917     C                   MOVEL     *BLANKS       VIDFI1
217300970526     C* DECODIFICHE
217400970526     C                   MOVEL     *BLANKS       VDDNAR
217500970526     C                   MOVEL     *BLANKS       VDDLNA
217600970526     C                   MOVEL     *BLANKS       VDDLOL
217700971118     C                   MOVEL     *BLANKS       VDDLO2
217800970604     C                   MOVEL     *BLANKS       VDDLOS
217900970526     C                   MOVEL     *BLANKS       VDDLFD
218000970526     C                   MOVEL     *BLANKS       VDDPRV
218100970526     C                   MOVEL     *BLANKS       VDDPRA
218200970526     C                   MOVEL     *BLANKS       VDDCTS
218300970526     C                   MOVEL     *BLANKS       VDDCTE
218400970526     C                   MOVEL     *BLANKS       VDDISO
218500000426     C                   MOVEL     *BLANKS       VDPLNA
218600130902     c
218700130913     c*****              clear                   caplna
218800130913     c*****              clear                   loclna
218900130903     c                   clear                   caplos
219000130903     c                   clear                   loclos
219100130903     c                   clear                   caplol
219200130903     c                   clear                   loclol
219300130903     c                   clear                   F07los            1
219400130903     c                   clear                   F07lna            1
219500130903     c                   clear                   F07lol            1
219600130913     c                   clear                   s_cpclna
219700130913     c                   clear                   s_cpclos
219800130913     c                   clear                   s_cpclol
219900970526     C*
220000970526     C                   ENDSR
220100970526     C*------------------------------------------------------------------------*
220200970526     C* IMPCPC - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
220300970526     C*------------------------------------------------------------------------*
220400970526     C     IMPCPC        BEGSR
220500970526     C*
220600970526     C                   MOVEL     VIDLNA        CPCLNA
220700970526     C                   MOVEL     VIDLOL        CPCLOL
220800971118     C                   MOVEL     VIDLO2        CPCLO2
220900970604     C                   MOVEL     VIDLOS        CPCLOS
221000970526     C                   Z-ADD     VIDZNC        CPCZNC
221100970526     C                   Z-ADD     VIDZOL        CPCZOL
221200971118     C                   Z-ADD     VIDZO2        CPCZO2
221300970604     C                   Z-ADD     VIDZOS        CPCZOS
221400970526     C                   Z-ADD     VIDZFD        CPCZFD
221500970526     C                   Z-ADD     VIDLKG        CPCLKG
221600971118     C                   Z-ADD     VIDLK2        CPCLK2
221700970526     C                   Z-ADD     VIDLMC        CPCLMC
221800971118     C                   Z-ADD     VIDLM2        CPCLM2
221900970604     C                   Z-ADD     VIDLKS        CPCLKS
222000970604     C                   Z-ADD     VIDLMS        CPCLMS
222100970526     C                   MOVEL     VIDPRV        CPCPRV
222200970526     C                   MOVEL     VIDPRA        CPCPRA
222300970526     C                   MOVEL     VIDCTS        CPCCTS
222400970526     C                   MOVEL     VIDCTE        CPCCTE
222500970526     C                   MOVEL     VIDFIT        CPCFIT
222600970526     C                   MOVEL     VIDFLG        CPCFLG
222700970526     C                   MOVEL     VIDISO        CPCISO
222800970526     C                   MOVEL     VIDPII        CPCPII
222900970603IF  3C     CPCISO        IFEQ      'I'
223000970603     C                   MOVEL     'S'           CPCPII
223100970603E   3C                   ENDIF
223200970603     C                   MOVEL     VIDPID        CPCPID
223300970603IF  3C     CPCISO        IFEQ      'D'
223400970603     C                   MOVEL     'S'           CPCPID
223500970603E   3C                   ENDIF
223600970603     C                   MOVEL     VIDPIS        CPCPIS
223700970603IF  3C     CPCISO        IFEQ      'S'
223800970603     C                   MOVEL     'S'           CPCPIS
223900970603E   3C                   ENDIF
224000970526     C                   MOVEL     VIDGDD        CPCGDD
224100970526     C                   MOVEL     VIDTTC        CPCTTC
224200970917     C                   MOVEL     VIDFI1        CPCFI1
224300010320     C                   MOVE(p)   VIDTTR        CPCFI2
224400970526     C*
224500970526     C                   ENDSR
224600970530     C*------------------------------------------------------------------------*
224700970530     C* LANPGM - LANCIA UN PROGRAMMA
224800970530     C*------------------------------------------------------------------------*
224900970530     C     LANPGM        BEGSR
225000970530     C*
225100970530     C* IMPOSTA LA DS DI PROCEDURA
225200970530     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
225300970530     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
225400970530     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
225500970530     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
225600970530     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
225700970530     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
225800970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
225900970602     C                   MOVEL     DCAP          D00CAP                         *CAP
226000970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
226100970530     C                   MOVEL     *BLANKS       D00TSP
226200970530     C                   Z-ADD     *ZEROS        D00TFP
226300970530     C                   MOVEL     *BLANKS       D00LOC
226400970603     C                   MOVEL     *BLANKS       D00SIN
226500970530     C                   MOVEL     *BLANKS       D00EPA
226600970530     C                   Z-ADD     *ZEROS        D00ETP
226700970530     C                   Z-ADD     *ZEROS        D00ETA
226800970530     C                   Z-ADD     *ZEROS        D00EDD
226900970530     C                   MOVEL     *BLANKS       D00SPT
227000970530     C                   Z-ADD     *ZEROS        D00SLP
227100970530     C                   Z-ADD     *ZEROS        D00SLA
227200970530     C                   MOVEL     *BLANKS       D00STS
227300970530     C                   Z-ADD     *ZEROS        D00SDD
227400970530     C*
227500970530     C                   CALL      WPGM
227600970612     C                   PARM                    KPJBA
227700970612     C                   PARM                    DSSI00
227800970530     C*
227900970530     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
228000970530IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
228100970530     C                   MOVEL     '1'           ERRL00                         *ERRORE
228200970530     C                   MOVEL     '1'           F03L00                         *FINE PGM
228300970530X   1C                   ELSE                                                   *NON PREMUTO F03
228400970530     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
228500970530E   1C                   ENDIF
228600970530     C*
228700970530     C                   ENDSR
228800970526     C*------------------------------------------------------------------------*
228900970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
229000970526     C*------------------------------------------------------------------------*
229100970526     C     IMPFUN        BEGSR
229200970603     C*
229300970603     C* SE INTERROGAZIONE/VISUALIZZAZIONE TOGLE LA GESTIONE ALLE ZONE DI CONSEGNA
229400970603IF  1C     DEPOP0        IFEQ      'C05'
229500970603     C     DEPOP1        OREQ      'O05'
229600970603     C                   MOVEL     FUD(14)       FUDRIG                         *AGGIORNAMENTO
229700970603     C                   MOVEL     'N'           FUDATT
229800970603     C                   MOVEL     FUDRIG        FUD(14)
229900970603E   1C                   ENDIF
230000970526     C*
230100970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
230200970526IF  1C     DEPOP1        IFEQ      'O05'
230300170116     c     wabiute       oreq      'I'
230400170116     c     wabiute       oreq      'O'
230500970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
230600970526     C                   MOVEL     'N'           FUDATT
230700970526     C                   MOVEL     FUDRIG        FUD(6)
230800970526E   1C                   ENDIF
230900970526     C*
231000970526     C* SE INSERIMENTO TOGLIE LE FUNZIONI DI ACCESSO AGLI ALTRI ARCHIVI
231100970526IF  1C     DEPOP1        IFEQ      'F06'
231200970526     C                   MOVEL     FUD(8)        FUDRIG                         *LOCALITA'
231300970526     C                   MOVEL     'N'           FUDATT
231400970526     C                   MOVEL     FUDRIG        FUD(8)
231500970527     C                   MOVEL     FUD(9)        FUDRIG                         *ECCEZIONI
231600970526     C                   MOVEL     'N'           FUDATT
231700970526     C                   MOVEL     FUDRIG        FUD(9)
231800970527     C                   MOVEL     FUD(10)       FUDRIG                         *TEMPI
231900970527     C                   MOVEL     'N'           FUDATT
232000970527     C                   MOVEL     FUDRIG        FUD(10)
232100970526E   1C                   ENDIF
232200131007     c
232300131007     c* Se utente non abilitato, non attivo F7 e F17
232400131007     c* lascio in manutenzione
232500131007     c                   if        *in10 and  depop0 = 'C05'
232600131007     C                   MOVEL     FUD(7)        FUDRIG                         *LOCALITA'
232700131007     C                   MOVEL     'N'           FUDATT
232800131007     C                   MOVEL     FUDRIG        FUD(7)
232900131007     C                   MOVEL     FUD(17)       FUDRIG                         *LOCALITA'
233000131007     C                   MOVEL     'N'           FUDATT
233100131007     C                   MOVEL     FUDRIG        FUD(17)
233200131007     c                   endif
233300970526     C*
233400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
233500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
233600970526     C                   EXSR      FUNGES
233700000925     C*
233800000925     C* VERIFICO CHE SOLO UTENTI "EDP*" POSSANO MODIFICARE IL CAMPO "UFFICIO POSTALE"
233900000925     C                   MOVEL     KNMUS         UTE               3
234000000925     C     UTE           IFNE      'EDP'
234100000925     C                   SETON                                        95
234200000925     C                   ENDIF
234300000925     C*
234400970526     C*
234500970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
234600970526     C*---
234700970526     C* 90 -> PROTEZIONE CHIAVE
234800970526     C* 91 -> PROTEZIONE TOTALE
234900050414     C* 93 -> PROTEZIONE COD.TASSAZIONE e TIPO INOLTRO e altri
235000970526     C*---
235100050413     C*
235200050413     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
235300050413     C                   SETON                                        93
235400170116     c
235500170116     c
235600170116     c                   if        wabiute='V'
235700170116     c* se manut completa solo se  czppario ancora da decorrere, verifico con
235800170116     c*  la versione
235900170116     c     d00ver        chain     azcpr01l
236000170116     c                   if        %found(azcpr01l)
236100170116     c     *iso          movel     cprdde        dataiso
236200170116     c                   if        dataiso>datasys
236300170116     c                   setoff                                       93
236400170116     c                   else
236500170116     c                   eval      wabiute='I'
236600170116     c                   endif
236700170116     c
236800170116     c                   else
236900170116     c                   eval      wabiute='I'
237000170116     c                   endif
237100170116     c                   endif
237200050413     C*
237300050413     C* VERIFICO X PRIMISSIMA COSA LE AUTORIZZAZIONI SPECIALI SULL'UTENTE CORRENTE
237400170116     C                   IF        wabiute = *blanks  or wabiute='M'
237500050413     C* TUTTO RIMANE INVARIATO
237600050413     C                   ENDIF
237700050413     C*
237800170116     C                   IF        wabiute = 'I'  or wabiute='O'
237900140206     C* FORZO SOLO INTERROGAZIONE ma permetto di modificare gli orari se c'e' "O"
238000050413     C                   EVAL      DEPOP1 = 'O05'
238100050413     C                   ENDIF
238200050413     C*
238300170116     C                   IF        wabiute = 'C'
238400050413     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
238500050413     C                   SETOFF                                       93
238600050413     C                   ENDIF
238700050413     C*
238800970526IF  1C     DEPOP0        IFEQ      'C02'                                        G E S T I O N E
238900970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
239000970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
239100970526     C                   SETOFF                                       91
239200970526E   2C                   ENDIF
239300970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
239400970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
239500970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
239600970526E   2C                   ENDIF
239700970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
239800970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
239900970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
240000970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
240100970526     C                   SETON                                        91
240200970526E   2C                   ENDIF
240300970526X   1C                   ELSE                                                   *I N T E R R O G A
240400970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
240500970526     C                   SETON                                        91
240600970526E   1C                   ENDIF
240700970526     C*
240800970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
240900970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
241000970526     C                   Z-ADD     D00VER        KCCVER
241100970526     C                   MOVEL     D00NAR        KCCNAR
241200970526     C                   MOVEL     D00CAP        KCCCAP
241300970526     C     KEYCPC        CHAIN(N)  AZCPC01L                           99
241400970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
241500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
241600970526     C                   MOVEL     MSG(6)        DSMSMS
241700970526X   2C                   ELSE
241800970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
241900980128IF  3C     DEPOP1        IFEQ      'O05'
242000980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
242100980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
242200980128E   3C                   ENDIF
242300980128E   2C                   ENDIF
242400970526X   1C                   ELSE                                                   *INSERIMENTO
242500970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
242600970526E   1C                   ENDIF
242700970526     C*
242800970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
242900970526IF  1C     DEPOP1        IFEQ      'O02'
243000970526     C                   MOVEL     DEC(1)        VIDFUN
243100970526E   1C                   ENDIF
243200970526IF  1C     DEPOP1        IFEQ      'O04'
243300970526     C                   MOVEL     DEC(2)        VIDFUN
243400970526E   1C                   ENDIF
243500970526IF  1C     DEPOP1        IFEQ      'O05'
243600970526     C                   MOVEL     DEC(3)        VIDFUN
243700970526E   1C                   ENDIF
243800970526IF  1C     DEPOP1        IFEQ      'O07'
243900970526     C                   MOVEL     DEC(4)        VIDFUN
244000970526E   1C                   ENDIF
244100970526IF  1C     DEPOP1        IFEQ      'F06'
244200970526     C                   MOVEL     DEC(5)        VIDFUN
244300970526E   1C                   ENDIF
244400970526     C*
244500970526     C                   ENDSR
244600130913     C*------------------------------------------------------------------------*
244700130913     C* Richiamo pgm annullamento orari servizi a fronte di modifica lna
244800130913     C*------------------------------------------------------------------------*
244900130913     c     sr_oraris     begsr
245000130913
245100130913     C* LINEA STANDARD
245200130913     c                   movel     vidlna        w0030             3 0
245300130913     c* Se cambiata la linea
245400130913     c                   if        w0030<>cpclna
245500130913     c                   clear                   tisio7ds
245600130913     c                   eval      io7mod='D'
245700130913     c                   eval      io7liv='C'
245800130913     c                   eval      io7ver=d00ver
245900130913     c                   eval      io7nar=vidnar
246000130913     c                   eval      io7cap=vidcap
246100130913     c                   eval      io7lna=cpclna
246200130913     c                   eval      io7lns=%dec(vidlna:3:0)
246300130913     c                   eval      io7los=%dec(vidlos:3:0)
246400130913     c                   eval      io7lol=%dec(vidlol:3:0)
246500131016     c******             movel     tisio7ds      kpjbu
246600130913     c                   call      'TISIO7R'
246700130913     c                   parm                    kpjba
246800131016     c                   parm                    tisio7ds
246900130913    3c                   endif
247000130913     C* LINEA SOTTO
247100130913     c                   movel     vidlos        w0030             3 0
247200130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
247300130913     c                   if        w0030<>cpclos and cpclos<>cpclna
247400130913     c                   clear                   tisio7ds
247500130913     c                   eval      io7mod='D'
247600130913     c                   eval      io7liv='C'
247700130913     c                   eval      io7ver=d00ver
247800130913     c                   eval      io7nar=vidnar
247900130913     c                   eval      io7cap=vidcap
248000130913     c                   eval      io7lna=cpclos
248100130913     c                   eval      io7lns=%dec(vidlna:3:0)
248200130913     c                   eval      io7los=%dec(vidlos:3:0)
248300130913     c                   eval      io7lol=%dec(vidlol:3:0)
248400131016     c******             movel     tisio7ds      kpjbu
248500130913     c                   call      'TISIO7R'
248600130913     c                   parm                    kpjba
248700131016     c                   parm                    tisio7ds
248800130913    3c                   endif
248900130913     C* LINEA OLTRE
249000130913     c                   movel     vidlol        w0030             3 0
249100130913     c* Se cambiata la linea a non ancora richiamato pgm per la linea stessa
249200130913     c                   if        w0030<>cpclol and cpclol<>cpclna
249300130913     c                                           and cpclol<>cpclos
249400130913     c                   clear                   tisio7ds
249500130913     c                   eval      io7mod='D'
249600130913     c                   eval      io7liv='C'
249700130913     c                   eval      io7ver=d00ver
249800130913     c                   eval      io7nar=vidnar
249900130913     c                   eval      io7cap=vidcap
250000130913     c                   eval      io7lna=cpclol
250100130913     c                   eval      io7lns=%dec(vidlna:3:0)
250200130913     c                   eval      io7los=%dec(vidlos:3:0)
250300130913     c                   eval      io7lol=%dec(vidlol:3:0)
250400131016     c*****              movel     tisio7ds      kpjbu
250500130913     c                   call      'TISIO7R'
250600130913     c                   parm                    kpjba
250700131016     c                   parm                    tisio7ds
250800130913    3c                   endif
250900130913     c                   endsr
251000970703     C*------------------------------------------------------------------------*
251100970703     C* FINSR - OPERAZIONI FINALI
251200970703     C*------------------------------------------------------------------------*
251300970703     C     FINSR         BEGSR
251400970703     C*
251500970704     C                   CLEAR                   DSSI95
251600970703     C                   MOVEL     'C'           I95TLA
251700970703     C                   CALL      'TISI95R'
251800970703     C                   PARM                    DSSI95
251900130913     c*
252000130913     c                   clear                   tisio7ds
252100130913     C                   MOVEL     'C'           Io7TLA
252200131016     c******             movel     tisio7ds      kpjbu
252300130913     C                   CALL      'TISIO7R'
252400130913     C                   PARM                    kpjba
252500131016     c                   parm                    tisio7ds
252600970703     C*
252700970703     C                   ENDSR
252800050413     C*--------------------------------------------------------------------------------------------*
252900050413     C* REPERISCE DATI UTENTE
253000050413     C*--------------------------------------------------------------------------------------------*
253100050413     C     REPDATIUTE    BEGSR
253200050413     C*
253300050413     C* REimposta le variabili di lavoro
253400050413     C                   CLEAR                   TIBS34DS
253500050413     C                   CLEAR                   AZUTEDS
253600050413     C                   CLEAR                   DDATIUTE
253700050413     C*
253800050413     C* Reperisce i dati del profilo in ingresso
253900050413     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
254000050413     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
254100050413     C                   IN(E)     *DTAARA
254200050413     C                   IF        %Error                                       *NO errori
254300050413     C                   EVAL      I34TLA = 'L'
254400050413     C                   CALL      'TIBS34R'
254500050413     C                   PARM                    TIBS34DS
254600050413     C                   IN        *DTAARA
254700050413     C                   ENDIF
254800050413     C*
254900050413     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
255000050413     C                   EVAL      DUTE01 = UTEFAF
255100050413     C*
255200050413     C                   ENDSR
255300970526     C*------------------------------------------------------------------------*
255400970526     C* *INZSR - OPERAZIONI INIZIALI
255500970526     C*------------------------------------------------------------------------*
255600970526     C     *INZSR        BEGSR
255700970526     C*---
255800970526     C* RICEVIMENTO PARAMETRI
255900970526     C*---
256000970526     C     *ENTRY        PLIST
256100970612     C                   PARM                    KPJBA
256200970612     C                   PARM                    DSSI00
256300170116
256400050413     C*
256500050413     C* REPERISCE I DATI DELL'UTENTE
256600050413     C                   EXSR      REPDATIUTE
256700060206     c                   movel     RSUT          DSfirs
256800170116
256900170116     c                   eval      wabiute=§utecap
257000970526     C*
257100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
257200970526     C     XMSGPA        PLIST
257300970526     C                   PARM                    DSMSMS
257400970526     C                   PARM                    DSMSPG
257500970526     C                   PARM                    DSMSKE
257600970526     C                   PARM                    DSMSER
257700131007      /free
257800131007        // Carico filiali di test abilitate
257900131007         if dutpou<>46  ;
258000131007
258100131007         clear TIBS02ds;
258200131007         T02Mod = 'C';
258300131007         T02sif = knsif;
258400131007         T02cod = 'VPO';
258500131007         t02tla = 'L'   ;
258600131007         T02ke1 ='DECOFI816' ;
258700131007         TNTBE_RicercaControllo  (kpjba : tibs02ds);
258800131007
258900131007         if  T02err  = *blank;
259000131007         xx= 1 ;
259100131007         yy= 1 ;
259200131007
259300131007         dow xx< 256   ;
259400131007           fil_ab(yy)= %subst(t02uni:xx:3)  ;
259500131007           yy=yy+ 1   ;
259600131007           xx=xx+ 3   ;
259700131007         enddo  ;
259800131007
259900131007         endif;
260000131007
260100131007         // se c'e' 999 tutte abilitate
260200131007         w003a='999'  ;
260300131007         if   %lookup(w003a:fil_ab) = 0  ;
260400131007         w003a=%editc(dutpou:'X')   ;
260500131007         if   %lookup(w003a:fil_ab) = 0  ;
260600131007         *in10 =*on  ;
260700131007         endif  ;
260800131007         endif  ;
260900131007
261000131007         endif  ;
261100131007
261200131007      /end-free
261300131007
261400090903     c*
261500090903     c* Reperisco DVPODECO per vedere quando si attiva il tipo servizio H
261600170116     c**                 clear                   tibs02ds
261700170116     c**                 clear                   dvpodeco
261800170116     c**                 eval      t02mod = 'C'
261900170116     c**                 eval      t02sif = knsif
262000170116     c**                 eval      t02cod = 'VPO'
262100170116     c**                 eval      t02ke1 = 'DECO'
262200170116     c**                 call      'TIBS02R'
262300170116     c**                 parm                    kpjba
262400170116     c**                 parm                    tibs02ds
262500170116     c**                 if        t02err = *blanks
262600170116     c**                 eval      dvpodeco = t02uni
262700170116     c**                 else
262800170116     c**                 eval      §vpotsph=20091001
262900170116     c**                 endif
263000090903
263100170116     c**   *iso          movel     §vpotsph      dataiso
263200090903     C*
263300000426     C*
263400000426     C* PRIMISSIME CONSIDERAZIONI SU PARAMETRI INIZIALI
263500000426     C     D00NAR        IFNE      *BLANKS
263600000426     C                   SETON                                        92
263700000426     C                   ELSE
263800000426     C                   SETOFF                                       92
263900000426     C                   ENDIF
264000970526     C*
264100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
264200970526     C                   CALL      'XKEY'
264300970526     C                   PARM                    DSKEY
264400970526     C*---
264500970526     C* VARIABILI NON RIFERITE AL DATA BASE
264600970526     C*---
264700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
264800970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
264900970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
265000970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
265100970526     C     *LIKE         DEFINE    D00OP1        WOP1
265200970602     C     *LIKE         DEFINE    D00NAR        DNAR
265300970602     C     *LIKE         DEFINE    D00CAP        DCAP
265400970526     C*---
265500970526     C* VARIABILI DI CONTROLLO
265600970526     C*---
265700970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
265800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
265900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
266000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
266100970917     C                   MOVEL     'S'           $LOBS             1            *LOCALITA' OBSOLETE
266200970526     C*
266300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
266400970526     C                   MOVEL     'TISI03R'     DSMSPG
266500970526     C                   MOVEL     'TISI03R'     MSGQUE
266600970526     C                   MOVEL     *BLANKS       MSGKEY
266700970526     C                   MOVEL     *BLANKS       DSMSMS
266800970526     C*---
266900970526     C* VARIABILI RIFERITE AL DATA BASE
267000970526     C*---
267100970526     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
267200970526     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
267300970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
267400970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
267500970526     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
267600970526     C     *LIKE         DEFINE    CPCNAR        KCCNAR
267700970526     C     *LIKE         DEFINE    CPCCAP        KCCCAP
267800970526     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
267900970526     C     *LIKE         DEFINE    CPENAR        KCENAR
268000970526     C     *LIKE         DEFINE    CPECAP        KCECAP
268100970526     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
268200970526     C     *LIKE         DEFINE    CPLNAR        KCLNAR
268300970526     C     *LIKE         DEFINE    CPLCAP        KCLCAP
268400130903     C     *LIKE         DEFINE    CPLloc        KCLloc
268500970526     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
268600970526     C     *LIKE         DEFINE    CPSNAR        KCSNAR
268700970526     C     *LIKE         DEFINE    CPSCAP        KCSCAP
268800970526     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
268900970526     C     *LIKE         DEFINE    CPTTTC        KCTTTC
269000970526     C     *LIKE         DEFINE    CPTTSP        KCTTSP
269100970526     C*---
269200970526     C* CHIAVI DI LETTURA
269300970526     C*---
269400970526     C* LETTURA AZORG01L - COMPLETA
269500970526     C     KEYORG        KLIST
269600970526     C                   KFLD                    KORFIL                         *FILIALE
269700970526     C* LETTURA TABEL00F - COMPLETA
269800970526     C     KEYTAB        KLIST
269900970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
270000970526     C                   KFLD                    KTBCOD                         *TABELLA
270100970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
270200970526     C* LETTURA TABEL00F - PARZIALE
270300970526     C     KE2TAB        KLIST
270400970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
270500970526     C                   KFLD                    KTBCOD                         *TABELLA
270600970526     C* LETTURA AZCPC01L - COMPLETA
270700970526     C     KEYCPC        KLIST
270800970526     C                   KFLD                    KCCVER                         *VERSIONE
270900970526     C                   KFLD                    KCCNAR                         *NAZIONE
271000970526     C                   KFLD                    KCCCAP                         *CAP
271100970527     C* LETTURA AZCPT01L - COMPLETA
271200970526     C     KEYCPT        KLIST
271300970526     C                   KFLD                    KCTVER                         *VERSIONE
271400970526     C                   KFLD                    KCTTTC                         *TEMPO CONSEGNA
271500970526     C                   KFLD                    KCTTSP                         *TIPO SERVIZIO
271600970527     C* LETTURA AZCPE01L - PARZIALE
271700970527     C     KEYCPE        KLIST
271800970527     C                   KFLD                    KCEVER                         *VERSIONE
271900970527     C                   KFLD                    KCENAR                         *NAZIONE
272000970527     C                   KFLD                    KCECAP                         *CAP
272100970526     C* LETTURA AZCPL01L - PARZIALE
272200970526     C     KEYCPL        KLIST
272300970526     C                   KFLD                    KCLVER                         *VERSIONE
272400970526     C                   KFLD                    KCLNAR                         *NAZIONE
272500970526     C                   KFLD                    KCLCAP                         *CAP
272600130903     C* LETTURA AZCPL01L - PARZIALE
272700130903     C     KEYCPL2       KLIST
272800130903     C                   KFLD                    KCLVER                         *VERSIONE
272900130903     C                   KFLD                    KCLNAR                         *NAZIONE
273000130903     C                   KFLD                    KCLCAP                         *CAP
273100130903     C                   KFLD                    KCLloc                         *CAP
273200970526     C* LETTURA AZCPS01L - PARZIALE
273300970526     C     KEYCPS        KLIST
273400970526     C                   KFLD                    KCSVER                         *VERSIONE
273500970526     C                   KFLD                    KCSNAR                         *NAZIONE
273600970526     C                   KFLD                    KCSCAP                         *CAP
273700130902     C*
273800130902     C     Kcos          KLIST
273900130903     C                   KFLD                    cosVER
274000130903     C                   KFLD                    cosnar
274100130903     C                   KFLD                    coscap
274200130903     C                   KFLD                    cosloc
274300130903     C                   KFLD                    coslna
274400130903     C     Kcos2         KLIST
274500130903     C                   KFLD                    cosVER
274600130903     C                   KFLD                    coslna
274700130903     C                   KFLD                    cosnar
274800130903     C                   KFLD                    coscap
274900130903     C                   KFLD                    cosloc
275000130903     C     Kcos22        KLIST
275100130903     C                   KFLD                    cosVER
275200130903     C                   KFLD                    coslna
275300130903     C                   KFLD                    cosnar
275400130903     C                   KFLD                    coscap
275500970526     C*
275600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
275700970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
275800970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
275900970526     C*
276000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
276100970526     C                   EXSR      IMPFUN
276200970526     C*
276300970526     C                   ENDSR
276400970526     C*------------------------------------------------------------------------*
276500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
276600970526     C*------------------------------------------------------------------------*
276700970526     C     FUNGES        BEGSR
276800970526     C*
276900970526     C* PULISCO CAMPI DI LAVORO
277000970526     C                   Z-ADD     0             W                 5 0
277100970526     C*
277200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
277300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
277400970526     C     1             DO        24            I
277500970526     C                   MOVEL     FUD(I)        FUDRIG
277600970526     C*
277700970526     C* PULIZIA FUNZIONI PRECEDENTI
277800970526     C                   MOVEL     *BLANKS       FUV(I)
277900970526     C*
278000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
278100970526     C* (SALTA F24)
278200970526     C     FUDATT        IFEQ      'S'
278300970526     C     FUDFUN        ANDNE     '24'
278400970526     C                   ADD       1             W
278500970526     C                   MOVEL     FUDDES        FUV(W)
278600970526     C                   END
278700970526     C                   END                                                    DO WHILE
278800970526     C*
278900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
279000970526     C* ALTRIMENTI DISATTIVO F24
279100970526     C     1             DO        24            I
279200970526     C                   MOVEL     FUD(I)        FUDRIG
279300970526     C*
279400970526     C     FUDFUN        IFEQ      '24'
279500970526     C     W             IFGT      6
279600970526     C                   MOVEL     'S'           FUDATT
279700970526     C                   ELSE
279800970526     C                   MOVEL     *BLANKS       FUDATT
279900970526     C                   END                                                    W GT 8
280000970526     C                   MOVEL     FUDRIG        FUD(I)
280100970526     C                   END
280200970526     C                   END                                                    DO WHILE
280300970526     C*
280400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
280500970526     C     W             DIV       6             I
280600970526     C                   MVR                     Z
280700970526     C*
280800970526     C     Z             IFNE      0
280900970526     C     I             ADD       1             Z
281000970526     C                   ELSE
281100970526     C                   Z-ADD     I             Z                 5 0
281200970526     C                   END
281300970526     C*
281400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
281500970526     C                   ADD       1             FUNGRU
281600970526     C     FUNGRU        IFGT      Z
281700970526     C     FUNGRU        ORLT      1
281800970526     C                   Z-ADD     1             FUNGRU
281900970526     C                   END
282000970526     C*
282100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
282200970526     C     FUNGRU        IFEQ      1
282300970526     C                   MOVEL     FUV(1)        FUN001
282400970526     C                   MOVEL     FUV(2)        FUN002
282500970526     C                   MOVEL     FUV(3)        FUN003
282600970526     C                   MOVEL     FUV(4)        FUN004
282700970526     C                   MOVEL     FUV(5)        FUN005
282800970526     C                   MOVEL     FUV(6)        FUN006
282900970526     C                   END
283000970526     C*
283100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
283200970526     C     FUNGRU        IFEQ      2
283300970526     C                   MOVEL     FUV(7)        FUN001
283400970526     C                   MOVEL     FUV(8)        FUN002
283500970526     C                   MOVEL     FUV(9)        FUN003
283600970526     C                   MOVEL     FUV(10)       FUN004
283700970526     C                   MOVEL     FUV(11)       FUN005
283800970526     C                   MOVEL     FUV(12)       FUN006
283900970526     C                   END
284000970526     C*
284100970526     C* TERZO GRUPPO DI 6 FUNZIONI
284200970526     C     FUNGRU        IFEQ      3
284300970526     C                   MOVEL     FUV(13)       FUN001
284400970526     C                   MOVEL     FUV(14)       FUN002
284500970526     C                   MOVEL     FUV(15)       FUN003
284600970526     C                   MOVEL     FUV(16)       FUN004
284700970526     C                   MOVEL     FUV(17)       FUN005
284800970526     C                   MOVEL     FUV(18)       FUN006
284900970526     C                   END
285000970526     C*
285100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
285200970526     C     FUNGRU        IFEQ      4
285300970526     C                   MOVEL     FUV(19)       FUN001
285400970526     C                   MOVEL     FUV(20)       FUN002
285500970526     C                   MOVEL     FUV(21)       FUN003
285600970526     C                   MOVEL     FUV(22)       FUN004
285700970526     C                   MOVEL     *BLANKS       FUN005
285800970526     C                   MOVEL     *BLANKS       FUN006
285900970526     C                   END
286000970526     C*
286100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
286200970526     C                   MOVEL     *BLANKS       FUN007
286300970526     C                   MOVEL     *BLANKS       FUN008
286400970526     C*
286500970526     C     1             DO        24            I
286600970526     C                   MOVEL     FUD(I)        FUDRIG
286700970526     C     FUDFUN        IFEQ      '24'
286800970526     C     FUDATT        ANDEQ     'S'
286900970526     C                   MOVEL     FUDDES        FUN008
287000970526     C                   END
287100970526     C                   END                                                    DO
287200970526     C*
287300970526     C                   ENDSR
287400970526     C*------------------------------------------------------------------------*
287500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
287600970526     C*------------------------------------------------------------------------*
287700970526     C     FUNCON        BEGSR
287800970526     C*
287900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
288000970526     C                   EXSR      FUNVAR
288100970526     C*
288200970526     C* PULISCO CAMPI DI LAVORO
288300970526     C                   Z-ADD     0             I                 5 0
288400970526     C                   Z-ADD     0             W                 5 0
288500970526     C*
288600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
288700970526     C     FUNKEY        IFNE      *BLANKS
288800970526     C     1             DO        24            I
288900970526     C                   MOVEL     FUD(I)        FUDRIG
289000970526     C*
289100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
289200970526     C     FUNKEY        IFEQ      FUDFUN
289300970526     C     FUDATT        ANDEQ     'S'
289400970526     C                   Z-ADD     I             W
289500970526     C                   END
289600970526     C                   END                                                    DO WHILE
289700970526     C*
289800970526     C* FUNZIONE NON ATTIVA (ERRORE)
289900970526     C     W             IFEQ      0
290000970526     C*
290100970526     C* EMISSIONE MESSAGGIO DI ERRORE
290200970526     C                   MOVEL     MSG(1)        DSMSMS
290300970526     C*
290400970526     C                   END                                                    W=0
290500970526     C                   END                                                    FUNKEY NE *BLANKS
290600970526     C*
290700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
290800970526     C     W             IFNE      0
290900970526     C     FUNKEY        OREQ      *BLANKS
291000970526     C     FUNKEY        IFEQ      *BLANKS
291100970526     C                   EXSR      ENTGES
291200970526     C                   END
291300970526     C     FUNKEY        IFEQ      '03'
291400970526     C                   EXSR      F03GES
291500970526     C                   END
291600010320     C     FUNKEY        IFEQ      '04'
291700010320     C                   EXSR      F04GES
291800010320     C                   END
291900130821     C     FUNKEY        IFEQ      '06'
292000130905     c***                if        f07lna='S' or f07los='S'
292100130905     c***                          or f07lol='S'
292200130905     C***                EXSR      F07GES
292300130905     c***                if        errl00='0'
292400130905     c***                clear                   f07lna
292500130905     c***                clear                   f07lol
292600130905     c***                clear                   f07los
292700130905     c***                endif
292800130903     c
292900130905     c***                else
293000130821     C                   EXSR      F06GES
293100130905     c***                endif
293200130902     c
293300130821     C                   END
293400130827     C     FUNKEY        IFEQ      '07'
293500130827     C                   EXSR      F07GES
293600130827     C                   END
293700970526     C     FUNKEY        IFEQ      '08'
293800970526     C                   EXSR      F08GES
293900970526     C                   END
294000970526     C     FUNKEY        IFEQ      '09'
294100970526     C                   EXSR      F09GES
294200970526     C                   END
294300970526     C     FUNKEY        IFEQ      '10'
294400970526     C                   EXSR      F10GES
294500970526     C                   END
294600970526     C     FUNKEY        IFEQ      '12'
294700970526     C                   EXSR      F12GES
294800970526     C                   END
294900970603     C     FUNKEY        IFEQ      '14'
295000970603     C                   EXSR      F14GES
295100970603     C                   END
295200130828     C     FUNKEY        IFEQ      '17'
295300130828     C                   EXSR      F17GES
295400130828     C                   END
295500970526     C     FUNKEY        IFEQ      '24'
295600970526     C                   EXSR      F24GES
295700970526     C                   END
295800970526     C                   END
295900970526     C*
296000970526     C                   ENDSR
296100970526     C*------------------------------------------------------------------------*
296200970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
296300970526     C*------------------------------------------------------------------------*
296400970526     C     FUNVAR        BEGSR
296500970526     C*
296600970526     C* INIZIALIZZA VARIABILE -FUNKEY-
296700970526     C*
296800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
296900970526     C     KEY           IFEQ      ENTER
297000970526     C                   MOVEL     *BLANKS       FUNKEY
297100970526     C                   END
297200970526     C     KEY           IFEQ      F1
297300970526     C                   MOVEL     '01'          FUNKEY
297400970526     C                   END
297500970526     C     KEY           IFEQ      F2
297600970526     C                   MOVEL     '02'          FUNKEY
297700970526     C                   END
297800970526     C     KEY           IFEQ      F3
297900970526     C                   MOVEL     '03'          FUNKEY
298000970526     C                   END
298100970526     C     KEY           IFEQ      F4
298200970526     C                   MOVEL     '04'          FUNKEY
298300970526     C                   END
298400970526     C     KEY           IFEQ      F5
298500970526     C                   MOVEL     '05'          FUNKEY
298600970526     C                   END
298700970526     C     KEY           IFEQ      F6
298800970526     C                   MOVEL     '06'          FUNKEY
298900970526     C                   END
299000970526     C     KEY           IFEQ      F7
299100970526     C                   MOVEL     '07'          FUNKEY
299200970526     C                   END
299300970526     C     KEY           IFEQ      F8
299400970526     C                   MOVEL     '08'          FUNKEY
299500970526     C                   END
299600970526     C     KEY           IFEQ      F9
299700970526     C                   MOVEL     '09'          FUNKEY
299800970526     C                   END
299900970526     C     KEY           IFEQ      F10
300000970526     C                   MOVEL     '10'          FUNKEY
300100970526     C                   END
300200970526     C     KEY           IFEQ      F11
300300970526     C                   MOVEL     '11'          FUNKEY
300400970526     C                   END
300500970526     C     KEY           IFEQ      F12
300600970526     C                   MOVEL     '12'          FUNKEY
300700970526     C                   END
300800970526     C     KEY           IFEQ      F13
300900970526     C                   MOVEL     '13'          FUNKEY
301000970526     C                   END
301100970526     C     KEY           IFEQ      F14
301200970526     C                   MOVEL     '14'          FUNKEY
301300970526     C                   END
301400970526     C     KEY           IFEQ      F15
301500970526     C                   MOVEL     '15'          FUNKEY
301600970526     C                   END
301700970526     C     KEY           IFEQ      F16
301800970526     C                   MOVEL     '16'          FUNKEY
301900970526     C                   END
302000970526     C     KEY           IFEQ      F17
302100970526     C                   MOVEL     '17'          FUNKEY
302200970526     C                   END
302300970526     C     KEY           IFEQ      F18
302400970526     C                   MOVEL     '18'          FUNKEY
302500970526     C                   END
302600970526     C     KEY           IFEQ      F19
302700970526     C                   MOVEL     '19'          FUNKEY
302800970526     C                   END
302900970526     C     KEY           IFEQ      F20
303000970526     C                   MOVEL     '20'          FUNKEY
303100970526     C                   END
303200970526     C     KEY           IFEQ      F21
303300970526     C                   MOVEL     '21'          FUNKEY
303400970526     C                   END
303500970526     C     KEY           IFEQ      F22
303600970526     C                   MOVEL     '22'          FUNKEY
303700970526     C                   END
303800970526     C     KEY           IFEQ      F23
303900970526     C                   MOVEL     '23'          FUNKEY
304000970526     C                   END
304100970526     C     KEY           IFEQ      F24
304200970526     C                   MOVEL     '24'          FUNKEY
304300970526     C                   END
304400970526     C     KEY           IFEQ      ROLLUP
304500970526     C                   MOVEL     'RU'          FUNKEY
304600970526     C                   END
304700970526     C     KEY           IFEQ      HELP
304800970526     C                   MOVEL     'HP'          FUNKEY
304900970526     C                   END
305000970526     C                   ENDSR
305100970526     C*------------------------------------------------------------------------*
305200970526     C* NOTE COSTRUZIONE SCHIERE
305300970526     C*------------------------------------------------------------------------*
305400970526     C*
305500970526     C* -MSG-MESSAGGI
305600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
305700970526     C* (NON TOCCARE MSG,1 E MSG,2)
305800970526     C*
305900970526     C* -CMD-COMANDI
306000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
306100970526     C*
306200970526     C* -OPD-OPZIONI
306300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
306400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
306500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
306600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
306700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
306800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
306900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
307000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
307100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
307200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
307300970526     C*
307400970526     C* -FUD-FUNZIONI
307500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
307600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
307700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
307800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
307900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
308000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
308100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
308200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
308300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
308400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
308500970526     O*------------------------------------------------------------------------*
308600970526** DEC - DECODIFICHE
308700970526    MODIFICA                                                      1
308800970526  ANNULLAMENTO                                                    2
308900970526VISUALIZZAZIONE                                                   3
309000970526   RIPRISTINO                                                     4
309100970526  INSERIMENTO                                                     5
309200970526** MSG - MESSAGGI
309300970526TISI03R- Tasto funzionale non gestito                                          1
309400970526TISI03R- Inserimento effettuato                                                2
309500970526TISI03R- Modifica effettuata                                                   3
309600970526TISI03R- Annullamento effettuato                                               4
309700970526TISI03R- Ripristino effettuato                                                 5
309800970526TISI03R- Richiesta funzione per record inesistente                             6
309900970526TISI03R- Record momentaneamente allocato: riprovare più tardi                  7
310000970527TISI03R- Richiesta funzione per record esistente                               8
310100970526TISI03R- Provincia errata                                                      9
310200970526TISI03R- Linea di arrivo errata                                               10
310300970526TISI03R- Zona di consegna errata                                              11
310400060927§§§§§§§§ Ufficio Postale inesistente o annullato                              12
310500970526TISI03R- Nazione errata                                                       13
310600970526TISI03R- Codice tassazione errato                                             14
310700970526TISI03R- Codice tassazione errato. Accettati solo I1, I2, I3, ecc ...         15
310800970526TISI03R- Per CAP estero lasciare il codice tassazione vuoto                   16
310900060927§§§§§§§§ Zona di consegna NON abilitata                                       17
311000970604TISI03R- Linea limite obbligatoria se immessi limiti di peso o volume         18
311100970604TISI03R- Limiti obbligatori se immesse linea o zona limite                    19
311200130905Esistono OrariServizi personalizz.x la linea variata. Saranno eliminati       20
311300970912TISI03R- Il limite oltre deve essere superiore del limite sotto               21
311400010320TISI03R- Tasto funzione non consentito sul campo corrente.                    22
311500130905TISI03R- F17 consentito soltando sui campi LINEA di ARRIVO valorizzati        23
311600010320TISI03R- Tipo tempo ritiro errato o incompleto                                24
311700970917TISI03R- Il cap può essere obsoleto se lo sono anche tutte le sue località    25
311800970917TISI03R- Impossibile inserire cap obsoleti                                    26
311900970526TISI03R- Immettere 'S' o 'N'                                                  27
312000110623TISI03R- Tipo inoltro errato o non utilizzabile nel cappario                  28
312100970917TISI03R- Immettere 'O' per rendere obsoleto il cap, altrimenti lasciare vuoto 29
312200970603TISI03R- Tipo tempo di consegna errato o incompleto                           30
312300970526** FUD - FUNZIONI
312400970526                   -        (S/N)=N                        funz.01¬
312500970530                   -        (S/N)=N                        funz.02¬
312600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
312700010320F04=Visual. tempi  -        (S/N)=S  <== S-FISSO           funz.04¬
312800130827                   -        (S/N)=N                        funz.05¬
312900970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
313000130827F07=Orari servizi  -        (S/N)=S                        funz.07¬
313100970526F08=Località       -        (S/N)=S                        funz.08¬
313200970526F09=Eccezioni      -        (S/N)=S                        funz.09¬
313300970526F10=Tempi consegna -        (S/N)=S                        funz.10¬
313400970526                   -        (S/N)=N                        funz.11¬
313500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
313600970526                   -        (S/N)=N                        funz.13¬
313700970603F14=Zone consegna  -        (S/N)=S                        funz.14¬
313800970526                   -        (S/N)=N                        funz.15¬
313900970526                   -        (S/N)=N                        funz.16¬
314000130905F17=OrariServLinea -        (S/N)=S                        funz.17¬
314100970526                   -        (S/N)=N                        funz.18¬
314200970526                   -        (S/N)=N                        funz.19¬
314300970526                   -        (S/N)=N                        funz.20¬
314400970526                   -        (S/N)=N                        funz.21¬
314500970526                   -        (S/N)=N                        funz.22¬
314600970526                   -        (S/N)=N                        funz.23¬
314700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
