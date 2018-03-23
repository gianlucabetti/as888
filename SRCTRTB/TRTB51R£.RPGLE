000100940111     H*------------------------------------------------------------------------*
000200940114     H* GESTIONE TABELLA TEMPI CALCOLO DELIVERY - ROUTINE
000300940111     H*------------------------------------------------------------------------*
000400940119     H DECEDIT('0,') DATEDIT(*DMY.)
000500940111     F*------------------------------------------------------------------------*
000600940111     F* DATA BASE
000700940111     F*------------------------------------------------------------------------*
000800940119     FTRTB51D   CF   E             WORKSTN INFDS(DEVDS1)
000900940119     F                                     SFILE(TB51DVM:MSGULT)
001000940117     FTABEL00F  UF A E           K DISK
001100061220     Ftntbe01l  UF A E           K DISK
001200940119     FAZORG01L  IF   E           K DISK
001300040624     d
001400040624     d prehh1          s                   Like(v1chh1)
001500040624     d WFORZ           s              1
001600940111     D*------------------------------------------------------------------------*
001700940111     D* SCHIERE
001800940111     D*------------------------------------------------------------------------*
001900130520     D MSG             S             78    DIM(21) CTDATA PERRCD(1)
002000930806     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
002100940117     D*-------------------
002200940117     D* DECODIFICA OPZIONE RICHIAMATA
002300940117     D*-------------------
002400940121     D DOPZ            S             16    DIM(5) CTDATA PERRCD(1)
002500930802     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600930803     D FFU             S              6    DIM(31)
002700930803     D FDE             S             19    DIM(31)
002800930803     D FAT             S              1    DIM(31)
002900930802     D* TABELLA DESCRIZIONI A VIDEO
003000930802     D FUV             S             19    DIM(8)
003100930802     D* TABELLA TASTI FUNZIONALI PREMUTI
003200930802     D KEYS            S              1    DIM(40)
003300940111     D*------------------------------------------------------------------------*
003400940111     D* INPUT
003500940111     D*------------------------------------------------------------------------*
003600940111     D*-------------------
003700930723     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003800940111     D*-------------------
003900930723     D DSFUN         E DS
004000930803     D  FUNRIG                 1     40
004100940117     D*-------------------
004200930127     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004300940111     D*-------------------
004400930127     D DSMSG         E DS
004500940111     D*-------------------
004600020923     D* DS REPERIMENTO DATI UTENTE
004700020923     D*-------------------
004800020923     D TIBS34DS      E DS
004900020923     D DDATIUTE      E DS
005000020923     D AZUTEDS       E DS                  extname(AZUTE00F)
005100940111     D*-------------------
005200000000     D* DS PER GESTIONE TASTI FUNZIONALI
005300940111     D*-------------------
005400930802     D DSKEYS        E DS
005500930803     D  KEYRIG                 1     40
005600940111     D*-------------------
005700930806     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005800940111     D*-------------------
005900930806     D                SDS
006000940111     D  USER                 254    263
006100940111     D*-------------------
006200930720     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006300940111     D*-------------------
006400000000     D DEVDS1          DS
006500930720     D* CHIAVE FUNZIONALE BATTUTA
006600000000     D  KEY                  369    369
006700930721     D* POSIZIONE CURSORE (IN HEX)
006800930721     D  CURSOR               370    371
006900930720     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
007000930720     D  FRTSFL               378    379B 0
007100940117     D*---------------------
007200940117     D* DS TEMPI FISSI CALCOLO DELIVERY
007300940117     D*---------------------
007400940117     D DS1X          E DS
007500061220     d
007600061220     D* DS PER TIBS02R - GESTIONE TNTBE00F
007700061220     D DSBS02        E DS                  EXTNAME(TIBS02DS)
007800061220     D trul09ds      E DS
007900940117     D*---------------------
008000940117     D* DS ESTERNA DI PROCEDURA
008100940117     D*---------------------
008200940117     D DS50          E DS                  EXTNAME(TRTB50DS)
008300940111     D*-------------------
008400000000     D* ARCHITETTURA
008500940111     D*-------------------
008600000000     D KPJBA         E DS
008700061220     dwatb             s              1
008800061220     dkcod             s                   like(tbecod) inz('SOT')
008900061220     dkke1             s                   like(tbeke1)
009000061220     dkke2             s                   like(tbeke2)
009100940111     C*------------------------------------------------------------------------*
009200940111     C* MAIN LINE
009300940111     C*------------------------------------------------------------------------*
009400931015     C*
009500940117     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
009600940117IF  1C     $FINE         IFEQ      '1'
009700931015     C*
009800931015     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
009900940117     C                   MOVEL     '1'           D50ERR
010000931015     C*
010100931015     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
010200940117X   1C                   ELSE
010300000000     C*
010400000000     C* CICLO FINO A CHE:
010500940117DO  2C     $FINE         DOWEQ     '0'
010600930805     C                   MOVEL     '0'           $ERR
010700000000     C*
010800930721     C* CONTROLLA GESTIONE VIDEO
010900940117IF  3C     $GES          IFEQ      'V1'
011000940117     C                   EXSR      GESV1
011100940117E   3C                   ENDIF
011200000000     C*
011300940117E   2C                   ENDDO
011400940117E   1C                   ENDIF
011500940105     C*
011600940117IF  1C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
011700940105     C     $F03          IFEQ      '1'
011800940117     C                   MOVEL     '1'           D50F03
011900940117E   1C                   ENDIF
012000940117     C* FINE PGM
012100000000     C                   SETON                                        LR
012200940111     C*------------------------------------------------------------------------*
012300940117     C* CARV1  - CARICA GESTIONE VIDEO 'TEMPO DELIVERY'
012400940111     C*------------------------------------------------------------------------*
012500930806     C     CARV1         BEGSR
012600930713     C*
012700930806     C                   MOVEL     'V1'          $GES
012800930713     C*
012900930804     C                   EXSR      CARFUN
013000930804     C                   EXSR      OUTFUN
013100930713     C*
013200940119     C                   WRITE     TB51DVT
013300940119     C                   WRITE     TB51DVF
013400930713     C*
013500930713     C                   ENDSR
013600940111     C*------------------------------------------------------------------------*
013700940117     C* GESV1  - GESTIONE VIDEO 'TEMPO DELIVERY'
013800940111     C*------------------------------------------------------------------------*
013900930806     C     GESV1         BEGSR
014000930804     C*
014100940117IF  1C     $TESVD        IFEQ      '1'
014200930806     C                   EXSR      CARV1
014300940117E   1C                   ENDIF
014400930804     C*
014500940117IF  1C     MSGULT        IFNE      *ZEROS
014600930804     C                   EXSR      OUTMSG
014700940117E   1C                   ENDIF
014800930804     C*
014900940119     C                   EXFMT     TB51DV1
015000930804     C*
015100930804     C                   EXSR      CONFUN
015200930804     C*
015300940117IF  1C     $ERR          IFEQ      '0'
015400940117SL  2C                   SELECT
015500930804     C     KEY           WHENEQ    ENTER
015600940117     C                   EXSR      ENTV1
015700930804     C     KEY           WHENEQ    F6
015800940117     C                   EXSR      F06GES
015900930804     C                   OTHER
016000930805     C                   EXSR      FUNSTD
016100940117E   2C                   ENDSL
016200940117E   1C                   ENDIF
016300930804     C*
016400930804     C                   ENDSR
016500940111     C*------------------------------------------------------------------------*
016600940117     C* ENTV1  - GESTIONE TASTO ENTER VIDEO 'TEMPO DELIVERY'
016700940111     C*------------------------------------------------------------------------*
016800930806     C     ENTV1         BEGSR
016900930806     C*
017000930806     C* CONTROLLA I CAMPI IMMESSI
017100930806     C                   EXSR      DECODI
017200940121     C*
017300940121     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
017400940121IF  1C     D50OP1        IFEQ      'O05'
017500940121     C                   MOVEL     '1'           $FINE
017600940121E   1C                   ENDIF
017700930804     C*
017800930804     C                   ENDSR
017900940111     C*------------------------------------------------------------------------*
018000940117     C* F06GES -GEST.TASTO FUNZ. F06 ===> CONFERMA OPERAZIONE
018100940111     C*------------------------------------------------------------------------*
018200930126     C     F06GES        BEGSR
018300930127     C*
018400930806     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
018500940117IF  1C     D50OP1        IFEQ      'F06'
018600940117     C                   EXSR      INSERI
018700940117E   1C                   ENDIF
018800940117IF  1C     D50OP1        IFEQ      'O02'
018900940117     C                   EXSR      MODIFI
019000940117E   1C                   ENDIF
019100940117IF  1C     D50OP1        IFEQ      'O04'
019200940117     C                   EXSR      CANCEL
019300940117E   1C                   ENDIF
019400940117IF  1C     D50OP1        IFEQ      'O07'
019500940117     C                   EXSR      RIPRIS
019600940117E   1C                   ENDIF
019700930127     C*
019800000000     C                   ENDSR
019900940111     C*------------------------------------------------------------------------*
020000930806     C* INSERI - FUNZIONE DI INSERIMENTO
020100940111     C*------------------------------------------------------------------------*
020200930806     C     INSERI        BEGSR
020300940119     C*
020400940119     C* PER OGNI RICHIESTA VENGONO CREATI DUE RECORD DI TABELLA X LA STESSA FIL.
020500940119     C* UNO E' IL TEMPO 'OSA' CHE CONTIENE LA PRIMA ORA SOGLIA, L'ALTRO E' IL
020600940119     C* TEMPO 'OSA1' CHE CONTIENE LA SECONDA ORA SOGLIA; ENTRAMBI HANNO COME
020700940119     C* ALTRI DATI DI TABELLA QUELLI STANDARD DI OSA E OSA1 CHE SONO STATI
020800940119     C* PASSATI DAL FILTRO
020900930806     C*
021000930806     C* CONTROLLA I CAMPI IMMESSI
021100930806     C                   EXSR      DECODI
021200940119     C*
021300940119IF  1C     $ERR          IFEQ      '0'
021400940119     C*
021500940119     C* CAMPI DI TABEL CHE VANNO RIEMPITI COSI' SIA PER 'OSA' CHE PER 'OSA1'
021600940119     C                   MOVEL     *BLANKS       TBLFLG
021700940119     C                   MOVEL     '1'           TBLFTT
021800940119     C                   Z-ADD     *ZEROS        TBLFLT
021900940119     C                   MOVEL     *BLANKS       TBLFTR
022000940119     C                   Z-ADD     *ZEROS        TBLDTR
022100940119     C*
022200940119     C*--------------------
022300940119     C* CREO RECORD 'OSA'
022400940119     C*--------------------
022500020923     C                   Z-ADD     1             TBLKUT
022600940119     C                   MOVEL     '1X'          TBLCOD
022700940119     C*
022800940119     C                   MOVEL     *BLANKS       TBLKEY
022900940119     C                   MOVEL     V1CFIL        OSA
023000940119     C                   MOVE      'OSA'         OSA
023100940119     C                   MOVEL     OSA           TBLKEY
023200940119     C*
023300940119     C                   MOVEL     OSADES        §1XDES
023400940119     C                   MOVEL     OSAFI1        §1XFI1
023500940119     C                   Z-ADD     OSAHHG        §1XHHG
023600940119     C                   MOVEL     OSAFI2        §1XFI2
023700940119     C*
023800940119     C                   Z-ADD     V1CHH1        §1XHHO
023900940119     C*
024000940119     C                   MOVEL     OSAFI3        §1XFI3
024100940119     C                   Z-ADD     OSAHHM        §1XHHM
024200940119     C                   MOVEL     OSAFI4        §1XFI4
024300940119     C                   Z-ADD     OSAHHP        §1XHHP
024400940119     C                   MOVEL     OSAFI5        §1XFI5
024500940119     C                   Z-ADD     OSAHHI        §1XHHI
024600940119     C                   MOVEL     OSAFI6        §1XFI6
024700940119     C                   Z-ADD     OSAHHS        §1XHHS
024800940119     C*
024900940119     C                   MOVEL     DS1X          TBLUNI
025000940119     C*
025100940119     C                   WRITE     TABEL
025200940119     C*--------------------
025300940119     C* CREO RECORD 'OSA1'
025400940119     C*--------------------
025500020923     C                   Z-ADD     1             TBLKUT
025600940119     C                   MOVEL     '1X'          TBLCOD
025700940119     C*
025800940119     C                   MOVEL     *BLANKS       TBLKEY
025900940119     C                   MOVEL     V1CFIL        OSA1
026000940119     C                   MOVE      'OSA1'        OSA1
026100940119     C                   MOVEL     OSA1          TBLKEY
026200940119     C*
026300940119     C                   MOVEL     OS1DES        §1XDES
026400940119     C                   MOVEL     OS1FI1        §1XFI1
026500940119     C                   Z-ADD     OS1HHG        §1XHHG
026600940119     C                   MOVEL     OS1FI2        §1XFI2
026700940119     C*
026800940119     C                   Z-ADD     V1CHH2        §1XHHO
026900940119     C*
027000940119     C                   MOVEL     OS1FI3        §1XFI3
027100940119     C                   Z-ADD     OS1HHM        §1XHHM
027200940119     C                   MOVEL     OS1FI4        §1XFI4
027300940119     C                   Z-ADD     OS1HHP        §1XHHP
027400940119     C                   MOVEL     OS1FI5        §1XFI5
027500940119     C                   Z-ADD     OS1HHI        §1XHHI
027600940119     C                   MOVEL     OS1FI6        §1XFI6
027700940119     C                   Z-ADD     OS1HHS        §1XHHS
027800940119     C*
027900940119     C                   MOVEL     DS1X          TBLUNI
028000940119     C*
028100940119     C                   WRITE     TABEL
028101130520
028102130520     c* Creo record tabella OST
028103130520     c                   eval      watb=' '
028104130520     c                   exsr      tabost
028200061220     c
028300061220     c* Creo record tabella sot
028400061220     c                   eval      watb=' '
028500061220     c                   exsr      tabsot
028600940119     C*
028700940119     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
028800940119     C                   MOVEL     '1'           $FINE
028900940119     C*
029000930806     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
029100931210     C                   MOVEL     MSG(17)       DSMSMS
029200930806     C*
029300940119E   1C                   ENDIF
029400940119     C*
029500930806     C                   ENDSR
029600061220     C***************************************************************
029700061220     C* AGGIORNAMEnTO TABELLA SOT
029800061220     C***************************************************************
029900061220     c     TABSOT        BEGSR
030000061220     c
030100061220     c                   movel     'SOT'         kcod
030200061220     c                   movel     v1cfil        kke1
030300061220     c                   clear                   kke2
030400061220     c     ktbe          chain     tntbe01l
030500061220     c
030600061220     c* Tabella non necessaria
030700061220    1c                   if        v1csot=0
030800061220     c*  Se presente la cancello
030900061220    2c                   if        %found(tntbe01l)
031000061220     c                   movel     'A'           tbeatb
031100061220     c                   clear                   tbeftr
031200061220     c                   update    tntbe000
031300061220    2c                   endif
031400061220     c
031500061220  x1ac                   else
031600061220     C                   CLEAR                   DSBS02
031700061220     C                   MOVEL     'C'           T02MOD
031800061220     C                   MOVEL     KNSIF         T02SIF
031900061220     C                   MOVEL     'SOT'         T02COD
032000061220     C                   MOVEL     'GEN'         T02KE1
032100061220     C                   CALL      'TIBS02R'
032200061220     C                   PARM                    KPJBA
032300061220     C                   PARM                    DSBS02
032400061220     c                   movel     t02uni        gensot            4 0
032500061220     c
032600061220    2c                   if        gensot = v1csot
032700061220    3c                   if        %found(tntbe01l)
032800061220     c                   movel     'A'           tbeatb
032900061220     c                   clear                   tbeftr
033000061220     c                   update    tntbe000
033100061220    3c                   endif
033200061220     c
033300061220   x2c                   else
033400061220     c                   movel     watb          tbeatb
033500061220     c                   movel     'SOT'         tbecod
033600061220     c                   movel(P)  v1cfil        tbeke1
033700061220     c                   clear                   tbeke2
033800061220     c                   movel     'TR'          tbeapl
033900061220     c                   clear                   tbesif
034000061220     c                   clear                   tbelin
034100061220     c                   movel(P)  v1csot        tbeuni
034200061220     c                   movel     'S'           tbeftt
034300061220     c                   clear                   tbeftr
034400061220    3c                   if        not %found(tntbe01l)
034500061220     c                   clear                   tbedtr
034600061220     c                   write     tntbe000
034700061220     c                   else
034800061220     c                   update    tntbe000
034900061220    3c                   endif
035000061220     c*
035100061220    2c                   endif
035200061220    1c                   endif
035300061220     C                   ENDSR
035301130520     C***************************************************************
035302130520     C* AGGIORNAMEnTO TABELLA OST
035303130520     C***************************************************************
035304130520     c     TABOST        BEGSR
035305130520
035306130520     c* Prima chaino il record standard
035307130520     c                   clear                   ds1x
035308130520     c                   clear                   genost            4 0
035309130520     C                   Z-ADD     1             KTBKUT
035310130520     C                   MOVEL     '1X'          KTBCOD
035311130520     C                   MOVEL     *BLANKS       KTBKEY
035312130520     c                   clear                   OSA
035313130520     C                   MOVE      'OST'         OSA
035314130520     C                   MOVEL     OSA           KTBKEY
035315130520     C     KEYTAB        CHAIN     TABEL00F
035316130520     c                   if        %found(tabel00F)
035317130520     c                   eval      ds1x=tbluni
035318130520     c                   eval      genost=§1xhho
035319130520     c                   endif
035320130520     c
035321130520     C                   Z-ADD     1             KTBKUT
035322130520     C                   MOVEL     '1X'          KTBCOD
035323130520     C                   MOVEL     *BLANKS       KTBKEY
035324130520     C                   MOVEL     V1CFIL        OSA
035325130520     C                   MOVE      'OST'         OSA
035326130520     C                   MOVEL     OSA           KTBKEY
035327130520     C     KEYTAB        CHAIN     TABEL00F
035328130520     c
035329130520     c* Tabella non necessaria
035330130520    1c                   if        v1cost=0
035331130520     c*  Se presente la cancello
035332130520    2c                   if        %found(tabel00f)
035333130520     c                   movel     '*'           tblflg
035334130520     C                   MOVEL     *BLANKS       TBLFTR
035335130520     C                   Z-ADD     *ZEROS        TBLDTR
035336130520     c                   update    tabel
035337130520    2c                   endif
035338130520     c
035339130520  x1ac                   else
035340130520     c
035341130520    2c                   if        v1cfil>*zeros and genost = v1cost
035342130520    3c                   if        %found(tabel00f)
035343130520     c                   movel     '*'           tblflg
035344130520     C                   MOVEL     *BLANKS       TBLFTR
035345130520     C                   Z-ADD     *ZEROS        TBLDTR
035346130520     c                   update    tabel
035347130520    3c                   endif
035348130520     c
035349130520   x2c                   else
035350130520     c                   if        watb=*blanks
035351130520     c                   clear                   tblflg
035352130520     c                   else
035353130520     c                   eval      tblflg='*'
035354130520     c                   endif
035355130520     c                   movel     '1X'          tblcod
035356130520     c                   movel(P)  ktbkey        tblkey
035357130520     C                   MOVEL     OSTDES        §1XDES
035358130520     C                   MOVEL     OSTFI1        §1XFI1
035359130520     C                   Z-ADD     OSTHHG        §1XHHG
035360130520     C                   MOVEL     OSTFI2        §1XFI2
035361130520     C*
035362130520     C                   Z-ADD     V1COST        §1XHHO
035363130520     C*
035364130520     C                   MOVEL     OSTFI3        §1XFI3
035365130520     C                   Z-ADD     OSTHHM        §1XHHM
035366130520     C                   MOVEL     OSTFI4        §1XFI4
035367130520     C                   Z-ADD     OSTHHP        §1XHHP
035368130520     C                   MOVEL     OSTFI5        §1XFI5
035369130520     C                   Z-ADD     OSTHHI        §1XHHI
035370130520     C                   MOVEL     OSTFI6        §1XFI6
035371130520     C                   Z-ADD     OSTHHS        §1XHHS
035372130520     C*
035373130520     C                   MOVEL     DS1X          TBLUNI
035374130520     C                   MOVEL     *BLANKS       TBLFTR
035375130520     C                   Z-ADD     *ZEROS        TBLDTR
035376130520    3c                   if        not %found(tabel00f)
035377130520     C                   MOVEL     '1'           TBLFTT
035378130520     C                   Z-ADD     *ZEROS        TBLFLT
035379130520     c                   write     tabel
035380130520     c                   else
035381130520     c                   update    tabel
035382130520    3c                   endif
035383130520     c*
035384130520    2c                   endif
035385130520    1c                   endif
035386130520     C                   ENDSR
035400940111     C*------------------------------------------------------------------------*
035500930806     C* MODIFI - FUNZIONE DI MODIFICA
035600940111     C*------------------------------------------------------------------------*
035700930806     C     MODIFI        BEGSR
035800930806     C*
035900930806     C* CONTROLLA I CAMPI IMMESSI
036000930806     C                   EXSR      DECODI
036100930806     C*
036200940119IF  1C     $ERR          IFEQ      '0'
036300930806     C*
036400940119     C* RIAGGANCIO IL RECORD 'OSA' PER LA VARIAZIONE
036500020923     C                   Z-ADD     1             KTBKUT
036600940119     C                   MOVEL     '1X'          KTBCOD
036700940119     C                   MOVEL     *BLANKS       KTBKEY
036800940119     C                   MOVEL     V1CFIL        OSA
036900940119     C                   MOVE      'OSA'         OSA
037000940119     C                   MOVEL     OSA           KTBKEY
037100940119     C     KEYTAB        CHAIN     TABEL00F                           99
037200940119IF  2C     *IN99         IFEQ      *OFF
037300940119     C                   MOVEL     TBLUNI        DS1X
037400940119     C                   Z-ADD     V1CHH1        §1XHHO
037500940119     C                   MOVEL     DS1X          TBLUNI
037600940119     C                   MOVEL     *BLANKS       TBLFTR
037700940119     C                   Z-ADD     *ZEROS        TBLDTR
037800940119     C*
037900940119     C                   UPDATE    TABEL
038000940119     C*
038100940119     C* RIAGGANCIO IL RECORD 'OSA1' PER LA VARIAZIONE
038200020923     C                   Z-ADD     1             KTBKUT
038300940119     C                   MOVEL     '1X'          KTBCOD
038400940119     C                   MOVEL     *BLANKS       KTBKEY
038500940119     C                   MOVEL     V1CFIL        OSA1
038600940119     C                   MOVE      'OSA1'        OSA1
038700940119     C                   MOVEL     OSA1          KTBKEY
038800940119     C     KEYTAB        CHAIN     TABEL00F                           99
038900940119IF  3C     *IN99         IFEQ      *OFF
039000940119     C                   MOVEL     TBLUNI        DS1X
039100940119     C                   Z-ADD     V1CHH2        §1XHHO
039200940119     C                   MOVEL     DS1X          TBLUNI
039300940119     C                   MOVEL     *BLANKS       TBLFTR
039400940119     C                   Z-ADD     *ZEROS        TBLDTR
039500940119     C*
039600940119     C                   UPDATE    TABEL
039601130520
039602130520     C*
039603130520     c* Tabella OST
039604130520     c                   eval      watb=' '
039605130520     c                   EXSR      TABOST
039700940119     C*
039800061220     c* Tabella SOT
039900061220     c                   eval      watb=' '
040000061220     c                   EXSR      TABSOT
040100061220     c
040200930806     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
040300930906     C                   MOVEL     '1'           $FINE
040400940119     C*
040500940119     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
040600940119     C                   MOVEL     MSG(18)       D50MSG
040700930806     C*
040800940119E   3C                   ENDIF
040900940119E   2C                   ENDIF
041000940119E   1C                   ENDIF
041100930806     C*
041200930806     C                   ENDSR
041300940111     C*------------------------------------------------------------------------*
041400930806     C* CANCEL - FUNZIONE DI CANCELLAZIONE
041500940111     C*------------------------------------------------------------------------*
041600930806     C     CANCEL        BEGSR
041700940119     C*
041800040624     c                   exsr      Sr_atten
041900040624     c*
042000040624     c     $err          ifeq      '0'
042100940119     C* RIAGGANCIO IL RECORD 'OSA' PER LA VARIAZIONE
042200020923     C                   Z-ADD     1             KTBKUT
042300940119     C                   MOVEL     '1X'          KTBCOD
042400940119     C                   MOVEL     *BLANKS       KTBKEY
042500940119     C                   MOVEL     V1CFIL        OSA
042600940119     C                   MOVE      'OSA'         OSA
042700940119     C                   MOVEL     OSA           KTBKEY
042800940119     C     KEYTAB        CHAIN     TABEL00F                           99
042900940119IF  1C     *IN99         IFEQ      *OFF
043000940119     C*
043100940119     C                   MOVEL     '*'           TBLFLG
043200940119     C                   MOVEL     *BLANKS       TBLFTR
043300940119     C                   Z-ADD     *ZEROS        TBLDTR
043400940119     C*
043500940119     C                   UPDATE    TABEL
043600940119     C*
043700940119     C* RIAGGANCIO IL RECORD 'OSA1' PER LA VARIAZIONE
043800020923     C                   Z-ADD     1             KTBKUT
043900940119     C                   MOVEL     '1X'          KTBCOD
044000940119     C                   MOVEL     *BLANKS       KTBKEY
044100940119     C                   MOVEL     V1CFIL        OSA1
044200940119     C                   MOVE      'OSA1'        OSA1
044300940119     C                   MOVEL     OSA1          KTBKEY
044400940119     C     KEYTAB        CHAIN     TABEL00F                           99
044500940119IF  2C     *IN99         IFEQ      *OFF
044600940119     C*
044700940119     C                   MOVEL     '*'           TBLFLG
044800940119     C                   MOVEL     *BLANKS       TBLFTR
044900940119     C                   Z-ADD     *ZEROS        TBLDTR
045000940119     C*
045100130520     C                   UPDATE    TABEL
045101130520
045102130520     c* Aggiornamento tabella OSt
045103130520     c                   eval      watb='A'
045104130520     c                   exsr      tabOST
045105130520
045200061220     c* Aggiornamento tabella SOT
045300061220     c                   eval      watb='A'
045400061220     c                   exsr      tabsot
045500940119     C*
045600940119     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
045700940119     C                   MOVEL     '1'           $FINE
045800940119     C*
045900940119     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
046000940119     C                   MOVEL     MSG(19)       D50MSG
046100940119     C*
046200940119E   2C                   ENDIF
046300940119E   1C                   ENDIF
046400040624     c                   endif
046500940119     C*
046600930806     C                   ENDSR
046700940111     C*------------------------------------------------------------------------*
046800930806     C* RIPRIS - FUNZIONE DI RIPRISTINO
046900940111     C*------------------------------------------------------------------------*
047000930806     C     RIPRIS        BEGSR
047100931217     C*
047200040624     c                   exsr      sr_atten
047300040624     c*
047400040624     c     $err          ifeq      '0'
047500130520     C* RIAGGANCIO IL RECORD 'OSA' PER LA VARIAZIONE
047600020923     C                   Z-ADD     1             KTBKUT
047700940119     C                   MOVEL     '1X'          KTBCOD
047800940119     C                   MOVEL     *BLANKS       KTBKEY
047900940119     C                   MOVEL     V1CFIL        OSA
048000940119     C                   MOVE      'OSA'         OSA
048100940119     C                   MOVEL     OSA           KTBKEY
048200940119     C     KEYTAB        CHAIN     TABEL00F                           99
048300940119IF  1C     *IN99         IFEQ      *OFF
048400940119     C*
048500940119     C                   MOVEL     *BLANKS       TBLFLG
048600940119     C                   MOVEL     *BLANKS       TBLFTR
048700940119     C                   Z-ADD     *ZEROS        TBLDTR
048800940119     C*
048900940119     C                   UPDATE    TABEL
049000940119     C*
049100940119     C* RIAGGANCIO IL RECORD 'OSA1' PER LA VARIAZIONE
049200020923     C                   Z-ADD     1             KTBKUT
049300940119     C                   MOVEL     '1X'          KTBCOD
049400940119     C                   MOVEL     *BLANKS       KTBKEY
049500940119     C                   MOVEL     V1CFIL        OSA1
049600940119     C                   MOVE      'OSA1'        OSA1
049700940119     C                   MOVEL     OSA1          KTBKEY
049800940119     C     KEYTAB        CHAIN     TABEL00F                           99
049900940119IF  2C     *IN99         IFEQ      *OFF
050000940119     C*
050100940119     C                   MOVEL     *BLANKS       TBLFLG
050200940119     C                   MOVEL     *BLANKS       TBLFTR
050300940119     C                   Z-ADD     *ZEROS        TBLDTR
050400940119     C*
050500940119     C                   UPDATE    TABEL
050501130520     c* Aggiornamento tabella OST
050502130520     c                   eval      watb=' '
050503130520     c                   exsr      tabOST
050600061220     c* Aggiornamento tabella SOT
050700061220     c                   eval      watb=' '
050800061220     c                   exsr      tabsot
050900940119     C*
051000940119     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
051100940119     C                   MOVEL     '1'           $FINE
051200940119     C*
051300940119     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
051400940119     C                   MOVEL     MSG(20)       D50MSG
051500940119     C*
051600940119E   2C                   ENDIF
051700940119E   1C                   ENDIF
051800040624     c                   endif
051900940119     C*
052000930806     C                   ENDSR
052100940111     C*------------------------------------------------------------------------*
052200930906     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
052300940111     C*------------------------------------------------------------------------*
052400930906     C     DECODI        BEGSR
052500930205     C*
052600940117     C* IMPOSTO INDICATORI DI ERRORE CAMPI
052700930906     C                   SETOFF                                       606162
052701130520     C                   SETOFF                                       64
052800940117     C*--------------------
052900940119     C* FILIALE (SOLO SE INSERIMENTO)
053000940117     C*--------------------
053100940119IF  1C     D50OP1        IFEQ      'F06'
053200940119IF  2C     V1CFIL        IFEQ      '   '
053300940119     C                   MOVEL     '1'           $ERR
053400940119     C                   MOVEL     MSG(8)        $MSG
053500940119     C                   EXSR      CARMSG
053600940119     C                   SETON                                        60
053700940119     C*
053800940119X   2C                   ELSE
053900940119     C                   MOVEL     V1CFIL        $FIL              3
054000940119     C                   MOVEL     *BLANKS       $DFIL            20
054100940119     C                   EXSR      CHKFIL
054200940119IF  3C     $ERR          IFEQ      '1'
054300940119     C                   EXSR      CARMSG
054400940119     C                   SETON                                        60
054500940119E   3C                   ENDIF
054600940119     C                   MOVEL     $DFIL         V1DFIL
054700940119E   2C                   ENDIF
054800940119E   1C                   ENDIF
054900940119     C*--------------------
055000940119     C* PRIMA ORA SOGLIA
055100940119     C*--------------------
055200940119IF  1C     $ERR          IFEQ      '0'
055300940119     C                   Z-ADD     V1CHH1        $ORA              4 0
055400940119     C                   EXSR      CHKORA
055500940119IF  1C     $ERR          IFEQ      '1'
055600940119     C                   MOVEL     MSG(9)        $MSG
055700940119     C                   EXSR      CARMSG
055800940119     C                   SETON                                        61
055900940119E   2C                   ENDIF
056000940119E   1C                   ENDIF
056100940119     C*--------------------
056200940119     C* SECONDA ORA SOGLIA
056300940119     C*--------------------
056400940119IF  1C     $ERR          IFEQ      '0'
056500940119     C                   Z-ADD     V1CHH2        $ORA
056600940119     C                   EXSR      CHKORA
056700940119IF  1C     $ERR          IFEQ      '1'
056800940119     C                   MOVEL     MSG(9)        $MSG
056900940119     C                   EXSR      CARMSG
057000940901     C                   SETON                                        62
057100940119E   2C                   ENDIF
057200940119E   1C                   ENDIF
057300940901     C* LA SECONDA SOGLIA DEVE ESSERE MAGGIORE O UGUALE ALLA PRIMA SOGLIA
057400940901IF  1C     $ERR          IFEQ      '0'
057500940901IF  2C     V1CHH2        IFLT      V1CHH1
057600940901     C                   MOVEL     MSG(11)       $MSG
057700940901     C                   EXSR      CARMSG
057800940901     C                   SETON                                        62
057900940901E   2C                   ENDIF
058000940901E   1C                   ENDIF
058100061220     c
058200061220     c* Se si tratta di terminal di arrivo, richiedo anche tabella SOT
058300061220     c                   clear                   trul09ds
058400061220     c                   movel     v1cfil        i09tfa
058500061220     c                   movel     'TFA'         i09mod
058600061220     c                   call      'TRUL09R'
058700061220     c                   parm                    trul09ds
058800061220     c
058901130520    1c                   if        o09err=' '  and $err='0'
059000061220     c* il campo deve essere pieno, altrimenti imposto ora generica
059100061220    2c                   if        v1csot=0
059200061220     C                   MOVEL     MSG(12)       $MSG
059300061220     C                   EXSR      CARMSG
059400061220     C                   SETON                                        63
059500061220     c                   movel     '1'           $err
059600061220E   2C                   ENDIF
059700061220     c                   endif
059800061220     c
059900061220IF  2C     $ERR          IFEQ      '0'
060000061220     c                   if        v1csot>0
060100061220     C                   Z-ADD     V1Csot        $ORA              4 0
060200061220     C                   EXSR      CHKORA
060300061220IF  3C     $ERR          IFEQ      '1'
060400061220     C                   MOVEL     MSG(9)        $MSG
060500061220     C                   EXSR      CARMSG
060600061220     C                   SETON                                        63
060700061220E   3C                   ENDIF
060800061220E   3C                   ENDIF
060900061220E   2C                   ENDIF
061000040624     c*
061002130520    1c                   if        (v1cfil=*blanks or o09err=' ')  and $err='0'
061003130520     c* il campo deve essere pieno, altrimenti imposto ora generica
061004130520    2c                   if        v1cost=0
061005130520     C                   MOVEL     MSG(21)       $MSG
061006130520     C                   EXSR      CARMSG
061007130520     C                   SETON                                        64
061008130520     c                   movel     '1'           $err
061009130520E   2C                   ENDIF
061010130520     c                   endif
061011130520     c
061012130520IF  2C     $ERR          IFEQ      '0'
061013130520     c                   if        v1cost>0
061014130520     C                   Z-ADD     V1Cost        $ORA              4 0
061015130520     C                   EXSR      CHKORA
061016130520IF  3C     $ERR          IFEQ      '1'
061017130520     C                   MOVEL     MSG(9)        $MSG
061018130520     C                   EXSR      CARMSG
061019130520     C                   SETON                                        64
061020130520E   3C                   ENDIF
061021130520E   3C                   ENDIF
061022130520E   2C                   ENDIF
061100040624     c                   exsr      sr_atten
061200930715     C*
061300930715     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
061400930715     C* CURSORE SUL CAMPO DEL CONTROL
061500940117IF  1C     $ERR          IFEQ      '1'
061600930805     C                   MOVEL     '0'           *IN43
061700940117X   1C                   ELSE
061800930805     C                   MOVEL     '1'           *IN43
061900940117E   1C                   ENDIF
062000930205     C*
062100930205     C                   ENDSR
062200040624     C*------------------------------------------------------------------------*
062300040624     C*  SR_atten -  Emette messaggio informativo per CED
062400040624     C*------------------------------------------------------------------------*
062500040624     C     SR_atten      BEGSR
062600040624     C*
062700040624     c* se premuto F6 ed è stata modificata la prima ora di soglia avverto
062800040624     c* di avvisare il CED per poter modificare anche la tabella "SOT"
062900040624     c*
063000040624     c* gestione o inserimento
063100040624     c     $ERR          ifeq      '0'
063200040624     c     key           andeq     F6
063300040624     c     prehh1        andne     v1chh1
063400040624     c     wforz         andeq     *blanks
063500040624     c* ripristino
063600040624     c     $ERR          oreq      '0'
063700040624     c     key           andeq     F6
063800040624     c     d50op1        andeq     'O07'
063900040624     c     wforz         andeq     *blanks
064000040624     c* cancellazione
064100040624     c     $ERR          oreq      '0'
064200040624     c     key           andeq     F6
064300040624     c     d50op1        andeq     'O04'
064400040624     c     wforz         andeq     *blanks
064500040624     c                   eval      wforz='F'
064600040624     c                   seton                                        41
064700040624     c                   eval      $err='1'
064800040624     c                   endif
064900040624     c*
065000040624     c                   endsr
065100940111     C*------------------------------------------------------------------------*
065200940117     C*  CHKFIL - CONTROLLA LA FILIALE IMMESSA
065300940119     C*           INPUT --> $FIL:  FILIALE IMMESSA
065400940119     C*                     $DFIL: DECODIFCA FILIALE VUOTA
065500940119     C*           OUTPUT--> $DFIL: DECODIFIA FILIALE
065600940111     C*------------------------------------------------------------------------*
065700940117     C     CHKFIL        BEGSR
065800940119     C*
065900020923     C                   Z-ADD     1             KTBKUT
066000940119     C                   MOVEL     '1X'          KTBCOD
066100940119     C                   MOVEL     $FIL          OSA
066200940119     C                   MOVE      'OSA'         OSA
066300940119     C                   MOVEL     *BLANKS       KTBKEY
066400940119     C                   MOVEL     OSA           KTBKEY
066500940119     C*
066600940119     C* PER QUELLA FILIALE TEMPO GIA' ESISTENTE, ERRORE
066700940119     C     KEYTAB        CHAIN     TABEL00F                           99
066800940119IF  1C     *IN99         IFEQ      *OFF
066900940119     C                   MOVEL     '1'           $ERR
067000940119     C                   MOVEL     MSG(10)       $MSG
067100940119X   1C                   ELSE
067200940119     C*
067300940119     C* FILIALE CON CARATTERI ALFANUMERICI, ERRORE
067400940119IF  2C     $FIL          IFLT      *ZEROS
067500940119     C                   MOVEL     '1'           $ERR
067600940119     C                   MOVEL     MSG(7)        $MSG
067700940119X   2C                   ELSE
067800940119     C*
067900940119     C* FILIALE INESITENTENTE NELL'ORGANIGRAMMA, ERRORE
068000940119     C                   MOVEL     $FIL          KORFIL
068100940119     C     KEYORG        CHAIN     AZORG01L                           99
068200940119IF  3C     *IN99         IFEQ      *ON
068300940119     C                   MOVEL     '1'           $ERR
068400940119     C                   MOVEL     MSG(7)        $MSG
068500940119X   3C                   ELSE
068600940119     C                   MOVEL     ORGDES        $DFIL            20
068700940119E   3C                   ENDIF
068800940119E   2C                   ENDIF
068900940119E   1C                   ENDIF
069000940119     C*
069100930712     C                   ENDSR
069200940111     C*------------------------------------------------------------------------*
069300940119     C*  CHKORA - CONTROLLA L' ORA DI SOGLIA
069400940119     C*           INPUT --> $ORA:  ORE/MINUTI IMMESSI
069500940111     C*------------------------------------------------------------------------*
069600940119     C     CHKORA        BEGSR
069700930712     C*
069800940119     C                   Z-ADD     *ZEROS        $HH               2 0
069900940119     C                   Z-ADD     *ZEROS        $MM               2 0
070000940119     C*
070100940119     C                   MOVEL     $ORA          $HH
070200940119     C                   MOVE      $ORA          $MM
070300940119     C*
070400940119IF  1C     $HH           IFGT      23
070500940119     C                   MOVEL     '1'           $ERR
070600940119E   1C                   ENDIF
070700940119     C*
070800940119IF  1C     $MM           IFGT      59
070900940119     C                   MOVEL     '1'           $ERR
071000940119E   1C                   ENDIF
071100940119     C*
071200930712     C                   ENDSR
071300940119     C*------------------------------------------------------------------------*
071400940119     C*  RECVID - RIEMPE I CAMPI DAI RECORD TABELLA AL VIDEO
071500940119     C*------------------------------------------------------------------------*
071600940119     C     RECVID        BEGSR
071700940119     C*
071800940119     C* DATI PRIMA ORA SOGLIA PER ARRIVO TRAZIONI ('OSA')
071900940119     C                   MOVEL     D50FIL        V1CFIL
072000061220     C
072100061220     C                   IF        D50ATB<>' '
072200061220     C                   SETON                                        01
072300061220     C                   ENDIF
072400940121     C*
072500940121     C* DECODIFCA FILIALE
072501130520     c                   if        d50fil=*blanks
072502130520     C                   eval      V1DFIL = 'STANDARD'
072503130520     c                   else
072600940121     C                   MOVEL     D50FIL        KORFIL
072700940121     C     KEYORG        CHAIN     AZORG01L                           99
072800940121IF  1C     *IN99         IFEQ      *OFF
072900940121     C                   MOVEL     ORGDES        V1DFIL
073000940121X   1C                   ELSE
073100940121     C                   MOVEL     *ALL'*'       V1DFIL
073200940121E   1C                   ENDIF
073201130520     c                   endif
073300940119     C*
073400940119     C                   MOVEL     TBLUNI        DS1X
073500940119     C                   Z-ADD     §1XHHO        V1CHH1
073600940119     C*
073700940119     C* DATI SECONDA ORA SOGLIA PER ARRIVO TRAZIONI ('OSA1')
073800020923     C                   MOVEL     1             KTBKUT
073900940119     C                   MOVEL     '1X'          KTBCOD
074000940119     C                   MOVEL     *BLANKS       KTBKEY
074100940119     C                   MOVEL     D50FIL        OSA1              7
074200940119     C                   MOVE      'OSA1'        OSA1
074300940119     C                   MOVEL     OSA1          KTBKEY
074400940119     C*
074500940119     C* SE IL RECORD NON ESISTE VALORIZZO A '99:99'
074600940119     C     KEYTAB        CHAIN     TABEL00F                           99
074700940119IF  1C     *IN99         IFEQ      '0'
074800940119     C                   MOVEL     TBLUNI        DS1X
074900940119     C                   Z-ADD     §1XHHO        V1CHH2
075000940119X   1C                   ELSE
075100940119     C                   Z-ADD     9999          V1CHH2
075200940119E   1C                   ENDIF
075201130520     c                   movel     osthho        v1cost
075300061220     c* tabella SOT
075400061220     c                   movel     d50sot        v1csot
075500940119     C*
075600940119     C                   ENDSR
075700940119     C*------------------------------------------------------------------------*
075800940119     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
075900940119     C*------------------------------------------------------------------------*
076000940119     C     AZZVID        BEGSR
076100940119     C*
076200940119     C                   MOVEL     *ZEROS        V1CFIL
076300940119     C                   MOVEL     *BLANKS       V1DFIL
076400940119     C                   Z-ADD     OSAHHO        V1CHH1
076500940119     C                   Z-ADD     OS1HHO        V1CHH2
076501130520     C                   Z-ADD     OSTHHO        V1Cost
076600940119     C*
076700940119     C                   ENDSR
076800020923     C*-------------------------------------------------------------------------
076900020923     C* REPERISCE I DATI UTENTE
077000020923     C*-------------------------------------------------------------------------
077100020923     C     REPDATIUTE    BEGSR
077200020923     C*
077300020923     C* INIZIALIZZA VARIABILI DI WRK
077400020923     C                   CLEAR                   TIBS34DS
077500020923     C                   CLEAR                   AZUTEDS
077600020923     C                   CLEAR                   DDATIUTE
077700020923     C*
077800020923     C     *DTAARA       DEFINE    §azute        azuteds
077900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
078000020923     C                   IN(E)     *DTAARA
078100020923if  1C                   IF        %Error
078200020923     c                   EVAL      I34Tla = 'L'
078300020923     C                   CALL      'TIBS34R'
078400020923     C                   PARM                    Tibs34Ds
078500020923     C                   IN        *DTAARA
078600020923e   1C                   ENDIF
078700020923     C*
078800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
078900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
079000020923     C*
079100020923     C                   ENDSR
079200940111     C*------------------------------------------------------------------------*
079300930713     C*  *INZSR - ROUTINE INIZIALE
079400940111     C*------------------------------------------------------------------------*
079500930713     C     *INZSR        BEGSR
079600000000     C*
079700000000     C* RICEVIMENTO PARAMETRI
079800000000     C     *ENTRY        PLIST
079900940114     C                   PARM                    DS50
080000940117     C*
080100940117     C* DEFINIZIONE VARIABILI
080200940117     C     *LIKE         DEFINE    TBLKUT        KTBKUT
080300940117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
080400940117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
080500940117     C     *LIKE         DEFINE    ORGFIL        KORFIL
080600940119     C                   MOVEL     'G'           $TGES             1
080700940117     C*
080800940119     C* CHIAVE LETTURA TABEL00F - COMPLETA
080900940117     C     KEYTAB        KLIST
081000940117     C                   KFLD                    KTBKUT
081100940117     C                   KFLD                    KTBCOD
081200940117     C                   KFLD                    KTBKEY
081300061220     C     Ktbe          KLIST
081400061220     C                   KFLD                    KCOD
081500061220     C                   KFLD                    KKE1
081600061220     C                   KFLD                    KKE2
081700930310     C*
081800930310     C* CHIAVE LETTURA AZORG01L - COMPLETA
081900930310     C     KEYORG        KLIST
082000940117     C                   KFLD                    KORFIL
082100930714     C*
082200930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
082300930803     C                   EXSR      INZSTD
082400940117     C*
082500940117     C                   MOVEL     DSFIRS        VTDRSU
082600930714     C*
082700940117     C* DECODIFICA OPZIONE RICHIAMATA
082800940117IF  1C     D50OP1        IFEQ      'F06'
082900940117     C                   MOVEL     DOPZ(1)       VTDFUN
083000940117E   1C                   ENDIF
083100940117IF  2C     D50OP1        IFEQ      'O02'
083200940117     C                   MOVEL     DOPZ(2)       VTDFUN
083300940117E   1C                   ENDIF
083400940117IF  1C     D50OP1        IFEQ      'O04'
083500940117     C                   MOVEL     DOPZ(3)       VTDFUN
083600940117E   1C                   ENDIF
083700940121IF  1C     D50OP1        IFEQ      'O05'
083800940121     C                   MOVEL     DOPZ(5)       VTDFUN
083900940121E   1C                   ENDIF
084000940117IF  1C     D50OP1        IFEQ      'O07'
084100940117     C                   MOVEL     DOPZ(4)       VTDFUN
084200940117E   1C                   ENDIF
084300931015     C*
084400931015     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
084500931015     C* 0=NON PROTETTO, 1=PROTETTO
084600940117     C*   90 - CAMPI CHIAVE (FILIALE)
084700940117     C*   91 - ALTRI CAMPI  (SOGLIE)
084800940117IF  1C     D50OP1        IFEQ      'F06'
084900940117     C                   SETOFF                                       9091
085000940117E   1C                   ENDIF
085100940117IF  1C     D50OP1        IFEQ      'O02'
085200940117     C                   SETON                                        90
085300940117E   1C                   ENDIF
085400940117IF  1C     D50OP1        IFEQ      'O04'
085500940121     C     D50OP1        OREQ      'O05'
085600940117     C     D50OP1        OREQ      'O07'
085700940117     C                   SETON                                        9091
085800940117E   1C                   ENDIF
085900940121     C*
086000940121     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
086100940121IF  1C     D50OP1        IFEQ      'O05'
086200940121     C                   MOVEL     FUN(6)        FUNRIG
086300940121     C                   MOVEL     'N'           FUNASG
086400940121     C                   MOVEL     'N'           FUNASI
086500940121     C                   MOVEL     'N'           FUNAFG
086600940121     C                   MOVEL     'N'           FUNAFG
086700940121     C                   MOVEL     FUNRIG        FUN(6)
086800940121E   1C                   ENDIF
086900931015     C*
087000940121     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
087100940117IF  1C     D50OP1        IFEQ      'O02'
087200940117     C     D50OP1        OREQ      'O04'
087300940121     C     D50OP1        OREQ      'O05'
087400940117     C     D50OP1        OREQ      'O07'
087500020923     C                   MOVEL     1             KTBKUT
087600940117     C                   MOVEL     '1X'          KTBCOD
087700940117     C                   MOVEL     *BLANKS       KTBKEY
087800940117     C                   MOVEL     D50FIL        OSA               6
087900940117     C                   MOVE      'OSA'         OSA
088000940117     C                   MOVEL     OSA           KTBKEY
088100940117     C*
088200940117     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
088300940117     C     KEYTAB        CHAIN     TABEL00F                           9998
088400940117IF  2C     *IN99         IFEQ      '1'
088500931015     C                   MOVEL     '1'           $ERR
088600931015     C                   MOVEL     '1'           $FINE
088700940117     C                   MOVEL     MSG(14)       D50MSG
088800940117X   2C                   ELSE
088900940117IF  3C     *IN98         IFEQ      '1'
089000940117     C                   MOVEL     '1'           $ERR
089100940117     C                   MOVEL     '1'           $FINE
089200940117     C                   MOVEL     MSG(15)       D50MSG
089300940117X   3C                   ELSE
089400940117     C                   EXSR      RECVID
089500940117     C                   EXSR      CARV1
089600940117E   3C                   ENDIF
089700940117E   2C                   ENDIF
089800940117E   1C                   ENDIF
089900940117     C*
090000940117     C* SE INSERIMENTO
090100940117IF  1C     D50OP1        IFEQ      'F06'
090200940117     C                   EXSR      AZZVID
090300940117     C                   EXSR      CARV1
090400940117E   1C                   ENDIF
090500040624     c
090600040624     c                   setoff                                       41
090700040624     c     d50op1        ifne      'F06'
090800040624     c                   eval      prehh1=v1chh1
090900040624     c                   else
091000040624     c                   clear                   prehh1
091100040624     c                   endif
091200000000     C*
091300000000     C                   ENDSR
091400940111     C*------------------------------------------------------------------------*
091500940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091600940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091700940111     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
091800940111     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
091900940111     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
092000940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
092100940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
092200940111     C*------------------------------------------------------------------------*
092300930803     C* INZSTD - ROUTINE INIZIALE STANDARD
092400940111     C*------------------------------------------------------------------------*
092500930803     C     INZSTD        BEGSR
092600930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
092700930802     C                   CALL      'XKEYS'
092800930802     C                   PARM                    DSKEYS
092900930805     C                   MOVEA     KEYRIG        KEYS
093000930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
093100930714     C     XMSGPA        PLIST
093200930714     C                   PARM                    DSMSMS
093300930714     C                   PARM                    DSMSPG
093400930714     C                   PARM                    DSMSKE
093500930714     C                   PARM                    DSMSER
093600930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
093700940119     C                   MOVEL     'TRTB51R'     DSMSPG
093800940119     C                   MOVEL     'TRTB51R'     VMCQUE
093900930806     C                   MOVEL     *BLANKS       VMCKEY
094000930714     C                   MOVEL     *BLANKS       DSMSMS
094100930805     C* INIZIALIZZO FLAG CONTROLLO PGM
094200930721     C                   MOVEL     '0'           $ERR              1
094300930721     C                   MOVEL     '0'           $F03              1
094400930806     C                   MOVEL     *BLANKS       $GES              2
094500930721     C                   MOVEL     *BLANKS       $MSG             80
094600930721     C                   MOVEL     '0'           $FINE             1
094700930721     C                   MOVEL     '0'           $TESVD            1
094800930714     C* LANCIO ROUTINE PER VERIFICARE SE
094900930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
095000930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
095100020923     C                   EXSR      REPDATIUTE
095200930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
095300930721     C                   BITOFF    '01234567'    POSMI1            1
095400930721     C                   BITOFF    '01234567'    POSMI2            1
095500930721     C                   BITOFF    '01234567'    POSMA1            1
095600930721     C                   BITOFF    '01234567'    POSMA2            1
095700930721     C                   BITON     '34'          POSMI1            1
095800930721     C                   BITON     '7'           POSMI2            1
095900930721     C                   BITON     '34'          POSMA1            1
096000930721     C                   BITON     '0'           POSMA2            1
096100930721     C                   MOVE      POSMI2        POSMIN            2
096200930721     C                   MOVEL     POSMI1        POSMIN
096300930721     C                   MOVE      POSMA2        POSMAX            2
096400930721     C                   MOVEL     POSMA1        POSMAX
096500930714     C                   ENDSR
096600940111     C*------------------------------------------------------------------------*
096700930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
096800940111     C*------------------------------------------------------------------------*
096900930804     C     FUNSTD        BEGSR
097000930804     C                   SELECT
097100930804     C     KEY           WHENEQ    F3
097200930804     C                   EXSR      F03STD
097300930804     C     KEY           WHENEQ    F12
097400930804     C                   EXSR      F12STD
097500930804     C     KEY           WHENEQ    F24
097600930804     C                   EXSR      F24STD
097700930804     C                   OTHER
097800930804     C                   MOVEL     MSG(2)        $MSG
097900930804     C                   EXSR      CARMSG
098000930804     C                   ENDSL
098100930804     C                   ENDSR
098200940111     C*------------------------------------------------------------------------*
098300930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
098400940111     C*------------------------------------------------------------------------*
098500930804     C     F03STD        BEGSR
098600940105     C* IMPOSTO FLAG DI PREMUTO TASTO F03
098700940105     C                   MOVEL     '1'           $F03
098800930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
098900930804     C                   MOVEL     '1'           $FINE
099000930804     C                   ENDSR
099100940111     C*------------------------------------------------------------------------*
099200930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
099300940111     C*------------------------------------------------------------------------*
099400930804     C     F12STD        BEGSR
099500930805     C* IMPOSTO FLAG DI FIN PGM
099600930804     C                   MOVEL     '1'           $FINE
099700930804     C                   ENDSR
099800940111     C*------------------------------------------------------------------------*
099900930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
100000940111     C*------------------------------------------------------------------------*
100100930804     C     F24STD        BEGSR
100200930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
100300930804     C                   EXSR      OUTFUN
100400930804     C                   ENDSR
100500940111     C*------------------------------------------------------------------------*
100600930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
100700930721     C*          CODA MESSAGGI DEL PGM
100800940111     C*------------------------------------------------------------------------*
100900930721     C     CARMSG        BEGSR
101000930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
101100930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
101200930721     C     MSGULT        IFEQ      0
101300930721     C                   MOVEL     '1'           *IN40
101400940119     C                   WRITE     TB51DVK
101500930721     C                   MOVEL     '0'           *IN40
101600930805     C                   ENDIF
101700930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
101800930721     C                   MOVEL     $MSG          DSMSMS
101900930721     C                   CALL      'XMSG'        XMSGPA
102000930721     C     DSMSER        IFEQ      0
102100930806     C                   MOVEL     DSMSKE        VMCKEY
102200930805     C                   ENDIF
102300930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
102400931015     C                   ADD       1             MSGULT            4 0
102500940119     C                   WRITE     TB51DVM
102600930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
102700930721     C                   MOVEL     *BLANKS       $MSG
102800930721     C                   ENDSR
102900940111     C*------------------------------------------------------------------------*
103000930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
103100940111     C*------------------------------------------------------------------------*
103200930721     C     OUTMSG        BEGSR
103300930721     C* VISUALIZZAZIONE MESSAGGIO
103400930721     C                   MOVEL     '1'           *IN44
103500940119     C                   WRITE     TB51DVK
103600930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
103700930721     C                   Z-ADD     *ZEROS        MSGULT
103800930721     C                   ENDSR
103900940111     C*------------------------------------------------------------------------*
104000930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
104100930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
104200930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
104300930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
104400930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
104500940111     C*------------------------------------------------------------------------*
104600930803     C     CARFUN        BEGSR
104700930803     C                   Z-ADD     *ZEROS        FUNULT            5 0
104800931015     C     1             DO        31            I                 5 0
104900930803     C                   MOVEL     FUN(I)        FUNRIG
105000930805     C                   MOVEL     FUNFUN        FFU(I)
105100930805     C                   MOVEL     FUNDES        FDE(I)
105200020923     C     DUTLPO        IFEQ      'S'
105300940119     C     $TGES         IFEQ      'G'
105400940119     C                   MOVEL     FUNASG        FAT(I)
105500940119     C                   ELSE
105600940119     C                   MOVEL     FUNASI        FAT(I)
105700940119     C                   ENDIF
105800940119     C                   ELSE
105900940119     C     $TGES         IFEQ      'G'
106000940119     C                   MOVEL     FUNAFG        FAT(I)
106100940119     C                   ELSE
106200940119     C                   MOVEL     FUNAFI        FAT(I)
106300940119     C                   ENDIF
106400940119     C                   ENDIF
106500930806     C     FAT(I)        IFEQ      'E'
106600930806     C                   MOVEL     USER          WEDP              3
106700930806     C     WEDP          IFEQ      'EDP'
106800930806     C                   MOVEL     'S'           FAT(I)
106900930806     C                   ELSE
107000930806     C                   MOVEL     'N'           FAT(I)
107100930806     C                   ENDIF
107200930806     C                   ENDIF
107300930803     C                   ENDDO
107400930803     C                   ENDSR
107500940111     C*------------------------------------------------------------------------*
107600930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
107700930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
107800930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
107900930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
108000940111     C*------------------------------------------------------------------------*
108100930803     C     OUTFUN        BEGSR
108200930805     C     1             DO        8             I
108300930805     C                   MOVEL     *BLANKS       FUV(I)
108400930803     C                   ENDDO
108500930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
108600930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0
108700930803     C     1             DO        31            I
108800930803     C     FAT(I)        IFEQ      'S'
108900930803     C     FDE(I)        ANDNE     *BLANKS
109000930803     C                   ADD       1             FUNNAT
109100930803     C                   ENDIF
109200930803     C                   ENDDO
109300930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
109400930803     C     FUNNAT        IFGT      8
109500930803     C                   MOVEL     'S'           FAT(24)
109600930803     C                   ENDIF
109700930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
109800930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
109900930803     C                   Z-ADD     8             FUNVIS            5 0
110000930803     C     FAT(23)       IFEQ      'S'
110100930803     C                   SUB       1             FUNVIS
110200930803     C                   ENDIF
110300930803     C     FAT(24)       IFEQ      'S'
110400930803     C                   SUB       1             FUNVIS
110500930803     C                   ENDIF
110600930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
110700931015     C                   Z-ADD     *ZEROS        W                 5 0
110800930803     C                   ADD       1             FUNULT
110900930803     C     FUNULT        IFGT      31
111000930803     C                   Z-ADD     1             FUNULT
111100930805     C                   ENDIF
111200930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
111300930803     C     FUNULT        DO        31            I
111400930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
111500930803     C     W             IFLT      FUNVIS
111600930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
111700930803     C     FAT(I)        IFEQ      'S'
111800930803     C     FDE(I)        ANDNE     *BLANKS
111900930805     C     I             IFEQ      23
112000930805     C     I             OREQ      24
112100930803     C                   ELSE
112200930803     C                   ADD       1             W
112300930803     C                   MOVEL     FDE(I)        FUV(W)
112400930803     C                   ENDIF
112500930803     C                   ENDIF
112600930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
112700930803     C                   Z-ADD     I             FUNULT
112800930803     C                   ENDIF
112900930803     C                   ENDDO
113000930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
113100930806     C                   MOVEL     FUV(1)        VFD001
113200930806     C                   MOVEL     FUV(2)        VFD002
113300930806     C                   MOVEL     FUV(3)        VFD003
113400930806     C                   MOVEL     FUV(4)        VFD004
113500930806     C                   MOVEL     FUV(5)        VFD005
113600930806     C                   MOVEL     FUV(6)        VFD006
113700930803     C     FAT(23)       IFEQ      'N'
113800930803     C     FAT(24)       ANDEQ     'N'
113900930806     C                   MOVEL     FUV(7)        VFD007
114000930806     C                   MOVEL     FUV(8)        VFD008
114100930803     C                   ELSE
114200930803     C     FAT(23)       IFEQ      'S'
114300930803     C     FAT(24)       ANDEQ     'S'
114400930806     C                   MOVEL     FDE(23)       VFD007
114500930806     C                   MOVEL     FDE(24)       VFD008
114600930803     C                   ELSE
114700930806     C                   MOVEL     FUV(7)        VFD007
114800930803     C     FAT(23)       IFEQ      'S'
114900930806     C                   MOVEL     FDE(23)       VFD008
115000930803     C                   ELSE
115100930806     C                   MOVEL     FDE(24)       VFD008
115200930805     C                   ENDIF
115300930805     C                   ENDIF
115400930805     C                   ENDIF
115500930803     C                   MOVEL     '1'           $TESVD
115600930803     C                   ENDSR
115700940111     C*------------------------------------------------------------------------*
115800930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
115900930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
116000930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
116100930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
116200940111     C*------------------------------------------------------------------------*
116300930803     C     CONFUN        BEGSR
116400930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
116500930803     C     1             DO        31            I
116600930803     C     KEY           IFEQ      KEYS(I)
116700940105     C* CONTROLLO SE IL TASTO E' ATTIVO
116800930803     C     FAT(I)        IFNE      'S'
116900930804     C                   MOVEL     MSG(1)        $MSG
117000930803     C                   EXSR      CARMSG
117100930803     C                   MOVEL     '1'           $ERR
117200930803     C                   ENDIF
117300930803     C                   ENDIF
117400930803     C                   ENDDO
117500930803     C                   ENDSR
117600940121** ======== SCHIERA: MSG  (MESSAGGI)                       ================
117700940119TRTB51R- Tasto funzionale non valido.                                          1
117800940119TRTB51R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
117900940119TRTB51R- Opzione non valida.                                                   3
118000940119TRTB51R- Opzione abilitata ma non gestita. Avvertire CED.                      4
118100940119TRTB51R- Inizio elenco.                                                        5
118200940119TRTB51R- Fine elenco.                                                          6
118300940119TRTB51R- Filiale errata o inesistente                                          7
118400940119TRTB51R- Impossibile inserire il tempo della filiale '   '                     8
118500940119TRTB51R- Ore/Minuti errati                                                     9
118600940119TRTB51R- Tempo della filiale già esistente: impossibile l'inserimento         10
118700940901TRTB51R- La seconda soglia deve essere maggiore o uguale alla prima           11
118800061220TRTB51R- Ora soglia per calcolo data teorica arrivo trazioni                  12
118900061220TRTB51R-Ora soglia tabella "SOT"  necessaria solo x Term.arrivo
119000940119TRTB51R- Richiesta funzione per Tempo Delivery inesistente                    14
119100940119TRTB51R- Tempo Delivery momentaneamente allocato: riprovare più tardi         15
119200940119TRTB51R- 16
119300940119TRTB51R- Inserimento effettuato.                                              17
119400940119TRTB51R- Modifica effettuata.                                                 18
119500940119TRTB51R- Cancellazione effettuata.                                            19
119600940119TRTB51R- Ripristino effettuato.                                               20
119601130520TRTB51R- Ora soglia per Elenco Trazioni in Arrivo obbligatoria                21
119700930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
119800930806F1    -                   -    NN     NN
119900930723F2    -                   -    NN     NN
120000930802F3    -F3=Fine            -    SS     SS
120100930723F4    -                   -    NN     NN
120200930723F5    -                   -    NN     NN
120300940119F6    -F6=Aggiornamento   -    SS     SS
120400930723F7    -                   -    NN     NN
120500930723F8    -                   -    NN     NN
120600940119F9    -                   -    NN     NN
120700930723F10   -                   -    NN     NN
120800930723F11   -                   -    NN     NN
120900930802F12   -F12=Ritorno        -    SS     SS
121000930723F13   -                   -    NN     NN
121100930723F14   -                   -    NN     NN
121200930723F15   -                   -    NN     NN
121300930723F16   -                   -    NN     NN
121400930723F17   -                   -    NN     NN
121500930723F18   -                   -    NN     NN
121600930723F19   -                   -    NN     NN
121700930723F20   -                   -    NN     NN
121800940119F21   -                   -    NN     NN
121900930804F22   -                   -    NN     NN
122000930723F23   -                   -    NN     NN
122100930803F24   -F24=Altri tasti    -    NN     NN
122200930802ENTER -                   -    SS     SS
122300930802ROLLUP-                   -    NN     NN
122400930802ROLLDN-                   -    NN     NN
122500930802HELP  -                   -    NN     NN
122600930802PRINT -                   -    NN     NN
122700930802BSPACE-                   -    NN     NN
122800930802CLEAR -                   -    NN     NN
122900940117** DOPZ  (OPZIONE RICHIAMATA)
123000940119* INSERIMENTO *                                                 1
123100940119*  VARIAZIONE  *                                                2
123200940119*CANCELLAZIONE*                                                 3
123300940119*  RIPRISTINO  *                                                4
123400940121VISUALIZZAZIONE                                                 5
