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
028200130520
028300130520     c* Creo record tabella OST
028400130520     c                   eval      watb=' '
028500130520     c                   exsr      tabost
028600061220     c
028700061220     c* Creo record tabella sot
028800061220     c                   eval      watb=' '
028900061220     c                   exsr      tabsot
029000940119     C*
029100940119     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
029200940119     C                   MOVEL     '1'           $FINE
029300940119     C*
029400930806     C* MANDO MESSAGGIO DI INSERIMENTO EFFETTUATO
029500931210     C                   MOVEL     MSG(17)       DSMSMS
029600930806     C*
029700940119E   1C                   ENDIF
029800940119     C*
029900930806     C                   ENDSR
030000061220     C***************************************************************
030100061220     C* AGGIORNAMEnTO TABELLA SOT
030200061220     C***************************************************************
030300061220     c     TABSOT        BEGSR
030400061220     c
030500061220     c                   movel     'SOT'         kcod
030600150821     c                   if        v1cfil<=*zeros
030700150821     c                   movel     'GEN'         kke1
030800150821     c                   else
030900150821     c                   movel     v1cfil        kke1
031000150821     c                   endif
031100061220     c                   clear                   kke2
031200061220     c     ktbe          chain     tntbe01l
031300061220     c
031400150821     c* Tabella non necessaria : no terminal di arrivo o
031500150821     c*  uguale allo standard
031600061220    1c                   if        v1csot=0
031700061220     c*  Se presente la cancello
031800061220    2c                   if        %found(tntbe01l)
031900061220     c                   movel     'A'           tbeatb
032000061220     c                   clear                   tbeftr
032100061220     c                   update    tntbe000
032200061220    2c                   endif
032300061220     c
032400061220  x1ac                   else
032500150821     c* non lo faccio se sto modificando il record generico
032600150821     c                   if        v1cfil>*zeros
032700061220     C                   CLEAR                   DSBS02
032800061220     C                   MOVEL     'C'           T02MOD
032900061220     C                   MOVEL     KNSIF         T02SIF
033000061220     C                   MOVEL     'SOT'         T02COD
033100061220     C                   MOVEL     'GEN'         T02KE1
033200061220     C                   CALL      'TIBS02R'
033300061220     C                   PARM                    KPJBA
033400061220     C                   PARM                    DSBS02
033500061220     c                   movel     t02uni        gensot            4 0
033600150821     c                   endif
033700061220     c
033800150821    2c                   if        gensot = v1csot and v1cfil>*zeros
033900061220    3c                   if        %found(tntbe01l)
034000061220     c                   movel     'A'           tbeatb
034100061220     c                   clear                   tbeftr
034200061220     c                   update    tntbe000
034300061220    3c                   endif
034400061220     c
034500061220   x2c                   else
034600061220     c                   movel     watb          tbeatb
034700061220     c                   movel     'SOT'         tbecod
034800061220     c                   movel(P)  v1cfil        tbeke1
034900150821     c                   if        v1cfil<=*zeros
035000150821     c                   movel     'GEN'         tbeke1
035100150821     c                   endif
035200061220     c                   clear                   tbeke2
035300061220     c                   movel     'TR'          tbeapl
035400061220     c                   clear                   tbesif
035500061220     c                   clear                   tbelin
035600061220     c                   movel(P)  v1csot        tbeuni
035700061220     c                   movel     'S'           tbeftt
035800061220     c                   clear                   tbeftr
035900061220    3c                   if        not %found(tntbe01l)
036000061220     c                   clear                   tbedtr
036100061220     c                   write     tntbe000
036200061220     c                   else
036300061220     c                   update    tntbe000
036400061220    3c                   endif
036500061220     c*
036600061220    2c                   endif
036700061220    1c                   endif
036800061220     C                   ENDSR
036900130520     C***************************************************************
037000130520     C* AGGIORNAMEnTO TABELLA OST
037100130520     C***************************************************************
037200130520     c     TABOST        BEGSR
037300130520
037400130520     c* Prima chaino il record standard
037500130520     c                   clear                   ds1x
037600130520     c                   clear                   genost            4 0
037700130520     C                   Z-ADD     1             KTBKUT
037800130520     C                   MOVEL     '1X'          KTBCOD
037900130520     C                   MOVEL     *BLANKS       KTBKEY
038000130520     c                   clear                   OSA
038100130520     C                   MOVE      'OST'         OSA
038200130520     C                   MOVEL     OSA           KTBKEY
038300130520     C     KEYTAB        CHAIN     TABEL00F
038400130520     c                   if        %found(tabel00F)
038500130520     c                   eval      ds1x=tbluni
038600130520     c                   eval      genost=§1xhho
038700130520     c                   endif
038800130520     c
038900130520     C                   Z-ADD     1             KTBKUT
039000130520     C                   MOVEL     '1X'          KTBCOD
039100130520     C                   MOVEL     *BLANKS       KTBKEY
039200130520     C                   MOVEL     V1CFIL        OSA
039300130520     C                   MOVE      'OST'         OSA
039400130520     C                   MOVEL     OSA           KTBKEY
039500130520     C     KEYTAB        CHAIN     TABEL00F
039600130520     c
039700130520     c* Tabella non necessaria
039800130520    1c                   if        v1cost=0
039900130520     c*  Se presente la cancello
040000130520    2c                   if        %found(tabel00f)
040100130520     c                   movel     '*'           tblflg
040200130520     C                   MOVEL     *BLANKS       TBLFTR
040300130520     C                   Z-ADD     *ZEROS        TBLDTR
040400130520     c                   update    tabel
040500130520    2c                   endif
040600130520     c
040700130520  x1ac                   else
040800130520     c
040900130520    2c                   if        v1cfil>*zeros and genost = v1cost
041000130520    3c                   if        %found(tabel00f)
041100130520     c                   movel     '*'           tblflg
041200130520     C                   MOVEL     *BLANKS       TBLFTR
041300130520     C                   Z-ADD     *ZEROS        TBLDTR
041400130520     c                   update    tabel
041500130520    3c                   endif
041600130520     c
041700130520   x2c                   else
041800130520     c                   if        watb=*blanks
041900130520     c                   clear                   tblflg
042000130520     c                   else
042100130520     c                   eval      tblflg='*'
042200130520     c                   endif
042300130520     c                   movel     '1X'          tblcod
042400130520     c                   movel(P)  ktbkey        tblkey
042500130520     C                   MOVEL     OSTDES        §1XDES
042600130520     C                   MOVEL     OSTFI1        §1XFI1
042700130520     C                   Z-ADD     OSTHHG        §1XHHG
042800130520     C                   MOVEL     OSTFI2        §1XFI2
042900130520     C*
043000130520     C                   Z-ADD     V1COST        §1XHHO
043100130520     C*
043200130520     C                   MOVEL     OSTFI3        §1XFI3
043300130520     C                   Z-ADD     OSTHHM        §1XHHM
043400130520     C                   MOVEL     OSTFI4        §1XFI4
043500130520     C                   Z-ADD     OSTHHP        §1XHHP
043600130520     C                   MOVEL     OSTFI5        §1XFI5
043700130520     C                   Z-ADD     OSTHHI        §1XHHI
043800130520     C                   MOVEL     OSTFI6        §1XFI6
043900130520     C                   Z-ADD     OSTHHS        §1XHHS
044000130520     C*
044100130520     C                   MOVEL     DS1X          TBLUNI
044200130520     C                   MOVEL     *BLANKS       TBLFTR
044300130520     C                   Z-ADD     *ZEROS        TBLDTR
044400130520    3c                   if        not %found(tabel00f)
044500130520     C                   MOVEL     '1'           TBLFTT
044600130520     C                   Z-ADD     *ZEROS        TBLFLT
044700130520     c                   write     tabel
044800130520     c                   else
044900130520     c                   update    tabel
045000130520    3c                   endif
045100130520     c*
045200130520    2c                   endif
045300130520    1c                   endif
045400130520     C                   ENDSR
045500940111     C*------------------------------------------------------------------------*
045600930806     C* MODIFI - FUNZIONE DI MODIFICA
045700940111     C*------------------------------------------------------------------------*
045800930806     C     MODIFI        BEGSR
045900930806     C*
046000930806     C* CONTROLLA I CAMPI IMMESSI
046100930806     C                   EXSR      DECODI
046200930806     C*
046300940119IF  1C     $ERR          IFEQ      '0'
046400930806     C*
046500940119     C* RIAGGANCIO IL RECORD 'OSA' PER LA VARIAZIONE
046600020923     C                   Z-ADD     1             KTBKUT
046700940119     C                   MOVEL     '1X'          KTBCOD
046800940119     C                   MOVEL     *BLANKS       KTBKEY
046900940119     C                   MOVEL     V1CFIL        OSA
047000940119     C                   MOVE      'OSA'         OSA
047100940119     C                   MOVEL     OSA           KTBKEY
047200940119     C     KEYTAB        CHAIN     TABEL00F                           99
047300940119IF  2C     *IN99         IFEQ      *OFF
047400940119     C                   MOVEL     TBLUNI        DS1X
047500940119     C                   Z-ADD     V1CHH1        §1XHHO
047600940119     C                   MOVEL     DS1X          TBLUNI
047700940119     C                   MOVEL     *BLANKS       TBLFTR
047800940119     C                   Z-ADD     *ZEROS        TBLDTR
047900940119     C*
048000940119     C                   UPDATE    TABEL
048100940119     C*
048200940119     C* RIAGGANCIO IL RECORD 'OSA1' PER LA VARIAZIONE
048300020923     C                   Z-ADD     1             KTBKUT
048400940119     C                   MOVEL     '1X'          KTBCOD
048500940119     C                   MOVEL     *BLANKS       KTBKEY
048600940119     C                   MOVEL     V1CFIL        OSA1
048700940119     C                   MOVE      'OSA1'        OSA1
048800940119     C                   MOVEL     OSA1          KTBKEY
048900940119     C     KEYTAB        CHAIN     TABEL00F                           99
049000940119IF  3C     *IN99         IFEQ      *OFF
049100940119     C                   MOVEL     TBLUNI        DS1X
049200940119     C                   Z-ADD     V1CHH2        §1XHHO
049300940119     C                   MOVEL     DS1X          TBLUNI
049400940119     C                   MOVEL     *BLANKS       TBLFTR
049500940119     C                   Z-ADD     *ZEROS        TBLDTR
049600940119     C*
049700940119     C                   UPDATE    TABEL
049800130520
049900130520     C*
050000130520     c* Tabella OST
050100130520     c                   eval      watb=' '
050200130520     c                   EXSR      TABOST
050300940119     C*
050400061220     c* Tabella SOT
050500061220     c                   eval      watb=' '
050600061220     c                   EXSR      TABSOT
050700061220     c
050800930806     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
050900930906     C                   MOVEL     '1'           $FINE
051000940119     C*
051100940119     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
051200940119     C                   MOVEL     MSG(18)       D50MSG
051300930806     C*
051400940119E   3C                   ENDIF
051500940119E   2C                   ENDIF
051600940119E   1C                   ENDIF
051700930806     C*
051800930806     C                   ENDSR
051900940111     C*------------------------------------------------------------------------*
052000930806     C* CANCEL - FUNZIONE DI CANCELLAZIONE
052100940111     C*------------------------------------------------------------------------*
052200930806     C     CANCEL        BEGSR
052300940119     C*
052400040624     c                   exsr      Sr_atten
052500040624     c*
052600040624     c     $err          ifeq      '0'
052700940119     C* RIAGGANCIO IL RECORD 'OSA' PER LA VARIAZIONE
052800020923     C                   Z-ADD     1             KTBKUT
052900940119     C                   MOVEL     '1X'          KTBCOD
053000940119     C                   MOVEL     *BLANKS       KTBKEY
053100940119     C                   MOVEL     V1CFIL        OSA
053200940119     C                   MOVE      'OSA'         OSA
053300940119     C                   MOVEL     OSA           KTBKEY
053400940119     C     KEYTAB        CHAIN     TABEL00F                           99
053500940119IF  1C     *IN99         IFEQ      *OFF
053600940119     C*
053700940119     C                   MOVEL     '*'           TBLFLG
053800940119     C                   MOVEL     *BLANKS       TBLFTR
053900940119     C                   Z-ADD     *ZEROS        TBLDTR
054000940119     C*
054100940119     C                   UPDATE    TABEL
054200940119     C*
054300940119     C* RIAGGANCIO IL RECORD 'OSA1' PER LA VARIAZIONE
054400020923     C                   Z-ADD     1             KTBKUT
054500940119     C                   MOVEL     '1X'          KTBCOD
054600940119     C                   MOVEL     *BLANKS       KTBKEY
054700940119     C                   MOVEL     V1CFIL        OSA1
054800940119     C                   MOVE      'OSA1'        OSA1
054900940119     C                   MOVEL     OSA1          KTBKEY
055000940119     C     KEYTAB        CHAIN     TABEL00F                           99
055100940119IF  2C     *IN99         IFEQ      *OFF
055200940119     C*
055300940119     C                   MOVEL     '*'           TBLFLG
055400940119     C                   MOVEL     *BLANKS       TBLFTR
055500940119     C                   Z-ADD     *ZEROS        TBLDTR
055600940119     C*
055700130520     C                   UPDATE    TABEL
055800130520
055900130520     c* Aggiornamento tabella OSt
056000130520     c                   eval      watb='A'
056100130520     c                   exsr      tabOST
056200130520
056300061220     c* Aggiornamento tabella SOT
056400061220     c                   eval      watb='A'
056500061220     c                   exsr      tabsot
056600940119     C*
056700940119     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
056800940119     C                   MOVEL     '1'           $FINE
056900940119     C*
057000940119     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
057100940119     C                   MOVEL     MSG(19)       D50MSG
057200940119     C*
057300940119E   2C                   ENDIF
057400940119E   1C                   ENDIF
057500040624     c                   endif
057600940119     C*
057700930806     C                   ENDSR
057800940111     C*------------------------------------------------------------------------*
057900930806     C* RIPRIS - FUNZIONE DI RIPRISTINO
058000940111     C*------------------------------------------------------------------------*
058100930806     C     RIPRIS        BEGSR
058200931217     C*
058300040624     c                   exsr      sr_atten
058400040624     c*
058500040624     c     $err          ifeq      '0'
058600130520     C* RIAGGANCIO IL RECORD 'OSA' PER LA VARIAZIONE
058700020923     C                   Z-ADD     1             KTBKUT
058800940119     C                   MOVEL     '1X'          KTBCOD
058900940119     C                   MOVEL     *BLANKS       KTBKEY
059000940119     C                   MOVEL     V1CFIL        OSA
059100940119     C                   MOVE      'OSA'         OSA
059200940119     C                   MOVEL     OSA           KTBKEY
059300940119     C     KEYTAB        CHAIN     TABEL00F                           99
059400940119IF  1C     *IN99         IFEQ      *OFF
059500940119     C*
059600940119     C                   MOVEL     *BLANKS       TBLFLG
059700940119     C                   MOVEL     *BLANKS       TBLFTR
059800940119     C                   Z-ADD     *ZEROS        TBLDTR
059900940119     C*
060000940119     C                   UPDATE    TABEL
060100940119     C*
060200940119     C* RIAGGANCIO IL RECORD 'OSA1' PER LA VARIAZIONE
060300020923     C                   Z-ADD     1             KTBKUT
060400940119     C                   MOVEL     '1X'          KTBCOD
060500940119     C                   MOVEL     *BLANKS       KTBKEY
060600940119     C                   MOVEL     V1CFIL        OSA1
060700940119     C                   MOVE      'OSA1'        OSA1
060800940119     C                   MOVEL     OSA1          KTBKEY
060900940119     C     KEYTAB        CHAIN     TABEL00F                           99
061000940119IF  2C     *IN99         IFEQ      *OFF
061100940119     C*
061200940119     C                   MOVEL     *BLANKS       TBLFLG
061300940119     C                   MOVEL     *BLANKS       TBLFTR
061400940119     C                   Z-ADD     *ZEROS        TBLDTR
061500940119     C*
061600940119     C                   UPDATE    TABEL
061700130520     c* Aggiornamento tabella OST
061800130520     c                   eval      watb=' '
061900130520     c                   exsr      tabOST
062000061220     c* Aggiornamento tabella SOT
062100061220     c                   eval      watb=' '
062200061220     c                   exsr      tabsot
062300940119     C*
062400940119     C* IMPOSTO FLAG PER RITORNO AL PGM CHIAMANTE
062500940119     C                   MOVEL     '1'           $FINE
062600940119     C*
062700940119     C* IMPOSTO MESSAGGIO AL PGM CHIAMANTE DI MODIFICA EFFETTUATA
062800940119     C                   MOVEL     MSG(20)       D50MSG
062900940119     C*
063000940119E   2C                   ENDIF
063100940119E   1C                   ENDIF
063200040624     c                   endif
063300940119     C*
063400930806     C                   ENDSR
063500940111     C*------------------------------------------------------------------------*
063600930906     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
063700940111     C*------------------------------------------------------------------------*
063800930906     C     DECODI        BEGSR
063900930205     C*
064000940117     C* IMPOSTO INDICATORI DI ERRORE CAMPI
064100930906     C                   SETOFF                                       606162
064200130520     C                   SETOFF                                       64
064300940117     C*--------------------
064400940119     C* FILIALE (SOLO SE INSERIMENTO)
064500940117     C*--------------------
064600940119IF  1C     D50OP1        IFEQ      'F06'
064700940119IF  2C     V1CFIL        IFEQ      '   '
064800940119     C                   MOVEL     '1'           $ERR
064900940119     C                   MOVEL     MSG(8)        $MSG
065000940119     C                   EXSR      CARMSG
065100940119     C                   SETON                                        60
065200940119     C*
065300940119X   2C                   ELSE
065400940119     C                   MOVEL     V1CFIL        $FIL              3
065500940119     C                   MOVEL     *BLANKS       $DFIL            20
065600940119     C                   EXSR      CHKFIL
065700940119IF  3C     $ERR          IFEQ      '1'
065800940119     C                   EXSR      CARMSG
065900940119     C                   SETON                                        60
066000940119E   3C                   ENDIF
066100940119     C                   MOVEL     $DFIL         V1DFIL
066200940119E   2C                   ENDIF
066300940119E   1C                   ENDIF
066400940119     C*--------------------
066500940119     C* PRIMA ORA SOGLIA
066600940119     C*--------------------
066700940119IF  1C     $ERR          IFEQ      '0'
066800940119     C                   Z-ADD     V1CHH1        $ORA              4 0
066900940119     C                   EXSR      CHKORA
067000940119IF  1C     $ERR          IFEQ      '1'
067100940119     C                   MOVEL     MSG(9)        $MSG
067200940119     C                   EXSR      CARMSG
067300940119     C                   SETON                                        61
067400940119E   2C                   ENDIF
067500940119E   1C                   ENDIF
067600940119     C*--------------------
067700940119     C* SECONDA ORA SOGLIA
067800940119     C*--------------------
067900940119IF  1C     $ERR          IFEQ      '0'
068000940119     C                   Z-ADD     V1CHH2        $ORA
068100940119     C                   EXSR      CHKORA
068200940119IF  1C     $ERR          IFEQ      '1'
068300940119     C                   MOVEL     MSG(9)        $MSG
068400940119     C                   EXSR      CARMSG
068500940901     C                   SETON                                        62
068600940119E   2C                   ENDIF
068700940119E   1C                   ENDIF
068800940901     C* LA SECONDA SOGLIA DEVE ESSERE MAGGIORE O UGUALE ALLA PRIMA SOGLIA
068900940901IF  1C     $ERR          IFEQ      '0'
069000940901IF  2C     V1CHH2        IFLT      V1CHH1
069100940901     C                   MOVEL     MSG(11)       $MSG
069200940901     C                   EXSR      CARMSG
069300940901     C                   SETON                                        62
069400940901E   2C                   ENDIF
069500940901E   1C                   ENDIF
069600061220     c
069700061220     c* Se si tratta di terminal di arrivo, richiedo anche tabella SOT
069800061220     c                   clear                   trul09ds
069900061220     c                   movel     v1cfil        i09tfa
070000061220     c                   movel     'TFA'         i09mod
070100061220     c                   call      'TRUL09R'
070200061220     c                   parm                    trul09ds
070300061220     c
070400150821    1c**                 if        o09err=' '  and $err='0'
070500061220     c* il campo deve essere pieno, altrimenti imposto ora generica
070600150731    2c**                 if        v1csot=0
070700150731     C**                 MOVEL     MSG(12)       $MSG
070800150731     C**                 EXSR      CARMSG
070900150731     C**                 SETON                                        63
071000150731     c**                 movel     '1'           $err
071100150731E   2C**                 ENDIF
071200150731     c**                 endif
071300061220     c
071400061220IF  2C     $ERR          IFEQ      '0'
071500061220     c                   if        v1csot>0
071600061220     C                   Z-ADD     V1Csot        $ORA              4 0
071700061220     C                   EXSR      CHKORA
071800061220IF  3C     $ERR          IFEQ      '1'
071900061220     C                   MOVEL     MSG(9)        $MSG
072000061220     C                   EXSR      CARMSG
072100061220     C                   SETON                                        63
072200061220E   3C                   ENDIF
072300061220E   3C                   ENDIF
072400061220E   2C                   ENDIF
072500040624     c*
072600150821    1c**                 if        (v1cfil=*blanks or o09err=' ')  and $err='0'
072700130520     c* il campo deve essere pieno, altrimenti imposto ora generica
072800150731    2c**                 if        v1cost=0
072900150731     C**                 MOVEL     MSG(21)       $MSG
073000150731     C**                 EXSR      CARMSG
073100150731     C**                 SETON                                        64
073200150731     c**                 movel     '1'           $err
073300150731E   2C**                 ENDIF
073400150821     c**                 endif
073500130520     c
073600130520IF  2C     $ERR          IFEQ      '0'
073700130520     c                   if        v1cost>0
073800130520     C                   Z-ADD     V1Cost        $ORA              4 0
073900130520     C                   EXSR      CHKORA
074000130520IF  3C     $ERR          IFEQ      '1'
074100130520     C                   MOVEL     MSG(9)        $MSG
074200130520     C                   EXSR      CARMSG
074300130520     C                   SETON                                        64
074400130520E   3C                   ENDIF
074500130520E   3C                   ENDIF
074600130520E   2C                   ENDIF
074700040624     c                   exsr      sr_atten
074800930715     C*
074900930715     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
075000930715     C* CURSORE SUL CAMPO DEL CONTROL
075100940117IF  1C     $ERR          IFEQ      '1'
075200930805     C                   MOVEL     '0'           *IN43
075300940117X   1C                   ELSE
075400930805     C                   MOVEL     '1'           *IN43
075500940117E   1C                   ENDIF
075600930205     C*
075700930205     C                   ENDSR
075800040624     C*------------------------------------------------------------------------*
075900040624     C*  SR_atten -  Emette messaggio informativo per CED
076000040624     C*------------------------------------------------------------------------*
076100040624     C     SR_atten      BEGSR
076200040624     C*
076300040624     c* se premuto F6 ed è stata modificata la prima ora di soglia avverto
076400040624     c* di avvisare il CED per poter modificare anche la tabella "SOT"
076500040624     c*
076600040624     c* gestione o inserimento
076700040624     c     $ERR          ifeq      '0'
076800040624     c     key           andeq     F6
076900040624     c     prehh1        andne     v1chh1
077000040624     c     wforz         andeq     *blanks
077100040624     c* ripristino
077200040624     c     $ERR          oreq      '0'
077300040624     c     key           andeq     F6
077400040624     c     d50op1        andeq     'O07'
077500040624     c     wforz         andeq     *blanks
077600040624     c* cancellazione
077700040624     c     $ERR          oreq      '0'
077800040624     c     key           andeq     F6
077900040624     c     d50op1        andeq     'O04'
078000040624     c     wforz         andeq     *blanks
078100040624     c                   eval      wforz='F'
078200040624     c                   seton                                        41
078300040624     c                   eval      $err='1'
078400040624     c                   endif
078500040624     c*
078600040624     c                   endsr
078700940111     C*------------------------------------------------------------------------*
078800940117     C*  CHKFIL - CONTROLLA LA FILIALE IMMESSA
078900940119     C*           INPUT --> $FIL:  FILIALE IMMESSA
079000940119     C*                     $DFIL: DECODIFCA FILIALE VUOTA
079100940119     C*           OUTPUT--> $DFIL: DECODIFIA FILIALE
079200940111     C*------------------------------------------------------------------------*
079300940117     C     CHKFIL        BEGSR
079400940119     C*
079500020923     C                   Z-ADD     1             KTBKUT
079600940119     C                   MOVEL     '1X'          KTBCOD
079700940119     C                   MOVEL     $FIL          OSA
079800940119     C                   MOVE      'OSA'         OSA
079900940119     C                   MOVEL     *BLANKS       KTBKEY
080000940119     C                   MOVEL     OSA           KTBKEY
080100940119     C*
080200940119     C* PER QUELLA FILIALE TEMPO GIA' ESISTENTE, ERRORE
080300940119     C     KEYTAB        CHAIN     TABEL00F                           99
080400940119IF  1C     *IN99         IFEQ      *OFF
080500940119     C                   MOVEL     '1'           $ERR
080600940119     C                   MOVEL     MSG(10)       $MSG
080700940119X   1C                   ELSE
080800940119     C*
080900940119     C* FILIALE CON CARATTERI ALFANUMERICI, ERRORE
081000940119IF  2C     $FIL          IFLT      *ZEROS
081100940119     C                   MOVEL     '1'           $ERR
081200940119     C                   MOVEL     MSG(7)        $MSG
081300940119X   2C                   ELSE
081400940119     C*
081500940119     C* FILIALE INESITENTENTE NELL'ORGANIGRAMMA, ERRORE
081600940119     C                   MOVEL     $FIL          KORFIL
081700940119     C     KEYORG        CHAIN     AZORG01L                           99
081800940119IF  3C     *IN99         IFEQ      *ON
081900940119     C                   MOVEL     '1'           $ERR
082000940119     C                   MOVEL     MSG(7)        $MSG
082100940119X   3C                   ELSE
082200940119     C                   MOVEL     ORGDES        $DFIL            20
082300940119E   3C                   ENDIF
082400940119E   2C                   ENDIF
082500940119E   1C                   ENDIF
082600940119     C*
082700930712     C                   ENDSR
082800940111     C*------------------------------------------------------------------------*
082900940119     C*  CHKORA - CONTROLLA L' ORA DI SOGLIA
083000940119     C*           INPUT --> $ORA:  ORE/MINUTI IMMESSI
083100940111     C*------------------------------------------------------------------------*
083200940119     C     CHKORA        BEGSR
083300930712     C*
083400940119     C                   Z-ADD     *ZEROS        $HH               2 0
083500940119     C                   Z-ADD     *ZEROS        $MM               2 0
083600940119     C*
083700940119     C                   MOVEL     $ORA          $HH
083800940119     C                   MOVE      $ORA          $MM
083900940119     C*
084000940119IF  1C     $HH           IFGT      23
084100940119     C                   MOVEL     '1'           $ERR
084200940119E   1C                   ENDIF
084300940119     C*
084400940119IF  1C     $MM           IFGT      59
084500940119     C                   MOVEL     '1'           $ERR
084600940119E   1C                   ENDIF
084700940119     C*
084800930712     C                   ENDSR
084900940119     C*------------------------------------------------------------------------*
085000940119     C*  RECVID - RIEMPE I CAMPI DAI RECORD TABELLA AL VIDEO
085100940119     C*------------------------------------------------------------------------*
085200940119     C     RECVID        BEGSR
085300940119     C*
085400940119     C* DATI PRIMA ORA SOGLIA PER ARRIVO TRAZIONI ('OSA')
085500940119     C                   MOVEL     D50FIL        V1CFIL
085600061220     C
085700061220     C                   IF        D50ATB<>' '
085800061220     C                   SETON                                        01
085900061220     C                   ENDIF
086000940121     C*
086100940121     C* DECODIFCA FILIALE
086200130520     c                   if        d50fil=*blanks
086300150821     C                   eval      V1DFIL = 'STANDARD/GENERICO'
086400130520     c                   else
086500940121     C                   MOVEL     D50FIL        KORFIL
086600940121     C     KEYORG        CHAIN     AZORG01L                           99
086700940121IF  1C     *IN99         IFEQ      *OFF
086800940121     C                   MOVEL     ORGDES        V1DFIL
086900940121X   1C                   ELSE
087000940121     C                   MOVEL     *ALL'*'       V1DFIL
087100940121E   1C                   ENDIF
087200130520     c                   endif
087300940119     C*
087400940119     C                   MOVEL     TBLUNI        DS1X
087500940119     C                   Z-ADD     §1XHHO        V1CHH1
087600940119     C*
087700940119     C* DATI SECONDA ORA SOGLIA PER ARRIVO TRAZIONI ('OSA1')
087800020923     C                   MOVEL     1             KTBKUT
087900940119     C                   MOVEL     '1X'          KTBCOD
088000940119     C                   MOVEL     *BLANKS       KTBKEY
088100940119     C                   MOVEL     D50FIL        OSA1              7
088200940119     C                   MOVE      'OSA1'        OSA1
088300940119     C                   MOVEL     OSA1          KTBKEY
088400940119     C*
088500940119     C* SE IL RECORD NON ESISTE VALORIZZO A '99:99'
088600940119     C     KEYTAB        CHAIN     TABEL00F                           99
088700940119IF  1C     *IN99         IFEQ      '0'
088800940119     C                   MOVEL     TBLUNI        DS1X
088900940119     C                   Z-ADD     §1XHHO        V1CHH2
089000940119X   1C                   ELSE
089100940119     C                   Z-ADD     9999          V1CHH2
089200940119E   1C                   ENDIF
089300150731
089400130520     c                   movel     osthho        v1cost
089500061220     c* tabella SOT
089600061220     c                   movel     d50sot        v1csot
089700150731     c
089800150731     C                   CLEAR                   DSBS02
089900150731     C                   MOVEL     'C'           T02MOD
090000150731     C                   MOVEL     KNSIF         T02SIF
090100150731     C                   MOVEL     'SOT'         T02COD
090200150821     C                   MOVEL(P)  'GEN'         T02KE1
090300150731     C                   CALL      'TIBS02R'
090400150731     C                   PARM                    KPJBA
090500150731     C                   PARM                    DSBS02
090600150731     c                   movel     t02uni        gensot            4 0
090700150731     c*
090800150731     c                   clear                   ds1x
090900150731     c                   clear                   genost            4 0
091000150731     C                   Z-ADD     1             KTBKUT
091100150731     C                   MOVEL     '1X'          KTBCOD
091200150731     C                   MOVEL     *BLANKS       KTBKEY
091300150731     c                   clear                   OSA
091400150731     C                   MOVE      'OST'         OSA
091500150731     C                   MOVEL     OSA           KTBKEY
091600150731     C     KEYTAB        CHAIN(n)  TABEL00F
091700150731     c                   if        %found(tabel00F)
091800150731     c                   eval      ds1x=tbluni
091900150731     c                   eval      genost=§1xhho
092000150731     c                   endif
092100940119     C*
092200940119     C                   ENDSR
092300940119     C*------------------------------------------------------------------------*
092400940119     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
092500940119     C*------------------------------------------------------------------------*
092600940119     C     AZZVID        BEGSR
092700940119     C*
092800940119     C                   MOVEL     *ZEROS        V1CFIL
092900940119     C                   MOVEL     *BLANKS       V1DFIL
093000940119     C                   Z-ADD     OSAHHO        V1CHH1
093100940119     C                   Z-ADD     OS1HHO        V1CHH2
093200130520     C                   Z-ADD     OSTHHO        V1Cost
093300150731     c* tabella SOT
093400150731     c                   movel     d50sot        v1csot
093500940119     C*
093600940119     C                   ENDSR
093700020923     C*-------------------------------------------------------------------------
093800020923     C* REPERISCE I DATI UTENTE
093900020923     C*-------------------------------------------------------------------------
094000020923     C     REPDATIUTE    BEGSR
094100020923     C*
094200020923     C* INIZIALIZZA VARIABILI DI WRK
094300020923     C                   CLEAR                   TIBS34DS
094400020923     C                   CLEAR                   AZUTEDS
094500020923     C                   CLEAR                   DDATIUTE
094600020923     C*
094700020923     C     *DTAARA       DEFINE    §azute        azuteds
094800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
094900020923     C                   IN(E)     *DTAARA
095000020923if  1C                   IF        %Error
095100020923     c                   EVAL      I34Tla = 'L'
095200020923     C                   CALL      'TIBS34R'
095300020923     C                   PARM                    Tibs34Ds
095400020923     C                   IN        *DTAARA
095500020923e   1C                   ENDIF
095600020923     C*
095700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
095800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
095900020923     C*
096000020923     C                   ENDSR
096100940111     C*------------------------------------------------------------------------*
096200930713     C*  *INZSR - ROUTINE INIZIALE
096300940111     C*------------------------------------------------------------------------*
096400930713     C     *INZSR        BEGSR
096500000000     C*
096600000000     C* RICEVIMENTO PARAMETRI
096700000000     C     *ENTRY        PLIST
096800940114     C                   PARM                    DS50
096900940117     C*
097000940117     C* DEFINIZIONE VARIABILI
097100940117     C     *LIKE         DEFINE    TBLKUT        KTBKUT
097200940117     C     *LIKE         DEFINE    TBLCOD        KTBCOD
097300940117     C     *LIKE         DEFINE    TBLKEY        KTBKEY
097400940117     C     *LIKE         DEFINE    ORGFIL        KORFIL
097500940119     C                   MOVEL     'G'           $TGES             1
097600940117     C*
097700940119     C* CHIAVE LETTURA TABEL00F - COMPLETA
097800940117     C     KEYTAB        KLIST
097900940117     C                   KFLD                    KTBKUT
098000940117     C                   KFLD                    KTBCOD
098100940117     C                   KFLD                    KTBKEY
098200061220     C     Ktbe          KLIST
098300061220     C                   KFLD                    KCOD
098400061220     C                   KFLD                    KKE1
098500061220     C                   KFLD                    KKE2
098600930310     C*
098700930310     C* CHIAVE LETTURA AZORG01L - COMPLETA
098800930310     C     KEYORG        KLIST
098900940117     C                   KFLD                    KORFIL
099000930714     C*
099100930714     C* ESEGUE OPERAZIONI INIZIALI STANDARD
099200930803     C                   EXSR      INZSTD
099300940117     C*
099400940117     C                   MOVEL     DSFIRS        VTDRSU
099500930714     C*
099600940117     C* DECODIFICA OPZIONE RICHIAMATA
099700940117IF  1C     D50OP1        IFEQ      'F06'
099800940117     C                   MOVEL     DOPZ(1)       VTDFUN
099900940117E   1C                   ENDIF
100000940117IF  2C     D50OP1        IFEQ      'O02'
100100940117     C                   MOVEL     DOPZ(2)       VTDFUN
100200940117E   1C                   ENDIF
100300940117IF  1C     D50OP1        IFEQ      'O04'
100400940117     C                   MOVEL     DOPZ(3)       VTDFUN
100500940117E   1C                   ENDIF
100600940121IF  1C     D50OP1        IFEQ      'O05'
100700940121     C                   MOVEL     DOPZ(5)       VTDFUN
100800940121E   1C                   ENDIF
100900940117IF  1C     D50OP1        IFEQ      'O07'
101000940117     C                   MOVEL     DOPZ(4)       VTDFUN
101100940117E   1C                   ENDIF
101200931015     C*
101300931015     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
101400931015     C* 0=NON PROTETTO, 1=PROTETTO
101500940117     C*   90 - CAMPI CHIAVE (FILIALE)
101600940117     C*   91 - ALTRI CAMPI  (SOGLIE)
101700940117IF  1C     D50OP1        IFEQ      'F06'
101800940117     C                   SETOFF                                       9091
101900940117E   1C                   ENDIF
102000940117IF  1C     D50OP1        IFEQ      'O02'
102100940117     C                   SETON                                        90
102200940117E   1C                   ENDIF
102300940117IF  1C     D50OP1        IFEQ      'O04'
102400940121     C     D50OP1        OREQ      'O05'
102500940117     C     D50OP1        OREQ      'O07'
102600940117     C                   SETON                                        9091
102700940117E   1C                   ENDIF
102800940121     C*
102900940121     C* SE VISUALIZZAZIONE TOLGO FUNZIONE F06=AGGIORNAMENTO
103000940121IF  1C     D50OP1        IFEQ      'O05'
103100940121     C                   MOVEL     FUN(6)        FUNRIG
103200940121     C                   MOVEL     'N'           FUNASG
103300940121     C                   MOVEL     'N'           FUNASI
103400940121     C                   MOVEL     'N'           FUNAFG
103500940121     C                   MOVEL     'N'           FUNAFG
103600940121     C                   MOVEL     FUNRIG        FUN(6)
103700940121E   1C                   ENDIF
103800931015     C*
103900940121     C* SE SONO IN MODIFICA/CANCELLAZIONE/VISUALIZZA/RIPRISTINO LEGGO IL RECORD
104000940117IF  1C     D50OP1        IFEQ      'O02'
104100940117     C     D50OP1        OREQ      'O04'
104200940121     C     D50OP1        OREQ      'O05'
104300940117     C     D50OP1        OREQ      'O07'
104400020923     C                   MOVEL     1             KTBKUT
104500940117     C                   MOVEL     '1X'          KTBCOD
104600940117     C                   MOVEL     *BLANKS       KTBKEY
104700940117     C                   MOVEL     D50FIL        OSA               6
104800940117     C                   MOVE      'OSA'         OSA
104900940117     C                   MOVEL     OSA           KTBKEY
105000940117     C*
105100940117     C* SE IL RECORD NON ESISTE O E' ALLOCATO MANDO ERRORE AL PGM CHIAMANTE
105200940117     C     KEYTAB        CHAIN     TABEL00F                           9998
105300940117IF  2C     *IN99         IFEQ      '1'
105400931015     C                   MOVEL     '1'           $ERR
105500931015     C                   MOVEL     '1'           $FINE
105600940117     C                   MOVEL     MSG(14)       D50MSG
105700940117X   2C                   ELSE
105800940117IF  3C     *IN98         IFEQ      '1'
105900940117     C                   MOVEL     '1'           $ERR
106000940117     C                   MOVEL     '1'           $FINE
106100940117     C                   MOVEL     MSG(15)       D50MSG
106200940117X   3C                   ELSE
106300940117     C                   EXSR      RECVID
106400940117     C                   EXSR      CARV1
106500940117E   3C                   ENDIF
106600940117E   2C                   ENDIF
106700940117E   1C                   ENDIF
106800940117     C*
106900940117     C* SE INSERIMENTO
107000940117IF  1C     D50OP1        IFEQ      'F06'
107100940117     C                   EXSR      AZZVID
107200940117     C                   EXSR      CARV1
107300940117E   1C                   ENDIF
107400040624     c
107500040624     c                   setoff                                       41
107600040624     c     d50op1        ifne      'F06'
107700040624     c                   eval      prehh1=v1chh1
107800040624     c                   else
107900040624     c                   clear                   prehh1
108000040624     c                   endif
108100000000     C*
108200000000     C                   ENDSR
108300940111     C*------------------------------------------------------------------------*
108400940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108500940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108600940111     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
108700940111     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
108800940111     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
108900940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
109000940111     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
109100940111     C*------------------------------------------------------------------------*
109200930803     C* INZSTD - ROUTINE INIZIALE STANDARD
109300940111     C*------------------------------------------------------------------------*
109400930803     C     INZSTD        BEGSR
109500930714     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
109600930802     C                   CALL      'XKEYS'
109700930802     C                   PARM                    DSKEYS
109800930805     C                   MOVEA     KEYRIG        KEYS
109900930714     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
110000930714     C     XMSGPA        PLIST
110100930714     C                   PARM                    DSMSMS
110200930714     C                   PARM                    DSMSPG
110300930714     C                   PARM                    DSMSKE
110400930714     C                   PARM                    DSMSER
110500930714     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
110600940119     C                   MOVEL     'TRTB51R'     DSMSPG
110700940119     C                   MOVEL     'TRTB51R'     VMCQUE
110800930806     C                   MOVEL     *BLANKS       VMCKEY
110900930714     C                   MOVEL     *BLANKS       DSMSMS
111000930805     C* INIZIALIZZO FLAG CONTROLLO PGM
111100930721     C                   MOVEL     '0'           $ERR              1
111200930721     C                   MOVEL     '0'           $F03              1
111300930806     C                   MOVEL     *BLANKS       $GES              2
111400930721     C                   MOVEL     *BLANKS       $MSG             80
111500930721     C                   MOVEL     '0'           $FINE             1
111600930721     C                   MOVEL     '0'           $TESVD            1
111700930714     C* LANCIO ROUTINE PER VERIFICARE SE
111800930714     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
111900930714     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
112000020923     C                   EXSR      REPDATIUTE
112100930805     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
112200930721     C                   BITOFF    '01234567'    POSMI1            1
112300930721     C                   BITOFF    '01234567'    POSMI2            1
112400930721     C                   BITOFF    '01234567'    POSMA1            1
112500930721     C                   BITOFF    '01234567'    POSMA2            1
112600930721     C                   BITON     '34'          POSMI1            1
112700930721     C                   BITON     '7'           POSMI2            1
112800930721     C                   BITON     '34'          POSMA1            1
112900930721     C                   BITON     '0'           POSMA2            1
113000930721     C                   MOVE      POSMI2        POSMIN            2
113100930721     C                   MOVEL     POSMI1        POSMIN
113200930721     C                   MOVE      POSMA2        POSMAX            2
113300930721     C                   MOVEL     POSMA1        POSMAX
113400930714     C                   ENDSR
113500940111     C*------------------------------------------------------------------------*
113600930804     C* FUNSTD - LANCIA SUBROUTINE STANDARD
113700940111     C*------------------------------------------------------------------------*
113800930804     C     FUNSTD        BEGSR
113900930804     C                   SELECT
114000930804     C     KEY           WHENEQ    F3
114100930804     C                   EXSR      F03STD
114200930804     C     KEY           WHENEQ    F12
114300930804     C                   EXSR      F12STD
114400930804     C     KEY           WHENEQ    F24
114500930804     C                   EXSR      F24STD
114600930804     C                   OTHER
114700930804     C                   MOVEL     MSG(2)        $MSG
114800930804     C                   EXSR      CARMSG
114900930804     C                   ENDSL
115000930804     C                   ENDSR
115100940111     C*------------------------------------------------------------------------*
115200930804     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
115300940111     C*------------------------------------------------------------------------*
115400930804     C     F03STD        BEGSR
115500940105     C* IMPOSTO FLAG DI PREMUTO TASTO F03
115600940105     C                   MOVEL     '1'           $F03
115700930804     C* IMPOSTO FLAG DI FINE PROGRAMMA
115800930804     C                   MOVEL     '1'           $FINE
115900930804     C                   ENDSR
116000940111     C*------------------------------------------------------------------------*
116100930804     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
116200940111     C*------------------------------------------------------------------------*
116300930804     C     F12STD        BEGSR
116400930805     C* IMPOSTO FLAG DI FIN PGM
116500930804     C                   MOVEL     '1'           $FINE
116600930804     C                   ENDSR
116700940111     C*------------------------------------------------------------------------*
116800930805     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
116900940111     C*------------------------------------------------------------------------*
117000930804     C     F24STD        BEGSR
117100930804     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
117200930804     C                   EXSR      OUTFUN
117300930804     C                   ENDSR
117400940111     C*------------------------------------------------------------------------*
117500930721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
117600930721     C*          CODA MESSAGGI DEL PGM
117700940111     C*------------------------------------------------------------------------*
117800930721     C     CARMSG        BEGSR
117900930723     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
118000930723     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
118100930721     C     MSGULT        IFEQ      0
118200930721     C                   MOVEL     '1'           *IN40
118300940119     C                   WRITE     TB51DVK
118400930721     C                   MOVEL     '0'           *IN40
118500930805     C                   ENDIF
118600930721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
118700930721     C                   MOVEL     $MSG          DSMSMS
118800930721     C                   CALL      'XMSG'        XMSGPA
118900930721     C     DSMSER        IFEQ      0
119000930806     C                   MOVEL     DSMSKE        VMCKEY
119100930805     C                   ENDIF
119200930723     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
119300931015     C                   ADD       1             MSGULT            4 0
119400940119     C                   WRITE     TB51DVM
119500930723     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
119600930721     C                   MOVEL     *BLANKS       $MSG
119700930721     C                   ENDSR
119800940111     C*------------------------------------------------------------------------*
119900930721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
120000940111     C*------------------------------------------------------------------------*
120100930721     C     OUTMSG        BEGSR
120200930721     C* VISUALIZZAZIONE MESSAGGIO
120300930721     C                   MOVEL     '1'           *IN44
120400940119     C                   WRITE     TB51DVK
120500930721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
120600930721     C                   Z-ADD     *ZEROS        MSGULT
120700930721     C                   ENDSR
120800940111     C*------------------------------------------------------------------------*
120900930803     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
121000930803     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
121100930803     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
121200930803     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
121300930803     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
121400940111     C*------------------------------------------------------------------------*
121500930803     C     CARFUN        BEGSR
121600930803     C                   Z-ADD     *ZEROS        FUNULT            5 0
121700931015     C     1             DO        31            I                 5 0
121800930803     C                   MOVEL     FUN(I)        FUNRIG
121900930805     C                   MOVEL     FUNFUN        FFU(I)
122000930805     C                   MOVEL     FUNDES        FDE(I)
122100020923     C     DUTLPO        IFEQ      'S'
122200940119     C     $TGES         IFEQ      'G'
122300940119     C                   MOVEL     FUNASG        FAT(I)
122400940119     C                   ELSE
122500940119     C                   MOVEL     FUNASI        FAT(I)
122600940119     C                   ENDIF
122700940119     C                   ELSE
122800940119     C     $TGES         IFEQ      'G'
122900940119     C                   MOVEL     FUNAFG        FAT(I)
123000940119     C                   ELSE
123100940119     C                   MOVEL     FUNAFI        FAT(I)
123200940119     C                   ENDIF
123300940119     C                   ENDIF
123400930806     C     FAT(I)        IFEQ      'E'
123500930806     C                   MOVEL     USER          WEDP              3
123600930806     C     WEDP          IFEQ      'EDP'
123700930806     C                   MOVEL     'S'           FAT(I)
123800930806     C                   ELSE
123900930806     C                   MOVEL     'N'           FAT(I)
124000930806     C                   ENDIF
124100930806     C                   ENDIF
124200930803     C                   ENDDO
124300930803     C                   ENDSR
124400940111     C*------------------------------------------------------------------------*
124500930803     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
124600930803     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
124700930806     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
124800930803     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
124900940111     C*------------------------------------------------------------------------*
125000930803     C     OUTFUN        BEGSR
125100930805     C     1             DO        8             I
125200930805     C                   MOVEL     *BLANKS       FUV(I)
125300930803     C                   ENDDO
125400930803     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
125500930803     C                   Z-ADD     *ZEROS        FUNNAT            2 0
125600930803     C     1             DO        31            I
125700930803     C     FAT(I)        IFEQ      'S'
125800930803     C     FDE(I)        ANDNE     *BLANKS
125900930803     C                   ADD       1             FUNNAT
126000930803     C                   ENDIF
126100930803     C                   ENDDO
126200930805     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
126300930803     C     FUNNAT        IFGT      8
126400930803     C                   MOVEL     'S'           FAT(24)
126500930803     C                   ENDIF
126600930803     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
126700930805     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
126800930803     C                   Z-ADD     8             FUNVIS            5 0
126900930803     C     FAT(23)       IFEQ      'S'
127000930803     C                   SUB       1             FUNVIS
127100930803     C                   ENDIF
127200930803     C     FAT(24)       IFEQ      'S'
127300930803     C                   SUB       1             FUNVIS
127400930803     C                   ENDIF
127500930805     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
127600931015     C                   Z-ADD     *ZEROS        W                 5 0
127700930803     C                   ADD       1             FUNULT
127800930803     C     FUNULT        IFGT      31
127900930803     C                   Z-ADD     1             FUNULT
128000930805     C                   ENDIF
128100930805     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
128200930803     C     FUNULT        DO        31            I
128300930803     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
128400930803     C     W             IFLT      FUNVIS
128500930803     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
128600930803     C     FAT(I)        IFEQ      'S'
128700930803     C     FDE(I)        ANDNE     *BLANKS
128800930805     C     I             IFEQ      23
128900930805     C     I             OREQ      24
129000930803     C                   ELSE
129100930803     C                   ADD       1             W
129200930803     C                   MOVEL     FDE(I)        FUV(W)
129300930803     C                   ENDIF
129400930803     C                   ENDIF
129500930803     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
129600930803     C                   Z-ADD     I             FUNULT
129700930803     C                   ENDIF
129800930803     C                   ENDDO
129900930803     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
130000930806     C                   MOVEL     FUV(1)        VFD001
130100930806     C                   MOVEL     FUV(2)        VFD002
130200930806     C                   MOVEL     FUV(3)        VFD003
130300930806     C                   MOVEL     FUV(4)        VFD004
130400930806     C                   MOVEL     FUV(5)        VFD005
130500930806     C                   MOVEL     FUV(6)        VFD006
130600930803     C     FAT(23)       IFEQ      'N'
130700930803     C     FAT(24)       ANDEQ     'N'
130800930806     C                   MOVEL     FUV(7)        VFD007
130900930806     C                   MOVEL     FUV(8)        VFD008
131000930803     C                   ELSE
131100930803     C     FAT(23)       IFEQ      'S'
131200930803     C     FAT(24)       ANDEQ     'S'
131300930806     C                   MOVEL     FDE(23)       VFD007
131400930806     C                   MOVEL     FDE(24)       VFD008
131500930803     C                   ELSE
131600930806     C                   MOVEL     FUV(7)        VFD007
131700930803     C     FAT(23)       IFEQ      'S'
131800930806     C                   MOVEL     FDE(23)       VFD008
131900930803     C                   ELSE
132000930806     C                   MOVEL     FDE(24)       VFD008
132100930805     C                   ENDIF
132200930805     C                   ENDIF
132300930805     C                   ENDIF
132400930803     C                   MOVEL     '1'           $TESVD
132500930803     C                   ENDSR
132600940111     C*------------------------------------------------------------------------*
132700930803     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
132800930803     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
132900930803     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
133000930803     C*                   - ERRORE SE FUNZIONE NON ATTIVA
133100940111     C*------------------------------------------------------------------------*
133200930803     C     CONFUN        BEGSR
133300930803     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
133400930803     C     1             DO        31            I
133500930803     C     KEY           IFEQ      KEYS(I)
133600940105     C* CONTROLLO SE IL TASTO E' ATTIVO
133700930803     C     FAT(I)        IFNE      'S'
133800930804     C                   MOVEL     MSG(1)        $MSG
133900930803     C                   EXSR      CARMSG
134000930803     C                   MOVEL     '1'           $ERR
134100930803     C                   ENDIF
134200930803     C                   ENDIF
134300930803     C                   ENDDO
134400930803     C                   ENDSR
134500940121** ======== SCHIERA: MSG  (MESSAGGI)                       ================
134600940119TRTB51R- Tasto funzionale non valido.                                          1
134700940119TRTB51R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
134800940119TRTB51R- Opzione non valida.                                                   3
134900940119TRTB51R- Opzione abilitata ma non gestita. Avvertire CED.                      4
135000940119TRTB51R- Inizio elenco.                                                        5
135100940119TRTB51R- Fine elenco.                                                          6
135200940119TRTB51R- Filiale errata o inesistente                                          7
135300940119TRTB51R- Impossibile inserire il tempo della filiale '   '                     8
135400940119TRTB51R- Ore/Minuti errati                                                     9
135500940119TRTB51R- Tempo della filiale già esistente: impossibile l'inserimento         10
135600940901TRTB51R- La seconda soglia deve essere maggiore o uguale alla prima           11
135700061220TRTB51R- Ora soglia per calcolo data teorica arrivo trazioni                  12
135800061220TRTB51R-Ora soglia tabella "SOT"  necessaria solo x Term.arrivo
135900940119TRTB51R- Richiesta funzione per Tempo Delivery inesistente                    14
136000940119TRTB51R- Tempo Delivery momentaneamente allocato: riprovare più tardi         15
136100940119TRTB51R- 16
136200940119TRTB51R- Inserimento effettuato.                                              17
136300940119TRTB51R- Modifica effettuata.                                                 18
136400940119TRTB51R- Cancellazione effettuata.                                            19
136500940119TRTB51R- Ripristino effettuato.                                               20
136600130520TRTB51R- Ora soglia per Elenco Trazioni in Arrivo obbligatoria                21
136700930806** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
136800930806F1    -                   -    NN     NN
136900930723F2    -                   -    NN     NN
137000930802F3    -F3=Fine            -    SS     SS
137100930723F4    -                   -    NN     NN
137200930723F5    -                   -    NN     NN
137300940119F6    -F6=Aggiornamento   -    SS     SS
137400930723F7    -                   -    NN     NN
137500930723F8    -                   -    NN     NN
137600940119F9    -                   -    NN     NN
137700930723F10   -                   -    NN     NN
137800930723F11   -                   -    NN     NN
137900930802F12   -F12=Ritorno        -    SS     SS
138000930723F13   -                   -    NN     NN
138100930723F14   -                   -    NN     NN
138200930723F15   -                   -    NN     NN
138300930723F16   -                   -    NN     NN
138400930723F17   -                   -    NN     NN
138500930723F18   -                   -    NN     NN
138600930723F19   -                   -    NN     NN
138700930723F20   -                   -    NN     NN
138800940119F21   -                   -    NN     NN
138900930804F22   -                   -    NN     NN
139000930723F23   -                   -    NN     NN
139100930803F24   -F24=Altri tasti    -    NN     NN
139200930802ENTER -                   -    SS     SS
139300930802ROLLUP-                   -    NN     NN
139400930802ROLLDN-                   -    NN     NN
139500930802HELP  -                   -    NN     NN
139600930802PRINT -                   -    NN     NN
139700930802BSPACE-                   -    NN     NN
139800930802CLEAR -                   -    NN     NN
139900940117** DOPZ  (OPZIONE RICHIAMATA)
140000940119* INSERIMENTO *                                                 1
140100940119*  VARIAZIONE  *                                                2
140200940119*CANCELLAZIONE*                                                 3
140300940119*  RIPRISTINO  *                                                4
140400940121VISUALIZZAZIONE                                                 5
