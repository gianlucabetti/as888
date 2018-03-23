000100970526     H*------------------------------------------------------------------------*
000200991001     H* Gestione - codici clienti unificanti -
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800020905     Ftibs12D   CF   E             WORKSTN INFDS(DEVDS1)
000900991011     F                                     SFILE(BS12D05:SFLULT)
001000991011     F                                     SFILE(BS12DMS:MSGULT)
001100970526     F*------------
001200160701     Ftikun00f  IF   E             DISK    rename(tikun000:tikunF)
001300160704     Ftikun11l  IF   E           K DISK
001400160704     F                                       infds(kun11)
001500160704     F*******tikun01l  IF   E           K DISK
001600160704     F*******                                       infds(kun01)
001700000731     Ftikun02l  IF   E           K DISK    rename(tikun000:tikun2)
001800160701     F                                       infds(kun02)
001900000731     Ftikun04l  IF   E           K DISK    rename(tikun000:tikun4)
002000160701     F                                       infds(kun04)
002100000731     Ftikun05l  IF   E           K DISK    rename(tikun000:tikun5)
002200160701     F                                       infds(kun05)
002300020927     Ftivss05l  IF   E           K DISK    usropn
002400000307     Ftntbe01l  IF   E           K DISK
002500970526     D*------------------------------------------------------------------------*
002600970526     D* SCHIERE
002700970526     D*------------------------------------------------------------------------*
002800160701      *
002900160704     d Kun11           ds
003000160704     d  K11_nrr              397    400b 0
003100160704      *
003200160704     d Kun01           ds
003300160704     d  K01_nrr              397    400b 0
003400160701      *
003500160701     d Kun02           ds
003600160701     d  K02_nrr              397    400b 0
003700160701      *
003800160701     d Kun04           ds
003900160701     d  K04_nrr              397    400b 0
004000160701      *
004100160701     d Kun05           ds
004200160701     d  K05_nrr              397    400b 0
004300160701      *
004400970526     D*------------
004500970526     D* DECODIFICHE
004600970526     D*------------
004700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
004800970526     D*------------
004900970526     D* MESSAGGI
005000970526     D*------------
005100970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
005200970528     D*------------
005300970528     D* COMANDI
005400970528     D*------------
005500970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
005600970526     D*------------
005700970526     D* DESCRIZIONI OPZIONI
005800970526     D*------------
005900970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
006000970526     D*------------
006100970526     D* DESCRIZIONI TASTI FUNZIONALI
006200970526     D*------------
006300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
006400970526     D*------------
006500970526     D* OPZIONI DA CARICARE A VIDEO
006600970526     D*------------
006700970526     D OPV             S             19    DIM(24)
006800970526     D*------------
006900970526     D* FUNZIONI DA CARICARE A VIDEO
007000970526     D*------------
007100970527     D FUV             S             19    DIM(24)
007200020709     D*------------
007300060113     D* SCHIERA P.O. IN GESTIONE ALL'UTENTE
007400020709     D*------------
007500060113     D skPOG           S              3    DIM(250) INZ(*zeros)
007600970526     D*------------------------------------------------------------------------*
007700970526     D* INPUT
007800970526     D*------------------------------------------------------------------------*
007900970526     D*------------
008000970526     D* RIDEFINIZIONE OPZIONI VIDEO
008100970526     D*------------
008200970526     D OPDDS           DS
008300970526     D  OPDOPZ                 1      2
008400970526     D  OPDDE1                 3     19
008500970526     D  OPDOP1                21     22
008600970526     D  OPDFI0                23     34
008700970526     D  OPDATT                35     35
008800970526     D  OPDFI1                36     80
008900970526     D  OPDDES                 1     19
009000970526     D  OPDRIG                 1     80
009100970526     D*------------
009200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
009300970526     D*------------
009400970526     D FUDDS           DS
009500970526     D  FUDDE1                 1      1
009600970526     D  FUDFUN                 2      3
009700970526     D  FUDDE2                 4     19
009800970526     D  FUDFI0                20     34
009900970526     D  FUDATT                35     35
010000970526     D  FUDFI1                36     80
010100970526     D  FUDDES                 1     19
010200970526     D  FUDRIG                 1     80
010300970526     D*------------
010400970526     D* DS GESTIONE TASTI FUNZIONALI
010500970526     D*------------
010600970526     D DSKEY         E DS
010700970526     D DEVDS1          DS
010800970526     D  KEY                  369    369
010900970526     D*------------
011000970526     D* DS "XMSG" - GESTIONE MESSAGGI
011100970526     D*------------
011200970526     D DSMSG         E DS
011300020212     D*------------
011400020212     D* DS X LETTURA NUOVA TABELLA ISV
011500020212     D*------------
011600020212     D DSISV         E DS                  EXTNAME(DISV)
011700100322     D*------------
011800100322     D* DS "XSRDA8" - CONTROLLA DATA (8)
011900100322     D*-------------
012000100322     D WLBDA8          DS                  INZ
012100100322     D  G08DAT                 1      8  0
012200100322     D  G08INV                 9     16  0
012300100322     D  G08ERR                17     17
012400100322     D  G08TGI                18     22  0
012500970526     D*------------
012600970526     D* DS DI PROCEDURA
012700970526     D*------------
012800991001     D tibds         E DS                  EXTNAME(TIbs12ds)
012900000607     D tis720        E DS                  EXTNAME(TIs720ds)
013000020212     D tibs02ds      E DS
013100991015     D dstle         E DS                  extname(dtle)
013200970526     D*------------
013300970526     D* ARCHITETTURA
013400970526     D*------------
013500970526     D KPJBA         E DS
013600000807     D*------------
013700000807     D* TABELLE "XCLIR"- RICERCA CLIENTI
013800000807     D*------------
013900000807     D ksc             s              7  0 dim(30)
014000000807     D ksa             s              4    dim(30)
014100020212     D isvkun          s              2    dim(100)
014200000807     D*------------------
014300000807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
014400000807     D*------------------
014500020212     D DSXC          e DS                  extname(XCLIRDS)
014600020212     D*------------------
014700000807     D                 DS
014800000807     D dsksn                   1      4p 0
014900000807     D dsksa                   1      4
015000000807     D*------------------
015100000807     D* DS REPERIMENTO DATI UTENTE
015200000807     D*-------------------
015300020212     D BS69DS        e ds                  extname(TIBS69DS)
015400020212     D ACODS         e ds                  extname(cnaco00f)
015500020212     D INDDS         e ds                  extname(cnind00f)
015600020212     D CLPDS         e ds                  extname(cnclp00f)
015700020212     D CLSDS         e ds                  extname(fncls00f)
015800000307     D*------------
015900000307     D* SCOMPOSIZIONE CODICI CLIENTI IN FORMATO "STRATEGI"
016000000307     D*------------
016100000307     D                 DS
016200020905     D  FILCLI                 5      7  0
016300000307     D  STRATECLI              1     11  0
016400000807     D*------------------
016500000807     D* VARIABILI DI WRK
016600000807     D*-------------------
016700000807     D tipprf          s              3    inz(*blanks)
016800020905     D*------------
016900020905     D* DS REPERIMENTO DATI UTENTE
017000020905     D*------------
017100020905     D TIBS34DS      E DS
017200020905     D DDATIUTE      E DS
017300020905     D AZUTEDS       E DS                  extname(AZUTE00F)
017400060113     d dute01        e ds
017500060113     d trul31ds      e ds
017600160704      *----------------------------------------------------*
017700160704      **********************************************************************
017800160704      * Constants
017900160704      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
018000160704      **********************************************************************
018100160704      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
018200160704     D MaxKey          C                   5
018300160704     D Ascendente      C                   1
018400160704     D Discendente     C                   2
018500160704     D Carattere       C                   6
018600160704     D Put             C                   1
018700160704     D EndPut          C                   2
018800160704     D Get             C                   3
018900160704     D Numerico        C                   8
019000160704      **********************************************************************
019100160704      * Campi utili:
019200160704      *     RRN1       - Numero relativo di record del Subfile
019300160704      *     SizeList   - Dimensione della lista
019400160704      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
019500160704      **********************************************************************
019600160704     D Rrn1            S              5I 0
019700160704     D NotUsed         S             16A
019800160704     D ReturnSize      S              9B 0
019900160704     D SizeList        S              9B 0
020000160704     D RrnLast         S              5I 0
020100160704     D WrkSort         S              1  0 inz(0)
020200160704     D WindSort        S              3I 0 inz(0)
020300160704      **********************************************************************
020400160704      * Data Structures
020500160704      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
020600160704      *     QLGSCB     - The sort request block for the QLGSORT API
020700160704      *     QLGSCB00   - The sort request block for the QLGSRTIO API
020800160704      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
020900160704      *     QUSEC      - Error structure for the QLGSORT API
021000160704      **********************************************************************
021100160704     D SflRcd          DS
021200160704     D  VIDKOP
021300160704     D  VIDCOF
021400160704     D  VIDTLE
021500160704     D  VIHDDE
021600160704     D  VIHDSC
021700160704     D  VIDDDE
021800160704     d  VIDDSC
021900160704     D  VIDDEP
022000160704     D  VIDVAT
022100160704     D  VIDDES
022200160704     D  OPZKEY
022300160704     D  NREC1
022400160704     D  VIDAZF
022500160704     D  VIDAZP
022600160704     D  HIN75
022700160704     D  HIN76
022800160704     D  Selected                      1A
022900160704
023000160704      /COPY QSYSINC/QRPGLESRC,QLGSORT
023100160704     D QLGKL                         16    DIM(MaxKey)
023200160704     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
023300160704     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
023400160704     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
023500160704     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
023600160704
023700160704      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
023800160704      /COPY QSYSINC/QRPGLESRC,QUSEC
023900160704      *----------------------------------------------------*
024000970526     C*------------------------------------------------------------------------*
024100970526     C* MAIN LINES
024200970526     C*------------------------------------------------------------------------*
024300970526     C*
024400970526     C* SE NON CI SONO ERRORI, PROSEGUE
024500970526IF  1C     ERRL00        IFNE      '1'
024600970526     C*
024700970526     C* CARICA IL SFILE
024800970526     C                   EXSR      POSSFL
024900970526     C*
025000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
025100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
025200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
025300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
025400970526     C*
025500970526     C* REIMPOSTA ERRORE DI PROCERURA
025600970526     C                   MOVEL     '0'           ERRL00
025700970526     C*
025800970526     C* EMETTE IL SFILE
025900970526     C                   EXSR      OUTSFL
026000970526     C*
026100970526     C* GESTIONE TASTI FUNZIONALI
026200970526     C                   EXSR      FUNCON
026300970526     C*
026400970526E   2C                   ENDDO
026500970527     C*
026600970527     C* CI SONO ERRORI IN ENTRATA
026700970527X   1C                   ELSE
026800991001     C                   MOVEL     '1'           k12ERR
026900991001     C                   MOVEL     DSMSMS        k12MSG
027000970526E   1C                   ENDIF
027100970526     C* FINE PGM
027200991210     c                   exsr      finsr
027300970526     C                   SETON                                        LR
027400970526     C*------------------------------------------------------------------------*
027500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
027600970526     C*------------------------------------------------------------------------*
027700970526     C     F03GES        BEGSR
027800970526     C*
027900970526     C* IMPOSTA "PREMUTO F03"
028000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
028100991001     C                   MOVEL     '1'           k12F03                         *DI PROCEDURA
028200970526     C*
028300970526     C                   ENDSR
028400970526     C*------------------------------------------------------------------------*
028500970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
028600970526     C*------------------------------------------------------------------------*
028700970526     C     F05GES        BEGSR
028800970526     C*
028900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
029000970526     C*
029100970526     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
029400970526     C*------------------------------------------------------------------------*
029500970526     C     F06GES        BEGSR
029600970526     C*
029700970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
029800970526     C*
029900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030000991001     C                   MOVEL     *BLANKS       Dtle
030100991001     C                   Z-ADD     *ZEROS        Dcop
030200991001     C                   Z-ADD     *ZEROS        Dcof
030300991001     C                   Z-ADD     *ZEROS        Ddde
030400970602     C                   MOVEL     DEPOP0        WOP0
030500970529     C                   MOVEL     'F06'         WOP1
030600991001     C                   MOVEL     'TIBS13R'     WPGM              8
030700970526     C                   EXSR      LANPGM                                       *LANCIA PGM
030800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
030900970526E   1C                   ENDIF
031000970526     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300970526     C* F12GES - GESTIONE F12 -> RITORNO
031400970526     C*------------------------------------------------------------------------*
031500970526     C     F12GES        BEGSR
031600970526     C*
031700970526     C* IMPOSTA "PREMUTO F12"
031800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
031900991001     C                   MOVEL     '1'           k12F12                         *DI PROCEDURA
032000970526     C*
032100970526     C                   ENDSR
032200970528     C*------------------------------------------------------------------------*
032300970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
032400970528     C*------------------------------------------------------------------------*
032500970528     C     F21GES        BEGSR
032600970612     C*
032700991001     C                   MOVEL     *BLANKS       Dtle
032800991001     C                   Z-ADD     *ZEROS        Dcop
032900991001     C                   Z-ADD     *ZEROS        Dcof
033000991001     C                   Z-ADD     *ZEROS        Ddde
033100970612     C                   MOVEL     DEPOP0        WOP0
033200970612     C                   MOVE      '03'          WOP0
033300970612     C                   MOVEL     'F21'         WOP1
033400991011     C                   MOVEL     'TIBS14R'     WPGM              8
033500000808     C*                  EXSR      LANPGM
033600970528     C*
033700970528     C                   ENDSR
033800970528     C*------------------------------------------------------------------------*
033900970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
034000970528     C*------------------------------------------------------------------------*
034100970528     C     F22GES        BEGSR
034200970528     C*
034300970528     C                   Z-ADD     80            LENGH            15 5
034400970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
034500970528     C                   PARM                    CMD(1)
034600970528     C                   PARM                    LENGH
034700970528     C*
034800970528     C                   ENDSR
034900970526     C*------------------------------------------------------------------------*
035000970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
035100970526     C*------------------------------------------------------------------------*
035200970526     C     F23GES        BEGSR
035300970526     C*
035400970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
035500970526     C*
035600970526     C                   ENDSR
035700970526     C*------------------------------------------------------------------------*
035800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
035900970526     C*------------------------------------------------------------------------*
036000970526     C     F24GES        BEGSR
036100970526     C*
036200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
036300970526     C*
036400970526     C                   ENDSR
036500970526     C*------------------------------------------------------------------------*
036600970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
036700970526     C*------------------------------------------------------------------------*
036800970526     C     RUPGES        BEGSR
036900970526     C*
037000970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
037100970526     C*
037200970526     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400970526     C* ENTGES - GESTIONE ENTER -> INVIO
037500970526     C*------------------------------------------------------------------------*
037600970526     C     ENTGES        BEGSR
037700970526     C*
037800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
037900970526     C                   EXSR      NEWPOS
038000970526     C*
038100970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
038200020212     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038300020212     C     SWPOSI        IFNE      0
038400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
038500020212     C                   ELSE
038600020212     C     SFLULT        IFNE      0
038700970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
038800970526     C*
038900970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
039000020212     C     SWMODI        IFNE      0
039100970526     C                   EXSR      POSSFL
039200020212     C                   ENDIF
039300020212     C                   ENDIF
039400020212     C                   ENDIF
039500020212     C                   ENDIF
039600970526     C*
039700970526     C                   ENDSR
039800970526     C*------------------------------------------------------------------------*
039900970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
040000970526     C*------------------------------------------------------------------------*
040100970526     C     O01GES        BEGSR
040200970526     C*
040300970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
040400991001     C                   MOVEL     vidtle        k12tle
040500991001     C                   move      vidazp        k12azp
040600991001     C                   Z-ADD     vidkop        k12cop
040700991001     C                   move      vidazf        k12azf
040800991001     C                   Z-ADD     vidcof        k12cof
040900991011     C                   Z-ADD     vihdde        k12dde
041000991011     C                   Z-ADD     vihdsc        k12dsc
041100150202     C*
041200150202     C                   movel     tibds         kpjbu
041300150202     C*
041400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
041500970526     C*
041600970526     C                   ENDSR
041700970526     C*------------------------------------------------------------------------*
041800970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
041900970526     C*------------------------------------------------------------------------*
042000970526     C     O02GES        BEGSR
042100970526     C*
042200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
042300020212     C     VIDvat        IFEQ      'A'
042400970526     C                   MOVEL     '1'           ERRL00
042500970526     C                   MOVEL     MSG(4)        DSMSMS
042600020212     C                   ENDIF
042700970527     C* MODIFICA
042800020212     C     ERRL00        IFEQ      '0'
042900000807     C*
043000000807     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (FILIALE/TIPO LEGAME/CLIENTI)
043100000807     C                   EXSR      CHKAUT
043200020212     C     ERRL00        IFEQ      '0'
043300020212     C                   EXSR      MOVCAMP
043400970602     C                   MOVEL     DEPOP0        WOP0
043500970526     C                   MOVEL     'O02'         WOP1
043600991011     C                   MOVEL     'TIBS13R'     WPGM
043700970526     C                   EXSR      LANPGM
043800020212     C                   ENDIF
043900020212     C                   ENDIF
044000970526     C*
044100970526     C                   ENDSR
044200991014     C*------------------------------------------------------------------------*
044300991014     C* O03GES - GESTIONE OPZIONE 3=copia
044400991014     C*------------------------------------------------------------------------*
044500991014     C     O03GES        BEGSR
044600991014     C*
044700991014     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
044800020212     C     VIDvat        IFEQ      'A'
044900991014     C                   MOVEL     '1'           ERRL00
045000991014     C                   MOVEL     MSG(4)        DSMSMS
045100020212     C                   ENDIF
045200000307     C* COPIA
045300020212     C     ERRL00        IFEQ      '0'
045400000807     C*
045500000807     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (FILIALE/TIPO LEGAME/CLIENTI)
045600000807     C                   EXSR      CHKAUT
045700020212     C     ERRL00        IFEQ      '0'
045800020212     C                   EXSR      MOVCAMP
045900991014     C                   MOVEL     DEPOP0        WOP0
046000991014     C                   MOVEL     'O03'         WOP1
046100991014     C                   MOVEL     'TIBS13R'     WPGM
046200991014     C                   EXSR      LANPGM
046300020212     C                   ENDIF
046400020212     C                   ENDIF
046500991014     C*
046600991014     C                   ENDSR
046700970526     C*------------------------------------------------------------------------*
046800970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
046900970526     C*------------------------------------------------------------------------*
047000000307     C     O04GES        BEGSR
047100970526     C*
047200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
047300020212     C     VIDvat        IFEQ      'A'
047400000307     C                   MOVEL     '1'           ERRL00
047500000307     C                   MOVEL     MSG(4)        DSMSMS
047600020212     C                   ENDIF
047700970527     C* ANNULLAMENTO
047800020212     C     ERRL00        IFEQ      '0'
047900000807     C*
048000000807     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (FILIALE/TIPO LEGAME/CLIENTI)
048100000807     C                   EXSR      CHKAUT
048200020212     C     ERRL00        IFEQ      '0'
048300020212     C                   EXSR      MOVCAMP
048400000307     C                   MOVEL     DEPOP0        WOP0
048500000307     C                   MOVEL     'O04'         WOP1
048600000307     C                   MOVEL     'TIBS13R'     WPGM
048700000307     C                   EXSR      LANPGM
048800020212     C                   ENDIF
048900020212     C                   ENDIF
049000970526     C*
049100000307     C                   ENDSR
049200970526     C*------------------------------------------------------------------------*
049300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
049400970526     C*------------------------------------------------------------------------*
049500970526     C     O05GES        BEGSR
049600970526     C*
049700020212     C                   EXSR      MOVCAMP
049800970602     C                   MOVEL     DEPOP0        WOP0
049900970526     C                   MOVEL     'O05'         WOP1
050000991011     C                   MOVEL     'TIBS13R'     WPGM
050100970526     C                   EXSR      LANPGM
050200970526     C*
050300970526     C                   ENDSR
050400970526     C*------------------------------------------------------------------------*
050500970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
050600970526     C*------------------------------------------------------------------------*
050700970526     C     O07GES        BEGSR
050800970526     C*
050900970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
051000020212     C     VIDvat        IFNE      'A'
051100970526     C                   MOVEL     '1'           ERRL00
051200970526     C                   MOVEL     MSG(5)        DSMSMS
051300020212     C                   ENDIF
051400970526     C* RIPRISITINO
051500020212     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
051600000807     C*
051700000807     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (FILIALE/TIPO LEGAME/CLIENTI)
051800000807     C                   EXSR      CHKAUT
051900020212     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
052000020212     C                   EXSR      MOVCAMP
052100970602     C                   MOVEL     DEPOP0        WOP0
052200970526     C                   MOVEL     'O07'         WOP1
052300991011     C                   MOVEL     'TIBS13R'     WPGM
052400970526     C                   EXSR      LANPGM
052500020212     C                   ENDIF
052600020212     C                   ENDIF
052700970526     C*
052800970526     C                   ENDSR
052900000607     C*------------------------------------------------------------------------*
053000000607     C* O09GES - GESTIONE OPZIONE 9=Richiamo pgm cliente padre abilitato
053100000607     C*------------------------------------------------------------------------*
053200000607     C     O09GES        BEGSR
053300000607     C*
053400020212     C* CLIENTE ABILITATO
053500020212     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
053600000607     C                   MOVEL     VIDtle        Dtle
053700020212     C                   Z-ADD     VIDkop        Dcop
053800000607     C                   MOVEL     DEPOP0        WOP0
053900000607     C                   MOVEL     'O09'         WOP1
054000000607     C                   MOVEL     'TIS724R'     WPGM
054100000607     C                   EXSR      LANPGM2
054200020212     C                   ENDIF
054300000607     C*
054400000607     C                   ENDSR
054500000307     C*------------------------------------------------------------------------*
054600000307     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
054700000307     C*------------------------------------------------------------------------*
054800000307     C     CHKAUT        BEGSR
054900000807     C*
055000020709     C* CONTROLLO SE UTENTE PUO' GESTIRE IL TIPO LEGAME SCELTO
055100000807     C                   eval      tipprf =  %subst(%trim(KNMUS):1:3)
055200020709     C     DUTLPO        ifeq      'S'
055300000807     C* X SEDE
055400000807     C     ERRL00        ifeq      '0'
055500000807     C                   movel     'TLE'         KTBcod
055600000807     C                   movel(P)  vidtle        KTBky1
055700000807     C     KEYTBE        chain     tntbe01l                           70
055800000807     C     *in70         ifeq      *off
055900000807     C     tbeatb        andeq     *blanks
056000000807     C                   movel     tbeuni        dstle
056100000807     C                   if        §tleaus = 'S' or
056200000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
056300000807     C                   else
056400000807     C                   MOVEL     '1'           ERRL00
056500000807     C                   MOVEL     MSG(13)       DSMSMS
056600000807     C                   endif
056700000807     C                   else
056800000807     C                   MOVEL     '1'           ERRL00
056900000807     C                   MOVEL     MSG(11)       DSMSMS
057000000807     C                   endif
057100000807     C                   endif
057200020709     C* X FILIALE
057300000807     C                   else
057400000807     C     ERRL00        ifeq      '0'
057500000807     C                   movel     'TLE'         KTBcod
057600000807     C                   movel(P)  vidtle        KTBky1
057700000807     C     KEYTBE        chain     tntbe01l                           70
057800000807     C     *in70         ifeq      *off
057900000807     C     tbeatb        andeq     *blanks
058000000807     C                   movel     tbeuni        dstle
058100000807     C                   if        §tleaut = 'S' or
058200000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
058300000807     C                   else
058400000807     C                   MOVEL     '1'           ERRL00
058500000807     C                   MOVEL     MSG(13)       DSMSMS
058600000807     C                   endif
058700000807     C                   else
058800000807     C                   MOVEL     '1'           ERRL00
058900000807     C                   MOVEL     MSG(11)       DSMSMS
059000000807     C                   endif
059100000807     C                   endif
059200020709     C                   endif
059300000807     C*
059400020709     C* CONTROLLO SE UTENTE PUO' GESTIRE IL CLIENTE PADRE SCELTO
059500001018     C     ERRL00        ifeq      '0'
059600160707     C     tipprf        andne     'EDP'
059700160707     C     dutntw        andne     'LOG'
059800160707      *
059900020709     C     vidkop        ifne      *zeros
060000020709     C                   move(p)   vidkop        STRATECLI
060100060113     c                   move      filcli        w003a             3
060200060113     c     w003a         lookup    skpog                                  55
060300020709     C                   if        not *in55
060400020709     C                   movel     '1'           ERRL00
060500020709     C                   movel     MSG(14)       DSMSMS
060600020709     C                   endif
060700020709     C                   endif
060800020709     C                   endif
060900000307     C*
061000170905     C*  CHIODO per la LOGISTICA che può usarla solo POC212
061100170905     C                   if        ERRL00 = '0' and vidTLE ='LT' and
061200170906     C                               knmus <> 'POC212    ' and
061300170906     C                              tipprf <> 'EDP'
061400170905     C                   movel     '1'           ERRL00
061500170905     C                   movel     MSG(15)       DSMSMS
061600170905     C                   endif
061700170905     C*
061800000307     C                   ENDSR
061900000307     C*------------------------------------------------------------------------*
062000000307     C* MOVCAMP - VALORIZZAZIONE CAMPI
062100000307     C*------------------------------------------------------------------------*
062200000307     C     MOVCAMP       BEGSR
062300000307     C*
062400991001     C                   MOVEL     VIDtle        Dtle
062500991001     C                   MOVEL     VIDazp        Dazp
062600991001     C                   Z-ADD     VIDkop        Dcop
062700991001     C                   MOVEL     VIDazf        Dazf
062800991001     C                   Z-ADD     VIDcof        Dcof
062900991011     C                   Z-ADD     VIhDDE        Ddde
063000991011     C                   Z-ADD     VIHDsc        Ddsc
063100000307     C*
063200000307     C                   ENDSR
063300970526     C*------------------------------------------------------------------------*
063400970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
063500970526     C*------------------------------------------------------------------------*
063600970526     C     NEWPOS        BEGSR
063700970526     C*
063800970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
063900970526     C*         1= RICHIESTO POSIZIONAMENTO
064000970526     C                   Z-ADD     0             SWPOSI            7 0
064100970526     C*
064200970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
064300970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
064400970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
064500970526     C                   Z-ADD     0             SWMODI            1 0
064600970530     C*
064700970530     C* REIMPOSTA INDICATORI DI ERRORE
064800970530     C                   SETOFF                                       808182
064900000215     C                   SETOFF                                       838485
065000000215     C                   SETOFF                                       86
065100970526     C*
065200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
065300021114     C                   IF        VIDtl2 <> DEPtl2 OR
065400021114     C                             VIDuni <> DEPuni OR
065500021114     C                             VIDele <> DEPele OR
065600021114     C                             VIDel2 <> DEPel2 OR
065700021114     C                             VIDun2 <> DEPun2 OR
065800021114     C                             VIDpdd <> DEPpdd OR
065900021114     C                             %SCAN('?':VIDUNI) > 0 OR
066000021114     C                             %SCAN('?':VIDUN2) > 0 OR
066100021114     C                             %SCAN('?':VIDELE) > 0 OR
066200021114     C                             %SCAN('?':VIDEL2) > 0
066300970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
066400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
066500970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
066600000308     C                   movel     VIDtl2        DEPtl2                         *DEPOSITI
066700991011     C                   move      VIDuni        DEPuni
066800991011     C                   move      VIDele        DEPele
066900000308     C                   move      VIDpdd        DEPpdd                         *DEPOSITI
067000991013     C                   move      VIDun2        DEPun2
067100991013     C                   move      VIDel2        DEPel2
067200970526E   2C                   ENDIF
067300970526E   1C                   ENDIF
067400970526     C*
067500970526     C                   ENDSR
067600970526     C*------------------------------------------------------------------------*
067700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
067800970526     C*------------------------------------------------------------------------*
067900970528     C     SETFIL        BEGSR
068000970526     C*
068100970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
068200000731     C                   MOVEL     *blanks       KCEtle
068300000731     C                   MOVEL     *blanks       KCEazp
068400000731     C                   MOVEL     *blanks       KCEazf
068500000731     C                   MOVEL(p)  PDDvid        KCEdde
068600000731     C                   MOVEL(p)  VIDuni        KCEcop
068700000731     C                   MOVEL(p)  VIDele        KCEcof
068800000731IF  1C     WTIPRI        IFEQ      'UNI'
068900160704     C**** KE1kun        SETLL     tikun01l                           99
069000160704     C     KE11kun       SETLL     tikun11l                           99
069100970528E   1C                   ENDIF
069200970528     C*
069300000731IF  1C     WTIPRI        IFEQ      'ELE'
069400000731     C     KE2kun        SETLL     tikun02L                           99
069500970528E   1C                   ENDIF
069600000731     C*
069700000731IF  1C     WTIPRI        IFEQ      'DAU'
069800000731     C     KE4kun        SETLL     tikun04l                           99
069900000731E   1C                   ENDIF
070000000731     C*
070100000731IF  1C     WTIPRI        IFEQ      'DAE'
070200000731     C     KE5kun        SETLL     tikun05L                           99
070300000731E   1C                   ENDIF
070400980107     C*
070500970526     C* LETTURA RECORD
070600970526IF  1C     *IN99         IFEQ      *OFF
070700970526     C                   EXSR      LETFIL
070800970526E   1C                   ENDIF
070900970526     C*
071000970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
071100970526IF  1C     *IN99         IFEQ      *ON
071200970526     C                   Z-ADD     1             SFLEOF
071300970526E   1C                   ENDIF
071400970526     C*
071500970526     C                   ENDSR
071600970526     C*------------------------------------------------------------------------*
071700970526     C* LETFIL - LETTURA RECORD
071800970526     C*------------------------------------------------------------------------*
071900970526     C     LETFIL        BEGSR
072000970526     C*
072100160701     c                   clear                   NRRECHID          9 0
072200970526     C* LEGGE FINO A:
072300970526     C*     - FINE FILE
072400970526     C*     - TROVATO RECORD VALIDO
072500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
072600970526DO  1C     *IN99         DOWEQ     *OFF
072700970526     C     WRECOK        ANDEQ     'N'
072800970526     C*
072900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
073000000731IF  2C     WTIPRI        IFEQ      'UNI'
073100160704     C******             READ      tikun01l                               99
073200160704     C                   READ      tikun11l                               99
073300160704     c**n99              z-add     K01_nrr       NRRECHID
073400160704     c  n99              z-add     K11_nrr       NRRECHID
073500970528E   2C                   ENDIF
073600970526     C*
073700000731IF  2C     WTIPRI        IFEQ      'ELE'
073800000731     C                   READ      tikun02L                               99
073900160701     c  n99              z-add     K02_nrr       NRRECHID
074000970528E   2C                   ENDIF
074100000731     C*
074200000731IF  2C     WTIPRI        IFEQ      'DAU'
074300000731     C                   READ      tikun04l                               99
074400160701     c  n99              z-add     K04_nrr       NRRECHID
074500000731E   2C                   ENDIF
074600000731     C*
074700000731IF  2C     WTIPRI        IFEQ      'DAE'
074800000731     C                   READ      tikun05L                               99
074900160701     c  n99              z-add     K05_nrr       NRRECHID
075000000731E   2C                   ENDIF
075100970528     C*
075200970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
075300970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
075400970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
075500970530X   2C                   ELSE
075600970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
075700970530E   2C                   ENDIF
075800970526E   1C                   ENDDO
075900970526     C*
076000160701DO  1C                   if        *in99 or WRECOK ='N'
076100160701     c                   clear                   NRRECHID
076200160701E   2C                   ENDIF
076300160701     C*
076400970526     C                   ENDSR
076500970526     C*------------------------------------------------------------------------*
076600970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
076700970526     C*------------------------------------------------------------------------*
076800970526     C     CHKREC        BEGSR
076900970526     C*
077000970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
077100100322     C                   setoff                                       7675
077200970530     C*
077300000731     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
077400000731     C*
077500000731     C     WRECOK        ifeq      'S'
077600000731     C     kundde        iflt      PDDvid
077700000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
077800000731     C                   endif
077900000731     C                   endif
078000000731     C*
078100000731     C     WRECOK        ifeq      'S'
078200000731     C                   move(p)   VIDuni        uninum           11 0
078300000731     C     kuncop        iflt      uninum
078400000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
078500000731     C                   endif
078600000731     C                   endif
078700000731     C*
078800000731     C     WRECOK        ifeq      'S'
078900000731     C                   move(p)   VIDele        elenum           11 0
079000000731     C     kuncof        iflt      elenum
079100000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
079200000731     C                   endif
079300000731     C                   endif
079400000731     C*
079500000731     C     WRECOK        ifeq      'S'
079600000731     C     VIDtl2        ifne      *blanks
079700000731     C     kuntle        ifne      VIDtl2
079800000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
079900000731     C                   endif
080000000731     C                   endif
080100000731     C                   endif
080200000731     C*
080300000731     C     WRECOK        ifeq      'S'
080400000731     C                   move      kuncop        alfa11           11
080500000731     C     VIDun2        ifeq      *zeros
080600000731     C     VIDun2        oreq      *blank
080700000731     C                   else
080800000731     C     VIDun2        ifne      alfa11
080900000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
081000000731     C                   endif
081100000731     C                   endif
081200000731     C                   endif
081300000731     C*
081400000731     C     WRECOK        ifeq      'S'
081500000731     C                   move      kuncof        alfa11
081600000731     C     VIDel2        ifeq      *zeros
081700000731     C     VIDel2        oreq      *blank
081800000731     C                   else
081900000731     C     VIDel2        ifne      alfa11
082000000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
082100000731     C                   endif
082200000731     C                   endif
082300000731     C                   endif
082400020212     C*
082500020927     C* Il controllo seguente lo effettuo solo se trattasi di utente di sede in quanto in filiale
082600020927     C* il file TIVSS nn c'è
082700091117     C*                  if        simfel = *zeros
082800091117     C*    WRECOK        ifeq      'S'
082900091117     C*                  if        kuntle = 'WW'    and
083000091117     C*                            datcor >= kundde and
083100091117     C*                            datcor <= kundsc
083200091117     C*                  move      kuncof        kssksu
083300091117     C*    keyvss        setll     tivss05l
083400091117     C*    keyvss        reade     tivss05l
083500091117     C*                  dow       not %eof(tivss05l)
083600091117     C*                  if        vssvat = *blanks                              *non annull/sosp.
083700091117     C*                  if        vssdsc < kundde or                            *non valido
083800091117     C*                            vssdde > kundsc
083900091117     C*                  else                                                    *valido
084000091117     C* Verifico che il servizio del record corrente del TIVSS sia soggetto a controllo unificazioni
084100091117     C*    vssisv        lookup    isvkun                                 22
084200091117     C*                  if        *in22 = *on
084300091117     C*                  seton                                        76
084400091117     C*                  leave
084500091117     C*                  endif
084600091117     C*                  endif
084700091117     C*                  endif
084800091117     C*    keyvss        reade     tivss05l
084900091117     C*                  enddo
085000091117     C*                  endif
085100091117     C*                  endif
085200091117     C*                  endif
085300100322     C*
085400100322     C                   if        WRECOK = 'S'
085500100322     C                   if        kundsc < datcor
085600100322     C                   seton                                        75
085700100322     C                   endif
085800100322     C                   endif
085900100322     C*
086000970526     C                   ENDSR
086100970526     C*------------------------------------------------------------------------*
086200970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
086300970526     C*------------------------------------------------------------------------*
086400970526     C     IRESFL        BEGSR
086500970526     C*
086600970526     C                   MOVEL     *BLANKS       OPZKEY
086700991001     C                   Z-ADD     kunDDE        VIHDDE
086800991011     C                   Z-ADD     kunDsc        VIHDsc
086900991001     C                   MOVEL     kuntle        VIDtle
087000991001     C                   MOVEL     kunazp        VIDazp
087100991001     C                   Z-ADD     kuncop        VIDkop
087200991001     C                   MOVEL     kunazf        VIDazf
087300991001     C                   Z-ADD     kuncof        VIDcof
087400991001     C                   move      kunvat        VIDvat
087500991001     C                   Z-ADD     kundde        G08INV
087600970618     C                   Z-ADD     *ZEROS        G08DAT
087700970618     C                   MOVEL     '3'           G08ERR
087800970618     C                   CALL      'XSRDA8'
087900970618     C                   PARM                    WLBDA8
088000970618     C                   Z-ADD     G08DAT        VIDDDE
088100991001     C                   Z-ADD     kunDSC        G08INV
088200970618     C                   Z-ADD     *ZEROS        G08DAT
088300970618     C                   MOVEL     '3'           G08ERR
088400970618     C                   CALL      'XSRDA8'
088500970618     C                   PARM                    WLBDA8
088600970618     C                   Z-ADD     G08DAT        VIDDSC
088700991015     c                   move      vidtle        wvidtl2
088800991015     c                   exsr      cnttle
088900970528     C* DECODIFICHE
089000091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
089100091126      * deciso di non utilizzarli per le unificazioni dei codici
089200091126      * quindi non controllo il tipo codice e do per scontato che è sempre 'C'=cliente
089300991210     C                   move      kuncop        wcli                           -codice
089400991210     c                   exsr      dectib
089500991210     c                   movel     wfvrag        viddep
089600991210     C                   move      kuncof        wcli              7 0          -codice
089700991210     c                   exsr      dectib
089800020905     c                   movel     wfvrag        viddes
089900091126
090000970526     C                   ENDSR
090100991210     C*------------------------------------------------------------------------*
090200991210     C* Dectib - DECODIFICA CLIENTE
090300991210     C*------------------------------------------------------------------------*
090400991210     C     Dectib        BEGSR
090500991210     C*
090600991210     C                   CLEAR                   BS69DS
090700991210     C                   CLEAR                   ACODS
090800991210     C                   MOVEL     KNSIF         I69SIF
090900991210     C                   Z-ADD     WCLI          I69KAC
091000991210     C                   CALL      'TIBS69R'
091100991210     C                   PARM                    BS69DS
091200991210     C                   PARM                    ACODS
091300991210     C                   PARM                    INDDS
091400991210     C                   PARM                    CLPDS
091500991210     C                   PARM                    CLSDS
091600991210     C     O69ERR        IFNE      '1'
091700991210     C                   MOVEL     ACORAG        WFVRAG           40
091800991210     C                   ELSE
091900991210     C                   MOVEL     *ALL'*'       WFVRAG
092000991210     c                   movel     '1'           errl00
092100991210     C                   ENDIF
092200991210     C*
092300991210     C                   ENDSR
092400970526     C*------------------------------------------------------------------------*
092500970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
092600970526     C*------------------------------------------------------------------------*
092700970526     C     CHKPAR        BEGSR
092800970528     C*
092900970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
093000140909     C***                MOVEL     'DAU'         WTIPRI            3
093100140909     C                   MOVEL     'UNI'         WTIPRI            3
093200970528     C*
093300000731     C* CONTROLLA LA DATA DECORRENZA
093400000731     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
093500970528     C                   Z-ADD     *ZEROS        PDDVID
093600970528IF  1C     ERRL00        IFEQ      '0'
093700970528IF  2C     VIDPDD        IFGT      *ZEROS
093800970528     C                   Z-ADD     VIDPDD        G08DAT
093900970528     C                   Z-ADD     *ZEROS        G08INV
094000970528     C                   MOVEL     '0'           G08ERR
094100970528     C                   CALL      'XSRDA8'
094200970528     C                   PARM                    WLBDA8
094300970528     C                   Z-ADD     G08DAT        VIDPDD
094400970528     C                   Z-ADD     G08INV        PDDVID
094500970528IF  3C     G08ERR        IFEQ      '1'
094600970528     C                   MOVEL     '1'           ERRL00
094700970528     C                   MOVEL     MSG(12)       DSMSMS
094800970528     C                   SETON                                        83
094900970528E   3C                   ENDIF
095000970528E   2C                   ENDIF
095100970528E   1C                   ENDIF
095200970528     C*
095300000731     C* CONTROLLA IL TIPO RICERCA
095400000731     C                   move      *blank        wtlecu
095500000731     C                   move      *blank        wtlece
095600000731IF  1C     ERRL00        IFEQ      '0'
095700000731IF  2C     VIDtl2        IFNE      *blank
095800000731     C                   move      vidtl2        wvidtl2           2
095900000731     C                   exsr      cnttle
096000000731     C                   move      wvidtl2       vidtl2
096100000731E   2C                   ENDIF
096200000731E   1C                   ENDIF
096300000731     C*
096400000731     C     ERRL00        IFEQ      '0'
096500991012IF  2C     VIDuni        IFNE      *ZEROS
096600991012IF  2C     VIDuni        andne     *blank
096700021114     C                   MOVEL     'UNI'         WTIPRI
096800991015     c                   move      wtlecu        clipot            1
096900991012     C                   MOVEL     VIDuni        Wcop
097000991012     C                   EXSR      CHKcop
097100991012     C                   MOVEL     Wcop          VIDuni
097200021114     C     PDDvid        IFNE      *zeros
097300021114     C                   MOVEL     'DAU'         WTIPRI
097400021114     C                   ENDIF
097500000731E   2C                   ELSE
097600980107     C*
097700000731IF  1C     VIDele        IFNE      *ZEROS
097800991012IF  2C     VIDele        andne     *blank
097900991001     C                   MOVEL     'ELE'         WTIPRI
098000991015     c                   movel     wtlece        clipot
098100991012     C                   MOVEL     VIDele        Wcop
098200991012     C                   EXSR      CHKcop
098300991012     C                   MOVEL     Wcop          VIDele
098400000731     C     PDDvid        IFNE      *zeros
098500000731     C                   MOVEL     'DAE'         WTIPRI
098600000731     C                   ENDIF
098700980107E   2C                   ENDIF
098800000731E   1C                   ENDIF
098900000731     C                   ENDIF
099000021114     C*
099100021114     C     ERRL00        IFEQ      '0'
099200021114IF  2C     VIDun2        IFNE      *ZEROS
099300021114IF  2C     VIDun2        andne     *blank
099400021114     C                   move      wtlecu        clipot
099500021114     C                   MOVEL     VIDun2        Wcop
099600021114     C                   EXSR      CHKcop
099700021114     C                   MOVEL     Wcop          VIDun2
099800021114E   2C                   ELSE
099900021114     C*
100000021114IF  1C     VIDel2        IFNE      *ZEROS
100100021114IF  2C     VIDel2        andne     *blank
100200021114     c                   movel     wtlece        clipot
100300021114     C                   MOVEL     VIDel2        Wcop
100400021114     C                   EXSR      CHKcop
100500021114     C                   MOVEL     Wcop          VIDel2
100600021114E   2C                   ENDIF
100700021114E   1C                   ENDIF
100800021114     C                   ENDIF
100900000731     C*
101000970526     C                   ENDSR
101100991012     C*------------------------------------------------------------------------*
101200991012     C* Cnttle - CONTROLLA codice tipo legame
101300991012     C*------------------------------------------------------------------------*
101400000731     C     cnttle        begsr
101500000731     C                   clear                   TIBS02DS
101600000731     C*
101700991015     C     '?'           SCAN      wVidtl2                                30
101800000731     C*  Ricerca
101900000731     C                   eval      t02tla = 'L'
102000000731    1C                   IF        *IN30 = *ON
102100000731     C                   movel     'R'           t02mod
102200000731     C                   else
102300000731     C                   movel     'C'           t02mod
102400000731     C                   movel(p)  Wvidtl2       t02ke1
102500000731     C                   end
102600000731     C                   movel     'TLE'         t02cod
102700000731     C                   movel     knsif         t02sif
102800991012     C                   CALL      'TIBS02R'
102900000731     C                   parm                    KPJBA
103000000731     C                   parm                    TIBS02DS
103100000731     C
103200000731     C*
103300991012    2C                   IF        T02err = *blanks
103400991015     C                   movel     T02ke1        wVidtl2
103500991015     C                   movel     T02uni        dstle
103600991015     C                   movel     §tlecu        Wtlecu            1
103700991015     C                   movel     §tlece        wtlece            1
103800000731     C                   else
103900000731     C                   movel     '1'           errl00
104000000731     C                   seton                                        86
104100000215     C                   MOVEL     MSG(11)       DSMSMS
104200991015     C                   ENDIF
104300000731     C
104400000731     C                   endsr
104500991012     C*------------------------------------------------------------------------*
104600991012     C* CHKcop - CONTROLLA codice padre figlio
104700991012     C*------------------------------------------------------------------------*
104800991012     C     CHKcop        BEGSR
104900991012     C*
105000991012     C     *LIKE         DEFINE    VIDuni        Wcop
105100991012     C*
105200991012     C* RICHIESTO ELENCO
105300991012     C                   SETOFF                                           28
105400991012     C     '?'           SCAN      Wcop                                   28
105500991012IF  1C     *IN28         IFEQ      *ON
105600991012     C* RICHIESTO ELENCO CODICI
105700091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
105800091126      * deciso di non utilizzarli per le unificazioni dei codici
105900091126      * quindi non controllo il tipo codice e do per scontato che è sempre cliente
106000991012     C                   CLEAR                   DSXC
106100991012     C                   Z-ADD     KCCCL         DXCCAP
106200991012     C                   CALL      'XCLIR'
106300991012     C                   PARM                    DSXC
106400991012     C* SCOMPATTA
106500991012     C                   MOVEA     DXCKSC        KSA                        ALF
106600991012DO  2C     1             DO        30            J                 3 0
106700991012     C                   MOVE      KSA(J)        DSKSA                      F
106800991012     C                   Z-ADD     DSKSN         KSC(J)                     M
106900991012E   2C                   ENDDO
107000991012     C                   MOVEL     KSC(1)        WKSC              7
107100991012     C                   MOVEL     '1'           $TESVD            1        TA
107200991012E   1C                   ENDIF
107300991012     C*
107400991012     C* CODICE CLIENTE NUMERICO
107500991012IF  1C     WKSC          IFLT      *ZEROS
107600991012     C                   MOVEL     '1'           WERR              1
107700991012X   1C                   ELSE
107800991012     C*
107900991012     C* SOLO CARATTERI NUMERICI
108000991012     c   28              move      *all'0'       wcop
108100991012     c   28              move      wksc          wcop
108200991012     C                   SETOFF                                       28
108300991012     C                   TESTN                   Wcop                 28
108400991012IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
108500991012     C                   MOVEL     '1'           ERRL00
108600991012X   1C                   ELSE                                                   *E'NUMERO DA TESTN
108700991012     C                   MOVE      Wcop          W001              1
108800991012IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
108900991012     C                   MOVEL     '1'           ERRL00
109000991012X   2C                   ELSE                                                   *NUMERO
109100991012     C*
109200991012     C* CONTROLLA ESISTENZA CODICE
109300991210     C                   MOVE      Wcop          wcli
109400991210     C                   exsr      dectib
109500991012E   2C                   ENDIF
109600991012E   1C                   ENDIF
109700991012E   1C                   ENDIF
109800991012     C*
109900991012     C                   ENDSR
110000970526     C*------------------------------------------------------------------------*
110100970526     C* LANPGM - LANCIA UN PROGRAMMA
110200970526     C*------------------------------------------------------------------------*
110300970526     C     LANPGM        BEGSR
110400970526     C*
110500970526     C* IMPOSTA LA DS DI PROCEDURA
110600991001     C                   MOVEL     WOP0          k12OP0                         *LIVELLO PROCEDURA
110700991001     C                   MOVEL     WOP1          k12OP1                         *LIVELLO PGM
110800991001     C                   MOVEL     '0'           k12F03                         *PREMUTO F03=FINE
110900991001     C                   MOVEL     '0'           k12F12                         *PREMUTO F12=RITORNO
111000991001     C                   MOVEL     '0'           k12ERR                         *'1'=ERRORE
111100991001     C                   MOVEL     *BLANKS       k12MSG                         *MESSAGGIO DI RITORN
111200991001     C                   MOVEL     Dtle          k12tle                         *PARTENZA/ARRIVO
111300991001     c                   movel     dazp          k12azp
111400991001     C                   Z-ADD     Dcop          k12cop                         *TERMINAL PARTENZA
111500991001     c                   movel     dazf          k12azf
111600991001     C                   Z-ADD     Dcof          k12cof                         *TERMINAL ARRIVO
111700991001     C                   Z-ADD     Ddde          k12dde                         *DECORRENZA
111800991001     C                   Z-ADD     Ddsc          k12dsc                         *DECORRENZA
111900970526     C*
112000991104     C                   movel     tibds         kpjbu
112100970526     C                   CALL      WPGM
112200970612     C                   PARM                    KPJBA
112300991104     C*                  PARM                    tibds
112400970526     C*
112500970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
112600991001IF  1C     k12F03        IFEQ      '1'                                          *PREMUTO F03
112700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
112800970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
112900970526X   1C                   ELSE                                                   *NON PREMUTO F03
113000991001     C                   MOVEL     k12MSG        DSMSMS                         *MESSAGGIO DI RITORN
113100970526E   1C                   ENDIF
113200970526     C*
113300970526     C                   ENDSR
113400000607     C*------------------------------------------------------------------------*
113500000607     C* LANPGM2- LANCIA UN Pgm alternativo per ds non di procedura
113600000607     C*------------------------------------------------------------------------*
113700000607     C     LANPGM2       BEGSR
113800000607     C*
113900000607     C* IMPOSTA LA DS DI PROCEDURA
114000000607     c                   clear                   tis720
114100000607     C                   MOVEL     WOP0          s20OP0                         *LIVELLO PROCEDURA
114200000607     C                   MOVEL     WOP1          s20OP1                         *LIVELLO PGM
114300000607     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
114400000607     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
114500000607     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
114600000607     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
114700000607     C                   MOVEL     Dtle          s20tle                         *PARTENZA/ARRIVO
114800000607     C*                  move      *zeros        s20ksc                         *TERMINAL PARTENZA
114900000607     C                   move(p)   Dcop          s20ksc                         *TERMINAL PARTENZA
115000000607     C*
115100000607     C                   movel     tis720        kpjbu
115200000607     C                   CALL      WPGM
115300000607     C                   PARM                    KPJBA
115400000607     C*
115500000607     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
115600000607IF  1C     s20F03        IFEQ      '1'                                          *PREMUTO F03
115700000607     C                   MOVEL     '1'           ERRL00                         *ERRORE
115800000607     C                   MOVEL     '1'           F03L00                         *FINE PGM
115900000607X   1C                   ELSE                                                   *NON PREMUTO F03
116000000607     C                   MOVEL     s20MSG        DSMSMS                         *MESSAGGIO DI RITORN
116100000607E   1C                   ENDIF
116200000607     C*
116300000607     C                   ENDSR
116400991001     c*--------------------------------------------------------------------------------------------*
116500991001     C* cartab - carica le tabelle occorrenti
116600991001     c*--------------------------------------------------------------------------------------------*
116700991001     C     cartab        BEGSR
116800020905     C*
116900020905     C* REPERISCE I DATI UTENTE
117000020905     C                   EXSR      REPDATIUTE
117100020905     C*
117200020212     C*----------
117300020212     C* Tipi servizi soggetti a controllo unificazioni
117400020212     C*----------
117500020212     C                   z-add     *zeros        x                 3 0
117600020212     C                   eval      tbecod = 'ISV'
117700020212     C     ke2tbe        setll     tntbe01l
117800020212     C     ke2tbe        reade     tntbe01l
117900020212     C                   dow       not %eof(tntbe01l)
118000020212     C                   movel     tbeuni        dsisv
118100020212     C                   if        §isvuni  = 'S'
118200020212     C                   add       1             x
118300020212     C                   eval      isvkun(x) = tbeke1
118400020212     C                   endif
118500020212     C     ke2tbe        reade     tntbe01l
118600020212     C                   enddo
118700020212     C*
118800991001     c*
118900991001     C                   ENDSR
119000970526     C*------------------------------------------------------------------------*
119100970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
119200970526     C*------------------------------------------------------------------------*
119300970526     C     CHKFOP        BEGSR
119400970626     C*
119500970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
119600970626IF  1C     DEPOP0        IFEQ      'P01'
119700970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
119800970626     C                   MOVEL     'S'           OPDATT
119900970626     C                   MOVEL     OPDRIG        OPD(1)
120000970626X   1C                   ELSE
120100970626     C* OPZIONI
120200970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
120300970626     C                   MOVEL     'S'           OPDATT
120400970626     C                   MOVEL     OPDRIG        OPD(5)
120500020709     C     DUTLPO        IFEQ      'S'
120600000607     C                   MOVEL     OPD(9)        OPDRIG                         *CLI ABILITATI
120700000607     C                   MOVEL     'S'           OPDATT
120800000607     C                   MOVEL     OPDRIG        OPD(9)
120900000830     C                   ENDIF
121000970626     C* FUNZIONI
121100970626E   1C                   ENDIF
121200970626     C*
121300970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
121400970626IF  1C     DEPOP0        IFEQ      'P02'
121500970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
121600970626     C                   MOVEL     'S'           OPDATT
121700970626     C                   MOVEL     OPDRIG        OPD(2)
121800020709     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
121900991014     C                   MOVEL     'S'           OPDATT
122000991014     C                   MOVEL     OPDRIG        OPD(3)
122100000307     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
122200000307     C                   MOVEL     'S'           OPDATT
122300000307     C                   MOVEL     OPDRIG        OPD(4)
122400970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
122500970626     C                   MOVEL     'S'           OPDATT
122600970626     C                   MOVEL     OPDRIG        OPD(7)
122700970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
122800970626     C                   MOVEL     'S'           FUDATT
122900970626     C                   MOVEL     FUDRIG        FUD(6)
123000970626E   1C                   ENDIF
123100970526     C*
123200970526     C* GESTIONE OPZIONI A VIDEO
123300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
123400970526     C                   EXSR      OPZGES
123500970526     C*
123600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
123700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
123800970526     C                   EXSR      FUNGES
123900970526     C*
124000970526     C                   ENDSR
124100991210     C*------------------------------------------------------------------------*
124200991210     C* FINSR - OPERAZIONI FINALI
124300991210     C*------------------------------------------------------------------------*
124400991210     C     FINSR         BEGSR
124500991210     C*
124600991210     C* CHIUDE I FILE DEI PGM CHIAMATI
124700991210     C                   CLEAR                   bs69ds
124800991210     C                   CLEAR                   acods
124900991210     C                   CLEAR                   indds
125000991210     C                   CLEAR                   clpds
125100991210     C                   CLEAR                   clsds
125200991210     C                   MOVEL     'C'           i69tla
125300991210     C                   MOVEL     knsif         i69sif
125400991210     C                   CALL      'TIBS69R'
125500991210     C                   PARM                    bs69ds
125600991210     C                   PARM                    acods
125700991210     C                   PARM                    indds
125800991210     C                   PARM                    clpds
125900991210     C                   PARM                    clsds
126000991210     C*
126100991210     C                   ENDSR
126200020905     C*--------------------------------------------------------------------------------------------*
126300020905     C* REPERISCE I DATI UTENTE
126400020905     C*--------------------------------------------------------------------------------------------*
126500020905     C     REPDATIUTE    BEGSR
126600020905     C*
126700020905     C* INIZIALIZZA VARIABILI DI WRK
126800020905     C                   CLEAR                   TIBS34DS
126900020905     C                   CLEAR                   AZUTEDS
127000020905     C                   CLEAR                   DDATIUTE
127100020905     C*
127200020905     C     *DTAARA       DEFINE    §azute        azuteds
127300020905     C     *DTAARA       DEFINE    §datiute      ddatiute
127400020905     C                   IN(E)     *DTAARA
127500020905if  1C                   IF        %Error
127600020905     c                   EVAL      I34Tla = 'L'
127700020905     C                   CALL      'TIBS34R'
127800020905     C                   PARM                    Tibs34Ds
127900020905     C                   IN        *DTAARA
128000020905e   1C                   ENDIF
128100020905     C*
128200020905     C                   MOVEL     RSUT          DSFIRS
128300060113     C*
128400060113     C* Carica le abilitazioni particolari del profilo
128500060113     C                   Movel     *blanks       wabi              2
128600060113     C                   Movel     utefaf        dute01
128700060113     C                   If        §utecli <> *Blanks
128800060113     C                   Eval      wabi = §utecli
128900060113     C                   Else
129000060113     C                   Eval      wabi = uteaut
129100060113     C                   EndIf
129200060113     C*
129300060113     C* Reperimento dei P.O. gestibili dall'utente per i codici clienti
129400060113     C                   Clear                   Trul31ds
129500060113     C                   Eval      i31abi = wabi
129600060113     C                   Eval      i31cdi = dutdis
129700060113     C                   Eval      i31car = dutare
129800060113     C                   Eval      i31cpo = dutpou
129900060113     C                   Call      'TRUL31R'
130000060113     C                   Parm                    kpjba
130100060113     C                   Parm                    Trul31ds
130200060113     C                   If        o31pog > *zeros
130300060113     C                   Movea     o31pog        skPOG
130400060113     C                   EndIf
130500020905     C*
130600020905     C                   ENDSR
130700970526     C*------------------------------------------------------------------------*
130800000307     C* *INZSR - OPERAZIONI INIZIALI
130900970526     C*------------------------------------------------------------------------*
131000970526     C     *INZSR        BEGSR
131100970526     C*---
131200970526     C* RICEVIMENTO PARAMETRI
131300970526     C*---
131400970526     C     *ENTRY        PLIST
131500970612     C                   PARM                    KPJBA
131600020709     c                   MOVEL     KPJBU         TIBDS
131700970526     C*
131800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
131900970526     C     XMSGPA        PLIST
132000970526     C                   PARM                    DSMSMS
132100970526     C                   PARM                    DSMSPG
132200970526     C                   PARM                    DSMSKE
132300970526     C                   PARM                    DSMSER
132400970526     C*
132500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
132600970526     C                   CALL      'XKEY'
132700970526     C                   PARM                    DSKEY
132800970526     C*---
132900970526     C* VARIABILI NON RIFERITE AL DATA BASE
133000970526     C*---
133100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
133200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
133300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
133400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
133500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
133600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
133700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
133800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
133900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
134000991001     C     *LIKE         DEFINE    k12OP0        DEPOP0                         *DI DEPOSITO
134100991001     C     *LIKE         DEFINE    k12OP1        DEPOP1
134200991001     C     *LIKE         DEFINE    k12OP0        WOP0                           *DI LAVORO
134300991001     C     *LIKE         DEFINE    k12OP1        WOP1
134400991001     C     *LIKE         DEFINE    k12tle        Dtle
134500991001     C     *LIKE         DEFINE    k12cop        Dcop
134600991001     C     *LIKE         DEFINE    k12azp        Dazp
134700991001     C     *LIKE         DEFINE    k12cof        Dcof
134800991001     C     *LIKE         DEFINE    k12azf        Dazf
134900991001     C     *LIKE         DEFINE    k12dde        Ddde
135000991001     C     *LIKE         DEFINE    k12dsc        Ddsc
135100970526     C*---
135200991001     C     *LIKE         DEFINE    k12tle        Deptle
135300991001     C     *LIKE         DEFINE    k12tle        Deptl2
135400991011     C     *LIKE         DEFINE    viduni        Depuni
135500991013     C     *LIKE         DEFINE    vidun2        Depun2
135600991011     C     *LIKE         DEFINE    videle        Depele
135700991013     C     *LIKE         DEFINE    videl2        Depel2
135800991013     C     *LIKE         DEFINE    vidpdd        Deppdd
135900000307     C*---
136000970526     C* VARIABILI DI CONTROLLO
136100970526     C*---
136200970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
136300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
136400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
136500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
136600970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
136700970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
136800000307     C*---
136900000307     C* INIZIALIZZAZIONE CAMPI A VIDEO
137000000307     C*---
137100020709     C                   MOVEL     *blanks       VIDUNI
137200020709     C                   MOVEL     *blanks       VIDUN2
137300020709     C                   MOVEL     *blanks       VIDELE
137400020709     C                   MOVEL     *blanks       VIDEL2
137500020709     C                   MOVEL     *blanks       VIDTL2
137600020709     C                   MOVEL     *blanks       VIDPDD
137700970526     C*
137800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
137900991001     C                   MOVEL     'TIBS12R'     DSMSPG
138000991001     C                   MOVEL     'TIBS12R'     MSGQUE
138100970526     C                   MOVEL     *BLANKS       MSGKEY
138200970526     C                   MOVEL     *BLANKS       DSMSMS
138300970526     C                   MOVEL     *BLANKS       DSMSKE
138400970526     C                   Z-ADD     *ZEROS        DSMSER
138500970526     C*---
138600970526     C* VARIABILI RIFERITE AL DATA BASE
138700970526     C*---
138800020830     C     *LIKE         DEFINE    acokcc        kcccl
138900000731     C     *LIKE         DEFINE    kuncop        KCEcop
139000000731     C     *LIKE         DEFINE    kuncof        KCEcof
139100000731     C     *LIKE         DEFINE    kundde        KCEdde
139200000731     C     *LIKE         DEFINE    kunazp        KCEazp
139300000731     C     *LIKE         DEFINE    kunazf        KCEazf
139400000731     C     *LIKE         DEFINE    kuntle        KCEtle
139500000307     C     *LIKE         DEFINE    tbecod        KTBcod
139600000307     C     *LIKE         DEFINE    tbeke1        KTBky1
139700020211     C     *LIKE         DEFINE    vssksu        KSSksu
139800020212     C     *LIKE         DEFINE    vssisv        KSSisv
139900020905     C     *LIKE         DEFINE    kpjbu         Depkpjbu
140000970526     C*---
140100970526     C* CHIAVI DI LETTURA
140200970526     C*---
140300000731     C* LETTURA tikun01L
140400000731     C     KE1kun        KLIST
140500000731     C                   KFLD                    KCEtle
140600000731     C                   KFLD                    KCEazp
140700000731     C                   KFLD                    KCEcop
140800000731     C                   KFLD                    KCEazf
140900000731     C                   KFLD                    KCEcof
141000160704     C* LETTURA tikun01L
141100160704     C     KE11kun       KLIST
141200160704     C                   KFLD                    KCEazp
141300160704     C                   KFLD                    KCEcop
141400160704     C                   KFLD                    KCEazf
141500160704     C                   KFLD                    KCEcof
141600160704     C                   KFLD                    KCEtle
141700020211     C* LETTURA tikun02L
141800000731     C     KE2kun        KLIST
141900000731     C                   KFLD                    KCEtle
142000000731     C                   KFLD                    KCEazf
142100000731     C                   KFLD                    KCEcof
142200000731     C                   KFLD                    KCEazp
142300000731     C                   KFLD                    KCEcop
142400020211     C* LETTURA tikun04L
142500000731     C     KE4kun        KLIST
142600000731     C                   KFLD                    KCEdde
142700000731     C                   KFLD                    KCEcop
142800000731     C                   KFLD                    KCEcof
142900020211     C* LETTURA tikun05L
143000000731     C     KE5kun        KLIST
143100000731     C                   KFLD                    KCEdde
143200000731     C                   KFLD                    KCEcof
143300000731     C                   KFLD                    KCEcop
143400991001     C* LETTURA cnaco00f
143500991001     C     keycli        KLIST                                                  *di cnaco/ind/clp
143600991001     C                   KFLD                    kclkut            1 0          -utente
143700991001     C                   KFLD                    kcccl                          -capoconto
143800991001     C                   KFLD                    kcksc             7 0          -codice
143900000307     C* LETTURA tntbe01l -
144000000307     C     KEYtbe        KLIST
144100000307     C                   KFLD                    Ktbcod                         *CODICE TABELLA
144200000307     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
144300020212     C* LETTURA tntbe01l - PARZIALE
144400020212     C     KE2tbe        KLIST
144500020212     C                   KFLD                    tbecod                         *CODICE TABELLA
144600020212     C* LETTURA tivss05L
144700020211     C     KEYvss        KLIST
144800020211     C                   KFLD                    KSSksu
144900970527     C*
145000970527     C* CARICA LE TABELLE OCCORRENTI
145100970527     C                   EXSR      CARTAB
145200020927     C*
145300020927     C* IN BASE ALL'UTENTE EFFETTUO ALCUNE CONSIDERAZIONI
145400021007     C                   IF        simfel = *zeros
145500020927     C                   OPEN      tivss05l
145600020927     C                   ENDIF
145700020916     C*
145800020916     C* INIZIALIZZAZIONE VARIABILI DI WRK
145900020916     C                   Z-ADD     1             kclkut
146000020916     C                   Z-ADD     DUTKCI        kcccl
146100970526     C*
146200970526     C* DEPOSITA LE OPZIONI DI ENTRATA
146300991001     C                   MOVEL     k12OP0        DEPOP0                         *LIVELLO PROCEDURA
146400991001     C                   MOVEL     k12OP1        DEPOP1                         *LIVELLO PROGRAMMA
146500970526     C*
146600970526     C* CALCOLA LA DATA CORRENTE
146700100322     C                   Z-ADD     *ZEROS        DATCOR            8 0          *DATA CORRENTE AA/M/
146800100322     C                   EVAL      datcor = %dec(%date() : *ISO)
146900970526     C*
147000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
147100991028     c                   if        k12tle <> *blank and
147200991028     c                             k12tle <> *zeros
147300991028     c                   move      k12tle        vidtl2
147400991028     c                   else
147500991028     C                   MOVE      *blank        VIDtl2                         *PARTENZA/ARRIVO
147600991028     c                   end
147700000731     C*                  move      k12cop        VIDuni                         *CODICE CLI PADRE
147800000602     C                   move      k12cop        VIDun2                         *CODICE CLI PADRE
147900000602     C                   move      *zeros        VIDele                         *CODICE CLI FIGLIO
148000000602     C                   Z-ADD     *ZEROS        VIDPDD                         *DATA DECORRENZA
148100991001IF  1C     k12dde        IFGT      *ZEROS
148200991001     C                   Z-ADD     k12dde        G08INV
148300970528     C                   Z-ADD     *ZEROS        G08DAT
148400970528     C                   MOVEL     '3'           G08ERR
148500970528     C                   CALL      'XSRDA8'
148600970528     C                   PARM                    WLBDA8
148700970528     C                   Z-ADD     G08DAT        VIDPDD
148800970528E   1C                   ENDIF
148900970526     C                   EXSR      CHKPAR
149000970526IF  1C     ERRL00        IFEQ      '1'
149100991001     C                   MOVEL     '1'           k12ERR
149200991001     C                   MOVEL     DSMSMS        k12MSG
149300970526X   1C                   ELSE
149400991001     C                   MOVEL     VIDtle        DEPtle                         *DEPOSITI
149500991011     C                   move      VIDuni        DEPuni
149600991011     C                   move      VIDele        DEPele
149700991013     C                   Z-ADD     VIDpdd        DEPpdd
149800991013     C                   move      VIDun2        DEPun2
149900991013     C                   move      VIDel2        DEPel2
150000140609     C                   movel     VIDtl2        DEPtl2                         *tipo legame
150100970526E   1C                   ENDIF
150200970526     C*
150300970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
150400970526     C                   EXSR      CHKFOP
150500970526     C*
150600970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
150700970530IF  1C     DEPOP0        IFEQ      'P01'
150800970530     C                   MOVEL     DEC(3)        VIDING
150900970530E   1C                   ENDIF
151000970530IF  1C     DEPOP0        IFEQ      'P02'
151100970526     C                   MOVEL     DEC(1)        VIDING
151200970530E   1C                   ENDIF
151300970530IF  1C     DEPOP0        IFEQ      'P05'
151400970526     C                   MOVEL     DEC(2)        VIDING
151500970526E   1C                   ENDIF
151600970526     C*
151700970526     C                   ENDSR
151800970526     C*------------------------------------------------------------------------*
151900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152100970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
152200970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
152300970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
152400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152600970526     C*------------------------------------------------------------------------*
152700970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
152800970526     C*------------------------------------------------------------------------*
152900970526     C     POSSFL        BEGSR
153000970526     C*
153100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
153200970526     C                   EXSR      CLRSFL
153300970526     C*
153400970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
153500970526     C                   EXSR      SETFIL
153600970526     C*
153700970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
153800970526     C     SFLEOF        IFEQ      0
153900970526     C                   EXSR      CARSFL
154000970526     C                   END
154100970526     C*
154200970526     C                   ENDSR
154300970526     C*------------------------------------------------------------------------*
154400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
154500970526     C*------------------------------------------------------------------------*
154600970526     C     ENTATT        BEGSR
154700970526     C*
154800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
154900970526     C                   Z-ADD     0             OPZOK             1 0
155000970526     C*
155100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
155200970526     C                   Z-ADD     0             SFLVAR            5 0
155300970526     C*
155400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
155500970526     C                   Z-ADD     0             SFLERR
155600970526     C*
155700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
155800970526     C                   Z-ADD     SFLULT        WFLULT
155900970526     C*
156000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
156100991011     C                   READC     BS12D05                                98
156200970526     C     *IN98         DOWEQ     '0'
156300970526     C*
156400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
156500970526     C                   ADD       1             SFLVAR
156600970526     C*
156700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
156800970526     C     OPZOK         IFEQ      0
156900970526     C*
157000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
157100970526     C     OPZKEY        IFNE      *BLANKS
157200970526     C                   EXSR      OPZCON
157300970526     C                   END
157400970526     C*
157500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
157600970526     C*            2. PULIZIA SCELTA IN SFL
157700970526     C     OPZOK         IFEQ      0
157800970526     C                   MOVEL     '0'           *IN41
157900970526     C                   MOVEL     *BLANKS       OPZKEY
158000970526     C                   ELSE
158100970526     C*
158200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
158300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
158400970526     C                   MOVEL     '1'           *IN41
158500970526     C                   Z-ADD     SFLULT        SFLERR
158600970526     C                   END
158700970526     C                   ELSE
158800970526     C*
158900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
159000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
159100970526     C                   MOVEL     '1'           *IN41
159200970526     C                   END                                                    OPZOK=0
159300970526     C*
159400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
159500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
159600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
159700970526     C     OPZOK         IFNE      0
159800970526     C                   Z-ADD     SFLERR        SFLNBR
159900970526     C                   ELSE
160000970526     C                   Z-ADD     SFLULT        SFLNBR
160100970526     C                   END
160200970526     C*
160300160701     C* Riaggiorna il record appena modificato
160400160701     c     nrec1         chain     tikun00f
160500160701     c                   if        %Found(tikun00f)
160600160701     c                   exsr      IRESFL
160700160701     c                   setoff                                       75
160800160701     C                   if        kundsc < datcor
160900160701     C                   seton                                        75
161000160701     C                   endif
161100160701     c                   end
161200160701     C*
161300970526     C* AGGIORNO RECORD SFL
161400160704     C                   EVAL         HIN75 = *IN75
161500160704     C                   EVAL      *IN76 = HIN76
161600991011     C                   UPDATE    BS12D05
161700970526     C*
161800991011     C                   READC     BS12D05                                98
161900970526     C                   END                                                    DO WHILE
162000970526     C*
162100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
162200970526     C                   Z-ADD     WFLULT        SFLULT
162300970526     C*
162400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
162500970526     C     SFLVAR        IFEQ      0
162600970526     C*---                 MOVEL'1'       FINL00
162700970526     C                   END
162800970526     C*
162900970526     C                   ENDSR
163000970526     C*------------------------------------------------------------------------*
163100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
163200970526     C*------------------------------------------------------------------------*
163300970526     C     CLRSFL        BEGSR
163400970526     C*
163500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
163600970526     C* (0=NON FINE FILE  1=FINE FILE)
163700970526     C                   Z-ADD     0             SFLEOF
163800970526     C*
163900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
164000970526     C                   Z-ADD     0             SFLULT
164100970526     C*
164200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
164300970526     C                   MOVEL     '1'           *IN40
164400991011     C                   WRITE     BS12D06
164500970526     C                   MOVEL     '0'           *IN40
164600970526     C*
164700970526     C                   ENDSR
164800970526     C*------------------------------------------------------------------------*
164900970526     C* CARSFL - CARICAMENTO SFL
165000970526     C*------------------------------------------------------------------------*
165100970526     C     CARSFL        BEGSR
165200970526     C*
165300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
165400970526     C     SFLULT        IFEQ      0
165500970526     C                   EXSR      SETFIL
165600970526     C                   END
165700970526     C*
165800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
165900970526     C                   Z-ADD     SFLULT        WFLULT
166000160704     c                   eval      RrnLast = 0
166100970526     C*
166200970526     C* CICLO:
166300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
166400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
166500970526     C                   Z-ADD     1             SFLRIG
166600160704     C     SFLRIG        DOWLE     SFLPAG
166700160704     C     SFLEOF        ANDEQ     0
166800970526     C*
166900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
167000970526     C                   MOVEL     *BLANKS       OPZKEY
167100970526     C*
167200970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
167300970526     C                   EXSR      IRESFL
167400970526     C*
167500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
167600970526     C                   ADD       1             SFLULT
167700160701     c                   z-add     nrrecHID      nrec1
167800970526     C*
167900970526     C* SCRITTURA RECORD SFL
168000160704     C                   EVAL      HIN75 = *IN75
168100160704     C                   EVAL      HIN76 = *IN76
168200160704     C*
168300991011     C                   WRITE     BS12D05
168400970526     C                   ADD       1             SFLRIG
168500160704     c                   eval      RrnLast = RrnLast +1
168600970526     C*
168700970526     C* LETTURA FILE DA CARICARE
168800970526     C                   EXSR      LETFIL
168900970526     C                   END                                                    DO WHILE
169000160704     C*
169100160704     C*  Riordina i dati da emettere
169200160704     C******             exsr      Ordina_SFL
169300970526     C*
169400970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
169500970526     C     WFLULT        IFNE      SFLULT
169600970526     C                   Z-ADD     WFLULT        SFLNBR
169700970526     C                   ADD       1             SFLNBR
169800970526     C                   END
169900970526     C*
170000970526     C                   ENDSR
170100970526     C*------------------------------------------------------------------------*
170200970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
170300970526     C*------------------------------------------------------------------------*
170400970526     C     OUTSFL        BEGSR
170500970526     C*
170600970526     C* WRITE RECORD TESTATA VIDEO
170700991011     C                   WRITE     BS12D01
170800970526     C*
170900970526     C* VISUALIZZAZIONE MESSAGGIO
171000970526     C                   EXSR      SNDMSG
171100970526     C*
171200000307     C* WRITE RECORD OPZIONI VIDEO
171300991011     C                   WRITE     BS12D02
171400970526     C*
171500970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
171600991011     C                   WRITE     BS12D03
171700970526     C*
171800970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
171900991011     C                   WRITE     BS12D04
172000970526     C*
172100970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
172200970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
172300970526     C     SFLEOF        IFEQ      0
172400970526     C                   MOVEL     '0'           *IN42
172500970526     C                   ELSE
172600970526     C*
172700970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
172800970526     C                   MOVEL     '1'           *IN42
172900970526     C                   END
173000970526     C*
173100970526     C* WRITE RECORD TASTI FUNZIONALI
173200991011     C                   WRITE     BS12D09
173300970526     C*
173400970526     C* CASO DI SFL VUOTO
173500970526     C     SFLULT        IFLE      0
173600970526     C*
173700970526     C*    - WRITE RECORD VUOTO
173800991011     C                   WRITE     BS12D0A
173900991011     C                   READ      BS12D0A                                99
174000970526     C*
174100970526     C                   ELSE
174200970526     C*
174300970526     C* CASO DI SFL CON RECORDS
174400970526     C*
174500970526     C*    - WRITE RECORD CONTROLLO SFL
174600991011     C                   WRITE     BS12D06
174700991011     C                   READ      BS12D06                                99
174800970526     C                   END
174900970526     C*
175000970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
175100991011     C                   READ      BS12D03                                99
175200970526     C*
175300970526     C                   ENDSR
175400970526     C*------------------------------------------------------------------------*
175500970526     C* SNDMSG - MANDA MESSAGGIO
175600970526     C*------------------------------------------------------------------------*
175700970526     C     SNDMSG        BEGSR
175800970526     C*
175900970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
176000970526     C                   MOVEL     '1'           *IN40
176100991011     C                   WRITE     BS12DMC
176200970526     C                   MOVEL     '0'           *IN40
176300970526     C*
176400970526     C* VISUALIZZAZIONE MESSAGGIO
176500970526     C                   Z-ADD     0             MSGULT
176600970526     C                   CALL      'XMSG'        XMSGPA
176700970526     C     DSMSER        IFEQ      0
176800970526     C                   MOVEL     DSMSKE        MSGKEY
176900970526     C                   END
177000970526     C                   Z-ADD     1             MSGULT
177100991011     C                   WRITE     BS12DMS                                      SFL MESS
177200991011     C                   WRITE     BS12DMC                                      CONTR MESS
177300970526     C*
177400970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
177500970526     C                   MOVEL     *BLANKS       DSMSMS
177600970526     C*
177700970526     C                   ENDSR
177800970526     C*------------------------------------------------------------------------*
177900970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
178000970526     C*------------------------------------------------------------------------*
178100970526     C     FUNCON        BEGSR
178200970526     C*
178300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
178400970526     C                   EXSR      FUNVAR
178500970526     C*
178600970526     C* PULISCO CAMPI DI LAVORO
178700970526     C                   Z-ADD     0             I                 5 0
178800970526     C                   Z-ADD     0             W                 5 0
178900970526     C*
179000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
179100970526     C     FUNKEY        IFNE      *BLANKS
179200970526     C     1             DO        24            I
179300970526     C                   MOVEL     FUD(I)        FUDRIG
179400970526     C*
179500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
179600970526     C     FUNKEY        IFEQ      FUDFUN
179700970526     C     FUDATT        ANDEQ     'S'
179800970526     C                   Z-ADD     I             W
179900970526     C                   END
180000970526     C                   END                                                    DO WHILE
180100970526     C*
180200970526     C* FUNZIONE NON ATTIVA (ERRORE)
180300970526     C     W             IFEQ      0
180400970526     C*
180500970526     C* EMISSIONE MESSAGGIO DI ERRORE
180600970526     C                   MOVEL     MSG(2)        DSMSMS
180700970526     C                   END                                                    W=0
180800970526     C                   END                                                    FUNKEY NE *BLANKS
180900970526     C*
181000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
181100970526     C     W             IFNE      0
181200970526     C     FUNKEY        OREQ      *BLANKS
181300970526     C*
181400970526     C     FUNKEY        IFEQ      *BLANKS
181500970526     C                   EXSR      ENTGES
181600970526     C                   END
181700970526     C     FUNKEY        IFEQ      '03'
181800970526     C                   EXSR      F03GES
181900970526     C                   END
182000970526     C     FUNKEY        IFEQ      '05'
182100970526     C                   EXSR      F05GES
182200970526     C                   END
182300970526     C     FUNKEY        IFEQ      '06'
182400970526     C                   EXSR      F06GES
182500970526     C                   END
182600970526     C     FUNKEY        IFEQ      '12'
182700970526     C                   EXSR      F12GES
182800970526     C                   END
182900970528     C     FUNKEY        IFEQ      '21'
183000970528     C                   EXSR      F21GES
183100970528     C                   END
183200970528     C     FUNKEY        IFEQ      '22'
183300970528     C                   EXSR      F22GES
183400970528     C                   END
183500970526     C     FUNKEY        IFEQ      '23'
183600970526     C                   EXSR      F23GES
183700970526     C                   END
183800970526     C     FUNKEY        IFEQ      '24'
183900970526     C                   EXSR      F24GES
184000970526     C                   END
184100970526     C     FUNKEY        IFEQ      'RU'
184200970526     C                   EXSR      RUPGES
184300970526     C                   END
184400970526     C*
184500970526     C                   END
184600970526     C*
184700970526     C                   ENDSR
184800970526     C*------------------------------------------------------------------------*
184900970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
185000970526     C*------------------------------------------------------------------------*
185100970526     C     FUNVAR        BEGSR
185200970526     C*
185300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
185400970526     C     KEY           IFEQ      ENTER
185500970526     C                   MOVEL     *BLANKS       FUNKEY
185600970526     C                   END
185700970526     C     KEY           IFEQ      F1
185800970526     C                   MOVEL     '01'          FUNKEY
185900970526     C                   END
186000970526     C     KEY           IFEQ      F2
186100970526     C                   MOVEL     '02'          FUNKEY
186200970526     C                   END
186300970526     C     KEY           IFEQ      F3
186400970526     C                   MOVEL     '03'          FUNKEY
186500970526     C                   END
186600970526     C     KEY           IFEQ      F4
186700970526     C                   MOVEL     '04'          FUNKEY
186800970526     C                   END
186900970526     C     KEY           IFEQ      F5
187000970526     C                   MOVEL     '05'          FUNKEY
187100970526     C                   END
187200970526     C     KEY           IFEQ      F6
187300970526     C                   MOVEL     '06'          FUNKEY
187400970526     C                   END
187500970526     C     KEY           IFEQ      F7
187600970526     C                   MOVEL     '07'          FUNKEY
187700970526     C                   END
187800970526     C     KEY           IFEQ      F8
187900970526     C                   MOVEL     '08'          FUNKEY
188000970526     C                   END
188100970526     C     KEY           IFEQ      F9
188200970526     C                   MOVEL     '09'          FUNKEY
188300970526     C                   END
188400970526     C     KEY           IFEQ      F10
188500970526     C                   MOVEL     '10'          FUNKEY
188600970526     C                   END
188700970526     C     KEY           IFEQ      F11
188800970526     C                   MOVEL     '11'          FUNKEY
188900970526     C                   END
189000970526     C     KEY           IFEQ      F12
189100970526     C                   MOVEL     '12'          FUNKEY
189200970526     C                   END
189300970526     C     KEY           IFEQ      F13
189400970526     C                   MOVEL     '13'          FUNKEY
189500970526     C                   END
189600970526     C     KEY           IFEQ      F14
189700970526     C                   MOVEL     '14'          FUNKEY
189800970526     C                   END
189900970526     C     KEY           IFEQ      F15
190000970526     C                   MOVEL     '15'          FUNKEY
190100970526     C                   END
190200970526     C     KEY           IFEQ      F16
190300970526     C                   MOVEL     '16'          FUNKEY
190400970526     C                   END
190500970526     C     KEY           IFEQ      F17
190600970526     C                   MOVEL     '17'          FUNKEY
190700970526     C                   END
190800970526     C     KEY           IFEQ      F18
190900970526     C                   MOVEL     '18'          FUNKEY
191000970526     C                   END
191100970526     C     KEY           IFEQ      F19
191200970526     C                   MOVEL     '19'          FUNKEY
191300970526     C                   END
191400970526     C     KEY           IFEQ      F20
191500970526     C                   MOVEL     '20'          FUNKEY
191600970526     C                   END
191700970526     C     KEY           IFEQ      F21
191800970526     C                   MOVEL     '21'          FUNKEY
191900970526     C                   END
192000970526     C     KEY           IFEQ      F22
192100970526     C                   MOVEL     '22'          FUNKEY
192200970526     C                   END
192300970526     C     KEY           IFEQ      F23
192400970526     C                   MOVEL     '23'          FUNKEY
192500970526     C                   END
192600970526     C     KEY           IFEQ      F24
192700970526     C                   MOVEL     '24'          FUNKEY
192800970526     C                   END
192900970526     C     KEY           IFEQ      ROLLUP
193000970526     C                   MOVEL     'RU'          FUNKEY
193100970526     C                   END
193200970526     C     KEY           IFEQ      HELP
193300970526     C                   MOVEL     'HP'          FUNKEY
193400970526     C                   END
193500970526     C*
193600970526     C                   ENDSR
193700970526     C*------------------------------------------------------------------------*
193800970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
193900970526     C*------------------------------------------------------------------------*
194000970526     C     FUNGES        BEGSR
194100970526     C*
194200970526     C* PULISCO CAMPI DI LAVORO
194300970526     C                   Z-ADD     0             W                 5 0
194400970526     C*
194500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
194600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
194700970526     C     1             DO        24            I
194800970526     C                   MOVEL     FUD(I)        FUDRIG
194900970526     C*
195000970526     C* PULIZIA FUNZIONI PRECEDENTI
195100970526     C                   MOVEL     *BLANKS       FUV(I)
195200970526     C*
195300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
195400970526     C* (SALTA F23 E F24)
195500970526     C     FUDATT        IFEQ      'S'
195600970526     C     FUDFUN        ANDNE     '23'
195700970526     C     FUDFUN        ANDNE     '24'
195800970526     C                   ADD       1             W
195900970526     C                   MOVEL     FUDDES        FUV(W)
196000970526     C                   END
196100970526     C                   END                                                    DO WHILE
196200970526     C*
196300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
196400970526     C* ALTRIMENTI DISATTIVO F24
196500970526     C     1             DO        24            I
196600970526     C                   MOVEL     FUD(I)        FUDRIG
196700970526     C*
196800970526     C     FUDFUN        IFEQ      '24'
196900970526     C     W             IFGT      6
197000970526     C                   MOVEL     'S'           FUDATT
197100970526     C                   ELSE
197200970526     C                   MOVEL     *BLANKS       FUDATT
197300970526     C                   END                                                    W GT 8
197400970526     C                   MOVEL     FUDRIG        FUD(I)
197500970526     C                   END
197600970526     C                   END                                                    DO WHILE
197700970526     C*
197800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
197900970526     C     W             DIV       6             I
198000970526     C                   MVR                     Z
198100970526     C*
198200970526     C     Z             IFNE      0
198300970526     C     I             ADD       1             Z
198400970526     C                   ELSE
198500970526     C                   Z-ADD     I             Z
198600970526     C                   END
198700970526     C*
198800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
198900970526     C                   ADD       1             FUNGRU
199000970526     C     FUNGRU        IFGT      Z
199100970526     C     FUNGRU        ORLT      1
199200970526     C                   Z-ADD     1             FUNGRU
199300970526     C                   END
199400970526     C*
199500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
199600970526     C     FUNGRU        IFEQ      1
199700970526     C                   MOVEL     FUV(1)        FUN001
199800970526     C                   MOVEL     FUV(2)        FUN002
199900970526     C                   MOVEL     FUV(3)        FUN003
200000970526     C                   MOVEL     FUV(4)        FUN004
200100970526     C                   MOVEL     FUV(5)        FUN005
200200970526     C                   MOVEL     FUV(6)        FUN006
200300970526     C                   END
200400970526     C*
200500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
200600970526     C     FUNGRU        IFEQ      2
200700970526     C                   MOVEL     FUV(7)        FUN001
200800970526     C                   MOVEL     FUV(8)        FUN002
200900970526     C                   MOVEL     FUV(9)        FUN003
201000970526     C                   MOVEL     FUV(10)       FUN004
201100970526     C                   MOVEL     FUV(11)       FUN005
201200970526     C                   MOVEL     FUV(12)       FUN006
201300970526     C                   END
201400970526     C*
201500970526     C* TERZO GRUPPO DI 6 FUNZIONI
201600970526     C     FUNGRU        IFEQ      3
201700970526     C                   MOVEL     FUV(13)       FUN001
201800970526     C                   MOVEL     FUV(14)       FUN002
201900970526     C                   MOVEL     FUV(15)       FUN003
202000970526     C                   MOVEL     FUV(16)       FUN004
202100970526     C                   MOVEL     FUV(17)       FUN005
202200970526     C                   MOVEL     FUV(18)       FUN006
202300970526     C                   END
202400970526     C*
202500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
202600970526     C     FUNGRU        IFEQ      4
202700970526     C                   MOVEL     FUV(19)       FUN001
202800970526     C                   MOVEL     FUV(20)       FUN002
202900970526     C                   MOVEL     FUV(21)       FUN003
203000970526     C                   MOVEL     FUV(22)       FUN004
203100970526     C                   MOVEL     *BLANKS       FUN005
203200970526     C                   MOVEL     *BLANKS       FUN006
203300970526     C                   END
203400970526     C*
203500970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
203600970526     C                   MOVEL     *BLANKS       FUN007
203700970526     C                   MOVEL     *BLANKS       FUN008
203800970526     C*
203900970526     C     1             DO        24            I
204000970526     C                   MOVEL     FUD(I)        FUDRIG
204100970526     C     FUDFUN        IFEQ      '23'
204200970526     C     FUDATT        ANDEQ     'S'
204300970526     C                   MOVEL     FUDDES        FUN007
204400970526     C                   END
204500970526     C     FUDFUN        IFEQ      '24'
204600970526     C     FUDATT        ANDEQ     'S'
204700970526     C                   MOVEL     FUDDES        FUN008
204800970526     C                   END
204900970526     C                   END                                                    DO
205000970526     C*
205100970526     C                   ENDSR
205200970526     C*------------------------------------------------------------------------*
205300970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
205400970526     C*------------------------------------------------------------------------*
205500970526     C     OPZGES        BEGSR
205600970526     C*
205700970526     C* PULISCO CAMPI DI LAVORO
205800970526     C                   Z-ADD     0             W                 5 0
205900970526     C*
206000970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
206100970526     C* E CARICO TABELLA OPZIONI ATTIVE
206200970526     C     1             DO        24            I
206300970526     C                   MOVEL     OPD(I)        OPDRIG
206400970526     C*
206500970526     C* PULIZIA OPZIONI PRECEDENTI
206600970526     C                   MOVEL     *BLANKS       OPV(I)
206700970526     C*
206800970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
206900970526     C     OPDATT        IFEQ      'S'
207000970526     C                   ADD       1             W
207100970526     C                   MOVEL     OPDDES        OPV(W)
207200970526     C                   END
207300970526     C                   END                                                    DO WHILE
207400970526     C*
207500970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
207600970526     C* ALTRIMENTI DISATTIVO F23
207700970526     C     1             DO        24            I
207800970526     C                   MOVEL     FUD(I)        FUDRIG
207900970526     C*
208000970526     C     FUDFUN        IFEQ      '23'
208100970526     C     W             IFGT      8
208200970526     C                   MOVEL     'S'           FUDATT
208300970526     C                   ELSE
208400970526     C                   MOVEL     *BLANKS       FUDATT
208500970526     C                   END                                                    W GT 8
208600970526     C                   MOVEL     FUDRIG        FUD(I)
208700970526     C                   END
208800970526     C                   END                                                    DO WHILE
208900970526     C*
209000970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
209100970526     C     W             DIV       8             I
209200970526     C                   MVR                     Z                 5 0
209300970526     C*
209400970526     C     Z             IFNE      0
209500970526     C     I             ADD       1             Z
209600970526     C                   ELSE
209700970526     C                   Z-ADD     I             Z
209800970526     C                   END
209900970526     C*
210000970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
210100970526     C                   ADD       1             OPZGRU
210200970526     C     OPZGRU        IFGT      Z
210300970526     C     OPZGRU        ORLT      1
210400970526     C                   Z-ADD     1             OPZGRU
210500970526     C                   END
210600970526     C*
210700970526     C* PRIMO GRUPPO DI 8 OPZIONI
210800970526     C     OPZGRU        IFEQ      1
210900970526     C                   MOVEL     OPV(1)        OPZ001
211000970526     C                   MOVEL     OPV(2)        OPZ002
211100970526     C                   MOVEL     OPV(3)        OPZ003
211200970526     C                   MOVEL     OPV(4)        OPZ004
211300970526     C                   MOVEL     OPV(5)        OPZ005
211400970526     C                   MOVEL     OPV(6)        OPZ006
211500970526     C                   MOVEL     OPV(7)        OPZ007
211600970526     C                   MOVEL     OPV(8)        OPZ008
211700970526     C                   END
211800970526     C*
211900970526     C* SECONDO GRUPPO DI 8 OPZIONI
212000970526     C     OPZGRU        IFEQ      2
212100970526     C                   MOVEL     OPV(9)        OPZ001
212200970526     C                   MOVEL     OPV(10)       OPZ002
212300970526     C                   MOVEL     OPV(11)       OPZ003
212400970526     C                   MOVEL     OPV(12)       OPZ004
212500970526     C                   MOVEL     OPV(13)       OPZ005
212600970526     C                   MOVEL     OPV(14)       OPZ006
212700970526     C                   MOVEL     OPV(15)       OPZ007
212800970526     C                   MOVEL     OPV(16)       OPZ008
212900970526     C                   END
213000970526     C*
213100970526     C* TERZO GRUPPO DI 8 OPZIONI
213200970526     C     OPZGRU        IFEQ      3
213300970526     C                   MOVEL     OPV(17)       OPZ001
213400970526     C                   MOVEL     OPV(18)       OPZ002
213500970526     C                   MOVEL     OPV(19)       OPZ003
213600970526     C                   MOVEL     OPV(20)       OPZ004
213700970526     C                   MOVEL     OPV(21)       OPZ005
213800970526     C                   MOVEL     OPV(22)       OPZ006
213900970526     C                   MOVEL     OPV(23)       OPZ007
214000970526     C                   MOVEL     OPV(24)       OPZ008
214100970526     C                   END
214200970526     C*
214300970526     C                   ENDSR
214400970526     C*------------------------------------------------------------------------*
214500970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
214600970526     C*------------------------------------------------------------------------*
214700970526     C     OPZCON        BEGSR
214800970526     C*
214900970526     C* PULISCO CAMPI DI LAVORO
215000970526     C                   Z-ADD     0             I                 5 0
215100970526     C                   Z-ADD     0             W                 5 0
215200970526     C*
215300970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
215400970526     C     1             DO        24            I
215500970526     C                   MOVEL     OPD(I)        OPDRIG
215600970526     C*
215700970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
215800970526     C     OPZKEY        IFEQ      OPDOPZ
215900970526     C     OPZKEY        OREQ      OPDOP1
216000970526     C*
216100970526     C     OPDATT        IFEQ      'S'
216200970526     C                   MOVEL     OPDOPZ        OPZKEY
216300970526     C                   Z-ADD     I             W
216400970526     C                   END
216500970526     C                   END
216600970526     C*
216700970526     C                   END                                                    DO WHILE
216800970526     C*
216900970526     C* OPZIONE NON ATTIVA (ERRORE)
217000970526     C     W             IFEQ      0
217100970526     C*
217200970526     C* OPZIONE NON OK
217300970526     C                   Z-ADD     1             OPZOK
217400970526     C*
217500970526     C* EMISSIONE MESSAGGIO DI ERRORE
217600970526     C                   MOVEL     MSG(1)        DSMSMS
217700970526     C                   ELSE
217800970526     C*
217900970526     C* OPZIONE ATTIVA (PROCEDI ...)
218000970526     C*
218100970526     C     OPZKEY        IFEQ      ' 1'
218200970526     C                   EXSR      O01GES
218300970526     C                   END
218400970526     C     OPZKEY        IFEQ      ' 2'
218500970526     C                   EXSR      O02GES
218600970526     C                   END
218700991014     C     OPZKEY        IFEQ      ' 3'
218800991014     C                   EXSR      O03GES
218900991014     C                   END
219000000307     C     OPZKEY        IFEQ      ' 4'
219100000307     C                   EXSR      O04GES
219200000307     C                   END
219300970526     C     OPZKEY        IFEQ      ' 5'
219400970526     C                   EXSR      O05GES
219500970526     C                   END
219600000307     C     OPZKEY        IFEQ      ' 7'
219700000307     C                   EXSR      O07GES
219800000307     C                   END
219900000607     C     OPZKEY        IFEQ      ' 9'
220000000607     C                   EXSR      O09GES
220100000607     C                   END
220200970526     C                   END                                                    OPZ.NON ATTIVA
220300970526     C*
220400970526     C                   ENDSR
220500160704     C/EJECT
220600160704      * ?-------------------------------------------------------------*?
220700160704      *? Riordina comunque il SFL                                     ?
220800160704      *?   Deve mostrare prima il cliente poi il tipo                 ?
220900160704      * ?-------------------------------------------------------------*?
221000160704     C     Ordina_SFL    BEGSR
221100160704     C*
221200160704     C*
221300160704      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
221400160704      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
221500160704      * Il campo è usato per selezionare i records dando un ordine a quelli
221600160704      * selezionati davanti ai non selezionati.
221700160704     C                   CLEAR                   QLGSCB
221800160704     C                   CLEAR                   QLGSCB00
221900160704      *
222000160704      *?  Ordinamento per Padre/Figlio/Tipo/Decorrenza/Scadenza   ?
222100160704     ** 5 campi chiave.
222200160704     C                   EVAL      QLGNBRK    = 5
222300160704
222400160704     ** Il codice Padre
222500160704     C                   EVAL      QLGSP      = 1
222600160704     C                   EVAL      QLGSS      = %SIZE(VIDKOP)
222700160704     C                   EVAL      QLGDT      = Carattere
222800160704     C                   EVAL      QLGSO      = Ascendente
222900160704     C                   EVAL      QLGKL(1)   = QLGSKL
223000160704     ** Il codice Figlio
223100160704     C                   EVAL      QLGSP   = 1 + %SIZE(VIDKOP)
223200160704     C                   EVAL      QLGSS    = %SIZE(VIDCOF)
223300160704     C                   EVAL      QLGDT    = Carattere
223400160704     C                   EVAL      QLGSO    = Ascendente
223500160704     C                   EVAL      QLGKL(2) = QLGSKL
223600160704     ** Il Tipo legame
223700160704     C                   EVAL      QLGSP   = 1 + %SIZE(VIDKOP) + %SIZE(VIDCOF)
223800160704     C                   EVAL      QLGSS    = %SIZE(VIDTLE)
223900160704     C                   EVAL      QLGDT    = Carattere
224000160704     C                   EVAL      QLGSO    = Ascendente
224100160704     C                   EVAL      QLGKL(3) = QLGSKL
224200160704     ** Data Decorrenza
224300160704     C                   EVAL      QLGSP   = 1 + %SIZE(VIDKOP) + %SIZE(VIDCOF)
224400160704     c                                         + %SIZE(VIDTLE)
224500160704     C                   EVAL      QLGSS    = %SIZE(VIHDDE)
224600160704     C                   EVAL      QLGDT    = Carattere
224700160704     C                   EVAL      QLGSO    = Ascendente
224800160704     C                   EVAL      QLGKL(4) = QLGSKL
224900160704     ** Data Scadenza
225000160704     C                   EVAL      QLGSP   = 1 + %SIZE(VIDKOP) + %SIZE(VIDCOF)
225100160704     c                                         + %SIZE(VIDTLE) + %SIZE(VIHDDE)
225200160704     C                   EVAL      QLGSS    = %SIZE(VIHDSC)
225300160704     C                   EVAL      QLGDT    = Carattere
225400160704     C                   EVAL      QLGSO    = Ascendente
225500160704     C                   EVAL      QLGKL(5) = QLGSKL
225600160704      *------------______________________________________________------------
225700160704      * Load other sort parameters.
225800160704     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
225900160704     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
226000160704     C                   EVAL      QLGRT     = 8
226100160704     C                   EVAL      QLGOKL    = 80
226200160704     C                   EVAL      QLGLKE    = 16
226300160704     C                   EVAL      QLGLSS    = 290
226400160704
226500160704      * Initialize Sort I/O API fields.
226600160704     C                   EVAL      QLGRL00  = QLGRL
226700160704     C                   EVAL      QLGRC00  = 1
226800160704     C                   CLEAR                   QUSEI
226900160704     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
227000160704
227100160704      * First step - Initialize the sort routine.
227200160704     C                   CALL      'QLGSORT'
227300160704     C                   PARM                    QLGSCB
227400160704     C                   PARM                    NotUsed
227500160704     C                   PARM                    NotUsed
227600160704     C                   PARM                    SizeList
227700160704     C                   PARM                    ReturnSize
227800160704     C                   PARM                    QUSEC
227900160704
228000160704      * Next step - Write records to I/O routine.
228100160704     C                   EVAL      QLGRT00 = Put
228200160704
228300160704     C                   DO        RrnLast       S2nrr_xx          5 0
228400160704     C     S2nrr_xx      CHAIN     BS12D05
228500160704
228600160704     ** Solo le righe con Selected = 'Y' sono riordinate,
228700160704     ** quindi per fare un ordinamento di tutte le righe
228800160704     ** metto 'Y' sempre.
228900160704     C                   EVAL      Selected  = 'Y'
229000160704
229100160704     C                   CLEAR                   QUSEI
229200160704     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
229300160704      *
229400160704     C                   CALL      'QLGSRTIO'
229500160704     C                   PARM                    QLGSCB00
229600160704     C                   PARM                    SflRcd
229700160704     C                   PARM                    NotUsed
229800160704     C                   PARM                    SizeList
229900160704     C                   PARM                    NotUsed
230000160704     C                   PARM                    QUSEC
230100160704
230200160704     C                   ENDDO
230300160704
230400160704      * Next step - Signal end of input, clear subfile for reload.
230500160704     C                   EVAL      QLGRT00 = EndPut
230600160704     C                   CLEAR                   QUSEI
230700160704     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
230800160704
230900160704     C                   CALL      'QLGSRTIO'
231000160704     C                   PARM                    QLGSCB00
231100160704     C                   PARM                    SflRcd
231200160704     C                   PARM                    NotUsed
231300160704     C                   PARM                    SizeList
231400160704     C                   PARM                    NotUsed
231500160704     C                   PARM                    QUSEC
231600160704      *
231700160704     C* ACCENSIONE INDICATORE PER CLEAR SFL
231800160704     C                   SETON                                          40
231900160704     C                   WRITE     BS12D06
232000160704     C                   SETOff                                         40
232100160704
232200160704      * Final step - Write the records back to the subfile.
232300160704     C                   EVAL      QLGRT00 = Get
232400160704
232500160704     C                   DO        RrnLast       S2nrr_xx
232600160704     c                   eval      SflULT = S2nrr_xx
232700160704     C                   CLEAR                   QUSEI
232800160704     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
232900160704     C                   CALL      'QLGSRTIO'
233000160704     C                   PARM                    QLGSCB00
233100160704     C                   PARM                    NotUsed
233200160704     C                   PARM                    SflRcd
233300160704     C                   PARM                    QLGRL00
233400160704     C                   PARM                    NotUsed
233500160704     C                   PARM                    QUSEC
233600160704      * SFLnxtCHG
233700160704     c                   if        opzkey <> *blank
233800160704     c                   seton                                        41
233900160704     c                   else
234000160704     C                   SetOFF                                       41
234100160704     c                   end
234200160704      *
234300160704     c                   eval      *in75 = Hin75
234400160704     c                   eval      *in76 = Hin76
234500160704     C                   WRITE     BS12D05
234600160704     C                   ENDDO
234700160704      *
234800160704      *  All'uscita di queste chiamate a routine di sistema il SFL record
234900160704      *   si incrementa stranamente quindi lo reimposto correttamente
235000160704      *    prima di lasciare la routine.
235100160704     C                   EVAL      SflULT= RrnLast
235200160704     C*
235300160704     C                   ENDSR
235400160704     C/EJECT
235500160704     C*-----------------------------------------------------*
235600970526     C*------------------------------------------------------------------------*
235700970526     C* NOTE COSTRUZIONE SCHIERE
235800970526     C*------------------------------------------------------------------------*
235900970526     C*
236000970526     C* -MSG-MESSAGGI
236100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
236200970526     C* (NON TOCCARE MSG,1 E MSG,2)
236300970526     C*
236400970526     C* -CMD-COMANDI
236500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
236600970526     C*
236700970526     C* -OPD-OPZIONI
236800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
236900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
237000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
237100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
237200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
237300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
237400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
237500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
237600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
237700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
237800970526     C*
237900970526     C* -FUD-FUNZIONI
238000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
238100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
238200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
238300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
238400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
238500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
238600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
238700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
238800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
238900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
239000970526     O*------------------------------------------------------------------------*
239100970526** DEC - DECODIFICHE
239200970526    GESTIONE                                                     1
239300970526INTERROGAZIONE                                                   2
239400970530     SCELTA                                                      3
2395009705264
2396009705265
239700970526** MSG - MESSAGGI
239800991001TIBS12R- Opzione non gestita                                                   1
239900991001TIBS12R- Tasto funzionale non gestito                                          2
240000000307TIBS12R-                                                                       3
240100991001TIBS12R- Record annullato. Consentito solo il ripristino                       4
240200991001TIBS12R- Record non annullato. Impossibile ripristinare                        5
240300991001TIBS12R- Accettato solo un parametro di inizio lista                           6
240400060113TIBS12R- Errore in caricamento dati utente, avvisare il CED                    7
240500991001TIBS12R- 8                                                                     8
240600991001TIBS12R- 9                                                                     9
240700991001TIBS12R- 10                                                                   10
240800000307TIBS12R- Tipo legame errato.                                                  11
240900000307TIBS12R- Data errata.                                                         12
241000000807TIBS12R- Tipo legame non autorizzato.                                         13
241100060113TIBS12R- Utente non abilitato alla gestione del cliente "Padre" scelto.       14
241200170905TIBS12R- Utente non abilitato alla gestione Logistica                         15
241300991001TIBS12R- 16                                                                   16
241400991001TIBS12R- 17                                                                   17
241500991001TIBS12R- 18                                                                   18
241600991001TIBS12R- 19                                                                   19
241700991001TIBS12R- 20                                                                   20
241800970528** CMD - COMANDI CL
241900970528WRKSPLF
242000970528
242100970528
242200970528
242300970528
242400970526** OPD - OPZIONI
242500970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
242600970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
242700991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
242800000307 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
242900970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
243000970526                   -6 -     (S/N)=N                        opz. 06¬
243100000307 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
243200970527                   -8 -     (S/N)=N                        opz. 08¬
243300000607 9=Cliente abili.  -9 -     (S/N)=N                        opz. 09¬
243400970527                   -10-     (S/N)=N                        opz. 10¬
243500970526                   -  -     (S/N)=N                        opz. 11¬
243600970526                   -  -     (S/N)=N                        opz. 12¬
243700970526                   -  -     (S/N)=N                        opz. 13¬
243800970526                   -  -     (S/N)=N                        opz. 14¬
243900970526                   -  -     (S/N)=N                        opz. 15¬
244000970526                   -  -     (S/N)=N                        opz. 16¬
244100970526                   -  -     (S/N)=N                        opz. 17¬
244200970526                   -  -     (S/N)=N                        opz. 18¬
244300970526                   -  -     (S/N)=N                        opz. 19¬
244400970526                   -  -     (S/N)=N                        opz. 20¬
244500970526                   -  -     (S/N)=N                        opz. 21¬
244600970526                   -  -     (S/N)=N                        opz. 22¬
244700970526                   -  -     (S/N)=N                        opz. 23¬
244800970526                   -  -     (S/N)=N                        opz. 24¬
244900970526** FUD - FUNZIONI
245000970526                   -        (S/N)=N                        funz.01¬
245100970526                   -        (S/N)=N                        funz.02¬
245200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
245300970526                   -        (S/N)=N                        funz.04¬
245400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
245500970526F06=Inserimento    -        (S/N)=N                        funz.06¬
245600970526                   -        (S/N)=N                        funz.07¬
245700970527                   -        (S/N)=N                        funz.08¬
245800970527                   -        (S/N)=N                        funz.09¬
245900970527                   -        (S/N)=N                        funz.10¬
246000970526                   -        (S/N)=N                        funz.11¬
246100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
246200970526                   -        (S/N)=N                        funz.13¬
246300970526                   -        (S/N)=N                        funz.14¬
246400970526                   -        (S/N)=N                        funz.15¬
246500970526                   -        (S/N)=N                        funz.16¬
246600970526                   -        (S/N)=N                        funz.17¬
246700970526                   -        (S/N)=N                        funz.18¬
246800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
246900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
247000000808F21=Stampa elenco  -        (S/N)=N                        funz.21¬
247100970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
247200970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
247300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
