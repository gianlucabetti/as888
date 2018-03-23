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
061000000307     C                   ENDSR
061100000307     C*------------------------------------------------------------------------*
061200000307     C* MOVCAMP - VALORIZZAZIONE CAMPI
061300000307     C*------------------------------------------------------------------------*
061400000307     C     MOVCAMP       BEGSR
061500000307     C*
061600991001     C                   MOVEL     VIDtle        Dtle
061700991001     C                   MOVEL     VIDazp        Dazp
061800991001     C                   Z-ADD     VIDkop        Dcop
061900991001     C                   MOVEL     VIDazf        Dazf
062000991001     C                   Z-ADD     VIDcof        Dcof
062100991011     C                   Z-ADD     VIhDDE        Ddde
062200991011     C                   Z-ADD     VIHDsc        Ddsc
062300000307     C*
062400000307     C                   ENDSR
062500970526     C*------------------------------------------------------------------------*
062600970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
062700970526     C*------------------------------------------------------------------------*
062800970526     C     NEWPOS        BEGSR
062900970526     C*
063000970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
063100970526     C*         1= RICHIESTO POSIZIONAMENTO
063200970526     C                   Z-ADD     0             SWPOSI            7 0
063300970526     C*
063400970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
063500970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
063600970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
063700970526     C                   Z-ADD     0             SWMODI            1 0
063800970530     C*
063900970530     C* REIMPOSTA INDICATORI DI ERRORE
064000970530     C                   SETOFF                                       808182
064100000215     C                   SETOFF                                       838485
064200000215     C                   SETOFF                                       86
064300970526     C*
064400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
064500021114     C                   IF        VIDtl2 <> DEPtl2 OR
064600021114     C                             VIDuni <> DEPuni OR
064700021114     C                             VIDele <> DEPele OR
064800021114     C                             VIDel2 <> DEPel2 OR
064900021114     C                             VIDun2 <> DEPun2 OR
065000021114     C                             VIDpdd <> DEPpdd OR
065100021114     C                             %SCAN('?':VIDUNI) > 0 OR
065200021114     C                             %SCAN('?':VIDUN2) > 0 OR
065300021114     C                             %SCAN('?':VIDELE) > 0 OR
065400021114     C                             %SCAN('?':VIDEL2) > 0
065500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
065600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
065700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
065800000308     C                   movel     VIDtl2        DEPtl2                         *DEPOSITI
065900991011     C                   move      VIDuni        DEPuni
066000991011     C                   move      VIDele        DEPele
066100000308     C                   move      VIDpdd        DEPpdd                         *DEPOSITI
066200991013     C                   move      VIDun2        DEPun2
066300991013     C                   move      VIDel2        DEPel2
066400970526E   2C                   ENDIF
066500970526E   1C                   ENDIF
066600970526     C*
066700970526     C                   ENDSR
066800970526     C*------------------------------------------------------------------------*
066900970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
067000970526     C*------------------------------------------------------------------------*
067100970528     C     SETFIL        BEGSR
067200970526     C*
067300970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
067400000731     C                   MOVEL     *blanks       KCEtle
067500000731     C                   MOVEL     *blanks       KCEazp
067600000731     C                   MOVEL     *blanks       KCEazf
067700000731     C                   MOVEL(p)  PDDvid        KCEdde
067800000731     C                   MOVEL(p)  VIDuni        KCEcop
067900000731     C                   MOVEL(p)  VIDele        KCEcof
068000000731IF  1C     WTIPRI        IFEQ      'UNI'
068100160704     C**** KE1kun        SETLL     tikun01l                           99
068200160704     C     KE11kun       SETLL     tikun11l                           99
068300970528E   1C                   ENDIF
068400970528     C*
068500000731IF  1C     WTIPRI        IFEQ      'ELE'
068600000731     C     KE2kun        SETLL     tikun02L                           99
068700970528E   1C                   ENDIF
068800000731     C*
068900000731IF  1C     WTIPRI        IFEQ      'DAU'
069000000731     C     KE4kun        SETLL     tikun04l                           99
069100000731E   1C                   ENDIF
069200000731     C*
069300000731IF  1C     WTIPRI        IFEQ      'DAE'
069400000731     C     KE5kun        SETLL     tikun05L                           99
069500000731E   1C                   ENDIF
069600980107     C*
069700970526     C* LETTURA RECORD
069800970526IF  1C     *IN99         IFEQ      *OFF
069900970526     C                   EXSR      LETFIL
070000970526E   1C                   ENDIF
070100970526     C*
070200970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
070300970526IF  1C     *IN99         IFEQ      *ON
070400970526     C                   Z-ADD     1             SFLEOF
070500970526E   1C                   ENDIF
070600970526     C*
070700970526     C                   ENDSR
070800970526     C*------------------------------------------------------------------------*
070900970526     C* LETFIL - LETTURA RECORD
071000970526     C*------------------------------------------------------------------------*
071100970526     C     LETFIL        BEGSR
071200970526     C*
071300160701     c                   clear                   NRRECHID          9 0
071400970526     C* LEGGE FINO A:
071500970526     C*     - FINE FILE
071600970526     C*     - TROVATO RECORD VALIDO
071700970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
071800970526DO  1C     *IN99         DOWEQ     *OFF
071900970526     C     WRECOK        ANDEQ     'N'
072000970526     C*
072100970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
072200000731IF  2C     WTIPRI        IFEQ      'UNI'
072300160704     C******             READ      tikun01l                               99
072400160704     C                   READ      tikun11l                               99
072500160704     c**n99              z-add     K01_nrr       NRRECHID
072600160704     c  n99              z-add     K11_nrr       NRRECHID
072700970528E   2C                   ENDIF
072800970526     C*
072900000731IF  2C     WTIPRI        IFEQ      'ELE'
073000000731     C                   READ      tikun02L                               99
073100160701     c  n99              z-add     K02_nrr       NRRECHID
073200970528E   2C                   ENDIF
073300000731     C*
073400000731IF  2C     WTIPRI        IFEQ      'DAU'
073500000731     C                   READ      tikun04l                               99
073600160701     c  n99              z-add     K04_nrr       NRRECHID
073700000731E   2C                   ENDIF
073800000731     C*
073900000731IF  2C     WTIPRI        IFEQ      'DAE'
074000000731     C                   READ      tikun05L                               99
074100160701     c  n99              z-add     K05_nrr       NRRECHID
074200000731E   2C                   ENDIF
074300970528     C*
074400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
074500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
074600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
074700970530X   2C                   ELSE
074800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
074900970530E   2C                   ENDIF
075000970526E   1C                   ENDDO
075100970526     C*
075200160701DO  1C                   if        *in99 or WRECOK ='N'
075300160701     c                   clear                   NRRECHID
075400160701E   2C                   ENDIF
075500160701     C*
075600970526     C                   ENDSR
075700970526     C*------------------------------------------------------------------------*
075800970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
075900970526     C*------------------------------------------------------------------------*
076000970526     C     CHKREC        BEGSR
076100970526     C*
076200970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
076300100322     C                   setoff                                       7675
076400970530     C*
076500000731     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
076600000731     C*
076700000731     C     WRECOK        ifeq      'S'
076800000731     C     kundde        iflt      PDDvid
076900000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
077000000731     C                   endif
077100000731     C                   endif
077200000731     C*
077300000731     C     WRECOK        ifeq      'S'
077400000731     C                   move(p)   VIDuni        uninum           11 0
077500000731     C     kuncop        iflt      uninum
077600000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
077700000731     C                   endif
077800000731     C                   endif
077900000731     C*
078000000731     C     WRECOK        ifeq      'S'
078100000731     C                   move(p)   VIDele        elenum           11 0
078200000731     C     kuncof        iflt      elenum
078300000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
078400000731     C                   endif
078500000731     C                   endif
078600000731     C*
078700000731     C     WRECOK        ifeq      'S'
078800000731     C     VIDtl2        ifne      *blanks
078900000731     C     kuntle        ifne      VIDtl2
079000000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
079100000731     C                   endif
079200000731     C                   endif
079300000731     C                   endif
079400000731     C*
079500000731     C     WRECOK        ifeq      'S'
079600000731     C                   move      kuncop        alfa11           11
079700000731     C     VIDun2        ifeq      *zeros
079800000731     C     VIDun2        oreq      *blank
079900000731     C                   else
080000000731     C     VIDun2        ifne      alfa11
080100000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
080200000731     C                   endif
080300000731     C                   endif
080400000731     C                   endif
080500000731     C*
080600000731     C     WRECOK        ifeq      'S'
080700000731     C                   move      kuncof        alfa11
080800000731     C     VIDel2        ifeq      *zeros
080900000731     C     VIDel2        oreq      *blank
081000000731     C                   else
081100000731     C     VIDel2        ifne      alfa11
081200000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
081300000731     C                   endif
081400000731     C                   endif
081500000731     C                   endif
081600020212     C*
081700020927     C* Il controllo seguente lo effettuo solo se trattasi di utente di sede in quanto in filiale
081800020927     C* il file TIVSS nn c'è
081900091117     C*                  if        simfel = *zeros
082000091117     C*    WRECOK        ifeq      'S'
082100091117     C*                  if        kuntle = 'WW'    and
082200091117     C*                            datcor >= kundde and
082300091117     C*                            datcor <= kundsc
082400091117     C*                  move      kuncof        kssksu
082500091117     C*    keyvss        setll     tivss05l
082600091117     C*    keyvss        reade     tivss05l
082700091117     C*                  dow       not %eof(tivss05l)
082800091117     C*                  if        vssvat = *blanks                              *non annull/sosp.
082900091117     C*                  if        vssdsc < kundde or                            *non valido
083000091117     C*                            vssdde > kundsc
083100091117     C*                  else                                                    *valido
083200091117     C* Verifico che il servizio del record corrente del TIVSS sia soggetto a controllo unificazioni
083300091117     C*    vssisv        lookup    isvkun                                 22
083400091117     C*                  if        *in22 = *on
083500091117     C*                  seton                                        76
083600091117     C*                  leave
083700091117     C*                  endif
083800091117     C*                  endif
083900091117     C*                  endif
084000091117     C*    keyvss        reade     tivss05l
084100091117     C*                  enddo
084200091117     C*                  endif
084300091117     C*                  endif
084400091117     C*                  endif
084500100322     C*
084600100322     C                   if        WRECOK = 'S'
084700100322     C                   if        kundsc < datcor
084800100322     C                   seton                                        75
084900100322     C                   endif
085000100322     C                   endif
085100100322     C*
085200970526     C                   ENDSR
085300970526     C*------------------------------------------------------------------------*
085400970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
085500970526     C*------------------------------------------------------------------------*
085600970526     C     IRESFL        BEGSR
085700970526     C*
085800970526     C                   MOVEL     *BLANKS       OPZKEY
085900991001     C                   Z-ADD     kunDDE        VIHDDE
086000991011     C                   Z-ADD     kunDsc        VIHDsc
086100991001     C                   MOVEL     kuntle        VIDtle
086200991001     C                   MOVEL     kunazp        VIDazp
086300991001     C                   Z-ADD     kuncop        VIDkop
086400991001     C                   MOVEL     kunazf        VIDazf
086500991001     C                   Z-ADD     kuncof        VIDcof
086600991001     C                   move      kunvat        VIDvat
086700991001     C                   Z-ADD     kundde        G08INV
086800970618     C                   Z-ADD     *ZEROS        G08DAT
086900970618     C                   MOVEL     '3'           G08ERR
087000970618     C                   CALL      'XSRDA8'
087100970618     C                   PARM                    WLBDA8
087200970618     C                   Z-ADD     G08DAT        VIDDDE
087300991001     C                   Z-ADD     kunDSC        G08INV
087400970618     C                   Z-ADD     *ZEROS        G08DAT
087500970618     C                   MOVEL     '3'           G08ERR
087600970618     C                   CALL      'XSRDA8'
087700970618     C                   PARM                    WLBDA8
087800970618     C                   Z-ADD     G08DAT        VIDDSC
087900991015     c                   move      vidtle        wvidtl2
088000991015     c                   exsr      cnttle
088100970528     C* DECODIFICHE
088200091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
088300091126      * deciso di non utilizzarli per le unificazioni dei codici
088400091126      * quindi non controllo il tipo codice e do per scontato che è sempre 'C'=cliente
088500991210     C                   move      kuncop        wcli                           -codice
088600991210     c                   exsr      dectib
088700991210     c                   movel     wfvrag        viddep
088800991210     C                   move      kuncof        wcli              7 0          -codice
088900991210     c                   exsr      dectib
089000020905     c                   movel     wfvrag        viddes
089100091126
089200970526     C                   ENDSR
089300991210     C*------------------------------------------------------------------------*
089400991210     C* Dectib - DECODIFICA CLIENTE
089500991210     C*------------------------------------------------------------------------*
089600991210     C     Dectib        BEGSR
089700991210     C*
089800991210     C                   CLEAR                   BS69DS
089900991210     C                   CLEAR                   ACODS
090000991210     C                   MOVEL     KNSIF         I69SIF
090100991210     C                   Z-ADD     WCLI          I69KAC
090200991210     C                   CALL      'TIBS69R'
090300991210     C                   PARM                    BS69DS
090400991210     C                   PARM                    ACODS
090500991210     C                   PARM                    INDDS
090600991210     C                   PARM                    CLPDS
090700991210     C                   PARM                    CLSDS
090800991210     C     O69ERR        IFNE      '1'
090900991210     C                   MOVEL     ACORAG        WFVRAG           40
091000991210     C                   ELSE
091100991210     C                   MOVEL     *ALL'*'       WFVRAG
091200991210     c                   movel     '1'           errl00
091300991210     C                   ENDIF
091400991210     C*
091500991210     C                   ENDSR
091600970526     C*------------------------------------------------------------------------*
091700970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
091800970526     C*------------------------------------------------------------------------*
091900970526     C     CHKPAR        BEGSR
092000970528     C*
092100970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
092200140909     C***                MOVEL     'DAU'         WTIPRI            3
092300140909     C                   MOVEL     'UNI'         WTIPRI            3
092400970528     C*
092500000731     C* CONTROLLA LA DATA DECORRENZA
092600000731     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
092700970528     C                   Z-ADD     *ZEROS        PDDVID
092800970528IF  1C     ERRL00        IFEQ      '0'
092900970528IF  2C     VIDPDD        IFGT      *ZEROS
093000970528     C                   Z-ADD     VIDPDD        G08DAT
093100970528     C                   Z-ADD     *ZEROS        G08INV
093200970528     C                   MOVEL     '0'           G08ERR
093300970528     C                   CALL      'XSRDA8'
093400970528     C                   PARM                    WLBDA8
093500970528     C                   Z-ADD     G08DAT        VIDPDD
093600970528     C                   Z-ADD     G08INV        PDDVID
093700970528IF  3C     G08ERR        IFEQ      '1'
093800970528     C                   MOVEL     '1'           ERRL00
093900970528     C                   MOVEL     MSG(12)       DSMSMS
094000970528     C                   SETON                                        83
094100970528E   3C                   ENDIF
094200970528E   2C                   ENDIF
094300970528E   1C                   ENDIF
094400970528     C*
094500000731     C* CONTROLLA IL TIPO RICERCA
094600000731     C                   move      *blank        wtlecu
094700000731     C                   move      *blank        wtlece
094800000731IF  1C     ERRL00        IFEQ      '0'
094900000731IF  2C     VIDtl2        IFNE      *blank
095000000731     C                   move      vidtl2        wvidtl2           2
095100000731     C                   exsr      cnttle
095200000731     C                   move      wvidtl2       vidtl2
095300000731E   2C                   ENDIF
095400000731E   1C                   ENDIF
095500000731     C*
095600000731     C     ERRL00        IFEQ      '0'
095700991012IF  2C     VIDuni        IFNE      *ZEROS
095800991012IF  2C     VIDuni        andne     *blank
095900021114     C                   MOVEL     'UNI'         WTIPRI
096000991015     c                   move      wtlecu        clipot            1
096100991012     C                   MOVEL     VIDuni        Wcop
096200991012     C                   EXSR      CHKcop
096300991012     C                   MOVEL     Wcop          VIDuni
096400021114     C     PDDvid        IFNE      *zeros
096500021114     C                   MOVEL     'DAU'         WTIPRI
096600021114     C                   ENDIF
096700000731E   2C                   ELSE
096800980107     C*
096900000731IF  1C     VIDele        IFNE      *ZEROS
097000991012IF  2C     VIDele        andne     *blank
097100991001     C                   MOVEL     'ELE'         WTIPRI
097200991015     c                   movel     wtlece        clipot
097300991012     C                   MOVEL     VIDele        Wcop
097400991012     C                   EXSR      CHKcop
097500991012     C                   MOVEL     Wcop          VIDele
097600000731     C     PDDvid        IFNE      *zeros
097700000731     C                   MOVEL     'DAE'         WTIPRI
097800000731     C                   ENDIF
097900980107E   2C                   ENDIF
098000000731E   1C                   ENDIF
098100000731     C                   ENDIF
098200021114     C*
098300021114     C     ERRL00        IFEQ      '0'
098400021114IF  2C     VIDun2        IFNE      *ZEROS
098500021114IF  2C     VIDun2        andne     *blank
098600021114     C                   move      wtlecu        clipot
098700021114     C                   MOVEL     VIDun2        Wcop
098800021114     C                   EXSR      CHKcop
098900021114     C                   MOVEL     Wcop          VIDun2
099000021114E   2C                   ELSE
099100021114     C*
099200021114IF  1C     VIDel2        IFNE      *ZEROS
099300021114IF  2C     VIDel2        andne     *blank
099400021114     c                   movel     wtlece        clipot
099500021114     C                   MOVEL     VIDel2        Wcop
099600021114     C                   EXSR      CHKcop
099700021114     C                   MOVEL     Wcop          VIDel2
099800021114E   2C                   ENDIF
099900021114E   1C                   ENDIF
100000021114     C                   ENDIF
100100000731     C*
100200970526     C                   ENDSR
100300991012     C*------------------------------------------------------------------------*
100400991012     C* Cnttle - CONTROLLA codice tipo legame
100500991012     C*------------------------------------------------------------------------*
100600000731     C     cnttle        begsr
100700000731     C                   clear                   TIBS02DS
100800000731     C*
100900991015     C     '?'           SCAN      wVidtl2                                30
101000000731     C*  Ricerca
101100000731     C                   eval      t02tla = 'L'
101200000731    1C                   IF        *IN30 = *ON
101300000731     C                   movel     'R'           t02mod
101400000731     C                   else
101500000731     C                   movel     'C'           t02mod
101600000731     C                   movel(p)  Wvidtl2       t02ke1
101700000731     C                   end
101800000731     C                   movel     'TLE'         t02cod
101900000731     C                   movel     knsif         t02sif
102000991012     C                   CALL      'TIBS02R'
102100000731     C                   parm                    KPJBA
102200000731     C                   parm                    TIBS02DS
102300000731     C
102400000731     C*
102500991012    2C                   IF        T02err = *blanks
102600991015     C                   movel     T02ke1        wVidtl2
102700991015     C                   movel     T02uni        dstle
102800991015     C                   movel     §tlecu        Wtlecu            1
102900991015     C                   movel     §tlece        wtlece            1
103000000731     C                   else
103100000731     C                   movel     '1'           errl00
103200000731     C                   seton                                        86
103300000215     C                   MOVEL     MSG(11)       DSMSMS
103400991015     C                   ENDIF
103500000731     C
103600000731     C                   endsr
103700991012     C*------------------------------------------------------------------------*
103800991012     C* CHKcop - CONTROLLA codice padre figlio
103900991012     C*------------------------------------------------------------------------*
104000991012     C     CHKcop        BEGSR
104100991012     C*
104200991012     C     *LIKE         DEFINE    VIDuni        Wcop
104300991012     C*
104400991012     C* RICHIESTO ELENCO
104500991012     C                   SETOFF                                           28
104600991012     C     '?'           SCAN      Wcop                                   28
104700991012IF  1C     *IN28         IFEQ      *ON
104800991012     C* RICHIESTO ELENCO CODICI
104900091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
105000091126      * deciso di non utilizzarli per le unificazioni dei codici
105100091126      * quindi non controllo il tipo codice e do per scontato che è sempre cliente
105200991012     C                   CLEAR                   DSXC
105300991012     C                   Z-ADD     KCCCL         DXCCAP
105400991012     C                   CALL      'XCLIR'
105500991012     C                   PARM                    DSXC
105600991012     C* SCOMPATTA
105700991012     C                   MOVEA     DXCKSC        KSA                        ALF
105800991012DO  2C     1             DO        30            J                 3 0
105900991012     C                   MOVE      KSA(J)        DSKSA                      F
106000991012     C                   Z-ADD     DSKSN         KSC(J)                     M
106100991012E   2C                   ENDDO
106200991012     C                   MOVEL     KSC(1)        WKSC              7
106300991012     C                   MOVEL     '1'           $TESVD            1        TA
106400991012E   1C                   ENDIF
106500991012     C*
106600991012     C* CODICE CLIENTE NUMERICO
106700991012IF  1C     WKSC          IFLT      *ZEROS
106800991012     C                   MOVEL     '1'           WERR              1
106900991012X   1C                   ELSE
107000991012     C*
107100991012     C* SOLO CARATTERI NUMERICI
107200991012     c   28              move      *all'0'       wcop
107300991012     c   28              move      wksc          wcop
107400991012     C                   SETOFF                                       28
107500991012     C                   TESTN                   Wcop                 28
107600991012IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
107700991012     C                   MOVEL     '1'           ERRL00
107800991012X   1C                   ELSE                                                   *E'NUMERO DA TESTN
107900991012     C                   MOVE      Wcop          W001              1
108000991012IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
108100991012     C                   MOVEL     '1'           ERRL00
108200991012X   2C                   ELSE                                                   *NUMERO
108300991012     C*
108400991012     C* CONTROLLA ESISTENZA CODICE
108500991210     C                   MOVE      Wcop          wcli
108600991210     C                   exsr      dectib
108700991012E   2C                   ENDIF
108800991012E   1C                   ENDIF
108900991012E   1C                   ENDIF
109000991012     C*
109100991012     C                   ENDSR
109200970526     C*------------------------------------------------------------------------*
109300970526     C* LANPGM - LANCIA UN PROGRAMMA
109400970526     C*------------------------------------------------------------------------*
109500970526     C     LANPGM        BEGSR
109600970526     C*
109700970526     C* IMPOSTA LA DS DI PROCEDURA
109800991001     C                   MOVEL     WOP0          k12OP0                         *LIVELLO PROCEDURA
109900991001     C                   MOVEL     WOP1          k12OP1                         *LIVELLO PGM
110000991001     C                   MOVEL     '0'           k12F03                         *PREMUTO F03=FINE
110100991001     C                   MOVEL     '0'           k12F12                         *PREMUTO F12=RITORNO
110200991001     C                   MOVEL     '0'           k12ERR                         *'1'=ERRORE
110300991001     C                   MOVEL     *BLANKS       k12MSG                         *MESSAGGIO DI RITORN
110400991001     C                   MOVEL     Dtle          k12tle                         *PARTENZA/ARRIVO
110500991001     c                   movel     dazp          k12azp
110600991001     C                   Z-ADD     Dcop          k12cop                         *TERMINAL PARTENZA
110700991001     c                   movel     dazf          k12azf
110800991001     C                   Z-ADD     Dcof          k12cof                         *TERMINAL ARRIVO
110900991001     C                   Z-ADD     Ddde          k12dde                         *DECORRENZA
111000991001     C                   Z-ADD     Ddsc          k12dsc                         *DECORRENZA
111100970526     C*
111200991104     C                   movel     tibds         kpjbu
111300970526     C                   CALL      WPGM
111400970612     C                   PARM                    KPJBA
111500991104     C*                  PARM                    tibds
111600970526     C*
111700970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
111800991001IF  1C     k12F03        IFEQ      '1'                                          *PREMUTO F03
111900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
112000970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
112100970526X   1C                   ELSE                                                   *NON PREMUTO F03
112200991001     C                   MOVEL     k12MSG        DSMSMS                         *MESSAGGIO DI RITORN
112300970526E   1C                   ENDIF
112400970526     C*
112500970526     C                   ENDSR
112600000607     C*------------------------------------------------------------------------*
112700000607     C* LANPGM2- LANCIA UN Pgm alternativo per ds non di procedura
112800000607     C*------------------------------------------------------------------------*
112900000607     C     LANPGM2       BEGSR
113000000607     C*
113100000607     C* IMPOSTA LA DS DI PROCEDURA
113200000607     c                   clear                   tis720
113300000607     C                   MOVEL     WOP0          s20OP0                         *LIVELLO PROCEDURA
113400000607     C                   MOVEL     WOP1          s20OP1                         *LIVELLO PGM
113500000607     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
113600000607     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
113700000607     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
113800000607     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
113900000607     C                   MOVEL     Dtle          s20tle                         *PARTENZA/ARRIVO
114000000607     C*                  move      *zeros        s20ksc                         *TERMINAL PARTENZA
114100000607     C                   move(p)   Dcop          s20ksc                         *TERMINAL PARTENZA
114200000607     C*
114300000607     C                   movel     tis720        kpjbu
114400000607     C                   CALL      WPGM
114500000607     C                   PARM                    KPJBA
114600000607     C*
114700000607     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
114800000607IF  1C     s20F03        IFEQ      '1'                                          *PREMUTO F03
114900000607     C                   MOVEL     '1'           ERRL00                         *ERRORE
115000000607     C                   MOVEL     '1'           F03L00                         *FINE PGM
115100000607X   1C                   ELSE                                                   *NON PREMUTO F03
115200000607     C                   MOVEL     s20MSG        DSMSMS                         *MESSAGGIO DI RITORN
115300000607E   1C                   ENDIF
115400000607     C*
115500000607     C                   ENDSR
115600991001     c*--------------------------------------------------------------------------------------------*
115700991001     C* cartab - carica le tabelle occorrenti
115800991001     c*--------------------------------------------------------------------------------------------*
115900991001     C     cartab        BEGSR
116000020905     C*
116100020905     C* REPERISCE I DATI UTENTE
116200020905     C                   EXSR      REPDATIUTE
116300020905     C*
116400020212     C*----------
116500020212     C* Tipi servizi soggetti a controllo unificazioni
116600020212     C*----------
116700020212     C                   z-add     *zeros        x                 3 0
116800020212     C                   eval      tbecod = 'ISV'
116900020212     C     ke2tbe        setll     tntbe01l
117000020212     C     ke2tbe        reade     tntbe01l
117100020212     C                   dow       not %eof(tntbe01l)
117200020212     C                   movel     tbeuni        dsisv
117300020212     C                   if        §isvuni  = 'S'
117400020212     C                   add       1             x
117500020212     C                   eval      isvkun(x) = tbeke1
117600020212     C                   endif
117700020212     C     ke2tbe        reade     tntbe01l
117800020212     C                   enddo
117900020212     C*
118000991001     c*
118100991001     C                   ENDSR
118200970526     C*------------------------------------------------------------------------*
118300970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
118400970526     C*------------------------------------------------------------------------*
118500970526     C     CHKFOP        BEGSR
118600970626     C*
118700970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
118800970626IF  1C     DEPOP0        IFEQ      'P01'
118900970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
119000970626     C                   MOVEL     'S'           OPDATT
119100970626     C                   MOVEL     OPDRIG        OPD(1)
119200970626X   1C                   ELSE
119300970626     C* OPZIONI
119400970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
119500970626     C                   MOVEL     'S'           OPDATT
119600970626     C                   MOVEL     OPDRIG        OPD(5)
119700020709     C     DUTLPO        IFEQ      'S'
119800000607     C                   MOVEL     OPD(9)        OPDRIG                         *CLI ABILITATI
119900000607     C                   MOVEL     'S'           OPDATT
120000000607     C                   MOVEL     OPDRIG        OPD(9)
120100000830     C                   ENDIF
120200970626     C* FUNZIONI
120300970626E   1C                   ENDIF
120400970626     C*
120500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
120600970626IF  1C     DEPOP0        IFEQ      'P02'
120700970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
120800970626     C                   MOVEL     'S'           OPDATT
120900970626     C                   MOVEL     OPDRIG        OPD(2)
121000020709     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
121100991014     C                   MOVEL     'S'           OPDATT
121200991014     C                   MOVEL     OPDRIG        OPD(3)
121300000307     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
121400000307     C                   MOVEL     'S'           OPDATT
121500000307     C                   MOVEL     OPDRIG        OPD(4)
121600970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
121700970626     C                   MOVEL     'S'           OPDATT
121800970626     C                   MOVEL     OPDRIG        OPD(7)
121900970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
122000970626     C                   MOVEL     'S'           FUDATT
122100970626     C                   MOVEL     FUDRIG        FUD(6)
122200970626E   1C                   ENDIF
122300970526     C*
122400970526     C* GESTIONE OPZIONI A VIDEO
122500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
122600970526     C                   EXSR      OPZGES
122700970526     C*
122800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
122900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
123000970526     C                   EXSR      FUNGES
123100970526     C*
123200970526     C                   ENDSR
123300991210     C*------------------------------------------------------------------------*
123400991210     C* FINSR - OPERAZIONI FINALI
123500991210     C*------------------------------------------------------------------------*
123600991210     C     FINSR         BEGSR
123700991210     C*
123800991210     C* CHIUDE I FILE DEI PGM CHIAMATI
123900991210     C                   CLEAR                   bs69ds
124000991210     C                   CLEAR                   acods
124100991210     C                   CLEAR                   indds
124200991210     C                   CLEAR                   clpds
124300991210     C                   CLEAR                   clsds
124400991210     C                   MOVEL     'C'           i69tla
124500991210     C                   MOVEL     knsif         i69sif
124600991210     C                   CALL      'TIBS69R'
124700991210     C                   PARM                    bs69ds
124800991210     C                   PARM                    acods
124900991210     C                   PARM                    indds
125000991210     C                   PARM                    clpds
125100991210     C                   PARM                    clsds
125200991210     C*
125300991210     C                   ENDSR
125400020905     C*--------------------------------------------------------------------------------------------*
125500020905     C* REPERISCE I DATI UTENTE
125600020905     C*--------------------------------------------------------------------------------------------*
125700020905     C     REPDATIUTE    BEGSR
125800020905     C*
125900020905     C* INIZIALIZZA VARIABILI DI WRK
126000020905     C                   CLEAR                   TIBS34DS
126100020905     C                   CLEAR                   AZUTEDS
126200020905     C                   CLEAR                   DDATIUTE
126300020905     C*
126400020905     C     *DTAARA       DEFINE    §azute        azuteds
126500020905     C     *DTAARA       DEFINE    §datiute      ddatiute
126600020905     C                   IN(E)     *DTAARA
126700020905if  1C                   IF        %Error
126800020905     c                   EVAL      I34Tla = 'L'
126900020905     C                   CALL      'TIBS34R'
127000020905     C                   PARM                    Tibs34Ds
127100020905     C                   IN        *DTAARA
127200020905e   1C                   ENDIF
127300020905     C*
127400020905     C                   MOVEL     RSUT          DSFIRS
127500060113     C*
127600060113     C* Carica le abilitazioni particolari del profilo
127700060113     C                   Movel     *blanks       wabi              2
127800060113     C                   Movel     utefaf        dute01
127900060113     C                   If        §utecli <> *Blanks
128000060113     C                   Eval      wabi = §utecli
128100060113     C                   Else
128200060113     C                   Eval      wabi = uteaut
128300060113     C                   EndIf
128400060113     C*
128500060113     C* Reperimento dei P.O. gestibili dall'utente per i codici clienti
128600060113     C                   Clear                   Trul31ds
128700060113     C                   Eval      i31abi = wabi
128800060113     C                   Eval      i31cdi = dutdis
128900060113     C                   Eval      i31car = dutare
129000060113     C                   Eval      i31cpo = dutpou
129100060113     C                   Call      'TRUL31R'
129200060113     C                   Parm                    kpjba
129300060113     C                   Parm                    Trul31ds
129400060113     C                   If        o31pog > *zeros
129500060113     C                   Movea     o31pog        skPOG
129600060113     C                   EndIf
129700020905     C*
129800020905     C                   ENDSR
129900970526     C*------------------------------------------------------------------------*
130000000307     C* *INZSR - OPERAZIONI INIZIALI
130100970526     C*------------------------------------------------------------------------*
130200970526     C     *INZSR        BEGSR
130300970526     C*---
130400970526     C* RICEVIMENTO PARAMETRI
130500970526     C*---
130600970526     C     *ENTRY        PLIST
130700970612     C                   PARM                    KPJBA
130800020709     c                   MOVEL     KPJBU         TIBDS
130900970526     C*
131000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
131100970526     C     XMSGPA        PLIST
131200970526     C                   PARM                    DSMSMS
131300970526     C                   PARM                    DSMSPG
131400970526     C                   PARM                    DSMSKE
131500970526     C                   PARM                    DSMSER
131600970526     C*
131700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
131800970526     C                   CALL      'XKEY'
131900970526     C                   PARM                    DSKEY
132000970526     C*---
132100970526     C* VARIABILI NON RIFERITE AL DATA BASE
132200970526     C*---
132300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
132400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
132500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
132600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
132700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
132800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
132900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
133000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
133100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
133200991001     C     *LIKE         DEFINE    k12OP0        DEPOP0                         *DI DEPOSITO
133300991001     C     *LIKE         DEFINE    k12OP1        DEPOP1
133400991001     C     *LIKE         DEFINE    k12OP0        WOP0                           *DI LAVORO
133500991001     C     *LIKE         DEFINE    k12OP1        WOP1
133600991001     C     *LIKE         DEFINE    k12tle        Dtle
133700991001     C     *LIKE         DEFINE    k12cop        Dcop
133800991001     C     *LIKE         DEFINE    k12azp        Dazp
133900991001     C     *LIKE         DEFINE    k12cof        Dcof
134000991001     C     *LIKE         DEFINE    k12azf        Dazf
134100991001     C     *LIKE         DEFINE    k12dde        Ddde
134200991001     C     *LIKE         DEFINE    k12dsc        Ddsc
134300970526     C*---
134400991001     C     *LIKE         DEFINE    k12tle        Deptle
134500991001     C     *LIKE         DEFINE    k12tle        Deptl2
134600991011     C     *LIKE         DEFINE    viduni        Depuni
134700991013     C     *LIKE         DEFINE    vidun2        Depun2
134800991011     C     *LIKE         DEFINE    videle        Depele
134900991013     C     *LIKE         DEFINE    videl2        Depel2
135000991013     C     *LIKE         DEFINE    vidpdd        Deppdd
135100000307     C*---
135200970526     C* VARIABILI DI CONTROLLO
135300970526     C*---
135400970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
135500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
135600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
135700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
135800970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
135900970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
136000000307     C*---
136100000307     C* INIZIALIZZAZIONE CAMPI A VIDEO
136200000307     C*---
136300020709     C                   MOVEL     *blanks       VIDUNI
136400020709     C                   MOVEL     *blanks       VIDUN2
136500020709     C                   MOVEL     *blanks       VIDELE
136600020709     C                   MOVEL     *blanks       VIDEL2
136700020709     C                   MOVEL     *blanks       VIDTL2
136800020709     C                   MOVEL     *blanks       VIDPDD
136900970526     C*
137000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
137100991001     C                   MOVEL     'TIBS12R'     DSMSPG
137200991001     C                   MOVEL     'TIBS12R'     MSGQUE
137300970526     C                   MOVEL     *BLANKS       MSGKEY
137400970526     C                   MOVEL     *BLANKS       DSMSMS
137500970526     C                   MOVEL     *BLANKS       DSMSKE
137600970526     C                   Z-ADD     *ZEROS        DSMSER
137700970526     C*---
137800970526     C* VARIABILI RIFERITE AL DATA BASE
137900970526     C*---
138000020830     C     *LIKE         DEFINE    acokcc        kcccl
138100000731     C     *LIKE         DEFINE    kuncop        KCEcop
138200000731     C     *LIKE         DEFINE    kuncof        KCEcof
138300000731     C     *LIKE         DEFINE    kundde        KCEdde
138400000731     C     *LIKE         DEFINE    kunazp        KCEazp
138500000731     C     *LIKE         DEFINE    kunazf        KCEazf
138600000731     C     *LIKE         DEFINE    kuntle        KCEtle
138700000307     C     *LIKE         DEFINE    tbecod        KTBcod
138800000307     C     *LIKE         DEFINE    tbeke1        KTBky1
138900020211     C     *LIKE         DEFINE    vssksu        KSSksu
139000020212     C     *LIKE         DEFINE    vssisv        KSSisv
139100020905     C     *LIKE         DEFINE    kpjbu         Depkpjbu
139200970526     C*---
139300970526     C* CHIAVI DI LETTURA
139400970526     C*---
139500000731     C* LETTURA tikun01L
139600000731     C     KE1kun        KLIST
139700000731     C                   KFLD                    KCEtle
139800000731     C                   KFLD                    KCEazp
139900000731     C                   KFLD                    KCEcop
140000000731     C                   KFLD                    KCEazf
140100000731     C                   KFLD                    KCEcof
140200160704     C* LETTURA tikun01L
140300160704     C     KE11kun       KLIST
140400160704     C                   KFLD                    KCEazp
140500160704     C                   KFLD                    KCEcop
140600160704     C                   KFLD                    KCEazf
140700160704     C                   KFLD                    KCEcof
140800160704     C                   KFLD                    KCEtle
140900020211     C* LETTURA tikun02L
141000000731     C     KE2kun        KLIST
141100000731     C                   KFLD                    KCEtle
141200000731     C                   KFLD                    KCEazf
141300000731     C                   KFLD                    KCEcof
141400000731     C                   KFLD                    KCEazp
141500000731     C                   KFLD                    KCEcop
141600020211     C* LETTURA tikun04L
141700000731     C     KE4kun        KLIST
141800000731     C                   KFLD                    KCEdde
141900000731     C                   KFLD                    KCEcop
142000000731     C                   KFLD                    KCEcof
142100020211     C* LETTURA tikun05L
142200000731     C     KE5kun        KLIST
142300000731     C                   KFLD                    KCEdde
142400000731     C                   KFLD                    KCEcof
142500000731     C                   KFLD                    KCEcop
142600991001     C* LETTURA cnaco00f
142700991001     C     keycli        KLIST                                                  *di cnaco/ind/clp
142800991001     C                   KFLD                    kclkut            1 0          -utente
142900991001     C                   KFLD                    kcccl                          -capoconto
143000991001     C                   KFLD                    kcksc             7 0          -codice
143100000307     C* LETTURA tntbe01l -
143200000307     C     KEYtbe        KLIST
143300000307     C                   KFLD                    Ktbcod                         *CODICE TABELLA
143400000307     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
143500020212     C* LETTURA tntbe01l - PARZIALE
143600020212     C     KE2tbe        KLIST
143700020212     C                   KFLD                    tbecod                         *CODICE TABELLA
143800020212     C* LETTURA tivss05L
143900020211     C     KEYvss        KLIST
144000020211     C                   KFLD                    KSSksu
144100970527     C*
144200970527     C* CARICA LE TABELLE OCCORRENTI
144300970527     C                   EXSR      CARTAB
144400020927     C*
144500020927     C* IN BASE ALL'UTENTE EFFETTUO ALCUNE CONSIDERAZIONI
144600021007     C                   IF        simfel = *zeros
144700020927     C                   OPEN      tivss05l
144800020927     C                   ENDIF
144900020916     C*
145000020916     C* INIZIALIZZAZIONE VARIABILI DI WRK
145100020916     C                   Z-ADD     1             kclkut
145200020916     C                   Z-ADD     DUTKCI        kcccl
145300970526     C*
145400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
145500991001     C                   MOVEL     k12OP0        DEPOP0                         *LIVELLO PROCEDURA
145600991001     C                   MOVEL     k12OP1        DEPOP1                         *LIVELLO PROGRAMMA
145700970526     C*
145800970526     C* CALCOLA LA DATA CORRENTE
145900100322     C                   Z-ADD     *ZEROS        DATCOR            8 0          *DATA CORRENTE AA/M/
146000100322     C                   EVAL      datcor = %dec(%date() : *ISO)
146100970526     C*
146200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
146300991028     c                   if        k12tle <> *blank and
146400991028     c                             k12tle <> *zeros
146500991028     c                   move      k12tle        vidtl2
146600991028     c                   else
146700991028     C                   MOVE      *blank        VIDtl2                         *PARTENZA/ARRIVO
146800991028     c                   end
146900000731     C*                  move      k12cop        VIDuni                         *CODICE CLI PADRE
147000000602     C                   move      k12cop        VIDun2                         *CODICE CLI PADRE
147100000602     C                   move      *zeros        VIDele                         *CODICE CLI FIGLIO
147200000602     C                   Z-ADD     *ZEROS        VIDPDD                         *DATA DECORRENZA
147300991001IF  1C     k12dde        IFGT      *ZEROS
147400991001     C                   Z-ADD     k12dde        G08INV
147500970528     C                   Z-ADD     *ZEROS        G08DAT
147600970528     C                   MOVEL     '3'           G08ERR
147700970528     C                   CALL      'XSRDA8'
147800970528     C                   PARM                    WLBDA8
147900970528     C                   Z-ADD     G08DAT        VIDPDD
148000970528E   1C                   ENDIF
148100970526     C                   EXSR      CHKPAR
148200970526IF  1C     ERRL00        IFEQ      '1'
148300991001     C                   MOVEL     '1'           k12ERR
148400991001     C                   MOVEL     DSMSMS        k12MSG
148500970526X   1C                   ELSE
148600991001     C                   MOVEL     VIDtle        DEPtle                         *DEPOSITI
148700991011     C                   move      VIDuni        DEPuni
148800991011     C                   move      VIDele        DEPele
148900991013     C                   Z-ADD     VIDpdd        DEPpdd
149000991013     C                   move      VIDun2        DEPun2
149100991013     C                   move      VIDel2        DEPel2
149200140609     C                   movel     VIDtl2        DEPtl2                         *tipo legame
149300970526E   1C                   ENDIF
149400970526     C*
149500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
149600970526     C                   EXSR      CHKFOP
149700970526     C*
149800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
149900970530IF  1C     DEPOP0        IFEQ      'P01'
150000970530     C                   MOVEL     DEC(3)        VIDING
150100970530E   1C                   ENDIF
150200970530IF  1C     DEPOP0        IFEQ      'P02'
150300970526     C                   MOVEL     DEC(1)        VIDING
150400970530E   1C                   ENDIF
150500970530IF  1C     DEPOP0        IFEQ      'P05'
150600970526     C                   MOVEL     DEC(2)        VIDING
150700970526E   1C                   ENDIF
150800970526     C*
150900970526     C                   ENDSR
151000970526     C*------------------------------------------------------------------------*
151100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
151400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
151500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
151600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151800970526     C*------------------------------------------------------------------------*
151900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
152000970526     C*------------------------------------------------------------------------*
152100970526     C     POSSFL        BEGSR
152200970526     C*
152300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
152400970526     C                   EXSR      CLRSFL
152500970526     C*
152600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
152700970526     C                   EXSR      SETFIL
152800970526     C*
152900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
153000970526     C     SFLEOF        IFEQ      0
153100970526     C                   EXSR      CARSFL
153200970526     C                   END
153300970526     C*
153400970526     C                   ENDSR
153500970526     C*------------------------------------------------------------------------*
153600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
153700970526     C*------------------------------------------------------------------------*
153800970526     C     ENTATT        BEGSR
153900970526     C*
154000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
154100970526     C                   Z-ADD     0             OPZOK             1 0
154200970526     C*
154300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
154400970526     C                   Z-ADD     0             SFLVAR            5 0
154500970526     C*
154600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
154700970526     C                   Z-ADD     0             SFLERR
154800970526     C*
154900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
155000970526     C                   Z-ADD     SFLULT        WFLULT
155100970526     C*
155200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
155300991011     C                   READC     BS12D05                                98
155400970526     C     *IN98         DOWEQ     '0'
155500970526     C*
155600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
155700970526     C                   ADD       1             SFLVAR
155800970526     C*
155900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
156000970526     C     OPZOK         IFEQ      0
156100970526     C*
156200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
156300970526     C     OPZKEY        IFNE      *BLANKS
156400970526     C                   EXSR      OPZCON
156500970526     C                   END
156600970526     C*
156700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
156800970526     C*            2. PULIZIA SCELTA IN SFL
156900970526     C     OPZOK         IFEQ      0
157000970526     C                   MOVEL     '0'           *IN41
157100970526     C                   MOVEL     *BLANKS       OPZKEY
157200970526     C                   ELSE
157300970526     C*
157400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
157500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
157600970526     C                   MOVEL     '1'           *IN41
157700970526     C                   Z-ADD     SFLULT        SFLERR
157800970526     C                   END
157900970526     C                   ELSE
158000970526     C*
158100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
158200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
158300970526     C                   MOVEL     '1'           *IN41
158400970526     C                   END                                                    OPZOK=0
158500970526     C*
158600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
158700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
158800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
158900970526     C     OPZOK         IFNE      0
159000970526     C                   Z-ADD     SFLERR        SFLNBR
159100970526     C                   ELSE
159200970526     C                   Z-ADD     SFLULT        SFLNBR
159300970526     C                   END
159400970526     C*
159500160701     C* Riaggiorna il record appena modificato
159600160701     c     nrec1         chain     tikun00f
159700160701     c                   if        %Found(tikun00f)
159800160701     c                   exsr      IRESFL
159900160701     c                   setoff                                       75
160000160701     C                   if        kundsc < datcor
160100160701     C                   seton                                        75
160200160701     C                   endif
160300160701     c                   end
160400160701     C*
160500970526     C* AGGIORNO RECORD SFL
160600160704     C                   EVAL         HIN75 = *IN75
160700160704     C                   EVAL      *IN76 = HIN76
160800991011     C                   UPDATE    BS12D05
160900970526     C*
161000991011     C                   READC     BS12D05                                98
161100970526     C                   END                                                    DO WHILE
161200970526     C*
161300970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
161400970526     C                   Z-ADD     WFLULT        SFLULT
161500970526     C*
161600970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
161700970526     C     SFLVAR        IFEQ      0
161800970526     C*---                 MOVEL'1'       FINL00
161900970526     C                   END
162000970526     C*
162100970526     C                   ENDSR
162200970526     C*------------------------------------------------------------------------*
162300970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
162400970526     C*------------------------------------------------------------------------*
162500970526     C     CLRSFL        BEGSR
162600970526     C*
162700970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
162800970526     C* (0=NON FINE FILE  1=FINE FILE)
162900970526     C                   Z-ADD     0             SFLEOF
163000970526     C*
163100970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
163200970526     C                   Z-ADD     0             SFLULT
163300970526     C*
163400970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
163500970526     C                   MOVEL     '1'           *IN40
163600991011     C                   WRITE     BS12D06
163700970526     C                   MOVEL     '0'           *IN40
163800970526     C*
163900970526     C                   ENDSR
164000970526     C*------------------------------------------------------------------------*
164100970526     C* CARSFL - CARICAMENTO SFL
164200970526     C*------------------------------------------------------------------------*
164300970526     C     CARSFL        BEGSR
164400970526     C*
164500970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
164600970526     C     SFLULT        IFEQ      0
164700970526     C                   EXSR      SETFIL
164800970526     C                   END
164900970526     C*
165000970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
165100970526     C                   Z-ADD     SFLULT        WFLULT
165200160704     c                   eval      RrnLast = 0
165300970526     C*
165400970526     C* CICLO:
165500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
165600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
165700970526     C                   Z-ADD     1             SFLRIG
165800160704     C     SFLRIG        DOWLE     SFLPAG
165900160704     C     SFLEOF        ANDEQ     0
166000970526     C*
166100970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
166200970526     C                   MOVEL     *BLANKS       OPZKEY
166300970526     C*
166400970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
166500970526     C                   EXSR      IRESFL
166600970526     C*
166700970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
166800970526     C                   ADD       1             SFLULT
166900160701     c                   z-add     nrrecHID      nrec1
167000970526     C*
167100970526     C* SCRITTURA RECORD SFL
167200160704     C                   EVAL      HIN75 = *IN75
167300160704     C                   EVAL      HIN76 = *IN76
167400160704     C*
167500991011     C                   WRITE     BS12D05
167600970526     C                   ADD       1             SFLRIG
167700160704     c                   eval      RrnLast = RrnLast +1
167800970526     C*
167900970526     C* LETTURA FILE DA CARICARE
168000970526     C                   EXSR      LETFIL
168100970526     C                   END                                                    DO WHILE
168200160704     C*
168300160704     C*  Riordina i dati da emettere
168400160704     C******             exsr      Ordina_SFL
168500970526     C*
168600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
168700970526     C     WFLULT        IFNE      SFLULT
168800970526     C                   Z-ADD     WFLULT        SFLNBR
168900970526     C                   ADD       1             SFLNBR
169000970526     C                   END
169100970526     C*
169200970526     C                   ENDSR
169300970526     C*------------------------------------------------------------------------*
169400970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
169500970526     C*------------------------------------------------------------------------*
169600970526     C     OUTSFL        BEGSR
169700970526     C*
169800970526     C* WRITE RECORD TESTATA VIDEO
169900991011     C                   WRITE     BS12D01
170000970526     C*
170100970526     C* VISUALIZZAZIONE MESSAGGIO
170200970526     C                   EXSR      SNDMSG
170300970526     C*
170400000307     C* WRITE RECORD OPZIONI VIDEO
170500991011     C                   WRITE     BS12D02
170600970526     C*
170700970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
170800991011     C                   WRITE     BS12D03
170900970526     C*
171000970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
171100991011     C                   WRITE     BS12D04
171200970526     C*
171300970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
171400970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
171500970526     C     SFLEOF        IFEQ      0
171600970526     C                   MOVEL     '0'           *IN42
171700970526     C                   ELSE
171800970526     C*
171900970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
172000970526     C                   MOVEL     '1'           *IN42
172100970526     C                   END
172200970526     C*
172300970526     C* WRITE RECORD TASTI FUNZIONALI
172400991011     C                   WRITE     BS12D09
172500970526     C*
172600970526     C* CASO DI SFL VUOTO
172700970526     C     SFLULT        IFLE      0
172800970526     C*
172900970526     C*    - WRITE RECORD VUOTO
173000991011     C                   WRITE     BS12D0A
173100991011     C                   READ      BS12D0A                                99
173200970526     C*
173300970526     C                   ELSE
173400970526     C*
173500970526     C* CASO DI SFL CON RECORDS
173600970526     C*
173700970526     C*    - WRITE RECORD CONTROLLO SFL
173800991011     C                   WRITE     BS12D06
173900991011     C                   READ      BS12D06                                99
174000970526     C                   END
174100970526     C*
174200970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
174300991011     C                   READ      BS12D03                                99
174400970526     C*
174500970526     C                   ENDSR
174600970526     C*------------------------------------------------------------------------*
174700970526     C* SNDMSG - MANDA MESSAGGIO
174800970526     C*------------------------------------------------------------------------*
174900970526     C     SNDMSG        BEGSR
175000970526     C*
175100970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
175200970526     C                   MOVEL     '1'           *IN40
175300991011     C                   WRITE     BS12DMC
175400970526     C                   MOVEL     '0'           *IN40
175500970526     C*
175600970526     C* VISUALIZZAZIONE MESSAGGIO
175700970526     C                   Z-ADD     0             MSGULT
175800970526     C                   CALL      'XMSG'        XMSGPA
175900970526     C     DSMSER        IFEQ      0
176000970526     C                   MOVEL     DSMSKE        MSGKEY
176100970526     C                   END
176200970526     C                   Z-ADD     1             MSGULT
176300991011     C                   WRITE     BS12DMS                                      SFL MESS
176400991011     C                   WRITE     BS12DMC                                      CONTR MESS
176500970526     C*
176600970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
176700970526     C                   MOVEL     *BLANKS       DSMSMS
176800970526     C*
176900970526     C                   ENDSR
177000970526     C*------------------------------------------------------------------------*
177100970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
177200970526     C*------------------------------------------------------------------------*
177300970526     C     FUNCON        BEGSR
177400970526     C*
177500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
177600970526     C                   EXSR      FUNVAR
177700970526     C*
177800970526     C* PULISCO CAMPI DI LAVORO
177900970526     C                   Z-ADD     0             I                 5 0
178000970526     C                   Z-ADD     0             W                 5 0
178100970526     C*
178200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
178300970526     C     FUNKEY        IFNE      *BLANKS
178400970526     C     1             DO        24            I
178500970526     C                   MOVEL     FUD(I)        FUDRIG
178600970526     C*
178700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
178800970526     C     FUNKEY        IFEQ      FUDFUN
178900970526     C     FUDATT        ANDEQ     'S'
179000970526     C                   Z-ADD     I             W
179100970526     C                   END
179200970526     C                   END                                                    DO WHILE
179300970526     C*
179400970526     C* FUNZIONE NON ATTIVA (ERRORE)
179500970526     C     W             IFEQ      0
179600970526     C*
179700970526     C* EMISSIONE MESSAGGIO DI ERRORE
179800970526     C                   MOVEL     MSG(2)        DSMSMS
179900970526     C                   END                                                    W=0
180000970526     C                   END                                                    FUNKEY NE *BLANKS
180100970526     C*
180200970526     C* FUNZIONE ATTIVA (PROCEDI ...)
180300970526     C     W             IFNE      0
180400970526     C     FUNKEY        OREQ      *BLANKS
180500970526     C*
180600970526     C     FUNKEY        IFEQ      *BLANKS
180700970526     C                   EXSR      ENTGES
180800970526     C                   END
180900970526     C     FUNKEY        IFEQ      '03'
181000970526     C                   EXSR      F03GES
181100970526     C                   END
181200970526     C     FUNKEY        IFEQ      '05'
181300970526     C                   EXSR      F05GES
181400970526     C                   END
181500970526     C     FUNKEY        IFEQ      '06'
181600970526     C                   EXSR      F06GES
181700970526     C                   END
181800970526     C     FUNKEY        IFEQ      '12'
181900970526     C                   EXSR      F12GES
182000970526     C                   END
182100970528     C     FUNKEY        IFEQ      '21'
182200970528     C                   EXSR      F21GES
182300970528     C                   END
182400970528     C     FUNKEY        IFEQ      '22'
182500970528     C                   EXSR      F22GES
182600970528     C                   END
182700970526     C     FUNKEY        IFEQ      '23'
182800970526     C                   EXSR      F23GES
182900970526     C                   END
183000970526     C     FUNKEY        IFEQ      '24'
183100970526     C                   EXSR      F24GES
183200970526     C                   END
183300970526     C     FUNKEY        IFEQ      'RU'
183400970526     C                   EXSR      RUPGES
183500970526     C                   END
183600970526     C*
183700970526     C                   END
183800970526     C*
183900970526     C                   ENDSR
184000970526     C*------------------------------------------------------------------------*
184100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
184200970526     C*------------------------------------------------------------------------*
184300970526     C     FUNVAR        BEGSR
184400970526     C*
184500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
184600970526     C     KEY           IFEQ      ENTER
184700970526     C                   MOVEL     *BLANKS       FUNKEY
184800970526     C                   END
184900970526     C     KEY           IFEQ      F1
185000970526     C                   MOVEL     '01'          FUNKEY
185100970526     C                   END
185200970526     C     KEY           IFEQ      F2
185300970526     C                   MOVEL     '02'          FUNKEY
185400970526     C                   END
185500970526     C     KEY           IFEQ      F3
185600970526     C                   MOVEL     '03'          FUNKEY
185700970526     C                   END
185800970526     C     KEY           IFEQ      F4
185900970526     C                   MOVEL     '04'          FUNKEY
186000970526     C                   END
186100970526     C     KEY           IFEQ      F5
186200970526     C                   MOVEL     '05'          FUNKEY
186300970526     C                   END
186400970526     C     KEY           IFEQ      F6
186500970526     C                   MOVEL     '06'          FUNKEY
186600970526     C                   END
186700970526     C     KEY           IFEQ      F7
186800970526     C                   MOVEL     '07'          FUNKEY
186900970526     C                   END
187000970526     C     KEY           IFEQ      F8
187100970526     C                   MOVEL     '08'          FUNKEY
187200970526     C                   END
187300970526     C     KEY           IFEQ      F9
187400970526     C                   MOVEL     '09'          FUNKEY
187500970526     C                   END
187600970526     C     KEY           IFEQ      F10
187700970526     C                   MOVEL     '10'          FUNKEY
187800970526     C                   END
187900970526     C     KEY           IFEQ      F11
188000970526     C                   MOVEL     '11'          FUNKEY
188100970526     C                   END
188200970526     C     KEY           IFEQ      F12
188300970526     C                   MOVEL     '12'          FUNKEY
188400970526     C                   END
188500970526     C     KEY           IFEQ      F13
188600970526     C                   MOVEL     '13'          FUNKEY
188700970526     C                   END
188800970526     C     KEY           IFEQ      F14
188900970526     C                   MOVEL     '14'          FUNKEY
189000970526     C                   END
189100970526     C     KEY           IFEQ      F15
189200970526     C                   MOVEL     '15'          FUNKEY
189300970526     C                   END
189400970526     C     KEY           IFEQ      F16
189500970526     C                   MOVEL     '16'          FUNKEY
189600970526     C                   END
189700970526     C     KEY           IFEQ      F17
189800970526     C                   MOVEL     '17'          FUNKEY
189900970526     C                   END
190000970526     C     KEY           IFEQ      F18
190100970526     C                   MOVEL     '18'          FUNKEY
190200970526     C                   END
190300970526     C     KEY           IFEQ      F19
190400970526     C                   MOVEL     '19'          FUNKEY
190500970526     C                   END
190600970526     C     KEY           IFEQ      F20
190700970526     C                   MOVEL     '20'          FUNKEY
190800970526     C                   END
190900970526     C     KEY           IFEQ      F21
191000970526     C                   MOVEL     '21'          FUNKEY
191100970526     C                   END
191200970526     C     KEY           IFEQ      F22
191300970526     C                   MOVEL     '22'          FUNKEY
191400970526     C                   END
191500970526     C     KEY           IFEQ      F23
191600970526     C                   MOVEL     '23'          FUNKEY
191700970526     C                   END
191800970526     C     KEY           IFEQ      F24
191900970526     C                   MOVEL     '24'          FUNKEY
192000970526     C                   END
192100970526     C     KEY           IFEQ      ROLLUP
192200970526     C                   MOVEL     'RU'          FUNKEY
192300970526     C                   END
192400970526     C     KEY           IFEQ      HELP
192500970526     C                   MOVEL     'HP'          FUNKEY
192600970526     C                   END
192700970526     C*
192800970526     C                   ENDSR
192900970526     C*------------------------------------------------------------------------*
193000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
193100970526     C*------------------------------------------------------------------------*
193200970526     C     FUNGES        BEGSR
193300970526     C*
193400970526     C* PULISCO CAMPI DI LAVORO
193500970526     C                   Z-ADD     0             W                 5 0
193600970526     C*
193700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
193800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
193900970526     C     1             DO        24            I
194000970526     C                   MOVEL     FUD(I)        FUDRIG
194100970526     C*
194200970526     C* PULIZIA FUNZIONI PRECEDENTI
194300970526     C                   MOVEL     *BLANKS       FUV(I)
194400970526     C*
194500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
194600970526     C* (SALTA F23 E F24)
194700970526     C     FUDATT        IFEQ      'S'
194800970526     C     FUDFUN        ANDNE     '23'
194900970526     C     FUDFUN        ANDNE     '24'
195000970526     C                   ADD       1             W
195100970526     C                   MOVEL     FUDDES        FUV(W)
195200970526     C                   END
195300970526     C                   END                                                    DO WHILE
195400970526     C*
195500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
195600970526     C* ALTRIMENTI DISATTIVO F24
195700970526     C     1             DO        24            I
195800970526     C                   MOVEL     FUD(I)        FUDRIG
195900970526     C*
196000970526     C     FUDFUN        IFEQ      '24'
196100970526     C     W             IFGT      6
196200970526     C                   MOVEL     'S'           FUDATT
196300970526     C                   ELSE
196400970526     C                   MOVEL     *BLANKS       FUDATT
196500970526     C                   END                                                    W GT 8
196600970526     C                   MOVEL     FUDRIG        FUD(I)
196700970526     C                   END
196800970526     C                   END                                                    DO WHILE
196900970526     C*
197000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
197100970526     C     W             DIV       6             I
197200970526     C                   MVR                     Z
197300970526     C*
197400970526     C     Z             IFNE      0
197500970526     C     I             ADD       1             Z
197600970526     C                   ELSE
197700970526     C                   Z-ADD     I             Z
197800970526     C                   END
197900970526     C*
198000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
198100970526     C                   ADD       1             FUNGRU
198200970526     C     FUNGRU        IFGT      Z
198300970526     C     FUNGRU        ORLT      1
198400970526     C                   Z-ADD     1             FUNGRU
198500970526     C                   END
198600970526     C*
198700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
198800970526     C     FUNGRU        IFEQ      1
198900970526     C                   MOVEL     FUV(1)        FUN001
199000970526     C                   MOVEL     FUV(2)        FUN002
199100970526     C                   MOVEL     FUV(3)        FUN003
199200970526     C                   MOVEL     FUV(4)        FUN004
199300970526     C                   MOVEL     FUV(5)        FUN005
199400970526     C                   MOVEL     FUV(6)        FUN006
199500970526     C                   END
199600970526     C*
199700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
199800970526     C     FUNGRU        IFEQ      2
199900970526     C                   MOVEL     FUV(7)        FUN001
200000970526     C                   MOVEL     FUV(8)        FUN002
200100970526     C                   MOVEL     FUV(9)        FUN003
200200970526     C                   MOVEL     FUV(10)       FUN004
200300970526     C                   MOVEL     FUV(11)       FUN005
200400970526     C                   MOVEL     FUV(12)       FUN006
200500970526     C                   END
200600970526     C*
200700970526     C* TERZO GRUPPO DI 6 FUNZIONI
200800970526     C     FUNGRU        IFEQ      3
200900970526     C                   MOVEL     FUV(13)       FUN001
201000970526     C                   MOVEL     FUV(14)       FUN002
201100970526     C                   MOVEL     FUV(15)       FUN003
201200970526     C                   MOVEL     FUV(16)       FUN004
201300970526     C                   MOVEL     FUV(17)       FUN005
201400970526     C                   MOVEL     FUV(18)       FUN006
201500970526     C                   END
201600970526     C*
201700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
201800970526     C     FUNGRU        IFEQ      4
201900970526     C                   MOVEL     FUV(19)       FUN001
202000970526     C                   MOVEL     FUV(20)       FUN002
202100970526     C                   MOVEL     FUV(21)       FUN003
202200970526     C                   MOVEL     FUV(22)       FUN004
202300970526     C                   MOVEL     *BLANKS       FUN005
202400970526     C                   MOVEL     *BLANKS       FUN006
202500970526     C                   END
202600970526     C*
202700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
202800970526     C                   MOVEL     *BLANKS       FUN007
202900970526     C                   MOVEL     *BLANKS       FUN008
203000970526     C*
203100970526     C     1             DO        24            I
203200970526     C                   MOVEL     FUD(I)        FUDRIG
203300970526     C     FUDFUN        IFEQ      '23'
203400970526     C     FUDATT        ANDEQ     'S'
203500970526     C                   MOVEL     FUDDES        FUN007
203600970526     C                   END
203700970526     C     FUDFUN        IFEQ      '24'
203800970526     C     FUDATT        ANDEQ     'S'
203900970526     C                   MOVEL     FUDDES        FUN008
204000970526     C                   END
204100970526     C                   END                                                    DO
204200970526     C*
204300970526     C                   ENDSR
204400970526     C*------------------------------------------------------------------------*
204500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
204600970526     C*------------------------------------------------------------------------*
204700970526     C     OPZGES        BEGSR
204800970526     C*
204900970526     C* PULISCO CAMPI DI LAVORO
205000970526     C                   Z-ADD     0             W                 5 0
205100970526     C*
205200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
205300970526     C* E CARICO TABELLA OPZIONI ATTIVE
205400970526     C     1             DO        24            I
205500970526     C                   MOVEL     OPD(I)        OPDRIG
205600970526     C*
205700970526     C* PULIZIA OPZIONI PRECEDENTI
205800970526     C                   MOVEL     *BLANKS       OPV(I)
205900970526     C*
206000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
206100970526     C     OPDATT        IFEQ      'S'
206200970526     C                   ADD       1             W
206300970526     C                   MOVEL     OPDDES        OPV(W)
206400970526     C                   END
206500970526     C                   END                                                    DO WHILE
206600970526     C*
206700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
206800970526     C* ALTRIMENTI DISATTIVO F23
206900970526     C     1             DO        24            I
207000970526     C                   MOVEL     FUD(I)        FUDRIG
207100970526     C*
207200970526     C     FUDFUN        IFEQ      '23'
207300970526     C     W             IFGT      8
207400970526     C                   MOVEL     'S'           FUDATT
207500970526     C                   ELSE
207600970526     C                   MOVEL     *BLANKS       FUDATT
207700970526     C                   END                                                    W GT 8
207800970526     C                   MOVEL     FUDRIG        FUD(I)
207900970526     C                   END
208000970526     C                   END                                                    DO WHILE
208100970526     C*
208200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
208300970526     C     W             DIV       8             I
208400970526     C                   MVR                     Z                 5 0
208500970526     C*
208600970526     C     Z             IFNE      0
208700970526     C     I             ADD       1             Z
208800970526     C                   ELSE
208900970526     C                   Z-ADD     I             Z
209000970526     C                   END
209100970526     C*
209200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
209300970526     C                   ADD       1             OPZGRU
209400970526     C     OPZGRU        IFGT      Z
209500970526     C     OPZGRU        ORLT      1
209600970526     C                   Z-ADD     1             OPZGRU
209700970526     C                   END
209800970526     C*
209900970526     C* PRIMO GRUPPO DI 8 OPZIONI
210000970526     C     OPZGRU        IFEQ      1
210100970526     C                   MOVEL     OPV(1)        OPZ001
210200970526     C                   MOVEL     OPV(2)        OPZ002
210300970526     C                   MOVEL     OPV(3)        OPZ003
210400970526     C                   MOVEL     OPV(4)        OPZ004
210500970526     C                   MOVEL     OPV(5)        OPZ005
210600970526     C                   MOVEL     OPV(6)        OPZ006
210700970526     C                   MOVEL     OPV(7)        OPZ007
210800970526     C                   MOVEL     OPV(8)        OPZ008
210900970526     C                   END
211000970526     C*
211100970526     C* SECONDO GRUPPO DI 8 OPZIONI
211200970526     C     OPZGRU        IFEQ      2
211300970526     C                   MOVEL     OPV(9)        OPZ001
211400970526     C                   MOVEL     OPV(10)       OPZ002
211500970526     C                   MOVEL     OPV(11)       OPZ003
211600970526     C                   MOVEL     OPV(12)       OPZ004
211700970526     C                   MOVEL     OPV(13)       OPZ005
211800970526     C                   MOVEL     OPV(14)       OPZ006
211900970526     C                   MOVEL     OPV(15)       OPZ007
212000970526     C                   MOVEL     OPV(16)       OPZ008
212100970526     C                   END
212200970526     C*
212300970526     C* TERZO GRUPPO DI 8 OPZIONI
212400970526     C     OPZGRU        IFEQ      3
212500970526     C                   MOVEL     OPV(17)       OPZ001
212600970526     C                   MOVEL     OPV(18)       OPZ002
212700970526     C                   MOVEL     OPV(19)       OPZ003
212800970526     C                   MOVEL     OPV(20)       OPZ004
212900970526     C                   MOVEL     OPV(21)       OPZ005
213000970526     C                   MOVEL     OPV(22)       OPZ006
213100970526     C                   MOVEL     OPV(23)       OPZ007
213200970526     C                   MOVEL     OPV(24)       OPZ008
213300970526     C                   END
213400970526     C*
213500970526     C                   ENDSR
213600970526     C*------------------------------------------------------------------------*
213700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
213800970526     C*------------------------------------------------------------------------*
213900970526     C     OPZCON        BEGSR
214000970526     C*
214100970526     C* PULISCO CAMPI DI LAVORO
214200970526     C                   Z-ADD     0             I                 5 0
214300970526     C                   Z-ADD     0             W                 5 0
214400970526     C*
214500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
214600970526     C     1             DO        24            I
214700970526     C                   MOVEL     OPD(I)        OPDRIG
214800970526     C*
214900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
215000970526     C     OPZKEY        IFEQ      OPDOPZ
215100970526     C     OPZKEY        OREQ      OPDOP1
215200970526     C*
215300970526     C     OPDATT        IFEQ      'S'
215400970526     C                   MOVEL     OPDOPZ        OPZKEY
215500970526     C                   Z-ADD     I             W
215600970526     C                   END
215700970526     C                   END
215800970526     C*
215900970526     C                   END                                                    DO WHILE
216000970526     C*
216100970526     C* OPZIONE NON ATTIVA (ERRORE)
216200970526     C     W             IFEQ      0
216300970526     C*
216400970526     C* OPZIONE NON OK
216500970526     C                   Z-ADD     1             OPZOK
216600970526     C*
216700970526     C* EMISSIONE MESSAGGIO DI ERRORE
216800970526     C                   MOVEL     MSG(1)        DSMSMS
216900970526     C                   ELSE
217000970526     C*
217100970526     C* OPZIONE ATTIVA (PROCEDI ...)
217200970526     C*
217300970526     C     OPZKEY        IFEQ      ' 1'
217400970526     C                   EXSR      O01GES
217500970526     C                   END
217600970526     C     OPZKEY        IFEQ      ' 2'
217700970526     C                   EXSR      O02GES
217800970526     C                   END
217900991014     C     OPZKEY        IFEQ      ' 3'
218000991014     C                   EXSR      O03GES
218100991014     C                   END
218200000307     C     OPZKEY        IFEQ      ' 4'
218300000307     C                   EXSR      O04GES
218400000307     C                   END
218500970526     C     OPZKEY        IFEQ      ' 5'
218600970526     C                   EXSR      O05GES
218700970526     C                   END
218800000307     C     OPZKEY        IFEQ      ' 7'
218900000307     C                   EXSR      O07GES
219000000307     C                   END
219100000607     C     OPZKEY        IFEQ      ' 9'
219200000607     C                   EXSR      O09GES
219300000607     C                   END
219400970526     C                   END                                                    OPZ.NON ATTIVA
219500970526     C*
219600970526     C                   ENDSR
219700160704     C/EJECT
219800160704      * ?-------------------------------------------------------------*?
219900160704      *? Riordina comunque il SFL                                     ?
220000160704      *?   Deve mostrare prima il cliente poi il tipo                 ?
220100160704      * ?-------------------------------------------------------------*?
220200160704     C     Ordina_SFL    BEGSR
220300160704     C*
220400160704     C*
220500160704      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
220600160704      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
220700160704      * Il campo è usato per selezionare i records dando un ordine a quelli
220800160704      * selezionati davanti ai non selezionati.
220900160704     C                   CLEAR                   QLGSCB
221000160704     C                   CLEAR                   QLGSCB00
221100160704      *
221200160704      *?  Ordinamento per Padre/Figlio/Tipo/Decorrenza/Scadenza   ?
221300160704     ** 5 campi chiave.
221400160704     C                   EVAL      QLGNBRK    = 5
221500160704
221600160704     ** Il codice Padre
221700160704     C                   EVAL      QLGSP      = 1
221800160704     C                   EVAL      QLGSS      = %SIZE(VIDKOP)
221900160704     C                   EVAL      QLGDT      = Carattere
222000160704     C                   EVAL      QLGSO      = Ascendente
222100160704     C                   EVAL      QLGKL(1)   = QLGSKL
222200160704     ** Il codice Figlio
222300160704     C                   EVAL      QLGSP   = 1 + %SIZE(VIDKOP)
222400160704     C                   EVAL      QLGSS    = %SIZE(VIDCOF)
222500160704     C                   EVAL      QLGDT    = Carattere
222600160704     C                   EVAL      QLGSO    = Ascendente
222700160704     C                   EVAL      QLGKL(2) = QLGSKL
222800160704     ** Il Tipo legame
222900160704     C                   EVAL      QLGSP   = 1 + %SIZE(VIDKOP) + %SIZE(VIDCOF)
223000160704     C                   EVAL      QLGSS    = %SIZE(VIDTLE)
223100160704     C                   EVAL      QLGDT    = Carattere
223200160704     C                   EVAL      QLGSO    = Ascendente
223300160704     C                   EVAL      QLGKL(3) = QLGSKL
223400160704     ** Data Decorrenza
223500160704     C                   EVAL      QLGSP   = 1 + %SIZE(VIDKOP) + %SIZE(VIDCOF)
223600160704     c                                         + %SIZE(VIDTLE)
223700160704     C                   EVAL      QLGSS    = %SIZE(VIHDDE)
223800160704     C                   EVAL      QLGDT    = Carattere
223900160704     C                   EVAL      QLGSO    = Ascendente
224000160704     C                   EVAL      QLGKL(4) = QLGSKL
224100160704     ** Data Scadenza
224200160704     C                   EVAL      QLGSP   = 1 + %SIZE(VIDKOP) + %SIZE(VIDCOF)
224300160704     c                                         + %SIZE(VIDTLE) + %SIZE(VIHDDE)
224400160704     C                   EVAL      QLGSS    = %SIZE(VIHDSC)
224500160704     C                   EVAL      QLGDT    = Carattere
224600160704     C                   EVAL      QLGSO    = Ascendente
224700160704     C                   EVAL      QLGKL(5) = QLGSKL
224800160704      *------------______________________________________________------------
224900160704      * Load other sort parameters.
225000160704     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
225100160704     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
225200160704     C                   EVAL      QLGRT     = 8
225300160704     C                   EVAL      QLGOKL    = 80
225400160704     C                   EVAL      QLGLKE    = 16
225500160704     C                   EVAL      QLGLSS    = 290
225600160704
225700160704      * Initialize Sort I/O API fields.
225800160704     C                   EVAL      QLGRL00  = QLGRL
225900160704     C                   EVAL      QLGRC00  = 1
226000160704     C                   CLEAR                   QUSEI
226100160704     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
226200160704
226300160704      * First step - Initialize the sort routine.
226400160704     C                   CALL      'QLGSORT'
226500160704     C                   PARM                    QLGSCB
226600160704     C                   PARM                    NotUsed
226700160704     C                   PARM                    NotUsed
226800160704     C                   PARM                    SizeList
226900160704     C                   PARM                    ReturnSize
227000160704     C                   PARM                    QUSEC
227100160704
227200160704      * Next step - Write records to I/O routine.
227300160704     C                   EVAL      QLGRT00 = Put
227400160704
227500160704     C                   DO        RrnLast       S2nrr_xx          5 0
227600160704     C     S2nrr_xx      CHAIN     BS12D05
227700160704
227800160704     ** Solo le righe con Selected = 'Y' sono riordinate,
227900160704     ** quindi per fare un ordinamento di tutte le righe
228000160704     ** metto 'Y' sempre.
228100160704     C                   EVAL      Selected  = 'Y'
228200160704
228300160704     C                   CLEAR                   QUSEI
228400160704     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
228500160704      *
228600160704     C                   CALL      'QLGSRTIO'
228700160704     C                   PARM                    QLGSCB00
228800160704     C                   PARM                    SflRcd
228900160704     C                   PARM                    NotUsed
229000160704     C                   PARM                    SizeList
229100160704     C                   PARM                    NotUsed
229200160704     C                   PARM                    QUSEC
229300160704
229400160704     C                   ENDDO
229500160704
229600160704      * Next step - Signal end of input, clear subfile for reload.
229700160704     C                   EVAL      QLGRT00 = EndPut
229800160704     C                   CLEAR                   QUSEI
229900160704     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
230000160704
230100160704     C                   CALL      'QLGSRTIO'
230200160704     C                   PARM                    QLGSCB00
230300160704     C                   PARM                    SflRcd
230400160704     C                   PARM                    NotUsed
230500160704     C                   PARM                    SizeList
230600160704     C                   PARM                    NotUsed
230700160704     C                   PARM                    QUSEC
230800160704      *
  pulizia SFL    ?
230900160704     C* ACCENSIONE INDICATORE PER CLEAR SFL
231000160704     C                   SETON                                          40
231100160704     C                   WRITE     BS12D06
231200160704     C                   SETOff                                         40
231300160704
231400160704      * Final step - Write the records back to the subfile.
231500160704     C                   EVAL      QLGRT00 = Get
231600160704
231700160704     C                   DO        RrnLast       S2nrr_xx
231800160704     c                   eval      SflULT = S2nrr_xx
231900160704     C                   CLEAR                   QUSEI
232000160704     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
232100160704     C                   CALL      'QLGSRTIO'
232200160704     C                   PARM                    QLGSCB00
232300160704     C                   PARM                    NotUsed
232400160704     C                   PARM                    SflRcd
232500160704     C                   PARM                    QLGRL00
232600160704     C                   PARM                    NotUsed
232700160704     C                   PARM                    QUSEC
232800160704      * SFLnxtCHG
232900160704     c                   if        opzkey <> *blank
233000160704     c                   seton                                        41
233100160704     c                   else
233200160704     C                   SetOFF                                       41
233300160704     c                   end
233400160704      *
233500160704     c                   eval      *in75 = Hin75
233600160704     c                   eval      *in76 = Hin76
233700160704     C                   WRITE     BS12D05
233800160704     C                   ENDDO
233900160704      *
234000160704      *  All'uscita di queste chiamate a routine di sistema il SFL record
234100160704      *   si incrementa stranamente quindi lo reimposto correttamente
234200160704      *    prima di lasciare la routine.
234300160704     C                   EVAL      SflULT= RrnLast
234400160704     C*
234500160704     C                   ENDSR
234600160704     C/EJECT
234700160704     C*-----------------------------------------------------*
234800970526     C*------------------------------------------------------------------------*
234900970526     C* NOTE COSTRUZIONE SCHIERE
235000970526     C*------------------------------------------------------------------------*
235100970526     C*
235200970526     C* -MSG-MESSAGGI
235300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
235400970526     C* (NON TOCCARE MSG,1 E MSG,2)
235500970526     C*
235600970526     C* -CMD-COMANDI
235700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
235800970526     C*
235900970526     C* -OPD-OPZIONI
236000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
236100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
236200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
236300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
236400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
236500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
236600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
236700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
236800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
236900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
237000970526     C*
237100970526     C* -FUD-FUNZIONI
237200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
237300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
237400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
237500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
237600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
237700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
237800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
237900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
238000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
238100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
238200970526     O*------------------------------------------------------------------------*
238300970526** DEC - DECODIFICHE
238400970526    GESTIONE                                                     1
238500970526INTERROGAZIONE                                                   2
238600970530     SCELTA                                                      3
2387009705264
2388009705265
238900970526** MSG - MESSAGGI
239000991001TIBS12R- Opzione non gestita                                                   1
239100991001TIBS12R- Tasto funzionale non gestito                                          2
239200000307TIBS12R-                                                                       3
239300991001TIBS12R- Record annullato. Consentito solo il ripristino                       4
239400991001TIBS12R- Record non annullato. Impossibile ripristinare                        5
239500991001TIBS12R- Accettato solo un parametro di inizio lista                           6
239600060113TIBS12R- Errore in caricamento dati utente, avvisare il CED                    7
239700991001TIBS12R- 8                                                                     8
239800991001TIBS12R- 9                                                                     9
239900991001TIBS12R- 10                                                                   10
240000000307TIBS12R- Tipo legame errato.                                                  11
240100000307TIBS12R- Data errata.                                                         12
240200000807TIBS12R- Tipo legame non autorizzato.                                         13
240300060113TIBS12R- Utente non abilitato alla gestione del cliente "Padre" scelto.       14
240400991001TIBS12R- 15                                                                   15
240500991001TIBS12R- 16                                                                   16
240600991001TIBS12R- 17                                                                   17
240700991001TIBS12R- 18                                                                   18
240800991001TIBS12R- 19                                                                   19
240900991001TIBS12R- 20                                                                   20
241000970528** CMD - COMANDI CL
241100970528WRKSPLF
241200970528
241300970528
241400970528
241500970528
241600970526** OPD - OPZIONI
241700970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
241800970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
241900991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
242000000307 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
242100970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
242200970526                   -6 -     (S/N)=N                        opz. 06¬
242300000307 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
242400970527                   -8 -     (S/N)=N                        opz. 08¬
242500000607 9=Cliente abili.  -9 -     (S/N)=N                        opz. 09¬
242600970527                   -10-     (S/N)=N                        opz. 10¬
242700970526                   -  -     (S/N)=N                        opz. 11¬
242800970526                   -  -     (S/N)=N                        opz. 12¬
242900970526                   -  -     (S/N)=N                        opz. 13¬
243000970526                   -  -     (S/N)=N                        opz. 14¬
243100970526                   -  -     (S/N)=N                        opz. 15¬
243200970526                   -  -     (S/N)=N                        opz. 16¬
243300970526                   -  -     (S/N)=N                        opz. 17¬
243400970526                   -  -     (S/N)=N                        opz. 18¬
243500970526                   -  -     (S/N)=N                        opz. 19¬
243600970526                   -  -     (S/N)=N                        opz. 20¬
243700970526                   -  -     (S/N)=N                        opz. 21¬
243800970526                   -  -     (S/N)=N                        opz. 22¬
243900970526                   -  -     (S/N)=N                        opz. 23¬
244000970526                   -  -     (S/N)=N                        opz. 24¬
244100970526** FUD - FUNZIONI
244200970526                   -        (S/N)=N                        funz.01¬
244300970526                   -        (S/N)=N                        funz.02¬
244400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
244500970526                   -        (S/N)=N                        funz.04¬
244600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
244700970526F06=Inserimento    -        (S/N)=N                        funz.06¬
244800970526                   -        (S/N)=N                        funz.07¬
244900970527                   -        (S/N)=N                        funz.08¬
245000970527                   -        (S/N)=N                        funz.09¬
245100970527                   -        (S/N)=N                        funz.10¬
245200970526                   -        (S/N)=N                        funz.11¬
245300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
245400970526                   -        (S/N)=N                        funz.13¬
245500970526                   -        (S/N)=N                        funz.14¬
245600970526                   -        (S/N)=N                        funz.15¬
245700970526                   -        (S/N)=N                        funz.16¬
245800970526                   -        (S/N)=N                        funz.17¬
245900970526                   -        (S/N)=N                        funz.18¬
246000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
246100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
246200000808F21=Stampa elenco  -        (S/N)=N                        funz.21¬
246300970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
246400970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
246500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
