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
001200000731     Ftikun01l  IF   E           K DISK
001300000731     Ftikun02l  IF   E           K DISK    rename(tikun000:tikun2)
001400000731     Ftikun04l  IF   E           K DISK    rename(tikun000:tikun4)
001500000731     Ftikun05l  IF   E           K DISK    rename(tikun000:tikun5)
001600020927     Ftivss05l  IF   E           K DISK    usropn
001700000307     Ftntbe01l  IF   E           K DISK
001800970526     D*------------------------------------------------------------------------*
001900970526     D* SCHIERE
002000970526     D*------------------------------------------------------------------------*
002100970526     D*------------
002200970526     D* DECODIFICHE
002300970526     D*------------
002400970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* MESSAGGI
002700970526     D*------------
002800970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002900970528     D*------------
003000970528     D* COMANDI
003100970528     D*------------
003200970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* DESCRIZIONI OPZIONI
003500970526     D*------------
003600970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003700970526     D*------------
003800970526     D* DESCRIZIONI TASTI FUNZIONALI
003900970526     D*------------
004000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004100970526     D*------------
004200970526     D* OPZIONI DA CARICARE A VIDEO
004300970526     D*------------
004400970526     D OPV             S             19    DIM(24)
004500970526     D*------------
004600970526     D* FUNZIONI DA CARICARE A VIDEO
004700970526     D*------------
004800970527     D FUV             S             19    DIM(24)
004900020709     D*------------
005000060113     D* SCHIERA P.O. IN GESTIONE ALL'UTENTE
005100020709     D*------------
005200060113     D skPOG           S              3    DIM(250) INZ(*zeros)
005300970526     D*------------------------------------------------------------------------*
005400970526     D* INPUT
005500970526     D*------------------------------------------------------------------------*
005600970526     D*------------
005700970526     D* RIDEFINIZIONE OPZIONI VIDEO
005800970526     D*------------
005900970526     D OPDDS           DS
006000970526     D  OPDOPZ                 1      2
006100970526     D  OPDDE1                 3     19
006200970526     D  OPDOP1                21     22
006300970526     D  OPDFI0                23     34
006400970526     D  OPDATT                35     35
006500970526     D  OPDFI1                36     80
006600970526     D  OPDDES                 1     19
006700970526     D  OPDRIG                 1     80
006800970526     D*------------
006900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007000970526     D*------------
007100970526     D FUDDS           DS
007200970526     D  FUDDE1                 1      1
007300970526     D  FUDFUN                 2      3
007400970526     D  FUDDE2                 4     19
007500970526     D  FUDFI0                20     34
007600970526     D  FUDATT                35     35
007700970526     D  FUDFI1                36     80
007800970526     D  FUDDES                 1     19
007900970526     D  FUDRIG                 1     80
008000970526     D*------------
008100970526     D* DS GESTIONE TASTI FUNZIONALI
008200970526     D*------------
008300970526     D DSKEY         E DS
008400970526     D DEVDS1          DS
008500970526     D  KEY                  369    369
008600970526     D*------------
008700970526     D* DS "XMSG" - GESTIONE MESSAGGI
008800970526     D*------------
008900970526     D DSMSG         E DS
009000020212     D*------------
009100020212     D* DS X LETTURA NUOVA TABELLA ISV
009200020212     D*------------
009300020212     D DSISV         E DS                  EXTNAME(DISV)
009400100322     D*------------
009500100322     D* DS "XSRDA8" - CONTROLLA DATA (8)
009600100322     D*-------------
009700100322     D WLBDA8          DS                  INZ
009800100322     D  G08DAT                 1      8  0
009900100322     D  G08INV                 9     16  0
010000100322     D  G08ERR                17     17
010100100322     D  G08TGI                18     22  0
010200970526     D*------------
010300970526     D* DS DI PROCEDURA
010400970526     D*------------
010500991001     D tibds         E DS                  EXTNAME(TIbs12ds)
010600000607     D tis720        E DS                  EXTNAME(TIs720ds)
010700020212     D tibs02ds      E DS
010800991015     D dstle         E DS                  extname(dtle)
010900970526     D*------------
011000970526     D* ARCHITETTURA
011100970526     D*------------
011200970526     D KPJBA         E DS
011300000807     D*------------
011400000807     D* TABELLE "XCLIR"- RICERCA CLIENTI
011500000807     D*------------
011600000807     D ksc             s              7  0 dim(30)
011700000807     D ksa             s              4    dim(30)
011800020212     D isvkun          s              2    dim(100)
011900000807     D*------------------
012000000807     D* DS "XCLIR" - INTERROGAZIONE CLIENTI
012100000807     D*------------------
012200020212     D DSXC          e DS                  extname(XCLIRDS)
012300020212     D*------------------
012400000807     D                 DS
012500000807     D dsksn                   1      4p 0
012600000807     D dsksa                   1      4
012700000807     D*------------------
012800000807     D* DS REPERIMENTO DATI UTENTE
012900000807     D*-------------------
013000020212     D BS69DS        e ds                  extname(TIBS69DS)
013100020212     D ACODS         e ds                  extname(cnaco00f)
013200020212     D INDDS         e ds                  extname(cnind00f)
013300020212     D CLPDS         e ds                  extname(cnclp00f)
013400020212     D CLSDS         e ds                  extname(fncls00f)
013500000307     D*------------
013600000307     D* SCOMPOSIZIONE CODICI CLIENTI IN FORMATO "STRATEGI"
013700000307     D*------------
013800000307     D                 DS
013900020905     D  FILCLI                 5      7  0
014000000307     D  STRATECLI              1     11  0
014100000807     D*------------------
014200000807     D* VARIABILI DI WRK
014300000807     D*-------------------
014400000807     D tipprf          s              3    inz(*blanks)
014500020905     D*------------
014600020905     D* DS REPERIMENTO DATI UTENTE
014700020905     D*------------
014800020905     D TIBS34DS      E DS
014900020905     D DDATIUTE      E DS
015000020905     D AZUTEDS       E DS                  extname(AZUTE00F)
015100060113     d dute01        e ds
015200060113     d trul31ds      e ds
015300970526     C*------------------------------------------------------------------------*
015400970526     C* MAIN LINES
015500970526     C*------------------------------------------------------------------------*
015600970526     C*
015700970526     C* SE NON CI SONO ERRORI, PROSEGUE
015800970526IF  1C     ERRL00        IFNE      '1'
015900970526     C*
016000970526     C* CARICA IL SFILE
016100970526     C                   EXSR      POSSFL
016200970526     C*
016300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016700970526     C*
016800970526     C* REIMPOSTA ERRORE DI PROCERURA
016900970526     C                   MOVEL     '0'           ERRL00
017000970526     C*
017100970526     C* EMETTE IL SFILE
017200970526     C                   EXSR      OUTSFL
017300970526     C*
017400970526     C* GESTIONE TASTI FUNZIONALI
017500970526     C                   EXSR      FUNCON
017600970526     C*
017700970526E   2C                   ENDDO
017800970527     C*
017900970527     C* CI SONO ERRORI IN ENTRATA
018000970527X   1C                   ELSE
018100991001     C                   MOVEL     '1'           k12ERR
018200991001     C                   MOVEL     DSMSMS        k12MSG
018300970526E   1C                   ENDIF
018400970526     C* FINE PGM
018500991210     c                   exsr      finsr
018600970526     C                   SETON                                        LR
018700970526     C*------------------------------------------------------------------------*
018800970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
018900970526     C*------------------------------------------------------------------------*
019000970526     C     F03GES        BEGSR
019100970526     C*
019200970526     C* IMPOSTA "PREMUTO F03"
019300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019400991001     C                   MOVEL     '1'           k12F03                         *DI PROCEDURA
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F05GES        BEGSR
020100970526     C*
020200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F06GES        BEGSR
020900970526     C*
021000970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
021100970526     C*
021200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021300991001     C                   MOVEL     *BLANKS       Dtle
021400991001     C                   Z-ADD     *ZEROS        Dcop
021500991001     C                   Z-ADD     *ZEROS        Dcof
021600991001     C                   Z-ADD     *ZEROS        Ddde
021700970602     C                   MOVEL     DEPOP0        WOP0
021800970529     C                   MOVEL     'F06'         WOP1
021900991001     C                   MOVEL     'TIBS13R'     WPGM              8
022000970526     C                   EXSR      LANPGM                                       *LANCIA PGM
022100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
022200970526E   1C                   ENDIF
022300970526     C*
022400970526     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* F12GES - GESTIONE F12 -> RITORNO
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F12GES        BEGSR
022900970526     C*
023000970526     C* IMPOSTA "PREMUTO F12"
023100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023200991001     C                   MOVEL     '1'           k12F12                         *DI PROCEDURA
023300970526     C*
023400970526     C                   ENDSR
023500970528     C*------------------------------------------------------------------------*
023600970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
023700970528     C*------------------------------------------------------------------------*
023800970528     C     F21GES        BEGSR
023900970612     C*
024000991001     C                   MOVEL     *BLANKS       Dtle
024100991001     C                   Z-ADD     *ZEROS        Dcop
024200991001     C                   Z-ADD     *ZEROS        Dcof
024300991001     C                   Z-ADD     *ZEROS        Ddde
024400970612     C                   MOVEL     DEPOP0        WOP0
024500970612     C                   MOVE      '03'          WOP0
024600970612     C                   MOVEL     'F21'         WOP1
024700991011     C                   MOVEL     'TIBS14R'     WPGM              8
024800000808     C*                  EXSR      LANPGM
024900970528     C*
025000970528     C                   ENDSR
025100970528     C*------------------------------------------------------------------------*
025200970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
025300970528     C*------------------------------------------------------------------------*
025400970528     C     F22GES        BEGSR
025500970528     C*
025600970528     C                   Z-ADD     80            LENGH            15 5
025700970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
025800970528     C                   PARM                    CMD(1)
025900970528     C                   PARM                    LENGH
026000970528     C*
026100970528     C                   ENDSR
026200970526     C*------------------------------------------------------------------------*
026300970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
026400970526     C*------------------------------------------------------------------------*
026500970526     C     F23GES        BEGSR
026600970526     C*
026700970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
026800970526     C*
026900970526     C                   ENDSR
027000970526     C*------------------------------------------------------------------------*
027100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
027200970526     C*------------------------------------------------------------------------*
027300970526     C     F24GES        BEGSR
027400970526     C*
027500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
027600970526     C*
027700970526     C                   ENDSR
027800970526     C*------------------------------------------------------------------------*
027900970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
028000970526     C*------------------------------------------------------------------------*
028100970526     C     RUPGES        BEGSR
028200970526     C*
028300970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
028400970526     C*
028500970526     C                   ENDSR
028600970526     C*------------------------------------------------------------------------*
028700970526     C* ENTGES - GESTIONE ENTER -> INVIO
028800970526     C*------------------------------------------------------------------------*
028900970526     C     ENTGES        BEGSR
029000970526     C*
029100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
029200970526     C                   EXSR      NEWPOS
029300970526     C*
029400970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
029500020212     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029600020212     C     SWPOSI        IFNE      0
029700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
029800020212     C                   ELSE
029900020212     C     SFLULT        IFNE      0
030000970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
030100970526     C*
030200970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
030300020212     C     SWMODI        IFNE      0
030400970526     C                   EXSR      POSSFL
030500020212     C                   ENDIF
030600020212     C                   ENDIF
030700020212     C                   ENDIF
030800020212     C                   ENDIF
030900970526     C*
031000970526     C                   ENDSR
031100970526     C*------------------------------------------------------------------------*
031200970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
031300970526     C*------------------------------------------------------------------------*
031400970526     C     O01GES        BEGSR
031500970526     C*
031600970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
031700991001     C                   MOVEL     vidtle        k12tle
031800991001     C                   move      vidazp        k12azp
031900991001     C                   Z-ADD     vidkop        k12cop
032000991001     C                   move      vidazf        k12azf
032100991001     C                   Z-ADD     vidcof        k12cof
032200991011     C                   Z-ADD     vihdde        k12dde
032300991011     C                   Z-ADD     vihdsc        k12dsc
032400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032500970526     C*
032600970526     C                   ENDSR
032700970526     C*------------------------------------------------------------------------*
032800970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
032900970526     C*------------------------------------------------------------------------*
033000970526     C     O02GES        BEGSR
033100970526     C*
033200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
033300020212     C     VIDvat        IFEQ      'A'
033400970526     C                   MOVEL     '1'           ERRL00
033500970526     C                   MOVEL     MSG(4)        DSMSMS
033600020212     C                   ENDIF
033700970527     C* MODIFICA
033800020212     C     ERRL00        IFEQ      '0'
033900000807     C*
034000000807     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (FILIALE/TIPO LEGAME/CLIENTI)
034100000807     C                   EXSR      CHKAUT
034200020212     C     ERRL00        IFEQ      '0'
034300020212     C                   EXSR      MOVCAMP
034400970602     C                   MOVEL     DEPOP0        WOP0
034500970526     C                   MOVEL     'O02'         WOP1
034600991011     C                   MOVEL     'TIBS13R'     WPGM
034700970526     C                   EXSR      LANPGM
034800020212     C                   ENDIF
034900020212     C                   ENDIF
035000970526     C*
035100970526     C                   ENDSR
035200991014     C*------------------------------------------------------------------------*
035300991014     C* O03GES - GESTIONE OPZIONE 3=copia
035400991014     C*------------------------------------------------------------------------*
035500991014     C     O03GES        BEGSR
035600991014     C*
035700991014     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
035800020212     C     VIDvat        IFEQ      'A'
035900991014     C                   MOVEL     '1'           ERRL00
036000991014     C                   MOVEL     MSG(4)        DSMSMS
036100020212     C                   ENDIF
036200000307     C* COPIA
036300020212     C     ERRL00        IFEQ      '0'
036400000807     C*
036500000807     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (FILIALE/TIPO LEGAME/CLIENTI)
036600000807     C                   EXSR      CHKAUT
036700020212     C     ERRL00        IFEQ      '0'
036800020212     C                   EXSR      MOVCAMP
036900991014     C                   MOVEL     DEPOP0        WOP0
037000991014     C                   MOVEL     'O03'         WOP1
037100991014     C                   MOVEL     'TIBS13R'     WPGM
037200991014     C                   EXSR      LANPGM
037300020212     C                   ENDIF
037400020212     C                   ENDIF
037500991014     C*
037600991014     C                   ENDSR
037700970526     C*------------------------------------------------------------------------*
037800970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
037900970526     C*------------------------------------------------------------------------*
038000000307     C     O04GES        BEGSR
038100970526     C*
038200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
038300020212     C     VIDvat        IFEQ      'A'
038400000307     C                   MOVEL     '1'           ERRL00
038500000307     C                   MOVEL     MSG(4)        DSMSMS
038600020212     C                   ENDIF
038700970527     C* ANNULLAMENTO
038800020212     C     ERRL00        IFEQ      '0'
038900000807     C*
039000000807     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (FILIALE/TIPO LEGAME/CLIENTI)
039100000807     C                   EXSR      CHKAUT
039200020212     C     ERRL00        IFEQ      '0'
039300020212     C                   EXSR      MOVCAMP
039400000307     C                   MOVEL     DEPOP0        WOP0
039500000307     C                   MOVEL     'O04'         WOP1
039600000307     C                   MOVEL     'TIBS13R'     WPGM
039700000307     C                   EXSR      LANPGM
039800020212     C                   ENDIF
039900020212     C                   ENDIF
040000970526     C*
040100000307     C                   ENDSR
040200970526     C*------------------------------------------------------------------------*
040300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
040400970526     C*------------------------------------------------------------------------*
040500970526     C     O05GES        BEGSR
040600970526     C*
040700020212     C                   EXSR      MOVCAMP
040800970602     C                   MOVEL     DEPOP0        WOP0
040900970526     C                   MOVEL     'O05'         WOP1
041000991011     C                   MOVEL     'TIBS13R'     WPGM
041100970526     C                   EXSR      LANPGM
041200970526     C*
041300970526     C                   ENDSR
041400970526     C*------------------------------------------------------------------------*
041500970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
041600970526     C*------------------------------------------------------------------------*
041700970526     C     O07GES        BEGSR
041800970526     C*
041900970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
042000020212     C     VIDvat        IFNE      'A'
042100970526     C                   MOVEL     '1'           ERRL00
042200970526     C                   MOVEL     MSG(5)        DSMSMS
042300020212     C                   ENDIF
042400970526     C* RIPRISITINO
042500020212     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042600000807     C*
042700000807     C* VERIFICA ESISTENZA TUTTE AUTORIZZAZIONI (FILIALE/TIPO LEGAME/CLIENTI)
042800000807     C                   EXSR      CHKAUT
042900020212     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
043000020212     C                   EXSR      MOVCAMP
043100970602     C                   MOVEL     DEPOP0        WOP0
043200970526     C                   MOVEL     'O07'         WOP1
043300991011     C                   MOVEL     'TIBS13R'     WPGM
043400970526     C                   EXSR      LANPGM
043500020212     C                   ENDIF
043600020212     C                   ENDIF
043700970526     C*
043800970526     C                   ENDSR
043900000607     C*------------------------------------------------------------------------*
044000000607     C* O09GES - GESTIONE OPZIONE 9=Richiamo pgm cliente padre abilitato
044100000607     C*------------------------------------------------------------------------*
044200000607     C     O09GES        BEGSR
044300000607     C*
044400020212     C* CLIENTE ABILITATO
044500020212     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
044600000607     C                   MOVEL     VIDtle        Dtle
044700020212     C                   Z-ADD     VIDkop        Dcop
044800000607     C                   MOVEL     DEPOP0        WOP0
044900000607     C                   MOVEL     'O09'         WOP1
045000000607     C                   MOVEL     'TIS724R'     WPGM
045100000607     C                   EXSR      LANPGM2
045200020212     C                   ENDIF
045300000607     C*
045400000607     C                   ENDSR
045500000307     C*------------------------------------------------------------------------*
045600000307     C* CHKAUT - CONTROLLO AUTORIZZAZIONI FILIALI/LEGAMI
045700000307     C*------------------------------------------------------------------------*
045800000307     C     CHKAUT        BEGSR
045900000807     C*
046000020709     C* CONTROLLO SE UTENTE PUO' GESTIRE IL TIPO LEGAME SCELTO
046100000807     C                   eval      tipprf =  %subst(%trim(KNMUS):1:3)
046200020709     C     DUTLPO        ifeq      'S'
046300000807     C* X SEDE
046400000807     C     ERRL00        ifeq      '0'
046500000807     C                   movel     'TLE'         KTBcod
046600000807     C                   movel(P)  vidtle        KTBky1
046700000807     C     KEYTBE        chain     tntbe01l                           70
046800000807     C     *in70         ifeq      *off
046900000807     C     tbeatb        andeq     *blanks
047000000807     C                   movel     tbeuni        dstle
047100000807     C                   if        §tleaus = 'S' or
047200000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
047300000807     C                   else
047400000807     C                   MOVEL     '1'           ERRL00
047500000807     C                   MOVEL     MSG(13)       DSMSMS
047600000807     C                   endif
047700000807     C                   else
047800000807     C                   MOVEL     '1'           ERRL00
047900000807     C                   MOVEL     MSG(11)       DSMSMS
048000000807     C                   endif
048100000807     C                   endif
048200020709     C* X FILIALE
048300000807     C                   else
048400000807     C     ERRL00        ifeq      '0'
048500000807     C                   movel     'TLE'         KTBcod
048600000807     C                   movel(P)  vidtle        KTBky1
048700000807     C     KEYTBE        chain     tntbe01l                           70
048800000807     C     *in70         ifeq      *off
048900000807     C     tbeatb        andeq     *blanks
049000000807     C                   movel     tbeuni        dstle
049100000807     C                   if        §tleaut = 'S' or
049200000807     C                             (tipprf = 'EDP' and §tleaue = 'S')
049300000807     C                   else
049400000807     C                   MOVEL     '1'           ERRL00
049500000807     C                   MOVEL     MSG(13)       DSMSMS
049600000807     C                   endif
049700000807     C                   else
049800000807     C                   MOVEL     '1'           ERRL00
049900000807     C                   MOVEL     MSG(11)       DSMSMS
050000000807     C                   endif
050100000807     C                   endif
050200020709     C                   endif
050300000807     C*
050400020709     C* CONTROLLO SE UTENTE PUO' GESTIRE IL CLIENTE PADRE SCELTO
050500001018     C     ERRL00        ifeq      '0'
050600020709     C     vidkop        ifne      *zeros
050700020709     C                   move(p)   vidkop        STRATECLI
050800060113     c                   move      filcli        w003a             3
050900060113     c     w003a         lookup    skpog                                  55
051000020709     C                   if        not *in55
051100020709     C                   movel     '1'           ERRL00
051200020709     C                   movel     MSG(14)       DSMSMS
051300020709     C                   endif
051400020709     C                   endif
051500020709     C                   endif
051600000307     C*
051700000307     C                   ENDSR
051800000307     C*------------------------------------------------------------------------*
051900000307     C* MOVCAMP - VALORIZZAZIONE CAMPI
052000000307     C*------------------------------------------------------------------------*
052100000307     C     MOVCAMP       BEGSR
052200000307     C*
052300991001     C                   MOVEL     VIDtle        Dtle
052400991001     C                   MOVEL     VIDazp        Dazp
052500991001     C                   Z-ADD     VIDkop        Dcop
052600991001     C                   MOVEL     VIDazf        Dazf
052700991001     C                   Z-ADD     VIDcof        Dcof
052800991011     C                   Z-ADD     VIhDDE        Ddde
052900991011     C                   Z-ADD     VIHDsc        Ddsc
053000000307     C*
053100000307     C                   ENDSR
053200970526     C*------------------------------------------------------------------------*
053300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
053400970526     C*------------------------------------------------------------------------*
053500970526     C     NEWPOS        BEGSR
053600970526     C*
053700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
053800970526     C*         1= RICHIESTO POSIZIONAMENTO
053900970526     C                   Z-ADD     0             SWPOSI            7 0
054000970526     C*
054100970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
054200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
054300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
054400970526     C                   Z-ADD     0             SWMODI            1 0
054500970530     C*
054600970530     C* REIMPOSTA INDICATORI DI ERRORE
054700970530     C                   SETOFF                                       808182
054800000215     C                   SETOFF                                       838485
054900000215     C                   SETOFF                                       86
055000970526     C*
055100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
055200021114     C                   IF        VIDtl2 <> DEPtl2 OR
055300021114     C                             VIDuni <> DEPuni OR
055400021114     C                             VIDele <> DEPele OR
055500021114     C                             VIDel2 <> DEPel2 OR
055600021114     C                             VIDun2 <> DEPun2 OR
055700021114     C                             VIDpdd <> DEPpdd OR
055800021114     C                             %SCAN('?':VIDUNI) > 0 OR
055900021114     C                             %SCAN('?':VIDUN2) > 0 OR
056000021114     C                             %SCAN('?':VIDELE) > 0 OR
056100021114     C                             %SCAN('?':VIDEL2) > 0
056200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
056300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
056400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
056500000308     C                   movel     VIDtl2        DEPtl2                         *DEPOSITI
056600991011     C                   move      VIDuni        DEPuni
056700991011     C                   move      VIDele        DEPele
056800000308     C                   move      VIDpdd        DEPpdd                         *DEPOSITI
056900991013     C                   move      VIDun2        DEPun2
057000991013     C                   move      VIDel2        DEPel2
057100970526E   2C                   ENDIF
057200970526E   1C                   ENDIF
057300970526     C*
057400970526     C                   ENDSR
057500970526     C*------------------------------------------------------------------------*
057600970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
057700970526     C*------------------------------------------------------------------------*
057800970528     C     SETFIL        BEGSR
057900970526     C*
058000970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
058100000731     C                   MOVEL     *blanks       KCEtle
058200000731     C                   MOVEL     *blanks       KCEazp
058300000731     C                   MOVEL     *blanks       KCEazf
058400000731     C                   MOVEL(p)  PDDvid        KCEdde
058500000731     C                   MOVEL(p)  VIDuni        KCEcop
058600000731     C                   MOVEL(p)  VIDele        KCEcof
058700000731IF  1C     WTIPRI        IFEQ      'UNI'
058800000731     C     KE1kun        SETLL     tikun01l                           99
058900970528E   1C                   ENDIF
059000970528     C*
059100000731IF  1C     WTIPRI        IFEQ      'ELE'
059200000731     C     KE2kun        SETLL     tikun02L                           99
059300970528E   1C                   ENDIF
059400000731     C*
059500000731IF  1C     WTIPRI        IFEQ      'DAU'
059600000731     C     KE4kun        SETLL     tikun04l                           99
059700000731E   1C                   ENDIF
059800000731     C*
059900000731IF  1C     WTIPRI        IFEQ      'DAE'
060000000731     C     KE5kun        SETLL     tikun05L                           99
060100000731E   1C                   ENDIF
060200980107     C*
060300970526     C* LETTURA RECORD
060400970526IF  1C     *IN99         IFEQ      *OFF
060500970526     C                   EXSR      LETFIL
060600970526E   1C                   ENDIF
060700970526     C*
060800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
060900970526IF  1C     *IN99         IFEQ      *ON
061000970526     C                   Z-ADD     1             SFLEOF
061100970526E   1C                   ENDIF
061200970526     C*
061300970526     C                   ENDSR
061400970526     C*------------------------------------------------------------------------*
061500970526     C* LETFIL - LETTURA RECORD
061600970526     C*------------------------------------------------------------------------*
061700970526     C     LETFIL        BEGSR
061800970526     C*
061900970526     C* LEGGE FINO A:
062000970526     C*     - FINE FILE
062100970526     C*     - TROVATO RECORD VALIDO
062200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
062300970526DO  1C     *IN99         DOWEQ     *OFF
062400970526     C     WRECOK        ANDEQ     'N'
062500970526     C*
062600970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
062700000731IF  2C     WTIPRI        IFEQ      'UNI'
062800000731     C                   READ      tikun01l                               99
062900970528E   2C                   ENDIF
063000970526     C*
063100000731IF  2C     WTIPRI        IFEQ      'ELE'
063200000731     C                   READ      tikun02L                               99
063300970528E   2C                   ENDIF
063400000731     C*
063500000731IF  2C     WTIPRI        IFEQ      'DAU'
063600000731     C                   READ      tikun04l                               99
063700000731E   2C                   ENDIF
063800000731     C*
063900000731IF  2C     WTIPRI        IFEQ      'DAE'
064000000731     C                   READ      tikun05L                               99
064100000731E   2C                   ENDIF
064200970528     C*
064300970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
064400970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
064500970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
064600970530X   2C                   ELSE
064700970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
064800970530E   2C                   ENDIF
064900970526E   1C                   ENDDO
065000970526     C*
065100970526     C                   ENDSR
065200970526     C*------------------------------------------------------------------------*
065300970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
065400970526     C*------------------------------------------------------------------------*
065500970526     C     CHKREC        BEGSR
065600970526     C*
065700970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
065800100322     C                   setoff                                       7675
065900970530     C*
066000000731     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
066100000731     C*
066200000731     C     WRECOK        ifeq      'S'
066300000731     C     kundde        iflt      PDDvid
066400000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
066500000731     C                   endif
066600000731     C                   endif
066700000731     C*
066800000731     C     WRECOK        ifeq      'S'
066900000731     C                   move(p)   VIDuni        uninum           11 0
067000000731     C     kuncop        iflt      uninum
067100000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
067200000731     C                   endif
067300000731     C                   endif
067400000731     C*
067500000731     C     WRECOK        ifeq      'S'
067600000731     C                   move(p)   VIDele        elenum           11 0
067700000731     C     kuncof        iflt      elenum
067800000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
067900000731     C                   endif
068000000731     C                   endif
068100000731     C*
068200000731     C     WRECOK        ifeq      'S'
068300000731     C     VIDtl2        ifne      *blanks
068400000731     C     kuntle        ifne      VIDtl2
068500000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
068600000731     C                   endif
068700000731     C                   endif
068800000731     C                   endif
068900000731     C*
069000000731     C     WRECOK        ifeq      'S'
069100000731     C                   move      kuncop        alfa11           11
069200000731     C     VIDun2        ifeq      *zeros
069300000731     C     VIDun2        oreq      *blank
069400000731     C                   else
069500000731     C     VIDun2        ifne      alfa11
069600000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
069700000731     C                   endif
069800000731     C                   endif
069900000731     C                   endif
070000000731     C*
070100000731     C     WRECOK        ifeq      'S'
070200000731     C                   move      kuncof        alfa11
070300000731     C     VIDel2        ifeq      *zeros
070400000731     C     VIDel2        oreq      *blank
070500000731     C                   else
070600000731     C     VIDel2        ifne      alfa11
070700000731     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
070800000731     C                   endif
070900000731     C                   endif
071000000731     C                   endif
071100020212     C*
071200020927     C* Il controllo seguente lo effettuo solo se trattasi di utente di sede in quanto in filiale
071300020927     C* il file TIVSS nn c'è
071400091117     C*                  if        simfel = *zeros
071500091117     C*    WRECOK        ifeq      'S'
071600091117     C*                  if        kuntle = 'WW'    and
071700091117     C*                            datcor >= kundde and
071800091117     C*                            datcor <= kundsc
071900091117     C*                  move      kuncof        kssksu
072000091117     C*    keyvss        setll     tivss05l
072100091117     C*    keyvss        reade     tivss05l
072200091117     C*                  dow       not %eof(tivss05l)
072300091117     C*                  if        vssvat = *blanks                              *non annull/sosp.
072400091117     C*                  if        vssdsc < kundde or                            *non valido
072500091117     C*                            vssdde > kundsc
072600091117     C*                  else                                                    *valido
072700091117     C* Verifico che il servizio del record corrente del TIVSS sia soggetto a controllo unificazioni
072800091117     C*    vssisv        lookup    isvkun                                 22
072900091117     C*                  if        *in22 = *on
073000091117     C*                  seton                                        76
073100091117     C*                  leave
073200091117     C*                  endif
073300091117     C*                  endif
073400091117     C*                  endif
073500091117     C*    keyvss        reade     tivss05l
073600091117     C*                  enddo
073700091117     C*                  endif
073800091117     C*                  endif
073900091117     C*                  endif
074000100322     C*
074100100322     C                   if        WRECOK = 'S'
074200100322     C                   if        kundsc < datcor
074300100322     C                   seton                                        75
074400100322     C                   endif
074500100322     C                   endif
074600100322     C*
074700970526     C                   ENDSR
074800970526     C*------------------------------------------------------------------------*
074900970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
075000970526     C*------------------------------------------------------------------------*
075100970526     C     IRESFL        BEGSR
075200970526     C*
075300970526     C                   MOVEL     *BLANKS       OPZKEY
075400991001     C                   Z-ADD     kunDDE        VIHDDE
075500991011     C                   Z-ADD     kunDsc        VIHDsc
075600991001     C                   MOVEL     kuntle        VIDtle
075700991001     C                   MOVEL     kunazp        VIDazp
075800991001     C                   Z-ADD     kuncop        VIDkop
075900991001     C                   MOVEL     kunazf        VIDazf
076000991001     C                   Z-ADD     kuncof        VIDcof
076100991001     C                   move      kunvat        VIDvat
076200991001     C                   Z-ADD     kundde        G08INV
076300970618     C                   Z-ADD     *ZEROS        G08DAT
076400970618     C                   MOVEL     '3'           G08ERR
076500970618     C                   CALL      'XSRDA8'
076600970618     C                   PARM                    WLBDA8
076700970618     C                   Z-ADD     G08DAT        VIDDDE
076800991001     C                   Z-ADD     kunDSC        G08INV
076900970618     C                   Z-ADD     *ZEROS        G08DAT
077000970618     C                   MOVEL     '3'           G08ERR
077100970618     C                   CALL      'XSRDA8'
077200970618     C                   PARM                    WLBDA8
077300970618     C                   Z-ADD     G08DAT        VIDDSC
077400991015     c                   move      vidtle        wvidtl2
077500991015     c                   exsr      cnttle
077600970528     C* DECODIFICHE
077700091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
077800091126      * deciso di non utilizzarli per le unificazioni dei codici
077900091126      * quindi non controllo il tipo codice e do per scontato che è sempre 'C'=cliente
078000991210     C                   move      kuncop        wcli                           -codice
078100991210     c                   exsr      dectib
078200991210     c                   movel     wfvrag        viddep
078300991210     C                   move      kuncof        wcli              7 0          -codice
078400991210     c                   exsr      dectib
078500020905     c                   movel     wfvrag        viddes
078600091126
078700970526     C                   ENDSR
078800991210     C*------------------------------------------------------------------------*
078900991210     C* Dectib - DECODIFICA CLIENTE
079000991210     C*------------------------------------------------------------------------*
079100991210     C     Dectib        BEGSR
079200991210     C*
079300991210     C                   CLEAR                   BS69DS
079400991210     C                   CLEAR                   ACODS
079500991210     C                   MOVEL     KNSIF         I69SIF
079600991210     C                   Z-ADD     WCLI          I69KAC
079700991210     C                   CALL      'TIBS69R'
079800991210     C                   PARM                    BS69DS
079900991210     C                   PARM                    ACODS
080000991210     C                   PARM                    INDDS
080100991210     C                   PARM                    CLPDS
080200991210     C                   PARM                    CLSDS
080300991210     C     O69ERR        IFNE      '1'
080400991210     C                   MOVEL     ACORAG        WFVRAG           40
080500991210     C                   ELSE
080600991210     C                   MOVEL     *ALL'*'       WFVRAG
080700991210     c                   movel     '1'           errl00
080800991210     C                   ENDIF
080900991210     C*
081000991210     C                   ENDSR
081100970526     C*------------------------------------------------------------------------*
081200970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
081300970526     C*------------------------------------------------------------------------*
081400970526     C     CHKPAR        BEGSR
081500970528     C*
081600970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
081700140909     C***                MOVEL     'DAU'         WTIPRI            3
081800140909     C                   MOVEL     'UNI'         WTIPRI            3
081900970528     C*
082000000731     C* CONTROLLA LA DATA DECORRENZA
082100000731     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
082200970528     C                   Z-ADD     *ZEROS        PDDVID
082300970528IF  1C     ERRL00        IFEQ      '0'
082400970528IF  2C     VIDPDD        IFGT      *ZEROS
082500970528     C                   Z-ADD     VIDPDD        G08DAT
082600970528     C                   Z-ADD     *ZEROS        G08INV
082700970528     C                   MOVEL     '0'           G08ERR
082800970528     C                   CALL      'XSRDA8'
082900970528     C                   PARM                    WLBDA8
083000970528     C                   Z-ADD     G08DAT        VIDPDD
083100970528     C                   Z-ADD     G08INV        PDDVID
083200970528IF  3C     G08ERR        IFEQ      '1'
083300970528     C                   MOVEL     '1'           ERRL00
083400970528     C                   MOVEL     MSG(12)       DSMSMS
083500970528     C                   SETON                                        83
083600970528E   3C                   ENDIF
083700970528E   2C                   ENDIF
083800970528E   1C                   ENDIF
083900970528     C*
084000000731     C* CONTROLLA IL TIPO RICERCA
084100000731     C                   move      *blank        wtlecu
084200000731     C                   move      *blank        wtlece
084300000731IF  1C     ERRL00        IFEQ      '0'
084400000731IF  2C     VIDtl2        IFNE      *blank
084500000731     C                   move      vidtl2        wvidtl2           2
084600000731     C                   exsr      cnttle
084700000731     C                   move      wvidtl2       vidtl2
084800000731E   2C                   ENDIF
084900000731E   1C                   ENDIF
085000000731     C*
085100000731     C     ERRL00        IFEQ      '0'
085200991012IF  2C     VIDuni        IFNE      *ZEROS
085300991012IF  2C     VIDuni        andne     *blank
085400021114     C                   MOVEL     'UNI'         WTIPRI
085500991015     c                   move      wtlecu        clipot            1
085600991012     C                   MOVEL     VIDuni        Wcop
085700991012     C                   EXSR      CHKcop
085800991012     C                   MOVEL     Wcop          VIDuni
085900021114     C     PDDvid        IFNE      *zeros
086000021114     C                   MOVEL     'DAU'         WTIPRI
086100021114     C                   ENDIF
086200000731E   2C                   ELSE
086300980107     C*
086400000731IF  1C     VIDele        IFNE      *ZEROS
086500991012IF  2C     VIDele        andne     *blank
086600991001     C                   MOVEL     'ELE'         WTIPRI
086700991015     c                   movel     wtlece        clipot
086800991012     C                   MOVEL     VIDele        Wcop
086900991012     C                   EXSR      CHKcop
087000991012     C                   MOVEL     Wcop          VIDele
087100000731     C     PDDvid        IFNE      *zeros
087200000731     C                   MOVEL     'DAE'         WTIPRI
087300000731     C                   ENDIF
087400980107E   2C                   ENDIF
087500000731E   1C                   ENDIF
087600000731     C                   ENDIF
087700021114     C*
087800021114     C     ERRL00        IFEQ      '0'
087900021114IF  2C     VIDun2        IFNE      *ZEROS
088000021114IF  2C     VIDun2        andne     *blank
088100021114     C                   move      wtlecu        clipot
088200021114     C                   MOVEL     VIDun2        Wcop
088300021114     C                   EXSR      CHKcop
088400021114     C                   MOVEL     Wcop          VIDun2
088500021114E   2C                   ELSE
088600021114     C*
088700021114IF  1C     VIDel2        IFNE      *ZEROS
088800021114IF  2C     VIDel2        andne     *blank
088900021114     c                   movel     wtlece        clipot
089000021114     C                   MOVEL     VIDel2        Wcop
089100021114     C                   EXSR      CHKcop
089200021114     C                   MOVEL     Wcop          VIDel2
089300021114E   2C                   ENDIF
089400021114E   1C                   ENDIF
089500021114     C                   ENDIF
089600000731     C*
089700970526     C                   ENDSR
089800991012     C*------------------------------------------------------------------------*
089900991012     C* Cnttle - CONTROLLA codice tipo legame
090000991012     C*------------------------------------------------------------------------*
090100000731     C     cnttle        begsr
090200000731     C                   clear                   TIBS02DS
090300000731     C*
090400991015     C     '?'           SCAN      wVidtl2                                30
090500000731     C*  Ricerca
090600000731     C                   eval      t02tla = 'L'
090700000731    1C                   IF        *IN30 = *ON
090800000731     C                   movel     'R'           t02mod
090900000731     C                   else
091000000731     C                   movel     'C'           t02mod
091100000731     C                   movel(p)  Wvidtl2       t02ke1
091200000731     C                   end
091300000731     C                   movel     'TLE'         t02cod
091400000731     C                   movel     knsif         t02sif
091500991012     C                   CALL      'TIBS02R'
091600000731     C                   parm                    KPJBA
091700000731     C                   parm                    TIBS02DS
091800000731     C
091900000731     C*
092000991012    2C                   IF        T02err = *blanks
092100991015     C                   movel     T02ke1        wVidtl2
092200991015     C                   movel     T02uni        dstle
092300991015     C                   movel     §tlecu        Wtlecu            1
092400991015     C                   movel     §tlece        wtlece            1
092500000731     C                   else
092600000731     C                   movel     '1'           errl00
092700000731     C                   seton                                        86
092800000215     C                   MOVEL     MSG(11)       DSMSMS
092900991015     C                   ENDIF
093000000731     C
093100000731     C                   endsr
093200991012     C*------------------------------------------------------------------------*
093300991012     C* CHKcop - CONTROLLA codice padre figlio
093400991012     C*------------------------------------------------------------------------*
093500991012     C     CHKcop        BEGSR
093600991012     C*
093700991012     C     *LIKE         DEFINE    VIDuni        Wcop
093800991012     C*
093900991012     C* RICHIESTO ELENCO
094000991012     C                   SETOFF                                           28
094100991012     C     '?'           SCAN      Wcop                                   28
094200991012IF  1C     *IN28         IFEQ      *ON
094300991012     C* RICHIESTO ELENCO CODICI
094400091126      * il tipo codice in tabella "TLE" è sempre Cliente in quanto i potenziali si è
094500091126      * deciso di non utilizzarli per le unificazioni dei codici
094600091126      * quindi non controllo il tipo codice e do per scontato che è sempre cliente
094700991012     C                   CLEAR                   DSXC
094800991012     C                   Z-ADD     KCCCL         DXCCAP
094900991012     C                   CALL      'XCLIR'
095000991012     C                   PARM                    DSXC
095100991012     C* SCOMPATTA
095200991012     C                   MOVEA     DXCKSC        KSA                        ALF
095300991012DO  2C     1             DO        30            J                 3 0
095400991012     C                   MOVE      KSA(J)        DSKSA                      F
095500991012     C                   Z-ADD     DSKSN         KSC(J)                     M
095600991012E   2C                   ENDDO
095700991012     C                   MOVEL     KSC(1)        WKSC              7
095800991012     C                   MOVEL     '1'           $TESVD            1        TA
095900991012E   1C                   ENDIF
096000991012     C*
096100991012     C* CODICE CLIENTE NUMERICO
096200991012IF  1C     WKSC          IFLT      *ZEROS
096300991012     C                   MOVEL     '1'           WERR              1
096400991012X   1C                   ELSE
096500991012     C*
096600991012     C* SOLO CARATTERI NUMERICI
096700991012     c   28              move      *all'0'       wcop
096800991012     c   28              move      wksc          wcop
096900991012     C                   SETOFF                                       28
097000991012     C                   TESTN                   Wcop                 28
097100991012IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
097200991012     C                   MOVEL     '1'           ERRL00
097300991012X   1C                   ELSE                                                   *E'NUMERO DA TESTN
097400991012     C                   MOVE      Wcop          W001              1
097500991012IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
097600991012     C                   MOVEL     '1'           ERRL00
097700991012X   2C                   ELSE                                                   *NUMERO
097800991012     C*
097900991012     C* CONTROLLA ESISTENZA CODICE
098000991210     C                   MOVE      Wcop          wcli
098100991210     C                   exsr      dectib
098200991012E   2C                   ENDIF
098300991012E   1C                   ENDIF
098400991012E   1C                   ENDIF
098500991012     C*
098600991012     C                   ENDSR
098700970526     C*------------------------------------------------------------------------*
098800970526     C* LANPGM - LANCIA UN PROGRAMMA
098900970526     C*------------------------------------------------------------------------*
099000970526     C     LANPGM        BEGSR
099100970526     C*
099200970526     C* IMPOSTA LA DS DI PROCEDURA
099300991001     C                   MOVEL     WOP0          k12OP0                         *LIVELLO PROCEDURA
099400991001     C                   MOVEL     WOP1          k12OP1                         *LIVELLO PGM
099500991001     C                   MOVEL     '0'           k12F03                         *PREMUTO F03=FINE
099600991001     C                   MOVEL     '0'           k12F12                         *PREMUTO F12=RITORNO
099700991001     C                   MOVEL     '0'           k12ERR                         *'1'=ERRORE
099800991001     C                   MOVEL     *BLANKS       k12MSG                         *MESSAGGIO DI RITORN
099900991001     C                   MOVEL     Dtle          k12tle                         *PARTENZA/ARRIVO
100000991001     c                   movel     dazp          k12azp
100100991001     C                   Z-ADD     Dcop          k12cop                         *TERMINAL PARTENZA
100200991001     c                   movel     dazf          k12azf
100300991001     C                   Z-ADD     Dcof          k12cof                         *TERMINAL ARRIVO
100400991001     C                   Z-ADD     Ddde          k12dde                         *DECORRENZA
100500991001     C                   Z-ADD     Ddsc          k12dsc                         *DECORRENZA
100600970526     C*
100700991104     C                   movel     tibds         kpjbu
100800970526     C                   CALL      WPGM
100900970612     C                   PARM                    KPJBA
101000991104     C*                  PARM                    tibds
101100970526     C*
101200970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
101300991001IF  1C     k12F03        IFEQ      '1'                                          *PREMUTO F03
101400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
101500970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
101600970526X   1C                   ELSE                                                   *NON PREMUTO F03
101700991001     C                   MOVEL     k12MSG        DSMSMS                         *MESSAGGIO DI RITORN
101800970526E   1C                   ENDIF
101900970526     C*
102000970526     C                   ENDSR
102100000607     C*------------------------------------------------------------------------*
102200000607     C* LANPGM2- LANCIA UN Pgm alternativo per ds non di procedura
102300000607     C*------------------------------------------------------------------------*
102400000607     C     LANPGM2       BEGSR
102500000607     C*
102600000607     C* IMPOSTA LA DS DI PROCEDURA
102700000607     c                   clear                   tis720
102800000607     C                   MOVEL     WOP0          s20OP0                         *LIVELLO PROCEDURA
102900000607     C                   MOVEL     WOP1          s20OP1                         *LIVELLO PGM
103000000607     C                   MOVEL     '0'           s20F03                         *PREMUTO F03=FINE
103100000607     C                   MOVEL     '0'           s20F12                         *PREMUTO F12=RITORNO
103200000607     C                   MOVEL     '0'           s20ERR                         *'1'=ERRORE
103300000607     C                   MOVEL     *BLANKS       s20MSG                         *MESSAGGIO DI RITORN
103400000607     C                   MOVEL     Dtle          s20tle                         *PARTENZA/ARRIVO
103500000607     C*                  move      *zeros        s20ksc                         *TERMINAL PARTENZA
103600000607     C                   move(p)   Dcop          s20ksc                         *TERMINAL PARTENZA
103700000607     C*
103800000607     C                   movel     tis720        kpjbu
103900000607     C                   CALL      WPGM
104000000607     C                   PARM                    KPJBA
104100000607     C*
104200000607     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
104300000607IF  1C     s20F03        IFEQ      '1'                                          *PREMUTO F03
104400000607     C                   MOVEL     '1'           ERRL00                         *ERRORE
104500000607     C                   MOVEL     '1'           F03L00                         *FINE PGM
104600000607X   1C                   ELSE                                                   *NON PREMUTO F03
104700000607     C                   MOVEL     s20MSG        DSMSMS                         *MESSAGGIO DI RITORN
104800000607E   1C                   ENDIF
104900000607     C*
105000000607     C                   ENDSR
105100991001     c*--------------------------------------------------------------------------------------------*
105200991001     C* cartab - carica le tabelle occorrenti
105300991001     c*--------------------------------------------------------------------------------------------*
105400991001     C     cartab        BEGSR
105500020905     C*
105600020905     C* REPERISCE I DATI UTENTE
105700020905     C                   EXSR      REPDATIUTE
105800020905     C*
105900020212     C*----------
106000020212     C* Tipi servizi soggetti a controllo unificazioni
106100020212     C*----------
106200020212     C                   z-add     *zeros        x                 3 0
106300020212     C                   eval      tbecod = 'ISV'
106400020212     C     ke2tbe        setll     tntbe01l
106500020212     C     ke2tbe        reade     tntbe01l
106600020212     C                   dow       not %eof(tntbe01l)
106700020212     C                   movel     tbeuni        dsisv
106800020212     C                   if        §isvuni  = 'S'
106900020212     C                   add       1             x
107000020212     C                   eval      isvkun(x) = tbeke1
107100020212     C                   endif
107200020212     C     ke2tbe        reade     tntbe01l
107300020212     C                   enddo
107400020212     C*
107500991001     c*
107600991001     C                   ENDSR
107700970526     C*------------------------------------------------------------------------*
107800970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
107900970526     C*------------------------------------------------------------------------*
108000970526     C     CHKFOP        BEGSR
108100970626     C*
108200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
108300970626IF  1C     DEPOP0        IFEQ      'P01'
108400970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
108500970626     C                   MOVEL     'S'           OPDATT
108600970626     C                   MOVEL     OPDRIG        OPD(1)
108700970626X   1C                   ELSE
108800970626     C* OPZIONI
108900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
109000970626     C                   MOVEL     'S'           OPDATT
109100970626     C                   MOVEL     OPDRIG        OPD(5)
109200020709     C     DUTLPO        IFEQ      'S'
109300000607     C                   MOVEL     OPD(9)        OPDRIG                         *CLI ABILITATI
109400000607     C                   MOVEL     'S'           OPDATT
109500000607     C                   MOVEL     OPDRIG        OPD(9)
109600000830     C                   ENDIF
109700970626     C* FUNZIONI
109800970626E   1C                   ENDIF
109900970626     C*
110000970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
110100970626IF  1C     DEPOP0        IFEQ      'P02'
110200970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
110300970626     C                   MOVEL     'S'           OPDATT
110400970626     C                   MOVEL     OPDRIG        OPD(2)
110500020709     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
110600991014     C                   MOVEL     'S'           OPDATT
110700991014     C                   MOVEL     OPDRIG        OPD(3)
110800000307     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
110900000307     C                   MOVEL     'S'           OPDATT
111000000307     C                   MOVEL     OPDRIG        OPD(4)
111100970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
111200970626     C                   MOVEL     'S'           OPDATT
111300970626     C                   MOVEL     OPDRIG        OPD(7)
111400970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
111500970626     C                   MOVEL     'S'           FUDATT
111600970626     C                   MOVEL     FUDRIG        FUD(6)
111700970626E   1C                   ENDIF
111800970526     C*
111900970526     C* GESTIONE OPZIONI A VIDEO
112000970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
112100970526     C                   EXSR      OPZGES
112200970526     C*
112300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
112400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
112500970526     C                   EXSR      FUNGES
112600970526     C*
112700970526     C                   ENDSR
112800991210     C*------------------------------------------------------------------------*
112900991210     C* FINSR - OPERAZIONI FINALI
113000991210     C*------------------------------------------------------------------------*
113100991210     C     FINSR         BEGSR
113200991210     C*
113300991210     C* CHIUDE I FILE DEI PGM CHIAMATI
113400991210     C                   CLEAR                   bs69ds
113500991210     C                   CLEAR                   acods
113600991210     C                   CLEAR                   indds
113700991210     C                   CLEAR                   clpds
113800991210     C                   CLEAR                   clsds
113900991210     C                   MOVEL     'C'           i69tla
114000991210     C                   MOVEL     knsif         i69sif
114100991210     C                   CALL      'TIBS69R'
114200991210     C                   PARM                    bs69ds
114300991210     C                   PARM                    acods
114400991210     C                   PARM                    indds
114500991210     C                   PARM                    clpds
114600991210     C                   PARM                    clsds
114700991210     C*
114800991210     C                   ENDSR
114900020905     C*--------------------------------------------------------------------------------------------*
115000020905     C* REPERISCE I DATI UTENTE
115100020905     C*--------------------------------------------------------------------------------------------*
115200020905     C     REPDATIUTE    BEGSR
115300020905     C*
115400020905     C* INIZIALIZZA VARIABILI DI WRK
115500020905     C                   CLEAR                   TIBS34DS
115600020905     C                   CLEAR                   AZUTEDS
115700020905     C                   CLEAR                   DDATIUTE
115800020905     C*
115900020905     C     *DTAARA       DEFINE    §azute        azuteds
116000020905     C     *DTAARA       DEFINE    §datiute      ddatiute
116100020905     C                   IN(E)     *DTAARA
116200020905if  1C                   IF        %Error
116300020905     c                   EVAL      I34Tla = 'L'
116400020905     C                   CALL      'TIBS34R'
116500020905     C                   PARM                    Tibs34Ds
116600020905     C                   IN        *DTAARA
116700020905e   1C                   ENDIF
116800020905     C*
116900020905     C                   MOVEL     RSUT          DSFIRS
117000060113     C*
117100060113     C* Carica le abilitazioni particolari del profilo
117200060113     C                   Movel     *blanks       wabi              2
117300060113     C                   Movel     utefaf        dute01
117400060113     C                   If        §utecli <> *Blanks
117500060113     C                   Eval      wabi = §utecli
117600060113     C                   Else
117700060113     C                   Eval      wabi = uteaut
117800060113     C                   EndIf
117900060113     C*
118000060113     C* Reperimento dei P.O. gestibili dall'utente per i codici clienti
118100060113     C                   Clear                   Trul31ds
118200060113     C                   Eval      i31abi = wabi
118300060113     C                   Eval      i31cdi = dutdis
118400060113     C                   Eval      i31car = dutare
118500060113     C                   Eval      i31cpo = dutpou
118600060113     C                   Call      'TRUL31R'
118700060113     C                   Parm                    kpjba
118800060113     C                   Parm                    Trul31ds
118900060113     C                   If        o31pog > *zeros
119000060113     C                   Movea     o31pog        skPOG
119100060113     C                   EndIf
119200020905     C*
119300020905     C                   ENDSR
119400970526     C*------------------------------------------------------------------------*
119500000307     C* *INZSR - OPERAZIONI INIZIALI
119600970526     C*------------------------------------------------------------------------*
119700970526     C     *INZSR        BEGSR
119800970526     C*---
119900970526     C* RICEVIMENTO PARAMETRI
120000970526     C*---
120100970526     C     *ENTRY        PLIST
120200970612     C                   PARM                    KPJBA
120300020709     c                   MOVEL     KPJBU         TIBDS
120400970526     C*
120500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
120600970526     C     XMSGPA        PLIST
120700970526     C                   PARM                    DSMSMS
120800970526     C                   PARM                    DSMSPG
120900970526     C                   PARM                    DSMSKE
121000970526     C                   PARM                    DSMSER
121100970526     C*
121200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
121300970526     C                   CALL      'XKEY'
121400970526     C                   PARM                    DSKEY
121500970526     C*---
121600970526     C* VARIABILI NON RIFERITE AL DATA BASE
121700970526     C*---
121800970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
121900970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
122000970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
122100970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
122200970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
122300970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
122400970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
122500970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
122600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
122700991001     C     *LIKE         DEFINE    k12OP0        DEPOP0                         *DI DEPOSITO
122800991001     C     *LIKE         DEFINE    k12OP1        DEPOP1
122900991001     C     *LIKE         DEFINE    k12OP0        WOP0                           *DI LAVORO
123000991001     C     *LIKE         DEFINE    k12OP1        WOP1
123100991001     C     *LIKE         DEFINE    k12tle        Dtle
123200991001     C     *LIKE         DEFINE    k12cop        Dcop
123300991001     C     *LIKE         DEFINE    k12azp        Dazp
123400991001     C     *LIKE         DEFINE    k12cof        Dcof
123500991001     C     *LIKE         DEFINE    k12azf        Dazf
123600991001     C     *LIKE         DEFINE    k12dde        Ddde
123700991001     C     *LIKE         DEFINE    k12dsc        Ddsc
123800970526     C*---
123900991001     C     *LIKE         DEFINE    k12tle        Deptle
124000991001     C     *LIKE         DEFINE    k12tle        Deptl2
124100991011     C     *LIKE         DEFINE    viduni        Depuni
124200991013     C     *LIKE         DEFINE    vidun2        Depun2
124300991011     C     *LIKE         DEFINE    videle        Depele
124400991013     C     *LIKE         DEFINE    videl2        Depel2
124500991013     C     *LIKE         DEFINE    vidpdd        Deppdd
124600000307     C*---
124700970526     C* VARIABILI DI CONTROLLO
124800970526     C*---
124900970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
125000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
125100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
125200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
125300970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
125400970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
125500000307     C*---
125600000307     C* INIZIALIZZAZIONE CAMPI A VIDEO
125700000307     C*---
125800020709     C                   MOVEL     *blanks       VIDUNI
125900020709     C                   MOVEL     *blanks       VIDUN2
126000020709     C                   MOVEL     *blanks       VIDELE
126100020709     C                   MOVEL     *blanks       VIDEL2
126200020709     C                   MOVEL     *blanks       VIDTL2
126300020709     C                   MOVEL     *blanks       VIDPDD
126400970526     C*
126500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
126600991001     C                   MOVEL     'TIBS12R'     DSMSPG
126700991001     C                   MOVEL     'TIBS12R'     MSGQUE
126800970526     C                   MOVEL     *BLANKS       MSGKEY
126900970526     C                   MOVEL     *BLANKS       DSMSMS
127000970526     C                   MOVEL     *BLANKS       DSMSKE
127100970526     C                   Z-ADD     *ZEROS        DSMSER
127200970526     C*---
127300970526     C* VARIABILI RIFERITE AL DATA BASE
127400970526     C*---
127500020830     C     *LIKE         DEFINE    acokcc        kcccl
127600000731     C     *LIKE         DEFINE    kuncop        KCEcop
127700000731     C     *LIKE         DEFINE    kuncof        KCEcof
127800000731     C     *LIKE         DEFINE    kundde        KCEdde
127900000731     C     *LIKE         DEFINE    kunazp        KCEazp
128000000731     C     *LIKE         DEFINE    kunazf        KCEazf
128100000731     C     *LIKE         DEFINE    kuntle        KCEtle
128200000307     C     *LIKE         DEFINE    tbecod        KTBcod
128300000307     C     *LIKE         DEFINE    tbeke1        KTBky1
128400020211     C     *LIKE         DEFINE    vssksu        KSSksu
128500020212     C     *LIKE         DEFINE    vssisv        KSSisv
128600020905     C     *LIKE         DEFINE    kpjbu         Depkpjbu
128700970526     C*---
128800970526     C* CHIAVI DI LETTURA
128900970526     C*---
129000000731     C* LETTURA tikun01L
129100000731     C     KE1kun        KLIST
129200000731     C                   KFLD                    KCEtle
129300000731     C                   KFLD                    KCEazp
129400000731     C                   KFLD                    KCEcop
129500000731     C                   KFLD                    KCEazf
129600000731     C                   KFLD                    KCEcof
129700020211     C* LETTURA tikun02L
129800000731     C     KE2kun        KLIST
129900000731     C                   KFLD                    KCEtle
130000000731     C                   KFLD                    KCEazf
130100000731     C                   KFLD                    KCEcof
130200000731     C                   KFLD                    KCEazp
130300000731     C                   KFLD                    KCEcop
130400020211     C* LETTURA tikun04L
130500000731     C     KE4kun        KLIST
130600000731     C                   KFLD                    KCEdde
130700000731     C                   KFLD                    KCEcop
130800000731     C                   KFLD                    KCEcof
130900020211     C* LETTURA tikun05L
131000000731     C     KE5kun        KLIST
131100000731     C                   KFLD                    KCEdde
131200000731     C                   KFLD                    KCEcof
131300000731     C                   KFLD                    KCEcop
131400991001     C* LETTURA cnaco00f
131500991001     C     keycli        KLIST                                                  *di cnaco/ind/clp
131600991001     C                   KFLD                    kclkut            1 0          -utente
131700991001     C                   KFLD                    kcccl                          -capoconto
131800991001     C                   KFLD                    kcksc             7 0          -codice
131900000307     C* LETTURA tntbe01l -
132000000307     C     KEYtbe        KLIST
132100000307     C                   KFLD                    Ktbcod                         *CODICE TABELLA
132200000307     C                   KFLD                    Ktbky1                         *CHIAVE 1 TABELLA
132300020212     C* LETTURA tntbe01l - PARZIALE
132400020212     C     KE2tbe        KLIST
132500020212     C                   KFLD                    tbecod                         *CODICE TABELLA
132600020212     C* LETTURA tivss05L
132700020211     C     KEYvss        KLIST
132800020211     C                   KFLD                    KSSksu
132900970527     C*
133000970527     C* CARICA LE TABELLE OCCORRENTI
133100970527     C                   EXSR      CARTAB
133200020927     C*
133300020927     C* IN BASE ALL'UTENTE EFFETTUO ALCUNE CONSIDERAZIONI
133400021007     C                   IF        simfel = *zeros
133500020927     C                   OPEN      tivss05l
133600020927     C                   ENDIF
133700020916     C*
133800020916     C* INIZIALIZZAZIONE VARIABILI DI WRK
133900020916     C                   Z-ADD     1             kclkut
134000020916     C                   Z-ADD     DUTKCI        kcccl
134100970526     C*
134200970526     C* DEPOSITA LE OPZIONI DI ENTRATA
134300991001     C                   MOVEL     k12OP0        DEPOP0                         *LIVELLO PROCEDURA
134400991001     C                   MOVEL     k12OP1        DEPOP1                         *LIVELLO PROGRAMMA
134500970526     C*
134600970526     C* CALCOLA LA DATA CORRENTE
134700100322     C                   Z-ADD     *ZEROS        DATCOR            8 0          *DATA CORRENTE AA/M/
134800100322     C                   EVAL      datcor = %dec(%date() : *ISO)
134900970526     C*
135000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
135100991028     c                   if        k12tle <> *blank and
135200991028     c                             k12tle <> *zeros
135300991028     c                   move      k12tle        vidtl2
135400991028     c                   else
135500991028     C                   MOVE      *blank        VIDtl2                         *PARTENZA/ARRIVO
135600991028     c                   end
135700000731     C*                  move      k12cop        VIDuni                         *CODICE CLI PADRE
135800000602     C                   move      k12cop        VIDun2                         *CODICE CLI PADRE
135900000602     C                   move      *zeros        VIDele                         *CODICE CLI FIGLIO
136000000602     C                   Z-ADD     *ZEROS        VIDPDD                         *DATA DECORRENZA
136100991001IF  1C     k12dde        IFGT      *ZEROS
136200991001     C                   Z-ADD     k12dde        G08INV
136300970528     C                   Z-ADD     *ZEROS        G08DAT
136400970528     C                   MOVEL     '3'           G08ERR
136500970528     C                   CALL      'XSRDA8'
136600970528     C                   PARM                    WLBDA8
136700970528     C                   Z-ADD     G08DAT        VIDPDD
136800970528E   1C                   ENDIF
136900970526     C                   EXSR      CHKPAR
137000970526IF  1C     ERRL00        IFEQ      '1'
137100991001     C                   MOVEL     '1'           k12ERR
137200991001     C                   MOVEL     DSMSMS        k12MSG
137300970526X   1C                   ELSE
137400991001     C                   MOVEL     VIDtle        DEPtle                         *DEPOSITI
137500991011     C                   move      VIDuni        DEPuni
137600991011     C                   move      VIDele        DEPele
137700991013     C                   Z-ADD     VIDpdd        DEPpdd
137800991013     C                   move      VIDun2        DEPun2
137900991013     C                   move      VIDel2        DEPel2
138000140609     C                   movel     VIDtl2        DEPtl2                         *tipo legame
138100970526E   1C                   ENDIF
138200970526     C*
138300970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
138400970526     C                   EXSR      CHKFOP
138500970526     C*
138600970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
138700970530IF  1C     DEPOP0        IFEQ      'P01'
138800970530     C                   MOVEL     DEC(3)        VIDING
138900970530E   1C                   ENDIF
139000970530IF  1C     DEPOP0        IFEQ      'P02'
139100970526     C                   MOVEL     DEC(1)        VIDING
139200970530E   1C                   ENDIF
139300970530IF  1C     DEPOP0        IFEQ      'P05'
139400970526     C                   MOVEL     DEC(2)        VIDING
139500970526E   1C                   ENDIF
139600970526     C*
139700970526     C                   ENDSR
139800970526     C*------------------------------------------------------------------------*
139900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
140000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
140100970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
140200970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
140300970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
140400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
140500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
140600970526     C*------------------------------------------------------------------------*
140700970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
140800970526     C*------------------------------------------------------------------------*
140900970526     C     POSSFL        BEGSR
141000970526     C*
141100970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
141200970526     C                   EXSR      CLRSFL
141300970526     C*
141400970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
141500970526     C                   EXSR      SETFIL
141600970526     C*
141700970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
141800970526     C     SFLEOF        IFEQ      0
141900970526     C                   EXSR      CARSFL
142000970526     C                   END
142100970526     C*
142200970526     C                   ENDSR
142300970526     C*------------------------------------------------------------------------*
142400970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
142500970526     C*------------------------------------------------------------------------*
142600970526     C     ENTATT        BEGSR
142700970526     C*
142800970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
142900970526     C                   Z-ADD     0             OPZOK             1 0
143000970526     C*
143100970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
143200970526     C                   Z-ADD     0             SFLVAR            5 0
143300970526     C*
143400970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
143500970526     C                   Z-ADD     0             SFLERR
143600970526     C*
143700970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
143800970526     C                   Z-ADD     SFLULT        WFLULT
143900970526     C*
144000970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
144100991011     C                   READC     BS12D05                                98
144200970526     C     *IN98         DOWEQ     '0'
144300970526     C*
144400970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
144500970526     C                   ADD       1             SFLVAR
144600970526     C*
144700970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
144800970526     C     OPZOK         IFEQ      0
144900970526     C*
145000970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
145100970526     C     OPZKEY        IFNE      *BLANKS
145200970526     C                   EXSR      OPZCON
145300970526     C                   END
145400970526     C*
145500970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
145600970526     C*            2. PULIZIA SCELTA IN SFL
145700970526     C     OPZOK         IFEQ      0
145800970526     C                   MOVEL     '0'           *IN41
145900970526     C                   MOVEL     *BLANKS       OPZKEY
146000970526     C                   ELSE
146100970526     C*
146200970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
146300970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
146400970526     C                   MOVEL     '1'           *IN41
146500970526     C                   Z-ADD     SFLULT        SFLERR
146600970526     C                   END
146700970526     C                   ELSE
146800970526     C*
146900970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
147000970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
147100970526     C                   MOVEL     '1'           *IN41
147200970526     C                   END                                                    OPZOK=0
147300970526     C*
147400970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
147500970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
147600970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
147700970526     C     OPZOK         IFNE      0
147800970526     C                   Z-ADD     SFLERR        SFLNBR
147900970526     C                   ELSE
148000970526     C                   Z-ADD     SFLULT        SFLNBR
148100970526     C                   END
148200970526     C*
148300970526     C* AGGIORNO RECORD SFL
148400020212     C                   EVAL      *IN76 = HIN76
148500991011     C                   UPDATE    BS12D05
148600970526     C*
148700991011     C                   READC     BS12D05                                98
148800970526     C                   END                                                    DO WHILE
148900970526     C*
149000970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
149100970526     C                   Z-ADD     WFLULT        SFLULT
149200970526     C*
149300970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
149400970526     C     SFLVAR        IFEQ      0
149500970526     C*---                 MOVEL'1'       FINL00
149600970526     C                   END
149700970526     C*
149800970526     C                   ENDSR
149900970526     C*------------------------------------------------------------------------*
150000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
150100970526     C*------------------------------------------------------------------------*
150200970526     C     CLRSFL        BEGSR
150300970526     C*
150400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
150500970526     C* (0=NON FINE FILE  1=FINE FILE)
150600970526     C                   Z-ADD     0             SFLEOF
150700970526     C*
150800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
150900970526     C                   Z-ADD     0             SFLULT
151000970526     C*
151100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
151200970526     C                   MOVEL     '1'           *IN40
151300991011     C                   WRITE     BS12D06
151400970526     C                   MOVEL     '0'           *IN40
151500970526     C*
151600970526     C                   ENDSR
151700970526     C*------------------------------------------------------------------------*
151800970526     C* CARSFL - CARICAMENTO SFL
151900970526     C*------------------------------------------------------------------------*
152000970526     C     CARSFL        BEGSR
152100970526     C*
152200970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
152300970526     C     SFLULT        IFEQ      0
152400970526     C                   EXSR      SETFIL
152500970526     C                   END
152600970526     C*
152700970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
152800970526     C                   Z-ADD     SFLULT        WFLULT
152900970526     C*
153000970526     C* CICLO:
153100970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
153200970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
153300970526     C                   Z-ADD     1             SFLRIG
153400970526     C     SFLRIG        DOWLE     SFLPAG
153500970526     C     SFLEOF        ANDEQ     0
153600970526     C*
153700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
153800970526     C                   MOVEL     *BLANKS       OPZKEY
153900970526     C*
154000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
154100970526     C                   EXSR      IRESFL
154200970526     C*
154300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
154400970526     C                   ADD       1             SFLULT
154500970526     C*
154600970526     C* SCRITTURA RECORD SFL
154700020212     C                   EVAL      HIN76 = *IN76
154800991011     C                   WRITE     BS12D05
154900970526     C                   ADD       1             SFLRIG
155000970526     C*
155100970526     C* LETTURA FILE DA CARICARE
155200970526     C                   EXSR      LETFIL
155300970526     C                   END                                                    DO WHILE
155400970526     C*
155500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
155600970526     C     WFLULT        IFNE      SFLULT
155700970526     C                   Z-ADD     WFLULT        SFLNBR
155800970526     C                   ADD       1             SFLNBR
155900970526     C                   END
156000970526     C*
156100970526     C                   ENDSR
156200970526     C*------------------------------------------------------------------------*
156300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
156400970526     C*------------------------------------------------------------------------*
156500970526     C     OUTSFL        BEGSR
156600970526     C*
156700970526     C* WRITE RECORD TESTATA VIDEO
156800991011     C                   WRITE     BS12D01
156900970526     C*
157000970526     C* VISUALIZZAZIONE MESSAGGIO
157100970526     C                   EXSR      SNDMSG
157200970526     C*
157300000307     C* WRITE RECORD OPZIONI VIDEO
157400991011     C                   WRITE     BS12D02
157500970526     C*
157600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
157700991011     C                   WRITE     BS12D03
157800970526     C*
157900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
158000991011     C                   WRITE     BS12D04
158100970526     C*
158200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
158300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
158400970526     C     SFLEOF        IFEQ      0
158500970526     C                   MOVEL     '0'           *IN42
158600970526     C                   ELSE
158700970526     C*
158800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
158900970526     C                   MOVEL     '1'           *IN42
159000970526     C                   END
159100970526     C*
159200970526     C* WRITE RECORD TASTI FUNZIONALI
159300991011     C                   WRITE     BS12D09
159400970526     C*
159500970526     C* CASO DI SFL VUOTO
159600970526     C     SFLULT        IFLE      0
159700970526     C*
159800970526     C*    - WRITE RECORD VUOTO
159900991011     C                   WRITE     BS12D0A
160000991011     C                   READ      BS12D0A                                99
160100970526     C*
160200970526     C                   ELSE
160300970526     C*
160400970526     C* CASO DI SFL CON RECORDS
160500970526     C*
160600970526     C*    - WRITE RECORD CONTROLLO SFL
160700991011     C                   WRITE     BS12D06
160800991011     C                   READ      BS12D06                                99
160900970526     C                   END
161000970526     C*
161100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
161200991011     C                   READ      BS12D03                                99
161300970526     C*
161400970526     C                   ENDSR
161500970526     C*------------------------------------------------------------------------*
161600970526     C* SNDMSG - MANDA MESSAGGIO
161700970526     C*------------------------------------------------------------------------*
161800970526     C     SNDMSG        BEGSR
161900970526     C*
162000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
162100970526     C                   MOVEL     '1'           *IN40
162200991011     C                   WRITE     BS12DMC
162300970526     C                   MOVEL     '0'           *IN40
162400970526     C*
162500970526     C* VISUALIZZAZIONE MESSAGGIO
162600970526     C                   Z-ADD     0             MSGULT
162700970526     C                   CALL      'XMSG'        XMSGPA
162800970526     C     DSMSER        IFEQ      0
162900970526     C                   MOVEL     DSMSKE        MSGKEY
163000970526     C                   END
163100970526     C                   Z-ADD     1             MSGULT
163200991011     C                   WRITE     BS12DMS                                      SFL MESS
163300991011     C                   WRITE     BS12DMC                                      CONTR MESS
163400970526     C*
163500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
163600970526     C                   MOVEL     *BLANKS       DSMSMS
163700970526     C*
163800970526     C                   ENDSR
163900970526     C*------------------------------------------------------------------------*
164000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
164100970526     C*------------------------------------------------------------------------*
164200970526     C     FUNCON        BEGSR
164300970526     C*
164400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
164500970526     C                   EXSR      FUNVAR
164600970526     C*
164700970526     C* PULISCO CAMPI DI LAVORO
164800970526     C                   Z-ADD     0             I                 5 0
164900970526     C                   Z-ADD     0             W                 5 0
165000970526     C*
165100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
165200970526     C     FUNKEY        IFNE      *BLANKS
165300970526     C     1             DO        24            I
165400970526     C                   MOVEL     FUD(I)        FUDRIG
165500970526     C*
165600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
165700970526     C     FUNKEY        IFEQ      FUDFUN
165800970526     C     FUDATT        ANDEQ     'S'
165900970526     C                   Z-ADD     I             W
166000970526     C                   END
166100970526     C                   END                                                    DO WHILE
166200970526     C*
166300970526     C* FUNZIONE NON ATTIVA (ERRORE)
166400970526     C     W             IFEQ      0
166500970526     C*
166600970526     C* EMISSIONE MESSAGGIO DI ERRORE
166700970526     C                   MOVEL     MSG(2)        DSMSMS
166800970526     C                   END                                                    W=0
166900970526     C                   END                                                    FUNKEY NE *BLANKS
167000970526     C*
167100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
167200970526     C     W             IFNE      0
167300970526     C     FUNKEY        OREQ      *BLANKS
167400970526     C*
167500970526     C     FUNKEY        IFEQ      *BLANKS
167600970526     C                   EXSR      ENTGES
167700970526     C                   END
167800970526     C     FUNKEY        IFEQ      '03'
167900970526     C                   EXSR      F03GES
168000970526     C                   END
168100970526     C     FUNKEY        IFEQ      '05'
168200970526     C                   EXSR      F05GES
168300970526     C                   END
168400970526     C     FUNKEY        IFEQ      '06'
168500970526     C                   EXSR      F06GES
168600970526     C                   END
168700970526     C     FUNKEY        IFEQ      '12'
168800970526     C                   EXSR      F12GES
168900970526     C                   END
169000970528     C     FUNKEY        IFEQ      '21'
169100970528     C                   EXSR      F21GES
169200970528     C                   END
169300970528     C     FUNKEY        IFEQ      '22'
169400970528     C                   EXSR      F22GES
169500970528     C                   END
169600970526     C     FUNKEY        IFEQ      '23'
169700970526     C                   EXSR      F23GES
169800970526     C                   END
169900970526     C     FUNKEY        IFEQ      '24'
170000970526     C                   EXSR      F24GES
170100970526     C                   END
170200970526     C     FUNKEY        IFEQ      'RU'
170300970526     C                   EXSR      RUPGES
170400970526     C                   END
170500970526     C*
170600970526     C                   END
170700970526     C*
170800970526     C                   ENDSR
170900970526     C*------------------------------------------------------------------------*
171000970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
171100970526     C*------------------------------------------------------------------------*
171200970526     C     FUNVAR        BEGSR
171300970526     C*
171400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
171500970526     C     KEY           IFEQ      ENTER
171600970526     C                   MOVEL     *BLANKS       FUNKEY
171700970526     C                   END
171800970526     C     KEY           IFEQ      F1
171900970526     C                   MOVEL     '01'          FUNKEY
172000970526     C                   END
172100970526     C     KEY           IFEQ      F2
172200970526     C                   MOVEL     '02'          FUNKEY
172300970526     C                   END
172400970526     C     KEY           IFEQ      F3
172500970526     C                   MOVEL     '03'          FUNKEY
172600970526     C                   END
172700970526     C     KEY           IFEQ      F4
172800970526     C                   MOVEL     '04'          FUNKEY
172900970526     C                   END
173000970526     C     KEY           IFEQ      F5
173100970526     C                   MOVEL     '05'          FUNKEY
173200970526     C                   END
173300970526     C     KEY           IFEQ      F6
173400970526     C                   MOVEL     '06'          FUNKEY
173500970526     C                   END
173600970526     C     KEY           IFEQ      F7
173700970526     C                   MOVEL     '07'          FUNKEY
173800970526     C                   END
173900970526     C     KEY           IFEQ      F8
174000970526     C                   MOVEL     '08'          FUNKEY
174100970526     C                   END
174200970526     C     KEY           IFEQ      F9
174300970526     C                   MOVEL     '09'          FUNKEY
174400970526     C                   END
174500970526     C     KEY           IFEQ      F10
174600970526     C                   MOVEL     '10'          FUNKEY
174700970526     C                   END
174800970526     C     KEY           IFEQ      F11
174900970526     C                   MOVEL     '11'          FUNKEY
175000970526     C                   END
175100970526     C     KEY           IFEQ      F12
175200970526     C                   MOVEL     '12'          FUNKEY
175300970526     C                   END
175400970526     C     KEY           IFEQ      F13
175500970526     C                   MOVEL     '13'          FUNKEY
175600970526     C                   END
175700970526     C     KEY           IFEQ      F14
175800970526     C                   MOVEL     '14'          FUNKEY
175900970526     C                   END
176000970526     C     KEY           IFEQ      F15
176100970526     C                   MOVEL     '15'          FUNKEY
176200970526     C                   END
176300970526     C     KEY           IFEQ      F16
176400970526     C                   MOVEL     '16'          FUNKEY
176500970526     C                   END
176600970526     C     KEY           IFEQ      F17
176700970526     C                   MOVEL     '17'          FUNKEY
176800970526     C                   END
176900970526     C     KEY           IFEQ      F18
177000970526     C                   MOVEL     '18'          FUNKEY
177100970526     C                   END
177200970526     C     KEY           IFEQ      F19
177300970526     C                   MOVEL     '19'          FUNKEY
177400970526     C                   END
177500970526     C     KEY           IFEQ      F20
177600970526     C                   MOVEL     '20'          FUNKEY
177700970526     C                   END
177800970526     C     KEY           IFEQ      F21
177900970526     C                   MOVEL     '21'          FUNKEY
178000970526     C                   END
178100970526     C     KEY           IFEQ      F22
178200970526     C                   MOVEL     '22'          FUNKEY
178300970526     C                   END
178400970526     C     KEY           IFEQ      F23
178500970526     C                   MOVEL     '23'          FUNKEY
178600970526     C                   END
178700970526     C     KEY           IFEQ      F24
178800970526     C                   MOVEL     '24'          FUNKEY
178900970526     C                   END
179000970526     C     KEY           IFEQ      ROLLUP
179100970526     C                   MOVEL     'RU'          FUNKEY
179200970526     C                   END
179300970526     C     KEY           IFEQ      HELP
179400970526     C                   MOVEL     'HP'          FUNKEY
179500970526     C                   END
179600970526     C*
179700970526     C                   ENDSR
179800970526     C*------------------------------------------------------------------------*
179900970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
180000970526     C*------------------------------------------------------------------------*
180100970526     C     FUNGES        BEGSR
180200970526     C*
180300970526     C* PULISCO CAMPI DI LAVORO
180400970526     C                   Z-ADD     0             W                 5 0
180500970526     C*
180600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
180700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
180800970526     C     1             DO        24            I
180900970526     C                   MOVEL     FUD(I)        FUDRIG
181000970526     C*
181100970526     C* PULIZIA FUNZIONI PRECEDENTI
181200970526     C                   MOVEL     *BLANKS       FUV(I)
181300970526     C*
181400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
181500970526     C* (SALTA F23 E F24)
181600970526     C     FUDATT        IFEQ      'S'
181700970526     C     FUDFUN        ANDNE     '23'
181800970526     C     FUDFUN        ANDNE     '24'
181900970526     C                   ADD       1             W
182000970526     C                   MOVEL     FUDDES        FUV(W)
182100970526     C                   END
182200970526     C                   END                                                    DO WHILE
182300970526     C*
182400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
182500970526     C* ALTRIMENTI DISATTIVO F24
182600970526     C     1             DO        24            I
182700970526     C                   MOVEL     FUD(I)        FUDRIG
182800970526     C*
182900970526     C     FUDFUN        IFEQ      '24'
183000970526     C     W             IFGT      6
183100970526     C                   MOVEL     'S'           FUDATT
183200970526     C                   ELSE
183300970526     C                   MOVEL     *BLANKS       FUDATT
183400970526     C                   END                                                    W GT 8
183500970526     C                   MOVEL     FUDRIG        FUD(I)
183600970526     C                   END
183700970526     C                   END                                                    DO WHILE
183800970526     C*
183900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
184000970526     C     W             DIV       6             I
184100970526     C                   MVR                     Z
184200970526     C*
184300970526     C     Z             IFNE      0
184400970526     C     I             ADD       1             Z
184500970526     C                   ELSE
184600970526     C                   Z-ADD     I             Z
184700970526     C                   END
184800970526     C*
184900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
185000970526     C                   ADD       1             FUNGRU
185100970526     C     FUNGRU        IFGT      Z
185200970526     C     FUNGRU        ORLT      1
185300970526     C                   Z-ADD     1             FUNGRU
185400970526     C                   END
185500970526     C*
185600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
185700970526     C     FUNGRU        IFEQ      1
185800970526     C                   MOVEL     FUV(1)        FUN001
185900970526     C                   MOVEL     FUV(2)        FUN002
186000970526     C                   MOVEL     FUV(3)        FUN003
186100970526     C                   MOVEL     FUV(4)        FUN004
186200970526     C                   MOVEL     FUV(5)        FUN005
186300970526     C                   MOVEL     FUV(6)        FUN006
186400970526     C                   END
186500970526     C*
186600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
186700970526     C     FUNGRU        IFEQ      2
186800970526     C                   MOVEL     FUV(7)        FUN001
186900970526     C                   MOVEL     FUV(8)        FUN002
187000970526     C                   MOVEL     FUV(9)        FUN003
187100970526     C                   MOVEL     FUV(10)       FUN004
187200970526     C                   MOVEL     FUV(11)       FUN005
187300970526     C                   MOVEL     FUV(12)       FUN006
187400970526     C                   END
187500970526     C*
187600970526     C* TERZO GRUPPO DI 6 FUNZIONI
187700970526     C     FUNGRU        IFEQ      3
187800970526     C                   MOVEL     FUV(13)       FUN001
187900970526     C                   MOVEL     FUV(14)       FUN002
188000970526     C                   MOVEL     FUV(15)       FUN003
188100970526     C                   MOVEL     FUV(16)       FUN004
188200970526     C                   MOVEL     FUV(17)       FUN005
188300970526     C                   MOVEL     FUV(18)       FUN006
188400970526     C                   END
188500970526     C*
188600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
188700970526     C     FUNGRU        IFEQ      4
188800970526     C                   MOVEL     FUV(19)       FUN001
188900970526     C                   MOVEL     FUV(20)       FUN002
189000970526     C                   MOVEL     FUV(21)       FUN003
189100970526     C                   MOVEL     FUV(22)       FUN004
189200970526     C                   MOVEL     *BLANKS       FUN005
189300970526     C                   MOVEL     *BLANKS       FUN006
189400970526     C                   END
189500970526     C*
189600970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
189700970526     C                   MOVEL     *BLANKS       FUN007
189800970526     C                   MOVEL     *BLANKS       FUN008
189900970526     C*
190000970526     C     1             DO        24            I
190100970526     C                   MOVEL     FUD(I)        FUDRIG
190200970526     C     FUDFUN        IFEQ      '23'
190300970526     C     FUDATT        ANDEQ     'S'
190400970526     C                   MOVEL     FUDDES        FUN007
190500970526     C                   END
190600970526     C     FUDFUN        IFEQ      '24'
190700970526     C     FUDATT        ANDEQ     'S'
190800970526     C                   MOVEL     FUDDES        FUN008
190900970526     C                   END
191000970526     C                   END                                                    DO
191100970526     C*
191200970526     C                   ENDSR
191300970526     C*------------------------------------------------------------------------*
191400970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
191500970526     C*------------------------------------------------------------------------*
191600970526     C     OPZGES        BEGSR
191700970526     C*
191800970526     C* PULISCO CAMPI DI LAVORO
191900970526     C                   Z-ADD     0             W                 5 0
192000970526     C*
192100970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
192200970526     C* E CARICO TABELLA OPZIONI ATTIVE
192300970526     C     1             DO        24            I
192400970526     C                   MOVEL     OPD(I)        OPDRIG
192500970526     C*
192600970526     C* PULIZIA OPZIONI PRECEDENTI
192700970526     C                   MOVEL     *BLANKS       OPV(I)
192800970526     C*
192900970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
193000970526     C     OPDATT        IFEQ      'S'
193100970526     C                   ADD       1             W
193200970526     C                   MOVEL     OPDDES        OPV(W)
193300970526     C                   END
193400970526     C                   END                                                    DO WHILE
193500970526     C*
193600970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
193700970526     C* ALTRIMENTI DISATTIVO F23
193800970526     C     1             DO        24            I
193900970526     C                   MOVEL     FUD(I)        FUDRIG
194000970526     C*
194100970526     C     FUDFUN        IFEQ      '23'
194200970526     C     W             IFGT      8
194300970526     C                   MOVEL     'S'           FUDATT
194400970526     C                   ELSE
194500970526     C                   MOVEL     *BLANKS       FUDATT
194600970526     C                   END                                                    W GT 8
194700970526     C                   MOVEL     FUDRIG        FUD(I)
194800970526     C                   END
194900970526     C                   END                                                    DO WHILE
195000970526     C*
195100970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
195200970526     C     W             DIV       8             I
195300970526     C                   MVR                     Z                 5 0
195400970526     C*
195500970526     C     Z             IFNE      0
195600970526     C     I             ADD       1             Z
195700970526     C                   ELSE
195800970526     C                   Z-ADD     I             Z
195900970526     C                   END
196000970526     C*
196100970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
196200970526     C                   ADD       1             OPZGRU
196300970526     C     OPZGRU        IFGT      Z
196400970526     C     OPZGRU        ORLT      1
196500970526     C                   Z-ADD     1             OPZGRU
196600970526     C                   END
196700970526     C*
196800970526     C* PRIMO GRUPPO DI 8 OPZIONI
196900970526     C     OPZGRU        IFEQ      1
197000970526     C                   MOVEL     OPV(1)        OPZ001
197100970526     C                   MOVEL     OPV(2)        OPZ002
197200970526     C                   MOVEL     OPV(3)        OPZ003
197300970526     C                   MOVEL     OPV(4)        OPZ004
197400970526     C                   MOVEL     OPV(5)        OPZ005
197500970526     C                   MOVEL     OPV(6)        OPZ006
197600970526     C                   MOVEL     OPV(7)        OPZ007
197700970526     C                   MOVEL     OPV(8)        OPZ008
197800970526     C                   END
197900970526     C*
198000970526     C* SECONDO GRUPPO DI 8 OPZIONI
198100970526     C     OPZGRU        IFEQ      2
198200970526     C                   MOVEL     OPV(9)        OPZ001
198300970526     C                   MOVEL     OPV(10)       OPZ002
198400970526     C                   MOVEL     OPV(11)       OPZ003
198500970526     C                   MOVEL     OPV(12)       OPZ004
198600970526     C                   MOVEL     OPV(13)       OPZ005
198700970526     C                   MOVEL     OPV(14)       OPZ006
198800970526     C                   MOVEL     OPV(15)       OPZ007
198900970526     C                   MOVEL     OPV(16)       OPZ008
199000970526     C                   END
199100970526     C*
199200970526     C* TERZO GRUPPO DI 8 OPZIONI
199300970526     C     OPZGRU        IFEQ      3
199400970526     C                   MOVEL     OPV(17)       OPZ001
199500970526     C                   MOVEL     OPV(18)       OPZ002
199600970526     C                   MOVEL     OPV(19)       OPZ003
199700970526     C                   MOVEL     OPV(20)       OPZ004
199800970526     C                   MOVEL     OPV(21)       OPZ005
199900970526     C                   MOVEL     OPV(22)       OPZ006
200000970526     C                   MOVEL     OPV(23)       OPZ007
200100970526     C                   MOVEL     OPV(24)       OPZ008
200200970526     C                   END
200300970526     C*
200400970526     C                   ENDSR
200500970526     C*------------------------------------------------------------------------*
200600970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
200700970526     C*------------------------------------------------------------------------*
200800970526     C     OPZCON        BEGSR
200900970526     C*
201000970526     C* PULISCO CAMPI DI LAVORO
201100970526     C                   Z-ADD     0             I                 5 0
201200970526     C                   Z-ADD     0             W                 5 0
201300970526     C*
201400970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
201500970526     C     1             DO        24            I
201600970526     C                   MOVEL     OPD(I)        OPDRIG
201700970526     C*
201800970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
201900970526     C     OPZKEY        IFEQ      OPDOPZ
202000970526     C     OPZKEY        OREQ      OPDOP1
202100970526     C*
202200970526     C     OPDATT        IFEQ      'S'
202300970526     C                   MOVEL     OPDOPZ        OPZKEY
202400970526     C                   Z-ADD     I             W
202500970526     C                   END
202600970526     C                   END
202700970526     C*
202800970526     C                   END                                                    DO WHILE
202900970526     C*
203000970526     C* OPZIONE NON ATTIVA (ERRORE)
203100970526     C     W             IFEQ      0
203200970526     C*
203300970526     C* OPZIONE NON OK
203400970526     C                   Z-ADD     1             OPZOK
203500970526     C*
203600970526     C* EMISSIONE MESSAGGIO DI ERRORE
203700970526     C                   MOVEL     MSG(1)        DSMSMS
203800970526     C                   ELSE
203900970526     C*
204000970526     C* OPZIONE ATTIVA (PROCEDI ...)
204100970526     C*
204200970526     C     OPZKEY        IFEQ      ' 1'
204300970526     C                   EXSR      O01GES
204400970526     C                   END
204500970526     C     OPZKEY        IFEQ      ' 2'
204600970526     C                   EXSR      O02GES
204700970526     C                   END
204800991014     C     OPZKEY        IFEQ      ' 3'
204900991014     C                   EXSR      O03GES
205000991014     C                   END
205100000307     C     OPZKEY        IFEQ      ' 4'
205200000307     C                   EXSR      O04GES
205300000307     C                   END
205400970526     C     OPZKEY        IFEQ      ' 5'
205500970526     C                   EXSR      O05GES
205600970526     C                   END
205700000307     C     OPZKEY        IFEQ      ' 7'
205800000307     C                   EXSR      O07GES
205900000307     C                   END
206000000607     C     OPZKEY        IFEQ      ' 9'
206100000607     C                   EXSR      O09GES
206200000607     C                   END
206300970526     C                   END                                                    OPZ.NON ATTIVA
206400970526     C*
206500970526     C                   ENDSR
206600970526     C*------------------------------------------------------------------------*
206700970526     C* NOTE COSTRUZIONE SCHIERE
206800970526     C*------------------------------------------------------------------------*
206900970526     C*
207000970526     C* -MSG-MESSAGGI
207100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
207200970526     C* (NON TOCCARE MSG,1 E MSG,2)
207300970526     C*
207400970526     C* -CMD-COMANDI
207500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
207600970526     C*
207700970526     C* -OPD-OPZIONI
207800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
207900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
208000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
208100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
208200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
208300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
208400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
208500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
208600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
208700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
208800970526     C*
208900970526     C* -FUD-FUNZIONI
209000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
209100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
209200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
209300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
209400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
209500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
209600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
209700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
209800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
209900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
210000970526     O*------------------------------------------------------------------------*
210100970526** DEC - DECODIFICHE
210200970526    GESTIONE                                                     1
210300970526INTERROGAZIONE                                                   2
210400970530     SCELTA                                                      3
2105009705264
2106009705265
210700970526** MSG - MESSAGGI
210800991001TIBS12R- Opzione non gestita                                                   1
210900991001TIBS12R- Tasto funzionale non gestito                                          2
211000000307TIBS12R-                                                                       3
211100991001TIBS12R- Record annullato. Consentito solo il ripristino                       4
211200991001TIBS12R- Record non annullato. Impossibile ripristinare                        5
211300991001TIBS12R- Accettato solo un parametro di inizio lista                           6
211400060113TIBS12R- Errore in caricamento dati utente, avvisare il CED                    7
211500991001TIBS12R- 8                                                                     8
211600991001TIBS12R- 9                                                                     9
211700991001TIBS12R- 10                                                                   10
211800000307TIBS12R- Tipo legame errato.                                                  11
211900000307TIBS12R- Data errata.                                                         12
212000000807TIBS12R- Tipo legame non autorizzato.                                         13
212100060113TIBS12R- Utente non abilitato alla gestione del cliente "Padre" scelto.       14
212200991001TIBS12R- 15                                                                   15
212300991001TIBS12R- 16                                                                   16
212400991001TIBS12R- 17                                                                   17
212500991001TIBS12R- 18                                                                   18
212600991001TIBS12R- 19                                                                   19
212700991001TIBS12R- 20                                                                   20
212800970528** CMD - COMANDI CL
212900970528WRKSPLF
213000970528
213100970528
213200970528
213300970528
213400970526** OPD - OPZIONI
213500970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
213600970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
213700991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
213800000307 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
213900970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
214000970526                   -6 -     (S/N)=N                        opz. 06¬
214100000307 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
214200970527                   -8 -     (S/N)=N                        opz. 08¬
214300000607 9=Cliente abili.  -9 -     (S/N)=N                        opz. 09¬
214400970527                   -10-     (S/N)=N                        opz. 10¬
214500970526                   -  -     (S/N)=N                        opz. 11¬
214600970526                   -  -     (S/N)=N                        opz. 12¬
214700970526                   -  -     (S/N)=N                        opz. 13¬
214800970526                   -  -     (S/N)=N                        opz. 14¬
214900970526                   -  -     (S/N)=N                        opz. 15¬
215000970526                   -  -     (S/N)=N                        opz. 16¬
215100970526                   -  -     (S/N)=N                        opz. 17¬
215200970526                   -  -     (S/N)=N                        opz. 18¬
215300970526                   -  -     (S/N)=N                        opz. 19¬
215400970526                   -  -     (S/N)=N                        opz. 20¬
215500970526                   -  -     (S/N)=N                        opz. 21¬
215600970526                   -  -     (S/N)=N                        opz. 22¬
215700970526                   -  -     (S/N)=N                        opz. 23¬
215800970526                   -  -     (S/N)=N                        opz. 24¬
215900970526** FUD - FUNZIONI
216000970526                   -        (S/N)=N                        funz.01¬
216100970526                   -        (S/N)=N                        funz.02¬
216200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
216300970526                   -        (S/N)=N                        funz.04¬
216400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
216500970526F06=Inserimento    -        (S/N)=N                        funz.06¬
216600970526                   -        (S/N)=N                        funz.07¬
216700970527                   -        (S/N)=N                        funz.08¬
216800970527                   -        (S/N)=N                        funz.09¬
216900970527                   -        (S/N)=N                        funz.10¬
217000970526                   -        (S/N)=N                        funz.11¬
217100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
217200970526                   -        (S/N)=N                        funz.13¬
217300970526                   -        (S/N)=N                        funz.14¬
217400970526                   -        (S/N)=N                        funz.15¬
217500970526                   -        (S/N)=N                        funz.16¬
217600970526                   -        (S/N)=N                        funz.17¬
217700970526                   -        (S/N)=N                        funz.18¬
217800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
217900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
218000000808F21=Stampa elenco  -        (S/N)=N                        funz.21¬
218100970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
218200970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
218300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
