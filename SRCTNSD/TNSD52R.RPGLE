000100921019     H DECEDIT('0,') DATEDIT(*DMY.)
000200960913     F* TNSD52R *-----------------------------------------------------*
000300131105     F* CREAZIONE AUTOMATICA BOLLE
000400040517      *
000500040517      *    A T T E N Z I O N E
000600040517      *
000700040517      * Dopo aver generato le bolle per tutti i p.o. validi presenti su azorg
000800040518      * è stata chiodata la generazione di una spedizione per la 212 con linea di arrivo
000900101015      * 222  (la linea di arrivo la prendo dall'organigramma)
001000921019     F*---------------------------------------------------------------*
001100960913     FTNSD52D   CF   E             WORKSTN
001200021029     FFIVABWKF  O    E             DISK
001300021029     F                                     RENAME(FIVAB000:FIVABWRK)
001400131024     FFIVAB46F  O  A E             DISK
001500921026     FAZORG01L  IF   E           K DISK
001600921019     FTABEL00F  IF   E           K DISK
001700101015     FTNTBE01L  IF   E           K DISK
001800941228     FFNLV22P   O    F  132        PRINTER
001900960916     D FER             S              3  0 DIM(11)                              filiali in errore
002000960916     D K78             S              1    DIM(78)                              SKI DI COMODO
002100960913     D MSG             S             78    DIM(2) CTDATA PERRCD(1)              MSG DI ERRORE
002200131025     D TES             S             15    DIM(3) CTDATA PERRCD(1)
002300131104     D ovr             S             80    DIM(1) CTDATA PERRCD(1)
002400131025     d
002500040517     d kazorg          s                   like(orgfil)
002600131024     d
002700960104     D WGGMM           DS
002800960104     D  WGG                    1      2  0
002900960104     D  WMM                    3      4  0
003000131024     d
003100921019     D KPJBA         E DS
003200921019     D DSBLD         E DS
003300971223     D DSBLDE        E DS                  INZ
003400921019     D DS3A          E DS
003500941228     D DS5E          E DS
003600101015     D DLDS          E DS
003700101015     D DNTW          E DS
003800921019     D CNCR80        E DS
003900101015     D OG143         E DS
004000101015     D OG147         E DS
004100101015     D OG148         E DS
004200971231     D DSLV55        E DS                  EXTNAME(FNLV55DS)
004300041201     d trul33ds      e ds
004400101015     D tibs02ds      E DS
004500131024     D trul13ds      E DS
004600131024     D  tI13Tla      e                     EXTFLD(I13Tla)
004700131024     D  tO13Err      e                     EXTFLD(O13Err)
004800131024     D  tO13Msg      e                     EXTFLD(O13Msg)
004900131024     d
005000921019     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005100131024     d
005200960118     D* STAMPANTE SEGNACOLLI
005300030626     D* DS ESTERA PER  STAMPA BOLLE
005400030626     d Fnlsb5ds      e ds
005500131024     d
005600970721     D* DS PER LANCIO PGM CONTROLLO CAP (FNLV13R)
005700970721     D DSLV13        E DS                  EXTNAME(FNLV13DS)
005800970721     D DSSI95        E DS                  EXTNAME(TISI95DS)
005900131024     d
006000980107     D WLBDAT          DS                  INZ
006100980107     D  G02DAT                 1      8  0
006200980107     D  G02INV                 9     16  0
006300980107     D  G02ERR                17     17
006400980107     D  G02TGI                18     22  0
006500980123     D DSACO         E DS                  EXTNAME(CNACO00F)
006600980123     D DSIND         E DS                  EXTNAME(CNIND00F)
006700980123     D DSCLP         E DS                  EXTNAME(CNCLP00F)
006800980123     D DSCLS         E DS                  EXTNAME(FNCLS00F)
006900980123     D DSBS69        E DS                  EXTNAME(TIBS69DS)
007000101018     d tntb82ds      e ds
007100131024
007200030626      * Parametri per richiesta stampanti
007300030626     d TRUL90DS      e ds
007400101015     d kcod            s                   like(tbecod)
007500101015     d kke1            s                   like(tbeke1)
007600131104     d savlista        s                   like(vlista)
007700131104     d datapiuGG       s              8  0
007800131104     d dateuGG         s              8  0
007900131104     d Indx            s              3  0
008000101015     d W105a           s            105
008100131024     d datapiu         s              8  0
008200131024     d wdsp            s              8  0
008300921019     C****************************************************************
008400921019     C*  RIEPILOGO INDICATORI
008500921019     C***************************************************************
008600131024     C* 01    - INSERITA DATA SPEDIZIONE POSTICIPATA
008700921103     C* 21    - RICERCA NUMERO BOLLA
008800960104     C* 28    - CAMPO MESSAGGI
008900921102     C* 31/38 - ERRORE FORMATO  VIDEO 2
009000921102     C* 51/57 - CHAIN NON AVVENUTE
009100960916     C* 80    - INDICA LA RICHIESTA DI STAMPA VETTURA
009200131024     C* 90    - ERRORE GENERICO SUL PRIMO FORMATO
009300960916     C* 95    - LETTURA FILE AZORG
009400921019     C*****************************************************************
009500921019     C     *ENTRY        PLIST
009600921019     C                   PARM                    KPJBA
009700030626     c                   Parm                    trul90ds
009800960118     C*
009900921019     C                   Z-ADD     1             CODUT
010000921019     C                   CALL      'X§PARUT'
010100921019     C                   PARM                    UT§DSE
010200921019     C                   MOVEL     RAGUT         RSUT             20
010300921019     C                   MOVEL     REC80         CNCR80
010400921019     C*----------------------------------------------------*
010500921019     C     KTAB2         KLIST
010600921019     C                   KFLD                    CODUT
010700921019     C                   KFLD                    COD               2
010800921019     C                   KFLD                    KEY               8
010900101015     C     KTbe          KLIST
011000101015     C                   KFLD                    kcod
011100101015     C                   KFLD                    kke1
011200921019     C*----------------------------------------------------*
011300960104     C     *LIKE         DEFINE    VABRMN        NUMBOL
011400960104     C     *LIKE         DEFINE    VABRMN        COMRMN
011500980107     C*
011600980107     C* REPERISCO DATA DEL SISTEMA
011700980107     C                   TIME                    W0140            14 0
011800980107     C                   MOVE      W0140         WDTGIO            8 0
011900980107     C                   Z-ADD     WDTGIO        G02DAT
012000980107     C                   MOVEL     *BLANK        G02ERR
012100980107     C                   CALL      'XSRDA8'
012200980107     C                   PARM                    WLBDAT
012300980107     C                   MOVEL     G02INV        DATEU             8 0
012400131104     C                   MOVEL     G02dat        DATEUGG           8 0
012500131024      * Richiamo pgm che calcola la prima data lavorativa disponibile
012600131024     c                   Clear                   Trul13ds
012700131024     c                   Eval      TI13Tla = 'L'
012800131024     c                   Eval      I13Mod = 'P'
012900131024     c                   Eval      I13Fil = 001
013000131024     c                   Eval      I13Din = dateu
013100160504     c                   Eval      I13gio = 2
013200131024     c                   Call      'TRUL13R'
013300131024     c                   Parm                    Trul13ds
013400131024     c
013500131024     c                   If        TO13Err = *Blanks and o13dfi>0
013600131024     c                   Eval      datapiu = O13Dfi
013700131024     c                   else
013800131024     c                   eval      datapiu=dateu
013900131024     c                   EndIf
014000131104     c* datapiu in ggmmaa
014100131104     C                   Z-ADD     datapiu       G02inv
014200131104     C                   MOVEL     3             G02ERR
014300131104     C                   CALL      'XSRDA8'
014400131104     C                   PARM                    WLBDAT
014500131104     c                   eval      datapiuGG=g02dat
014600131024     C
014700131025     c                   select
014800131025     c                   when      %subst(kpjbu:20:3)='ACQ'
014900131025     c                   eval      vidtes2=tes(1)
015000131025     c                   when      %subst(kpjbu:20:3)='SED'
015100131025     c                   eval      vidtes2=tes(2)
015200131025     c                   other
015300131025     c                   eval      vidtes2=tes(3)
015400131025     c                   endsl
015500001003     C*
015600921019     C*----------------------------------------------------*
015700960913     C* PULIZIA CAMPI
015800960917     C                   EXSR      PULIZ
015900921102     C*
016000101015    1C     *IN90         DOWEQ     *ON
016100960916     C     *INKC         ANDEQ     *OFF
016200101015     C                   SETOFF                                       90
016300921019     C* EMMETTO IL PRIMO FORMATO
016400960916     C                   EXFMT     SD52D01
016500101015     C* F3 FINE LAVORO
016600960916     C  NKC              EXSR      CTRFM1
016700921026     C*
016800960104     C* SCRIVO AGGIORNO LE BOLLE CON I NUOVI DATI
016900960104     C* E LANCIO PGM SEGNACOLLI E STAMPA LETTERA DI VETTURA
017000101015     C  N90
017100960916     CANNKC
017200960916    2CAN KF              DO
017300960916     C                   EXSR      CREA
017400101015     c
017500101015     c* Creao per la 212 se richisti i primi livello o tutte le filiali e non
017600101015     c*  selezionato il NTW
017700101015     c                   if        (vpunti= '1' or vpunti='T')  and
017800101015     c                             vntw=*blanks
017900040517     c                   ExSr      Crea212
018000101015     c                   endif
018100960916     C*
018200960916     C* SE HO TROVATO ALMENO UN ERRORE, EMETTO MESSAGGIO
018300960916     C     FER(1)        IFNE      0
018400960916     C                   EXSR      EMIERR
018500960917     C                   ELSE
018600960917     C                   EXSR      PULIZ
018700960916     C                   ENDIF
018800960916     C*
018900960916    2C                   ENDDO
019000960916     C*
019100101015     C                   SETON                                        90
019200960916    1C                   ENDDO
019300921026     C*
019400960104     C*  CHIUSURA PGM E FINE
019500960104     C     *INKC         IFEQ      *ON
019600030626     C                   CLEAR                   Fnlsb5ds
019700960104     C                   MOVEL     'C'           DB0TLA
019800030626     C                   CALL      d90psl
019900030626     C                   PARM                    Fnlsb5ds
020000970721     C                   CLEAR                   DSLV13
020100960104     C*
020200970721     C                   MOVEL     'C'           I13TLA
020300970721     C                   CALL      'FNLV13R'
020400970721     C                   PARM                    KPJBA
020500970721     C                   PARM                    DSLV13
020600970721     C                   PARM                    DSSI95
020700971231     C*
020800971231     C                   MOVEL     'C'           D55TLA
020900971231     C                   CALL      'FNLV55R'
021000971231     C                   PARM                    DSLV55
021100980123     C*
021200980123     C                   MOVEL     'C'           I69TLA
021300980123     C                   CALL      'TIBS69R'
021400980123     C                   PARM                    DSBS69
021500041201     C*
021600041201     c                   clear                   trul33ds
021700041201     C                   MOVEL     'C'           I33TLA
021800041201     c                   movel(p)  trul33ds      kpjbu
021900041201     C                   CALL      'TRUL33R'
022000041201     C                   PARM                    KPJBA
022100041201     C*
022200960104     C*
022300960104     C                   SETON                                        LR
022400960104     C                   ENDIF
022500921026     C*---------------------------------------------------------------
022600960916     C*   CTRFM1  CONTROLLO FORMATO1
022700921026     C*---------------------------------------------------------------
022800960916     C     CTRFM1        BEGSR
022900960916     C                   CLEAR                   DS3A
023000960916     C                   CLEAR                   DS5E
023100131104     C                   CLEAR                   DLDS
023200960913     C*
023300921026     C* CONTROLLO  IL NUMERO COLLI
023400960104    2C     VNUCOL        IFEQ      *ZEROS
023500101015     C                   SETON                                          3190
023600101015     c                   leavesr
023700960104    2C                   END
023800921026     C* CONTROLLO  IL PESO
023900101015    2C     VPESO         IFEQ      *ZEROS
024000101015     C                   SETON                                          3290
024100101015     c                   leavesr
024200960104    2C                   END
024300921026     C* CONTROLLO  IL TIPO BOLLA
024400101015    2C     VTIBOL        IFEQ      *BLANK
024500101015     C                   SETON                                          3490
024600101015     c                   leavesr
024700960104   X2C                   ELSE
024800921026     C* CONTROLLO LA VERIDICITA' DEL CODICE
024900921026     C                   MOVEL     '3A'          COD
025000921026     C                   MOVEL     *BLANKS       KEY
025100921102     C                   MOVEL     VTIBOL        KEY
025200921026     C     KTAB2         CHAIN     TABEL                              52
025300921026     C* CONTROLLO SE ANNULLATO
025400960104    3C  N52TBLFLG        IFNE      ' '
025500921026     C                   SETON                                        52
025600960104    3C                   END
025700101015     C   52              SETON                                        3490
025800921026     C  N52              MOVEL     TBLUNI        DS3A
025900921026     C* VEDO SE CAUSALE UTILIZZABILE
026000960104    3C  N52§3AUCB        IFEQ      'N'
026100101015     C                   SETON                                        3490
026200101015     c                   leavesr
026300960104    3C                   END
026400921026     C*
026500960104    2C                   END
026600101018     c* controllo se si vuole visualizzare la lista per i punti scelti
026700101018    1c                   if        vpunti= ' ' and vcrea='S'
026800101018     C                   SETON                                        4490
026900101018     c                   leavesr
027000101018    1C                   END
027100101018     c
027200101018     c* Lista distribuzione in alternativa alla scelta punti
027300101018    1c                   if        vpunti<>' ' and vlista<>*blanks
027400101018     C                   SETON                                        4090
027500101018     c                   leavesr
027600101018    1C                   END
027700101018     c
027800101018    1c                   if        vcrea='S'
027900101018     c                   clear                   tntb82ds
028000101018     c                   eval      §tb82tipol=vpunti
028100101018    2c                   if        vpunti='1'
028200101018     c                   eval      §tb82lis='TERMINAL'
028300101018     c                   eval      §tb82dlis='Terminal partenza+212'
028400101018    2c                   endif
028500101018    2c                   if        vpunti='2'
028600101018     c                   eval      §tb82lis='SECONDI '
028700101018     c                   eval      §tb82dlis='Secondi livello '
028800101018    2c                   endif
028900101018    2c                   if        vpunti='T'
029000101018     c                   eval      §tb82lis='TUTTE FIL'
029100101018     c                   eval      §tb82dlis='Tutte filiali +212'
029200101018    2c                   endif
029300101018    2c                   if        vpunti='P'
029400101018     c                   eval      §tb82lis='PICKING  '
029500101018     c                   eval      §tb82dlis='Picking-Rulliera'
029600101018    2c                   endif
029700101018     c
029800101018     c                   eval      kpjbu=TNTB82ds
029900101018     c                   call      'TNTB82R'
030000101018     c                   parm                    kpjba
030100101018     c
030200101018     c                   clear                   vcrea
030300101018     c* Se è stata creata la lista pulisco i campi del tipo e la imposto
030400101018     c                   eval      tntb82ds=kpjbu
030500101018    2c                   if        §tb82crea='S'
030600101018     c                   clear                   vpunti
030700101018     c                   eval      vlista=§tb82lis
030800101018     c                   eval      vdlista=§tb82dlis
030900101018    2C                   END
031000101018     c                   seton                                        90
031100101018     c                   leavesr
031200101018    1C                   END
031300101015     c
031400101015     c* '?' su lista
031500101015     c                   clear                   vdlista
031600101015     c                   eval      Indx=%scan('?':vlista)
031700101015    1c                   if        Indx>0
031800101015     c                   Clear                   tibs02ds
031900101015     c                   Eval      T02Mod = 'R'
032000101015     c                   Eval      T02Sif = Knsif
032100101015     c                   Eval      T02Cod = 'LDS'
032200101015     c                   Call      'TIBS02R'
032300101015     c                   Parm                    Kpjba
032400101015     c                   Parm                    tibs02ds
032500101015    2c                   if        t02ke1=*blanks  and t02err=*blanks
032600101015     c                   Movel     T02Ke2        vlista
032700101015     c                   Movel     T02uni        vdlista
032800131106     c                   Movel     T02uni        Dlds
032900101015     c                   else
033000101015     c                   clear                   vlista
033100101015     c                   clear                   vdlista
033200131104     c                   clear                   Dlds
033300101015     c                   endif
033400101015     c
033500131106     c****               exsr      propDSP
033600101015     c                   seton                                        90
033700101015     c                   leavesr
033800101015    1c                   endif
033900101018     c
034000101018     c                   if        vlista=*blanks and vvisl='S'
034100101018     C                   SETON                                        4590
034200101018     c                   leavesr
034300101018    1C                   END
034400921026     C*
034500101015    1c                   if        vlista<>*blanks
034600101015     c                   Clear                   tibs02ds
034700101015     c                   Eval      T02Mod = 'C'
034800101015     c                   Eval      T02Sif = Knsif
034900101015     c                   Eval      T02Cod = 'LDS'
035000101015     c                   Eval      T02ke2 = vlista
035100101015     c                   Call      'TIBS02R'
035200101015     c                   Parm                    Kpjba
035300101015     c                   Parm                    tibs02ds
035400101015    2c                   if        t02err<>*blanks
035500101015     c                   seton                                        4190
035600101015     c                   leavesr
035700101015     c                   else
035800131104     c                   movel     t02uni        dlds
035900131104     c                   movel     t02uni        vdlista
036000101015    2c                   endif
036100101018
036200101018    2c                   if        vvisl='S'
036300101018     c                   clear                   tntb82ds
036400101018     c                   eval      §tb82lis=vlista
036500101018     c
036600101018     c                   eval      kpjbu=TNTB82ds
036700101018     c                   call      'TNTB82R'
036800101018     c                   parm                    kpjba
036900101018     c
037000101018     c                   clear                   vvisl
037100101018     c                   seton                                        90
037200101018     c                   leavesr
037300101018    2c                   endif
037400131104     c
037500131104     c* se immessa nuova lista propongo la data in base a quanto detto in tabella
037600131104     c*  solo la prima volta
037700131106     c**                 exsr      propDSP
037800131106     c**
037900131104     c                   else
038000131104     c                   clear                   savlista
038100131105     c                   clear                   dlds
038200101018    1c                   endif
038300101015     c
038400101015     c* '?' su NTW
038500101015     c                   clear                   vdntw
038600101015     c                   eval      Indx=%scan('?':vntw)
038700101015    1c                   if        Indx>0
038800101015     c                   Clear                   tibs02ds
038900101015     c                   Eval      T02Mod = 'R'
039000101015     c                   Eval      T02Sif = Knsif
039100101015     c                   Eval      T02Cod = 'NTW'
039200101015     c                   Call      'TIBS02R'
039300101015     c                   Parm                    Kpjba
039400101015     c                   Parm                    tibs02ds
039500101015    2c                   if         t02err=*blanks
039600101015     c                   Movel     T02Ke1        vntw
039700101015     c                   Movel     T02uni        vdntw
039800101015     c                   else
039900101015     c                   clear                   vntw
040000101015     c                   clear                   vdntw
040100101015     c                   endif
040200101015     c
040300101015     c                   seton                                        90
040400101015     c                   leavesr
040500101015    1c                   endif
040600101015     C*
040700101015    1c                   if        vntw  <>*blanks
040800101015     c                   Clear                   tibs02ds
040900101015     c                   Eval      T02Mod = 'C'
041000101015     c                   Eval      T02Sif = Knsif
041100101015     c                   Eval      T02Cod = 'NTW'
041200101015     c                   Eval      T02ke1 = vntw
041300101015     c                   Call      'TIBS02R'
041400101015     c                   Parm                    Kpjba
041500101015     c                   Parm                    tibs02ds
041600101015    2c                   if        t02err<>*blanks
041700101015     c                   seton                                        4290
041800101015     c                   leavesr
041900101015     c                   else
042000101015     c                   movel     t02uni        vdntw
042100101015     c                   movel     t02uni        dntw
042200101015     c
042300101015     c                   if        §ntwcrb<>'S'
042400101015     c                   seton                                        4390
042500101015     c                   leavesr
042600101015    2c                   endif
042700101015     c
042800101015    2c                   endif
042900101015    1c                   endif
043000131024     C
043100131024     C* DATA SPEDIZIONE
043200131024     c* controllo data spedizione: max + 1 gg lavorativ
043300131024     C                   Z-ADD     vdsp          G02DAT
043400131024     C                   MOVEL     *BLANK        G02ERR
043500131024     C                   CALL      'XSRDA8'
043600131024     C                   PARM                    WLBDAT
043700131024     c
043800131024    1C     G02ERR        IFEQ      '1'
043900131024     c                   seton                                        3990
044000131024     c                   leavesr
044100131024     c                   else
044200131024     C                   MOVEL     G02INV        wdsp              8 0
044300131024     c                   endif
044400131024
044500131024     c                   if        wdsp>datapiu
044600131024     c                             or wdsp<dateu
044700131024     c                   seton                                        3990
044800131024     c                   endif
044900131024     c
045000131024     c                   if        wdsp>dateu
045100131024     c                   seton                                        01
045200131105     c                   else
045300131105     c                   setoff                                       01
045400131024     c                   endif
045500131106     c
045600131106     c                   if        vcong='S'
045700131106     c                   seton                                        02
045800131106     c                   else
045900131106     c                   setoff                                       02
046000131106     c                   endif
046100131024     c
046200960913     C                   ENDSR
046300131104     C*---------------------------------------------------------------
046400131104     C*   Proposta data spedizione per conferma bolle
046500131104     C*---------------------------------------------------------------
046600131106     c**   PropDSP       BEGSR
046700131106     c**                 if        vlista<>savlista and vlista <>*blanks
046800131106     c**                 setoff                                       02
046900131104
047000131106     c**                 if        §LDSPOST='S'
047100131106     c**                 eval      vdsp=datapiuGG
047200131106     c**                 seton                                        01
047300131106     c**                 else
047400131106     c**                 eval      vdsp=dateuGG
047500131106     c**                 endif
047600131104
047700131106     c**                 if        §LDSLVDH='S'
047800131106     c**                 seton                                        02
047900131106     c**                 endif
048000131105     c
048100131106     c**                 eval      savlista=vlista
048200131106     c*+                 seton                                        90
048300131105     c
048400131106     c**                 endif
048500131106     c**                 ENDSR
048600921026     C*---------------------------------------------------------------
048700960916     C*   CREA    CREAZIONE BOLLE
048800921026     C*---------------------------------------------------------------
048900960916     C     CREA          BEGSR
049000960916     C                   SETOFF                                       80
049100960916     C                   CLEAR                   FER
049200960916     C                   Z-ADD     1             X                 3 0
049300131104
049400131105     c* Se devo congelare la stampa lo faccio
049500131106     c                   if        vcong='S'
049600131104     C                   MOVEA     ovr           COMMAN           85
049700131104     c
049800131104     C                   Z-ADD     80            LUNG             15 5
049900131104      *
050000131104     C                   CALL      'QCMDEXC'                              99
050100131104     C                   PARM                    COMMAN
050200131104     C                   PARM                    LUNG
050300131105     c                   endif
050400131104      *
050500960916     C*
050600960916     C* VEDO SE SI VUOLA LA STAMPA DELLA LETTERA DI VETTURA
050700960916     C     VCONF         IFEQ      'S'
050800960916     C     VSINOV        ANDEQ     'S'
050900960916     C                   SETON                                        80
051000960916     C                   ENDIF
051100101015     C*
051200101015     c* S E L E Z I O N E   F I L I A L I   D A    O R G A N I G R A  M M A
051300960916     C*
051400101015    0C                   IF        VPUNTI<>*BLANKS
051500101015     C
051600960916     C     *LOVAL        SETLL     AZORG01L
051700960916     C                   READ      AZORG01L                               95
051800960916    1C     *IN95         DOWEQ     *OFF
051900960916     C*
052000960916    2C     ORGFVA        IFEQ      ' '
052100960917     C     ORGFAG        ANDEQ     'A'
052200960917     C     ORGFVA        OREQ      ' '
052300960917     C     ORGFAG        ANDEQ     'F'
052400101015     c* Per le logistiche entro comunque perchè il tipo è "V"
052500101015     c     orgfva        oreq      ' '
052600101015     c     vntw          andeq     'LOG'
052700101015     c
052800960916     C                   MOVEL     ORGDE7        OG147
052900101015     C                   MOVEL     ORGDE3        OG143
053000101015     c
053100960916     C* DA NON CREARE  SE = 'N'
053200960916   2AC     §OGNOC        IFNE      'N'
053300980107     C* DETERMINO TERMINAL DI PARTENZA DEL P.O.
053400980107     C                   CLEAR                   DSLV55
053500980107     C                   MOVEL     'P'           D55TPT
053600980107     C                   MOVE      ORGFIL        D55LIN
053700980107     C                   MOVE      DATEU         D55DRF
053800980107     C                   CALL      'FNLV55R'
053900980107     C                   PARM                    DSLV55
054000960916     C*
054100960916    3C     VPUNTI        IFEQ      '1'
054200980107     C     D55TFP        ANDEQ     ORGFIL
054300960916     C*
054400960916     C     VPUNTI        OREQ      '2'
054500980107     C     D55TFP        ANDNE     ORGFIL
054600960916     C*
054700960916     C     VPUNTI        OREQ      'A'
054800980107     C     D55TFP        ANDNE     ORGFIL
054900960916     C     ORGFAG        ANDEQ     'A'
055000960916     C*
055100980107     C     VPUNTI        OREQ      'P'
055200980107     C     D55TFP        ANDNE     ORGFIL
055300960916     C     ORGFAG        ANDEQ     'F'
055400980109     C*
055500101015     C     VPUNTI        OREQ      'T'
055600101015     c
055700101015     c* Vedo che ntw scelto
055800101015     c                   if        vntw=*blanks   or vntw=§ogntw
055900101015     c
056000101015     c                   exsr      ScriviVAB
056100101015     C
056200971202    4C                   ENDIF
056300960916    3C                   ENDIF
056400960916   2AC                   ENDIF
056500960916    2C                   ENDIF
056600960916     C*
056700960916     C                   READ      AZORG01L                               95
056800960916    1C                   ENDDO
056900101015    0C                   ENDIF
057000921026     C*
057100101015     C* L I S T A   D I S T R I B U Z I O N E
057200101015     C*
057300101015    0C                   IF        vlista<>*blanks
057400101015     c                   eval      kcod='LDS'
057500101015     c                   eval      kke1=vlista
057600101015     c     ktbe          setll     tntbe01l
057700101015     c     ktbe          reade     tntbe01l
057800101015     c
057900101015    1c                   dow       not %eof(tntbe01l)
058000101015     c                   movel     tbeuni        dlds
058100101015     c     §ldsfil       chain     azorg01l
058200101015    2c                   if        %found(azorg01l)
058300101015     c
058400101015     c                   exsr      ScriviVAB
058500101015     C
058600101015    2c                   endif
058700101015     c
058800101015     c     ktbe          reade     tntbe01l
058900101015    1c                   enddo
059000101015     c
059100101015    0C                   ENDIF
059200921026     C                   ENDSR
059300101015     C*---------------------------------------------------------------
059400101015     C*   Scrittura FIVAB
059500101015     C*---------------------------------------------------------------
059600101015     c     ScriviVAB     BEGSR
059700101015     c
059800101015     C                   CLEAR                   FIVAB000
059900101015     C                   CLEAR                   FIVABWRK
060000101015     C**
060100101015     C* IMPOSTO I DATI DEL DESTINATARIO
060200101015     C                   MOVEL     ORGDES        VABRSD
060300101015     C                   MOVEL     ORGIND        VABIND
060400101015     C                   MOVEL     ORGCPF        VABCAD
060500101015     C                   MOVEL     ORGLOC        VABLOD
060600101015     C                   MOVEL     ORGPRO        VABPRD
060700101015     C* CODICE BOLLA PARTENZA
060800101015     C                   MOVE      VTIBOL        VABCBO
060900101015     C* COLLI
061000101015     C                   Z-ADD     VNUCOL        VABNCL
061100101015     C* PESO
061200101015     C                   Z-ADD     VPESO         VABPKB
061300101015     C* VOLUME
061400101015     C                   Z-ADD     VVOLUM        VABVLB
061500101015     C* NOTE
061600101015     C                   MOVEL     VNOTE         VABNOT
061700101015     C                   MOVEL     VNOTE2        VABNT2
061800101015     C* NATURA MERCE
061900101015     C                   MOVEL     VNATUR        VABNAS
062000101015     C* FERMI DEPOSITO
062100101015    1C     VFERMO        IFEQ      'SI'
062200101015     C                   MOVEL     'S'           VABFFD
062300150216     C                   MOVEL     'C'           VABTSP
062400101015     C                   ELSE
062500101015     C                   MOVE      ' '           VABFFD
062600150216     C                   MOVEL     'E'           VABTSP
062700101015    1C                   END
062800101015     C* NEL RIFERIMENTO MITTENTE METTO IL NUMERO DELLA FILIALE
062900101015     C                   Z-ADD     ORGFIL        VABRMN
063000101015     C* IMPOSTO COMUNQUE LA LNA E LA ZONA
063100101015     C                   MOVE      *ZEROS        VABZNC
063200101015     C                   MOVE      ORGFIL        VABLNA
063300101015     c
063400101015     c* Se piena linea per invio materiale di sede, la imposto
063500101015     c                   movel     orgde8        og148
063600150216     c                   movel     orgde3        og143
063700101015     c                   if        §ogfiue>*zeros
063800101015     c                   move      §ogfiue       vablna
063900150216     c* se si tratta di logistica tolgo il fermo deposito
064000150216     c                   if        §ogntw='LOG'
064100150216     C                   MOVE      ' '           VABFFD
064200150216     C                   MOVEL     'E'           VABTSP
064300101015     c                   endif
064400150216     c                   endif
064500101015     c
064600101015     C     ORGDIT        IFEQ      'SDI'
064700101015     C                   MOVEL     'C'           VABTSP
064800101015     C                   END
064900101015     c**
065000101015     c* Imposto i campi dalla tabella LDS se pieni
065100101015    1c                   if        vlista<>*blanks
065200101015     c
065300101015    2c                   if        §ldsncl>0
065400101015     c                   eval      vabncl=§ldsncl
065500101015    2c                   endif
065600101015    2c                   if        §ldspkg>0
065700101015     c                   eval      vabpkb=§ldspkg
065800101015     c                   clear                   vabvlb
065900101015    2c                   endif
066000101015    2c                   if        §ldsca<>*blanks
066100101015     c                   select
066200101015    3c                   when      vabnot=*blanks
066300101015     c                   eval      vabnot='C.A. sig. '+§ldsca
066400101015     c
066500101015    3c                   when      vabnt2=*blanks
066600101015     c                   eval      vabnt2='C.A. sig. '+§ldsca
066700101015   x3c                   other
066800101015     c*
066900101015     c                   eval      w105a='C.A. sig. '+%trim(§ldsca) + ' ' +
067000101015     c                             %trim(vabnot) + ' ' + %trim(vabnt2)
067100101015     c                   eval      vabnot=w105a
067200101015     c                   eval      vabnt2=%subst(w105a:36:35)
067300101015    3c                   endsl
067400101015    2c                   endif
067500101015    1c                   endif
067600101015     C**
067700101015     C* SE NON SI VUOLE LA CONFERMA AUTOMATICA, SCRIVO VABWKF
067800101015    1C     VCONF         IFEQ      'N'
067900101015     C                   WRITE     FIVABWRK
068000101015   X1C                   ELSE
068100101015     C* VALORIZZO I CAMPI NON ANCORA VALORIZZATI
068200101015     C*
068300101015     C* ANNO SPEDIZIONE
068400101015     C                   TIME                    W0140            14 0
068500101015     C* UDATE IN GGMMAAAA
068600131024     C***                MOVE      W0140         WDTGIO            8 0
068700131024     C***                MOVE      WDTGIO        VABAAS
068800131024     C***                MOVEL     WDTGIO        WGGMM
068900101015     C* MESE/GIORNO SPEDIZIONE
069000131024     C***                MOVEL     WMM           VABMGS
069100131024     C***                MOVE      WGG           VABMGS
069200131024     c                   movel     wdsp          vabaas
069300131024     c                   move      wdsp          vabmgs
069400131024     c
069500101015     C* LINEA  DI PARTENZA
069600101015     C                   Z-ADD     102           VABLNP
069700101015     C                   Z-ADD     102           VABFGS
069800101015     C**
069900101015     C* COME PRIMA COSA CONTROLLO SE IL CAP IN ORGANIGRAMMA E' CORRETTO
070000101015     C                   EXSR      CHKCAP
070100101015     C   90              GOTO      ENDCRE
070200101015     C*
070300101015     C     VABPRD        IFEQ      '  '
070400101015     C                   MOVEL     O95PRV        VABPRD
070500101015     C                   ENDIF
070600101015     C* LNA E ZONA SOLO SE NON SDI
070700101015     C     ORGDIT        IFNE      'SDI'
070800101015     C**   O95LNA        IFNE      ORGFIL
070900101015     c** non serve lo fa già sopra
071000101015     C**                 MOVEL     ORGFIL        VABLNA
071100101015     C**                 MOVE      *ZEROS        VABZNC
071200101015     C**                 ELSE
071300101015     c
071400101015     C     O95LNA        IFeq      vablna
071500101015     C***                MOVEL     O95LNA        VABLNA
071600101015     C                   MOVEL     O95ZNC        VABZNC
071700101015     C                   END
071800101015     C                   END
071900101015     c
072000101015     C                   MOVEL     O95CTS        VABCTS
072100110622     C***                MOVEL     O95ISO        WFIN              1
072200101015     C* NUMERO SERIE SPEDIZIONE
072300101015     C                   Z-ADD     01            VABNRS
072400101015     C* NUMERO SPEDIZIONE
072500101015     c* lo prendo da AZNUM con trul33r
072600101015     C                   CLEAR                   TRUL33DS
072700101015     C                   Z-ADD     103           I33CNU
072800101015     C                   Z-ADD     vablnp        I33PO1
072900101015     C                   Z-ADD     1             I33NUM
073000101015     c                   movel(p)  trul33ds      kpjbu
073100101015     c                   call      'TRUL33R'
073200101015     c                   parm                    kpjba
073300101015     c                   movel     kpjbu         trul33ds
073400101015     C                   IF        O33ERR = 0
073500101015     C                   Z-ADD     O33Nrf        vabnsp
073600101015     C                   ELSE
073700101015     C                   CLEAR                   VABNSP
073800101015     C                   ENDIF
073900101015     C
074000101015     C                   Z-ADD     VABNSP        VABRMN
074100101015     C* COD.MITTENTE
074200101015     C                   Z-ADD     0000001       VABCCM
074300101015     C                   MOVEL     VABLNP        VABCCM
074400101015     C* RICERCO IL NUMERO SEGNACOLLI E LANCIO IL PROGRAMMA STAMPA
074500101015     C* SEGNACOLLI
074600101015     c* lo prendo da AZNUM con trul33r
074700101015     C                   CLEAR                   TRUL33DS
074800101015     C                   Z-ADD     109           I33CNU
074900101015     C                   Z-ADD     vablnp        I33PO1
075000101015     C                   Z-ADD     vabncl        I33NUM
075100101015     c                   movel(p)  trul33ds      kpjbu
075200101015     c                   call      'TRUL33R'
075300101015     c                   parm                    kpjba
075400101015     c                   movel     kpjbu         trul33ds
075500101015     C                   IF        O33ERR<> 0
075600101015     C*
075700101015     C                   Z-ADD     0             VABNCD
075800101015     C                   Z-ADD     0             VABNCA
075900101015     C                   ELSE
076000101015     c                   z-add     o33nri        vabncd
076100101015     c                   z-add     o33nrf        vabnca
076200101015     C                   END
076300101015     C*
076400101015     C* SE AL = DAL AZZERO IL CAMPO
076500101015     C     VABNCA        IFEQ      VABNCD
076600101015     C                   Z-ADD     0             VABNCA
076700101015     C                   END
076800101015     C*
076900101015     C* CODICE TRATTAMENTO MERCE
077000101015     C   80              MOVEL     '3 '          VABCTM
077100101015     C*
077200101015     C  N80              MOVEL     '2 '          VABCTM
077300101015     C*
077400101015     C*  WRITE
077500101015     C                   WRITE     FIVAB000
077600101015     C* STAMPO I SEGNACOLLI
077700101015     C* SALVO LA FILIALE CHE STO ELABORANDO
077800101015     C                   MOVEL     ORGFIL        SAVFIL            3 0
077900101015     C*
078000101015     C                   EXSR      GESSNC
078100101015     C*
078200101015     C     SAVFIL        CHAIN     AZORG01L                           95
078300101015     C* STAMPA LETTERA DI VETTURA
078400101015     C   80              EXSR      GESLET
078500101015     C     ENDCRE        TAG
078600101015    1C                   ENDIF
078700040517
078800101015     c                   ENDSR
078900040517      *---------------------------------------------------------------
079000040517      *   CREA212     CREAZIONE BOLLA per p.o. 212
079100040517      *---------------------------------------------------------------
079200040517     c     Crea212       BegSr
079300040517
079400040517     c                   Clear                   Fivabwrk
079500040517     c                   Clear                   Fivab000
079600040517
079700040517      * Il p.o. è il 212 Logistica Albairate
079800040518     c                   Eval      kAzorg = 212
079900040518     c     kAzorg        Chain     Azorg01l
080000040518     c                   If        Not %Found(Azorg01l) or OrgFva <> *Blanks
080100101015     c                   Else
080200101015     c
080300101015     c                   exsr      ScriviVAB
080400101015     c                   endif
080500101015     c
080600040517     c                   EndSr
080700921026     C*---------------------------------------------------------------
080800921026     C*   GESSNC  GESTIONE STAMPA SEGNACOLLI
080900921026     C*---------------------------------------------------------------
081000921026     C*
081100921026     C     GESSNC        BEGSR
081200921026     C*
081300960104     C                   Z-ADD     VABNCD        CO2SCD            7 0
081400960104     C     VABNCA        IFEQ      0
081500960104     C                   Z-ADD     VABNCD        VABNCA
081600921104     C                   END
081700921104     C*
081800921026     C* VALORIZZO LA DS PARAMETRO PASSANTE PER PGM STAMPA SEGNACOLLI
081900921026     C*
082000921026     C                   Z-ADD     0             §LDNC1
082100960104     C                   MOVEL     VABLNP        §LDLNP
082200960104     C     VABLNP        CHAIN     AZORG                              56
082300980128    1C     *IN56         IFEQ      *OFF
082400980128    2C     ORGDE5        IFNE      *BLANKS
082500980128     C                   MOVEL     ORGDE5        §LDDLP
082600980128   X2C                   ELSE
082700980128     C                   MOVEL     ORGDES        §LDDLP
082800980128    2C                   END
082900980128   X1C                   ELSE
083000980128     C                   MOVEL     *BLANKS       §LDDLP
083100980128    1C                   END
083200921026     C                   MOVEL     *BLANK        §LDCDP
083300960104     C                   MOVEL     VABNRS        §LDNRS
083400960104     C                   MOVEL     VABLNA        §LDLNA
083500960104     C     VABLNA        CHAIN     AZORG                              56
083600980128     C     *IN56         IFEQ      *OFF
083700980128    2C     ORGDE5        IFNE      *BLANKS
083800980128     C                   MOVEL     ORGDE5        §LDDLA
083900980128     C                   ELSE
084000980128     C                   MOVEL     ORGDES        §LDDLA
084100980128     C                   END
084200980128     C                   ELSE
084300980128     C                   MOVEL     *BLANKS       §LDDLA
084400980128     C                   END
084500971231     C                   CLEAR                   DSLV55
084600971231     C                   MOVEL     'A'           D55TPT
084700971231     C                   MOVE      VABLNA        D55LIN
084800971231     C                   MOVE      VABMGS        D55DRF
084900971231     C                   MOVEL     VABAAS        D55DRF
085000971231     C                   CALL      'FNLV55R'
085100971231     C                   PARM                    DSLV55
085200971231     C                   MOVEL     D55TFA        §LDTNA
085300960104     C                   MOVEL     VABTSP        §LDTSP
085400960104     C                   MOVEL     VABZNC        §LDZSC
085500960104     C                   MOVE      VABNCL        §LDNC2
085600980123     C* REPERISCO DATI ANAGRAIFCI CLIENTE MITTENTE
085700980123     C                   CLEAR                   DSBS69
085800980123     C                   Z-ADD     VABCCM        I69KAC
085900980123     C                   Z-ADD     VABCCM        I69KIN
086000980123     C                   MOVEL     KNSIF         I69SIF
086100980123     C                   CALL      'TIBS69R'
086200980123     C                   PARM                    DSBS69
086300980123     C                   PARM                    DSACO
086400980123     C                   PARM                    DSIND
086500980123     C                   PARM                    DSCLP
086600980123     C                   PARM                    DSCLS
086700980123     C     O69ERR        IFNE      '1'
086800980123     C     ACOFLG        IFEQ      '*'
086900980123     C                   MOVEL     *BLANKS       §LDRSM
087000980123     C                   ELSE
087100980123     C                   MOVEL     ACORAG        §LDRSM
087200980123     C                   END
087300980123     C                   MOVEL     INDPRV        §LDPRM
087400980123     C                   END
087500960104     C                   MOVEL     VABRSD        §LDRSD
087600960104     C                   MOVEL     VABCAD        §LDCAD
087700960104     C                   MOVEL     VABLOD        §LDLOD
087800960104     C                   MOVEL     VABIND        §LDIND
087900960104     C                   MOVEL     VABPRD        §LDPRD
088000921026     C                   MOVEL     999           §LDCPD
088100921026     C                   MOVEL     *ZEROS        §LDNCP
088200921026     C                   MOVEL     *ZEROS        §LDNCU
088300960105     C     VABVLB        IFGT      0
088400921026     C                   MOVEL     'R'           §LDFVR
088500960105     C                   ELSE
088600960105     C                   CLEAR                   §LDFVR
088700960105     C                   ENDIF
088800960104     C                   Z-ADD     VABVLB        §LDVOL
088900960104     C                   Z-ADD     VABPKB        §LDPKG
089000960104     C                   Z-ADD     VABAAS        §LDAAS
089100960104     C                   Z-ADD     VABMGS        §LDMGS
089200971223     C                   MOVEL     'ITA'         §DETST
089300960104     C     CO2SCD        DOWLE     VABNCA
089400921026     C                   ADD       1             §LDNC1
089500921026     C                   Z-ADD     CO2SCD        §LDNCD
089600921026     C*
089700030626     C                   CALL      d90pss
089800921026     C                   PARM                    DSBLD
089900971223     C                   PARM                    DSBLDE
090000921026     C*
090100921026     C                   ADD       1             CO2SCD
090200921026     C*
090300921026     C                   END
090400921026     C*
090500921026     C                   ENDSR
090600921026     C*---------------------------------------------------------------
090700921026     C*   GESLET  GESTIONE STAMPA LETTERA DI VETTURA
090800921026     C*---------------------------------------------------------------
090900921026     C     GESLET        BEGSR
091000030626     C                   CLEAR                   Fnlsb5ds
091100941228     C                   MOVEL     'B'           DB0GES
091200941228     C                   MOVEL     'P'           DB0TBO
091300921026     C*
091400960104     C                   Z-ADD     VABLNP        DB0LNP
091500960104     C                   Z-ADD     VABAAS        DB0AAS
091600960104     C                   Z-ADD     VABMGS        DB0MGS
091700960104     C                   Z-ADD     VABNRS        DB0NRS
091800960104     C                   Z-ADD     VABNSP        DB0NSP
091900960104     C                   MOVE      VABCBO        DB0CBO
092000960104     C                   MOVEL     §3ATB1        DB0TB1
092100960104     C                   MOVEL     §3ATB2        DB0TB2
092200960104     C                   MOVEL     §3ADES        DB0DCB
092300960104     C                   Z-ADD     VABLNA        DB0LNA
092400941228     C* PASSO INDIRIZZO COMPLETO DELLA LNA
092500960916     C*  CHE E' UGUALE AL DESTINATARIO
092600941228     C                   MOVEL     ORGFAG        DB0FAG
092700941228     C                   MOVEL     ORGDES        DB0DLA
092800941228     C                   MOVEL     ORGIND        DB0INL
092900941228     C                   MOVEL     ORGLOC        DB0LOL
093000941228     C                   MOVEL     ORGPRO        DB0PRL
093100941228     C                   MOVEL     ORGTEL        DB0TEL
093200160519     C                   MOVEL     ORGdd2        DB0FAX
093300030626     c                   Movel     OrgCpf        Db0Cal
093400960916     C*
093500960104     C                   Z-ADD     VABZNC        DB0ZNC
093600980109     C* VEDO SE STAMPARE IL P.O. DI BUCA CASELLARIO PARTENZA
093700980109     C                   MOVEL     ORGDE7        OG147
093800960104     C* MITTENTE
093900960104     C                   MOVEL     VABCCM        DB0CCM
094000941228     C                   MOVEL     ACORAG        DB0RSM
094100941228     C                   MOVEL     INDVIA        DB0INM
094200941228     C                   MOVEL     INDCAE        DB0CAM
094300941228     C                   MOVEL     INDCIT        DB0LOM
094400941228     C                   MOVEL     INDPRV        DB0PRM
094500941228     C                   MOVEL     INDSTA        DB0NZM
094600960104     C                   MOVEL     VABRSD        DB0RSD
094700960104     C                   MOVEL     VABIND        DB0IND
094800960104     C                   MOVEL     VABLOD        DB0LOD
094900960104     C                   MOVEL     VABPRD        DB0PRD
095000960104     C                   MOVEL     VABCAD        DB0CAD
095100110622     c*  ai fini della stampa i campi non servono per cui preferisco non pasarlo in vista
095200110622     c*  dei nuovi tipi inoltro per centro storico
095300110622     C* FLAG ANTEPORTO SEMPRE 'C' PERCHE' PADRONCINO DI CONSEGNA A MAGA
095400110622     C***                MOVEL     'C'           DB0FAP
095500960104     C* FLAG INOLTRO  --> SE FERMO DEPOSITO SEMPRE 'C'
095600110622     C***  VABFFD        IFEQ      'S'
095700110622     C***                MOVEL     'C'           DB0FIN
095800110622     C***                ELSE
095900110622     C***                MOVEL     WFIN          DB0FIN
096000110622     C***                ENDIF
096100960104     C                   MOVEL     VABGC1        DB0GC1
096200960104     C                   MOVEL     VABGC2        DB0GC2
096300960104     C                   MOVEL     VABTSP        DB0TSP
096400941228     C* DECODIFICA TIPO SERVIZIO
096500960104     C     VABTSP        IFNE      ' '
096600941228     C                   MOVEL     '5E'          COD
096700960104     C                   MOVEL(P)  VABTSP        KEY
096800941228     C     KTAB2         CHAIN     TABEL                              30
096900941228     C  N30              MOVEL     TBLUNI        DS5E
097000941228     C   30              CLEAR                   DS5E
097100941228     C                   MOVEL     §5EDES        DB0DTS
097200941228     C                   MOVEL     §5EFST        DB0FST
097300960104     C                   ENDIF
097400941228     C*
097500960104     C                   Z-ADD     VABIAS        DB0IAS
097600960104     C                   MOVEL     VABVAS        DB0VAS
097700960104     C                   MOVEL     VABNAS        DB0NAS
097800960104     C                   Z-ADD     VABNCL        DB0NCL
097900960104     C                   Z-ADD     VABPKB        DB0PKF
098000960104     C                   Z-ADD     VABVLB        DB0VLF
098100960105     C     DB0VLF        IFGT      0
098200941228     C                   MOVEL     'R'           DB0FVF
098300960105     C                   ENDIF
098400960104     C                   Z-ADD     VABRMN        DB0RMN
098500960104     C                   Z-ADD     VABLNP        DB0FLS
098600960104     C                   Z-ADD     VABNCD        DB0NCD
098700960104     C                   Z-ADD     VABNCA        DB0NCA
098800960105     C     DB0NCA        IFEQ      DB0NCD
098900960105     C                   CLEAR                   DB0NCA
099000960105     C                   ENDIF
099100960104     C                   MOVEL     VABXCO        DB0XCO
099200960104     C                   MOVEL     VABNOT        DB0NT1
099300960104     C                   MOVEL     VABNT2        DB0NT2
099400960104     C                   MOVEL     VABFFD        DB0FFD
099500921026     C*
099600030626     C                   CALL      d90psl
099700030626     C                   PARM                    Fnlsb5ds
099800921026     C*
099900921026     C                   ENDSR
100000960916     C*------------------------------------------------------------------------*
100100960916     C*  CHKCAP - CONTROLLO FORMALE DEL CAP
100200960916     C*------------------------------------------------------------------------*
100300960916     C     CHKCAP        BEGSR
100400101015     C                   SETOFF                                       90
100500960916     C*
100600960916     C* IMPOSTO CAMPI DS EST. E LANCIO PGM PER CONTROLLO
100700970721     C                   CLEAR                   DSSI95
100800970721     C                   CLEAR                   DSLV13
100900960916     C* CONTROLLO: 1-FORMALE  2-SU CAPPARIO 3-CONGRUENZA CON PROVINC
101000970721     C                   MOVEL     '3'           I95TCN                                     LLO FORM
101100970721     C                   MOVEL     VABCAD        I95CAP                         *CAP
101200970721     C                   MOVEL     VABPRD        I95PRV
101300970721     C                   MOVEL     VABLOD        I95LOC
101400970721     C                   Z-ADD     VPESO         I95LKG
101500970721     C                   Z-ADD     VVOLUM        I95LMC
101600970721     C                   MOVEL     VABFFD        I95FFD
101700970721     C                   Z-ADD     VABMGS        I95DAT
101800970721     C                   MOVEL     VABAAS        I95DAT
101900970721     C                   MOVEL     'S'           I13AF0
102000970721     C                   MOVEL     'S'           I13CNG
102100971201     C     §3AFCA        IFNE      0
102200971201     C                   MOVEL     'S'           I95FCA
102300971201     C                   ENDIF
102400971201     C                   MOVEL     §3ATB1        I95TPO
102500980602     C                   MOVEL     'S'           I95FRE
102600980602     C                   MOVEL     VABTSP        I95TSP
102700980602     C**
102800980602     C**  TERMINAL DI PARTENZA DELLA LNP
102900980602     C                   CLEAR                   DSLV55
103000980602     C                   MOVEL     'P'           D55TPT
103100980602     C                   MOVE      VABLNP        D55LIN
103200980602     C                   MOVE      VABMGS        D55DRF
103300980602     C                   MOVEL     VABAAS        D55DRF
103400980602     C                   CALL      'FNLV55R'
103500980602     C                   PARM                    DSLV55
103600980602     C**
103700980602     C                   MOVEL     D55TFP        I95TFP
103800960916     C*
103900970721     C                   CALL      'FNLV13R'
104000970721     C                   PARM                    KPJBA
104100970721     C                   PARM                    DSLV13
104200970721     C                   PARM                    DSSI95
104300960916     C**
104400970721     C* ERRORE
104500970721    1C     O13ERR        IFNE      ' '
104600960916     C* MEMORIZZO SOLTANTO 11 FILIALI SBAGLIATE
104700960916     C     X             IFLE      11
104800960916     C                   MOVEL     ORGFIL        FER(X)
104900960916     C                   ADD       1             X
105000960916     C                   ENDIF
105100960916     C**
105200101015     C                   SETON                                        90
105300960916    1C                   ENDIF
105400960916     C*
105500960916     C                   ENDSR
105600960916     C*------------------------------------------------------------------------*
105700960916     C*  EMIERR - EMISSIONE DELL'ERRORE SE NON TUTTE LE FIL ELABORATE
105800960916     C*------------------------------------------------------------------------*
105900960916     C     EMIERR        BEGSR
106000960916     C                   MOVEA     MSG(1)        K78
106100960916     C                   Z-ADD     1             X
106200960916     C                   Z-ADD     40            Y                 3 0
106300960916     C*
106400101015     C     X             DOWLE     10
106500960916     C     FER(X)        ANDGT     0
106600960916     C                   MOVEL     FER(X)        W003A             3
106700960916     C                   MOVEA     W003A         K78(Y)
106800960916     C                   ADD       4             Y
106900960916     C                   ADD       1             X
107000960916     C                   ENDDO
107100960916     C*
107200960916     C* SE UNDICESIMA POSIZIONE PIENA METTO I PUNTINI PER FAR CAPIRE
107300960916     C*  CHE CI SONO ALTRE FILIALI
107400960916     C     FER(11)       IFGT      0
107500960916     C                   MOVEA     '...'         K78(76)
107600960916     C                   ENDIF
107700960916     C*
107800960916     C                   MOVEA     K78           VIDMSG
107900960916     C                   SETON                                        28
108000960916     C                   ENDSR
108100960917     C*------------------------------------------------------------------------*
108200960917     C*  PULIZ  - PULIZIA CAMPI FORMATO
108300960917     C*------------------------------------------------------------------------*
108400960917     C     PULIZ         BEGSR
108500131105     c                   setoff                                       0102
108600131104     C                   MOVEL     dateugg       VDSP
108700960917     C                   CLEAR                   VVOLUM
108800960917     C                   CLEAR                   VNOTE
108900960917     C                   CLEAR                   VNOTE2
109000101015     C***                MOVEL     '   '         VSDIBA
109100101015     C                   MOVEL     '   '         Vntw
109200101015     C                   MOVEL     *blanks       Vdntw
109300960917     C                   MOVEL     'A'           VTIBOL
109400960917     C                   MOVEL     'S'           VSINOV
109500131106     C                   MOVEL     'N'           Vcong
109600960917     C                   MOVEL     '1'           VPUNTI
109700101018     c                   clear                   vcrea
109800101018     c                   clear                   vvisl
109900960917     C                   MOVEL     'S'           VCONF
110000960917     C                   MOVEL     'SI'          VFERMO
110100960917     C                   MOVEL     'INV.POST'    VNATUR
110200960917     C                   MOVE      'A SEDE '     VNATUR
110300960917     C                   Z-ADD     1             VNUCOL
110400960917     C                   Z-ADD     1             VPESO
110500101015     c                   clear                   vlista
110600101015     C                   SETON                                        90
110700960917     C                   ENDSR
110800941228     OFNLV22P   T    LRNLR
110900960917**
111000960917Filiali non elaborate x errori sul CAP:
111100131025**
111200131025A C Q U I S T I
111300131025S   E   D   E
111400131025A  L  T  R  O
111500131104**
111600131105OVRPRTF   FILE(FNLSB6P)  hold(*YES)
