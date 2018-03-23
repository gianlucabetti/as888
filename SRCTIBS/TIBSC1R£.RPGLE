000100071108    ?H* TIBSC1R *-----------------------------------------------------*
000200130715?    H*            CREAZIONE unica dati per Filiale - FILTRO -        *
000300011206?    H*---------------------------------------------------------------*
000400171124     H DECEDIT('0,') DATEDIT(*DMY.)  option(*nodebugio)
000500071108     FTIBSc1D   CF   E             WORKSTN
000600071108     FAZORG01L  IF a E           K DISK
000700071109     FAZCAE01L  IF a E           K DISK
000800071109     Ffiapd01l  IF a E           K DISK
000900170613     fAZCMM01L  if a e           k disk
001000170616     fCNACO00F  if a e           k disk
001100170616     fCNIND00F  if a e           k disk
001200170616     fCNCLP00F  if a e           k disk
001300170616     fFNCLS01L  if a e           k disk
001400170616     fTABEL00F  o    e             disk    usropn extfile(wFLib)
001500170711     fTNTBE00F  o    e             disk    usropn extfile(wFLib)
001600011206?    D*---------------------------------------------------------------*
001700011206     D L1              S              3  0 DIM(30)                              P.O.GESTITE
001800170705     D msg             S             78    DIM(25) CTDATA PERRCD(1)
001900170710     d des             s             78    dim(02) ctdata perrcd(1)
002000011206?    D*
002100061201     D* PASSAGGIO DATI PER CREAZIONE numeratori di gruppo  - AZNUM00F -
002200061201     d*                                       e di filiale - FLNUF00F -
002300011206     D PARAM           DS
002400011206     D* Codice P.O.
002500011206     D  PARFIL                 3      5  0
002600061201     D* terminal partenza di  PARFIL per numeratori partenza
002700061201     D  partfp                 6      8  0
002800011206     D* Codice Numeratore
002900061201     D  parCNU                 9     11
003000011206     D* Tipo P.O.
003100061201     D  parFTF                12     12
003200011206     D* Anno
003300061201     D  parANN                13     16  0
003400020206     D* Società
003500061201     D  parSOC                17     19
003600061201     D*
003700011206     D WLBDAT          DS                  INZ
003800011206     D  G02DAT                 1      8  0
003900011206     D  G02INV                 9     16  0
004000011206     D  G02ERR                17     17
004100011206     D  G02TGI                18     22  0
004200011206?    D*
004300011206     D* DS PER TRUL06R - CARICAMENTO £X
004400011206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004500011206     D  LIN                    1     90  0
004600011206     D                                     DIM(30)                              P.O. COMODO
004700011206     D*
004800011206     D TIBS02DS      E DS                  INZ
004900011206     D FNLV55DS      E DS                  INZ
005000071109     D og150         E DS                  INZ
005100071109     D og149         E DS                  INZ
005200071109     D og148         E DS                  INZ
005300071108     D og147         E DS                  INZ
005400071108     D og146         E DS                  INZ
005500071108     D og144         E DS                  INZ
005600071108     D og141         E DS                  INZ
005700011206?    D*
005800011206     D KPJBA         E DS
005900011206     D CNCR80        E DS
006000011206     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006100071108     d SAvpod          s                   like(v1cpod)
006200071109     d Ktip            s                   like(apdtip) inz('A')
006300071109     d Kpdr            s                   like(apdpdr)
006400071108     d vidaan          s              2  0
006500071109     d WRSC            s             35
006600071109     d Werrcrea        s              1
006700011206     D*
006800170511     D fiapdds       E DS                  extname(fiapd00f)
006900170511     d fnlv36ds      e ds                  inz
007000170511     d fiapda        e ds                  extname(fiapd00f)
007100170511     d                                     prefix(a)
007200170511     d fiapdb        e ds                  extname(fiapd00f)
007300170511     d                                     prefix(b)
007400170511     d fiapd4A       e ds                  extname(fiapd40f)
007500170511     d                                     prefix(a)
007600170511     d fiapd4b       e ds                  extname(fiapd40f)
007700170511     d                                     prefix(b)
007701171116
007702171116     dYCRT2080I      e ds                  QUALIFIED INZ(*EXTDFT)
007705171116     dYCRT2080O      e ds                  QUALIFIED INZ(*EXTDFT)
007800170711
007900170711     d dMTC          e ds
008000170616
008100170616     d wAnno2          s              2s 0 inz
008200170616     d wMMGG           s              4s 0 inz
008300170616     d wFlib           s             21a   inz
008400170616     d wlibFiliale     s             10a   inz
008500170616     d wlibSede        s             10a   inz
008501171116
008503171116      //---------------------------------------------------------------
008504171116      //?Definizione procedure esterne.
008505171116      //---------------------------------------------------------------
008506171116      // - Scrittura conti incassi da attribuire
008507171116      /COPY GAITRASRC/SRCCONST,YCRT2080R
008508171116     d YCRT2080R       pr                  extpgm('YCRT2080R')
008510171116     d  kpjba                              likeds(kpjba)
008511171116
008600170616
008700011206     C*****************************************************************
008800011206     C* RIEPILOGO INDICATORI
008900011206     C*****************************************************************
009000011206     C* 30    - DI COMODO
009100011206     C* 35    - USATO PER TESTN
009200011206     C* 40/43 - INDICATORI PER MESSAGGI DI ERRORE
009300011206     C* 90    - SEGNALA LA PRESENZA DI ERRORI
009400011206     C*****************************************************************
009500011206     C     *ENTRY        PLIST
009600011206     C                   PARM                    KPJBA
009700011206     C                   Z-ADD     1             CODUT
009800011206     C                   CALL      'X§PARUT'
009900011206     C                   PARM                    UT§DSE
010000011206     C                   MOVEL     RAGUT         RSUT             20
010100011206     C                   MOVEL     REC80         CNCR80
010200170616
010300170616       //?Determino la libreria
010400170616         IF  %subst(knsif : 7 : 1) = 'P';
010500170616           wLibSede = 'GAITRAGRPS';
010600170616           wLibFiliale = 'FILTRAGRPF';
010700170616         ELSE;
010800170616           wLibSede = 'GAITRAGRU';
010900170616           wLibFiliale = 'FILTRAGRU';
011000170616         ENDIF;
011100170616
011200170511      * variazioni
011300170511     c                   clear                   fnlv36ds
011400170511     c                   clear                   fiapdB
011500170511     c                   clear                   fiapdA
011600170511     c                   clear                   fiapd4b
011700170511     c                   clear                   fiapd4A
011800170511     c                   eval      ILV36PRU  = knmus
011900170511     c                   eval      ILV36NOJ  = knmeb
012000170511     c                   eval      ILV36PGM  = 'TIBSC1R'
012100020206     C*
012200011206     C* DATA DEL GIORNO
012300011206     C                   TIME                    W0140            14 0
012400011206     C* UDATE IN GGMMAAAA
012500011206     C                   MOVE      W0140         WDTGIO            8 0
012600011206     C* UDATE IN AAAAMMGG
012700011206     C                   Z-ADD     WDTGIO        G02DAT
012800011206     C                   MOVEL     *BLANK        G02ERR
012900011206     C                   CALL      'XSRDA8'
013000011206     C                   PARM                    WLBDAT
013100011206     C                   MOVEL     G02INV        DATEU             8 0
013200061201     c                   movel     dateu         annocor           4 0
013300170616     c                   move      annocor       wAnno2
013400170616     c                   move      dateu         wMMGG
013500071108     C                   MOVEL     G02dat        viddat            8 0
013600170616
013700071109     c*
013800071109     c     kapd          klist
013900071109     c                   kfld                    ktip
014000071109     c                   kfld                    kpdr
014100011206     C*
014200011206     C* PULIZIA CAMPI FORMATO
014300071109     C                   CLEAR                   v1cpOD
014400071109     C                   CLEAR                   v1dpod
014500071109     c                   clear                   v1cape
014600011206     C*
014700011206?    C     INIZIO        TAG
014800071108     c                   clear                   v1ctfp
014900071108     c                   clear                   v1dtfp
015000071108     c                   clear                   v1ctfa
015100071108     c                   clear                   v1dtfa
015200071108     c                   clear                   v1corg
015300071109     c                   clear                   v1cpad
015400071108     c                   clear                   v1cpoc
015500071108     c                   clear                   v1dpoc
015600071108     c                   clear                   v1cter
015700071108     c                   clear                   wforzapod         1
015800170615       clear V1Cind;
015900170615       clear V1Ccap;
016000170615       clear V1Cloc;
016100170615       clear V1Cprv;
016200170613       clear V1Ccmm;
016300170613       clear V1Cksc;
016600170710       descri = des(01);
016700011206     C*
016800011206     C     FOR01         TAG
016900071108     C                   EXFMT     BSc1D01
017000071108     c                   setoff                                       289040
017100071108     c                   setoff                                       414243
017200071109     c                   setoff                                       444546
017300170613       *in47 = *off;
017400170613       *in48 = *off;
017500170613       *in49 = *off;
017600170705       *in50 = *off;
017700011206     C*
017800061201?    C* F3 - FINE LAVORO
017900011206     C   KC              GOTO      FINE
018000011206     C*
018100011206?    C* CONTROLLI FORMATO
018200011206     C                   EXSR      CONTR
018300170710     C   90              GOTO      FOR01
018400170710
018500170710?      // ENTER va in seconda videata
018600170711       clear V1Ctab03;
018700170711       clear V1Ctab4G;
018800170711       clear V1Ctab3G;
018900170711       clear V1Ctab4D;
019000170711       clear V1CtabMTC;
019100170710     C     FOR02         TAG
019200170711     C                   EXFMT     BSC1D02
019300170711       *in28 = *off;
019400170711       *in90 = *off;
019500170710     C*
019600170710?    C* F3 - FINE LAVORO
019700170710     C   KC              GOTO      FINE
019800170710?    C* F12 - Ritorno
019900170710     C   KL              GOTO      for01
020000170710?    C* CONTROLLI VIDEO
020100170710     C                   EXSR      CONTR2
020200170710     C  NKF
020300170710     COR 90              GOTO      FOR02
020400170710
020500011206     C*
020600061201?    C* F6 - ELABORAZIONE
020700071108     c*
020800071108     c                   if        v1corg='SI'
020900071108     c                   EXSR      CREAORG
021000071108     c                   endif
021100071108     c                   if        v1cTER='SI'
021200071108     c                   EXSR      CREATER
021300071108     c                   endif
021400071109     c                   if        v1cpad='SI'
021500071109     c                   EXSR      CREApadr
021600071109     c                   endif
021700170613       //?Creo anagrafica commerciali
021800170613       IF  V1Ccmm = 'SI';
021900170613         exsr CreaCmm;
022000170613       ENDIF;
022100170613       //?Creo anagrafica clienti
022200170613       IF  V1Cksc = 'SI';
022300170613         exsr CreaKsc;
022400170613       ENDIF;
022500170710       //?creo tabella 03
022600170710       IF  V1Ctab03 = 'SI';
022700170710         exsr CreaTab03;
022800170616       ENDIF;
022900170710       //?creo tabella 4G
023000170710       IF  V1Ctab4G = 'SI';
023100170710         exsr CreaTab4G;
023200170710       ENDIF;
023300170710       //?creo tabella 3G
023400170710       IF  V1Ctab3G = 'SI';
023500170710         exsr CreaTab3G;
023600170710       ENDIF;
023700170710       //?creo tabella 4D
023800170710       IF  V1Ctab4D = 'SI';
023900170710         exsr CreaTab4D;
024000170710       ENDIF;
024100170710       //?creo tabella MTC
024200170710       IF  V1CtabMTC = 'SI';
024300170710         exsr CreaTabMTC;
024400170710       ENDIF;
024401171124       //?Creo anagrafica conto incassi da attribuire
024402171124       IF  V1Cksc = 'SI';
024403171124         exsr CreaIncassi;
024404171124       ENDIF;
024500071109     c
024600071109     C*  Se ci sono stati errori in creazione segnalo
024700071109     c                   if        werrcrea='S'
024800071109     c                   movel     msg(14)       vidmsg
024900071109     c                   else
025000071109     c                   movel     msg(15)       vidmsg
025100071109     c                   endif
025200071109     c                   seton                                        9028
025300061201     C*
025400011206     C                   GOTO      INIZIO
025500011206     C     FINE          TAG
025600011206     C                   SETON                                        LR
025700011206     C*
025800011206?    C*---------------------------------------------------------------*
025900011206?    C* CONTR * CONTROLLI FORMATO                                     *
026000011206?    C*---------------------------------------------------------------*
026100011206     C     CONTR         BEGSR
026200011206     C                   SETOFF                                       90
026300020206     C*
026400011206     C*
026500020206?    C****  P.O.  ****
026600071108IF  1C     V1cPOD        IFEQ      0
026700071108     c                   movel     msg(2)        vidmsg
026800071108     C                   SETON                                        289040
026900011206     C                   GOTO      ENDCTR
027000071108X  x1C                   ELSE
027100011206     C*
027200071108     C     V1cPOD        CHAIN     AZORG                              30
027300071108    2c                   if        not *in30 and (orgfag='V' or
027400071108     c                             orgfva<>' ')
027500071108     c                   movel     msg(2)        vidmsg
027600071108     C                   SETON                                        289040
027700071108     C                   GOTO      ENDCTR
027800071108    2c                   endif
027900071108     c
028000071108     c*Se cambia p.o. pulisco forzatura
028100071108    2c                   if        savpod<>v1cpod
028200071108     c                   clear                   wforzapod
028300071108     c                   eval      savpod=v1cpod
028400071108    2c                   endif
028500071108     c
028600071108     C* Se non esiste potrebbe essere creazione
028700071108    2c                   if        *in30  and wforzapod=' '
028800071108     c                   movel     msg(1)        vidmsg
028900071108     C                   SETON                                        289040
029000071108     c                   eval      wforzapod='S'
029100071108     C                   GOTO      ENDCTR
029200071108    2c                   endif
029300071108     c
029400071108     c* Se p.o. inesistente forzato, descrizione obbligatoria
029500071108    2c                   if        wforzapod='S'
029600071108    3c                   if        v1dpod=*blanks
029700071108     c                   movel     msg(3)        vidmsg
029800071108     C                   SETON                                        289045
029900071108     C                   GOTO      ENDCTR
030000071108    3c                   endif
030100071108     c
030200071108   x2c                   else
030300071108     c                   movel     orgdes        v1dpod
030400071108    2c                   endif
030500071108     c
030600011206E   1C                   ENDIF
030700071108     c*************************+
030800071108     c* Data apertura
030900071108     c*************************+
031000071108     c* Obbligatoria per nuovo p.o o per creazione terminal
031100071108    1c                   if        wforzapod='S' or v1cter='SI'
031200071108    2c                   if        v1cape=0
031300071108     c                   movel     msg(4)        vidmsg
031400071108     C                   SETON                                        289041
031500071108     C                   GOTO      ENDCTR
031600071108    2c                   endif
031700071108    1c                   endif
031800071108     c
031900071108    1c                   if        v1cape>0
032000071108     C                   Z-ADD     v1cape        G02DAT
032100071108     C                   MOVEL     *BLANK        G02ERR
032200071108     C                   CALL      'XSRDA8'
032300071108     C                   PARM                    WLBDAT
032400071108     c
032500071108    2c                   if        g02err='1'
032600071108     c                   movel     msg(6)        vidmsg
032700071108     C                   SETON                                        289041
032800071108     C                   GOTO      ENDCTR
032900071108E   2C                   ENDIF
033000071108     C                   MOVEL     G02INV        invape            8 0
033100071108     c                   movel     g02dat        v1cape
033200071108    1c                   endif
033300170710
033400170710       //?obbligo indirizzo
033500170710         IF  (V1Cind = *blanks or V1Cloc = *blanks or V1Cloc = *zeros or
033600170710             V1Cprv = *blanks);
033700170710           VIDmsg = msg(20);
033800170710           *in28 = *on;
033900170710           *in50 = *on;
034000170710           *in90 = *on;
034100170710           leavesr;
034200170710         ENDIF;
034300071108     C*
034400071108     c*************************+
034500071108     c* Crezione organigramma
034600071108     c*************************+
034700071108     c*  Obbligatoria se p.o. nuovo
034800071108    1c                   if        wforzapod='S' and v1corg='  '
034900071108     c                   movel     msg(5)        vidmsg
035000071108     C                   SETON                                        289046
035100071108     C                   GOTO      ENDCTR
035200071108    1c                   endif
035300071109     c
035400071108    1c                   if        v1corg='SI'
035500071109     c* Se esiste già non posso selezionarlo
035600071109     c     v1cpod        chain     azorg01l
035700071109     c                   if        %found(azorg01l)
035800071109     c                   movel     msg(13)       vidmsg
035900071109     C                   SETON                                        289046
036000071109     C                   GOTO      ENDCTR
036100071109    1c                   endif
036200071108     c
036300071108     c* Controllo p.o. di copia
036400071108     c                   clear                   v1dpoc
036500071108    2c                   if        v1cpoc=0
036600071108     c                   movel     msg(7)        vidmsg
036700071108     C                   SETON                                        289042
036800071108     C                   GOTO      ENDCTR
036900071108   x2c                   else
037000071108     C     V1cPOc        CHAIN     AZORG                              30
037100071108    3c                   if        *in30 or  orgfag='V' or
037200071108     c                             orgfva<>' '
037300071108     c                   movel     msg(7)        vidmsg
037400071108     C                   SETON                                        289042
037500071108     C                   GOTO      ENDCTR
037600071108    3c                   endif
037700071108     c                   movel     orgdes        v1dpoc
037800071108    2c                   endif
037900071108    1c                   endif
038000071108     c*************************+
038100071108     c* Crezione Terminal     non devono esistere
038200071108     c*************************+
038300071108    1c                   if        v1cter='SI'
038400071108    2c                   if        v1ctfp=0 and v1ctfa=0
038500071108     c                   movel     msg(8)        vidmsg
038600071108     C                   SETON                                        289043
038700071108     C                   GOTO      ENDCTR
038800071108    2c                   endif
038900071108     c
039000071108    2c                   if        v1ctfp>0
039100071108     C                   CLEAR                   FNLV55DS
039200071108     C                   MOVEL     'P'           D55TPT
039300071108     C                   MOVEL     V1cpod        D55LIN
039400071108     C                   MOVEL     invape        d55DRF
039500071108     C                   CALL      'FNLV55R'
039600071108     C                   PARM                    FNLV55DS
039700071108     c
039800071108     c* Se già presente errore
039900071108    3c                   if        d55err=' '
040000071108     c                   movel     msg(9)        vidmsg
040100130715     c                   eval      %subst(vidmsg:51:3)=%editc(d55tfp: 'X')
040200071108     C                   SETON                                        289043
040300071108     C                   GOTO      ENDCTR
040400071108    3c                   endif
040500071108     c
040600071108     c* il p.o. deve essere esistente ed un terminal di partenza
040700071108     c*  se <> dal p.o. stesso
040800071108    3c                   if        v1ctfp=v1cpod
040900071108     c                   movel     v1dpod        v1dtfp
041000071108     c                   else
041100071108     c     v1ctfp        chain     azorg01l
041200071108    4c                   if        *in30 or  orgfag='V' or
041300071108     c                             orgfva<>' '
041400071108     c                   movel     msg(10)       vidmsg
041500071108     C                   SETON                                        289043
041600071108     C                   GOTO      ENDCTR
041700071108    4c                   endif
041800071108     c
041900071108     c                   movel     orgdes        v1dtfp
042000071108     c
042100071108     c* Verifico se terminal di partenza
042200071108     C                   CLEAR                   FNLV55DS
042300071108     C                   MOVEL     'P'           D55TPT
042400071108     C                   MOVEL     V1ctfp        D55LIN
042500071108     C                   MOVEL     invape        d55DRF
042600071108     C                   CALL      'FNLV55R'
042700071108     C                   PARM                    FNLV55DS
042800071108     c
042900071108    4c                   if        d55err<>' ' or v1ctfp<>d55tfp
043000071108     c                   movel     msg(10)       vidmsg
043100071108     C                   SETON                                        289043
043200071108     C                   GOTO      ENDCTR
043300071108    4c                   endif
043400071108     c
043500071108    3c                   endif
043600071108    2c                   endif
043700071108     c
043800071108    2c                   if        v1ctfa>0
043900071108     C                   CLEAR                   FNLV55DS
044000071108     C                   MOVEL     'A'           D55TPT
044100071108     C                   MOVEL     V1cpod        D55LIN
044200071108     C                   MOVEL     invape        d55DRF
044300071108     C                   CALL      'FNLV55R'
044400071108     C                   PARM                    FNLV55DS
044500071108     c
044600071108    3c                   if        d55err=' '
044700071108     c                   movel     msg(11)       vidmsg
044800130715     c                   eval      %subst(vidmsg:51:3)=%editc(d55tfa: 'X')
044900071108     C                   SETON                                        289044
045000071108     C                   GOTO      ENDCTR
045100071108    3c                   endif
045200071108     c
045300071108     c* il p.o. deve essere esistente ed un terminal di arrivo
045400071108     c*  se <> dal p.o. stesso
045500071108    3c                   if        v1ctfa=v1cpod
045600071108     c                   movel     v1dpod        v1dtfa
045700071108     c                   else
045800071108     c     v1ctfa        chain     azorg01l
045900071108    4c                   if        *in30 or  orgfag='V' or
046000071108     c                             orgfva<>' '
046100071108     c                   movel     msg(12)       vidmsg
046200071108     C                   SETON                                        289044
046300071108     C                   GOTO      ENDCTR
046400071108    4c                   endif
046500071108     c
046600071109     c                   movel     orgdes        v1dtfa
046700071108     c
046800071108     c* Verifico se terminal di arrivo
046900071108     C                   CLEAR                   FNLV55DS
047000071108     C                   MOVEL     'A'           D55TPT
047100071108     C                   MOVEL     V1ctfa        D55LIN
047200071108     C                   MOVEL     invape        d55DRF
047300071108     C                   CALL      'FNLV55R'
047400071108     C                   PARM                    FNLV55DS
047500071108     c
047600071108    4c                   if        d55err<>' ' or v1ctfa<>d55tfa
047700071108     c                   movel     msg(12)       vidmsg
047800071108     C                   SETON                                        289044
047900071108     C                   GOTO      ENDCTR
048000071108    4c                   endif
048100071108     c
048200071108    3c                   endif
048300071108    2c                   endif
048400071108    1c                   endif
048500170613
048600170613       //?Controllo per creazione codici commerciali
048700170613       //?se esistono già i commerciali 0000 o 0999 errore
048800170613         IF  V1Ccmm = 'SI';
048900170613         //?Commerciale VARI
049000170613           CMMcod = V1Cpod * 10000;
049100170613           chain (CMMcod) AZCMM01L;
049200170613           IF  %found(AZCMM01L);
049300170613             VIDmsg = msg(16);
049400170613             VIDmsg = %trim(VIDmsg) + ' VARI';
049500170613             *in28 = *on;
049600170613             *in47 = *on;
049700170613             *in90 = *on;
049800170613             leavesr;
049900170613           ENDIF;
050000170613         //?Commerciale CLIENTI INATTIVI
050100170613           CMMcod += 999;
050200170613           chain (CMMcod) AZCMM01L;
050300170613           IF  %found(AZCMM01L);
050400170613             VIDmsg = msg(16);
050500170613             VIDmsg = %trim(VIDmsg) + ' CLIENTI INATTIVI';
050600170613             *in28 = *on;
050700170613             *in47 = *on;
050800170613             *in90 = *on;
050900170613             leavesr;
051000170613           ENDIF;
051100170613         ENDIF;
051200170613
051300170613       //?Controllo per creazione codici clienti
051400170613       //?Se richiesto devo impostare anche se filiale di primo o secondo livello
051500170710       //?e deve esserci la creazione dei commerciali
051600170613         IF  V1Cksc = 'SI';
053100170613       //?se esistono già i clienti 8888 e 9999 errore
053200170613           ACOkut = 1;
053300170613           ACOkcc = 151;
053400170613           ACOksc = (V1Cpod * 10000) + 8888;
053500170613           chain (ACOkut:ACOkcc:ACOksc) CNACO00F;
053600170613           IF  %found(CNACO00F);
053700170613             VIDmsg = msg(17);
053800170613             VIDmsg = %trim(VIDmsg) + ' FRANCHI VARI';
053900170613             *in28 = *on;
054000170613             *in48 = *on;
054100170613             *in90 = *on;
054200170613             leavesr;
054300170613           ENDIF;
054400170613           ACOksc = (V1Cpod * 10000) + 9999;
054500170613           chain (ACOkut:ACOkcc:ACOksc) CNACO00F;
054600170613           IF  %found(CNACO00F);
054700170613             VIDmsg = msg(17);
054800170613             VIDmsg = %trim(VIDmsg) + ' ASSEGNATI VARI';
054900170613             *in28 = *on;
055000170613             *in48 = *on;
055100170613             *in90 = *on;
055200170613             leavesr;
055300170613           ENDIF;
057900170705       //?se creazione anagrafica obbligo creazione commerciali
058000170705           IF  V1Ccmm <> 'SI';
058100170705             VIDmsg = msg(21);
058200170705             *in28 = *on;
058300170705             *in47 = *on;
058400170705             *in90 = *on;
058500170705             leavesr;
058600170705           ENDIF;
058700170613         ENDIF;
058800170613
058900011206     C*
059000011206     C     ENDCTR        ENDSR
059100170710
059200170710       //--------------------------------------------------------------
059300170710       //?Controlli seconda videata.
059400170710       //--------------------------------------------------------------
059500170710       BEGSR Contr2;
059600170710
059700170710       ENDSR;
059800170710
059900071108?    C*---------------------------------------------------------------*
060000071108?    C* CREAORG * creazione organigramma
060100071108?    C*---------------------------------------------------------------*
060200071108     C     CREAorg       BEGSR
060300071108     c     v1cpoc        chain     azorg01l
060400071108     c* campi da modificare
060500071108     c                   movel     v1cpod        orgfil
060600071108     c                   movel     v1dpod        orgdes
060700071108     c                   movel(P)  'V.'          orgind
060800170615       IF  V1Cind <> *blanks;
060900170615         ORGind = V1Cind;
061000170615       ENDIF;
061100170615       IF  V1Ccap <> *zeros;
061200170615         ORGcpf = V1Ccap;
061300170615       ENDIF;
061400170615       IF  V1Cloc <> *blanks;
061500170615         ORGloc = V1Cloc;
061600170615       ENDIF;
061700170615       IF  V1Cprv <> *blanks;
061800170615         ORGpro = V1Cprv;
061900170615       ENDIF;
062000071109     c                   movel(P)  '0'           orgtel
062100071109     c                   movel(P)  '0'           orgfax
062200071108     c                   clear                   orgtlx
062300071108     c                   if        orgfag='A'
062400071108     c                   movel     v1dpod        orgdea
062500071108     c                   movel(P)  'V.'          orgina
062600170705       IF  V1Cind <> *blanks;
062700170705         ORGina = V1Cind;
062800170705       ENDIF;
062900170705       IF  V1Ccap <> *zeros;
063000170705         ORGcpa = V1Ccap;
063100170705       ENDIF;
063200170705       IF  V1Cloc <> *blanks;
063300170705         ORGloa = V1Cloc;
063400170705       ENDIF;
063500170705       IF  V1Cprv <> *blanks;
063600170705         ORGpra = V1Cprv;
063700170705       ENDIF;
063800071109     c                   movel(P)  '0'           orgtea
063900071109     c                   movel(P)  '0'           orgfaa
064000071108     c                   clear                   orgtla
064100071108     c                   endif
064200071109     c* Descrizione 150
064300071109     c                   movel     orgdf0        og150
064400071109     c                   clear                   §ogpck
064500071109     c                   clear                   §ogpkm
064600160509     c                   clear                   §ogturno
064700080418     c                   clear                   §ogtis
064800071109     c                   movel     og150         orgdf0
064900071108     c* Descrizione 148
065000071108     c                   movel     orgde8        og148
065100071108     c                   clear                   §oglpo
065200071108     c                   clear                   §ogtlls
065300080418     c                   clear                   §oggeot
065400080418     c                   clear                   §ogbd1
065500080418     c                   clear                   §ogbd2
065600071108     c                   movel     'N'           §oglbl
065700071108     c                   movel     'N'           §oglfv
065800071108     c                   movel     'N'           §ognid
065900071109     c                   movel     'A'           §ogorm
066000080418     c                   movel     ' '           §ogpdaorm
066100080418     c                   movel     ' '           §ogpdacon
066200071108     c                   movel     og148         orgde8
066300071109     c* Descrizione 149
066400071109     c                   movel     orgde9        og149
066500071109     c                   movel(P)  '0'           §ogtld
066600071109     c                   movel     og149         orgde9
066700071108     c* Descrizione 146
066800071108     c                   movel     orgde6        og146
066900071108     c                   movel     invape        §ogdsd
067000071108     c                   movel     invape        §ogdse
067100071108     c                   clear                   §ogdda
067200071108     c                   movel     og146         orgde6
067300071108     c
067400071108     c                   movel     orgde1        og141
067500071109     c                   eval      §ogssc='RxxxSEDE   '
067600071108     c                   eval      %subst(§ogssc:2:3)=%editc(v1cpod:'X')
067700071108     c                   movel     og141         orgde1
067800071108     c
067900071108     c                   movel     orgde4        og144
068000071109     c                   eval      §ogssp='RxxxSEDE   '
068100071108     c                   eval      %subst(§ogssp:2:3)=%editc(v1cpod:'X')
068200071108     c                   movel     og144         orgde4
068300071108     c
068400080418     c* Descrizione 147
068500071108     c                   movel     orgde7        og147
068600071108     c                   clear                   §ogcgio
068700071108     c                   clear                   §ogddao
068800071108     c                   clear                   §ogfrtuo
068900071108     c                   clear                   §ogdoada
069000071108     c                   clear                   §ogfppda
069100071108     c                   movel     'N'           §ognoc
069200071108     c                   movel     og147         orgde7
069300071108     c* Pulisco le descrizione degli utenti
069400071108     c                   clear                   orgd10
069500071108     c                   clear                   orgf10
069600071108     c                   clear                   orgd13
069700071108     c                   clear                   orgf13
069800071108     c                   clear                   orgd15
069900071108     c                   clear                   orgf15
070000071108     c                   clear                   orgd18
070100071108     c                   clear                   orgf18
070200071108     c                   clear                   orgf25
070300071108     c                   clear                   orgd25
070400071108     c                   clear                   orgf28
070500071108     c                   clear                   orgd28
070600071108     c                   clear                   orgf31
070700071108     c                   clear                   orgd31
070800071108     c                   clear                   orgf33
070900071108     c                   clear                   orgd33
071000071108     c                   clear                   orgf36
071100071108     c                   clear                   orgd36
071200071108     c                   clear                   orgf41
071300071108     c                   clear                   orgd41
071400071108     c                   clear                   orgf46
071500071108     c                   clear                   orgd46
071600071108     c                   clear                   orgf47
071700071108     c                   clear                   orgd47
071800071108     c                   clear                   orgf49
071900071108     c                   clear                   orgd49
072000071108     c                   clear                   orgf56
072100071108     c                   clear                   orgd56
072200071108     c                   clear                   orgf58
072300071108     c                   clear                   orgd58
072400071108     c                   clear                   orgf61
072500071108     c                   clear                   orgd61
072600080418     c                   movel(P)  '.'           orgd71
072700080418     c                   movel(P)  '.'           orgd72
072800080418     c* Se primo livello se stesso, altrimenti il terminal di partenza
072900071108    1c                   if        v1ctfp>0
073000071108     c                   movel     v1dtfp        orgdc0
073100071109     c                   movel     v1ctfp        orgfc0
073200071108   x1c                   else
073300071108     c* Cerco il terminal di partenza
073400071108     C                   CLEAR                   FNLV55DS
073500071108     C                   MOVEL     'P'           D55TPT
073600071108     C                   MOVEL     V1cpod        D55LIN
073700071108     C                   MOVEL     invape        d55DRF
073800071108     C                   CALL      'FNLV55R'
073900071108     C                   PARM                    FNLV55DS
074000071108     c                   movel     d55tfp        orgfc0
074100071108     c     d55tfp        chain     azorg01l
074200071108    2c                   if        %found(azorg01l)
074300071108     c                   movel     orgdes        orgdc0
074400071108     c                   else
074500071108     c                   clear                   orgdc0
074600071108    2c                   endif
074700071108    1c                   endif
074800071108     c
074900071109     c                   movel     v1dpod        orgfc5
075000071109     c                   movel     v1cpod        orgfc5
075100071109     c                   movel     v1cpod        orgfe0
075200071109     c                   movel     v1cpod        orgfe1
075300071109     c                   movel     v1cpod        orgfe2
075400071109     c                   movel     v1cpod        orgfe3
075500071109     c                   movel     v1cpod        orgfe4
075600071109     c                   movel     v1cpod        orgfe5
075700071109     c                   movel     v1cpod        orgfe6
075800071109     c                   movel     v1cpod        orgfe7
075900071109     c                   movel     v1cpod        orgfe8
076000071109     c                   movel     v1cpod        orgfe9
076100071109     c                   movel     v1cpod        orgff0
076200080418     c                   movel     v1cpod        orgf71
076300080418     c                   movel     v1cpod        orgf72
076400071109     c                   eval      orgftr='N'
076500071108     c
076600071108     c                   write     azorg
076700071108     c* Richiamo la manutenzione organigramma
076800071108     c                   call      'TNSD15R'
076900071108     c                   parm                    kpjba
077000071108     c
077100071108     c                   ENDSR
077200071108?    C*---------------------------------------------------------------*
077300071108?    C* CREATER * creazione terminal
077400071108?    C*---------------------------------------------------------------*
077500071108     C     CREAter       BEGSR
077600071109     c* terminal di partenza
077700071109     c                   if        v1ctfp>0
077800071109     c                   clear                   azcae000
077900071109     c                   eval      caeepa='P'
078000071109     c                   eval      caetfp=v1cpod
078100071109     c                   eval      caetfe=v1ctfp
078200071109     c                   eval      caedde=dateu
078300071109     c                   eval      caedsc=20391231
078400071109     c                   write     azcae000
078500071109     c                   endif
078600071109     c
078700071109     c* terminal di Arrivo
078800071109     c                   if        v1ctfa>0
078900071109     c                   clear                   azcae000
079000071109     c                   eval      caeepa='A'
079100071109     c                   eval      caetfa=v1cpod
079200071109     c                   eval      caetfe=v1ctfa
079300071109     c                   eval      caedde=invape
079400071109     c                   eval      caedsc=20391231
079500071109     c                   write     azcae000
079600071109     c                   endif
079700071108     c                   ENDSR
079800071109?    C*---------------------------------------------------------------*
079900071109?    C* CREApadr * creazione cpdici padroncini standard
080000071109?    C*---------------------------------------------------------------*
080100071109     C     CREApadr      BEGSR
080200071109     c* Se non esistono già
080300071109     c                   clear                   kpdr
080400071109     c                   movel     v1cpod        kpdr
080500071109     c                   move      993           kpdr
080600071109     c                   eval      wrsC='C.A. CHIUSE            '
080700071109     C                   EXSR      SCRIVIAPD
080800071109     C
080900071109     c                   clear                   kpdr
081000071109     c                   movel     v1cpod        kpdr
081100071109     c                   move      996           kpdr
081200071109     c                   eval      wrsC='RESI                   '
081300071109     C                   EXSR      SCRIVIAPD
081400071109     C
081500071109     c                   clear                   kpdr
081600071109     c                   movel     v1cpod        kpdr
081700071109     c                   move      997           kpdr
081800071109     c                   eval      wrsC='C/SERVIZI              '
081900071109     C                   EXSR      SCRIVIAPD
082000071109     C
082100071109     c                   clear                   kpdr
082200071109     c                   movel     v1cpod        kpdr
082300071109     c                   move      998           kpdr
082400071109     c                   eval      wrsC='GIAC./DIROTT.          '
082500071109     C                   EXSR      SCRIVIAPD
082600071109     C
082700071109     c                   clear                   kpdr
082800071109     c                   movel     v1cpod        kpdr
082900071109     c                   move      999           kpdr
083000071109     c                   eval      wrsC='CONSEGNE A MAGAZZINO   '
083100071109     C                   EXSR      SCRIVIAPD
083200071109     C
083300071109     c                   clear                   kpdr
083400071109     c                   movel     v1cpod        kpdr
083500071109     c                   move      9876          kpdr
083600071109     c                   eval      wrsC='CHIUSURA PERIODICA     '
083700071109     C                   EXSR      SCRIVIAPD
083800160331     c
083900160331     c                   clear                   kpdr
084000160331     c                   movel     v1cpod        kpdr
084100160331     c                   move      9510          kpdr
084200160331     c                   eval      wrsC='VEDI PACCO             '
084300160331     C                   EXSR      SCRIVIAPD
084400071109     C
084500071109     C                   ENDSR
084600071109?    C*---------------------------------------------------------------*
084700071109?    C* SCRIVIAPD* scrittura file fiapd
084800071109?    C*---------------------------------------------------------------*
084900071109     C     SCRIVIAPD     BEGSR
085000071109     c     kapd          chain     fiapd01l
085100071109     c                   if        not %found(fiapd01l)
085200071109     c                   movel     070           kpdr
085300071109     c     kapd          chain     fiapd01l
085400071109     c                   if        not %found(fiapd01l)
085500071109     c                   eval      werrcrea='S'
085600071109     c                   else
085700071109     c                   movel     v1cpod        apdpdr
085800071109     c                   eval      apdrsc=wrsc
085900071109     c                   eval      apdrsf=wrsc
086000071109     c                   z-add     dateu         apddtr
086100071109     c                   clear                   apdftr
086200071109     c                   clear                   apdatb
086300071109     c                   write     fiapd000
086400170511     c                   eval      fiapdA = fiapdds
086500170511     c                   exsr      variazione
086600071109     c                   endif
086700071109     c                   endif
086800071109     c
086900071109     C                   ENDSR
087000170613
087100170613       //--------------------------------------------------------------
087200170613       //?Crea Anagrafica commerciali.
087300170613       //--------------------------------------------------------------
087400170613       BEGSR CreaCmm;
087500170613
087600170613       //?Scrivo commerciale VARI
087700170613         CMMcod = V1Cpod * 10000;
087800170613         chain (CMMcod) AZCMM01L;
087900170613         IF  not %found(AZCMM01L);
088000170613           CMMcod = V1Cpod * 10000;
088100170613           CMMdes = 'VARI ';
088200170613           CMMDes = %trim(CMMdes) + ' ' + %trim(V1Dpod);
088300170613           CMMpar = '1';
088400170616           CMMdtini = %dec(%date());
088500170616           CMMdtfin = 20391231;
088600170616           CMMuni = CMMcod;
088700170613           write AZCMM000;
088800170613         ENDIF;
088900170613
089000170613       //?Scrivo commerciale CLIENTI INATTIVI
089100170616         CMMcod = (V1Cpod * 10000) + 0999;
089200170613         chain (CMMcod) AZCMM01L;
089300170613         IF  not %found(AZCMM01L);
089400170613           CMMcod = (V1Cpod * 10000) + 0999;
089500170613           CMMdes = 'CLIENTI INATTIVI';
089600170613           CMMpar = '2';
089700170616           CMMdtini = %dec(%date());
089800170616           CMMdtfin = 20391231;
089900170616           CMMuni = CMMcod;
090000170613           write AZCMM000;
090100170613         ENDIF;
090200170613
090300170613       ENDSR;
090400170613
090500170613       //--------------------------------------------------------------
090600170613       //?Crea Anagrafica Clienti.
090700170613       //--------------------------------------------------------------
090800170613       BEGSR CreaKsc;
090900170613
091000170613       //?Scrivo Franchi Vari
091100170616         clear CNACO000;
091200170616         ATB02 = 'N';
091300170613         ACOkut = 1;
091400170613         ACOkcc = 151;
091500170613         ACOksc = (V1Cpod * 10000) + 8888;
091600170613         ACOrag = 'FRANCHI VARI';
091700170613         ACOrag = %trim(ACOrag) + ' ' + %trim(V1Dpod);
091800170616         ACOopz = '1100100001';
091900170616         ACOrx1 = 1;
092000170616         ACOry1 = 1;
092100170616         ACOtic = 'CG';
092200170616         ACOduv = (wAnno2 * 10000) + wMMGG;
092300170616         ACOitc = 21;
092400170616         ACOflt = %dec(%subst(%editc(ACOksc:'X'):1:3):3:0);
092500170613         write CNACO000;
092600170613
092700170616         clear CNIND000;
092800170613         INDkut = 1;
092900170613         INDkcc = 151;
093000170613         INDksc = (V1Cpod * 10000) + 8888;
093100170615         INDvia = V1Cind;
093200170615         INDcap = V1Ccap;
093300170616         INDcit = V1Cloc;
093400170615         INDprv = V1Cprv;
093500170616         INDcdf = '00506810019';
093600170616         INDiva = '00506810019';
093700170616         INDcdp = *all'0';
093800170616         INDlin = '1';
093900170616         INDopz = '0      000';
094000170615         INDcae = %editc(INDcap:'X');
094100170613         write CNIND000;
094200170613
094300170616         clear CNCLP000;
094400170613         CLPkut = 1;
094500170613         CLPkcc = 151;
094600170613         CLPksc = (V1Cpod * 10000) + 8888;
094700170616         CLPscf = CLPksc;
094800170616         CLPfft = 4;
094900170616         CLPsol = 'N';
095000170616         CLPfun = 'I';
095100170613         CLPage = V1Cpod * 10000;
095200170616         CLPclv = 'C';
095300170616         CLPfpc = '1';
095400170613         write CNCLP000;
095500170613
095600170616         clear FNCLS000;
095700170613         CLSksc = (V1Cpod * 10000) + 8888;
095800170616         CLStic = '11';
095900170616         CLSflo = 'N       AP';
096000170613         write FNCLS000;
096100170613
096200170616       //?Scrivo Assegnati
096300170616         clear CNACO000;
096400170616         ATB02 = 'N';
096500170616         ACOkut = 1;
096600170616         ACOkcc = 151;
096700170616         ACOksc = (V1Cpod * 10000) + 9999;
096800170616         ACOrag = 'ASSEGNATI';
096900170616         ACOrag = %trim(ACOrag) + ' ' + %trim(V1Dpod);
097000170616         ACOopz = '1100100001';
097100170616         ACOrx1 = 1;
097200170616         ACOry1 = 1;
097300170616         ACOtic = 'CG';
097400170616         ACOduv = (wAnno2 * 10000) + wMMGG;
097500170616         ACOitc = 21;
097600170616         ACOflt = %dec(%subst(%editc(ACOksc:'X'):1:3):3:0);
097700170616         write CNACO000;
097800170616
097900170616         clear CNIND000;
098000170616         INDkut = 1;
098100170616         INDkcc = 151;
098200170616         INDksc = (V1Cpod * 10000) + 9999;
098300170616         INDvia = V1Cind;
098400170616         INDcap = V1Ccap;
098500170616         INDcit = V1Cloc;
098600170616         INDprv = V1Cprv;
098700170616         INDcdf = '00506810019';
098800170616         INDiva = '00506810019';
098900170616         INDcdp = *all'0';
099000170616         INDlin = '1';
099100170616         INDopz = '0      000';
099200170616         INDcae = %editc(INDcap:'X');
099300170616         write CNIND000;
099400170616
099500170616         clear CNCLP000;
099600170616         CLPkut = 1;
099700170616         CLPkcc = 151;
099800170616         CLPksc = (V1Cpod * 10000) + 9999;
099900170616         CLPscf = CLPksc;
100000170616         CLPfft = 4;
100100170616         CLPsol = 'N';
100200170616         CLPfun = 'I';
100300170616         CLPage = V1Cpod * 10000;
100400170616         CLPclv = 'C';
100500170616         CLPfpc = '1';
100600170616         write CNCLP000;
100700170616
100800170616         clear FNCLS000;
100900170616         CLSksc = (V1Cpod * 10000) + 9999;
101000170616         CLStic = '11';
101100170616         CLSflo = 'N       AP';
101200170616         write FNCLS000;
101201171116
101300170613
112100170613       ENDSR;
112200170616
112300170616       //--------------------------------------------------------------
112400170710       //?Crea Tabella 03.
112500170616       //--------------------------------------------------------------
112600170710       BEGSR CreaTab03;
112700170710
112800170710       //?Se mai fatto prima apro file tabelle di SEDE
112900170710         IF  not %open(TABEL00F);
113000170710           wFLib = %trim(wLibSede) + '/TABEL00F';
113100170711           open TABEL00F;
113200170710         ELSE;
113300170710       //?Se aperto chiudo e apro file tabelle di SEDE
113400170710       //?potrei aver aperto quello di filiale
113500170710           close TABEL00F;
113600170710           wFLib = %trim(wLibSede) + '/TABEL00F';
113700170711           open TABEL00F;
113800170710         ENDIF;
113900170710
114000170710       //?Tabella Zone
114100170710         clear TABEL;
114200170710         TBLkut = 1;
114300170710         TBLcod = '03';
114400170710         TBLkey = %editc(V1Cpod:'X') + '00';
114500170710         TBLftt = '1';
114600170710         write TABEL;
114700170710
114800170710       ENDSR;
114900170710
115000170710       //--------------------------------------------------------------
115100170710       //?Crea Tabella 4G.
115200170710       //--------------------------------------------------------------
115300170710       BEGSR CreaTab4G;
115400170710
115500170711       //?Se mai fatto prima apro file tabelle di SEDE
115600170711         IF  not %open(TABEL00F);
115700170711           wFLib = %trim(wLibSede) + '/TABEL00F';
115800170711           open TABEL00F;
115900170711         ELSE;
116000170711       //?Se aperto chiudo e apro file tabelle di SEDE
116100170711       //?potrei aver aperto quello di filiale
116200170711           close TABEL00F;
116300170711           wFLib = %trim(wLibSede) + '/TABEL00F';
116400170711           open TABEL00F;
116500170711         ENDIF;
116600170710
116700170710       //?Tabella Chiusura automatica distinte
116800170710         clear TABEL;
116900170710         TBLkut = 1;
117000170710         TBLcod = '4G';
117100170710         TBLkey = %editc(V1Cpod:'X') + 'CDA';
117200170710         TBLuni = %editc(V1Cpod:'X') + '98760059MN';
117300170710         TBLftt = '1';
117400170710         write TABEL;
117500170710
117600170710       ENDSR;
117700170711
117800170711       //--------------------------------------------------------------
117900170711       //?Crea Tabella 3G.
118000170711       //--------------------------------------------------------------
118100170711       BEGSR CreaTab3G;
118200170711
118300170711       //?Se mai fatto prima apro file tabelle di Filiale
118400170711         IF  not %open(TABEL00F);
118500170711           wFLib = %trim(wLibFiliale) + '/TABEL00F';
118600170711           open TABEL00F;
118700170711         ELSE;
118800170711       //?Se aperto chiudo e apro file tabelle di Filiale
118900170711       //?potrei aver aperto quello di sede
119000170711           close TABEL00F;
119100170711           wFLib = %trim(wLibFiliale) + '/TABEL00F';
119200170711           open TABEL00F;
119300170711         ENDIF;
119400170711
119500170711       //?Tabella Codice trattamento merce
119600170711         clear TABEL;
119700170711         TBLkut = 1;
119800170711         TBLcod = '3G';
119900170711         TBLkey = %editc(V1Cpod:'X');
120000170711         TBLuni = '7K';
120100170711         TBLftt = '2';
120200170711         TBLflt = V1Cpod;
120300170711         write TABEL;
120400170711
120500170711       ENDSR;
120600170711
120700170711       //--------------------------------------------------------------
120800170711       //?Crea Tabella 4D.
120900170711       //--------------------------------------------------------------
121000170711       BEGSR CreaTab4D;
121100170616
121200170711       //?Se mai fatto prima apro file tabelle di Filiale
121300170711         IF  not %open(TABEL00F);
121400170711           wFLib = %trim(wLibFiliale) + '/TABEL00F';
121500170711           open TABEL00F;
121600170711         ELSE;
121700170711       //?Se aperto chiudo e apro file tabelle di Filiale
121800170711       //?potrei aver aperto quello di sede
121900170711           close TABEL00F;
122000170711           wFLib = %trim(wLibFiliale) + '/TABEL00F';
122100170711           open TABEL00F;
122200170711         ENDIF;
122300170711
122400170711       //?Tabella Numerazione distinte di versamento
122500170711         clear TABEL;
122600170711         TBLkut = 1;
122700170711         TBLcod = '4D';
122800170711         TBLkey = %editc(V1Cpod:'X');
122900170711         TBLuni = '000000';
123000170711         TBLftt = '2';
123100170711         TBLflt = V1Cpod;
123200170711         write TABEL;
123300170616
123400170616       ENDSR;
123500170711
123600170711       //--------------------------------------------------------------
123700170711       //?Crea Tabella MTC.
123800170711       //--------------------------------------------------------------
123900170711       BEGSR CreaTabMTC;
124000170711
124100170711       //?Se mai fatto prima apro file tabelle di Filiale
124200170711         IF  not %open(TNTBE00F);
124300170711           wFLib = %trim(wLibFiliale) + '/TNTBE00F';
124400170711           open TNTBE00F;
124500170711         ELSE;
124600170711       //?Se aperto chiudo e apro file tabelle di Filiale
124700170711       //?potrei aver aperto quello di sede
124800170711           close TNTBE00F;
124900170711           wFLib = %trim(wLibFiliale) + '/TNTBE00F';
125000170711           open TNTBE00F;
125100170711         ENDIF;
125200170711
125300170711       //?Tabella Password Tariffe/Piano dei Conti
125400170711         clear TNTBE000;
125500170711         clear dMTC;
125600170711         TBEcod = 'MTC';
125700170711         TBEke1 = %editc(V1Cpod:'X');
125800170711         §MTCpwd = %subst(V1Dpod:1:10);
125900170711         §MTCdta = dateu;
126000170711         TBEuni = dMTC;
126100170711         TBEftt = 'S';
126200170711         TBEflt = V1Cpod;
126300170711         write TNTBE000;
126400170711
126500170711       ENDSR;
126501171124
126502171124       //--------------------------------------------------------------
126503171124       //?Crea Conto Incassi.
126504171124       //--------------------------------------------------------------
126505171124       BEGSR CreaIncassi;
126506171124         //creo anche i conti incassi da attribuire
126507171124         reset YCRT2080I;
126508171124         ycrt2080i.IdFiliale = V1Cpod;
126509171124         ycrt2080i.DzFiliale = V1Dpod;
126510171124         kpjbu = YCRT2080I;
126511171124         YCRT2080R (kpjba);
126512171124         YCRT2080O = kpjbu;
126513171124
126514171124       ENDSR;
126600171124
126700170511     C*------------------------------------------------------------------------*
126800170511     C* richiama pgm variazioni
126900170511     C*------------------------------------------------------------------------*
127000170511     C     variazione    BEGSR
127100170511      * preparazione dati richiamo PGM variazioni per Inserimento
127200170511      *richiama pgm registrazione variazioni
127300170511     c                   eval      ILV36CTA  = 'I'
127400170511     c                   call      'FNLV36R'
127500170511     c                   parm                    fnlv36ds
127600170511     c                   parm                    fiapdb
127700170511     c                   parm                    fiapda
127800170511     c                   parm                    fiapd4b
127900170511     c                   parm                    fiapd4A
128000170511     c                   ENDsr
128100071108**
128200090420Filiale inesistente: enter per forzare e creare la nuova filiale
128300130715Filiale inesistente, annullata o tipo "V"
128400130715Descrizione obbligatoria per nuova Filiale
128500130715Data apertura obbligatoria per Filiale nuova o creazione terminal
128600130715Creazione organigramma obbligatoria per nuova Filiale
128700071108Data errata
128800130715Filiale da cui copiare inesistente
128900071108Per creare i terminal immettere T.Partenza e/o T.Arrivo
129000130715Per la Filiale esiste già il terminal di partenza xxx alla data di apertura
129100071108Terminal di Partenza errato o non è un terminal di partenza                    10
129200130715Per la Filiale esiste già il terminal di Arrivo   xxx alla data di apertura
129300071109Terminal di Arrivo errato o non è un terminal di Arrivo                        12
129400130715Esiste già l'organigramma per la Filiale indicata
129500071109Errori in creazione dati: verificare!!!!
129600071109Dati creati ...
129700170613Per la Filiale esiste già il Commerciale                                       16
129800170613Per la Filiale esiste già il Cliente                                           17
129900170922Libero                                                                         18
130000170922Libero                                                                         19
130100170705Immettere l'indirizzo completo                                                 20
130200170705Se creazione anagrafica clienti obbligo creazione commerciali                  21
130300170710**    DES Lung. 78
130400170710L'indirizzo verrà memorizzato in organigramma e nelle anagrafiche clienti      01
