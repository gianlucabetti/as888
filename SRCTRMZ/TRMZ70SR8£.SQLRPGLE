000100120727     H OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR('PRNPGM') ACTGRP('QILE')
000200970214     H DECEDIT('0,') DATEDIT(*DMY.)
000300940223      *
000400940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000500940620      *  03           PROTECT TUTTI I CAMPI
000600090608      *  19           se Contratto RISTAMPATO
000700940223      *  21           GENERICO OPERAZIONI I/O
000800940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000900940223      *  30           SFLDSP
001000940223      * N31           SFLCLR
001100940128      *  31           SFLDSPCTL
001200940128      *  32           SFLNXTCHG
001300940128      *  33           SFLEND
001400940128      *  39           OF PRTF
001500940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001600940317    > *  Specificare l'uso dei singoli indicatori
001700940315      *  50 <---> 98  ERRORI SU VIDEO
001800940317    > *  Specificare l'uso dei singoli indicatori
001900940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002000940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002100940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002200940128      *  99           INDIC. GENERALE DI ERRORE
002300940128     F*----------------------------------------------------*
002400150616     FAITRc01l  UF A E           K DISK
002500150710     FAITRs01l  iF   E           K DISK
002600090317      *
002700150615$001 FTRMZ70SD8 CF   E             WORKSTN
002800940201     F                                     INFDS(DSFMT)
002900940128     D*----------------------------------------------------*
003000940127     D* Passaggio Parametri
003100940127     D KPJBA         E DS
003200090320     D KPJBus          S                   like(KPJBU)
003300090408     D trmz70s1ds    E DS                  prefix(S1_)
003400150710     D dtrsflr1      E DS
003500090320     D*-------------
003600030204     D  S_TASTO        S                   like($Tasto)
003700940201     D DSFMT           DS
003800940506     D  $TASTO               369    369
003900940201     D  NRG                  370    370
004000940201     D  NCL                  371    371
004100940207     D*-------------
004200940207     D* Reperimento nome PGM
004300940207     D STATUS         SDS           333
004400940207     D  DSPGM            *PROC
004500940225     D*-------------
004600940225     D* COSTANTI
004700940225     D*-------------
004800940506     D* Tasti di funzione
004900940506     D F01             C                   CONST(X'31')
005000940506     D F02             C                   CONST(X'32')
005100940506     D F03             C                   CONST(X'33')
005200940506     D F04             C                   CONST(X'34')
005300940506     D F05             C                   CONST(X'35')
005400940506     D F06             C                   CONST(X'36')
005500940506     D F07             C                   CONST(X'37')
005600940506     D F08             C                   CONST(X'38')
005700940506     D F09             C                   CONST(X'39')
005800940506     D F10             C                   CONST(X'3A')
005900940506     D F11             C                   CONST(X'3B')
006000940506     D F12             C                   CONST(X'3C')
006100940506     D F13             C                   CONST(X'B1')
006200940506     D F14             C                   CONST(X'B2')
006300940506     D F15             C                   CONST(X'B3')
006400940506     D F16             C                   CONST(X'B4')
006500940506     D F17             C                   CONST(X'B5')
006600940506     D F18             C                   CONST(X'B6')
006700940506     D F19             C                   CONST(X'B7')
006800940506     D F20             C                   CONST(X'B8')
006900940506     D F21             C                   CONST(X'B9')
007000940506     D F22             C                   CONST(X'BA')
007100940506     D F23             C                   CONST(X'BB')
007200940506     D F24             C                   CONST(X'BC')
007300940506     D ENTER           C                   CONST(X'F1')
007400940506     D ROLDWN          C                   CONST(X'F4')
007500940506     D ROLLUP          C                   CONST(X'F5')
007600940506     D*-------------
007700941108     D DATA            C                   CONST('0001-01-01')
007800030114     D Annull          C                   CONST('  ANNULLATO  ')
007900090318     D OPz_Imm         C                   CONST('     IMMISSIONE     ')
008000090318     D OPz_Var         C                   CONST('     VARIAZIONE     ')
008100090318     D OPz_Del         C                   CONST('       ANNULLA      ')
008200090318     D OPz_Vis         C                   CONST('     VISUALIZZA     ')
008300090319      *
008400090319      * Opzioni --> Azioni da fare :
008500090319     D Immissione      C                   CONST('1')
008600090319     D Modifica        C                   CONST('2')
008700090319     D Copia           C                   CONST('3')
008800090319     D Cancella        C                   CONST('4')
008900090319     D Visualizza      C                   CONST('5')
009000090319     D Acceso          C                   '1'
009100090319     D Spento          C                   '0'
009200090323     D*-------------
009300150617     D WLBDA8          DS
009400150617     D  G02DAT                 1      8  0
009500150617     D  G02INV                 9     16  0
009600150617     D  G02ERR                17     17
009700150617     D  G02TGI                18     22  0
009800150625     D*-------------
009900090323      * Controllo IVA
010000151117     d xcfiva1ds     e ds
010100090323     d  ivaeu                  3      4
010200090323     d  ivait                  5     18
010300150625     D*-------------
010400150625     d VALUE_IND       s              5i 0
010500150625     D*-------------
010600150615     D* Parametri in ricezione
010700150622     D TRMZ70S8DS    E DS
010800150625     D*-------------
010900090317     d dataiso         s               d   datfmt(*iso)
011000090317     d dataeur         s               d   datfmt(*eur)
011100090323     d Data_oggi       s              8  0
011200090318     d Errore          s              1
011300090317
011400090409     d digits          c                   '0123456789'
011500940127     C*----------------------------------------------------*
011600940127     C*                MAIN LINE PROGRAM
011700940127     C*----------------------------------------------------*
011800940223     C*
011900090319      * ? * Entra nella sequenza delle 3 videate + aggiornamento D.Base *
012000940223     C     $FINE         DOWEQ     *OFF
012100150615     C                   exsr      GESD1
012200940117     C                   END
012300090317     C*
012400030113     C                   Seton                                        LR
012500940131     C************************************************************
012600090319      *?  GESTIONE VIDEO RECORD D1                               ?
012700940131     C************************************************************
012800940127     C     GESD1         BEGSR
012900030113      *
013000940225     C* inizializzazione videata
013100090319     C* settaggi 1°video campi attivi o campi protetti
013200090319     C     $INZD1        IFEQ      *On
013300940127     C                   EXSR      INZD1
013400940223     C                   MOVE      *OFF          $INZD1
013500940117     C                   END
013600090311     C*
013700940620     C                   EXFMT     D1
013800030114     C*
0139000301141    C                   SELECT
014000030114     C* F12=Ritorno
014100030114     C     $TASTO        WHENEQ    F12
014200090319     C                   EXSR      F12Ritorna
014300090319     c                   leaveSR
014400030114     C*
0145000903202-   C                   ENDSL
014600030114     C*
014700030114     C                   EXSR      CTRD1
014800090319     C* Video 2 o Video 3
014900940506     C     *IN99         IFEQ      *OFF
015000090319      *
015100150703     C     $TASTO        ifeq      F06
015200150703     C     $TASTO        oreq      F16
015300150615     c                   exsr      aggimmis
015400150616     c  n04              exsr      f03fine
015500150615     c                   endif
015600090319      *
0157000301142-   C                   ENDIF
015800030114     C*
015900940117     C                   ENDSR
016000940117      ************************************************************
016100090319      *?  INIZIALIZZAZIONE VIDEATA 1                       ?
016200940117      ************************************************************
016300940127     C     INZD1         BEGSR
016400090319     C*
0165001506221   >C     DS8opz        IFEQ      Immissione
0166001506221   >C     DS8opz        oreq      Copia
016700150616     c                   setoff                                       02
016800090319      * ? * Reperisce il Numeratore x Nuova Società *
016900150615     c                   clear                   d1iva
017000150615     c                   clear                   d1proj
017100150615     c                   clear                   d1ivaa
017200150616     c                   clear                   d1ivad
017300150615     c                   clear                   d1durc
017400150615     c                   clear                   d1cip
017500150615     c                   clear                   d1dfc
017600150616     c                   clear                   h1nrc
017700150623     c*******            if        DS8opz = copia
017800150625     c***                eval      d1socg = DS8socg
017900150622     c                   eval      d1iva  = DS8iva
018000150623     c                   eval      d1ivaa = DS8ivaa
018100150623     c*******            endif
018200090323     C*
0183000903191   >C                   ELSE
018400090319     C*
018500090319      * ? * Reperisce informazioni della società solo se non in IMMISSIONE *
018600150622     c                   eval      wtrciva  = DS8iva
018700150622     c                   eval      wtrcpicf = DS8ivaa
018800150615     C     ktrc          chain(N)  AITRc01l
018900150615     C                   if        %Found(AITRc01l)
019000150615     c                   seton                                        02
019100090320     C*
019200150615     c                   eval      d1IVA    = trcIVA
019300150615     c                   eval      d1ivaa   = trcpicf
019400150616     c                   eval      d1ivad  = trcragsoc
019500150615      *
019600150615     c                   if        trcddurc > 0
019700150615     C                   move      trcddurc      dataiso
019800150615     C                   move      dataiso       dataeur
019900150615     C                   move      dataeur       d1durc
020000150615     c                   Endif
020100150615     c                   if        trcdcip  > 0
020200150615     C                   move      trcdcip       dataiso
020300150615     C                   move      dataiso       dataeur
020400150615     C                   move      dataeur       d1cip
020500150615     c                   Endif
020600150615     c                   if        trcdfc   > 0
020700150615     C                   move      trcdfc        dataiso
020800150615     C                   move      dataiso       dataeur
020900150615     C                   move      dataeur       d1dfc
021000150615     c                   Endif
021100090319     C*
021200090319      * ? * richiamo routine dei ctrl per decodifiche e controlli
021300940614    >C                   EXSR      CTRD1
021400030113      *
0215001506152-   C                   endif
0216000903172-   C                   ENDIF
021700940224     C*
021800940117     C                   ENDSR
021900940207     C/EJECT
022000090320     C************************************************************
022100090320      *?  CONTROLLO VIDEATA 1                              ?
022200090320     C************************************************************
022300090320     C     CTRD1         BEGSR
022400090320     C*
022500940127     C                   SETOFF                                       99
022600090317     C*
022700090317     C* controllo codice vuoto P.IVA
022800150626     C                   SETOff                                       61
022900090317$017 C     D1IVA         IFEQ      *ALL' '
023000150615     C                   SETON                                        6199
023100090319     c                   leavesr
023200090317     C                   ELSE
023300090323      **
023400090323      ** Controllo Formale P.IVA
023500090323      *  richiama il nuovo driver fatto per controllare il codice
023600090323      * fiscale o la partita iva
023700151117     c                   clear                   xcfiva1ds
023800090323     c                   eval      xcfivapar = D1iva
023900151117     c                   call      'XCFIVAR1'
024000151117     c                   parm                    xcfiva1ds
024100090323      * --> errore
024200090323     c                   if        XCFIVAFLG < *zeros
024300150615     C                   SETON                                        6199
024400090323     c                   leavesr
024500090323     c                   Else
024600090323      **
024700150616     c                   eval      trcIVA  = d1IVA
024800090317     c                   exsr      Decod_RAG_IVA
024900090319      *
025000090408     c                   if        S1_Trovato = *Off or S1_Errore  <> '0'
025100090410     c                   if        S1_Errore <>'2'
025200150615     C                   SETON                                        6199
025300090406     c                   leavesr
025400090410     C                   Elseif    S1_Errore = '2'
025500150615     C                   SETON                                        6199
025600090410     c                   leavesr
025700090410     C                   End
025800090317     C                   Else
025900090408     C                   eval      d1ProJ = s1_RagSocKSC
026000090319      *
026100090320     C                   End
026200090323     C                   End
026300090317      *
0264009402241-   C                   END
026500090323     C*
026600150625     C*  Deve essere presente su AITRS e deve avere Natura Giuridica "11"
026700150625      *
026800150625     c                   clear                   trovato_TRS       1
026900150710     c                   clear                   contratto         7 0
027000150625     c                   eval       SQLCOD = 0
027100150625     c                   eval       VALUE_IND = 0
027200150625      *
027300150625      * Controlla la PIVA presente sul file delle Società
027400150625     C/EXEC SQL
027500150710     C+ SELECT  'S', trsnrc
027600150710     C+ into :trovato_TRS, :contratto   INDICATOR :VALUE_IND
027700150625     C+   FROM AITRS00F WHERE (TRSDFC =0 or TRSDFC >:Data_oggi)
027800150625     C+    and substr(TRSIVA, 1, 11) = :D1IVA
027900150625     C+    fetch first row only
028000150625     C/END-EXEC
028100150710     C                   SETOff                                       6769
028200150625     c                   if           SQLCOD = 0 and VALUE_IND = 0
028300150625     c                                and trovato_TRS <> 'S'
028400150625     C                   SETON                                        6799
028500150625     c                   leavesr
028600150625     c                   end
028700150625     C*
028800150625     C*
028900150615     C* controllo codice vuoto P.IVA
029000150626     c                   setoff                                       62
029100150615$017 C     D1IVAa        IFEQ      *ALL' '
029200150615     C                   SETON                                        6299
029300150615     c                   leavesr
029400150615     C                   ELSE
029500150615      **
029600150615      ** Controllo Formale P.IVA
029700150615      *  richiama il nuovo driver fatto per controllare il codice
029800150615      * fiscale o la partita iva
029900151117     c                   clear                   xcfiva1ds
030000150615     c                   eval      xcfivapar = D1ivaa
030100151117     c                   call      'XCFIVAR1'
030200151117     c                   parm                    xcfiva1ds
030300150615      * --> errore
030400150615     c                   if        XCFIVAFLG < *zeros
030500150615     C                   SETON                                        6299
030600150615     c                   leavesr
030700150615     c                   Endif
030800150615     c                   Endif
030900150626      *
031000150710      * se partita iva consorziata = partita iva consorzio proteggo e forzo date cip durc
031100150710$017 C     D1IVAa        IFEQ      D1IVA
031200150710     c     contratto     chain     aitrs01l
031300150710     c                   if        %found(aitrs01l)
031400150710     c                   movel     trsflr1       dtrsflr1
031500150730     c                   if        §trsddurc <> *blank  and §trsddurc <> *zero
031600150710     c                   move      §trsddurc     d1durc
031700150710     c                   move      d1durc        dataiso
031800150710     c                   move      dataiso       dataeur
031900150710     c                   move      dataeur       d1durc
032000150710     c                   endif
032100150730     c                   if        §trsdcip  <> *blank and §trsdcip  <> *zero
032200150710     c                   move      §trsdcip      d1cip
032300150710     c                   move      d1cip         dataiso
032400150710     c                   move      dataiso       dataeur
032500150710     c                   move      dataeur       d1cip
032600150710     c                   endif
032700150731     c                   else
032800150710     C                   SETON                                        69
032900150710     c                   leavesr
033000150710     C                   Endif
033100150710     C                   Endif
033200150703      *
0333001507031   >C     DS8opz        IFEQ      Immissione
0334001507031   >C     DS8opz        oreq      Copia
033500150703      **  Deve controllare se esiste l'anagrafica inserita AITRC
033600150703     C                   eval        wTRCIVA = d1iva
033700150703     C                   eval        wTRCPICF= d1ivaa
033800150706     c     Ktrc          chain     aitrc01l
033900150706     c                   if        %found(aitrc01l)
034000150703     C                   SETON                                        7099
034100150703     c                   leavesr
034200150703     c                   end
034300150703     c                   end
034400150703     C*
034500150617      * controllo date
034600150617      * DURC
034700150617     c                   setoff                                       63
034800150703     C                   move      *zero         ddurc             8 0
034900150615     c                   if        d1durc > 0
035000150617     C                   MOVE      d1durc        G02DAT
035100150617     C                   MOVEL     *BLANK        G02ERR
035200150617     C                   CALL      'XSRDA8'
035300150617     C                   PARM                    WLBDA8
035400150625     C                   IF        G02ERR = '1' or
035500150625     C                             g02inv > Data_Oggi
035600150617     C                   SETON                                        6399
035700150617     c                   leavesr
035800150617     C                   END
035900150617     c                   move      g02dat        d1durc
036000150703     C                   move      g02inv        ddurc
036100150615     c                   Endif
036200150617      * CIP
036300150626     c                   setoff                                       6466
036400150703     C                   move      *zero         dcip              8 0
036500150617     c                   if        d1cip  > 0
036600150617     C                   MOVE      d1cip         G02DAT
036700150617     C                   MOVEL     *BLANK        G02ERR
036800150617     C                   CALL      'XSRDA8'
036900150617     C                   PARM                    WLBDA8
037000150625     C                   IF        G02ERR = '1' or
037100150625     C                             g02inv > Data_Oggi
037200150617     C                   SETON                                        6499
037300150617     c                   leavesr
037400150617     C                   END
037500150617     c                   move      g02dat        d1cip
037600150703     C                   move      g02inv        dcip
037700150617     c                   Endif
037800150706     c                   if        ddurc > 0 and dcip > 0 and
037900150706     c                             ddurc < dcip
038000150617     c                   seton                                        6699
038100150617     c                   Endif
038200150617      * DFC
038300150617     c                   setoff                                       65
038400150703     C                   move      *zero         ddfc              8 0
038500150623     c                   if        d1dfc  = 0
038600150623     c                   z-add     31122039      d1dfc
038700150623     c                   Endif
038800150617     c                   if        d1dfc  > 0
038900150617     C                   MOVE      d1dfc         G02DAT
039000150617     C                   MOVEL     *BLANK        G02ERR
039100150617     C                   CALL      'XSRDA8'
039200150617     C                   PARM                    WLBDA8
039300150617     C     G02ERR        IFEQ      '1'
039400150626     C                   SETON                                        6599
039500150617     c                   leavesr
039600150617     C                   END
039700150617     c                   move      g02dat        d1dfc
039800150703     C                   move      g02inv        ddfc
039900150617     c                   Endif
040000150707      * Data fine deve essere maggiore di durc o cip
040100150707     c                   if        ddfc  < ddurc or
040200150707     c                             ddfc  < dcip
040300150707     c                   seton                                        7199
040400150707     c                   leavesr
040500150707     c                   Endif
040600150707     C*   Non è un Consorzio
040700150707     C                   SETOff                                       68
040800150707     c                   if           S1_NatGiur <>'11'  and ddfc > data_oggi
040900150707     C                   SETON                                        6899
041000150707     c                   leavesr
041100150707     c                   end
041200940117     C                   ENDSR
041300940131     C************************************************************
041400090320      *?  OPERAZIONI INIZIALI                             ?
041500940131     C************************************************************
041600940131     C     *INZSR        BEGSR
041700090706     C*
041800940127     C* Reperimento parametri
041900940127     C*
042000940117     C     *ENTRY        PLIST
042100940117     C                   PARM                    KPJBA
042200150622     C                   movel     kpjbu         TRMZ70S8DS
042300120323     c                   setoff                                       23  25
042400150626     C*
042500940223     C* Variabili per gestione videate
042600940223     C                   MOVE      *OFF          $FINE
042700940223     C                   MOVE      *ON           $INZD1
042800940224     C                   MOVE      *BLANK        $LASTG
042900940224     C                   MOVE      *BLANK        $LASTV
043000940506     C                   Z-ADD     0             $ULKD1            3 0
043100940127     C*
043200940223     C* Variabili appoggio
043300940224     C                   Z-ADD     0             CURR
043400940224     C                   Z-ADD     0             CURC
043500940207     C*
043600940207     C* Valorizzazione campi univoci testate
043700150616     C                   MOVEL     DSPGM         d1pgm
043800090319     C*
043900090319     C* ESSENDO SEQUENZIALI I 3 DISPLAY LI PULISCO TUTTI UNA VOLTA SOLA
044000090319     C* Pulisce entrambe i 3 video
044100090319     C                   CLEAR                   D1
044200090319     C*
044300090323     C*  giro la udate da GMA in AMG
044400090323     C                   move      *date         dataeur
044500090323     C                   move      dataeur       dataiso
044600090323     C                   move      dataiso       data_oggi
044700090323     C*
044800090319     C* Inizializzazione/Decodifica prima videata e seconda
044900090319     C                   MOVE      'D1'          $GEST
045000090319     C                   MOVE      *ON           $INZD1
045100150626     C*
045200150626     C*  Controlla che se richiamato in immissione NON ci sia già il record
045300150626     C*   altrimenti deve trasformare in Manutenzione.
045400150626      *
0455001507031   >C     DS8opz        IFEQ      Immissione
045600150626      *
045700150626      **  Deve controllare se esiste l'anagrafica inserita AITRC
045800150626     C                   eval        wTRCIVA = ds8IVA
045900150626     C                   eval        wTRCPICF= ds8Ivaa
046000150706     c     Ktrc          chain     aitrc01l
046100150706     c                   if        %found(aitrc01l)
046200150703     c                   eval      ds8OPZ = Modifica
046300150626     c                   end
046400150626     C*
046500150626     c                   end
046600150703     C* x copia
046700150703     c                   if        ds8OPZ = Copia
046800150703     C                   clear                   ds8Ivaa
046900150703     c                   eval      ds8OPZ = Immissione
047000150703     c                   end
047100090319     C*
047200090319     C*  Campi protetti o HI o RI
047300090319     C                   SETOFF                                       020304
047400090319     C                   SETOFF                                       050607
047500090319     C                   SETOFF                                       080911
047600090327     C                   SETOFF                                       101213
047700090706     C                   SETOFF                                       151601
047800090706     C*
047900150623     C*  se richiamato in immissione ricevendo la chiave
048000090706     C*
048100090319     C                   SELECT
048200150622    >C     DS8opz        WHENEQ    Immissione
048300090319     c                   movel     Opz_Imm       D1Fun
048400150615     C                   SETON                                        02
048500150622    >C     DS8opz        WHENEQ    Visualizza
048600090319     c                   movel     Opz_Vis       D1Fun
048700150615     C                   SETON                                        0203
048800150622    >C     DS8opz        WHENEQ    Cancella
048900090319     c                   movel     Opz_Del       D1Fun
049000090319     C                   SETON                                        0203
049100150703     C                   SETON                                        16
049200150622    >C     DS8opz        WHENEQ    Modifica
049300090319     c                   movel     Opz_Var       D1Fun
049400090319     C                   SETON                                        02
049500090327     C*
049600090319     C                   OTHER
049700090330     C*
049800090319     C                   ENDSL
049900940127     C*
050000940117     C                   ENDSR
050100090317     c*--------------------------------------------------------------
050200090317     c* tramite Società e Unità decodifica P.IVA su PROJ
050300090317     c*--------------------------------------------------------------
050400090317     C     Decod_Rag_IVA BegSR
050500090317      **
050600090317      **  Routine x Reperire dati Fornitore:
050700090317      **    La routine in base alla sottonatura può funzionare
050800090317      **     x F=Fornitore/C=Cliente
050900110401      *
051000090408     C                   clear                   trmz70s1ds                     Input
051100150615     C                   movel(p)  TRciva        s1_PartitaIVA                  Input
051200090408     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
051300150625     C                   movel(p)  *blank        s1_Societa                     Input/Output
051400090401     c                   call      'TRMZ70SR1'
051500090408     C                   PARM                    trmz70s1ds                     Input
051600150616      *
051700090317     C                   ENDSR
051800090402     C************************************************************
051900090402     C* GESTIONE F03 VIDEO D1
052000090402     C************************************************************
052100090402     C     F03Fine       BEGSR
052200090402     C*
052300090319     C                   MOVE      *ON           $FINE
052400150622    >C                   MOVE      '1'           DS8ret
052500090319     C*
052600090319     C                   ENDSR
052700090319     C************************************************************
052800090319     C* GESTIONE F12 VIDEO D1
052900090319     C************************************************************
053000090319     C     F12Ritorna    BEGSR
053100090319     C*
053200090319     C                   MOVE      *ON           $FINE
053300090319     C*
053400090319     C                   ENDSR
053500090319     C************************************************************
053600150615     C* aggiornamento immissione
053700090319     C************************************************************
053800150615     C     AggImmis      BEGSR
053900090409     C*
054000150615     c                   if        not *in02
054100150615     c                   eval      TRcIVA   =  D1IVA
054200150617     c                   eval      TRcpicf  =  D1IVAa
054300150616     c                   else
054400150616     c                   eval      wtrciva  = d1iva
054500150616     c                   eval      wtrcpicf = d1ivaa
054600150616     c     ktrc          chain     aitrc01l
054700150616     c                   if        not %found(aitrc01l)
054800150616     c                   seton                                        9904
054900150616     c                   leavesr
055000150616     c                   endif
055100150615     c                   endif
055200150703      *
055300150703     c   16              eval      trcann = 'A'
055400150703     c                   eval      TRCDDURC =  Ddurc
055500150703     c                   eval      TRCDCIP  =  Dcip
055600150703     c                   eval      TRCDFC   =  Ddfc
055700150703      *
055800150616     c                   eval      trcpicf  =  D1IVAa
055900150616     c                   eval      TRCRAGSOC=  D1IVAd
056000150616     c                   eval      trcute   =  knmus
056100150616     c                   eval      trcdva   =  data_oggi
056200090409      *
056300150615     c  n02              write     aitrc000
056400150615     c   02              update    aitrc000
056500090330      *
056600090330     C                   ENDSR
056700090506     C************************************************************
056800090506      *? DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...   ?
056900090506     C************************************************************
057000090506     C     DEFCAM        BEGSR
057100090506     C*
057200090320     C* Variabili per gestione videate
057300090320     C                   MOVE      *BLANK        $GEST             2
057400090320     C                   MOVE      *BLANK        $FINE             1
057500090320     C                   MOVE      *BLANK        $INZD1            1
057600090320     C                   MOVE      *BLANK        $INZD2            1
057700090320     C                   MOVE      *BLANK        $INZD3            1
057800090320     C                   MOVE      *BLANK        $INZD4            1
057900090320     C                   MOVE      *BLANK        $LASTG            2
058000090320     C                   MOVE      *BLANK        $LASTV            2
058100090320     C*
058200090320     C* Variabili appoggio
058300090320     C                   Z-ADD     0             CURR              2 0
058400090320     C                   Z-ADD     0             CURC              2 0
058500090320     C                   MOVE      *ZEROS        WIN              99
058600090320     C                   MOVE      *BLANK        WTBERR            1
058700090320     C*
058800150616     c     Ktrc          klist
058900150616     c                   kfld                    wTRCIVA          16
059000150616     c                   kfld                    wTRCPICF         16
059100090320     C* Indici
059200090320     C                   Z-ADD     0             X                 3 0
059300090320     C                   Z-ADD     0             Y                 3 0
059400090320     C*
059500090320     C                   ENDSR
059600091215     C************************************************************
