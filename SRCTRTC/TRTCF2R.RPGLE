000100960530      ***********************************************************************
000200020621      *   importa DATI stati BOLLE FEDEX       ESTERO     Export            *
000300021114      *
000400021114      *  =======================
000500021114      *  C O M P I L A Z I O N E   avvertenze :
000600021114      *  =======================
000700021114      *
000800960530      ***********************************************************************
000900101215     H DECEDIT('0,') DATEDIT(*YMD/)
001000000207      *---------------------------------------------------------------------*
001100021022     fFifes00f  UF   E           K DISK
001200020625      *
001300020625     FFNCDE02L  UF A E           K DISK
001400020626     FFNCDS02L  UF A E           K DISK
001500020625     FFNEVB00F  O    E             DISK
001600020624      *
001700020709     FFnLbl02L  IF   E           K DISK
001800060213     FFiAr404L  IF   E           K DISK
001900051017     FFIArs10L  UF   E           K DISK
002000020709     FFnArt27L  IF   E           K DISK
002100021021     FFnArt01L  iF   E           K DISK    RENAME(FNART000:FNART001)
002200021028     FFnArb01L  iF   E           K DISK
002300021028      *
002400030527     FTABEL00F  IF   E           K DISK
002500021028     FTNTBE01L  iF   E           K DISK
002600020624      *
002700020625     FEDTAB01L  IF   E           K DISK
002800020625      *
002900020625     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003000020625     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)
003100021114      *
003200021114     FFEDEXPRT  O    F  132        PRINTER OFLIND(*INOB)
003300000207      *---------------------------------------------------------------------*
003400000207      * Schiere
003500000207      *---------------------------------------------------------------------*
003600020625     D  ERR            S             70    DIM(10) CTDATA PERRCD(1)
003700020625      *---------------------------------------------------------------------*
003800020625      * Tabella codici eventi
003900020625     D CEV             S              6    DIM(200)
004000020625     D DEV             S             90    DIM(200)
004100021028      * Per i riferimenti bolle non trovati memorizza in una schiera
004200021028     D AWB             S             12    DIM(200)
004300000207      *---------------------------------------------------------------------*
004400000207      * DS
004500000207      *---------------------------------------------------------------------*
004600020621     D DStesta       E DS                  EXTNAME(TRTCF1DS)
004700020621     D DSdetta       E DS                  EXTNAME(TRTCF2DS)
004800020621     D DSd20         E DS                  EXTNAME(TRTCF20DS)
004900020621     D DSd25         E DS                  EXTNAME(TRTCF25DS)
005000020621     D DSd29         E DS                  EXTNAME(TRTCF29DS)
005100020621     D DSd30         E DS                  EXTNAME(TRTCF30DS)
005200020621     D DSd321        E DS                  EXTNAME(TRTCF32DS1)
005300020621     D DSd322        E DS                  EXTNAME(TRTCF32DS2)
005400020621     D DSd323        E DS                  EXTNAME(TRTCF32DS3)
005500020621     D DSd324        E DS                  EXTNAME(TRTCF32DS4)
005600020621     D DSd325        E DS                  EXTNAME(TRTCF32DS5)
005700020621     D DSd326        E DS                  EXTNAME(TRTCF32DS6)
005800020621     D DSd327        E DS                  EXTNAME(TRTCF32DS7)
005900020621     D DSd328        E DS                  EXTNAME(TRTCF32DS8)
006000030218     D DSd3214       E DS                  EXTNAME(TRTCF32DS9)
006100020621     D DSd92         E DS                  EXTNAME(TRTCF92DS)
006200020621     D DSd93         E DS                  EXTNAME(TRTCF93DS)
006300020625      *
006400030605
006500030605     d Param           ds
006600030605     d  Aas                    1      4  0
006700030605     d  Lnp                    5      7  0
006800030605     d  Nrs                    8      9  0
006900030605     d  Nsp                   10     16  0
007000030605     d  VolMCt                17     21  3
007100030605
007200950428     D KPJBA         E DS
007300020625     D UT§DSE0F      E DS
007400030527     D DSQT1         E DS                  INZ
007500021114     D Tibs36ds      E DS
007600020920     d trul26ds      e ds
007700020920     d  skpo                  11    760  0 dim(250)
007800020920     d  skpot                761   1510  0 dim(250)
007900020625     D EDIDSCE       E DS
008000020625      *
008100960530     D WLBDA8          DS                  INZ
008200950428     D  G02DAT                 1      8  0
008300950428     D  G02INV                 9     16  0
008400950428     D  G02ERR                17     17
008500950428     D  G02TGI                18     22  0
008600020625      *
008700020625     D DSSPE           DS
008800020625     D  WAAS                   1      4  0
008900020625     D  WLNP                   5      7  0
009000020703     D  WNRS                   8      9  0
009100020625     D  WNSP                  10     16  0
009200021217      *
009300021018     D  Errori         S              1
009400021018      *
009500021018     D  Bolla_ok       S              2
009600020715     D  Master_ok      S              2
009700021031     D  Multicollo     S              2
009800020625     D  Wvalor         S             35
009900020625     D  WANNO          S              4  0
010000020625     D  WLINPA         S              3  0
010100020625     D  WSERIE         S              2  0
010200020625     D  WSPEDI         S              7  0
010300021031      *
010400021031     D  svcdendc       S                   like(Kndc)
010500021031     D  svcdeaas       S                   like(Kaas)
010600021031     D  svcdelnp       S                   like(Klnp)
010700021031     D  svcdenrs       S                   like(Knrs)
010800021031     D  svcdensp       S                   like(Knsp)
010900021028      *
011000021028     D SCAN01DS        DS
011100120305      * Luoghi di presa in carico merce da BRT a Fedex
011200021028     D  SC01                   1    125    DIM(5)
011300020624      *
011400020703     D*  Ds per composizione errore <> nr.colli
011500020703     D ERRCOL          DS            35
011600020703     D  COLCON                 7     11  0
011700020703     D  COLSPE                16     20  0
011800020625      *---------------------------------------------------------------------*
011900020625      * Campi del Messaggio impostati Fissi -> eventualmente da modificare <-
012000020625      *---------------------------------------------------------------------*
012100020703     D digits          C                   CONST('0123456789')
012200020624      *
012300020625      ***********************************************************************
012400020624      * Ciclo principale                                                   -*
012500020625      ***********************************************************************
012600020625     c                   clear                   typrec            1
012700020625      *
012800020621     c                   read      fifes00f
012900020624      *
013000020621     c                   dow       not %eof(fifes00f)
013100020625     c                   movel     fesbol        typrec            1
013200020621      *
013300020621     c                   select
013400020621      *
013500020624      *   decodifica dati di testata
013600020625      *     ed ad ogni testata reinizializza tutto (clear-all)
013700020621     c                   when      typrec ='H'
013800020625     C                   clear                   DStesta
013900020625     C                   clear                   DSspe
014000021021     C                   clear                   wri_cde           1
014100030218      * per prendere nel dettaglio solo il primo cambio di peso segnalato
014200030218      * perchè il più recente.               "Change Weight"
014300030513      *  e solo se si riferisce alla testata dell'AWB Master (così
014400030513      *  lo gestisce la Fedex).
014500030218     C                   clear                   chg_wgt           1
014600121016     c                   clear                   sav_F3AWTC        1
014700021021      *
014800020621     c                   exsr      Dec_testa
014900020621      *
015000020624      *   decodifica dati di dettaglio
015100020705     c                   when      typrec ='D' and bolla_ok='ok'
015200020625     c                   exsr      Clear_DS
015300021021      *
015400020625     c                   exsr      Dec_detta
015500020621      *
015600020621     c                   endsl
015700020621      *
015800020625      *   quindi elimina il record letto e ne legge un altro
015900120305      *    solo se la testata è stata riconosciuta come bolla BRT
016000020705      *     altrimenti testata e dettagli rimangono da cancellare e da
016100020705      *    controllare in seguito.
016200020705     c                   if        bolla_ok='ok'
016300020625     c                   delete    fifes000
016400020705     c                   end
016500020705      *
016600020621     c                   read      fifes00f
016700020628      *
016800020628      * identifica se testata o dettaglio
016900020628     c                   if        not %eof(fifes00f)
017000020628     c                   movel     fesbol        typrec
017100020628     c                   end
017200020628      *
017300020628      *  o per (EoF) o per nuova testata letta scrive il FNCDE
017400020705     c                   if        bolla_ok='ok'
017500021021     c                   if        wri_cde <> *blank
017600020628     c                   exsr      Write_CDE
017700021021     c                   clear                   wri_cde
017800020628     c                   end
017900020705     c                   endif
018000020628      *
018100020621     c                   enddo
018200030611      * a conclusione
018300030611      *  stampa il totale delle bolle variate.
018400030611     C                   EXCEPT    ChgVolTot
018500030611      *
018600021018     c                   exsr      EXIT
018700020621      *----------------------------------------------------------------
018800020625      * reinizializza ad ogni giro tutti i campi delle DS
018900020621      *----------------------------------------------------------------
019000020625     C     Clear_DS      Begsr
019100020625      *
019200020625     C                   clear                   DSdetta
019300020625     C                   clear                   DSd20
019400020625     C                   clear                   DSd25
019500020625     C                   clear                   DSd29
019600020625     C                   clear                   DSd30
019700020625     C                   clear                   DSd321
019800020625     C                   clear                   DSd322
019900020625     C                   clear                   DSd323
020000020625     C                   clear                   DSd324
020100020625     C                   clear                   DSd325
020200020625     C                   clear                   DSd326
020300020625     C                   clear                   DSd327
020400020625     C                   clear                   DSd328
020500030218     C                   clear                   DSd3214
020600020625     C                   clear                   DSd92
020700020625     C                   clear                   DSd93
020800021022     C                   clear                   Carica_eve        1
020900020625      *
021000020625     C                   Endsr
021100020625      *----------------------------------------------------------------
021200020625      * decodifica dati di testata
021300020625      *----------------------------------------------------------------
021400020625     C     Dec_Testa     Begsr
021500020703      *
021600020710      * Eseguo clear formato record
021700020710     C                   CLEAR                   FNCDE000
021800020710      *
021900020715     C                   clear                   Wteslb            1
022000020703     C                   clear                   WMEMCO            1
022100020703     C                   CLEAR                   WTOCOP
022200020703      *
022300020705     C                   clear                   CD§DCM
022400020705     C                   clear                   CD§HMC
022500020705     C                   clear                   CD§DLA
022600020705     C                   clear                   CD§DAG
022700020705     C                   clear                   CD§DRC
022800020705     C                   clear                   CD§NO1
022900020705     C                   clear                   CD§NO2
023000020705     C                   clear                   CD§CMC
023100020705     C                   clear                   CD§CCA
023200020705     C                   clear                   CD§ERR
023300020705     C                   clear                   CD§STS
023400020705     C                   clear                   CD§COD
023500020705     C                   clear                   CD§MOT
023600020705     C                   clear                   CD§FL1
023700020705     C                   clear                   CD§FL2
023800020625      *---------
023900020621     c                   movel     fesbol        DStesta
024000020627     c                   movel     f1hsna        w1hsna           25
024100020627     c                   movel     f1hsco        w1hsco           25
024200020627     c                   movel     f1hrna        w1hrna           25
024300020627     c                   movel     f1hrco        w1hrco           25
024400020627     c                   movel     f1hsad        w1hsad           20
024500020627     c                   movel     f1hrad        w1hrad           20
024600020627     c                   movel     f1hsci        w1hsci           25
024700020627     c                   movel     f1hrci        w1hrci           25
024800021217     c                   movel     f1href        w1href           16
024900020625      *---------
025000020711      * con AWB è possibile risalire alla bolla tramite ARS ART e ARB
025100020625     c                   move      'ok'          bolla_ok
025200020711     c                   move      'no'          master_ok
025300021031     c                   move      'no'          Multicollo
025400020711      *
025500060213      * cerca comunque se presente il riferimento numerico su FIAR400F
025600020711      *  per controllare se la testata contiene l'AWB Master.
025700020711     C                   eval      Ktrc = 'E'
025800020711     C                   clear                   Kn14
025900020711     c                   movel     F1HANR        Kn14
026000060213     C     Kar4          chain     FiAR404L
026100020711      *
026200021031      *              *----------*
026300060213     c                   if        %Found(FiAr404L)
026400021031      *              *----------*
026500020711     c                   move      'ok'          master_ok
026600020711      * quindi imposta la chiave per reperire la bolla
026700020711     C                   Z-ADD     Ar4AAS        WAAS
026800020711     C                   Z-ADD     Ar4LNP        WLNP
026900020711     C                   Z-ADD     Ar4NRS        WNRS
027000020711     C                   Z-ADD     Ar4NSP        WNSP
027100021031      *
027200021031      * Loop su FNART per verificare se spedizione multicollo
027300021031     C                   Z-ADD     0             nrcolli           5 0
027400080128     C**** Kart_ar4      setll     FNART01L
027500080128     C**** Kart_ar4      reade(N)  FNART01L
027600080128     C     Kart_w        setll     FNART01L
027700080128     C     Kart_w        reade(N)  FNART01L
027800021031     C                   DOW       Not %Eof(FNART01L)
027900021031     C                   eval      nrcolli = nrcolli + 1
028000080128     C**** Kart_ar4      reade(N)  FNART01L
028100080128     C     Kart_w        reade(N)  FNART01L
028200021031     C                   END
028300021031      *
028400021031      * Più segnacolli quindi multicollo
028500021031     c                   if        nrcolli > 1
028600021031     c                   move      'si'          Multicollo
028700021031     c                   endif
028800020711      *
028900021031      *              *----------*
029000020711     c                   else
029100021031      *              *----------*
029200051017      *  altrimenti deve prima cercare con AWB su FIARS poi su FnART
029300020711      *   per giungere finalmente alla chiave di FnARB
029400020711      *
029500020625     c                   clear                   Karsnot
029600020625     c                   movel     F1HANR        Karsnot
029700021029      *
029800020624      *  imposta il campo Note
029900051017     c     Karsnot       setll     FIArs10L
030000051017     c     Karsnot       reade(N)  FIArs10L
030100051017     c                   dow       not %eof(FIArs10L)
030200021029     c                   if        ArsTrc ='F'
030300021029     c                   leave
030400021029     c                   end
030500051017     c     Karsnot       reade(N)  FIArs10L
030600021029     c                   enddo
030700020711      *
030800051017     c                   if        %EoF(FIArs10L)
030900020625     c                   move      'no'          bolla_ok
031000020624     c                   else
031100020624      * preso riferimenti su ARS cerca su ART
031200020624     C                   Z-ADD     ARSFLS        KFLS
031300020624     C                   Z-ADD     ARSLNA        KLNA
031400020624     C                   Z-ADD     ARSNRS        KNRS
031500020624     C                   Z-ADD     ARSNSC        KNSC
031600020627     C     Kart27        CHAIN     FnArt27l
031700020711      *
031800020625     c                   if        Not %Found(FnArt27L)
031900020625     c                   move      'no'          bolla_ok
032000020625     c                   else
032100080128      *
032200080128      *  imposta la chiave per reperire la bolla
032300080128     c                   move      'no'          Multicollo
032400020625     C                   Z-ADD     ARTAAS        WAAS
032500020625     C                   Z-ADD     ARTLNP        WLNP
032600020625     C                   Z-ADD     ARTNRS        WNRS
032700020625     C                   Z-ADD     ARTNSP        WNSP
032800080128      *
032900080128      * Loop su FNART per verificare se spedizione multicollo
033000080128     C                   Z-ADD     0             nrcolli           5 0
033100080128     C     Kart_w        setll     FNART01L
033200080128     C     Kart_w        reade(N)  FNART01L
033300080128     C                   DOW       Not %Eof(FNART01L)
033400080128     C                   eval      nrcolli = nrcolli + 1
033500080128     C     Kart_w        reade(N)  FNART01L
033600080128     C                   END
033700080128      *
033800080128      * Più segnacolli quindi multicollo
033900080128     c                   if        nrcolli > 1
034000080128     c                   move      'si'          Multicollo
034100080128     c                   endif
034200080128      *
034300020711     C                   end
034400020711     C                   end
034500020711      *
034600021031      *              *----------*
034700020711     c                   endif
034800021031      *              *----------*
034900020711      *
035000021217      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
035100021217      *  Se non è riuscito a trovare nulla con AWB prova con il riferimento
035200120305      *   BRT. E'però possibile che Fedex cambi il nostro rif.nr.bolla
035300021217      *    quindi non è possibile fare sempre affidamento su questo dato restituito.
035400021217     c                   If        bolla_ok = 'no'
035500021217     C     Digits        Check     w1href:1
035600021217     C                   If        Not %Found
035700021217      *  queste specifiche non sono state attivate occorre controllare bene in quanto
035800021217      *  Fedex cambia il nr.AWB per ogni modifica apportata ai dati sul PC di Bovisa
035900021217      *  quindi si perde il legame della bolla.
036000021217      *  Resta il nostro riferimento Bolla inviato che però può essere cambiato in Fedex
036100021217      *  e utilizzato come campo info varie
036200021217     C********           movel     f1href        DSSpe
036300021217     C********           If        waas > 2001 and waas < 2099
036400021217     c********           move      'ok'          bolla_ok
036500021217     c********           end
036600021217     c                   end
036700021217     c                   endIF
036800021217      *
036900021031      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
037000021031      *
037100020715      * Aggancio FNARB controllando se figlia o mamma
037200020715      *   e se ha già un record su FNCDE/FNCDS
037300020711     c                   if        bolla_ok ='ok'
037400020626     c                   exsr      Chk_Bolla
037500020711     C                   endif
037600021028      *
037700021028     C                   MOVEL     'S'           in_stampa         1
037800020711      *
037900020711      * Msg di errore x bolla
038000021022      *  se non ha agganciato la bolla lo stampa
038100021022     c                   setoff                                       77
038200020625     c                   if        bolla_ok ='no'
038300020709     c                   movel     Err(1)        deserr           78
038400021022     c                   seton                                        77
038500020625     C                   clear                   WVALOR
038600020625     C                   MOVEL     Karsnot       WVALOR
038700021018     C                   Z-ADD     0             WANNO
038800021018     C                   Z-ADD     0             WLINPA
038900021018     C                   Z-ADD     0             WSERIE
039000021018     C                   Z-ADD     0             WSPEDI
039100021028      *
039200021028      * memorizza x non ripetere in stampa questo tipo d'informazione
039300021028     c                   z-add     1             ax                3 0
039400021028     c     f1hanr        lookup    awb(ax)                                88
039500021028     c     *in88         ifeq      *off
039600021028     c                   z-add     1             ax                3 0
039700021028     c     *blank        lookup    awb(ax)                                88
039800021028     c                   movel     f1hanr        awb(ax)
039900020625     C                   EXSR      STPERR
040000021028     C                   else
040100021028     C                   MOVEL     'N'           in_stampa
040200021028     C                   end
040300021028      *
040400020625     C                   end
040500020625      *
040600020627      * Stampa informazioni spedizione
040700021028     c     in_stampa     ifeq      'S'
040800020627     C   OA              EXCEPT    TEST1
040900020627     C                   EXCEPT    DATMSG
041000021028     C                   Else
041100021028     C                   MOVEL     'S'           in_stampa
041200021028     C                   End
041300020627      *
041400020621     C                   Endsr
041500020626      *----------------------------------------------------------------
041600020626      *  Aggancia la bolla se già arrivata
041700020626      *----------------------------------------------------------------
041800020626     C     Chk_Bolla     Begsr
041900020626      *
042000020626      * Memorizzo la Key sped. x eventuale MIC
042100020626     C                   z-add     WAAS          MICAAS
042200020626     C                   z-add     WLNP          MICLNP
042300020626     C                   z-add     WNRS          MICNRS
042400020626     C                   z-add     WNSP          MICNSP
042500020626      *
042600020626      * Se la spedizione ha una figlia eseguo chain su FNCDE con quest'ultima
042700020626     C                   MOVEL     WAAS          KAAP
042800020626     C                   MOVEL     WLNP          KLPP
042900020626     C                   MOVEL     WNRS          KNRP
043000020626     C                   MOVEL     WNSP          KNSP1
043100020715     C     Kart          SETGT     FNLBL02L
043200020715     C     Kart          READPE    FNLBL02L                               33
043300020626      *
043400020626     C     *IN33         IFEQ      '0'
043500020626     C     LBLLAN        ANDEQ     LBLLAP
043600020626     C                   MOVEL     LBLAAN        KAAS
043700020626     C                   MOVEL     LBLLPN        KLNP
043800020626     C                   MOVEL     LBLNRN        KNRS
043900020626     C                   MOVEL     LBLNSN        KNSP
044000020626     C                   MOVEL     'S'           WMAMMA            1
044100020626     C                   ELSE
044200020626     C                   MOVEL     WAAS          KAAS
044300020626     C                   MOVEL     WLNP          KLNP
044400020626     C                   MOVEL     WNRS          KNRS
044500020626     C                   MOVEL     WNSP          KNSP
044600020627     C                   MOVEL     'N'           WMAMMA
044700020626     C                   END
044800020626      *
044900020626      * Controllo esistenza spedizione in FNCDE
045000020708     c                   move      'NO'          esiste_CDE        2
045100020708     c                   move      'NO'          esiste_CDS        2
045200020708     C                   MOVEL     *BLANKS       WSTS
045300020708     C                   MOVEL     *BLANKS       WCODE
045400020708     C                   SETOFF                                       37
045500020708      *
045600021031      * Salva la chiave per aggiornare in seguito
045700021031     C                   eval      svcdendc = KNDC
045800021031     C                   eval      svcdeaas = KAAS
045900021031     C                   eval      svcdelnp = KLNP
046000021031     C                   eval      svcdenrs = KNRS
046100021031     C                   eval      svcdensp = KNSP
046200021031      *
046300021031     C     KCDE          CHAIN(N)  FnCde02l
046400020708     c                   if        %Found(FnCde02l)
046500020708     c                   move      'SI'          esiste_CDE
046600020708     C                   MOVEL     CDESTS        WSTS
046700020708     C                   MOVEL     CDECOD        WCODE
046800020626      *
046900020626      * Controllo esistenza spedizione in FNCDS
047000020708     C     KCDE          SETLL     FnCds000                               37
047100020710     c   37              move      'SI'          esiste_CDS
047200020708      *
047300020708     c                   endif
047400020626      *
047500020711      * Aggancio FNARB mamma se esiste e prende la linea di ARRIVO
047600020626     C                   Z-ADD     *ZEROS        micLNA
047700020626     C                   SETON                                        38
047800020626     C     KAAS          IFNE      micAAS
047900020626     C     KLNP          ORNE      micLNP
048000020626     C     KNRS          ORNE      micNRS
048100020626     C     KNSP          ORNE      micNSP
048200020718     C     Karb_Mic      chain(N)  FnArb01l                           38
048300020626     C  N38              z-add     arbLNA        micLNA
048400020626     C                   End
048500020626      *
048600020711      * Aggancio comunque di nuovo FNARB
048700020626     C                   Z-ADD     KAAS          CDEAAS
048800020626     C                   Z-ADD     KLNP          CDELNP
048900020626     C                   Z-ADD     KNRS          CDENRS
049000020626     C                   Z-ADD     KNSP          CDENSP
049100020718     C     KARB          CHAIN(N)  FnArb01l                           34
049200020715     c                   if        Not %Found(FnArb01L)
049300020715     c                   move      'no'          bolla_ok
049400020715     C                   end
049500020626      *
049600020626     C                   Endsr
049700020621      *----------------------------------------------------------------
049800020624      * decodifica dati di dettaglio
049900020621      *----------------------------------------------------------------
050000020621     C     Dec_Detta     Begsr
050100020624      *
050200020621     c                   movel     fesbol        DSdetta
050300021018      *
050400021018      * Dettaglio ricevuto
050500021022     C   OA              EXCEPT    TEST1
050600021022     C                   EXCEPT    DETRIC
050700021022      *
050800021022      *  reperisce tabella "CE" Eventi
050900021022     c                   exsr      TAB_Eventi
051000021022      *
051100021022      *  altre informazioni particolari su SCAN specifici
051200021022     c                   exsr      SubScan_DS
051300020625      *
051400020621     C                   Endsr
051500020624      *----------------------------------------------------------------
051600020625      *  aggancia la tabella Eventi EDI (CE - EDTAB00F)
051700020625      *----------------------------------------------------------------
051800020625     C     TAB_Eventi    Begsr
051900020626      *
052000020626     C                   Z-ADD     0             WDTEVE            8 0
052100020626     C                   Z-ADD     0             WHHEVE            4 0
052200020626      *
052300021029      *
052400020626      *  Imposto dati spedizione in stampa
052500020626     C                   Z-ADD     KAAS          WANNO
052600020626     C                   Z-ADD     KLNP          WLINPA
052700020626     C                   Z-ADD     KNRS          WSERIE
052800020626     C                   Z-ADD     KNSP          WSPEDI
052900020626      *
053000020626      *   aggancia tab.CE con lo SCAN Type
053100020627      *   "Fxx___"
053200020625     c                   clear                   wcev              6
053300020625     C                   MOVEL     'F  '         fld3              3
053400020625     C                   move      f2dsty        fld3
053500020625     C                   movel     fld3          WCEV
053600020627      *
053700021021     C                   move      'SC'          Scan_Stat         2
053800020627     c                   exsr      chk_evento
053900020625      *
054000020627      *   aggancia tab.CE con lo SCAN Status
054100020627      *   "F__xx_"
054200020628     C                   if        f2dsta <> *blank
054300020628      *
054400020627     c                   clear                   wcev
054500020627     C                   MOVEL     'F  '         wcev
054600020627     c                   clear                   fld3
054700020627     C                   movel     f2dsta        fld3
054800020627     C                   move      fld3          WCEV
054900020627      *
055000021021     C                   move      'ST'          Scan_Stat         2
055100020627     c                   exsr      chk_evento
055200020628      *
055300020628     c                   endif
055400020625      *
055500020625     C                   Endsr
055600021029      *-------------------------------------------------------------*
055700021029      *? Esegue i controlli sui flags dell'evento della tab.:CE
055800021029      *----------------------------------------------------------------
055900021029     C     Chk_Evento    BEGSR
056000021029      *
056100021029      *      *-----------------------------------------------------*
056200021029     c                   clear                   proofpck          1
056300021029      *
056400021029      *  Controllo su scan "01" particolare per la presa in carico della merce
056500021029      *    se è nella schiera lo deve accettare come proof of pickup
056600021029      *       altrimenti lo imposta come da ignorare.
056700021029     C                   if        Scan_Stat = 'SC'
056800021029     C                             and f2dsty = '01'
056900021029     c     f2dsci        lookup    SC01                                   88
057000021029     c   88              move      'S'           proofpck
057100021029      *                *---------------------*
057200021029     c   88              exsr      Upd_FNARS
057300021029      *                *---------------------*
057400021029     c  N88              move      'N'           proofpck
057500021029     c                   endif
057600021029      * Attenzione: se lo scan 01 non rientra fra quelli particolari
057700021029      * occorre ignorarlo poichè per noi è ininfluente
057800021029      *      *-----------------------------------------------------*
057900021029      *
058000021029      * Se le date ricevute sono superiori a quelle memorizzate nel CDE
058100021029      *  oppure su ho ricevuto una presa in carico merce (Scan 01 particolare)
058200020627     C     f2dsdt        IFGE      CDEDCM
058300020627     C     f2dsdt        ANDGE     CDEDAG
058400020627     C     f2dsdt        ANDGE     CDEDLA
058500021029     c     Proofpck      oreq      'S'
058600020627      *
058700021029      *  Scan TYPE/STATUS
058800020627     C                   Z-ADD     1             X
058900020627     C     WCEV          LOOKUP    CEV(X)                                 32
059000020627      *
059100021029      *  Attenzione: se è uno Scan Type (01) ma non è un "proof of pickup"
059200021029      *   lo scan deve esser ignorato.
059300021029      *
059400021022      *  Scan Type/Status: non previsto in tabella
059500020627     C     *IN32         IFEQ      '0'
059600020627      *
059700020627     C                   MOVEL     ERR(2)        DESERR                         Evento
059800020627     C                   MOVEL     *BLANKS       WVALOR                         non
059900020627     C                   MOVEL     WCev          WVALOR                         previsto
060000021114     c                   move      'S'           wscnsts           1
060100020627     C                   EXSR      STPERR
060200021114     c                   move      ' '           wscnsts
060300020627     C                   MOVEL     'S'           §CECAR
060400020627      *
060500020627     c                   else
060600020627      *
060700021022     C                   MOVEL     'S'           Carica_eve        1
060800020627     C                   MOVEL     DEV(X)        EDIDSCE
060900021029      *
061000021029      * Attenzione se il flag ProofPck -> "N"
061100021029      *  deve impostare la DS come se l'evento fosse da ignorare
061200021029     c     Proofpck      ifeq      'N'
061300021029     C                   clear                   EDIDSCE
061400021029     C                   MOVEL     'S'           §CECAR
061500021029     c                   endif
061600021029      *
061700020627     C                   MOVEL     §CECCA        CD§CCA
061800020627     C                   MOVEL     §CEERR        CD§ERR
061900020627     C                   MOVEL     §CEDEC        CD§MOT
062000020703     C                   MOVEL     §CEMEM        WMEMCO
062100021022      *---------
062200021022      *  Se trovato stampo codice mancata consegna altrimenti stampo ***
062300021022     C                   MOVEL     §CEDEC        WDEC             35
062400021022     C                   MOVEL     §CECEV        WC2A              3
062500021025     C                   MOVEL     *blank        WC2X             10
062600021022      *
062700021022      *  se codice evento ATR stampo in ogni caso '???'
062800021022     C     §CETPE        IFEQ      'V'
062900021025     C                   MOVEL     '? Arrivo ?'  WC2X
063000021022     C                   END
063100021022      *
063200021022      *  Stampo dati spedizione ed evento - solo se trovato FNARB
063300021022     c                   setoff                                       44
063400021022     c                   if        Scan_Stat = 'ST'
063500021022     c                   seton                                        44
063600021022     C                   End
063700021022      *
063800021022     C   OA              EXCEPT    TEST1
063900021022     C                   EXCEPT    DecTabCE
064000021022      *---------
064100021021      *
064200021021     c* se deve forzare la scrittura del CDE imposta il flag
064300021021     c*  per evidenziare lo scan ricevuto
064400021021      *               *-----------------------------------*
064500021022     c                   if        §ceerr ='S'
064600021022     c                   move      'S'           wri_cde
064700021022     c                   endif
064800021021      *               *-----------------------------------*
064900020703     c*
065000020703      *  controllo se devo memorizzare tot.colli
065100020703     C     WMEMCO        IFEQ      'S'
065200020703     c* check se dati numerici
065300020703     C                   move      *zeros        WTOCOP            5 0
065400020703     C     Digits        Check     f1hTQU:1
065500020703     C                   If        Not %Found
065600020703     C                   move      f1hTQU        WTOCOP
065700020703     c                   end
065800021022      *
065900020703     C                   END
066000021022      *
066100020627     C                   MOVEL     f2dsty        CD§STS
066200020627     C                   MOVEL     f2dsta        CD§COD
066300020627      *
066400021021      * ----------------------->
066500020627      *  Se EVENTO da non ignorare elaboro
066600020627     C     §CECAR        IFEQ      *BLANKS
066700020627      *
066800020627      *  Imposto evento consegna
066900020627     C     §CECEV        IFNE      'CON'
067000020627     C     f2dsdt        ANDNE     CDEDAG
067100020627     C     §CECEV        ORNE      'AVV'
067200020627     C     §CECEV        ANDNE     'CON'
067300020627     C                   MOVE      §CECEV        CD§CMC
067400020627     C                   END
067500020627      *
067600020627      *  Spedizione consegnata totalmente
067700020627     C     §CETPE        IFEQ      'C'
067800020627     C     CDEDCM        IFLT      f2dsdt
067900021021     c                   move      'S'           wri_cde
068000020627     C                   MOVE      f2dsdt        CD§DCM
068100020627     C                   MOVE      f2dstm        CD§HMC
068200020627     C                   END
068300020627     C                   ELSE
068400020627      *  RISERVA
068500020627     C     §CETPE        IFEQ      'R'
068600021021     c                   move      'S'           wri_cde
068700020627     C                   MOVE      f2dsdt        CD§DRC
068800020627     C                   MOVE      f2dstm        CD§HMC
068900020627     C                   ELSE
069000020627      *  Spedizione consegnata parzialmente
069100020627     C     §CETPE        IFEQ      'P'
069200021021     c                   move      'S'           wri_cde
069300020627     C                   MOVE      f2dsdt        CD§DCM
069400020627     C                   MOVE      f2dstm        CD§HMC
069500020627     C                   ELSE
069600020627      *  Spedizione l.avviso
069700020627     C     §CETPE        IFEQ      'A'
069800021021     c                   move      'S'           wri_cde
069900020627     C                   MOVE      f2dsdt        CD§DLA
070000020627     C                   MOVE      f2dstm        CD§HMC
070100020627     C                   ELSE
070200020627      *
070300020627      *  Spedizione giacente
070400021021     c                   move      'S'           wri_cde
070500020627     C                   MOVE      f2dsdt        CD§DAG
070600020627     C                   MOVE      f2dstm        CD§HMC
070700020627     C                   END                                                    Eve.l.avv.
070800020627     C                   END                                                    Eve.parzi.
070900020627     C                   END                                                    RISERVA
071000020627     C                   END                                                    Eve.conse.
071100020627      *
071200020627      * Evento da ignorare
071300020627     C                   ELSE
071400020627      *
071500020715      * Se previsto gestisco l'evento
071600020627     C     §CECAR        IFEQ      'E'
071700020715     C     §CECEV        ANDNE     *blank
071800020627     C                   EXSR      WRTEVE
071900020627     C                   END
072000020715      *
072100020627     C                   CLEAR                   EDIDSCE
072200021022     C                   MOVEL     *BLANKS       Carica_eve
072300020627     C                   END                                                    ignora evento
072400020627      *
072500020627     C                   End
072600020627      *
072700020627     C                   END
072800021021      *
072900021029      *
073000020627     C                   Endsr
073100020627      *-------------------------------------------------------------*
073200020627      *? WRTEVE - SCRIVO EVENTO DI MESSA IN CONSEGNA
073300020627      *----------------------------------------------------------------
073400020627     C     WRTEVE        BEGSR
073500020627      *
073600020626      *  Se la spedizione risulta consegnata stampo segnalazione d'errore
073700020626     C     ARBDCM        IFGT      *ZEROS
073800020626     C                   MOVEL     ERR(3)        DESERR
073900020626     C                   MOVEL     *BLANKS       WVALOR
074000021022     C                   MOVEL     F1hanr        WVALOR
074100020626     C                   EXSR      STPERR
074200020626     C                   ELSE
074300020626      *
074400020715     C                   clear                   FNEVB000
074500020626     C                   Z-ADD     DATA8G        EVBDTV
074600020626     C                   Z-ADD     WORA          EVBORV
074700020920      *
074800020920      * P.O.Utente e Codice Utente e non più la LNA
074900021114     C                   Z-ADD     O36pou        EVBFLE
075000021114     C                   Z-ADD     O36cou        EVBCDU
075100020920      *
075200020626     C                   Z-ADD     ARBAAS        EVBAAS
075300020626     C                   Z-ADD     ARBLNP        EVBLNP
075400020626     C                   Z-ADD     ARBNRS        EVBNRS
075500020626     C                   Z-ADD     ARBNSP        EVBNSP
075600020626      *
075700020626      * Se esiste mamma con LNA uguale scrivo l'evento sulla mamma
075800020626     C     *IN38         IFEQ      '0'
075900020626     C     ARBLNA        ANDEQ     MICLNA
076000020626     C                   Z-ADD     MICAAS        EVBAAS
076100020626     C                   Z-ADD     MICLNP        EVBLNP
076200020626     C                   Z-ADD     MICNRS        EVBNRS
076300020626     C                   Z-ADD     MICNSP        EVBNSP
076400020626     C                   END
076500020626      *
076600020626     C                   MOVE      f2dsdt        EVBDEV
076700020626     C                   Z-ADD     f2dstm        EVBHEV
076800020715     C                   MOVEL     §cecev        EVBCEV
076900020626      *
077000020626     C                   WRITE     FNEVB000
077100021022      *
077200021022     C   OA              EXCEPT    TEST1
077300021022     C                   EXCEPT    EVENTW
077400020626      *
077500020626     C                   END
077600020626      *
077700020626     C                   ENDSR
077800020625     C*----------------------------------------------------------------
077900020625     C*? STPERR: STAMPA ERRORE
078000020625     C*----------------------------------------------------------------
078100020625     C     STPERR        BEGSR
078200020625     C*
078300021114     c     wscnsts       ifeq      *blank
078400021028     C*
078500020625     C   of              EXCEPT    TESTA
078600020625     C                   EXCEPT    DETER
078700020628     C*
078800021114     c                   else
078900021114     C*
079000021114     C*  mette a parte gli scan/status non codificati
079100120305     C*     ossia concordati fra Fedex e BRT
079200120305     C*  o meglio: non gestiti da BRT
079300021114     C   ob              EXCEPT    TESTAF
079400021114     C                   EXCEPT    DETERF
079500021114     C*
079600021114     c                   end
079700021114     C*
079800021114     c                   MOVE      'E'           ERRORI
079900021114     C*
080000020628     C                   ENDSR
080100020715      *----------------------------------------------------------------
080200020715      *? Scrive data e ora di arrivo su ARS
080300020715      *----------------------------------------------------------------
080400020715     C     Upd_FnArs     BEGSR
080500021031      *
080600021031     c                   clear                   Karsnot
080700021031     c                   movel     F1HANR        Karsnot
080800021031      *
080900021031      *  imposta il campo Note
081000051017     c     Karsnot       setll     FIArs10L
081100051017     c     Karsnot       reade     FIArs10L
081200051017     c                   dow       not %eof(FIArs10L)
081300021031     c                   if        ArsTrc ='F'
081400021031     c                   leave
081500021031     c                   end
081600051017     c     Karsnot       reade     FIArs10L
081700021031     c                   enddo
081800021031      *
081900021031      *  se trovato il record  lo aggiorna
082000051017     c                   if        not %EoF(FIArs10L)
082100021029      *
082200021029      * aggiornas solo se non era già stata impostata
082300021029     c                   if        arsdat = 0
082400020718     c                   move      f2dsdt        arsdat
082500021029     c                   else
082600021029     C                   MOVEL     ERR(5)        DESERR
082700021029     C                   MOVEL     *BLANKS       WVALOR
082800021029     C                   MOVEL     F1hanr        WVALOR
082900021029     C                   EXSR      STPERR
083000021029     c                   end
083100021029      *
083200051017     c                   update    fIars000
083300020718     c                   end
083400020715     C*
083500020715     C                   ENDSR
083600020715      *----------------------------------------------------------------
083700020715      *? Scrive ad ogni bolla il FNCDE e/o FNCDS
083800020715      *----------------------------------------------------------------
083900020715     C     Write_CDE     BEGSR
084000020715      *
084100020628      *-----------------------------------------------------
084200020628      *     YY = 0 evento totale in elaborazione
084300020628      *     YY > 0 evento parziale in elaborazione
084400021022      * ----------------------------------------------------------
084500020715      *  fisso evento totale e non parziale
084600020715      *  ????? occorre definirlo in seguito se parziale e come.....?????
084700021022      * ----------------------------------------------------------
084800021031     C*****              Z-ADD     0             YY                3 0
084900021022      * ----------------------------------------------------------
085000020715      *
085100020628      * Verifico se segnalare se non ho consegnato tutti i colli
085200020628     C                   EXSR      MEMCOL
085300020628      *
085400020708     C     esiste_CDE    IFEQ      'NO'                                         -- 1
085500020628      *
085600021022      * ----------------------------------------------------------
085700021022      * -->> al momento scrive solo evento totale e mai parziale
085800021022      * ----------------------------------------------------------
085900021031     C***  YY            IFEQ      *ZEROS                                       -- 2
086000021031     c                   if        multicollo ='no'
086100020628     C                   EXSR      WRTOLD                                       come prima
086200020708     C                   ELSE                                                   -- 2 --
086300020628     C                   EXSR      WRCDE0                                       scrivo cde a 0
086400020628     C                   EXSR      WRTCDS                                       scrivo cds
086500020708     C                   END                                                       2 --
086600020628      *
086700020708     C                   ELSE                                                   -- 1 --
086800020628      *
086900020708     C     esiste_CDS    IFEQ      'NO'                                         -- 2
087000020628      *
087100021022      * ----------------------------------------------------------
087200021022      * -->> al momento scrive solo evento totale e mai parziale
087300021022      * ----------------------------------------------------------
087400021031     C**** YY            IFEQ      *ZEROS                                       -- 3
087500021031     c                   if        multicollo ='no'
087600020628     C                   EXSR      WRTOLD                                       come prima
087700020708     C                   ELSE                                                   -- 3 --
087800020628     C                   EXSR      UPCDE0                                       upd cde 0 + wrt cds
087900020628     C                   EXSR      WRTCDS                                       scrivo cds
088000020708     C                   END                                                       3 --
088100020628      *
088200020708     C                   ELSE                                                   -- 2 --
088300020628     C                   EXSR      WRTCDS                                       wrt cds 0 o > 0
088400020708     C                   END                                                       2 --
088500020628      *
088600020708     C                   END                                                       1 --
088700020628      *  Azzero variabili
088800021031     C*******            Z-ADD     0             YY                3 0
088900020628      *
089000020628     C                   ENDSR
089100020627      *----------------------------------------------------------------
089200020715      *? MEMCOL - Segnalazione di consegna parziale e
089300020715     C*?        - Calcolo totale colli da consegnare rispetto a tutti i colli
089400020627      *----------------------------------------------------------------
089500020627     C     MEMCOL        BEGSR
089600020627      *
089700020627     C     WMEMCO        IFEQ      'S'
089800020627      *
089900020715      * Loop su FNART per verificare quanti colli sono da consegnare
090000020715     C                   Z-ADD     0             WTOCOC            5 0
090100020718     C     KART          CHAIN(N)  FNART01L                           36
090200020715     C     *IN36         DOWEQ     '0'
090300020715     C     ARTDCM        IFEQ      0
090400020715     C                   ADD       1             WTOCOC
090500020715     C                   END
090600020718     C     KART          READE(N)  FNART01L                               36
090700020715     C                   END
090800020627      *
090900020627      * Scrivo NOTE
091000020703     C                   MOVEL     ERR(4)        ERRCOL
091100020627     C                   Z-ADD     WTOCOC        COLSPE
091200020627     C                   Z-ADD     WTOCOP        COLCON
091300020627     C                   MOVEL     ERRCOL        CD§NO1
091400020703      *
091500020627     C                   END
091600020627      *
091700020627     C                   ENDSR
091800020628      *----------------------------------------------------------------
091900020628      *? WRTOLD - Scrittura dati FNCDE metodo standard
092000020628      *----------------------------------------------------------------
092100020628     C     WRTOLD        BEGSR
092200020628      *
092300020628      * Imposto date per confronto
092400020628     C     CD§DCM        IFEQ      *ZEROS                                       consegna
092500020628     C                   Z-ADD     CDEDCM        CD§DCM
092600020628     C                   END
092700021022      *
092800020628     C     CD§DLA        IFEQ      *ZEROS                                       lasciato avviso
092900020628     C                   Z-ADD     CDEDLA        CD§DLA
093000020628     C                   END
093100021022      *
093200020628     C     CD§DAG        IFEQ      *ZEROS                                       giacenza
093300020628     C                   Z-ADD     CDEDAG        CD§DAG
093400020628     C                   END
093500021022      *
093600020628     C     CD§DRC        IFEQ      *ZEROS                                       riserva
093700020628     C                   Z-ADD     CDEDRC        CD§DRC
093800020628     C                   END
093900020628      *
094000020628      * Se ho impostato data consegna + altra data abblenco l'altra
094100020628     C     CD§DCM        IFGT      CD§DAG
094200020628     C                   Z-ADD     0             CD§DAG
094300021022      *
094400020628     C     CD§CMC        IFNE      'MAN'
094500020628     C     CD§CMC        ANDNE     'P  '
094600020628     C     CD§CMC        ANDNE     'RES'
094700020628     C     CD§CMC        ANDNE     'IDD'
094800020628     C     CD§CMC        ANDNE     'RIT'
094900020628     C                   MOVEL     *BLANKS       CD§CMC
095000020628     C                   END
095100021022      *
095200020628     C                   END
095300020628      *
095400020628     C     CD§DCM        IFGT      CD§DLA
095500020628     C                   Z-ADD     0             CD§DLA
095600021022      *
095700020628     C     CD§CMC        IFEQ      'AVV'
095800020628     C     CD§CMC        OREQ      'AV7'
095900020628     C                   MOVEL     *BLANKS       CD§CMC
096000020628     C                   END
096100021022      *
096200020628     C                   END
096300020628      *
096400020628      * Aggiorno FNCDE
096500021031     c     KsavCde       chain     Fncde02l
096600020628     C                   MOVEL     WTESLB        CDEFTX
096700021021     C                   Z-ADD     CD§DCM        CDEDCM
096800021021     C                   Z-ADD     CD§HMC        CDEHMC
096900021021     C                   Z-ADD     CD§DLA        CDEDLA
097000021021     C                   Z-ADD     CD§DAG        CDEDAG
097100021021     C                   Z-ADD     CD§DRC        CDEDRC
097200021021     C                   MOVEL     CD§NO1        CDENO1
097300021021     C                   MOVEL     CD§NO2        CDENO2
097400021021     C                   MOVEL     CD§CMC        CDECMC
097500021021     C                   MOVEL     CD§CCA        CDECCA
097600021021     C                   MOVEL     CD§ERR        CDEERR
097700021021     C                   MOVEL     CD§STS        CDESTS
097800021021     C                   MOVEL     CD§COD        CDECOD
097900021021     C                   MOVEL     CD§MOT        CDEMOT
098000021021     C                   MOVEL     CD§FL1        CDEFL1
098100021021     C                   MOVEL     CD§FL2        CDEFL2
098200021031      *
098300021031     C                   IF        not %found(Fncde02l)
098400021031     c                   z-add     svcdeaas      cdeaas
098500021031     c                   z-add     svcdelnp      cdelnp
098600021031     c                   z-add     svcdenrs      cdenrs
098700021031     c                   z-add     svcdensp      cdensp
098800020628     C                   MOVEL     ARBRMN        CDERMN
098900020628     C                   Z-ADD     ARBLNA        CDELNA
099000020628     C                   WRITE     FNCDE000
099100020628     C                   ELSE
099200020628     C                   UPDATE    FNCDE000
099300020628     C                   END
099400020628      *
099500020628     C                   ENDSR
099600020628      *----------------------------------------------------------------
099700020628      *? WRCDE0 - Scrittura dati FNCDE, solo dati base
099800020628      *----------------------------------------------------------------
099900020628     C     WRCDE0        BEGSR
100000020628      *
100100020628     C                   MOVEL     CD§FL1        CDEFL1
100200020628     C                   MOVEL     CD§FL2        CDEFL2
100300020628     C                   Z-ADD     ARBLNA        CDELNA
100400020628     C                   MOVEL     ARBRMN        CDERMN
100500020628     C                   MOVEL     WTESLB        CDEFTX
100600020628     C                   WRITE     FNCDE000
100700020628      *
100800020628     C                   ENDSR
100900020628      *----------------------------------------------------------------
101000020628      *? WRTCDS - Scrivo dati FNCDS
101100020628      *----------------------------------------------------------------
101200020628     C     WRTCDS        BEGSR
101300020628      *
101400020628     C                   Z-ADD     CDEAAS        CDSAAS
101500020628     C                   Z-ADD     CDELNP        CDSLNP
101600020628     C                   Z-ADD     CDENRS        CDSNRS
101700020628     C                   Z-ADD     CDENSP        CDSNSP
101800020628     C                   Z-ADD     CDELNA        CDSLNA
101900020628     C                   Z-ADD     CDERMN        CDSRMN
102000020628     C                   Z-ADD     CD§DCM        CDSDCM
102100020628     C                   Z-ADD     CD§HMC        CDSHMC
102200020628     C                   Z-ADD     CD§DLA        CDSDLA
102300020628     C                   Z-ADD     CD§DAG        CDSDAG
102400020628     C                   Z-ADD     CD§DRC        CDSDRC
102500020628     C                   MOVEL     CD§NO1        CDSNO1
102600020628     C                   MOVEL     CD§NO2        CDSNO2
102700020628     C                   MOVEL     CD§CMC        CDSCMC
102800020628     C                   MOVEL     CD§CCA        CDSCCA
102900020628     C                   MOVEL     CD§ERR        CDSERR
103000020628     C                   MOVEL     CD§STS        CDSSTS
103100020628     C                   MOVEL     CD§COD        CDSCOD
103200020628     C                   MOVEL     CD§MOT        CDSMOT
103300020628     C                   MOVEL     CD§FL1        CDSFL1
103400020628     C                   MOVEL     CD§FL2        CDSFL2
103500020628      *
103600021031      * imposta il segnacollo
103700021031     C                   MOVEL     f1hanr        CDSSEE
103800020628     C                   WRITE     FNCDS000
103900020628      *
104000020628     C                   ENDSR
104100020628      *----------------------------------------------------------------
104200020628      *? UPCDE0 - Trasformo CDE esistente in CDS e azzero CDE
104300020628      *----------------------------------------------------------------
104400020628     C     UPCDE0        BEGSR
104500020628      *
104600020628     C                   Z-ADD     CDEAAS        CDSAAS
104700020628     C                   Z-ADD     CDELNP        CDSLNP
104800020628     C                   Z-ADD     CDENRS        CDSNRS
104900020628     C                   Z-ADD     CDENSP        CDSNSP
105000020628     C                   Z-ADD     CDELNA        CDSLNA
105100020628     C                   Z-ADD     CDERMN        CDSRMN
105200020628     C                   Z-ADD     CDEDCM        CDSDCM
105300020628     C                   Z-ADD     CDEHMC        CDSHMC
105400020628     C                   Z-ADD     CDEDLA        CDSDLA
105500020628     C                   Z-ADD     CDEDAG        CDSDAG
105600020628     C                   Z-ADD     CDEDRC        CDSDRC
105700020628     C                   MOVEL     CDENO1        CDSNO1
105800020628     C                   MOVEL     CDENO2        CDSNO2
105900020628     C                   MOVEL     CDECMC        CDSCMC
106000020628     C                   MOVEL     CDECCA        CDSCCA
106100020628     C                   MOVEL     CDEERR        CDSERR
106200020628     C                   MOVEL     CDESTS        CDSSTS
106300020628     C                   MOVEL     CDECOD        CDSCOD
106400020628     C                   MOVEL     CDEMOT        CDSMOT
106500020628     C                   MOVEL     CDEFL1        CDSFL1
106600020628     C                   MOVEL     CDEFL2        CDSFL2
106700021031     C                   MOVEL     f1hanr        CDSSEE
106800020628      *
106900020628     C                   WRITE     FNCDS000
107000020628      *
107100021031      * Aggiorno FNCDE
107200021031     c     KsavCde       chain     Fncde02l
107300021031      *
107400020628     C                   Z-ADD     *ZEROS        CDEDCM
107500020628     C                   Z-ADD     *ZEROS        CDEHMC
107600020628     C                   Z-ADD     *ZEROS        CDEDLA
107700020628     C                   Z-ADD     *ZEROS        CDEDAG
107800020628     C                   Z-ADD     *ZEROS        CDEDRC
107900020628     C                   MOVEL     *BLANKS       CDENO1
108000020628     C                   MOVEL     *BLANKS       CDENO2
108100020628     C                   MOVEL     *BLANKS       CDECMC
108200020628     C                   MOVEL     *BLANKS       CDECCA
108300020628     C                   MOVEL     *BLANKS       CDEERR
108400020628     C                   MOVEL     *BLANKS       CDESTS
108500020628     C                   MOVEL     *BLANKS       CDECOD
108600020628     C                   MOVEL     *BLANKS       CDEMOT
108700020628     C                   MOVEL     *BLANKS       CDEFL1
108800020628     C                   MOVEL     *BLANKS       CDEFL2
108900020628      *
109000020628     C                   MOVEL     CD§FL1        CDEFL1
109100020628     C                   MOVEL     CD§FL2        CDEFL2
109200020628      *
109300021031     C                   IF        %found(Fncde02l)
109400020628     C                   UPDATE    FNCDE000
109500021031     c                   end
109600020628      *
109700020628     C                   ENDSR
109800021022      *----------------------------------------------------------------
109900021022      *  DS Particolari da gestire
110000021022      *----------------------------------------------------------------
110100021022     C     SubScan_DS    Begsr
110200021022      *
110300021022      *   se ci sono altri dati di dettaglio da decodificare come
110400021022      *    ulteriori informazioni deve considerare il tipo di Scan
110500021022      *     a cui fanno riferimento.
110600021022      *
110700021022     c                   if        F2DEXI <> *blanks
110800021022      * testata x ovrflw
110900021022     C   OA              EXCEPT    TEST1
111000021022      *
111100021022     c                   select
111200021025      *
111300021025      * ????? SESTO ULTERIANO
111400021025     c                   when      F2DSTY = '01'
111500021022      *
111600021022      * proof of delivery (Data consegna su ARB e ART)
111700021022     c                   when      F2DSTY = '20'
111800021022     c                   movel     F2DEXI        DSd20
111900021022     C                   EXCEPT    SCAN20
112000021022      *
112100021022      * pickup/pickup exception   (Ritiro scrive ARS le date di arrivo)
112200021022     c                   when      F2DSTY = '08' or F2DSTY = '29'
112300021022     c                   movel     F2DEXI        DSd29
112400021022     C                   EXCEPT    SCAN08
112500021022      * rev scan
112600021022     c                   when      F2DSTY = '25'
112700021022     c                   movel     F2DEXI        DSd25
112800021022     C                   EXCEPT    SCAN25
112900021022      * delivery exception
113000021022     c                   when      F2DSTY = '30'
113100021022     c                   movel     F2DEXI        DSd30
113200021022     c                   movel     f30ade        f30ore            2
113300021022     c                   move      f30ade        f30min            2
113400021022     C                   EXCEPT    SCAN30
113500021022      * cer
113600021022     c                   when      F2DSTY = '92'
113700021022     c                   movel     F2DEXI        DSd92
113800021022     C                   EXCEPT    SCAN92
113900021022      * mps
114000021022     c                   when      F2DSTY = '93'
114100021022     c                   movel     F2DEXI        DSd93
114200021022     C                   EXCEPT    SCAN93
114300021022      * rex-qty
114400021022     c                   when      F2DSTY = '32'
114500021022     c                   exsr      d32det
114600021022      *
114700021022     c                   endsl
114800021022      *
114900021022     c                   endif
115000021022      *
115100021022     C                   Endsr
115200021022      *----------------------------------------------------------------
115300021022      *  Dettaglio tipo scan 32
115400021022      *----------------------------------------------------------------
115500021022     C     d32det        Begsr
115600021022      *
115700021022     c                   movel     F2DEXI        REX_id            2
115800021022      *
115900021022     c                   select
116000021022      *
116100021022      *  REX-QTY weight or pieces change
116200021022     c                   when      REX_id = '01'
116300021022     c                   movel     F2DEXI        DSd321
116400021022     C                   EXCEPT    SCN321
116500121016     c                   eval      sav_F3AWTC = F3AWTC
116600021022      *
116700021022      *  REX-PMT Payment Method Change
116800021022     c                   when      REX_id = '02'
116900021022     c                   movel     F2DEXI        DSd322
117000021022     C                   EXCEPT    SCN322
117100021022      *
117200021022      *  REX-SVC Service / Handling Change
117300021022     c                   when      REX_id = '03'
117400021022     c                   movel     F2DEXI        DSd323
117500021022     C                   EXCEPT    SCN323
117600021022      *
117700021022      *  REX-CHG Change Information
117800021022     c                   when      REX_id = '05'
117900021022     c                   movel     F2DEXI        DSd325
118000021022     C                   EXCEPT    SCN325
118100021022      *
118200021022      *  REX-DT  Date Information
118300021022     c                   when      REX_id = '06'
118400021022     c                   movel     F2DEXI        DSd326
118500021022     C                   EXCEPT    SCN326
118600021022      *
118700021022      *  REX-DIM Dimension Information
118800021022     c                   when      REX_id = '07'
118900021022     c                   movel     F2DEXI        DSd327
119000021022     C                   EXCEPT    SCN327
119100021022      *
119200021022      *  REX-CHG Chargeable Code Change
119300021022     c                   when      REX_id = '08'
119400021022     c                   movel     F2DEXI        DSd328
119500021022     C                   EXCEPT    SCN328
119600021022      *
119700021022      *  REX-ADD Address Change
119800021022     c                   when      REX_id = '04' or
119900021022     c                             REX_id = '09' or
120000021022     c                             REX_id = '10' or
120100021022     c                             REX_id = '11' or
120200021022     c                             REX_id = '12'
120300021022     c                   movel     F2DEXI        DSd324
120400021022     C                   EXCEPT    SCN324
120500021022      *
120600021022      *  REX-WGT Dimension Weight
120700021022     c                   when      REX_id = '14'
120800030218     c                   movel     F2DEXI        DSd3214
120900021022     C                   EXCEPT    SCN329
121000030609     c                   move      *blank        Vol_Changed       1
121100030513      * ATTENZIONE:
121200030513      *  Il cambio di peso può avvenire solo se durante la decodifica della
121300030513      *   testata si trattava di AWB Master e se si era agganciato FNARB
121400030513      *    altrimenti non esegue nulla.
121500030513     c                   if        Chg_Wgt  <> 'S'  and
121600030513     c                             Master_ok = 'ok' and
121700030513     c                             Bolla_ok  = 'ok'
121800121016      * U.Mis.impostata precedentemente
121900121016     c                   if        F3WUTC = ' ' and sav_F3AWTC <> ' '
122000121016     c                   eval      F3WUTC = sav_F3AWTC
122100121016     c                   end
122200080702      *
122300080702      *  e solo se l'unità di misura è fra quelle previste:
122400080702     c                   if        F3WUTC = 'P' or                               *
122500080702     c                             F3WUTC = 'L' or                               *
122600121017     c                             F3WUTC = ' ' or                               *
122700080702     c                             F3WUTC = 'K'                                  *
122800030218     C                   move      'S'           chg_wgt
122900030218     c                   exsr      Cambio_Peso
123000080702     c                   end
123100080702      *
123200030218     c                   end
123300030609      *
123400030609      * Se non è stato variato il Volume
123500030609     c                   If        Vol_Changed = *blank
123600030609     C                   EXCEPT    NoChgVol
123700030609     c                   end
123800021022      *
123900021022     C                   endsl
124000021022      *
124100021022     C                   Endsr
124200020625     C*----------------------------------------------------------------
124300030218     C*? Modificato il Peso della spedizione
124400020625     C*----------------------------------------------------------------
124500030218     C     Cambio_Peso   BEGSR
124600030218      *
124700030218      * Poichè Fedex manda il dettaglio seguendo un ordinamento decrescente
124800030218      *  rispetto alle date di rilevamento modifiche, se c'è una variazione a qualcosa,
124900030218      *  prendo sempre la prima fra tutte le righe con stesso SCAN/STATUS in quanto ha
125000030218      *  sicuramente la data più recente quindi è stata rilevata per ultima.
125100030218      *
125200030513      * Se dati in Pounds li deve trasformare in KG. --> coefficiente 0,453597
125300030513      *  ossia 1 kg. equivale a 2,2046 Libbre quindi
125400030513      *                            1
125500030513      *   1 Pound equivale a  ----------- = 0,453597 Kg.
125600030513      *                          2,2046
125700030513      *--------------------------------------------------------------------------*
125800030513      * Peso Dimensionale --> Da Volume                                          *
125900030513     c                   z-add     0             Peso_inKG                       *
126000121009      ***                                                                        *
126100121009      *  Chiarimento ricevuto da FEDEX in settembre 2012 :                       *
126200121009      *    qualora il valore del peso da volume sia superiore a quello passato   *
126300121009      *    come peso reale. Viene omesso il peso e passato esclusivamente il     *
126400121009      *    Volume. Bisogna quindi impostarlo stando attenti all'unità di misura  *
126500121009      *    anglosassone. (x individuare comunque la variazione della spedizione) *
126600121016      ***---------                                                               *
126700121009     c                   IF        F3WDIW  > 0                                   *
126800121016      ***---------                                                               *
126900030513      * muove a destra in quanto la stringa ricevuta è lunga 7 ma ha 1 decimale  *
127000030513     c                   move      F3WDIW        Peso_inKG         7 1           *
127100030513      *                                                                          *
127200030609      * Se dati in Pounds deve applicare il rapporto quindi dividere x 2,2046    *
127300080702      *   FEDEX invia "L" Libbra                                                 *
127400080702     c                   if        F3WUTC = 'P' or                               *
127500121017     c                             F3WUTC = ' ' or                               *
127600080702     c                             F3WUTC = 'L'                                  *
127700030609     c                   div(h)    2,2046        Peso_inKG                       *
127800030513     C                   End                                                     *
127900030513      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
128000030513      *  Regole:                                                                :
128100030513      * calcolo il volume dal Peso dimensionale utilizzando la formula:         :
128200030513      *  con il lato misurato in cm.                                            :
128300030513      *                Volume in cm.cubi     --> (lato x lato x lato)           :
128400030513      * Peso in Kg. = --------------------                                      :
128500030513      *                      6000                                               :
128600030513      *  quindi la formula diventa considerando anche il rapporto fra           :
128700030513      *   cm.cubi e m.cubi:                                                     :
128800030513      * Volume in m.cubi = (Peso in Kg. x 0,006)                                :
128900101215      ***
129000101215      * dal 2011 il divisore non sarà più 6000 ma 5000 da notizia di FEDEX
129100101215      ***
129200101215     c                   if        *date < 20110101
129300030604     c     Peso_inKG     mult(h)   0,006         Vol_inMC          5 3          :
129400101215     c                   else
129500101215     c     Peso_inKG     mult(h)   0,005         Vol_inMC          5 3          :
129600101215     c                   end
129700121016      ***---------                                                               *
129800121010     c                   ELSE
129900121016      ***---------                                                               *
130000121010      ***  se il PESO fosse stato impostato a (0) occorre basarsi sul Volume     *
130100121016      * Se espresso in sistema Anglosassone occorre ricordare che                *
130200121016      *  1 pollice = 2,54 cm.                                                    *
130300121016      *  in tal caso ogni volume deve essere espresso x 2,54 al cubo = 16,387064 *
130400121016      ***                                                                        *
130500121016      *** se in sistema anglosassone                                             *
130600121016     c                   if        F3WUTC <>'K'                                  *
130700121016     c     F3WDIV        mult      16,387064     campo_15_3       15 3          :
130800121029     c     campo_15_3    div       10000000      Vol_inMC          5 3          :
130900121016      ***                                                                        *
131000121016     C                   Else                                                    *
131100121016      ***                                                                        *
131200121029     c     F3WDIV        div       10000000      Vol_inMC                       :
131300121016     C                   End                                                     *
131400121016      ***---------                                                               *
131500121010     c                   END
131600121016      ***---------                                                               *
131700030527      *                                                                         :
131800030513      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
131900121009      *    qui ci deve arrivare con il campo VOL_inMC carico                    :
132000121009      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
132100121009      *
132200121009      *  Trovato il Peso Dimensionale Fedex in KG., occorre compararlo con quello
132300121009      *   da CML o Bollettato da BRT e se quello dato da Fedex è maggiore
132400121009      *    effettuare la variazione.
132500121009     c                   Exsr      Vol_Pes
132600121009      *
132700030604      *  Se il Peso dichiarato da Fedex è diverso   eseguo rettifica.           +
132800030604      *    e prima ricavo il volume arrotondandolo                              +
132900030619     c                   if        Vol_inMC <> Wvol and                         +
133000030619     c                             arbNCL = F3WSPQ or                           +
133100030619     c                             Vol_inMC > Wvol                              +
133200030605      *                                                                         +
133300030605      * Ricavo il volume unitario per i colli tenendo a parte il resto della    +
133400030605      *  divisione che verrà riportata sul primo collo letto su FNART           +
133500030605     c     Vol_inMC      div       arbNCL        Vol_MC_uni        5 3          +
133600030605     c                   mvr                     Vol_MC_res        3 0          +
133700030604      *                                                                         +
133800030604      *      >>  stampa la segnalazione di cambio peso  <<                      +
133900030604     C                   Except    ChgPeso                                      +
134000030609     c                   move      'S'           Vol_Changed                    +
134100030611     c                   add       1             NrBolChg                       +
134200120305     c                   add       Wvol          VolBrt                         +
134300030611     c                   add       Vol_inMC      VolFed                         +
134400120305     c     VolFed        sub       VolBrt        VolDif                         +
134500030604      *                                                                         +
134600030604      *  ESEGUE RETTIFICA: dividendo equamente il volume totale per i colli     +
134700030604      *                    della spedizione impostando sul primo il resto della +
134800030604      *                    divisione.                                           +
134900030605      * i campi passati sono:                                                   +
135000030605      *    la spedizione e il volume totale da variare                          +
135100030609     c                   z-add     ARBaas        aas                            +
135200030609     c                   z-add     ARBlnp        lnp                            +
135300030609     c                   z-add     ARBnrs        nrs                            +
135400030609     c                   z-add     ARBnsp        nsp                            +
135500030609     c                   z-add     Vol_inMC      volMCt                         +
135600030609     c                   movel     Param         KPJBU                          +
135700030609     c                   call      'TRTCF2R1'                                   +
135800030609     c                   parm                    KPJBA                          +
135900030604      *                                                                         +
136000030604     C                   End                                                    +
136100030604      *                                                                         +
136200030604      * :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
136300030218      *
136400030218     C                   Endsr
136500030513     C*----------------------------------------------------------------
136600030513     C*  Reperisce Peso/Volume o da CML o da Bollettato
136700030513     C*----------------------------------------------------------------
136800030513     C     VOL_PES       BEGSR
136900030513     C*
137000030513     C* La routine è identica al TRTC83R21 attenzione
137100030513     C*
137200030513     c     *like         define    arbvlb        wvol
137300030513     c     *like         define    arbpkb        wpes
137400030527     C     *LIKE         DEFINE    §qttpc        WTARA
137500030513      *
137600030513      * Prima cerca il volume fra bollettato e CML
137700050112     C     arbncR        ifeq      arbncl
137800030513      * se è considerata un'unica spedizione
137900030513     C                   z-add     ARBVLC        WVOL
138000030513     C                   else
138100030513     C*
138200030513     C     ARBVLC        ifge      ARBVLb
138300030513     C                   z-add     ARBVLC        WVOL
138400030513     C                   else
138500030513     C                   z-add     ARBVLb        WVOL
138600030513     C                   end
138700030513     C*
138800030513     C                   endif
138900030527     C*
139000030527     C* PESO VDL - TARA
139100030527     C     §QTTPC        MULT      arbncp        WTARA
139200030527     C                   Z-ADD     arbPKC        wpes
139300030527     c                   sub(h)    wtara         wpes
139400030513      *
139500030513      * Poi cerca il peso fra bollettato e CML
139600050112     C     arbncP        ifeq      arbncl
139700030513      * se è considerata un'unica spedizione
139800030513     C                   ELSE
139900030513     C*
140000030527     C     wpes          iflt      arbPKb
140100030513     C                   Z-ADD     arbPKb        wpes
140200030513     C                   end
140300030513     C*
140400030513     C                   ENDIF
140500030513     C*
140600030513     C                   ENDSR
140700030218     C*----------------------------------------------------------------
140800030218     C*? *INZSR - OPERAZIONI INIZIALI
140900030218     C*----------------------------------------------------------------
141000030218     C     *INZSR        BEGSR
141100030218      *
141200020625     C     *ENTRY        PLIST
141300020625     C                   PARM                    KPJBA
141400020625      *
141500020625      * Definisco chiavi di accesso (Klist)
141600020711     C     Kar4          KLIST
141700020711     C                   KFLD                    Ktrc              1
141800020711     C                   KFLD                    Kn14             14
141900020627      *
142000080128     C**** Kart_ar4      KLIST
142100080128     C****               KFLD                    ar4aas
142200080128     C****               KFLD                    ar4lnp
142300080128     C****               KFLD                    ar4nrs
142400080128     C****               KFLD                    ar4nsp
142500021031      *
142600080128     C     Kart_W        KLIST
142700080128     C                   KFLD                    wAAS
142800080128     C                   KFLD                    wLNP
142900080128     C                   KFLD                    wNRS
143000080128     C                   KFLD                    wNSP
143100080128      *
143200021031     C     Kart          KLIST
143300021031     C                   KFLD                    KAAP
143400021031     C                   KFLD                    KLPP
143500021031     C                   KFLD                    KNRP
143600021031     C                   KFLD                    KNSP1
143700020711      *
143800020627     C     Kart27        KLIST
143900020625     C                   KFLD                    KFLS
144000020625     C                   KFLD                    KLNA
144100020625     C                   KFLD                    KNRS
144200020625     C                   KFLD                    KNSC
144300020626      *
144400020711     C     Karb          KLIST
144500020626     C                   KFLD                    kAAS
144600020626     C                   KFLD                    kLNP
144700020626     C                   KFLD                    kNRS
144800020626     C                   KFLD                    kNSP
144900020626      *
145000020711     C     Karb_Mic      KLIST
145100020626     C                   KFLD                    micAAS
145200020626     C                   KFLD                    micLNP
145300020626     C                   KFLD                    micNRS
145400020626     C                   KFLD                    micNSP
145500020718      *
145600020718     C     Karb_Upd      KLIST
145700020718     C                   KFLD                    arbAAS
145800020718     C                   KFLD                    arbLNP
145900020718     C                   KFLD                    arbNRS
146000020718     C                   KFLD                    arbNSP
146100020625      *
146200020626     C     KCDE          KLIST
146300020626     C                   KFLD                    KNDC
146400020626     C                   KFLD                    KAAS
146500020626     C                   KFLD                    KLNP
146600020626     C                   KFLD                    KNRS
146700020626     C                   KFLD                    KNSP
146800020703     C                   Z-ADD     0             KNDC
146900021031      *
147000021031     C     KsavCDE       KLIST
147100021031     C                   KFLD                    SvCdeNDC
147200021031     C                   KFLD                    SvCdeAAS
147300021031     C                   KFLD                    SvCdeLNP
147400021031     C                   KFLD                    SvCdeNRS
147500021031     C                   KFLD                    SvCdeNSP
147600020718      *
147700020718     C     KARS          KLIST
147800020718     C                   KFLD                    ARBFLS
147900020718     C                   KFLD                    ARBLNA
148000020718     C                   KFLD                    ARBNRS
148100020718     C                   KFLD                    ARBNCD
148200020718     C                   KFLD                    ktrcs             1
148300021029     C                   move      'F'           ktrcs
148400021028      *
148500021028     C     KTBE          KLIST
148600021028     C                   KFLD                    TBECOD
148700021028     C                   KFLD                    TBEKE1
148800021028     C                   KFLD                    TBEKE2
148900030527      *
149000030527     C     KTAB1         KLIST
149100030527     C                   KFLD                    KKUT
149200030527     C                   KFLD                    KCOD
149300030527     C                   KFLD                    KKEY
149400020626      *
149500020625      * Definizione campi di work   (Define)
149600020626     C     *LIKE         DEFINE    LBLAAP        KAAP
149700020626     C     *LIKE         DEFINE    LBLLPP        KLPP
149800020626     C     *LIKE         DEFINE    LBLNRO        KNRP
149900020626     C     *LIKE         DEFINE    LBLNSP        KNSP1
150000020625     C     *LIKE         DEFINE    ARTNSC        KNSC
150100020625     C     *LIKE         DEFINE    ARTLNA        KLNA
150200020625     C     *LIKE         DEFINE    ARTFLS        KFLS
150300020626     C     *LIKE         DEFINE    CDENDC        KNDC
150400020626     C     *LIKE         DEFINE    CDEAAS        KAAS
150500020626     C     *LIKE         DEFINE    CDELNP        KLNP
150600020626     C     *LIKE         DEFINE    CDENRS        KNRS
150700020626     C     *LIKE         DEFINE    CDENSP        KNSP
150800020626     C     *LIKE         DEFINE    CDESTS        WSTS
150900020626     C     *LIKE         DEFINE    CDECOD        WCODE
151000020626     C     *LIKE         DEFINE    ARBLNA        MICLNA
151100020626     C     *LIKE         DEFINE    CDEAAS        MICAAS
151200020626     C     *LIKE         DEFINE    CDELNP        MICLNP
151300020626     C     *LIKE         DEFINE    CDENRS        MICNRS
151400020626     C     *LIKE         DEFINE    CDENSP        MICNSP
151500020625     c     *like         define    arsnot        Karsnot
151600030527     C     *LIKE         DEFINE    TBLKUT        KKUT
151700030527     C     *LIKE         DEFINE    TBLCOD        KCOD
151800030527     C     *LIKE         DEFINE    TBLKEY        KKEY
151900020703      *
152000020703     C     *LIKE         DEFINE    CDEDCM        CD§DCM
152100020703     C     *LIKE         DEFINE    CDEHMC        CD§HMC
152200020703     C     *LIKE         DEFINE    CDEDLA        CD§DLA
152300020703     C     *LIKE         DEFINE    CDEDAG        CD§DAG
152400020703     C     *LIKE         DEFINE    CDEDRC        CD§DRC
152500020703     C     *LIKE         DEFINE    CDENO1        CD§NO1
152600020703     C     *LIKE         DEFINE    CDENO2        CD§NO2
152700020703     C     *LIKE         DEFINE    CDECMC        CD§CMC
152800020703     C     *LIKE         DEFINE    CDECCA        CD§CCA
152900020703     C     *LIKE         DEFINE    CDEERR        CD§ERR
153000020703     C     *LIKE         DEFINE    CDESTS        CD§STS
153100020703     C     *LIKE         DEFINE    CDECOD        CD§COD
153200020703     C     *LIKE         DEFINE    CDEMOT        CD§MOT
153300020703     C     *LIKE         DEFINE    CDEFL1        CD§FL1
153400020703     C     *LIKE         DEFINE    CDEFL2        CD§FL2
153500020626      *
153600020625      * Recupera dati dell'utente
153700020625     C                   Z-ADD     1             CODUT             1 0
153800020625     C                   CALL      'XPARUT'
153900020625     C                   PARM                    UT§DSE0F
154000020625     C                   MOVEL     RAGUT         RSUT             20
154100020920
154200020920      * carico tutti i terminal
154300020920      * con 2 schiere SKPOT e SKPO
154400020920     c                   clear                   trul26ds
154500020920     c                   eval      i26tpt = 'P'
154600020920     c                   eval      i26tla = 'L'
154700020920     c                   call      'TRUL26R'
154800020920     c                   parm                    trul26ds
154900020625      *
155000021009      *  In base all'utente prende i P.O. di EDPCEDxxx
155100021009      *   da riportare sugli eventi
155200021114     C                   CLEAR                   Tibs36Ds
155300021114     c                   EVAL      I36ute = Knmus
155400021114     c                   EVAL      I36Tla = 'L'
155500021114     C                   CALL      'TIBS36R'
155600021114     C                   PARM                    Tibs36Ds
155700020625     C* Imposto variabili
155800020625      *
155900020625     C* RECUPERO DATA E ORA
156000020625     C                   TIME                    WHHDAT           14 0
156100020625     C                   TIME                    WHHD12           12 0
156200020625     C                   MOVE      WHHDAT        DATA8             8 0
156300020625     C                   MOVE      WHHD12        DATA6             6 0
156400020625     C                   MOVEL     DATA6         DATA              6 0
156500020625     C                   TIME                    WORA              6 0
156600020625      * Giro la data
156700020625     C                   CLEAR                   WLBDA8
156800020625     C                   Z-ADD     DATA8         G02DAT
156900020625     C                   MOVEL     *BLANKS       G02ERR
157000020625     C                   CALL      'XSRDA8'
157100020625     C                   PARM                    WLBDA8
157200020625     C                   Z-ADD     G02INV        DATA8G            8 0
157300020625      *
157400020625     C* Tab codici eventi
157500020625     C                   Z-ADD     0             X                 3 0
157600020625     C                   MOVEL     'CE'          TABCOD
157700020625     C     TABCOD        CHAIN     EDTAB01L                           30
157800020625     C     *IN30         DOWEQ     '0'
157900020625      * carica solo gli eventi Fedex che cominciano con una (F)
158000020625     C                   if        %subst(tabkey:1:1)='F' and tabflg=*blank
158100020625     C                   ADD       1             X
158200020625     C                   MOVEL     TABKEY        CEV(X)
158300020625     C                   MOVEL     TABUNI        DEV(X)
158400020625     c                   end
158500020625     C     TABCOD        READE     EDTAB01L                               30
158600020625     C                   END
158700030527     C*
158800030527     C** CARICAMENTO DATI VARI TASSAZIONE
158900030527     C                   Z-ADD     1             KKUT
159000030527     C                   MOVEL     'QT'          kcod
159100030527     C                   MOVEL     *BLANKS       kKEY
159200030527     C                   MOVEL     '1'           kKEY
159300030527     C     KTAB1         CHAIN     TABEL00F                           30
159400030527     C  N30              MOVEL     TBLUNI        DSQT1
159500020625     C*
159600021018     C* Se non ha caricato nulla dalla tabella Eventi deve uscire
159700021018     C*  dal programma forzatamente.
159800021018     c                   if        x = 0
159900021018     c                   exsr      EXIT
160000021018     c                   endif
160100021018     C*
160200020625     C                   MOVEL     *ALL'-'       CMP132          132
160300020625     C                   SETON                                        OA
160400021114     C                   SETON                                        OB
160500020625     C                   SETON                                        OF
160600020625      *------------
160700020625      *  Definisco variabili
160800020625      *------------
160900020625      * Inizializzo variabili totale
161000020625      *------------
161100020625      * Imposto data del giorno
161200020625     C                   TIME                    W0140            14 0
161300020625     C                   MOVE      W0140         G02DAT            8 0
161400020625     C                   MOVEL     *BLANK        G02ERR
161500020625     C                   CALL      'XSRDA8'
161600020625     C                   PARM                    WLBDA8
161700020625     C                   MOVEL     G02INV        WOGGI             8 0
161800020625      *
161900021028      * Pulisce la schiera dei riferimenti non trovati
162000021028     C                   clear                   AWB
162100021028      *
162200021028      * Schiera luoghi di presa in carico merce
162300021028     C                   clear                   SC01
162400021028     C*
162500021028     C* Punti di presa in carico
162600021028     C*  merce
162700021028     C                   movel     'FED'         TBECOD
162800021028     C                   eval      TBEKE1='SCAN.01        '
162900021028     C                   movel     *blanks       TBEKE2
163000021028     c                   clear                   SCAN01DS
163100021028     C     KTBE          chain     TNTBE01L
163200021028     c                   if        %found(TNTBE01L)
163300021028     c                   movel     TBEuni        SCAN01DS
163400021028     c                   end
163500030611      *
163600030611      *   azzera totali generali
163700030611     c                   z-add     0             NrBolChg          5 0
163800120305     c                   z-add     0             VolBrt            7 3
163900030611     c                   z-add     0             VolFed            7 3
164000030611     c                   z-add     0             VolDif            7 3
164100030611      *
164200020625     C                   ENDSR
164300020625      *****************************************************************
164400021018     C     Exit          Begsr
164500021018      *
164600021018     C                   MOVE      ERRORI        KPJBU
164700021018     c                   seton                                        LR
164800021018     c                   Return
164900021018      *
165000021018     C                   ENDSR
165100021018      *****************************************************************
165200021114     OFEDEXPRT  E            TESTAF           01
165300021114     O                       RSUT                20
165400021114     O                                           46 '** STAMPA Scan/Status Fe'
165500120305     O                                              'dex non gestiti da BRT**'
165600021114     O                       DATA               121 '  /  /  '
165700021114     O                                          127 'Pag.:'
165800021114     O                       PAGE          Z    132
165900021114     O          E            TESTAF         2 02
166000021114     O                       WORA               121 '  :  :  '
166100021114     O                                          132 'TRTCF2R'
166200021114     O*------------------------------------
166300021114     O          E            DETERF         1
166400021114     O                       DESERR              78
166500021114     O                       WVALOR             113
166600021114     O                       WANNO              120
166700021114     O                       WLINPA             123
166800021114     O                       WSERIE             125
166900021114     O                       WSPEDI             132
167000021114     O*----------------------------------------------------------------
167100020625     OQSYSPRT   E            TESTA            01
167200020625     O                       RSUT                20
167300020625     O                                           46 '** STAMPA ERRORI RICEZIO'
167400020625     O                                              'NE DATE CONSEGNA DA FED'
167500020625     O                                              'EX **'
167600020625     O                       DATA               121 '  /  /  '
167700020625     O                                          127 'Pag.:'
167800020625     O                       PAGE          Z    132
167900021028     O          E            TESTA          2 02
168000020625     O                       WORA               121 '  :  :  '
168100021028     O                                          132 'TRTCF2R'
168200020625     O*------------------------------------
168300020625     O          E            DETER          1
168400020625     O                       DESERR              78
168500020625     O                       WVALOR             113
168600021029     O                       WANNO              120
168700021029     O                       WLINPA             123
168800021029     O                       WSERIE             125
168900021029     O                       WSPEDI             132
169000020625     O*----------------------------------------------------------------
169100020625     OSYSPRT    E            TEST1            01
169200020625     O                       RSUT                20
169300020625     O                                           45 '** STAMPA DATI RICEVUTI'
169400020625     O                                         +  1 'IN FASE DI TRASCODIFICA'
169500020625     O                                              ' DATE CONSEGNA DA FEDEX'
169600020625     O                                              ' **'
169700020625     O                       DATA               121 '  /  /  '
169800020625     O                                          127 'Pag.:'
169900020625     O                       PAGE1         Z    132
170000020625     O          E            TEST1            02
170100020625     O                       WORA               121 '  :  :  '
170200021028     O                                          132 'TRTCF2R'
170300020625     O*  Dati messaggio
170400021022     O          E            DATMSG      2
170500021018     O                       CMP132             132
170600021022     O          E            DATMSG      1
170700021018     O                                            8 'RIF.AWB.'
170800021018     O                       F1HANR            +  1
170900021022     O                                         + 10 'CUST.REF.NR'
171000021018     O                       F1HREF            +  1
171100021022     o                                         +  1 ' '
171200120305     O              N77                        +  9 'BOLLA BRT:'
171300021022     O              N77      ARBAAS        z   +  1
171400021022     O              N77      ARBLNP        z   +  1
171500021022     O              N77      ARBNRS        z   +  1
171600021022     O              N77      ARBNSP        z   +  1
171700021022     O               77                        +  1 'Bolla non Trovata'
171800021022     O          E    77      DATMSG      0
171900021022     O                                            8 'RIF.AWB.'
172000021022     O                       F1HANR            +  1
172100021022     O                                         + 10 'CUST.REF.NR'
172200021022     O                       F1HREF            +  1
172300021022     o                                         +  1 ' '
172400021028     O                                         +  9 '                '
172500021028     O                                         +  1 '    '
172600021028     O                                         +  1 '   '
172700021028     O                                         +  1 '  '
172800021028     O                                         +  1 '  -->  '
172900021028     O                                         +  1 'Bolla non Trovata'
173000021028     O          E   N77      DATMSG      0
173100021028     O                                            8 '        '
173200021028     O                       F1HANR            +  1
173300021028     O                                         + 10 '           '
173400021028     O                       F1HREF            +  1
173500021028     o                                         +  1 ' '
173600021028     O                                         +  9 '                '
173700021028     O                       ARBAAS        z   +  1
173800021028     O                       ARBLNP        z   +  1
173900021028     O                       ARBNRS        z   +  1
174000021028     O                       ARBNSP        z   +  1
174100021018      *
174200021022     O          E            DATMSG      1
174300021022     O                                           20 'DATI MSG: DEST.'
174400021018     O                       W1HRNA            +  1
174500021018     O                       W1HRCO            +  1
174600021018      *
174700021022     O          E            DATMSG      1  1
174800021022     O                                           20 'ADDRESS: DEST.'
174900021018     O                       W1HRAD            +  1
175000021018     O                       W1HRCI            +  1
175100021018     O                       F1HRST            +  1
175200021018     O                       F1HRZC            +  1
175300021018     O                       F1HRCN            +  1
175400021018      *
175500020625     O*  Spedizione
175600021022     O          E            DecTabCE    1  1
175700021022     O                 N44                       10 'Scan'
175800021022     O                 N44   F2DSTY              14
175900021022     O                  44                       10 'Status'
176000021022     O                  44   F2DSTA              14
176100021022     o                                           20 ' '
176200021022     O                                         +  1 '('
176300021022     O                       WC2A              +  0
176400021022     O                                         +  0 ')'
176500021022     O                       WDEC              +  1
176600021025     O                                         +  0 ' - '
176700021025     O                       WC2X              +  0
176800021022      *-
176900021022     O          E            DETric      1
177000021022     O                                           11 'Scan Type'
177100021022     O                       F2DSTY            +  1
177200021022     O                                         +  1 'Status Cod.'
177300021022     O                       F2DSTA            +  1
177400021022     O                                         +  1 'Date'
177500021022     O                       F2DSDT            +  1
177600021022     O                                         +  1 'Time'
177700021022     O                       F2DSTM            +  1
177800021022     O                                         +  1 'City'
177900021022     O                       F2DSCI            +  1
178000021022     O                                         +  1 'zip code'
178100021022     O                       F2DSZC            +  1
178200021022     O                                         +  1 'State'
178300021022     O                       F2DSST            +  1
178400021022     O*  Evento
178500021022     O          E            EVENTW      1
178600021022     O                                           16 'Immesso EVENTO'
178700021022     O                                         +  1 'x Sped.Nr.:'
178800021022     O                       evbaas        Z   +  1
178900021022     O                       evblnp        Z   +  1
179000021022     O                       evbnrs        Z   +  1
179100021022     O                       evbnsp        Z   +  1
179200021022     O                                         +  1 'in data'
179300021022     O                       evbdev            +  1
179400021022     O                       evbhev            +  1
179500021022     O                                         +  1 'cod.eve.'
179600021022     O                       evbcev            +  1
179700021022     O                                         +  1 'filiale'
179800021022     O                       evbfle            +  1
179900021022     O                                         +  1 'immesso'
180000021022     O                       evbdtv            +  1
180100021022     O                       evborv            +  1
180200021028      *-
180300021028     O          E            SCAN01      1  1
180400021028     O                                           12 '  Scan Tp 01'
180500020708      *-
180600021022     O          E            SCAN20      1
180700021022     O                                           12 '  Scan Tp 20'
180800020708     O                                         +  1 'prova di consegna'
180900021022     O          E            SCAN20      1  1
181000020708     O                                           18 'Delivery signature'
181100020708     O                       F20DSI            +  1
181200020708     O                                         +  1 'Delivery address'
181300020708     O                       F20DAD            +  1
181400020708      *-
181500021022     O          E            SCAN25      1  1
181600021022     O                                           12 '  Scan Tp 25'
181700020708     O                                         +  1 'dati fatturazione'
181800020709     O                                         +  1 'wgt'
181900020708     O                       F25RWE            +  1
182000020709     O                                         +  1 'wgt UOM'
182100020708     O                       F25RWU            +  1
182200020709     O                                         +  1 'n.pkg'
182300020708     O                       F25RNP            +  1
182400020709     O                                         +  1 'cd flg'
182500020708     O                       F25RCF            +  1
182600020708     O                                         +  1 'shipper name'
182700020708     O                       F25RSN            +  1
182800020709     O                                         +  1 'dang goods'
182900020708     O                       F25RDG            +  1
183000020708     O                                         +  1 'recp name'
183100020708     O                       F25RRN            +  1
183200020708      *-
183300021022     O          E            SCAN30      1  1
183400021022     O                                           12 '  Scan Tp 30'
183500020708     O                                         +  1 'tentativo di consegna'
183600020708     O                                         +  1 'delle ore'
183700020710     O                       F30ORE            +  1
183800020710     O                                         +  0 ':'
183900020710     O                       F30MIN            +  0
184000020708      *-
184100021022     O          E            SCAN08      1  1
184200021022     O                                           14 '  Scan Tp 8/29'
184300020708     O                                         +  1 'ritiro /'
184400020708     O                                         +  1 'problemi di ritiro'
184500020708     O                                         +  1 'Fedex courier dispatch'
184600020708     O                                         +  1 'nr'
184700020708     O                       F29DNR            +  1
184800020708      *-
184900021022     O          E            SCAN92      1  1
185000021022     O                                           12 '  Scan Tp 92'
185100020708     O                                         +  1 'danno generico'
185200020708     O                                         +  1 'CER Date'
185300020708     O                       F92CDA            +  1
185400020708     O                                         +  1 'CER Seq'
185500020708     O                       F92CSE            +  1
185600020708      *-
185700021022     O          E            SCAN93      1  1
185800021022     O                                           12 '  Scan Tp 93'
185900020708     O                                         +  1 'multicollo'
186000020708     O                                         +  1 'MPS Master'
186100020708     O                       F93MPS            +  1
186200020708      *-
186300021022     O          E            SCN321      1  1
186400021022     O                                           17 '  Scan Tp 32 (01)'
186500020708     O                                         +  1 'modifiche qtà'
186600020708     O                                         +  1 'weight type cd'
186700020708     O                       F3AWTC            +  1
186800020708     O                                         +  1 'pkg wgt'
186900020708     O                       F3APWE            +  1
187000020708     O                                         +  1 'pkg nbr'
187100020708     O                       F3APNR            +  1
187200020708     O                                         +  1 'ovsz pkg flg'
187300020708     O                       F3AOPF            +  1
187400020708      *-
187500021022     O          E            SCN322      1  1
187600021022     O                                           17 '  Scan Tp 32 (02)'
187700020708     O                                         +  1 'modifiche modalità di'
187800020708     O                                         +  1 'pagamento'
187900020708     O                                         +  1 'pmt cd'
188000020708     O                       F3BPMC            +  1
188100020708     O                                         +  1 'curr cd'
188200020708     O                       F3BCUR            +  1
188300020708     O                                         +  1 'pmt amt'
188400020708     O                       F3BPMA            +  1
188500020709     O                                         +  1 'cust.acc.'
188600020708     O                       F3BCAN            +  1
188700020709     O                                         +  1 'duty tax'
188800020708     O                       F3BDTF            +  1
188900020709     O                                         +  1 'freight chg'
189000020708     O                       F3BFCF            +  1
189100020708      *-
189200021022     O          E            SCN323      1
189300021022     O                                           17 '  Scan Tp 32 (03)'
189400020708     O                                         +  1 'modifiche di servizio'
189500020708     O                                         +  1 'di gestione'
189600020709     O                                         +  1 'Type svc cd'
189700020708     O                       F3CTSC            +  1
189800020708     O                                         +  1 'del Type cd'
189900020708     O                       F3CDTC            +  1
190000020708     O                                         +  1 'pkg wgt'
190100020709     O                       F3CPKW        Z   +  1
190200020709      *
190300021022     O          E            SCN323      1  1
190400020709     O                                           17 '          32 (03)'
190500020708     O                                         +  1 'spcl hndlg cd1'
190600020708     O                       F3CSH1            +  1
190700020708     O                                         +  1 'spcl hndlg cd2'
190800020708     O                       F3CSH2            +  1
190900020708     O                                         +  1 'spcl hndlg cd3'
191000020708     O                       F3CSH3            +  1
191100020708     O                                         +  1 'spcl hndlg cd4'
191200020708     O                       F3CSH4            +  1
191300020708     O                                         +  1 'chrg grp'
191400020708     O                       F3CCHG            +  1
191500020708     O                                         +  1 'wgt uom cd'
191600020708     O                       F3CWUC            +  1
191700020708      *-
191800020708     O          E            SCN324      1
191900021022     O                                           17 '  Scan Tp 32 (04)'
192000020708     O                                         +  1 'modifiche destinatario'
192100020708     O                                         +  1 'address desc'
192200020708     O                       F3DADD            +  1
192300020708     O                                         +  1 'postal cd'
192400020708     O                       F3DPOC            +  1
192500020708     O                                         +  1 'country cd'
192600020708     O                       F3DCOC            +  1
192700021022     O          E            SCN324      1  1
192800020709     O                                           17 '          32 (04)'
192900020708     O                                         +  1 'chrg grp'
193000020708     O                       F3DCHG            +  1
193100020708     O                                         +  1 'rt id'
193200020708     O                       F3DRTI            +  1
193300020708     O                                         +  1 'state cd'
193400020708     O                       F3DSTC            +  1
193500020708     O                                         +  1 'city nm'
193600020708     O                       F3DCIN            +  1
193700020708     O                                         +  1 'phone'
193800020708     O                       F3DPHO            +  1
193900020708      *-
194000020708     O          E            SCN325      1
194100021022     O                                           17 '  Scan Tp 32 (05)'
194200020708     O                                         +  1 'modifiche di riepilogo'
194300020708     O                                         +  1 'wgt cd'
194400020708     O                       F3EWGC            +  1
194500020708     O                                         +  1 'pkg wgt'
194600020708     O                       F3EPKW            +  1
194700020708     O                                         +  1 'pkg nbr'
194800020708     O                       F3EPKN        Z   +  1
194900020708     O                                         +  1 'ovsz pkg flg'
195000020708     O                       F3EOPF            +  1
195100020708     O                                         +  1 'curr cd'
195200020708     O                       F3ECUC            +  1
195300020708     O                                         +  1 'pymt amt'
195400020708     O                       F3EPYA        Z   +  1
195500020709     O          E            SCN325      1
195600020709     O                                           17 '          32 (05)'
195700020708     O                                         +  1 'cust acct'
195800020708     O                       F3ECUA            +  1
195900020708     O                                         +  1 'type svc cd'
196000020708     O                       F3ETSC            +  1
196100020708     O                                         +  1 'del typ cd'
196200020708     O                       F3EDTC            +  1
196300020708     O                                         +  1 'spcl hndlg cd1'
196400020708     O                       F3ESH1            +  1
196500020708     O                                         +  1 'spcl hndlg cd2'
196600020708     O                       F3ESH2            +  1
196700020708     O                                         +  1 'spcl hndlg cd3'
196800020708     O                       F3ESH3            +  1
196900020709     O          E            SCN325      1
197000020709     O                                           17 '          32 (05)'
197100020708     O                                         +  1 'spcl hndlg cd4'
197200020708     O                       F3ESH4            +  1
197300020709     O                                         +  1 'addr des'
197400020708     O                       F3EADD            +  1
197500020708     O                                         +  1 'post cd'
197600020708     O                       F3EPOC            +  1
197700020709     O                                         +  1 'cntry cd'
197800020708     O                       F3ECNC            +  1
197900020708     O                                         +  1 'chrg grp'
198000020708     O                       F3ECHG            +  1
198100021022     O          E            SCN325      1  1
198200020709     O                                           17 '          32 (05)'
198300020708     O                                         +  1 'rt id'
198400020708     O                       F3ERTI            +  1
198500020708      *-
198600021022     O          E            SCN326      1  1
198700021022     O                                           17 '  Scan Tp 32 (06)'
198800020708     O                                         +  1 'modifiche di riepilogo'
198900020708     O                                         +  1 'orgnl shp dt'
199000020708     O                       F3FOSD            +  1
199100020708     O                                         +  1 'invoice nbr'
199200020708     O                       F3FINV        Z   +  1
199300020708     O                                         +  1 'cust acct'
199400020708     O                       F3FCUA            +  1
199500020708      *-
199600021022     O          E            SCN327      1  1
199700021022     O                                           17 '  Scan Tp 32 (07)'
199800020708     O                                         +  1 'modifiche dimensionali'
199900020708     O                                         +  1 'wgt type cd'
200000020708     O                       F3GWTC            +  1
200100020708     O                                         +  1 'pkg wgt'
200200020708     O                       F3GPKW        Z   +  1
200300020708     O                                         +  1 'dim wgt'
200400020708     O                       F3GDMW        Z   +  1
200500020708     O                                         +  1 'dim vol'
200600020708     O                       F3GDMV        Z   +  1
200700020708     O                                         +  1 'pkg nbr'
200800020708     O                       F3GPKN        Z   +  1
200900020708     O                                         +  1 'dim lth'
201000020708     O                       F3GDIL            +  1
201100020709     O          E            SCN327      1
201200020709     O                                           17 '          32 (07)'
201300020708     O                                         +  1 'dim wth'
201400020708     O                       F3GDIW            +  1
201500020708     O                                         +  1 'dim ht'
201600020708     O                       F3GDIH            +  1
201700020708      *-
201800021022     O          E            SCN328      1  1
201900021022     O                                           17 '  Scan Tp 32 (08)'
202000020708     O                                         +  1 'modifiche cod.caricamento'
202100020708     O                                         +  1 'charge cd grp'
202200020708     O                       F3HCCG            +  1
202300020708     O                                         +  1 'payment type cd'
202400020708     O                       F3HPTC            +  1
202500020708     O                                         +  1 'curr cd'
202600020708     O                       F3HCUC            +  1
202700020708     O                                         +  1 'cust acct'
202800020708     O                       F3HCAN            +  1
202900020709     O          E            SCN328      1
203000020709     O                                           17 '          32 (08)'
203100020708     O                                         +  1 'recept nm'
203200020708     O                       F3HREN            +  1
203300020708      *-
203400021022     O          E            SCN329      1
203500021022     O                                           17 '  Scan Tp 32 (14)'
203600020708     O                                         +  1 'modifiche dimens.pesi'
203700020708     O                                         +  1 'UOM type cd'
203800020708     O                       F3WUTC            +  1
203900020708     O                                         +  1 'dim length qty'
204000020708     O                       F3WDLQ        Z   +  1
204100020708     O                                         +  1 'dim width qty'
204200020708     O                       F3WDWQ        Z   +  1
204300020708     O                                         +  1 'dim height qty'
204400020708     O                       F3WDHQ        Z   +  1
204500020708     O                                         +  1 'ship piece qty'
204600020708     O                       F3WSPQ        Z   +  1
204700021022     O          E            SCN329      1  1
204800020709     O                                           17 '          32 (14)'
204900020708     O                                         +  1 'ship wgt'
205000020708     O                       F3WSHW        Z   +  1
205100020708     O                                         +  1 'ship vol qty'
205200020708     O                       F3WSVQ        Z   +  1
205300020708     O                                         +  1 'dim vol'
205400020708     O                       F3WDIV        Z   +  1
205500020708     O                                         +  1 'dim wgt'
205600020708     O                       F3WDIW        Z   +  1
205700030514     O          E            ChgPeso     1  1
205800030514     O                                           17 'Peso/Volume Calc.'
205900030514     O                                         +  1 'Peso Kg.'
206000030515     o                       Peso_inKG     2   +  1
206100120305     O                                         +  1 'Pes. BRT'
206200030515     O                       WPes          2   +  1
206300030604     O                                         +  1 'Nr. Colli'
206400030604     O                       ARBNCL        Z   +  1
206500030604     O                                         +  1 'Vol. Mc.'
206600030604     O                       Vol_inMC      2   +  1
206700120305     O                                         +  1 'Vol. BRT'
206800030515     O                       WVol          2   +  1
206900030604     O                                         +  1 'Vol.Uni.'
207000030604     O                       Vol_MC_uni    2   +  1
207100030604     O                                         +  1 'Vol.Res.'
207200030604     O                       Vol_MC_res    2   +  1
207300030609     O          E            NoChgVol    1
207400030609     O                                           17 ' -->  No Changed '
207500030609     O          E            NoChgVol    0  1
207600030609     O                                           17 ' -->  No Changed '
207700030611     O          E            ChgVolTot   2
207800030611     O                                           17 ' Nr.Bolle Changed'
207900030611     O                       NrBolChg      2   +  1
208000030611     O                                         +  1 '(Vol.Fed.'
208100030611     O                       VolFed        2   +  1
208200120305     O                                         +  1 '- Vol. BRT'
208300120305     O                       VolBrt        2   +  1
208400030611     O                                         +  1 ') = Diffe.'
208500030611     O                       VolDif        2   +  1
208600020708      *---------------------------------------------------------- -----
208700020624**
208800120305Non trovata bolla BRT con questo riferimento AWB               ->
208900020626ATTENZIONE - Codice Evento non previsto in tabella "CE"        ->
209000021029Ricevuto evento MIC messa in consegna per sped. già consegnata ->
209100020703Colli xxxxx DI xxxxx
209200021029Data arrivo merce già impostata per la spedizione con AWB      ->
