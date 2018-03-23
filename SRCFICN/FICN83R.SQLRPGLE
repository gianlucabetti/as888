000100090112     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200091111     H* ficn83R *----------------------------------------------------*
000300100107     H*---------*                                                    *
000400091221     H*           MANIPOLAZIONE TARIFFA autista SUPERtestata         *
000500000000     H*--------------------------------------------------------------*
000600091111     Fficn83D   CF   E             WORKSTN
000700091221      **
000800100105     Ffiapd01L  IF   E           K DISK
000900100108     FfiTGT01L  uF a E           K DISK    commit
001000100108     FfiFGT01L  uF   E           K DISK    commit
001100100105     Ffifpt01L  uF   E           K DISK    commit
001200100105     Ffifpd01L  uF   E           K DISK    commit
001300091111      **
001400090112     D* SCHIERA DI COMODO
001500100105     D ERR             S             78    DIM(30) CTDATA PERRCD(1)             messaggi errore
001600100105      *
001700140117      ****-?
001800140117     D**** TRUL72DS      E DS
001900140117     D****  L72                           3s 0 DESCEND
002000140117     D****                                     DIM(60)
002100140117     D****                                     OVERLAY(O72Lia)
002200140117      ****-?Caricamento filiali contabili
002300140117     d TRUL31ds      e ds                  inz
002400140117     d $POg            s              3    inz   dim(250)
002500140117      ****-?
002600950113     D* SCHIERE PER CARICAMENTO L1 E L6
002700950113     D L6              S              3  0 DIM(30)                              FIL GESTITE
002800090109     D parana          S              1    inz('A')                             FIL GESTITE
002900090114     D comtsr          S              1                                         FIL GESTITE
003000091221     D*
003100100105     d datapre         s               d   datfmt(*iso)
003200100105     d datapos         s               d   datfmt(*iso)
003300091223     d dataiso         s               d   datfmt(*iso)
003400091223     d dataeur         s               d   datfmt(*eur)
003500091223     d Data_oggi       s              8  0
003600100105     d Data_gma        s              8  0
003700100107     D*
003800100107     d PRGnewM1        s                   like(PRGnew)
003900100107     d PRGnewP1        s                   like(PRGnew)
004000100224     d  filSAV         s                   like(filOLD)
004100100224     d  pdrsav         s                   like(pdrOLD)
004200100107     d ultimo_progressivo...
004300100107     d                 s                   like(PRGnew)
004400100222     d primo_progressivo...
004500100222     d                 s                   like(PRGnew)
004600091223     D*
004700100105     D* schiere di controllo
004800100105     d Progr           s              3  0 dim(50)
004900100222     d datDDT          s              8s 0 dim(50)
005000100222     d datDST          s              8s 0 dim(50)
005100100222     d dcvDES          s             15A   dim(50)
005200100105     d piumen          s              2    dim(50)
005300100105     D*
005400091221     D da_forzare      S              1    inz(' ')
005500100222     d data8           s              8s 0
005600091222     D*
005700090112     d ficn06ds      e ds
005800021203     D fnlv24ds      E DS
005900000000     D KPJBA         E DS
006000091221     D*
006100090112     D                 DS
006200090112     D  AA                     1      4  0
006300090112     D  MM                     5      6  0
006400090112     D  GG                     7      8  0
006500090112     D  DATA                   1      8  0
006600091111      *
006700940922     D* DS PER TRUL06R - CARICAMENTO £1
006800940922     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006900940922     D  LIN                    1     90  0
007000940922     D                                     DIM(30)                              SKI COMODO
007100931018     D CNCR80        E DS
007200921030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007300091221     D*
007400011109     D WLBDAT          DS
007500011109     D  G02DAT                 1      8  0
007600011109     D  G02INV                 9     16  0
007700011109     D  G02ERR                17     17
007800011109     D  G02TGI                18     22  0
007900091111      *
008000000000     I/SPACE 3
008100931018     C*****************************************************************
008200931018     C* RIEPILOGO INDICATORI
008300931018     C*****************************************************************
008400931018     C* 30    - DI COMODO
008500931018     C* 35    - USATO PER LOKUP SU SCHIERE E TESTN
008600940923     C* 40/52 - INDICATORI PER MESSAGGI DI ERRORE
008700020717     C* 89    - SONO UN 2° liv.
008800931018     C* 90    - SEGNALA LA PRESENZA DI ERRORI
008900090112     C*****************************************************************
009000100127      * Chiave principale da quale tariffa copiare
009100100127     C                   move(p)   simPOU        filNEW
009200100127     C                   clear                   pdrNEW
009300100127     C                   clear                   dpdNEW
009400100127     C                   clear                   prgOLD
009500100127      *
009600091221     c     Inizio        tag
009700091221     C                   EXSR      inzD1
009800931018     C*
009900090112     C                   do        *hival
010000091221     C*
010100091221     C*   ind.gen.errori da spegnere
010200091221      *                *-------------------*
010300091221     C                   EXFMT     cn83D01
010400091221      *                *-------------------*
010500100105     c                   clear                   vidmsg
010600100224     C                   setoff                                       90
010700100105     C                   setoff                                       505152
010800100105     C                   setoff                                       535455
010900100224     C                   setoff                                       565758
011000100224     C                   setoff                                       59
011100931018     C** CMD3 - FINE LAVORO
011200091221     C   KC              goto      FINE_pgm
011300100224      *
011400100224     C*  CONTROLLI
011500100303     C  nKH              EXSR      ctrd1
011600100303      *
011700100224      *  Se cambiato l'Autista  ci sono altre tariffe già presenti
011800100224     C*   emette la finestre di allerta
011900100303      *     oppure
012000100303     C** CMD8 - Cancella progressivi di periodi precedenti
012100100224     c                   if        FILNEW <> FILsav or PDRNEW <> PDRsav
012200100303     c                              or
012300100303     c                             *INKH
012400100303      *
012500100224     c                   if         altriProgr = 'S'
012600100303     c                              or
012700100303     c                             *INKH
012800100224     c                   eval        FILsav = FILnew
012900100224     c                   eval        PDRsav = PDRnew
013000100224     c                   movel(p)  comNEW        autista           7
013100100224     c                   clear                   progres           3
013200100224     c                   clear                   da_data           8
013300100224     c                   clear                   a_data            8
013400100224     c                   call      'FICN83R1'
013500100224     c                   parm                    autista           7
013600100224     c                   parm                    progres           3
013700100224     c                   parm                    da_data           8
013800100224     c                   parm                    a_data            8
013900100224     c                   if        progres <> *blank
014000100303     c                   setoff                                       56
014100100303     c                   clear                   vidmsg
014200100224     c                   move      progres       prgNEW
014300100224     c                   move      da_data       dadata
014400100224     c                   move      a_data        adata
014500100224     c                   end
014600100224     c                   end
014700100224     c                   end
014800100224      *
014900100107      *
015000091221     C*
015100091221     C*  Se la tariffa è già presente avvisa se ci si vuole andare sopra.....
015200091221      *                *-------------------*
015300091221     c  N90
015400091221     cAn KF              if        Da_Forzare = 'S'
015500091222     c                   exsr      win_Forza
015600091221     c                   end
015700091221      *                *-------------------*
015800091221     c  N90
015900091221     cAN KF              leave
016000090112     c                   enddo
016100091221      *
016200091221      * COMUNQUE se c'era già i progressivo in simulazione procede:
016300091221      *  Prima tenta di cancellarlo poi copia comunque copia ed infine
016400091221      * modifica la nuova tariffa in base alle regole di manipolazione.
016500091221     c   kf              do
016600091221     c                   exsr      elimina
016700091221     c                   exsr      copia
016800091221     c                   exsr      manipola
016900100105     c                   commit
017000091221     c                   end
017100100107     c                   goto      inizio
017200091221     C*
017300091221     c     FINE_pgm      tag
017400091222      *
017500931018     C                   SETON                                        LR
017600091222     C**********************************************************************
017700091222     C*--- PULIZIA CAMPI DEL FORMATO ---------------------------------*
017800091222     C**********************************************************************
017900091222     C     InzD1         BEGSR
018000091222      *
018100100127     C                   clear                   prgNEW
018200100107     C                   clear                   prgNEWM1
018300100107     C                   clear                   prgNEWP1
018400091221     C                   clear                   filOLD
018500091221     C                   clear                   pdrOLD
018600100224     C                   clear                   filSAV
018700100224     C                   clear                   pdrsav
018800100222     C                   clear                   prgNEWsav         3 0
018900100127     C                   clear                   conperc
019000100127     C                   clear                   ritperc
019100100127     C                   clear                   gioperc
019200091223     C                   clear                   destar
019300100107     C                   clear                   dadata
019400100127     c                   z-add     31122039      adata
019500100107     C                   clear                   hdadata
019600100107     C                   clear                   hadata
019700100208     c                   move      '-'           consegn
019800100208     c                   move      '-'           ritsegn
019900100208     c                   move      '-'           giosegn
020000091111      *
020100100224      *  si è deciso di annullare al momento tutti i default sopraimpostati
020200100224     c                   clear                   cn83D01
020300100224      *
020400100303     c                   eval      rnmus = knmus
020500100303     c                   eval      rnsif = knsif
020600100303     C                   MOVEL     RAGUT         RSUT             20
020700100303      *
020800931018     C                   ENDSR
020900090112     C**********************************************************************
021000931018     C*--- CONTROLLI FORMATO -----------------------------------------*
021100090112     C**********************************************************************
021200091221     C     CTRd1         BEGSR
021300100105     C*
021400100222     C                   SETOFF                                       901112
021500100127     c                   clear                   percent_con       5 2
021600100127     c                   clear                   percent_rit       5 2
021700100127     c                   clear                   percent_gio       5 2
021800100222     c                   clear                   prg_cambiato      1
021900940922     C*
022000100222     c                   if        prgNEW <> prgNewsav
022100100222     c                   eval      prgNewsav = prgNEW
022200100222     c                   eval       prg_cambiato = 'S'
022300100222     c                   end
022400100222     C*
022500940922     C****  CODICE FILIALE NUOVA  ****
022600950113     C* FILIALE OBBLIGATORIA
022700950113     C     FILNEW        IFEQ      0
022800950113     C                   SETON                                        5190
022900100105     c                   eval      vidmsg = err(1)
023000950113     C                   GOTO      ENDCTR
023100950113     C                   ENDIF
023200950113     C*
023300950113     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
023400950113     C     *IN89         IFEQ      *OFF
023500021216     C     FILNEW        IFNE      SIMpou
023600021216     C     simtpp        andeq     '1'
023700140117     C*****FILNEW        LOOKUP    L72                                    35
023800140117     C                   move      filNEW        Fil03A            3
023900140117     C     Fil03A        LOOKUP    $POg                                   35
024000940922     C  N35              SETON                                        5190
024100100105     c  N35              eval      vidmsg = err(1)
024200940922     C  N35              GOTO      ENDCTR
024300940922     C                   ENDIF
024400931018     C*
024500950113     C                   ELSE
024600020717     C* 89 ON  - SONO UN 2° liv.  QUINDI CONTROLLO LA FILIALE NELLA L6
024700020717     C     filnew        IFNE      simpou
024800950113     C     FILNEW        LOOKUP    L6                                     35
024900950113     C  N35              SETON                                        5190
025000100105     c  N35              eval      vidmsg = err(1)
025100950113     C  N35              GOTO      ENDCTR
025200950113     C                   ENDIF
025300950113     C                   ENDIF
025400091222     C                   Z-ADD     FILnew        FILold
025500950113     C*
025600931018     C****  CODICE PADRONCINO NUOVO  ****
025700931018     C     '?'           SCAN      PDRNEW                                 90
025800931018     C*
025900931018     C     *IN90         IFEQ      *ON
026000021203     c                   clear                   fnlv24ds
026100021203     C                   Z-ADD     FILNEW        d24FIL
026200021203     C                   MOVEL     'R'           d24FLG
026300030107     C                   MOVEL     parana        d24tip
026400021203     C                   MOVEL(p)  fnlv24ds      KPJBU
026500091111     C                   CALL      'FNLV24R'
026600931018     C                   PARM                    KPJBA
026700021203     C                   MOVEL     KPJBU         fnlv24ds
026800950113     C*
026900950113     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
027000021203     C     d24pdr        IFNE      0
027100021203     C                   MOVEL     d24pdr        FILNEW
027200931018     C                   MOVEL     *ZEROS        PDRNEW
027300021203     C                   MOVE      d24pdr        PDRNEW
027400021203     C                   MOVEL     d24rsc        DPDNEW
027500950113     C                   ENDIF
027600950113     C*
027700931018     C                   GOTO      ENDCTR
027800931018     C                   ENDIF
027900931018     C*
028000931018     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
028100931018     C                   TESTN                   PDRNEW               35
028200100105     C  N35              SETON                                        5590
028300100105     c  N35              eval      vidmsg = err(2)
028400931018     C  N35              GOTO      ENDCTR
028500090504     C                   MOVEL     FILNEW        COMNEW            7 0
028600090504     C                   MOVE      PDRNEW        COMNEW
028700091222     C*
028800091222     C                   move(p)   pdrNEW        pdrOLD
028900091222     C                   z-add     filNEW        filOLD
029000091222     C                   MOVE      comNEW        comOLD            7 0
029100931018     C*
029200940922     C* CONTROLLO ESISTENZA DEL PADRONCINO IN ANAGRAFICA
029300021203     C     kapdn         CHAIN     fiapd000                           30
029400940922     C  N30APDATB        IFNE      ' '
029500940922     C                   SETON                                        30
029600940922     C                   ENDIF
029700940922     C* CODICE PADRONCINO ERRATO
029800100105     C   30              SETON                                        5590
029900100105     c   30              eval      vidmsg = err(2)
030000940922     C   30              GOTO      ENDCTR
030100100223     C                   MOVEL     APDRSF        DPDNEW
030200931018     C*
030300100107      ******************  +++++++++++++++++++++  *******************
030400090112     C****  CONTROLLO VALIDITA' DELLA TARIFFA old  ****
030500090112     c                   clear                   ok                1
030600100105     C     KTGTO         setll     fitgt01l
030700090112     c                   do        *hival
030800100108     C     KTGTO         reade(n)  fitgt01l
030900100105     c                   if        %eof(fitgt01l)
031000090112     c                   leave
031100090112     c                   end
031200100105     C                   IF        tgtatb <> ' '
031300090112     c                   iter
031400090112     c                   end
031500100105     C                   IF        tgtprg <> prgold
031600090112     c                   iter
031700090112     c                   end
031800090112     c                   move      '1'           ok
031900091223      * propone la descrizione della tariffa da cui copiare
032000100105     c                   if        DesTar = *blank
032100091223     c                   movel     tgtDCV        DesTar
032200100105     c                   end
032300090112     c                   leave
032400090112     C                   ENDdo
032500091221      **-----**
032600100105     c* progressivo tariffa da cui copiare inesistente
032700090112     c                   if        ok = ' '
032800100105     c                   eval      vidmsg = err(3)
032900090112     c                   seton                                        5090
033000090112     c                   goto      endctr
033100011126     c                   end
033200091221      *
033300100208      ******************  +++++++++++++++++++++  *******************
033400100208      * TARIFFA NUOVA DA CREARE
033500100107      ******************  +++++++++++++++++++++  *******************
033600100208      ** si schierizza i periodi già immessi
033700100208      *
033800100208     C****   CONTROLLO sovrapposizione di PERIODI   ****
033900100208     c                   clear                   Progr
034000100208     c                   clear                   datDDT
034100100208     c                   clear                   datDST
034200100222     c                   clear                   dcvDES
034300100208     c                   clear                   piuMEN
034400100208     c                   clear                   indice            3 0
034500100208     c                   clear                   altriProgr        1
034600100208      *
034700100208     c                   clear                   sovrapposto       1
034800100208     c                   clear                   da_forzare
034900100208     c                   clear                   DDT_limite        8 0
035000100208     c                   clear                   ERRDDT_limite     8 0
035100100208     c                   eval      ultimo_progressivo = 0
035200100222     c                   eval      primo_progressivo = 0
035300100208     c                   clear                   tariffa_nuova     1
035400100208      *
035500100208     C     KTGTn         setll     fitgt01l
035600100208      *
035700100208     c                   do        *hival
035800100208     C     KTGTn         reade(n)  fitgt01l
035900100208      *
036000100208     c                   if        %eof(fitgt01l)
036100100208     c                   leave
036200100208     c                   end
036300100208      *  se annullato
036400100208     C                   IF        tgtatb <> ' '
036500100208     c                   iter
036600100208     c                   end
036700100208      *
036800100208      *  presenti altri progressivi
036900100208     c                   move      'S'           altriProgr        1
037000100208      *
037100100208      * si memorizza tutti i periodi inseriti per ulteriore controllo di contiguità
037200100208     c                   add       1             indice
037300100208     c                   z-add     tgtprg        Progr(indice)
037400100208     c                   z-add     tgtDDT        DatDDT(indice)
037500100208     c                   z-add     tgtDST        DatDST(indice)
037600100222     c                   move      tgtDCV        DcvDES(indice)
037700100208      *
037800100208      *  periodo subito precedente
037900100208     c                   if        tgtprg = prgNEW -1
038000100208     c                   movel     '-1'          piuMEN(indice)
038100100208     c                   elseif    tgtprg = prgNEW
038200100208     c                   movel     '00'          piuMEN(indice)
038300100208     c                   elseif    tgtprg = prgNEW +1
038400100208     c                   movel     '+1'          piuMEN(indice)
038500100208     c                   end
038600100208      *
038700100208      *  memorizza l'ultimo
038800100208     c                   eval      ultimo_progressivo = tgtprg
038900100222     c                   if        primo_progressivo = 0
039000100222     c                   eval      primo_progressivo = tgtprg
039100100222     C                   END
039200100208     c                   z-add     indice        ultimo_ind        3 0
039300100208      *
039400100208     C                   ENDdo
039500100208      *
039600100208      *  se c'erano dei periodi già inseriti sul cliente li ha schierizzati
039700100208      **-----**
039800100208      **
039900100208      **  se non Immesso il progressivo
040000100208     c                   if        prgNEW = 0
040100100224      **
040200100224     c                   seton                                        56  90
040300100224     c                   eval      vidmsg = err(15)
040400100224     c                   goto      endctr
040500100208      **
040600100208     c                   if          Ultimo_progressivo > 0
040700100224      **   NON SI VUOLE PROPORRE un default
040800100224     c*************      eval      prgNEW = ultimo_progressivo + 1
040900100208      **
041000100208     c                   if        dadata = 0
041100100224     c*************      eval      data8 = DatDST(ultimo_ind)
041200100224     c*************      move      Data8         dataiso
041300100224     c*************      adddur    1:*d          dataiso
041400100224     c*************      move      dataiso       dataeur
041500100224     c*************      move      dataeur       dadata
041600100208     c                   end
041700100208     c                   if         adata = 0
041800100224     c*************      z-add     31122039      adata
041900100208     c                   end
042000100208      **
042100100208      **   se non c'erano dei periodi già immessi....
042200100208     c                   else
042300100224     c*************      z-add     PRGold        PRGnew
042400100208     c                   if         adata = 0
042500100224     c*************      z-add     31122039      adata
042600100208     c                   end
042700100208     c                   end
042800100208      **
042900100208     c                   else
043000100208      *
043100100208      **  se immesso un progressivo o è presente nelle schiere
043200100208      *   o deve essere consecutivo
043300100208     c                   z-add     1             indice
043400100208     c     prgNEW        lookup    Progr(indice)                          95
043500100208      *
043600100208     c                   if        *in95
043700100222      *
043800100222      * -----
043900100222     c                   eval      data8 = DatDDT(indice)
044000100222     c                   move      Data8         dataiso
044100100222     c                   move      dataiso       dataeur
044200100222      *
044300100222      *  se si tratta del primo progressivo e la data a video non è (0)
044400100222      *   non si deve sovrapporre la data trovata
044500100222     c                   If        prg_cambiato = 'S'
044600100222     c                   move      dataeur       dadata
044700100222     c                   end
044800100222      * -----
044900100222     c                   eval      data8 = DatDST(indice)
045000100222     c                   move      Data8         dataiso
045100100222     c                   move      dataiso       dataeur
045200100222      *
045300100222      *  se si tratta dell'ultimo progressivo e la data a video non è (0)
045400100222      *   non si deve sovrapporre la data trovata
045500100222     c                   If         prg_cambiato = 'S'
045600100222     c                   move      dataeur       adata
045700100222     c                   end
045800100222      *
045900100222      *  solo se non presente una descrizione sul campo a video
046000100222     c                   if        Destar = *blank
046100100222     c                   eval      DesTar = DcvDES(indice)
046200100222     c                   end
046300100222      * -----
046400100222      * protegge le date di periodi
046500100222     c                   if        prgNEW =  primo_progressivo
046600100222     C                   SETON                                        12
046700100222     c                   elseIf    prgNEW =  ultimo_progressivo
046800100222     C                   SETON                                        11
046900100222     c                   elseIf    prgNEW = ultimo_progressivo and
047000100222     c                             prgNEW =  primo_progressivo
047100100222     C                   SETOFF                                       1112
047200100222     c                   else
047300100222     C                   SETON                                        1112
047400100222     c                   end
047500100222      *
047600100208     c                   movel     'S'           sovrapposto
047700100208     c                   move      'S'           da_forzare
047800100224      *
047900100208     c                   else
048000100224      *
048100100224     c                   if        ultimo_progressivo > 0
048200100224      *  Controlla la sequenza dei progressivi
048300100224     c                   if        prgNEW > ultimo_progressivo + 1
048400100224     c                   seton                                        56  90
048500100224     c                   eval      vidmsg = err(13)
048600100224     c                   eval      vidmsg = %trim(vidmsg) +
048700100224     c                               %editc(ultimo_progressivo+1:'Z')
048800100224     c                   goto      endctr
048900100224     c                   elseif    prgNEW < primo_progressivo
049000100224     c                   seton                                        56  90
049100100224     c                   eval      vidmsg = err(17)
049200100224     c                   eval      vidmsg = %trim(vidmsg) +
049300100224     c                               %editc(primo_progressivo:'Z')
049400100224     c                   goto      endctr
049500100224     c                   end
049600100224     c                   end
049700100224      *
049800100208     c                   eval      tariffa_nuova ='S'
049900100208      *
050000100208      * altrimenti incrementato di 1
050100100208     c                   if          Ultimo_progressivo > 0
050200100208     c                   eval      prgNEW = ultimo_progressivo + 1
050300100208      **
050400100224     c                   if        dadata = 0 and ultimo_ind > 0
050500100222     c                   eval      data8 = DatDST(ultimo_ind)
050600100222     c                   move      Data8         dataiso
050700100208     c                   adddur    1:*d          dataiso
050800100208     c                   move      dataiso       dataeur
050900100208     c                   move      dataeur       dadata
051000100208     c                   end
051100100208      **
051200100208     c                   else
051300100208     c                   z-add     PRGold        PRGnew
051400100208      *
051500100208      *   imposta quella da cui copiare
051600100208     C     KTGTold       chain(n)  fiTGT01l
051700100208     c                   if        %Found(fiTGT01l)
051800100208     c                   if        dadata = 0
051900100208     c                   move      tgtddt        dataiso
052000100208     c                   move      dataiso       dataeur
052100100208     c                   move      dataeur       dadata
052200100208     c                   end
052300100208     c                   if        adata = 0
052400100208     c                   move      tgtdst        dataiso
052500100208     c                   move      dataiso       dataeur
052600100208     c                   move      dataeur       adata
052700100208     c                   end
052800100208     c                   end
052900100208      *
053000100208     c                   end
053100100208      *
053200100208      * lasciata solo data limite
053300100208     c                   if         adata = 0
053400100208     c                   z-add     31122039      adata
053500100208     c                   end
053600100208      *
053700100208     c                   end
053800100208      **
053900100208     c                   end
054000100208      **
054100100208      **--------------**
054200100208      *  problemi con le date di periodo
054300100208      **--------------**
054400100208      * Da Data
054500100208     c                   clear                   hdadata
054600100208$017 C     dadata        IFgt      *zeros
054700100208     c     *eur          test(d)                 dadata                 52
054800100208     c                   if        *in52
054900100208     c     *dmy          test(d)                 dadata                 52
055000100208     c  n52*dmy          move      dadata        dataeur
055100100208     c  n52              move      dataeur       dadata
055200100208     C                   end
055300100208     C                   end
055400100208     c                   if        dadata <> *zeros and *in52
055500100208     c                              or
055600100208     c                             dadata =  *zeros
055700100208     c                   eval      vidmsg = err(4)
055800100208     c                   seton                                        5290
055900100208     c                   goto      endctr
056000100208     C                   ENDIF
056100100208      * gira la data in AMG
056200100208$017 C     dadata        IFgt      *zeros
056300100208     c                   move      dadata        dataeur
056400100208     c                   move      dataeur       dataiso
056500100208     c                   move      dataiso       hdadata
056600100208     C                   end
056700100208      *
056800100208      * Le date non possono superare il limite del 31/12/2039
056900100208     c                   if        hdadata > 20391231
057000100208     c                   seton                                        5290
057100100208     c                   eval      vidmsg = err(8)
057200100208     c                   goto      endctr
057300100208     C                   end
057400100208      *
057500100208      **--------------**
057600100208      *  a Data
057700100208     c                   clear                   hadata
057800100208$017 C     adata         IFgt      *zeros
057900100208     c     *eur          test(d)                 adata                  53
058000100208     c                   if        *in53
058100100208     c     *dmy          test(d)                 adata                  53
058200100208     c  n53*dmy          move      adata         dataeur
058300100208     c  n53              move      dataeur       adata
058400100208     C                   end
058500100208     C                   end
058600100208     c                   if         adata <> *zeros and *in53
058700100208     c                              or
058800100208     c                             adata =  *zeros
058900100208     c                   seton                                        5390
059000100208     c                   eval      vidmsg = err(4)
059100100208     c                   goto      endctr
059200100208     C                   ENDIF
059300100208      * gira la data in AMG
059400100208$017 C     adata         IFgt      *zeros
059500100208     c                   move      adata         dataeur
059600100208     c                   move      dataeur       dataiso
059700100208     c                   move      dataiso       hadata
059800100208     c                   end
059900100208      *
060000100208      * Le date non possono superare il limite del 31/12/2039
060100100208     c                   if        hadata > 20391231
060200100208     c                   seton                                        5390
060300100208     c                   eval      vidmsg = err(8)
060400100208     c                   goto      endctr
060500100208     C                   end
060600100208      *
060700100208      *  date in successione defiscono il periodo
060800100208     c                   if        hdadata > hadata
060900100208     c                   seton                                        525390
061000100208     c                   eval      vidmsg = err(5)
061100100208     c                   goto      endctr
061200100208     C                   end
061300100208      ***
061400100208      ***  controlla sovrapposizioni di date nei periodi
061500100224     c                   if        tariffa_nuova ='S'  and ultimo_ind > 0
061600100224     C                   IF        hdadata <= DatDDT(ultimo_ind)
061700100222     c                   eval      data8 = DatDDT(ultimo_ind)
061800100222     c                   move      Data8         dataiso
061900100208     c                   adddur    1:*d          dataiso
062000100208     c                   move      dataiso       DDT_Limite
062100100208     c                   move      dataiso       dataeur
062200100208     c                   move      dataeur       ERRDDT_limite
062300100208     C                   end
062400100222     C                   end
062500100222      ***
062600100208     c* se il periodo si sovrappone ad un altro precedente
062700100208     c*
062800100208     c                   if            ERRDDT_limite > 0
062900100208     c                   seton                                        525390
063000100208     c                   eval      vidmsg = err(12)
063100100208     c                   eval      vidmsg = %trim(vidmsg) +
063200100208     c                               %editc(ERRDDT_limite:'Y')
063300100208     c                   goto      endctr
063400100208     c                   end
063500100224      ***
063600100224      ******************  +++++++++++++++++++++  *******************
063700100208      ******************  +++++++++++++++++++++  *******************
063800100208      ******************  +++++++++++++++++++++  *******************
063900100105      *
064000100105      **--------------**
064100100127      *  Non si può NON scegliere almeno un Incremento e un decremento
064200100127     c                   if        conPERC = 0 and ritPERC = 0 and gioPERC = 0
064300100105     c                   seton                                        5490
064400100105     c                   eval      vidmsg = err(11)
064500100105     c                   goto      endctr
064600100105     c                   end
064700100105      *
064800100105      * Memorizza algebricamente l'incremento se in più o in meno
064900100127     c                   if        conPERC <> 0
065000100224     c                   if        conSEGN = *blank
065100100224     c                   seton                                        5790
065200100224     c                   eval      vidmsg = err(16)
065300100224     c                   goto      endctr
065400100224     c                   elseif    conSEGN = '+'
065500100127     c                   z-add     conPERC       percent_con
065600100208     c                   elseIf    conSEGN = '-'
065700100208     c                   z-sub     conPERC       percent_con
065800100127     c                   end
065900100208     c                   end
066000100127      *
066100100127     c                   if        ritPERC <> 0
066200100224     c                   if        ritSEGN = *blank
066300100224     c                   seton                                        5890
066400100224     c                   eval      vidmsg = err(16)
066500100224     c                   goto      endctr
066600100224     c                   elseif    ritSEGN = '+'
066700100127     c                   z-add     ritPERC       percent_rit
066800100208     c                   elseIf    ritSEGN = '-'
066900100208     c                   z-sub     ritPERC       percent_rit
067000100127     c                   end
067100100208     c                   end
067200100127      *
067300100127     c                   if        gioPERC <> 0
067400100224     c                   if        gioSEGN = *blank
067500100224     c                   seton                                        5990
067600100224     c                   eval      vidmsg = err(16)
067700100224     c                   goto      endctr
067800100224     c                   elseif    gioSEGN = '+'
067900100127     c                   z-add     gioPERC       percent_gio
068000100208     c                   elseIf    gioSEGN = '-'
068100100208     c                   z-sub     gioPERC       percent_gio
068200100127     c                   end
068300100208     c                   end
068400100127      *
068500091221     C     ENDCTR        ENDSR
068600091221     C*---------------------------------------------------------------*
068700091221     C* se viene FORZATA la sostituzione di un progressivo precedente *
068800091221     C*---------------------------------------------------------------*
068900091221     C     ELIMINA       BEGSR
069000091221      *
069100091221      *  Per sostituire il progressivo precedentemente inserito
069200100127      *  prima lo deve eliminare quindi può successivamente duplicarlo
069300091221      *
069400091221     C/EXEC SQL
069500091221     C+ DELETE FROM FIFGT00F WHERE FGTSML = 'S' and FGTPRG= :prgnew
069600100127     C+    and FGTPDR = :COMNEW
069700091221     C/END-EXEC
069800091221      *
069900091221     C/EXEC SQL
070000091221     C+ DELETE FROM FIFPT00F WHERE FPTSML = 'S' and FPTPRG= :prgnew
070100100127     C+    and FPTPDR = :COMNEW
070200091221     C/END-EXEC
070300091221      *
070400091221     C/EXEC SQL
070500091221     C+ DELETE FROM FIFPD00F WHERE FPDSML = 'S' and FPDPRG= :prgnew
070600100127     C+    and FPDPDR = :COMNEW
070700091221     C/END-EXEC
070800091221      *
070900091221     C/EXEC SQL
071000091221     C+ DELETE FROM FITGT00F WHERE TGTSML = 'S' and TGTPRG= :prgnew
071100091221     C+    and TGTPDR = :COMNEW
071200091221     C/END-EXEC
071300091221      *
071400091221     C                   ENDSR
071500090113     C*---------------------------------------------------------------*
071600091221     C*--- GESTIONE DELLA COPIA della TARIFFA ------------------------*
071700090112     C*---------------------------------------------------------------*
071800091221     C     COPIA         BEGSR
071900091221      *
072000100108      *  Se va in sovrapposizione al periodo precedente
072100100108      *   poichè già controllati i limiti in automatico si fa scadere
072200100108      *    il periodo precedente il giorno prima.
072300100222     c                   if        tariffa_nuova ='S'
072400100108      * SUPERTESTATA
072500100108      *   controlla esistenza del periodo precedente   Meno 1
072600100108     c     prgNEW        sub       1             PRGnewM1
072700100208     C     KtgtNEWm1     setgt     fitgt01l
072800100108      *
072900100108     c     prog_preced   tag
073000100208     C     KtgtN         readpe    fitgt01l
073100100108      *
073200100108     c                   if        not %EoF(fitgt01l)
073300100108      *
073400100108     c                   if        tgtatb <> *blank
073500100108     c                   goto      prog_preced
073600100108     c                   else
073700100108      * aggiorna la scadenza del periodo precedente in automatico
073800100108     c                   move      hdadata       dataiso
073900100108     c                   subdur    1:*d          dataiso
074000100108     c                   move      dataiso       tgtDST
074100100108     c                   update    fitgt000
074200100108     c                   end
074300100108     c                   end
074400100108      *
074500100108      * TESTATA
074600100108      *   controlla esistenza del periodo precedente   Meno 1
074700100108     C     KTGTN         setll     fifgt01l
074800100108     C     KTGTN         reade     fifgt01l
074900100108      *
075000100108     c                   Dow       not %EoF(fifgt01l)
075100100108      *
075200100108     c                   if        fgtatb = *blank and fgtprg = prgNEWm1
075300100108      * aggiorna la scadenza del periodo precedente in automatico
075400100108     c                   move      hdadata       dataiso
075500100108     c                   subdur    1:*d          dataiso
075600100108     c                   move      dataiso       fgtDST
075700100108     c                   update    fifgt000
075800100108     c                   end
075900100108     C     KTGTN         reade     fifgt01l
076000100108     c                   end
076100100108      *
076200100108     c                   end
076300100108      *
076400091222      * Prima copia la testata
076500100108     C     KTGTold       chain(n)  fiTGT01l
076600091222     c                   if        %Found(fiTGT01l)
076700091222     c                   eval      Tgtpdr = COMnew
076800091222     c                   eval      TgtSML = SMLnew
076900091222     c                   eval      TgtPRG = PRGnew
077000100105     c                   move      Hdadata       tgtddt
077100100105     c                   move      Hadata        tgtdst
077200100105     c                   movel     DesTar        tgtDCV
077300100127     c                   clear                   tgtdts
077400100127     c                   clear                   tgtdcn
077500100127     c                   clear                   tgtdrc
077600091222     c                   write     fiTGT000
077700091222     c                   enD
077800091222      *
077900091222      * Poi Tutti i Dettagli
078000100105     C     KTGTO         SETLL     fifgt01L
078100091222     C*
078200091222     C                   DO        *HIVAL
078300100108     C     KTGTO         READE(n)  fifgt01L
078400091222     C* Eof esce dal ciclo
078500091222     C                   if        %EOF(FIFGT01L)
078600091222     C                   LEAVE
078700091222     C                   end
078800091222     C*
078900091222     C* se il progressivo è diverso o il record è annullato
079000091222     c                   if        fgtprg <> prgOLD or fgtATB <> ' '
079100091222     c                   ITER
079200091222     c                   end
079300091222      *
079400091222     C* richiama il programma che esegue la copia dei dettagli tariffari
079500091222     C                   MOVEL     fgtTSR        TSRold
079600091222     C                   MOVEL     fgtCTR        CTRold
079700091222      *
079800091222     c                   movel     ficn06ds      kpjbu
079900091222     c                   call      'FICN06R'
080000091222     c                   parm                    kpjba
080100091222     C*
080200091222     C                   ENDDO
080300091222     C*
080400091222     C                   ENDSR
080500091222     C*****************************************************************
080600091222     c     *inzsr        begsr
080700091222     C*****************************************************************
080800091222     C     *ENTRY        PLIST
080900091222     C                   PARM                    KPJBA
081000091222     c*
081100090113     C                   Z-ADD     1             CODUT
081200090113     C                   CALL      'X§PARUT'
081300090113     C                   PARM                    UT§DSE
081400090113     C                   MOVEL     RAGUT         RSUT             20
081500090113     C                   MOVEL     REC80         CNCR80
081600091111     c*
081700090112     c                   clear                   ficn06ds
081800091221     c*
081900091221     c*  la copia avviene sempre da tariffe reali a tariffe simulate
082000091221     c*   quindi predispongo i flags per le chiavi di accesso o di scrittura
082100091221     c*   degli archivi tariffe.
082200091221     c                   eval      smlold =' '
082300090113     c                   eval      smlnew ='S'
082400091111     C*
082500090112     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
082600090112     C                   TIME                    W0120            14 0
082700090112     C                   MOVE      W0120         WDAT              8 0
082800090112     C                   Z-ADD     WDAT          G02DAT
082900090112     C                   MOVEL     *BLANK        G02ERR
083000090112     C                   CALL      'XSRDA8'
083100090112     C                   PARM                    WLBDAT
083200091111      *
083300090112     C* UDATE A 8 IN AAAA/MM/GG
083400090112     C                   Z-ADD     G02INV        WUDA              8 0
083500090112     C*---------------------------------------------------------------*
083600090112     C* ACCESSO fifgt
083700100105     C     KTGTO         KLIST
083800090112     C                   KFLD                    COMOLD
083900090112     C                   KFLD                    SMLOLD
084000091222      *--
084100100105     C     KTGTN         KLIST
084200100105     C                   KFLD                    COMnew
084300100105     C                   KFLD                    SMLnew
084400100105      *--
084500090112     C* ACCESSO fiTgt
084600091222     C     KTGTnew       KLIST
084700091222     C                   KFLD                    COMnew
084800091222     C                   KFLD                    SMLnew
084900091222     C                   KFLD                    PRGnew
085000091223      *--
085100100107     C     KTGTnewM1     KLIST
085200100107     C                   KFLD                    COMnew
085300100107     C                   KFLD                    SMLnew
085400100107     C                   KFLD                    PRGnewM1
085500100107      *--
085600100107     C     KTGTnewP1     KLIST
085700100107     C                   KFLD                    COMnew
085800100107     C                   KFLD                    SMLnew
085900100107     C                   KFLD                    PRGnewP1
086000100107      *--
086100091222     C* ACCESSO fiTgt
086200091222     C     KTGTold       KLIST
086300091222     C                   KFLD                    COMold
086400091222     C                   KFLD                    SMLold
086500091222     C                   KFLD                    PRGold
086600091222      *--
086700090112     C* ACCESSO fiapd
086800090112     C     Kapdn         KLIST
086900090112     C                   KFLD                    parana
087000090112     C                   KFLD                    comnew
087100091111      *
087200090112     C     Kapdo         KLIST
087300090112     C                   KFLD                    parana
087400090112     C                   KFLD                    comold
087500091111      *
087600090112     C                   MOVEL     'A'           PARANA
087700091111     C*
087800090112     C* CARICO TABELLA FILIALI contabili GESTITE
087900140117     c***                clear                   trul72ds
088000140117     c***                eval      i72tla = 'L'
088100140117     c***                eval      I72POU = simpou
088200140117     c***                eval      kpjbu = trul72ds
088300140117     C***                CALL      'TRUL72R'
088400140117     C***                PARM                    KPJBA
088500140117     c***                movel     kpjbu         trul72ds
088600140117     C***
088700140117      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
088800140117     c                   clear                   TRUL31DS
088900140117     c                   eval      I31abi = 'AC'
089000140117     c                   eval      I31cpo = simpou
089100140117     c                   call      'TRUL31R'
089200140117     c                   parm                    KPJBA
089300140117     c                   parm                    TRUL31DS
089400140117     c                   if        o31pog > *zeros
089500140117     c                   movea     o31pog        $pog
089600140117     c                   endif
089700091111     C*
089800090112     C* VEDO SE SONO SIMFEL O REMOTO
089900090112     c                   if        simtpp = '2'
090000090112     C                   SETON                                        89
090100090112     C***
090200090112     C* CARICO TABELLA FILIALI GESTITE £6
090300090112     C                   CLEAR                   DSUL06
090400090112     C                   MOVE      '£6'          D06COD
090500090112     C                   MOVEL     simpou        D06KEY
090600090112     C                   MOVEL     DSUL06        KPJBU
090700090112     C*
090800090112     C                   CALL      'TRUL06R'
090900090112     C                   PARM                    KPJBA
091000090112     C                   MOVEL     KPJBU         DSUL06
091100090112     C                   MOVEA     LIN           L6
091200090112     C                   ENDIF
091300090112     C*
091400090112     C                   ENDSR
091500100104     C**********************************************************************
091600100104     C*--- Windows di forzatura           ---------------------------------*
091700100104     C**********************************************************************
091800100104     C     Win_Forza     BEGSR
091900100104      *
092000100104     c                   eval      WTitolo = 'SOSTITUZIONE ?'
092100100104     c                   movel     'F6=Forza'    WmsgFNZ
092200100104     c                   move      'F12=Ritorno' WmsgFNZ
092300100104     c                   eval      WMsg1 = 'Il progressivo tariffa è già presen-
092400100104     c                             te.'
092500100104     c                   eval      WMsg3 = 'F6 -> sostituisce la precedente tar-
092600100104     c                             iffa'
092700100104     c                   eval      WMsg4 = 'F12-> Torna al video precedente.'
092800100104      *
092900100104     C     FINESTRA      TAG
093000100104     c                   exfmt     MSGWIND
093100100104     c  NKL
093200100104     CANNKF              GOTO      FINESTRA
093300100104      *
093400100104     C                   endSR
093500100224     C**********************************************************************
093600100224     C*--- Windows di Allerta             ---------------------------------*
093700100224     C**********************************************************************
093800100224     C     Win_Allerta   BEGSR
093900100224      *
094000100224     c                   clear                   ALERTWD
094100100224     c                   eval      WTit    = 'Altre T A R I F F E !'
094200100224     c                   evalr     WmsgFNZ = 'Enter per proseguire'
094300100224     c                   eval      WMsg1 = '    Attenzione per l''autista sono-
094400100224     c                              già presenti'
094500100224     c                   eval      WMsg2 = '    altre TARIFFE'
094600100224     c                   eval      WMsg3 = 'Prima di procedere, VERIFICARE i pe-
094700100224     c                             riodi già utilizzati.'
094800100224     c                   eval      WMsg4 = '> con F8 visualizzare i periodi <'
094900100224      *
095000100224     c                   exfmt     ALERTWD
095100100224      *
095200100224     C                   endSR
095300100104     C*****************************************************************
095400100104     c*  Qui esegue il calcolo in più o in meno di ogni singola tariffa
095500100104     C*****************************************************************
095600100104     c     Manipola      begsr
095700100104     C*
095800100105     C*  rielabora la tariffa nelle testate e nei dettagli che ha appena copiato
095900100105     C     KTGTN         setll     FIFPT01l
096000100105     C     KTGTN         reade     FIFPT01l
096100100105     c                   dow       not %EoF(FIFPT01L)
096200100105     C*
096300100105     C*  Si applica l'incremento sul campo FPTATA solo per il progressivo creato
096400100105     C*     ed il Tipo Servizio richiesto
096500100105     C*  ...e se FPTATA ha un valore
096600100105     c                   if        fptprg = prgNEW and FPTATA > 0
096700100105     C*
096800100127     C*  imposta la percentuale da usare
096900100127     c                   clear                   percentuale       5 2
097000100127     c                   if        fpttsr ='C'
097100100127     c                   z-add     percent_con   percentuale
097200100127     c                   elseIf    fpttsr ='R'
097300100127     c                   z-add     percent_rit   percentuale
097400100127     c                   elseIf    fpttsr ='G'
097500100127     c                   z-add     percent_gio   percentuale
097600100127     c                   end
097700100127      *
097800100105     C* calcola la percentuale
097900100208     c                   if        percentuale <> 0 and fptata <> 0
098000100127     c     fptata        mult      percentuale   valore_dec       25 9
098100100107     c                   div       100           valore_dec
098200100105     C*  quindi somma algebricamente
098300100107     c                   add       valore_dec    fptATA
098400100127     c                   end
098500100105     C*
098600100105     C*    NON deve andare sotto al limite prefissato di 0,001
098700100309      * solo se è valorizzata
098800100309     c                   if        fptATA > 0
098900100105     c                   if        fptATA < 0,001
099000100105     c                   eval      fptATA = 0,001
099100100105     C                   END
099200100309     C                   END
099300100105     C*
099400100105     C                   END
099500100105     c                   update    fifpt000
099600100105     C*
099700100105     C     KTGTN         reade     FIFPT01l
099800100105     C                   ENDdo
099900100104     C*
100000100105     C*--------------------**
100100100105     C*  rielabora la tariffa nelle testate e nei dettagli che ha appena copiato
100200100105     C     KTGTN         setll     FIFPd01l
100300100105     C     KTGTN         reade     FIFPd01l
100400100105     c                   dow       not %EoF(FIFPd01L)
100500100105     C*
100600100105     C*  Si applica l'incremento  solo per il progressivo creato
100700100105     C*     ed il Tipo Servizio richiesto
100800100105     c                   if          fpdprg = prgNEW
100900100208     C*
101000100208     C*  imposta la percentuale da usare
101100100208     c                   clear                   percentuale       5 2
101200100208     c                   if        fpdtsr ='C'
101300100208     c                   z-add     percent_con   percentuale
101400100208     c                   elseIf    fpdtsr ='R'
101500100208     c                   z-add     percent_rit   percentuale
101600100208     c                   elseIf    fpdtsr ='G'
101700100208     c                   z-add     percent_gio   percentuale
101800100208     c                   end
101900100105     C*
102000100105     C*  ...e se FPDITR ha un valore
102100100208     c                   if            FPdITR <> 0 and percentuale <> 0
102200100105     C* calcola la percentuale
102300100107     c     fpdITR        mult      percentuale   valore_dec
102400100107     c                   div       100           valore_dec
102500100105     C*  quindi somma algebricamente
102600100107     c                   add       valore_dec    fpdITR
102700100127     c                   end
102800100105     C*
102900100105     C*    NON deve andare sotto al limite prefissato di 0,001
103000100309      * solo se valorizzata
103100100309     c                   if        fpdITR > 0
103200100105     c                   if        fpdITR < 0,001
103300100105     c                   eval      fpdITR = 0,001
103400100105     C                   END
103500100309     C                   END
103600100105     C*
103700100105     C*  ...e se FPDMIN ha un valore
103800100208     c                   if            FPdMIN <> 0 and percentuale <> 0
103900100105     C* calcola la percentuale
104000100107     c     fpdMIN        mult      percentuale   valore_dec
104100100107     c                   div       100           valore_dec
104200100105     C*  quindi somma algebricamente
104300100107     c                   add       valore_dec    fpdMIN
104400100127     c                   end
104500100105     C*
104600100105     C*    NON deve andare sotto al limite prefissato di 0,001
104700100309      * solo se valorizzata
104800100309     c                   if        fpdMIN > 0
104900100105     c                   if        fpdMIN < 0,001
105000100105     c                   eval      fpdMIN = 0,001
105100100105     C                   END
105200100309     C                   END
105300100105     C*
105400100105     C*  ...e se FPDMAX ha un valore
105500100208     c                   if            FPdMAX <> 0 and percentuale <> 0
105600100105     C* calcola la percentuale
105700100107     c     fpdMAX        mult      percentuale   valore_dec
105800100107     c                   div       100           valore_dec
105900100105     C*  quindi somma algebricamente
106000100107     c                   add       valore_dec    fpdMAX
106100100127     c                   end
106200100105     C*
106300100105     C*    NON deve andare sotto al limite prefissato di 0,001
106400100309      * solo se valorizzata
106500100309     c                   if        fpdMAX > 0
106600100105     c                   if        fpdMAX < 0,001
106700100105     c                   eval      fpdMAX = 0,001
106800100105     C                   END
106900100309     C                   END
107000100105     C*
107100100105     C                   END
107200100105     C*
107300100105     c                   update    fifpd000
107400100105     C*
107500100105     C     KTGTN         reade     FIFPd01l
107600100105     C                   ENDdo
107700100105     C*
107800100104     C                   ENDSR
107900091222     C*---------------------------------------------------------------*
108000100105** err
108100100108Filiale Errata !!                                                                ind.51        1
108200100224Codice Autista inesistente o Annullato !!                                        ind.55        2
108300100108Tariffa o progressivo inesistenti !!                                             ind.50        3
108400100108Data formalmente Errata !!                                                       ind.52/53     4
108500100108Periodo non Valido. Le date NON sono ordinate correttamente !!                   ind.52/53     5
108600100108Periodo non Valido. In sovrapposizione ad un altro già presente !!               ind.52/53     6
108700100108Il periodo precedente scade il 31/12/2039. Non è ammesso inserirne oltre !!      ind.52        7
108800100108Non è ammesso inserire una data oltre il 31/12/2039.                             ind.53        8
108900100108Data NON VALIDA. Il periodo precedente finisce il giorno :                       ind.52        9
109000100108Data NON VALIDA. Il periodo successivo inizia il giorno :                        ind.53       10
109100100127Si deve effettuare una variazione in % alla tariffa che si sta simulando !!      ind.54       11
109200100108NON si può far decorrere il nuovo progressivo tariffa prima del :                ind.52       12
109300100224Come nuovo progressivo occorre utilizzare il :                                   ind.56       13
109400100108>> Progressivo non valido, Duplica non possibile      <<                         ind.XX       14
109500100108>> Periodo non Valido per il progressivo richiesto    <<                         ind.XX       15
109600100224>> Manca il segno positivo o negativo per il valore immesso  <<                  ind.XX       16
109700100224Il progressivo non può essere inferiore a :                                      ind.56       17
