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
001700070307     D TRUL72DS      E DS
001800070307     D  L72                           3s 0 DESCEND
001900070307     D                                     DIM(60)
002000070307     D                                     OVERLAY(O72Lia)
002100950113     D* SCHIERE PER CARICAMENTO L1 E L6
002200950113     D L6              S              3  0 DIM(30)                              FIL GESTITE
002300090109     D parana          S              1    inz('A')                             FIL GESTITE
002400090114     D comtsr          S              1                                         FIL GESTITE
002500091221     D*
002600100105     d datapre         s               d   datfmt(*iso)
002700100105     d datapos         s               d   datfmt(*iso)
002800091223     d dataiso         s               d   datfmt(*iso)
002900091223     d dataeur         s               d   datfmt(*eur)
003000091223     d Data_oggi       s              8  0
003100100105     d Data_gma        s              8  0
003200100107     D*
003300100107     d PRGnewM1        s                   like(PRGnew)
003400100107     d PRGnewP1        s                   like(PRGnew)
003500100224     d  filSAV         s                   like(filOLD)
003600100224     d  pdrsav         s                   like(pdrOLD)
003700100107     d ultimo_progressivo...
003800100107     d                 s                   like(PRGnew)
003900100222     d primo_progressivo...
004000100222     d                 s                   like(PRGnew)
004100091223     D*
004200100105     D* schiere di controllo
004300100105     d Progr           s              3  0 dim(50)
004400100222     d datDDT          s              8s 0 dim(50)
004500100222     d datDST          s              8s 0 dim(50)
004600100222     d dcvDES          s             15A   dim(50)
004700100105     d piumen          s              2    dim(50)
004800100105     D*
004900091221     D da_forzare      S              1    inz(' ')
005000100222     d data8           s              8s 0
005100091222     D*
005200090112     d ficn06ds      e ds
005300021203     D fnlv24ds      E DS
005400000000     D KPJBA         E DS
005500091221     D*
005600090112     D                 DS
005700090112     D  AA                     1      4  0
005800090112     D  MM                     5      6  0
005900090112     D  GG                     7      8  0
006000090112     D  DATA                   1      8  0
006100091111      *
006200940922     D* DS PER TRUL06R - CARICAMENTO £1
006300940922     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006400940922     D  LIN                    1     90  0
006500940922     D                                     DIM(30)                              SKI COMODO
006600931018     D CNCR80        E DS
006700921030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006800091221     D*
006900011109     D WLBDAT          DS
007000011109     D  G02DAT                 1      8  0
007100011109     D  G02INV                 9     16  0
007200011109     D  G02ERR                17     17
007300011109     D  G02TGI                18     22  0
007400091111      *
007500000000     I/SPACE 3
007600931018     C*****************************************************************
007700931018     C* RIEPILOGO INDICATORI
007800931018     C*****************************************************************
007900931018     C* 30    - DI COMODO
008000931018     C* 35    - USATO PER LOKUP SU SCHIERE E TESTN
008100940923     C* 40/52 - INDICATORI PER MESSAGGI DI ERRORE
008200020717     C* 89    - SONO UN 2° liv.
008300931018     C* 90    - SEGNALA LA PRESENZA DI ERRORI
008400090112     C*****************************************************************
008500100127      * Chiave principale da quale tariffa copiare
008600100127     C                   move(p)   simPOU        filNEW
008700100127     C                   clear                   pdrNEW
008800100127     C                   clear                   dpdNEW
008900100127     C                   clear                   prgOLD
009000100127      *
009100091221     c     Inizio        tag
009200091221     C                   EXSR      inzD1
009300931018     C*
009400090112     C                   do        *hival
009500091221     C*
009600091221     C*   ind.gen.errori da spegnere
009700091221      *                *-------------------*
009800091221     C                   EXFMT     cn83D01
009900091221      *                *-------------------*
010000100105     c                   clear                   vidmsg
010100100224     C                   setoff                                       90
010200100105     C                   setoff                                       505152
010300100105     C                   setoff                                       535455
010400100224     C                   setoff                                       565758
010500100224     C                   setoff                                       59
010600931018     C** CMD3 - FINE LAVORO
010700091221     C   KC              goto      FINE_pgm
010800100224      *
010900100224     C*  CONTROLLI
011000100303     C  nKH              EXSR      ctrd1
011100100303      *
011200100224      *  Se cambiato l'Autista  ci sono altre tariffe già presenti
011300100224     C*   emette la finestre di allerta
011400100303      *     oppure
011500100303     C** CMD8 - Cancella progressivi di periodi precedenti
011600100224     c                   if        FILNEW <> FILsav or PDRNEW <> PDRsav
011700100303     c                              or
011800100303     c                             *INKH
011900100303      *
012000100224     c                   if         altriProgr = 'S'
012100100303     c                              or
012200100303     c                             *INKH
012300100224     c                   eval        FILsav = FILnew
012400100224     c                   eval        PDRsav = PDRnew
012500100224     c                   movel(p)  comNEW        autista           7
012600100224     c                   clear                   progres           3
012700100224     c                   clear                   da_data           8
012800100224     c                   clear                   a_data            8
012900100224     c                   call      'FICN83R1'
013000100224     c                   parm                    autista           7
013100100224     c                   parm                    progres           3
013200100224     c                   parm                    da_data           8
013300100224     c                   parm                    a_data            8
013400100224     c                   if        progres <> *blank
013500100303     c                   setoff                                       56
013600100303     c                   clear                   vidmsg
013700100224     c                   move      progres       prgNEW
013800100224     c                   move      da_data       dadata
013900100224     c                   move      a_data        adata
014000100224     c                   end
014100100224     c                   end
014200100224     c                   end
014300100224      *
014400100107      *
014500091221     C*
014600091221     C*  Se la tariffa è già presente avvisa se ci si vuole andare sopra.....
014700091221      *                *-------------------*
014800091221     c  N90
014900091221     cAn KF              if        Da_Forzare = 'S'
015000091222     c                   exsr      win_Forza
015100091221     c                   end
015200091221      *                *-------------------*
015300091221     c  N90
015400091221     cAN KF              leave
015500090112     c                   enddo
015600091221      *
015700091221      * COMUNQUE se c'era già i progressivo in simulazione procede:
015800091221      *  Prima tenta di cancellarlo poi copia comunque copia ed infine
015900091221      * modifica la nuova tariffa in base alle regole di manipolazione.
016000091221     c   kf              do
016100091221     c                   exsr      elimina
016200091221     c                   exsr      copia
016300091221     c                   exsr      manipola
016400100105     c                   commit
016500091221     c                   end
016600100107     c                   goto      inizio
016700091221     C*
016800091221     c     FINE_pgm      tag
016900091222      *
017000931018     C                   SETON                                        LR
017100091222     C**********************************************************************
017200091222     C*--- PULIZIA CAMPI DEL FORMATO ---------------------------------*
017300091222     C**********************************************************************
017400091222     C     InzD1         BEGSR
017500091222      *
017600100127     C                   clear                   prgNEW
017700100107     C                   clear                   prgNEWM1
017800100107     C                   clear                   prgNEWP1
017900091221     C                   clear                   filOLD
018000091221     C                   clear                   pdrOLD
018100100224     C                   clear                   filSAV
018200100224     C                   clear                   pdrsav
018300100222     C                   clear                   prgNEWsav         3 0
018400100127     C                   clear                   conperc
018500100127     C                   clear                   ritperc
018600100127     C                   clear                   gioperc
018700091223     C                   clear                   destar
018800100107     C                   clear                   dadata
018900100127     c                   z-add     31122039      adata
019000100107     C                   clear                   hdadata
019100100107     C                   clear                   hadata
019200100208     c                   move      '-'           consegn
019300100208     c                   move      '-'           ritsegn
019400100208     c                   move      '-'           giosegn
019500091111      *
019600100224      *  si è deciso di annullare al momento tutti i default sopraimpostati
019700100224     c                   clear                   cn83D01
019800100224      *
019900100303     c                   eval      rnmus = knmus
020000100303     c                   eval      rnsif = knsif
020100100303     C                   MOVEL     RAGUT         RSUT             20
020200100303      *
020300931018     C                   ENDSR
020400090112     C**********************************************************************
020500931018     C*--- CONTROLLI FORMATO -----------------------------------------*
020600090112     C**********************************************************************
020700091221     C     CTRd1         BEGSR
020800100105     C*
020900100222     C                   SETOFF                                       901112
021000100127     c                   clear                   percent_con       5 2
021100100127     c                   clear                   percent_rit       5 2
021200100127     c                   clear                   percent_gio       5 2
021300100222     c                   clear                   prg_cambiato      1
021400940922     C*
021500100222     c                   if        prgNEW <> prgNewsav
021600100222     c                   eval      prgNewsav = prgNEW
021700100222     c                   eval       prg_cambiato = 'S'
021800100222     c                   end
021900100222     C*
022000940922     C****  CODICE FILIALE NUOVA  ****
022100950113     C* FILIALE OBBLIGATORIA
022200950113     C     FILNEW        IFEQ      0
022300950113     C                   SETON                                        5190
022400100105     c                   eval      vidmsg = err(1)
022500950113     C                   GOTO      ENDCTR
022600950113     C                   ENDIF
022700950113     C*
022800950113     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
022900950113     C     *IN89         IFEQ      *OFF
023000021216     C     FILNEW        IFNE      SIMpou
023100021216     C     simtpp        andeq     '1'
023200070307     C     FILNEW        LOOKUP    L72                                    35
023300940922     C  N35              SETON                                        5190
023400100105     c  N35              eval      vidmsg = err(1)
023500940922     C  N35              GOTO      ENDCTR
023600940922     C                   ENDIF
023700931018     C*
023800950113     C                   ELSE
023900020717     C* 89 ON  - SONO UN 2° liv.  QUINDI CONTROLLO LA FILIALE NELLA L6
024000020717     C     filnew        IFNE      simpou
024100950113     C     FILNEW        LOOKUP    L6                                     35
024200950113     C  N35              SETON                                        5190
024300100105     c  N35              eval      vidmsg = err(1)
024400950113     C  N35              GOTO      ENDCTR
024500950113     C                   ENDIF
024600950113     C                   ENDIF
024700091222     C                   Z-ADD     FILnew        FILold
024800950113     C*
024900931018     C****  CODICE PADRONCINO NUOVO  ****
025000931018     C     '?'           SCAN      PDRNEW                                 90
025100931018     C*
025200931018     C     *IN90         IFEQ      *ON
025300021203     c                   clear                   fnlv24ds
025400021203     C                   Z-ADD     FILNEW        d24FIL
025500021203     C                   MOVEL     'R'           d24FLG
025600030107     C                   MOVEL     parana        d24tip
025700021203     C                   MOVEL(p)  fnlv24ds      KPJBU
025800091111     C                   CALL      'FNLV24R'
025900931018     C                   PARM                    KPJBA
026000021203     C                   MOVEL     KPJBU         fnlv24ds
026100950113     C*
026200950113     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
026300021203     C     d24pdr        IFNE      0
026400021203     C                   MOVEL     d24pdr        FILNEW
026500931018     C                   MOVEL     *ZEROS        PDRNEW
026600021203     C                   MOVE      d24pdr        PDRNEW
026700021203     C                   MOVEL     d24rsc        DPDNEW
026800950113     C                   ENDIF
026900950113     C*
027000931018     C                   GOTO      ENDCTR
027100931018     C                   ENDIF
027200931018     C*
027300931018     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
027400931018     C                   TESTN                   PDRNEW               35
027500100105     C  N35              SETON                                        5590
027600100105     c  N35              eval      vidmsg = err(2)
027700931018     C  N35              GOTO      ENDCTR
027800090504     C                   MOVEL     FILNEW        COMNEW            7 0
027900090504     C                   MOVE      PDRNEW        COMNEW
028000091222     C*
028100091222     C                   move(p)   pdrNEW        pdrOLD
028200091222     C                   z-add     filNEW        filOLD
028300091222     C                   MOVE      comNEW        comOLD            7 0
028400931018     C*
028500940922     C* CONTROLLO ESISTENZA DEL PADRONCINO IN ANAGRAFICA
028600021203     C     kapdn         CHAIN     fiapd000                           30
028700940922     C  N30APDATB        IFNE      ' '
028800940922     C                   SETON                                        30
028900940922     C                   ENDIF
029000940922     C* CODICE PADRONCINO ERRATO
029100100105     C   30              SETON                                        5590
029200100105     c   30              eval      vidmsg = err(2)
029300940922     C   30              GOTO      ENDCTR
029400100223     C                   MOVEL     APDRSF        DPDNEW
029500931018     C*
029600100107      ******************  +++++++++++++++++++++  *******************
029700090112     C****  CONTROLLO VALIDITA' DELLA TARIFFA old  ****
029800090112     c                   clear                   ok                1
029900100105     C     KTGTO         setll     fitgt01l
030000090112     c                   do        *hival
030100100108     C     KTGTO         reade(n)  fitgt01l
030200100105     c                   if        %eof(fitgt01l)
030300090112     c                   leave
030400090112     c                   end
030500100105     C                   IF        tgtatb <> ' '
030600090112     c                   iter
030700090112     c                   end
030800100105     C                   IF        tgtprg <> prgold
030900090112     c                   iter
031000090112     c                   end
031100090112     c                   move      '1'           ok
031200091223      * propone la descrizione della tariffa da cui copiare
031300100105     c                   if        DesTar = *blank
031400091223     c                   movel     tgtDCV        DesTar
031500100105     c                   end
031600090112     c                   leave
031700090112     C                   ENDdo
031800091221      **-----**
031900100105     c* progressivo tariffa da cui copiare inesistente
032000090112     c                   if        ok = ' '
032100100105     c                   eval      vidmsg = err(3)
032200090112     c                   seton                                        5090
032300090112     c                   goto      endctr
032400011126     c                   end
032500091221      *
032600100208      ******************  +++++++++++++++++++++  *******************
032700100208      * TARIFFA NUOVA DA CREARE
032800100107      ******************  +++++++++++++++++++++  *******************
032900100208      ** si schierizza i periodi già immessi
033000100208      *
033100100208     C****   CONTROLLO sovrapposizione di PERIODI   ****
033200100208     c                   clear                   Progr
033300100208     c                   clear                   datDDT
033400100208     c                   clear                   datDST
033500100222     c                   clear                   dcvDES
033600100208     c                   clear                   piuMEN
033700100208     c                   clear                   indice            3 0
033800100208     c                   clear                   altriProgr        1
033900100208      *
034000100208     c                   clear                   sovrapposto       1
034100100208     c                   clear                   da_forzare
034200100208     c                   clear                   DDT_limite        8 0
034300100208     c                   clear                   ERRDDT_limite     8 0
034400100208     c                   eval      ultimo_progressivo = 0
034500100222     c                   eval      primo_progressivo = 0
034600100208     c                   clear                   tariffa_nuova     1
034700100208      *
034800100208     C     KTGTn         setll     fitgt01l
034900100208      *
035000100208     c                   do        *hival
035100100208     C     KTGTn         reade(n)  fitgt01l
035200100208      *
035300100208     c                   if        %eof(fitgt01l)
035400100208     c                   leave
035500100208     c                   end
035600100208      *  se annullato
035700100208     C                   IF        tgtatb <> ' '
035800100208     c                   iter
035900100208     c                   end
036000100208      *
036100100208      *  presenti altri progressivi
036200100208     c                   move      'S'           altriProgr        1
036300100208      *
036400100208      * si memorizza tutti i periodi inseriti per ulteriore controllo di contiguità
036500100208     c                   add       1             indice
036600100208     c                   z-add     tgtprg        Progr(indice)
036700100208     c                   z-add     tgtDDT        DatDDT(indice)
036800100208     c                   z-add     tgtDST        DatDST(indice)
036900100222     c                   move      tgtDCV        DcvDES(indice)
037000100208      *
037100100208      *  periodo subito precedente
037200100208     c                   if        tgtprg = prgNEW -1
037300100208     c                   movel     '-1'          piuMEN(indice)
037400100208     c                   elseif    tgtprg = prgNEW
037500100208     c                   movel     '00'          piuMEN(indice)
037600100208     c                   elseif    tgtprg = prgNEW +1
037700100208     c                   movel     '+1'          piuMEN(indice)
037800100208     c                   end
037900100208      *
038000100208      *  memorizza l'ultimo
038100100208     c                   eval      ultimo_progressivo = tgtprg
038200100222     c                   if        primo_progressivo = 0
038300100222     c                   eval      primo_progressivo = tgtprg
038400100222     C                   END
038500100208     c                   z-add     indice        ultimo_ind        3 0
038600100208      *
038700100208     C                   ENDdo
038800100208      *
038900100208      *  se c'erano dei periodi già inseriti sul cliente li ha schierizzati
039000100208      **-----**
039100100208      **
039200100208      **  se non Immesso il progressivo
039300100208     c                   if        prgNEW = 0
039400100224      **
039500100224     c                   seton                                        56  90
039600100224     c                   eval      vidmsg = err(15)
039700100224     c                   goto      endctr
039800100208      **
039900100208     c                   if          Ultimo_progressivo > 0
040000100224      **   NON SI VUOLE PROPORRE un default
040100100224     c*************      eval      prgNEW = ultimo_progressivo + 1
040200100208      **
040300100208     c                   if        dadata = 0
040400100224     c*************      eval      data8 = DatDST(ultimo_ind)
040500100224     c*************      move      Data8         dataiso
040600100224     c*************      adddur    1:*d          dataiso
040700100224     c*************      move      dataiso       dataeur
040800100224     c*************      move      dataeur       dadata
040900100208     c                   end
041000100208     c                   if         adata = 0
041100100224     c*************      z-add     31122039      adata
041200100208     c                   end
041300100208      **
041400100208      **   se non c'erano dei periodi già immessi....
041500100208     c                   else
041600100224     c*************      z-add     PRGold        PRGnew
041700100208     c                   if         adata = 0
041800100224     c*************      z-add     31122039      adata
041900100208     c                   end
042000100208     c                   end
042100100208      **
042200100208     c                   else
042300100208      *
042400100208      **  se immesso un progressivo o è presente nelle schiere
042500100208      *   o deve essere consecutivo
042600100208     c                   z-add     1             indice
042700100208     c     prgNEW        lookup    Progr(indice)                          95
042800100208      *
042900100208     c                   if        *in95
043000100222      *
043100100222      * -----
043200100222     c                   eval      data8 = DatDDT(indice)
043300100222     c                   move      Data8         dataiso
043400100222     c                   move      dataiso       dataeur
043500100222      *
043600100222      *  se si tratta del primo progressivo e la data a video non è (0)
043700100222      *   non si deve sovrapporre la data trovata
043800100222     c                   If        prg_cambiato = 'S'
043900100222     c                   move      dataeur       dadata
044000100222     c                   end
044100100222      * -----
044200100222     c                   eval      data8 = DatDST(indice)
044300100222     c                   move      Data8         dataiso
044400100222     c                   move      dataiso       dataeur
044500100222      *
044600100222      *  se si tratta dell'ultimo progressivo e la data a video non è (0)
044700100222      *   non si deve sovrapporre la data trovata
044800100222     c                   If         prg_cambiato = 'S'
044900100222     c                   move      dataeur       adata
045000100222     c                   end
045100100222      *
045200100222      *  solo se non presente una descrizione sul campo a video
045300100222     c                   if        Destar = *blank
045400100222     c                   eval      DesTar = DcvDES(indice)
045500100222     c                   end
045600100222      * -----
045700100222      * protegge le date di periodi
045800100222     c                   if        prgNEW =  primo_progressivo
045900100222     C                   SETON                                        12
046000100222     c                   elseIf    prgNEW =  ultimo_progressivo
046100100222     C                   SETON                                        11
046200100222     c                   elseIf    prgNEW = ultimo_progressivo and
046300100222     c                             prgNEW =  primo_progressivo
046400100222     C                   SETOFF                                       1112
046500100222     c                   else
046600100222     C                   SETON                                        1112
046700100222     c                   end
046800100222      *
046900100208     c                   movel     'S'           sovrapposto
047000100208     c                   move      'S'           da_forzare
047100100224      *
047200100208     c                   else
047300100224      *
047400100224     c                   if        ultimo_progressivo > 0
047500100224      *  Controlla la sequenza dei progressivi
047600100224     c                   if        prgNEW > ultimo_progressivo + 1
047700100224     c                   seton                                        56  90
047800100224     c                   eval      vidmsg = err(13)
047900100224     c                   eval      vidmsg = %trim(vidmsg) +
048000100224     c                               %editc(ultimo_progressivo+1:'Z')
048100100224     c                   goto      endctr
048200100224     c                   elseif    prgNEW < primo_progressivo
048300100224     c                   seton                                        56  90
048400100224     c                   eval      vidmsg = err(17)
048500100224     c                   eval      vidmsg = %trim(vidmsg) +
048600100224     c                               %editc(primo_progressivo:'Z')
048700100224     c                   goto      endctr
048800100224     c                   end
048900100224     c                   end
049000100224      *
049100100208     c                   eval      tariffa_nuova ='S'
049200100208      *
049300100208      * altrimenti incrementato di 1
049400100208     c                   if          Ultimo_progressivo > 0
049500100208     c                   eval      prgNEW = ultimo_progressivo + 1
049600100208      **
049700100224     c                   if        dadata = 0 and ultimo_ind > 0
049800100222     c                   eval      data8 = DatDST(ultimo_ind)
049900100222     c                   move      Data8         dataiso
050000100208     c                   adddur    1:*d          dataiso
050100100208     c                   move      dataiso       dataeur
050200100208     c                   move      dataeur       dadata
050300100208     c                   end
050400100208      **
050500100208     c                   else
050600100208     c                   z-add     PRGold        PRGnew
050700100208      *
050800100208      *   imposta quella da cui copiare
050900100208     C     KTGTold       chain(n)  fiTGT01l
051000100208     c                   if        %Found(fiTGT01l)
051100100208     c                   if        dadata = 0
051200100208     c                   move      tgtddt        dataiso
051300100208     c                   move      dataiso       dataeur
051400100208     c                   move      dataeur       dadata
051500100208     c                   end
051600100208     c                   if        adata = 0
051700100208     c                   move      tgtdst        dataiso
051800100208     c                   move      dataiso       dataeur
051900100208     c                   move      dataeur       adata
052000100208     c                   end
052100100208     c                   end
052200100208      *
052300100208     c                   end
052400100208      *
052500100208      * lasciata solo data limite
052600100208     c                   if         adata = 0
052700100208     c                   z-add     31122039      adata
052800100208     c                   end
052900100208      *
053000100208     c                   end
053100100208      **
053200100208     c                   end
053300100208      **
053400100208      **--------------**
053500100208      *  problemi con le date di periodo
053600100208      **--------------**
053700100208      * Da Data
053800100208     c                   clear                   hdadata
053900100208$017 C     dadata        IFgt      *zeros
054000100208     c     *eur          test(d)                 dadata                 52
054100100208     c                   if        *in52
054200100208     c     *dmy          test(d)                 dadata                 52
054300100208     c  n52*dmy          move      dadata        dataeur
054400100208     c  n52              move      dataeur       dadata
054500100208     C                   end
054600100208     C                   end
054700100208     c                   if        dadata <> *zeros and *in52
054800100208     c                              or
054900100208     c                             dadata =  *zeros
055000100208     c                   eval      vidmsg = err(4)
055100100208     c                   seton                                        5290
055200100208     c                   goto      endctr
055300100208     C                   ENDIF
055400100208      * gira la data in AMG
055500100208$017 C     dadata        IFgt      *zeros
055600100208     c                   move      dadata        dataeur
055700100208     c                   move      dataeur       dataiso
055800100208     c                   move      dataiso       hdadata
055900100208     C                   end
056000100208      *
056100100208      * Le date non possono superare il limite del 31/12/2039
056200100208     c                   if        hdadata > 20391231
056300100208     c                   seton                                        5290
056400100208     c                   eval      vidmsg = err(8)
056500100208     c                   goto      endctr
056600100208     C                   end
056700100208      *
056800100208      **--------------**
056900100208      *  a Data
057000100208     c                   clear                   hadata
057100100208$017 C     adata         IFgt      *zeros
057200100208     c     *eur          test(d)                 adata                  53
057300100208     c                   if        *in53
057400100208     c     *dmy          test(d)                 adata                  53
057500100208     c  n53*dmy          move      adata         dataeur
057600100208     c  n53              move      dataeur       adata
057700100208     C                   end
057800100208     C                   end
057900100208     c                   if         adata <> *zeros and *in53
058000100208     c                              or
058100100208     c                             adata =  *zeros
058200100208     c                   seton                                        5390
058300100208     c                   eval      vidmsg = err(4)
058400100208     c                   goto      endctr
058500100208     C                   ENDIF
058600100208      * gira la data in AMG
058700100208$017 C     adata         IFgt      *zeros
058800100208     c                   move      adata         dataeur
058900100208     c                   move      dataeur       dataiso
059000100208     c                   move      dataiso       hadata
059100100208     c                   end
059200100208      *
059300100208      * Le date non possono superare il limite del 31/12/2039
059400100208     c                   if        hadata > 20391231
059500100208     c                   seton                                        5390
059600100208     c                   eval      vidmsg = err(8)
059700100208     c                   goto      endctr
059800100208     C                   end
059900100208      *
060000100208      *  date in successione defiscono il periodo
060100100208     c                   if        hdadata > hadata
060200100208     c                   seton                                        525390
060300100208     c                   eval      vidmsg = err(5)
060400100208     c                   goto      endctr
060500100208     C                   end
060600100208      ***
060700100208      ***  controlla sovrapposizioni di date nei periodi
060800100224     c                   if        tariffa_nuova ='S'  and ultimo_ind > 0
060900100224     C                   IF        hdadata <= DatDDT(ultimo_ind)
061000100222     c                   eval      data8 = DatDDT(ultimo_ind)
061100100222     c                   move      Data8         dataiso
061200100208     c                   adddur    1:*d          dataiso
061300100208     c                   move      dataiso       DDT_Limite
061400100208     c                   move      dataiso       dataeur
061500100208     c                   move      dataeur       ERRDDT_limite
061600100208     C                   end
061700100222     C                   end
061800100222      ***
061900100208     c* se il periodo si sovrappone ad un altro precedente
062000100208     c*
062100100208     c                   if            ERRDDT_limite > 0
062200100208     c                   seton                                        525390
062300100208     c                   eval      vidmsg = err(12)
062400100208     c                   eval      vidmsg = %trim(vidmsg) +
062500100208     c                               %editc(ERRDDT_limite:'Y')
062600100208     c                   goto      endctr
062700100208     c                   end
062800100224      ***
062900100224      ******************  +++++++++++++++++++++  *******************
063000100208      ******************  +++++++++++++++++++++  *******************
063100100208      ******************  +++++++++++++++++++++  *******************
063200100105      *
063300100105      **--------------**
063400100127      *  Non si può NON scegliere almeno un Incremento e un decremento
063500100127     c                   if        conPERC = 0 and ritPERC = 0 and gioPERC = 0
063600100105     c                   seton                                        5490
063700100105     c                   eval      vidmsg = err(11)
063800100105     c                   goto      endctr
063900100105     c                   end
064000100105      *
064100100105      * Memorizza algebricamente l'incremento se in più o in meno
064200100127     c                   if        conPERC <> 0
064300100224     c                   if        conSEGN = *blank
064400100224     c                   seton                                        5790
064500100224     c                   eval      vidmsg = err(16)
064600100224     c                   goto      endctr
064700100224     c                   elseif    conSEGN = '+'
064800100127     c                   z-add     conPERC       percent_con
064900100208     c                   elseIf    conSEGN = '-'
065000100208     c                   z-sub     conPERC       percent_con
065100100127     c                   end
065200100208     c                   end
065300100127      *
065400100127     c                   if        ritPERC <> 0
065500100224     c                   if        ritSEGN = *blank
065600100224     c                   seton                                        5890
065700100224     c                   eval      vidmsg = err(16)
065800100224     c                   goto      endctr
065900100224     c                   elseif    ritSEGN = '+'
066000100127     c                   z-add     ritPERC       percent_rit
066100100208     c                   elseIf    ritSEGN = '-'
066200100208     c                   z-sub     ritPERC       percent_rit
066300100127     c                   end
066400100208     c                   end
066500100127      *
066600100127     c                   if        gioPERC <> 0
066700100224     c                   if        gioSEGN = *blank
066800100224     c                   seton                                        5990
066900100224     c                   eval      vidmsg = err(16)
067000100224     c                   goto      endctr
067100100224     c                   elseif    gioSEGN = '+'
067200100127     c                   z-add     gioPERC       percent_gio
067300100208     c                   elseIf    gioSEGN = '-'
067400100208     c                   z-sub     gioPERC       percent_gio
067500100127     c                   end
067600100208     c                   end
067700100127      *
067800091221     C     ENDCTR        ENDSR
067900091221     C*---------------------------------------------------------------*
068000091221     C* se viene FORZATA la sostituzione di un progressivo precedente *
068100091221     C*---------------------------------------------------------------*
068200091221     C     ELIMINA       BEGSR
068300091221      *
068400091221      *  Per sostituire il progressivo precedentemente inserito
068500100127      *  prima lo deve eliminare quindi può successivamente duplicarlo
068600091221      *
068700091221     C/EXEC SQL
068800091221     C+ DELETE FROM FIFGT00F WHERE FGTSML = 'S' and FGTPRG= :prgnew
068900100127     C+    and FGTPDR = :COMNEW
069000091221     C/END-EXEC
069100091221      *
069200091221     C/EXEC SQL
069300091221     C+ DELETE FROM FIFPT00F WHERE FPTSML = 'S' and FPTPRG= :prgnew
069400100127     C+    and FPTPDR = :COMNEW
069500091221     C/END-EXEC
069600091221      *
069700091221     C/EXEC SQL
069800091221     C+ DELETE FROM FIFPD00F WHERE FPDSML = 'S' and FPDPRG= :prgnew
069900100127     C+    and FPDPDR = :COMNEW
070000091221     C/END-EXEC
070100091221      *
070200091221     C/EXEC SQL
070300091221     C+ DELETE FROM FITGT00F WHERE TGTSML = 'S' and TGTPRG= :prgnew
070400091221     C+    and TGTPDR = :COMNEW
070500091221     C/END-EXEC
070600091221      *
070700091221     C                   ENDSR
070800090113     C*---------------------------------------------------------------*
070900091221     C*--- GESTIONE DELLA COPIA della TARIFFA ------------------------*
071000090112     C*---------------------------------------------------------------*
071100091221     C     COPIA         BEGSR
071200091221      *
071300100108      *  Se va in sovrapposizione al periodo precedente
071400100108      *   poichè già controllati i limiti in automatico si fa scadere
071500100108      *    il periodo precedente il giorno prima.
071600100222     c                   if        tariffa_nuova ='S'
071700100108      * SUPERTESTATA
071800100108      *   controlla esistenza del periodo precedente   Meno 1
071900100108     c     prgNEW        sub       1             PRGnewM1
072000100208     C     KtgtNEWm1     setgt     fitgt01l
072100100108      *
072200100108     c     prog_preced   tag
072300100208     C     KtgtN         readpe    fitgt01l
072400100108      *
072500100108     c                   if        not %EoF(fitgt01l)
072600100108      *
072700100108     c                   if        tgtatb <> *blank
072800100108     c                   goto      prog_preced
072900100108     c                   else
073000100108      * aggiorna la scadenza del periodo precedente in automatico
073100100108     c                   move      hdadata       dataiso
073200100108     c                   subdur    1:*d          dataiso
073300100108     c                   move      dataiso       tgtDST
073400100108     c                   update    fitgt000
073500100108     c                   end
073600100108     c                   end
073700100108      *
073800100108      * TESTATA
073900100108      *   controlla esistenza del periodo precedente   Meno 1
074000100108     C     KTGTN         setll     fifgt01l
074100100108     C     KTGTN         reade     fifgt01l
074200100108      *
074300100108     c                   Dow       not %EoF(fifgt01l)
074400100108      *
074500100108     c                   if        fgtatb = *blank and fgtprg = prgNEWm1
074600100108      * aggiorna la scadenza del periodo precedente in automatico
074700100108     c                   move      hdadata       dataiso
074800100108     c                   subdur    1:*d          dataiso
074900100108     c                   move      dataiso       fgtDST
075000100108     c                   update    fifgt000
075100100108     c                   end
075200100108     C     KTGTN         reade     fifgt01l
075300100108     c                   end
075400100108      *
075500100108     c                   end
075600100108      *
075700091222      * Prima copia la testata
075800100108     C     KTGTold       chain(n)  fiTGT01l
075900091222     c                   if        %Found(fiTGT01l)
076000091222     c                   eval      Tgtpdr = COMnew
076100091222     c                   eval      TgtSML = SMLnew
076200091222     c                   eval      TgtPRG = PRGnew
076300100105     c                   move      Hdadata       tgtddt
076400100105     c                   move      Hadata        tgtdst
076500100105     c                   movel     DesTar        tgtDCV
076600100127     c                   clear                   tgtdts
076700100127     c                   clear                   tgtdcn
076800100127     c                   clear                   tgtdrc
076900091222     c                   write     fiTGT000
077000091222     c                   enD
077100091222      *
077200091222      * Poi Tutti i Dettagli
077300100105     C     KTGTO         SETLL     fifgt01L
077400091222     C*
077500091222     C                   DO        *HIVAL
077600100108     C     KTGTO         READE(n)  fifgt01L
077700091222     C* Eof esce dal ciclo
077800091222     C                   if        %EOF(FIFGT01L)
077900091222     C                   LEAVE
078000091222     C                   end
078100091222     C*
078200091222     C* se il progressivo è diverso o il record è annullato
078300091222     c                   if        fgtprg <> prgOLD or fgtATB <> ' '
078400091222     c                   ITER
078500091222     c                   end
078600091222      *
078700091222     C* richiama il programma che esegue la copia dei dettagli tariffari
078800091222     C                   MOVEL     fgtTSR        TSRold
078900091222     C                   MOVEL     fgtCTR        CTRold
079000091222      *
079100091222     c                   movel     ficn06ds      kpjbu
079200091222     c                   call      'FICN06R'
079300091222     c                   parm                    kpjba
079400091222     C*
079500091222     C                   ENDDO
079600091222     C*
079700091222     C                   ENDSR
079800091222     C*****************************************************************
079900091222     c     *inzsr        begsr
080000091222     C*****************************************************************
080100091222     C     *ENTRY        PLIST
080200091222     C                   PARM                    KPJBA
080300091222     c*
080400090113     C                   Z-ADD     1             CODUT
080500090113     C                   CALL      'X§PARUT'
080600090113     C                   PARM                    UT§DSE
080700090113     C                   MOVEL     RAGUT         RSUT             20
080800090113     C                   MOVEL     REC80         CNCR80
080900091111     c*
081000090112     c                   clear                   ficn06ds
081100091221     c*
081200091221     c*  la copia avviene sempre da tariffe reali a tariffe simulate
081300091221     c*   quindi predispongo i flags per le chiavi di accesso o di scrittura
081400091221     c*   degli archivi tariffe.
081500091221     c                   eval      smlold =' '
081600090113     c                   eval      smlnew ='S'
081700091111     C*
081800090112     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
081900090112     C                   TIME                    W0120            14 0
082000090112     C                   MOVE      W0120         WDAT              8 0
082100090112     C                   Z-ADD     WDAT          G02DAT
082200090112     C                   MOVEL     *BLANK        G02ERR
082300090112     C                   CALL      'XSRDA8'
082400090112     C                   PARM                    WLBDAT
082500091111      *
082600090112     C* UDATE A 8 IN AAAA/MM/GG
082700090112     C                   Z-ADD     G02INV        WUDA              8 0
082800090112     C*---------------------------------------------------------------*
082900090112     C* ACCESSO fifgt
083000100105     C     KTGTO         KLIST
083100090112     C                   KFLD                    COMOLD
083200090112     C                   KFLD                    SMLOLD
083300091222      *--
083400100105     C     KTGTN         KLIST
083500100105     C                   KFLD                    COMnew
083600100105     C                   KFLD                    SMLnew
083700100105      *--
083800090112     C* ACCESSO fiTgt
083900091222     C     KTGTnew       KLIST
084000091222     C                   KFLD                    COMnew
084100091222     C                   KFLD                    SMLnew
084200091222     C                   KFLD                    PRGnew
084300091223      *--
084400100107     C     KTGTnewM1     KLIST
084500100107     C                   KFLD                    COMnew
084600100107     C                   KFLD                    SMLnew
084700100107     C                   KFLD                    PRGnewM1
084800100107      *--
084900100107     C     KTGTnewP1     KLIST
085000100107     C                   KFLD                    COMnew
085100100107     C                   KFLD                    SMLnew
085200100107     C                   KFLD                    PRGnewP1
085300100107      *--
085400091222     C* ACCESSO fiTgt
085500091222     C     KTGTold       KLIST
085600091222     C                   KFLD                    COMold
085700091222     C                   KFLD                    SMLold
085800091222     C                   KFLD                    PRGold
085900091222      *--
086000090112     C* ACCESSO fiapd
086100090112     C     Kapdn         KLIST
086200090112     C                   KFLD                    parana
086300090112     C                   KFLD                    comnew
086400091111      *
086500090112     C     Kapdo         KLIST
086600090112     C                   KFLD                    parana
086700090112     C                   KFLD                    comold
086800091111      *
086900090112     C                   MOVEL     'A'           PARANA
087000091111     C*
087100090112     C* CARICO TABELLA FILIALI contabili GESTITE
087200090112     c                   clear                   trul72ds
087300090112     c                   eval      i72tla = 'L'
087400090112     c                   eval      I72POU = simpou
087500090112     c                   eval      kpjbu = trul72ds
087600090112     C                   CALL      'TRUL72R'
087700090112     C                   PARM                    KPJBA
087800090112     c                   movel     kpjbu         trul72ds
087900091111     C*
088000090112     C* VEDO SE SONO SIMFEL O REMOTO
088100090112     c                   if        simtpp = '2'
088200090112     C                   SETON                                        89
088300090112     C***
088400090112     C* CARICO TABELLA FILIALI GESTITE £6
088500090112     C                   CLEAR                   DSUL06
088600090112     C                   MOVE      '£6'          D06COD
088700090112     C                   MOVEL     simpou        D06KEY
088800090112     C                   MOVEL     DSUL06        KPJBU
088900090112     C*
089000090112     C                   CALL      'TRUL06R'
089100090112     C                   PARM                    KPJBA
089200090112     C                   MOVEL     KPJBU         DSUL06
089300090112     C                   MOVEA     LIN           L6
089400090112     C                   ENDIF
089500090112     C*
089600090112     C                   ENDSR
089700100104     C**********************************************************************
089800100104     C*--- Windows di forzatura           ---------------------------------*
089900100104     C**********************************************************************
090000100104     C     Win_Forza     BEGSR
090100100104      *
090200100104     c                   eval      WTitolo = 'SOSTITUZIONE ?'
090300100104     c                   movel     'F6=Forza'    WmsgFNZ
090400100104     c                   move      'F12=Ritorno' WmsgFNZ
090500100104     c                   eval      WMsg1 = 'Il progressivo tariffa è già presen-
090600100104     c                             te.'
090700100104     c                   eval      WMsg3 = 'F6 -> sostituisce la precedente tar-
090800100104     c                             iffa'
090900100104     c                   eval      WMsg4 = 'F12-> Torna al video precedente.'
091000100104      *
091100100104     C     FINESTRA      TAG
091200100104     c                   exfmt     MSGWIND
091300100104     c  NKL
091400100104     CANNKF              GOTO      FINESTRA
091500100104      *
091600100104     C                   endSR
091700100224     C**********************************************************************
091800100224     C*--- Windows di Allerta             ---------------------------------*
091900100224     C**********************************************************************
092000100224     C     Win_Allerta   BEGSR
092100100224      *
092200100224     c                   clear                   ALERTWD
092300100224     c                   eval      WTit    = 'Altre T A R I F F E !'
092400100224     c                   evalr     WmsgFNZ = 'Enter per proseguire'
092500100224     c                   eval      WMsg1 = '    Attenzione per l''autista sono-
092600100224     c                              già presenti'
092700100224     c                   eval      WMsg2 = '    altre TARIFFE'
092800100224     c                   eval      WMsg3 = 'Prima di procedere, VERIFICARE i pe-
092900100224     c                             riodi già utilizzati.'
093000100224     c                   eval      WMsg4 = '> con F8 visualizzare i periodi <'
093100100224      *
093200100224     c                   exfmt     ALERTWD
093300100224      *
093400100224     C                   endSR
093500100104     C*****************************************************************
093600100104     c*  Qui esegue il calcolo in più o in meno di ogni singola tariffa
093700100104     C*****************************************************************
093800100104     c     Manipola      begsr
093900100104     C*
094000100105     C*  rielabora la tariffa nelle testate e nei dettagli che ha appena copiato
094100100105     C     KTGTN         setll     FIFPT01l
094200100105     C     KTGTN         reade     FIFPT01l
094300100105     c                   dow       not %EoF(FIFPT01L)
094400100105     C*
094500100105     C*  Si applica l'incremento sul campo FPTATA solo per il progressivo creato
094600100105     C*     ed il Tipo Servizio richiesto
094700100105     C*  ...e se FPTATA ha un valore
094800100105     c                   if        fptprg = prgNEW and FPTATA > 0
094900100105     C*
095000100127     C*  imposta la percentuale da usare
095100100127     c                   clear                   percentuale       5 2
095200100127     c                   if        fpttsr ='C'
095300100127     c                   z-add     percent_con   percentuale
095400100127     c                   elseIf    fpttsr ='R'
095500100127     c                   z-add     percent_rit   percentuale
095600100127     c                   elseIf    fpttsr ='G'
095700100127     c                   z-add     percent_gio   percentuale
095800100127     c                   end
095900100127      *
096000100105     C* calcola la percentuale
096100100208     c                   if        percentuale <> 0 and fptata <> 0
096200100127     c     fptata        mult      percentuale   valore_dec       25 9
096300100107     c                   div       100           valore_dec
096400100105     C*  quindi somma algebricamente
096500100107     c                   add       valore_dec    fptATA
096600100127     c                   end
096700100105     C*
096800100105     C*    NON deve andare sotto al limite prefissato di 0,001
096801100309      * solo se è valorizzata
096802100309     c                   if        fptATA > 0
096900100105     c                   if        fptATA < 0,001
097000100105     c                   eval      fptATA = 0,001
097100100105     C                   END
097101100309     C                   END
097200100105     C*
097300100105     C                   END
097400100105     c                   update    fifpt000
097500100105     C*
097600100105     C     KTGTN         reade     FIFPT01l
097700100105     C                   ENDdo
097800100104     C*
097900100105     C*--------------------**
098000100105     C*  rielabora la tariffa nelle testate e nei dettagli che ha appena copiato
098100100105     C     KTGTN         setll     FIFPd01l
098200100105     C     KTGTN         reade     FIFPd01l
098300100105     c                   dow       not %EoF(FIFPd01L)
098400100105     C*
098500100105     C*  Si applica l'incremento  solo per il progressivo creato
098600100105     C*     ed il Tipo Servizio richiesto
098700100105     c                   if          fpdprg = prgNEW
098800100208     C*
098900100208     C*  imposta la percentuale da usare
099000100208     c                   clear                   percentuale       5 2
099100100208     c                   if        fpdtsr ='C'
099200100208     c                   z-add     percent_con   percentuale
099300100208     c                   elseIf    fpdtsr ='R'
099400100208     c                   z-add     percent_rit   percentuale
099500100208     c                   elseIf    fpdtsr ='G'
099600100208     c                   z-add     percent_gio   percentuale
099700100208     c                   end
099800100105     C*
099900100105     C*  ...e se FPDITR ha un valore
100000100208     c                   if            FPdITR <> 0 and percentuale <> 0
100100100105     C* calcola la percentuale
100200100107     c     fpdITR        mult      percentuale   valore_dec
100300100107     c                   div       100           valore_dec
100400100105     C*  quindi somma algebricamente
100500100107     c                   add       valore_dec    fpdITR
100600100127     c                   end
100700100105     C*
100800100105     C*    NON deve andare sotto al limite prefissato di 0,001
100801100309      * solo se valorizzata
100802100309     c                   if        fpdITR > 0
100900100105     c                   if        fpdITR < 0,001
101000100105     c                   eval      fpdITR = 0,001
101100100105     C                   END
101101100309     C                   END
101200100105     C*
101300100105     C*  ...e se FPDMIN ha un valore
101400100208     c                   if            FPdMIN <> 0 and percentuale <> 0
101500100105     C* calcola la percentuale
101600100107     c     fpdMIN        mult      percentuale   valore_dec
101700100107     c                   div       100           valore_dec
101800100105     C*  quindi somma algebricamente
101900100107     c                   add       valore_dec    fpdMIN
102000100127     c                   end
102100100105     C*
102200100105     C*    NON deve andare sotto al limite prefissato di 0,001
102201100309      * solo se valorizzata
102202100309     c                   if        fpdMIN > 0
102300100105     c                   if        fpdMIN < 0,001
102400100105     c                   eval      fpdMIN = 0,001
102500100105     C                   END
102501100309     C                   END
102600100105     C*
102700100105     C*  ...e se FPDMAX ha un valore
102800100208     c                   if            FPdMAX <> 0 and percentuale <> 0
102900100105     C* calcola la percentuale
103000100107     c     fpdMAX        mult      percentuale   valore_dec
103100100107     c                   div       100           valore_dec
103200100105     C*  quindi somma algebricamente
103300100107     c                   add       valore_dec    fpdMAX
103400100127     c                   end
103500100105     C*
103600100105     C*    NON deve andare sotto al limite prefissato di 0,001
103601100309      * solo se valorizzata
103602100309     c                   if        fpdMAX > 0
103700100105     c                   if        fpdMAX < 0,001
103800100105     c                   eval      fpdMAX = 0,001
103900100105     C                   END
103901100309     C                   END
104000100105     C*
104100100105     C                   END
104200100105     C*
104300100105     c                   update    fifpd000
104400100105     C*
104500100105     C     KTGTN         reade     FIFPd01l
104600100105     C                   ENDdo
104700100105     C*
104800100104     C                   ENDSR
104900091222     C*---------------------------------------------------------------*
105000100105** err
105100100108Filiale Errata !!                                                                ind.51        1
105200100224Codice Autista inesistente o Annullato !!                                        ind.55        2
105300100108Tariffa o progressivo inesistenti !!                                             ind.50        3
105400100108Data formalmente Errata !!                                                       ind.52/53     4
105500100108Periodo non Valido. Le date NON sono ordinate correttamente !!                   ind.52/53     5
105600100108Periodo non Valido. In sovrapposizione ad un altro già presente !!               ind.52/53     6
105700100108Il periodo precedente scade il 31/12/2039. Non è ammesso inserirne oltre !!      ind.52        7
105800100108Non è ammesso inserire una data oltre il 31/12/2039.                             ind.53        8
105900100108Data NON VALIDA. Il periodo precedente finisce il giorno :                       ind.52        9
106000100108Data NON VALIDA. Il periodo successivo inizia il giorno :                        ind.53       10
106100100127Si deve effettuare una variazione in % alla tariffa che si sta simulando !!      ind.54       11
106200100108NON si può far decorrere il nuovo progressivo tariffa prima del :                ind.52       12
106300100224Come nuovo progressivo occorre utilizzare il :                                   ind.56       13
106400100108>> Progressivo non valido, Duplica non possibile      <<                         ind.XX       14
106500100108>> Periodo non Valido per il progressivo richiesto    <<                         ind.XX       15
106600100224>> Manca il segno positivo o negativo per il valore immesso  <<                  ind.XX       16
106700100224Il progressivo non può essere inferiore a :                                      ind.56       17
