000100010920     H DECEDIT('0,') DATEDIT(*YMD.)
000200161011     h option(*nodebugio)
000300161027     h dftactgrp(*no)
000400161027     h bnddir('UBBNDDIR')
000500060221
000600001009     FAZORG01L  IF   E           K DISK
000700161027     FTNTBE01L  UF A E           K DISK     usropn
000800161027     f                                      extfile(wLibFile)
000900060221     FTNTB11D   CF   E             WORKSTN  sfile(TNTB11S:S01nrr)
001000161012     F                                      sfile(TNTB11S5:S05nrr)
001100060221
001200980521      *------------------------------------------------------------------------*
001300080411
001400080411      *   C O S T A N T I
001500080411
001600161011     d c_SflPag        c                   const(17)
001700161013     d c_SflSizMax     c                   const(80)
001800161027
001900161027       // -?Costanti per la definizione delle schiere con i nomi?
002000161027       //  ?degli iSeries da elaborare e delle relative librerie?
002100161027     d c_NrSyst        c                   const(2)
002200161027     d c_NrLibr        c                   const(2)
002300060221
002400980521      *   S C H I E R E
002500161027
002600161027       // -?iSeries  &  Librerie con il file tabella?
002700161104     d sk_iSystem      s                   like(currSysNetA)
002800161027     d                                     dim(c_NrSyst)
002900161027     d                                     ctdata   perrcd( 1)
003000161104     d sk_Libraries    s                   like(ds_Libr)
003100161027     d                                     dim(c_NrSyst)
003200161104     d                                     alt(sk_iSystem)
003300161027
003400161027       // -?Ridefinizione elenco librerie in cui elaborare la tabella?
003500161027     d ds_Libr         ds                  inz
003600161104     d   sk_Libr                     10    dim(c_NrLibr) inz
003700060221
003800161027       // -?Messaggi di errore?
003900161104     D MSG             S             78    DIM(9) CTDATA PERRCD(1)              MSG VIDEO
004000161104
004100161104       // -?Postazioni Entrata già rilevate?
004200161104     d sk_PEP          s                   like(V3Cpep)  inz
004300161104     d                                     dim(c_SflSizMax)
004400001006
004500980521      *   D S   I N T E R N E / E S T E R N E
004600060221
004700001009     D WLBDAT          DS                  INZ
004800001009     D  G02DAT                 1      8  0
004900001009     D  G02INV                 9     16  0
005000001009     D  G02ERR                17     17
005100001009     D  G02TGI                18     22  0
005200060221     D                 DS
005300060221     D  ds_giorno              1      2  0
005400060221     D  ds_mese                3      4  0
005500060221     D  ds_anno                5      8  0
005600060221     D  ds_data                1      8  0
005700070611      *
005800070611     d W3Cpep          ds            14    inz
005900070611     d  W3C_blk                       4
006000070611     d  V3Cpep
006100010308
006200001016     D TIBS02DS      E DS
006300011016     D DMTP          E DS
006400011016     D DMSL          E DS
006500010605     D DMSL_vd       E DS                  EXTNAME(DMSL) prefix(W)
006600980521      *
006700010919      *
006800980521     D KPJBA         E DS
006900060221      * - Parametri x Controllo profilo utenti
007000060221     d TIBS34DS      e ds                  inz
007100060221      * - Ds di riferimento al file esterno AZUTE00F
007200060221     d AZUTEDS       e ds                  extname(AZUTE00F)
007300060221      * - Ds per dati organigramma
007400060221     d DDatiUte      e ds
007500980521      *
007600010601     D OG150         E DS
007700060221
007800060221      *   V A R I A B I L I
007900060221
008000060221     D WMESS           S                   LIKE(V2CMSG)
008100060221     d S01nrr          s                   like(C01rcd) inz
008200161012     d S05nrr          s                   like(C05rcd) inz
008300060221     D RIMNRR          S                   LIKE(S01nrr)
008400060221     D KCOD            S                   LIKE(TBECOD)
008500060221     D KKE1            S                   LIKE(TBEKE1)
008600060221     D KKE2            S                   LIKE(TBEKE2)
008700060221     d klin            s                   like(tbelin)
008800060221     d ksif            s                   like(tbesif)
008900161027     d sav_tbeAPL      s                   like(tbeAPL)
009000060221     d sav_tbeftt      s                   like(tbeftt)
009100161027     d*// sav_tbeflt      s                   like(tbeflt)
009200060221      *
009300060221     d wConta          s              3  0 inz
009400161012      *
009500161012     d $InzS05         s               n   inz
009600161027     d $Ok_Sys         s               n
009700161104
009800161104       // -?Indici di schiera?
009900161104     d xx              s              3  0 inz
010000161027
010100161027       // -?Nome del sistema?
010200161027     d currSysNeta     s              8a
010300161027
010400161027       // -?Nome esteso Libreria/File del file tabella?
010500161027     d wLibFile        s             21a   inz
010600161027
010700161027       // -?Campi di comodo?
010800161027     d w_iSystem       s              1  0 inz
010900161027     d w_SisInf        s              3  0 inz
011000161027     d wDate           s              8  0 inz
011100161027
011200161027       //--------------------------------------------------------------
011300161027       //?Definizione prototipi procedure.                             ?
011400161027       //--------------------------------------------------------------
011500161027
011600161027       // -?Reperimento NETA sistema AS/400 corrente?
011700161027      /copy gaitrasrc/srcProtoPR,UBRTVNETA
011800060221
011900001006      *------------------------------------------------------------------------*
012000060221
012100060221      ****************************************************************
012200060221      *  RIEPILOGO INDICATORI
012300060221      ****************************************************************
012400060221      * 20 - NON ripulisce il subfile      (sfldsp)
012500060221      * 21 - Emette il subfile             (sfldspctl)
012600060221      * 23 - Fine dati nel subfile         (sflend)
012700060221      * 28 - ERRORE GENERICO DSPF
012800060221      * 29 - Eseguita ricerca sul campo tipo applicazione
012900060221      * 30 - PROTEZIONE DEI RECORD ANNULLATI DEL SFL
013000060221      * 31 - GENERICO
013100060221      * 32 - TENTATIVO DI CANCELLARE RECORD ESISTENTE
013200060221      * 40 - ERRORE Inserire P.O.
013300060221      * 41 - ERRORE P.O. inesistente
013400060221      * 42 - ERRORE P.O. annullato
013500060221      * 43 - ERRORE P.O. non valido
013600060221      * 44 - ERRORE P.O. non abilitato al Disk "C"
013700060221      * 47 - POSIZIONAMENTO CURSORE
013800161014      * 90 - Riemissione videata
013900060221      ****************************************************************
014000161027
014100161027     C                   eval      *inlr = *on
014200060221
014300161027     c                   if        $Ok_Sys
014400161027      *
014500001006      * Emissione 1° Videata
014600001006     C                   do        *hival
014700001009      * Pulizia 1° Videata
014800010601     C                   if        (*in28 = *off and *in90 = *off)
014900010604     C                   MOVE      *BLANKS       V1CFIL
015000010601     C                   MOVE      '?'           V1CFIL
015100001009     C                   endif
015200001011
015300010601     C                   exfmt     TNTB111
015400001006      * F3=Fine
015500001006     C   kc              leave
015600010405
015700001006      * Controlli 1° videata
015800010604     C                   SETOFF                                       28
015900001006     C                   exsr      Sr_Contr01
016000980623
016100010601     C   28              iter
016200001009     C   90              iter
016300001009
016400001009     ?* Gestione subfile ?
016500001009     C                   exsr      Sr_Gessfl
016600981008
016700010606      * F3=Fine
016800010606     C   kc              LEAVE
016900010606
017000001006     C                   enddo
017100161027
017200161027     c                   EndIf
017300981204
017400161027     C*//                eval      *inlr = *on
017500980521      **********************************************************************
017600980521      * CONTROLLI VIDEO 1
017700980521      **********************************************************************
017800001006     C     Sr_Contr01    begsr
017900001009      * Reset indicatori
018000010601     C                   setoff                                       404142
018100060221     C                   setoff                                       4344
018200060221     C                   setoff                                       28  90
018300010601
018400010919     C     '?'           scan      v1cfil                                 31
018500010601     C     *in31         ifeq      *on
018600010601     C                   eval      *in90 = *on
018700010601     C                   clear                   TIBS02DS
018800010601     C                   movel     'R'           t02mod
018900010601     C                   movel     knsif         t02sif
019000010601     C                   movel     'MSL'         t02cod
019100010601     C                   call      'TIBS02R'
019200010601     C                   parm                    KPJBA
019300010601     C                   parm                    TIBS02DS
019400010601     C                   if        t02err =  *blanks
019500010601     C                   movel     t02ke1        v1cfil
019600011016     C                   ELSE
019700010601     C                   SETON                                        4028
019800010601     C                   SETON                                        90
019900010601     C                   GOTO      ENDSRCONTR
020000010601     C                   endif
020100010604     C                   ENDIF
020200010601
020300010601     C                   IF        V1CFIL = *ZEROS
020400010601     C                   SETON                                        4028      controllo P.O.emi.
020500010601     C                   GOTO      ENDSRCONTR                                   normale
020600010601     C                   ENDIF
020700010212
020800010604     C                   MOVEL     V1CFIL        W3CAR             3 0
020900010604     C     W3CAR         CHAIN     AZORG01L                           31
021000010601     C                   if        *IN31 = *ON
021100010601     C                   SETON                                        4128
021200010601     C                   GOTO      ENDSRCONTR                                   normale
021300010212     C                   endif
021400001013
021500010604     C                   IF        ORGFVA <> *BLANKS
021600010601     C                   SETON                                        4228
021700010601     C                   GOTO      ENDSRCONTR                                   normale
021800001013     C                   endif
021900010213
022000010604     C                   IF        ORGFAG <> 'A'
022100010604     C                   IF        ORGFAG <> 'F'
022200010601     C                   SETON                                        4328
022300010601     C                   GOTO      ENDSRCONTR
022400010604     C                   ENDIF
022500001011     C                   endif
022600001006
022700001006
022800010604     C                   MOVEL     ORGDES        V2DFIL
022900010604
023000010601     C     ENDSRCONTR    endsr
023100010601      **********************************************************************
023200010601      * CONTROLLI SUBFILE
023300010601      **********************************************************************
023400010601     C     Sr_CONTSBF    begsr
023500010919      *
023600011017     C                   setoff                                       29
023700161014      * Ricerca sui campi Tipi Applicazione
023800010919     C     '?'           scan      v3capl                                 31
023900161011if  1C     *in31         ifeq      *on
024000010919     C                   clear                   TIBS02DS
024100010919     C                   movel     'R'           t02mod
024200010919     C                   movel     knsif         t02sif
024300010919     C                   movel     'MTP'         t02cod
024400010919     C                   call      'TIBS02R'
024500010919     C                   parm                    KPJBA
024600010919     C                   parm                    TIBS02DS
024700010919     C                   if        t02err =  *blanks
024800010919     C                   movel     t02ke1        v3capl
024900011017     c                   seton                                        29
025000011016     c                   else
025100161103     C                   eval      V2Cmsg =  T02msg
025200010920     C                   EVAL      *IN28 = *ON
025300010920     C                   GOTO      ENDSRCONTS
025400010919     C                   ENDIF
025500161013e   1C                   ENDIF
025600161025     C     '?'           scan      v3cap5                                 31
025700161025if  1C     *in31         ifeq      *on
025800161025     C                   clear                   TIBS02DS
025900161025     C                   movel     'R'           t02mod
026000161025     C                   movel     knsif         t02sif
026100161025     C                   movel     'MTP'         t02cod
026200161025     C                   call      'TIBS02R'
026300161025     C                   parm                    KPJBA
026400161025     C                   parm                    TIBS02DS
026500161025     C                   if        t02err =  *blanks
026600161025     C                   movel     t02ke1        v3cap5
026700161025     c                   seton                                        29
026800161025     c                   else
026900161103     C                   eval      V2Cmsg =  T02msg
027000161025     C                   EVAL      *IN28 = *ON
027100161025     C                   GOTO      ENDSRCONTS
027200161025     C                   ENDIF
027300161025e   1C                   ENDIF
027400010605
027500161011if  1C                   IF        (V3CDES <> *BLANKS AND V3CENT <> *ZEROS) AND
027600161011     C                             (V3CPEP <> *BLANKS AND V3CAPL <> *BLANKS and
027700161028     c                              V3Cap5 <> *blanks and V3Cde5 <> *blanks)
027800161104      *
027900161104      * Se ho valorizzato V3CPEP controllo che il valore sia univoco
028000161104if  2c                   If        V3scel <> '4'  and  V3Cann = *blank
028100161104     c                             or
028200161104     c                             V3scel =  '5'  and  V3Cann = 'A'
028300161104     c                   eval      xx = %lookup( V3Cpep : sk_PEP )
028400161104if  3c                   if        xx     >  *zero
028500161104     c                   eval      V2Cmsg =  %trimR( Msg(9) ) + ' ' +
028600161104     c                                       %trim( %editc( xx : '3' ) )
028700161104     c                   eval      *in28  =  *on
028800161104     c                   leavesr
028900161104e   3c                   endif
029000161104     c                   eval      xx = %lookup( *blank : sk_PEP )
029100161104if  3c                   if        xx > *zero
029200161104     c                   eval      sk_PEP(xx) = V3Cpep
029300161104e   3c                   endif
029400161104e   2c                   EndIf
029500010919      *
029600010919      * Se ho valorizzato V3APL controllo che il valore sia valido, che
029700010919      * esista nella tabella 'MTP' tipo applicazione
029800161011if  2C                   IF         V3CAPL <> *BLANKS
029900010920     C                   clear                   TIBS02DS
030000010920     C                   movel     'C'           t02mod
030100010920     C                   movel     knsif         t02sif
030200010920     C                   movel     'MTP'         t02cod
030300010920     C                   movel     V3CAPL        t02ke1
030400010920     C                   call      'TIBS02R'
030500010920     C                   parm                    KPJBA
030600010920     C                   parm                    TIBS02DS
030700161011if  3C                   if        t02err <> *blanks
030800010920     C                   MOVEL     MSG(5)        V2CMSG
030900010920     C                   EVAL      *IN28 = *ON
031000010920     C                   GOTO      ENDSRCONTS
031100161011x   3c                   Else
031200011016     c                   eval      DMTP =t02uni
031300011016      * controllo la validità dell'applicazione per il Vdl
031400161011if  4c     §MtpSwV       ifeq      'N'
031500040128     C                   MOVEL     MSG(6)        V2CMSG
031600011016     C                   EVAL      *IN28 = *ON
031700011016     C                   GOTO      ENDSRCONTS
031800161011e   4c                   endif
031900161011e   3C                   ENDIF
032000161011e   2C                   endif
032100161025      *
032200161025      * Se ho valorizzato V3CAP5 controllo che il valore sia valido, che
032300161025      * esista nella tabella 'MTP' tipo applicazione
032400161025if  2C                   IF         V3Cap5 <> *blanks
032500161025     C                   clear                   TIBS02ds
032600161025     C                   movel     'C'           T02mod
032700161025     C                   movel     knsif         T02sif
032800161025     C                   movel     'MTP'         T02cod
032900161025     C                   movel     V3Cap5        T02ke1
033000161025     C                   call      'TIBS02R'
033100161025     C                   parm                    KPJBA
033200161025     C                   parm                    TIBS02ds
033300161025if  3C                   If        T02err <> *blanks
033400161025     C                   movel     Msg(5)        V2Cmsg
033500161025     C                   eval      *in28 = *on
033600161025     C                   leavesr
033700161025x   3c                   Else
033800161025     c                   eval      DMTP = T02uni
033900161025      * controllo la validità dell'applicazione per il Vdl
034000161025if  4c     §MtpSwV       ifeq      'N'
034100161025     C                   movel     Msg(6)        V2Cmsg
034200161025     C                   eval      *in28 = *on
034300161025     C                   leavesr
034400161025e   4c                   endif
034500161025e   3C                   EndIf
034600161025e   2C                   ENDIF
034700031223      *
034800031223      * Controllo che il flag "Postazione entrata picking" sia
034900161014      *   impostato "S" solo per applicazione di FV 2 = "K"
035000161014     c                   if            V3Cpke =  'S'
035100161025     c                             and V3Capl <> 'K'
035200040128     c                   movel     Msg(7)        V2Cmsg
035300031223     c                   eval      *in28  = *on
035400031223     c                   goto      EndSrContS
035500031223     c                   endif
035600060221      *
035700060221      * Conteggio record inseriti
035800060221     c                   add       1             wConta
035900080512     c***                if        wConta > 45
036000080512     c***                movel     Msg(8)        V2Cmsg
036100080512     c***                eval      *in28 = *on
036200080512     c***                leavesr
036300080512     c***                endif
036400010919      *
036500161011x   1C                   ELSE
036600161014      *
036700161011if  2C                   IF        (V3CDES = *BLANKS AND V3CENT = *ZEROS) AND
036800161011     C                             (V3CPEP = *BLANKS AND V3CAPL = *BLANKS and
036900161028     C                              V3Cap5 = *blanks and V3Cde5 = *blanks)
037000161011if  3C                   IF        (V3DESH <> *BLANKS AND V3ENTH <> *ZEROS) AND
037100161011     C                             (V3PEPH <> *BLANKS AND V3APLH <> *BLANKS and
037200161028     C                              V3ap5H <> *blanks and V3de5H <> *blanks)
037300010606      * è stato cancellato un record esistente
037400010606     C                   EVAL      *IN32 = *ON
037500010606      **  riemetto i campi della ds
037600010606     C                   EVAL      V3CDES = V3DESH
037700161028     c                   eval      V3Cde5 = V3de5H
037800031223     c                   move      V3PEPH        V3Cpep
037900010606     C                   EVAL      V3CENT = V3ENTH
038000031223     C                   EVAL      V3CAPL = V3APLH
038100161025     c                   eval      V3Cap5 = V3ap5H
038200031223     c                   eval      V3Cpke = V3pkeH
038300161011     c                   eval      V3Cpwf = V3pwfH
038400010606     C                   EVAL      V3CANN = V3ANNH
038500010606     C                   GOTO      ENDSRCONTS
038600161011e   3C                   ENDIF
038700161011x   2C                   ELSE
038800010606      * qualche campo è vuoto
038900010606     C                   MOVEL     MSG(1)        V2CMSG
039000010606     C                   EVAL      *IN28 = *ON
039100010606     C                   GOTO      ENDSRCONTS                                   normale
039200161011e   2C                   ENDIF
039300161011e   1C                   ENDIF
039400010601
039500010601     C     ENDSRCONTS    endsr
039600001009      **********************************************************************
039700001009      * GESTIONE SUBFILE
039800001009      **********************************************************************
039900001009     C     Sr_Gessfl     begsr
040000001009
040100010604     C                   MOVEL     V1CFIL        V2CFIL
040200010604
040300060221     c                   clear                   C01rcd
040400060221     c                   clear                   C01csr
040500161027
040600161027      /free
040700161027
040800161027         // -?1ª apertura file tabelle del 1° S.I. (sede)?
040900161027         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
041000161027           w_SisInf = 1;
041100161027           exsr  sr_OpenFileTab;
041200161027         endif;
041300161027
041400161027      /end-free
041500161027
041600060221do  1C                   do        *hival
041700010604
041800010604     C                   SETOFF                                       32
041900010604
042000001009      * Pulisco subfile
042100060221     C                   z-add     *zeros        wCONTA
042200060221     C                   z-add     *zeros        S01nrr
042300060221     C                   eval      *in20 = *off
042400001009     C                   eval      *in21 = *off
042500060221     C                   eval      *in23 = *off
042600010601     C                   write     TNTB11C
042700001009     C                   eval      *in20 = *on
042800001009     C                   eval      *in21 = *on
042900010223
043000010912     c                   clear                   kke1
043100010601     C                   MOVEL     'MSL'         KCOD
043200010604     C                   MOVEL     V1CFIL        KKE1
043300010601     C     KTNTBE        setll     TNTBE01L
043400001016
043500060221do  2C                   do        *hival
043600010601
043700010601     C     KTNTBE        reade     TNTBE01L                               31
043800001016
043900010601     C                   SETOFF                                       30
044000010601
044100060221if  3C                   IF        *IN31 = *ON  OR %EOF
044200161013     C*//                eval      RimNrr = (c_SflPag * 6) - S01nrr
044300161013     C                   eval      RimNrr = c_SflSizMax - S01nrr
044400010601     C                   DO        RIMNRR
044500010601     C                   EXSR      SR_CARVUO
044600010601     C                   ENDDO
044700060221     C                   eval      *in23 = *on
044800010601     C                   LEAVE
044900060221e   3C                   ENDIF
045000010601
045100060221     C                   ADD       1             wCONTA
045200010601     C                   MOVEL     TBEKE2        WKE2              3 0
045300060221if  3C                   IF        wCONTA <> WKE2
045400060221     C     WKE2          SUB       wCONTA        RIMNRR
045500010601     C                   DO        RIMNRR
045600010601     C                   EXSR      SR_CARVUO
045700010601     C                   ENDDO
045800060221     C                   ADD       RIMNRR        wCONTA
045900060221e   3C                   ENDIF
046000010601      *
046100010601     C                   MOVEL     TBEUNI        DMSL
046200001009     C                   exsr      Sr_Carsfl
046300010601
046400060221e   2C                   enddo
046500010220
046600010606     C                   EVAL      V2CMSG = WMESS
046700010606     C                   IF        WMESS <> *BLANKS
046800010606     C                   EVAL      *IN28 = *ON
046900010606     C                   ENDIF
047000010606
047100010220     C     emettosf      tag
047200010228
047300060221      * Posiziono il cursore
047400060221     c                   if        C01csr > *zeros
047500060221     c                   z-add     C01csr        C01rcd
047600060221     c                   else
047700060221     c                   z-add     1             C01rcd
047800060221     c                   endif
047900001009
048000001009      * Se ho caricato emetto il subfile
048100060221     C                   if        S01nrr > *zeros
048200010601     C                   write     TNTB112
048300010601     C                   exfmt     TNTB11C
048400001009     C                   endif
048500001009
048600161011     C                   eval      *in28 = *off
048700010606     C                   EVAL      *IN32 = *OFF
048800001011      * F3=Fine
048900161104     C   kc              UNLOCK    TNTBE01L
049000010601     C   kc              LEAVE
049100001011
049200001009      * F12=Ritorno
049300161104     C   kl              UNLOCK    TNTBE01L
049400001009     C   kl              leave
049500161012
049600161012      *?F9=Altra vista?
049700161012if  2c                   if        *inKI = *on
049800161012     c                   eval      $InzS05 = *on
049900161012     c                   doW       *inKC = *off  and  *inKL = *off
050000161012     c                   exsr      sr_GesS05
050100161012     c                   enddo
050200161012     c                   select
050300161012     c                   when      *inKL
050400161012     c                   goto      EmettoSF
050500161012     c                   when      *inKC
050600161012     c                   leave
050700161012     c                   endsl
050800161012e   2c                   endif
050900001009
051000001009     ?*    Leggo Scelte Effettuate dall'utente ?
051100001107
051200060221if  2C                   if        S01nrr > *zeros
051300001107
051400161104     c                   clear                   sk_PEP
051500060221     C                   clear                   wConta
051600060221     C                   eval      S01nrr = *zeros
051700060221do  3C                   do        *hival
051800060221     C                   eval      S01nrr = S01nrr + 1
051900060221     C     S01nrr        chain     TNTB11S                            31
052000010601     C                   if        *in31 = *on
052100001107     C                   leave
052200001107     C                   endif
052300001018
052400010604      * Controlli
052500060221     c                   z-add     S01nrr        C01rcd
052600010606     C                   EVAL      *IN28 = *OFF
052700010606     C                   EVAL      *IN47 = *OFF
052800010604     C                   EXSR      SR_CONTSBF
052900010606
053000010919     C                   IF        *IN28 = *ON or *IN29 = *ON
053100161013     C                                         or *IN32 = *ON
053200010606     C                   EVAL      *IN47 = *ON
053300161013     c                   else
053400161013     c                   eval      *in47 = *off
053500161013     C                   ENDIF
053600161017
053700161017     c                   if        V3Cann =  'A'
053800161017     c                   eval      *in30  =  *on
053900161017     c                   endif
054000010606
054100010601      * Ripristino
054200060221if  4C                   if        v3scel = '5'
054300010604     C                   CLEAR                   V3SCEL
054400010604     C                   IF        V3CANN = *BLANKS
054500161012     C                   EVAL      *IN47 = *ON
054600010604     c                   SETON                                        28
054700010604     C                   MOVEL     MSG(2)        V2CMSG
054800010604     C                   ENDIF
054900010601     C                   MOVEL     *BLANKS       V3CANN
055000010601     c                   setoff                                       30
055100060221e   4C                   endif
055200010601      * Annullamento
055300060221if  4C                   if        v3scel = '4'
055400010604     C                   CLEAR                   V3SCEL
055500010604     C                   IF        V3CANN = 'A'
055600161012     C                   EVAL      *IN47 = *ON
055700010604     c                   SETON                                        28
055800010604     C                   MOVEL     MSG(3)        V2CMSG
055900010604     C                   ENDIF
056000010601     C                   MOVEL     'A'           V3CANN
056100010601     c                   seton                                        30
056200060221e   4C                   endif
056300161013
056400161013     C                   update    tntb11s
056500010601
056600010604     C                   SETOFF                                       30
056700161013      *
056800161013     C                   EVAL      *IN47 = *OFF
056900161013     C                   IF        *IN28 = *ON or *IN29 = *ON
057000161013     c                   z-add     C01rcd        C01csr
057100161013     C                   LEAVE
057200161013     C                   ENDIF
057300010604
057400060221e   3C                   enddo
057500010606
057600010606     C                   IF        *IN32 = *ON
057700161012     C                   EVAL      *IN47 = *ON
057800010606     C                   EVAL      *IN28 = *ON
057900010606     C                   MOVEL     MSG(4)        V2CMSG
058000010606     C                   GOTO      EMETTOSF
058100010606     C                   ENDIF
058200060221e   2C                   endif
058300010604
058400010604      * F6=Conferma Record
058500060221if  2C                   IF        *INKF = *ON  and *in28 = *off
058600010606
058700060221if  3C                   if        S01nrr > *zeros
058800010606
058900060221     C                   eval      S01nrr = *zeros
059000161027
059100060221do  4C                   do        *hival
059200161027
059300060221     C                   eval      S01nrr = S01nrr + 1
059400060221     C     S01nrr        chain     TNTB11S                            31
059500010606     C                   if        *in31 = *on
059600010606     C                   leave
059700010606     C                   endif
059800010606
059900010606      * Salvataggio sul file
060000010606     C                   CLEAR                   DMSL
060100161027     C                   MOVEL     V3CDES        §MSLDES
060200161027     c                   clear                   §MSLpep
060300161027     c                   MOVE      V3Cpep        §MSLPEP
060400161027     C                   Z-ADD     V3CENT        §MSLENT
060500161027     c                   clear                   §MSLpke
060600161027     c                   if        V3Cpke = 'S'
060700161027     c                   movel     V3Cpke        §MSLpke
060800161027     c                   endif
060900161027     C                   MOVEL     V3CAPL        §MSLAPL
061000161027     c                   movel     V3CAp5        §MSLap5
061100161028     c                   movel     V3Cde5        §MSLde5
061200161027     c                   movel     V3Cpwf        §MSLpwf
061300010606
061400010606     C                   MOVEL     'MSL'         KCOD
061500010606     C                   MOVEL     V2CFIL        KKE1
061600060221     C                   Z-ADD     S01nrr        W3                3 0
061700010606     C                   MOVEL     W3            KKE2
061800161027
061900161027      *
062000161027      /free
062100161027
062200161027         // -?Apertura file tabelle del 1° S.I. (sede)?
062300161027         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
062400161027           w_SisInf = 1;
062500161027           exsr  sr_OpenFileTab;
062600161027         endif;
062700161027
062800161027         // -?Ciclo di elaborazione per ogni sistema informativo?
062900161104         For  w_SisInf = 1  To  %elem(sk_Libr);
063000161027
063100161027           // -?Apertura degli archivi?
063200161027           if  w_SisInf > 1;
063300161027             exsr  sr_OpenFileTab;
063400161027           endif;
063500161027
063600161027      /end-free
063700161027
063800010606     C     KTNTB1        CHAIN     TNTBE01L                           31
063900161027
064000010912      * Aggiorna file
064100060221if  5C                   IF        *IN31 = *OFF
064200010606
064300010606     C                   MOVEL     TBEUNI        DMSL_VD
064400010606
064500161011if  6c                   IF        TBEatb  <> V3Cann  or
064600160929     c                             dMSL_vd <> dMSL
064700161027
064800161027     c                   exsr      sr_RiempiCampi
064900010912
065000010606     C                   UPDATE    TNTBE000
065100161027
065200161011e   6C                   ENDIF
065300010912
065400010912      * Scrive file
065500060221x   5C                   ELSE
065600161027
065700010912     c                   clear                   tntbe000
065800161027
065900060221if  6C                   IF        (V3CDES <> *BLANKS AND V3CPEP <> *BLANKS) AND
066000161011     C                             (V3CENT <> *ZEROS AND V3CAPL <> *BLANKS and
066100161028     c                              V3Cap5 <> *blanks and V3Cde5 <> *blanks)
066200161027
066300010606     C                   MOVEL     'MSL'         TBECOD
066400161027     C*//                MOVEL     'MS'          TBEAPL
066500010606     C                   MOVEL     V2CFIL        TBEKE1
066600010606     C                   MOVEL     W3            TBEKE2
066700161027
066800161027     c                   exsr      sr_RiempiCampi
066900010912
067000010606     C                   WRITE     TNTBE000
067100161027
067200060221e   6C                   ENDIF
067300161027
067400060221e   5C                   ENDIF
067500161027
067600161027      /free
067700161027
067800161027         EndFor;
067900161027
068000161027      /end-free
068100010606
068200060221e   4C                   enddo
068300010606
068400161027     C                   UNLOCK    TNTBE01L
068500010606     C                   LEAVE
068600010606
068700060221e   3C                   ENDIF
068800060221e   2C                   endif
068900010604
069000010604     C                   GOTO      EMETTOSF
069100001009
069200060221e   1C                   enddo
069300001009
069400001009     C                   endsr
069500161027
069600161027      /free
069700161027
069800161027       //--------------------------------------------------------------
069900161027       //?Apertura del file tabelle nel sistema informativo impostato. ?
070000161027       //--------------------------------------------------------------
070100161027       BEGSR  sr_OpenFileTab;
070200161027
070300161027         // -?Chiusura (eventuale) file tabelle?
070400161027         if  %open(TNTBE01L);
070500161027           close  TNTBE01L;
070600161027         endif;
070700161027
070800161027         // -?Apertura file tabelle?
070900161104         ds_Libr  = sk_Libraries(w_iSystem);
071000161104         wLibFile = %trimr( sk_Libr(w_SisInf) ) + '/' + 'TNTBE01L';
071100161027         open  TNTBE01L;
071200161027
071300161027       ENDSR;
071400161027
071500161027       //--------------------------------------------------------------
071600161027       //?Impostazione dati nel record in aggiornamento.               ?
071700161027       //--------------------------------------------------------------
071800161027       //? N.B.                                                        ?
071900161027       //?L'annullamento  ed  il ripristino  vanno lasciati in         ?
072000161027       //?  trasmissione (vedi flag TBEFTR).                           ?
072100161027       //?L'aggiornamento  e  l'inserimento  NO: vanno registrati      ?
072200161027       //?  subito nello stesso file di entrambi i S.I. - in due       ?
072300161027       //?  cicli diversi - ma NON vanno messi in trasmissione.        ?
072400161027       //--------------------------------------------------------------
072500161027       BEGSR  sr_RiempiCampi;
072600161027
072700161027         // -?Dati della tabella?
072800161027         TBEuni = dMSL;
072900161027         TBEatb = V3Cann;
073000161027
073100161027         // -?Dati della matrice?
073200161027         TBEapl = sav_tbeAPL;
073300161027         //*·TBEftt = sav_TBEftt;
073400161027         TBEftt = 'S';
073500161027         //*·TBEflt = sav_TBEflt;
073600161027         clear  TBEflt;
073700161027
073800161027         // -?Dati della "trasmissione"?
073900161027if  6    If  V3Cann = *blank  and  V3Cann = V3annH;
074000161027           if  w_SisInf = 1;
074100161027             TBEftr = 'T';
074200161027           else;
074300161027             TBEftr = 'R';
074400161027           endif;
074500161027         Else;
074600161027           clear  TBEftr;
074700161027         EndIf;
074800161027         TBEdtr = wDate;
074900161027
075000161027       ENDSR;
075100161027
075200161027      /end-free
075300161027
075400001009      **********************************************************************
075500001009      * CARICO SUBFILE
075600001009      **********************************************************************
075700001009     C     Sr_Carsfl     begsr
075800001009
075900161011     c                   clear                   TNTB11S
076000161011
076100010601     C                   clear                   v3scel
076200010606     C                   eval      v3CDES = §MSLDES
076300161028     c                   eval      V3Cde5 = §MSLde5
076400031223     c                   move      §MSLPEP       V3Cpep
076500010601     C                   eval      v3CENT = §MSLENT
076600010919     C                   eval      v3CAPL = §MSLAPL
076700161025     c                   eval      V3Cap5 = §MSLap5
076800031223     c                   eval      V3Cpke = §MSLpke
076900160914     c                   eval      V3Cpwf = §MSLpwf
077000010601     C                   eval      v3CANN = TBEATB
077100010606     C                   eval      v3DESH = §MSLDES
077200161028     C                   eval      V3de5H = §MSLde5
077300010606     C                   eval      v3PEPH = §MSLPEP
077400010606     C                   eval      v3ENTH = §MSLENT
077500010919     C                   eval      v3APLH = §MSLAPL
077600161025     C                   eval      V3ap5H = §MSLap5
077700031223     C                   eval      V3pkeh = §MSLpke
077800161011     c                   eval      V3pwfH = §MSLpwf
077900010606     C                   eval      v3ANNH = TBEATB
078000001009
078100010601     C                   IF        V3CANN <> *BLANKS
078200010601     C                   SETON                                        30
078300010601     C                   ENDIF
078400010601
078500010604     C                   MOVEL     TBEKE2        W3                3 0
078600060221     C                   Z-ADD     W3            S01nrr
078700010601     C                   write     TNTB11S
078800001009
078900010604     C                   SETOFF                                       30
079000010604
079100001009     C                   endsr
079200010601      **********************************************************************
079300010601      * CARICO SUBFILE VUOTO
079400010601      **********************************************************************
079500010601     C     Sr_CARVUO     begsr
079600010601
079700161011     c                   clear                   TNTB11S
079800010601
079900060221     C                   ADD       1             S01nrr
080000010601     C                   write     TNTB11S
080100010601
080200010601     C                   endsr
080300161012      /free
080400161012
080500161012       //--------------------------------------------------------------
080600161012       //?Visualizzazione postazioni finora gestite.                   ?
080700161012       //--------------------------------------------------------------
080800161012       BEGSR  sr_GesS05;
080900161012
081000161012         // -?Inizializzazione videata?
081100161014         if  $InzS05;
081200161014           exsr  sr_InzS05;
081300161014           $InzS05 = *off;
081400161014           clear  C05csr;
081500161014         endif;
081600161012
081700161012         // -?Posizionamento cursore?
081800161012         if  C05csr > *zero;
081900161012           C05rcd = C05csr;
082000161012         else;
082100161012           C05rcd = 1;
082200161012         endif;
082300161014
082400161014         // -?Emissione Piede?
082500161014         write  TNTB11P5;
082600161012
082700161012         // -?Emissione videata?
082800161012         exfmt  TNTB11C5;
082900161012
083000161012         clear  *in90;
083100161012         clear  *in28;
083200161012         clear  V2Cmsg;
083300161012
083400161012       ENDSR;
083500161012
083600161012       //--------------------------------------------------------------
083700161012       //?Preparazione videata con le postazioni finora gestite.       ?
083800161012       //--------------------------------------------------------------
083900161012       BEGSR  sr_InzS05;
084000161012
084100161012         // -?Pulizia subfile?
084200161012         clear  S05nrr;
084300161012         *in25 = *off;
084400161012         *in26 = *off;
084500161012         *in27 = *off;
084600161012         write  TNTB11C5;
084700161012         *in25 = *on;
084800161012         *in26 = *on;
084900161012
085000161012         // -?Caricamento subfile?
085100161012         if  S01nrr <= *zero;
085200161012           leavesr;
085300161012         endif;
085400161012
085500161012         S05nrr = 1;
085600161012         chain  S05nrr  TNTB11S;
085700161012
085800161012         DoW  %found( TNTB11D );
085900161012
086000161012           clear  TNTB11S5;
086100161012
086200161012           if  V3Cann = *blank;
086300161012
086400161012             V5Cdes = V3Cdes;
086500161012             V5Cpep = V3Cpep;
086600161012             V5Cent = V3Cent;
086700161012             V5Capl = V3Capl;
086800161025             V5Cap5 = V3Cap5;
086900161012             V5Cpke = V3Cpke;
087000161012             V5Cpwf = V3Cpwf;
087100161012             //*·V5Cann = V3Cann;
087200161012
087300161012           endif;
087400161012
087500161012           write  TNTB11S5;
087600161012
087700161012           S05nrr += 1;
087800161012           chain  S05nrr  TNTB11S;
087900161012
088000161012         EndDo;
088100161012
088200161012         *in27 = *on;
088300161012
088400161012       ENDSR;
088500161012
088600161012      /end-free
088700980923      *****************************************************************
088800980923      * ROUTINE INIZIALE
088900980923      *****************************************************************
089000980923     C     *INZSR        BEGSR
089100980923      *
089200980521     C     *ENTRY        PLIST
089300980521     C                   PARM                    KPJBA
089400161027      *
089500161027      /free
089600161027
089700161027         $Ok_Sys = *off;
089800161027
089900161027         // -?Verifica del sistema AS/400 corrente?
090000161027         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
090100161027           leavesr;
090200161027         endif;
090300161027
090400161027         // -?Impostazione elenco librerie in cui gestire le tabelle?
090500161027         //  ?(a seconda del sistema in cui si stà lavorando)?
090600161104         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : sk_iSystem );
090700161027         if  w_iSystem = *zero;
090800161027           leavesr;
090900161027         endif;
091000161027
091100161027         $Ok_Sys = *on;
091200161027
091300161027         // -?1ª apertura file TABEL00F del 1° S.I. (sede)?
091400161027         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
091500161027           w_SisInf = 1;
091600161027           exsr  sr_OpenFileTab;
091700161027         endif;
091800161027
091900161027         // -?Reperimento data odierna?
092000161027         wDate = %int( %subst( %char( %dec( %timestamp() ) )
092100161027                               : 1 : 8 ) );
092200161027
092300161027      /end-free
092400060221      *
092500060221     c     *dtaara       define    §azute        azuteds
092600060221     c     *dtaara       define    §datiute      ddatiute
092700060221      *
092800060221     c                   in(E)     *dtaara
092900060221     c                   IF        %ERROR or RSUT = *blanks
093000060221     c                   clear                   Tibs34Ds
093100060221     c                   call      'TIBS34R'
093200060221     c                   parm                    Tibs34Ds
093300060221     c                   in        *dtaara
093400060221     c                   ENDIF
093500001221
093600010601     C     KTNTBE        KLIST
093700010604     C                   KFLD                    KCOD
093800010601     C                   KFLD                    KKE1
093900010510
094000010604     C     KTNTB1        KLIST
094100010604     C                   KFLD                    KCOD
094200010604     C                   KFLD                    KKE1
094300010604     C                   KFLD                    KKE2
094400010912
094500010912     c     ktntb2        klist
094600010912     c                   kfld                    kcod
094700010912     c                   kfld                    kke1
094800010912     c                   kfld                    kke2
094900010912     c                   kfld                    klin
095000010912     c                   kfld                    ksif
095100010912
095200010912      * recupro da subito i dati relativi alla matrice
095300010912      * prima con il sistema informativo
095400161027     c                   clear                   sav_tbeAPL
095500161027     c                   clear                   sav_tbeFTT
095600010912     c                   clear                   kcod
095700010912     c                   eval      kke1 = *all'0'
095800010912     c                   move      'MSL'         kke1
095900010912     c                   clear                   kke2
096000010912     c                   clear                   klin
096100010912     c                   movel     knsif         ksif
096200010912     c     ktntb2        chain     tntbe01l
096300161027      * poi senza il sistema informativo
096400161027     c                   if        NOT %found(tntbe01l)
096500010912     c                   clear                   ksif
096600010912     c     ktntb2        chain     tntbe01l
096700161027     c                   endif
096800010912     c                   if        %found(tntbe01l)
096900161027     c                   eval      sav_tbeAPL = tbeAPL
097000010912     c                   eval      sav_tbeftt = tbeftt
097100161027     c*//                eval      sav_tbeflt = tbeflt
097200010912     c                   endif
097300010604
097400001006     C                   endsr
097500980521      *---------------------------------------------------------------------------------------------
097600161027
097700161027       //--------------------------------------------------------------
097800161027       //?Definizione schiere a tempo di compilazione                  ?
097900161027       //--------------------------------------------------------------
098000161027
098100161104** --?sk_iSystem / sk_Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
098200161027SETRAS  GAITRAGRU FILTRAGRU
098300161027AS888   GAITRAGRPSFILTRAGRPF
098400980521** MSG  Lungh. 78                                                            *
098500161014Inserire tutti i dati                                                           1
098600161014Impossibile ripristinare un record già valido                                   2
098700161014Record già annullato                                                            3
098800161014Non è possibile cancellare un record esistente                                  4
098900161014Codice tipo applicazione non valido                                             5
099000161014Tipo applicazione non valido per Software VDL                                   6
099100161014Entrata picking ammessa solo per applicazione di FV 2 = "K"                     7
099200161014Inserite più di 45 postazioni (numero massimo inseribile)         - LIBERO -    8
099300161104Postazione Entrata già inserita nella posizione n°                              9
