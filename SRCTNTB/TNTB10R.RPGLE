000100091130     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200091130     h dftactgrp(*no)
000300091130     h bnddir('UBRTVNETA')
000400980909      *------------------------------------------------------------------------*
000500010521      * GESTIONE TABELLA CONTROLLO BARCODE CLIENTE DISK C                      *
000600980909      *------------------------------------------------------------------------*
000700110803      *?  ATTENZIONE!!  ?
000800110803      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000900110803      *?    'Interrogazione abilitazioni clienti'              ?
001000110803      *?    In caso di aggiunta/modifica campi alla tabella    ?
001100110803      *?    verificare se sono validi per la visualizzazione   ?
001200110803      *?    da TA61                                            ?
001300980909
001400980909      *--------------------------------------------
001500980909      *  RIEPILOGO INDICATORI
001600980909      *--------------------------------------------
001700981102      * 01 - RECORD annullato
001800991001      * 02 - acceso IMMISSIONE - spento MANUTENZIONE
001900990305      * 30 - Comodo
002000980909      *--------------------------------------------
002100980909
002200091130     fTNTBE01L  Uf A e           k disk
002300091130     f                                     extfile(wLibFile)  usropn
002400091130      *
002500010521     FTNTB10D   CF   E             WORKSTN
002600091130
002700091130       //--------------------------------------------------------------
002800091130       //?Definizione costanti.                                        ?
002900091130       //--------------------------------------------------------------
003000091130
003100091130       // - Costanti per la definizione delle schiere con i nomi
003200091130       //   degli iSeries da elaborare e delle relative librerie
003300091130     d c_NrSyst        c                   const(2)
003400091130     d c_NrLibr        c                   const(2)
003500091130
003600091130       //--------------------------------------------------------------
003700091130       //?Definizione schiere.                                         ?
003800091130       //--------------------------------------------------------------
003900091130
004000091130       // - iSeries  &  Librerie con entrambi i file tabelle
004100091130     d $iSystem        s                   like(currSysNetA)
004200091130     d                                     dim(c_NrSyst)
004300091130     d                                     ctdata   perrcd( 1)
004400091130     d $SisInf         s                   like(ds_Libr)
004500091130     d                                     dim(c_NrSyst)
004600091130     d                                     alt($iSystem)
004700091130
004800010607     D MSG             S             78    DIM(8) CTDATA PERRCD(1)              MSG VIDEO
004900091130
005000091130       //--------------------------------------------------------------
005100091130       //?Definizione strutture dati.                                  ?
005200091130       //--------------------------------------------------------------
005300091130
005400010521     d TIBS69DS      E DS
005500010521     d TIBS02DS      E DS
005600010521     d DS_cnaco      E DS                  extname(CNACO00F)
005700010521     d DS_cnind      E DS                  extname(CNIND00F)
005800010521     d DS_cnclp      E DS                  extname(CNCLP00F)
005900010521     d DS_fncls      E DS                  extname(FNCLS00F)
006000010521     D DDKC          E DS
006100980507     D KPJBA         E DS
006200110803     d TNTA61ds      e ds
006300091130
006400091130       // - Parametri per Reperimento dati utente
006500091130     d TIBS34ds      e ds                  inz
006600091130
006700091130       // - Ridefinizione elenco librerie in elaborare le tabelle
006800091130     d ds_Libr         ds                  inz
006900091130     d  $Libr                        10    dim(c_NrLibr) inz
007000020321     D***
007100020321     D* DS DI PROCEDURA
007200020321     D***
007300020321     D PARAM           DS
007400020328     D  PARKSC                        7S 0 INZ
007500091130
007600091130       // - Status ds
007700091130     d Status         sds
007800091130     d  SDSpgm           *proc
007900091130
008000091130       //--------------------------------------------------------------
008100091130       //?Definizione variabili globali.                               ?
008200091130       //--------------------------------------------------------------
008300091130
008400091130       // - Flags booleani
008500091130     d $Fine           s               n   inz(*off)
008600091130
008700091130       // - Nome del sistema
008800091130     d currSysNeta     s              8a   inz
008900091130
009000091130       // - Nome esteso Libreria/File dei 2 file tabella
009100091130     d wLibFile        s             21a   inz
009200091130
009300091130       // - Campi di comodo
009400091130     d w_iSystem       s              1  0 inz
009500091130     d w_SisInf        s              3  0 inz
009600091130     d wSavIn28        s               n   inz
009700110907     d wSavIn10        s               n   inz
009800091130
009900091130       //--------------------------------------------------------------
010000091130       //?Definizione aree dati.                                       ?
010100091130       //--------------------------------------------------------------
010200091130
010300091130       // - Dati utente
010400091130     d §AzUte        e ds                  extname(AZUTE00F)
010500091130     d                                     dtaara
010600091130     d §DatiUte      e ds                  extname(dDatiUte)
010700091130     d                                     dtaara
010800091130
010900091130       //--------------------------------------------------------------
011000091130       //?Definizione prototipi procedure.                             ?
011100091130       //--------------------------------------------------------------
011200091130
011300091130       // - Reperimento dati utente
011400091130      /copy gaitrasrc/srcProtoPR,TIBS34R
011500091130
011600091130       // - Reperimento NETA sistema AS/400 corrente
011700091130      /copy gaitrasrc/srcProtoPr,UBRTVNETA
011800091130
011900091130      ******************************************************
012000091130
012100091130     c                   exsr      sr_RoutInz
012200110803
012300110803     c                   eval      *in10 = *on
012400110803     c                   Select
012500110803     c                   When      %parms() > 1  and  TA61ksc <= *zero
012600110803     c                   eval      TA61err = *on
012700110803     c                   eval      TA61msg = 'Richiesto il codice cliente'
012800110803     c                   goto      Fine
012900110803     c                   When      %parms() > 1
013000110803     c                   eval      parksc  = TA61ksc
013100110803     c                   eval      *in10 = *off
013200110803     c                   EndSl
013300091130
013400091130      * Gestione 1.a videata (SE non sono stati ricevuti parametri)
013500091130if  1C     PARKSC        IFEQ      *ZEROS
013600020321      *
013700091130      * - EMISSIONE PRIMA VIDEATA
013800010522     C                   MOVE      '?'           V1CCCM
013900091130do  2C                   DO        *hival
014000091130     c                   clear                   V1Topz
014100091130     c                   write     TB10T01
014200091130     c                   write     TB10P01
014300091130     C                   EXFMT     TB10D01
014400980930      *
014500091130     C                   clear                   V1DMSG
014600991006     C                   eval      *in90 = *off
014700010525     C                   eval      *in28 = *off
014800980930      *
014900091130      * - F3=FINE
015000981117     c   KC              leave
015100981102      *
015200091130      * - CONTROLLI VIDEATA
015300010521     C                   EXSR      CTR01
015400991001     C   90              iter
015500010521      *
015600010521     c                   exsr      rie01
015700010521     C   90              iter
015800980506      *
015900091130     C                   CLEAR                   V1DMSG
016000980909      * IMPOSTO CAMPI SECONDA VIDEATA
016100010521     C                   EXSR      RIE02
016200981117      *
016300981117      * GESTIONE SECONDA VIDEATA
016400010521     C                   EXSR      GES02
016500091130e   2c                   enddo
016600091130      *
016700091130      * Gestione 2.a videata (SE sono stati ricevuti parametri)
016800091130x   1C                   ELSE
016900091130      *
017000020321     C                   MOVEL     PARKSC        V1CCCM
017100020321     C                   EXSR      RIE01
017200091130     C                   CLEAR                   V1DMSG
017300020321      * IMPOSTO CAMPI SECONDA VIDEATA
017400020321     C                   EXSR      RIE02
017500020321      *
017600020321      * GESTIONE SECONDA VIDEATA
017700020321     C                   EXSR      GES02
017800091130e   1C                   ENDIF
017900110803
018000110803     c     fine          tag
018100981102      *
018200981102      *****************************************************************
018300981102      *   ROUTINE PER CONTROLLO PRIMA VIDEATA
018400981102      *****************************************************************
018500010521     C     CTR01         BEGSR
018600091130     C                   CLEAR                   V1DMSG
018700981102      *
018800010521     c* controllo codice cliente
018900010522     c                   move      v1cccm        wv1cccm           1
019000010522     C                   IF        wV1CCCM='?'
019100010521     c                   clear                   TIBS02DS
019200991001     c
019300010521     c                   movel     'R'           T02MOD
019400010521     c                   movel     'DKC'         T02COD
019500010521     c                   movel     KNSIF         T02SIF
019600010521     c                   call      'TIBS02R'
019700010522     c                   parm                    KPJBA
019800010521     c                   parm                    TIBS02DS
019900991001     c
020000010521     c                   if        T02ERR<>' '
020100991001     c                   seton                                        90
020200010521     c                   else
020300010522     c                   movel     t02ke1        v1cccm
020400010522      *riemetto la videata
020500010522     c                   seton                                        90
020600991001     c                   endif
020700010521     C                   ENDIF
020800010521      *
020900981102     C                   ENDSR
021000010521      *****************************************************************
021100010521      *   ROUTINE PER REPERIRE LA RAGIONE SOCIALE DEL CLIENTE
021200010521      *****************************************************************
021300010521     C     RIE01         BEGSR
021400010521     C                   SETOFF                                       90
021500010521      *
021600010522     C                   IF        V1CCCM=*zeros
021700010525     C                   SETON                                        9028
021800091130     C                   MOVEL     MSG(1)        V1DMSG
021900010521     C                   GOTO      ENDRIE
022000010521     C                   ENDIF
022100010521      *
022200010522     C                   IF        V1CCCM=*blanks
022300010525     C                   SETON                                        9028
022400091130     C                   MOVEL     MSG(1)        V1DMSG
022500010521     C                   GOTO      ENDRIE
022600010521     C                   ENDIF
022700010521      *CONTROLLO SE ESISTE IL CODICE
022800010521     C                   CLEAR                   TIBS69DS
022900091130     C                   z-add     dutKCI        I69KCC
023000010521     C                   MOVEL     V1CCCM        I69KAC
023100010521     C                   MOVEL     KNSIF         I69SIF
023200010521     C                   CALL      'TIBS69R'
023300010521     C                   PARM                    TIBS69DS
023400010521     C                   PARM                    DS_CNACO
023500010521     C                   PARM                    DS_CNIND
023600010521     C                   PARM                    DS_CNCLP
023700010521     C                   PARM                    DS_FNCLS
023800010521     C* ERRORE
023900010521     C                   IF        O69ERR<>' '
024000091130     C                   MOVEL     MSG(8)        V1DMSG
024100010525     C                   SETON                                        9028
024200010521     C                   GOTO      ENDRIE
024300010521     C                   ENDIF
024400010521     C*
024500010522     C* VEDO SE ESISTE RECORD IN TNTBE
024600091201      *
024700091201      /free
024800091201
024900091201         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
025000091201         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
025100091201           w_SisInf = 1;
025200091201           exsr  sr_OpenFileTab;
025300091201         endif;
025400091201
025500091201      /end-free
025600091130     c                   exsr      sr_ChainTNTBE
025700010522      *
025800010521      * PER CLIENTE NUOVO NON PUO' ESSERE ANULLATO O BLOCCATO
025900010521     C* CODICE  ANNULLATO
026000010612     C   02              IF        ACOFLG<>' '
026100091130     C                   MOVEL     MSG(2)        V1DMSG
026200010612     C                   SETON                                        9028
026300010521     C                   GOTO      ENDRIE
026400010521     C                   ENDIF
026500010521     C* CODICE BLOCCATO
026600130315     C   02              IF        ACOABL <> *blanks
026700091130     C                   MOVEL     MSG(2)        V1DMSG
026800010525     C                   SETON                                        9028
026900010521     C                   GOTO      ENDRIE
027000010521     C                   ENDIF
027100010521      *
027200010521     C                   MOVEL     ACORAG        V1DCCM
027300010521      *
027400010521     C     ENDRIE        TAG
027500010521     C                   ENDSR
027600091130      *
027700091130      *****************************************************************
027800091130      *   AGGANCIO RECORD DA AGGIORNARE IN TNTBE01L
027900091130      *****************************************************************
028000091130     c     sr_ChainTNTBE BEGSR
028100091201      *
028200091130     c                   clear                   TNTBE000
028300091130      *
028400091130     c                   movel     'DKC'         TBECOD
028500091130     c                   movel(P)  V1CCCM        TBEKE1
028600091130     C                   MOVE      *blanks       TBEKE2
028700091130     C                   MOVE      *blanks       TBELIN
028800091130     C                   MOVE      *blanks       TBESIF
028900091130      *
029000091130     c     KTBE          CHAIN     TNTBE000                           02
029100091130      *
029200091130     c                   ENDSR
029300980506      *****************************************************************
029400980506      *   ROUTINE PER CARICAMENTO SECONDA VIDEATA DA FILE
029500980506      *****************************************************************
029600010521     C     RIE02         BEGSR
029700980909      *
029800091130      /free
029900091130
030000110907         wSavIn10 = *in10;
030100091130         clear  TB10D02;
030200091130
030300091130         // -?Impostazione testata?
030400091130         clear  V1Topz;
030500091130         select;
030600091130           when  Not %found(TNTBE01L);
030700091130             V1Topz = '  INSERIMENTO  ';
030800091203             *in01  = *off;
030900091130             *in02  = *on;
031000091130           when  TBEatb = 'A';
031100091130             V1Topz = '  RIPRISTINO   ';
031200091130             *in01  = *on;
031300091203             *in02  = *off;
031400091130           other;
031500091130             V1Topz = '   MODIFICA    ';
031600091203             *in01  = *off;
031700091203             *in02  = *off;
031800091130         endsl;
031900110803
032000110803         IF  %parms() > 1;
032100110803           V1Topz = '   VISUALIZZA  ';
032200110803         ENDIF;
032300110907
032400110907         *in10 = wSavIn10;
032500091130
032600091130      /end-free
032700010521     c                   clear                   DDKC
032800010521     c  N02              movel     TBEuni        DDKC
032900980909      *
033000010521     C                   IF        *IN02= *OFF
033100010521      *
033200010521     C                   z-add     §DKCMN        V2CCMN
033300010521     C                   z-add     §DKCMX        V2CCMX
033400010521     C                   MOVEL     §DKCIN        V2CCIN
033500010521     C                   z-add     §DKCIL        V2CCIL
033600980506      *
033700010521     C                   ENDIF
033800010521      *
033900980506     C                   ENDSR
034000981117      *****************************************************************
034100981117      *   ROUTINE PER GESTIONE SECONDA VIDEATA
034200981117      *****************************************************************
034300010521     C     GES02         BEGSR
034400981117      *
034500981117      * EMISSIONE
034600981117     C                   DO        *HIVAL
034700091130     c                   eval      wSavIn28 = *in28
034800091130     c                   eval      *in28    = *off
034900091130     c                   write     TB10T01
035000091130     c                   write     TB10D01
035100091130     c                   write     Protect
035200091130     c                   write     TB10P02
035300091130     c                   eval      *in28 = (wSavIn28 = *on)
035400110803     C                   EXFMT     TB10D02
035500981117      *
035600091130     C                   CLEAR                   V1DMSG
035700981117      *
035800010607     c                   setoff                                       9028
035900981117      * F3=FINE
036000091201     c   KC              unlock    TNTBE01L
036100981117     c   KC              leave
036200981117      *
036300981117      * F12=RITORNO
036400091201     c   KL              unlock    TNTBE01L
036500981117     c   KL              leave
036600981117      *
036700981117      * CONTROLLI VIDEATA
036800110803     C   10              EXSR      CTR02
036900981117      * errori
037000010521     C   90              iter
037100981117      *
037200981117      * GESTIONE CONFERME: F5(ke)=Ripristino; F6(kf)=Conferma; F16(kq)=Annullamento
037300981117     c                   IF        *inKE = *ON  or  *inKF = *ON  or  *inKQ = *ON
037400981117     c                   exsr      RIECDD
037500981117     C                   leave
037600981117     c                   ENDIF
037700981117      *
037800981117     c                   enddo
037900981117      *
038000981117     C                   endsr
038100010521      *****************************************************************
038200010521      *   ROUTINE PER CONTROLLO SECONDA VIDEATA
038300010521      *****************************************************************
038400010521     C     CTR02         BEGSR
038500010525     C                   SETOFF                                       902840
038600010525     C                   SETOFF                                       414243
038700010521      *
038800010521     C                   IF        V2CCMN=0
038900010525     C                   SETON                                        902840
039000091130     C                   MOVEL     MSG(3)        V1DMSG
039100010521     C                   GOTO      ENDCTR
039200010521     C                   ENDIF
039300010521      *
039400010521     C                   IF        V2CCMX=0
039500010525     C                   SETON                                        902841
039600091130     C                   MOVEL     MSG(4)        V1DMSG
039700010521     C                   GOTO      ENDCTR
039800010521     C                   ENDIF
039900010521      *
040000010521     C                   IF        V2CCMX<V2CCMN
040100010525     C                   SETON                                        902841
040200091130     C                   MOVEL     MSG(5)        V1DMSG
040300010521     C                   GOTO      ENDCTR
040400010521     C                   ENDIF
040500010521      *
040600010522     C     ' '           CHECKR    V2CCIN        XX                1 0
040700010522     C                   IF        XX>V2CCMX
040800091130     C                   MOVEL     MSG(6)        V1DMSG
040900010525     C                   SETON                                        902842
041000010522     C                   GOTO      ENDCTR
041100010522     C                   ENDIF
041200010522      *
041300010522     C                   IF        V2CCIL>0  AND V2CCIN=*blanks
041400010525     C                   SETON                                        902843
041500091130     C                   MOVEL     MSG(7)        V1DMSG
041600010521     C                   GOTO      ENDCTR
041700010521     C                   ENDIF
041800010521      *
041900010525     C                   IF        V2CCIL=0  AND V2CCIN<>*BLANKS
042000010525     C                   SETON                                        902843
042100091130     C                   MOVEL     MSG(7)        V1DMSG
042200010525     C                   GOTO      ENDCTR
042300010525     C                   ENDIF
042400010525      *
042500010522     C                   IF        XX<>V2CCIL
042600091130     C                   MOVEL     MSG(7)        V1DMSG
042700010607     C                   SETON                                        902843
042800010522     C                   GOTO      ENDCTR
042900010522     C                   ENDIF
043000010522     C
043100010521      *
043200010521      *
043300010521     C     ENDCTR        TAG
043400010521     C                   ENDSR
043500980507      *****************************************************************
043600980507      *   ROUTINE PER CARICAMENTO FILE DA VIDEO
043700980507      *****************************************************************
043800981117     C     RIECDD        BEGSR
043900980909      *
044000091130      /free
044100091130
044200091130         //? N.B.                                                      ?
044300091130         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
044400091130         //   trasmissione (vedi flag TBEFTR).                        ?
044500091130         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
044600091130         //   subito nello stesso file di entrambi i S.I. - in due    ?
044700091130         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
044800091130
044900091130         // -?Ciclo di elaborazione per ogni sistema informativo?
045000091130         For  w_SisInf = 1  To  %elem($Libr);
045100091130
045200091201           // -?Apertura del file TNTBE nel S.I. in gestione  e?
045300091201           //  ?reperimento dati tab. "DKC"?
045400091130           if  w_SisInf > 1;
045500091201             exsr  sr_OpenFileTab;
045600091130             exsr  sr_ChainTNTBE;
045700091130           endif;
045800091130
045900091130           // -?Inserim./Aggiorn.tab."DKC" = Controllo BarCode Cliente?  ?
046000091130           clear  dDKC;
046100091130      /end-free
046200091130      *
046300010521     C                   Z-ADD     V2CCMN        §DKCMN
046400010521     C                   Z-ADD     V2CCMX        §DKCMX
046500010521     C                   movel     V2CCIN        §DKCIN
046600010521     C                   Z-ADD     V2CCIL        §DKCIL
046700990308      *
046800010521     c                   movel(p)  DDKC          TBEuni
046900981102      *
047000010522     C                   movel     'S'           TBEftt
047100980507      *
047200981102     C   KQ              movel     'A'           TBEatb
047300981102     C   KE              movel     *blanks       TBEatb
047400980909      *
047500091203     ***c                   clear                   TBEftr
047600981102      *
047700981102     c                   IF        *IN02 = *ON
047800091130      *
047900991001     C                   z-add     0             TBEflt
048000991001     c                   clear                   TBEdtr
048100091203     c                   clear                   TBEftr
048200010522     c                   movel     *blanks       TBEsif
048300010522     c                   movel     *blanks       TBElin
048400010525     c                   movel     'DC'          TBEapl
048500010521     c                   movel     'DKC'         TBEcod
048600010521     C                   movel(p)  V1CCCM        TBEke1
048700981102     C                   WRITE     TNTBE000
048800981102      *
048900981102     c                   ELSE
049000091130      *
049100091203      /free
049200091203             select;
049300091203               when  *inKQ  or  *inKE;
049400091203                 clear  TBEftr;
049500091203                 clear  TBEdtr;
049600091203               when  w_SisInf = 1;
049700091203                 TBEftr = 'T';
049800091203                 TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
049900091203                                        : 1 : 8 ) );
050000091203               other;
050100091203                 TBEftr = 'R';
050200091203                 TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
050300091203                                        : 1 : 8 ) );
050400091203             endsl;
050500091203      /end-free
050600981102     C                   UPDATE    TNTBE000
050700091130      *
050800981102     c                   ENDIF
050900091130      /free
051000091130         EndFor;
051100091130      /end-free
051200980507      *
051300980507     C                   ENDSR
051400980506      *****************************************************************
051500980506      *   ROUTINE INIZIALE
051600980506      *****************************************************************
051700091130     C     sr_RoutInz    BEGSR
051800980506      *
051900980506     C     *ENTRY        PLIST
052000980506     C                   PARM                    KPJBA
052100110803     c                   parm                    TNTA61ds
052200981102      *
052300020328     C                   IF        KPJBU <> *BLANKS
052400020321     C                   MOVEL     KPJBU         PARAM
052500020328     C                   ENDIF
052600020321      *
052700091130      /free
052800091130
052900091130         *inLR = *on;
053000091130
053100091130         // -?Verifica del sistema AS/400 corrente?
053200091130         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
053300091130           $Fine = *on;
053400091130           leavesr;
053500091130         endif;
053600091130
053700091130         // -?Impostazione elenco librerie in cui gestire le tabelle?
053800091130         //  ?(a seconda del sistema in cui si stà lavorando)?
053900091130         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
054000091130         if  w_iSystem = *zero;
054100091130           $Fine = *on;
054200091130           leavesr;
054300091130         endif;
054400091130
054500091130         // -?Reperimento dati job?
054600091130         exsr  sr_DatiJob;
054700091130
054800091130         // -?Impostazione nome programma a video?
054900091130         V1Tpgm = SDSpgm;
055000091130
055100091130      /end-free
055200981102      *
055300010521     C* POSIZIONAMENTO TABELLE
055400010521     C     KTBE          KLIST
055500010521     C                   KFLD                    TBECOD
055600010521     C                   KFLD                    TBEKE1
055700010521     C                   KFLD                    TBEKE2
055800010521     C                   KFLD                    TBELIN
055900010521     C                   KFLD                    TBESIF
056000010521      *
056100980506     C                   ENDSR
056200091130      /free
056300091130
056400091130       //--------------------------------------------------------------
056500091130       //?Reperimento Dati del job (Utente/Operativi).                 ?
056600091130       //--------------------------------------------------------------
056700091130       BEGSR  sr_DatiJob;
056800091130
056900091130         in(e) §AzUte;
057000091130         if NOT %error;
057100091130           in(e) §DatiUte;
057200091130         endif;
057300091130         if %error or RSut = *blank;
057400091130           tibs34r ( tibs34ds );
057500091130           in §AzUte;
057600091130           in §DatiUte;
057700091130         endif;
057800091130
057900091130       ENDSR;
058000091130
058100091130       //--------------------------------------------------------------
058200091130       //?Apertura dei files tabelle nel sistema informativo impostato.?
058300091130       //--------------------------------------------------------------
058400091130       BEGSR  sr_OpenFileTab;
058500091130
058600091130         // -?Chiusura (eventuale) archivi?
058700091130         if  %open(TNTBE01L);
058800091130           close  TNTBE01L;
058900091130         endif;
059000091130
059100091130         // -?Apertura archivi?
059200091130         ds_Libr  = $SisInf(w_iSystem);
059300091130         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
059400091130         open  TNTBE01L;
059500091130
059600091130       ENDSR;
059700091130
059800091130      /end-free
059900091130
060000091130       //--------------------------------------------------------------
060100091130       //?Schiere a tempo di compilazione.                             ?
060200091130       //--------------------------------------------------------------
060300091130
060400091130** - $iSystem / $SisInf:?Sistemi AS/400 & Librerie con entrambi i file?
060500091130SETRAS  GAITRAGRU FILTRAGRU
060600091130AS888   GAITRAGRPSFILTRAGRPF
060700980909** MSG  (Lungh. 78)                                                          *
060800010521Immettere il Codice Cliente                                                    1
060900010521Cliente annullato o bloccato                                                   2
061000010521Immettere lunghezza minima                                                     3
061100010521Immettere lunghezza massima
061200010521Lunghezza massima inferiore alla minima
061300010521Caratteri iniziali superiori alla lunghezza massima
061400010521Caratteri iniziali non corrispondenti alla lunghezza
061500010607Codice errato
