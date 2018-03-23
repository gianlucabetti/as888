000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500010109      *       MODIFICA DATA ELABORAZIONE RITIRI FISSI                 *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001006
000900001010      ****************************************************************
001000001010      *  RIEPILOGO INDICATORI
001100001010      ****************************************************************
001200010108      * 05 - SPROTEGGO P.O. IN GESTIONE
001300001010      * 28 - ERRORE GENERICO DSPF
001400001010      * 30 - Comodo
001500010108      * 40 - ERRORE p.o. in gestione
001600010109      * 46 - ERRORE Data elaborazione
001700001010      ****************************************************************
001800001010
001900010109     FFNORS01L  UF   E           K DISK
002000001011     FAZORG01L  IF   E           K DISK
002100010109     FFIOR14D   CF   E             WORKSTN
002200001010
002300001010      *------------------------------------------------------------------------*
002400001026     D kazorg          S                   LIKE(ORGfil)
002500001030
002600010109     D vdela           S                   LIKE(v1dela)
002700010108
002800010109     D §doggi          S                   LIKE(v1dela)
002900001025     D §kpjbu          S                   LIKE(KPJBU)
003000001009
003100001006     D W0140           S             14  0
003200001006     D Wdtgio          S              8  0
003300001006     D dateu           S              8  0
003400001009
003500001006     D DATA_oggi       S               D   DATFMT(*eur)
003600130702
003700130702     d wabi            s                   like(UTEaut)
003800130702     d wEsci           s               n   inz(*off)
003900130702     d wErrGrave       s               n   inz(*off)
004000130702     d w003a           s              3
004100001006
004200001010      *  titolo videata (lunghezza massima 34)
004300010109     D TIT_A           C                   CONST('***  Mod.data elaborazione   -
004400001010     D                                       ***')
004500001031
004600001031      *   S C H I E R E
004700010109     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG VIDEO
004800130702     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
004900001006
005000001010      *   D S   I N T E R N E / E S T E R N E
005100001009
005200001009     D WLBDAT          DS                  INZ
005300001009     D  G02DAT                 1      8  0
005400001009     D  G02INV                 9     16  0
005500001009     D  G02ERR                17     17
005600001009     D  G02TGI                18     22  0
005700001009
005800130702     d Azuteds       e ds                  Extname(Azute00f)
005900130702     d dDatiute      e ds
006000130702     d Tibs34ds      e ds
006100130702
006200130702     d dLAT          e ds
006300130702     d dUTE01        e ds
006400130702
006500130702     d TIBS02DS      e ds
006600130702     d TRUL31DS      e ds
006700001010
006800001010      *
006900001010     D KPJBA         E DS
007000001010      *
007100001010     D                SDS
007200001010     D  VTCPGM                 1     10
007300001010
007400001006      *------------------------------------------------------------------------*
007500010109
007600010108
007700010108      * Emissione 1° Videata
007800010108
007900010108     C                   do        *hival
008000010109      * Pulizia Videata
008100010109     C                   if        (*in28 = *off  and  *in05 = *off)
008200010109     C                   exsr      Sr_Pulvid
008300010108     C                   endif
008400130702
008500130702      /free
008600130702       //?Se utente non autorizzato alla funzione esco
008700130702         IF  wErrGrave;
008800130702           *in28 = *on;
008900130702           wEsci = *on;
009000130702           V1Cmsg = 'Utente non autorizzato alla funzione';
009100130702         ENDIF;
009200130702      /end-free
009300010108
009400010109     C                   exfmt     fior141
009500130702
009600130702      /free
009700130702         IF  wEsci;
009800130702           leave;
009900130702         ENDIF;
010000130702      /end-free
010100010108
010200010108      * F3=Fine
010300010108     C   kc              leave
010400010109     C                   setoff                                       28
010500010108
010600010108      * F18=Cambio P.O. in gestione
010700010108     C   ks              eval      *in05 = *on
010800010108     C   ks              iter
010900010108
011000010108      * 05 on --> controllo P.O. in gestione
011100010108     C                   if        *in05 = *on
011200010108     C                   exsr      Sr_Contrpo
011300130702     C  n28              eval      *in05 = *off
011400130702     C   28              iter
011500010108     C                   endif
011600010109
011700010109      * Controlli
011800010109     C                   exsr      Sr_Contr
011900010109
012000010109      * F6=Elabora
012100010109     C   kf              exsr      Sr_Modifica
012200010109     C   kf
012300010109     Cann28              leave
012400010108
012500010108     C   28              iter
012600010108
012700010108     C                   enddo
012800001010
012900001006     C                   eval      *inlr = *on
013000010108      *****************************************************************
013100010109      * PULIZIA VIDEATA
013200010108      *****************************************************************
013300010109     C     Sr_Pulvid     BEGSR
013400010108
013500010109     C                   movel     data_oggi     v1dela
013600010108
013700010108     C                   endsr
013800010108      **********************************************************************
013900010108      * CONTROLLO IL P.O. IN IN GESTIONE
014000010108      **********************************************************************
014100010108     C     Sr_Contrpo    begsr
014200010108
014300010108     C                   setoff                                       40
014400010108     C                   clear                   v1dfgs
014500010108
014600130702     c                   move      V1Cfgs        w003a
014700130702     c     w003a         lookup    SKpog                                  30
014800010108     C     *in30         ifeq      *off
014900010108     C                   seton                                        40  28
015000010109     C                   movel     msg(3)        v1cmsg
015100130702     c                   leavesr
015200130702     C                   endif
015300010108
015400010108      * Decodifica
015500010108     C     v1cfgs        chain     azorg01L
015600010108     C                   if        %found(azorg01l) and ORGfva = *blanks
015700010108     C                   movel     orgDES        v1dfgs
015800010108     C                   else
015900010108     C                   seton                                        40  28
016000010109     C                   movel     msg(3)        v1cmsg
016100010108     C                   endif
016200010108
016300010108     C                   endsr
016400010108      **********************************************************************
016500010109      * CONTROLLI VIDEO
016600010108      **********************************************************************
016700010109     C     Sr_Contr      begsr
016800010108
016900010108      * Reset indicatori
017000010108
017100010109     C                   setoff                                       41
017200010108
017300010109     C                   if        v1dela =  *zeros
017400010109     C                   seton                                        41  28
017500010109     C                   movel     msg(1)        v1cmsg
017600010109     C                   else
017700010109     C                   exsr      Sr_Contrdela                                 controllo data ela.
017800010108     C                   endif
017900010108
018000010109     C                   endsr
018100010108      **********************************************************************
018200010109      * Controlla data elaborazione
018300010108      **********************************************************************
018400010109     C     Sr_Contrdela  BEGSR
018500010108
018600010108     C                   clear                   wlbdat
018700010109     C                   z-add     v1dela        G02dat
018800010108     C                   call      'XSRDA8'
018900010108     C                   parm                    wlbdat
019000010108     C                   if        G02err = '1'
019100010109     C                   eval      *in41 = *ON
019200010108     C                   eval      *in28 = *ON
019300010109     C                   movel     msg(2)        v1cmsg
019400010108     C                   else
019500010109     C                   z-add     G02dat        v1dela
019600010109     C                   z-add     G02inv        vdela
019700010108     C                   endif
019800010108
019900010109     C                   endsr
020000001031      **********************************************************************
020100010109      * Aggiorno data elaborazione nei ritiri fissi
020200001031      **********************************************************************
020300010109     C     Sr_Modifica   begsr
020400001031
020500010109     C     v1cfgs        setll     fnors01l
020600010109     C                   do        *hival
020700010109     C     v1cfgs        reade     fnors01l
020800010109     C                   if        %eof(fnors01l)
020900010109     C                   leave
021000010109     C                   endif
021100010109
021200010109     C                   if        ORSdte = vdela
021300010109     C                   clear                   ORSdte
021400040818     c                   Z-add     dateu         OrsDtt
021500010109     C                   update    fnors000
021600010109     C                   endif
021700010109
021800010109     C                   enddo
021900001031
022000001031     C                   endsr
022100001010      *****************************************************************
022200001010      * ROUTINE INIZIALE
022300001010      *****************************************************************
022400001010     C     *INZSR        BEGSR
022500001009
022600001010     C     *ENTRY        PLIST
022700001010     C                   PARM                    KPJBA
022800001011
022900001011     C                   movel     TIT_A         VTCtit
023000001016
023100130702     c     *dtaara       define    §azute        azuteds
023200130702     c     *dtaara       define    §datiute      ddatiute
023300130702     c                   in(e)     *dtaara
023400130702     c                   if        %error or rsut = *blanks
023500130702     c                   clear                   tibs34ds
023600130702     c                   call      'TIBS34R'
023700130702     c                   parm                    tibs34ds
023800130702     c                   in        *dtaara
023900130702     c                   endif
024000130702
024100130702      * controllo abilitazione utente
024200130702     c                   clear                   wabi
024300130702     c                   clear                   dLAT
024400130702     c                   eval      dUTE01 = UTEfaf
024500130702
024600130702      * Verifica errori e autorità profilo
024700130702     c                   SELECT
024800130702      * se ho errori nei dati utente esco dal pgm
024900130702     c                   WHEN      DUTerr = 'E'
025000130702     c                   eval      wErrGrave = *on
025100130702     c                   leavesr
025200130702      * carica abilitazioni del profilo
025300130702     c                   WHEN      §UTEorm <> *blanks
025400130702     c                   eval      wabi = §UTEorm
025500130702     c                   WHEN      UTEaut <> *blanks
025600130702     c                   eval      wabi = UTEaut
025700130702      * se non c'è l'abilitazione
025800130702      * --> se 1° livello, abilitazioni al terminal
025900130702      *     se 2° livello, abilitazioni al punto operativo
026000130702      *     se sede no abilitazioni
026100130702     c                   OTHER
026200130702     c                   IF        DUTlpo = '1'
026300130702     c                   eval      wabi   = 'TP'
026400130702     c                   ENDIF
026500130702     c                   IF        DUTlpo = '2'
026600130702     c                   eval      wabi   = 'PO'
026700130702     c                   ENDIF
026800130702     c                   ENDSL
026900130702
027000130702      * controllo se ok l'abilitazione dell'utente
027100130702     c                   clear                   TIBS02DS
027200130702     c                   eval      T02mod = 'C'
027300130702     c                   eval      T02sif = knsif
027400130702     c                   eval      T02cod = 'LAT'
027500130702     c                   eval      T02ke1 = wabi
027600130702     c                   Call      'TIBS02R'
027700130702     c                   Parm                    KPJBA
027800130702     c                   Parm                    TIBS02DS
027900130702     c                   IF        T02err <> *blanks or §LATabi = 'S'
028000130702     c                   eval      wErrGrave = *on
028100130702     c                   leavesr
028200130702     c                   ENDIF
028300130702     c                   eval      dLAT = T02uni
028400130702
028500130702      * Reperimento delle filiali gestibili dall'utente
028600130702     c                   clear                   TRUL31DS
028700130702     c                   eval      I31abi = wabi
028800130702     c                   eval      I31cdi = DUTdis
028900130702     c                   eval      I31car = DUTare
029000130702     c                   eval      I31cpo = DUTpou
029100130702     c                   call      'TRUL31R'
029200130702     c                   parm                    KPJBA
029300130702     c                   parm                    TRUL31DS
029400130702     c                   IF        O31pog > *zeros
029500130702     c                   movea     O31pog        SKpog
029600130702     c                   ELSE
029700130702     c                   eval      wErrGrave = *on
029800130702     c                   leavesr
029900130702     c                   ENDIF
030000010108
030100010108      * IMPOSTO IL P.O. DI GESTIONE
030200130702     C                   eval      V1Cfgs = DUTpou
030300001016
030400001019     C                   movel     v1cfgs        kazorg
030500001019     C     kazorg        chain     azorg01L
030600001016     C                   if        %found(azorg01l)
030700001016     C                   movel     orgDES        v1dfgs
030800001016     C                   else
030900001016     C                   clear                   v1dfgs
031000001016     C                   endif
031100001009
031200001023      * reperisce data e ora
031300001010     C                   TIME                    W0140
031400001010      * UDATE IN GGMMAAAA
031500001010     C                   MOVE      W0140         WDTGIO
031600001010      * UDATE IN AAAAMMGG
031700001010     C                   Z-ADD     WDTGIO        G02DAT
031800001010     C                   MOVEL     *BLANK        G02ERR
031900001010     C                   CALL      'XSRDA8'
032000001010     C                   PARM                    WLBDAT
032100001025     C                   Z-ADD     G02INV        DATEU
032200001010     C     *iso          MOVEL     DATEU         DATA_oggi
032300001010
032400001025     C                   z-add     dateu         §doggi
032500001006
032600001006     C                   endsr
032700001010      *---------------------------------------------------------------------------------------------
032800001031** MSG  Lungh. 78                                                            *
032900010109Inserire la data elaborazione                                                       1
033000010109Data elaborazione errata                                                            2
033100070418Filiale gestione errata                                                             3
