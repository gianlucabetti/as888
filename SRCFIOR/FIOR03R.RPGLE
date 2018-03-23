000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400001006      *                                                               *
000500001006      *       GESTIONE ORDINI RITIRO FISSI                            *
000600001006      *                                                               *
000700001006      *****************************************************************
000800001006
000900001010      ****************************************************************
001000001010      *  RIEPILOGO INDICATORI
001100001010      ****************************************************************
001200001009      * 01 - IMMISSIONE
001300001009      * 02 - MANUTENZIONE
001400001009      * 03 - VISUALIZZAZIONE
001500001009      * 04 - ANNULLATO - RIPRISTINARE
001600001010      * 05 - Dati cliente consegna protetti
001700060104      * 06 - Inibischio il richiamo al fior01r
001800010730      * 26 - Data inizio ferie errata
001900010730      * 26 - Data fine ferie errata
002000001010      * 28 - ERRORE GENERICO DSPF
002100001010      * 30 - Comodo
002200001110      * 31 - Comodo
002300001010      * 41 - Cliente ritiro obbligatorio o non valido
002400001010      * 42 - Cliente ordinante non valido
002500001010      * 43 - Colli obbligatori
002600001010      * 44 - Peso obbligatorio
002700001010      * 45 - Volume obbligatorio
002800001010      * 46 - Bancali obbligatori
002900001010      * 47 - Bilico obbligatorio
003000001010      * 48 - Autotreno obbligatorio
003100001010      * 49 - Motrice obbligatoria
003200001010      * 50 - Inserire almeno un dato (ritiro - ora - giorno)
003300001010      * 52 - Tipo ordine errato
003400001010      * 53 - Inserire i dati (pagamento - tassazione - tariffa)
003500001010      * 54 - Tassazione e tariffa errati
003600001010      * 55 - P.O. Consegna errato
003700001010      * 56 - Cliente consegna non valido
003800001012      * 58 - Ora errata
003900010131      * 59 - Almeno un gg della settimana
004000001010      * 60 - Posizione cursore Ragione Sociale
004100001010      * 61 - Posizione cursore Indirizzo
004200001010      * 62 - Posizione cursore CAP
004300001010      * 63 - Posizione cursore Località
004400001010      * 64 - Posizione cursore Provincia
004500001010      * 65 - Posizione cursore Nazione
004600001012      * 66 - Enter x forzare colli
004700001012      * 67 - Enter x forzare peso
004800001012      * 68 - Enter x forzare volume
004900001012      * 69 - Enter x forzare bancali
005000001012      * 70 - Enter x forzare bilico
005100001012      * 71 - Enter x forzare autotreno
005200001012      * 72 - Enter x forzare motrice
005300001010      * 90 - Riemissione videata
005400001010      ****************************************************************
005500001010
005600001011     FFNORS01L  UF A E           K DISK
005700070905     ffnors11l  uf a e           k disk
005800001011     FAZORG01L  IF   E           K DISK
005900001006     FFNACR01L  IF   E           K DISK
006000071108     fFNACR13L  if   e           k disk
006100001010     FTNTAM01L  IF   E           K DISK
006200001009     FFIOR03D   CF   E             WORKSTN
006300001010
006400001010      *------------------------------------------------------------------------*
006500001011     D kpoe            S                   LIKE(V1cpoe)
006600001006     D knor            S                   LIKE(V1nor)
006700001009
006800001011     D kfnacr          S                   LIKE(ACRcro)
006900001010
007000001010     D kksc            S                   LIKE(TAMksc)
007100001010     D kctr            S                   LIKE(TAMctr)
007200001011
007300001011     D kazorg          S                   LIKE(ORGfil)
007400001006
007500001011     D wmsg            S                   LIKE(O13msg)
007600001011     D wfzv            S                   LIKE(E13fzv)
007700001011     D wfz1            S                   LIKE(E13fz1)
007800001011     D wfz2            S                   LIKE(E13fz2)
007900001011     D wfz3            S                   LIKE(E13fz3)
008000001011
008100010119     D sav_acrfcl      S                   LIKE(ACRfcl)
008200010119     D sav_acrfpk      S                   LIKE(ACRfpk)
008300010119     D sav_acrfmc      S                   LIKE(ACRfmc)
008400010119     D sav_acrfbn      S                   LIKE(ACRfbn)
008500010119     D sav_acrfbl      S                   LIKE(ACRfbl)
008600010119     D sav_acrfat      S                   LIKE(ACRfat)
008700010119     D sav_acrfmt      S                   LIKE(ACRfmt)
008800010119     D sav_v1ccra      S                   LIKE(ds_v1ccra)
008900010119     D sav_v1ccrc      S                   LIKE(ds_v1ccrc)
009000010119     D sav_v1cac       S                   LIKE(v1cac)
009100010119     D sav_v1loc       S                   LIKE(v1loc)
009200010119     D sav_v1prc       S                   LIKE(v1prc)
009300001012
009400010730     D vddif           S                   LIKE(v1cdif)
009500010730     D vddff           S                   LIKE(v1cdff)
009600010730
009700001011     D werr            S              1
009800001013
009900010131     D totqta          S              8  3
010000010119     D ore             S              2  0
010100010119     D minuti          S              2  0
010200030922
010300030922     D §kpjbu          S                   LIKE(KPJBU)
010400070919     d wccgi           s                   like(v1ccgi)
010500070919     d wdcgi           s                   like(v1dcgi)
010600001006
010700001215      *  titolo videata (lunghezza massima 33)
010800001215     D TIT_A           C                   CONST('***  ANAGRAFICA RITIRI FISSI-
010900001010     D                                       ***')
011000001006
011100001010      *   S C H I E R E
011200070905     D MSG             S             78    DIM(42) CTDATA PERRCD(1)              MSG VIDEO
011300001011     D MOD             S             15    DIM(4)  CTDATA PERRCD(1)              MODALITA'
011400001006
011500001010      *   D S   I N T E R N E / E S T E R N E
011600001009
011700001009     D WLBDAT          DS                  INZ
011800001009     D  G02DAT                 1      8  0
011900001009     D  G02INV                 9     16  0
012000001009     D  G02ERR                17     17
012100001009     D  G02TGI                18     22  0
012200001020
012300010119     D ds_v1ccra       DS
012400010119     D  v1cra1
012500010119     D  v1cra2
012600010119     D  v1cra3
012700001020
012800010119     D ds_v1ccor       DS
012900010119     D  v1cor1
013000010119     D  v1cor2
013100010119     D  v1cor3
013200001020
013300010119     D ds_v1ccrc       DS
013400010119     D  v1crc1
013500010119     D  v1crc2
013600010119     D  v1crc3
013700001011
013800010119     D ds_tnsd         DS
013900010119     D  ds_cod                        3
014000010119     D  ds_tip                        1
014100010119     D  ds_des                       25
014200010119     D  ds_pra                        3
014300001009
014400001009     D Parm01          DS
014500001009     D  ppoe                   1      3  0
014600001009     D  pnor                   4     10  0
014700010124     D  psce                  11     11
014800060104     d  pfior01               12     12
014900060111     d  pcro                  13     22  0
015000001009
015100001010     D TIBS02DS      E DS
015200001010     D FNLV14DS      E DS
015300001010     D FNLV13DS      E DS
015400001010     D TISI95DS      E DS
015500070808     d FIOR37ds      e ds                  inz
015600070808     d  I37opz       e                     inz('1')
015700070808     d  I37cgi       e                     inz(*all'9')
015800071108     d FIOR38ds      e ds                  inz
015900070810      *
016000070905     d fidg09ds      e ds
016100070810      *
016200010131     D ddft          E DS
016300010730     D DORS01        E DS
016400030922     d Trul33ds      e ds
016500001010
016600001010     D KPJBA         E DS
016700060203      * - Parametri x Controllo profilo utenti
016800060203     d TIBS34ds      e ds
016900060203      * - Ds di riferimento al file esterno AZUTE00F
017000060203     d AZUTEds       e ds                  extname(AZUTE00F)
017100060203     d dUTE01        e ds
017200060203      * - Ds per dati organigramma
017300060203     d DDatiUte      e ds
017400030922
017500001010     D                SDS
017600001010     D  VTCPGM                 1     10
017700001010
017800001006      *------------------------------------------------------------------------*
017900001009
018000001011      * Se sono in manutenzione/visualizzazione/annullato mi carico i dati dal file
018100001011     C                   if        *in02 = *on or *in03 = *on
018200001011     C     kfnor         chain(n)  fnors01L
018300001009     C                   if        %found(fnors01l)
018400031030     C                   if        ORSatb <> *blanks
018500031030     C   02              eval      *in02 = *off
018600001009     C                   eval      *in04 = *on
018700001009     C                   movel     mod(4)        v1mod
018800001009     C                   endif
018900001009     C                   exsr      Sr_Carvid
019000001009     C                   endif
019100001009     C                   endif
019200060111      * se immissione e richiamato imposto già il cliente ritiro
019300060111     c                   If        *In06 and *In01
019400060111     c                   Move      pcro          ds_v1ccra
019500060111     c                   EndIf
019600001006
019700001006      * Emissione 1° Videata
019800001010
019900001006     C                   do        *hival
020000001010
020100001009     C                   exfmt     fior031
020200001011     C                   setoff                                       28  90
020300001009
020400001011      * F6=Conferma
020500001025     C                   if        *inkf = *on
020600001017     C                   exsr      Sr_Contr
020700001017     C  n28              exsr      Sr_Conferma
020800001017     C  n28              leave
020900001011     C                   endif
021000001009
021100001009      * F7=Int.Cod.Ritiro
021200001011     C   kg              exsr      Sr_RicRitiro
021300001009
021400001009      * F8=Int.Cod.Ordinante
021500001011     C   kh              exsr      Sr_RicOrdin
021600001009
021700001009      * F9=Int.Cod.Consegna
021800001011     C   ki              exsr      Sr_RicCons
021900001011
022000001011      * F12=Ritorno
022100001011     C   kl              leave
022200001011
022300001011      * F16=Annullamento
022400001020     C                   if        *inkq = *on and *in01 = *off
022500001011     C                   exsr      Sr_Annulla
022600001020     C                   leave
022700001011     C                   endif
022800001010
022900001011      * Controlli
023000071108     ***C                   if        *in28 = *off and *in03 = *off
023100071108     ***C                             and *in04 = *off
023200071108     c                   if        *in28 = *off
023300071108     c                   if        *in03 = *off and *in04 = *off
023400001020     C                   exsr      Sr_Contr
023500071108     c                   else
023600071108     c                   exsr      sr_DecoCRA
023700071108     c                   endif
023800001020     C                   endif
023900001010
024000001010     C   28
024100001010     Cor 90              iter
024200001010
024300001006     C                   enddo
024400030922
024500030922     C                   movel(p)  kpjbu         §kpjbu
024600030922     c                   clear                   TRUL33DS
024700030922     c                   eval      I33tla = 'C'
024800030922     c                   movel(p)  TRUL33DS      KPJBU
024900030922     c                   call      'TRUL33R'
025000030922     c                   parm                    KPJBA
025100030922     C                   movel     §kpjbu        kpjbu
025200001010
025300001006     C                   eval      *inlr = *on
025400001009      **********************************************************************
025500001009      * CARICO DATI NELLA VIDEATA DAL FILE
025600001009      **********************************************************************
025700001009     C     Sr_Carvid     begsr
025800001009
025900001011     C                   movel     ORStor        v1ctor
026000010119     C                   movel     ORScor        ds_v1ccor
026100010119     C                   movel     ORScra        ds_v1ccra
026200001009     C                   movel     ORSrer        v1rer
026300001009     C                   movel     ORSter        v1ter
026400001020      * Dati Consegna a
026500010119     C                   movel     ORScrc        ds_v1ccrc
026600010119     C                   eval      *in05 = (ORScrc <> *zeros)
026700001009     C                   movel     ORSrsc        v1rsc
026800001009     C                   movel     ORSinc        v1inc
026900001011     C                   movel     ORSloc        v1loc
027000001009     C                   movel     ORScac        v1cac
027100001009     C                   movel     ORSprc        v1prc
027200001110
027300001009     C                   movel     ORSnac        v1nac
027400001009     C                   movel     ORSffd        v1ffd
027500001110     C                   z-add     ORSorr        v1orr
027600010122     C                   movel     ORSrmp        v1rmp
027700001009     C                   movel     ORSrlu        v1rlu
027800001009     C                   movel     ORSrma        v1rma
027900001009     C                   movel     ORSrme        v1rme
028000001009     C                   movel     ORSrgi        v1rgi
028100001009     C                   movel     ORSrve        v1rve
028200001009     C                   movel     ORSnam        v1nam
028300001110
028400001110     C                   z-add     ORSncl        v1ncl
028500001110     C                   z-add     ORSpkg        v1pkg
028600001110     C                   z-add     ORSvlm        v1vlm
028700001110     C                   z-add     ORSbnc        v1bnc
028800001110     C                   z-add     ORSblc        v1blc
028900001110     C                   z-add     ORSatt        v1att
029000001110     C                   z-add     ORSmtc        v1mtc
029100001110
029200001009     C                   movel     ORSpag        v1pag
029300001110     C                   z-add     ORSksc        v1ksc
029400001009     C                   movel     ORSctr        v1ctr
029500001110     C                   if        ORSksc = *zeros and ORSctr = *zeros
029600001110     C                   clear                   v1ctr
029700001110     C                   endif
029800001110
029900001110     C                   if        ORSpoc <> *zeros
030000001009     C                   movel     ORSpoc        v1cpoc
030100001110     C                   endif
030200001011     C                   movel     ORSno1        v1not1
030300001011     C                   movel     ORSno2        v1not2
030400010118     C                   movel     ORSspi        v1spi
030500001009
030600010730     C                   movel     ORSflo        dors01
030700010730     C                   z-add     §orsdif       G02inv
030800010730     C                   movel     '3'           G02err
030900010730     C                   call      'XSRDA8'
031000010730     C                   parm                    wlbdat
031100010730     C                   movel     G02dat        v1cdif
031200010730
031300010730     C                   z-add     §orsdff       G02inv
031400010730     C                   movel     '3'           G02err
031500010730     C                   call      'XSRDA8'
031600010730     C                   parm                    wlbdat
031700010730     C                   movel     G02dat        v1cdff
031800070905
031900070905      * recupero il codice giro
032000070905     c                   clear                   v1ccgi
032100070905     c     kfnor         chain(n)  fnors11l
032200070905     c                   if        %found(fnors11l)
032300070905     c                   eval      v1ccgi = ors1cgi
032400070905     c                   endif
032500010730
032600001020     C                   exsr      Sr_Decod
032700001009
032800001009     C                   endsr
032900001009      **********************************************************************
033000001009      * DECODIFICHE
033100001009      **********************************************************************
033200001009     C     Sr_Decod      begsr
033300001009
033400001010     C                   clear                   v1dcor
033500001010     C                   clear                   v1icor
033600001010     C                   clear                   v1pcor
033700001010     C                   clear                   v1lcor
033800001010     C                   clear                   v1rcor
033900001010     C                   clear                   v1ncor
034000001010
034100001010     C                   clear                   v1dcra
034200001010     C                   clear                   v1icra
034300001010     C                   clear                   v1pcra
034400001010     C                   clear                   v1lcra
034500001010     C                   clear                   v1rcra
034600001010     C                   clear                   v1ncra
034700071108     c                   clear                   V1Ccgi1
034800071108     c                   clear                   V1Ccgi2
034900071108     c                   clear                   V1Ccgi3
035000010124
035100070905     c                   clear                   v1dcgi
035200010124     C                   clear                   v1dtor
035300001009
035400001010      * Cliente Ordinante
035500010119     C                   if        ds_v1ccor <> *zeros
035600001011     C                   exsr      Sr_Decocor
035700001009     C                   endif
035800001009
035900001011      * Cliente Ritiro
036000010119     C                   if        ds_v1ccra <> *zeros
036100001011     C                   exsr      Sr_Decocra
036200001011     C                   endif
036300001010
036400001010      * Cliente Consegna
036500010119     C                   if        ds_v1ccrc <> *zeros
036600001011     C                   exsr      Sr_Decocrc
036700001010     C                   endif
036800070905
036900070905      * giro
037000070919     c                   eval      wccgi = v1ccgi
037100070905     c                   exsr      sr_decocgi
037200070919     c                   eval      v1dcgi = wdcgi
037300001110
037400010124      * P.O. Consegna
037500001110     C                   movel     v1cpoc        kazorg
037600001110     C     kazorg        chain     azorg01L
037700001110     C                   if        %found(azorg01l)
037800001110     C                   movel     ORGdes        v1dpoc
037900001110     C                   endif
038000001110
038100010124      * Tipo ORM
038200001110     C                   clear                   TIBS02DS
038300001110     C                   movel     'C'           T02mod
038400001110     C                   movel     knsif         t02sif
038500001110     C                   movel     'TOR'         t02cod
038600001110     C                   movel(p)  v1ctor        T02ke1
038700001110     C                   call      'TIBS02R'
038800001110     C                   parm                    KPJBA
038900001110     C                   parm                    TIBS02DS
039000001110     C                   movel     t02uni        v1dtor
039100001009
039200001013     C                   endsr
039300001011      **********************************************************************
039400001011      * DECODIFICHE     * cliente ordinante *
039500001011      **********************************************************************
039600001011     C     Sr_Decocor    begsr
039700001011
039800010119     C                   movel     ds_v1ccor     Kfnacr
039900001011     C     kfnacr        chain     fnacr01l
040000001012     C                   if        %found(fnacr01l) and ACRatb = *blanks
040100001012     C*                            and ACRtac = 'O'
040200001011     C                   movel     ACRrsr        v1dcor
040300001011     C                   movel     ACRinr        v1icor
040400001011     C                   movel     ACRcar        v1pcor
040500001011     C                   movel     ACRlor        v1lcor
040600001011     C                   movel     ACRprr        v1rcor
040700001011     C                   movel     ACRnar        v1ncor
040800001011     C                   endif
040900001011
041000001011     C                   endsr
041100001011      **********************************************************************
041200001011      * DECODIFICHE     * cliente ritiro *
041300001011      **********************************************************************
041400001011     C     Sr_Decocra    begsr
041500001011
041600010119     C                   movel     ds_v1ccra     Kfnacr
041700001011     C     kfnacr        chain     fnacr01l
041800071108if  1C                   IF        %found(fnacr01l) and ACRatb = *blanks
041900071108      *
042000001011     C                   movel     ACRrsr        v1dcra
042100001011     C                   movel     ACRinr        v1icra
042200001011     C                   movel     ACRcar        v1pcra
042300001011     C                   movel     ACRlor        v1lcra
042400001011     C                   movel     ACRprr        v1rcra
042500001011     C                   movel     ACRnar        v1ncra
042600070919      * recupero il giro dell'anagrafica
042700071108     c     k02acr13      setll     FNACR100
042800071108     c     k02acr13      reade     FNACR100
042900071108do  2c                   dow       NOT  %eof(FNACR13L)
043000071108sel 3c                   select
043100071108w   3c                   when      ACR1atb <> ACRatb
043200071108w   3c                   when      ACR1ain  = '='
043300071108     c                   eval      V1Ccgi1  = ACR1cgi
043400071108w   3c                   when      ACR1ain  = '>'
043500071108     c                   eval      V1Ccgi2  = ACR1cgi
043600071108w   3c                   when      ACR1ain  = '<'
043700071108     c                   eval      V1Ccgi3  = ACR1cgi
043800071108e   3c                   endsl
043900071108     c     k02acr13      reade     FNACR100
044000071108e   2c                   enddo
044100071108      *
044200071108e   1C                   ENDIF
044300071108     *** * se ho un giro recuperato da FNACR lo decodifico
044400071108     ***c                   if        v1ccgiacr <> *blanks
044500071108     ***c                   eval      wccgi = v1ccgiacr
044600071108     ***c                   exsr      sr_decocgi
044700071108     ***c                   eval      v1dcgiacr = wdcgi
044800071108     ***c                   endif
044900071108      *
045000071108      * Interrogazione giri ritiro per cliente/filiale (se richiesta)
045100071108     c                   if        V1CcgiX  = '?'
045200071108     c                   clear                   V1CcgiX
045300071108     c                   clear                   FIOR38ds
045400071108     c                   eval      I38cro   = KFNacr
045500071108     c                   eval      I38pocgi = V1Cpoe
045600071108     c                   movel(p)  FIOR38ds      KPJBU
045700071108     c                   call      'FIOR38R'
045800071108     c                   parm                    KPJBA
045900071108     c                   endif
046000010122
046100001011     C                   endsr
046200001011      **********************************************************************
046300001011      * DECODIFICHE     * cliente consegna *
046400001011      **********************************************************************
046500001011     C     Sr_Decocrc    begsr
046600001011
046700001011     C                   clear                   v1rsc
046800001011     C                   clear                   v1inc
046900001011     C                   clear                   v1cac
047000001011     C                   clear                   v1loc
047100001011     C                   clear                   v1prc
047200001011     C                   clear                   v1nac
047300001011
047400010119     C                   movel     ds_v1ccrc     Kfnacr
047500001011     C     kfnacr        chain     fnacr01l
047600001012     C                   if        %found(fnacr01l) and ACRatb = *blanks
047700001011     C                   movel     ACRrsr        v1rsc
047800001011     C                   movel     ACRinr        v1inc
047900001011     C                   movel     ACRcar        v1cac
048000001011     C                   movel     ACRlor        v1loc
048100001011     C                   movel     ACRprr        v1prc
048200001011     C                   movel     ACRnar        v1nac
048300001011     C                   endif
048400001011
048500001011     C                   endsr
048600070905
048700070905      *------------------------------------------------------------------------*
048800070905      *   Controllo e decodifica giro
048900070905      *------------------------------------------------------------------------*
049000070905     c     sr_decocgi    begsr
049100070905
049200070905     c                   clear                   fidg09ds
049300070905     c                   eval      d09iop0 = '001'
049400070905     c                   eval      d09ifgs = v1cpoe
049500070905     c                   eval      d09idat = *date
049600070919     c                   eval      d09icgi = wccgi
049700070905     c                   eval      d09itug = 'R'
049800070905     c                   eval      kpjbu = fidg09ds
049900070905     c                   call      'FIDG09R'
050000070905     c                   parm                    kpjba
050100070905     c                   eval      fidg09ds = kpjbu
050200070905     c                   if        d09ocgi <> ' '
050300070919     c                   eval      wdcgi = d09odes
050400070919     c                   else
050500070919     c                   clear                   wdcgi
050600070905     c                   endif
050700070905
050800070905     c                   endsr
050900070905
051000001010      **********************************************************************
051100001010      * CONTROLLI
051200001010      **********************************************************************
051300001010     C     Sr_Contr      begsr
051400001006
051500001009      * Reset indicatori
051600001010     C                   setoff                                       414243
051700001010     C                   setoff                                       444546
051800001010     C                   setoff                                       474849
051900001010     C                   setoff                                       505152
052000001010     C                   setoff                                       535455
052100070925     C                   setoff                                       56  58
052200010131     C                   setoff                                       59
052300001011     C                   setoff                                       606162
052400001011     C                   setoff                                       636465
052500010730     C                   setoff                                       2627
052600001009
052700001010      * Cliente ritiro
052800010119     C                   if        ds_v1ccra <> *zeros
052900001010     C                   exsr      Sr_Contrcra
053000001011     C                   exsr      Sr_Carflag
053100010119     C                   if        *in01 = *on and ds_v1ccra <> sav_v1ccra
053200010119     C                   exsr      Sr_Caranacr
053300010119     C                   eval      sav_v1ccra = ds_v1ccra
053400010131     C  nkf              goto      endsrcontr
053500001011     C                   endif
053600001010     C                   else
053700001010     C                   movel     msg(1)        v1cmsg
053800001010     C                   seton                                        41  28
053900001010     C                   endif
054000001011     C   28              goto      endsrcontr
054100001006
054200001010      * Cliente ordinante
054300010119     C                   if        ds_v1ccor <> *zeros
054400001010     C                   exsr      Sr_Contrcor
054500001013     C   28              goto      endsrcontr
054600001013     C                   exsr      Sr_Decocor
054700001010     C                   endif
054800001006
054900001010      * Colli - Peso - Volume - Bancali - Bilico - Autotreno - Motrice
055000001010     C                   exsr      Sr_Contrqta
055100001011     C   28              goto      endsrcontr
055200001010
055300010131      * Ritiro Ora - M/P - Giorni
055400010131     C                   exsr      Sr_Contrrit
055500010131     C   28              goto      endsrcontr
055600001010
055700070905
055800070905      * giro
055900070905     c                   exsr      sr_contrcgi
056000070905     c   28              leavesr
056100001010
056200001010      * Pagamento - Cod.Tassazione - Cod.Tariffa
056300001011     C                   exsr      Sr_Contrpag
056400001011     C   28              goto      endsrcontr
056500001010
056600001010      * P.O. Consegna
056700001110     C                   if        v1cpoc <> *blanks
056800001011     C                   exsr      Sr_Contrpoc
056900001110     C                   endif
057000001011     C   90
057100001011     Cor 28              goto      endsrcontr
057200001010
057300001010      * Consegna a
057400010119     C                   if        ds_v1ccrc <> *zeros
057500001010     C                   eval      *in05 = *on
057600001010     C                   exsr      Sr_Contrcrc
057700010119     C                   eval      sav_v1ccrc = ds_v1ccrc
057800001010     C                   else
057900010119     C                   clear                   sav_v1ccrc
058000001011     C                   if        v1rsc <> *blanks or v1inc <> *blanks
058100001010     C                   exsr      Sr_Contrsrc
058200001010     C                   endif
058300001011     C                   if        v1cac <> *blanks or v1loc <> *blanks or
058400001011     C                             v1prc <> *blanks or v1nac <> *blanks
058500001011     C                   exsr      Sr_Contrcap
058600001011     C  n28              exsr      Sr_Contrsrc
058700001011     C                   endif
058800001010     C                   eval      *in05 = *off
058900001010     C                   endif
059000001011     C   28              goto      endsrcontr
059100010131
059200010131      * Tipo ordine
059300010131     C                   if        v1ctor = *blanks
059400010131     C                   eval      v1ctor = d§dfttor
059500010131     C                   endif
059600010131     C                   exsr      Sr_Contrtor
059700010131     C   90
059800010131     Cor 28              goto      endsrcontr
059900001010
060000010730      * Periodo chiusura
060100010730     C                   exsr      Sr_Contrchiu
060200010730     C   28              goto      endsrcontr
060300010730
060400001010     C                   exsr      Sr_Decod
060500001006
060600001011     C     endsrcontr    endsr
060700001010      **********************************************************************
060800001010      * CONTROLLI     * cliente ritiro *
060900001010      **********************************************************************
061000001011     C     Sr_Contrcra   begsr
061100001010
061200010119     C                   movel     ds_v1ccra     kfnacr
061300001011     C     kfnacr        chain     fnacr01l
061400001010     C                   if        %found(fnacr01l)
061500071105     C                   if        ACRatb <> *blanks
061600001010     C                   movel     msg(2)        v1cmsg
061700001010     C                   seton                                        41  28
061800001010     C                   endif
061900001010     C                   else
062000001010     C                   movel     msg(2)        v1cmsg
062100001010     C                   seton                                        41  28
062200001010     C                   endif
062300010124
062400001010     C                   endsr
062500001011      **********************************************************************
062600001011      * CARICO DATI DA ANAGRAFICO RITIRI
062700001011      **********************************************************************
062800010119     C     Sr_Caranacr   begsr
062900001011
063000010131     C                   movel     ACRksc        v1ksc
063100040629     c                   If        AcrCcc = 999
063200040629     c                   Clear                   V1Ctr
063300040629     c                   Else
063400040629     c                   Move      AcrCcc        V1Ctr
063500040629     c                   EndIf
063600001011     C                   movel     ACRper        v1rer
063700001011     C                   movel     ACRtel        v1ter
063800001011     C                   movel     ACRntm        v1nam
063900001011     C                   movel     ACRno1        v1not1
064000001011     C                   movel     ACRno2        v1not2
064100010119     C                   movel     ACRspi        v1spi
064200001013
064300001013      * Controllo i flag per posizionamento cursore
064400001013     C                   eval      *in43 = (ACRfcl = 'O')
064500001013     C   43              goto      endflg
064600001013     C                   eval      *in44 = (ACRfpk = 'O')
064700001013     C   44              goto      endflg
064800001013     C                   eval      *in45 = (ACRfmc = 'O')
064900001013     C   45              goto      endflg
065000001013     C                   eval      *in46 = (ACRfbn = 'O')
065100001013     C   46              goto      endflg
065200001013     C                   eval      *in47 = (ACRfbl = 'O')
065300001013     C   47              goto      endflg
065400001013     C                   eval      *in48 = (ACRfat = 'O')
065500001013     C   48              goto      endflg
065600001013     C                   eval      *in49 = (ACRfmt = 'O')
065700001013     C   49              goto      endflg
065800001013     C                   eval      *in43 = (ACRfcl = 'F')
065900001013     C   43              goto      endflg
066000001013     C                   eval      *in44 = (ACRfpk = 'F')
066100001013     C   44              goto      endflg
066200001013     C                   eval      *in45 = (ACRfmc = 'F')
066300001013     C   45              goto      endflg
066400001013     C                   eval      *in46 = (ACRfbn = 'F')
066500001013     C   46              goto      endflg
066600001013     C                   eval      *in47 = (ACRfbl = 'F')
066700001013     C   47              goto      endflg
066800001013     C                   eval      *in48 = (ACRfat = 'F')
066900001013     C   48              goto      endflg
067000001013     C                   eval      *in49 = (ACRfmt = 'F')
067100001013     C   49              goto      endflg
067200001013     C                   eval      *in43 = *on
067300001013     C     endflg        tag
067400010122
067500010122     C                   exsr      Sr_Decocra
067600001011
067700001011      * Recupero i dati del cliente ordinante se inserito
067800001011     C                   if        ACRcco <> *zeros
067900010119     C                   movel     ACRcco        ds_v1ccor
068000001013     C                   exsr      Sr_Decocor
068100001011     C                   endif
068200001011
068300001011     C                   endsr
068400001011      **********************************************************************
068500010131      * CARICO FLAG COLLI - PESO - VOLUME - ETC ETC
068600001011      **********************************************************************
068700001011     C     Sr_Carflag    begsr
068800001011
068900010119     C                   eval      sav_acrfcl = ACRfcl
069000010119     C                   eval      sav_acrfpk = ACRfpk
069100010119     C                   eval      sav_acrfmc = ACRfmc
069200010119     C                   eval      sav_acrfbn = ACRfbn
069300010119     C                   eval      sav_acrfbl = ACRfbl
069400010119     C                   eval      sav_acrfat = ACRfat
069500010119     C                   eval      sav_acrfmt = ACRfmt
069600001011
069700001011     C                   endsr
069800001010      **********************************************************************
069900001010      * CONTROLLI     * cliente ordinante *
070000001010      **********************************************************************
070100001011     C     Sr_Contrcor   begsr
070200001010
070300010119     C                   movel     ds_v1ccor     kfnacr
070400001011     C     kfnacr        chain     fnacr01l
070500001010     C                   if        %found(fnacr01l)
070600001013     C                   if        ACRatb <> *blanks
070700001013     C*                            or ACRtac <> 'O'
070800001010     C                   movel     msg(3)        v1cmsg
070900001010     C                   seton                                        42  28
071000001010     C                   endif
071100001010     C                   else
071200001010     C                   movel     msg(3)        v1cmsg
071300001010     C                   seton                                        42  28
071400001010     C                   endif
071500001010
071600001010     C                   endsr
071700001010      **********************************************************************
071800001010      * CONTROLLI     * colli - peso - volume - etc etc *
071900001010      **********************************************************************
072000001010     C     Sr_Contrqta   begsr
072100010119
072200010119      * Se il flag è F può non essere inserita la qtà
072300010119      * Se il flag è O la qtà deve essere inserita
072400010119     C                   if        sav_acrfcl <> *blanks and v1ncl = *zeros
072500010119     C                   if        sav_acrfcl = 'F' and *in66 = *off
072600010119     C                   movel     msg(25)       v1cmsg
072700010119     C                   seton                                        436628
072800010119     C                   goto      endsrcqta
072900010119     C                   endif
073000010119     C                   if        sav_acrfcl = 'O'
073100010119     C                   movel     msg(4)        v1cmsg
073200010119     C                   seton                                        43  28
073300010119     C                   goto      endsrcqta
073400010119     C                   endif
073500010119     C                   endif
073600010119
073700010119     C                   if        sav_acrfpk <> *blanks and v1pkg = *zeros
073800010119     C                   if        sav_acrfpk = 'F' and *in67 = *off
073900010119     C                   movel     msg(26)       v1cmsg
074000010119     C                   seton                                        446728
074100010119     C                   goto      endsrcqta
074200010119     C                   endif
074300010119     C                   if        sav_acrfpk = 'O'
074400010119     C                   movel     msg(5)        v1cmsg
074500010119     C                   seton                                        44  28
074600010119     C                   goto      endsrcqta
074700010119     C                   endif
074800010119     C                   endif
074900010119
075000010119     C                   if        sav_acrfmc <> *blanks and v1vlm = *zeros
075100010119     C                   if        sav_acrfmc = 'F' and *in68 = *off
075200010119     C                   movel     msg(27)       v1cmsg
075300010119     C                   seton                                        456828
075400010119     C                   goto      endsrcqta
075500010119     C                   endif
075600010119     C                   if        sav_acrfmc = 'O'
075700010119     C                   movel     msg(6)        v1cmsg
075800010119     C                   seton                                        45  28
075900010119     C                   goto      endsrcqta
076000010119     C                   endif
076100010119     C                   endif
076200010119
076300010119     C                   if        sav_acrfbn <> *blanks and v1bnc = *zeros
076400010119     C                   if        sav_acrfbn = 'F' and *in69 = *off
076500010119     C                   movel     msg(28)       v1cmsg
076600010119     C                   seton                                        466928
076700010119     C                   goto      endsrcqta
076800010119     C                   endif
076900010119     C                   if        sav_acrfbn = 'O'
077000010119     C                   movel     msg(7)        v1cmsg
077100010119     C                   seton                                        46  28
077200010119     C                   goto      endsrcqta
077300010119     C                   endif
077400010119     C                   endif
077500010119
077600010119     C                   if        sav_acrfbl <> *blanks and v1blc = *zeros
077700010119     C                   if        sav_acrfbl = 'F' and *in70 = *off
077800010119     C                   movel     msg(29)       v1cmsg
077900010119     C                   seton                                        477028
078000010119     C                   goto      endsrcqta
078100010119     C                   endif
078200010119     C                   if        sav_acrfbl = 'O'
078300010119     C                   movel     msg(8)        v1cmsg
078400010119     C                   seton                                        47  28
078500010119     C                   goto      endsrcqta
078600010119     C                   endif
078700010119     C                   endif
078800010119
078900010119     C                   if        sav_acrfat <> *blanks and v1att = *zeros
079000010119     C                   if        sav_acrfat = 'F' and *in71 = *off
079100010119     C                   movel     msg(30)       v1cmsg
079200010119     C                   seton                                        487128
079300010119     C                   goto      endsrcqta
079400010119     C                   endif
079500010119     C                   if        sav_acrfat = 'O'
079600010119     C                   movel     msg(9)        v1cmsg
079700010119     C                   seton                                        48  28
079800010119     C                   goto      endsrcqta
079900010119     C                   endif
080000010119     C                   endif
080100010119
080200010119     C                   if        sav_acrfmt <> *blanks and v1mtc = *zeros
080300010119     C                   if        sav_acrfmt = 'F' and *in72 = *off
080400010119     C                   movel     msg(31)       v1cmsg
080500010119     C                   seton                                        497228
080600010119     C                   goto      endsrcqta
080700010119     C                   endif
080800010119     C                   if        sav_acrfmt = 'O'
080900010119     C                   movel     msg(10)       v1cmsg
081000010119     C                   seton                                        49  28
081100010119     C                   goto      endsrcqta
081200010119     C                   endif
081300010119     C                   endif
081400001010
081500010131      * Almeno 1 tra volume peso e bancali deve essere inserito
081600010131     C                   eval      totqta =  v1pkg + v1vlm + v1bnc
081700010131     C                   if        totqta = *zeros
081800010131     C                   movel     msg(37)       v1cmsg
081900010131     C                   seton                                        44  28
082000010131     C                   goto      endsrcqta
082100010131     C                   endif
082200001010
082300001012     C     endsrcqta     endsr
082400010131      **********************************************************************
082500010131      * CONTROLLI     * ritiro ora M/P giorno *
082600010131      **********************************************************************
082700010131     C     Sr_Contrrit   begsr
082800010131
082900010131      * Mattino/Pomeriggio
083000010131     C                   if        v1rmp = *blanks
083100010131     C                   movel     msg(35)       v1cmsg
083200010131     C                   seton                                        50  28
083300010131     C                   goto      endsrrit
083400010131     C                   endif
083500010131      * Ora
083600010131     C                   if        v1orr > *zeros
083700010131     C                   movel     v1orr         ore
083800010131     C                   move      v1orr         minuti
083900010131
084000010131     C                   if        ore > 24 or minuti > 59
084100010131     C                   seton                                        58  28
084200010131     C                   movel     msg(24)       v1cmsg
084300010131     C                   goto      endsrrit
084400010131     C                   endif
084500010131
084600010131     C                   else
084700010131     C                   seton                                        58  28
084800010131     C                   movel     msg(34)       v1cmsg
084900010131     C                   goto      endsrrit
085000010131     C                   endif
085100010131      * Giorno
085200010131     C                   if        v1rlu = *blanks and v1rma = *blanks and
085300010131     C                             v1rme = *blanks and v1rgi = *blanks and
085400010131     C                             v1rve = *blanks
085500010131     C                   movel     msg(11)       v1cmsg
085600010131     C                   seton                                        59  28
085700010131     C                   goto      endsrrit
085800010131     C                   endif
085900010131
086000010131     C     endsrrit      endsr
086100070905
086200070905      *------------------------------------------------------------------------*
086300070905      * CODICE GIRO
086400070905      *------------------------------------------------------------------------*
086500070905     c     sr_contrcgi   begsr
086600070905
086700070905     c                   clear                   v1dcgi
086800070905
086900070905      * ricerca giro con "?"
087000070905     c                   if        %scan('?':v1ccgi) > *zeros
087100070905     c                   clear                   fidg09ds
087200070905     c                   eval      d09iop0 = 'P01'
087300070905     c                   eval      d09ifgs = v1cpoe
087400070905     c                   eval      d09idat = *date
087500070905     c                   eval      d09itug = 'R'
087600070905     c                   eval      kpjbu = fidg09ds
087700070905     c                   call      'FIDG09R'
087800070905     c                   parm                    kpjba
087900070905     c                   eval      fidg09ds = kpjbu
088000070905      *     imposto il giro scelto
088100070905     c                   eval      v1ccgi = d09ocgi
088200070905     c                   endif
088300070905      *     controllo
088400070905     c                   if        v1ccgi <> *blank
088500070919     c                   eval      wccgi = v1ccgi
088600070905     c                   exsr      sr_decocgi
088700070905      *     giro errato
088800070905     c                   if        d09oerr = '1'
088900070905     c                   eval      *in28 = *on
089000070905     c                   eval      *in51 = *on
089100070905     c                   eval      v1cmsg = msg(42)
089200070905     c                   leavesr
089300070905     c                   endif
089400070919     c                   eval      v1dcgi = wdcgi
089500070905     c                   endif
089600070905
089700070905     c                   endsr
089800070905
089900001010      **********************************************************************
090000001010      * CONTROLLI     * tipo ordine *
090100001010      **********************************************************************
090200001010     C     Sr_Contrtor   begsr
090300001013
090400001010     C     '?'           scan      v1ctor                                 30
090500001010     C     *in30         ifeq      *on
090600001010     C                   eval      *in52 = *on
090700001010     C                   eval      *in90 = *on
090800001010     C                   clear                   TIBS02DS
090900001010     C                   movel     'R'           t02mod
091000001010     C                   movel     knsif         t02sif
091100001010     C                   movel     'TOR'         t02cod
091200001010     C                   call      'TIBS02R'
091300001010     C                   parm                    KPJBA
091400001010     C                   parm                    TIBS02DS
091500001011     C                   movel     T02ke1        v1ctor
091600001012     C                   movel     T02uni        v1dtor
091700001010     C                   goto      endsrctor
091800001010     C                   endif
091900001010
092000001012     C                   if        v1ctor <> *blanks
092100001010     C                   movel     'C'           T02mod
092200001010     C                   movel     knsif         t02sif
092300001010     C                   movel     'TOR'         t02cod
092400001011     C                   movel(p)  v1ctor        T02ke1
092500001010     C                   call      'TIBS02R'
092600001010     C                   parm                    KPJBA
092700001010     C                   parm                    TIBS02DS
092800001010     C                   if        t02err <> *blanks
092900001010     C                   movel     msg(13)       v1cmsg
093000001010     C                   seton                                        52  28
093100001010     C                   else
093200001010     C                   movel     t02ke1        v1ctor
093300001010     C                   movel     t02uni        v1dtor
093400001010     C                   endif
093500001012     C                   else
093600001012     C                   movel     msg(14)       v1cmsg
093700001012     C                   seton                                        52  28
093800001012     C                   endif
093900001010
094000001010     C     endsrctor     endsr
094100001010      **********************************************************************
094200001010      * CONTROLLI     * pagamento *
094300001010      **********************************************************************
094400001010     C     Sr_Contrpag   begsr
094500001010
094600001110      * Se inseriti tariffa o codice deve essere inserito anche chi paga
094700010131     C                   if        v1ksc <> *zeros or v1ctr <> *blanks
094800001110
094900001110      * Controlla validità codice + tariffa
095000001110     C                   if        v1ksc <> *zeros and v1ctr <> *blanks
095100001110     C                   z-add     v1ksc         kksc
095200001010     C                   movel     v1ctr         kctr
095300071108     C     ktntam        chain     tntam000
095400001010     C                   if        %found(tntam01l)
095500001010     C                   if        TAMatb <> *blanks
095600001010     C                   movel     msg(16)       v1cmsg
095700001010     C                   seton                                        54  28
095800001110     C                   goto      endsrcpag
095900001010     C                   endif
096000001010     C                   else
096100001010     C                   movel     msg(16)       v1cmsg
096200001010     C                   seton                                        54  28
096300001110     C                   goto      endsrcpag
096400001110     C                   endif
096500001110     C                   endif
096600001010     C                   endif
096700001110      * Se inserito chi paga il pagante deve essere codificato
096800001110     C                   if        v1pag <> *blanks
096900010119     C                   if        v1pag = 'O' and ds_v1ccor = *zeros
097000001110     C                   movel     msg(23)       v1cmsg
097100001110     C                   seton                                        42  28
097200001110     C                   goto      endsrcpag
097300001110     C                   endif
097400001110     C                   endif
097500001010
097600001110     C     endsrcpag     endsr
097700001011      **********************************************************************
097800001011      * CONTROLLI     * P.O. consegna *
097900001011      **********************************************************************
098000001011     C     Sr_Contrpoc   begsr
098100001110
098200001110     C                   eval      *in31 = *off
098300001013
098400001011     C     '?'           scan      v1cpoc                                 30
098500001011     C     *in30         ifeq      *on
098600001011     C                   eval      *in55 = *on
098700001011     C                   eval      *in90 = *on
098800010119     C                   clear                   ds_tnsd
098900001011     C                   call      'TNSD24R'
099000010119     C                   parm                    ds_cod
099100010119     C                   parm                    ds_tip
099200010119     C                   parm                    ds_des
099300010119     C                   parm                    ds_pra
099400010119     C                   movel     ds_cod        v1cpoc
099500010119     C                   movel     ds_des        v1dpoc
099600001011     C                   endif
099700001106
099800001110     C                   testn                   v1cpoc               31
099900001106     C                   move      v1cpoc        w001a             1
100000001110     C     *in31         ifeq      *off
100100001106     C     w001a         orlt      '0'
100200001106     C                   seton                                        55  28
100300001106     C                   movel     msg(17)       v1cmsg
100400001106     C                   goto      endsrcpoc
100500001106     C                   endif
100600001011
100700001110     C                   if        v1cpoc <> *blanks
100800001011     C                   movel     v1cpoc        kazorg
100900001011     C     kazorg        chain     azorg01L
101000001011     C                   if        %found(azorg01l)
101100001204     C                   if        ORGfva <> *blanks or
101200001204     C                             (ORGfag <> 'A' and ORGfag<> 'F')
101300001011     C                   movel     msg(17)       v1cmsg
101400001011     C                   seton                                        55  28
101500001013     C                   else
101600001013     C                   movel     ORGdes        v1dpoc
101700001011     C                   endif
101800001011     C                   else
101900001011     C                   movel     msg(17)       v1cmsg
102000001011     C                   seton                                        55  28
102100001011     C                   endif
102200001011     C                   endif
102300001011
102400001011     C     endsrcpoc     endsr
102500001010      **********************************************************************
102600001010      * CONTROLLI     * cliente consegna *
102700001010      **********************************************************************
102800001011     C     Sr_Contrcrc   begsr
102900001010
103000010119     C                   movel     ds_v1ccrc     kfnacr
103100001011     C     kfnacr        chain     fnacr01l
103200001010     C                   if        %found(fnacr01l)
103300001010     C                   if        ACRatb <> *blanks
103400001010     C                   movel     msg(18)       v1cmsg
103500001010     C                   seton                                        56  28
103600001010     C                   endif
103700001010     C                   else
103800001010     C                   movel     msg(18)       v1cmsg
103900001010     C                   seton                                        56  28
104000001010     C                   endif
104100001010
104200001010     C                   endsr
104300001010      **********************************************************************
104400001010      * CONTROLLI     * dati cliente consegna *
104500001010      **********************************************************************
104600001011     C     Sr_Contrsrc   begsr
104700001010
104800001010     C                   clear                   FNLV14DS
104900001010     C                   z-add     *date         I14dri
105000001010     C                   movel     v1rsc         I14rsc
105100001010     C                   movel     v1inc         I14ind
105200001010     C                   movel     v1loc         E14loc
105300001010     C                   movel     v1prc         E14prv
105400001010     C                   movel     v1cac         E14cap
105500001011     C                   movel     v1nac         E14nar
105600001010
105700001010     C                   call      'FNLV14R'
105800001010     C                   parm                    KPJBA
105900001010     C                   parm                    FNLV14DS
106000001010
106100001010     C                   movel     I14ind        v1inc
106200001010     C                   movel     E14loc        v1loc
106300001010     C                   movel     E14prv        v1prc
106400001010     C                   movel     E14cap        v1cac
106500001010     C                   movel     E14nar        v1nac
106600001010
106700001010     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
106800001010     C                   eval      *in61 = (O14err = '2')                       Indirizzo
106900001010     C                   eval      *in62 = (O14err = '5')                       CAP
107000001010     C                   eval      *in63 = (O14err = '3')                       Località
107100001010     C                   eval      *in64 = (O14err = '4')                       Provincia
107200001010     C                   eval      *in65 = (O14err = '6')                       Nazione
107300001010
107400001010     C                   if        O14msg <> *blanks
107500001011     C                   movel(p)  O14msg        v1cmsg
107600001010     C                   eval      *IN28 = *ON
107700001010     C                   endif
107800001010
107900001010     C                   endsr
108000001010      **********************************************************************
108100001010      * CONTROLLI     * cap - località - provincia *
108200001010      **********************************************************************
108300001011     C     Sr_Contrcap   begsr
108400001010
108500001011     C                   clear                   TISI95DS
108600001011     C                   clear                   FNLV13DS
108700001011     C                   movel     ' '           I95tla
108800001011     C                   movel     '7'           I95tcn
108900001011     C                   movel     v1cac         I95cap
109000001011     C                   movel     v1nac         I95nar
109100001011     C                   movel     v1inc         I95ind
109200001011     C                   movel     v1loc         I95loc
109300001011     C                   movel     v1prc         I95prv
109400001011     C                   movel     'S'           I13af0
109500001011     C                   movel     'S'           I13af1
109600001011     C                   movel     'S'           I13sz2
109700001011     C                   movel     'S'           I13la3
109800001011     C                   movel     'S'           I13sz3
109900001011     C                   movel     'S'           I13cnv
110000001011     C                   movel     'S'           I13szv
110100001010
110200001011      * Gestione errori forzabili: se cambiato CAP/Località
110300001011      * riazzero i valori di errori forzabili
110400001010
110500001011      * Modifica cap
110600001010
110700010119     C     v1cac         ifne      sav_v1cac
110800001011     C                   clear                   wfzv
110900001011     C                   clear                   wfz1
111000001011     C                   clear                   wfz2
111100001011     C                   clear                   wfz3
111200010119     C                   movel     v1cac         sav_v1cac
111300001011     C                   endif
111400001010
111500001011      * Modifica località
111600001010
111700010119     C     v1loc         ifne      sav_v1loc
111800001011     C                   clear                   wfz1
111900001011     C                   clear                   wfz2
112000001011     C                   clear                   wfz3
112100010119     C                   movel     v1loc         sav_v1loc
112200001011     C                   endif
112300001010
112400001011      * Modifica provincia
112500001010
112600010119     C     v1prc         ifne      sav_v1prc
112700001011     C                   clear                   wfz3
112800010119     C                   movel     v1prc         sav_v1prc
112900001011     C                   endif
113000001010
113100001011     C                   movel     wfzv          E13fzv
113200001011     C                   movel     wfz1          E13fz1
113300001011     C                   movel     wfz2          E13fz2
113400001011     C                   movel     wfz3          E13fz3
113500001011     C                   call      'FNLV13R'
113600001011     C                   parm                    KPJBA
113700001011     C                   parm                    FNLV13DS
113800001011     C                   parm                    TISI95DS
113900001010
114000001011      * Ricontrollo errori
114100001010
114200001011     C                   movel     O13err        werr
114300001011     C                   movel     O13msg        wmsg
114400001011     C                   movel     E13fzv        wfzv
114500001011     C                   movel     E13fz1        wfz1
114600001011     C                   movel     E13fz2        wfz2
114700001011     C                   movel     E13fz3        wfz3
114800001010
114900001011      * Se il livello di affidabilità non è inferiore a 2 e
115000001011      * il livello di reperimento dati non è inferiore a 3
115100001011      * reimposto i dati da TISI95DS
115200001010
115300001011     C     O13ron        ifeq      'S'
115400110121     C                   movel     O95nar        v1nac
115500110121     C                   end
115600110121     C     O13roc        ifeq      'S'
115700110121     C                   movel     O95cap        v1cac
115800110121     C                   end
115900110121     C     O13rop        ifeq      'S'
116000110121     C                   movel     O95prv        v1prc
116100110121     C                   end
116200110121     C     O13rol        ifeq      'S'
116300001011     C                   movel     O95loc        v1loc
116400001011     C                   end
116500001010
116600001011     C     werr          ifne      ' '
116700001011     C     wmsg          andne     *blanks
116800001010
116900001011     C                   select
117000001010
117100001011     C     werr          wheneq    '3'
117200001010     C                   eval      *in63 = *on
117300001010     C                   eval      *in28 = *on
117400001010
117500001011     C     werr          wheneq    '5'
117600001010     C                   eval      *in62 = *on
117700001010     C                   eval      *in28 = *on
117800001010
117900001011     C     werr          wheneq    '4'
118000001010     C                   eval      *in64 = *on
118100001010     C                   eval      *in28 = *on
118200001010
118300001011     C     werr          wheneq    '6'
118400001010     C                   eval      *in65 = *on
118500001010     C                   eval      *in28 = *on
118600001011     C                   endsl
118700001010      *
118800001011     C                   movel(p)  wmsg          v1cmsg
118900001011     C                   end
119000001010
119100001011     C   28              goto      endsrcap
119200001010
119300001011      * Controllo se reimpostata località
119400001010
119500001011     C     O13rol        ifeq      'S'
119600001010     C                   eval      *in63 = *on
119700001010     C                   eval      *in28 = *on
119800001011     C                   movel     msg(19)       v1cmsg
119900001011     C                   goto      endsrcap
120000001011     C                   end
120100001010
120200001011      * Controllo se reimpostata Provincia
120300001010
120400001011     C     O13rop        ifeq      'S'
120500001010     C                   eval      *in64 = *on
120600001010     C                   eval      *in28 = *on
120700001011     C                   movel     msg(21)       v1cmsg
120800001011     C                   goto      endsrcap
120900001011     C                   end
121000001010
121100001011      * Controllo se reimpostato cap
121200001010
121300001011     C     O13roc        ifeq      'S'
121400001010     C                   eval      *in62 = *on
121500001010     C                   eval      *in28 = *on
121600001011     C                   movel     msg(20)       v1cmsg
121700001011     C                   goto      endsrcap
121800001011     C                   end
121900001010
122000001011      * Controllo se reimpostata Nazione
122100001010
122200001011     C     O13ron        ifeq      'S'
122300001010     C                   eval      *in65 = *on
122400001010     C                   eval      *in28 = *on
122500001011     C                   movel     msg(22)       v1cmsg
122600001011     C                   goto      endsrcap
122700001011     C                   end
122800001010
122900001011     C     endsrcap      endsr
123000010730      **********************************************************************
123100010730      * CONTROLLI     * Periodo chiusura dal/al *
123200010730      **********************************************************************
123300010730     C     Sr_Contrchiu  BEGSR
123400010730
123500010730     C                   if        v1cdif <> *zeros
123600010730     C                   clear                   wlbdat
123700010730     C                   z-add     v1cdif        G02dat
123800010730     C                   call      'XSRDA8'
123900010730     C                   parm                    wlbdat
124000010730     C                   if        G02err = '1'
124100010730     C                   eval      *in26 = *ON
124200010730     C                   eval      *in28 = *ON
124300010730     C                   movel     msg(39)       v1cmsg
124400010730     C                   goto      endsrchiu
124500010730     C                   else
124600010730     C                   z-add     G02dat        v1cdif
124700010730     C                   z-add     G02inv        vddif
124800010730     C                   endif
124900010730     C                   endif
125000010730
125100010730     C                   if        v1cdff <> *zeros
125200010730     C                   clear                   wlbdat
125300010730     C                   z-add     v1cdff        G02dat
125400010730     C                   call      'XSRDA8'
125500010730     C                   parm                    wlbdat
125600010730     C                   if        G02err = '1'
125700010730     C                   eval      *in27 = *ON
125800010730     C                   eval      *in28 = *ON
125900010730     C                   movel     msg(39)       v1cmsg
126000010730     C                   goto      endsrchiu
126100010730     C                   else
126200010730     C                   z-add     G02dat        v1cdff
126300010730     C                   z-add     G02inv        vddff
126400010730     C                   endif
126500010730     C                   endif
126600010730
126700010730     C                   if        v1cdif <> *zeros and v1cdff = *zeros
126800010730     C                   seton                                        2827
126900010730     C                   movel     msg(40)       v1cmsg
127000010730     C                   goto      endsrchiu
127100010730     C                   endif
127200010730
127300010730     C                   if        v1cdff <> *zeros and v1cdif = *zeros
127400010730     C                   seton                                        2826
127500010730     C                   movel     msg(40)       v1cmsg
127600010730     C                   goto      endsrchiu
127700010730     C                   endif
127800010730
127900010730     C                   if        vddif > vddff
128000010730     C                   seton                                        2826
128100010730     C                   movel     msg(41)       v1cmsg
128200010730     C                   goto      endsrchiu
128300010730     C                   endif
128400010730
128500010730     C     endsrchiu     endsr
128600010730
128700001011      **********************************************************************
128800001011      * F6 - Conferma
128900001011      **********************************************************************
129000001011     C     Sr_Conferma   BEGSR
129100001011
129200001011     C                   clear                   fnors000
129300001011
129400001011      * Scrivo
129500001011     C                   if        *in01 = *on
129600001011     C                   exsr      Sr_Numor
129700001011     C                   exsr      Sr_Carfile
129800070905      * imposto il codice giro
129900070905     c                   exsr      sr_ors1
130000001011     C                   write     fnors000
130100001011     C                   endif
130200001011
130300001011      * Aggiorno
130400001011     C                   if        *in02 = *on
130500001011     C     kfnor         chain     fnors01L
130600001011     C                   exsr      Sr_Carfile
130700001011     C                   if        %found(fnors01l)
130800070905      * imposto il codice giro
130900070905     c                   exsr      sr_ors1
131000001011     C                   update    fnors000
131100001011     C                   endif
131200001011     C                   endif
131300001011
131400001011     C                   endsr
131500001011      **********************************************************************
131600001011      * F16 - Annullamento
131700001011      **********************************************************************
131800001011     C     Sr_Annulla    BEGSR
131900001011
132000001011     C     kfnor         chain     fnors01L
132100001011     C                   if        %found(fnors01l)
132200001011     C                   movel     'A'           ORSatb
132300010312
132400040818     c                   Z-add     *Date         OrsDtt
132500010312
132600001011     C                   update    fnors000
132700001011     C                   endif
132800001011
132900001011     C                   endsr
133000001011      **********************************************************************
133100001011      * F7 - Ricerca cliente ritiro
133200001011      **********************************************************************
133300001011     C     Sr_RicRitiro  BEGSR
133400001011
133500070808     c                   reset                   FIOR37ds
133600070808     c                   movel     V1Cpoe        I37fgs
133700070808     c                   movel(p)  FIOR37ds      KPJBU
133800070808     c                   call      'FIOR37R'
133900070808     c                   parm                    KPJBA
134000070808     c                   movel     KPJBU         FIOR37ds
134100070808     c                   select
134200070808     c                   when      O37err <> *blanks
134300070808     c                   movel     O37msg        V1Cmsg
134400070808     c                   seton                                        41  28
134500070808     c                   when      O37cro <> *zeros
134600070808     c                   movel     O37cro        ds_V1Ccra
134700070808     c                   exsr      Sr_DecoCRA
134800070808     c                   endsl
134900001011
135000001011     C                   endsr
135100001011      **********************************************************************
135200001011      * F8 - Ricerca cliente ordinante
135300001011      **********************************************************************
135400001011     C     Sr_RicOrdin   BEGSR
135500001011
135600070808     c                   reset                   FIOR37ds
135700070808     c                   movel     V1Cpoe        I37fgs
135800070808     c                   movel(p)  FIOR37ds      KPJBU
135900070808     c                   call      'FIOR37R'
136000070808     c                   parm                    KPJBA
136100070808     c                   movel     KPJBU         FIOR37ds
136200070808     c                   select
136300070808     c                   when      O37err <> *blanks
136400070808     c                   movel     O37msg        V1Cmsg
136500070808     c                   seton                                        42  28
136600070808     c                   when      O37cro <> *zeros
136700070808     c                   movel     O37cro        ds_V1Ccor
136800070808     c                   exsr      Sr_DecoCOR
136900070808     c                   endsl
137000001011
137100001011     C                   endsr
137200001011      **********************************************************************
137300001011      * F9 - Ricerca cliente consegna
137400001011      **********************************************************************
137500001011     C     Sr_RicCons    BEGSR
137600001011
137700070808     c                   reset                   FIOR37ds
137800070808     c                   movel     V1Cpoe        I37fgs
137900070808     C                   movel     V1rsc         I37rag
138000070808     c                   movel(p)  FIOR37ds      KPJBU
138100070808     c                   call      'FIOR37R'
138200070808     c                   parm                    KPJBA
138300070808     c                   movel     KPJBU         FIOR37ds
138400070808     c                   select
138500070808     c                   when      O37err <> *blanks
138600070808     c                   movel     O37msg        V1Cmsg
138700070808     c                   seton                                        56  28
138800070808     c                   when      O37cro <> *zeros
138900070808     c                   movel     O37cro        ds_V1Ccrc
139000070808     c                   exsr      Sr_DecoCRC
139100070808     c                   endsl
139200001011
139300001011     C                   endsr
139400001011      **********************************************************************
139500001011      * PRENDO IL NUOVO NUMERO ORDINE
139600001011      **********************************************************************
139700001011     C     Sr_Numor      BEGSR
139800001011
139900030922     c                   clear                   TRUL33DS
140000030922     c                   Eval      I33cnu = 301
140100030922     c                   Eval      I33po1 = V1cpoe
140200030922     c                   Eval      I33num = 1
140300030922     c                   movel(p)  TRUL33DS      KPJBU
140400030922     c                   call      'TRUL33R'
140500030922     c                   parm                    KPJBA
140600030922     c                   movel     KPJBU         TRUL33DS
140700030922     C                   z-add     O33nrf        v1nor
140800001011
140900001011     C                   endsr
141000001011      **********************************************************************
141100001011      * CARICO IL FILE CON I DATI DELLA VIDEATA
141200001011      **********************************************************************
141300001011     C     Sr_Carfile    BEGSR
141400001011
141500001215     C                   z-add     v1nor         ORSnor
141600001215     C                   z-add     v1cpoe        ORSpoe
141700001011     C                   movel     v1ctor        ORStor
141800010131     C                   movel     'S'           ORStco
141900010119     C                   movel     ds_v1ccor     ORScor
142000010119     C                   movel     ds_v1ccra     ORScra
142100001011     C                   movel     v1rer         ORSrer
142200001011     C                   movel     v1ter         ORSter
142300010119     C                   movel     ds_v1ccrc     ORScrc
142400001011     C                   movel     v1rsc         ORSrsc
142500001011     C                   movel     v1inc         ORSinc
142600001011     C                   movel     v1loc         ORSloc
142700001011     C                   movel     v1cac         ORScac
142800001011     C                   movel     v1prc         ORSprc
142900001011     C                   movel     v1nac         ORSnac
143000001011     C                   movel     v1ffd         ORSffd
143100001011     C                   movel     v1orr         ORSorr
143200010122     C                   movel     v1rmp         ORSrmp
143300001011     C                   movel     v1rlu         ORSrlu
143400001011     C                   movel     v1rma         ORSrma
143500001011     C                   movel     v1rme         ORSrme
143600001011     C                   movel     v1rgi         ORSrgi
143700001011     C                   movel     v1rve         ORSrve
143800001011     C                   movel     v1nam         ORSnam
143900001011     C                   movel     v1ncl         ORSncl
144000001011     C                   movel     v1pkg         ORSpkg
144100001011     C                   movel     v1vlm         ORSvlm
144200001011     C                   movel     v1bnc         ORSbnc
144300001011     C                   movel     v1blc         ORSblc
144400001011     C                   movel     v1att         ORSatt
144500001011     C                   movel     v1mtc         ORSmtc
144600001011     C                   movel     v1pag         ORSpag
144700001011     C                   movel     v1ksc         ORSksc
144800001020     C                   if        v1ctr  <> *blanks
144900001011     C                   movel     v1ctr         ORSctr
145000001020     C                   endif
145100001215     C                   z-add     v1cpoe        ORSpor
145200080530     c                   clear                   orspoc
145300001011     C                   if        v1cpoc <> *blanks
145400001011     C                   movel     v1cpoc        ORSpoc
145500001020     C                   endif
145600001011     C                   movel     v1not1        ORSno1
145700001011     C                   movel     v1not2        ORSno2
145800010118     C                   movel     v1spi         ORSspi
145900010730
146000010730     C                   z-add     vddif         §orsdif
146100010730     C                   z-add     vddff         §orsdff
146200010730     C                   movel     dors01        ORSflo
146300010312
146400040818     c                   Z-add     *Date         OrsDtt
146500001204
146600001204      * Mette sempre in positivo i campi delle quantità
146700001204     C                   mllzo     1             ORSorr
146800001204     C                   mllzo     1             ORSncl
146900001204     C                   mllzo     1             ORSpkg
147000001204     C                   mllzo     1             ORSvlm
147100001204     C                   mllzo     1             ORSbnc
147200001204     C                   mllzo     1             ORSblc
147300001204     C                   mllzo     1             ORSatt
147400001204     C                   mllzo     1             ORSmtc
147500001011
147600001011     C                   endsr
147700070905
147800070905      *--------------------------------------------------------------------*
147900070905      * File FNORS10F
148000070905      *--------------------------------------------------------------------*
148100070905     c     sr_ors1       begsr
148200070905
148300070905     c     kfnor         chain     fnors11l
148400070905     c                   if        %found(fnors11l)
148500080123     c                   if        v1ccgi <> *blanks
148600070905     c                   eval      ors1pocgi = orspor
148700070905     c                   eval      ors1cgi = v1ccgi
148800070905     c                   eval      ors1dtv = *date
148900070905     c                   eval      ors1ute = knmus
149000070905     c                   update    fnors100
149100080123     c                   else
149200080123     c                   delete    fnors100
149300070905     c                   endif
149400080123     c                   endif
149500070905     c                   if        not %found(fnors11l)
149600071018     c                             and v1ccgi <> *blanks
149700070905     c                   clear                   fnors100
149800070905     c                   eval      ors1poe = orspoe
149900070905     c                   eval      ors1nor = orsnor
150000070905     c                   eval      ors1pocgi = orspor
150100070905     c                   eval      ors1cgi = v1ccgi
150200070905     c                   eval      ors1dtv = *date
150300070905     c                   eval      ors1ute = knmus
150400070905     c                   write     fnors100
150500070905     c                   endif
150600070905
150700070905     c                   endsr
150800070905
150900001010      *****************************************************************
151000001010      * ROUTINE INIZIALE
151100001010      *****************************************************************
151200001010     C     *INZSR        BEGSR
151300001009
151400001010     C     *ENTRY        PLIST
151500001010     C                   PARM                    KPJBA
151600001011
151700001011     C                   movel     TIT_A         VTCtit
151800001009
151900001009     C                   movel     kpjbu         parm01
152000001020
152100060203     C                   Z-ADD     1             CODUT             1 0
152200060203
152300060203      * Reperisco dati job
152400060203     c                   exsr      DatiJob
152500001010
152600001009     C     Kfnor         klist
152700001011     C                   kfld                    kpoe
152800001009     C                   kfld                    knor
152900001010
153000001010     C     Ktntam        klist
153100001010     C                   kfld                    kksc
153200001010     C                   kfld                    kctr
153300071108      *
153400071108     c     k02acr13      klist
153500071108     c                   kfld                    KFNacr
153600071108     c                   kfld                    V1Cpoe
153700021202
153800010119     C                   movel     *zeros        ds_v1ccra
153900010119     C                   movel     *zeros        ds_v1ccor
154000010119     C                   movel     *zeros        ds_v1ccrc
154100001009
154200001009     C                   select
154300001009     C                   when      psce = '1'
154400001009     C                   movel     mod(1)        v1mod
154500001110     C                   eval      *in01 = *on
154600001009     C                   when      psce = '2'
154700001009     C                   movel     mod(2)        v1mod
154800001110     C                   eval      *in02 = *on
154900001011     C                   when      psce = '5'
155000001009     C                   movel     mod(3)        v1mod
155100001009     C                   eval      *in03 = *on
155200001009     C                   endsl
155300060104
155400060104      * inibischio il richiamo al fior01r se arrivo già da lui
155500060104     c                   Eval      *in06 = (pfior01 = 'S')
155600001009
155700001011     C                   eval      kpoe = ppoe
155800001011     C                   eval      knor = pnor
155900001011
156000001011     C                   eval      v1nor  = pnor
156100001011     C                   eval      v1cpoe = ppoe
156200001009
156300001009      * Decodifica P.O.gestione
156400001009     C     v1cpoe        chain     azorg01L
156500001009     C                   if        %found(azorg01l)
156600001009     C                   movel     orgDES        v1dpoe
156700001009     C                   else
156800001009     C                   clear                   v1dpoe
156900001010     C                   endif
157000010131
157100010131      * Recupero i dati di default prima con il mio p.o.
157200010131     C                   clear                   ddft
157300010131     C                   clear                   TIBS02DS
157400010131     C                   movel     'C'           T02mod
157500010131     C                   movel     knsif         t02sif
157600010131     C                   movel     'DFT'         t02cod
157700010131     C                   movel(p)  v1cpoe        T02ke1
157800010131     C                   call      'TIBS02R'
157900010131     C                   parm                    KPJBA
158000010131     C                   parm                    TIBS02DS
158100010131     C                   if        t02err =  *blanks
158200010131     C                   movel     t02uni        ddft
158300010131     C                   else
158400010131      * poi con 999 generico se non trovo con il mio p.o.
158500010131     C                   clear                   TIBS02DS
158600010131     C                   movel     'C'           T02mod
158700010131     C                   movel     knsif         t02sif
158800010131     C                   movel     'DFT'         t02cod
158900010131     C                   movel(p)  '999'         T02ke1
159000010131     C                   call      'TIBS02R'
159100010131     C                   parm                    KPJBA
159200010131     C                   parm                    TIBS02DS
159300010131     C                   if        t02err =  *blanks
159400010131     C                   movel     t02uni        ddft
159500010131     C                   endif
159600010131     C                   endif
159700001006
159800001006     C                   endsr
159900060203
160000060203      *---------------------------------------------------------------*
160100060203      * Reperimento Dati del job (Utente/Operativi)                   *
160200060203      *---------------------------------------------------------------*
160300060203     c     DatiJob       BEGSR
160400060203      *
160500060203     c     *dtaara       define    §azute        azuteds
160600060203     c     *dtaara       define    §datiute      ddatiute
160700060203      *
160800060203     c                   in(E)     *dtaara
160900060203     c                   IF        %ERROR or RSUT = *blanks
161000060203     c                   clear                   Tibs34Ds
161100060203     c                   call      'TIBS34R'
161200060203     c                   parm                    Tibs34Ds
161300060203     c                   in        *dtaara
161400060203     c                   ENDIF
161500060203      *
161600060203     c                   ENDSR
161700001010      *---------------------------------------------------------------------------------------------
161800001010** MSG  Lungh. 78                                                            *
161900010420Inserire cliente mittente                                                           1
162000010420Cliente mittente non valido                                                         2
162100001010Cliente ordinante non valido                                                        3
162200001010Immettere i colli                                                                   4
162300001010Immettere il peso                                                                   5
162400001010Immettere il volume                                                                 6
162500001010Immettere i bancali                                                                 7
162600001010Immettere il bilico                                                                 8
162700001010Immettere l'autotreno                                                               9
162800001010Immettere la motrice                                                                10
162900010131Inserire almeno un giorno                                                           11
163000130411--Libero                                                                            12
163100001010Tipo ordine non valido                                                              13
163200001010Inserire tipo ordine                                                                14
163300130411--Libero                                                                            15
163400001010Codice tassazione o codice tariffa errati                                           16
163500070418Filiale consegna errata                                                             17
163600010420Cliente destinatario non valido                                                     18
163700001010Attenzione !! Modificata Località                                                   19
163800001010Attenzione !! Modificato CAP                                                        20
163900001010Attenzione !! Modificata Provincia                                                  21
164000001010Attenzione !! Modificata Nazione                                                    22
164100001011Pagamento a carico dell'ordinante...inserire il codice                              23
164200001012Ora errata                                                                          24
164300001012Immettere i colli ------> ENTER x forzare                                           25
164400001012Immettere il peso ------> ENTER x forzare                                           26
164500001012Immettere il volume ------> ENTER x forzare                                         27
164600001012Immettere i bancali ------> ENTER x forzare                                         28
164700001012Immettere il bilico ------> ENTER x forzare                                         29
164800001012Immettere l'autotreno ------> ENTER x forzare                                       30
164900001012Immettere la motrice ------> ENTER x forzare                                        31
165000130411-- LIBERO <?                                                                        32
165100130411-- LIBERO <?                                                                        33
165200010122Immettere l'ora                                                                     34
165300010123Immettere M o P                                                                     35
165400070925Libero.........................                                                     36
165500010131Immettere almeno una quantità (peso, volume, bancali)                               37
165600130411--Libero                                                                            38
165700010730Data formalmente errata                                                             39
165800010730Inserire entrambe le date                                                           40
165900010730Data iniziale maggiore della data finale                                            41
166000070905Codice Giro non corretto                                                            42
166100001009** MOD  Lungh. 15                                                            *
166200001013  IMMISSIONE                                                                        1
166300001013 MANUTENZIONE                                                                       2
166400001009VISUALIZZAZIONE                                                                     3
166500001013   ANNULLATO                                                                        4
