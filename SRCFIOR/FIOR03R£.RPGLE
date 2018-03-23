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
001800070905      * 07 - procedura GEO ORM attiva
001900010730      * 26 - Data inizio ferie errata
002000010730      * 26 - Data fine ferie errata
002100001010      * 28 - ERRORE GENERICO DSPF
002200001010      * 30 - Comodo
002300001110      * 31 - Comodo
002400001010      * 41 - Cliente ritiro obbligatorio o non valido
002500001010      * 42 - Cliente ordinante non valido
002600001010      * 43 - Colli obbligatori
002700001010      * 44 - Peso obbligatorio
002800001010      * 45 - Volume obbligatorio
002900001010      * 46 - Bancali obbligatori
003000001010      * 47 - Bilico obbligatorio
003100001010      * 48 - Autotreno obbligatorio
003200001010      * 49 - Motrice obbligatoria
003300001010      * 50 - Inserire almeno un dato (ritiro - ora - giorno)
003400001010      * 51 - Zona ritiro errata
003500001010      * 52 - Tipo ordine errato
003600001010      * 53 - Inserire i dati (pagamento - tassazione - tariffa)
003700001010      * 54 - Tassazione e tariffa errati
003800001010      * 55 - P.O. Consegna errato
003900001010      * 56 - Cliente consegna non valido
004000001012      * 58 - Ora errata
004100010131      * 59 - Almeno un gg della settimana
004200001010      * 60 - Posizione cursore Ragione Sociale
004300001010      * 61 - Posizione cursore Indirizzo
004400001010      * 62 - Posizione cursore CAP
004500001010      * 63 - Posizione cursore Località
004600001010      * 64 - Posizione cursore Provincia
004700001010      * 65 - Posizione cursore Nazione
004800001012      * 66 - Enter x forzare colli
004900001012      * 67 - Enter x forzare peso
005000001012      * 68 - Enter x forzare volume
005100001012      * 69 - Enter x forzare bancali
005200001012      * 70 - Enter x forzare bilico
005300001012      * 71 - Enter x forzare autotreno
005400001012      * 72 - Enter x forzare motrice
005500001010      * 90 - Riemissione videata
005600001010      ****************************************************************
005700001010
005800001011     FFNORS01L  UF A E           K DISK
005900070905     ffnors11l  uf a e           k disk
006000001011     FAZORG01L  IF   E           K DISK
006100001006     FFNACR01L  IF   E           K DISK
006200071108     ***ffnacr11l  if   e           k disk
006300071108     fFNACR13L  if   e           k disk
006400010119     FTABEL00F  IF   E           K DISK
006500001010     FTNTAM01L  IF   E           K DISK
006600001009     FFIOR03D   CF   E             WORKSTN
006700001010
006800001010      *------------------------------------------------------------------------*
006900001011     D kpoe            S                   LIKE(V1cpoe)
007000001006     D knor            S                   LIKE(V1nor)
007100001009
007200001011     D kfnacr          S                   LIKE(ACRcro)
007300010119
007400010119     D kcod            S                   LIKE(TBLcod)
007500010119     D kkey            S                   LIKE(TBLkey)
007600010119     D kdes            S             30
007700001010
007800001010     D kksc            S                   LIKE(TAMksc)
007900001010     D kctr            S                   LIKE(TAMctr)
008000001011
008100001011     D kazorg          S                   LIKE(ORGfil)
008200001006
008300001011     D wmsg            S                   LIKE(O13msg)
008400001011     D wfzv            S                   LIKE(E13fzv)
008500001011     D wfz1            S                   LIKE(E13fz1)
008600001011     D wfz2            S                   LIKE(E13fz2)
008700001011     D wfz3            S                   LIKE(E13fz3)
008800001011
008900010119     D sav_acrfcl      S                   LIKE(ACRfcl)
009000010119     D sav_acrfpk      S                   LIKE(ACRfpk)
009100010119     D sav_acrfmc      S                   LIKE(ACRfmc)
009200010119     D sav_acrfbn      S                   LIKE(ACRfbn)
009300010119     D sav_acrfbl      S                   LIKE(ACRfbl)
009400010119     D sav_acrfat      S                   LIKE(ACRfat)
009500010119     D sav_acrfmt      S                   LIKE(ACRfmt)
009600010119     D sav_v1ccra      S                   LIKE(ds_v1ccra)
009700010119     D sav_v1ccrc      S                   LIKE(ds_v1ccrc)
009800010119     D sav_v1cac       S                   LIKE(v1cac)
009900010119     D sav_v1loc       S                   LIKE(v1loc)
010000010119     D sav_v1prc       S                   LIKE(v1prc)
010100001012
010200010730     D vddif           S                   LIKE(v1cdif)
010300010730     D vddff           S                   LIKE(v1cdff)
010400010730
010500001011     D werr            S              1
010600001013
010700010131     D totqta          S              8  3
010800010119     D ore             S              2  0
010900010119     D minuti          S              2  0
011000030922
011100030922     D §kpjbu          S                   LIKE(KPJBU)
011200070919     d wccgi           s                   like(v1ccgi)
011300070919     d wdcgi           s                   like(v1dcgi)
011400001006
011500001215      *  titolo videata (lunghezza massima 33)
011600001215     D TIT_A           C                   CONST('***  ANAGRAFICA RITIRI FISSI-
011700001010     D                                       ***')
011800001006
011900001010      *   S C H I E R E
012000070905     D MSG             S             78    DIM(42) CTDATA PERRCD(1)              MSG VIDEO
012100001011     D MOD             S             15    DIM(4)  CTDATA PERRCD(1)              MODALITA'
012200001006
012300001010      *   D S   I N T E R N E / E S T E R N E
012400001009
012500001009     D WLBDAT          DS                  INZ
012600001009     D  G02DAT                 1      8  0
012700001009     D  G02INV                 9     16  0
012800001009     D  G02ERR                17     17
012900001009     D  G02TGI                18     22  0
013000001020
013100010119     D ds_v1ccra       DS
013200010119     D  v1cra1
013300010119     D  v1cra2
013400010119     D  v1cra3
013500001020
013600010119     D ds_v1ccor       DS
013700010119     D  v1cor1
013800010119     D  v1cor2
013900010119     D  v1cor3
014000001020
014100010119     D ds_v1ccrc       DS
014200010119     D  v1crc1
014300010119     D  v1crc2
014400010119     D  v1crc3
014500001011
014600010119     D ds_tnsd         DS
014700010119     D  ds_cod                        3
014800010119     D  ds_tip                        1
014900010119     D  ds_des                       25
015000010119     D  ds_pra                        3
015100010119
015200010119     D  ds_kkey        DS
015300010208     D  ds_v1cpoe                     3  0
015400010119     D  ds_v1czor                     2
015500001009
015600001009     D Parm01          DS
015700001009     D  ppoe                   1      3  0
015800001009     D  pnor                   4     10  0
015900010124     D  psce                  11     11
016000060104     d  pfior01               12     12
016100060111     d  pcro                  13     22  0
016200001009
016300001010     D TIBS02DS      E DS
016400001010     D FNLV14DS      E DS
016500001010     D FNLV13DS      E DS
016600001010     D TISI95DS      E DS
016700001011     D FIOR01DS      E DS
016800070808     d FIOR37ds      e ds                  inz
016900070808     d  I37opz       e                     inz('1')
017000070808     d  I37cgi       e                     inz(*all'9')
017100071108     d FIOR38ds      e ds                  inz
017200070810      *
017300070810     d Og147         e ds
017400070905     d fidg09ds      e ds
017500070810      *
017600010131     D ddft          E DS
017700010730     D DORS01        E DS
017800030922     d Trul33ds      e ds
017900060203      * Tabella "03" = Codici Zona
018000060203     d ds03          e ds                  inz
018100001010
018200001010     D KPJBA         E DS
018300060203      * - Parametri x Controllo profilo utenti
018400060203     d TIBS34ds      e ds
018500060203      * - Ds di riferimento al file esterno AZUTE00F
018600060203     d AZUTEds       e ds                  extname(AZUTE00F)
018700060203     d dUTE01        e ds
018800060203      * - Ds per dati organigramma
018900060203     d DDatiUte      e ds
019000030922
019100001010     D                SDS
019200001010     D  VTCPGM                 1     10
019300001010
019400001006      *------------------------------------------------------------------------*
019500001009
019600001011      * Se sono in manutenzione/visualizzazione/annullato mi carico i dati dal file
019700001011     C                   if        *in02 = *on or *in03 = *on
019800001011     C     kfnor         chain(n)  fnors01L
019900001009     C                   if        %found(fnors01l)
020000031030     C                   if        ORSatb <> *blanks
020100031030     C   02              eval      *in02 = *off
020200001009     C                   eval      *in04 = *on
020300001009     C                   movel     mod(4)        v1mod
020400001009     C                   endif
020500001009     C                   exsr      Sr_Carvid
020600001009     C                   endif
020700001009     C                   endif
020800060111      * se immissione e richiamato imposto già il cliente ritiro
020900060111     c                   If        *In06 and *In01
021000060111     c                   Move      pcro          ds_v1ccra
021100060111     c                   EndIf
021200001006
021300001006      * Emissione 1° Videata
021400001010
021500001006     C                   do        *hival
021600001010
021700001009     C                   exfmt     fior031
021800001011     C                   setoff                                       28  90
021900001009
022000001011      * F6=Conferma
022100001025     C                   if        *inkf = *on
022200001017     C                   exsr      Sr_Contr
022300001017     C  n28              exsr      Sr_Conferma
022400001017     C  n28              leave
022500001011     C                   endif
022600001009
022700001009      * F7=Int.Cod.Ritiro
022800001011     C   kg              exsr      Sr_RicRitiro
022900001009
023000001009      * F8=Int.Cod.Ordinante
023100001011     C   kh              exsr      Sr_RicOrdin
023200001009
023300001009      * F9=Int.Cod.Consegna
023400001011     C   ki              exsr      Sr_RicCons
023500001011
023600001011      * F12=Ritorno
023700001011     C   kl              leave
023800001011
023900001011      * F16=Annullamento
024000001020     C                   if        *inkq = *on and *in01 = *off
024100001011     C                   exsr      Sr_Annulla
024200001020     C                   leave
024300001011     C                   endif
024400001010
024500001011      * Controlli
024600071108     ***C                   if        *in28 = *off and *in03 = *off
024700071108     ***C                             and *in04 = *off
024800071108     c                   if        *in28 = *off
024900071108     c                   if        *in03 = *off and *in04 = *off
025000001020     C                   exsr      Sr_Contr
025100071108     c                   else
025200071108     c                   exsr      sr_DecoCRA
025300071108     c                   endif
025400001020     C                   endif
025500001010
025600001010     C   28
025700001010     Cor 90              iter
025800001010
025900001006     C                   enddo
026000030922
026100030922     C                   movel(p)  kpjbu         §kpjbu
026200030922     c                   clear                   TRUL33DS
026300030922     c                   eval      I33tla = 'C'
026400030922     c                   movel(p)  TRUL33DS      KPJBU
026500030922     c                   call      'TRUL33R'
026600030922     c                   parm                    KPJBA
026700030922     C                   movel     §kpjbu        kpjbu
026800001010
026900001006     C                   eval      *inlr = *on
027000001009      **********************************************************************
027100001009      * CARICO DATI NELLA VIDEATA DAL FILE
027200001009      **********************************************************************
027300001009     C     Sr_Carvid     begsr
027400001009
027500001011     C                   movel     ORStor        v1ctor
027600010119     C                   movel     ORScor        ds_v1ccor
027700010119     C                   movel     ORScra        ds_v1ccra
027800001009     C                   movel     ORSrer        v1rer
027900001009     C                   movel     ORSter        v1ter
028000001020      * Dati Consegna a
028100010119     C                   movel     ORScrc        ds_v1ccrc
028200010119     C                   eval      *in05 = (ORScrc <> *zeros)
028300001009     C                   movel     ORSrsc        v1rsc
028400001009     C                   movel     ORSinc        v1inc
028500001011     C                   movel     ORSloc        v1loc
028600001009     C                   movel     ORScac        v1cac
028700001009     C                   movel     ORSprc        v1prc
028800001110
028900001009     C                   movel     ORSnac        v1nac
029000001009     C                   movel     ORSffd        v1ffd
029100001110     C                   z-add     ORSorr        v1orr
029200010122     C                   movel     ORSrmp        v1rmp
029300001009     C                   movel     ORSrlu        v1rlu
029400001009     C                   movel     ORSrma        v1rma
029500001009     C                   movel     ORSrme        v1rme
029600001009     C                   movel     ORSrgi        v1rgi
029700001009     C                   movel     ORSrve        v1rve
029800001009     C                   movel     ORSnam        v1nam
029900001110
030000001110     C                   z-add     ORSncl        v1ncl
030100001110     C                   z-add     ORSpkg        v1pkg
030200001110     C                   z-add     ORSvlm        v1vlm
030300001110     C                   z-add     ORSbnc        v1bnc
030400001110     C                   z-add     ORSblc        v1blc
030500001110     C                   z-add     ORSatt        v1att
030600001110     C                   z-add     ORSmtc        v1mtc
030700001110
030800001009     C                   movel     ORSpag        v1pag
030900001110     C                   z-add     ORSksc        v1ksc
031000001009     C                   movel     ORSctr        v1ctr
031100001110     C                   if        ORSksc = *zeros and ORSctr = *zeros
031200001110     C                   clear                   v1ctr
031300001110     C                   endif
031400001110
031500001009     C                   movel     ORSzor        v1czor
031600001110     C                   if        ORSpoc <> *zeros
031700001009     C                   movel     ORSpoc        v1cpoc
031800001110     C                   endif
031900001011     C                   movel     ORSno1        v1not1
032000001011     C                   movel     ORSno2        v1not2
032100010118     C                   movel     ORSspi        v1spi
032200001009
032300010730     C                   movel     ORSflo        dors01
032400010730     C                   z-add     §orsdif       G02inv
032500010730     C                   movel     '3'           G02err
032600010730     C                   call      'XSRDA8'
032700010730     C                   parm                    wlbdat
032800010730     C                   movel     G02dat        v1cdif
032900010730
033000010730     C                   z-add     §orsdff       G02inv
033100010730     C                   movel     '3'           G02err
033200010730     C                   call      'XSRDA8'
033300010730     C                   parm                    wlbdat
033400010730     C                   movel     G02dat        v1cdff
033500070905
033600070905      * recupero il codice giro
033700070905     c                   clear                   v1ccgi
033800070905     c     kfnor         chain(n)  fnors11l
033900070905     c                   if        %found(fnors11l)
034000070905     c                   eval      v1ccgi = ors1cgi
034100070905     c                   endif
034200010730
034300001020     C                   exsr      Sr_Decod
034400001009
034500001009     C                   endsr
034600001009      **********************************************************************
034700001009      * DECODIFICHE
034800001009      **********************************************************************
034900001009     C     Sr_Decod      begsr
035000001009
035100001010     C                   clear                   v1dcor
035200001010     C                   clear                   v1icor
035300001010     C                   clear                   v1pcor
035400001010     C                   clear                   v1lcor
035500001010     C                   clear                   v1rcor
035600001010     C                   clear                   v1ncor
035700001010
035800001010     C                   clear                   v1dcra
035900001010     C                   clear                   v1icra
036000001010     C                   clear                   v1pcra
036100001010     C                   clear                   v1lcra
036200001010     C                   clear                   v1rcra
036300001010     C                   clear                   v1ncra
036400071108     ***c                   clear                   v1ccgiacr
036500071108     ***c                   clear                   v1dcgiacr
036600071108     c                   clear                   V1Ccgi1
036700071108     c                   clear                   V1Ccgi2
036800071108     c                   clear                   V1Ccgi3
036900010124
037000010124     C                   clear                   v1dzor
037100070905     c                   clear                   v1dcgi
037200010124     C                   clear                   v1dtor
037300001009
037400001010      * Cliente Ordinante
037500010119     C                   if        ds_v1ccor <> *zeros
037600001011     C                   exsr      Sr_Decocor
037700001009     C                   endif
037800001009
037900001011      * Cliente Ritiro
038000010119     C                   if        ds_v1ccra <> *zeros
038100001011     C                   exsr      Sr_Decocra
038200001011     C                   endif
038300001010
038400001010      * Cliente Consegna
038500010119     C                   if        ds_v1ccrc <> *zeros
038600001011     C                   exsr      Sr_Decocrc
038700001010     C                   endif
038800001110
038900010124      * Zona ritiro
039000010215     C                   clear                   ds_kkey
039100010119     C                   movel     v1cpoe        ds_v1cpoe
039200010119     C                   eval      ds_v1czor = v1czor
039300010119     C                   movel     ds_kkey       kkey
039400071108     C     ktab          chain     tabel
039500010119     C                   if        %found(tabel00f)
039600010119     C                   movel     TBLuni        v1dzor
039700010119     C                   endif
039800070905
039900070905      * giro
040000070919     c                   eval      wccgi = v1ccgi
040100070905     c                   exsr      sr_decocgi
040200070919     c                   eval      v1dcgi = wdcgi
040300001110
040400010124      * P.O. Consegna
040500001110     C                   movel     v1cpoc        kazorg
040600001110     C     kazorg        chain     azorg01L
040700001110     C                   if        %found(azorg01l)
040800001110     C                   movel     ORGdes        v1dpoc
040900001110     C                   endif
041000001110
041100010124      * Tipo ORM
041200001110     C                   clear                   TIBS02DS
041300001110     C                   movel     'C'           T02mod
041400001110     C                   movel     knsif         t02sif
041500001110     C                   movel     'TOR'         t02cod
041600001110     C                   movel(p)  v1ctor        T02ke1
041700001110     C                   call      'TIBS02R'
041800001110     C                   parm                    KPJBA
041900001110     C                   parm                    TIBS02DS
042000001110     C                   movel     t02uni        v1dtor
042100001009
042200001013     C                   endsr
042300001011      **********************************************************************
042400001011      * DECODIFICHE     * cliente ordinante *
042500001011      **********************************************************************
042600001011     C     Sr_Decocor    begsr
042700001011
042800010119     C                   movel     ds_v1ccor     Kfnacr
042900001011     C     kfnacr        chain     fnacr01l
043000001012     C                   if        %found(fnacr01l) and ACRatb = *blanks
043100001012     C*                            and ACRtac = 'O'
043200001011     C                   movel     ACRrsr        v1dcor
043300001011     C                   movel     ACRinr        v1icor
043400001011     C                   movel     ACRcar        v1pcor
043500001011     C                   movel     ACRlor        v1lcor
043600001011     C                   movel     ACRprr        v1rcor
043700001011     C                   movel     ACRnar        v1ncor
043800001011     C                   endif
043900001011
044000001011     C                   endsr
044100001011      **********************************************************************
044200001011      * DECODIFICHE     * cliente ritiro *
044300001011      **********************************************************************
044400001011     C     Sr_Decocra    begsr
044500001011
044600010119     C                   movel     ds_v1ccra     Kfnacr
044700001011     C     kfnacr        chain     fnacr01l
044800071108if  1C                   IF        %found(fnacr01l) and ACRatb = *blanks
044900071105     C******                       and ACRtac = 'R'
045000071108      *
045100001011     C                   movel     ACRrsr        v1dcra
045200001011     C                   movel     ACRinr        v1icra
045300001011     C                   movel     ACRcar        v1pcra
045400001011     C                   movel     ACRlor        v1lcra
045500001011     C                   movel     ACRprr        v1rcra
045600001011     C                   movel     ACRnar        v1ncra
045700070919      * recupero il giro dell'anagrafica
045800071108     ***c     kfnacr        chain     fnacr11l
045900071108     ***c                   if        %found(fnacr11l) and acr1cgi <> *blanks and
046000071108     ***c                             acr1pocgi = v1cpoe
046100071108     ***c                   eval      v1ccgiacr = acr1cgi
046200071108     ***c                   endif
046300071108     c     k02acr13      setll     FNACR100
046400071108     c     k02acr13      reade     FNACR100
046500071108do  2c                   dow       NOT  %eof(FNACR13L)
046600071108sel 3c                   select
046700071108w   3c                   when      ACR1atb <> ACRatb
046800071108w   3c                   when      ACR1ain  = '='
046900071108     c                   eval      V1Ccgi1  = ACR1cgi
047000071108w   3c                   when      ACR1ain  = '>'
047100071108     c                   eval      V1Ccgi2  = ACR1cgi
047200071108w   3c                   when      ACR1ain  = '<'
047300071108     c                   eval      V1Ccgi3  = ACR1cgi
047400071108e   3c                   endsl
047500071108     c     k02acr13      reade     FNACR100
047600071108e   2c                   enddo
047700071108      *
047800071108e   1C                   ENDIF
047900071108     *** * se ho un giro recuperato da FNACR lo decodifico
048000071108     ***c                   if        v1ccgiacr <> *blanks
048100071108     ***c                   eval      wccgi = v1ccgiacr
048200071108     ***c                   exsr      sr_decocgi
048300071108     ***c                   eval      v1dcgiacr = wdcgi
048400071108     ***c                   endif
048500071108      *
048600071108      * Interrogazione giri ritiro per cliente/filiale (se richiesta)
048700071108     c                   if        V1CcgiX  = '?'
048800071108     c                   clear                   V1CcgiX
048900071108     c                   clear                   FIOR38ds
049000071108     c                   eval      I38cro   = KFNacr
049100071108     c                   eval      I38pocgi = V1Cpoe
049200071108     c                   movel(p)  FIOR38ds      KPJBU
049300071108     c                   call      'FIOR38R'
049400071108     c                   parm                    KPJBA
049500071108     c                   endif
049600010122
049700001011     C                   endsr
049800001011      **********************************************************************
049900001011      * DECODIFICHE     * cliente consegna *
050000001011      **********************************************************************
050100001011     C     Sr_Decocrc    begsr
050200001011
050300001011     C                   clear                   v1rsc
050400001011     C                   clear                   v1inc
050500001011     C                   clear                   v1cac
050600001011     C                   clear                   v1loc
050700001011     C                   clear                   v1prc
050800001011     C                   clear                   v1nac
050900001011
051000010119     C                   movel     ds_v1ccrc     Kfnacr
051100001011     C     kfnacr        chain     fnacr01l
051200001012     C                   if        %found(fnacr01l) and ACRatb = *blanks
051300001011     C                   movel     ACRrsr        v1rsc
051400001011     C                   movel     ACRinr        v1inc
051500001011     C                   movel     ACRcar        v1cac
051600001011     C                   movel     ACRlor        v1loc
051700001011     C                   movel     ACRprr        v1prc
051800001011     C                   movel     ACRnar        v1nac
051900001011     C                   endif
052000001011
052100001011     C                   endsr
052200070905
052300070905      *------------------------------------------------------------------------*
052400070905      *   Controllo e decodifica giro
052500070905      *------------------------------------------------------------------------*
052600070905     c     sr_decocgi    begsr
052700070905
052800070905     c                   clear                   fidg09ds
052900070905     c                   eval      d09iop0 = '001'
053000070905     c                   eval      d09ifgs = v1cpoe
053100070905     c                   eval      d09idat = *date
053200070919     c                   eval      d09icgi = wccgi
053300070905     c                   eval      d09itug = 'R'
053400070905     c                   eval      kpjbu = fidg09ds
053500070905     c                   call      'FIDG09R'
053600070905     c                   parm                    kpjba
053700070905     c                   eval      fidg09ds = kpjbu
053800070905     c                   if        d09ocgi <> ' '
053900070919     c                   eval      wdcgi = d09odes
054000070919     c                   else
054100070919     c                   clear                   wdcgi
054200070905     c                   endif
054300070905
054400070905     c                   endsr
054500070905
054600001010      **********************************************************************
054700001010      * CONTROLLI
054800001010      **********************************************************************
054900001010     C     Sr_Contr      begsr
055000001006
055100001009      * Reset indicatori
055200001010     C                   setoff                                       414243
055300001010     C                   setoff                                       444546
055400001010     C                   setoff                                       474849
055500001010     C                   setoff                                       505152
055600001010     C                   setoff                                       535455
055700070925     C                   setoff                                       56  58
055800010131     C                   setoff                                       59
055900001011     C                   setoff                                       606162
056000001011     C                   setoff                                       636465
056100010730     C                   setoff                                       2627
056200001009
056300001010      * Cliente ritiro
056400010119     C                   if        ds_v1ccra <> *zeros
056500001010     C                   exsr      Sr_Contrcra
056600001011     C                   exsr      Sr_Carflag
056700010119     C                   if        *in01 = *on and ds_v1ccra <> sav_v1ccra
056800010119     C                   exsr      Sr_Caranacr
056900010119     C                   eval      sav_v1ccra = ds_v1ccra
057000010131     C  nkf              goto      endsrcontr
057100001011     C                   endif
057200001010     C                   else
057300001010     C                   movel     msg(1)        v1cmsg
057400001010     C                   seton                                        41  28
057500001010     C                   endif
057600001011     C   28              goto      endsrcontr
057700001006
057800001010      * Cliente ordinante
057900010119     C                   if        ds_v1ccor <> *zeros
058000001010     C                   exsr      Sr_Contrcor
058100001013     C   28              goto      endsrcontr
058200001013     C                   exsr      Sr_Decocor
058300001010     C                   endif
058400001006
058500001010      * Colli - Peso - Volume - Bancali - Bilico - Autotreno - Motrice
058600001010     C                   exsr      Sr_Contrqta
058700001011     C   28              goto      endsrcontr
058800001010
058900010131      * Ritiro Ora - M/P - Giorni
059000010131     C                   exsr      Sr_Contrrit
059100010131     C   28              goto      endsrcontr
059200001010
059300001010      * Zona ritiro
059400001010     C                   exsr      Sr_Contrzor
059500001010     C   90
059600001011     Cor 28              goto      endsrcontr
059700070905
059800070905      * giro
059900070905     c                   exsr      sr_contrcgi
060000070905     c   28              leavesr
060100001010
060200001010      * Pagamento - Cod.Tassazione - Cod.Tariffa
060300001011     C                   exsr      Sr_Contrpag
060400001011     C   28              goto      endsrcontr
060500001010
060600001010      * P.O. Consegna
060700001110     C                   if        v1cpoc <> *blanks
060800001011     C                   exsr      Sr_Contrpoc
060900001110     C                   endif
061000001011     C   90
061100001011     Cor 28              goto      endsrcontr
061200001010
061300001010      * Consegna a
061400010119     C                   if        ds_v1ccrc <> *zeros
061500001010     C                   eval      *in05 = *on
061600001010     C                   exsr      Sr_Contrcrc
061700010119     C                   eval      sav_v1ccrc = ds_v1ccrc
061800001010     C                   else
061900110121     C****               eval      *in56 = (ds_v1ccrc <> sav_v1ccrc)
062000010119     C                   clear                   sav_v1ccrc
062100001011     C                   if        v1rsc <> *blanks or v1inc <> *blanks
062200001010     C                   exsr      Sr_Contrsrc
062300001010     C                   endif
062400001011     C                   if        v1cac <> *blanks or v1loc <> *blanks or
062500001011     C                             v1prc <> *blanks or v1nac <> *blanks
062600001011     C                   exsr      Sr_Contrcap
062700001011     C  n28              exsr      Sr_Contrsrc
062800001011     C                   endif
062900001010     C                   eval      *in05 = *off
063000001010     C                   endif
063100001011     C   28              goto      endsrcontr
063200010131
063300010131      * Tipo ordine
063400010131     C                   if        v1ctor = *blanks
063500010131     C                   eval      v1ctor = d§dfttor
063600010131     C                   endif
063700010131     C                   exsr      Sr_Contrtor
063800010131     C   90
063900010131     Cor 28              goto      endsrcontr
064000001010
064100010730      * Periodo chiusura
064200010730     C                   exsr      Sr_Contrchiu
064300010730     C   28              goto      endsrcontr
064400010730
064500001010     C                   exsr      Sr_Decod
064600001006
064700001011     C     endsrcontr    endsr
064800001010      **********************************************************************
064900001010      * CONTROLLI     * cliente ritiro *
065000001010      **********************************************************************
065100001011     C     Sr_Contrcra   begsr
065200001010
065300010119     C                   movel     ds_v1ccra     kfnacr
065400001011     C     kfnacr        chain     fnacr01l
065500001010     C                   if        %found(fnacr01l)
065600071105     C*******            if        ACRtac <> 'R' or ACRatb <> *blanks
065700071105     C                   if        ACRatb <> *blanks
065800001010     C                   movel     msg(2)        v1cmsg
065900001010     C                   seton                                        41  28
066000001010     C                   endif
066100001010     C                   else
066200001010     C                   movel     msg(2)        v1cmsg
066300001010     C                   seton                                        41  28
066400001010     C                   endif
066500010124
066600001010     C                   endsr
066700001011      **********************************************************************
066800001011      * CARICO DATI DA ANAGRAFICO RITIRI
066900001011      **********************************************************************
067000010119     C     Sr_Caranacr   begsr
067100001011
067200010131     C                   movel     ACRksc        v1ksc
067300040629     c                   If        AcrCcc = 999
067400040629     c                   Clear                   V1Ctr
067500040629     c                   Else
067600040629     c                   Move      AcrCcc        V1Ctr
067700040629     c                   EndIf
067800001011     C                   movel     ACRper        v1rer
067900001011     C                   movel     ACRtel        v1ter
068000001011     C                   movel     ACRntm        v1nam
068100001011     C                   movel     ACRznr        v1czor
068200001011     C                   movel     ACRno1        v1not1
068300001011     C                   movel     ACRno2        v1not2
068400010119     C                   movel     ACRspi        v1spi
068500001013
068600001013      * Controllo i flag per posizionamento cursore
068700001013     C                   eval      *in43 = (ACRfcl = 'O')
068800001013     C   43              goto      endflg
068900001013     C                   eval      *in44 = (ACRfpk = 'O')
069000001013     C   44              goto      endflg
069100001013     C                   eval      *in45 = (ACRfmc = 'O')
069200001013     C   45              goto      endflg
069300001013     C                   eval      *in46 = (ACRfbn = 'O')
069400001013     C   46              goto      endflg
069500001013     C                   eval      *in47 = (ACRfbl = 'O')
069600001013     C   47              goto      endflg
069700001013     C                   eval      *in48 = (ACRfat = 'O')
069800001013     C   48              goto      endflg
069900001013     C                   eval      *in49 = (ACRfmt = 'O')
070000001013     C   49              goto      endflg
070100001013     C                   eval      *in43 = (ACRfcl = 'F')
070200001013     C   43              goto      endflg
070300001013     C                   eval      *in44 = (ACRfpk = 'F')
070400001013     C   44              goto      endflg
070500001013     C                   eval      *in45 = (ACRfmc = 'F')
070600001013     C   45              goto      endflg
070700001013     C                   eval      *in46 = (ACRfbn = 'F')
070800001013     C   46              goto      endflg
070900001013     C                   eval      *in47 = (ACRfbl = 'F')
071000001013     C   47              goto      endflg
071100001013     C                   eval      *in48 = (ACRfat = 'F')
071200001013     C   48              goto      endflg
071300001013     C                   eval      *in49 = (ACRfmt = 'F')
071400001013     C   49              goto      endflg
071500001013     C                   eval      *in43 = *on
071600001013     C     endflg        tag
071700010122
071800010122     C                   exsr      Sr_Decocra
071900001011
072000001011      * Recupero i dati del cliente ordinante se inserito
072100001011     C                   if        ACRcco <> *zeros
072200010119     C                   movel     ACRcco        ds_v1ccor
072300001013     C                   exsr      Sr_Decocor
072400001011     C                   endif
072500001011
072600001011     C                   endsr
072700001011      **********************************************************************
072800010131      * CARICO FLAG COLLI - PESO - VOLUME - ETC ETC
072900001011      **********************************************************************
073000001011     C     Sr_Carflag    begsr
073100001011
073200010119     C                   eval      sav_acrfcl = ACRfcl
073300010119     C                   eval      sav_acrfpk = ACRfpk
073400010119     C                   eval      sav_acrfmc = ACRfmc
073500010119     C                   eval      sav_acrfbn = ACRfbn
073600010119     C                   eval      sav_acrfbl = ACRfbl
073700010119     C                   eval      sav_acrfat = ACRfat
073800010119     C                   eval      sav_acrfmt = ACRfmt
073900001011
074000001011     C                   endsr
074100001010      **********************************************************************
074200001010      * CONTROLLI     * cliente ordinante *
074300001010      **********************************************************************
074400001011     C     Sr_Contrcor   begsr
074500001010
074600010119     C                   movel     ds_v1ccor     kfnacr
074700001011     C     kfnacr        chain     fnacr01l
074800001010     C                   if        %found(fnacr01l)
074900001013     C                   if        ACRatb <> *blanks
075000001013     C*                            or ACRtac <> 'O'
075100001010     C                   movel     msg(3)        v1cmsg
075200001010     C                   seton                                        42  28
075300001010     C                   endif
075400001010     C                   else
075500001010     C                   movel     msg(3)        v1cmsg
075600001010     C                   seton                                        42  28
075700001010     C                   endif
075800001010
075900001010     C                   endsr
076000001010      **********************************************************************
076100001010      * CONTROLLI     * colli - peso - volume - etc etc *
076200001010      **********************************************************************
076300001010     C     Sr_Contrqta   begsr
076400010119
076500010119      * Se il flag è F può non essere inserita la qtà
076600010119      * Se il flag è O la qtà deve essere inserita
076700010119     C                   if        sav_acrfcl <> *blanks and v1ncl = *zeros
076800010119     C                   if        sav_acrfcl = 'F' and *in66 = *off
076900010119     C                   movel     msg(25)       v1cmsg
077000010119     C                   seton                                        436628
077100010119     C                   goto      endsrcqta
077200010119     C                   endif
077300010119     C                   if        sav_acrfcl = 'O'
077400010119     C                   movel     msg(4)        v1cmsg
077500010119     C                   seton                                        43  28
077600010119     C                   goto      endsrcqta
077700010119     C                   endif
077800010119     C                   endif
077900010119
078000010119     C                   if        sav_acrfpk <> *blanks and v1pkg = *zeros
078100010119     C                   if        sav_acrfpk = 'F' and *in67 = *off
078200010119     C                   movel     msg(26)       v1cmsg
078300010119     C                   seton                                        446728
078400010119     C                   goto      endsrcqta
078500010119     C                   endif
078600010119     C                   if        sav_acrfpk = 'O'
078700010119     C                   movel     msg(5)        v1cmsg
078800010119     C                   seton                                        44  28
078900010119     C                   goto      endsrcqta
079000010119     C                   endif
079100010119     C                   endif
079200010119
079300010119     C                   if        sav_acrfmc <> *blanks and v1vlm = *zeros
079400010119     C                   if        sav_acrfmc = 'F' and *in68 = *off
079500010119     C                   movel     msg(27)       v1cmsg
079600010119     C                   seton                                        456828
079700010119     C                   goto      endsrcqta
079800010119     C                   endif
079900010119     C                   if        sav_acrfmc = 'O'
080000010119     C                   movel     msg(6)        v1cmsg
080100010119     C                   seton                                        45  28
080200010119     C                   goto      endsrcqta
080300010119     C                   endif
080400010119     C                   endif
080500010119
080600010119     C                   if        sav_acrfbn <> *blanks and v1bnc = *zeros
080700010119     C                   if        sav_acrfbn = 'F' and *in69 = *off
080800010119     C                   movel     msg(28)       v1cmsg
080900010119     C                   seton                                        466928
081000010119     C                   goto      endsrcqta
081100010119     C                   endif
081200010119     C                   if        sav_acrfbn = 'O'
081300010119     C                   movel     msg(7)        v1cmsg
081400010119     C                   seton                                        46  28
081500010119     C                   goto      endsrcqta
081600010119     C                   endif
081700010119     C                   endif
081800010119
081900010119     C                   if        sav_acrfbl <> *blanks and v1blc = *zeros
082000010119     C                   if        sav_acrfbl = 'F' and *in70 = *off
082100010119     C                   movel     msg(29)       v1cmsg
082200010119     C                   seton                                        477028
082300010119     C                   goto      endsrcqta
082400010119     C                   endif
082500010119     C                   if        sav_acrfbl = 'O'
082600010119     C                   movel     msg(8)        v1cmsg
082700010119     C                   seton                                        47  28
082800010119     C                   goto      endsrcqta
082900010119     C                   endif
083000010119     C                   endif
083100010119
083200010119     C                   if        sav_acrfat <> *blanks and v1att = *zeros
083300010119     C                   if        sav_acrfat = 'F' and *in71 = *off
083400010119     C                   movel     msg(30)       v1cmsg
083500010119     C                   seton                                        487128
083600010119     C                   goto      endsrcqta
083700010119     C                   endif
083800010119     C                   if        sav_acrfat = 'O'
083900010119     C                   movel     msg(9)        v1cmsg
084000010119     C                   seton                                        48  28
084100010119     C                   goto      endsrcqta
084200010119     C                   endif
084300010119     C                   endif
084400010119
084500010119     C                   if        sav_acrfmt <> *blanks and v1mtc = *zeros
084600010119     C                   if        sav_acrfmt = 'F' and *in72 = *off
084700010119     C                   movel     msg(31)       v1cmsg
084800010119     C                   seton                                        497228
084900010119     C                   goto      endsrcqta
085000010119     C                   endif
085100010119     C                   if        sav_acrfmt = 'O'
085200010119     C                   movel     msg(10)       v1cmsg
085300010119     C                   seton                                        49  28
085400010119     C                   goto      endsrcqta
085500010119     C                   endif
085600010119     C                   endif
085700001010
085800010131      * Almeno 1 tra volume peso e bancali deve essere inserito
085900010131     C                   eval      totqta =  v1pkg + v1vlm + v1bnc
086000010131     C                   if        totqta = *zeros
086100010131     C                   movel     msg(37)       v1cmsg
086200010131     C                   seton                                        44  28
086300010131     C                   goto      endsrcqta
086400010131     C                   endif
086500001010
086600001012     C     endsrcqta     endsr
086700010131      **********************************************************************
086800010131      * CONTROLLI     * ritiro ora M/P giorno *
086900010131      **********************************************************************
087000010131     C     Sr_Contrrit   begsr
087100010131
087200010131      * Mattino/Pomeriggio
087300010131     C                   if        v1rmp = *blanks
087400010131     C                   movel     msg(35)       v1cmsg
087500010131     C                   seton                                        50  28
087600010131     C                   goto      endsrrit
087700010131     C                   endif
087800010131      * Ora
087900010131     C                   if        v1orr > *zeros
088000010131     C                   movel     v1orr         ore
088100010131     C                   move      v1orr         minuti
088200010131
088300010131     C                   if        ore > 24 or minuti > 59
088400010131     C                   seton                                        58  28
088500010131     C                   movel     msg(24)       v1cmsg
088600010131     C                   goto      endsrrit
088700010131     C                   endif
088800010131
088900010131     C                   else
089000010131     C                   seton                                        58  28
089100010131     C                   movel     msg(34)       v1cmsg
089200010131     C                   goto      endsrrit
089300010131     C                   endif
089400010131      * Giorno
089500010131     C                   if        v1rlu = *blanks and v1rma = *blanks and
089600010131     C                             v1rme = *blanks and v1rgi = *blanks and
089700010131     C                             v1rve = *blanks
089800010131     C                   movel     msg(11)       v1cmsg
089900010131     C                   seton                                        59  28
090000010131     C                   goto      endsrrit
090100010131     C                   endif
090200010131
090300010131     C     endsrrit      endsr
090400001009      **********************************************************************
090500001010      * CONTROLLI     * zona ritiro *
090600001009      **********************************************************************
090700001009     C     Sr_Contrzor   begsr
090800001013
090900010219     C                   clear                   ds_kkey
091000010219
091100010301     C                   if        v1czor <> *blanks
091200001009      *    Ricerca Zona Ritiro con "?"
091300001009     C     '?'           scan      v1czor                                 30
091400001009     C     *in30         ifeq      *on
091500001010     C                   eval      *in51 = *on
091600001009     C                   eval      *in90 = *on
091700010208     C                   clear                   ds_kkey
091800010119     C                   movel     v1cpoe        ds_v1cpoe
091900010119     C                   movel     ds_kkey       kkey
092000010119     C                   CALL      'X§TABER'
092100010119     C                   PARM                    codut
092200010119     C                   PARM                    kcod
092300010119     C                   PARM                    kkey
092400010119     C                   PARM                    kdes
092500010215     C                   if        kkey = *blanks
092600010215     C                   clear                   v1czor
092700010215     C                   goto      endsrczor
092800010215     C                   endif
092900010119     C                   movel     kkey          ds_kkey
093000010119     C                   eval      v1czor = ds_v1czor
093100010119     C                   movel     kdes          v1dzor
093200001009     C                   endif
093300010208
093400010208     C                   if        ds_v1cpoe <> *zeros and ds_v1cpoe <> v1cpoe
093500010212     C                             and v1czor <> *blanks
093600010208     C                   movel     msg(38)       v1cmsg
093700010212     C                   clear                   v1czor
093800010215     C                   clear                   ds_kkey
093900010208     C                   seton                                        51  28
094000010208     C                   goto      endsrczor
094100010208     C                   endif
094200010212
094300010212     C                   testn                   v1czor               31
094400010212     C                   move      v1czor        w001a             1
094500010212     C     *in31         ifeq      *off
094600010212     C     w001a         orlt      '0'
094700010212     C                   seton                                        51  28
094800010212     C                   movel     msg(12)       v1cmsg
094900010212     C                   goto      endsrczor
095000010212     C                   endif
095100001009
095200060203     c                   clear                   V1Dzor
095300010215     C                   clear                   ds_kkey
095400001020     C                   if        v1czor <> *blanks
095500010119     C                   movel     v1cpoe        ds_v1cpoe
095600010119     C                   eval      ds_v1czor = v1czor
095700010119     C                   movel     ds_kkey       kkey
095800071108     C     ktab          chain     tabel
095900010208     C                   if        not%found(tabel00f)
096000010208     C                   movel     msg(12)       v1cmsg
096100010208     C                   seton                                        51  28
096200010208     C                   goto      endsrczor
096300010208     C                   endif
096400010208     C                   if        TBLFLG <> *blanks
096500001010     C                   movel     msg(12)       v1cmsg
096600001010     C                   seton                                        51  28
096700010208     C                   goto      endsrczor
096800001010     C                   endif
096900010208
097000060203     c                   movel     TBLuni        ds03
097100060203     c                   movel     §03des        V1Dzor
097200060203     c                   if        §03nuc = 'S'
097300060203     c                   movel     Msg(15)       V1Cmsg
097400060203     c                   seton                                        51  28
097500060203     c                   leavesr
097600060203     c                   endif
097700001013     C                   endif
097800010301     C                   endif
097900001009
098000001009     C     endsrczor     endsr
098100070905
098200070905      *------------------------------------------------------------------------*
098300070905      * CODICE GIRO
098400070905      *------------------------------------------------------------------------*
098500070905     c     sr_contrcgi   begsr
098600070905
098700070905     c                   clear                   v1dcgi
098800070905
098900070905      * ricerca giro con "?"
099000070905     c                   if        %scan('?':v1ccgi) > *zeros
099100070905     c                   clear                   fidg09ds
099200070905     c                   eval      d09iop0 = 'P01'
099300070905     c                   eval      d09ifgs = v1cpoe
099400070905     c                   eval      d09idat = *date
099500070905     c                   eval      d09itug = 'R'
099600070905     c                   eval      kpjbu = fidg09ds
099700070905     c                   call      'FIDG09R'
099800070905     c                   parm                    kpjba
099900070905     c                   eval      fidg09ds = kpjbu
100000070905      *     imposto il giro scelto
100100070905     c                   eval      v1ccgi = d09ocgi
100200070905     c                   endif
100300070905      *     controllo
100400070905     c                   if        v1ccgi <> *blank
100500070919     c                   eval      wccgi = v1ccgi
100600070905     c                   exsr      sr_decocgi
100700070905      *     giro errato
100800070905     c                   if        d09oerr = '1'
100900070905     c                   eval      *in28 = *on
101000070905     c                   eval      *in51 = *on
101100070905     c                   eval      v1cmsg = msg(42)
101200070905     c                   leavesr
101300070905     c                   endif
101400070919     c                   eval      v1dcgi = wdcgi
101500070905     c                   endif
101600070905
101700070905     c                   endsr
101800070905
101900001010      **********************************************************************
102000001010      * CONTROLLI     * tipo ordine *
102100001010      **********************************************************************
102200001010     C     Sr_Contrtor   begsr
102300001013
102400001010     C     '?'           scan      v1ctor                                 30
102500001010     C     *in30         ifeq      *on
102600001010     C                   eval      *in52 = *on
102700001010     C                   eval      *in90 = *on
102800001010     C                   clear                   TIBS02DS
102900001010     C                   movel     'R'           t02mod
103000001010     C                   movel     knsif         t02sif
103100001010     C                   movel     'TOR'         t02cod
103200001010     C                   call      'TIBS02R'
103300001010     C                   parm                    KPJBA
103400001010     C                   parm                    TIBS02DS
103500001011     C                   movel     T02ke1        v1ctor
103600001012     C                   movel     T02uni        v1dtor
103700001010     C                   goto      endsrctor
103800001010     C                   endif
103900001010
104000001012     C                   if        v1ctor <> *blanks
104100001010     C                   movel     'C'           T02mod
104200001010     C                   movel     knsif         t02sif
104300001010     C                   movel     'TOR'         t02cod
104400001011     C                   movel(p)  v1ctor        T02ke1
104500001010     C                   call      'TIBS02R'
104600001010     C                   parm                    KPJBA
104700001010     C                   parm                    TIBS02DS
104800001010     C                   if        t02err <> *blanks
104900001010     C                   movel     msg(13)       v1cmsg
105000001010     C                   seton                                        52  28
105100001010     C                   else
105200001010     C                   movel     t02ke1        v1ctor
105300001010     C                   movel     t02uni        v1dtor
105400001010     C                   endif
105500001012     C                   else
105600001012     C                   movel     msg(14)       v1cmsg
105700001012     C                   seton                                        52  28
105800001012     C                   endif
105900001010
106000001010     C     endsrctor     endsr
106100001010      **********************************************************************
106200001010      * CONTROLLI     * pagamento *
106300001010      **********************************************************************
106400001010     C     Sr_Contrpag   begsr
106500001010
106600001110      * Se inseriti tariffa o codice deve essere inserito anche chi paga
106700010131     C                   if        v1ksc <> *zeros or v1ctr <> *blanks
106800001110
106900001110      * Controlla validità codice + tariffa
107000001110     C                   if        v1ksc <> *zeros and v1ctr <> *blanks
107100001110     C                   z-add     v1ksc         kksc
107200001010     C                   movel     v1ctr         kctr
107300071108     C     ktntam        chain     tntam000
107400001010     C                   if        %found(tntam01l)
107500001010     C                   if        TAMatb <> *blanks
107600001010     C                   movel     msg(16)       v1cmsg
107700001010     C                   seton                                        54  28
107800001110     C                   goto      endsrcpag
107900001010     C                   endif
108000001010     C                   else
108100001010     C                   movel     msg(16)       v1cmsg
108200001010     C                   seton                                        54  28
108300001110     C                   goto      endsrcpag
108400001110     C                   endif
108500001110     C                   endif
108600001010     C                   endif
108700001110      * Se inserito chi paga il pagante deve essere codificato
108800001110     C                   if        v1pag <> *blanks
108900010119     C                   if        v1pag = 'O' and ds_v1ccor = *zeros
109000001110     C                   movel     msg(23)       v1cmsg
109100001110     C                   seton                                        42  28
109200001110     C                   goto      endsrcpag
109300001110     C                   endif
109400001110     C                   endif
109500001010
109600001110     C     endsrcpag     endsr
109700001011      **********************************************************************
109800001011      * CONTROLLI     * P.O. consegna *
109900001011      **********************************************************************
110000001011     C     Sr_Contrpoc   begsr
110100001110
110200001110     C                   eval      *in31 = *off
110300001013
110400001011     C     '?'           scan      v1cpoc                                 30
110500001011     C     *in30         ifeq      *on
110600001011     C                   eval      *in55 = *on
110700001011     C                   eval      *in90 = *on
110800010119     C                   clear                   ds_tnsd
110900001011     C                   call      'TNSD24R'
111000010119     C                   parm                    ds_cod
111100010119     C                   parm                    ds_tip
111200010119     C                   parm                    ds_des
111300010119     C                   parm                    ds_pra
111400010119     C                   movel     ds_cod        v1cpoc
111500010119     C                   movel     ds_des        v1dpoc
111600001011     C                   endif
111700001106
111800001110     C                   testn                   v1cpoc               31
111900001106     C                   move      v1cpoc        w001a             1
112000001110     C     *in31         ifeq      *off
112100001106     C     w001a         orlt      '0'
112200001106     C                   seton                                        55  28
112300001106     C                   movel     msg(17)       v1cmsg
112400001106     C                   goto      endsrcpoc
112500001106     C                   endif
112600001011
112700001110     C                   if        v1cpoc <> *blanks
112800001011     C                   movel     v1cpoc        kazorg
112900001011     C     kazorg        chain     azorg01L
113000001011     C                   if        %found(azorg01l)
113100001204     C                   if        ORGfva <> *blanks or
113200001204     C                             (ORGfag <> 'A' and ORGfag<> 'F')
113300001011     C                   movel     msg(17)       v1cmsg
113400001011     C                   seton                                        55  28
113500001013     C                   else
113600001013     C                   movel     ORGdes        v1dpoc
113700001011     C                   endif
113800001011     C                   else
113900001011     C                   movel     msg(17)       v1cmsg
114000001011     C                   seton                                        55  28
114100001011     C                   endif
114200001011     C                   endif
114300001011
114400001011     C     endsrcpoc     endsr
114500001010      **********************************************************************
114600001010      * CONTROLLI     * cliente consegna *
114700001010      **********************************************************************
114800001011     C     Sr_Contrcrc   begsr
114900001010
115000010119     C                   movel     ds_v1ccrc     kfnacr
115100001011     C     kfnacr        chain     fnacr01l
115200001010     C                   if        %found(fnacr01l)
115300001010     C                   if        ACRatb <> *blanks
115400001010     C                   movel     msg(18)       v1cmsg
115500001010     C                   seton                                        56  28
115600001010     C                   endif
115700001010     C                   else
115800001010     C                   movel     msg(18)       v1cmsg
115900001010     C                   seton                                        56  28
116000001010     C                   endif
116100001010
116200001010     C                   endsr
116300001010      **********************************************************************
116400001010      * CONTROLLI     * dati cliente consegna *
116500001010      **********************************************************************
116600001011     C     Sr_Contrsrc   begsr
116700001010
116800001010     C                   clear                   FNLV14DS
116900001010     C                   z-add     *date         I14dri
117000001010     C                   movel     v1rsc         I14rsc
117100001010     C                   movel     v1inc         I14ind
117200001010     C                   movel     v1loc         E14loc
117300001010     C                   movel     v1prc         E14prv
117400001010     C                   movel     v1cac         E14cap
117500001011     C                   movel     v1nac         E14nar
117600001010
117700001010     C                   call      'FNLV14R'
117800001010     C                   parm                    KPJBA
117900001010     C                   parm                    FNLV14DS
118000001010
118100001010     C                   movel     I14ind        v1inc
118200001010     C                   movel     E14loc        v1loc
118300001010     C                   movel     E14prv        v1prc
118400001010     C                   movel     E14cap        v1cac
118500001010     C                   movel     E14nar        v1nac
118600001010
118700001010     C                   eval      *in60 = (O14err = '1')                       Ragione Sociale
118800001010     C                   eval      *in61 = (O14err = '2')                       Indirizzo
118900001010     C                   eval      *in62 = (O14err = '5')                       CAP
119000001010     C                   eval      *in63 = (O14err = '3')                       Località
119100001010     C                   eval      *in64 = (O14err = '4')                       Provincia
119200001010     C                   eval      *in65 = (O14err = '6')                       Nazione
119300001010
119400001010     C                   if        O14msg <> *blanks
119500001011     C                   movel(p)  O14msg        v1cmsg
119600001010     C                   eval      *IN28 = *ON
119700001010     C                   endif
119800001010
119900001010     C                   endsr
120000001010      **********************************************************************
120100001010      * CONTROLLI     * cap - località - provincia *
120200001010      **********************************************************************
120300001011     C     Sr_Contrcap   begsr
120400001010
120500001011     C                   clear                   TISI95DS
120600001011     C                   clear                   FNLV13DS
120700001011     C                   movel     ' '           I95tla
120800001011     C                   movel     '7'           I95tcn
120900001011     C                   movel     v1cac         I95cap
121000001011     C                   movel     v1nac         I95nar
121100001011     C                   movel     v1inc         I95ind
121200001011     C                   movel     v1loc         I95loc
121300001011     C                   movel     v1prc         I95prv
121400001011     C                   movel     'S'           I13af0
121500001011     C                   movel     'S'           I13af1
121600001011     C                   movel     'S'           I13sz2
121700001011     C                   movel     'S'           I13la3
121800001011     C                   movel     'S'           I13sz3
121900001011     C                   movel     'S'           I13cnv
122000001011     C                   movel     'S'           I13szv
122100001010
122200001011      * Gestione errori forzabili: se cambiato CAP/Località
122300001011      * riazzero i valori di errori forzabili
122400001010
122500001011      * Modifica cap
122600001010
122700010119     C     v1cac         ifne      sav_v1cac
122800001011     C                   clear                   wfzv
122900001011     C                   clear                   wfz1
123000001011     C                   clear                   wfz2
123100001011     C                   clear                   wfz3
123200010119     C                   movel     v1cac         sav_v1cac
123300001011     C                   endif
123400001010
123500001011      * Modifica località
123600001010
123700010119     C     v1loc         ifne      sav_v1loc
123800001011     C                   clear                   wfz1
123900001011     C                   clear                   wfz2
124000001011     C                   clear                   wfz3
124100010119     C                   movel     v1loc         sav_v1loc
124200001011     C                   endif
124300001010
124400001011      * Modifica provincia
124500001010
124600010119     C     v1prc         ifne      sav_v1prc
124700001011     C                   clear                   wfz3
124800010119     C                   movel     v1prc         sav_v1prc
124900001011     C                   endif
125000001010
125100001011     C                   movel     wfzv          E13fzv
125200001011     C                   movel     wfz1          E13fz1
125300001011     C                   movel     wfz2          E13fz2
125400001011     C                   movel     wfz3          E13fz3
125500001011     C                   call      'FNLV13R'
125600001011     C                   parm                    KPJBA
125700001011     C                   parm                    FNLV13DS
125800001011     C                   parm                    TISI95DS
125900001010
126000001011      * Ricontrollo errori
126100001010
126200001011     C                   movel     O13err        werr
126300001011     C                   movel     O13msg        wmsg
126400001011     C                   movel     E13fzv        wfzv
126500001011     C                   movel     E13fz1        wfz1
126600001011     C                   movel     E13fz2        wfz2
126700001011     C                   movel     E13fz3        wfz3
126800001010
126900001011      * Se il livello di affidabilità non è inferiore a 2 e
127000001011      * il livello di reperimento dati non è inferiore a 3
127100001011      * reimposto i dati da TISI95DS
127200001010
127300001011     C     O13ron        ifeq      'S'
127301110121     C                   movel     O95nar        v1nac
127302110121     C                   end
127400110121     C     O13roc        ifeq      'S'
127401110121     C                   movel     O95cap        v1cac
127402110121     C                   end
127500110121     C     O13rop        ifeq      'S'
127501110121     C                   movel     O95prv        v1prc
127502110121     C                   end
127600110121     C     O13rol        ifeq      'S'
127700001011     C                   movel     O95loc        v1loc
128100001011     C                   end
128200001010
128300001011     C     werr          ifne      ' '
128400001011     C     wmsg          andne     *blanks
128500001010
128600001011     C                   select
128700001010
128800001011     C     werr          wheneq    '3'
128900001010     C                   eval      *in63 = *on
129000001010     C                   eval      *in28 = *on
129100001010
129200001011     C     werr          wheneq    '5'
129300001010     C                   eval      *in62 = *on
129400001010     C                   eval      *in28 = *on
129500001010
129600001011     C     werr          wheneq    '4'
129700001010     C                   eval      *in64 = *on
129800001010     C                   eval      *in28 = *on
129900001010
130000001011     C     werr          wheneq    '6'
130100001010     C                   eval      *in65 = *on
130200001010     C                   eval      *in28 = *on
130300001011     C                   endsl
130400001010      *
130500001011     C                   movel(p)  wmsg          v1cmsg
130600001011     C                   end
130700001010
130800001011     C   28              goto      endsrcap
130900001010
131000001011      * Controllo se reimpostata località
131100001010
131200001011     C     O13rol        ifeq      'S'
131300001010     C                   eval      *in63 = *on
131400001010     C                   eval      *in28 = *on
131500001011     C                   movel     msg(19)       v1cmsg
131600001011     C                   goto      endsrcap
131700001011     C                   end
131800001010
131900001011      * Controllo se reimpostata Provincia
132000001010
132100001011     C     O13rop        ifeq      'S'
132200001010     C                   eval      *in64 = *on
132300001010     C                   eval      *in28 = *on
132400001011     C                   movel     msg(21)       v1cmsg
132500001011     C                   goto      endsrcap
132600001011     C                   end
132700001010
132800001011      * Controllo se reimpostato cap
132900001010
133000001011     C     O13roc        ifeq      'S'
133100001010     C                   eval      *in62 = *on
133200001010     C                   eval      *in28 = *on
133300001011     C                   movel     msg(20)       v1cmsg
133400001011     C                   goto      endsrcap
133500001011     C                   end
133600001010
133700001011      * Controllo se reimpostata Nazione
133800001010
133900001011     C     O13ron        ifeq      'S'
134000001010     C                   eval      *in65 = *on
134100001010     C                   eval      *in28 = *on
134200001011     C                   movel     msg(22)       v1cmsg
134300001011     C                   goto      endsrcap
134400001011     C                   end
134500001010
134600001011     C     endsrcap      endsr
134700010730      **********************************************************************
134800010730      * CONTROLLI     * Periodo chiusura dal/al *
134900010730      **********************************************************************
135000010730     C     Sr_Contrchiu  BEGSR
135100010730
135200010730     C                   if        v1cdif <> *zeros
135300010730     C                   clear                   wlbdat
135400010730     C                   z-add     v1cdif        G02dat
135500010730     C                   call      'XSRDA8'
135600010730     C                   parm                    wlbdat
135700010730     C                   if        G02err = '1'
135800010730     C                   eval      *in26 = *ON
135900010730     C                   eval      *in28 = *ON
136000010730     C                   movel     msg(39)       v1cmsg
136100010730     C                   goto      endsrchiu
136200010730     C                   else
136300010730     C                   z-add     G02dat        v1cdif
136400010730     C                   z-add     G02inv        vddif
136500010730     C                   endif
136600010730     C                   endif
136700010730
136800010730     C                   if        v1cdff <> *zeros
136900010730     C                   clear                   wlbdat
137000010730     C                   z-add     v1cdff        G02dat
137100010730     C                   call      'XSRDA8'
137200010730     C                   parm                    wlbdat
137300010730     C                   if        G02err = '1'
137400010730     C                   eval      *in27 = *ON
137500010730     C                   eval      *in28 = *ON
137600010730     C                   movel     msg(39)       v1cmsg
137700010730     C                   goto      endsrchiu
137800010730     C                   else
137900010730     C                   z-add     G02dat        v1cdff
138000010730     C                   z-add     G02inv        vddff
138100010730     C                   endif
138200010730     C                   endif
138300010730
138400010730     C                   if        v1cdif <> *zeros and v1cdff = *zeros
138500010730     C                   seton                                        2827
138600010730     C                   movel     msg(40)       v1cmsg
138700010730     C                   goto      endsrchiu
138800010730     C                   endif
138900010730
139000010730     C                   if        v1cdff <> *zeros and v1cdif = *zeros
139100010730     C                   seton                                        2826
139200010730     C                   movel     msg(40)       v1cmsg
139300010730     C                   goto      endsrchiu
139400010730     C                   endif
139500010730
139600010730     C                   if        vddif > vddff
139700010730     C                   seton                                        2826
139800010730     C                   movel     msg(41)       v1cmsg
139900010730     C                   goto      endsrchiu
140000010730     C                   endif
140100010730
140200010730     C     endsrchiu     endsr
140300010730
140400001011      **********************************************************************
140500001011      * F6 - Conferma
140600001011      **********************************************************************
140700001011     C     Sr_Conferma   BEGSR
140800001011
140900001011     C                   clear                   fnors000
141000001011
141100001011      * Scrivo
141200001011     C                   if        *in01 = *on
141300001011     C                   exsr      Sr_Numor
141400001011     C                   exsr      Sr_Carfile
141500070905      * imposto il codice giro
141600070905     c                   exsr      sr_ors1
141700001011     C                   write     fnors000
141800001011     C                   endif
141900001011
142000001011      * Aggiorno
142100001011     C                   if        *in02 = *on
142200001011     C     kfnor         chain     fnors01L
142300001011     C                   exsr      Sr_Carfile
142400001011     C                   if        %found(fnors01l)
142500070905      * imposto il codice giro
142600070905     c                   exsr      sr_ors1
142700001011     C                   update    fnors000
142800001011     C                   endif
142900001011     C                   endif
143000001011
143100001011     C                   endsr
143200001011      **********************************************************************
143300001011      * F16 - Annullamento
143400001011      **********************************************************************
143500001011     C     Sr_Annulla    BEGSR
143600001011
143700001011     C     kfnor         chain     fnors01L
143800001011     C                   if        %found(fnors01l)
143900001011     C                   movel     'A'           ORSatb
144000010312
144100040818     c                   Z-add     *Date         OrsDtt
144200010312
144300001011     C                   update    fnors000
144400001011     C                   endif
144500001011
144600001011     C                   endsr
144700001011      **********************************************************************
144800001011      * F7 - Ricerca cliente ritiro
144900001011      **********************************************************************
145000001011     C     Sr_RicRitiro  BEGSR
145100001011
145200070905     c                   IF        §OGcgio = 'S'
145300070808      * - nuovo pgm
145400070808     c                   reset                   FIOR37ds
145500070808     c*** già così:      eval      I37opz  = '1'
145600071105     c***********        eval      I37tac  = 'R'
145700070808     c                   movel     V1Cpoe        I37fgs
145800070808     c                   movel(p)  FIOR37ds      KPJBU
145900070808     c                   call      'FIOR37R'
146000070808     c                   parm                    KPJBA
146100070808     c                   movel     KPJBU         FIOR37ds
146200070808     c                   select
146300070808     c                   when      O37err <> *blanks
146400070808     c                   movel     O37msg        V1Cmsg
146500070808     c                   seton                                        41  28
146600070808     c                   when      O37cro <> *zeros
146700070808     c                   movel     O37cro        ds_V1Ccra
146800070808     c                   exsr      Sr_DecoCRA
146900070808     c                   endsl
147000070808      *
147100070808     c                   leavesr
147200070808     c                   ENDIF
147300070808      * - vecchio pgm
147400001011     C                   clear                   fior01ds
147500001011     C                   movel     '4'           I01ric
147600071105     C**********         movel     'R'           I01tac
147700001215     C                   z-add     v1cpoe        I01poa
147800001011     C                   movel     fior01ds      kpjbu
147900001011     C                   call      'FIOR01R'
148000001011     C                   parm                    kpjba
148100001011     C                   movel     kpjbu         fior01ds
148200001011
148300001011     C                   if        O01err <> *blanks
148400001011     C                   movel     O01msg        v1cmsg
148500001011     C                   seton                                        41  28
148600001011     C                   else
148700001011     C                   if        O01cro <> *zeros
148800010119     C                   movel     O01cro        ds_v1ccra
148900001011     C                   exsr      Sr_Decocra
149000001011     C                   endif
149100001011     C                   endif
149200001011
149300001011     C                   endsr
149400001011      **********************************************************************
149500001011      * F8 - Ricerca cliente ordinante
149600001011      **********************************************************************
149700001011     C     Sr_RicOrdin   BEGSR
149800001011
149900070905     c                   IF        §OGcgio = 'S'
150000070808      * - nuovo pgm
150100070808     c                   reset                   FIOR37ds
150200070808     c*** già così:      eval      I37opz  = '1'
150300071105     c***********        eval      I37tac  = 'O'
150400070808     c                   movel     V1Cpoe        I37fgs
150500070808     c                   movel(p)  FIOR37ds      KPJBU
150600070808     c                   call      'FIOR37R'
150700070808     c                   parm                    KPJBA
150800070808     c                   movel     KPJBU         FIOR37ds
150900070808     c                   select
151000070808     c                   when      O37err <> *blanks
151100070808     c                   movel     O37msg        V1Cmsg
151200070808     c                   seton                                        42  28
151300070808     c                   when      O37cro <> *zeros
151400070808     c                   movel     O37cro        ds_V1Ccor
151500070808     c                   exsr      Sr_DecoCOR
151600070808     c                   endsl
151700070808      *
151800070808     c                   leavesr
151900070808     c                   ENDIF
152000070808      * - vecchio pgm
152100001011     C                   clear                   fior01ds
152200001011     C                   movel     '4'           I01ric
152300071105     C**********         movel     'O'           I01tac
152400001215     C                   z-add     v1cpoe        I01poa
152500001011     C                   movel     fior01ds      kpjbu
152600001011     C                   call      'FIOR01R'
152700001011     C                   parm                    kpjba
152800001011     C                   movel     kpjbu         fior01ds
152900001011
153000001011     C                   if        O01err <> *blanks
153100001011     C                   movel     O01msg        v1cmsg
153200001011     C                   seton                                        42  28
153300001011     C                   else
153400001011     C                   if        O01cro <> *zeros
153500010119     C                   movel     O01cro        ds_v1ccor
153600001011     C                   exsr      Sr_Decocor
153700001011     C                   endif
153800001011     C                   endif
153900001011
154000001011     C                   endsr
154100001011      **********************************************************************
154200001011      * F9 - Ricerca cliente consegna
154300001011      **********************************************************************
154400001011     C     Sr_RicCons    BEGSR
154500001011
154600070905     c                   IF        §OGcgio = 'S'
154700070808      * - nuovo pgm
154800070808     c                   reset                   FIOR37ds
154900070808     c*** già così:      eval      I37opz  = '1'
155000070808     c                   movel     V1Cpoe        I37fgs
155100070808     C                   movel     V1rsc         I37rag
155200070808     c                   movel(p)  FIOR37ds      KPJBU
155300070808     c                   call      'FIOR37R'
155400070808     c                   parm                    KPJBA
155500070808     c                   movel     KPJBU         FIOR37ds
155600070808     c                   select
155700070808     c                   when      O37err <> *blanks
155800070808     c                   movel     O37msg        V1Cmsg
155900070808     c                   seton                                        56  28
156000070808     c                   when      O37cro <> *zeros
156100070808     c                   movel     O37cro        ds_V1Ccrc
156200070808     c                   exsr      Sr_DecoCRC
156300070808     c                   endsl
156400070808      *
156500070808     c                   leavesr
156600070808     c                   ENDIF
156700070808      * - vecchio pgm
156800001011     C                   clear                   fior01ds
156900001011     C                   movel     '4'           I01ric
157000001011     C                   movel     v1rsc         I01rag
157100001215     C                   z-add     v1cpoe        I01poa
157200001011     C                   movel     fior01ds      kpjbu
157300001011     C                   call      'FIOR01R'
157400001011     C                   parm                    kpjba
157500001011     C                   movel     kpjbu         fior01ds
157600001011
157700001011     C                   if        O01err <> *blanks
157800001011     C                   movel     O01msg        v1cmsg
157900001011     C                   seton                                        56  28
158000001011     C                   else
158100001011     C                   if        O01cro <> *zeros
158200010119     C                   movel     O01cro        ds_v1ccrc
158300001011     C                   exsr      Sr_Decocrc
158400001011     C                   endif
158500001011     C                   endif
158600001011
158700001011     C                   endsr
158800001011      **********************************************************************
158900001011      * PRENDO IL NUOVO NUMERO ORDINE
159000001011      **********************************************************************
159100001011     C     Sr_Numor      BEGSR
159200001011
159300030922     c                   clear                   TRUL33DS
159400030922     c                   Eval      I33cnu = 301
159500030922     c                   Eval      I33po1 = V1cpoe
159600030922     c                   Eval      I33num = 1
159700030922     c                   movel(p)  TRUL33DS      KPJBU
159800030922     c                   call      'TRUL33R'
159900030922     c                   parm                    KPJBA
160000030922     c                   movel     KPJBU         TRUL33DS
160100030922     C                   z-add     O33nrf        v1nor
160200001011
160300001011     C                   endsr
160400001011      **********************************************************************
160500001011      * CARICO IL FILE CON I DATI DELLA VIDEATA
160600001011      **********************************************************************
160700001011     C     Sr_Carfile    BEGSR
160800001011
160900001215     C                   z-add     v1nor         ORSnor
161000001215     C                   z-add     v1cpoe        ORSpoe
161100001011     C                   movel     v1ctor        ORStor
161200010131     C                   movel     'S'           ORStco
161300010119     C                   movel     ds_v1ccor     ORScor
161400010119     C                   movel     ds_v1ccra     ORScra
161500001011     C                   movel     v1rer         ORSrer
161600001011     C                   movel     v1ter         ORSter
161700010119     C                   movel     ds_v1ccrc     ORScrc
161800001011     C                   movel     v1rsc         ORSrsc
161900001011     C                   movel     v1inc         ORSinc
162000001011     C                   movel     v1loc         ORSloc
162100001011     C                   movel     v1cac         ORScac
162200001011     C                   movel     v1prc         ORSprc
162300001011     C                   movel     v1nac         ORSnac
162400001011     C                   movel     v1ffd         ORSffd
162500001011     C                   movel     v1orr         ORSorr
162600010122     C                   movel     v1rmp         ORSrmp
162700001011     C                   movel     v1rlu         ORSrlu
162800001011     C                   movel     v1rma         ORSrma
162900001011     C                   movel     v1rme         ORSrme
163000001011     C                   movel     v1rgi         ORSrgi
163100001011     C                   movel     v1rve         ORSrve
163200001011     C                   movel     v1nam         ORSnam
163300001011     C                   movel     v1ncl         ORSncl
163400001011     C                   movel     v1pkg         ORSpkg
163500001011     C                   movel     v1vlm         ORSvlm
163600001011     C                   movel     v1bnc         ORSbnc
163700001011     C                   movel     v1blc         ORSblc
163800001011     C                   movel     v1att         ORSatt
163900001011     C                   movel     v1mtc         ORSmtc
164000001011     C                   movel     v1pag         ORSpag
164100001011     C                   movel     v1ksc         ORSksc
164200001020     C                   if        v1ctr  <> *blanks
164300001011     C                   movel     v1ctr         ORSctr
164400001020     C                   endif
164500001215     C                   z-add     v1cpoe        ORSpor
164600001011     C                   movel     v1czor        ORSzor
164700080530     c                   clear                   orspoc
164800001011     C                   if        v1cpoc <> *blanks
164900001011     C                   movel     v1cpoc        ORSpoc
165000001020     C                   endif
165100001011     C                   movel     v1not1        ORSno1
165200001011     C                   movel     v1not2        ORSno2
165300010118     C                   movel     v1spi         ORSspi
165400010730
165500010730     C                   z-add     vddif         §orsdif
165600010730     C                   z-add     vddff         §orsdff
165700010730     C                   movel     dors01        ORSflo
165800010312
165900040818     c                   Z-add     *Date         OrsDtt
166000001204
166100001204      * Mette sempre in positivo i campi delle quantità
166200001204     C                   mllzo     1             ORSorr
166300001204     C                   mllzo     1             ORSncl
166400001204     C                   mllzo     1             ORSpkg
166500001204     C                   mllzo     1             ORSvlm
166600001204     C                   mllzo     1             ORSbnc
166700001204     C                   mllzo     1             ORSblc
166800001204     C                   mllzo     1             ORSatt
166900001204     C                   mllzo     1             ORSmtc
167000001011
167100001011     C                   endsr
167200070905
167300070905      *--------------------------------------------------------------------*
167400070905      * File FNORS10F
167500070905      *--------------------------------------------------------------------*
167600070905     c     sr_ors1       begsr
167700070905
167800070905     c     kfnor         chain     fnors11l
167900070905     c                   if        %found(fnors11l)
168000080123     c                   if        v1ccgi <> *blanks
168100070905     c                   eval      ors1pocgi = orspor
168200070905     c                   eval      ors1cgi = v1ccgi
168300070905     c                   eval      ors1dtv = *date
168400070905     c                   eval      ors1ute = knmus
168500070905     c                   update    fnors100
168600080123     c                   else
168700080123     c                   delete    fnors100
168800070905     c                   endif
168900080123     c                   endif
169000070905     c                   if        not %found(fnors11l)
169100071018     c                             and v1ccgi <> *blanks
169200070905     c                   clear                   fnors100
169300070905     c                   eval      ors1poe = orspoe
169400070905     c                   eval      ors1nor = orsnor
169500070905     c                   eval      ors1pocgi = orspor
169600070905     c                   eval      ors1cgi = v1ccgi
169700070905     c                   eval      ors1dtv = *date
169800070905     c                   eval      ors1ute = knmus
169900070905     c                   write     fnors100
170000070905     c                   endif
170100070905
170200070905     c                   endsr
170300070905
170400001010      *****************************************************************
170500001010      * ROUTINE INIZIALE
170600001010      *****************************************************************
170700001010     C     *INZSR        BEGSR
170800001009
170900001010     C     *ENTRY        PLIST
171000001010     C                   PARM                    KPJBA
171100001011
171200001011     C                   movel     TIT_A         VTCtit
171300001009
171400001009     C                   movel     kpjbu         parm01
171500001020
171600060203     C                   Z-ADD     1             CODUT             1 0
171700060203
171800060203      * Reperisco dati job
171900060203     c                   exsr      DatiJob
172000001010
172100001009     C     Kfnor         klist
172200001011     C                   kfld                    kpoe
172300001009     C                   kfld                    knor
172400010119
172500010119     C     Ktab          klist
172600010119     C                   kfld                    codut
172700010119     C                   kfld                    kcod
172800010119     C                   kfld                    kkey
172900010119     C                   movel     '03'          kcod
173000001010
173100001010     C     Ktntam        klist
173200001010     C                   kfld                    kksc
173300001010     C                   kfld                    kctr
173400071108      *
173500071108     c     k02acr13      klist
173600071108     c                   kfld                    KFNacr
173700071108     c                   kfld                    V1Cpoe
173800021202
173900010119     C                   movel     *zeros        ds_v1ccra
174000010119     C                   movel     *zeros        ds_v1ccor
174100010119     C                   movel     *zeros        ds_v1ccrc
174200001009
174300001009     C                   select
174400001009     C                   when      psce = '1'
174500001009     C                   movel     mod(1)        v1mod
174600001110     C                   eval      *in01 = *on
174700001009     C                   when      psce = '2'
174800001009     C                   movel     mod(2)        v1mod
174900001110     C                   eval      *in02 = *on
175000001011     C                   when      psce = '5'
175100001009     C                   movel     mod(3)        v1mod
175200001009     C                   eval      *in03 = *on
175300001009     C                   endsl
175400060104
175500060104      * inibischio il richiamo al fior01r se arrivo già da lui
175600060104     c                   Eval      *in06 = (pfior01 = 'S')
175700001009
175800001011     C                   eval      kpoe = ppoe
175900001011     C                   eval      knor = pnor
176000001011
176100001011     C                   eval      v1nor  = pnor
176200001011     C                   eval      v1cpoe = ppoe
176300001009
176400001009      * Decodifica P.O.gestione
176500001009     C     v1cpoe        chain     azorg01L
176600001009     C                   if        %found(azorg01l)
176700070810     c                   movel     ORGde7        Og147
176800001009     C                   movel     orgDES        v1dpoe
176900001009     C                   else
177000070810     c                   clear                   Og147
177100001009     C                   clear                   v1dpoe
177200001010     C                   endif
177300070905      * controllo se attiva la procedura GEO ORM
177400070905     c                   eval      *in07 = (§ogcgio = 'S')
177500010131
177600010131      * Recupero i dati di default prima con il mio p.o.
177700010131     C                   clear                   ddft
177800010131     C                   clear                   TIBS02DS
177900010131     C                   movel     'C'           T02mod
178000010131     C                   movel     knsif         t02sif
178100010131     C                   movel     'DFT'         t02cod
178200010131     C                   movel(p)  v1cpoe        T02ke1
178300010131     C                   call      'TIBS02R'
178400010131     C                   parm                    KPJBA
178500010131     C                   parm                    TIBS02DS
178600010131     C                   if        t02err =  *blanks
178700010131     C                   movel     t02uni        ddft
178800010131     C                   else
178900010131      * poi con 999 generico se non trovo con il mio p.o.
179000010131     C                   clear                   TIBS02DS
179100010131     C                   movel     'C'           T02mod
179200010131     C                   movel     knsif         t02sif
179300010131     C                   movel     'DFT'         t02cod
179400010131     C                   movel(p)  '999'         T02ke1
179500010131     C                   call      'TIBS02R'
179600010131     C                   parm                    KPJBA
179700010131     C                   parm                    TIBS02DS
179800010131     C                   if        t02err =  *blanks
179900010131     C                   movel     t02uni        ddft
180000010131     C                   endif
180100010131     C                   endif
180200001006
180300001006     C                   endsr
180400060203
180500060203      *---------------------------------------------------------------*
180600060203      * Reperimento Dati del job (Utente/Operativi)                   *
180700060203      *---------------------------------------------------------------*
180800060203     c     DatiJob       BEGSR
180900060203      *
181000060203     c     *dtaara       define    §azute        azuteds
181100060203     c     *dtaara       define    §datiute      ddatiute
181200060203      *
181300060203     c                   in(E)     *dtaara
181400060203     c                   IF        %ERROR or RSUT = *blanks
181500060203     c                   clear                   Tibs34Ds
181600060203     c                   call      'TIBS34R'
181700060203     c                   parm                    Tibs34Ds
181800060203     c                   in        *dtaara
181900060203     c                   ENDIF
182000060203      *
182100060203     c                   ENDSR
182200001010      *---------------------------------------------------------------------------------------------
182300001010** MSG  Lungh. 78                                                            *
182400010420Inserire cliente mittente                                                           1
182500010420Cliente mittente non valido                                                         2
182600001010Cliente ordinante non valido                                                        3
182700001010Immettere i colli                                                                   4
182800001010Immettere il peso                                                                   5
182900001010Immettere il volume                                                                 6
183000001010Immettere i bancali                                                                 7
183100001010Immettere il bilico                                                                 8
183200001010Immettere l'autotreno                                                               9
183300001010Immettere la motrice                                                                10
183400010131Inserire almeno un giorno                                                           11
183500001010Zona ritiro errata                                                                  12
183600001010Tipo ordine non valido                                                              13
183700001010Inserire tipo ordine                                                                14
183800060203Zona di ritiro NON abilitata                                                        15
183900001010Codice tassazione o codice tariffa errati                                           16
184000070418Filiale consegna errata                                                             17
184100010420Cliente destinatario non valido                                                     18
184200001010Attenzione !! Modificata Località                                                   19
184300001010Attenzione !! Modificato CAP                                                        20
184400001010Attenzione !! Modificata Provincia                                                  21
184500001010Attenzione !! Modificata Nazione                                                    22
184600001011Pagamento a carico dell'ordinante...inserire il codice                              23
184700001012Ora errata                                                                          24
184800001012Immettere i colli ------> ENTER x forzare                                           25
184900001012Immettere il peso ------> ENTER x forzare                                           26
185000001012Immettere il volume ------> ENTER x forzare                                         27
185100001012Immettere i bancali ------> ENTER x forzare                                         28
185200001012Immettere il bilico ------> ENTER x forzare                                         29
185300001012Immettere l'autotreno ------> ENTER x forzare                                       30
185400001012Immettere la motrice ------> ENTER x forzare                                        31
185500060203?> LIBERO <?- ex   Pagamento a carico del mittente ...inserire il codice            32
185600060203?> LIBERO <?- ex   Pagamento a carico del destinatario ...inserire il codice        33
185700010122Immettere l'ora                                                                     34
185800010123Immettere M o P                                                                     35
185900070925Libero.........................                                                     36
186000010131Immettere almeno una quantità (peso, volume, bancali)                               37
186100070418Scelta zona relativa ad altra filiale                                               38
186200010730Data formalmente errata                                                             39
186300010730Inserire entrambe le date                                                           40
186400010730Data iniziale maggiore della data finale                                            41
186500070905Codice Giro non corretto                                                            42
186600001009** MOD  Lungh. 15                                                            *
186700001013  IMMISSIONE                                                                        1
186800001013 MANUTENZIONE                                                                       2
186900001009VISUALIZZAZIONE                                                                     3
187000001013   ANNULLATO                                                                        4
