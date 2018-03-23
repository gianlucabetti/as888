000100990610     H DECEDIT('0,') DATEDIT(*DMY.)
000200150202     H* FNLSI3R *----------------------------------------------------*
000300970410     H*                                                              *
000400150202     H*       - CHIUSURA BOLLE IN PARTENZA CON PRATICA IDD  - NO EDI *
000500050209     H*                                                              *
000600970410     H*--------------------------------------------------------------*
000700150205     FFNBLP01L  uF   E           K DISK
000800970410     F*--------
000900150204     FFNBLT01L  UF   E           K DISK
001000150204     FFNBrv07L  IF   E           K DISK
001100150204     FFiar401l  IF   E           K DISK
001200150206     FFiAR403L  IF   E           K DISK    RENAME(FiAR4000:FiAR4003)
001300970410     F*--------
001400970410     FFNLBL01L  IF   E           K DISK
001500970410     F*--------
001600150219     FTABEL00F  IF   E           K DISK
001700970410     F*--------
001800150219     FAZORG01L  IF   e           k DISK
001900160125     F*--------
002000160125     Ffibsp01l  IF   e           k DISK
002100160125     Ffibsp02l  IF   e           k DISK    rename(fibsp000:fibsp002)
002200970414     F*--------
002300150202     FFNLSI3D   CF   E             WORKSTN
002400150218     f                                     sfile(lsi1S01 : S01nrr)
002500080918     F*--------
002600970410     D*--------------------------------------------------------------*
002700970410     D*  SCHIERE
002800970410     D*--------------------------------------------------------------*
002900970612     D*  Caricamento capoconto clienti
003000970410     D* Schiera codici tabella 3K: invio date consegna
003100030108     D C3K             S              7  0 DIM(9990)
003200150219     D*CKS             S              7  0 DIM(9990)
003300030108     D CSD             S              2    DIM(9990)
003400970715     D* Tipi bolla: tabella 3A
003500970715     D C3A             S              2    DIM(500)
003600970715     D TPO             S              2    DIM(500)
003700970410     D* Caricamento £1
003800020506     D*UNI             S              3    DIM(29)
003900020506     D £1              S              3  0 DIM(30)
004000150204     D sel             S              1  0 DIM(3)
004100970414     D* ERRORI
004200150507     D ERR             S             78    DIM(33) CTDATA PERRCD(1)
004300150204
004400150204     d wdata           s              8  0
004500150204     D KTRC            S                   LIKE(AR4TRC)
004600150204     D wsel            S              1  0
004700150205     D x               S              5  0
004800150205     D wbsp            S              1    inz
004900150209     D kn30            S                   like(ar4n30)
005000150209     D wksc            S                   like(blpksc)
005100150209     D wlnp            S                   like(blplnp)
005200150217     d s01rcd          s                   like(c01rcd)
005300150217     d $Inzd02         s               n   inz(*on)
005400150217     d $Inzs01         s               n   inz(*on)
005500150217     d $Finerec        s               n   inz(*off)
005600150218     d S01nrr          s              4  0 inz
005700150218     d wopz            s              1a
005800150218     d wtfp            s                   LIKE(BLPTFP)
005900150217
006000150217     D WrkStringaSql   S           4500
006100150217     D                                     VARYING
006200150218     d Datadmy         s               d   datfmt(*dmy)
006300150219     d data_eur        s               D   datfmt(*eur)
006400970410     D*--------------------------------------------------------------*
006500970410     D* DS
006600970410     D*--------------------------------------------------------------*
006700020506     d trul06ds      e ds
006800020506     d  lin                    1     90  0 dim(30)
006900150218     d fnlv55ds      e ds
007000970414     D WLBDA8          DS
007100970414     D  G02DAT                 1      8  0
007200150203     D  G02INV                 9     16  0
007300970414     D  G02ERR                17     17
007400970414     D  G02TGI                18     22  0
007500150204     d WDAT8           DS
007600150204     d  datada                 1      8  0
007700150204     d  dataa                  9     16  0
007800150204     d  ggl                   17     21  0
007900080421      *
008000970410     D KPJBA         E DS
008100150217     d fnblpds       e ds                  extname(fnblp00f) inz
008200150204       // -?Parametri x Controllo profilo utenti?
008300150204     d TIBS34ds      e ds
008400150211     d TIBS69DS      E DS                  INZ
008500150211     d DS_cnaco      E DS                  extname(CNACO00F)
008600150211     d DS_cnind      E DS                  extname(CNIND00F)
008700150211     d DS_cnclp      E DS                  extname(CNCLP00F)
008800150211     d DS_fncls      E DS                  extname(FNCLS00F)
008900150204       // -?Ds di riferimento al file esterno AZUTE00F?
009000150204     d AZUTEds       e ds                  extname(AZUTE00F)
009100150204       // -?Ds per dati organigramma?
009200150204     d dDATIUTE      e ds
009201161108     d fnlru6ds      e ds
009202161107      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
009203161107     D PARAMu6ds       DS
009204161107     d* CAMPI DI OUTPUT:
009205161107     d* . Key spedizione
009206161107     D  pa1AAS                14     17  0
009207161107     D  pa1LNP                18     20  0
009208161107     D  pa1NRS                21     22  0
009209161107     D  pa1NSP                23     29  0
009210161107     d* . Flag '1'=Premuto F3=Fine
009211161107     D  PA1F03                30     30
009212161107     d* CAMPI DI INPUT:
009213161107     d* . Modalità di richiamo
009214161107     D  PA1FLG                31     31
009215161107     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
009216161107     d*                                in interrogazione della bolla ed evitare
009217161107     d*                                errore di chiamata ricorsiva
009218161107     D  PA1GIA               144    144
009219161107     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
009220161107     d  pa1rma               149    163
009221161107     d  pa1xco               164    164
009222161107     d* non utilizzato
009223161107     d  pa1flo               165    256
009224161107      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
009225161107     D PARAMU6ds1      DS
009226161107     d** INPUT/OUTPUT (se richiesta la selezione bolla)
009227161107     D  pa1AAS_               14     17  0
009228161107     D  pa1LNP_               18     20  0
009229161107     D  pa1NRS_               21     22  0
009230161107     D  pa1NSP_               23     29  0
009231161107     d** OUTPUT
009232161107     D  PA1F03_               30     30
009233161107     d** INPUT
009234161107     D  PA1FLG_               31     31
009235161107     D*    SE RICH DA PGM GIAC. = 1
009236161107     D  PA1GIA_              144    144
009237161107     d** OUTPUT
009238161107     d  pa1rma_              149    163
009239161107     d  pa1xco_              164    164
009240161107     d** INPUT
009241161107     d*   Tipo lancio: "C" -           CHIUSO CON LR
009242161107     d*                "L" - ELABORO E CHIUDO CON LR
009243161107     d*                " " - ELABORO E CHIUDO IN RETRN
009244161107     d  pa1tla_              165    165
009245161107     d  pa1flo_              166    256
011500080421     D***** TIBS55        E DS                  EXTNAME(TIBS55DS)
011600970410     D DS3K          E DS
011700970715     D DS3A          E DS
011800970410     D DS7O          E DS
011900060811     D OG143         E DS
012000150204     D ds3i          E DS
012100150209     D dsbl4i        E DS
012200160125      * Ds esterna per ricerca cliente da file "BSP"
012300160125     d fnlsbsp1ds    e ds                  inz
012400040715
012500040715     d Fidn12ds      e ds
012600040715     d  skKey                 26    305    dim(20)
012700040715     d  skAnn                306    325    dim(20)
012800040715     d  skDca                326    485    dim(20)
012900040715     d  skFca                486    545  0 dim(20)
013000040715     d  skDch                546    705  0 dim(20)
013100040715     d  skCch                706    745    dim(20)
013200040715
013300970410     D                SDS
013400970410     D  NOMPGM                 1     10
013500150211      /copy gaitrasrc/srcprotopr,tibs69r
013600150211
013700970410     C*--------------------------------------------------------------*
013800970410     C*  C I C L O     P R I N C I P A L E
013900970410     C*--------------------------------------------------------------*
014000150217      /free
014100150217         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014200150217      /end-free
014300150217
014400970410     C*  Inizializzo varibili videata
014500970410     C                   EXSR      INZ01
014600080421      *
014700970410     C*  Gestione della prima videata
014800970410     C     WFINE         DOWEQ     'N'
014900970611     C     WTPVID        CASEQ     '1'           GESD01
015000970611     C     WTPVID        CASEQ     '2'           GESD02
015100150211     C     WTPVID        CASEQ     'S'           GESsfl
015200970410     C                   END
015300970611     C                   END
015400970410     C*  Fine pgm
015401161114     c                   eval      pa1tla_='C'
015402161114     c                   eval      kpjbu=paramu6ds1
015403161114     c                   call      'FNLRU6R1'
015404161114     c                   parm                    kpjba
015405161114
015500970410     C                   SETON                                        LR
015600970611     C*--------------------------------------------------------------*
015700970611     C* INZ01: Impostazione della prima videata
015800970611     C*--------------------------------------------------------------*
015900970611     C     INZ01         BEGSR
016000970611     C*
016100970611     C                   SETOFF                                       2840
016200970616     C                   MOVEL     '1'           WTPVID
016300970616     C                   MOVEL     'N'           WFINE
016400970611     C                   MOVEL     NOMPGM        V1CPGM
016500150205     C                   clear                   V1CKSC
016600150204     C                   clear                   V1dKSC
016700150204     C                   clear                   V1clnp
016800150204     C                   clear                   V1cnrs
016900150204     C                   clear                   V1cnsp
017000160330     C****               z-add     2015          V1caas
017100160330     C                   z-add     UYEAR         V1CAAS
017200160330     c     uyear         iflt      60
017300160330     c                   movel     20            v1caas
017400160330     c                   else
017500160330     C                   MOVEL     19            V1CAAS
017600160330     c                   endif
017700150204     C                   clear                   V1cdpd
017800970611     C*
017900970611     C                   ENDSR
018000970410     C*--------------------------------------------------------------*
018100970611     C* INZ02: Impostazione della prima videata
018200970410     C*--------------------------------------------------------------*
018300970611     C     INZ02         BEGSR
018400970410     C*
018500970410     C                   SETOFF                                       284041
018600970611     C                   SETOFF                                       424344
018700150209     C                   SETOFF                                       45
018800970611     C                   MOVEL     NOMPGM        V2CPGM
018900150204     C                   z-add     blpksc        V2CKSC
019000150204     C                   MOVEL     blprsm        V2DKSC
019100150204     c                   z-add     blplnp        V2clnp
019200150204     C                   z-add     blpnrs        V2Cnrs
019300150204     C                   z-add     blpnsp        v2cnsp
019400150204     C                   z-add     blpaas        V2Caas
019500150204     c     *iso          move      blpdim        data_eur
019600150204     c                   move      data_eur      v2cdim
019700150204     c                   eval      wdata=(blpaas*10000)+blpmgs
019800150204     c     *iso          move      wdata         data_eur
019900150204     c                   move      data_eur      v2cdsp
020000150204     c                   eval      v2crsm=BLPRSM
020100150204     c                   eval      v2cinm=BLPINM
020200150204     c                   eval      v2ccam=BLPCAM
020300150204     c                   eval      v2clom=BLPLOM
020400150204     c                   eval      v2cprm=BLPPRM
020500150204     c                   eval      v2cnzm=BLPnzm
020600150204     C* CERCO LA DESCRIZIONE AGGIUNTIVA PER IL DESTINATARIO
020700150204     C                   MOVEL     'D'           KTRC
020800150204     c                   clear                   ar4not
020900150204     C     Kar41         CHAIN     Fiar401L
021000150204     c                   eval      v2crsd=BLPRSd+ar4not
021100150204     c                   eval      v2cind=BLPINd
021200150204     c                   eval      v2ccad=BLPCAd
021300150204     c                   eval      v2clod=BLPLOd
021400150204     c                   eval      v2cprd=BLPPRd
021500150204     c                   eval      v2cnzd=BLPnzd
021600970410     C*
021700970410     C                   ENDSR
021800970410     C*--------------------------------------------------------------*
021900970410     C* GESD01: Gestione della prima videata
022000970410     C*--------------------------------------------------------------*
022100970410     C     GESD01        BEGSR
022200970410     C*
022300970410     C* Eseguo exfmt della prima videata
022400970410     C                   EXFMT     LSI1D01
022500150203     C                   SETOFF                                       284041
022600150209     C                   SETOFF                                       434445
022700150219     c                   setoff                                       90
022800970410     C* Fine
022900970410     C     *INKC         IFEQ      '1'
023000970414     C                   MOVEL     'S'           WFINE             1
023100150205     C                   leavesr
023200970410     C                   END
023300150204     C* Interrogazione bolle
023400150204     C     *INKG         IFEQ      '1'
023500150204     C     V1CNSP        IFGT      0
025101161107     C                   CLEAR                   paramu6ds1
025102161107     C                   Z-ADD     V1CAAS        pa1AAS_
025103161107     C                   Z-ADD     V1CLNP        Pa1LNP_
025104161107     C                   Z-ADD     V1CNRS        Pa1NRS_
025105161107     C                   Z-ADD     V1CNSP        Pa1NSP_
025106161107     C                   eval      pa1flg_='2'
025107161107     C                   MOVEL     paramu6ds1    KPJBU
025108161107     C                   CALL      'FNLRU6R1'
025109161107     C                   PARM                    KPJBA
025110161107     C                   MOVEL     KPJBU         PARAmu6ds1
025111161107     C     Pa1F03_       IFEQ      *BLANKS
025112161107     C                   Z-ADD     Pa1AAS_       v1CAAS
025113161107     C                   Z-ADD     Pa1LNP_       v1CLNP
025114161107     C                   Z-ADD     Pa1NRS_       v1CNRS
025115161107     C                   Z-ADD     Pa1NSP_       v1CNSP
025116161107     C                   ENDIF
025200150204     C*
025300150204     C                   ELSE
025400150204     C*
025401161107     C                   CLEAR                   PARAmu6ds
025402161107     C                   MOVEL     '2'           Pa1FLG
025403161107     C                   MOVEL     PARAmu6ds     KPJBU
025404161108     c                   clear                   fnlru6ds
025405161108     c                   eval      ILRU6SSP='N'
025407161107     C                   CALL      'FNLRU6R'
025408161107     C                   PARM                    KPJBA
025409161108     C                   PARM                    fnlru6ds
025410161107     C                   MOVEL     KPJBU         PARAMu6ds
025411161107     C     Pa1F03        IFEQ      *BLANKS
025412161107     C                   Z-ADD     Pa1AAS        V1CAAS
025413161107     C                   Z-ADD     Pa1LNP        V1CLNP
025414161107     C                   Z-ADD     Pa1NRS        V1CNRS
025415161107     C                   Z-ADD     Pa1NSP        V1CNSP
025416161107     C                   ENDIF
026700150204     C                   ENDIF
026800150204     C                   leavesr
026900150204     C                   END
027000970410     C* Controlli
027100970410     C                   EXSR      CTR01
027200150219     c   90
027300150219     Cor 28              leavesr
027400970611     C*
027500970611     C* Se no errori passo a videata successiva
027600150211     c                   if        v1cksc<>*blanks and v1cksc<>*zeros
027700150217     C                   MOVEL     'S'           WTPVID
027800150217     c                   eval      $inzS01=*on
027900150211     c                   else
028000150217     C                   MOVEL     '2'           WTPVID
028100150219     c**                 eval      $inzd02=*on
028200150211     c                   endif
028300970410     C*
028400150203     C                   ENDSR
028500970611     C*--------------------------------------------------------------*
028600970611     C* GESD02: Gestione della seconda videata
028700970611     C*--------------------------------------------------------------*
028800970611     C     GESD02        BEGSR
028900150217     c*
029000150219     c****               if        $inzd02=*on
029100150217     C                   EXSR      INZ02
029200150219     c****               eval      $inzd02=*off
029300150219     c****               endif
029400970611     C*
029500970611     C* Eseguo exfmt della prima videata
029600150219     c                   do        *hival
029700970611     C                   EXFMT     LSI1D02
029800970611     C                   SETOFF                                       284041
029900970611     C                   SETOFF                                       424344
030000150209     C                   SETOFF                                       45
030100970611     C* Fine
030200150219     C*    *INKC         IFEQ      '1'
030300150219     C*                  MOVEL     'S'           WFINE             1
030400150219     c*                  leavesr
030500150219     c*                  endif
030600970611     C* Prima videata
030700970611     C     *INKL         IFEQ      '1'
030800150218     c                   if        wtpvid<>'S'
030900970611     C                   MOVEL     '1'           WTPVID            1
031000150218     c                   endif
031100150204     C                   leavesr
031200970611     C                   END
031300080421      *
031400970611     C* Controlli
031500150204     C***                EXSR      CTR02
031600150204     C***28              GOTO      FINVD2
031700970611     C*
031800150204     c                   if        *inkf
031900150219     c     kblt          chain(n)  fnblp01l
032000150219     c                   exsr      ctr01_uni
032100150219     c                   if        not *in28
032200150204     c                   exsr      sr_ChiuBolla
032300150219     c                   if        not *in28
032400150220     c                   if        wtpvid='2'
032500150204     C                   MOVEL     '1'           WTPVID            1
032600150220     c                   endif
032700150204     C                   leavesr
032800150219     c                   endif
032900150219     c                   endif
033000150204     c                   endif
033100150219
033200150219     c                   enddo
033300970611     C*
033400150204     C                   ENDSR
033500150217     C*--------------------------------------------------------------*
033600150217     C* gessfl:  videata sfl per codice cliente
033700150217     C*--------------------------------------------------------------*
033800150217     C     gessfl        BEGSR
033900150217      /free
034000150217         if $inzs01=*on;
034100150217            exsr inzs01;
034200150217            c01csr=1;
034300150217            c01rcd=1;
034400150217            $inzs01=*off;
034500150217         // Se non trovati record errore in prima videata
034600150217            if s01nrr=0   ;
034700150217               $msg=err(21)  ;
034800150217               *in28=*on  ;
034900150219               *in45=*on  ;
035000150217               wtpvid='1'  ;
035100150217               leavesr   ;
035200150217            endif  ;
035300150217         // Se trovato un solo record emetto direttamente la videata singola
035400150217            if s01nrr=1;
035500151007              chain 1 lsi1s01;
035600151007              chain(n) (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l;
035700150217              wtpvid='2'  ;
035800150217              leavesr;
035900150217            endif;
036000150217         endif;
036100150217       // c01csr   contiene il numero di riga del subfile su cui era posizionato
036200150217       // impostando c01rcd   visualizzo la pagina che vedeva l'utente quando ha
036300150218       // l'ultimo tasto
0364001502172         if c01csr   > 0;
036500150220           c01rcd   = c01csr;
036600150217          else;
036700150217           c01rcd   = s01rcd   ;
0368001502172         endif;
036900150217       // se non so quale pagina visualizzare forzo pagina 1
0370001502172         if c01rcd   < 1;
037100150217           eval c01rcd   = 1;
0372001502172         endif;
037300150217
037400150218       // valorizzo campi del sfl control
037500150218          vc1ksc=%int(v1cksc);
037600150218          vc1kscd=v1dksc;
037700150217
037800150217
037900150217         // Emissione videata
038000150217         write  lsi1z01;
038100150218         exfmt  lsi1C01;
038200150217
038300150217         s01nrr=1  ;
038400150217
038500150218 1       SELECT;
038600150217
038700150217         // - F3=Fine
038800150217 1         WHEN  *inkc;
038900150219               wFine = 'S';
039000150217
039100150217         // - F12=Ritorno
039200150218 1         WHEN  *inkl;
039300150217               wtpvid='1'  ;
039400150217
039500150217
039600150220         // -
039700150220 1       //WHEN  *inkb ;
039800150218         //  exsr seltot;
039900150217
040000150217
040100150217 x1      // Invio
040200150217           OTHER;
040300150217
040400150217               exsr ContrS01 ;
040500150217
040600150217 1       ENDSL;
040700150217      /end-free
040800150217     c                   endsr
040900150203     C*--------------------------------------------------------------*
041000150203     C* CTR01: Controlli prima videata
041100150203     C*--------------------------------------------------------------*
041200150203     C     CTR01         BEGSR
041300150219     c                   clear                   v1dksc
041400150204     c*  verifico la richiesta:
041500150204     c* è possibile entrare per - cod. cliente
041600150204     c*                         - key spedizione
041700150204     c*                         - Numero parcel DPD
041800150204     C                   clear                   sel
041900150204     C                   clear                   wsel
042000150204     c* selezionato cliente
042100150205     c                   if        v1cksc<>*blanks and v1cksc<>*zeros
042200150204     c                   eval      sel(1)=1
042300150204     c                   endif
042400150204     c* selezionata spedizione
042500150204     c                   if        v1cnsp>0
042600150204     c                   eval      sel(2)=1
042700150204     c                   endif
042800150204     c* selezionato n.parcel DPD
042900150204     c                   if        v1cdpd<>*blanks
043000150204     c                   eval      sel(3)=1
043100150204     c                   endif
043200150204     c                   xfoot     sel           wsel
043300150204     c                   if        wsel>1
043400150204     C                   SETON                                        28
043500150205     C                   MOVEL     ERR(30)       $MSG
043600150204     c                   leavesr
043700150204     c                   endif
043800150507     c                   if        wsel=0
043900150507     C                   SETON                                        28
044000150507     C                   MOVEL     ERR(33)       $MSG
044100150507     c                   leavesr
044200150507     c                   endif
044300150204     c                   select
044400150204     c* selezione per codice cliente
044500150204     c                   when      sel(1)=1
044600150205     c                   exsr      ctr01_ksc
044700150204     c* selezione per key spedizione
044800150204     c                   when      sel(2)=1
044900150204     c                   exsr      ctr01_nsp
045000150204     c* selezione per numero parcel dpd
045100150204     c                   when      sel(3)=1
045200150204     c                   exsr      ctr01_dpd
045300150204     c                   endsl
045400150204     c                   endsr
045500150204     c
045600150204     C*--------------------------------------------------------------*
045700150204     C* CTR01_nsp - Controlli prima videata - Accesso per key spedizione
045800150204     C*--------------------------------------------------------------*
045900150204     C     CTR01_nsp     BEGSR
046000130822     C*
046100970410     C* Controllo se immesso anno
046200150203     C     V1CAAS        IFEQ      0
046300970410     C                   SETON                                        2840
046400970611     C                   MOVEL     ERR(5)        $MSG
046500150203     c                   leavesr
046600970410     C                   END
046700970410     C* Se anno digitato di 2 --> 4 cifre
046800150203     C                   MOVEL     V1CAAS        WSECOL            2 0
046900970410     C     WSECOL        IFEQ      0
047000150203     C     V1CAAS        IFGT      60
047100150203     C                   ADD       1900          V1CAAS
047200970410     C                   ELSE
047300150203     C                   ADD       2000          V1CAAS
047400970410     C                   END
047500970410     C                   END
047600970410     C* Controllo se immessa linea di partenza
047700150203     C     V1CLNP        IFEQ      0
047800970414     C                   SETON                                        2841
047900970611     C                   MOVEL     ERR(6)        $MSG
048000150203     c                   leavesr
048100970410     C                   END
048200970410     C* Controllo esistenza lnp
048300150203     C                   Z-ADD     V1CLNP        ORGFIL
048400150219     C     ORGFIL        CHAIN     AZORG01L
048500150219     C                   if        not %found(azorg01l)
048600970410     C                   SETON                                        2841
048700970611     C                   MOVEL     ERR(7)        $MSG
048800150203     c                   leavesr
048900970414     C                   END
049000150204     c                   movel     orgde3        og143
049100970414     C* Controllo se lnp in £1
049200150203     C     V1CLNP        LOOKUP    £1                                     32
049300970414     C     *IN32         IFEQ      '0'
049400970414     C                   SETON                                        2841
049500970611     C                   MOVEL     ERR(8)        $MSG
049600150203     c                   leavesr
049700970414     C                   END
049800970414     C* Controllo se immesso numero di spedizione
049900150203     C     V1CNSP        IFEQ      0
050000970414     C                   SETON                                        2843
050100970611     C                   MOVEL     ERR(9)        $MSG
050200150203     c                   leavesr
050300970414     C                   END
050400970414     C* Controllo se esiste spedizione
050500150203     C                   Z-ADD     V1CAAS        KAAS
050600150203     C                   Z-ADD     V1CLNP        KLNP
050700150203     C                   Z-ADD     V1CNRS        KNRS
050800150203     C                   Z-ADD     V1CNSP        KNSP
050900150219     C     KBLP          CHAIN(N)  FNBLP01L
051000150219     C                   if        not %found(fnblp01l)
051100970414     C                   SETON                                        2843
051200970611     C                   MOVEL     ERR(10)       $MSG
051300150203     c                   leavesr
051400970616     C                   END
051500150209     c                   exsr      ctr01_uni
051600150209     c                   if        *in28
051700150209     c                   seton                                        43
051800150209     c                   endif
051900150204     c
052000040715      **
052100970414     C*
052200150203     C                   ENDSR
052300150205     C*--------------------------------------------------------------*
052400150205     C*
052500150205     C*--------------------------------------------------------------*
052600150205     C     ctr01_ksc     BEGSR
052700150219     c                   if        %scan( '?' : V1Cksc ) > *zero
052800150219     c                   clear                   V1Cksc
052900160125     c                   clear                   fnlsbsp1ds
053000150303     c                   eval      ibsptfp=duttfp
053100160125     c                   eval      IBSPTPS='K'
053200160125     c                   eval      KPJBU = fnlsbsp1ds
053300160125     c                   call      'FNLSBSP1R'
053400150219     c                   parm                    KPJBA
053500160125     c                   eval      FNLSBSP1ds = KPJBU
053600150219     c                   if        oBSPfxx <= *zero  and
053700160125     c                             oBSPerr  = ' '
053800150219     c                   eval      V1Cksc = %editc( oBSPcli : 'X' )
053900150219     c                   eval      V1Dksc = oBSPdes
054000150219     c                   endif
054100150219     c                   eval      *in45 = *on
054200150219     c                   eval      *in90 = *on
054300150219     c                   leavesr
054400150219     c                   endif
054500150209     c* Il codice o la filiale cliente deve esistere in BSP
054600150211     c                   move      v1cksc        wksc
054700150209     c                   movel     v1cksc        wlnp
054800150209     c                   exsr      cer_bsp
054900150209     c                   if        wbsp=' '
055000150209     c                   seton                                        2845
055100150209     C                   MOVEL     ERR(32)       $MSG
055200150209     c                   leavesr
055300150209     c                   endif
055400150209     c                   clear                   og143
055500150219     C     wlnp          CHAIN     AZORG01L
055600150209     c                   movel     orgde3        og143
055700150209     c* il cliente non deve essere EDI
055800150209     C                   Z-ADD     1             XX                5 0
055900150211     c     wksc          lookup    c3k(xx)                                32
056000150209     c                   if        (*in32 and csd(xx)='EW' ) or
056100150209     c                             (not *in32 and §ogntw='DPD')
056200150209     c                   seton                                        2845
056300150209     C                   MOVEL     ERR(31)       $MSG
056400150209     c                   endif
056500150209     c* decodifico il cliente
056600150211     c                   eval      i69kac=wksc
056700150211     c                   call      'TIBS69R'
056800150211     c                   parm                    tibs69ds
056900150211     c                   parm                    ds_cnaco
057000150211     c                   parm                    ds_cnind
057100150211     c                   parm                    ds_cnclp
057200150211     c                   parm                    ds_fncls
057300150211     c                   eval      v1dksc=acorag
057400150205     c                   endsr
057500150205     C*--------------------------------------------------------------*
057600150205     C*
057700150205     C*--------------------------------------------------------------*
057800150205     C     ctr01_dpd     BEGSR
057900150205     c*
058000150209     c                   clear                   wTrovS            1
058100150209     c                   clear                   wDpdOk            1
058200150206     c                   movel(p)  v1cdpd        kn30
058300150206     c                   movel     'I'           ktrc
058400150206     c     kar403        setll     fiar403l
058500150206     c     ktrc          reade     fiar403l
058600150209    1c                   dow       not %eof(fiar403l)
058700150209     c                   movel     ar4n30        dsbl4i
058800150209    2c                   if        §b4ipn=v1cdpd
058900150209     c                   z-add     ar4aas        kaas
059000150209     c                   z-add     ar4lnp        klnp
059100150209     c                   z-add     ar4nrs        knrs
059200150209     c                   z-add     ar4nsp        knsp
059300150209     c     kblp          chain(n)  fnblp01l
059400150209    3c                   if        %found(fnblp01l)
059500150209     c                   eval      wTrovS='1'
059600150209    3c                   endif
059700150209     C* Controllo se lnp in £1
059800150209    3C                   if        %lookup(blplnp:£1)>0
059900150209     c                   exsr      ctr01_uni
060000150209    4c                   if        not *in28
060100150209     c                   eval      wDpdOk='1'
060200150209     c                   leavesr
060300150209    4c                   endif
060400150209   x3c                   else
060500150209     c                   movel     err(8)        $msg
060600150209    3c                   endif
060700150209     c                   else
060800150209     c                   if        §b4ipn>v1cdpd
060900150209     c                   leave
061000150209     c                   endif
061100150209    2c                   endif
061200150206     c     ktrc          reade     fiar403l
061300150209    1c                   enddo
061400150209     c* se non ho trovato la spedizione restituisco errore
061500150209    1c                   if        wTrovS=*blanks
061600150209     c                   seton                                        2844
061700150209     C                   MOVEL     ERR(10)       $MSG
061800150209     c                   leavesr
061900150209    1c                   endif
062000150209    1c                   if        wDpdOk=*blanks
062100150209     c                   seton                                        2844
062200150209     c                   leavesr
062300150209    1c                   endif
062400150205     c                   endsr
062500150209     C*--------------------------------------------------------------*
062600150209     C*    Controlli unici per tutti i tipi di entrata
062700150209     C*--------------------------------------------------------------*
062800150209     C     ctr01_uni     BEGSR
062900150209     C* Controllo se bolla figlia
063000150219     C                   Z-ADD     blpAAS        KAAS
063100150209     C                   Z-ADD     blpLNP        KLNP
063200150219     C                   Z-ADD     blpNRS        KNRS
063300150209     C                   Z-ADD     blpnsp        KNSP
063400150219     C     KBLP          CHAIN     FNLBL01L
063500150219     C                   if        %found(fnlbl01l)
063600150209     C                   SETON                                        28
063700150209     C                   MOVEL     ERR(12)       $MSG
063800150209     c                   leavesr
063900150209     C                   END
064000150209     C* Controllo che non sia stata data per consegnata
064100150209     C     BLPDCM        IFNE      0
064200150209     C     BLPCCA        ANDNE     '6'
064300150209     C     BLPDCM        ORNE      0
064400150209     C     BLPCCA        ANDNE     '2'
064500150209     C                   SETON                                        28
064600150209     C                   MOVEL     ERR(22)       $MSG
064700150209     c                   leavesr
064800150209     C                   END
064900150209     C* Controllo che abbia il dettaglio segnacolli
065000150209     C***                Z-ADD     V1CAAS        KAAS
065100150209     C***                Z-ADD     V1CLNP        KLNP
065200150209     C***                Z-ADD     V1CNRS        KNRS
065300150209     C***                Z-ADD     V1CNSP        KNSP
065400150219     C     KBLT          CHAIN(n)  FNBLT01L
065500150219     C                   IF        not %found(fnblt01l)
065600150209     C                   SETON                                        28
065700150209     C                   MOVEL     ERR(16)       $MSG
065800150209     c                   leavesr
065900150209     C                   END
066000150209     C* Se bolla in assegnato no IDD
066100150209     C                   Z-ADD     1             X
066200150209     C     BLPCBO        LOOKUP    C3A(X)                                 32
066300150209     C                   MOVEL     TPO(X)        WTPO              1
066400150209     C     *IN32         IFEQ      '1'
066500150209     C     WTPO          ANDEQ     'A'
066600150209     C                   SETON                                        28
066700150209     C                   MOVEL     ERR(20)       $MSG
066800150209     c                   leavesr
066900150209     C                   END
067000150209     C* Controllo che non sia stata data per partita
067100150209     C     BLPDT1        IFNE      0
067200150209     C                   SETON                                        28
067300150209     C                   MOVEL     ERR(13)       $MSG
067400150209     c                   leavesr
067500150209     C                   END
067600150209     C* Controllo che non sia stata trasmessa in sede
067700150209     C     BLPDT2        IFNE      0
067800150209     C                   SETON                                        28
067900150209     C                   MOVEL     ERR(14)       $MSG
068000150209     c                   leavesr
068100150209     C                   END
068200150209     C* Controllo che non sia stata trasmessa al cliente
068300150209     C     BLPDT3        IFNE      0
068400150209     C                   SETON                                        28
068500150209     C                   MOVEL     ERR(15)       $MSG
068600150209     c                   leavesr
068700150209     C                   END
068800150209      * Controllo C.A.
068900150209      *
069000150209     c                   clear                   fidn12ds
069100150209     c                   eval      i12aas = blpaAS
069200150209     c                   eval      i12lnp = blpLNP
069300150209     c                   eval      i12nrs = blpNRS
069400150209     c                   eval      i12nsp = blpNSP
069500150209     c                   eval      i12fel = 'E'
069600150209     c                   eval      i12fan = 'E'
069700150209     c                   eval      i12fch = 'E'
069800150209     c                   call      'FIDN12R'
069900150209     c                   parm                    fidn12ds
070000150209      **
070100150209      * se c'è almeno una C.A. aperta
070200150209     c     o12nca        ifgt      0
070300150209     C                   SETON                                        28
070400150209     C                   MOVEL     ERR(24)       $MSG
070500150209     c                   leavesr
070600150209     c                   end
070700150209     c* controllo che la spedizione non abbia spunte
070800150209     c                   clear                   wsispu            1
070900150209     c     kblt          setll     fnblt01l
071000150209     c     kblt          reade(n)  fnblt01l                               99
071100150209    2c                   dow       not *in99
071200150209     c
071300150219     c* Cerco una qualsiasi spunta
071400150209     c     kbrv          chain     fnbrv07l
071500150209    3c                   if        %found(fnbrv07l)
071600150209     c                   eval      wsispu='S'
071700150209     c                   seton                                        99
071800150209   x3c                   else
071900150209     c     kblt          reade(N)  fnblt01l                               99
072000150209    3c                   endif
072100150209    2c                   enddo
072200150209     c*
072300150209    2c                   if        wsispu='S'
072400150209     c                   seton                                        28
072500150209     C                   MOVEL     ERR(29)       $MSG
072600150209     c                   leavesr
072700150209     c                   endif
072800150209     c* il cliente o la lnp della bolla deve essere in tabella BSP con
072900150209     c* la sospensione da spunte
073000150209     c                   z-add     blpksc        wksc
073100150209     c                   z-add     blplnp        wlnp
073200150209     c                   exsr      cer_bsp
073300150209     c                   if        wbsp=' '
073400150209     c                   seton                                        28
073500150209     C                   MOVEL     ERR(27)       $MSG
073600150209     c                   leavesr
073700150209     c                   endif
073800150209     c* il cliente non deve essere EDI
073900150209     C                   Z-ADD     1             XX                5 0
074000150209     c     blpksc        lookup    c3k(xx)                                32
074100150209     c                   if        (*in32 and csd(xx)='EW' ) or
074200150209     c                             (not *in32 and §ogntw='DPD')
074300150209     c                   seton                                        28
074400150209     C                   MOVEL     ERR(31)       $MSG
074500150209     c                   endif
074600150209     c
074700150209     c* per poter chiudere la spedizione come merce mai affidata
074800150209     c* devono essere trascorsi i giorni max di sospensione previsti
074900150209     c* da tabella
075000150209     c                   clear                   wdat8
075100150209     c                   z-add     blpdim        datada
075200150209     c                   eval      dataa=(blpaas*10000)+blpmgs
075300150209     c                   call      'XSRLAV8'
075400150209     c                   parm                    wdat8
075500150209     c                   if        (ggl-1) <= §3iggc
075600150209     c                   seton                                        28
075700150209     C                   MOVEL     ERR(28)       $MSG
075800150209     c                   leavesr
075900150209     c                   endif
076000150209     c
076100150209     c                   endsr
076200150217      /free
076300150211       //--------------------------------------------------------------
076400150211       BEGSR InzS01;
076500150211
076600150211       // Pulizia subfile
076700150216         *in30  = *on;
076800150216         *in31  = *on;
076900150211         write  lsi1c01;
077000150216         *in31  = *off;
077100150216         *in30  = *off;
077200150216         *in32  = *off;
077300150211
077400150217         exec sql close BLPcsr;
077500150217       // Richiamo routine di preparazione stringa SQL
077600150217             exsr sr_prepsql;
077700150217
077800150211         S01nrr=0 ;
077900150211         clear $msg;
078000150216         *in28  = *off;
078100150216         *in90  = *off;
078200150728         *in19  = *off;
078300150217       // Elaboro i records estratti
078400150217           $finerec=*off;
078500150217           exec sql prepare STRINGASQL from :WrkStringaSql;
078600150217           exec sql declare BLPCSR cursor for StringaSql;
078700150217           exec sql open BLPcsr;
078800150217           dow $finerec=*off;
078900150218              exec sql Fetch BLPcsr into :fnblpds;
079000150217              if sqlcod=100 or sqlcod<0;
079100150217                 $finerec = *on;
079200150217                 leave;
079300150217              endif;
079400150218              if %lookup(blplnp:£1)>0;
079500150928                 *in28=*off ;
079600150218                 exsr ctr01_uni;
079700150218                 if not *in28;
079800150217           // Carico record nel sfl
079900150218                     exsr carsf01;
080000150218                 endif;
080100150218              endif;
080200150217           enddo;
080300150211
080400150217          s01rcd    = s01nrr  ;
080500150219          *in28=*off ;
080600150211
080700150211         ENDSR  ;
080800150217       //--------------------------------------------------------------
080900150217       //?Preparazione stringa SQL
081000150217       //--------------------------------------------------------------
081100150217       BEGSR sr_prepsql;
081200150217       WrkStringaSql='select * from fnblp00f' +
081300150218       ' where (blpksc=' + v1cksc + ' or '      +
081400150218       ' blpccm=' + v1cksc + ') and blptfp=' + %editc(wtfp:'X') +
081500150218       ' and blpnfv=0' +
081600150217       ' order by  blptfp, blpnfv, blpdim';
081700150217       endsr;
081800150217       //--------------------------------------------------------------
081900150217       //?eseguo caricamento SFL
082000150217       //--------------------------------------------------------------
082100150217          BEGSR   carSF01    ;
082200150218          clear vs1opz  ;
082300150217
082400150217          exsr CarRECORD  ;
082500150218          vshaas=blpaas;
082600150218          vshlnp=blplnp;
082700150218          vshnrs=blpNRs;
082800150218          vshnsp=blpNsp;
082900150217          s01nrr=s01nrr+1   ;
083000150217          write  lsi1S01;
083100150217
083200150217          ENDSR         ;
083300150217       //--------------------------------------------------------------
083400150217       //?Caricamento record SFL
083500150217       //--------------------------------------------------------------
083600150217       BEGSR CarRecord;
083700150218       // data Immissione bolla
083800150218             clear vs1dim;
083900150218             datadmy=%date(blpdim:*iso);
084000150218             vs1dim=%dec(datadmy);
084100150218       // Data Spedizione
084200150218             clear vs1dsp;
084300150219             eval datadmy=%date((blpaas*10000+blpmgs):*iso);
084400150218             vs1dsp=%dec(datadmy);
084500150219       // linea di arrivo
084600150219             vs1lna=blplna;
084700150219             chain blplna azorg01l ;
084800150219             if orgde5<>*blanks;
084900150219                 vs1lnad=%subst(orgde5:1:10);
085000150219             else;
085100150219                 vs1lnad=%subst(orgdes:1:10);
085200150219             endif;
085300150218       // Destinatario
085400150218             vs1rsd=blprsd;
085500150217       ENDSR         ;
085600150218       //--------------------------------------------------------------
085700150218       //?controlli SFL
085800150218       //--------------------------------------------------------------
085900150218       BEGSR ContrS01;
086000150218
086100150218       s01nrr=1  ;
086200150218       readc lsi1s01    ;
086300150218
086400150218 0     dow not %eof                      ;
086500150218
086600150218       // Verifica l'opzione
086700150218          if vs1opz<>*blanks    ;
086800150218            *in19=*off;
086900150218            wopz='1';
087000150218          select;
087100150218          when vs1opz='1'                     ;
087200150219            chain(n) (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
087300150218            exsr gesd02;
087400150218            chain(n) (vshaas:vshlnp:vshnrs:vshnsp) fnblp01l ;
087500150218            if blpdcm>0;
087600150218               eval *in19=*on;
087700150218            endif;
087800150218          when vs1opz='5';
087900150218               exsr int_bolla;
088000150218          endsl;
088100150219               clear vs1opz;
088200150218               update  lsi1s01  ;
088300150218          endif;
088400150218
088500150218       readc    lsi1s01    ;
088600150218 0     enddo         ;
088700150218
088800150218       ENDSR  ;
088900150218       //--------------------------------------------------------------
089000150218       //?Aggiornamento sfl
089100150218       //--------------------------------------------------------------
089200150218       BEGSR AGGIOSFL  ;
089300150218
089400150218       //Sflnxtchg=*on  ;
089500150218
089600150218       update  lsi1s01  ;
089700150218
089800150218       ENDSR   ;
089900150218       //--------------------------------------------------------------
090000150218       //?Cerca cliente in tabella BSP
090100150218       //--------------------------------------------------------------
090200150218       BEGSR cer_bsp;
090300150218       clear wbsp;
090400160125       chain wksc fibsp02l  ;
090500160125       if %found(fibsp02l)  ;
090600160125        wbsp='S' ;
090700160125       else  ;
090800160125         chain wlnp fibsp01l  ;
090900160125         if %found(fibsp01l)  ;
091000160125          wbsp='S' ;
091100160125         endif  ;
091200160125       endif  ;
091300160125
091400160125       // ksife=knsif;
091500160125       // setll (kcode: kline: ksife) tntbe02l;
091600160125       // if not %equal(tntbe02l);
091700160125       //    clear ksife;
091800160125       //    setll (kcode: kline: ksife) tntbe02l;
091900160125       // endif;
092000160125       // if %equal(tntbe02l);
092100160125       //    reade (kcode: kline: ksife) tntbe02l;
092200160125       //   dow not %eof(tntbe02l) and wbsp=' ';
092300160125       //       if tbeatb=' ' and (tbeke2=%editc(wksc:'X') or
092400160125       //                          tbeke2=%editc(wlnp:'X'));
092500160125       //          wbsp='S';
092600160125       //          dbsp=tbeuni;
092700160125       //       endif;
092800160125       //       reade (kcode: kline: ksife) tntbe02l;
092900160125       //   enddo;
093000160125       // endif;
093100150218       endsr;
093200150217      /end-free
093300150218     C*--------------------------------------------------------------*
093400150218     C* interroga bolla in partenza
093500150218     C*--------------------------------------------------------------*
093600150218     C     int_bolla     BEGSR
093601161107     c                   clear                   paramu6ds1
093602161107     C                   Z-ADD     VshAAS        Pa1AAS_
093603161107     C                   Z-ADD     VshLNP        Pa1LNP_
093604161107     C                   Z-ADD     VshNRS        Pa1NRS_
093605161107     C                   Z-ADD     VshNSP        Pa1NSP_
093606161107     C                   MOVEL     paramu6ds1    KPJBU
093607161107     C                   CALL      'FNLRU6R1'
093608161107     C                   PARM                    KPJBA
094600150218     c                   endsr
094700150204     C*--------------------------------------------------------------*
094800150204     C* Chiusura bolla per merce mai affidata
094900150204     C*--------------------------------------------------------------*
095000150204     C     sr_ChiuBolla  BEGSR
095100150219     C     KBLT          CHAIN(e)  FNBLP01L
095200150219     c                   if        %error
095300150219     c                   eval      $msg=err(11)
095400150219     c                   seton                                        28
095500150219     c                   leavesr
095600150219     c                   endif
095700150204     C     KBLT          CHAIN     FNBLT01L                           32
095800150204     C     *IN32         DOWEQ     '0'
095900150204     C                   Z-ADD     OGGI          BLTDFV
096000150303     C                   Z-ADD     99998         BLTNFV
096100150204     C                   Z-ADD     83            BLTNPP
096200150204     C                   Z-ADD     OGGI          BLTDCM
096300150204     C                   Z-ADD     OGGI          BLTDTR
096400150204     C                   MOVEL     'N'           BLTFTR
096500150204     C                   MOVEL     '7'           BLTFL1
096600150204     C                   UPDATE    FNBLT000
096700150204     C* Lettura successiva
096800150204     C     KBLT          READE     FNBLT01L                               32
096900150204     C                   ENDdo
097000150204
097100150204     C                   Z-ADD     0             BLPDCE
097200150204     C                   Z-ADD     0             BLPNPI
097300150204     C                   Z-ADD     0             BLPNCI
097400150204     C                   Z-ADD     0             BLPNRC
097500150204     C                   Z-ADD     OGGI          BLPDBR
097600150204     C                   Z-ADD     OGGI          BLPDPC
097700150204     C                   Z-ADD     OGGI          BLPDUC
097800150204     C                   Z-ADD     OGGI          BLPDDC
097900150204     C                   Z-ADD     OGGI          BLPDCM
098000150204     C                   TIME                    WTIME             6 0
098100150204     C                   MOVEL     WTIME         BLPHMC
098200150204     C                   Z-ADD     99999         BLPNFV
098300150204     C                   Z-ADD     9999999       BLPNDC
098400150204     C                   Z-ADD     OGGI          BLPDT1
098500150204     C                   Z-ADD     OGGI          BLPDT2
098600150204     C                   MOVEL     'N'           BLPFT1
098700150204     C                   MOVEL     'N'           BLPFT2
098800150204     C* Per idd metto codice consegna anomala sulla bolla
098900150204     C                   MOVEL     '7'           BLPCCA
099000150204     C* devo inviare date consegna cliente
099100150303     C                   clear                   BLPDT3
099200150303     C                   clear                   BLPFT3
099300150204     C* Update bolla
099400150204     C                   UPDATE    FNBLP000
099500150204     c                   endsr
099600970410     C*--------------------------------------------------------------*
099700970410     C* *INZSR: Operazioni iniziali
099800970410     C*--------------------------------------------------------------*
099900970410     C     *INZSR        BEGSR
100000970410     C*
100100970410     C     *ENTRY        PLIST
100200970410     C                   PARM                    KPJBA
100300150204
100400150204     c     *dtaara       define    §azute        AZUTEds
100500150204     c     *dtaara       define    §datiute      dDATIUTE
100600150204     c                   in(E)     *dtaara
100700150204     c                   if        %ERROR or RSUT = *blanks
100800150204     c                   clear                   Tibs34Ds
100900150204     c                   call      'TIBS34R'
101000150204     c                   parm                    Tibs34Ds
101100150204     c                   in        *dtaara
101200150204     c                   endif
101300150204      *
101400150204     c                   z-add     1             CodUt             1 0
101500030924      *
101600030924      *
101700970410      *  Definizione chiavi
101800970410     C     KBLP          KLIST
101900970410     C                   KFLD                    KAAS
102000970410     C                   KFLD                    KLNP
102100970410     C                   KFLD                    KNRS
102200970410     C                   KFLD                    KNSP
102300970410     C     KBLT          KLIST
102400150204     C                   KFLD                    blpaas
102500150204     C                   KFLD                    blplnp
102600150204     C                   KFLD                    blpnrs
102700150204     C                   KFLD                    blpnsp
102800150204     C     Kar41         KLIST
102900150204     C                   KFLD                    blpaas
103000150204     C                   KFLD                    blplnp
103100150204     C                   KFLD                    blpnrs
103200150204     C                   KFLD                    blpnsp
103300150204     C                   KFLD                    ktrc
103400150209     C     Kar403        KLIST
103500150209     C                   KFLD                    ktrc
103600150209     C                   KFLD                    kn30
103700150204     C     KBrv          KLIST
103800150204     C                   KFLD                    bltfls
103900150204     C                   KFLD                    bltlna
104000150204     C                   KFLD                    bltNRS
104100150204     C                   KFLD                    bltnsc
104200970410     C     KTAB1         KLIST
104300970410     C                   KFLD                    KKUT
104400970410     C                   KFLD                    KCOD
104500970410     C     KTAB2         KLIST
104600970410     C                   KFLD                    KKUT
104700970410     C                   KFLD                    KCOD
104800970410     C                   KFLD                    KKEY
104900970410      *  Definizione variabili
105000970410     C     *LIKE         DEFINE    BLPAAS        KAAS
105100970410     C     *LIKE         DEFINE    BLPLNP        KLNP
105200970410     C     *LIKE         DEFINE    BLPNRS        KNRS
105300970410     C     *LIKE         DEFINE    BLPNSP        KNSP
105400970410     C     *LIKE         DEFINE    TBLKUT        KKUT
105500970410     C     *LIKE         DEFINE    TBLCOD        KCOD
105600970410     C     *LIKE         DEFINE    TBLKEY        KKEY
105700970410      *
105800970410      *  Reperisco la data del giorno
105900970410     C                   TIME                    WHHDAT           14 0
106000970414     C                   MOVE      WHHDAT        G02DAT
106100970414     C                   MOVEL     *BLANK        G02ERR
106200970414     C                   CALL      'XSRDA8'
106300970414     C                   PARM                    WLBDA8
106400970414     C                   Z-ADD     G02INV        OGGI              8 0
106500970611     C*
106600970410     C* Eseguo caricamento tabella 3K
106700970410     C                   Z-ADD     0             X
106800970410     C                   Z-ADD     1             KKUT
106900970410     C                   MOVEL     '3K'          KCOD
107000970410     C     KTAB1         CHAIN     TABEL                              30
107100970410     C     *IN30         DOWEQ     '0'
107200030213     C     X             ANDLT     9990
107300970410     C     TBLFLG        IFEQ      *BLANKS
107400970410     C                   ADD       1             X
107500970617     C                   MOVEL     TBLUNI        DS3K
107600970617     C                   MOVEL     TBLKEY        C3K(X)
107700150219     C****               MOVEL     §3KCKS        CKS(X)
107800010725     C                   MOVEL     §3KCSD        CSD(X)
107900970410     C                   END
108000970410     C     KTAB1         READE     TABEL                                  30
108100970410     C                   END
108200970715     C* Eseguo caricamento tabella 3A
108300970721     C                   Z-ADD     0             X
108400970715     C                   MOVEL     '3A'          KCOD
108500970715     C     KTAB1         CHAIN     TABEL                              30
108600970715     C     *IN30         DOWEQ     '0'
108700970721     C     X             ANDLT     500
108800970715     C     TBLFLG        IFEQ      *BLANKS
108900970715     C                   ADD       1             X
109000970715     C                   MOVEL     TBLUNI        DS3A
109100970715     C                   MOVEL     TBLKEY        C3A(X)
109200970715     C                   MOVEL     §3ATB1        TPO(X)
109300970715     C                   END
109400970715     C     KTAB1         READE     TABEL                                  30
109500970715     C                   END
109600150204     C***
109700150204     C                   MOVEL     '3I'          Kcod
109800150204     C                   MOVEL     '1       '    kKEY
109900150204     c                   clear                   ds3i
110000150204     C     KTAB2         CHAIN     TABEL00F
110100150205     c                   if        %found(tabel00f)
110200150204     C                   MOVEL     TBLUNI        DS3I
110300150204     c                   endif
110400150204     C***
110500970410     C* Eseguo caricamento £1
110600020506     c                   clear                   trul06ds
110700020506     c                   eval      d06cod = '£1'
110800150205     c                   movel     dutpou        d06key
110900020506     c                   movel     trul06ds      kpjbu
111000020506     c                   call      'TRUL06R'
111100020506     c                   parm                    kpjba
111200020506     c                   movel     kpjbu         trul06ds
111300020506     c                   movea     lin           £1
111400150218     c* terminal di partenza del p.o. utente
111500150218     c                   clear                   fnlv55ds
111600150218     C                   MOVEL     'P'           D55TPT
111700150218     C                   MOVEL     dutpou        D55LIN
111800150218     C                   z-add     oggi          D55drf
111900150218     C                   CALL      'FNLV55R'
112000150218     C                   PARM                    fnlv55ds
112100150218     c                   z-add     d55tfp        wtfp
112200020912      *
112300970410     C*
112400970410     C                   ENDSR
112500080930      *------------------------------------------------------------------------*
112600080930      * Routine di controllo x eventuali Errori
112700080930      *------------------------------------------------------------------------*
112800080930     C     *PSSR         BEGSR
112900080930      *
113000080930     C                   ENDSR     '*CANCL'
113100080930     C*------------------------------------------------------------------------*
113200971008**
113300150220                                                                              01
113400150220                                                                              02
113500150220                                                                              03
113600150220                                                                              04
113700970701Immettere anno della spedizione                                               05
113800970701Immettere linea di partenza della spedizione                                  06
113900970701Linea di partenza inesistente                                                 07
114000150209Spedizione non in gestione alla filiale                                       08
114100970701Immettere numero della spedizione                                             09
114200970701Spedizione inesistente                                                        10
114300150219Richiesta non elaborabile: bolla allocata da altro utente. Attendi e riprova  11
114400980206Bolla figlia: impossibile gestirla                                            12
114500970701Spedizione già trasmessa alla filiale di arrivo                               13
114600970701Spedizione già trasmessa alla sede                                            14
114700970701Spedizione già trasmessa al cliente                                           15
114800980202Spedizione senza colli: impossibile eseguire chiusura                         16
114900980202Spedizione non appartenente al cliente selezionato                            17
115000150220                                                                              18
115100150220                                                                              19
115200980202Bolla in assegnato: chiusura definitiva possibile solo in arrivo              20
115300150217Nessun dato da visualizzare per la selezione richiesta
115400980204Spedizione già consegnata                                                     22
115500150220                                                                              23
115600150220Esistono C.A. aperte per la spedizione                                        24
115700150220                                                                              25
115800150220                                                                              26
115900150205Sped. non gestibile:non prevista dalla Sospensione Bolle x colli non spuntati 27
116000150205Sped. non gestibile:non sono ancora trascorsi i giorni max previsti x sospen. 28
116100150205Spedizione non gestibile:presenti spunte                                      29
116200150204Effettuare una sola selezione                                                 30
116300150209Richiesta non elaborabile:cliente E.D.I.                                      31
116400150303Cliente non previsto dalla sospensione Bolle x colli non spuntati             32
116500150507Effettuare almeno una selezione                                               33
